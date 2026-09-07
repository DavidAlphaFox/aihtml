# 02 · 目标架构

## 1. 核心思路：把「缓存」从 ets 移到 BEAM code server

模板的编译产物**就是代码本身**。运行期不查表、不发消息、不拷贝。

```
views/index.mustache
   │
   ├─ ai_mustache_scanner      词法：切 tag、处理 standalone / delimiter
   ├─ ai_mustache_parser       语法：产出 AST
   │
   └─ ai_mustache_compiler     AST → Erlang abstract forms      唯一编译核心
         │
         ├─ rebar3 plugin      erl_prettypr:format → _gen/view_index.erl → .beam
         └─ parse_transform    直接注入调用方模块的 forms
```

> **一个 compiler，两个消费者。** 这是让「rebar3 plugin」与「parse_transform」两个目标不互相打架的关键。二者共享同一份 AST → forms 的实现，行为天然一致。

## 2. 去 ets 的四条技术论据

### 2.1 literal pool 零拷贝 vs ets 深拷贝

静态文本编译进模块后进入 BEAM 的 **literal pool**，跨进程按引用共享；ets 每次 `lookup` 都是完整深拷贝。这是性能上的量级差异，也是本次重构最核心的收益。

### 2.2 partial 编译成跨模块函数调用

`{{> shared/item}}` → `view_shared_item:render_stack(S)`。三个白送的好处：

1. **增量编译粒度天然正确** —— 改 partial 不需要重编父模板（对比：内联展开方案必须重编所有引用者）。
2. **循环 partial 引用天然支持** —— `a` 引用 `b`、`b` 引用 `a` 在模块互调下完全合法（现有解释器会无限递归）。
3. **热更新走 `code:load_binary/3`** —— OTP 原生机制，不需要自己实现失效通知。

### 2.3 开发期热重载不需要任何额外状态

生成的模块自带 attribute：

```erlang
-mustache_source(#{path => <<"views/index.mustache">>, mtime => 1757..., hash => <<...>>, vsn => 1}).
```

dev 模式渲染前读 `Mod:module_info(attributes)`，用 `ai_mustache_compiler:source_hash/2` 重算 stamp 与 attribute 比对，变了就重编重载。**code server 本身就是缓存**，连 `persistent_term` 都不需要。

> **不要用 mtime 做快速过滤。** 「mtime 未变即未过期」看起来是免费的优化，但 POSIX mtime 只有秒级精度，而「改完立刻刷新」正是 dev 热重载存在的理由 —— 同一秒内的编辑会被判为未变更，用户看到的是改了没生效。读一个模板文件并算 md5 只要几微秒，一次误判却要搭进一段调试时间。attribute 里的 `mtime` 只作诊断信息保留。

### 2.4 运行期退化为 library application

删掉 gen_server、supervisor、application 回调后，aihtml 是纯 library app（`.app.src` 中无 `mod` 项），零进程、零 ets、零启动顺序依赖。使用方不需要 `application:start(aihtml)`。

## 3. 目标目录结构

```
aihtml/
├── rebar.config                      # 主构建（rebar3）
├── Makefile / erlang.mk              # 保留但降级，通过 DEP_PLUGINS 接入 plugin
├── designs/                          # 本目录
├── src/
│   ├── aihtml.app.src                # library app，无 mod 项
│   ├── ai_mustache.erl               # 门面：render/2 · render_iolist/2 · inline/2
│   ├── ai_mustache_scanner.erl       # 词法（从现 parser 拆出 split_tag/2 · standalone/3）
│   ├── ai_mustache_parser.erl        # 语法 → AST（保留 bbmustache MIT 版权头）
│   ├── ai_mustache_compiler.erl      # AST → abstract forms          核心
│   ├── ai_mustache_text.erl          # 规范文本表示：UTF-8 binary 的唯一转换点
│   ├── ai_mustache_rt.erl            # 生成代码的运行时依赖：lookup/2 · escape/1
│   ├── ai_mustache_transform.erl     # parse_transform
│   ├── ai_mustache_ext.erl           # 扩展 behaviour
│   └── ai_mustache_dev.erl           # 开发期 mtime 热编译（可选，默认不启用）
├── include/
│   └── ai_mustache.hrl               # AST 记录定义（compiler 与 plugin 共享）
├── rebar3_aihtml/                    # plugin 子项目（同仓，subdir 引用）
│   ├── rebar.config
│   └── src/
│       ├── rebar3_aihtml.erl         # init/1 注册 provider
│       ├── rebar3_aihtml_prv.erl     # compile provider
│       └── rebar3_aihtml_migrate.erl # migrate provider（语义迁移辅助）
├── scripts/check.sh                  # 一键 compile / eunit / xref / dialyzer
├── test/
│   ├── ai_mustache_test_lib.erl      # spec 加载、key 转换、用例名归一、阶段判定
│   ├── ai_mustache_test_lib_tests.erl
│   ├── ai_mustache_spec_tests.erl    # 官方 spec 一致性（EUnit generator）
│   ├── ai_mustache_ext_tests.erl     # aihtml 扩展语义
│   ├── ai_mustache_parser_tests.erl  # 阶段 2
│   ├── ai_mustache_compiler_tests.erl# 阶段 3
│   ├── rebar3_aihtml_SUITE.erl       # 阶段 4，plugin 端到端，需起子进程故用 CT
│   └── spec/                         # 官方 spec JSON（不取 YAML，OTP 无 YAML 解析）
└── examples/
    ├── complex.erl / complex.mustache
    └── shared/*.mustache             # 按新语义重写
```

## 4. 删除清单（决策 D4 + 架构收敛）

| 文件 | 原因 |
|---|---|
| `src/ai_mustache_loader.erl` | ets + gen_server 缓存机制被编译期方案完全取代 |
| `src/ai_mustache_runner.erl` | 解释器被生成代码取代 |
| `src/aihtml_app.erl` | 退化为 library app |
| `src/aihtml_sup.erl` | 无进程可管 |
| `src/ai_dom_node.erl` | 与 mustache 无关、无内部引用 |
| `src/ai_dom_render.erl` | 同上，且有必崩 bug（见 [01](01-current-state.md#b2-·-ai_dom_renderrender2-必崩)） |
| `examples/dom_render.erl` | 随 `ai_dom_*` 一并删除 |

## 5. 公开 API

```erlang
%% 编译期已知模板 —— 推荐路径，直接调生成模块
view_index:render(Ctx)         -> binary().
view_index:render_iolist(Ctx)  -> iolist().

%% 门面 —— 模板名运行期才确定时使用
ai_mustache:render(view_index, Ctx)         -> binary().
ai_mustache:render_iolist(view_index, Ctx)  -> iolist().

%% 内联模板（parse_transform 展开；未展开时退化为运行期解释）
ai_mustache:inline(Template :: binary(), Ctx :: map()) -> binary().

%% 开发期热重载（dev profile）
ai_mustache_dev:check()          -> ok | {stale, [module()]}.
ai_mustache_dev:reload(Mod | all) -> ok | {error, term()}.
```

> **`ai_mustache_dev` 没有「开关」。** 架构不变量禁止 ets / persistent_term / 进程，因此无处存放启用状态。`check/0` 是一次性自检（对比磁盘内容 hash 与模块 `-mustache_source` 的 `opts`+`hash`），`reload/1` 是显式重编重载。**触发必须由调用方显式发起** —— 典型做法是在使用方的开发期请求入口处调一次 `reload(all)`，或绑到编辑器保存钩子。这一点必须在 module doc 与 README 中写清楚，否则用户会以为调用一次就自动生效。

**输出 iolist 是纯收益**：`render_iolist/1` 可直接喂给 cowboy `Body`，省掉一次 `iolist_to_binary`；`render/1` 保留 binary 返回以兼容既有调用。

门面 `ai_mustache:render/2` 保留的意义：模板名在运行期才确定的场景（如按路由派发布局模板）。实现就是一次 `Mod:render(Ctx)` 调用，无查表开销。

## 6. 零依赖决策

> **决策 D5：彻底移除 ailib 依赖，全部重新实现。**

原先用到的 ailib 函数与替代方案：

| 原用途 | 替代 |
|---|---|
| `ai_maps:get/3` | `ai_mustache_rt:lookup/2` —— 语义本就不同（需 context stack 回溯），无法复用 |
| `ai_string:html_escape/1` | `ai_mustache_rt:escape/1` —— 单遍 binary 扫描替代 8 遍 `re:replace`，并修掉 `&amp` 缺分号（见 [B3](01-current-state.md#b3-·-html-escape-缺分号且极慢（在-ailib-中）)） |
| `ai_string:to_string/1` | `ai_mustache_rt:to_binary/1` —— 需按 mustache spec 处理数值格式（见下） |
| `ai_function:while/2` | scanner 内直接递归 |

替代实现全部落在 `ai_mustache_rt`（不足 200 行），**生成代码的唯一运行期依赖，且该模块只依赖 OTP**。

### 6.1 测试期也零依赖

mustache 官方 spec 是 JSON。**OTP 27+ 的 stdlib 自带 `json` 模块**，`json:decode/1` 直接可用，因此测试期同样不需要引入 JSON 解析依赖。

### 6.2 数值格式化必须匹配 spec

`ai_mustache_rt:to_binary/1` 的实现细节，由官方 spec 直接约束：

```erlang
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(F) when is_float(F)   -> float_to_binary(F, [short]);   %% 1.21 而非 1.21000...e+00
```

`interpolation.json` 的 `Basic Decimal Interpolation` 要求 `1.21` 渲染为 `"1.21"`，`sections.json` 的 `Implicit Iterator - Decimal` 要求 `1.1` → `"1.1"`。OTP 24+ 的 `float_to_binary/2` `short` 选项正好满足。

### 6.3 根 context 不限于 map

`interpolation.json` 的 `Implicit Iterators - Basic Integer Interpolation` 的 data 是裸整数 `85`。因此 `render/1` 的入参类型是 `term()` 而非 `map()`，栈初始化为 `[Ctx]` 即可。

## 7. 文本的规范表示：UTF-8 binary

模板正文与 aihtml 处理的每一个路径，规范表示都是 **UTF-8 binary**。

早先的实现里 scanner / parser / ast / compiler 各自带一份 `source_of/1`，都同时接受 binary 和 list；`prefix` 之类的选项传 string 会在 `<<Prefix/binary, ...>>` 处直接崩。更隐蔽的是 **`source_hash/2` 对 `views => "views"` 和 `views => <<"views">>` 算出不同的 stamp** —— 这会让 plugin 与 `ai_mustache_dev` 在「是否过期」上产生分歧，正是 [§4.1 hash 归属](04-codegen.md#41-source-hash-的计算归属) 要防的那类漂移，只是换了个入口。

### 7.1 转换只发生在两处

| 方向 | 位置 |
|---|---|
| 入站 | `ai_mustache_scanner:scan/2`（模板正文的唯一入口）与各 API 入口的 `ai_mustache_text:opts/1` |
| 出站 | `ai_mustache_text:to_list/1`，只用于 `code:load_binary/3` 这类必须收 string 的 OTP 调用 |

中间的所有模块只见 binary，不做任何判型。

### 7.2 非法 UTF-8 的模板直接拒绝

`ai_mustache_text:template/1` 校验后报 `{invalid_utf8, ByteOffset}`。按字节透传看似宽容，但错误会推迟到生成模块的输出里才显形，比在门口拒绝难查得多。路径则**只规范化不拒绝** —— 路径来自文件系统和 rebar3，在非 UTF-8 文件名的系统上可能是裸字节，但它仍然指向真实文件。

### 7.3 为什么按 ASCII 字节切分是安全的

UTF-8 是自同步编码，小于 128 的字节绝不会出现在多字节序列内部，而 scanner 只按 `{{`、`}}` 和换行切分。因此**在入口校验一次，就保证了 AST 中每个 text 节点也是合法 UTF-8**，`bin/2` 生成 `<<"..."/utf8>>` 字面量才成立。

## 8. 架构不变量

实现过程中必须始终成立的约束，作为 code review 检查项：

1. 运行期**不创建任何进程**，不使用 ets / persistent_term（dev 模式除外，且 dev 模块不进 prod profile）。
2. `ai_mustache_compiler` 是唯一的 AST → forms 实现，plugin 与 parse_transform 都不得旁路它。
3. 生成的模块只依赖 `ai_mustache_rt` 和其它生成模块，不依赖 aihtml 的任何其它模块。**扩展生成的代码不受此约束** —— 扩展把 `{{@ key}}` 编译成对自己运行时模块的调用正是设计意图，compiler 无权审查；这些模块由 compiler 在扩展产出表达式时自动收集进白名单。
4. 生成代码中所有静态文本必须是字面量，不得在运行期拼接构造。
5. 每个生成模块必须带 `-mustache_source/1` attribute，供 dev 热重载与 plugin 增量判断使用。
6. 模板正文与路径的规范表示是 UTF-8 binary（§7）。任何模块不得自带转换，一律走 `ai_mustache_text`。
