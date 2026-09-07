# 08 · 双引擎架构：把 Jinja2 接进来

## 0. 这份文档在回答什么

aihtml 现在是一个 mustache 引擎。要再加一套 Jinja2，问题不是「能不能写一个 Jinja parser」——
那是纯工作量；问题是**两套引擎如何共处**，才不会出现下面这三种结局：

1. 复制粘贴一份 `ai_mustache_*` 改名成 `ai_jinja_*`，从此每个 bug 修两遍；
2. 强行抽象成一个「通用模板引擎框架」，为了容纳两套差异极大的语义，抽象层比两个实现加起来还复杂；
3. plugin 与 parse_transform 各写一套，`rebar3 compile` 要挂两个 hook，增量判断、孤儿清理、命名冲突检测全部翻倍。

结论先写在这里：

> **前端各写各的，后端共享；共享的边界划在「与模板语言无关」这条线上，一寸都不越。**

## 1. 复用边界

把现有管线拆成两半看：

```
     ┌─────────── 前端：懂模板语言 ───────────┐ ┌──── 后端：只懂 Erlang forms ────┐
.mustache → scanner → parser → AST → compiler ─┼→ forms → erl_prettypr → .erl → .beam
     .j2  → scanner → parser → AST → compiler ─┘         └ parse_transform 直接注入
                                                │
                                                └── stamp / attribute / 增量 / 孤儿清理
```

竖线右边的东西，**没有一个字节知道 mustache 或 Jinja 的存在**。这就是共享层的边界。

### 1.1 共享层（新增）

| 模块 | 内容 | 来源 |
|---|---|---|
| `ai_html_forms` | Erlang abstract forms 构造器：`a/2` `v/2` `bin/2` `mklist/2` `fn/4` `spec/4` `param/3` `uses_var/2` `remotes/2` `anno/1` | 从 `ai_mustache_compiler` 私有函数抽出 |
| `ai_html_text` | UTF-8 规范化：`template/1` `path/1` `opts/1` `source/1` `to_list/1` | 现 `ai_mustache_text` 改名，保留 `ai_mustache_text` 作转发壳 |
| `ai_html_escape` | `escape/1`（单遍扫描）· `to_binary/1`（含 `float_to_binary(F,[short])`） | 从 `ai_mustache_rt` 抽出 |
| `ai_html_engine` | behaviour：`forms/2` · `source_hash/2` · `module_name/2` · `suffix/0` · `attribute/0` · `parse/2` | 新写 |
| `ai_html_path` | 模板路径解析与 `.erl` 静态扫描 | 现 `ai_mustache_path` 泛化（属性名参数化） |

`ai_html_forms` 是这次抽取里最有价值的一块。它包含两个**已经踩过坑、且下一个引擎必然会再踩一次**的实现细节：

- `{string, _, Chars}` 的 bin_element 必须带 **default 类型**而非 `[binary]`；
- 非 ASCII 静态文本必须以 `[utf8]` 类型的字符发出，不能是裸字节，否则经 `erl_prettypr` → 文件 → `erlc` 往返后每个多字节字符都会被截断。

这两条写在 `ai_mustache_compiler:bin/2` 的注释里。抽成共享模块后，Jinja 引擎自动继承，而不是等着被同一个坑咬第二次。

### 1.2 各自实现（不共享）

| 关注点 | mustache | Jinja2 | 能否共享 |
|---|---|---|---|
| tag 内容 | key 路径 `[atom()]` | 完整表达式文法 | **否** |
| 变量解析 | context stack 回溯 | 词法作用域 + 根 context | **否**（见 [10 §2](10-jinja-semantics.md)） |
| 空白处理 | standalone line 规则 | `{%-`/`-%}` + `trim_blocks`/`lstrip_blocks` | **否** |
| 块结构 | `{{#x}}...{{/x}}` 单一形态 | 15+ 种语句，各有配对规则 | **否** |
| 复用机制 | partial = 跨模块调用 | include + extends/block + macro/import | 部分（见 §3） |
| 转义 | 默认转义，`{{{x}}}` 关闭 | autoescape 策略 + `\|safe` 传播 | **否** |

前端不共享是**结论而非妥协**。Jinja2 的 `{{ users\|selectattr("active")\|map(attribute="name")\|join(", ") }}`
与 mustache 的 `{{name}}` 之间没有可抽象的公共结构，硬抽只会得到一个空接口。

## 2. `ai_html_engine` behaviour

plugin 与 dev 模块面向它编程，从此不认识具体引擎：

```erlang
-module(ai_html_engine).

%% 模板正文 → AST。AST 的具体形状是引擎私有的，调用方只传递不解读。
-callback parse(Body :: binary(), Opts :: map()) -> {ok, term()} | {error, term()}.

%% AST → 完整模块 forms，外加该模板依赖的其它生成模块。
-callback forms(Ast :: term(), Opts :: map()) ->
    {ok, [erl_parse:abstract_form()], Deps :: [module()]} | {error, term()}.

%% 构建戳：正文 + 归一化选项 + 生成代码版本号 的合并摘要。
-callback source_hash(Body :: binary(), Opts :: map()) -> binary().

%% 模板名 → 生成模块名。
-callback module_name(Name :: binary(), Opts :: map()) -> module().

%% 该引擎写进生成模块的自描述 attribute 名。
%% mustache -> mustache_source；jinja -> jinja_source。
-callback attribute() -> atom().

%% 默认模板后缀与默认模块前缀。
-callback default_suffix() -> string().
-callback default_prefix() -> binary().
```

`ai_mustache_compiler` 已经导出了 `forms/2`、`source_hash/2`、`normalize_opts/1`，
`ai_mustache_ast` 已经导出 `module_name/2`。因此 mustache 侧的适配只需要一个薄壳
`ai_mustache_engine`，不动核心模块的任何逻辑。

**为什么 `attribute()` 是 callback 而不是常量 `template_source`。**
把 mustache 的 `-mustache_source` 改名会让所有已构建产物一次性失效、
`ai_mustache_dev` 与旧 `.erl` 全部对不上，收益却只是少一个 callback。
两个引擎生成的模块本就属于不同族群（不同前缀、不同目录、不同 render 契约），
让它们的自描述 attribute 也各自独立是更小的耦合。

## 3. 生成模块契约：Jinja 侧的扩展

mustache 生成模块导出 `render/1` · `render_iolist/1` · `render_stack/1,2` · `partials/0`。
Jinja 需要更多，因为它有模板继承。完整契约见 [11 §2](11-jinja-codegen.md)，这里只说架构后果：

```erlang
%% 叶子入口
render(Ctx)                -> binary().
render_iolist(Ctx)         -> iolist().
%% 被 include 时的入口（V 是作用域，I 是缩进）
render_scope(V, I)         -> iolist().
%% 被继承链上层调用的入口，B 是 block 覆盖表
render_with(V, I, B)       -> iolist().
%% 本模板自己定义的 block
blocks()                   -> #{atom() => fun((V, I, B) -> iolist())}.
%% 沿继承链合并后的 block 表
all_blocks()               -> #{atom() => fun()}.
%% 依赖的其它生成模块（include / extends / import 的目标）
partials()                 -> [module()].
```

`partials/0` **名字保持不变**，即使 Jinja 里它包含的是 include/extends/import 目标而非 partial。
理由是 `rebar3_aihtml_scan:partials_of/1` 与孤儿清理、反向依赖表都读这个函数；
为了措辞准确而改名，代价是 plugin 里又多一个引擎分支，不值得。

## 4. 目录结构

```
aihtml/
├── src/
│   ├── ai_html_forms.erl          # 新 · 共享 forms 构造器
│   ├── ai_html_text.erl           # 新 · 现 ai_mustache_text 迁入
│   ├── ai_html_escape.erl         # 新 · 现 ai_mustache_rt 的 escape/to_binary 迁入
│   ├── ai_html_engine.erl         # 新 · behaviour 定义
│   ├── ai_html_path.erl           # 新 · 现 ai_mustache_path 泛化
│   │
│   ├── ai_mustache*.erl           # 既有 · 只做「改为调用共享层」的机械替换
│   ├── ai_mustache_engine.erl     # 新 · behaviour 适配壳
│   │
│   ├── ai_jinja.erl               # 门面
│   ├── ai_jinja_scanner.erl       # 词法：三种定界符 + 空白控制 + raw
│   ├── ai_jinja_lexer.erl         # 表达式词法
│   ├── ai_jinja_expr.erl          # 表达式语法（优先级爬升）
│   ├── ai_jinja_parser.erl        # 语句结构 → AST
│   ├── ai_jinja_ast.erl           # AST 后处理 pass
│   ├── ai_jinja_compiler.erl      # AST → forms          核心
│   ├── ai_jinja_engine.erl        # behaviour 适配壳
│   ├── ai_jinja_rt.erl            # 生成代码的运行期依赖
│   ├── ai_jinja_filters.erl       # 内置 filter 库
│   ├── ai_jinja_tests.erl         # 内置 test 库
│   ├── ai_jinja_ext.erl           # 自定义 filter/test/tag 的 behaviour
│   ├── ai_jinja_transform.erl     # parse_transform
│   └── ai_jinja_dev.erl           # 开发期热重载
├── include/
│   ├── ai_mustache.hrl            # 既有
│   ├── ai_html.hrl                # 新 · 两引擎共用的类型（loc、path、error 外壳）
│   └── ai_jinja.hrl               # 新 · Jinja 的 token / AST / opts / error
└── rebar3_aihtml/
    └── src/
        ├── rebar3_aihtml_prv.erl      # 参数化 engine
        ├── rebar3_aihtml_jinja.erl    # 新 · `rebar3 jinja` provider
        └── ...                        # scan/name/emit/gc/stale/check 全部参数化
```

## 5. 架构不变量（Jinja 侧）

沿用 [02 §8](02-architecture.md#8-架构不变量) 的全部六条，并追加四条：

7. **`ai_jinja_compiler` 是唯一的 Jinja AST → forms 实现。** plugin 与 parse_transform 都不得旁路它。
8. **生成模块只依赖 `ai_jinja_rt`、`ai_jinja_filters`、`ai_jinja_tests`、其它生成模块，以及用户显式注册的 filter/test 模块。**
   注册模块由 compiler 在解析 filter 名时收集进白名单，`check_remotes/3` 照常执行。
9. **模板继承在编译期只解析到直接父模板。** 一个模块的 forms 里不得出现祖父模板的模块名，
   否则「改父模板不重编子模板」的增量粒度就破了（[02 §2.2](02-architecture.md)）。
10. **共享层模块不得引用任何 `ai_mustache_*` 或 `ai_jinja_*` 模块。** 依赖方向单向向下，
    这是「共享层真的与语言无关」的可检查形式（`rebar3 xref` 可验证）。

## 6. 迁移风险与缓解

抽取共享层要改动已完成并通过全部测试的 mustache 代码，这是本方案唯一的回归风险来源。

| 风险 | 缓解 |
|---|---|
| `ai_mustache_compiler` 抽走私有函数后行为漂移 | 抽取是**纯机械替换**：函数体逐字搬运，只改模块名。抽取提交里不得夹带任何逻辑改动，`rebar3 eunit && rebar3 ct` 必须在抽取前后产出相同结果 |
| `ai_mustache_text` 改名破坏外部调用方 | 保留 `ai_mustache_text` 作为转发壳并标 `-deprecated`，v0.6 再删 |
| 生成产物字节变化触发全量重编 | 抽取不改 `?AI_MUSTACHE_VSN`、不改 `normalize_opts/1`，因此 stamp 不变，已构建产物继续有效。**这一条要在验收里实测**：抽取前后对同一模板生成的 `.erl` 应逐字节相同 |

## 7. 与 mustache 的关系：并列，不是替代

两套引擎在同一个项目里可以共存，各自扫描各自的目录：

```erlang
{provider_hooks, [{pre, [{compile, mustache}, {compile, jinja}]}]}.

{mustache_opts, [{views, "views/mustache"}, {prefix, "view_"}]}.
{jinja_opts,    [{views, "views/jinja"},    {prefix, "j2_"}, {suffix, ".j2"}]}.
```

生成模块前缀不同即可避免命名冲突；`rebar3_aihtml_check` 的重名检测本就在**全项目范围**内做，
两个引擎的模块名会一起进入同一张表，冲突会被正常报出来。
