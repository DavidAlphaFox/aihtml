# 01 · 现状盘点与问题清单

分析基线：commit `82c01eb`，8 个模块共 1038 行。

## 1. 模块清单

| 模块 | 行数 | 职责 |
|---|---|---|
| `ai_mustache.erl` | 31 | 门面：`render/2`、`bootstrap/0,1`、`reload/0` |
| `ai_mustache_loader.erl` | 342 | gen_server，持有 ets 表 `ai_mustache`，负责扫描目录、parse、缓存 |
| `ai_mustache_parser.erl` | 368 | 词法+语法分析，产出 IR。改自 [bbmustache](https://github.com/soranoba/bbmustache)（MIT） |
| `ai_mustache_runner.erl` | 107 | 栈式解释器，消费 IR 产出 binary |
| `aihtml_app.erl` | 13 | application 回调 |
| `aihtml_sup.erl` | 24 | supervisor，仅挂 `ai_mustache_loader` |
| `ai_dom_node.erl` | 98 | DOM 节点 record 与增删改查（**与 mustache 无关**） |
| `ai_dom_render.erl` | 55 | DOM 树 → HTML binary（**与 mustache 无关**） |

`ai_dom_*` 没有任何模块引用，只有 `examples/dom_render.erl` 使用。

## 2. 当前数据流

```
views/**/*.mustache
   │
   ├─ ai_mustache_loader:bootstrap/0,1     启动时全量扫描 recursive_dir/1
   │     └─ load/3 → ai_mustache_parser:parse/1
   │           └─ ets:insert  {c,Name} => IR
   │                          {t,Name} => [PartialName]
   │
   └─ ai_mustache:render/2
         ├─ erlang:get(Template)                    ← 进程字典二级缓存
         ├─ ai_mustache_loader:template/1
         │     ├─ ets:lookup {t,Name}               ← 深拷贝
         │     ├─ template_partial/1 递归 fold + maps:merge
         │     └─ ets:lookup {c,Name}               ← 深拷贝
         ├─ erlang:put(Template, Code)
         └─ ai_mustache_runner:render/2             ← 栈式解释，binary append
```

## 3. IR 指令集

parser 产出、runner 消费的中间表示：

```erlang
{binary, Bin}                       %% 静态文本
{tag, none,    Keys}                %% {{x}}      HTML 转义输出
{tag, raw,     Keys}                %% {{{x}}} {{&x}}  原样输出
{tag, partial, Name}                %% {{> path}}
{section, Keys, IR, true}           %% {{#x}}
{section, Keys, IR, false}          %% {{^x}}
{has, Keys, IR, true}               %% {{+x}}     非标准扩展
{has, Keys, IR, false}              %% {{-x}}     非标准扩展
{lambda, Keys}                      %% {{*x}}     非标准扩展
```

---

## 4. 架构层问题（重构的根本原因）

### 4.1 ets lookup 是深拷贝

`ai_mustache_loader.erl:38-51`。IR 树存在 ets 里，每次 `ets:lookup` 都会把整棵树从 ets 的独立内存区**拷贝**到调用进程堆上。模板越大、partial 越多，拷贝越贵。

> 这是去掉 ets 最强的技术论据：编译成 BEAM 模块后，静态文本与结构进入模块的 **literal pool**，跨进程按引用共享，零拷贝。

### 4.2 进程字典缓存架空了 ets

`ai_mustache.erl:19-26`：

```erlang
Run = case erlang:get(Template) of
          undefined -> Code = ai_mustache_loader:template(Template),
                       erlang:put(Template, Code), Code;
          Cache -> Cache
      end,
```

三个后果：

1. **`reload/0` 对已存在的长生命周期进程完全无效**。cowboy handler pool、gen_server 等一旦缓存过就永远拿旧 IR，只有进程重启才生效。这让开发期热重载事实上不可用。
2. **内存 N 倍放大**。每个 worker 进程各持一份完整 IR 拷贝。
3. ets 表上 `{read_concurrency, true}` 的优化基本白费 —— 真正的热路径是进程字典，ets 只是 cold path。

另外 process dict 的 key 直接用模板名（string/binary），与调用方自己的进程字典命名空间冲突。

### 4.3 状态耦合在单进程

`ai_mustache_loader.erl:98-105`：ets 表由 gen_server 创建且为 `protected`。loader 崩溃时：

- ets 表随之销毁（数据可 lazy load 恢复，尚可接受）
- **`bootstrap/1` 设置的 `view_path` / `suffix` 回退到 `init/1` 的默认值**（`CWD/views` + `.mustache`），行为静默改变

`aihtml_sup.erl` 用 `intensity => 1, period => 5`，容错窗口极窄。

### 4.4 首载路径重复计算

`ai_mustache_loader.erl:53-61` 的 `template_partial/1` 递归遍历所有 partial 并 `maps:merge`，每个模板首次渲染时都跑一遍，O(partials)。

### 4.5 模板名 `binary_to_atom`

`ai_mustache_loader.erl:35`：

```erlang
Name = erlang:binary_to_atom(ai_string:to_string(Template), utf8),
```

若模板名来自 HTTP 路由等外部输入，是 atom 表耗尽的 DoS 面。

---

## 5. 确实的 bug

### B1 · 多层 section 名丢失兄弟键（正确性）

`ai_mustache_runner.erl:105`：

```erlang
SectionCtx = maps:merge(Ctx, ai_maps:put(Name, H, #{})),
```

`{{#a.b}}` 时 `Name = [a,b]`，`ai_maps:put([a,b], H, #{})` 产出 `#{a => #{b => H}}`。`maps:merge` 是**浅合并**，于是 `Ctx` 中 `a` 下的所有其它键被整体顶掉。

单层 section 名（`[items]`）不受影响，所以现有 examples 掩盖了这个问题。

### B2 · `ai_dom_render:render/2` 必崩

`ai_dom_render.erl:107`：

```erlang
case ai_dom_node:opening(Tag) of      %% Tag 是 atom，不是 #ai_dom_node{}
```

`Tag` 来自 `ai_dom_node:tag(El)`，是 atom；`opening/1` 做 record 字段访问，必然 `badrecord`。同文件 `:147` 还有 `ture` 拼写错误（应为 `true`），导致布尔属性走错分支。

### B3 · HTML escape 缺分号且极慢（在 ailib 中）

`ailib/src/stdlib/ai_string.erl:12`：

```erlang
{"\\&", "\\&amp"},     %% 缺少结尾分号，产出 &amp 而非 &amp;
```

且 `html_escape/1` 对每个变量做 **8 遍 `re:replace` 全局扫描**，是渲染热路径上最贵的单点。

### B4 · 带点的 partial 路径直接崩

`ai_mustache_parser.erl:177`：

```erlang
parse_partial(State0, [Tag], NextBin0, Result0) ->     %% 只匹配单元素列表
```

`keys/1` 按 `.` 切分，`{{> layout.default}}` 产出 `[layout, default]` → `function_clause`。

### B5 · `{{.}}` 隐式迭代取不到值

`keys/1`（`ai_mustache_parser.erl:246-251`）对 `.` 产出 `['.']`，runner 走 `ai_maps:get(['.'], Ctx)` 去 map 里找键 `'.'`，永远取不到。标准 mustache 的隐式迭代器不可用。

---

## 6. 性能问题

### P1 · 后处理是 O(n²)

`merge_continuous_binary/1` 与 `remove_empty_section/1` 都用 `Acc ++ [I]` 累加（`ai_mustache_parser.erl:328,338,341,363,366`）。

### P2 · `merge_continuous_binary/1` 递归不完整

`ai_mustache_parser.erl:336-342` 只对 `{section, ...}` 递归下钻，**`{has, ...}` 内部的 IR 不做合并**，导致 `{{+x}}` 块内的连续静态文本在运行期被反复 binary append。

### P3 · `remove_empty_section/1` 含死代码且只处理顶层

`ai_mustache_parser.erl:355-368`：只遍历顶层不递归；其中 `[<<>>]` 分支永不成立 —— `?ADD` 宏保证空 binary 不会被插入 IR，且 IR 里是 `{binary, B}` 而非裸 binary。

### P4 · 运行期 binary append

`ai_mustache_runner.erl` 全程 `<<Acc/binary, X/binary>>`。ERTS 对尾部追加有优化，但 `run_section/7` 和 lambda 分支会重新起 `<<>>` 累加器，优化链被打断。输出应改为 **iolist**。

---

## 7. 语义特点（非 bug，但决定重构边界）

当前实现是**扁平 context + 全路径寻址**，不是标准 mustache 的 context stack。

`examples/shared/item.mustache`：

```mustache
{{+ items.current }}
  <li><strong>{{ items.name }}</strong></li>
{{/ items.current }}
```

section 内部引用写的是全路径 `items.name` 而非标准的 `name`。所有现存模板与下游项目（如 aiwiki）都依赖此语义。

处置见 [03-semantics.md](03-semantics.md)（决策 D2：改为标准语义 + 提供迁移工具）。

## 8. 测试现状

**零测试**。无 `test/` 目录、无 eunit、无 CT、未接入 mustache 官方 spec 测试集。

这是重构第一步必须先补齐的（见 [07-roadmap.md](07-roadmap.md) 阶段 1）。
