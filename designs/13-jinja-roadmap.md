# 13 · Jinja2 实施路径

## 1. 原则

沿用 [07](07-roadmap.md) 的「先立测试基线，再动刀」，但有一个关键差异必须先讲清楚。

## 2. 没有官方 spec，基线要自己造

mustache 那条路走得稳，是因为官方 spec 的 136 个 JSON 用例先立住了。
**Jinja2 没有语言级 spec**——只有 CPython 实现和它的 pytest 套件。

方案：**从 CPython Jinja2 生成 fixture**。

```
tools/gen_jinja_fixtures.py     # 读 jinja_cases.py，用真的 jinja2 渲染，产出 JSON
test/jinja_spec/*.json          # {name, template, context, expected} 或 {name, template, error}
```

- 用例来源：手写覆盖表（按 [09](09-jinja-syntax.md) 的文法逐条构造）+ 从
  `pallets/jinja/tests/` 移植的边界用例。
- **期望输出由真的 CPython Jinja2 产生**，不由人手写。这是关键：
  [09 §3.2](09-jinja-syntax.md) 里 `**` 的结合性、filter 的绑定紧度这类细节，
  人的直觉靠不住，参照实现说了算。
- 生成脚本产出的 JSON **提交进仓库**，测试时不需要 Python。
  与 mustache 侧 vendoring spec JSON 的做法一致，保持零依赖。
- 已知偏离（[10 §9](10-jinja-semantics.md) 的 J1–J13）在 fixture 里标 `{"deviation": "J3"}`，
  测试 harness 对这些用例断言**我们的行为**而非参照实现的行为，
  并有一条元测试断言「差异清单里的每一条都至少有一个 fixture」。

工期上这一步约占总量的 10%，但没有它，后面 5000 行的回归网是空的。

## 3. 阶段划分

### 阶段 7 · 共享层抽取（T31–T33）

**目标**：把与模板语言无关的东西抽出来，且 mustache 的行为与产物字节**完全不变**。

- T31 `ai_html_forms` —— forms 构造器
- T32 `ai_html_text` / `ai_html_escape` / `ai_html_path` —— 文本、转义、路径
- T33 `ai_html_engine` behaviour + `ai_mustache_engine` 适配壳

**验收**：`rebar3 eunit && rebar3 ct && rebar3 xref && rebar3 dialyzer` 全绿；
且对同一模板，抽取前后生成的 `.erl` **逐字节相同**。

### 阶段 8 · 测试基线（T34）

- T34 Jinja fixture 生成器与 harness

**验收**：`rebar3 eunit` 退出码 0（未实现的用例挂起而非失败）；
fixture 数量与覆盖清单一致；差异清单元测试真跑。

### 阶段 9 · 词法与表达式（T35–T38）

- T35 `include/ai_jinja.hrl` —— 数据契约
- T36 `ai_jinja_scanner` —— 三种定界符、空白控制、raw
- T37 `ai_jinja_lexer` —— 表达式词法
- T38 `ai_jinja_expr` —— 表达式语法（优先级爬升）

**验收**：fixture 中所有「纯表达式」用例的 AST 断言通过；空白控制的组合矩阵全覆盖。

### 阶段 10 · 语句与 AST（T39–T40）

- T39 `ai_jinja_parser` —— 语句结构、配对校验
- T40 `ai_jinja_ast` —— 后处理 pass（文本合并、常量折叠、目标解析、依赖收集）

**验收**：所有语句形态的 AST 断言通过；配对错误的 8 类诊断各有用例。

### 阶段 11 · 运行时与内置库（T41–T44）

- T41 `ai_jinja_rt` —— 作用域、真值、运算、迭代、loop、call
- T42 autoescape 与 safe 传播
- T43 `ai_jinja_filters` —— 内置 filter 库
- T44 `ai_jinja_tests` + `ai_jinja_ext` —— 内置 test 与自定义注册

**验收**：filter/test 逐个有 fixture；safe 传播分类表每条有用例。

### 阶段 12 · 编译器（T45–T50）

- T45 `ai_jinja_compiler` 骨架与模块契约
- T46 表达式 codegen + 常量折叠
- T47 `if` / `for` / `set` / `with` / `filter` / `do` codegen
- T48 `extends` / `block` / `super` codegen
- T49 `macro` / `call` / `import` / `from` / `include` codegen
- T50 `compile_inline/3`

**验收**：fixture 全部点亮（差异清单除外）；生成模块通过 `check_remotes`；
`rebar3 dialyzer` 对生成代码全绿。

### 阶段 13 · 工具链（T51–T54）

- T51 plugin engine 参数化
- T52 `rebar3 jinja` provider + `jinja_opts` + 双引擎 gc/重名检测
- T53 `ai_jinja_transform`
- T54 `ai_jinja_dev` + `ai_jinja` 门面

**验收**：`examples/` 双引擎端到端跑通；同一 out_dir 反复构建文件数稳定；
改父模板不重编子模板可被观测验证。

### 阶段 14 · 收尾（T55）

- T55 文档、README、性能基线

## 4. 关键路径与可并行项

```
T31 ─ T32 ─ T33 ──────────────────────────┐
                                          │
T34（可与 T31-33 并行）                     │
  │                                       │
  └─ T35 ─ T36 ─ T37 ─ T38 ─ T39 ─ T40 ──┴─ T45 ─ T46 ─ T47 ─ T48 ─ T49 ─ T50 ─ T51 ─ T52 ─ T53 ─ T54 ─ T55
                                             │
T41 ─ T42 ─ T43 ─ T44（可与 T35-40 并行）─────┘
```

`ai_jinja_rt` 与内置库（T41–T44）不依赖 parser，可以与前端并行推进。
这是缩短工期最有效的一处。

## 5. 可裁剪项

工期紧张时按此顺序砍，每一项都能独立砍掉而不破坏其余部分：

| 顺序 | 砍什么 | 后果 |
|---|---|---|
| 1 | `namespace()` 赋值（[10 §2.5](10-jinja-semantics.md)） | `{% set ns.x = %}` 报编译错误 |
| 2 | `recursive` 循环 | `{% for ... recursive %}` 报编译错误 |
| 3 | `{% call %}` | 宏仍可用，只是不能带块体 |
| 4 | `groupby` / `selectattr` / `rejectattr` / `xmlattr` | 少 4 个 filter |
| 5 | `{% from ... import %}` | 保留 `{% import ... as %}` |

**不可裁剪**：`extends`/`block`/`super`（继承是选「接近全量」的核心理由）、
autoescape 与 safe 传播（安全相关）、fixture 基线（没它就没有正确性保证）。

## 6. 规模估计

| 部分 | 行数 |
|---|---|
| 共享层抽取（净新增） | ~250 |
| scanner + lexer + expr parser | ~1400 |
| 语句 parser + AST | ~800 |
| rt + filters + tests | ~1100 |
| compiler | ~1300 |
| transform + dev + 门面 | ~800 |
| plugin 参数化（改动） | ~400 |
| 测试与 fixture | ~1500 |
| **合计** | **~7500** |

与当前 mustache 全栈（src 约 3000 行 + plugin 约 1600 行 + 测试）同量级。

---

## 7. 实施结果

全部 25 个任务（T31–T55）完成。与计划的偏差，逐条：

| 计划 | 实际 | 原因 |
|---|---|---|
| 用例源用 `.yml` | 用 `.py`（`tools/jinja_cases.py`） | 100+ 条 filter 用例是从一张表循环展开的，YAML 里只能逐条抄 |
| `scoped` 一律按 scoped 处理，记一条差异 | 按参照实现实现，不记差异 | 是否 scoped 在**父模板**的 block 定义处就已知，调用点传 `globals(V)` 还是 `V` 即可，比预期简单 |
| 内置函数含 `cycler` / `joiner` / `lipsum` | 只有 `range` / `dict` / `namespace` | 前两个在参照实现里是有状态对象，与「运行期无状态」不相容；`loop.cycle` 覆盖其用法 |
| — | 新增差异 J14 / J15 / J16 / J17 | 实现中确认的四条：undefined 可链式、不支持嵌套解构、不支持 namespace 赋值、宏捕获模板级作用域 |
| — | 生成模块的 `opts` 从 map 改为有序列表 | **在 T52 的双引擎测试中发现**：map 的 `maps:to_list/1` 顺序跟随 VM 的 atom 表，同一份构建跑两次会产出不同字节。这条同时修掉了 mustache 侧的同一个潜在问题 |
| — | `source_hash/2` 加 `[deterministic]` | 同上，`term_to_binary` 对 map 的编码顺序也不是跨 VM 稳定的 |

### 规模

| 部分 | 计划 | 实际 |
|---|---|---|
| 共享层（净新增） | ~250 | ~430 |
| scanner + lexer + expr parser | ~1400 | ~1250 |
| 语句 parser + AST | ~800 | ~1080 |
| rt + filters + tests | ~1100 | ~1900 |
| compiler | ~1300 | ~1300 |
| transform + dev + 门面 | ~800 | ~900 |
| plugin 参数化（改动） | ~400 | ~350 |
| 测试与 fixture | ~1500 | ~2600 |
| **合计** | **~7500** | **~9800** |

### 验收状态

- `rebar3 eunit`：1719 个用例，0 失败（其中 520 条是 CPython 生成的一致性 fixture，全部点亮）
- `rebar3 ct`：35 个端到端用例，0 失败
- `rebar3 xref`、`rebar3 dialyzer`：全绿
- 生成器幂等：`tools/gen_jinja_fixtures.py --check` 通过
- `examples/run.sh`：两套引擎端到端跑通
- 继承的运行期开销实测 0.10 µs/render，且不随模板增长（[bench/README.md](../bench/README.md)）
