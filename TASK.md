# TASK

aihtml 全面重构任务清单。任务细节见 [`tasks/`](tasks/)，设计依据见 [`designs/`](designs/)。

## 阶段 1 · 项目骨架与测试基础设施

- [x] [T01](tasks/T01.md)
- [x] [T02](tasks/T02.md)
- [x] [T03](tasks/T03.md)
- [x] [T04](tasks/T04.md)
- [x] [T05](tasks/T05.md)

## 阶段 2 · scanner / parser 重写

- [x] [T06](tasks/T06.md)
- [x] [T07](tasks/T07.md)
- [x] [T08](tasks/T08.md)
- [x] [T09](tasks/T09.md)
- [x] [T10](tasks/T10.md)

## 阶段 3 · 编译器与运行时

- [x] [T11](tasks/T11.md)
- [x] [T12](tasks/T12.md)
- [x] [T13](tasks/T13.md)
- [x] [T14](tasks/T14.md)
- [x] [T15](tasks/T15.md)

## 阶段 4 · rebar3 plugin

- [x] [T16](tasks/T16.md)
- [x] [T17](tasks/T17.md)
- [x] [T18](tasks/T18.md)
- [x] [T19](tasks/T19.md)
- [x] [T20](tasks/T20.md)

## 阶段 5 · parse_transform

- [x] [T21](tasks/T21.md)
- [x] [T22](tasks/T22.md)
- [x] [T23](tasks/T23.md)
- [x] [T24](tasks/T24.md)
- [x] [T25](tasks/T25.md)

## 阶段 6 · 清理与发布

- [x] [T26](tasks/T26.md)
- [x] [T27](tasks/T27.md)
- [x] [T28](tasks/T28.md)
- [x] [T29](tasks/T29.md)
- [x] [T30](tasks/T30.md)

---

# TASK · Jinja2 引擎

在既有 mustache 引擎旁并列加入一套 Jinja2 引擎，共享后端、各写前端。
设计依据见 [`designs/08`](designs/08-jinja-architecture.md) — [`13`](designs/13-jinja-roadmap.md)。

## 阶段 7 · 共享层抽取

抽取期间 mustache 的行为与产物字节必须完全不变。

- [x] [T31](tasks/T31.md) `ai_html_forms`
- [x] [T32](tasks/T32.md) `ai_html_text` / `ai_html_escape` / `ai_html_path`
- [x] [T33](tasks/T33.md) `ai_html_engine` behaviour + mustache 适配壳

## 阶段 8 · 测试基线

- [x] [T34](tasks/T34.md) Jinja fixture 生成器与 harness

## 阶段 9 · 词法与表达式

- [x] [T35](tasks/T35.md) `include/ai_jinja.hrl` 数据契约
- [x] [T36](tasks/T36.md) `ai_jinja_scanner`
- [x] [T37](tasks/T37.md) `ai_jinja_lexer`
- [x] [T38](tasks/T38.md) `ai_jinja_expr`

## 阶段 10 · 语句与 AST

- [x] [T39](tasks/T39.md) `ai_jinja_parser`
- [x] [T40](tasks/T40.md) `ai_jinja_ast`

## 阶段 11 · 运行时与内置库

可与阶段 9/10 并行。

- [x] [T41](tasks/T41.md) `ai_jinja_rt`
- [x] [T42](tasks/T42.md) autoescape 与 safe 传播
- [x] [T43](tasks/T43.md) `ai_jinja_filters`
- [x] [T44](tasks/T44.md) `ai_jinja_tests` / `ai_jinja_ext`

## 阶段 12 · 编译器

- [x] [T45](tasks/T45.md) compiler 骨架与模块契约
- [x] [T46](tasks/T46.md) 表达式 codegen
- [x] [T47](tasks/T47.md) if / for / set / with / filter / do
- [x] [T48](tasks/T48.md) extends / block / super
- [x] [T49](tasks/T49.md) macro / call / import / from / include
- [x] [T50](tasks/T50.md) `compile_inline/3`

## 阶段 13 · 工具链

- [x] [T51](tasks/T51.md) plugin engine 参数化
- [x] [T52](tasks/T52.md) `rebar3 jinja` provider 与双引擎共存
- [x] [T53](tasks/T53.md) `ai_jinja_transform`
- [x] [T54](tasks/T54.md) `ai_jinja` 门面与 `ai_jinja_dev`

## 阶段 14 · 收尾

- [x] [T55](tasks/T55.md) 文档、示例与性能基线
