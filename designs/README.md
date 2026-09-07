# aihtml 全面重构设计文档

本目录记录 aihtml 从「运行期 ets 缓存 + 解释执行」重构为「编译期生成 Erlang 模块」的完整设计。

## 任务追踪

任务清单见仓库根目录 [`TASK.md`](../TASK.md)，每个任务的细节在 [`tasks/`](../tasks/) 下。

## 文档索引

| 文档 | 内容 |
|---|---|
| [01-current-state.md](01-current-state.md) | 现状盘点、数据流、问题清单（架构 / bug / 性能） |
| [02-architecture.md](02-architecture.md) | 目标架构、模块划分、去 ets 的技术论据 |
| [03-semantics.md](03-semantics.md) | 模板语义定稿（标准 context stack）与迁移方案 |
| [04-codegen.md](04-codegen.md) | AST 设计、编译器、生成代码形态 |
| [05-rebar3-plugin.md](05-rebar3-plugin.md) | rebar3 plugin provider 设计 |
| [06-parse-transform.md](06-parse-transform.md) | parse_transform 三形态与扩展 behaviour |
| [07-roadmap.md](07-roadmap.md) | 分阶段实施路径与验收标准 |

### Jinja2 引擎（v0.5）

| 文档 | 内容 |
|---|---|
| [08-jinja-architecture.md](08-jinja-architecture.md) | 双引擎架构、共享层边界、`ai_html_engine` behaviour、生成模块契约 |
| [09-jinja-syntax.md](09-jinja-syntax.md) | 定界符、空白控制、表达式文法与优先级、语句集三档划分 |
| [10-jinja-semantics.md](10-jinja-semantics.md) | 作用域、undefined、autoescape 与 safe 传播、真值、与 CPython 的差异清单 |
| [11-jinja-codegen.md](11-jinja-codegen.md) | Jinja AST、表达式与语句 codegen、模板继承的模块契约 |
| [12-jinja-toolchain.md](12-jinja-toolchain.md) | plugin 引擎参数化、`rebar3 jinja`、parse_transform |
| [13-jinja-roadmap.md](13-jinja-roadmap.md) | 阶段划分、关键路径、可裁剪项、规模估计 |

## 重构三大目标

1. **符合 Erlang 最佳实践，去掉 ets 缓存机制** —— 模板编译成 BEAM 模块，用 code server 替代 ets，运行期零进程零查表。
2. **提供 rebar3 plugin**，将 `.mustache` 文件直接编译成 `.erl` 文件。
3. **通过 parse_transform**，让用户扩展 mustache 的新功能。

## v0.5 的目标

4. **并列加入 Jinja2 引擎** —— 前端各写各的，后端共享；两套引擎可在同一项目、同一 `views`、同一 `out_dir` 下共存。详见 [08](08-jinja-architecture.md)。

## 已确认的关键决策

| # | 决策项 | 结论 |
|---|---|---|
| D1 | parse_transform 的角色 | **三种形态全要**：扩展点注册 + 内联模板 + 编译入口。详见 [06](06-parse-transform.md) |
| D2 | 兼容性策略 | **改成标准 mustache 语义**（context stack）。现有模板需迁移，提供 `rebar3 mustache migrate` 辅助。详见 [03](03-semantics.md) |
| D3 | 构建系统与仓库归属 | **rebar3 为主**，plugin 放本仓 `rebar3_aihtml/` 子目录，通过 `{subdir, ...}` 引用。erlang.mk 保留但降级 |
| D4 | `ai_dom_node` / `ai_dom_render` | **删除**。与 mustache 无关、无内部引用、且 `ai_dom_render:render/2` 有必崩 bug |
| D5 | 依赖 ailib | **彻底移除**。全部重新实现，运行期与测试期零依赖。详见 [02 §6](02-architecture.md#6-零依赖决策) |
| D6 | 旧实现的处置 | **不保留、不做行为固化**。语义本就要换（D2），旧行为不是目标；回归网改用 mustache 官方 spec，走 TDD。详见 [07](07-roadmap.md) |
| D7 | 是否加入 Jinja2 引擎 | **加入，与 mustache 并列而非替代**。前端各写各的，后端共享。详见 [08](08-jinja-architecture.md) |
| D8 | Jinja 的共享层边界 | 划在「与模板语言无关」这条线：forms 构造器、文本规范化、转义、路径解析、增量机制。**语义相关的一律不共享**（真值、作用域、转义策略）。详见 [08 §1](08-jinja-architecture.md) |
| D9 | 模板继承如何映射 | `render_with/3` + 运行期 `maps:merge` 的 block 覆盖表。**一个模块只认识直接父模板**，保住「改父模板不重编子模板」。详见 [11 §5](11-jinja-codegen.md) |
| D10 | `extends`/`include`/`import` 的目标 | **必须是字面量**。动态目标要么把 `binary_to_atom/2` 放进模板数据路径，要么回到运行期注册表，两者都与架构不变量冲突。详见 [11 §5.3](11-jinja-codegen.md) |
| D11 | Jinja 的测试基线 | Jinja2 没有官方 spec。**从 CPython Jinja2 生成 fixture 并提交进仓库**，测试期仍零依赖。详见 [13 §2](13-jinja-roadmap.md) |

## 环境基线

- OTP 28 / erts 16.4.0.3
- rebar3 3.27.0
- **零依赖**：不再依赖 ailib，运行期与测试期均只依赖 OTP。详见 [02 §6](02-architecture.md#6-零依赖决策)
