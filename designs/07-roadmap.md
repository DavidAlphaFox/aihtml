# 07 · 实施路径

## 原则

**先立测试基线，再动刀。** 现在是零测试（见 [01 §8](01-current-state.md#8-测试现状)），在没有回归网的情况下重写 parser 与执行模型风险过高。

但基线**不是**旧实现的行为快照：

1. 语义本就要换成标准 mustache（决策 D2），旧行为不是目标，固化它没有价值。
2. 旧实现依赖 ailib，而 ailib 已被彻底移除（决策 D5），旧代码在新构建下根本编译不过。

因此基线是 **mustache 官方 spec 测试集**，整体走 TDD：阶段 1 先把 spec harness 建起来（此时一致性用例挂起、inventory 真跑），阶段 2/3 逐步点亮。

测试框架用 **EUnit**：CT 的 testcase 名必须静态导出，无法从 spec 数据动态生成 136 个；EUnit 的 generator 天生适配数据驱动测试，每个 case 有独立标题与独立失败报告。CT 留给真正需要它的场景（T20 的 plugin 端到端测试需要起 rebar3 子进程）。

---

## 阶段 1 · 项目骨架与测试基础设施

**目标**：`rebar3 ct` 可跑；spec harness 就位；旧实现清出构建路径。

- [x] `rebar.config`（零依赖，见 [05 §8](05-rebar3-plugin.md#8-本仓自身的构建)）+ `src/aihtml.app.src`（library app，无 `mod` 项）
- [x] `.gitignore` 增补 `_gen/`；`Makefile` / `erlang.mk` 降级为非主路径
- [x] 旧实现移出构建路径，暂存于 `attic/`，阶段 6 删除：`ai_mustache` / `ai_mustache_loader` / `ai_mustache_parser` / `ai_mustache_runner` / `aihtml_app` / `aihtml_sup` / `ai_dom_node` / `ai_dom_render` / `examples/dom_render.erl`。其中 `ai_mustache_parser` 与 `ai_mustache_runner` 是阶段 2/3 的移植参考，必须一并保留
- [x] vendoring mustache 官方 spec 的 6 个必选模块 JSON 到 `test/spec/`（另存 3 个可选模块备查）
- [x] `test/ai_mustache_test_lib.erl`：spec 加载（`json:decode/1`，零依赖）、binary key → atom 转换、用例名归一与去重、阶段判定
- [x] `test/ai_mustache_spec_tests.erl`：spec 驱动，每个 case 一个具名测试；inventory 断言（12/14/42/22/12/34，总数 136）真跑
- [x] `test/ai_mustache_test_lib_tests.erl`：支撑库自测，全程真跑
- [x] `test/ai_mustache_ext_tests.erl`：aihtml 扩展语法（`{{+}}` / `{{-}}` / `{{*}}`）、section 分派表、falsy 集合、escape 字符集的用例表
- [x] `scripts/check.sh` 一键检查；dialyzer / xref 配置，`warnings_as_errors`

**验收**：`rebar3 eunit` 退出码为 0（未实现的一致性用例挂起而非失败，但 inventory 与用例表良构性真实通过）；`rebar3 xref`、`rebar3 dialyzer` 全绿。

---

## 阶段 2 · scanner / parser 重写

**目标**：产出带位置信息的 AST，修掉 parser 层所有 bug。

- [x] 从 `ai_mustache_parser` 拆出 `ai_mustache_scanner`（`split_tag/2`、`standalone/3`、delimiter 处理）
- [x] 新 AST（[04 §1](04-codegen.md#1-ast-设计)），带 `loc()`，保留 bbmustache MIT 版权头
- [x] AST 后处理三 pass（[04 §2](04-codegen.md#2-ast-后处理（修掉-p1p2p3）)），修掉 P1/P2/P3
- [x] 修 [B4](01-current-state.md#b4-·-带点的-partial-路径直接崩) 带点 partial 路径
- [x] 修 [B5](01-current-state.md#b5-·--隐式迭代取不到值) `{{.}}` 隐式迭代
- [x] `{ext, ...}` 节点与扩展 marker 的解析支持

**验收**：parser 单测覆盖所有 tag 类型 + 边界（未闭合、错配、嵌套 delimiter、standalone 各组合）。

---

## 阶段 3 · 编译器与运行时

**目标**：`ai_mustache_compiler` 可用，语义切换到标准 context stack。

- [x] `ai_mustache_rt`：`lookup/2`（栈回溯）、`escape/1`（单遍扫描，修 [B3](01-current-state.md#b3-·-html-escape-缺分号且极慢（在-ailib-中）)）、`section/4`、`lambda/2`、`truthy/1`
- [x] `ai_mustache_compiler:forms/2` + `compile_inline/3`
- [x] 语义切换（[03](03-semantics.md)）：context stack、falsy 定义、section 分派表、partial 缩进
- [x] 编译期优化（[04 §6](04-codegen.md#6-编译期优化)）
- [x] `to_binary/1` 的数值格式化对齐 spec（`float_to_binary(F, [short])`，见 [02 §6.2](02-architecture.md#62-数值格式化必须匹配-spec)）

**验收**：官方 spec 必选模块**全部通过**；known deviation 清单（[03 §8](03-semantics.md#8-与官方-spec-的差异清单)）与实际一致。

---

## 阶段 4 · rebar3 plugin

**目标**：`rebar3 compile` 自动把 `.mustache` 编译成 `.erl`。

- [x] `rebar3_aihtml/` 子项目骨架 + `rebar3_aihtml_prv`（[05](05-rebar3-plugin.md)）
- [x] 增量编译（内容 hash，读生成物的 `-mustache_source` attribute，无 cache 文件）
- [x] 模块命名与重名冲突检测
- [x] 孤儿 `.erl` 清理
- [x] partial 缺失的交叉校验
- [x] `rebar3 mustache migrate`（[03 §7.2](03-semantics.md#72-rebar3-mustache-migrate)）
- [x] plugin 自身的测试（用 `test/fixtures/` 下的小项目做端到端）

**验收**：`examples/` 用 plugin 全流程跑通；改 partial 不重编父模板可被观测验证。

---

## 阶段 5 · parse_transform

**目标**：三种形态全部可用。

- [x] `ai_mustache_ext` behaviour（[06 §2.1](06-parse-transform.md#21-behaviour)）
- [x] `ai_mustache_transform`：三 pass 骨架（[06 §5](06-parse-transform.md#5-实现骨架)）
- [x] 形态 (a)：`-mustache_tag` 声明与校验 + compiler 的 `{ext, ...}` 编译
- [x] 形态 (b)：`ai_mustache:inline/2` 展开 + 运行期降级实现
- [x] 形态 (c)：`-mustache_template` 函数生成 + plugin 侧 staleness 兜底（[05 §6](05-rebar3-plugin.md#6--mustache_template-的-staleness-兜底)）
- [x] marker 冲突检测

**验收**：`test/fixtures/` 中有一个用到全部三种形态的示例项目，编译并渲染正确；故意写错的扩展模块能在编译期报出可读错误。

---

## 阶段 6 · 清理与发布

- [x] 删除 `attic/`（[02 §4](02-architecture.md#4-删除清单（决策-d4--架构收敛）) 清单中的全部文件）
- [x] `ai_mustache_dev`：mtime/hash 热重载（读 `-mustache_source` attribute，无额外状态）
- [x] `examples/` 按新语义重写
- [x] README 重写：新架构、新语义、Incompatible Changes、`{{#}}` vs `{{+}}` 的区分、`0` 为 truthy 的说明、escape 字符集变更
- [x] 版本号 → **0.4.0**
- [x] 基准测试：与 v0.3.7 对比渲染吞吐（预期收益主要来自零拷贝 literal + 无 maps:merge + 单遍 escape）

**验收**：`sh scripts/check.sh` 全绿（compile / eunit / xref / dialyzer），`rebar3 ct` 全绿（plugin 端到端）；benchmark 数据写入 README。

---

## 风险与缓解

| 风险 | 缓解 |
|---|---|
| 语义切换破坏下游（aiwiki 等） | `migrate` 子命令 + 明确的 Incompatible Changes 公告 + 版本号跳 0.4.0 |
| 官方 spec 与 aihtml 扩展语法冲突 | 扩展用独立 marker，spec 测试集单独 suite，deviation 显式清单化 |
| 生成模块数量膨胀导致 atom 表增长 | 模块名来自文件系统而非用户输入，数量有界；文档中说明 |
| 形态 (c) 的依赖跟踪 | plugin `touch` 兜底；README 中标注为 plugin-recommended |
| 编译期错误信息难懂 | 统一 `{File, Line, Reason}` 格式（[04 §7](04-codegen.md#7-错误处理)），与 erlc 诊断格式一致 |

## 阶段依赖

```
1 ──▶ 2 ──▶ 3 ──┬──▶ 4 ──┐
                │        ├──▶ 6
                └──▶ 5 ──┘
```

阶段 4 与 5 都依赖阶段 3 的 compiler，彼此独立，可并行。
