# 05 · rebar3 plugin 设计

> **决策 D3：rebar3 为主构建；plugin 放本仓 `rebar3_aihtml/` 子目录，通过 `{subdir, ...}` 引用。**

## 1. 使用方式

使用方的 `rebar.config`：

```erlang
{deps, [{aihtml, {git, "https://github.com/DavidAlphaFox/aihtml.git", {tag, "v0.4.0"}}}]}.

{plugins, [
    {rebar3_aihtml, {git, "https://github.com/DavidAlphaFox/aihtml.git",
                     {tag, "v0.4.0"}}, {subdir, "rebar3_aihtml"}}
]}.

{provider_hooks, [{pre, [{compile, mustache}]}]}.

{mustache_opts, [
    {views,      "views"},        %% 模板根目录
    {suffix,     ".mustache"},
    {out_dir,    "_gen"},         %% 生成 .erl 的目录
    {prefix,     "view_"},        %% 模块名前缀
    {extensions, [my_app_tags]},  %% 用户扩展模块，见 06
    {ext_opts,   #{my_app_tags => #{default_locale => en}}},  %% 透传给扩展
    {line_map,   true},
    {warnings_as_errors, false}
]}.

{erl_opts, [{src_dirs, ["src", "_gen"]}]}.
```

`_gen/` 应加入 `.gitignore`。

> **为什么生成到 `_gen/` 而不是 `src/`**：保持用户 `src/` 干净、生成物不进版本库、`rebar3 clean` 语义清晰。选 `.erl` 而非直接 `compile:forms` 出 `.beam` 的理由：可读可 grep、dialyzer 能看见、堆栈行号有意义 —— 这也正是目标 2 的原话「编译成 erl 文件」。

## 2. Provider 注册

```erlang
-module(rebar3_aihtml).
-export([init/1]).

init(State) ->
    lists:foldl(fun(M, {ok, S}) -> M:init(S) end, {ok, State},
                [rebar3_aihtml_prv, rebar3_aihtml_migrate]).
```

```erlang
-module(rebar3_aihtml_prv).
-behaviour(provider).
-export([init/1, do/1, format_error/1]).

init(State) ->
    P = providers:create([
          {name,       mustache},
          {module,     ?MODULE},
          {namespace,  default},
          {bare,       true},
          {deps,       [app_discovery]},
          {example,    "rebar3 mustache"},
          {opts,       [{force, $f, "force", boolean, "忽略缓存，全量重新编译"}]},
          {short_desc, "Compile mustache templates to Erlang modules"},
          {desc,       "扫描 views 目录，将 .mustache 编译为 .erl 放入 out_dir"}]),
    {ok, rebar_state:add_provider(State, P)}.
```

## 3. 编译流程

```
do/1
 ├─ 对每个 app（rebar_state:project_apps/1）
 │   ├─ 读 {mustache_opts, ...}（app 级覆盖 project 级）
 │   ├─ 扫描 views/**/*.mustache
 │   ├─ 对每个模板：
 │   │    ├─ 计算 {mtime, hash}
 │   │    ├─ 读目标 _gen/view_x.erl 的 -mustache_source attr
 │   │    ├─ hash 相同 且 非 --force  → skip
 │   │    └─ 否则 scanner → parser → compiler:forms → erl_prettypr → 写文件
 │   ├─ 处理 -mustache_template attribute 的 staleness（见 §6）
 │   └─ 清理孤儿：_gen 中存在但源模板已删除的 .erl
 └─ 返回 {ok, State}
```

### 3.1 增量判断只看内容 hash

不用 mtime 做判断依据（git checkout、CI 缓存都会打乱 mtime），只用**模板内容 hash + 编译选项 hash + compiler 版本号**三者的组合 hash。读取方式是解析已生成 `.erl` 的 `-mustache_source` attribute，**不需要额外的 cache 文件** —— 生成物自描述。

hash **由 aihtml 侧的 `ai_mustache_compiler:source_hash/2` 计算，plugin 只负责调用**，不得自行实现（[04 §4.1](04-codegen.md#41-source-hash-的计算归属)）。`-mustache_source` 的 `opts` 字段同时供 dev 热重载（[T27](../tasks/T27.md)）复现编译，二者共用同一份口径。

### 3.2 partial 的增量粒度

因为 partial 编译成跨模块调用（见 [02 §2.2](02-architecture.md#22-partial-编译成跨模块函数调用)），**改 partial 不需要重编父模板**。plugin 不需要维护模板依赖图，这是编译成模块调用相对于内联展开的核心优势。

唯一例外：partial 从「存在」变为「不存在」时，父模板的编译应报错。由 `resolve_partials/2` 在编译期检查，且父模板未重编时错误不会被发现 —— 处置：plugin 在扫描阶段先建立一次全量 partial 名集合，对所有已生成模块的 `partials()` 做交叉校验，缺失时报错并强制重编该父模板。

## 4. 模块命名

```
views/index.mustache          →  view_index
views/shared/item.mustache    →  view_shared_item
views/layout/default.mustache →  view_layout_default
```

规则：去掉 `views/` 前缀与后缀，路径分隔符 `/` 与 `-` 归一为 `_`，加 `prefix`。

**冲突检测**：`shared/item.mustache` 与 `shared_item.mustache` 会撞名，plugin 必须在扫描后做一次全量重名检查并报错，而不是静默覆盖。

## 5. `rebar3 mustache migrate`

语义迁移辅助（见 [03 §7.2](03-semantics.md#72-rebar3-mustache-migrate)）：

```
rebar3 mustache migrate          # 只输出 diff
rebar3 mustache migrate --write  # 落盘
```

对每个 section body 内前缀匹配所在 section key 的引用剥掉前缀；无法确定的原样保留并在报告末尾列出，交人工处理。

## 6. `-mustache_template` 的 staleness 兜底

parse_transform 的「编译入口」形态（见 [06 §4](06-parse-transform.md#4-形态-c：编译入口)）有个 rebar3 认不出的依赖：改 `views/index.mustache` 不会触发 `my_views.erl` 重编。

处置：plugin 在 `do/1` 中扫描所有源文件的 `-mustache_template` attribute，建立 `.erl → .mustache` 反向依赖表；模板 hash 变化时 `touch` 对应 `.erl`，逼 rebar3 重编。

> 这是 D1 选择「三种形态全要」的已知成本。形态 (a) 与 (b) 没有这个问题。

## 7. erlang.mk 接入（降级支持）

```makefile
BUILD_DEPS = rebar3_aihtml
DEP_PLUGINS = rebar3_aihtml
dep_rebar3_aihtml = git https://github.com/DavidAlphaFox/aihtml.git v0.4.0
```

erlang.mk 支持 `DEP_PLUGINS` 加载 rebar3 plugin。本仓自身的 `Makefile` 保留但不再是主构建路径，CI 以 rebar3 为准。

## 8. 本仓自身的构建

`rebar.config`：

```erlang
{erl_opts, [debug_info, warn_export_vars, warn_shadow_vars, warn_obsolete_guard,
            warnings_as_errors]}.
{deps, []}.                                   %% 零依赖，见 02 §6
{profiles, [
    {test, [{deps, []},                       %% spec 用 OTP 自带的 json 模块解析
            {erl_opts, [nowarn_export_all, nowarn_missing_spec]},
            {plugins, [{rebar3_aihtml, {path, "rebar3_aihtml"}}]}]}
]}.
{dialyzer, [{warnings, [unknown, no_improper_lists]}]}.
{xref_checks, [undefined_function_calls, locals_not_used, deprecated_function_calls]}.
```

测试 profile 用 `{path, "rebar3_aihtml"}` 直接引本地 plugin 子目录，实现「同仓改动即时生效」—— 这正是 D3 选同仓的理由。
