# 06 · parse_transform 设计

> **决策 D1：三种形态全要** —— (a) 扩展点注册、(b) 内联模板、(c) 编译入口。

## 0. 前提修正：Erlang 不支持自定义 sigil

OTP 27 引入的 sigil 是**固定集合**（`~"..."`、`~b`、`~B`、`~s`、`~S`），编译器对未知 sigil 直接报词法错误。`~mustache"..."` **根本过不了词法**，parse_transform 拿不到 forms。

因此形态 (b) 的内联模板必须落在**合法 Erlang 语法**上 —— 采用「识别特定函数调用 + 字面量参数」的方案（见 §3）。

## 1. 统一入口

```erlang
-module(my_views).
-compile({parse_transform, ai_mustache_transform}).

%% (a) 扩展点注册
-mustache_tag({$@, my_i18n}).

%% (c) 编译入口
-mustache_template({index, "views/index.mustache"}).

%% (b) 内联模板
greet(Name) ->
    ai_mustache:inline(~"Hello {{name}}, you have {{count}} msgs.",
                       #{name => Name, count => 3}).
```

`ai_mustache_transform:parse_transform/2` 单遍扫描 forms：收集 attribute → 校验 → 生成/替换 → 返回新 forms。

---

## 2. 形态 (a)：扩展点注册

让用户**扩展 mustache 语法**，比如注册 `{{@ key}}` 做 i18n。

> `-mustache_tag` 的第二个元素是**模块名**，不是 `{Module, Function}`。入口函数由 behaviour 固定为 `compile_tag/4`；再让用户自选函数名只会多一条与 behaviour 对不上的岔路。

### 2.1 behaviour

```erlang
-module(ai_mustache_ext).

%% 本扩展模块接管哪些 marker 字符
-callback markers() -> [char()].

%% 编译期把一个扩展 tag 编译成 Erlang abstract expression
%%   Marker : 触发的字符，如 $@
%%   Keys   : tag 中的点分路径，如 [greeting, title]
%%   Body   : 块级扩展的 body 已编译 forms；行内扩展为 []
%%   Opts   : 编译选项，含 module / source / 用户自定义配置
%% 返回的表达式求值结果必须是 iodata()
-callback compile_tag(Marker :: char(), Keys :: [atom()],
                      Body :: [erl_parse:abstract_expr()],
                      Opts :: map()) -> erl_parse:abstract_expr().

%% 可选：声明该 marker 是块级（需要 {{/x}} 闭合）还是行内
-callback block_markers() -> [char()].
-optional_callbacks([block_markers/0]).
```

### 2.2 用户实现示例

```erlang
-module(my_i18n).
-behaviour(ai_mustache_ext).
-export([markers/0, compile_tag/4]).

markers() -> [$@].

%% {{@ hello}}  →  my_i18n_rt:t(hello, ai_mustache_rt:lookup([locale], S))
compile_tag($@, [Key], [], _Opts) ->
    L = 0,
    {call, L, {remote, L, {atom, L, my_i18n_rt}, {atom, L, t}},
     [{atom, L, Key},
      {call, L, {remote, L, {atom, L, ai_mustache_rt}, {atom, L, lookup}},
       [{cons, L, {atom, L, locale}, {nil, L}}, {var, L, 'S'}]}]}.
```

### 2.3 作用域约束与装配方式

**parse_transform 的作用域是单个模块的编译。** `my_i18n` 里的 `-mustache_tag` 声明，`view_index.erl` 编译时**看不见**。

所以职责这样切：

| 机制 | 职责 |
|---|---|
| `-mustache_tag` + parse_transform | **声明与校验**：编译期检查目标模块确实 `-behaviour(ai_mustache_ext)`、导出了 `markers/0` 与 `compile_tag/4`、且 `markers/0` 的返回值包含声明的字符，不匹配则报编译错误 |
| `{mustache_opts, [{extensions, [my_i18n]}]}` | **实际装配**：告诉 compiler 编译模板时启用哪些扩展 |

parse_transform 在这里的价值是**把配置错误从运行期提前到编译期**，而不是承担装配本身。这一点必须在 README 里讲清楚，否则用户会误以为只写 attribute 就能生效。

### 2.4 marker 冲突与保留字符

内置已占用：`# ^ / > ! = & { } + - *`。扩展只能注册这些之外的字符。plugin 在装配时做冲突检测：

- 扩展 marker 与内置冲突 → 编译错误
- 两个扩展模块注册同一 marker → 编译错误，列出冲突模块

---

## 3. 形态 (b)：内联模板

```erlang
greet(Name) ->
    ai_mustache:inline(~"Hello {{name}}!", #{name => Name}).
```

parse_transform 匹配 `ai_mustache:inline/2` 且**第一个参数是 binary 字面量**时，编译期把模板展开为原地 iolist 构造：

```erlang
greet(Name) ->
    erlang:iolist_to_binary(
      begin
          S__1 = [#{name => Name}],
          [<<"Hello ">>,
           ai_mustache_rt:escape(ai_mustache_rt:lookup([name], S__1)),
           <<"!">>]
      end).
```

### 3.1 优雅降级

`ai_mustache:inline/2` 本身有真实实现（运行期 scanner + parser + 解释求值）。所以：

- **加了 parse_transform** → 编译期展开，零运行时开销
- **没加** → 照常工作，只是慢

语义完全一致。这让「忘了加 `-compile({parse_transform, ...})`」不会变成一个难查的 bug。

> **降级路径不支持扩展 tag。** `compile_tag/4` 的产出是 abstract expression，只在编译期有意义；运行期解释器无法执行它。所以内联模板里用了自定义 marker 又没加 parse_transform 时，会得到 `{unknown_marker, Char}` 运行期错误而不是静默降级。这条例外必须写进 README。

### 3.2 第一参数非字面量时

`ai_mustache:inline(Tpl, Ctx)` 中 `Tpl` 是变量时，transform 原样保留调用，走运行期路径。可选地在 `warnings_as_errors` 下发一条 warning 提示无法优化。

### 3.3 内联模板不支持 partial

`{{> x}}` 在内联模板中报编译错误 —— 内联场景没有 views 目录上下文。需要 partial 就该用文件模板。

---

## 4. 形态 (c)：编译入口

```erlang
-mustache_template({index, "views/index.mustache"}).
%% 生成 index/1 和 index_iolist/1，并自动 export
```

路径解析顺序：模块所在目录 → `{mustache_opts, {views, ...}}` → 编译选项 `i` 路径。

### 4.1 已知成本：rebar3 认不出依赖

改 `views/index.mustache` 不会触发 `my_views.erl` 重编 —— parse_transform 无法向 rebar3 注册额外的文件依赖（只有 `-include` 会被跟踪）。

**兜底方案**：plugin 扫描所有 `-mustache_template` attribute 建反向依赖表，模板 hash 变化时 `touch` 对应 `.erl`。详见 [05 §6](05-rebar3-plugin.md#6--mustache_template-的-staleness-兜底)。

这是三种形态里唯一需要外部机制兜底的，也是它与 rebar3 plugin 功能重叠的部分。保留它的价值在于**不依赖 plugin 也能用**（比如纯 erlang.mk 或手写 Makefile 的项目）。

---

## 5. 实现骨架

```erlang
-module(ai_mustache_transform).
-export([parse_transform/2, format_error/1]).

parse_transform(Forms, Opts) ->
    St0 = #st{opts = Opts, file = file_of(Forms)},
    %% pass 1：收集 -mustache_tag / -mustache_template，校验扩展模块
    St1 = lists:foldl(fun collect/2, St0, Forms),
    %% pass 2：展开 ai_mustache:inline/2，剔除已消费的 attribute
    {Forms1, St2} = lists:mapfoldl(fun expand/2, St1, Forms),
    %% pass 3：追加 -mustache_template 生成的函数与 export
    inject(Forms1, St2).
```

错误通过标准 `{error, [{File, [{Line, ?MODULE, Reason}]}], []}` 返回，配合 `format_error/1` 输出可读诊断，与 erlc 一致。

## 6. 三形态对比

| | (a) 扩展点 | (b) 内联 | (c) 编译入口 |
|---|---|---|---|
| 触发 | `-mustache_tag` | `ai_mustache:inline/2` | `-mustache_template` |
| 主要价值 | 扩展语法 | 零开销内联 | 脱离 plugin 可用 |
| 依赖跟踪 | 无问题 | 无问题 | 需 plugin 兜底 |
| 无 transform 时 | 装配仍可用，只是丢失校验 | 降级为运行期解释（**但扩展 tag 不可用**，见下） | 不生成函数，编译报错 |
| 与 plugin 关系 | 互补（plugin 负责装配） | 正交 | 重叠 |
