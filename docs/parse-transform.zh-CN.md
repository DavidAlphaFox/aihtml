# 使用 parse_transform

[English](parse-transform.md) · [中文](parse-transform.zh-CN.md) · [返回 README](../README.zh-CN.md)

`ai_mustache_transform` 是一个 parse_transform，承担三件互不相干的事。按模块启用：

```erlang
-module(my_views).
-compile({parse_transform, ai_mustache_transform}).
```

| 形态 | 写法 | 作用 | 不加 transform 时 |
|---|---|---|---|
| **(a)** 扩展 tag | `-mustache_tag(my_i18n).` | 声明并**校验**扩展模块 | 不做校验；只要在 `mustache_opts` 里登记过，扩展照常工作 |
| **(b)** 内联模板 | `ai_mustache:inline(~"Hi {{name}}!", Ctx)` | 编译期就地展开 | 输出相同，但每次调用都在运行期解析并解释 |
| **(c)** 文件模板 | `-mustache_template({index, "views/index.mustache"}).` | 把 `.mustache` 编译成本模块的 `index/1` 与 `index_iolist/1` | **直接失败** —— 函数根本不存在 |

只有 (c) 离不开 transform。(a) 和 (b) 会优雅降级，这是刻意的：漏写 `-compile` 那行不应该改变行为，只应该改变性能。

---

## 没有 `~mustache` 这种 sigil

Erlang 的 sigil 是**封闭集合** —— `~"..."`、`~b`、`~B`、`~s`、`~S` —— 未知的 sigil 是**词法错误**，`~mustache"..."` 根本到不了 parse_transform 这一层。所以形态 (b) 只能挂在一个普通的远程调用上，靠"第一个参数是 binary 字面量"来识别。

`~"Hello {{x}}"` 可以用：它是 OTP 27+ 的标准字符串 sigil，产出的就是普通 binary 字面量，和 `<<"Hello {{x}}">>` 完全一样。

---

## (a) 扩展 tag

自定义 marker 让你加上 mustache 本身没有的语法。最典型的是编译期 i18n：`{{@ hello}}`。

### 写一个扩展

```erlang
-module(my_i18n).
-behaviour(ai_mustache_ext).

-export([markers/0, compile_tag/4]).

markers() -> [$@].

table() ->
    #{hello => #{<<"en">> => <<"Hello">>, <<"fr">> => <<"Bonjour">>},
      bye   => #{<<"en">> => <<"Bye">>,   <<"fr">> => <<"Au revoir">>}}.

%% {{@ hello}} becomes:
%%   case ai_mustache_rt:lookup([locale], S) of
%%       <<"en">> -> <<"Hello">>;
%%       <<"fr">> -> <<"Bonjour">>;
%%       _        -> <<"hello">>
%%   end
compile_tag($@, [Key], _Body, Opts) ->
    A = ai_mustache_ext:anno(Opts),
    Table = ai_mustache_ext:ext_opt(?MODULE, Opts, table()),
    Locales = maps:get(Key, Table, #{}),
    Clauses = [{clause, A, [bin(A, L)], [], [bin(A, T)]}
               || {L, T} <- lists:sort(maps:to_list(Locales))]
        ++ [{clause, A, [{var, A, '_'}], [], [bin(A, atom_to_binary(Key, utf8))]}],
    {'case', A, ai_mustache_ext:lookup_expr([locale], Opts), Clauses}.

bin(A, <<>>) -> {bin, A, []};
bin(A, B)    -> {bin, A, [{bin_element, A, {string, A, binary_to_list(B)},
                           default, default}]}.
```

`compile_tag/4` 返回一个**抽象表达式**，求值结果必须是 `iodata()`。它在编译期执行，所以上面那张翻译表被烤进了生成的模块，渲染时不查任何表。

### 不要手写 `{var, 0, 'S'}`

模板被内联展开时，context stack 的变量会被改名，所以变量名必须从 `Opts` 里取。`ai_mustache_ext` 提供了构造函数，常见场景都不用碰 abstract forms：

| 辅助函数 | 给你什么 |
|---|---|
| `anno(Opts)` | 当前 tag 的 `erl_anno:anno()`，让报错指回模板行号 |
| `lookup_expr(Keys, Opts)` | `ai_mustache_rt:lookup(Keys, S)` |
| `escape_expr(Expr, Opts)` | `ai_mustache_rt:escape(Expr)` |
| `to_binary_expr(Expr, Opts)` | `ai_mustache_rt:to_binary(Expr)` |
| `stack_var(Opts)` / `indent_var(Opts)` | 需要裸变量名时用 |
| `stack_expr(Opts)` / `indent_expr(Opts)` | 上面两个的表达式形式 |
| `ext_opt(Module, Opts, Default)` | 从 `{ext_opts, #{...}}` 取你自己的配置 |

### 块级扩展

加上 `block_markers/0`，tag 就带 body，且 body 到手时**已经编译好**，是一个表达式列表：

```erlang
-module(my_wrap).
-behaviour(ai_mustache_ext).
-export([markers/0, block_markers/0, compile_tag/4]).

markers()       -> [$%].
block_markers() -> [$%].

%% {{%b}}text{{/b}}  ->  <b>text</b>
compile_tag($%, [Tag], Body, Opts) ->
    A = ai_mustache_ext:anno(Opts),
    T = atom_to_binary(Tag, utf8),
    mklist(A, [bin(A, <<"<", T/binary, ">">>)] ++ Body
              ++ [bin(A, <<"</", T/binary, ">">>)]).

mklist(A, [])      -> {nil, A};
mklist(A, [H | T]) -> {cons, A, H, mklist(A, T)}.

bin(A, B) -> {bin, A, [{bin_element, A, {string, A, binary_to_list(B)},
                        default, default}]}.
```

不导出 `block_markers/0` 时，该模块声明的所有 marker 都是行内的，`Body` 为 `[]`。

### 「声明」不等于「装配」—— 最容易误解的一点

```erlang
-mustache_tag(my_i18n).
```

这行只做**声明与校验**，不做装配。parse_transform 只看得见一个模块，所以 `my_views.erl` 里的声明，对 rebar3 plugin 怎么编译 `views/*.mustache` 毫无影响。真正的登记在 `rebar.config`：

```erlang
{mustache_opts, [{extensions, [my_i18n]},
                 {ext_opts,   #{my_i18n => #{default_locale => en}}}]}.
```

但仍然值得写这行声明 —— 它把一类错误从运行期提前成了声明它的那个模块的编译错误：

- 模块不存在，或没写 `-behaviour(ai_mustache_ext)`
- 没导出 `markers/0` 或 `compile_tag/4`
- marker 与内置的冲突（`# ^ / > ! = & { } + - *`）
- 两个扩展抢同一个字符
- `block_markers/0` 返回了 `markers/0` 里没有的字符

如果你声明的扩展没出现在 `mustache_opts` 里，还会得到一个警告 —— 这正是「扩展悄无声息地从未生效」最常见的成因。

在**同一个模块内**这行声明是真起作用的：形态 (b) 和 (c) 就在这里编译，看得见它。

### 扩展模块必须在使用它的模块编译时可加载

`compile_tag/4` 是在编译期被调用的，所以那时扩展模块必须能在编译器的 VM 里加载 —— 这和任何 parse_transform 模块的要求一样。加载不到就会在展开阶段报 `{unknown_marker, Char}`。

在 rebar3 里用这个来固定顺序：

```erlang
{erl_first_files, ["src/my_i18n.erl"]}.
```

校验比展开宽松一些：beam 还不存在时，`-mustache_tag` 会退而读扩展的 `.erl` 源码来检查 behaviour 和回调。这能在干净的树上抓住模块名写错、导出缺失这类问题，但它没法执行 `compile_tag/4` —— 那需要模块真的在那里。

`-mustache_tag({$@, my_i18n})` 也接受，但那个 marker 是多余的 —— `markers/0` 已经说了。`-mustache_tag({$@, {my_i18n, my_fun}})` 会告警：behaviour 把入口固定在 `compile_tag/4`，被调用的一定是它。

---

## (b) 内联模板

```erlang
greet(Name) ->
    ai_mustache:inline(~"Hello {{name}}!", #{name => Name}).
```

加了 transform，这行会变成模板构造出的那个 iolist，就地展开进 `greet/1`。运行期没有解析、没有解释、没有模块查找。

**它会降级。** 不加 transform，同样这行调用照常工作 —— 在运行期解析并渲染，产出完全相同的字节。测试里对这一点做了逐字节断言，因为一个和编译路径悄悄不一致的降级路径，比没有降级路径更糟。

有两件事不降级：

- **partial 被拒绝。** 内联模板里的 `{{> x}}`，加 transform 时是编译错误（`inline_partial_unsupported`），不加时抛 `partial_in_inline_template`。内联模板没有 views 目录可供解析，两条路径在这点上是一致的。
- **自定义 tag 必须有 transform。** `compile_tag/4` 返回的是抽象表达式，运行期毫无意义。内联模板里用了自定义 marker，解释路径会抛 `{unknown_marker, Char}`。

如果第一个参数不是字面量，调用原样保留，并给出警告：

```
the first argument of ai_mustache:inline/2 is not a binary literal, so it
cannot be expanded at compile time; this call will parse and interpret the
template on every invocation. Use <<"...">> or ~"..." to get the compiled
path, or add nowarn_mustache_inline to silence this.
```

模板确实是动态的时候，用 `-compile(nowarn_mustache_inline).` 按模块关掉它。

---

## (c) 文件模板

```erlang
-mustache_template({index, "views/index.mustache"}).
-mustache_template("views/greet.mustache").          %% 名字取自文件名
```

生成并导出：

```erlang
index/1         %% -> binary()
index_iolist/1  %% -> iolist()
greet/1
greet_iolist/1
```

路径解析顺序：模块所在目录 → `{mustache_opts, {views, ...}}` → 编译器的 `i` 包含路径。

两个模板会生成同名函数时是编译错误（`duplicate_template_name`），给其中一个显式命名即可。

### 什么时候用它而不用 plugin

rebar3 plugin 才是编译模板的常规方式 —— 一个模板一个模块、增量构建、孤儿清理、跨模板 partial 都有。形态 (c) 适合的是：你希望某个模板的渲染以**你自己模块的函数**形式存在，或者你压根没用 plugin。

### 已知限制

**rebar3 看不到你的模块依赖那个模板文件。** parse_transform 没有办法向 rebar3 注册额外的文件依赖，只有 `-include` 会被跟踪。所以改了 `views/index.mustache`，`my_views.erl` 不会自动重编。

plugin 补上了这一环：它扫描 `-mustache_template` attribute，模板 hash 变化时 touch 对应的 `.erl`。没有 plugin 的话，`rebar3 clean` 或手动 touch。形态 (a) 和 (b) 不受影响 —— 内联模板就住在 `.erl` 里。

形态 (c) 的模板里如果有 partial，需要 plugin，因为 partial 模块必须真实存在（`template_partial_needs_plugin`）。

---

## 三种形态一起用

```erlang
-module(tf_all).
-compile({parse_transform, ai_mustache_transform}).

-mustache_tag(my_i18n).
-mustache_template({index, "views/index.mustache"}).
-mustache_template("views/greet.mustache").

-export([hi/2]).

hi(Name, Locale) ->
    ai_mustache:inline(~"{{@hello}}, {{name}}!",
                       #{name => Name, locale => Locale}).
```

`hi/2` 用了 `my_i18n` 声明的 `{{@}}` marker，能解析是因为声明和内联模板在同一个模块里。

---

## 诊断信息

错误和警告走标准的 `{error, [{File, [{Line, Module, Reason}]}], []}` 通道并配 `format_error/1`，所以看起来和任何编译错误一样，编辑器可以直接跳转。内联模板报的行号是**模板内部**的行，不是调用起始的那一行。

| Reason | 含义 |
|---|---|
| `{bad_mustache_tag, Term}` | attribute 的参数既不是模块名也不是 `{Marker, Module}` |
| `{bad_mustache_template, Term}` | 既不是路径也不是 `{Name, Path}` |
| `{template_not_found, Path}` / `{template_unreadable, Path}` | 路径解析失败 |
| `{duplicate_template_name, Name}` | 两个模板生成同名函数 |
| `{template_name_clash, Name}` | 生成的函数名和你自己写的撞了 |
| `{inline_partial_unsupported, Name}` | 内联模板里出现 `{{> x}}` |
| `{template_partial_needs_plugin, Name}` | 形态 (c) 的模板里有 partial |
| `{unclosed_tag, Keys}` / `{mismatched_close, _, _}` / `{partial_not_found, _}` | 普通的模板语法错误 |
| `ext_not_assembled`（警告） | 声明了但 `mustache_opts` 里没有 |
| `{ext_callback_name_ignored, M, F}`（警告） | 写了 `{Marker, {Mod, Fun}}`，函数名会被忽略 |
| `inline_not_literal`（警告） | 见形态 (b) |

扩展校验失败由 `ai_mustache_ext` 报出，走同一通道：`not_an_ext_module`、`ext_missing_callback`、`marker_reserved`、`marker_conflict` 等。

---

## jinja 的 transform

`ai_jinja_transform` 是同样的三种形态，只是换了引擎；两个 transform 可以用在同一个模块上：

```erlang
-module(my_views).
-compile({parse_transform, ai_mustache_transform}).
-compile({parse_transform, ai_jinja_transform}).

-mustache_template({legacy, "views/legacy.mustache"}).
-jinja_template({page, "views/page.j2"}).
```

| 形态 | mustache | jinja |
|---|---|---|
| **(a)** 扩展 | `-mustache_tag(my_i18n).` | `-jinja_ext(my_filters).` |
| **(b)** 内联 | `ai_mustache:inline(~"...", Ctx)` | `ai_jinja:inline(~"...", Ctx)` |
| **(c)** 文件模板 | `-mustache_template({index, "..."}).` | `-jinja_template({page, "..."}).` |
| 配置块 | `mustache_opts` | `jinja_opts` |
| 关掉 (b) 的 warning | `nowarn_mustache_inline` | `nowarn_jinja_inline` |

两个 transform 互不认识对方的 attribute，也都不假设自己是第一个或最后一个。

jinja 的内联模板还额外不能带 `{% include %}`、`{% extends %}`、`{% import %}`、
`{% from %}`、`{% block %}`——没有 views 目录可解析——它的宏也不能互相调用，
因为展开时宏编译成匿名 fun，而绑定的 fun 引用不了自己、也引用不了在它之后绑定的。
**这两条限制在运行期路径上由同一个谓词同样地执行**，所以一份模板不会「加了 transform 是编译错误、
不加就静默渲染成空」。

语言本身见 [Jinja2 引擎](jinja.zh-CN.md)。
