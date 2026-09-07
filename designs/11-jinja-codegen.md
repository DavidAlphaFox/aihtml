# 11 · Jinja2 代码生成

`ai_jinja_compiler` 的规格。前置阅读：[04](04-codegen.md)（mustache 的对应文档，
form 构造、indent 处理、常量折叠的手法完全沿用）与 [10 §2](10-jinja-semantics.md)（作用域）。

## 1. 与 mustache codegen 的三处结构差异

| | mustache | Jinja |
|---|---|---|
| body 的编译结果 | `[Expr]`（扁平 iolist） | `{[Expr], ScopeVar}`（作用域串行穿引） |
| 复用的调用形态 | 一种：`Mod:render_stack(S, I)` | 四种：include / extends / block / macro |
| 分派 | section 分派表编译期展开 | 表达式树逐节点编译 |

第一条是最深的：**Jinja 的 body 编译是一个 fold，不是一个 map**。
每个节点吃进当前作用域变量名，吐出表达式列表与新的作用域变量名。

```erlang
-spec body_exprs([jinja_node()], var(), state()) ->
        {[erl_parse:abstract_expr()], var(), state()}.
```

`var()` 是形如 `'V0'` / `'V1'` 的原子。只有 `set` / `set_block` 会推进它；
其余节点原样返回传入的那个，因此绝大多数模板生成的代码里根本看不到编号。

## 2. 生成模块契约

以 `views/page.j2` → `j2_page` 为例：

```erlang
-module(j2_page).
-jinja_source(#{path => <<"views/page.j2">>, stamp => <<...>>,
                mtime => 1757..., vsn => 1, opts => #{...}}).

-export([render/1, render_iolist/1, render_scope/2,
         render_with/3, blocks/0, all_blocks/0, partials/0]).

render(Ctx)        -> erlang:iolist_to_binary(render_iolist(Ctx)).
render_iolist(Ctx) -> render_with([Ctx], <<>>, all_blocks()).

%% 被 {% include %} 调用
render_scope(V, I) -> render_with(V, I, all_blocks()).

%% 继承链入口。B 是 block 覆盖表，由继承链最下游的模块构造后一路传上来。
render_with(V, I, B) -> [ ... ].

%% 本模板自己定义的 block
blocks() -> #{title => fun ?MODULE:block_title/3,
              body  => fun ?MODULE:block_body/3}.

%% 沿继承链合并；无父模板时就是 blocks/0
all_blocks() -> blocks().

block_title(V, I, B) -> [ ... ].
block_body(V, I, B)  -> [ ... ].

macro_input(Args, V) -> [ ... ].     % {% macro %} 生成的

partials() -> [j2_base, j2_widgets].
```

`V` 是作用域链（`[map()]`），`I` 是缩进（沿用 mustache 的做法，
`{% include %}` 需要它），`B` 是 block 覆盖表。

### 2.1 为什么是 `render_with/3` 而不是把 block 表编进模块

因为不变量 9（[08 §5](08-jinja-architecture.md)）：一个模块的 forms 里只能出现直接父模板的名字。
如果 block 表在编译期展平成字面量，那么改动祖父模板的 block 集合就必须重编整条继承链，
增量粒度立刻退化，而这正是整个架构最大的卖点之一。

代价是每次渲染一次 `maps:merge/2`（小 map，微秒级），只在**继承链的入口**发生一次。

## 3. 表达式 AST 与编译

### 3.1 表达式 AST

```erlang
-type expr() ::
      {lit,    loc(), term()}                           % 字面量（含 list/map/tuple 字面量）
    | {name,   loc(), atom()}                           % 名字
    | {attr,   loc(), expr(), atom()}                   % a.b
    | {sub,    loc(), expr(), expr()}                   % a[b]
    | {slice,  loc(), expr(), expr()|undefined,
                             expr()|undefined, expr()|undefined}
    | {binop,  loc(), op(), expr(), expr()}
    | {unop,   loc(), '-' | '+' | 'not', expr()}
    | {'and',  loc(), expr(), expr()}                   % 短路，与 binop 分开
    | {'or',   loc(), expr(), expr()}
    | {cond,   loc(), Cond :: expr(), Then :: expr(), Else :: expr() | undefined}
    | {filter, loc(), atom(), expr(), args()}
    | {test,   loc(), atom(), expr(), args(), Negated :: boolean()}
    | {call,   loc(), expr(), args()}
    | {tuple,  loc(), [expr()]}
    | {list,   loc(), [expr()]}
    | {map,    loc(), [{expr(), expr()}]}.

-type args() :: {[expr()], [{atom(), expr()}]}.   % 位置参数 · 关键字参数
```

### 3.2 编译规则

| 节点 | 生成 |
|---|---|
| `{lit, _, V}` | `erl_parse:abstract(V)`，直接进 literal pool |
| `{name, _, N}` | `ai_jinja_rt:resolve(N, V)` |
| `{attr, _, E, K}` | `ai_jinja_rt:attr(E', K)` |
| `{sub, _, E, I}` | `ai_jinja_rt:subscript(E', I')` |
| `{binop, _, Op, A, B}` | `ai_jinja_rt:Op(A', B')`，**不是** Erlang 原生运算符（见 §3.3） |
| `{'and', _, A, B}` | `case truthy(A') of false -> A''; true -> B' end`，A 求值一次 |
| `{'or', _, A, B}` | 对偶 |
| `{cond, _, C, T, E}` | `case truthy(C') of true -> T'; false -> E' end` |
| `{filter, _, F, E, Args}` | `Mod:F(E', ArgsMap)`，Mod 编译期确定 |
| `{test, _, T, E, Args, Neg}` | `ai_jinja_tests:T(E', ArgsMap)`，取反时包 `not` |
| `{call, _, F, Args}` | 见 §3.4 |

### 3.3 算术不能直接用 Erlang 运算符

`{{ a + b }}` 不能编译成 `A + B`。三个原因：

1. **`+` 在 Jinja 里对字符串是拼接**（Python 语义），Erlang 的 `+` 会 badarith；
2. **undefined 参与运算要给出可读错误**，`badarith` 定位不到模板行；
3. **`/` 是真除**（`1/2` = `0.5`），Erlang 的 `/` 恰好一致，但 `//` 是 floor 除、
   `%` 是取模（对负数与 `rem` 不同），都要自己实现。

所以全部走 `ai_jinja_rt:add/2` `sub/2` `mul/2` `divide/2` `floordiv/2` `mod/2`
`pow/2` `concat/2` `eq/2` `ne/2` `lt/2` `le/2` `gt/2` `ge/2` `contains/2`。

**编译期常量折叠**：两侧都是 `{lit, _, _}` 时直接算出结果，避免为
`{{ 1 + 2 }}` 生成一次远程调用。折叠必须走与运行期同一份实现
（`ai_jinja_rt:add/2` 在编译期直接调用），否则两条路径会漂移。

### 3.4 调用

`{{ f(1, x=2) }}` 的 `f` 有三种来源，编译期就能分辨：

| f 的形态 | 生成 |
|---|---|
| `{% import %}` 引入的宏 | 直接跨模块调用 `j2_m:macro_f(#{...}, V)` |
| 本模板 `{% macro %}` 定义的 | 本地调用 `macro_f(#{...}, V)` |
| 内置函数（`range` `dict` `namespace`） | `ai_jinja_rt:builtin(N, Pos, Kw)` |
| 其它（作用域里的 `fun`） | `ai_jinja_rt:call(F', Pos, Kw)` |

前三种是静态调用，零开销。第四种是唯一需要运行期派发的情况。

## 4. 语句编译

### 4.1 `if`

```erlang
%% {% if a %}X{% elif b %}Y{% else %}Z{% endif %}
{Res, V1} = case ai_jinja_rt:truthy(<a>) of
                true  -> {[<X>], <VX>};
                false ->
                    case ai_jinja_rt:truthy(<b>) of
                        true  -> {[<Y>], <VY>};
                        false -> {[<Z>], <VZ>}
                    end
            end
```

分支里没有 `set` 时退化为不带元组的普通 `case`，这是绝大多数情况。
是否需要元组由「任一分支的 body 推进了作用域变量」决定，编译期可知。

### 4.2 `for`

无 `loop` 引用、无 `else`、无 namespace 累加时——最常见的情况——编译成 list comprehension：

```erlang
%% {% for x in xs %}...{% endfor %}
[ begin V1 = ai_jinja_rt:bind(V, x, X__), [...] end
  || X__ <- ai_jinja_rt:to_list(<xs>) ]
```

需要 `loop` 时改为带索引的 `lists:foldl/3`，`loop` 作为一个 map 绑进作用域：

```erlang
{_, Acc} = lists:foldl(
    fun(X__, {N__, A__}) ->
        L__ = ai_jinja_rt:loop(N__, Len__, X__, Items__),
        V1 = ai_jinja_rt:bind2(V, x, X__, loop, L__),
        {N__ + 1, [[...] | A__]}
    end, {1, []}, Items__),
lists:reverse(Acc)
```

`{% for %}...{% else %}` 的 else 分支在序列为空时执行，编译成外层一个 `case Items__ of [] -> ... end`。

`recursive` 循环：body 里可以调用 `loop(children)`。编译成一个**命名 fun**
（`fun Rec__/2`），`loop/1` 在 body 内被识别为对它的递归调用。
`loop.depth` 由该 fun 的第二个参数携带。

### 4.3 `set` / `with` / `filter`

```erlang
%% {% set a = E %}          →  V1 = ai_jinja_rt:bind(V0, a, <E>)
%% {% set a %}B{% endset %} →  V1 = ai_jinja_rt:bind(V0, a, {safe, [<B>]})
%% {% with a = E %}B{% endwith %}
%%     →  begin V1 = ai_jinja_rt:push(V0, #{a => <E>}), [<B>] end
%% {% filter f %}B{% endfilter %}
%%     →  ai_jinja_filters:f({safe, [<B>]}, #{})
```

`set` 的块形式与 `filter` 块都把内容包成 `{safe, _}`——内容已经过转义，
再转一次就是 `&amp;amp;`（[10 §4.3](10-jinja-semantics.md)）。

### 4.4 `include`

```erlang
%% {% include "widgets/box.j2" %}
j2_widgets_box:render_scope(V, I)

%% {% include "x.j2" ignore missing %}
case erlang:function_exported(j2_x, render_scope, 2) of
    true  -> j2_x:render_scope(V, I);
    false -> []
end

%% {% include "x.j2" without context %}
j2_x:render_scope([RootOf(V)], I)
```

`ignore missing` 之所以要在运行期检查而不是编译期：模板可能确实不存在，
这时编译期就该报错；`ignore missing` 的语义正是「允许它不存在」，
所以编译期跳过存在性检查，运行期用 `function_exported/3` 兜底
（比 `code:is_loaded/1` 可靠，且不触发加载）。

## 5. 继承

### 5.1 `extends`

```erlang
%% 子模板 j2_page：{% extends "base.j2" %}
all_blocks() -> maps:merge(j2_base:all_blocks(), blocks()).
render_with(V, I, B) -> j2_base:render_with(V, I, B).
```

**`render_with/3` 完全把控制权交给父模板**，子模板自己的顶层文本被丢弃——
这正是 Jinja2 的语义（extends 之外的顶层内容不输出）。
parser 检测到 extends 时，对同层的非 `block`/`set`/`macro`/`import` 节点发 warning。

### 5.2 `block` 与 `super()`

```erlang
%% 父模板 j2_base 中 {% block title %}默认{% endblock %} 的调用点：
(maps:get(title, B, fun ?MODULE:block_title/3))(V, I, B)

%% 子模板 j2_page 的 block_title 里的 {{ super() }}：
j2_base:block_title(V, I, B)
```

`super()` 编译成对**直接父模板**同名 block 函数的调用。多级继承时
`super()` 逐级向上是自然成立的：孙的 super 调子，子的 super 调父。

`{% block name scoped %}`：默认情况下 block 内**看不到**定义它的位置的循环变量，
`scoped` 才能看到。两者的差别落在**调用点传什么作用域**：

```erlang
%% {% block item %}        —— 只传模板级作用域
(maps:get(item, B, Default))(ai_jinja_rt:globals(V), I, B)
%% {% block item scoped %} —— 传整条作用域链
(maps:get(item, B, Default))(V, I, B)
```

因为 `scoped` 与否在**父模板**的 block 定义处就已知，覆盖它的子模板不需要参与决定，
这一条按参照实现的语义实现即可，不进差异清单。

`{% block name required %}`：父模板中声明为 required 的 block，
若继承链最下游没有提供实现 → **运行期**错误。编译期无法判断
（不知道谁会继承自己），因此 `block_name/3` 的默认实现就是抛错。

### 5.3 目标必须是字面量

`{% extends parent_var %}` 在 v1 报 `{dynamic_target_unsupported, Loc}`。

原因是整个方案建立在「编译期把模板名解析成模块名」之上。
若要支持动态目标，需要运行期从字符串算模块名，等于把 `binary_to_atom/2`
放在模板数据路径上（atom 表攻击面），或维护一张运行期注册表（回到 ets）。
两者都与架构不变量冲突。

**将来若必须支持**：正确做法是编译期收集候选集合（`{% extends %}` 的所有可能值
由用户在 `jinja_opts` 里显式列出），生成一个 `case` 分派到静态调用。
这条留给 v2，现在不做。

## 6. 缩进

沿用 mustache 的做法（[04 §5](04-codegen.md)）：编译期跟踪「是否处于行首」，
只在行首的内容前放缩进变量 `I`。`{% include %}` 把自己的缩进传给被包含模板，
被包含模板对**自己的静态文本**施加缩进，不对插值结果施加。

Jinja2 本身**没有** partial 缩进的概念（`{% include %}` 不缩进）。
本实现保留 `I` 参数并默认传 `<<>>`，行为与参照实现一致；
`{% include "x" indent %}` 作为扩展语法开启缩进——列为可选，不在 v1 验收内。

## 7. 编译期优化

| 优化 | 说明 |
|---|---|
| 静态文本合并 | 与 mustache 同：相邻 text 节点合并成一个字面量，进 literal pool |
| 全静态模板折叠 | 整个模板无动态节点时，`render/1` 直接返回一个字面量 |
| 常量折叠 | §3.3。`{{ 1 + 2 }}`、`{{ "a" ~ "b" }}`、`{% if true %}` |
| 死分支消除 | `{% if false %}` 的 body 整体不生成 |
| filter 静态绑定 | [10 §8.1](10-jinja-semantics.md) |
| `loop` 按需 | body 不引用 `loop` 时用 list comprehension 而非 foldl |
| 作用域省略 | body 不含 `set` 时不生成作用域变量的元组穿引 |

前四条与 mustache 共享思路，可直接借鉴 `ai_mustache_compiler:static_text/1`
与 `literal_of/1` 的实现。

## 8. `compile_inline/3`

与 mustache 一致（[04](04-codegen.md)、`ai_mustache_compiler:compile_inline/3`）：
把模板编译成**单个表达式**，供 parse_transform 就地展开。

Jinja 侧的限制更多，因为内联模板没有 views 目录也没有模块可挂函数：

| 形态 | 内联模板 |
|---|---|
| `{% include %}` / `{% extends %}` / `{% import %}` | **拒绝** `{target_in_inline_template, Loc}` |
| `{% block %}` | **拒绝**（没有继承链，block 无意义） |
| `{% macro %}` | 允许，编译成 `begin Macro__ = fun(...) ... end` 的绑定 |
| `{% for %}` / `{% if %}` / `{% set %}` | 允许 |

辅助函数一律编译成绑定的匿名 fun，与 mustache 的 `ref/2` → `{bound, Var}` 同一套机制。

## 9. 错误

沿用 mustache 的 `{error, {File, Line, Reason}}` 外壳。Reason 新增：

```erlang
-type ai_jinja_reason() ::
      {unclosed_block, atom(), loc()}
    | {mismatched_end, Expected :: atom(), Got :: atom()}
    | {block_name_mismatch, atom(), atom()}
    | {duplicate_block, atom(), loc()}
    | {orphan_clause, atom()}
    | {unexpected_token, term()}
    | {chained_comparison, loc()}
    | {unknown_filter, atom()}
    | {unknown_test, atom()}
    | {filter_name_conflict, atom(), module()}
    | {dynamic_target_unsupported, loc()}
    | {template_not_found, binary()}
    | {namespace_assignment_unsupported, loc()}
    | {target_in_inline_template, atom()}
    | {invalid_utf8, non_neg_integer()}
    | {unexpected_remote_calls, [module()]}.
```

表达式内部的错误必须带**表达式子节点的位置**而非整个 tag 的位置
（[09 §6](09-jinja-syntax.md)）。
