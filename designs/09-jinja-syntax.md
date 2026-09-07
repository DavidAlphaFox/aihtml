# 09 · Jinja2 语法定稿：词法与文法

本文档是 `ai_jinja_scanner` / `ai_jinja_lexer` / `ai_jinja_expr` / `ai_jinja_parser` 四个模块的规格说明。
参照实现是 CPython 的 [pallets/jinja](https://github.com/pallets/jinja) 3.x；**偏离之处在 [10 §9](10-jinja-semantics.md#9-与-cpython-jinja2-的差异清单) 集中列出，本文不逐条重复。**

## 1. 三种定界符

| 形态 | 用途 | 输出 |
|---|---|---|
| `{{ expr }}` | 插值 | 表达式求值后写入输出 |
| `{% stmt %}` | 语句 | 控制结构，本身不产出文本 |
| `{# ... #}` | 注释 | 完全丢弃，包括其中的 `{{`/`{%` |

定界符**不可配置**（决策 D8，见 [10 §9](10-jinja-semantics.md)）。CPython Jinja2 允许通过
`Environment(block_start_string=...)` 改，但那会让 stamp 必须包含定界符配置、
让 `raw` 的扫描逻辑参数化、让所有错误信息动态拼接，代价远大于收益。

### 1.1 扫描顺序

scanner 单遍扫描，遇到 `{` 时看下一字节：

```
{{  → 插值开始      {%  → 语句开始      {#  → 注释开始      其它 → 普通文本
```

**`{#` 必须最先被识别**：注释里可以合法地包含未闭合的 `{%`，如果先按语句扫会得到一个虚假的未闭合错误。

`}}` / `%}` / `#}` 的查找不做嵌套计数，但**必须跳过字符串字面量内部**：
`{{ a|replace("%}", "x") }}` 中的 `%}` 不是结束符。因此插值与语句体的结束符查找
不能用 `binary:match/2`，必须由 `ai_jinja_lexer` 边词法分析边推进（见 §3.4）。
注释体反之——注释里没有字符串字面量的概念，`binary:match/2` 找 `#}` 即可。

## 2. 空白控制

### 2.1 显式标记

| 写法 | 效果 |
|---|---|
| `{{-` `{%-` `{#-` | 删除**紧邻左侧**的全部空白（含换行），不限行数 |
| `-}}` `-%}` `-#}` | 删除**紧邻右侧**的全部空白（含换行），不限行数 |
| `+%}` `+}}` | 显式关闭该侧的 `trim_blocks`（见 §2.2） |

「不限行数」是 Jinja2 的真实行为，与直觉不同：`a\n\n\n   {{- x }}` 渲染为 `ax`。

### 2.2 全局选项

| 选项 | 默认 | 效果 |
|---|---|---|
| `trim_blocks` | `false` | `%}` / `#}` **正后方**紧跟的一个换行被删除（`}}` 不受影响） |
| `lstrip_blocks` | `false` | `{%` / `{#` **正前方**从行首起的纯空白被删除（`{{` 不受影响） |
| `keep_trailing_newline` | `false` | 为 `false` 时，模板正文末尾的一个 `\n` 被删除 |

**推荐默认值改为 `trim_blocks=true, lstrip_blocks=true`**，理由见 [10 §9](10-jinja-semantics.md)。
两个选项都参与 `source_hash/2`，改动会触发重编。

### 2.3 优先级

显式标记压过全局选项。三者的作用顺序固定：

```
1. lstrip_blocks   （左侧行首空白）
2. 显式 - / +      （覆盖 1 与 3 的结论）
3. trim_blocks     （右侧换行）
```

`keep_trailing_newline` 在扫描开始前对整个正文做一次预处理，不参与逐 tag 的判断。

### 2.4 实现约束

空白裁剪**只作用于 text token 的内容**，绝不作用于插值的输出。
`{{- x -}}` 裁掉的是它两侧的静态文本，`x` 求值出来的前后空白原样保留。
这与 mustache 的 partial 缩进是同一类约束（[04 §5](04-codegen.md)），做反了会在
「值本身以换行开头」的情况下才暴露，非常难查。

## 3. 表达式文法

### 3.1 词法单元

```
名字        [A-Za-z_][A-Za-z0-9_]*        （UTF-8 标识符不支持，见 10 §9）
整数        123 · 1_000 · 0x1f · 0o17 · 0b1010
浮点        1.5 · 1e10 · 1.5e-3 · 1_000.5
字符串      "..." · '...'   支持 \n \t \r \\ \" \' \xHH \uHHHH
布尔/空     true·True · false·False · none·None
运算符      + - * / // % ** ~ == != < > <= >= = . , : | [ ] ( ) { } 
关键字      and or not in is if else
```

`true`/`True` 两种拼写都接受（Jinja2 亦然）。`~` 是字符串拼接，不是按位取反。

### 3.2 优先级表（低 → 高）

| 级 | 运算 | 结合性 |
|---|---|---|
| 1 | `X if C else Y` | 右（条件表达式） |
| 2 | `or` | 左 |
| 3 | `and` | 左 |
| 4 | `not X` | 前缀 |
| 5 | `==` `!=` `<` `>` `<=` `>=` `in` `not in` `is` `is not` | 左，**不链式** |
| 6 | `~` `+` `-` | 左 |
| 7 | `*` `/` `//` `%` | 左 |
| 8 | `+X` `-X` | 前缀 |
| 9 | `**` | **左**（见下） |
| 10 | `\|filter` | 左 |
| 11 | `.attr` `[sub]` `(call)` | 左（后缀链） |

三条容易写错的：

- **`**` 在 Jinja2 里是左结合**，与 Python 相反：`2**3**2` = `(2**3)**2` = 64，不是 512。
  原因是 CPython Jinja2 的 `parse_pow` 与其它二元运算一样用 while 循环左折叠。
  这类「参照实现与其宿主语言不一致」的细节**一律以 fixture 实测为准**（[13 §2](13-jinja-roadmap.md)），
  不得凭 Python 直觉实现。
- **filter 比算术和 `**` 都结合得紧，但比一元符号松**：
  `a + b|upper` = `a + (b|upper)`；`2**3|abs` = `2 ** (3|abs)`；而 `-a|abs` = `(-a)|abs`。
  实现上 filter 挂在一元表达式解析的最外层，见 §3.3 的 `signed`/`filtered`。
- **比较不链式**：Python 的 `1 < x < 3` 在 Jinja2 里也成立，但语义是链式比较。
  本实现**不支持链式**，`1 < x < 3` 解析为 `(1 < x) < 3` 会得到布尔与整数比较——
  因此在 parser 层直接**报错** `{chained_comparison, Loc}`，而不是静默给出错误答案。
  这是 [10 §9](10-jinja-semantics.md) 的一条显式偏离。

### 3.3 文法（EBNF）

```ebnf
expr        = condexpr
condexpr    = or_expr [ "if" or_expr [ "else" condexpr ] ]
or_expr     = and_expr { "or" and_expr }
and_expr    = not_expr { "and" not_expr }
not_expr    = "not" not_expr | compare
compare     = math { compop math }              (* 最多一个 compop，见 §3.2 *)
compop      = "==" | "!=" | "<" | ">" | "<=" | ">=" 
            | "in" | "not" "in" | "is" [ "not" ] test_call
math        = term { ("+"|"-"|"~") term }
term        = power { ("*"|"/"|"//"|"%") power }
power       = filtered { "**" filtered }        (* 左结合，见 §3.2 *)
filtered    = signed { "|" filter_call }
signed      = ("+"|"-") signed_nf | postfix     (* 一元符号在 filter 之内 *)
signed_nf   = ("+"|"-") signed_nf | postfix     (* 同上，但不再接 filter *)
filter_call = NAME [ "(" arglist ")" ]
test_call   = NAME [ "(" arglist ")" ]
postfix     = primary { "." NAME | "[" subscript "]" | "(" arglist ")" }
subscript   = expr | [expr] ":" [expr] [ ":" [expr] ]      (* 切片 *)
primary     = NAME | NUMBER | STRING+ | "true" | "false" | "none"
            | "(" expr [ "," ... ] ")"          (* 括号 / 元组 *)
            | "[" [ expr { "," expr } [","] ] "]"
            | "{" [ pair { "," pair } [","] ] "}"
pair        = expr ":" expr
arglist     = [ arg { "," arg } [","] ]
arg         = expr | NAME "=" expr | "*" expr | "**" expr
```

要点：

- **相邻字符串字面量自动拼接**：`"a" "b"` = `"ab"`（Python/Jinja2 行为）。
- **括号内单个表达式不是元组**：`(a)` = `a`，`(a,)` 与 `(a, b)` 才是元组。
- **`is` 的右侧是 test 名而非表达式**：`x is divisibleby(3)`。
  这是文法里唯一需要在解析时切换「名字空间」的地方。
- **切片支持到三段** `a[1:9:2]`，缺省段允许省略。

### 3.4 lexer 与 scanner 的分工

scanner 需要知道插值/语句在哪里结束，而结束符可能出现在字符串字面量里（§1.1）。
方案：**scanner 委托 lexer 做一次前向扫描**，lexer 逐 token 推进并在遇到顶层
`}}` / `%}` 时停下，把消费到的字节数还给 scanner。

因此 `ai_jinja_lexer` 的接口是流式的：

```erlang
-spec tokens(binary(), Loc, Stop :: expr_end | stmt_end) ->
        {ok, [token()], Rest :: binary(), EndLoc} | {error, reason()}.
```

括号深度由 lexer 维护：`{{ {"a": 1} }}` 中间那个 `}` 后面跟着 ` }}`，
只有在括号深度为 0 时看到 `}}` 才算结束。

## 4. 语句集

按实现优先级分三档。第一档是 v1 必须，第二档是本轮目标，第三档明确不做。

### 4.1 第一档 · 核心

```jinja
{% if C %} ... {% elif C %} ... {% else %} ... {% endif %}
{% for x in seq %} ... {% else %} ... {% endfor %}
{% for k, v in mapping.items() %} ... {% endfor %}          (* 解构 *)
{% for x in seq if C %} ... {% endfor %}                    (* 内联过滤 *)
{% for x in seq recursive %} ... {{ loop(x.children) }} ... {% endfor %}
{% set name = expr %}
{% set name %} ... {% endset %}                             (* 块形式 *)
{% with a = 1, b = 2 %} ... {% endwith %}
{% filter upper %} ... {% endfilter %}
{% raw %} ... {% endraw %}
{% include "path" [ignore missing] [with[out] context] %}
{% do expr %}
```

### 4.2 第二档 · 继承与宏

```jinja
{% extends "base.j2" %}
{% block name [scoped] [required] %} ... {% endblock [name] %}
{{ super() }}
{% macro name(a, b=1) %} ... {% endmacro %}
{% call [(args)] macroname(...) %} ... {% endcall %}
{% import "m.j2" as m [with[out] context] %}
{% from "m.j2" import a, b as c [with[out] context] %}
{% namespace %}  →  实为内置函数 namespace()，非语句
```

### 4.3 第三档 · 不做

| 语句 | 不做的理由 |
|---|---|
| `{% autoescape %}` 块 | autoescape 是**编译期常量**（[10 §4](10-jinja-semantics.md)）。块级切换需要运行期传播 safe 标记，与「静态文本编译成字面量」的核心收益冲突 |
| `{% trans %}` / i18n 扩展 | 属于 Jinja2 的扩展体系而非语言本体。aihtml 已有 `{{@ }}` 风格的扩展点思路，i18n 应走 [12 §5](12-jinja-toolchain.md) 的自定义 tag |
| `{% debug %}` | 依赖运行期反射整个 context，与零依赖运行时不符 |
| `{% continue %}` / `{% break %}` | Jinja2 本体也没有（属 `loopcontrols` 扩展）。生成代码里实现它需要把 list comprehension 改写成带状态的递归，代价高 |
| 自定义定界符 | 见 §1 |

### 4.4 配对与嵌套校验

parser 维护一个块栈，每个开块语句记录 `{Kind, Loc, Name}`。

- `{% endblock name %}` 的 `name` 若与开块不符 → `{block_name_mismatch, Open, Close, Loc}`；
- 闭合语句与栈顶 Kind 不符 → `{mismatched_end, Expected, Got, Loc}`；
- 文件结束时栈非空 → `{unclosed_block, Kind, OpenLoc}`；
- `{% elif %}` / `{% else %}` 出现在非 `if`/`for` 的栈顶 → `{orphan_clause, Kind, Loc}`；
- `{% extends %}` 不在模板**顶层第一个非空白节点**位置 → 只报 warning 并照常处理
  （Jinja2 允许 extends 出现在任意位置，但只有第一个生效；把它当错误会拒掉合法模板）。
- 同一模板内重复的 `{% block name %}` → `{duplicate_block, Name, FirstLoc, Loc}`，**错误**。

## 5. token 与 AST

### 5.1 scanner 输出

```erlang
-type jinja_token() ::
      {text,    loc(), binary()}
    | {expr,    loc(), [expr_token()]}                 % {{ ... }}
    | {stmt,    loc(), Keyword :: atom(), [expr_token()]}  % {% kw ... %}
    | {raw,     loc(), binary()}.                      % {% raw %} 的原文
```

注释不产生 token。`raw` 在 scanner 层就吃掉，parser 不需要知道它存在。

### 5.2 AST 节点

```erlang
-type jinja_node() ::
      {text,     loc(), binary()}
    | {output,   loc(), expr()}                                   % {{ e }}
    | {'if',     loc(), [{expr(), [jinja_node()]}], Else :: [jinja_node()]}
    | {'for',    loc(), Targets :: [atom()], Iter :: expr(),
                 Filter :: expr() | undefined, Recursive :: boolean(),
                 Body :: [jinja_node()], Else :: [jinja_node()]}
    | {set,      loc(), Target :: target(), expr()}
    | {set_block,loc(), Target :: target(), [jinja_node()],
                 Filter :: expr() | undefined}
    | {with,     loc(), [{atom(), expr()}], [jinja_node()]}
    | {filter,   loc(), expr(), [jinja_node()]}                   % {% filter f %}
    | {include,  loc(), Target :: binary() | module() | expr(),
                 IgnoreMissing :: boolean(), WithCtx :: boolean()}
    | {do,       loc(), expr()}
    | {block,    loc(), Name :: atom(), Scoped :: boolean(),
                 Required :: boolean(), [jinja_node()]}
    | {extends,  loc(), Target :: binary() | module()}
    | {macro,    loc(), Name :: atom(), [param()], [jinja_node()]}
    | {call,     loc(), [param()], expr(), [jinja_node()]}
    | {import,   loc(), Target :: binary() | module(), As :: atom(), WithCtx :: boolean()}
    | {from,     loc(), Target :: binary() | module(),
                 [{atom(), atom()}], WithCtx :: boolean()}.

-type target() :: atom() | {attr, expr(), atom()}.       % set 的左值
-type param()  :: {atom(), Default :: expr() | undefined}.
```

`expr()` 是表达式 AST，形状见 [11 §3](11-jinja-codegen.md)。

与 mustache AST 的两点结构差异，都是刻意的：

1. **`if` 的分支是一个 `[{Cond, Body}]` 列表**而不是嵌套的 if/else 节点。
   `{% elif %}` 链在 Jinja2 里可以很长，嵌套表示会让 codegen 递归层数等于 elif 数量，
   而扁平列表直接对应一个 Erlang `case`/`if` 的多个子句。
2. **`extends` / `include` / `import` 的 Target 与 mustache 的 partial 一样，
   在 AST 后处理阶段从 `binary()` 变成 `module()`**（[11 §5](11-jinja-codegen.md)）。
   保留 `expr()` 分支是为了给「动态 include」留位置，但 v1 遇到它直接报错
   `{dynamic_target_unsupported, Loc}`（[10 §9](10-jinja-semantics.md)）。

## 6. 错误位置

每个 token 与 AST 节点都带 `{Line, Col}`。表达式内部的子节点**也各自带位置**——
`{{ a|unknownfilter }}` 的错误必须指向 filter 名的列号而不是整个 `{{` 的位置，
否则一行里有五个 filter 时用户无从下手。

这是相对 mustache 的一处提升：mustache 的 tag 内容是单个 key 路径，
指到 tag 就够了；Jinja 的一个 `{{ }}` 里可以有几十个子表达式。
