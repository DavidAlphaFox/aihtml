# Jinja2 引擎

aihtml 同时提供两套模板引擎，二者是并列关系：谁也不替代谁，同一个项目里可以一起用。
本文讲 jinja 这套；mustache 见[项目 README](../README.zh-CN.md)。

这里写的每一条行为都由 `test/jinja_spec/` 下的 520 条 fixture 固定，
它们由 `tools/gen_jinja_fixtures.py` 用真的 CPython `jinja2` 3.1 生成。
刻意偏离参照实现的地方，fixture 会标出来，并列在下面的[差异清单](#差异清单)里。

---

## 快速开始

```erlang
%% rebar.config
{plugins, [rebar3_aihtml]}.
{deps, [aihtml]}.
{provider_hooks, [{pre, [{compile, jinja}]}]}.

{jinja_opts, [{views, "views"}, {suffix, ".j2"}, {prefix, "j2_"}]}.
{erl_opts, [debug_info, {src_dirs, ["src", "_gen"]}]}.
```

`views/page.j2` 编译成模块 `j2_page`：

```erlang
j2_page:render(#{title => <<"Hi">>, users => [#{name => <<"ada">>}]}).
%% => <<"...">>

j2_page:render_iolist(Ctx).   %% 直接喂给 cowboy，省掉最后一次拷贝
```

模板在**构建期**编译成 Erlang 代码。没有模板缓存、没有 ets、没有进程：
编译产物由 BEAM code server 持有，静态文本进模块的 literal pool，跨进程按引用共享。

---

## 支持的语法

### 表达式

完整表达式语言：字面量、算术（`+ - * / // % ** ~`）、比较、`and`/`or`/`not`、
`X if C else Y`、属性访问、下标与切片、调用、filter（`|`）与 test（`is`）。

```jinja
{{ users|selectattr("active")|map(attribute="name")|join(", ") }}
{{ (a + b) * 2 if ready else 0 }}
{{ items[1:5:2] }}
{{ n is divisibleby(3) }}
```

有两条优先级规则值得记住，因为它们反直觉，而且与参照实现一致：

* **`**` 是左结合**，与 Python 相反：`2 ** 3 ** 2` 是 64。
* **filter 比任何算术运算符结合得都紧，但一元符号在它之内**：
  `1 + -2|abs` 是 `1 + abs(-2)` = 3，而 `-a|abs` 是 `(-a)|abs`。

### 语句

```jinja
{% if %} {% elif %} {% else %} {% endif %}
{% for x in xs %} {% else %} {% endfor %}        {# 支持 loop、if、recursive #}
{% set x = ... %}   {% set x %}...{% endset %}
{% with a = 1 %}...{% endwith %}
{% filter upper %}...{% endfilter %}
{% raw %}...{% endraw %}
{% do expr %}
{% include "p.j2" [ignore missing] [with|without context] %}
{% extends "base.j2" %}  {% block name [scoped] [required] %}  {{ super() }}
{% macro m(a, b=1) %}...{% endmacro %}  {% call(x) m() %}...{% endcall %}
{% import "lib.j2" as lib %}  {% from "lib.j2" import a, b as c %}
```

`loop` 变量带 `index index0 revindex revindex0 first last length depth depth0
previtem nextitem cycle(...) changed(...)`。

**不实现**：`{% autoescape %}`、`{% trans %}`、`{% debug %}`、
`{% break %}` / `{% continue %}`，以及自定义定界符。原因见[差异清单](#差异清单)；
用到它们是**编译错误**，不会静默忽略。

### 空白控制

`{{- -}}`、`{%- -%}`、`{#- -#}`，加上 `trim_blocks`、`lstrip_blocks`、
`keep_trailing_newline`。**`trim_blocks` 与 `lstrip_blocks` 在这里默认为 `true`**，
与参照实现相反——见差异 J8。

---

## 值

| Jinja 概念 | Erlang term |
|---|---|
| dict / mapping | key 为 `atom()` 的 `map()` |
| list / sequence | `list()` |
| tuple | `tuple()` |
| string | `binary()`，UTF-8 |
| int / float | `integer()` / `float()` |
| true / false | `true` / `false` |
| none 与 undefined | `undefined` |
| safe（Markup）字符串 | `{safe, iodata()}` |
| callable | `fun/N` |

**输出形式对齐参照实现，而不是对齐 Erlang 的习惯**，因为从 jinja2 移植过来的模板
期待的是前者：`true` 输出 `True`，列表输出 `[1, 2]`，map 输出 `{'a': 1}`，
容器里的字符串带引号、顶层的不带。

真值判断用 Python 的规则，**与 mustache 相反**：`0`、`0.0`、`#{}`、`{}` 在这里是假，
在那边是真。

---

## 自动转义

默认开启。`{{ x }}` 转义 `& < > " '`；`|safe` 标记「已安全」，
`|forceescape` 连已标记的也转义。

每个 filter 都按它对 safe 标记的处理方式分类——透明、转义、生成、非文本——
并在 `src/ai_jinja_filters.erl` 里以注解写在函数上。有一条测试**机械地**断言
没有一个 filter 漏标，因为标错一个就是一个 XSS。

`|tojson` 把 `< > & '` 转成 `\uXXXX`，所以值里的 `</script>`
不会提前结束它所嵌入的 script 元素。

---

## 配置

`rebar.config` 里的 `jinja_opts`：

| 键 | 默认 | 进构建戳 |
|---|---|---|
| `views` | `"views"` | 否 |
| `suffix` | `".j2"` | 否 |
| `out_dir` | `"_gen"` | 否 |
| `prefix` | `"j2_"` | **是** |
| `extensions` | `[]` | **是** |
| `escape` | `true` | **是** |
| `trim_blocks` | `true` | **是** |
| `lstrip_blocks` | `true` | **是** |
| `keep_trailing_newline` | `false` | **是** |
| `strict_undefined` | `false` | **是** |
| `line_map` | `true` | **是** |
| `warnings_as_errors` | `false` | 否 |

「进构建戳」意味着改了它会重编全部模板。不进戳的那些改不了生成代码，
把它们折进去只会让「改个 out_dir 名字」重编整个世界。

---

## 自定义 filter 与 test

```erlang
-module(my_filters).
-behaviour(ai_jinja_ext).
-export([filters/0, tests/0, params/0, money/2]).

filters() -> #{money => {?MODULE, money}}.
tests()   -> #{}.
params()  -> #{money => [currency]}.       %% 位置参数的名字

money(V, Args) ->
    Cur = maps:get(currency, Args, <<"USD">>),
    <<(ai_jinja_rt:to_binary(V))/binary, " ", Cur/binary>>.
```

```erlang
{jinja_opts, [{extensions, [my_filters]}]}.
```

`{{ 9|money("EUR") }}` 与 `{{ 9|money(currency="EUR") }}` 到达函数的形式完全一致。
**与内置同名是编译错误，不是覆盖**：某个依赖注册了一个 `join`，模板行为悄悄变了，
这种 bug 没人找得到。

**不提供自定义语句**。`{% mytag %}` 需要暴露的接口远比 filter 宽，
在没有真实需求驱动的情况下设计它，几乎注定要推翻重来。

---

## parse_transform

```erlang
-module(my_views).
-compile({parse_transform, ai_jinja_transform}).

-jinja_ext(my_filters).                      % (a) 声明并校验
-jinja_template({page, "views/page.j2"}).    % (c) 编译成 page/1

greet(Name) ->                               % (b) 就地展开
    ai_jinja:inline(~"Hello {{ name }}!", #{name => Name}).
```

形态 (b) 加不加 transform 都能用——不加时同一份模板在运行期编译，
**输出一致，失败也一致**：两条路径用同一个谓词拒绝同样的语句。
形态 (c) 必须有 transform，否则函数根本不存在。

内联模板不能带 `{% include %}`、`{% extends %}`、`{% import %}`、`{% from %}`、
`{% block %}`——没有 views 目录可解析——它的宏也不能互相递归，
因为展开时宏编译成匿名 fun。

mustache 侧的对应文档见 [Using the parse_transform](parse-transform.zh-CN.md)；
两者是同样的三种形态，可以在同一个模块里一起用。

---

## 开发期热重载

```erlang
ai_jinja_dev:check().        %% 一次性自检
ai_jinja_dev:reload(all).    %% 重编并重载所有过期模板
ai_html_dev:reload_all().    %% 两个引擎一起
```

**没有开关，也不监听任何东西**：不用 ets、不用 persistent_term、不起进程，
就没有地方存「是否启用」。在 dev profile 的请求入口调一次 `reload(all)`，
或者绑到编辑器的保存钩子上。

过期判断只看内容摘要，**从不看 mtime**：POSIX mtime 只有秒级精度，
而「改完立刻刷新」这个场景本身就常常发生在同一秒内。

---

## 差异清单

每一条都是刻意的，每一条都有 fixture。

| # | 差异 | 理由 |
|---|---|---|
| J1 | map 的 key 是 `atom()` 而非字符串 | 沿用 mustache 侧的决定；避免热路径上的 binary key 比较 |
| J2 | `none` 与 undefined 合并为一个值 | Erlang 只有一个惯用的「空」。后果：显式传 `none` 时 `x is defined` 为 **false** |
| J3 | 拒绝链式比较 `1 < x < 3` | 宁可编译报错，也不给出一个理直气壮的错误答案 `(1 < x) < 3` |
| J4 | 未知 filter / test 是**编译**错误 | 拼写错误不该等到页面被访问时才暴露 |
| J5 | 定界符不可配置 | 它得进构建戳、得参数化 raw 扫描，收益极小 |
| J6 | 无 `{% autoescape %}` 块 | 自动转义是编译期常量；块级切换需要在运行期传播 safe 标记 |
| J7 | 无 `{% trans %}`、`{% debug %}`、`{% break %}`、`{% continue %}` | 它们属于参照实现的扩展体系而非语言本体。用自定义 filter 代替 |
| J8 | `trim_blocks` 与 `lstrip_blocks` 默认**开** | 参照实现的默认值会让真实模板塞满空行，几乎所有项目都会打开 |
| J9 | 迭代字符串报错 | 在模板里这几乎总是笔误 |
| J10 | 标识符只支持 ASCII | 非 ASCII 名字需要 `binary_to_atom/2`，那是无界的 atom 表 |
| J11 | `extends`/`include`/`import` 的目标必须是字面量 | 编译期解析成模块名，正是它们能变成直接跨模块调用的前提 |
| J12 | 无沙箱 | 模板编译成 Erlang 代码，沙箱只能做在代码生成层 |
| J13 | 属性访问不回退到方法调用 | Erlang 的 map 没有方法。`.items()` `.keys()` `.values()` 是特例化的三个 |
| J14 | undefined 可链式 | `a.b.c` 渲染为空而不抛错，等价于 `ChainableUndefined`。`strict_undefined` 提供严格行为 |
| J15 | `{% for %}` 不支持嵌套解构 | 平铺的 `{% for k, v in ... %}` 覆盖真实用法 |
| J16 | 不支持 `{% set ns.x = ... %}` | Erlang 没有可变容器；支持它会让所有外层循环从列表推导降级为 foldl |
| J17 | 宏看到的是模板级名字，与定义顺序无关 | 参照实现捕获定义点的作用域，模块级函数做不到 |

---

## 两个引擎一起用

```erlang
{provider_hooks, [{pre, [{compile, mustache}, {compile, jinja}]}]}.
{mustache_opts, [{views, "views"}, {prefix, "view_"}]}.
{jinja_opts,    [{views, "views"}, {suffix, ".j2"}, {prefix, "j2_"}]}.
```

它们可以共用同一个 `views` 和同一个 `out_dir`：每个生成文件在 banner 里写明自己出自哪个引擎，
孤儿清理只删自己那一份。模块名是全局的，所以两个引擎之间撞名会像任何其它冲突一样被报出来。

唯一要记住的是：这两门语言是**不一样**的。

| | mustache | jinja |
|---|---|---|
| 变量查找 | 动态回溯 context stack | 词法作用域 |
| `0` 与 `#{}` | 真 | 假 |
| 缺失变量 | 空 | 空（`strict_undefined` 可改） |
| 关掉转义 | `{{{x}}}` | `\|safe` |
| 复用 | `{{> p}}` | include / extends / macro / import |
| `true` 输出为 | `true` | `True` |
| 列表输出为 | 它的字符 | `[1, 2]` |
