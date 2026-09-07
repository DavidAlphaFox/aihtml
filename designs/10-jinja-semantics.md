# 10 · Jinja2 语义定稿

[09](09-jinja-syntax.md) 定义「怎么写」，本文定义「写了会发生什么」。
四个决策点（作用域、undefined、autoescape、真值）互相牵连，必须一起定。

## 1. 值域

模板里的值来自用户传入的 Erlang term。映射关系：

| Jinja2 概念 | Erlang 表示 |
|---|---|
| dict / mapping | `map()`，key 为 `atom()` |
| list / sequence | `list()` |
| tuple | `tuple()`（只读序列，`\|list` 可转） |
| string | `binary()`（UTF-8） |
| int / float | `integer()` / `float()` |
| `true` / `false` | `true` / `false` |
| `none` | `undefined`（**不是** `null`，见 §3） |
| undefined | `undefined` |
| Markup（safe 字符串） | `{safe, iodata()}` |
| callable | `fun/N` |

### 1.1 渲染形式对齐参照实现

`{{ e }}` 输出的文本形式与 CPython Jinja2 一致，**这是刻意的**，不是疏忽：

| 值 | 输出 |
|---|---|
| `true` / `false` | `True` / `False` |
| 列表 / 元组 / map | Python 的 `repr`：`[1, 2]` · `(1, 2)` · `{'a': 1}` |
| 容器内的字符串 | 带引号：`['a', 'b']` |
| 顶层字符串 | 不带引号 |
| 整数 / 浮点 | `integer_to_binary/1` · `float_to_binary(F, [short])` |

理由是模板语言的输出形式属于**模板语言**而不属于宿主语言。用户写 `{{ items }}`
调试一个列表时，期望看到的是 Jinja 文档里的那个形式；让 Erlang 的 `true`
渲染成 `true` 只会让所有从 Jinja2 移植过来的模板输出静默变样。
`ai_jinja_rt:to_binary/1` 因此带一个小型的 Python `repr` 实现（约 60 行）。

**与 mustache 侧不同**：那边 `to_binary(true)` 是 `<<"true">>`。两个引擎的
输出形式各自对齐各自的参照实现，这条要进 [§10](#10-与-mustache-语义的对照) 的对照表。

**map 的 key 是 atom**，沿用 mustache 侧的既有决定（[03 §8](03-semantics.md)）。
`{{ d["a"] }}` 中的字符串下标在编译期若是字面量，直接转成 atom；
运行期字符串下标由 `ai_jinja_rt:subscript/2` 处理，**不做 `binary_to_atom/2`**——
那会把模板数据变成 atom 表的攻击面。改为 `binary_to_existing_atom/2`，失败即 undefined。

## 2. 作用域

这是 Jinja 与 mustache 最深的分歧，也是 codegen 形态的决定因素。

### 2.1 mustache 没有作用域，Jinja 有

mustache 的 `lookup(Keys, Stack)` 是**动态**的：从栈顶向外找第一个含该 key 的 frame。
Jinja 是**词法**的：`{% for x in xs %}` 在块内引入名字 `x`，块外不可见；
`{% set y = 1 %}` 之后的语句能看到 `y`，之前的看不到。

### 2.2 表示：作用域链

```erlang
-type scope() :: [frame()].
-type frame() :: map().          % atom() => term()
```

栈底永远是用户传入的根 context（可为非 map 的裸值，与 mustache 一致）。
`for` / `with` / `macro` / `call` / `filter` 块各压一帧。

名字解析 `ai_jinja_rt:resolve(Name, Scope)`：从栈顶向外找第一个含该名字的 map frame，
找不到返回 `undefined`。**注意这与 mustache 的 `lookup/2` 形状相同但含义不同**：
mustache 的栈是数据嵌套栈，Jinja 的栈是词法环境栈。两者不共享实现。

### 2.3 `{% set %}` 必须串行穿引

`{% set %}` 改变**其后**语句可见的环境。因此一段 body 不能再编译成扁平 iolist——
它是一个带状态的序列：

```erlang
%% {% set a = 1 %}{{ a }}{% set a = 2 %}{{ a }}
V1 = ai_jinja_rt:bind(V0, a, 1),
E1 = ai_jinja_rt:resolve(a, V1),
V2 = ai_jinja_rt:bind(V1, a, 2),
E2 = ai_jinja_rt:resolve(a, V2),
[E1, E2]
```

codegen 因此要以 `{Exprs, ScopeVar}` 的形式在节点间穿引作用域变量（[11 §4](11-jinja-codegen.md)）。
mustache 的 codegen 里没有这个维度，这也是两个 compiler 无法合并的直接原因。

### 2.4 块作用域的逃逸规则

Jinja2 的规定，逐条落地：

| 场景 | 行为 |
|---|---|
| `{% for %}` 内 `{% set %}` | **不逃逸**到循环外。每轮迭代从循环入口的环境开始 |
| `{% if %}` 内 `{% set %}` | **逃逸**。if 不是作用域，只是分支 |
| `{% with %}` / `{% macro %}` / `{% block %}` 内 | **不逃逸** |
| `{% for %}` 内累加计数 | 需 `namespace()`（§2.5） |

「if 内逃逸、for 内不逃逸」在 Erlang 里的落地：

- `if` 编译成 `case`，两个分支必须绑定**同一组**作用域变量名，才能在 case 之后继续使用。
  做法是让每个分支的最后一个表达式统一返回 `{Iolist, Scope}` 二元组，
  `case` 的结果直接匹配成 `{E, V1}`。
- `for` 编译成 list comprehension 或 `lists:foldl/3`，循环体内产生的新作用域**丢弃**，
  只保留 iolist。这天然满足「不逃逸」。

### 2.5 `namespace()`

```jinja
{% set ns = namespace(total=0) %}
{% for i in items %}{% set ns.total = ns.total + i %}{% endfor %}
{{ ns.total }}
```

`namespace()` 是一个内置函数，返回可变容器。Erlang 里没有可变容器，
但**这个用法可以在编译期识别并改写**：`{% set ns.total = E %}` 的左值是
`{attr, ns, total}` 形态，且 `ns` 绑定自 `namespace(...)`。

v1 的落地方案：`namespace(...)` 返回 `{ns, map()}`，`{% set ns.f = E %}` 编译成
「重新绑定 `ns` 这个名字」，并且 `for` 循环体的编译在检测到内部有 namespace 赋值时
**从 list comprehension 降级为 `lists:foldl/3`**，把 namespace 作为累加器穿过迭代。

这是全套 Jinja 语义里实现最曲折的一处。**如果工期需要取舍，这一条是第一个应该砍掉的**——
砍掉后 `{% set ns.x = %}` 报编译错误 `{namespace_assignment_unsupported, Loc}`，
用户改用 `\|sum` 之类的 filter，绝大多数场景够用。

## 3. undefined 与 none

CPython Jinja2 区分 `Undefined`（名字不存在）与 `None`（值为空），
且提供 `Undefined` / `ChainableUndefined` / `DebugUndefined` / `StrictUndefined` 四种策略。

本实现的定稿：

| 概念 | 表示 | `{{ }}` 输出 | 属性访问 | 参与运算 |
|---|---|---|---|---|
| 未定义 | `undefined` | `<<>>` | `undefined`（可链式） | 抛 `{ai_jinja, {undefined_operation, Op}}` |
| `none` | `undefined` | `<<>>` | 同上 | 同上 |
| JSON null | `null` | `<<>>` | `undefined` | 同上 |

**`none` 与 `undefined` 合并为同一个 Erlang 值。** 理由：
Erlang 没有对应 `None` 的惯用原子，而 `undefined` 就是 Erlang 里表达「无值」的惯例；
引入第二个哨兵值会让每个 filter 都要处理两种空，收益只是让 `x is none` 与 `x is undefined` 可区分。
代价明确记在 §9 的差异清单里：**`is defined` 对显式传入的 `none` 返回 `false`**。

`{strict_undefined, true}` 选项把「输出 undefined」也变成错误，用于 CI。
默认关闭，与 mustache 侧「缺失变量渲染为空」保持一致。

## 4. autoescape

### 4.1 默认开启，编译期常量

`{escape, true}`（默认）时每个 `{{ e }}` 编译成 `ai_jinja_rt:escape(E)`；
`{escape, false}` 时编译成 `ai_jinja_rt:to_binary(E)`。
**没有块级 `{% autoescape %}`**（[09 §4.3](09-jinja-syntax.md)）。

### 4.2 safe 的表示与传播

`{safe, IoData}` 是一个运行期标记：

```erlang
escape({safe, D}) -> D;                    % 已标记安全，原样输出
escape(V)         -> ai_html_escape:escape(ai_jinja_rt:to_binary(V)).
```

`|safe` filter 返回 `{safe, ...}`，`|escape` 强制转义，`|e` 是 `|escape` 的别名。

**传播规则**：产出字符串的内置 filter 分三类。

| 类 | 行为 | 例 |
|---|---|---|
| 透明 | 输入 safe → 输出 safe | `upper` `lower` `trim` `indent` `center` |
| 转义 | 对非 safe 的输入先转义，输出 safe | `join` `title` `wordwrap` |
| 生成 | 无条件产出 safe（自己保证输出合法） | `tojson` `urlencode` `xmlattr` |

这张分类表必须写死在 `ai_jinja_filters` 的每个函数上，且**每条都要有 fixture**。
搞错一条就是一个 XSS。

### 4.3 `{% filter %}` 块与 safe

`{% filter upper %}...{% endfilter %}` 的块体是**已经渲染并转义过的输出**，
因此传给 filter 的值必须包成 `{safe, Iolist}`，否则会被二次转义（`&amp;amp;`）。

## 5. 真值

Jinja2 用 Python 真值语义，与 mustache 的五元 falsy 集合**不同**：

| 值 | mustache | Jinja2（本实现） |
|---|---|---|
| `0` / `0.0` | **真** | **假** |
| `#{}` | **真** | **假** |
| `[]` `<<>>` `false` `undefined` `null` | 假 | 假 |
| `{}`（空 tuple） | 真 | 假 |
| 其它 | 真 | 真 |

```erlang
truthy(undefined) -> false;
truthy(false)     -> false;
truthy(null)      -> false;
truthy(0)         -> false;
truthy(0.0)       -> false;
truthy([])        -> false;
truthy(<<>>)      -> false;
truthy(M) when is_map(M)   -> map_size(M)   > 0;
truthy(T) when is_tuple(T) -> tuple_size(T) > 0;
truthy(_)         -> true.
```

**不要复用 `ai_mustache_rt:truthy/1`。** 两者语义不同，共用会让其中一个引擎悄悄错。
这正是 [08 §1.2](08-jinja-architecture.md) 说「真值不共享」的原因。

## 6. 迭代

`{% for %}` 可迭代的类型与 `loop` 变量：

| 类型 | 迭代产出 |
|---|---|
| `list()` | 元素 |
| `map()` | key（与 Python dict 一致）；`.items()` 产出 `{K, V}` 二元组 |
| `binary()` | **报错**。Python 里迭代字符串产出字符，模板里几乎总是笔误 |
| `tuple()` | 元素 |
| `undefined` | 空迭代，不报错 |

`loop` 的字段：`index` `index0` `revindex` `revindex0` `first` `last` `length`
`cycle(...)` `depth` `depth0` `previtem` `nextitem` `changed(...)`。

**`length` / `revindex` / `last` / `nextitem` 需要预先知道总长**，因此迭代前
`ai_jinja_rt:to_list/1` 会把可迭代物实体化成 list。这排除了惰性流，
但换来 `loop` 的完整语义与一次 `length/1`。编译期若能证明 body 不引用这四个字段，
可跳过实体化——列为可选优化，不在 v1 验收内。

## 7. 宏

```jinja
{% macro input(name, value="", type="text") %}
  <input name="{{ name }}" value="{{ value|e }}" type="{{ type }}">
{% endmacro %}
```

编译成模块级函数 `macro_input/2`：接收一个参数 map 与调用点作用域，
返回 iolist。宏内部可见：自己的参数、模板全局（`{% set %}` 于顶层的）、
以及 `varargs` / `kwargs` / `caller`。**宏不能看见调用点的局部变量**——
这是 Jinja2 的规定，也正好对应「编译成独立函数」。

`{% call %}` 把块体包成 `caller` 传给宏：

```erlang
macro_input(#{name := N, caller := C}, V) -> [..., C(#{}, V), ...].
```

`{% import %}` 把另一个模板的宏引入当前作用域。因为宏编译成目标模块的导出函数，
`{% import "m.j2" as m %}` 之后的 `{{ m.input(...) }}` 编译成
**直接的跨模块调用** `j2_m:macro_input(Args, V)`——与 mustache 的 partial 一样，
零运行期查表，且改被导入模板不需要重编导入方（只要宏签名不变）。

## 8. 内置 filter 与 test

### 8.1 静态绑定

filter 名在编译期已知，因此 `{{ x|upper }}` 编译成 `ai_jinja_filters:upper(X)`——
**没有运行期名字查找，没有 apply**。这与 mustache 把 section 分派表在编译期展开
（[04 §4](04-codegen.md)）是同一个手法。

**未知 filter 是编译错误** `{unknown_filter, Name, Loc}`，不是运行期错误。
这条比 CPython Jinja2 严格（那边是运行期 `TemplateAssertionError`），是刻意的收紧：
模板里的错别字应该在 `rebar3 compile` 时就炸。

### 8.2 内置清单（v1）

**字符串**：`upper` `lower` `capitalize` `title` `trim` `striptags` `truncate`
`wordwrap` `wordcount` `center` `indent` `replace` `format` `urlencode` `urlize`
`escape`/`e` `forceescape` `safe` `string`

**序列**：`first` `last` `length`/`count` `list` `join` `reverse` `sort` `sum`
`min` `max` `unique` `slice` `batch` `groupby` `map` `select` `reject`
`selectattr` `rejectattr` `random`

**映射**：`dictsort` `items` `attr` `tojson`

**数值**：`abs` `round` `int` `float` `filesizeformat`

**通用**：`default`/`d` `pprint`

`select`/`reject`/`map` 的参数是 **test 名或 filter 名的字符串**，
因此这一组无法完全静态绑定，走 `ai_jinja_rt:apply_named/3` 的白名单派发——
白名单只含内置与已注册模块，不接受任意 atom。

### 8.3 内置 test（v1）

`defined` `undefined` `none` `boolean` `false` `true` `integer` `float` `number`
`string` `sequence` `mapping` `iterable` `callable` `sameas` `escaped`
`in` `eq`/`==` `ne`/`!=` `lt` `le` `gt` `ge` `odd` `even` `divisibleby`
`lower` `upper` `filter` `test`

### 8.4 自定义 filter / test

沿用 mustache 扩展 behaviour 的形态（[06](06-parse-transform.md)），但简化：
filter 不需要参与 codegen，只需要是一个普通函数。

```erlang
-module(my_filters).
-behaviour(ai_jinja_ext).
-export([filters/0, tests/0, money/2]).

filters() -> #{money => {?MODULE, money, 2}}.
tests()   -> #{}.

money(V, _Args) -> ...
```

配置 `{jinja_opts, [{extensions, [my_filters]}]}`。compiler 在编译期读 `filters/0`
并把模块加入 `check_remotes/3` 白名单（[08 §5](08-jinja-architecture.md) 不变量 8）。
名字冲突（用户 filter 与内置同名）→ 编译错误，不静默覆盖。

## 9. 与 CPython Jinja2 的差异清单

**每一条都是刻意的，且必须在 README 与 `docs/jinja.md` 中原样列出。**

| # | 差异 | 理由 |
|---|---|---|
| J1 | map key 是 `atom()` 而非字符串 | 沿用 mustache 侧决定，避免热路径上的 binary key 比较 |
| J2 | `none` 与 undefined 合并 | §3。后果：`x is defined` 对显式 `none` 返回 `false` |
| J3 | 不支持链式比较 `1 < x < 3` | 直接报错而非给出错误答案（[09 §3.2](09-jinja-syntax.md)） |
| J4 | 未知 filter / test 是**编译错误** | §8.1。比参照实现严格 |
| J5 | 定界符不可配置 | [09 §1](09-jinja-syntax.md) |
| J6 | 无 `{% autoescape %}` 块 | §4.1 |
| J7 | 无 `{% trans %}` / `{% debug %}` / `loopcontrols` | [09 §4.3](09-jinja-syntax.md) |
| J8 | `trim_blocks` / `lstrip_blocks` 默认**开** | 参照实现默认关，但那个默认让绝大多数模板输出里塞满空行，几乎所有真实项目都会打开 |
| J9 | 迭代 `binary()` 报错 | §6 |
| J10 | 标识符只支持 ASCII | 非 ASCII 名字需 `binary_to_atom/2`，是 atom 表攻击面 |
| J11 | `extends` / `include` / `import` 的目标必须是字面量 | 编译期解析成模块名是「跨模块调用」方案的前提。动态目标见 [11 §5.3](11-jinja-codegen.md) |
| J12 | 无沙箱（`SandboxedEnvironment`） | 模板编译成 Erlang 代码，沙箱要在 codegen 层做，不在 v1 范围 |
| J13 | 属性访问不回退到方法调用 | Erlang 的 map 没有方法。`.items()` `.keys()` `.values()` 是特例化的三个 |
| J14 | undefined 可链式：`a.b.c` 全程不抛错，渲染为空 | 等价于参照实现的 `ChainableUndefined` 而非默认的 `Undefined`。默认那个在属性访问时抛错，对模板作者极不友好；`strict_undefined` 选项提供严格模式 |
| J15 | 不支持嵌套解构 `{% for (a, b), c in xs %}` | 平铺解构 `{% for k, v in ... %}` 覆盖真实用法的绝大多数，嵌套形式要让 for 的 pattern 生成变成一棵树 |
| J16 | `{% set ns.x = ... %}`（namespace 赋值）不支持 | 见 §2.5。Erlang 没有可变容器，支持它要让含 namespace 赋值的 for 从列表推导降级为 foldl。v1 报编译错误 |

## 10. 与 mustache 语义的对照

同一个仓库里有两套模板语义，最容易出的事故是「按 mustache 的直觉写 Jinja」。
下表放进 README：

| | mustache | Jinja2 |
|---|---|---|
| 变量解析 | 动态回溯 context stack | 词法作用域 |
| `0` / `#{}` | 真 | 假 |
| 缺失变量 | 空串 | 空串（`strict_undefined` 可改） |
| 转义 | 默认转义，`{{{x}}}` 关 | 默认转义，`\|safe` 关 |
| `true` 的输出 | `true` | `True` |
| 列表的输出 | 当作 chardata | Python `repr`：`[1, 2]` |
| 复用 | `{{> p}}` 单一机制 | include / extends / macro / import 四种 |
| 逻辑 | 只有真值判断 | 完整表达式 |
