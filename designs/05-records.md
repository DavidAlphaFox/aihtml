# 05 元素 record

组件的数据模型改为带统一前缀 `ah_` 的 record，`button/4` 这类函数只是构建 record 的简写。本文定下 record 的格式、渲染分发、构建函数的兼容方式和迁移步骤。迁移先以按钮组为样板，再推广到全部组件；之后每个组件拆成了自己的模块 `aihtml_<name>`（见 [04-components.md](04-components.md)）。

## 动机

原来的组件函数一调用就生成 `#el{}` 元素树，选项和 HTML 属性混在同一个 Attrs 列表里：

- 选项名写错不报错。`split_options` 只取出目录登记过的键，`{fisrt_day, 1}` 会原样输出成 `fisrt-day="1"`。
- 修饰符的取值只在运行时校验，dialyzer 帮不上忙。
- 元素生成后就是 HTML 结构，不能再查看或修改"这个按钮的变体"。

借鉴 Nitrogen 的 `#button{text = ..., postback = ...}`，改用 record 以后：

- 字段名在编译期检查，`#ah_datepicker{fisrt_day = 1}` 编译失败。
- 字段带类型，`variant = primery` 这种错误 dialyzer 能查出来。
- 渲染之前元素是数据，可以模式匹配、修改字段、组合。
- 选项是字段，HTML 属性单独放在 `attrs` 字段里，两者在结构上分开。

## 两种写法，一个模型

```erlang
%% 构建函数：签名不变，返回 record
button(<<"Save">>, save, [primary, <<"mt-2">>], [{disabled, true}])

%% record：字段有名字，postback 回到当前模块
#ah_button{body = <<"Save">>, value = save, variant = primary,
           css = [<<"mt-2">>], disabled = true,
           postback = {save, #{id => Id}}}
```

两者得到同一个 `#ah_button{}`，由同一段代码渲染。嵌套布局仍然用 `'div'(Children, Css, Attrs)` 这类函数写，比 record 紧凑；选项多的组件直接写 record 更清楚。

## 公共字段

每个元素 record 都以 `?AH_BASE(Module)` 开头（`include/aihtml_element.hrl`）：

| 位置 | 字段 | 默认值 | 说明 |
|---|---|---|---|
| 2 | `module` | 组件模块，如 `aihtml_button` | 负责渲染的模块，必须导出 `render/1` |
| 3 | `id` | `undefined` | 根元素的 `id` |
| 4 | `css` | `[]` | 字面类名（binary），通常是 Tailwind 类；修饰符写在字段里，不写在这里 |
| 5 | `attrs` | `[]` | 根元素的 HTML 属性，写法与原来的 Attrs 相同 |
| 6 | `postback` | `undefined` | `Action`、`{Action, Args}` 或 `{Action, Args, OnOpts}` |
| 7 | `delegate` | `?MODULE` | postback 调用的 action 模块 |

**位置是约定的一部分。** 渲染器只看第 2 位的 `module`，`aihtml_element:base/1` 按位置读公共字段，所以组件自己的字段一律排在公共字段后面。

**`delegate` 的默认值是调用方模块。** 头文件里的 `?MODULE` 在引用它的模块里展开，所以在页面模块里写 `#ah_button{postback = {save, Args}}`，就等于原来的 `on(click, {?MODULE, save, Args})`。postback 绑定在组件的主事件上：按钮是 `click`，取值控件是 `change`，由各组件的渲染子句决定。没有主事件的组件如果设置了 postback，会报 `{aihtml, {no_postback_event, Name}}`。

## 组件 record

- **命名**：`ah_` 加组件名，例如 `#ah_button{}`、`#ah_split_button{}`。前缀避免与使用方自己的 record 冲突。
- **字段**：
  - 构建函数的位置参数：内容统一叫 `body`，列表项叫 `items`，值叫 `value`，链接叫 `href`。
  - 修饰符组：字段名就是目录里的组名，取值是组内的原子，默认值与目录一致；目录默认是 `none` 的，字段默认 `undefined`。
  - 标志：同名布尔字段，默认 `false`。
  - 选项：目录 `options` 里的每个键。
  - 组件读取的 HTML 属性（`disabled`、`name`）也改为字段，布尔的默认 `false`。
- **类型**：每个字段都写类型，修饰符字段写出所有取值。
- **头文件**：每个组件一个，`include/aihtml_<name>.hrl`，里面只有 record；字段类型引用组件模块导出的类型（如 `aihtml_button:variant()`），头文件自己不定义类型，所以全部头文件放在一起也不会重名。`include/aihtml_records.hrl` 由 `scripts/gen-facade.escript` 生成，汇总全部头文件；`aihtml.hrl` 引用它，因此页面模块 include `aihtml.hrl` 就能同时用构建函数和 record。组件模块自己不 include `aihtml.hrl`（会与导入的函数冲突），只 include 自己的头文件。

字段与目录一致由测试保证：每个组、标志、选项都要有同名字段，默认值与目录的默认一致。目录仍是文档页 API 表的来源。

## 渲染

`aihtml_html:render/1` 遇到第 1 位是原子、第 2 位也是原子、至少 7 个元素的元组，就调用 `Module:render(R)`，再渲染它的返回值。返回值可以是 `#el{}`、iodata，也可以是另一个组件的 record，所以组件之间能直接组合。`{safe, IoData}` 的第 2 位不是原子，不会被误判。

每个组件模块实现 `aihtml_element` 行为，导出 `render/1` 渲染自己的 record：

```erlang
-module(aihtml_button).
-behaviour(aihtml_element).
-include("aihtml_button.hrl").

button(Content, Value, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_button{body = Content, value = Value}, Css, Attrs).

render(#ah_button{} = B) ->
    aihtml_html:el(button, B#ah_button.body, aihtml_element:classes(?MODULE, B),
                   [[{type, button}], aihtml_element:root_attrs(B, click)]).
```

`aihtml_element` 提供构建函数和渲染子句常用的辅助函数：
- `build(?MODULE, R, Css, Attrs)`：把 Css 里的修饰符原子和 Attrs 里与字段同名的键填进 record（字段表取自模块的 `fields/1`，目录条目取自 `catalog/0`）。
- `classes(?MODULE, R)`：把修饰符字段和标志字段转成类名，并校验每个取值属于对应的组。
- `root_attrs(R, Event)`：`id`、postback 绑定和 `attrs` 字段，拼在组件自己的根属性后面。

组件不再在构建时生成 HTML，因此**字段取值在渲染时校验**，例如 `icon_position = middle` 在 `render/1` 里报错。修饰符名在构建函数解析 Css 时就会报错，字段名在编译期报错。

### 单个元素改换渲染方式

`module` 可以逐个元素改写：

```erlang
#ah_button{module = myapp_fancy_button, body = <<"Save">>}
```

`myapp_fancy_button:render/1` 可以先处理自己的部分，再调用 `aihtml_button:render/1`。

### 自定义组件

使用方的库可以用同样的方式定义组件，不需要在任何地方注册：

```erlang
-include_lib("aihtml/include/aihtml_element.hrl").
-record(myapp_card, {?AH_BASE(myapp_card), title, body = []}).

-behaviour(aihtml_element).
render(#myapp_card{title = T, body = B} = R) ->
    aihtml:'div'([aihtml:h3(T), B], [<<"card">> | R#myapp_card.css],
                 aihtml_element:root_attrs(R, click)).
```

## 构建函数

签名保持不变：`button(Content, Value, Css, Attrs)` 仍然可用，只是返回 record。它通过 `aihtml_element:build/5` 把参数填进 record：

- **Css**：原子按目录解析成组字段和标志字段，重复或冲突的修饰符照旧立即报错；binary 放进 `css` 字段。
- **Attrs**：原子键与 record 字段同名的，取出来放进字段，包括 `id`、`disabled`、`name` 和各个选项；其余留在 `attrs` 字段里原样输出。`module`、`css`、`attrs`、`postback`、`delegate` 不会从 Attrs 里取，postback 只能在 record 里写，因为构建函数里的 `?MODULE` 是组件模块而不是调用方。

**构建函数这条路仍然查不出选项拼写错误**：写错的键会被当成 HTML 属性。需要编译期检查的地方请直接写 record。

## 兼容性

- **输出的 HTML 不变。** 现有页面、JS 行为和 CSS 都不受影响。迁移时 10 组共 214 个演示逐一与迁移前的输出比对（自动生成的 id 归一化后），全部一致。
- **属性顺序的细微变化。** 演示和测试都没有遇到，但使用方可能碰到：
  - `id` 由 `root_attrs/2` 写出，排在使用方其它属性的最前面，不再保持它在 Attrs 里的位置。
  - Attrs 里与字段同名的原子键（如 `{disabled, true}`、`{hidden, true}`）会进入字段，由组件按自己的规则输出，而不是原样作为 HTML 属性输出。
  - 只识别原子键：`{<<"id">>, _}`、`{<<"name">>, _}` 这类 binary 键留在 `attrs` 里。
  - 值为 `undefined` 的键会被忽略，字段保持默认值。
- **HTML 属性与字段同名。** 例如 select 的修饰符组 `size` 与 HTML 的 `size` 属性同名：构建函数 `select/4` 会把 `{size, N}` 留作 HTML 属性；写 record 时 HTML 的 size 放进 `attrs`。
- **action、推送、形变替换不变。** `aihtml_action:html/3` 等函数本来就调用 `aihtml_html:render/1`，record 可以直接传入。
- **头文件带来编译期耦合。** 给组件加字段会改变元组大小，用旧头文件编译的模块在渲染时会报 `badrecord`。rebar3 升级依赖时会整体重新编译，一般没有问题；分开部署的 beam 或热升级会受影响。所以增删、调整字段都算作不兼容的改动。
- **错误出现的时机。** 修饰符名的错误仍在构建时报出；字段取值的错误从构建时移到了渲染时。

## 普通标签

`'div'` 等普通标签目前仍然直接生成 `#el{}`。计划是改为一个公共 record `#ah_el{tag, body}`（同样以 `?AH_BASE` 开头），让所有元素都是同一种数据。这一步要把属性校验从构建时移到渲染时，影响面大，放在组件迁移完成之后单独做。

## 迁移步骤

1. **基础设施与按钮组（已完成）**：`aihtml_element`（行为、`build/5`、辅助函数）、`aihtml_html` 的渲染分发、`aihtml_catalog:parse_css/2`、按钮组的头文件与 `render/1`、字段与目录一致的测试。
2. **其余 9 组（已完成）**：每组照按钮组的样板改，演示和测试的 HTML 输出不变。每组都在演示站加了一个"record 写法"示例。
3. **一个组件一个模块（已完成）**：原来的 26 个组模块拆成 110 个组件模块和若干 `aihtml_lib_*` 共享模块，头文件、JS、CSS、测试、演示都按组件一一对应；449 个演示的 HTML 与拆分前一致。
4. **普通标签**：`#ah_el{}`。
5. **文档（已完成）**：演示站 API 页加上 record 写法和字段表（`aihtml_example_records` 从 debug_info 读字段，从头文件读注释），README 的调用约定加上 record 写法。

## 样板：按钮组

| record | 主事件 | 字段（公共字段之外） |
|---|---|---|
| `ah_button` | click | body, value, variant, size, round, disabled, icon, img, icon_position |
| `ah_link_button` | click | body, href, variant, size, round, disabled |
| `ah_toggle_button` | change | body, value（布尔）, name, variant, size, round, disabled, icon, img, icon_position |
| `ah_button_group` | radio/checkbox 模式为 change，默认模式为 click | items, value, name, mode, orientation, shape, fill, disabled |
| `ah_segmented_control` | change | items, value, name, size, full_width, disabled |
| `ah_dropdown_button` | change | body, items, value, name, variant, size, rounded, auto_open, disabled |
| `ah_split_button` | click | body, items, value, name, variant, size, menu_align, disabled |
