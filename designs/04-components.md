# 04 组件移植约定

把 sigil（`~/workspace/sigil`，ClojureScript + jQuery）的核心组件移植为 aihtml 的 Erlang 预制件。本文是所有组件共同遵守的约定。

## 范围

核心组件的判定标准有两条：
- 通用、常用。
- 不依赖大型 npm 包，适合"服务端渲染 + 浏览器端增强"。

共 112 个：从 sigil 分四批移植的 111 个，加上 aihtml 自己的 markdown_view（服务端渲染 Markdown，markdown_editor 的阅读端）。**每个组件一个 Erlang 模块** `aihtml_<name>`（`<name>` 就是组件名，也就是构建函数名），此外 `aihtml_theme` 提供主题切换器。按类别：

| 类别 | 组件 |
|---|---|
| 按钮 | button, link_button, toggle_button, button_group, segmented_control, dropdown_button, split_button, repeat_button |
| 选择 | checkbox, radiobutton, switch_button, checkbox_group, radiobutton_group, radio_cards, rating_group |
| 文本与录入 | input, textarea, password_input, number_input, input_otp, tag_input, masked_input, formatted_input, markdown_editor, markdown_view |
| 选择与表单 | dropdownlist, select（原生）, slider, range_selector, field, form_layout（sigil 的 form，校验用 `validate/1`） |
| 选择器与日期 | datepicker, combobox, timepicker, colorpicker, calendar（事件日历）, datetime_input |
| 列表与上传 | cascader, listbox, transfer, upload |
| 基础布局 | card, panel, expander, tabs, tab_bar, breadcrumbs, pagination, steps, skeleton, loader, empty |
| 导航与工具栏 | menu, navbar, sidenav, toolbar, splitter, listmenu, status_bar, activity_bar, navigationbar, nav_tree |
| 滚动与工作区 | scrollview, scrollbar, responsive_panel, sortable, dragdrop, docking, dock_layout, ribbon, tile_layout |
| 浮层 | tooltip, popover, drawer, sheet, toast, notification, window, command |
| 展示 | avatar, badge, chip, aspect_ratio, kbd, time_ago, expandable_text, alert, progressbar, progress_circle, meter, statistic, kpi_card, timeline, ranking_list, tag_cloud |
| 数据 | tree, diff, heatmap_calendar, datagrid, pivotgrid, treegrid, datatable, gantt, scheduler, swimlane, node_graph |
| 图表 | chart, area_chart, bar_chart, donut_chart, radar_chart, relation_graph |

组件的目录顺序（演示站导航、首页卡片的顺序）由 `aihtml_catalog` 的 `?COMPONENTS` 决定。

所有组件都按 [05-records.md](05-records.md) 的 record 方式实现。重型组件另有两条约定：
- **数据留在服务端**：大数据量组件支持本地和远程两种模式，远程模式下每次视图变化发一个 action，由服务端渲染新的一页（见 README「组件」一节）。
- **第三方库按需加载**：echarts、xlsx、jspdf 放在 `priv/static/vendor`，组件用 `AH.vendor(name)` 在需要时加载，不打包进 `aihtml.js`。

**暂不移植**：
- drawn：2.5 万行的手绘风白板引擎（含流程图、思维导图、插件体系），以后单独评估。
- 编辑器：rich_editor、prose_editor（markdown_editor 已移植，复用了 prose_editor 的核心）。
- AG-UI 与 chat 组件。

## 文件与所有权

一个组件的全部文件都以它的名字命名，一一对应。以 button 为例：

```
apps/aihtml/src/aihtml_button.erl                     构建函数、render/1、fields/1、catalog/0（一条）、facade_extras/0（有辅助函数时）
apps/aihtml/include/aihtml_button.hrl                 record #ah_button{}（只有 record，字段类型引用模块导出的类型）
apps/aihtml/assets/js/components/button.js            行为（有时）
apps/aihtml/priv/css/extra/button.css                 aihtml 的补充样式（有时）
apps/aihtml/templates/button_*.mustache               共享模板（有时）
apps/aihtml/test/aihtml_button_tests.erl              EUnit
apps/aihtml/test/js/button.test.js                    浏览器测试（有时）
apps/aihtml_example/src/aihtml_example_demo_button.erl  演示
```

- **共享代码**：几个组件共用的代码放在内部模块 `aihtml_lib_<主题>.erl`，JS 放在 `components/_lib_<主题>.js`（挂在 `AH.lib.<主题>` 上；文件名以下划线开头，构建时排在组件文件之前），CSS 放在 `extra/lib_<主题>.css`。例如 `aihtml_lib_rrule`（calendar 与 scheduler 共用的重复规则展开）、`aihtml_lib_table`（treegrid 与 datatable 共用的列模型）、`_lib_chart.js`（六种图表共用的 echarts 加载、主题和缩放）。几行的小函数直接复制，不必抽出。组件之间也可以直接复用：例如 repeat_button 的 `render/1` 返回一个 `#ah_button{}`，由 `aihtml_button` 渲染成按钮。
- **多个组件共用的模板**在它们的 lib 模块里声明；只有一个组件用的模板跟着组件走。
- **演示共用的数据**放在 `apps/aihtml_example/src/aihtml_example_fixture_<主题>.erl`。
- **生成的文件不要手改**：`aihtml.erl` 的组件部分、`aihtml.hrl` 的导入、`aihtml_records.hrl` 由 `scripts/gen-facade.escript` 生成；`priv/css/extra/index.css` 由 `scripts/gen-css-index.mjs` 生成。
- **sigil 样式已统一移植**到 `apps/aihtml/priv/css/sigil/components/*.css`，由 `scripts/port-sigil.mjs` 导入，`sigil-` 已改为 `ah-`。组件应输出与 sigil 相同的 DOM 结构和类名，这样这些样式能直接生效；修正写在组件自己的 `extra/<name>.css` 里。
- **共享文件由集成者修改**：`aihtml_catalog.erl`（新组件要加进 `?COMPONENTS`）、`aihtml_html.erl`、`aihtml_element.erl`、`core.js`、`aihtml.css`、`components.css`。
- **组件模块不要 include `aihtml.hrl`**：它会导入所有组件函数，与模块里的同名定义冲突。标签请用 `aihtml_html:el/4` 和 `aihtml_html:void/3`；record 只 include 自己的头文件。

## Erlang 约定

**record**：按 [05-records.md](05-records.md) 在 `include/aihtml_<name>.hrl` 里定义 `#ah_<name>{}`。头文件不定义类型，字段类型引用组件模块（或 lib 模块）导出的类型，例如 `aihtml_button:variant()`。构建函数写 `aihtml_element:build(?MODULE, #ah_<name>{...}, Css, Attrs)`，`render/1` 里用 `aihtml_element:classes(?MODULE, R)` 取类名。

**签名**：最后两个参数固定为 `Css, Attrs`。

| 类型 | 签名 | 例子 |
|---|---|---|
| 取值控件 | `(Content 或 Items, Value, Css, Attrs)` | `button(Content, Value, Css, Attrs)`、`slider(Range, Value, Css, Attrs)`、`dropdownlist(Items, Value, Css, Attrs)` |
| 容器与展示 | `(Children 或 Content, Css, Attrs)` | `card(Children, Css, Attrs)`、`badge(Content, Css, Attrs)` |
| 无内容 | `(Css, Attrs)` | `loader(Css, Attrs)` |

**Css**：
- 原子是语义修饰符，由 catalog 的 `groups`、`flags` 校验，生成 `<root>-<mod>`。
- 类名不符合这个规则时，在 entry 的 `classes` 里映射。
- binary 是字面类名，通常是 Tailwind 类，追加在语义类后面。

**Attrs**：
- 默认是写到根元素或原生控件上的 HTML 属性。
- sigil 的非 HTML 属性（标题、图标、最大值……）作为组件选项，键名列在 entry 的 `options` 里，用 `aihtml_catalog:split_options(Entry, Attrs)` 取出。

**Catalog**：`catalog() -> [entry()]`，只有本组件的一条，格式见 `aihtml_catalog` 的类型说明。
- `name` 就是函数名。
- `category` 取 form | layout | overlay | data | media | text。
- `behavior` 是 `data-ah` 的值，没有行为时写 `none`。
- `events` 是组件触发的 DOM 事件。

**示例**：放在 `apps/aihtml_example/src/aihtml_example_demo_<name>.erl`，不放在库里。这些是演示站的内容，由 `/components/<name>` 展示，参照 `aihtml_example_demo_button.erl`：
- **包含头文件**：模块 `-include_lib("aihtml/include/aihtml.hrl")`，写法和应用代码一样。示例模块可以 include，因为它不定义组件函数。
- **`demos/0` 的格式**：返回 `[#{component, title, summary, demos}]`，只有本组件的一条。
  - `title`：站点上显示的名字，例如 `<<"RadioButton">>`。
  - `summary`：一句中文简介，用在首页卡片上。
  - `demos`：`[{中文小标题, 函数名}]`。
- **每个示例一个函数**：导出的无参函数，带 `-spec`，返回 `aihtml:html()`。文档页会在示例下方显示这个函数的源码（取自 debug_info），所以函数要短小、自成一体、像真实用法。注释不会出现在页面上，示例的意图靠小标题和函数名说明。
- **覆盖面**：主要变体、尺寸和状态分成几个示例，不要一个函数塞下所有情况。

**API 说明**：catalog 条目可以带两个可选键，文档页的 API 标签页会展示它们：
- `option_docs`：`#{选项或标志 => 说明}`。
- `methods`：`[#{name, args, doc}]`，列出 `aihtml_action:call/4` 和 `AH.invoke` 能调用的方法。

**转义**：文本一律按子节点渲染，`aihtml_html` 会转义。只有确定可信的 HTML 才用 `{safe, _}`。

## 取值控件的约定

非原生的取值控件包括 slider、rating、dropdownlist、segmented、tag_input 等，与 action 和表单的衔接方式如下：

1. **当前值**写在根元素的 `data-ah-value` 上。多值用逗号分隔，例如 `a,b,c`；值里的逗号和反斜杠用反斜杠转义（`\,`、`\\`），不含它们的值写法不变。两端用同一套函数：Erlang 的 `aihtml_value:join/1`、`split/1`，JS 的 `AH.lib.values.join/split`。单值控件（单选的 listbox、combobox、按钮组 radio 模式等）的值原样写出，不转义。
2. **参与表单提交**时，渲染一个 `<input type="hidden" name=Name value=...>`，`name` 从 Attrs 取。
3. **值改变**时，行为同步更新 `data-ah-value` 和隐藏 input，并在根元素上触发 `change` 事件；拖动等连续变化中触发 `input`。这样 `on(change, {M, A, Args})` 写在根元素的 Attrs 上就能收到事件，`Event.value` 取的就是 `data-ah-value`。
4. **原生控件**（checkbox、radio、input）直接把 Attrs 写到原生 `<input>` 上，保持原生事件。

## JS 约定

文件结构：

每个组件一个 ES 模块 `components/<name>.js`，由 Vite 打包成一个按需加载的代码块（见 [06-bundling.md](06-bundling.md)）。行为是原生 Stimulus 控制器，不用 jQuery；样板是 `components/rating_group.js` 和它的测试 `test/js/rating_group.test.js`：

```js
import AH from "../core.js";
import "./_lib_values.js";                // 用到的共享代码
import "virtual:ah-tpl/slider_marks";     // 用到的共享模板（注册到 AH.tpl）

AH.register("slider", class extends AH.Controller {
  setup() {                               // 元素进入页面时运行一次
    this.delegate("click", ".ah-slider-thumb", (e, thumb) => { ... });
    this.listen(document, "keydown", (e) => { ... });   // 卸载时自动解绑
  }
  teardown() { /* 元素真正离开页面时运行一次 */ }
  setValue(v) { ...; }                    // 公开方法：服务端 call/4 与 AH.invoke 可调用
  getValue() { ... }
  commit(v) { ...; this.fire("change"); } // 原生事件，详情放在 e.detail
});
AH.fn("toast", (opts) => { ... });        // 页面级函数
```

- **挂载**：根元素写 `data-ah="<behavior>"`，它就是 Stimulus 控制器。页面上出现这个组件时加载代码块并连接控制器。元素被移动（形变替换、`preserve()`）时控制器保留，`setup`/`teardown` 不会重复运行。
- **`AH.Controller` 提供的工具**：`this.listen(target, type, handler)`（卸载时自动解绑，document、window 上的也一样）；`this.delegate(type, selector, handler)`（委托监听，mouseenter/mouseleave 要用 mouseover/mouseout 代替）；`this.fire(type, detail)`（冒泡、可取消的原生 CustomEvent）；`this.signal`（给 fetch 用的 AbortSignal）。每个元素的状态放在控制器实例上；几个组件共享、按元素保存的状态放在 lib 里的 `WeakMap`。
- **事件**：组件之间、组件和服务端 action 之间都用原生事件，数据放在 `e.detail`；在文件头注释里写明每个事件的 detail 结构。
- **按需加载的条件**：构建时扫描组件文件得到。`AH.register("名字")`、`AH.fn("名字")` 自动识别；通过辅助函数注册、名字是算出来的行为，在文件里写 `// ah-define: 名字`；不靠 `data-ah` 根元素、而是作用于某个属性的文件（tooltip、浮层开关、表单校验），写 `// ah-load: 选择器`。`aihtml_tests` 会检查目录里的每个行为名都能在 JS 源码里找到。
- **服务端驱动**：控制器的公开方法可由服务端 `aihtml_action:call(Ctx, Target, Method, Args)` 调用，客户端用 `AH.invoke(el, method, ...)`；组件还没加载或还没连接时，调用会排队。页面级函数用 `AH.fn`，服务端写 `call(Ctx, global, Name, Args)`。
- **浏览器测试**：用原生事件（`T.fire(el, type, init)`、`T.key(el, "Enter")`），插入 fixture 后 `await T.ready(fx)`；检查卸载结果前要等 `setTimeout 0`（卸载推迟一个微任务执行）。每个组件至少测试主要交互、取值与 `change`、一个服务端可调用的方法，以及移除后重新插入仍能工作。
- **还原 sigil 的交互**：键盘操作、ARIA、焦点管理、点击外部关闭等，对照 sigil 的 cljs 实现。
- **不引入新的依赖**，只用浏览器的原生 API。

## 验证

单独验证一个组件（多个人并行时不要运行 rebar3，它们会争用 `_build` 锁）：

```sh
cd /home/david/workspace/aihtml
N=button; OUT=/tmp/aihtml-$N; mkdir -p $OUT/ebin
FLAGS="+debug_info +warnings_as_errors +warn_missing_spec +warn_export_vars +warn_shadow_vars +warn_obsolete_guard"
PA="-pa _build/default/lib/aihtml/ebin -pa _build/default/lib/beamai_render/ebin"

erlc -o $OUT/ebin -pa $OUT/ebin $PA -I apps/aihtml/include $FLAGS apps/aihtml/src/aihtml_$N.erl
# 私有的门面和 aihtml.hrl，能看到刚编译的模块；源码里的门面在集成时再生成
escript scripts/gen-facade.escript --out $OUT/facade $OUT/ebin
erlc -o $OUT/ebin $PA -I $OUT/facade/aihtml/include $OUT/facade/aihtml.erl
erlc -o $OUT/ebin -pa $OUT/ebin $PA -I apps/aihtml/include +debug_info apps/aihtml/test/aihtml_${N}_tests.erl
erlc -o $OUT/ebin -pa $OUT/ebin $PA -I $OUT/facade $FLAGS apps/aihtml_example/src/aihtml_example_demo_$N.erl
# 注意：多个 -pa 时，后出现的排在代码路径最前面，所以 _build 要用 -pz 放到最后，
# 否则会加载 _build 里旧的 beam，测到的不是刚编译的模块。
erl -noshell -pa $OUT/ebin -pz _build/default/lib/*/ebin \
    -eval 'case eunit:test(aihtml_'$N'_tests) of ok -> halt(0); _ -> halt(1) end.'

node --check apps/aihtml/assets/js/components/$N.js
node scripts/test-js.mjs "$N:"          # 过滤条件匹配测试名

# 预览：渲染演示，构建 CSS/JS，截图并收集控制台错误
node scripts/gen-css-index.mjs
escript scripts/preview-group.escript aihtml_example_demo_$N $OUT/page $OUT/ebin
node scripts/preview.mjs $OUT/page/index.html $OUT/shot.png --width=1200
node scripts/preview.mjs $OUT/page/index.html $OUT/dark.png --theme=dark
node scripts/preview.mjs $OUT/page/index.html $OUT/x.png --script=$OUT/interact.js  # 交互测试
```

接入之后，完整检查用 `rebar3 compile && escript scripts/gen-facade.escript && rebar3 eunit && rebar3 dialyzer && npm run build && npm test`。

`preview.mjs` 每次启动独立的无头 Chromium，并行运行不会互相干扰。截图用 Read 工具查看，并与 sigil 的外观对照。

## 浏览器端生成 HTML 的规则

借鉴 htmx 的思路：HTML 尽量由服务端生成。浏览器端自己生成 HTML 只限于"孤岛"组件，并且要用共享模板。按优先级处理：

1. **能由服务端渲染的，由服务端渲染。** 例如服务端搜索的结果列表。action 用 Erlang 组件函数渲染片段，用 `aihtml_action:html(Ctx, Target, Html, morph_inner | morph)` 形变替换进页面。
   - 形变替换保留存活节点，焦点、光标、滚动位置、弹层和组件状态都不受影响。
   - 所有替换方式都会按 id 恢复焦点。
   - 有状态的节点应当带稳定的 id，形变时按 id 匹配。
2. **必须在浏览器生成的，用共享 Mustache 模板。** 例如日期网格、客户端 toast、客户端新增的标签。模板放在 `apps/aihtml/templates/<name>.mustache`，名字以组件名开头。构建时同一份模板编译到两端：
   - **Erlang 端**：组件模块加上下面两行，再用 `aihtml_tpl:safe(tpl_<name>(Data))` 放进元素树。Data 的键是原子。
     ```erlang
     -compile({parse_transform, beamai_mustache_transform}).
     -mustache_template({tpl_<name>, "../templates/<name>.mustache"}).
     ```
   - **JS 端**：`AH.tpl.<name>(data)` 返回字符串，键是字符串。
   - **fixture**：每个模板配一个 `templates/<name>.fixtures.json`，是一个数据对象数组。`aihtml_tpl_tests` 用这些数据在两端渲染，要求字节相同。
   - **逻辑放在视图数据里**：模板没有逻辑，类名开关、日期计算等在构建视图数据时算好。
   - **不支持**：partial、自定义分隔符、lambda。文件末尾的一个换行不计入输出。
3. **单个外壳元素**（遮罩、弹层容器、提示气泡）可以继续用 `$('<div class="…">')` 创建，不必套模板。

验证方法：
- **两端一致**：`aihtml_tpl_tests` 可以和其它 EUnit 一样用 erlc 编译运行。
- **模板编译器**：`node scripts/mustache.test.mjs` 运行 Mustache 规范用例。
- **浏览器测试**：`node scripts/test-js.mjs` 运行 `apps/aihtml/test/js/*.test.js`，每个组件一个测试文件。
- **参考实现**：`tag_input` 的标签（`templates/tag_input_chip.mustache`）。

## 弹层定位

所有下拉、弹出、浮动面板都用 `AH.float(popup, anchor, opts)` 定位。它把弹层改为 `position: fixed` 并贴住锚点，所以卡片、面板、滚动区这类带 `overflow: hidden` 的祖先裁不到它。空间不足时它会翻转方向，并在页面滚动和窗口缩放时跟随锚点。

- **打开时**：调用它，并保存返回的句柄。
- **关闭或 destroy 时**：调用 `handle.stop()`。
- **选项**：`placement` 取 bottom、top、right、left；`align` 取 start、end、center；`offset` 是间距；`matchWidth` 让弹层至少和锚点一样宽，适合列表。
- **测试**：见 `apps/aihtml/test/js/float.test.js`。
