# 04 组件移植约定

把 sigil（`~/workspace/sigil`，ClojureScript + jQuery）的核心组件移植为 aihtml 的 Erlang 预制件。本文是所有组件组共同遵守的约定。

## 范围

核心组件的判定标准有两条：
- 通用、常用。
- 不依赖大型 npm 包，适合"服务端渲染 + jQuery 增强"。

共 110 个，分为 26 组，分三批移植。每组一个 Erlang 模块、一个 JS 文件、一个补充 CSS 文件和一个测试模块：

| 组 | 模块 | 组件（函数名） |
|---|---|---|
| form_buttons | `aihtml_form_buttons` | button, button_group, link_button, toggle_button, dropdown_button, split_button, segmented_control |
| form_choice | `aihtml_form_choice` | checkbox, checkbox_group, radiobutton, radiobutton_group, radio_cards, switch_button, rating_group |
| form_text | `aihtml_form_text` | input, textarea, password_input, number_input, input_otp, tag_input |
| form_select | `aihtml_form_select` | dropdownlist, select（原生）, slider, form_layout（sigil 的 form）, field, validator 相关 |
| form_pickers | `aihtml_form_pickers` | datepicker, combobox |
| form_time_color | `aihtml_form_time_color` | timepicker, colorpicker |
| layout_basic | `aihtml_layout_basic` | card, panel, expander, tabs, tab_bar, breadcrumbs, pagination, steps, skeleton, loader, empty |
| layout_nav | `aihtml_layout_nav` | menu, navbar, sidenav, toolbar, splitter, listmenu, status_bar |
| overlay | `aihtml_overlay` | tooltip, popover, drawer, sheet, toast, notification, window |
| display | `aihtml_display` | avatar, badge, chip, aspect_ratio, kbd, time_ago, expandable_text, progressbar, progress_circle, meter, statistic, kpi_card, timeline, ranking_list, tag_cloud, alert |
| form_calendar | `aihtml_form_calendar` | calendar（事件日历）, datetime_input |
| form_lists | `aihtml_form_lists` | cascader, listbox, transfer |
| form_entry | `aihtml_form_entry` | masked_input, formatted_input, range_selector, repeat_button |
| form_upload | `aihtml_form_upload` | upload |
| data_tree | `aihtml_data_tree` | tree, nav_tree, diff, heatmap_calendar |
| layout_scroll | `aihtml_layout_scroll` | scrollview, scrollbar, responsive_panel |
| layout_bars | `aihtml_layout_bars` | activity_bar, navigationbar, command |
| layout_dnd | `aihtml_layout_dnd` | sortable, dragdrop |

| data_grid | `aihtml_data_grid` | datagrid |
| data_pivot | `aihtml_data_pivot` | pivotgrid |
| data_tables | `aihtml_data_tables` | treegrid, datatable |
| data_schedule | `aihtml_data_schedule` | gantt, scheduler, swimlane |
| data_charts | `aihtml_data_charts` | chart, area_chart, bar_chart, donut_chart, radar_chart, relation_graph |
| data_graph | `aihtml_data_graph` | node_graph |
| layout_dock | `aihtml_layout_dock` | docking, dock_layout |
| layout_tiles | `aihtml_layout_tiles` | ribbon, tile_layout |

第二批和第三批从一开始就按 [05-records.md](05-records.md) 的 record 方式实现。第三批的重型组件另有两条约定：
- **数据留在服务端**：大数据量组件支持本地和远程两种模式，远程模式下每次视图变化发一个 action，由服务端渲染新的一页（见 README「组件」一节）。
- **第三方库按需加载**：echarts、xlsx、jspdf 放在 `priv/static/vendor`，组件用 `AH.vendor(name)` 在需要时加载，不打包进 `aihtml.js`。

**暂不移植**：
- drawn：2.5 万行的手绘风白板引擎（含流程图、思维导图、插件体系），以后单独评估。
- 编辑器：rich_editor、prose_editor、markdown_editor。
- AG-UI 与 chat 组件。

## 文件与所有权

每组只写自己的文件，不改共享文件：

```
apps/aihtml/src/aihtml_<group>.erl          组件函数 + catalog/0 + examples/0
apps/aihtml/assets/js/components/<group>.js 行为
apps/aihtml/priv/css/extra/<group>.css      aihtml 需要的补充样式
apps/aihtml/test/aihtml_<group>_tests.erl   EUnit
```

- **sigil 样式已统一移植**到 `apps/aihtml/priv/css/sigil/components/*.css`，由 `scripts/port-sigil.mjs` 导入，`sigil-` 已改为 `ah-`。组件应输出与 sigil 相同的 DOM 结构和类名，这样这些样式能直接生效。必要时可以修改本组组件对应的 sigil css 文件，但要在报告里写明。
- **共享文件由集成者修改**：`aihtml.erl`（门面）、`aihtml.hrl`、`aihtml_catalog.erl`、`aihtml_html.erl`、`core.js`、`aihtml.css`、`components.css`。有需要请写进报告。
- **不要 include `aihtml.hrl`。** 集成后它会导入所有组件函数，与组内同名定义冲突。标签请用 `aihtml_html:el/4` 和 `aihtml_html:void/3`。

## Erlang 约定

**record**：每组按 [05-records.md](05-records.md) 在 `include/aihtml_<group>.hrl` 里定义 `ah_` 前缀的 record（头文件里的类型名要带本组前缀，如 `ah_nav_item()`），组件函数只负责构建 record，HTML 由 `render/1` 生成。

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

**Catalog**：`catalog() -> [entry()]`，格式见 `aihtml_catalog` 的类型说明。
- `name` 就是函数名。
- `category` 取 form | layout | overlay | data | media | text。
- `behavior` 是 `data-ah` 的值，没有行为时写 `none`。
- `events` 是组件触发的 DOM 事件。
- 组内调用写 `aihtml_catalog:classes(aihtml_catalog:entry(?MODULE, Name), Css)`，不要依赖全局查找。

**示例**：放在 `apps/aihtml_example/src/aihtml_example_demo_<group>.erl`，不放在库里。这些是演示站的内容，由 `/components/<name>` 展示，参照 `aihtml_example_demo_form_buttons.erl`：
- **包含头文件**：模块 `-include_lib("aihtml/include/aihtml.hrl")`，写法和应用代码一样。示例模块可以 include，因为它不定义组件函数。
- **`demos/0` 的格式**：返回 `[#{component, title, summary, demos}]`。
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

1. **当前值**写在根元素的 `data-ah-value` 上。多值用逗号分隔，例如 `a,b,c`。
2. **参与表单提交**时，渲染一个 `<input type="hidden" name=Name value=...>`，`name` 从 Attrs 取。
3. **值改变**时，行为同步更新 `data-ah-value` 和隐藏 input，并在根元素上触发 jQuery 事件 `change`；拖动等连续变化中触发 `input`。这样 `on(change, {M, A, Args})` 写在根元素的 Attrs 上就能收到事件，`Event.value` 取的就是 `data-ah-value`。
4. **原生控件**（checkbox、radio、input）直接把 Attrs 写到原生 `<input>` 上，保持原生事件。

## JS 约定

文件结构：

```js
(function ($, AH) {
  "use strict";
  AH.define("slider", {
    init: function (el, $el) { /* 绑定事件，用 AH.NS 命名空间 */ },
    destroy: function (el, $el) { /* 解绑文档级事件等 */ },
    methods: { setValue: function (el, $el, v) { ... } }
  });
  AH.fn("toast", function (opts) { ... });   // 页面级函数
})(window.jQuery, window.AH);
```

- **挂载**：根元素写 `data-ah="<behavior>"`，页面加载和 HTML 替换后由 `AH.mount` 调用 `init`。
- **事件命名空间**：元素上的事件用 `"click" + AH.NS` 这种形式，`AH.destroy` 会统一解绑。绑在 document 或 window 上的事件，要在 `destroy` 里自己解绑。
- **服务端驱动**：`methods` 里的方法可由服务端 `aihtml_action:call(Ctx, Target, Method, Args)` 调用，客户端用 `AH.invoke(el, method, ...)`。页面级函数用 `AH.fn`，服务端写 `call(Ctx, global, Name, Args)`。
- **还原 sigil 的交互**：键盘操作、ARIA、焦点管理、点击外部关闭等，对照 sigil 的 cljs 实现。
- **不引入新的依赖**，只用 jQuery 3.5+ 或 4 的 API。

## 验证

不要运行 rebar3，多个组并行时会争用 `_build` 锁。请用下面的命令：

```sh
cd /home/david/workspace/aihtml
OUT=/tmp/claude-1000/-home-david-workspace-aihtml/76c5fd47-5eb3-4cff-98d3-521ddcab594c/scratchpad/<group>
mkdir -p $OUT/ebin
FLAGS="+debug_info +warnings_as_errors +warn_missing_spec +warn_export_vars +warn_shadow_vars +warn_obsolete_guard"
PA="-pa _build/default/lib/aihtml/ebin -pa _build/default/lib/beamai_render/ebin"

erlc -o $OUT/ebin $PA $FLAGS apps/aihtml/src/aihtml_<group>.erl
erlc -o $OUT/ebin -pa $OUT/ebin $PA +debug_info apps/aihtml/test/aihtml_<group>_tests.erl
# 注意：多个 -pa 时，后出现的排在代码路径最前面，所以 _build 要用 -pz 放到最后，
# 否则会加载 _build 里旧的 beam，测到的不是刚编译的模块。
erl -noshell -pa $OUT/ebin -pz _build/default/lib/*/ebin \
    -eval 'case eunit:test(aihtml_<group>_tests) of ok -> halt(0); _ -> halt(1) end.'

node --check apps/aihtml/assets/js/components/<group>.js

# 预览：渲染 examples/0，构建 CSS/JS，截图并收集控制台错误
escript scripts/preview-group.escript aihtml_example_demo_<group> $OUT/page $OUT/ebin
node scripts/preview.mjs $OUT/page/index.html $OUT/shot.png --width=1200
node scripts/preview.mjs $OUT/page/index.html $OUT/dark.png --theme=dark
node scripts/preview.mjs $OUT/page/index.html $OUT/x.png --script=$OUT/interact.js  # 交互测试
```

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
- **浏览器测试**：`node scripts/test-js.mjs` 运行 `apps/aihtml/test/js/*.test.js`，可以添加本组的测试文件。
- **参考实现**：`tag_input` 的标签（`templates/tag_input_chip.mustache`）。

## 弹层定位

所有下拉、弹出、浮动面板都用 `AH.float(popup, anchor, opts)` 定位。它把弹层改为 `position: fixed` 并贴住锚点，所以卡片、面板、滚动区这类带 `overflow: hidden` 的祖先裁不到它。空间不足时它会翻转方向，并在页面滚动和窗口缩放时跟随锚点。

- **打开时**：调用它，并保存返回的句柄。
- **关闭或 destroy 时**：调用 `handle.stop()`。
- **选项**：`placement` 取 bottom、top、right、left；`align` 取 start、end、center；`offset` 是间距；`matchWidth` 让弹层至少和锚点一样宽，适合列表。
- **测试**：见 `apps/aihtml/test/js/float.test.js`。
