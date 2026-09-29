# aihtml

用 Erlang 函数直接编写 HTML 页面。页面由"预制件"拼装而成，服务端输出完整的静态 HTML，浏览器端由 Stimulus 控制器增强，样式用 TailwindCSS。

按钮的点击直接由 Erlang 函数响应，每个事件一次无状态请求，响应按 [AG-UI](https://docs.ag-ui.com) 事件流返回：

```erlang
button(<<"删除">>, Id, [borderless], [on(click, {?MODULE, delete, #{id => Id}})])

%% 同一个按钮的 record 写法，postback 默认回到当前模块
#ah_button{body = <<"删除">>, variant = borderless, postback = {delete, #{id => Id}}}

action(delete, #{id := Id}, _Event, Ctx) ->
    ok = todo_db:delete(Id),
    aihtml_action:remove(Ctx, {id, [<<"todo-">>, integer_to_binary(Id)]}).
```

```erlang
-include_lib("aihtml/include/aihtml.hrl").

login() ->
    'div'([checkbox(<<"记住我">>, yes, [], [{name, remember}]),
           button(<<"保存">>, save, [primary, <<"mt-4">>], [{type, submit}])],
          [<<"flex flex-col gap-2">>], [{id, login}]).

%% aihtml:render(login()) -> iodata()
```

- 渲染依赖 [beamai_render](https://github.com/TTalkPro/beamai_render)：转义使用 `beamai_html_escape`，`{safe, iodata()}` 与 beamai_jinja 的安全标记一致，渲染结果可直接放进 Jinja 模板。
- 前端基础：Stimulus 3 与 Tailwind CSS v4，浏览器端代码用 Vite 打包（见 `designs/06-bundling.md`）。组件行为是原生 DOM 写的 Stimulus 控制器，库不依赖 jQuery。需要 OTP 27 以上，因为用到 OTP 自带的 `json` 模块。
- 组件与主题移植自 [sigil](../sigil)（MIT）：111 个组件，另有服务端渲染的 markdown_view，共 112 个；以及四轴主题（外观、配色、排版、外形）。

## 仓库结构

| 路径 | 内容 |
|---|---|
| `apps/aihtml` | 类库本体，其它项目只依赖它 |
| `apps/aihtml/src` | 核心模块（`aihtml`、`aihtml_html`、`aihtml_action`……），每个组件一个模块 `aihtml_<组件名>`，以及组件共用的 `aihtml_lib_*` |
| `apps/aihtml/include` | `aihtml.hrl`（导入全部构建函数和 record）、每个组件一个 record 头文件 `aihtml_<组件名>.hrl`、自定义组件用的 `aihtml_element.hrl` |
| `apps/aihtml/priv/css/aihtml.css` | 源样式：令牌、四轴、预制件，供使用方的 Tailwind 构建引入 |
| `apps/aihtml/priv/static` | 预构建产物（只派发这些）：`aihtml.css`、`js/`（Vite 打包的运行时：入口 + 每个组件一个代码块 + `manifest.json`），echarts、xlsx、jspdf 是按需加载的代码块，ProseMirror 在 markdown_editor 的代码块里（见「第三方库」）；`vendor/` 只放给页面脚本用的 jQuery |
| `apps/aihtml_cowboy` | cowboy 接入：action 端点、静态资源路由、整页回复 |
| `apps/aihtml/templates` | 共享 Mustache 模板，构建时同时编译为 Erlang 和 JS |
| `apps/aihtml/assets/js` | 浏览器端 TypeScript 源码（不派发）：入口 `main.ts`、运行时 `core.ts` 和 `runtime/`、各组件行为 `components/<组件名>.ts`（控制器类，共用部分在 `_lib_*.ts`），由 Vite（`vite.config.mjs`）打包到 `priv/static/js`；`npm run typecheck` 做类型检查 |
| `apps/aihtml_example` | cowboy 示例站：`/` 首页，`/components/:name` 组件文档（演示、代码、API），`/demo` 实时演示，`/fetch` URL 片段模式。组件示例 `aihtml_example_demo_*` 也在这里，不在库里 |
| `scripts/` | 构建与测试脚本：样式移植、JS 构建、模板编译器、门面生成、预览 |
| `designs/` | 设计文档 |

## 调用约定

所有构建函数都返回元素 record：普通标签返回公共的 `#ah_el{tag, body}`，组件返回各自的 record（见下文「record 写法」）。两者可以任意嵌套，最后由 `aihtml:render/1` 输出 iodata。元素在渲染前是普通数据，可以模式匹配和修改；属性名等错误在渲染时报出。

| 形式 | 例子 |
|---|---|
| 通用标签 | `p(Children)`、`p(Children, Css, Attrs)`；`div` 是 Erlang 保留字，写作 `'div'` |
| 空元素 | `img(Css, Attrs)`、`hr(Css, Attrs)`、`br()` |
| 取值组件 | `button(Content, Value, Css, Attrs)`、`dropdownlist(Items, Value, Css, Attrs)`、`datepicker(Value, Css, Attrs)` |
| 容器与展示 | `card(Children, Css, Attrs)`、`chip(Content, Css, Attrs)`、`loader(Css, Attrs)` |
| 任意标签 | `aihtml:el(Tag, Children, Css, Attrs)`、`aihtml:void(Tag, Css, Attrs)` |
| record 写法 | `#ah_el{tag = section, body = Children, css = Css, attrs = Attrs, id = x}`，空元素的 `body` 可以不写 |

**Children**：binary、数字、原子和可打印字符串都作为文本转义输出；其它列表是子节点序列；`safe(IoData)` 原样输出。

**Css**：原子是预制件的语义修饰符，由 `aihtml_catalog` 校验，未知或冲突的修饰符会直接报错；binary 是字面 class，通常是 Tailwind 工具类，排在语义 class 之后。

**Attrs**：proplist 或 map，可以嵌套列表。`true` 输出布尔属性，`false`、`undefined` 会被省略，`aria_label` 写成 `aria-label`，`{data, #{k => v}}` 展开为 `data-k`。后出现的同名属性覆盖前面的，`class` 则累加。包着原生控件的组件（checkbox、radio、switch、input、textarea、select 等），id、Attrs 和 postback 作用在内部的原生控件上，而不是外层容器；具体见各组件 API 页 record 上方的说明。

预制件清单、修饰符、选项、事件和方法都在 `aihtml_catalog:prefabs/0` 中。示例站的 `/components/:name` 为每个组件提供文档页，参照 sigil 的样式，分三个标签：
- **演示**：实时示例，下方附渲染它的 Erlang 函数源码。
- **代码**：该组件的全部示例函数。
- **API**：签名、record 字段、修饰符、选项、事件、方法、CSS 类名。

示例写在 `apps/aihtml_example/src/aihtml_example_demo_<组件名>.erl`，写法见 `designs/04-components.md`。

### record 写法

每个组件都有一个带 `ah_` 前缀的 record，`button/4` 这类函数只是构建它的简写。页面模块 include `aihtml.hrl` 后，两种写法可以混用：

```erlang
-include_lib("aihtml/include/aihtml.hrl").

%% 函数写法
button(<<"Save">>, save, [success, lg], [{disabled, true}])

%% record 写法：同一个元素
#ah_button{body = <<"Save">>, value = save, variant = success, size = lg,
           disabled = true}
```

record 写法适合选项多的组件，也便于在渲染前查看和修改元素：

- **编译期检查字段名**：`#ah_datepicker{fisrt_day = 1}` 编译失败。函数写法里写错的选项会被当成 HTML 属性输出，不报错。
- **修饰符是带类型的字段**：修饰符组（variant、size……）和标志（round、disabled……）是同名字段，dialyzer 能查出 `variant = primery`，渲染时也会校验取值。
- **没写的字段取默认值**，默认值与函数写法一致。

所有 record 都以同一组公共字段开头：

| 字段 | 说明 |
|---|---|
| `id` | 根元素的 id |
| `css` | 字面类名（binary），通常是 Tailwind 类；修饰符写在字段里，不写在这里 |
| `attrs` | HTML 属性，写法与函数写法的 Attrs 相同，可以放 `on/3`、`fetch/3` 的结果 |
| `postback` | `Action`、`{Action, Args}` 或 `{Action, Args, OnOpts}`，绑定在组件的主事件上 |
| `delegate` | postback 调用的 action 模块，默认是写这个 record 的模块 |
| `module` | 负责渲染的模块，默认是组件自己的模块，例如 `aihtml_button` |

`postback` 是 `on/3` 的简写。下面两种写法等价：

```erlang
#ah_button{body = <<"Save">>, postback = {save, #{id => Id}}}
button(<<"Save">>, undefined, [], [on(click, {?MODULE, save, #{id => Id}})])
```

主事件由组件决定：按钮是 `click`，取值控件是 `change`，drawer、window 等是 `'ah:close'`，没有主事件的组件（card、badge……）设置 postback 会报 `no_postback_event`。每个组件的字段、类型、默认值和主事件，见演示站文档页的 API 标签。

**自定义组件**：使用方的库可以用同样的方式定义组件，不需要注册。record 以 `?AH_BASE(渲染模块)` 开头，渲染模块实现 `aihtml_element` 行为：

```erlang
-include_lib("aihtml/include/aihtml_element.hrl").
-record(myapp_card, {?AH_BASE(myapp_card), title, body = []}).

-behaviour(aihtml_element).
render(#myapp_card{title = T, body = B} = R) ->
    aihtml:'div'([aihtml:h3(T), B], [<<"card">> | R#myapp_card.css],
                 aihtml_element:root_attrs(R, click)).
```

把 `module` 改成别的模块，还可以替换单个元素的渲染方式，例如 `#ah_button{module = myapp_fancy_button}`。

**注意**：
- 头文件定义了 record 的元组结构，组件增删字段后，使用方要重新编译。
- 字段取值的错误在渲染时报出；修饰符名写错在调用函数时报出，字段名写错在编译时报出。
- 函数写法里，Attrs 中与字段同名的原子键（如 `{disabled, true}`、`{name, x}`）会填进字段；`postback`、`delegate`、`module` 只能在 record 里写。

设计细节见 `designs/05-records.md`。

## 组件

从 sigil 移植了 111 个组件，加上 aihtml 自己的 markdown_view 共 112 个，每个组件一个模块 `aihtml_<组件名>`，文件组织和约定见 `designs/04-components.md`。按用途分类如下：

| 类别 | 组件 |
|---|---|
| 按钮 | button, button_group, link_button, toggle_button, dropdown_button, split_button, segmented_control |
| 选择 | checkbox, checkbox_group, radiobutton, radiobutton_group, radio_cards, switch_button, rating_group |
| 文本输入 | input, textarea, password_input, number_input, input_otp, tag_input, markdown_editor（所见即所得，基于 ProseMirror）, markdown_view（服务端把 Markdown 渲染成 HTML） |
| 选择与表单 | dropdownlist, select, slider, field, form_layout；校验用 `validate/1` |
| 选择器 | datepicker, combobox（支持服务端搜索）, timepicker, colorpicker |
| 基础布局 | card, panel, expander, tabs, tab_bar, breadcrumbs, pagination, steps, skeleton, loader, empty |
| 导航 | menu, navbar, sidenav, toolbar, splitter, listmenu, status_bar |
| 浮层 | tooltip, popover, drawer, sheet, window, notification；`toast/3`、`opens/1` 等触发器 |
| 展示 | avatar, badge, chip, aspect_ratio, kbd, time_ago, expandable_text, progressbar, progress_circle, meter, statistic, kpi_card, timeline, ranking_list, tag_cloud, alert |
| 日历 | calendar（事件日历：月、周、日、日程视图，可拖动）, datetime_input |
| 列表选择 | cascader（下一级可由服务端懒加载）, listbox（支持服务端搜索）, transfer |
| 录入 | masked_input, formatted_input（进制输入）, range_selector, repeat_button |
| 上传 | upload（XHR 上传到指定 URL，带进度；无 URL 时走原生表单提交） |
| 树与数据 | tree（子节点可由服务端懒加载）, nav_tree, diff（Erlang 计算差异）, heatmap_calendar |
| 滚动与响应式 | scrollview（翻页轮播）, scrollbar, responsive_panel |
| 工具栏与命令 | activity_bar, navigationbar, command（命令面板，支持服务端搜索） |
| 拖放 | sortable, dragdrop |
| 数据网格 | datagrid（排序、筛选、分页、分组、编辑、列固定、导出） |
| 透视表 | pivotgrid（Erlang 汇总，可拖动字段重新透视，导出 Excel） |
| 表格 | treegrid（子节点可懒加载）, datatable（行详情、高级筛选、编辑） |
| 日程 | gantt, scheduler（日、周、月、时间轴、日程视图，按资源分列）, swimlane（跨职能流程图） |
| 图表 | chart（任意 echarts 配置）, area_chart, bar_chart, donut_chart, radar_chart, relation_graph |
| 节点图 | node_graph（节点编辑器：连线、分组、撤销重做，Erlang 自动布局） |
| 停靠布局 | docking, dock_layout（IDE 式分栏、标签组、浮动、自动隐藏） |
| 功能区与分栏 | ribbon, tile_layout（可拖分隔条、标签组的分栏布局） |

- **取值组件**：自定义控件把当前值写在根元素的 `data-ah-value`，用隐藏 input 参与表单，值改变时在根元素上触发 `change`。所以 `on(change, {M, A, Args})` 写在组件的 Attrs 里就能收到事件，`Event.value` 就是这个值。
- **多个值**（多选的 combobox、listbox、transfer、checkbox_group，datagrid 的选中行，sortable 的顺序等）用逗号连接；值本身的逗号和反斜杠写成 `\,`、`\\`。服务端用 `aihtml_value:split(Event.value)` 拆开、`aihtml_value:join(List)` 拼接，浏览器端对应 `AH.lib.values.split/join`；不含逗号的值写法与原来相同。
- **服务端驱动**：action 里用 `aihtml_action:call(Ctx, Target, Method, Args)` 调用组件方法，例如打开抽屉、设置进度；也可以用 `aihtml_toast:toast(Ctx, Msg, Opts)`、`aihtml_lib_overlay:open(Ctx, Target)` 等封装。
- **辅助函数**：组件之外的函数也由 `aihtml` 门面导出，include `aihtml.hrl` 后可直接调用。前几个在 action 里用：需要服务端补数据的组件，由 action 调用它们回应，它们在服务端渲染 HTML，再形变替换进组件，浏览器不拼 HTML。后几个生成属性，拼进元素的 Attrs：

  | 函数 | 用途 |
  |---|---|
  | `set_items/3,4` | combobox 的 `search` 选项：回填搜索结果 |
  | `listbox_items/3,4` | listbox 的 `search` 选项：回填搜索结果 |
  | `cascader_children/3,4` | cascader 的 `load` 选项：回填懒加载的下一级 |
  | `set_children/3` | tree 的 `load` 选项：回填懒加载的子节点 |
  | `set_command_items/3` | command 的 `search` 选项：回填命令列表 |
  | `set_events/3`、`add_event/3` | calendar：按当前可见范围回填或追加事件 |
  | `uploaded_files/1` | upload 的 `change` 事件：解码已上传文件列表 |
  | `validate/1` | 浏览器端表单校验属性，拼进控件的 Attrs，例如 `validate([required, email])`；校验不过时表单不会提交 |
  | `tooltip_attrs/2`、`opens/1`、`closes/0,1,2`、`toggles/1`、`shows_toast/2` | 浮层的触发属性，拼进任意元素的 Attrs |
  | `draggable_attrs/2`、`drop_zone_attrs/2` | dragdrop 的可拖元素和放置区属性 |
  | `datagrid_query/1`、`datagrid_rows/4`、`datagrid_row/3`、`datagrid_select/2` | datagrid 远程模式：读取查询、回填一页、重绘一行、在内存数据上执行查询 |
  | `datatable_query/1`、`datatable_rows/3`、`datatable_row/4`、`treegrid_children/3` | datatable 远程模式和编辑回应；treegrid 懒加载 |
  | `pivotgrid_view/1`、`pivotgrid_rows/3`、`pivotgrid_cell/1` | pivotgrid 远程模式和单元格点击 |
  | `scheduler_range/1`、`scheduler_update/3`、`gantt_update/3`、`swimlane_update/3` | scheduler 切换日期范围；三者编辑后重绘 |
  | `chart_update/3`、`chart_option/1` | 更新已有图表的数据或配置，不重新渲染 |
  | `set_node_graph/3`、`node_graph_layout/1` | 替换节点图；在服务端自动布局 |
  | `docking_add_window/4`、`dock_layout_open/3,4` | 往停靠布局里加窗口、打开面板 |

  ```erlang
  %% 树的某个节点展开时加载子节点：tree(Items, undefined, [], [{load, {?MODULE, children, #{}}}])
  %% 展开的节点的值在 Event 的 data 里（节点的 data-value 属性）
  action(children, _Args, #{data := #{<<"value">> := Parent}} = Event, Ctx) ->
      set_children(Ctx, Event, [{C, C} || C <- db:children(Parent)]).
  ```
- **大数据量组件**（datagrid、datatable、pivotgrid、scheduler 等）支持两种模式，服务端都不保存视图状态：
  - **本地模式**：服务端一次渲染出全部数据，排序、筛选、分页、分组由浏览器完成。
  - **远程模式**：给出 `source` 选项（一个 action），每次视图变化（排序、筛选、翻页、换日期范围）都发这个 action，查询条件在 `Event` 里；action 用上表的辅助函数回应，由服务端渲染新的一页再形变替换进页面。
  - 布局类组件（dock_layout、tile_layout、node_graph）把用户调整后的布局以 JSON 写在 `data-ah-value` 并触发 `change`，服务端保存后，下次用这个 JSON 渲染出同样的布局。

  ```erlang
  datagrid(Columns, FirstPage, [pageable], [{source, {?MODULE, orders, #{}}}, {total, Total}])

  action(orders, _Args, Event, Ctx) ->
      #{offset := Off, limit := Lim, sort := Sort, filters := F} = datagrid_query(Event),
      {Rows, Total} = orders_db:page(Off, Lim, Sort, F),
      datagrid_rows(Ctx, Event, Rows, Total).
  ```
- **样式**：sigil 的样式由 `scripts/port-sigil.mjs` 导入到 `priv/css/sigil`，前缀由 `sigil-` 改为 `ah-`，并保留 MIT 声明。
- **门面**：`aihtml` 的组件函数、`aihtml.hrl` 的导入和 `aihtml_records.hrl` 由 `scripts/gen-facade.escript` 从各组件模块生成，包括构建函数和各模块 `facade_extras/0` 列出的辅助函数。新增组件时，把模块加进 `aihtml_catalog` 的 `?COMPONENTS`，再运行 `rebar3 compile && escript scripts/gen-facade.escript`。

## 交互模型

服务端渲染全部 HTML，页面由普通 HTTP handler 输出。浏览器端有两种交互方式，都是无状态的。

### action 模式

参照 AG-UI：每个事件发一次 POST，响应是 SSE 事件流。服务端在请求之间不保存任何东西，状态全部在数据层。请求可以落到任意节点，负载均衡不需要粘性，服务器重启后已打开的页面照常可用。

- **绑定**：`on(Event, {Module, Action, Args})`，可加选项 `#{debounce => Ms, include => [选择器], confirm => 提问}`，以及下面"请求协调与加载指示"一节的 `sync`、`sync_scope`、`indicator`、`disable`。
- **签名**：`{Module, Action, Args}` 用应用密钥做 HMAC 签名后写进 HTML。浏览器无法伪造 action，也改不了参数。Args 只签名不加密，页面能看到内容，所以只放 id 这类数据。
- **执行**：`Module:action(Action, Args, Event, Ctx)` 在请求进程中运行。只有声明了 `-behaviour(aihtml_action)` 的模块才能被调用。
- **Event**：包含元素 `id`、`value`、`checked`、`key`、所在表单的全部字段 `form`、`include` 指定的其它控件值 `values`、`data-*` 属性 `data`。
- **页面操作**：`aihtml_action:html/3,4`、`remove/2`、`attr/4`、`add_class/3`、`remove_class/3`、`set_value/3`、`focus/2`、`title/2`、`redirect/2`、`js/2`、`call/4`、`trigger/4`、`push_url/2`、`replace_url/2`。操作先缓冲，action 返回时一起发送。`flush/1` 可以提前发送，用来先显示加载状态、再显示数据。
- **事件流**：依次是 `RUN_STARTED`、若干 `CUSTOM "aihtml.ui"`（值为 DOM 操作列表）、`RUN_FINISHED`。action 崩溃时以 `RUN_ERROR` 结束，只记日志，不向浏览器泄露细节。
- **并发**：同一元素的 click、submit 在请求进行中会忽略重复触发。input、change 等事件以最新一次为准，旧请求会被取消。
- **授权**：认证与授权在 action 里做，请求可以从 `aihtml_action:meta(Ctx)` 取得，cowboy 下是 `#{req => Req}`。

```erlang
-module(todo_page).
-behaviour(aihtml_action).
-include_lib("aihtml/include/aihtml.hrl").
-export([init/2, action/4]).

init(Req, State) ->                       %% GET /，普通 cowboy handler
    {ok, aihtml_cowboy:reply(Req, view(todo_db:all()), #{title => <<"Todos">>}), State}.

action(toggle, #{id := Id}, #{checked := Done}, Ctx) ->
    ok = todo_db:set_done(Id, Done),
    aihtml_action:html(Ctx, {id, dom_id(Id)}, item(todo_db:get(Id)), outer).
```

**密钥**：在 aihtml 应用环境中配置 `secret`，至少 32 字节，所有节点相同：

```erlang
%% sys.config
[{aihtml, [{secret, <<"...至少 32 字节的随机值...">>}]}].
```

不配置时每个节点随机生成一个密钥，只适合开发环境：重启或换节点后，已打开页面的 action 会被拒绝，返回 403。

### 服务端推送

页面订阅主题，服务端向订阅者推送与 action 相同的 DOM 操作。推送同样不需要业务状态：

```erlang
%% 页面：这个列表跟随 todos 主题；断线重连后运行 refresh 补齐
ul(Items, [], [{id, todo_list},
               subscribe(todos, #{refresh => {?MODULE, refresh_todos, #{}}})])

%% 任意节点、任意进程，通常在 action 写完数据层之后
aihtml_push:publish(todos, fun(C) ->
    aihtml_action:html(C, {id, todo_list}, item(Todo), append)
end, #{except => Ctx})     %% 跳过发起者，它已经通过 action 响应更新过了
```

- **一条流**：每个页面只开一个 EventSource，访问 `GET /aihtml/events?t=...`，带上页面上所有主题的签名令牌。页面内容变化导致订阅集合改变时，自动重开。
- **跨节点分发**：连接进程加入 OTP `pg` 进程组，只负责转发，不保存业务状态。`pg` 覆盖所有已连接的节点，任意节点发布，全集群的订阅者都会收到。
- **至多一次送达**：断线期间的推送会丢失。EventSource 会自动重连，重连后运行订阅上的 `refresh` action，从数据层补齐。
- **主题**可以是任意纯数据，比如 `todos`、`{room, 42}`。主题同样签名，页面只能订阅服务端为它渲染的主题，所以按用户决定渲染哪些主题即可实现权限控制。主题名对页面可见。
- **需要运行 aihtml 应用**，由它启动 `pg` scope。把 `aihtml` 写进你的应用的 `applications` 列表即可。

### fetch 模式

`fetch/3,4` 生成的属性让元素请求开发者自己路由的 URL，返回的 HTML 片段替换到目标位置。适合已有 REST 路由的场景：

```erlang
button(<<"更多">>, more, [outlined],
       [fetch(get, <<"/items?page=2">>, <<"#items">>, #{swap => append})])
```

### 文件上传

文件内容不走 action：action 请求是 JSON，不适合传二进制。upload 组件有两种用法：
- **给出 `url`**：浏览器用 XHR 把每个文件 POST 到这个地址（multipart，字段名默认 `file`），显示进度。服务端返回的 JSON 构成组件的值；上传完成后根元素触发 `change`，所以 postback 能在普通 action 里拿到文件列表：

  ```erlang
  upload([], [], [{url, <<"/upload">>}, {name, files},
                  on(change, {?MODULE, uploaded, #{}})])

  action(uploaded, _Args, Event, Ctx) ->
      Files = uploaded_files(Event),        %% [#{<<"name">> => ..., <<"size">> => ...}]
      ...
  ```

  接收地址由应用自己路由和实现，参考演示站的 `aihtml_example_upload`（cowboy 的 `read_part` 读取 multipart）。
- **不给 `url`**：组件就是样式化的 `<input type="file">`，文件随所在表单按原生 multipart 提交。

### 预制件行为

带 `data-ah="<名字>"` 的根元素由同名的 Stimulus 控制器增强，例如 tabs 切换、alert 关闭。

- **按需加载**：页面只加载运行时入口（gzip 后约 23 KB）。页面上第一次出现某个组件时，才加载它的代码块；服务端之后插入的组件也一样，Stimulus 会自动连接，不需要手动挂载。
- **页面引入**：`aihtml_page` 读取打包产物的 `manifest.json`，写出 `<script type="module" src="/aihtml/js/main-<哈希>.js">`。静态资源不挂在 `/aihtml/` 时用 `assets` 选项指定路径；`js` 选项里的页面脚本会加上 `defer`，在运行时之后按顺序执行。
- **全局变量**：运行时在 `window.AH` 上。页面自己的脚本需要 jQuery 时，用 `aihtml_page` 的 `jquery` 选项单独引入（`priv/static/vendor/jquery.min.js`）。
- **事件是原生的**：组件和运行时派发的都是冒泡的原生事件（`change`、`ah:close`、`ah:theme` 等），附带的数据在 `e.detail` 里。页面脚本用 `addEventListener` 监听即可；用 jQuery 监听时，数据要从 `e.originalEvent.detail` 读取，而不是处理函数的第二个参数。服务端 `aihtml_action:trigger/4` 的 `Detail` 同样成为 `e.detail`。

### 替换方式与形变替换

action 的 `aihtml_action:html(Ctx, Target, Html, Swap)`、服务端推送、`fetch/4` 的 `swap` 选项，都用同一组替换方式：

| Swap | 效果 |
|---|---|
| `inner`（默认） | 替换目标的内容 |
| `outer` | 替换目标元素本身 |
| `append` / `prepend` | 在内容末尾或开头追加 |
| `morph` | 把目标元素本身形变为新 HTML，新 HTML 必须只有一个根元素 |
| `morph_inner` | 把目标的内容形变为新 HTML |
| `none` | 不改动页面 |

**形变替换**不拆掉旧节点，而是把现有 DOM 朝新 HTML 修补：
- **节点匹配**：子节点先按 id 匹配，没有 id 时按位置和标签匹配；属性逐个同步。
- **状态保留**：留下来的节点保持原样，焦点、光标、滚动位置、打开的弹层和组件状态都不受影响。
- **表单控件**：没有焦点的控件取服务端的新值；有焦点的控件保留用户正在输入的内容。
- **组件**：内部有变化的组件只在最外层重新初始化一次；新增节点会挂载行为，被删节点会先清理。

**焦点保留**对所有替换方式都生效：替换前记下焦点元素的 id 和选区，替换后按 id 找回并恢复。所以需要保持焦点的元素应当带稳定的 id。

典型用法是让服务端重新渲染整块区域，再形变替换回去。比如一个带搜索框的列表，用户边打字边刷新，输入框的焦点和光标都不会丢：

```erlang
%% 页面：搜索框和结果放在同一块里，都有稳定的 id
results(Query, Rows) ->
    'div'([input(Query, [], [{id, q}, {name, q},
                             on(input, {?MODULE, search, #{}}, #{debounce => 200})]),
           ul([li(R) || R <- Rows], [], [{id, rows}])],
          [], [{id, search_box}]).

action(search, _, #{value := Q}, Ctx) ->
    aihtml_action:html(Ctx, {id, search_box}, results(Q, db:search(Q)), morph).
```

浏览器端也可以直接调用：`AH.swap($("#box"), Html, "morph")`。

### 元素保留

带 `preserve()`（即 `data-ah-preserve`）且有 id 的元素，在任何替换方式下都不会被替换。新内容里出现同 id 的元素时，页面上已有的那个节点会被原样移到新位置，新的那份丢弃。

适用于：正在播放的视频、有未保存内容的编辑器、已经挂载且内部有状态的组件。浏览器支持 `moveBefore` 时用它移动，iframe 和媒体不会重新加载。

```erlang
'div'([video_player(Url), comments(Items)], [], [{id, player_box}])
%% 播放器本身：
'div'(Player, [], [{id, player}, preserve()])
```

### 过渡动画（settle）

普通替换（`inner`、`outer`、`append`、`prepend`）完成后有一个 20 毫秒的过渡期：
- **新加入的顶层元素**带 `ah-added` 类，替换目标带 `ah-settling` 类，过渡期结束后移除。
- **与旧元素 id 相同的元素**先沿用旧元素的 `class`、`style`、`width`、`height`，过渡期结束后才换成新值。所以两次渲染之间的样式变化会成为 CSS 过渡。
- 形变替换本来就在原节点上改属性，过渡自然生效。形变新增的节点同样带 `ah-added`。
- 组件根元素（`data-ah`）不参与，它们的行为在初始化时要读真实属性。

```css
.ah-added { opacity: 0; }
#status { transition: color 300ms, background-color 300ms; }
```

### 请求协调与加载指示

`on/3` 的这几个选项写成元素属性，作用于这个元素上绑定的所有 action：

| 选项 | 作用 |
|---|---|
| `sync => drop` | 同一元素的请求还在进行时忽略新的请求。click、submit 默认如此。 |
| `sync => replace` | 中止进行中的请求，发送新的。input、change、键盘事件默认如此。 |
| `sync => queue` | 等进行中的请求结束再发送。排队的只保留最新一个。 |
| `sync_scope => Selector` | 以最近的匹配祖先为单位协调，例如 `<<"form">>` 让整个表单共用一个队列。 |
| `indicator => Selector` | 请求期间给匹配元素加 `ah-request` 类。带 `ah-indicator` 类的元素只在此时显示。 |
| `disable => Selector` | 请求期间禁用匹配元素；原本就禁用的保持禁用。 |

选择器可以写 `this` 或 `<<"closest 选择器">>`。多个请求同时占用一个指示器时，要等全部结束它才消失。`fetch/4` 也支持 `indicator` 和 `disable`。

组件事件（`ah:` 开头，例如 datagrid 的 `ah:edit`、scheduler 的 `ah:event-change`）默认是 `drop`：上一次请求还没结束时，新的事件会被丢掉。用户可能很快连续编辑时，绑定这类事件要写 `sync => queue`，例如 `on('ah:edit', {?MODULE, save_cell, #{}}, #{sync => queue})`。

```erlang
button(<<"Save">>, save, [],
       [on(click, {?MODULE, save, #{}},
           #{indicator => <<"#saving">>, disable => <<"closest form">>})]),
span(<<"Saving…"/utf8>>, [<<"ah-indicator">>], [{id, saving}])
```

### 触发事件与浏览器历史

- **触发事件**：`aihtml_action:trigger(Ctx, Target | document, Event, Detail)` 在浏览器里触发一个会冒泡的 DOM 事件，`Detail` 以 JSON 传过去。页面脚本可以监听它，元素也可以用 `on('ah:saved', ...)` 把它接到别的 action 上。
- **浏览器历史**：`aihtml_action:push_url(Ctx, Url)` 和 `replace_url(Ctx, Url)` 写入浏览器历史，但不发起请求，这样地址栏和书签能对上刚显示的内容。用户前进或后退到这些条目时，页面会重新加载那个 URL，由服务端直接渲染。页面仍然是无状态的，前提是这些 URL 能渲染出对应的内容。

```erlang
action(page, #{n := N}, _Ev, Ctx) ->
    aihtml_action:html(Ctx, {id, list}, list_page(N), morph),
    aihtml_action:push_url(Ctx, [<<"/items?page=">>, integer_to_binary(N)]).
```

示例首页的"Load data"卡片演示了指示器、禁用、同一队列，以及 `?view=processes` 的历史记录。

### 浮动弹层

组件的下拉、弹出、浮动面板都用 `AH.float(popup, anchor, opts)` 定位。它用 `position: fixed` 贴住锚点，所以卡片、面板等带 `overflow: hidden` 的容器不会把弹层裁掉；空间不足时自动翻转，滚动和缩放时跟随锚点。自己写弹层时也用它：

```js
var h = AH.float(popup, button, { placement: "bottom", align: "start", matchWidth: true });
// 关闭时
h.stop();
```

## SEO

页面内容全部由服务端输出，搜索引擎不执行脚本也能读到。几处原本要靠脚本的地方也有服务端版本：

- **页面元信息**：`aihtml_page:render/2` 的选项写进 `<head>`：`description`、`robots`、`canonical`、`alternates`（`[{Lang, Url}]`，写成 hreflang 链接）、`og`（Open Graph，`#{title => ..., image => [...]}`，列表值写多次）、`meta`（其它 `<meta name>`，如 `twitter:card`）、`json_ld`（schema.org 结构化数据，一个 map 或 map 列表）。map 按键排序输出，JSON-LD 里的 `<` 写成 `\u003c`。
  ```erlang
  aihtml_page:render(Body, #{title => <<"订单"/utf8>>,
                             description => <<"本月订单一览"/utf8>>,
                             canonical => <<"https://example.com/orders">>,
                             og => #{title => <<"订单"/utf8>>, type => website},
                             json_ld => #{<<"@context">> => <<"https://schema.org">>, <<"@type">> => <<"WebPage">>}})
  ```
- **Markdown**：`markdown_view(Markdown, Css, Attrs)` 在服务端把 Markdown 渲染成 HTML（`aihtml_lib_markdown`，移植自 markdown-it，配置与 markdown_editor 相同，输出与浏览器端逐字节一致）。可以直接显示用户写的 Markdown：原始 HTML 当作文本，链接只保留 http、https、mailto 和相对地址。
- **图表**：各类图表在画布旁输出一份视觉隐藏的数据表（`<table>`，带标题、表头），图表容器的角色是 `figure`。搜索引擎和读屏软件读到的是数据本身。
- **导航链接**：pagination、datagrid、datatable、calendar、scheduler 有 `href` 选项（URL 模板），翻页、切换日期和视图的按钮变成真正的 `<a href>`：

  | 组件 | 模板里的占位符 |
  |---|---|
  | pagination | `{page}`、`{size}` |
  | datagrid、datatable | `{page}`、`{size}`、`{sort}`、`{search}` |
  | calendar、scheduler | `{date}`、`{view}` |

  页面处理函数从查询参数读出状态，渲染同样的组件，于是每个状态都有自己的 URL：爬虫能跟进，新标签页和刷新都能打开，没有脚本也能用。组件同时绑定了 `on(change, Action)` 时，普通点击在页面内处理（action 用 `html/4` 形变替换内容），浏览器把链接的 URL 推入历史，前进、后退、刷新都从服务端加载对应状态。演示站的 `/components/:name/state?...`（`aihtml_example_state`）就是这样的页面处理函数。
- **datagrid 远程模式**：首页的数据行总是在服务端渲染（`source` 模式下给出的行就是第一页），挂载时不再发请求，之后的排序、筛选、翻页才调用 action。

## 第三方库

图表、表格导出和 Markdown 编辑器用到几个较大的库，由 Vite 打包成各自的代码块，只在需要时加载：

| 库 | 用途 | 代码块（压缩后 / gzip） | 许可证 |
|---|---|---|---|
| echarts | chart、各类图表、relation_graph | `vendor-echarts`，1.1 MB / 360 KB | Apache-2.0 |
| xlsx（SheetJS） | datagrid、pivotgrid 导出 Excel | `vendor-xlsx`，415 KB / 135 KB | Apache-2.0 |
| jspdf、jspdf-autotable | datagrid 导出 PDF | `vendor-jspdf` 392 KB / 125 KB，`vendor-jspdf-autotable` 29 KB / 9 KB | MIT |
| ProseMirror、markdown-it | markdown_editor | 由 `markdown_editor.ts` 直接 import，和组件代码同在 `markdown_editor` 代码块，共 390 KB / 131 KB | MIT |

- **位置**：代码块和运行时的其他代码块一起在 `priv/static/js`（文件名带内容哈希），版本见 `package.json`。纯 Erlang 的使用方不需要运行 npm，也不需要额外配置路径。jsPDF 自己还会按需引入 html2canvas、canvg、dompurify（`vendor-html2canvas` 等），只在调用它的 `html()` 时才加载，表格导出用不到。
- **许可证**：代码块里不保留许可证注释；每次构建由 `vite.config.mjs` 生成 `priv/static/js/THIRD-PARTY-LICENSES.txt`，列出打包进去的每个 npm 包（名称、版本、许可证、所在代码块、许可证全文和 NOTICE），包括入口里的 Stimulus。
- **按需加载**：组件在第一次需要时用动态 `import()` 加载库：图表在挂载时加载 echarts，导出在点击时加载 xlsx 或 jspdf。ProseMirror 只有 markdown_editor 用，由它直接 `import`，页面上出现编辑器时随组件的代码块一起下载。没用到这些组件的页面不会下载它们。页面脚本用 `AH.vendor(name)` 取得同一份库，返回 Promise，每个页面只加载一次；传列表时按顺序返回列表：
  ```js
  AH.vendor("echarts").then(function (echarts) { ... });
  AH.vendor(["jspdf", "jspdf-autotable"]).then(function (libs) { ... });   // [{jsPDF, ...}, autoTable]
  ```
  可用的名字：`echarts`、`xlsx`、`jspdf`、`jspdf-autotable`（解析为 `autoTable(doc, options)` 函数）。库不再作为全局变量出现（`window.echarts` 等），页面上自己引入的同名全局变量也不会被使用。
- **jQuery**：运行时不用 jQuery。`npm run vendor` 只把 `jquery.min.js` 和它的许可证复制到 `priv/static/vendor`，供 `aihtml_page` 的 `jquery` 选项使用。
- **PDF 中的中文**：jsPDF 的默认字体不含中文，导出的 PDF 里中文显示不出来，和 sigil 相同。

## 共享模板

少数 HTML 必须在浏览器里生成，例如日期网格、客户端 toast、用户刚输入的标签。这类片段写成一份 Mustache 模板，构建时同时编译到两端，两边输出完全一致：

- **Erlang 端**：用 beamai_render 的编译期转换编译成模块函数，数据的键是原子。
- **浏览器端**：`scripts/mustache.mjs` 把模板编译成 `AH.tpl.<name>(data)`，没有运行时依赖，数据的键是字符串。

**1. 写模板**：`apps/aihtml/templates/my_badge.mustache`，名字以组件名开头。

```mustache
<span class="ah-chip" data-color="{{color}}">{{label}}{{#count}} <b>{{count}}</b>{{/count}}</span>
```

**2. 准备 fixture**：`apps/aihtml/templates/my_badge.fixtures.json`，列出几组有代表性的数据，测试会在两端分别渲染并比较。

```json
[{"color": "primary", "label": "Inbox", "count": 3},
 {"color": "error", "label": "<b>", "count": 0}]
```

**3. Erlang 端使用**：在组件模块里声明，并用 `aihtml_tpl:safe/1` 放进元素树。

```erlang
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_my_badge, "../templates/my_badge.mustache"}).

my_badge(Label, Count, Css, Attrs) ->
    aihtml_tpl:safe(tpl_my_badge(#{color => primary, label => Label, count => Count})).
```

**4. 浏览器端使用**：构建后直接调用。

```js
$list.append(AH.tpl.my_badge({ color: "primary", label: name, count: n }));
```

**规则**：
- **没有逻辑**：模板里没有逻辑，类名开关、日期计算等在两端构建视图数据时算好。
- **转义**：`{{x}}` 转义，`{{{x}}}` 原样输出。
- **真假值**：两端一致，`undefined`、`null`、`false`、空字符串和空列表为假，0 为真。
- **不支持**：partial、自定义分隔符、lambda，编译时报错。
- **末尾换行**：文件末尾的一个换行不计入输出。
- **改完模板要重新编译 Erlang 模块**：rebar3 看不到模块对模板文件的依赖，需要 `touch` 对应的 `.erl` 或 `rebar3 clean`。模块没有重新编译时，一致性测试会失败。

**先考虑服务端渲染**：能由服务端生成的片段，优先由服务端渲染再形变替换回页面，不需要模板。完整约定见 `designs/04-components.md`。

## 四轴主题

| 轴 | `<html>` 属性 | 取值 | 只负责 |
|---|---|---|---|
| appearance | `data-theme` | light、dark、paper | 中性色 |
| palette | `data-palette` | default、green、luxury、retro、arctic、nature、editorial、ember、dracula、midnight、brutal、brutal-blue、island、phoqus | 强调色 |
| typography | `data-typography` | default、serif、grotesk | 字体 |
| skin | `data-skin` | default、brutal、island、phoqus | 圆角、边框、阴影 |

服务端通过 `aihtml:page(Body, #{theme => #{...}})` 设置初始值。浏览器端用 `AH.theme.set(Axis, Value)` 切换，选择保存在 localStorage，首屏绘制前恢复。Tailwind 的 `bg-primary`、`text-muted`、`rounded-control` 等工具类同样跟随四轴变化。

## 作为依赖使用

```erlang
{deps, [{aihtml, {git_subdir, "https://github.com/DavidAlphaFox/aihtml.git",
                  {branch, "master"}, "apps/aihtml"}},
        %% 可选：cowboy 的 action 端点
        {aihtml_cowboy, {git_subdir, "https://github.com/DavidAlphaFox/aihtml.git",
                         {branch, "master"}, "apps/aihtml_cowboy"}}]}.
```

`aihtml_cowboy:routes/1` 提供三条路由：action 端点 `/aihtml/action`、推送流 `/aihtml/events`、静态资源 `/aihtml/[...]`。前两者默认只接受同源请求。推送流在 HTTP/1.1 下关闭了空闲超时，并每 25 秒发送一次心跳。

核心库不依赖 cowboy。其它服务器需要实现两个端点：
- **action 端点（POST）**
  1. 用 `aihtml_action:verify/1` 校验请求体里的 `action`。
  2. 用 `aihtml_action:execute/3` 执行，`emit` 函数把每个事件写成一行 `data: JSON` 的 SSE。
- **推送端点（GET，长连接）**
  1. 用 `aihtml_push:verify/1` 校验令牌。
  2. 用 `aihtml_push:join/1` 加入主题。
  3. 第一条事件发送 `aihtml.stream` 流 id。
  4. 之后把收到的 `{aihtml_push, Except, Json}` 写出，但 `Except` 等于自己的流 id 时跳过。

**静态资源**：不用 `aihtml_cowboy` 时，把 aihtml 的 `priv/static` 挂到 `/aihtml/`，cowboy 写法如下：

```erlang
{"/aihtml/[...]", cowboy_static, {priv_dir, aihtml, "static"}}
```

**样式**：只用预制件时直接用预构建的 `aihtml.css`。自己写 Tailwind 工具类时，在自己的入口 CSS 里引入源样式并扫描 Erlang 源码：

```css
@import "tailwindcss" source(none);
@import "../_build/default/lib/aihtml/priv/css/aihtml.css";
@source "../_build/default/lib/aihtml/src";
@source "../src";
```

Tailwind 按字面扫描 `.erl` 文件，所以 class 必须写成完整的字面量，不能在运行时拼接。

## 演示站

`apps/aihtml_example` 是一个完整的 cowboy 应用，同时承担三件事：组件文档、实时交互演示、部署示范。启动方法见"开发"一节的 `rebar3 shell`，端口默认 8080。

### 页面

| 路由 | 模块 | 内容 |
|---|---|---|
| `/` | `aihtml_example_home` | 首页：主视觉区和统计数字，按类别排列的组件卡片，特性介绍 |
| `/components` | `aihtml_example_docs` | 跳转到第一个组件 |
| `/components/:name` | `aihtml_example_docs` | 组件文档页，结构见下文 |
| `/demo` | `aihtml_example_actions` | 实时演示：计数器、实时输入、分步加载、服务端搜索、待办、服务端推送的时钟。`?view=processes` 或 `?view=system` 直接渲染对应的数据视图，供浏览器前进后退使用 |
| `/fetch` | `aihtml_example_page`、`aihtml_example_api`、`aihtml_example_views` | URL 片段模式的同类演示；片段接口是 `/counter`、`/greet`、`/todos`、`/todos/:id`、`/todos/:id/toggle` |
| `/upload` | `aihtml_example_upload` | upload 组件演示的上传接口：读取 multipart 请求，返回文件名、大小和类型的 JSON，不保存文件内容（默认上限 5 MB） |
| `/aihtml/action`、`/aihtml/events`、`/aihtml/[...]` | `aihtml_cowboy:routes/1` | action 端点、推送流、库的静态资源 |
| `/static/[...]` | cowboy_static | 演示站自己的样式 `example.css` |

所有页面共用 `aihtml_example_site:topbar/1` 顶栏，导航到组件、实时演示和片段模式。

### 组件文档页

左侧是按类别分组的导航，用库里的 `sidenav` 组件；右侧标题区有组件名、说明、签名和主题切换器，下面是三个标签页：

| 标签 | 内容 | 数据来源 |
|---|---|---|
| 演示 | 逐个实时渲染示例，每个示例下方附渲染它的 Erlang 函数 | 示例模块的 `demos/0`；函数源码由 `aihtml_example_source` 从 debug_info 取出、用 `erl_pp` 格式化并做语法高亮 |
| 代码 | 这个组件的全部示例函数 | 同上 |
| API | 签名、record 字段（类型、默认值、说明）、修饰符、选项说明、事件及绑定写法、方法、CSS 根类名、行为名 | 库的组件目录 `aihtml_catalog`（`option_docs`、`methods` 等字段）；record 的字段由 `aihtml_example_records` 从组模块的 debug_info 取出，说明取自头文件里 record 上方的注释 |

页面上的代码就是实际运行的代码，不需要另外维护一份文本。注释不在 debug_info 里，示例的意图靠小标题和函数名说明。

### 模块分工

| 模块 | 职责 |
|---|---|
| `aihtml_example_app`、`aihtml_example_sup` | 启动数据层、路由和时钟推送进程 |
| `aihtml_example_site` | 页面外壳、顶栏、组件分类、显示名和简介 |
| `aihtml_example_home` | 首页 |
| `aihtml_example_docs` | 组件文档页 |
| `aihtml_example_source` | 示例函数的源码提取和语法高亮 |
| `aihtml_example_records` | 组件 record 的字段、类型、默认值和说明，供 API 标签使用 |
| `aihtml_example_demos` | 示例注册表：按组件组找到 `aihtml_example_demo_<group>` 模块 |
| `aihtml_example_demo_<组件名>` | 示例模块，每个组件一个 |
| `aihtml_example_fixture_*` | 几个示例模块共用的演示数据和小函数 |
| `aihtml_example_actions` | `/demo` 页面及其 action |
| `aihtml_example_page`、`aihtml_example_api`、`aihtml_example_views` | `/fetch` 页面、片段接口和共用视图 |
| `aihtml_example_store` | Mnesia 数据层 |
| `aihtml_example_upload` | 上传演示的接收端 |
| `aihtml_example_clock` | 每秒向 `clock` 主题推送服务器时间，集群中只运行一份 |

演示内容只放在这个应用里。库只保留组件目录，也就是组件的 API 元数据。

### 添加组件示例

在组件对应的 `aihtml_example_demo_<组件名>.erl` 里做三件事：
1. 写一个导出的无参函数，返回 `aihtml:html()`，写法和应用代码一样。
2. 在 `demos/0` 里登记这个函数，并给出中文小标题。
3. 组件第一次出现时，还要给出站点上的显示名 `title` 和一句中文简介 `summary`，简介用在首页卡片上。

```erlang
-include_lib("aihtml/include/aihtml.hrl").

demos() ->
    [#{component => combobox, title => <<"ComboBox">>,
       summary => <<"可输入、可过滤的下拉选择，支持服务端搜索。"/utf8>>,
       demos => [{<<"服务端搜索"/utf8>>, combo_search}]}].

-spec combo_search() -> aihtml:html().
combo_search() ->
    combobox([], undefined, [<<"w-72">>],
             [{name, city}, {search, {?MODULE, search, #{}}}]).
```

示例需要服务端参与时，示例模块本身声明 `-behaviour(aihtml_action)` 并实现 `action/4`，文档页上就能直接操作：

```erlang
action(search, _Args, #{value := Query} = Event, Ctx) ->
    set_items(Ctx, Event, [C || C <- cities(), string:find(C, Query) =/= nomatch]).
```

新增组件时，`aihtml_example_demos` 会按命名（`aihtml_<name>` 对应 `aihtml_example_demo_<name>`）自动找到它的示例模块，不需要登记。

### 测试与样式

- **测试**：`aihtml_example_site_tests` 检查以下几项：
  - 每个组件都有示例。
  - 每个示例都能渲染，源码能提取和高亮。
  - 每个文档页都能渲染。
  - 首页链接到所有组件。
  - 每个组件的 API 页都展示了它的 record（theme_switcher 和 toast 没有 record）。
- **数据层测试**：`aihtml_example_store_tests` 覆盖数据层。
- **样式**：演示站的样式入口是 `apps/aihtml_example/assets/example.css`。它引入库的样式，扫描库和示例应用的 Erlang 源码生成 Tailwind 工具类，并包含首页主视觉、代码块高亮和 API 表格的少量样式。`npm run build` 会一起构建它。

### 数据层

示例用 Mnesia 保存计数器和待办，数据目录默认是 `_build/mnesia/<节点名>`。配置项在 `aihtml_example` 应用环境中：

| 配置 | 含义 |
|---|---|
| `db_storage` | `disc_copies`（默认，重启后保留）或 `ram_copies` |
| `db_join` | 已在运行的节点名；设置后本节点加入它的 Mnesia 集群并复制表 |

两节点集群，任意一个节点都能处理任意请求，数据与推送全集群共享：

```sh
S='<<"0123456789abcdef0123456789abcdef">>'   # 仅作示例，生产环境请用随机密钥
EBIN="-pa _build/default/lib/*/ebin"

erl -sname a -setcookie demo $EBIN -aihtml secret "$S" -aihtml_example port 8080 \
    -eval 'application:ensure_all_started(aihtml_example).'

erl -sname b -setcookie demo $EBIN -aihtml secret "$S" -aihtml_example port 8081 \
    -aihtml_example db_join "'a@$(hostname -s)'" \
    -eval 'application:ensure_all_started(aihtml_example).'
```

之后节点 a 重启时不需要 `db_join`，它的磁盘 schema 已经记录了集群成员，启动时会从 b 同步最新数据。

### 发布

`rebar.config` 里的 relx 配置把示例打成 release。mnesia 在 release 中是 `{mnesia, load}`：只加载、不自动启动。数据层会先创建磁盘 schema，再自己启动 mnesia，因为 Mnesia 运行时无法创建磁盘 schema。

```sh
rebar3 release          # 开发用，_build/default/rel/aihtml_example
rebar3 as prod tar      # 含 ERTS 的独立包，_build/prod/rel/aihtml_example/*.tar.gz
```

启动时由环境变量配置，替换规则见 `config/sys.config.src` 与 `config/vm.args.src`：

| 变量 | 含义 |
|---|---|
| `AIHTML_SECRET` | 必填，至少 32 字节，所有节点相同。缺失或过短时 aihtml 应用拒绝启动 |
| `PORT` | HTTP 端口，默认 8080 |
| `NODE_NAME` | 短节点名，默认 `aihtml_example` |
| `DB_STORAGE` | `disc_copies`（默认）或 `ram_copies` |
| `DB_JOIN` | 要加入的节点，例如 `aihtml_example@host1`；第一个节点留空 |
| `MNESIA_DIR` | Mnesia 目录，默认为 release 根目录下的 `data/mnesia` |

```sh
AIHTML_SECRET=... PORT=8080 bin/aihtml_example daemon
AIHTML_SECRET=... PORT=8081 NODE_NAME=web2 DB_JOIN=aihtml_example@host1 bin/aihtml_example daemon
```

vm.args 里没有写 cookie，VM 和启动脚本都使用 `~/.erlang.cookie`。组成集群的节点需要使用相同的 cookie 文件。

## 开发

```sh
rebar3 compile
rebar3 eunit --app aihtml
rebar3 dialyzer && rebar3 xref

npm install
npm run build          # 复制并打包第三方库，Vite 打包运行时到 priv/static/js，构建 aihtml.css 与 example.css
npm run js:dev         # 开发时：未压缩、带 source map，文件变化时重新打包
npm run typecheck      # TypeScript 类型检查（strict），并确认 types/catalog.d.ts 是最新的
npm test               # 模板编译器的 Mustache 规范用例 + 类型检查 + 浏览器端测试（无头 Chromium）

rebar3 shell           # 启动示例站：http://localhost:8080/（首页）、/components、/demo、/fetch
```

- **配置文件分两份**：`rebar3 shell` 读取 `config/shell.config`（普通 Erlang 配置）；release 读取 `config/sys.config.src`，其中的 `${VAR}` 只有 release 启动脚本会替换，rebar3 shell 读不了它。
- **新增或修改组件后**，重新生成门面和头文件：`rebar3 compile && escript scripts/gen-facade.escript`；目录里的 `methods` 变了，再运行 `npm run types:catalog` 重新生成 `types/catalog.d.ts`（`AH.register` 按它检查控制器类有没有这些方法）。
  - `--out Dir` 选项把门面和 `aihtml.hrl` 生成到 `Dir`，不改动源码。适合在组还没接入时单独验证它的示例。
- **导入 sigil 样式**：在 `scripts/port-sigil.mjs` 的清单里加上组件名，运行 `node scripts/port-sigil.mjs`。它只写入新文件，已有文件（可能改过）要加 `--force` 才会覆盖。
- **全部测试**：`rebar3 eunit` 跑类库和演示站；`--app aihtml` 只跑类库。
- **模板一致性**由 EUnit 的 `aihtml_tpl_tests` 检查，需要能调用 `node`。
- **浏览器端测试**放在 `apps/aihtml/test/js/*.test.js`，由 `scripts/test-js.mjs` 运行：它先打包一份运行时，用本地 HTTP 服务提供测试页（ES 模块不能从 `file://` 加载），加载全部组件后再运行测试。
- **打包产物要提交**：改了 `assets/js` 或模板后运行 `npm run js`，把 `priv/static/js` 一起提交，使用方不需要运行 npm。组件文件的按需加载条件由构建时扫描得到，写法见 `designs/04-components.md` 的「TypeScript 约定」。

## 许可证

Apache-2.0
