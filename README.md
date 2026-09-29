# aihtml

用 Erlang 函数直接编写 HTML 页面。页面由"预制件"拼装而成，预制件直接映射到 jQuery 行为和 TailwindCSS 样式上。

按钮的点击直接由 Erlang 函数响应，每个事件一次无状态请求，响应按 [AG-UI](https://docs.ag-ui.com) 事件流返回：

```erlang
button(<<"删除">>, Id, [ghost], [on(click, {?MODULE, delete, #{id => Id}})])

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
- 前端基础：jQuery 4 与 Tailwind CSS v4（Tailwind CLI 构建）。需要 OTP 27 以上，因为用到 OTP 自带的 `json` 模块。
- 组件与主题移植自 [sigil](../sigil)（MIT）：67 个核心组件，以及四轴主题（外观、配色、排版、外形）。

## 仓库结构

| 路径 | 内容 |
|---|---|
| `apps/aihtml` | 类库本体，其它项目只依赖它 |
| `apps/aihtml/priv/css/aihtml.css` | 源样式：令牌、四轴、预制件，供使用方的 Tailwind 构建引入 |
| `apps/aihtml/priv/static` | 预构建产物：`aihtml.css`、`aihtml.js`、`vendor/jquery.min.js` |
| `apps/aihtml_cowboy` | cowboy 接入：action 端点、静态资源路由、整页回复 |
| `apps/aihtml/templates` | 共享 Mustache 模板，构建时同时编译为 Erlang 和 JS |
| `apps/aihtml/assets/js` | 运行时 `core.js` 与各组件行为，由 `scripts/build-js.mjs` 拼成 `aihtml.js` |
| `apps/aihtml_example` | cowboy 示例站：`/` 首页，`/components/:name` 组件文档（演示、代码、API），`/demo` 实时演示，`/fetch` URL 片段模式。组件示例 `aihtml_example_demo_*` 也在这里，不在库里 |
| `scripts/` | 构建与测试脚本：样式移植、JS 构建、模板编译器、门面生成、预览 |
| `designs/` | 设计文档 |

## 调用约定

所有构建函数都返回元素树，最后由 `aihtml:render/1` 输出 iodata。

| 形式 | 例子 |
|---|---|
| 通用标签 | `p(Children)`、`p(Children, Css, Attrs)`；`div` 是 Erlang 保留字，写作 `'div'` |
| 空元素 | `img(Css, Attrs)`、`hr(Css, Attrs)`、`br()` |
| 取值组件 | `button(Content, Value, Css, Attrs)`、`dropdownlist(Items, Value, Css, Attrs)`、`datepicker(Value, Css, Attrs)` |
| 容器与展示 | `card(Children, Css, Attrs)`、`chip(Content, Css, Attrs)`、`loader(Css, Attrs)` |
| 任意标签 | `aihtml:el(Tag, Children, Css, Attrs)`、`aihtml:void(Tag, Css, Attrs)` |

**Children**：binary、数字、原子和可打印字符串都作为文本转义输出；其它列表是子节点序列；`safe(IoData)` 原样输出。

**Css**：原子是预制件的语义修饰符，由 `aihtml_catalog` 校验，未知或冲突的修饰符会直接报错；binary 是字面 class，通常是 Tailwind 工具类，排在语义 class 之后。

**Attrs**：proplist 或 map，可以嵌套列表。`true` 输出布尔属性，`false`、`undefined` 会被省略，`aria_label` 写成 `aria-label`，`{data, #{k => v}}` 展开为 `data-k`。后出现的同名属性覆盖前面的，`class` 则累加。checkbox、radio、switch 的 Attrs 作用在内部的 `<input>` 上。

预制件清单、修饰符、选项、事件和方法都在 `aihtml_catalog:prefabs/0` 中。示例站的 `/components/:name` 为每个组件提供文档页，参照 sigil 的样式，分三个标签：
- **演示**：实时示例，下方附渲染它的 Erlang 函数源码。
- **代码**：该组件的全部示例函数。
- **API**：签名、修饰符、选项、事件、方法、CSS 类名。

示例写在 `apps/aihtml_example/src/aihtml_example_demo_<group>.erl`，写法见 `designs/04-components.md`。

## 组件

从 sigil 移植了 67 个核心组件，分为 10 组，约定见 `designs/04-components.md`：

| 组 | 组件 |
|---|---|
| 按钮 | button, button_group, link_button, toggle_button, dropdown_button, split_button, segmented_control |
| 选择 | checkbox, checkbox_group, radiobutton, radiobutton_group, radio_cards, switch_button, rating_group |
| 文本输入 | input, textarea, password_input, number_input, input_otp, tag_input |
| 选择与表单 | dropdownlist, select, slider, field, form_layout；校验用 `validate/1` |
| 选择器 | datepicker, combobox（支持服务端搜索）, timepicker, colorpicker |
| 基础布局 | card, panel, expander, tabs, tab_bar, breadcrumbs, pagination, steps, skeleton, loader, empty |
| 导航 | menu, navbar, sidenav, toolbar, splitter, listmenu, status_bar |
| 浮层 | tooltip, popover, drawer, sheet, window, notification；`toast/3`、`opens/1` 等触发器 |
| 展示 | avatar, badge, chip, aspect_ratio, kbd, time_ago, expandable_text, progressbar, progress_circle, meter, statistic, kpi_card, timeline, ranking_list, tag_cloud, alert |

- **取值组件**：自定义控件把当前值写在根元素的 `data-ah-value`，用隐藏 input 参与表单，值改变时在根元素上触发 `change`。所以 `on(change, {M, A, Args})` 写在组件的 Attrs 里就能收到事件，`Event.value` 就是这个值。
- **服务端驱动**：action 里用 `aihtml_action:call(Ctx, Target, Method, Args)` 调用组件方法，例如打开抽屉、设置进度；也可以用 `aihtml_overlay:toast(Ctx, Msg, Opts)` 等封装。
- **样式**：sigil 的样式由 `scripts/port-sigil.mjs` 导入到 `priv/css/sigil`，前缀由 `sigil-` 改为 `ah-`，并保留 MIT 声明。
- **门面**：`aihtml` 的组件函数和 `aihtml.hrl` 的导入由 `scripts/gen-facade.escript` 从各组模块生成。新增组件后，运行 `rebar3 compile && escript scripts/gen-facade.escript`。

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

### 预制件行为

带 `data-ah` 的根元素在加载时挂载 jQuery 行为，例如 tabs 切换、alert 关闭。

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

## 开发

```sh
rebar3 compile
rebar3 eunit --app aihtml
rebar3 dialyzer && rebar3 xref

npm install
npm run build          # 复制 jQuery，编译模板并拼出 aihtml.js，构建 aihtml.css 与 example.css
npm test               # 模板编译器的 Mustache 规范用例 + 浏览器端测试（无头 Chromium）

rebar3 shell           # 启动示例站：http://localhost:8080/（首页）、/components、/demo、/fetch
```

- **配置文件分两份**：`rebar3 shell` 读取 `config/shell.config`（普通 Erlang 配置）；release 读取 `config/sys.config.src`，其中的 `${VAR}` 只有 release 启动脚本会替换，rebar3 shell 读不了它。
- **新增或修改组件后**，重新生成门面和头文件：`rebar3 compile && escript scripts/gen-facade.escript`。
- **模板一致性**由 EUnit 的 `aihtml_tpl_tests` 检查，需要能调用 `node`。
- **浏览器端测试**放在 `apps/aihtml/test/js/*.test.js`，由 `scripts/test-js.mjs` 运行。

### 示例的数据层

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

## 许可证

Apache-2.0
