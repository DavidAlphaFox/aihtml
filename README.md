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
- 主题借鉴 [sigil](../sigil) 的四轴设计：外观、配色、排版、外形。

## 仓库结构

| 路径 | 内容 |
|---|---|
| `apps/aihtml` | 类库本体，其它项目只依赖它 |
| `apps/aihtml/priv/css/aihtml.css` | 源样式：令牌、四轴、预制件，供使用方的 Tailwind 构建引入 |
| `apps/aihtml/priv/static` | 预构建产物：`aihtml.css`、`aihtml.js`、`vendor/jquery.min.js` |
| `apps/aihtml_cowboy` | cowboy 接入：action 端点、静态资源路由、整页回复 |
| `apps/aihtml_example` | cowboy 示例，`/` 是 action 模式，`/fetch` 是 URL 片段模式 |
| `designs/` | 设计文档 |

## 调用约定

所有构建函数都返回元素树，最后由 `aihtml:render/1` 输出 iodata。

| 形式 | 例子 |
|---|---|
| 通用标签 | `p(Children)`、`p(Children, Css, Attrs)`；`div` 是 Erlang 保留字，写作 `'div'` |
| 空元素 | `img(Css, Attrs)`、`hr(Css, Attrs)`、`br()` |
| 表单预制件 | `button(Content, Value, Css, Attrs)`、`checkbox/4`、`radio/4`、`switch/4`、`select(Options, Value, Css, Attrs)` |
| 其它预制件 | `input/3`、`textarea/3`、`field/4`、`card/3`、`alert/3`、`badge/3`、`tabs/4`、`theme_switcher/2` |
| 任意标签 | `aihtml:el(Tag, Children, Css, Attrs)`、`aihtml:void(Tag, Css, Attrs)` |

**Children**：binary、数字、原子和可打印字符串都作为文本转义输出；其它列表是子节点序列；`safe(IoData)` 原样输出。

**Css**：原子是预制件的语义修饰符，由 `aihtml_catalog` 校验，未知或冲突的修饰符会直接报错；binary 是字面 class，通常是 Tailwind 工具类，排在语义 class 之后。

**Attrs**：proplist 或 map，可以嵌套列表。`true` 输出布尔属性，`false`、`undefined` 会被省略，`aria_label` 写成 `aria-label`，`{data, #{k => v}}` 展开为 `data-k`。后出现的同名属性覆盖前面的，`class` 则累加。checkbox、radio、switch 的 Attrs 作用在内部的 `<input>` 上。

预制件清单、修饰符、选项和事件都在 `aihtml_catalog:prefabs/0` 中。

## 交互模型

服务端渲染全部 HTML，页面由普通 HTTP handler 输出。浏览器端有两种交互方式，都是无状态的。

### action 模式

参照 AG-UI：每个事件发一次 POST，响应是 SSE 事件流。服务端在请求之间不保存任何东西，状态全部在数据层。请求可以落到任意节点，负载均衡不需要粘性，服务器重启后已打开的页面照常可用。

- **绑定**：`on(Event, {Module, Action, Args})`，可加选项 `#{debounce => Ms, include => [选择器], confirm => 提问}`。
- **签名**：`{Module, Action, Args}` 用应用密钥做 HMAC 签名后写进 HTML。浏览器无法伪造 action，也改不了参数。Args 只签名不加密，页面能看到内容，所以只放 id 这类数据。
- **执行**：`Module:action(Action, Args, Event, Ctx)` 在请求进程中运行。只有声明了 `-behaviour(aihtml_action)` 的模块才能被调用。
- **Event**：包含元素 `id`、`value`、`checked`、`key`、所在表单的全部字段 `form`、`include` 指定的其它控件值 `values`、`data-*` 属性 `data`。
- **页面操作**：`aihtml_action:html/3,4`、`remove/2`、`attr/4`、`add_class/3`、`remove_class/3`、`set_value/3`、`focus/2`、`title/2`、`redirect/2`、`js/2`。操作先缓冲，action 返回时一起发送。`flush/1` 可以提前发送，用来先显示加载状态、再显示数据。
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
button(<<"更多">>, more, [outline],
       [fetch(get, <<"/items?page=2">>, <<"#items">>, #{swap => append})])
```

### 预制件行为

带 `data-ah` 的根元素在加载时挂载 jQuery 行为，例如 tabs 切换、alert 关闭。

## 四轴主题

| 轴 | `<html>` 属性 | 取值 | 只负责 |
|---|---|---|---|
| appearance | `data-theme` | light, dark | 中性色 |
| palette | `data-palette` | indigo, emerald, rose, amber | 强调色 |
| typography | `data-typography` | sans, serif, mono | 字体 |
| skin | `data-skin` | soft, sharp, pill, brutal | 圆角、边框、阴影 |

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
npm run build          # 复制 jQuery，构建 aihtml.css 与 example.css

rebar3 shell           # 启动示例：http://localhost:8080/ 与 /fetch
```

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
