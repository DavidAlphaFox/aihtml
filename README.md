# aihtml

用 Erlang 函数直接编写 HTML 页面。页面由"预制件"拼装而成，预制件直接映射到 jQuery 行为和 TailwindCSS 样式上。

参照 [CLOG](../clog)，按钮的点击可以直接由 Erlang 代码响应：

```erlang
button(<<"加载">>, load, [], [on(click, fun(Win, _Ev) ->
    aihtml_live:html(Win, {id, out}, load_rows())
end)])
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
- 前端基础：jQuery 4 与 Tailwind CSS v4（Tailwind CLI 构建）。需要 OTP 27 以上，live 模式使用 OTP 自带的 `json` 模块。
- 主题借鉴 [sigil](../sigil) 的四轴设计：外观、配色、排版、外形。

## 仓库结构

| 路径 | 内容 |
|---|---|
| `apps/aihtml` | 类库本体，其它项目只依赖它 |
| `apps/aihtml/priv/css/aihtml.css` | 源样式：令牌、四轴、预制件，供使用方的 Tailwind 构建引入 |
| `apps/aihtml/priv/static` | 预构建产物：`aihtml.css`、`aihtml.js`、`vendor/jquery.min.js` |
| `apps/aihtml_cowboy` | live 模式的 cowboy 传输层：启动页、WebSocket、静态资源路由 |
| `apps/aihtml_example` | cowboy 示例，`/` 是 live 模式，`/fetch` 是无状态片段模式 |
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

服务端渲染全部 HTML。浏览器端有两种模式，可以混用。

### live 模式（CLOG 风格）

每个浏览器窗口经 WebSocket 连到一个 Erlang 会话进程：

- **事件处理器写在 Attrs 里**：`on(Event, fun(Win, Ev) -> ... end)`，也可以加防抖，`on(input, Fun, #{debounce => 150})`。点击、输入、提交等事件发生时，这个 fun 在会话进程里执行。
- **Ev 带上常用数据**：元素 `id`、`value`、`checked`、`key`、所在表单的全部字段 `form`、`data-*` 属性 `data`。多数处理器不需要再向浏览器查询。
- **推送 DOM 操作**：`aihtml_live:html/3,4`、`remove/2`、`attr/4`、`add_class/3`、`set_value/3`、`focus/2`、`title/2`、`redirect/2`、`js/2`。一个处理器里的操作合并成一帧发送。
- **读取浏览器**：`aihtml_live:query(Win, <<"return window.innerWidth">>)` 同步返回 `{ok, Value}`，对应 CLOG 的 js-query。
- **服务端主动推送**：会话收到的普通消息交给可选回调 `handle_info/2`，例如定时器。其它进程也可以直接调用这些操作，它们会被转发到会话里执行。
- **窗口状态**放在会话进程里，可以用闭包或进程字典。
- **断线重连后恢复原会话**，与 CLOG 相同。断线后会话继续运行，默认保留 60 秒。页面带着会话令牌和最后应用的帧号重连，会话补发断线期间的帧。页面、处理器和状态都原样延续。只有超时、或缺失的帧已超出重放缓冲，才会重新开始。刷新页面总是开始一个新会话。

```erlang
-module(hello).
-behaviour(aihtml_live).
-include_lib("aihtml/include/aihtml.hrl").
-export([mount/2]).

mount(Win, _Params) ->
    aihtml_live:render(Win,
        'div'([button(<<"点我">>, go, [],
                      [on(click, fun(W, _) -> aihtml_live:html(W, {id, out}, <<"来自 Erlang">>) end)]),
               'div'([], [], [{id, out}])], [], [])).

%% cowboy 路由
Routes = aihtml_cowboy:routes(hello, #{path => "/", page => #{title => <<"Hello">>}}).
```

处理器只能出现在会话渲染的 HTML 里，静态渲染包含处理器会直接报错。WebSocket 默认只接受同源连接。

### fetch 模式（无状态）

`fetch/3,4` 生成的属性让元素通过普通 HTTP 请求 HTML 片段，再替换到目标位置，不需要会话：

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
        %% 只有 live 模式需要
        {aihtml_cowboy, {git_subdir, "https://github.com/DavidAlphaFox/aihtml.git",
                         {branch, "master"}, "apps/aihtml_cowboy"}}]}.
```

live 模式需要运行 aihtml 应用，它负责会话注册表和会话监督树。把 `aihtml` 写进你的应用的 `applications` 列表即可自动启动。

核心库不依赖 cowboy。其它服务器只需实现一个传输进程：
- 用 `aihtml_live:connect/4` 取得会话，重连时带上 `resume` 和 `last`。
- 把 `{aihtml_send, iodata()}` 发给浏览器，收到 `{aihtml_close, Code, Reason}` 时关闭连接。
- 把收到的 JSON 帧交给 `aihtml_live:incoming/2`。

`aihtml_cowboy:routes/2` 的 `resume_timeout` 和 `replay_limit` 选项分别设置会话等待重连的时间和重放缓冲的帧数。

**静态资源**：把 aihtml 的 `priv/static` 挂到 `/aihtml/`，cowboy 写法如下：

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

## 许可证

Apache-2.0
