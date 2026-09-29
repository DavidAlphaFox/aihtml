# 02 action 模式

## 目标

浏览器事件直接调用 Erlang 函数，同时业务层完全无状态：状态只存在于数据层，任意节点都能处理任意请求，服务器重启不影响已打开的页面。

## 为什么不用 WebSocket

之前的 live 模式为每个窗口保留一个 Erlang 会话进程，处理器是闭包。它带来三个问题：

- **负载均衡**：需要粘性会话，或者跨节点查找会话。
- **可靠性**：服务器重启或节点宕机，会话随之丢失。要恢复，就得把状态和闭包持久化，代价过高。
- **状态分散**：状态散落在各个进程里，而不是集中在数据层。

本模式改为每次交互一个请求，状态由数据层负责（思路来自 AG-UI，但格式是 aihtml 自己的精简协议，见下文"与 AG-UI 的关系"）。

## 流程

```
渲染 on(click, {Mod, Action, Args})
  └─ token = base64url(term_to_binary(Ref)) "." base64url(HMAC-SHA256(secret, payload))
     写入 data-ah-on="click:TOKEN[:debounce]"

点击
  └─ POST /aihtml/action  {"action": TOKEN, "event": {...}, "stream": 推送流 id（可省）}
       ├─ Origin 不同源               → 403 {"error":"forbidden_origin"}
       ├─ 请求体不对                   → 400 {"error":"bad_request"}
       ├─ 签名不符或模块未声明 behaviour → 403 {"error":"invalid_action"}
       ├─ action 崩溃（未 flush 过）    → 500 {"error":"action_failed"}
       ├─ 200 application/json        {"ops": [操作...]}
       └─ 200 application/x-ndjson    （action 调用过 flush/1）
            {"ops": [操作...]}         每次 flush 一行
            {"ops": [操作...]}         返回时剩下的
            {"done": true}             或 {"error": "action_failed"}（flush 之后崩溃）
```

绝大多数 action 一次返回，响应就是一个 JSON；只有调用了 `flush/1` 的 action（先显示加载状态、再显示数据）才变成逐行的 NDJSON 流。操作的格式在两种响应里、以及服务端推送里都相同。

## 安全

- **HMAC 签名**：保证 action 与参数由服务端生成，浏览器无法伪造或篡改。比较签名使用 `crypto:hash_equals/2`，是常数时间比较。
- **解码**：签名通过之后，payload 才用 `binary_to_term(_, [safe])` 解码。
- **模块白名单**：只有声明了 `-behaviour(aihtml_action)` 并导出 `action/4` 的模块才能被调用，即使签名有效也不能调用 `lists` 之类的模块。
- **参数限制**：参数只签名不加密，必须是纯数据，不能包含 fun、pid、port、reference。渲染时会检查。
- **来源检查**：端点默认只接受同源 Origin。请求体是 JSON，跨站表单无法构造这样的请求。
- **授权**：令牌只证明"这个 action 是我们生成的"，不能证明"当前用户有权执行"。用户身份与权限必须在 action 中根据请求检查。
- **没有过期时间**：令牌不过期，只要密钥不变，页面上的按钮就一直可用。需要失效时轮换密钥，旧页面会收到 403。

## 密钥

应用环境 `{aihtml, [{secret, Bin}]}`，至少 32 字节，所有节点必须相同，首次使用时读入 `persistent_term`。未配置时随机生成一个并记录警告，这只适合开发环境。

## 执行与流式输出

- **执行位置**：action 在 HTTP 请求进程中运行。操作写入进程字典中的缓冲，所以 Ctx 只能在本请求进程中使用，在其它进程使用会报错。
- **发送时机**：action 返回时，`aihtml_action:execute/3` 把缓冲的操作交给传输层，作为整个响应。`flush/1` 立即通过传输层的 `send` 回调发出已缓冲的操作，传输层从这时起改为流式响应。
- **传输层决定格式**：核心库只产出操作列表（`execute/3` 返回 `{ok, Ops}` 或 `error`），JSON、NDJSON 和状态码由 `aihtml_cowboy_action` 决定。
- **崩溃处理**：action 崩溃时写日志，响应只有错误码 `action_failed`，不暴露异常细节：还没发出任何内容时是 HTTP 500，已经 flush 过则是流的最后一行。

## 客户端

- **事件委托**：对 click、dblclick、change、input、submit、keydown、keyup、focusin、focusout、mouseenter、mouseleave 统一委托处理。
- **收集事件数据**：元素没有 id 时先补一个，然后收集 value、checked、key、所在表单字段、`data-ah-include` 选中控件的值和 `data-*` 属性。
- **读取响应**：用 `fetch` 发送请求。`application/json` 直接解析；`application/x-ndjson` 从 `ReadableStream` 逐行读取，每行一到就应用。
- **并发**：click、submit 在请求进行中忽略重复触发。input、change、keyup、keydown 以最新为准，用 `AbortController` 取消旧请求。
- **错误**：请求被拒或出错时，在元素上触发 `ah:error`，detail 是 `{status, error}`（HTTP 状态和服务端的错误码）。

## 取舍

放弃的能力：
- **服务端主动推送**：action 本身没有长连接。推送由独立的 SSE 订阅端点提供，见 [03-push.md](03-push.md)。
- **查询浏览器**：`query/2` 不再存在。需要其它控件的值时，用 `include` 随事件一起发送。
- **闭包处理器**：处理器必须是具名的 `{Module, Action, Args}`。

得到的能力：
- **无需粘性**：任意节点、任意负载均衡都能处理请求。
- **重启无感**：服务器重启后，已打开的页面继续可用。
- **页面可直接渲染**：整页由普通 HTTP handler 渲染，不需要启动页和连接，搜索引擎和首屏都能直接拿到内容。
- **轻量**：一次点击的响应就是 `{"ops":[...]}`，可以正常压缩；错误用 HTTP 状态表达，日志和监控看得到。

## 与 AG-UI 的关系

action 响应不是 AG-UI 事件流：按钮点击不是"一次 agent 运行"，套用 `RUN_STARTED`/`RUN_FINISHED` 既重又会和真正的 AG-UI 应用混淆。以后做 AG-UI 应用（聊天、工具调用）时：
- AG-UI 走它自己的端点和客户端组件，完整使用 AG-UI 的事件。
- agent 要更新页面时，在 AG-UI 流里发 `CUSTOM` 事件，名字为 `aihtml.ops`，值就是这里的操作列表，客户端组件收到后调用 `AH.apply(ops)`；要更新其它页面，直接调用 `aihtml_push:publish`。
这是两者唯一的交汇点，其余格式互不相干。

## 借鉴 htmx 的补充能力

这些能力借鉴 htmx 的设计，但由运行时（`runtime/swap.ts`、`actions.ts`、`requests.ts`）自行实现，不依赖 htmx。

- **形变替换与焦点保留**：替换方式增加 `morph` 和 `morph_inner`；所有替换方式都会按 id 恢复焦点和选区。
- **元素保留**：带 `data-ah-preserve` 且有 id 的元素在替换时移动而不重建。浏览器支持时用 `moveBefore`，不支持时用 `insertBefore`。形变替换会跳过这类元素。
- **settle 过渡**：新加入的顶层元素带 `ah-added`，替换目标带 `ah-settling`，20 毫秒后移除。同 id 元素先沿用旧的 class、style、width、height，再换成新值。组件根元素不参与。
- **请求协调**：按"元素加事件"协调；设置了 `data-ah-sync-scope` 时按最近的匹配祖先协调。策略有 drop、replace、queue 三种，queue 只保留最新一个等待的请求。
- **加载状态**：请求期间元素带 `aria-busy`，元素和 `data-ah-indicator` 指向的元素带 `ah-request`，`data-ah-disable` 指向的元素被禁用。重叠的请求按计数处理。
- **新操作**：
  - `trigger`：在目标元素或 document 上触发事件，带 detail。
  - `url`：push 或 replace 浏览器历史。条目的 `history.state` 带 `{ah: true}`，前进或后退到这些条目时整页重新加载，由服务端渲染该 URL，所以服务端依然不需要保存页面状态。

测试见 `apps/aihtml/test/js/morph.test.js` 和 `request.test.js`。
