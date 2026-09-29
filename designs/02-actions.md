# 02 action 模式

## 目标

浏览器事件直接调用 Erlang 函数，同时业务层完全无状态：状态只存在于数据层，任意节点都能处理任意请求，服务器重启不影响已打开的页面。

## 为什么不用 WebSocket

之前的 live 模式为每个窗口保留一个 Erlang 会话进程，处理器是闭包。它带来三个问题：

- **负载均衡**：需要粘性会话，或者跨节点查找会话。
- **可靠性**：服务器重启或节点宕机，会话随之丢失。要恢复，就得把状态和闭包持久化，代价过高。
- **状态分散**：状态散落在各个进程里，而不是集中在数据层。

AG-UI 的做法是每次交互一个请求，状态由数据层负责。本模式采用同样的思路。

## 流程

```
渲染 on(click, {Mod, Action, Args})
  └─ token = base64url(term_to_binary(Ref)) "." base64url(HMAC-SHA256(secret, payload))
     写入 data-ah-on="click:TOKEN[:debounce]"

点击
  └─ POST /aihtml/action  {"action": TOKEN, "event": {...}, "threadId", "runId"}
       ├─ Origin 不同源               → 403
       ├─ 签名不符或模块未声明 behaviour → 403 invalid_action
       └─ 200 text/event-stream
            data: {"type":"RUN_STARTED", ...}
            data: {"type":"CUSTOM","name":"aihtml.ui","value":[操作...]}   (flush 一次一条)
            data: {"type":"RUN_FINISHED", ...}   或 RUN_ERROR
```

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
- **发送时机**：`flush/1` 立即以一条 CUSTOM 事件发出已缓冲的操作。action 返回时自动 flush。
- **崩溃处理**：action 崩溃时写日志，并发送 `RUN_ERROR`，消息固定为 "action failed"，不暴露异常细节。

## 客户端

- **事件委托**：对 click、dblclick、change、input、submit、keydown、keyup、focusin、focusout、mouseenter、mouseleave 统一委托处理。
- **收集事件数据**：元素没有 id 时先补一个，然后收集 value、checked、key、所在表单字段、`data-ah-include` 选中控件的值和 `data-*` 属性。
- **读取响应**：用 `fetch` 发送请求，从 `ReadableStream` 解析 SSE。`EventSource` 不能发 POST，所以没有用它。
- **并发**：click、submit 在请求进行中忽略重复触发。input、change、keyup、keydown 以最新为准，用 `AbortController` 取消旧请求。
- **错误**：请求被拒或出错时，在元素上触发 `ah:error`。

## 取舍

放弃的能力：
- **服务端主动推送**：action 本身没有长连接。推送由独立的 SSE 订阅端点提供，见 [03-push.md](03-push.md)。
- **查询浏览器**：`query/2` 不再存在。需要其它控件的值时，用 `include` 随事件一起发送。
- **闭包处理器**：处理器必须是具名的 `{Module, Action, Args}`。

得到的能力：
- **无需粘性**：任意节点、任意负载均衡都能处理请求。
- **重启无感**：服务器重启后，已打开的页面继续可用。
- **页面可直接渲染**：整页由普通 HTTP handler 渲染，不需要启动页和连接，搜索引擎和首屏都能直接拿到内容。
- **兼容 AG-UI**：事件格式与 AG-UI 一致，可以与 beamai_agui 这类 AG-UI 服务共存。
