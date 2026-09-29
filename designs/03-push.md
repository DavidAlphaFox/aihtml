# 03 服务端推送

## 目标

在不引入业务状态的前提下，让服务端主动更新页面。典型场景：
- 其它用户修改了数据
- 后台任务有了进度
- 定时刷新

## 结构

```
页面 ── EventSource GET /aihtml/events?t=T1&t=T2 ── SSE 进程 (cowboy_loop)
                                                    │ pg:join(aihtml_push, {topic, T})
任意节点上的任意进程                                   │
  aihtml_push:publish(T, Fun, #{except => Ctx}) ─────┘ {aihtml_push, Except, Json}
```

- **aihtml 应用**启动 `pg` scope `aihtml_push`。`pg` 通过 Erlang 分布式自动覆盖所有已连接的节点，不需要额外的注册中心。
- **SSE 进程只做转发。** 它校验令牌、加入主题、发送流 id、转发消息，并每 25 秒发一次心跳。进程退出时，`pg` 会自动把它从组里移除。
- **`publish/3` 在发布者进程里只渲染一次。** 它用 `aihtml_action:render_ops/1` 执行 `Fun`，得到操作列表，编码成 JSON 后发给组内所有成员。在 action 里调用时，不会影响该 action 自己的响应缓冲。

## 主题令牌

- **签名**：`aihtml_push:token(Topic)` 对 `{aihtml_topic, Topic}` 签名，使用与 action 相同的密钥和格式。
- **互不通用**：action 令牌是 3 元组，主题令牌是 2 元组，二者不能互相冒充。
- **权限**：页面只能订阅服务端为它渲染过的主题，权限在渲染时决定。令牌只签名不加密，主题名对页面可见。
- **上限**：每条流最多 32 个主题。

## 发起者去重

发起修改的页面已经通过 action 响应更新过自己，推送给它会造成重复，比如 append 两次。处理方式：

1. SSE 进程生成随机流 id，作为第一条事件 `CUSTOM "aihtml.stream"` 发给页面。
2. 页面发起 action 时，在请求体里带上 `streamId`，action 的 ctx 保存它。
3. `publish(T, Fun, #{except => Ctx})` 把这个 id 随消息一起发出，SSE 进程发现与自己的 id 相同就跳过。

流 id 是 16 字节随机数，只有该页面知道。

## 送达语义

送达是**至多一次**，不做重放：
- **断线期间的推送会丢失。**
- **EventSource 自动重连。** 服务器通过 `retry: 2000` 把重连间隔设为 2 秒。
- **重连后补齐。** 客户端在重连时（不是首次连接）运行页面上所有 `data-ah-refresh` 指定的 action，从数据层取回最新内容。

不做重放的原因：重放需要按流保存事件，而状态已经在数据层了，重新读取比重放更简单、也更可靠。

## 客户端

- **自动同步订阅。** `syncStream()` 在页面加载和每次应用操作之后运行。它收集页面上的 `data-ah-subscribe`，集合变化时关闭旧流、打开新流。新内容里的订阅会自动生效，被移除内容的订阅会自动退订。
- **推送与 action 共用处理逻辑。** 推送来的事件和 action 响应一样，都由 `onAgui` 处理。
- **被拒绝时触发错误。** 服务端拒绝时 EventSource 进入 CLOSED 状态，页面在 document 上触发 `ah:error`。

## 超时

- **HTTP/1.1**：SSE 进程通过 `cowboy_req:cast({set_options, #{idle_timeout => infinity}}, Req)` 关闭空闲超时。心跳让代理不会因为长时间没有数据而断开连接。已验证一条没有任何推送的流能保持 70 秒以上。
- **HTTP/2**：cowboy 忽略这个选项，连接可能因空闲超时断开。EventSource 会自动重连，再由 refresh 补齐。如有需要，可以调大监听器的 `idle_timeout`。

## 集群中的单实例任务

周期性发布任务，比如示例里的时钟，在集群中应当只运行一份，否则每个节点都会发布一次。示例的做法：
- 用 `global:register_name/3` 注册全局名，冲突解析函数用 `global:random_notify_name/3`。
- 注册失败的节点每 5 秒重试一次，持有者消失后接管。
- 两个集群合并时，输掉名字的一方会收到通知，自行退回等待状态，而不是被直接杀掉。

## 示例的数据层

示例用 Mnesia 作为数据层（`aihtml_example_store`），用来验证"业务层无状态、状态全在数据层"这一前提：

- **表**：计数表 `aihtml_example_counter`（页面计数和待办 id 序列），以及按 id 排序的 `aihtml_example_todo`（ordered_set）。
- **写入**：一律使用事务，读改写时加写锁。测试中 50 个并发写入者结果精确，id 不重复。
- **存储**：默认 `disc_copies`。设置 `db_join` 的节点会加入已有集群，把 schema 转为 disc 并复制两张表。
- **已验证的行为**：
  - 节点 b 写入的数据，在节点 a 新打开的页面上能读到。
  - 节点 a 停机期间，b 照常读写。
  - a 重启后从 b 同步数据。a 上已经打开的页面重连后，通过 refresh action 拿到停机期间新增的数据。
