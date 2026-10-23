# `/aihtml/action` 与 `/aihtml/events` 没有鉴权挂点，token 永不过期

报的人：beamai_muse　严重度：🟡　回没回：✅ 已实现

## 症状

一个字都不报。用户登出（会话 cookie 已失效）后，还开着的旧标签页：

- 点按钮照样触发动作（动作 token 仍然有效）；
- 照样收到推送（主题 token 仍然有效，SSE 连接也不会被断开）。

## 位置

- `apps/aihtml_cowboy/src/aihtml_cowboy.erl:39-47` —— `routes/1` 只认
  `action / events / static / origins`，没有「请求进来先问宿主」的挂点；
- `apps/aihtml_cowboy/src/aihtml_cowboy_events.erl:32-45` —— `init/2` 只做 origin
  检查和 `aihtml_push:verify/1`；
- `apps/aihtml/src/aihtml_action.erl:122-139` —— `sign/1` 的载荷只有项本身，
  `unsign/1` 只验签，**不带时间**，所以 token 没有寿命。

## 怎么撞上的

beamai_muse 的页面在渲染时为 `{user, Owner}` 等主题签 token。登出后旧页面继续
订阅 `/aihtml/events?t=…` —— 仍然 200、仍然收消息。

## 影响面

所有有登录态的 aihtml 应用。不包一层就是静默越权（读到推送、触发动作）。

下游当前的绕法：
1. 不用 `aihtml_cowboy:routes/1` 生成的两条路由，换成自己的 handler：先查 cookie，
   通过了再委托给 `aihtml_cowboy_action:init/2` / `aihtml_cowboy_events:init/2`；
2. 主题里带「会话纪元」（`{user, Owner, Epoch}`），登出时换纪元，让旧 token 订阅的
   主题再也没人发；
3. 每个动作第一行再查一次会话。

能用，但每个宿主都要写一遍，而且 2 只能让旧页面「收不到」，挡不住它「订阅成功」。

## 建议

- `routes(#{authorize => fun((Req) -> ok | {deny, Status})})`，在 verify / join 之前调用；
- 和/或 token 可选寿命：`sign/2` 带 `max_age`，`unsign` 校验过期（`page/2` 渲染时决定）。
