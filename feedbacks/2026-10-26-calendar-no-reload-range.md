# `ah_calendar` 没有「按当前可见范围重新加载」的挂点：服务端推送只能靠触发 `change` 绕

报的人：beamai_muse　严重度：🟢　回没回：✅ 已实现

## 症状

数据在别处变了（另一个标签页、智能体的审批通过、JSON API），服务端想让页面上的日历
刷新**它此刻显示的那段**。但服务端不知道每个页面停在哪个月 / 哪一周：

- `set_events/3` 要服务端自己给出事件，得先知道范围；
- `call(…, setEvents, …)` 推送给所有订阅者时，各页面的范围不同；
- 组件方法里没有 `reload` / `refetch`（catalog 只有 prev/next/today/changeView/setValue/
  getValue/setEvents/addEvent/updateEvent/removeEvent/getEvents）。

一个字都不报，就是没有办法。

## 位置

- `apps/aihtml/src/aihtml_calendar.erl` catalog 的 `methods`（约 686–702 行）；
- `apps/aihtml/assets/js/components/calendar.ts:637`：只有日期 / 视图真的变了才 `fire("change")`。

## 怎么撞上的

beamai_muse 的 `/calendar`：日历的 `change` 动作按 `data-start` / `data-end` 懒加载
（`set_events(Ctx, Event, …)`）。用户在表单里保存、切换日历显示、或者智能体的修改被批准后，
要让所有打开的日历页重新加载自己的范围。

现在的绕法：`aihtml_action:trigger(Ctx, {id, Cal}, change, null)`（推送里用同一个 op）——
在根元素上派发一个 `change`，运行时把根的 `data-view/start/end` 带进事件，懒加载的动作再
`set_events`。能用，但这是借用「导航了」的语义：凡是把 `change` 当成「用户换了日期」的页面
（比如顺手 `push_url`）都会被误触发。

## 影响面

所有「懒加载 + 服务端推送」的日历页面都会撞上；不报错，只是没有正路。

## 建议

二选一（下游不拍板）：

1. 组件方法 `reload()`：不改日期 / 视图，以当前范围派发一个专门的事件（如 `ah:range`，
   detail 与 `change` 相同：`view/start/end`），懒加载动作可以同时绑它；
2. 或者 `change` 的 detail 带一个 `reason`（`navigate | view | reload`），`reload()` 方法以
   `reason: "reload"` 触发。

已实现：组件方法 `reload()` 和服务端 `reload/2`。`reload()` 只按当前视图重绘，再触发
`ah:range`；`detail` 是 `{view, start, end}`，当前范围也仍写在根元素 `data-view` /
`data-start` / `data-end`。懒加载动作可直接绑 `ah:range` 并调用 `set_events/3`，不会触发
`change`，也不会借用导航语义。

顺带：`ah:event-click` 对重复日程只在 `detail.raw` 里有被点的那一次的起止时间，`data-*` 里
只有系列 id（`event`）。要「只改这一次」的页面拿不到是哪一次（`detail` 能用，但 `data` 里
缺一个 `occurrence` 字段会更顺手）。
