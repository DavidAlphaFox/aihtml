# 动作的事件载荷没有鼠标坐标、修饰键和 CustomEvent `detail`

报的人：beamai_muse　严重度：🔴　回没回：✅ 已实现

## 症状

一个字都不报。动作拿到的 `Event` 只有
`#{type, id, value, checked, key, form, values, data}`：

- 绑在 `<img>` 上的 `on(click, …)`：**不知道点在哪**；
- `keydown`：有 `key`，但 **ctrl / shift / alt / meta 一个都没有**；
- `on('ah:close', …)`：`closes(closest, ok)` 带出来的 `{result}` **到不了动作**
  （CustomEvent 的 `detail` 被丢掉了）。

## 位置

- `apps/aihtml/assets/js/runtime/actions.ts:137-175` —— `#eventPayload` 只取
  `e.type` / `e.key` 和元素本身的值、表单、`data-*`；
- `apps/aihtml/assets/js/runtime/actions.ts:405` ——
  `const ev = { type: e.type, key: (e as KeyboardEvent).key };`，原始事件的其余字段
  在这里就没了，后面的 `run()` 再也拿不到。

## 怎么撞上的

beamai_muse 的浏览器「接管」控制台：页面显示远端浏览器的截图，用户点截图上的某处，
服务端要把点击换算成 1280×800 视口坐标发给 Playwright worker。

```erlang
ah_img([], [{id, shot}, {src, Url}, on(click, {muse_ui_browser, click, #{id => Sid}})])
%% action(click, …, Event, Ctx)：Event 里没有任何坐标
```

## 影响面

所有「远程操控」类界面（截图点选、画布、图片热区、拖拽到坐标）都做不出来；
所有依赖 CustomEvent `detail` 的组件事件（`ah:close` 的结果等）都只能退化成
「每个按钮各绑一个动作」。不报错，字段就是没有。

下游当前的绕法：在 `<img>` 上挂一个自写的 Stimulus 控制器，在 document 委托处理之前
把 `offsetX/offsetY` 和图片自然尺寸写进 `dataset`，让它随 `data` 带上来。能用，但每个
宿主都得自己写一遍，而且依赖监听顺序。

## 建议

在载荷里加三个可选字段（缺省就不出现，不破坏现有动作）：

- `detail` —— CustomEvent 的 `detail`（JSON-safe 的部分）；
- `mods` —— `#{ctrl, shift, alt, meta}`，键盘和鼠标事件都带；
- `pointer` —— 鼠标事件的 `#{x, y, w, h}`：相对元素的偏移和元素的显示尺寸
  （`<img>` 再加 `natural_w / natural_h`），服务端自己换算。
