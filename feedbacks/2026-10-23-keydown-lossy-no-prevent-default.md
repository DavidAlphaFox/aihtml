# `on(keydown, …)` 快速输入丢键，且无法 `preventDefault`

报的人：beamai_muse　严重度：🔴　回没回：✅ 已实现

## 症状

一个字都不报，键就丢了：

- 默认策略：`keydown` 在 `LATEST_WINS` 里 ⇒ `replace`，新键**中止**还在途的上一个请求；
- 改成 `sync => queue`：等待位只留**最后一个**，中间的键被覆盖；
- Tab / Backspace / 空格在发请求的同时**照常作用于页面**（焦点跳走、页面滚动）。

## 位置

- `apps/aihtml/assets/js/runtime/actions.ts:76` ——
  `const LATEST_WINS = { input: true, change: true, keyup: true, keydown: true };`
- `apps/aihtml/assets/js/runtime/actions.ts:316-331` —— `run()`：`queue` 是
  `running.queued = () => …`（单槽，后来的覆盖先来的），`replace` 是 `ctrl.abort()`；
- `apps/aihtml/assets/js/runtime/actions.ts:387-394` —— `handled()` 只对 `submit`
  和 `<a>` / submit 按钮的 `click` 调 `preventDefault`，键盘事件没有入口。

## 怎么撞上的

浏览器「接管」控制台：一个获得焦点的元素绑 `on(keydown, {M, key, #{}})`，每个键转发给
远端浏览器。正常打字速度下，一个往返（~30 ms）内就会来下一个键 ⇒ 字母成片丢失。
按 Tab 想在远端表单里换字段，结果是本页焦点跳走。

## 影响面

任何「把键盘逐键转发出去」的界面：远程控制台、终端模拟、快捷键捕获。不报错。

下游当前的绕法：放弃逐键，改成「文本框 + 发送按钮 + 一排特殊键按钮」；或自写控制器
本地缓冲按键、批量 POST 到宿主自己的 JSON 端点（绕开 aihtml 的动作通道）。

## 建议

- 增加 `sync => fifo`：保留所有排队的请求，按序发出；
- 或 `batch => Ms`：在窗口内把键序列攒成一个列表，一次带上（`#{keys => [...]}`）；
- 键盘事件可选 `prevent_default => true`（可按键名过滤，如 `[<<"Tab">>, <<"Backspace">>]`）。
