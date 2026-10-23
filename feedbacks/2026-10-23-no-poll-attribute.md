# 没有声明式的定时刷新属性

报的人：beamai_muse　严重度：🟢　回没回：✅ 已实现

## 症状

想让一块内容每 N 秒刷新一次（截图预览、状态面板），只能：

- 服务端起定时进程，按 `aihtml_push:subscribers/1` 判断有人在看再推；
- 或自写一个 Stimulus 控制器 `setInterval` 换 `src`。

## 位置

运行时里没有定时触发动作的机制（`apps/aihtml/assets/js/runtime/*.ts` 中没有
`setInterval` / poll 相关代码）。

## 怎么撞上的

beamai_muse 浏览器控制台：截图 `<img>` 需要每 2 秒刷新（原 OpenMuse 控制台就是 2 秒轮询）。

## 影响面

锦上添花。现有两种绕法都能用，只是样板代码多。

## 建议

`poll(Ms, {M, A, Args})` 属性（类似 htmx 的 `hx-trigger="every 2s"`），页面隐藏时暂停、
元素移除时停止。
