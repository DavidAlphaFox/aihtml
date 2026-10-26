# `confirm` 选项用的是浏览器原生 `window.confirm()`：样式、文案、按钮都不受主题控制，没有页面内确认的替代

报的人：beamai_muse　严重度：🟡　回没回：✅ 已实现

## 症状

`on(click, Ref, #{confirm => <<"归档这个对话？">>})` 点下去弹出的是浏览器自带的确认框：标题是
「192.168.3.13:8787 显示」、按钮是浏览器语言的「确定 / 取消」，和 aihtml 的主题、圆角、按钮样式
完全无关；移动端还会把页面整个挡住。下游用户直接反馈「不要再用浏览器自带的 alert，应该用 aihtml 自己的」。

## 位置

aihtml `056cba3`（1.0.0）：

- `apps/aihtml/assets/js/runtime/actions.ts:463`：`if (question && !window.confirm(question)) { return; }`
- `apps/aihtml/assets/js/runtime/fetch.ts:58`：同上，`data-ah-fetch` 的请求前确认
- `apps/aihtml/src/aihtml.erl:450-467`、`:484-516`：`confirm => iodata()` 只落成 `data-ah-confirm` 属性

组件库里有 `aihtml_window`（`modal => true`）、`aihtml_sheet`、`aihtml_drawer`，但 `confirm` 不用它们，
也没有「把问题交给页面里的某个对话框」的挂点。

## 怎么撞上的

任意 `on(…, #{confirm => …})` 或 `data-ah-fetch` + `data-ah-confirm`，点一下。

## 影响面

所有用 `confirm` 的下游（删除、归档、停止、拒绝审批之类的危险操作，正好是最该好看、最该说清楚的地方）。
不报错，只是体验和主题脱节；`window.confirm` 是同步阻塞的，也没法异步定制。

beamai_muse 现在的绕法（`apps/muse_web/priv/static/js/muse.js`）：外壳里放一个隐藏的
`ah_window(…, [{modal, true}])`，在 document 捕获阶段拦下带 `data-ah-confirm` 的 click / submit，
`AH.invoke(win, "open")` 提问；用户点「确定」后**临时摘掉 `data-ah-confirm` 再重放一次** click /
`requestSubmit()`，让 actions.ts 以为没有问题要问。能用，但依赖 aihtml 读取属性的时机，属于偷跑。

## 建议

- `confirm` 默认用 aihtml 自己的模态对话框（复用 `aihtml_window` / sheet 的样式与焦点陷阱），
  异步等待结果后再 `run`；
- 选项扩成 `confirm => iodata() | #{title, body, ok, cancel, variant => danger}`，危险操作可以红色确定按钮；
- 或者至少给一个挂点：`AH.confirm = async (question, el) => boolean` 可被宿主替换，默认实现再回落到 `window.confirm`。
