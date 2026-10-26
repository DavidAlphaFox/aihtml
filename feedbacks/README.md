# feedbacks —— 下游宿主报上来的账

一条一个文件，`<日期>-<slug>.md`。**只记「aihtml 这一侧该改」的**：下游自己
绕过去了但绕法很蠢、或者根本绕不过去的那些。下游自己的 bug 不进这里。

每份的格式（症状先行，病因其次，建议最后）：

| 段 | 写什么 |
|---|---|
| **症状** | 下游看到的**那一行**（报错原文 / 或者「一个字都不报」） |
| **位置** | `文件:行`，aihtml 这一侧的 |
| **怎么撞上的** | 最短复现路径 |
| **影响面** | 谁会撞、撞了会怎样、**报不报错** |
| **建议** | 可选，下游不替仓主拍板 |

## 现有

| # | 标题 | 报的人 | 严重度 | 回没回 |
|---|---|---|---|---|
| [2026-10-23](2026-10-23-event-payload-no-pointer-mods-detail.md) | 动作的事件载荷只有 `type/key/value/form/values/data`：**没有鼠标坐标、没有修饰键、没有 CustomEvent `detail`** ⇒ 点图片拿不到点在哪；`closes(closest, ok)` 的结果到不了 `ah:close` 的动作 | beamai_muse | 🔴 「远程操控」类界面做不出来；不报错，字段就是没有 | ✅ 已回 |
| [2026-10-23](2026-10-23-keydown-lossy-no-prevent-default.md) | 绑了 `on(keydown, …)` 的元素**快速打字会丢键**（`replace` 中止在途请求，`queue` 只留最后一个），且 Tab / Backspace / 空格同时作用于页面（没有可选的 `preventDefault`） | beamai_muse | 🔴 逐键转发的输入框不可用；不报错，键就丢了 | ✅ 已回 |
| [2026-10-23](2026-10-23-endpoints-no-auth-hook-tokens-never-expire.md) | `/aihtml/action` 与 `/aihtml/events` **没有鉴权挂点**，动作与主题 token **永不过期** ⇒ 登出后的旧标签页仍能触发动作、仍收得到推送 | beamai_muse | 🟡 有登录的应用都得自己包一层；不包则静默越权 | ✅ 已回 |
| [2026-10-23](2026-10-23-no-text-op.md) | 没有「追加文本」的 op：流式输出 LLM 文本只能每块 `html(…, append)`，每块都走 `insertAdjacentHTML` + settle + 挂载扫描 | beamai_muse | 🟡 能用，费 | ✅ 已回 |
| [2026-10-23](2026-10-23-action-cannot-set-response-headers.md) | 动作里 `cowboy_req:set_resp_cookie/3` **无效**：回复是用 handler 自己手里的 `Req0` 发的 | beamai_muse | 🟢 登录改走原生表单 POST 即可；但不报错，容易白写 | ✅ 已回 |
| [2026-10-23](2026-10-23-no-poll-attribute.md) | 没有声明式的定时刷新（htmx `every Ns` 那种）：周期刷新只能靠服务端定时推送或自写控制器 | beamai_muse | 🟢 锦上添花 | ✅ 已回 |
| [2026-10-26](2026-10-26-confirm-uses-window-confirm.md) | `confirm` 选项弹的是浏览器原生 `window.confirm()`：标题是「某某网址显示」、按钮跟浏览器语言，**完全不吃主题**；没有页面内确认对话框的选项或挂点 | beamai_muse | 🟡 不报错，体验与主题脱节；下游只能捕获阶段拦截再重放 | ✅ 已回 |
| [2026-10-26](2026-10-26-calendar-no-reload-range.md) | `ah_calendar` 没有「按当前可见范围重新加载」的方法：服务端推送想让各页面刷新自己显示的那段，只能 `trigger(…, change, null)` 借用导航语义 | beamai_muse | 🟢 能绕，不报错 | ✅ 已实现 |
| [2026-10-26](2026-10-26-window-title-not-settable.md) | `ah_window` 的标题没有公开的改法：复用窗口做「新建 / 编辑」只能按内部 id `<id>-title` 写字 | beamai_muse | 🟢 能绕，依赖内部 id | ✅ 已实现 |
| [2026-10-26](2026-10-26-toast-no-action-link.md) | `ah_toast/3` 只有标题和描述两行文本，**没有操作链接或按钮**（「已保存 · 打开」） | beamai_muse | 🟢 能绕，不报错，多给的选项被忽略 | ✅ 已实现 |

> 背景：beamai_muse（OpenMuse 的 Erlang 重写）用 aihtml 做全部前端：聊天流式输出、
> 任务时间线实时更新、审批卡片、浏览器「接管」控制台（截图 + 点击 + 输入）、Linux
> 终端。上面几条是对照源码 `327668d` 核实过的。

## ⭐ 先说做得好的几处（不进上面那张表，但值得记）

* **`flush/1` 把动作回复变成 NDJSON 流** —— 终端那种「请求期间持续吐输出」的场景
  一个动作就写完了，不需要另开 SSE。
* **推送按 DOM 订阅、断线后跑 `refresh` 动作补齐** —— at-most-once 的推送配上
  「从数据层重绘」的兜底，语义是诚实的，下游不用自己做序号和补洞。
* **`publish/2` 在调用方渲染一次、按主题扇出，且跨节点（pg）** —— 多标签页 / 多节点
  的实时更新不用下游操心。
* **动作参数签名、按 `-behaviour(aihtml_action)` 校验模块**，`unsign/1` 先验签再
  `binary_to_term(…, [safe])` —— 外部输入进不到任意模块/原子，这一层是收紧的。
