# `ah_toast/3` 只有标题和描述两行文本：**没有操作链接或按钮**（“已保存 · 打开”这种）

报的人：beamai_muse　严重度：🟢　回没回：✅ 已实现

## 症状

动作做完想弹一条「已保存到“文件”」并带一个「打开」链接，`ah_toast/3` 只认
`description`、`variant`、`duration`、`position`、`closable`、`close_on_click`、`width`；
多给的 `action => #{label, href}` 被静默忽略，toast 里只有两行文字。

不报错，链接就是没有。

## 位置

`apps/aihtml/src/aihtml_toast.erl:42–52`（`ah_toast/3` 只把 `title` / `description`
交给 `tpl_toast`，文本走 `?L:text/1`，没有放链接或按钮的位置）。

## 怎么撞上的

beamai_muse `/mail` 阅读区的「保存到文件」（`muse_ui_mail` 的 `save_attachment`
动作）：附件存进文件库后，想在提示里给一个「打开」直达 `/documents/<id>`。现在的绕法是
toast 只报结果，再把那颗按钮原地换成「已保存 · 打开」链接；保存邮箱账号后想给的
「打开收件箱」干脆没给。

## 影响面

凡是「做完一件事、顺手去看结果」的提示都会撞上（保存、上传、发送后去看）。能绕，
但结果入口只能塞回页面里，提示一消失用户就不知道去哪看。

## 建议

可选：`ah_toast/3` 加 `action => #{label := iodata(), href => iodata()}`（渲染成 toast
里的一个链接，点了关掉 toast），或者 `action => #{label, ref => aihtml_action:ref()}`
（点了走一个动作）。前者就够大多数场景。

已实现：`ah_toast/3` 与 `shows_toast/2` 都支持
`action => #{label := iodata(), href => iodata()}`，渲染为 toast 内的
`.ah-toast__action` 链接；点击链接会关闭 toast 并正常跳转。缺少 `label` 或
`href`、或有多余键时直接报 `{aihtml, {bad_option, action, …}}`，不再静默忽略。
动作里传 `ref` 的需求暂不做（走 `aihtml_action:trigger/4` 的场景可先在目标页绑事件）。
