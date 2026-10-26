# `ah_window` 的标题没有公开的改法：同一个窗口复用为「新建 / 编辑」时只能按内部 id 写字

报的人：beamai_muse　严重度：🟢　回没回：✅ 已实现

## 症状

一个模态窗口既做「新建日程」又做「编辑日程」，动作里重渲染了窗体后想把标题也换掉。
`ah_window` 没有 `setTitle` 方法，也没有文档化的标题元素 id；只能依赖渲染细节：

```erlang
aihtml_action:text(Ctx, {id, <<WinId/binary, "-title">>}, <<"编辑日程"/utf8>>, replace)
```

不报错；哪天 `TitleId` 的拼法变了，标题就静默不更新。

## 位置

`apps/aihtml/src/aihtml_window.erl:66–78`（`TitleId = <<Id/binary, "-title">>`，只给
`aria-labelledby` 用）。

## 怎么撞上的

beamai_muse `/calendar` 的日程窗口与日历窗口（muse_ui_calendar：`open_event_form/4`、
`open_calendar_form/3`）。另一种绕法是整窗 `outer` 替换后立刻 `open`，但新节点的行为挂载
与 `open` op 的先后不好保证。

## 影响面

复用窗口的页面都会撞上；能绕，绕法依赖内部 id。

## 建议

已实现：组件方法 `setTitle(Text)` 和服务端 `set_title/3`。`set_title/3` 只接受
`{id, Id}` 目标，生成 `call(..., setTitle, [Text])`；下游不再需要依赖 `<id>-title`。
