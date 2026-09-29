%% @doc Demos of the overlay components (aihtml_overlay), shown on
%% /components/<name>. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it. The
%% server-driven demos call action/4 below.
-module(aihtml_example_demo_overlay).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([tooltip_positions/0, tooltip_triggers/0, tooltip_on_any_element/0,
         popover_basic/0, popover_positions/0, popover_modal/0,
         drawer_bottom/0, drawer_sides/0, drawer_options/0,
         sheet_form/0, sheet_sides/0, sheet_from_server/0,
         toast_variants/0, toast_options/0, toast_from_server/0,
         notification_variants/0, notification_sticky/0, notify_from_server/0,
         window_dialog/0, window_tool/0, window_from_server/0, window_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => tooltip, title => <<"Tooltip">>,
       summary => <<"悬停、聚焦或点击时浮出的一句话提示。"/utf8>>,
       demos => [{<<"四个方向"/utf8>>, tooltip_positions},
                 {<<"点击触发、跟随鼠标、无箭头"/utf8>>, tooltip_triggers},
                 {<<"不包裹元素：tooltip_attrs/2"/utf8>>, tooltip_on_any_element}]},
     #{component => popover, title => <<"Popover">>,
       summary => <<"锚定在触发元素上的气泡卡片，可放任意内容。"/utf8>>,
       demos => [{<<"标题与关闭按钮"/utf8>>, popover_basic},
                 {<<"方向"/utf8>>, popover_positions},
                 {<<"模态：带遮罩，点外部不关"/utf8>>, popover_modal}]},
     #{component => drawer, title => <<"Drawer">>,
       summary => <<"从边缘滑出的抽屉，可下滑关闭。"/utf8>>,
       demos => [{<<"底部抽屉"/utf8>>, drawer_bottom},
                 {<<"左右两侧"/utf8>>, drawer_sides},
                 {<<"不可滑动关闭、无把手"/utf8>>, drawer_options}]},
     #{component => sheet, title => <<"Sheet">>,
       summary => <<"从侧边滑入的模态面板，带遮罩、滚动锁和焦点陷阱。"/utf8>>,
       demos => [{<<"表单面板"/utf8>>, sheet_form},
                 {<<"四个方向"/utf8>>, sheet_sides},
                 {<<"由服务端打开"/utf8>>, sheet_from_server}]},
     #{component => toast, title => <<"Toast">>,
       summary => <<"角落里弹出的简短消息，一行代码调用，自动消失。"/utf8>>,
       demos => [{<<"四种语义"/utf8>>, toast_variants},
                 {<<"常驻与位置"/utf8>>, toast_options},
                 {<<"在 action 里调用 toast/3"/utf8>>, toast_from_server}]},
     #{component => notification, title => <<"Notification">>,
       summary => <<"服务端渲染的通知模板，每次打开克隆一张卡片堆叠在角落。"/utf8>>,
       demos => [{<<"四种语义"/utf8>>, notification_variants},
                 {<<"不自动关闭、右下角"/utf8>>, notification_sticky},
                 {<<"在 action 里调用 notify/2"/utf8>>, notify_from_server}]},
     #{component => window, title => <<"Window">>,
       summary => <<"可拖拽、可调整大小的窗口，也可作模态对话框。"/utf8>>,
       demos => [{<<"模态对话框"/utf8>>, window_dialog},
                 {<<"工具窗口：折叠、拖拽、缩放"/utf8>>, window_tool},
                 {<<"由服务端打开"/utf8>>, window_from_server},
                 {<<"record 写法"/utf8>>, window_record}]}].

%%%===================================================================
%%% Tooltip
%%%===================================================================

-spec tooltip_positions() -> aihtml:html().
tooltip_positions() ->
    row([tooltip(<<"Shown above">>, button(<<"Top">>, undefined, [outlined], []), [top], []),
         tooltip(<<"Shown below">>, button(<<"Bottom">>, undefined, [outlined], []), [], []),
         tooltip(<<"On the left">>, button(<<"Left">>, undefined, [outlined], []), [left], []),
         tooltip(<<"On the right">>, button(<<"Right">>, undefined, [outlined], []), [right], [])]).

-spec tooltip_triggers() -> aihtml:html().
tooltip_triggers() ->
    row([tooltip(<<"Opened by a click">>, button(<<"Click me">>, undefined, [], []),
                 [top], [{trigger, click}]),
         tooltip(<<"Follows the mouse">>, button(<<"Mouse">>, undefined, [default], []),
                 [mouse], []),
         tooltip(<<"No arrow, stays until you leave">>,
                 button(<<"No arrow">>, undefined, [default], []),
                 [no_arrow], [{auto_hide, false}])]).

-spec tooltip_on_any_element() -> aihtml:html().
tooltip_on_any_element() ->
    row([button(<<"Save">>, save, [primary],
                tooltip_attrs(<<"Save the document (Ctrl+S)">>, #{position => top})),
         span(<<"Hover this text">>, [<<"underline decoration-dotted">>],
              [{tabindex, 0}, tooltip_attrs(<<"Any element works">>, #{})])]).

%%%===================================================================
%%% Popover
%%%===================================================================

-spec popover_basic() -> aihtml:html().
popover_basic() ->
    row([button(<<"Show details">>, undefined, [primary], toggles({id, <<"pop-details">>})),
         popover([p(<<"Popovers hold any content: text, links, small forms.">>,
                    [<<"mb-2">>], []),
                  button(<<"Got it">>, undefined, [sm], closes())],
                 [], [{id, <<"pop-details">>}, {title, <<"Details">>},
                      {closable, true}, {width, 260}])]).

-spec popover_positions() -> aihtml:html().
popover_positions() ->
    row([button(<<"Top">>, undefined, [outlined], toggles({id, <<"pop-top">>})),
         popover(<<"Above its trigger.">>, [top], [{id, <<"pop-top">>}]),
         button(<<"Right">>, undefined, [outlined], toggles({id, <<"pop-right">>})),
         popover(<<"To the right, without an arrow.">>, [right, no_arrow],
                 [{id, <<"pop-right">>}])]).

-spec popover_modal() -> aihtml:html().
popover_modal() ->
    row([button(<<"Confirm">>, undefined, [warning], opens({id, <<"pop-modal">>})),
         popover([p(<<"Outside clicks do not close this one.">>, [<<"mb-2">>], []),
                  button(<<"OK">>, undefined, [sm, primary], closes())],
                 [], [{id, <<"pop-modal">>}, {title, <<"Modal popover">>}, {modal, true}])]).

%%%===================================================================
%%% Drawer
%%%===================================================================

-spec drawer_bottom() -> aihtml:html().
drawer_bottom() ->
    'div'([button(<<"Open drawer">>, undefined, [primary], opens({id, <<"drawer-bottom">>})),
           drawer(p(<<"Drag the handle down, press Escape or click the scrim to close.">>),
                  [], [{id, <<"drawer-bottom">>}, {title, <<"Filters">>},
                       {description, <<"Swipe down to close.">>},
                       {footer, [button(<<"Reset">>, undefined, [default], closes()),
                                 button(<<"Apply">>, undefined, [primary], closes(closest, apply))]}])]).

-spec drawer_sides() -> aihtml:html().
drawer_sides() ->
    row([button(<<"Left">>, undefined, [outlined], opens({id, <<"drawer-left">>})),
         button(<<"Right">>, undefined, [outlined], opens({id, <<"drawer-right">>})),
         drawer(nav_links(), [left], [{id, <<"drawer-left">>}, {title, <<"Menu">>}, {size, 280}]),
         drawer(nav_links(), [right], [{id, <<"drawer-right">>}, {title, <<"Menu">>}, {size, 280}])]).

-spec drawer_options() -> aihtml:html().
drawer_options() ->
    'div'([button(<<"Open">>, undefined, [secondary], opens({id, <<"drawer-plain">>})),
           drawer(p(<<"No handle and no swipe; only the close button closes it.">>),
                  [top], [{id, <<"drawer-plain">>}, {title, <<"Announcement">>},
                          {size, 200}, {handle, false}, {dismissible, false},
                          {close_on_overlay, false}, {close_on_esc, false}])]).

%%%===================================================================
%%% Sheet
%%%===================================================================

-spec sheet_form() -> aihtml:html().
sheet_form() ->
    'div'([button(<<"Edit profile">>, undefined, [primary], opens({id, <<"sheet-profile">>})),
           sheet([input(undefined, [], [{name, name}, {placeholder, <<"Name">>}]),
                  input(undefined, [<<"mt-3">>], [{name, email}, {placeholder, <<"Email">>}])],
                 [], [{id, <<"sheet-profile">>}, {title, <<"Edit profile">>},
                      {description, <<"Changes are saved when you click Save.">>},
                      {footer, [button(<<"Cancel">>, undefined, [default], closes()),
                                button(<<"Save">>, undefined, [primary], closes(closest, save))]}])]).

-spec sheet_sides() -> aihtml:html().
sheet_sides() ->
    row([button(atom_to_binary(Side), undefined, [outlined],
                opens({id, <<"sheet-", (atom_to_binary(Side))/binary>>}))
         || Side <- [left, top, bottom]]
        ++ [sheet(<<"Slides in from the ", (atom_to_binary(Side))/binary, ".">>, [Side],
                  [{id, <<"sheet-", (atom_to_binary(Side))/binary>>},
                   {title, atom_to_binary(Side)}, {size, 240}])
            || Side <- [left, top, bottom]]).

-spec sheet_from_server() -> aihtml:html().
sheet_from_server() ->
    'div'([button(<<"Ask the server">>, undefined, [primary],
                  on(click, {?MODULE, open, #{target => <<"sheet-server">>}})),
           sheet(p(<<"The server opened this sheet with aihtml_overlay:open/2.">>),
                 [], [{id, <<"sheet-server">>}, {title, <<"Opened by an action">>}])]).

%%%===================================================================
%%% Toast
%%%===================================================================

-spec toast_variants() -> aihtml:html().
toast_variants() ->
    row([button(<<"Info">>, undefined, [info],
                shows_toast(<<"Heads up">>, #{description => <<"Something happened.">>})),
         button(<<"Success">>, undefined, [success],
                shows_toast(<<"Saved">>, #{variant => success,
                                           description => <<"Your changes are live.">>})),
         button(<<"Warning">>, undefined, [warning],
                shows_toast(<<"Disk almost full">>, #{variant => warning})),
         button(<<"Error">>, undefined, [error],
                shows_toast(<<"Upload failed">>, #{variant => error}))]).

-spec toast_options() -> aihtml:html().
toast_options() ->
    row([button(<<"Sticky">>, undefined, [outlined],
                shows_toast(<<"Stays until closed">>, #{duration => 0})),
         button(<<"Bottom left">>, undefined, [outlined],
                shows_toast(<<"Bottom left">>, #{position => bottom_left})),
         button(<<"Bottom right, 1 s">>, undefined, [outlined],
                shows_toast(<<"Quick one">>, #{position => bottom_right, duration => 1000}))]).

-spec toast_from_server() -> aihtml:html().
toast_from_server() ->
    row([button(<<"Save on the server">>, undefined, [primary],
                on(click, {?MODULE, save, #{}}))]).

%%%===================================================================
%%% Notification
%%%===================================================================

-spec notification_variants() -> aihtml:html().
notification_variants() ->
    row([button(<<"Info">>, undefined, [info], opens({id, <<"note-info">>})),
         button(<<"Success">>, undefined, [success], opens({id, <<"note-success">>})),
         button(<<"Warning">>, undefined, [warning], opens({id, <<"note-warning">>})),
         button(<<"Error">>, undefined, [error], opens({id, <<"note-error">>})),
         notification(<<"A new version is available.">>, [info], [{id, <<"note-info">>}]),
         notification([strong(<<"Upload complete. ">>), <<"3 files were added.">>],
                      [success], [{id, <<"note-success">>}]),
         notification(<<"Your session expires in 5 minutes.">>, [warning],
                      [{id, <<"note-warning">>}]),
         notification(<<"The server could not be reached.">>, [error],
                      [{id, <<"note-error">>}])]).

-spec notification_sticky() -> aihtml:html().
notification_sticky() ->
    row([button(<<"Notify">>, undefined, [outlined], opens({id, <<"note-sticky">>})),
         button(<<"Close all">>, undefined, [default], closes({id, <<"note-sticky">>})),
         notification(<<"Stays until you close it.">>, [warning, bottom_right],
                      [{id, <<"note-sticky">>}, {auto_close, false},
                       {close_on_click, false}, {width, 320}])]).

-spec notify_from_server() -> aihtml:html().
notify_from_server() ->
    row([button(<<"Import on the server">>, undefined, [primary],
                on(click, {?MODULE, import, #{}}))]).

%%%===================================================================
%%% Window
%%%===================================================================

-spec window_dialog() -> aihtml:html().
window_dialog() ->
    'div'([button(<<"Delete file">>, undefined, [error], opens({id, <<"win-confirm">>})),
           window(p(<<"Delete report.pdf? This cannot be undone.">>),
                  [], [{id, <<"win-confirm">>}, {title, <<"Confirm delete">>},
                       {modal, true}, {width, 380}, {resizable, false},
                       {footer, [button(<<"Cancel">>, undefined, [default],
                                        closes(closest, cancel)),
                                 button(<<"Delete">>, undefined, [error],
                                        closes(closest, ok))]}])]).

-spec window_tool() -> aihtml:html().
window_tool() ->
    'div'([button(<<"Open tool window">>, undefined, [outlined], toggles({id, <<"win-tool">>})),
           window(p(<<"Drag the title bar, resize from the edges, collapse with the arrow.">>),
                  [], [{id, <<"win-tool">>}, {title, <<"Properties">>}, {width, 320},
                       {collapsible, true}])]).

-spec window_from_server() -> aihtml:html().
window_from_server() ->
    'div'([button(<<"Load and open">>, undefined, [primary],
                  on(click, {?MODULE, open, #{target => <<"win-server">>}})),
           window(p(<<"Opened with aihtml_overlay:open/2 from an action.">>),
                  [], [{id, <<"win-server">>}, {title, <<"From the server">>}, {width, 360}])]).

-spec window_record() -> aihtml:html().
window_record() ->
    'div'([button(<<"Rename file">>, undefined, [outlined], opens({id, <<"win-rename">>})),
           #ah_window{id = <<"win-rename">>, title = <<"Rename">>, modal = true, width = 360,
                      resizable = false,
                      body = input(<<"report.pdf">>, [], [{name, file_name}]),
                      footer = [button(<<"Cancel">>, undefined, [default], closes()),
                                button(<<"Rename">>, undefined, [primary],
                                       closes(closest, ok))],
                      %% fires when the window closes, handled by action/4 below
                      postback = window_closed}]).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), map(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(open, #{target := Id}, _Ev, Ctx) ->
    aihtml_overlay:open(Ctx, {id, Id});
action(save, _, _Ev, Ctx) ->
    toast(Ctx, <<"Saved on the server">>,
          #{variant => success, description => <<"Rendered by toast/3.">>});
action(window_closed, _, _Ev, Ctx) ->
    toast(Ctx, <<"The window was closed">>, #{description => <<"Sent by its postback.">>});
action(import, _, _Ev, Ctx) ->
    aihtml_overlay:notify(Ctx, #{content => [strong(<<"Import finished. ">>),
                                             <<"128 rows added.">>],
                                 variant => success, position => bottom_right}).

%%%===================================================================
%%% Helpers
%%%===================================================================

nav_links() ->
    ul([li(a(<<"Dashboard">>, [], [{href, <<"#">>}])),
        li(a(<<"Projects">>, [], [{href, <<"#">>}])),
        li(a(<<"Settings">>, [], [{href, <<"#">>}]))],
       [<<"flex flex-col gap-2">>], []).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-3">>], []).
