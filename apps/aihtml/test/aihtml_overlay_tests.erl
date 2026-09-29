-module(aihtml_overlay_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_overlay.hrl").

-define(O, aihtml_overlay).

r(H) -> aihtml_html:render_binary(H).

has(Html, Part) ->
    case binary:match(r(Html), Part) of
        nomatch -> ?assertEqual({missing, Part}, r(Html));
        _ -> ok
    end.

lacks(Html, Part) ->
    ?assertEqual(nomatch, binary:match(r(Html), Part)).

%% Operations an action body would send, JSON round-tripped like the wire.
ops(Fun) ->
    Ops = aihtml_action:render_ops(Fun),
    json:decode(iolist_to_binary(json:encode(Ops))).

%%%===================================================================
%%% Catalog and examples
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([tooltip, popover, drawer, sheet, toast, notification, window],
                 [N || #{name := N} <- ?O:catalog()]).

catalog_entries_are_complete_test() ->
    [begin
         ?assert(is_binary(S)), ?assert(is_binary(R)),
         ?assertEqual(overlay, C),
         ?assert(lists:member(<<"ah:open">>, maps:get(events, E)))
     end || #{signature := S, root := R, category := C} = E <- ?O:catalog()].

every_entry_documents_options_and_methods_test() ->
    [begin
         Docs = maps:get(option_docs, E),
         [?assert(maps:is_key(O, Docs)) || O <- maps:get(options, E, [])],
         ?assert(is_list(maps:get(methods, E)))
     end || E <- ?O:catalog()].

%%%===================================================================
%%% Tooltip
%%%===================================================================

tooltip_wraps_trigger_test() ->
    H = ?O:tooltip(<<"Hint <b>">>, aihtml_html:el(button, <<"B">>, [], []), [top], [{id, <<"t">>}]),
    has(H, <<"<span class=\"ah-tooltip-host\" data-ah=\"tooltip\" data-ah-tip-position=\"top\" id=\"t\">">>),
    has(H, <<"<button>B</button>">>),
    has(H, <<"class=\"ah-tooltip ah-tooltip-top\" role=\"tooltip\"">>),
    has(H, <<"Hint &lt;b&gt;">>).

tooltip_options_and_mouse_test() ->
    H = ?O:tooltip(<<"x">>, <<"y">>, [mouse, no_arrow, <<"max-w-xs">>],
                   [{trigger, click}, {auto_hide, false}, {show_delay, 0}, {width, 200}]),
    has(H, <<"data-ah-tip-position=\"mouse\"">>),
    has(H, <<"data-ah-tip-trigger=\"click\"">>),
    has(H, <<"data-ah-tip-auto-hide=\"false\"">>),
    has(H, <<"data-ah-tip-delay=\"0\"">>),
    has(H, <<"ah-tooltip ah-tooltip-bottom ah-tooltip-no-arrow max-w-xs">>),
    has(H, <<"style=\"width:200px\"">>),
    %% options are not written as HTML attributes
    lacks(H, <<" trigger=">>).

tooltip_bad_modifier_test() ->
    ?assertError({aihtml, {unknown_modifier, tooltip, primary, _}},
                 ?O:tooltip(<<"x">>, <<"y">>, [primary], [])),
    ?assertError({aihtml, {conflicting_modifiers, tooltip, position, _}},
                 ?O:tooltip(<<"x">>, <<"y">>, [top, left], [])).

tooltip_attrs_test() ->
    B = aihtml_html:el(button, <<"b">>, [], ?O:tooltip_attrs(<<"Save \"now\"">>,
                                                           #{position => left, arrow => false})),
    has(B, <<"data-ah-tooltip=\"Save &quot;now&quot;\"">>),
    has(B, <<"data-ah-tip-arrow=\"false\"">>),
    has(B, <<"data-ah-tip-position=\"left\"">>),
    lacks(aihtml_html:el(i, [], [], ?O:tooltip_attrs(<<"t">>, #{})), <<"arrow">>).

%%%===================================================================
%%% Popover
%%%===================================================================

popover_test() ->
    H = ?O:popover(<<"Body">>, [left, no_arrow, <<"w-64">>],
                   [{id, <<"p">>}, {title, <<"T">>}, {closable, true}, {modal, true},
                    {anchor, <<"#btn">>}]),
    has(H, <<"class=\"ah-popover ah-popover-left ah-popover-no-arrow w-64\"">>),
    has(H, <<"data-ah=\"popover\" data-state=\"closed\" role=\"dialog\"">>),
    has(H, <<"data-ah-anchor=\"#btn\"">>),
    has(H, <<"data-ah-modal=\"true\"">>),
    has(H, <<"<div class=\"ah-popover-title\">T<div class=\"ah-popover-close-btn\"">>),
    has(H, <<"<div class=\"ah-popover-content\">Body</div>">>).

popover_defaults_test() ->
    H = ?O:popover(<<"x">>, [], []),
    has(H, <<"ah-popover ah-popover-bottom">>),
    lacks(H, <<"ah-popover-title">>).

%%%===================================================================
%%% Drawer and sheet
%%%===================================================================

drawer_structure_test() ->
    H = ?O:drawer(<<"Body">>, [<<"bg-red-50">>],
                  [{id, <<"d">>}, {title, <<"Title">>}, {description, <<"Desc">>},
                   {footer, <<"F">>}]),
    has(H, <<"<div class=\"ah-drawer__overlay\" data-ah=\"drawer\" data-state=\"closed\" id=\"d\">">>),
    has(H, <<"class=\"ah-drawer__panel bg-red-50\" role=\"dialog\" aria-modal=\"true\" "
             "aria-labelledby=\"d-title\" data-side=\"bottom\" data-state=\"closed\" "
             "style=\"height:50vh;\" tabindex=\"-1\"">>),
    has(H, <<"<div class=\"ah-drawer__handle\" aria-hidden=\"true\">">>),
    has(H, <<"<h2 class=\"ah-drawer__title\" id=\"d-title\">Title</h2>">>),
    has(H, <<"<p class=\"ah-drawer__description\">Desc</p>">>),
    has(H, <<"class=\"ah-drawer__close\" type=\"button\" aria-label=\"close\" data-ah-close=\"\"">>),
    has(H, <<"<div class=\"ah-drawer__body\">Body</div>">>),
    has(H, <<"<div class=\"ah-drawer__footer\">F</div>">>).

drawer_options_test() ->
    H = ?O:drawer(<<"x">>, [right], [{size, 320}, {handle, false}, {closable, false},
                                     {dismissible, false}, {close_on_overlay, false},
                                     {close_on_esc, false}, {open, true}]),
    has(H, <<"data-side=\"right\"">>),
    has(H, <<"style=\"width:320px;\"">>),
    has(H, <<"data-ah-esc=\"false\" data-ah-scrim=\"false\" data-ah-dismissible=\"false\" "
             "data-ah-initial=\"open\"">>),
    lacks(H, <<"__handle">>),
    lacks(H, <<"__header">>),
    lacks(H, <<"__footer">>).

sheet_test() ->
    H = ?O:sheet(<<"x">>, [], [{title, <<"S">>}]),
    has(H, <<"class=\"ah-sheet__overlay\" data-ah=\"sheet\"">>),
    has(H, <<"data-side=\"right\"">>),
    has(H, <<"style=\"width:380px;\"">>),
    has(H, <<"aria-label=\"S\"">>),
    lacks(H, <<"handle">>),
    ?assertError({aihtml, {unknown_modifier, sheet, middle, _}}, ?O:sheet(<<"x">>, [middle], [])),
    %% attributes are checked when the record is rendered
    ?assertError(_, r(?O:sheet(<<"x">>, [], [{handle, true}, {bogus, 1}, {"bad attr", 1}]))).

%%%===================================================================
%%% Window
%%%===================================================================

window_test() ->
    H = ?O:window(<<"Body">>, [<<"shadow-xl">>],
                  [{id, <<"w">>}, {title, <<"Title">>}, {modal, true}, {footer, <<"F">>},
                   {width, 400}, {height, 300}, {collapsible, true}]),
    has(H, <<"class=\"ah-window shadow-xl ah-window-resizable\" data-ah=\"window\" "
             "data-state=\"closed\" role=\"dialog\" tabindex=\"-1\" aria-modal=\"true\" "
             "aria-labelledby=\"w-title\" style=\"display:none;width:400px;height:300px;\"">>),
    has(H, <<"data-ah-modal=\"true\"">>),
    has(H, <<"<div class=\"ah-window-header ah-window-header-draggable\">">>),
    has(H, <<"<div class=\"ah-window-title\" id=\"w-title\">Title</div>">>),
    has(H, <<"class=\"ah-window-collapse-btn\" type=\"button\" aria-label=\"Collapse\" aria-expanded=\"true\"">>),
    has(H, <<"class=\"ah-window-close-btn\" type=\"button\" aria-label=\"Close\" data-ah-close=\"\"">>),
    has(H, <<"<div class=\"ah-window-content\">Body</div><div class=\"ah-window-footer\">F</div>">>),
    ?assertEqual(8, length(binary:matches(r(H), <<"ah-window-resize-handle">>))).

window_plain_test() ->
    H = ?O:window(<<"x">>, [], [{resizable, false}, {draggable, false}, {closable, false},
                                {collapsed, true}]),
    has(H, <<"class=\"ah-window ah-window-collapsed\"">>),
    has(H, <<"aria-modal=\"false\"">>),
    has(H, <<"aria-expanded=\"false\"">>),
    lacks(H, <<"ah-window-close-btn">>),
    lacks(H, <<"header-draggable">>),
    lacks(H, <<"data-ah-modal">>).

%%%===================================================================
%%% Notification and toast
%%%===================================================================

notification_template_test() ->
    H = ?O:notification(<<"Saved <ok>">>, [success, bottom_left],
                        [{id, <<"n">>}, {auto_close, false}, {width, 300},
                         {close_on_click, false}]),
    has(H, <<"<div class=\"ah-notify-tpl\" data-ah=\"notification\" hidden "
             "data-ah-position=\"bottom-left\" data-ah-duration=\"0\" id=\"n\">"
             "<div class=\"ah-notify ah-notify-success\" role=\"alert\" style=\"width:300px\">">>),
    has(H, <<"<div class=\"ah-notify-content\">Saved &lt;ok&gt;</div>">>),
    has(H, <<"class=\"ah-notify-close\"">>),
    D = ?O:notification(<<"x">>, [], [{closable, false}]),
    has(D, <<"data-ah-position=\"top-right\" data-ah-duration=\"3000\">">>),
    has(D, <<"ah-notify ah-notify-info ah-notify-clickable">>),
    has(D, <<"<circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"16\"">>),
    lacks(D, <<"ah-notify-close">>).

shows_toast_test() ->
    B = aihtml_html:el(button, <<"b">>, [],
                       ?O:shows_toast(<<"Done">>, #{variant => success, duration => 0,
                                                    position => bottom_right,
                                                    description => <<"<x>">>})),
    has(B, <<"data-ah-toast=\"Done\" data-ah-toast-description=\"&lt;x&gt;\" "
             "data-ah-toast-variant=\"success\" data-ah-toast-duration=\"0\" "
             "data-ah-toast-position=\"bottom-right\"">>).

%%%===================================================================
%%% Declarative triggers
%%%===================================================================

triggers_test() ->
    has(aihtml_html:el(button, <<"o">>, [], ?O:opens({id, <<"d">>})),
        <<"data-ah-open=\"#d\" aria-haspopup=\"dialog\" aria-controls=\"d\"">>),
    has(aihtml_html:el(button, <<"t">>, [], ?O:toggles(<<".x">>)), <<"data-ah-toggle=\".x\"">>),
    lacks(aihtml_html:el(button, <<"t">>, [], ?O:toggles(<<".x">>)), <<"aria-controls">>),
    has(aihtml_html:el(button, <<"c">>, [], ?O:closes()), <<"data-ah-close=\"\"">>),
    has(aihtml_html:el(button, <<"c">>, [], ?O:closes({id, w})), <<"data-ah-close=\"#w\"">>),
    has(aihtml_html:el(button, <<"c">>, [], ?O:closes(closest, ok)),
        <<"data-ah-close=\"\" data-ah-result=\"ok\"">>),
    has(aihtml_html:el(button, <<"c">>, [], ?O:closes(<<"#w">>, <<"cancel">>)),
        <<"data-ah-close=\"#w\" data-ah-result=\"cancel\"">>).

%%%===================================================================
%%% Server-driven helpers
%%%===================================================================

open_close_toggle_ops_test() ->
    ?assertEqual([#{<<"op">> => <<"call">>, <<"id">> => <<"cart">>,
                    <<"method">> => <<"open">>, <<"args">> => []},
                  #{<<"op">> => <<"call">>, <<"sel">> => <<"#cart">>,
                    <<"method">> => <<"close">>, <<"args">> => []},
                  #{<<"op">> => <<"call">>, <<"id">> => <<"cart">>,
                    <<"method">> => <<"toggle">>, <<"args">> => []}],
                 ops(fun(Ctx) ->
                             ?O:open(Ctx, {id, <<"cart">>}),
                             ?O:close(Ctx, <<"#cart">>),
                             ?O:toggle(Ctx, {id, cart})
                     end)).

toast_op_test() ->
    [Op] = ops(fun(Ctx) ->
                       ?O:toast(Ctx, <<"Saved <b>">>, #{variant => success, duration => 0,
                                                         position => bottom_left,
                                                         close_on_click => false,
                                                         description => "multi word"})
               end),
    #{<<"op">> := <<"call">>, <<"method">> := <<"notify">>,
      <<"args">> := [#{<<"card">> := Card, <<"position">> := <<"bottom-left">>,
                       <<"duration">> := 0} = Args]} = Op,
    ?assertEqual(3, map_size(Args)),
    ?assertMatch({_, _}, binary:match(Card, <<"<div class=\"ah-notify ah-notify-success\" role=\"alert\">">>)),
    ?assertMatch({_, _}, binary:match(Card, <<"<div class=\"ah-toast__title\">Saved &lt;b&gt;</div>"
                                              "<div class=\"ah-toast__description\">multi word</div>">>)),
    %% the default duration of a toast
    [#{<<"args">> := [#{<<"duration">> := 4000, <<"position">> := <<"top-right">>}]}] =
        ops(fun(Ctx) -> ?O:toast(Ctx, <<"t">>, #{}) end).

notify_op_renders_and_escapes_content_test() ->
    [#{<<"method">> := <<"notify">>, <<"args">> := [#{<<"card">> := Card, <<"duration">> := 3000}]}] =
        ops(fun(Ctx) ->
                    ?O:notify(Ctx, #{content => [aihtml_html:el(b, <<"Hi">>, [], []),
                                                 <<" <script>">>],
                                     variant => error, closable => false})
            end),
    ?assertMatch({_, _}, binary:match(Card, <<"<div class=\"ah-notify-content\"><b>Hi</b> &lt;script&gt;</div>">>)),
    ?assertMatch({_, _}, binary:match(Card, <<"ah-notify ah-notify-error ah-notify-clickable">>)),
    ?assertEqual(nomatch, binary:match(Card, <<"ah-notify-close">>)).

%% The card the server renders is byte-identical to the template output the
%% browser produces for the same view (see also aihtml_tpl_tests).
card_matches_template_test() ->
    {safe, B} = aihtml_tpl:safe(?O:tpl_notification(
                                  #{variant => <<"info">>, info => true, success => false,
                                    warning => false, error => false, clickable => true,
                                    closable => true, width => null, content => <<"x">>})),
    has(?O:notification(<<"x">>, [], []), B).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?O:tooltip(<<"Hint">>, <<"B">>, [left, no_arrow, <<"max-w-xs">>],
                              [{id, t}, {trigger, click}, {width, 200}, {title, <<"x">>}])),
                 r(#ah_tooltip{body = <<"Hint">>, anchor = <<"B">>, position = left,
                               no_arrow = true, css = [<<"max-w-xs">>], id = t,
                               trigger = click, width = 200, attrs = [{title, <<"x">>}]})),
    ?assertEqual(r(?O:popover(<<"P">>, [top], [{id, <<"p">>}, {title, <<"T">>},
                                               {closable, true}, {modal, false}])),
                 r(#ah_popover{body = <<"P">>, position = top, id = <<"p">>, title = <<"T">>,
                               closable = true, modal = false})),
    ?assertEqual(r(?O:drawer(<<"D">>, [right, <<"bg-white">>],
                             [{id, <<"d">>}, {title, <<"T">>}, {size, 280},
                              {handle, false}, {close_on_esc, false}])),
                 r(#ah_drawer{body = <<"D">>, side = right, css = [<<"bg-white">>],
                              id = <<"d">>, title = <<"T">>, size = 280, handle = false,
                              close_on_esc = false})),
    ?assertEqual(r(?O:sheet(<<"S">>, [top], [{id, s}, {footer, <<"F">>}, {open, true}])),
                 r(#ah_sheet{body = <<"S">>, side = top, id = s, footer = <<"F">>,
                             open = true})),
    ?assertEqual(r(?O:notification(<<"N">>, [error, bottom_left],
                                   [{id, <<"n">>}, {auto_close, false}, {width, 320}])),
                 r(#ah_notification{body = <<"N">>, variant = error, position = bottom_left,
                                    id = <<"n">>, auto_close = false, width = 320})),
    ?assertEqual(r(?O:window(<<"W">>, [<<"shadow">>],
                             [{id, <<"w">>}, {title, <<"T">>}, {modal, true},
                              {height, 200}, {collapsed, true}])),
                 r(#ah_window{body = <<"W">>, css = [<<"shadow">>], id = <<"w">>,
                              title = <<"T">>, modal = true, height = 200,
                              collapsed = true})).

builder_fills_fields_test() ->
    W = ?O:window(<<"x">>, [<<"c">>], [{id, <<"w">>}, {width, 400}, {draggable, false},
                                        {close_on_esc, false}, {data_x, 1}]),
    ?assertMatch(#ah_window{id = <<"w">>, css = [<<"c">>], width = 400, draggable = false,
                            close_on_esc = false, closable = true, attrs = [{data_x, 1}]}, W),
    %% an option of the drawer is an HTML attribute of the sheet
    ?assertMatch(#ah_sheet{attrs = [{handle, false}]}, ?O:sheet(<<"x">>, [], [{handle, false}])),
    ?assertMatch(#ah_tooltip{body = <<"b">>, anchor = <<"a">>, position = mouse},
                 ?O:tooltip(<<"b">>, <<"a">>, [mouse], [])),
    ?assertError({aihtml, {record_only_field, ah_drawer, postback}},
                 ?O:drawer(<<"x">>, [], [{postback, save}])).

ids_test() ->
    %% the title id follows the id field, also for an atom id
    has(#ah_drawer{id = cart, title = <<"Cart">>},
        <<"aria-labelledby=\"cart-title\"">>),
    has(#ah_window{id = <<"w">>}, <<"<div class=\"ah-window-title\" id=\"w-title\">">>),
    %% without an id a window still gets a unique title id
    ?assertMatch({match, _}, re:run(r(#ah_window{}),
                                    <<"aria-labelledby=\"ah-window-[0-9]+-title\"">>)),
    lacks(#ah_drawer{title = <<"T">>}, <<"aria-labelledby">>).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"(ah:[a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ah, Ev, Tok] = binary:split(T, <<":">>, [global]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {<<Ah/binary, ":", Ev/binary>>, Ref}
            end,
    Close = <<"ah:close">>,
    ?assertEqual({Close, {?MODULE, closed, #{id => 1}}},
                 Token(#ah_drawer{id = <<"d">>, postback = {closed, #{id => 1}}})),
    ?assertEqual({Close, {?MODULE, closed, #{}}}, Token(#ah_sheet{postback = closed})),
    ?assertEqual({Close, {other_mod, closed, #{}}},
                 Token(#ah_window{postback = closed, delegate = other_mod})),
    ?assertEqual({Close, {?MODULE, gone, 2}},
                 Token(#ah_notification{postback = {gone, 2}})),
    ?assertEqual({Close, {?MODULE, closed, #{}}}, Token(#ah_popover{postback = closed})),
    ?assertError({aihtml, {no_postback_event, ah_tooltip}},
                 r(#ah_tooltip{body = <<"x">>, postback = closed})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, tooltip, position, middle, _}},
                 r(#ah_tooltip{position = middle})),
    ?assertError({aihtml, {bad_flag, popover, no_arrow, yes}},
                 r(#ah_popover{no_arrow = yes})),
    ?assertError({aihtml, {bad_modifier, drawer, side, center, _}},
                 r(#ah_drawer{side = center})),
    ?assertError({aihtml, {bad_modifier, notification, variant, danger, _}},
                 r(#ah_notification{variant = danger})),
    ?assertError({aihtml, {bad_option, trigger, hovering}},
                 r(#ah_tooltip{trigger = hovering})),
    ?assertError({aihtml, {bad_option, modal, <<"true">>}},
                 r(#ah_window{modal = <<"true">>})),
    ?assertError({aihtml, {bad_option, delay, -1}},
                 r(#ah_notification{delay = -1})),
    ?assertError({aihtml, {bad_option, close_on_esc, 1}},
                 r(#ah_sheet{close_on_esc = 1})),
    ?assertError({aihtml, {modifier_in_css, window, large}},
                 r(#ah_window{css = [large]})),
    %% modifier names still fail in the builder
    ?assertError({aihtml, {unknown_modifier, drawer, center, _}},
                 ?O:drawer(<<"x">>, [center], [])).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?O:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(default(Tag))))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?O, maps:get(module, Defaults))
     end || #{name := N} = E <- ?O:catalog(),
            %% toast is an action (toast/3), not an element: no record
            N =/= toast].

default(ah_tooltip) -> #ah_tooltip{};
default(ah_popover) -> #ah_popover{};
default(ah_drawer) -> #ah_drawer{};
default(ah_sheet) -> #ah_sheet{};
default(ah_notification) -> #ah_notification{};
default(ah_window) -> #ah_window{}.
