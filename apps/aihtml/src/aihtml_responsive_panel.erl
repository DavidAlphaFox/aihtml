%%%-------------------------------------------------------------------
%%% @doc ResponsivePanel, ported from sigil (DOM and classes as sigil
%%% renders them, so the styles in priv/css/sigil apply): content shown in
%%% place while the parent is wide, folded into a toggle and a floating
%%% overlay when it is narrower than a breakpoint.
%%%
%%% responsive_panel/3 builds an element record (#ah_responsive_panel{},
%%% defined in include/aihtml_responsive_panel.hrl) and render/1 turns it
%%% into HTML, so pages may also write the record directly
%%% (designs/05-records.md). The behaviour is in
%%% assets/js/components/responsive_panel.ts.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_responsive_panel).
-behaviour(aihtml_element).

-include("aihtml_responsive_panel.hrl").

-export([responsive_panel/3, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_scroll).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% @doc A panel that folds into a toggle button when its parent is not
%% wider than `breakpoint' px; the toggle then opens the content as a
%% floating overlay. Options: breakpoint, collapse_width, height,
%% animation (fade | slide | none), show_duration, hide_duration,
%% auto_close, toggle_button (a selector of an extra toggle), toggle_size,
%% toggle_content, toggle_label, load (an action ref fired the first
%% time the content is shown).
-spec responsive_panel(html(), css(), attrs()) -> #ah_responsive_panel{}.
responsive_panel(Children, Css, Attrs) ->
    ?E:build(?MODULE, #ah_responsive_panel{body = Children}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_responsive_panel) -> record_info(fields, ah_responsive_panel).

-spec render(#ah_responsive_panel{}) -> html().
render(#ah_responsive_panel{body = Children} = R0) ->
    Classes = ?E:classes(?MODULE, R0),
    {Id, R} = ?L:with_id(R0, <<"ah-responsive-panel">>),
    Anim = ?L:one_of(animation, R#ah_responsive_panel.animation, [fade, slide, none]),
    Bp = ?L:non_neg_int(breakpoint, R#ah_responsive_panel.breakpoint),
    ShowMs = ?L:non_neg_int(show_duration, R#ah_responsive_panel.show_duration),
    HideMs = ?L:non_neg_int(hide_duration, R#ah_responsive_panel.hide_duration),
    AutoClose = ?L:bool(auto_close, R#ah_responsive_panel.auto_close),
    Size = ?L:pos_int(toggle_size, R#ah_responsive_panel.toggle_size),
    Label = R#ah_responsive_panel.toggle_label,
    ContentId = <<Id/binary, "-content">>,
    Load = case R#ah_responsive_panel.load of
               undefined -> [];
               {M, A, _} = Ref when is_atom(M), is_atom(A) -> aihtml:on('ah:load', Ref);
               Other -> error({aihtml, {bad_option, load, Other}})
           end,
    SizePx = <<(integer_to_binary(Size))/binary, "px">>,
    Toggle = ?H:el('div', R#ah_responsive_panel.toggle_content,
                   [<<"ah-responsive-panel-toggle">>],
                   [{role, button}, {tabindex, <<"0">>}, {title, Label}, {aria_label, Label},
                    {aria_expanded, <<"false">>}, {aria_controls, ContentId},
                    {style, Size =/= 30 andalso
                         ?L:style([{<<"width">>, SizePx}, {<<"height">>, SizePx}])}]),
    Content = ?H:el('div', Children, [<<"ah-responsive-panel-content">>],
                    [[{id, ContentId},
                      {style, ?L:style([{<<"height">>, ?L:len(R#ah_responsive_panel.height)}])}],
                     Load]),
    ?H:el('div', [Toggle, Content], Classes,
          [[{id, Id}, {data_ah, <<"responsive-panel">>},
            {data_breakpoint, integer_to_binary(Bp)},
            {data_collapse_width, ?L:len(R#ah_responsive_panel.collapse_width)},
            {data_animation, Anim},
            {data_show_duration, integer_to_binary(ShowMs)},
            {data_hide_duration, integer_to_binary(HideMs)},
            {data_auto_close, not AutoClose andalso <<"false">>},
            {data_toggle_button, R#ah_responsive_panel.toggle_button},
            {aria_disabled, R#ah_responsive_panel.disabled andalso <<"true">>}],
           ?E:root_attrs(R, none)]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => responsive_panel, category => layout,
       signature => <<"responsive_panel(Children, Css, Attrs)">>,
       root => <<"ah-responsive-panel">>, flags => [disabled],
       options => [breakpoint, collapse_width, height, animation, show_duration,
                   hide_duration, auto_close, toggle_button, toggle_size, toggle_content,
                   toggle_label, load],
       behavior => <<"responsive-panel">>,
       events => [<<"ah:collapse">>, <<"ah:expand">>, <<"ah:open">>, <<"ah:close">>,
                  <<"ah:load">>],
       doc => <<"Content shown in place while the parent is wider than a breakpoint; "
                "below it the panel folds into a toggle button that opens the content "
                "as a floating overlay.">>,
       option_docs => #{disabled => <<"Dim the panel and ignore the toggle.">>,
                        breakpoint => <<"Fold when the parent is at most this wide, px "
                                        "(default 1000).">>,
                        collapse_width => <<"Width of the overlay when folded (integer px "
                                            "or CSS length, default 200).">>,
                        height => <<"Height of the content (integer px or CSS length).">>,
                        animation => <<"How the overlay opens and closes: fade (default), "
                                       "slide or none.">>,
                        show_duration => <<"Opening animation time in ms (default 200).">>,
                        hide_duration => <<"Closing animation time in ms (default 200).">>,
                        auto_close => <<"Close the overlay on a click outside it and on "
                                        "Escape (default true).">>,
                        toggle_button => <<"Selector of another element that also toggles "
                                           "the overlay.">>,
                        toggle_size => <<"Size of the built-in toggle in px (default 30).">>,
                        toggle_content => <<"Html of the toggle (default ☰)."/utf8>>,
                        toggle_label => <<"Accessible label of the toggle (default Toggle "
                                          "panel).">>,
                        load => <<"Action ref {M, A, Args} fired as ah:load on the content "
                                  "the first time it is shown; the action fills it, e.g. "
                                  "aihtml_action:html(Ctx, {id, Id}, Html) with the "
                                  "event's id.">>},
       methods => [#{name => open, args => <<"()">>, doc => <<"Open the overlay (when folded).">>},
                   #{name => close, args => <<"()">>, doc => <<"Close the overlay.">>},
                   #{name => toggle, args => <<"()">>, doc => <<"Open or close the overlay.">>},
                   #{name => refresh, args => <<"()">>,
                     doc => <<"Check the parent's width against the breakpoint again.">>},
                   #{name => isCollapsed, args => <<"()">>,
                     doc => <<"Return true while folded.">>},
                   #{name => isOpen, args => <<"()">>,
                     doc => <<"Return true while the overlay is open.">>}]}].
