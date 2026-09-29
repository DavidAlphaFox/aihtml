%%%-------------------------------------------------------------------
%%% @doc Scrolling layout components, ported from sigil (DOM and classes
%%% as sigil renders them, so the styles in priv/css/sigil apply):
%%%
%%%   scrollview/3        a horizontal pager (carousel): one page at a time,
%%%                       dragged or swiped, dots to pick a page, optional
%%%                       slide show; value = the current page index
%%%   scrollbar/3         a custom scrollbar: standalone with a value
%%%                       (min..max) when `Children' is empty, otherwise a
%%%                       scroll area whose content gets custom bars
%%%   responsive_panel/3  content shown in place while the parent is wide,
%%%                       folded into a toggle and a floating overlay when
%%%                       it is narrower than a breakpoint
%%%
%%% Each function builds an element record (#ah_scrollview{} ..., defined
%%% in include/aihtml_layout_scroll.hrl) and render/1 turns it into HTML,
%%% so pages may also write the records directly (designs/05-records.md).
%%%
%%% Value-bearing components (scrollview, standalone scrollbar) keep their
%%% value in `data-ah-value' on the root, render a hidden input when they
%%% have a `name' and fire `change' on the root when the user changes the
%%% value (designs/04-components.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_layout_scroll).
-behaviour(aihtml_element).

-include("aihtml_layout_scroll.hrl").

-export([scrollview/3, scrollbar/3, responsive_panel/3,
         render/1, fields/1, catalog/0]).

-export_type([element/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
-type element() :: #ah_scrollview{} | #ah_scrollbar{} | #ah_responsive_panel{}.

%%%===================================================================
%%% Builders
%%%===================================================================

%% @doc A horizontal pager. `Pages' is a list; each item is one page.
%% Options: current_page (0-based), width, height, show_buttons,
%% slide_show, slide_duration, animation_duration, move_threshold,
%% bounce, label. A `name' in `Attrs' adds a hidden input holding the
%% page index.
-spec scrollview([html()], css(), attrs()) -> #ah_scrollview{}.
scrollview(Pages, Css, Attrs) ->
    build(#ah_scrollview{body = Pages}, Css, Attrs).

%% @doc A custom scrollbar. With `[]' as `Children' it is a standalone bar
%% (value, min, max, step, large_step, thumb_min_size, show_buttons,
%% width / height, label); with content it is a scroll area of the given
%% `height' / `width' whose vertical and horizontal bars appear when the
%% content overflows.
-spec scrollbar(html(), css(), attrs()) -> #ah_scrollbar{}.
scrollbar(Children, Css, Attrs) ->
    build(#ah_scrollbar{body = Children}, Css, Attrs).

%% @doc A panel that folds into a toggle button when its parent is not
%% wider than `breakpoint' px; the toggle then opens the content as a
%% floating overlay. Options: breakpoint, collapse_width, height,
%% animation (fade | slide | none), show_duration, hide_duration,
%% auto_close, toggle_button (a selector of an extra toggle), toggle_size,
%% toggle_content, toggle_label, load (an action ref fired the first
%% time the content is shown).
-spec responsive_panel(html(), css(), attrs()) -> #ah_responsive_panel{}.
responsive_panel(Children, Css, Attrs) ->
    build(#ah_responsive_panel{body = Children}, Css, Attrs).

build(R, Css, Attrs) ->
    Tag = element(1, R),
    ?E:build(R, fields(Tag), entry(?E:component_name(Tag)), Css, Attrs).

%% @doc The field names of one of this group's records.
-spec fields(atom()) -> [atom()].
fields(ah_scrollview) -> record_info(fields, ah_scrollview);
fields(ah_scrollbar) -> record_info(fields, ah_scrollbar);
fields(ah_responsive_panel) -> record_info(fields, ah_responsive_panel).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_scrollview{body = Pages} = R0) ->
    Classes = classes(R0),
    {Id, R} = with_id(R0, <<"ah-scrollview">>),
    is_list(Pages) orelse error({aihtml, {bad_option, body, Pages}}),
    N = length(Pages),
    Cur0 = R#ah_scrollview.current_page,
    (is_integer(Cur0) andalso Cur0 >= 0) orelse error({aihtml, {bad_option, current_page, Cur0}}),
    Cur = min(Cur0, max(0, N - 1)),
    Show = bool(show_buttons, R#ah_scrollview.show_buttons),
    Slide = bool(slide_show, R#ah_scrollview.slide_show),
    Bounce = bool(bounce, R#ah_scrollview.bounce),
    SlideMs = pos_int(slide_duration, R#ah_scrollview.slide_duration),
    AnimMs = non_neg_int(animation_duration, R#ah_scrollview.animation_duration),
    Threshold = case R#ah_scrollview.move_threshold of
                    T when is_number(T), T > 0, T =< 1 -> T;
                    T -> error({aihtml, {bad_option, move_threshold, T}})
                end,
    Disabled = R#ah_scrollview.disabled,
    Total = integer_to_binary(N),
    PageEls =
        [el('div', P, [<<"ah-scrollview-page">>],
            [{role, group}, {aria_roledescription, <<"slide">>},
             {aria_label, <<(integer_to_binary(I + 1))/binary, " / ", Total/binary>>},
             {aria_hidden, I =/= Cur andalso <<"true">>}, {inert, I =/= Cur}])
         || {I, P} <- lists:zip(lists:seq(0, N - 1), Pages)],
    Bullets =
        [el(span, [], [<<"ah-scrollview-button">>,
                       [<<"ah-scrollview-button-active">> || I =:= Cur]],
            [{role, button}, {tabindex, <<"-1">>},
             {aria_label, <<"Page ", (integer_to_binary(I + 1))/binary>>},
             {aria_current, I =:= Cur andalso <<"true">>}])
         || I <- lists:seq(0, N - 1)],
    Margin = case Cur of
                 0 -> undefined;
                 _ -> <<"margin-left:-", (integer_to_binary(Cur * 100))/binary, "%;">>
             end,
    WrapStyle = iolist_to_binary(
                  [[Margin || Margin =/= undefined],
                   [[<<"transition-duration:">>, integer_to_binary(AnimMs), <<"ms;">>]
                    || AnimMs =/= 300]]),
    V = integer_to_binary(Cur),
    el('div',
       [el('div', PageEls, [<<"ah-scrollview-wrapper">>],
           [{id, <<Id/binary, "-pages">>},
            {style, WrapStyle =/= <<>> andalso WrapStyle},
            {aria_live, not Slide andalso <<"polite">>}]),
        el('div', Bullets, [<<"ah-scrollview-buttons">>],
           [{role, group}, {aria_label, <<"Pages">>},
            {style, not Show andalso <<"display:none">>}]),
        hidden(R#ah_scrollview.name, V)],
       Classes,
       [[{id, Id}, {data_ah, <<"scrollview">>}, {data_ah_value, V},
         {role, region}, {aria_roledescription, <<"carousel">>},
         {aria_label, R#ah_scrollview.label},
         {tabindex, case Disabled of true -> <<"-1">>; false -> <<"0">> end},
         {aria_disabled, Disabled andalso <<"true">>},
         {style, style([{<<"width">>, len(R#ah_scrollview.width)},
                        {<<"height">>, len(R#ah_scrollview.height)}])},
         {data_slide_show, Slide},
         {data_slide_duration, SlideMs =/= 3000 andalso integer_to_binary(SlideMs)},
         {data_duration, AnimMs =/= 300 andalso integer_to_binary(AnimMs)},
         {data_threshold, Threshold /= 0.5 andalso num(Threshold)},
         {data_bounce, not Bounce andalso <<"false">>}],
        ?E:root_attrs(R, change)]);

render(#ah_scrollbar{body = []} = R0) ->
    Classes = classes(R0),
    {Id, R} = with_id(R0, <<"ah-scrollbar">>),
    #ah_scrollbar{orientation = Orient, min = Min, max = Max, step = Step,
                  large_step = Large, disabled = Disabled} = R,
    [is_number(X) orelse error({aihtml, {bad_option, F, X}})
     || {F, X} <- [{min, Min}, {max, Max}, {step, Step}, {large_step, Large},
                   {value, R#ah_scrollbar.value}]],
    Max >= Min orelse error({aihtml, {bad_option, max, Max}}),
    Value = max(Min, min(Max, R#ah_scrollbar.value)),
    ThumbMin = non_neg_int(thumb_min_size, R#ah_scrollbar.thumb_min_size),
    Buttons = bool(show_buttons, R#ah_scrollbar.show_buttons),
    V = num(Value),
    el('div',
       [bar(Orient), hidden(R#ah_scrollbar.name, V)],
       Classes,
       [[{id, Id}, {data_ah, <<"scrollbar">>}, {data_ah_value, V},
         {role, scrollbar}, {aria_orientation, Orient},
         {aria_valuemin, num(Min)}, {aria_valuemax, num(Max)}, {aria_valuenow, V},
         {aria_label, R#ah_scrollbar.label},
         {aria_disabled, Disabled andalso <<"true">>},
         {tabindex, case Disabled of true -> <<"-1">>; false -> <<"0">> end},
         {data_min, num(Min)}, {data_max, num(Max)}, {data_step, num(Step)},
         {data_large_step, num(Large)}, {data_thumb_min, integer_to_binary(ThumbMin)},
         {data_buttons, not Buttons andalso <<"false">>},
         {style, style([{<<"width">>, len(R#ah_scrollbar.width)},
                        {<<"height">>, len(R#ah_scrollbar.height)}])}],
        ?E:root_attrs(R, change)]);

render(#ah_scrollbar{body = Children} = R0) ->
    Classes = classes(R0),
    {Id, R} = with_id(R0, <<"ah-scrollbar">>),
    Buttons = bool(show_buttons, R#ah_scrollbar.show_buttons),
    ThumbMin = non_neg_int(thumb_min_size, R#ah_scrollbar.thumb_min_size),
    Step = R#ah_scrollbar.step,
    is_number(Step) orelse error({aihtml, {bad_option, step, Step}}),
    el('div',
       [el('div', el('div', Children, [<<"ah-scrollbar-content">>], []),
           [<<"ah-scrollbar-viewport">>],
           [{id, <<Id/binary, "-viewport">>}, {tabindex, <<"0">>},
            {role, R#ah_scrollbar.label =/= undefined andalso region},
            {aria_label, R#ah_scrollbar.label}]),
        bar(vertical), bar(horizontal),
        el('div', [], [<<"ah-scrollbar-corner">>], [])],
       [Classes, <<"ah-scrollbar-area">>],
       [[{id, Id}, {data_ah, <<"scrollbar">>}, {data_area, true},
         {data_step, num(Step)}, {data_thumb_min, integer_to_binary(ThumbMin)},
         {data_buttons, not Buttons andalso <<"false">>},
         {style, style([{<<"width">>, len(R#ah_scrollbar.width)},
                        {<<"height">>, len(R#ah_scrollbar.height)}])}],
        ?E:root_attrs(R, none)]);

render(#ah_responsive_panel{body = Children} = R0) ->
    Classes = classes(R0),
    {Id, R} = with_id(R0, <<"ah-responsive-panel">>),
    Anim = one_of(animation, R#ah_responsive_panel.animation, [fade, slide, none]),
    Bp = non_neg_int(breakpoint, R#ah_responsive_panel.breakpoint),
    ShowMs = non_neg_int(show_duration, R#ah_responsive_panel.show_duration),
    HideMs = non_neg_int(hide_duration, R#ah_responsive_panel.hide_duration),
    AutoClose = bool(auto_close, R#ah_responsive_panel.auto_close),
    Size = pos_int(toggle_size, R#ah_responsive_panel.toggle_size),
    Label = R#ah_responsive_panel.toggle_label,
    ContentId = <<Id/binary, "-content">>,
    Load = case R#ah_responsive_panel.load of
               undefined -> [];
               {M, A, _} = Ref when is_atom(M), is_atom(A) -> aihtml:on('ah:load', Ref);
               Other -> error({aihtml, {bad_option, load, Other}})
           end,
    SizePx = <<(integer_to_binary(Size))/binary, "px">>,
    Toggle = el('div', R#ah_responsive_panel.toggle_content,
                [<<"ah-responsive-panel-toggle">>],
                [{role, button}, {tabindex, <<"0">>}, {title, Label}, {aria_label, Label},
                 {aria_expanded, <<"false">>}, {aria_controls, ContentId},
                 {style, Size =/= 30 andalso
                      style([{<<"width">>, SizePx}, {<<"height">>, SizePx}])}]),
    Content = el('div', Children, [<<"ah-responsive-panel-content">>],
                 [[{id, ContentId},
                   {style, style([{<<"height">>, len(R#ah_responsive_panel.height)}])}],
                  Load]),
    el('div', [Toggle, Content], Classes,
       [[{id, Id}, {data_ah, <<"responsive-panel">>},
         {data_breakpoint, integer_to_binary(Bp)},
         {data_collapse_width, len(R#ah_responsive_panel.collapse_width)},
         {data_animation, Anim},
         {data_show_duration, integer_to_binary(ShowMs)},
         {data_hide_duration, integer_to_binary(HideMs)},
         {data_auto_close, not AutoClose andalso <<"false">>},
         {data_toggle_button, R#ah_responsive_panel.toggle_button},
         {aria_disabled, R#ah_responsive_panel.disabled andalso <<"true">>}],
        ?E:root_attrs(R, none)]).

%% sigil's scrollbar markup; the behaviour sizes the parts.
bar(Orient) ->
    el('div', [el('div', [], [<<"ah-scrollbar-btn-up">>], []),
               el('div', [], [<<"ah-scrollbar-track-up">>], []),
               el('div', [], [<<"ah-scrollbar-thumb">>], []),
               el('div', [], [<<"ah-scrollbar-track-down">>], []),
               el('div', [], [<<"ah-scrollbar-btn-down">>], [])],
       [<<"ah-scrollbar">>, <<"ah-scrollbar-", (atom_to_binary(Orient, utf8))/binary>>],
       [{aria_hidden, <<"true">>}]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => scrollview, category => layout,
       signature => <<"scrollview(Pages, Css, Attrs)">>,
       root => <<"ah-scrollview">>, flags => [disabled],
       options => [current_page, width, height, show_buttons, slide_show, slide_duration,
                   animation_duration, move_threshold, bounce, label],
       behavior => <<"scrollview">>, events => [<<"change">>, <<"ah:page-changed">>],
       doc => <<"A horizontal pager showing one page at a time: drag or swipe, arrow "
                "keys and dots change the page, optionally on a timer. The value is "
                "the 0-based page index.">>,
       option_docs => #{disabled => <<"Dim the pager and ignore input.">>,
                        current_page => <<"Initial page, 0-based (default 0).">>,
                        width => <<"Width (integer px or CSS length); default the container's.">>,
                        height => <<"Height (integer px or CSS length); default the page content's.">>,
                        show_buttons => <<"Show the page dots (default true).">>,
                        slide_show => <<"Advance to the next page on a timer, wrapping around; "
                                        "pauses while hovered or focused.">>,
                        slide_duration => <<"Slide show interval in ms (default 3000).">>,
                        animation_duration => <<"Page transition time in ms (default 300).">>,
                        move_threshold => <<"Share of the width a drag must cover to change "
                                            "the page (default 0.5).">>,
                        bounce => <<"Let a drag pull past the first and last page and "
                                    "spring back (default true).">>,
                        label => <<"Accessible name of the carousel (default Carousel).">>},
       methods => [#{name => setValue, args => <<"(Index)">>,
                     doc => <<"Go to a page (animated) without firing change.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"Return the page index.">>},
                   #{name => forward, args => <<"()">>,
                     doc => <<"Go to the next page, if any, without firing change.">>},
                   #{name => back, args => <<"()">>,
                     doc => <<"Go to the previous page, if any, without firing change.">>},
                   #{name => startSlideShow, args => <<"()">>, doc => <<"Start the timer.">>},
                   #{name => stopSlideShow, args => <<"()">>, doc => <<"Stop the timer.">>},
                   #{name => refresh, args => <<"()">>,
                     doc => <<"Re-read the pages after their markup changed.">>}]},
     #{name => scrollbar, category => layout,
       signature => <<"scrollbar(Children, Css, Attrs)">>,
       root => <<"ah-scrollbar-host">>,
       groups => #{orientation => {[horizontal, vertical], horizontal}},
       flags => [disabled],
       classes => #{horizontal => [], vertical => [<<"ah-scrollbar-host-vertical">>],
                    disabled => [<<"ah-scrollbar-disabled">>]},
       options => [value, min, max, step, large_step, thumb_min_size, show_buttons,
                   width, height, label],
       behavior => <<"scrollbar">>, events => [<<"change">>, <<"input">>],
       doc => <<"A custom scrollbar with arrow buttons, a draggable thumb and a clickable "
                "track. Without children it is a standalone bar with a value (min..max); "
                "with children it is a scroll area whose bars follow the content.">>,
       option_docs => #{horizontal => <<"A horizontal standalone bar (default).">>,
                        vertical => <<"A vertical standalone bar (default height 200px).">>,
                        disabled => <<"Dim the bar and ignore input.">>,
                        value => <<"Standalone: the initial value (default 0).">>,
                        min => <<"Standalone: the smallest value (default 0).">>,
                        max => <<"Standalone: the largest value (default 1000).">>,
                        step => <<"Change per arrow click or arrow key (default 10; "
                                  "px in a scroll area).">>,
                        large_step => <<"Standalone: change per track click or Page key "
                                        "(default 50).">>,
                        thumb_min_size => <<"Smallest thumb length in px (default 10).">>,
                        show_buttons => <<"Show the arrow buttons at both ends (default true).">>,
                        width => <<"Width (integer px or CSS length).">>,
                        height => <<"Height (integer px or CSS length); a scroll area "
                                    "needs one to scroll vertically.">>,
                        label => <<"Accessible name of the bar, or of the scroll area.">>},
       methods => [#{name => setValue, args => <<"(Value)">>,
                     doc => <<"Standalone: set the value without firing change.">>},
                   #{name => getValue, args => <<"()">>,
                     doc => <<"Standalone: return the value.">>},
                   #{name => setMax, args => <<"(Max)">>,
                     doc => <<"Standalone: change the largest value.">>},
                   #{name => scrollTo, args => <<"(X, Y)">>,
                     doc => <<"Scroll area: scroll the content to X, Y px.">>},
                   #{name => refresh, args => <<"()">>,
                     doc => <<"Measure again and lay out the bars.">>}]},
     #{name => responsive_panel, category => layout,
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

%%%===================================================================
%%% Internal
%%%===================================================================

entry(Name) -> aihtml_catalog:entry(?MODULE, Name).

el(Tag, Children, Css, Attrs) -> ?H:el(Tag, Children, Css, Attrs).

classes(R) ->
    Tag = element(1, R),
    ?E:classes(R, fields(Tag), entry(?E:component_name(Tag))).

%% The root id as a binary and the record carrying it: the `id' field, an
%% id among the attrs, or a generated Prefix-N.
with_id(R, Prefix) ->
    Id = case element(3, R) of
             undefined ->
                 case lists:keyfind(<<"id">>, 1, ?H:attrs(element(5, R))) of
                     {_, V} when is_binary(V) -> V;
                     _ -> <<Prefix/binary, "-",
                            (integer_to_binary(erlang:unique_integer([positive])))/binary>>
                 end;
             V -> bin(V)
         end,
    {Id, setelement(3, R, Id)}.

bool(Field, V) -> one_of(Field, V, [true, false]).

one_of(Field, V, Allowed) ->
    lists:member(V, Allowed) orelse error({aihtml, {bad_option, Field, V}}),
    V.

pos_int(_Field, N) when is_integer(N), N > 0 -> N;
pos_int(Field, V) -> error({aihtml, {bad_option, Field, V}}).

non_neg_int(_Field, N) when is_integer(N), N >= 0 -> N;
non_neg_int(Field, V) -> error({aihtml, {bad_option, Field, V}}).

hidden(undefined, _) -> [];
hidden(Name, Value) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

num(I) when is_integer(I) -> integer_to_binary(I);
num(F) when is_float(F) -> float_to_binary(F, [short]).

bin(B) when is_binary(B) -> B;
bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(L) when is_list(L) -> unicode:characters_to_binary(L).

len(undefined) -> undefined;
len(N) when is_integer(N) -> <<(integer_to_binary(N))/binary, "px">>;
len(V) when is_binary(V); is_list(V) -> bin(V);
len(V) -> error({aihtml, {bad_length, V}}).

style(Decls) ->
    case [[K, $:, V, $;] || {K, V} <- Decls, V =/= false, V =/= undefined] of
        [] -> undefined;
        S -> iolist_to_binary(S)
    end.
