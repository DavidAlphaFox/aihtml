%%%-------------------------------------------------------------------
%%% @doc ScrollView, ported from sigil (DOM and classes as sigil renders
%%% them, so the styles in priv/css/sigil apply): a horizontal pager
%%% (carousel) showing one page at a time, dragged or swiped, dots to pick
%%% a page, optional slide show; the value is the current page index.
%%%
%%% scrollview/3 builds an element record (#ah_scrollview{}, defined in
%%% include/aihtml_scrollview.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%%
%%% The value is kept in `data-ah-value' on the root, a `name' renders a
%%% hidden input, and `change' fires on the root when the user changes
%%% the page (designs/04-components.md). The behaviour is in
%%% assets/js/components/scrollview.ts.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_scrollview).
-behaviour(aihtml_element).

-include("aihtml_scrollview.hrl").

-export([scrollview/3, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_scroll).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% @doc A horizontal pager. `Pages' is a list; each item is one page.
%% Options: current_page (0-based), width, height, show_buttons,
%% slide_show, slide_duration, animation_duration, move_threshold,
%% bounce, label. A `name' in `Attrs' adds a hidden input holding the
%% page index.
-spec scrollview([html()], css(), attrs()) -> #ah_scrollview{}.
scrollview(Pages, Css, Attrs) ->
    ?E:build(?MODULE, #ah_scrollview{body = Pages}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_scrollview) -> record_info(fields, ah_scrollview).

-spec render(#ah_scrollview{}) -> html().
render(#ah_scrollview{body = Pages} = R0) ->
    Classes = ?E:classes(?MODULE, R0),
    {Id, R} = ?L:with_id(R0, <<"ah-scrollview">>),
    is_list(Pages) orelse error({aihtml, {bad_option, body, Pages}}),
    N = length(Pages),
    Cur0 = R#ah_scrollview.current_page,
    (is_integer(Cur0) andalso Cur0 >= 0) orelse error({aihtml, {bad_option, current_page, Cur0}}),
    Cur = min(Cur0, max(0, N - 1)),
    Show = ?L:bool(show_buttons, R#ah_scrollview.show_buttons),
    Slide = ?L:bool(slide_show, R#ah_scrollview.slide_show),
    Bounce = ?L:bool(bounce, R#ah_scrollview.bounce),
    SlideMs = ?L:pos_int(slide_duration, R#ah_scrollview.slide_duration),
    AnimMs = ?L:non_neg_int(animation_duration, R#ah_scrollview.animation_duration),
    Threshold = case R#ah_scrollview.move_threshold of
                    T when is_number(T), T > 0, T =< 1 -> T;
                    T -> error({aihtml, {bad_option, move_threshold, T}})
                end,
    Disabled = R#ah_scrollview.disabled,
    Total = integer_to_binary(N),
    PageEls =
        [?H:el('div', P, [<<"ah-scrollview-page">>],
               [{role, group}, {aria_roledescription, <<"slide">>},
                {aria_label, <<(integer_to_binary(I + 1))/binary, " / ", Total/binary>>},
                {aria_hidden, I =/= Cur andalso <<"true">>}, {inert, I =/= Cur}])
         || {I, P} <- lists:zip(lists:seq(0, N - 1), Pages)],
    Bullets =
        [?H:el(span, [], [<<"ah-scrollview-button">>,
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
    ?H:el('div',
          [?H:el('div', PageEls, [<<"ah-scrollview-wrapper">>],
                 [{id, <<Id/binary, "-pages">>},
                  {style, WrapStyle =/= <<>> andalso WrapStyle},
                  {aria_live, not Slide andalso <<"polite">>}]),
           ?H:el('div', Bullets, [<<"ah-scrollview-buttons">>],
                 [{role, group}, {aria_label, <<"Pages">>},
                  {style, not Show andalso <<"display:none">>}]),
           ?L:hidden(R#ah_scrollview.name, V)],
          Classes,
          [[{id, Id}, {data_ah, <<"scrollview">>}, {data_ah_value, V},
            {role, region}, {aria_roledescription, <<"carousel">>},
            {aria_label, R#ah_scrollview.label},
            {tabindex, case Disabled of true -> <<"-1">>; false -> <<"0">> end},
            {aria_disabled, Disabled andalso <<"true">>},
            {style, ?L:style([{<<"width">>, ?L:len(R#ah_scrollview.width)},
                              {<<"height">>, ?L:len(R#ah_scrollview.height)}])},
            {data_slide_show, Slide},
            {data_slide_duration, SlideMs =/= 3000 andalso integer_to_binary(SlideMs)},
            {data_duration, AnimMs =/= 300 andalso integer_to_binary(AnimMs)},
            {data_threshold, Threshold /= 0.5 andalso ?L:num(Threshold)},
            {data_bounce, not Bounce andalso <<"false">>}],
           ?E:root_attrs(R, change)]).

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
                     doc => <<"Re-read the pages after their markup changed.">>}]}].
