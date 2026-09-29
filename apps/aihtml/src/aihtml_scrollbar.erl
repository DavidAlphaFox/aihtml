%%%-------------------------------------------------------------------
%%% @doc Scrollbar, ported from sigil (DOM and classes as sigil renders
%%% them, so the styles in priv/css/sigil apply): a custom scrollbar,
%%% standalone with a value (min..max) when `Children' is empty, otherwise
%%% a scroll area whose content gets custom bars.
%%%
%%% scrollbar/3 builds an element record (#ah_scrollbar{}, defined in
%%% include/aihtml_scrollbar.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%%
%%% A standalone bar keeps its value in `data-ah-value' on the root, a
%%% `name' renders a hidden input, and `change' fires on the root when the
%%% user changes the value (designs/04-components.md). The behaviour is in
%%% assets/js/components/scrollbar.js.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_scrollbar).
-behaviour(aihtml_element).

-include("aihtml_scrollbar.hrl").

-export([scrollbar/3, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_scroll).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% @doc A custom scrollbar. With `[]' as `Children' it is a standalone bar
%% (value, min, max, step, large_step, thumb_min_size, show_buttons,
%% width / height, label); with content it is a scroll area of the given
%% `height' / `width' whose vertical and horizontal bars appear when the
%% content overflows.
-spec scrollbar(html(), css(), attrs()) -> #ah_scrollbar{}.
scrollbar(Children, Css, Attrs) ->
    ?E:build(?MODULE, #ah_scrollbar{body = Children}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(atom()) -> [atom()].
fields(ah_scrollbar) -> record_info(fields, ah_scrollbar).

-spec render(#ah_scrollbar{}) -> html().
render(#ah_scrollbar{body = []} = R0) ->
    Classes = ?E:classes(?MODULE, R0),
    {Id, R} = ?L:with_id(R0, <<"ah-scrollbar">>),
    #ah_scrollbar{orientation = Orient, min = Min, max = Max, step = Step,
                  large_step = Large, disabled = Disabled} = R,
    [is_number(X) orelse error({aihtml, {bad_option, F, X}})
     || {F, X} <- [{min, Min}, {max, Max}, {step, Step}, {large_step, Large},
                   {value, R#ah_scrollbar.value}]],
    Max >= Min orelse error({aihtml, {bad_option, max, Max}}),
    Value = max(Min, min(Max, R#ah_scrollbar.value)),
    ThumbMin = ?L:non_neg_int(thumb_min_size, R#ah_scrollbar.thumb_min_size),
    Buttons = ?L:bool(show_buttons, R#ah_scrollbar.show_buttons),
    V = ?L:num(Value),
    ?H:el('div',
          [bar(Orient), ?L:hidden(R#ah_scrollbar.name, V)],
          Classes,
          [[{id, Id}, {data_ah, <<"scrollbar">>}, {data_ah_value, V},
            {role, scrollbar}, {aria_orientation, Orient},
            {aria_valuemin, ?L:num(Min)}, {aria_valuemax, ?L:num(Max)}, {aria_valuenow, V},
            {aria_label, R#ah_scrollbar.label},
            {aria_disabled, Disabled andalso <<"true">>},
            {tabindex, case Disabled of true -> <<"-1">>; false -> <<"0">> end},
            {data_min, ?L:num(Min)}, {data_max, ?L:num(Max)}, {data_step, ?L:num(Step)},
            {data_large_step, ?L:num(Large)}, {data_thumb_min, integer_to_binary(ThumbMin)},
            {data_buttons, not Buttons andalso <<"false">>},
            {style, ?L:style([{<<"width">>, ?L:len(R#ah_scrollbar.width)},
                              {<<"height">>, ?L:len(R#ah_scrollbar.height)}])}],
           ?E:root_attrs(R, change)]);

render(#ah_scrollbar{body = Children} = R0) ->
    Classes = ?E:classes(?MODULE, R0),
    {Id, R} = ?L:with_id(R0, <<"ah-scrollbar">>),
    Buttons = ?L:bool(show_buttons, R#ah_scrollbar.show_buttons),
    ThumbMin = ?L:non_neg_int(thumb_min_size, R#ah_scrollbar.thumb_min_size),
    Step = R#ah_scrollbar.step,
    is_number(Step) orelse error({aihtml, {bad_option, step, Step}}),
    ?H:el('div',
          [?H:el('div', ?H:el('div', Children, [<<"ah-scrollbar-content">>], []),
                 [<<"ah-scrollbar-viewport">>],
                 [{id, <<Id/binary, "-viewport">>}, {tabindex, <<"0">>},
                  {role, R#ah_scrollbar.label =/= undefined andalso region},
                  {aria_label, R#ah_scrollbar.label}]),
           bar(vertical), bar(horizontal),
           ?H:el('div', [], [<<"ah-scrollbar-corner">>], [])],
          [Classes, <<"ah-scrollbar-area">>],
          [[{id, Id}, {data_ah, <<"scrollbar">>}, {data_area, true},
            {data_step, ?L:num(Step)}, {data_thumb_min, integer_to_binary(ThumbMin)},
            {data_buttons, not Buttons andalso <<"false">>},
            {style, ?L:style([{<<"width">>, ?L:len(R#ah_scrollbar.width)},
                              {<<"height">>, ?L:len(R#ah_scrollbar.height)}])}],
           ?E:root_attrs(R, none)]).

%% sigil's scrollbar markup; the behaviour sizes the parts.
bar(Orient) ->
    ?H:el('div', [?H:el('div', [], [<<"ah-scrollbar-btn-up">>], []),
                  ?H:el('div', [], [<<"ah-scrollbar-track-up">>], []),
                  ?H:el('div', [], [<<"ah-scrollbar-thumb">>], []),
                  ?H:el('div', [], [<<"ah-scrollbar-track-down">>], []),
                  ?H:el('div', [], [<<"ah-scrollbar-btn-down">>], [])],
          [<<"ah-scrollbar">>, <<"ah-scrollbar-", (atom_to_binary(Orient, utf8))/binary>>],
          [{aria_hidden, <<"true">>}]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => scrollbar, category => layout,
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
                     doc => <<"Measure again and lay out the bars.">>}]}].
