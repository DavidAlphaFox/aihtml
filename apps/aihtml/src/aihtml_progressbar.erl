%%%-------------------------------------------------------------------
%%% @doc The progressbar component (designs/04-components.md): `progressbar/3'
%%% builds an #ah_progressbar{} element record (include/aihtml_progressbar.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_progressbar).
-behaviour(aihtml_element).

-include("aihtml_progressbar.hrl").

-export([progressbar/3, render/1, fields/1, catalog/0]).

-import(aihtml_lib_display, [num/1, method/3]).
-import(aihtml_lib_progress, [clamp/3, pct/3]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Linear progress. `Value' is clamped to [min, max]; with the
%% `indeterminate' flag it may be `undefined'. Options: `min' (0),
%% `max' (100), `text' (label instead of the percentage), `color_ranges'
%% (`[{Stop, Color}]', Color a colour atom or a CSS colour).
-spec progressbar(number() | undefined, css(), attrs()) -> #ah_progressbar{}.
progressbar(Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_progressbar{value = Value}, Css, Attrs).

%% @doc The field names of #ah_progressbar{}.
-spec fields(atom()) -> [atom()].
fields(ah_progressbar) -> record_info(fields, ah_progressbar).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_progressbar{}) -> html().
render(#ah_progressbar{value = Value, min = Min, max = Max, indeterminate = Indet,
                       orientation = Orientation, text = Text} = R) ->
    Cls = ?E:classes(?MODULE, R),
    V = clamp(if is_number(Value) -> Value; true -> Min end, Min, Max),
    Pct = pct(V, Min, Max),
    Vert = Orientation =:= vertical,
    Dim = if Vert -> <<"height">>; true -> <<"width">> end,
    Size = fun(X) -> iolist_to_binary([Dim, <<": ">>, num(X), <<"%;">>]) end,
    Ranges = R#ah_progressbar.color_ranges,
    N = length(Ranges),
    RangeEls = [?H:el('div', [], [<<"ah-progressbar-range">>],
                      [{data_range_index, I - 1}, {data_ah_stop, Stop},
                       {style, iolist_to_binary(
                                 [<<"background-color: ">>, aihtml_lib_color:css(C),
                                  <<"; z-index: ">>, integer_to_binary(N - I + 1), <<"; ">>,
                                  Size(pct(lists:min([Stop, Max, V]), Min, Max))])}])
                || {I, {Stop, C}} <- lists:enumerate(Ranges)],
    Label = case Text of undefined -> [integer_to_binary(round(Pct)), <<"%">>]; _ -> Text end,
    ShowText = R#ah_progressbar.show_text andalso not Indet,
    ?H:el('div',
          [?H:el('div', [], [if Vert -> <<"ah-progressbar-value-vertical">>;
                                true -> <<"ah-progressbar-value">> end],
                 [{style, not Indet andalso Size(Pct)}]),
           RangeEls,
           ?H:el('div', ?H:el(span, Label, [<<"ah-progressbar-text">>],
                              [{style, not ShowText andalso <<"display: none;">>}]),
                 [<<"ah-progressbar-text-host">>], [])],
          Cls,
          [[{role, <<"progressbar">>}, {aria_valuemin, Min}, {aria_valuemax, Max},
            {aria_valuenow, not Indet andalso V},
            {aria_valuetext, not Indet andalso iolist_to_binary(Label)},
            {aria_orientation, Orientation},
            {aria_busy, Indet andalso <<"true">>},
            {aria_disabled, R#ah_progressbar.disabled andalso <<"true">>},
            {data_ah, <<"progressbar">>}, {data_ah_value, not Indet andalso V},
            {data_ah_min, Min}, {data_ah_max, Max},
            {data_ah_text, Text =/= undefined andalso <<"custom">>}],
           ?E:root_attrs(R, change)]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => progressbar, category => data, root => <<"ah-progressbar">>,
      signature => <<"progressbar(Value, Css, Attrs)">>,
      groups => #{orientation => {[horizontal, vertical], horizontal},
                  layout => {[normal, reverse], normal},
                  color => {[primary, success, warning, error, info], primary}},
      flags => [show_text, disabled, indeterminate, striped, animated],
      classes => #{normal => [], primary => [], show_text => []},
      options => [min, max, text, color_ranges], behavior => <<"progressbar">>,
      events => [<<"change">>, <<"ah:complete">>],
      doc => <<"Linear progress with optional text, colour ranges, stripes and an "
               "indeterminate state. Methods setValue(v[, text]), getValue().">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{min => <<"Lower bound (default 0).">>,
                       max => <<"Upper bound (default 100).">>,
                       text => <<"Label shown instead of the percentage.">>,
                       color_ranges => <<"[{Stop, Color}]: bands filled up to each stop; Color is a "
                                         "theme colour atom or a CSS colour.">>,
                       show_text => <<"Show the percentage (or text) on the bar.">>,
                       disabled => <<"Dimmed.">>,
                       indeterminate => <<"Unknown progress: a sliding bar, no aria-valuenow.">>,
                       striped => <<"Diagonal stripes on the fill.">>,
                       animated => <<"Move the stripes.">>},
      methods => [method(setValue, <<"(Value[, Text])">>, <<"Set the value (clamped); fires change, and "
                                                       "ah:complete at max.">>),
                  method(getValue, <<"()">>, <<"Return the current value.">>)]}.
