%%%-------------------------------------------------------------------
%%% @doc The meter component (designs/04-components.md): `meter/3'
%%% builds an #ah_meter{} element record (include/aihtml_meter.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_meter).
-behaviour(aihtml_element).

-include("aihtml_meter.hrl").

-export([meter/3, render/1, fields/1, catalog/0]).

-import(aihtml_lib_display, [blank/1, num/1, none_for/1]).
-import(aihtml_lib_progress, [clamp/3, pct/3]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Meter: a measurement in a known range, coloured low / optimum /
%% high like the native `<meter>'. Options: `min' (0), `max' (100), `low',
%% `high', `optimum', `label', `helper_text', `show_value' (false).
-spec meter(number(), css(), attrs()) -> #ah_meter{}.
meter(Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_meter{value = Value}, Css, Attrs).

%% @doc The field names of #ah_meter{}.
-spec fields(atom()) -> [atom()].
fields(ah_meter) -> record_info(fields, ah_meter).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_meter{}) -> html().
render(#ah_meter{value = Value, min = Min, max = Max, label = Label,
                 helper_text = Helper} = R) ->
    Cls = ?E:classes(?MODULE, R),
    ShowValue = R#ah_meter.show_value =:= true,
    State = meter_state(Value, Min, Max, R#ah_meter.low, R#ah_meter.high,
                        R#ah_meter.optimum),
    ?H:el('div',
          [[?H:el('div', [[?H:el(span, Label, [<<"ah-meter__label">>], []) || not blank(Label)],
                          [?H:el(span, Value, [<<"ah-meter__value">>], []) || ShowValue]],
                  [<<"ah-meter__head">>], []) || ShowValue orelse not blank(Label)],
           ?H:el('div', ?H:el('div', [], [<<"ah-meter__fill">>],
                              [{data_state, State},
                               {style, iolist_to_binary([<<"width: ">>,
                                                         num(pct(clamp(Value, Min, Max), Min, Max)),
                                                         <<"%;">>])}]),
                 [<<"ah-meter__track">>],
                 [{role, <<"meter">>}, {aria_valuenow, Value}, {aria_valuemin, Min},
                  {aria_valuemax, Max}, {aria_label, not blank(Label) andalso Label}]),
           [?H:el('div', Helper, [<<"ah-meter__helper">>], []) || not blank(Helper)]],
          Cls, [[{data_size, R#ah_meter.size}], ?E:root_attrs(R, none)]).

%% sigil's reading of the <meter> thresholds, kept as is.
meter_state(V, Min, Max, Low, High, Opt) ->
    Lo = if Low =:= undefined -> Min; true -> Low end,
    Hi = if High =:= undefined -> Max; true -> High end,
    if Opt =:= undefined ->
           if V < Lo -> low; V > Hi -> high; true -> optimum end;
       Opt > Hi -> if V < Lo -> low; true -> optimum end;
       Opt < Lo -> if V > Hi -> low; true -> optimum end;
       true -> if V < Lo -> low; V > Hi -> high; true -> optimum end
    end.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => meter, category => data, root => <<"ah-meter">>,
      signature => <<"meter(Value, Css, Attrs)">>,
      groups => #{size => {[sm, md, lg], md}},
      classes => none_for([sm, md, lg]),
      options => [min, max, low, high, optimum, label, helper_text, show_value],
      doc => <<"Measurement in a range, coloured low / optimum / high.">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{min => <<"Lower bound (default 0).">>,
                       max => <<"Upper bound (default 100).">>,
                       low => <<"Below this the value is low.">>,
                       high => <<"Above this the value is high.">>,
                       optimum => <<"Best value: decides whether the low or the high zone is good.">>,
                       label => <<"Label above the track.">>,
                       helper_text => <<"Small text under the track.">>,
                       show_value => <<"Show the value above the track.">>},
      methods => []}.
