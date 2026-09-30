%%%-------------------------------------------------------------------
%%% @doc The progress_circle component (designs/04-components.md): `ah_progress_circle/3'
%%% builds an #ah_progress_circle{} element record (include/aihtml_progress_circle.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_progress_circle).
-behaviour(aihtml_element).

-include("aihtml_progress_circle.hrl").

-export([ah_progress_circle/3, render/1, fields/1, catalog/0]).

-import(aihtml_lib_display, [blank/1, num/1, method/3]).
-import(aihtml_lib_progress, [clamp/3]).

-define(H, aihtml_html).
-define(E, aihtml_element).
%% 2 * pi * 45, the progress circle's circumference (r = 45 in a 100 box)
-define(CIRC, 282.74333882308139).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Circular progress (SVG), `Value' 0..100. Options: `label' (text
%% under the ring, also the aria-label), `show_value' (true).
-spec ah_progress_circle(number() | undefined, css(), attrs()) -> #ah_progress_circle{}.
ah_progress_circle(Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_progress_circle{value = Value}, Css, Attrs).

%% @doc The field names of #ah_progress_circle{}.
-spec fields(atom()) -> [atom()].
fields(ah_progress_circle) -> record_info(fields, ah_progress_circle).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_progress_circle{}) -> html().
render(#ah_progress_circle{value = Value, indeterminate = Indet, label = Label} = R) ->
    Cls = ?E:classes(?MODULE, R),
    V = trunc(clamp(if is_number(Value) -> Value; true -> 0 end, 0, 100)),
    Circle = fun(C, Extra) ->
                     ?H:el(circle, [], [C], [{cx, 50}, {cy, 50}, {r, 45} | Extra])
             end,
    Svg = ?H:el(svg,
                [Circle(<<"ah-progress-circle-track">>, []),
                 Circle(<<"ah-progress-circle-fill">>,
                        [{transform, <<"rotate(-90 50 50)">>},
                         {stroke_dasharray, num(?CIRC)},
                         {stroke_dashoffset, num(offset(V))}])],
                [], [{<<"viewBox">>, <<"0 0 100 100">>},
                     {xmlns, <<"http://www.w3.org/2000/svg">>},
                     {aria_hidden, <<"true">>}]),
    ShowValue = R#ah_progress_circle.show_value =/= false andalso not Indet,
    ?H:el('div',
          [?H:el('div', [Svg, [?H:el(span, [integer_to_binary(V), <<"%">>],
                                     [<<"ah-progress-circle-value">>], []) || ShowValue]],
                 [<<"ah-progress-circle-ring">>], []),
           [?H:el(span, Label, [<<"ah-progress-circle-label">>], []) || not blank(Label)]],
          Cls,
          [[{role, <<"progressbar">>}, {aria_valuemin, 0}, {aria_valuemax, 100},
            {aria_valuenow, not Indet andalso V},
            {aria_valuetext, not Indet andalso <<(integer_to_binary(V))/binary, "%">>},
            {aria_busy, Indet andalso <<"true">>},
            {aria_label, not blank(Label) andalso Label},
            {aria_disabled, R#ah_progress_circle.disabled andalso <<"true">>},
            {data_ah, <<"progress-circle">>}, {data_ah_value, not Indet andalso V}],
           ?E:root_attrs(R, change)]).

offset(V) -> ?CIRC * (1 - V / 100).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => progress_circle, category => data, root => <<"ah-progress-circle">>,
      signature => <<"ah_progress_circle(Value, Css, Attrs)">>,
      groups => #{size => {[sm, md, lg], md},
                  color => {[primary, success, warning, info, error], primary}},
      flags => [disabled, indeterminate],
      classes => maps:merge(
                   maps:from_list([{M, [<<"ah-progress-circle--", (atom_to_binary(M))/binary>>]}
                                   || M <- [sm, md, lg, primary, success, warning, info,
                                            error, indeterminate]]),
                   #{disabled => [<<"ah-progress-circle-disabled">>]}),
      options => [label, show_value], behavior => <<"progress-circle">>,
      events => [<<"change">>, <<"ah:complete">>],
      doc => <<"SVG progress ring, 0-100. Method setValue(v).">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{label => <<"Text under the ring, also the aria-label.">>,
                       show_value => <<"Show the percentage in the ring (default true).">>,
                       disabled => <<"Dimmed.">>,
                       indeterminate => <<"Spinning arc, no value.">>},
      methods => [method(setValue, <<"(Value)">>, <<"Set 0..100; fires change, and ah:complete at 100.">>),
                  method(getValue, <<"()">>, <<"Return the current value.">>)]}.
