%%%-------------------------------------------------------------------
%%% @doc Star rating ported from sigil, a value-bearing custom control
%%% (designs/04-components.md): the value is in `data-ah-value', a hidden
%%% input carries it when Attrs has a `name', and `change' fires on the
%%% root.
%%%
%%% rating_group/4 builds an #ah_rating_group{}
%%% (include/aihtml_rating_group.hrl) and render/1 turns it into HTML, so
%%% pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_rating_group).
-behaviour(aihtml_element).

-include("aihtml_rating_group.hrl").

-export([rating_group/4, render/1, fields/1, catalog/0]).

-export_type([color/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(L, aihtml_lib_choice).

-type color() :: warning | primary | success | error.

-define(STAR_SVG,
        {safe, <<"<svg viewBox=\"0 0 24 24\" fill=\"currentColor\" aria-hidden=\"true\">"
                 "<path d=\"M12 2l3.09 6.26L22 9.27l-5 4.87 1.18 6.88L12 17.77l-6.18 "
                 "3.25L7 14.14 2 9.27l6.91-1.01L12 2z\"/></svg>">>}).

%% @doc Star rating from 0 to `Max'. Options in Attrs: `name' (hidden
%% input), `precision' (1 | 0.5), `allow_clear' (clicking the current
%% value clears it, default true), `readonly', `disabled'.
-spec rating_group(pos_integer(), number() | undefined, aihtml_html:css(),
                   aihtml_html:attrs()) -> #ah_rating_group{}.
rating_group(Max, Value, Css, Attrs) when is_integer(Max), Max >= 1 ->
    ?E:build(?MODULE, #ah_rating_group{max = Max, value = Value}, Css, Attrs);
rating_group(Max, _Value, _Css, _Attrs) ->
    error({aihtml, {bad_max, rating_group, Max}}).

%% @doc The field names of #ah_rating_group{}.
-spec fields(atom()) -> [atom()].
fields(ah_rating_group) -> record_info(fields, ah_rating_group).

-spec render(#ah_rating_group{}) -> aihtml_html:html().
render(#ah_rating_group{max = Max, value = Value, size = Size, color = Color} = R) ->
    is_integer(Max) andalso Max >= 1
        orelse error({aihtml, {bad_max, rating_group, Max}}),
    V = case Value of
            undefined -> 0;
            _ when is_number(Value) -> Value;
            _ -> error({aihtml, {bad_value, rating_group, Value}})
        end,
    Precision = case R#ah_rating_group.precision of
                    P when P == 1 -> <<"1">>;
                    P when P == 0.5 -> <<"0.5">>;
                    P -> error({aihtml, {bad_option, rating_group, precision, P}})
                end,
    Readonly = ?L:truthy(R#ah_rating_group.readonly),
    Disabled = ?L:truthy(R#ah_rating_group.disabled),
    Static = Readonly orelse Disabled,
    Stars = [?H:el(button,
                 [?H:el(span, ?STAR_SVG, [<<"ah-rating__empty">>], []),
                  ?H:el(span, ?STAR_SVG, [<<"ah-rating__filled">>],
                        [{style, [<<"width:">>, pct(fill_ratio(I, V)), <<"%;">>]}])],
                 [<<"ah-rating__star">>],
                 [{type, button}, {data_index, I}, {role, radio},
                  {aria_checked, ?L:bool(V >= I + 1)},
                  {aria_label, [integer_to_binary(I + 1), <<" / ">>, integer_to_binary(Max)]},
                  {tabindex, case Static of true -> -1; false -> 0 end},
                  {disabled, Disabled}])
             || I <- lists:seq(0, Max - 1)],
    Hidden = case R#ah_rating_group.name of
                 undefined -> [];
                 Name -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, ?L:num(V)}])
             end,
    ?H:el('div', [Stars, Hidden], ?E:classes(?MODULE, R),
        [{data_ah, <<"rating">>}, {role, radiogroup},
         {data_ah_value, ?L:num(V)}, {data_ah_max, Max},
         {data_size, Size}, {data_color, Color},
         {data_precision, Precision},
         {data_readonly, ?L:bool(Readonly)}, {data_disabled, ?L:bool(Disabled)},
         {data_allow_clear, ?L:bool(?L:truthy(R#ah_rating_group.allow_clear))},
         {aria_readonly, Readonly andalso <<"true">>},
         {aria_disabled, Disabled andalso <<"true">>},
         ?E:root_attrs(R, change)]).

fill_ratio(I, V) ->
    R = V - I,
    if R >= 1 -> 1; R =< 0 -> 0; true -> R end.

pct(R) -> ?L:num(R * 100).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => rating_group, category => form,
       signature => <<"rating_group(Max, Value, Css, Attrs)">>,
       root => <<"ah-rating">>,
       groups => #{size => {[sm, md, lg], md},
                   color => {[warning, primary, success, error], warning}},
       classes => #{sm => [], md => [], lg => [], warning => [], primary => [],
                    success => [], error => []},
       options => [name, precision, allow_clear, readonly, disabled],
       behavior => <<"rating">>, events => [<<"change">>, <<"ah:hover">>],
       doc => <<"Star rating with hover preview, half stars (precision 0.5) and "
                "arrow keys. Value in data-ah-value, hidden input when name is given, "
                "change on the root.">>,
       option_docs =>
           #{sm => <<"16px stars.">>, md => <<"22px stars (default).">>, lg => <<"30px stars.">>,
             warning => <<"Gold stars (default).">>, primary => <<"Stars in the primary colour.">>,
             success => <<"Green stars.">>, error => <<"Red stars.">>,
             name => <<"Name of the hidden input that submits the value.">>,
             precision => <<"1 (default) or 0.5 for half stars.">>,
             allow_clear => <<"Clicking the current value resets to 0 (default true).">>,
             readonly => <<"true: shows the value, no interaction.">>,
             disabled => <<"true: dimmed, no interaction.">>},
       methods => [#{name => setValue, args => <<"(value)">>,
                     doc => <<"Set the rating (snapped to the precision); no change event.">>},
                   #{name => getValue, args => <<"()">>, doc => <<"The current rating.">>}]}].
