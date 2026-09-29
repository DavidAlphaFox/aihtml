%%%-------------------------------------------------------------------
%%% @doc A segmented date/time field, ported from sigil
%%% (form/datetime_input). See designs/04-components.md.
%%%
%%%   datetime_input(Value, Css, Attrs)    a segmented date/time field
%%%
%%% A value-bearing component: `Attrs' go to the root, which carries
%%% `data-ah-value' and fires `change'; `name' goes to a hidden input. The
%%% behaviour is `datetime_input' (assets/js/components/datetime_input.js),
%%% which draws the drop-down calendar from the shared template
%%% templates/datetime_input_calendar.mustache.
%%%
%%% datetime_input/3 builds an #ah_datetime_input{}
%%% (include/aihtml_datetime_input.hrl) and render/1 turns it into HTML,
%%% so pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_datetime_input).
-behaviour(aihtml_element).

-include("aihtml_datetime_input.hrl").

-export([datetime_input/3, render/1, fields/1, catalog/0]).

-export_type([element/0, value/0, label_key/0, labels/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(D, aihtml_lib_date).

%% Shared template (see aihtml_tpl): compiled to AH.tpl.* for the browser.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_datetime_input_calendar, "../templates/datetime_input_calendar.mustache"}).

%% The value of a datetime_input: an ISO date, date-time or time
%% (<<"2026-09-29">>, <<"2026-09-29T14:30">>, <<"14:30">>), a
%% calendar:date(), a calendar:datetime() or undefined.
-type value() :: binary() | string() | calendar:date() | calendar:datetime() | undefined.
-type label_key() :: months | weekdays | title | time | prev_month | next_month.
%% Texts of the drop-down calendar: `months' (12) and `weekdays' (7, from
%% Sunday) are lists, `title' is a display format (yyyy MMMM MM M).
-type labels() :: #{label_key() => unicode:chardata() | [unicode:chardata()]}.
-type element() :: #ah_datetime_input{}.

%% @doc A segmented date/time field (sigil's datetime_input): the text is
%% split into the parts of `format', edited one at a time with digits,
%% the arrow keys, PageUp/PageDown (±10), Home/End and Backspace. `Value'
%% is an ISO date, date-time or time, a `calendar:date()' or
%% `calendar:datetime()'; `data-ah-value' has the shape the format implies
%% (yyyy-MM-dd, yyyy-MM-ddTHH:mm[:ss] or HH:mm[:ss]).
%%
%% Css: `disabled', `readonly', `spinner' (up/down buttons), `no_calendar'
%% (no drop-down calendar), `show_time' (hour and minute fields in the
%% drop-down), `floating_label' (the placeholder is a label that floats
%% above the value), `no_rounded' (square corners).
%% Options (in Attrs): `placeholder', `format' (tokens yyyy yy MM M dd d
%% HH H hh h mm m ss s a; single letters show two digits too; default
%% "yyyy-MM-dd"), `min', `max', `first_day' (0 = Sunday .. 6), `labels'
%% (a map with `months', `weekdays' (7, from Sunday), `title' (a format),
%% `time', `prev_month', `next_month').
-spec datetime_input(value(), aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_datetime_input{}.
datetime_input(Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_datetime_input{value = Value}, Css, Attrs).

%% @doc The field names of #ah_datetime_input{}.
-spec fields(atom()) -> [atom()].
fields(ah_datetime_input) -> record_info(fields, ah_datetime_input).

-spec render(element()) -> aihtml_html:html().
render(#ah_datetime_input{value = Value0, format = Format0, name = Name,
                          disabled = Disabled, placeholder = Placeholder,
                          first_day = First} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),           % checks the flag fields first
    check_first_day(First),
    Format = text(Format0),
    Segs = segments(Format),
    [] =/= [T || {tok, T, _} <- Segs] orelse error({aihtml, {bad_datetime_format, Format0}}),
    Kind = kind(Segs),
    Labels = dti_labels(R#ah_datetime_input.labels),
    Value = dti_value(Value0),
    Iso = dti_iso(Value, Kind),
    Display = case Value of
                  undefined -> <<>>;
                  _ -> iolist_to_binary([seg_text(S, Value) || S <- Segs])
              end,
    Float = R#ah_datetime_input.floating_label,
    %% a time alone has no calendar
    Cal = not R#ah_datetime_input.no_calendar andalso Kind =/= {time, true}
        andalso Kind =/= {time, false},
    InputId = sub_id(Id, <<"input">>),
    Svg = fun(D) -> {safe, [<<"<svg width=\"9\" height=\"9\" viewBox=\"0 0 24 24\" fill=\"none\" "
                              "stroke=\"currentColor\" stroke-width=\"3\" stroke-linecap=\"round\" "
                              "stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"">>, D,
                            <<"\"/></svg>">>]}
          end,
    Input = ?H:void(input, [<<"ah-dti-input">>],
                    [{type, text}, {id, InputId}, {readonly, true},
                     {placeholder, case Float of true -> undefined; false -> Placeholder end},
                     {autocomplete, off}, {spellcheck, <<"false">>}, {value, Display},
                     {disabled, Disabled},
                     {aria_label, case Float of true -> undefined; false -> nonempty(Placeholder) end},
                     {aria_haspopup, Cal andalso dialog},
                     {aria_expanded, Cal andalso <<"false">>},
                     {aria_controls, Cal andalso sub_id(Id, <<"dropdown">>)},
                     {aria_description, <<"Arrow keys change the selected part">>}]),
    ?H:el('div',
          [?H:el('div',
                 [Input,
                  [?H:el('div', <<"📅"/utf8>>, [<<"ah-dti-cal-btn">>],
                         [{data_action, <<"toggle-dropdown">>}, {aria_hidden, <<"true">>}])
                   || Cal],
                  [?H:el('div',
                         [?H:el(button, Svg(<<"m6 15 6-6 6 6">>), [<<"ah-dti-spin ah-dti-spin-up">>],
                                [{type, button}, {tabindex, <<"-1">>}, {aria_label, <<"Increment">>}]),
                          ?H:el(button, Svg(<<"m6 9 6 6 6-6">>),
                                [<<"ah-dti-spin ah-dti-spin-down">>],
                                [{type, button}, {tabindex, <<"-1">>}, {aria_label, <<"Decrement">>}])],
                         [<<"ah-dti-spinner">>], [{aria_hidden, <<"true">>}])
                   || R#ah_datetime_input.spinner]],
                 [<<"ah-dti-row">>], []),
           ?H:el(span, [], [<<"ah-dti-live">>], [{aria_live, polite}, {aria_atomic, <<"true">>}]),
           [?H:el(label, Placeholder,
                  [<<"ah-dti-label">>, [<<"ah-dti-label-float">> || Value =/= undefined]],
                  [{for, InputId}])
            || Float],
           hidden(Name, Iso),
           [?H:el('div', [], [<<"ah-dti-dropdown">>],
                  [{id, sub_id(Id, <<"dropdown">>)}, {role, dialog},
                   {aria_label, <<"Choose date">>}, {hidden, true}])
            || Cal]],
          Classes,
          [[{id, Id}, {data_ah, <<"datetime_input">>}, {data_ah_value, Iso},
            {data_ah_format, Format},
            {data_ah_min, opt_iso(R#ah_datetime_input.min, Kind)},
            {data_ah_max, opt_iso(R#ah_datetime_input.max, Kind)},
            {data_ah_first_day, First},
            {data_ah_show_time, R#ah_datetime_input.show_time},
            {data_ah_labels, case R#ah_datetime_input.labels of
                                 M when map_size(M) =:= 0 -> undefined;
                                 _ -> iolist_to_binary(json:encode(Labels))
                             end},
            {aria_disabled, Disabled andalso <<"true">>}],
           ?E:root_attrs(R, change)]).

nonempty(undefined) -> undefined;
nonempty(T) -> case text(T) of <<>> -> undefined; B -> B end.

dti_labels(Custom) ->
    Defaults = #{months => [<<"January">>, <<"February">>, <<"March">>, <<"April">>, <<"May">>,
                            <<"June">>, <<"July">>, <<"August">>, <<"September">>,
                            <<"October">>, <<"November">>, <<"December">>],
                 weekdays => [<<"Su">>, <<"Mo">>, <<"Tu">>, <<"We">>, <<"Th">>, <<"Fr">>,
                              <<"Sa">>],
                 title => <<"MMMM yyyy">>, time => <<"Time">>,
                 prev_month => <<"Previous month">>, next_month => <<"Next month">>},
    aihtml_lib_calendar:labels(Custom, Defaults, bad_datetime_label,
                               [{months, 12}, {weekdays, 7}]).

%% The parts of a format: {tok, Type, Pattern} or {lit, Text}. Single
%% letter tokens are two digits wide, like their doubled forms, so every
%% part keeps its place in the text.
segments(F) -> segments(F, []).

segments(<<>>, Acc) -> lists:reverse(Acc);
segments(B, Acc) ->
    Toks = [{<<"yyyy">>, year}, {<<"yy">>, year2}, {<<"MM">>, month}, {<<"M">>, month},
            {<<"dd">>, day}, {<<"d">>, day}, {<<"HH">>, hour}, {<<"H">>, hour},
            {<<"hh">>, hour12}, {<<"h">>, hour12}, {<<"mm">>, minute}, {<<"m">>, minute},
            {<<"ss">>, second}, {<<"s">>, second}, {<<"aa">>, ampm}, {<<"a">>, ampm}],
    case [{P, T} || {P, T} <- Toks, binary:longest_common_prefix([P, B]) =:= byte_size(P)] of
        [{P, T} | _] ->
            segments(binary:part(B, byte_size(P), byte_size(B) - byte_size(P)),
                     [{tok, T, P} | Acc]);
        [] ->
            <<C/utf8, Rest/binary>> = B,
            case Acc of
                [{lit, L} | Acc1] -> segments(Rest, [{lit, <<L/binary, C/utf8>>} | Acc1]);
                _ -> segments(Rest, [{lit, <<C/utf8>>} | Acc])
            end
    end.

kind(Segs) ->
    Types = [T || {tok, T, _} <- Segs],
    Date = lists:any(fun(T) -> lists:member(T, [year, year2, month, day]) end, Types),
    Time = lists:any(fun(T) -> lists:member(T, [hour, hour12, minute, second, ampm]) end, Types),
    Sec = lists:member(second, Types),
    case {Date, Time} of
        {true, false} -> date;
        {false, true} -> {time, Sec};
        _ -> {datetime, Sec}
    end.

seg_text({lit, L}, _) -> L;
seg_text({tok, T, _}, {{Y, Mo, D}, {H, Mi, S}}) ->
    case T of
        year -> ?D:pad4(Y);
        year2 -> ?D:pad(Y rem 100);
        month -> ?D:pad(Mo);
        day -> ?D:pad(D);
        hour -> ?D:pad(H);
        hour12 -> ?D:pad(h12(H));
        minute -> ?D:pad(Mi);
        second -> ?D:pad(S);
        ampm when H < 12 -> <<"AM">>;
        ampm -> <<"PM">>
    end.

%% A value as {{Y, M, D}, {H, Mi, S}}; a time alone takes today's date.
dti_value(undefined) -> undefined;
dti_value(<<>>) -> undefined;
dti_value({{_, _, _} = D, {H, Mi, S}} = V) when is_integer(H), is_integer(Mi), is_integer(S) ->
    _ = ?D:days(D),
    (H >= 0 andalso H < 24 andalso Mi >= 0 andalso Mi < 60 andalso S >= 0 andalso S < 60)
        orelse error({aihtml, {bad_date, V}}),
    V;
dti_value({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    _ = ?D:days(Date),
    {Date, {0, 0, 0}};
dti_value(L) when is_list(L) -> dti_value(unicode:characters_to_binary(L));
dti_value(<<Date:10/binary>>) -> dti_value(calendar:gregorian_days_to_date(?D:days(Date)));
dti_value(<<Date:10/binary, Sep, Time/binary>> = B) when Sep =:= $T; Sep =:= $\s ->
    {{H, Mi, S}, _} = hms(Time, B),
    dti_value({calendar:gregorian_days_to_date(?D:days(Date)), {H, Mi, S}});
dti_value(<<_:2/binary, ":", _/binary>> = B) ->
    {T, _} = hms(B, B),
    dti_value({date(), T});
dti_value(Other) -> error({aihtml, {bad_date, Other}}).

hms(<<H:2/binary, ":", Mi:2/binary, Rest/binary>>, B) ->
    S = case Rest of
            <<":", S0:2/binary, _/binary>> -> S0;
            _ -> <<"00">>
        end,
    try {{binary_to_integer(H), binary_to_integer(Mi), binary_to_integer(S)}, ok}
    catch _:_ -> error({aihtml, {bad_date, B}})
    end;
hms(_, B) -> error({aihtml, {bad_date, B}}).

dti_iso(undefined, _) -> <<>>;
dti_iso({{Y, M, D}, _}, date) ->
    <<(?D:pad4(Y))/binary, "-", (?D:pad(M))/binary, "-", (?D:pad(D))/binary>>;
dti_iso({_, {H, Mi, S}}, {time, Sec}) ->
    <<(?D:pad(H))/binary, ":", (?D:pad(Mi))/binary, (secs(S, Sec))/binary>>;
dti_iso({Date, _} = V, {datetime, Sec}) ->
    <<(dti_iso({Date, {0, 0, 0}}, date))/binary, "T", (dti_iso(V, {time, Sec}))/binary>>.

secs(S, true) -> <<":", (?D:pad(S))/binary>>;
secs(_, false) -> <<>>.

opt_iso(undefined, _) -> undefined;
opt_iso(V, Kind) -> dti_iso(dti_value(V), Kind).

check_first_day(F) ->
    (is_integer(F) andalso F >= 0 andalso F =< 6) orelse error({aihtml, {bad_first_day, F}}).

h12(0) -> 12;
h12(H) when H > 12 -> H - 12;
h12(H) -> H.

%% A root without an id gets one: the parts refer to each other by id.
%% Returns the id and the record holding it, for root_attrs/2.
ensure_id(R) ->
    Id = case R#ah_datetime_input.id of
             undefined -> <<"ah-cal", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, R#ah_datetime_input{id = Id}}.

sub_id(Id, Part) -> <<Id/binary, "-", Part/binary>>.

hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => datetime_input, category => form,
       signature => <<"datetime_input(Value, Css, Attrs)">>,
       root => <<"ah-dti-group">>,
       flags => [disabled, readonly, spinner, no_calendar, show_time, floating_label,
                 no_rounded],
       classes => #{disabled => [<<"ah-dti-disabled">>],
                    readonly => [<<"ah-dti-readonly">>],
                    spinner => [], no_calendar => [], show_time => [],
                    floating_label => [],
                    no_rounded => [<<"ah-dti-no-rounded">>]},
       options => [placeholder, format, min, max, first_day, labels],
       behavior => <<"datetime_input">>,
       events => [<<"change">>, <<"input">>, <<"ah:open">>, <<"ah:close">>],
       doc => <<"A segmented date/time field edited part by part with digits and arrow "
                "keys, with a drop-down calendar; value in data-ah-value as ISO.">>,
       option_docs =>
           #{disabled => <<"Not editable, not focusable.">>,
             readonly => <<"Shows the value; keys and the calendar do nothing.">>,
             spinner => <<"Up/down buttons that step the selected part.">>,
             no_calendar => <<"No calendar button and drop-down.">>,
             show_time => <<"Hour and minute fields under the drop-down calendar.">>,
             floating_label => <<"The placeholder becomes a label floating above the value.">>,
             no_rounded => <<"Square corners.">>,
             placeholder => <<"Text of the empty field.">>,
             format => <<"Parts: yyyy yy MM M dd d HH H hh h mm m ss s a (default "
                         "yyyy-MM-dd); the value is yyyy-MM-dd, yyyy-MM-ddTHH:mm[:ss] or "
                         "HH:mm[:ss] accordingly.">>,
             min => <<"Earliest value; later edits are clamped on blur.">>,
             max => <<"Latest value.">>,
             first_day => <<"First day of the calendar week, 0 = Sunday (default) .. 6.">>,
             labels => <<"Map of months, weekdays (from Sunday), title (a format), time, "
                         "prev_month, next_month.">>},
       methods =>
           [#{name => setValue, args => <<"(Iso | null)">>,
              doc => <<"Set the value without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
            #{name => clear, args => <<"()">>, doc => <<"Empty the value and fire change.">>},
            #{name => open, args => <<"()">>, doc => <<"Open the calendar.">>},
            #{name => close, args => <<"()">>, doc => <<"Close the calendar.">>}]}].
