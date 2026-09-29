%%%-------------------------------------------------------------------
%%% @doc Picker components, ported from sigil (form/datepicker and
%%% form/combobox). See designs/04-components.md.
%%%
%%%   datepicker(Value, Css, Attrs)        a text field with a month grid popup
%%%   combobox(Items, Value, Css, Attrs)   an editable field with a filtered list
%%%   set_items(Ctx, Target, Items[, Opts]) (in an action) replace a combobox's list
%%%
%%% Both are value-bearing components: `Attrs' go to the root, which
%%% carries `data-ah-value' and fires `change'; `name' goes to a hidden
%%% input. The popups are rendered inside the root and driven by the
%%% `datepicker' and `combobox' behaviours (assets/js/components/form_pickers.js).
%%%
%%% == Server-side search ==
%%%
%%% `combobox(Items, Value, Css, [{search, {Mod, Action, Args}}])' binds
%%% `aihtml:on(input, Ref, #{debounce => 250})' to the text field. Each
%%% pause in typing POSTs the action with
%%%
%%%   Event.value                  the text typed so far (the query)
%%%   Event.data                   #{<<"combobox">> => <root id>}
%%%
%%% and the action answers with `set_items(Ctx, Event, Items)'. The items
%%% are rendered here, by the same code as the first render, and morphed
%%% into the list (`aihtml_action:html(Ctx, {id, <root id>-list}, Items,
%%% morph_inner)'), so the text field keeps its focus and caret; then the
%%% behaviour method `itemsLoaded' re-reads the list, highlights the query
%%% and opens the popup. While a query is pending the popup shows
%%% "Loading...". The client does not filter search results again.
%%%
%%% The browser only builds HTML from shared templates
%%% (templates/datepicker_month.mustache, templates/combobox_tag.mustache).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_form_pickers).

-export([datepicker/3, combobox/4, set_items/3, set_items/4,
         catalog/0, facade_extras/0]).

-export_type([date_value/0, item/0]).

-define(H, aihtml_html).

%% Shared templates (see aihtml_tpl): also compiled to AH.tpl.* for the browser.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_datepicker_month, "../templates/datepicker_month.mustache"}).
-mustache_template({tpl_combobox_tag, "../templates/combobox_tag.mustache"}).

-type date_value() :: binary() | calendar:date() | undefined
                    | {binary() | calendar:date() | undefined,
                       binary() | calendar:date() | undefined}.
%% A combobox item: a text that is both value and label, `{Value, Label}',
%% or a map with `value' and optionally `label', `description', `group'
%% and `disabled'.
-type item() :: binary() | atom() | integer() | {term(), term()}
              | #{value := term(), label => term(), description => term(),
                  group => term(), disabled => boolean()}.

-define(MONTHS, [<<"January">>, <<"February">>, <<"March">>, <<"April">>, <<"May">>,
                 <<"June">>, <<"July">>, <<"August">>, <<"September">>, <<"October">>,
                 <<"November">>, <<"December">>]).
-define(MONTHS_SHORT, [<<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>, <<"May">>, <<"Jun">>,
                       <<"Jul">>, <<"Aug">>, <<"Sep">>, <<"Oct">>, <<"Nov">>, <<"Dec">>]).
-define(WEEKDAYS, [<<"Su">>, <<"Mo">>, <<"Tu">>, <<"We">>, <<"Th">>, <<"Fr">>, <<"Sa">>]).

%%%===================================================================
%%% datepicker
%%%===================================================================

%% @doc A read-only text field with sigil's calendar popup. `Value' is an
%% ISO date (`<<"2026-09-29">>'), a `calendar:date()' or `undefined'; a
%% pair `{From, To}' (or the `range' modifier) selects a range, whose
%% `data-ah-value' is `"from,to"'.
%%
%% Css: `disabled', `readonly', `range', `clearable', `inline' (the month
%% grid is always shown under the field, rendered on the server with the
%% template the browser uses).
%% Options (in Attrs): `placeholder' (default "Select date..."), `format'
%% (display format: yyyy yy MMMM MMM MM M dd d, default "yyyy-MM-dd"),
%% `min', `max' (dates), `disabled_dates' (a list of dates),
%% `first_day' (0 = Sunday .. 6, default 0), `week_numbers' (default
%% false), `other_month_days' (default true), `weekends' (weekend days
%% in the error colour, sigil's enable-weekend; default false), `labels' (a map with
%% `months', `months_short', `weekdays' (7, from Sunday), `title' (a
%% format, default "MMMM yyyy"), `today', `clear', `prev_month',
%% `next_month', `prev_year', `next_year').
-spec datepicker(date_value(), aihtml_html:css(), aihtml_html:attrs()) ->
          aihtml_html:element().
datepicker(Value0, Css, Attrs0) ->
    Entry = aihtml_catalog:entry(?MODULE, datepicker),
    {Opts, Attrs1} = aihtml_catalog:split_options(Entry, Attrs0),
    {Name, Id, Attrs} = take_name_id(Attrs1),
    Flags = aihtml_catalog:flags(Entry, Css),
    Range = lists:member(range, Flags) orelse is_range(Value0),
    Disabled = lists:member(disabled, Flags),
    Readonly = lists:member(readonly, Flags),
    Inline = lists:member(inline, Flags),
    Value = case Range of
                true -> range_value(Value0);
                false -> iso(Value0)
            end,
    Labels = labels(maps:get(labels, Opts, #{})),
    Format = text(maps:get(format, Opts, <<"yyyy-MM-dd">>)),
    FirstDay = maps:get(first_day, Opts, 0),
    (is_integer(FirstDay) andalso FirstDay >= 0 andalso FirstDay =< 6)
        orelse error({aihtml, {bad_first_day, FirstDay}}),
    {Iso, Display} = case Value of
                         undefined -> {<<>>, <<>>};
                         {F, T} -> {<<(nz(F))/binary, ",", (nz(T))/binary>>,
                                    range_display(F, T, Format, Labels)};
                         D -> {D, format(D, Format, Labels)}
                     end,
    Clear = [?H:el(button, {safe, <<"&times;">>}, [<<"ah-datepicker-clear">>],
                   [{type, button}, {tabindex, <<"-1">>},
                    {aria_label, maps:get(<<"clear">>, Labels)}])
             || lists:member(clearable, Flags), not Disabled, not Readonly],
    Input = ?H:void(input, [<<"ah-datepicker-input">>],
                    [{type, text}, {id, sub_id(Id, <<"input">>)},
                     {autocomplete, off}, {spellcheck, <<"false">>}, {readonly, true},
                     {placeholder, maps:get(placeholder, Opts, <<"Select date...">>)},
                     {value, Display}, {disabled, Disabled},
                     {role, combobox}, {aria_haspopup, dialog},
                     {aria_expanded, <<"false">>}]),
    ?H:el('div',
          [?H:el('div',
                 [Input, Clear,
                  ?H:el(span, ?H:el(span, <<"📅"/utf8>>, [<<"ah-datepicker-icon">>], []),
                        [<<"ah-datepicker-trigger">>], [{aria_hidden, <<"true">>}])],
                 [<<"ah-datepicker-input-area">>], []),
           hidden(Name, Iso),
           ?H:el('div',
                 case Inline of
                     false -> [];
                     true ->
                         Off = [iso(D) || D <- maps:get(disabled_dates, Opts, [])],
                         aihtml_tpl:safe(tpl_datepicker_month(
                           month_view(#{id => Id, value => Value, first_day => FirstDay,
                                        week_numbers => maps:get(week_numbers, Opts, false),
                                        weekends => maps:get(weekends, Opts, false),
                                        other_month => maps:get(other_month_days, Opts, true),
                                        min => iso_opt(maps:get(min, Opts, undefined)),
                                        max => iso_opt(maps:get(max, Opts, undefined)),
                                        off => Off, labels => Labels})))
                 end,
                 [<<"ah-datepicker-popup">>],
                 [{role, case Inline of true -> group; false -> dialog end},
                  {aria_label, <<"Choose date">>}])],
          [aihtml_catalog:classes(Entry, Css),
           [<<"ah-datepicker-range">> || Range, not lists:member(range, Flags)]],
          [[{id, Id}, {data_ah, <<"datepicker">>}, {data_ah_value, Iso},
            {data_ah_range, Range},
            {data_ah_format, Format},
            {data_ah_min, iso_opt(maps:get(min, Opts, undefined))},
            {data_ah_max, iso_opt(maps:get(max, Opts, undefined))},
            {data_ah_disabled_dates,
             case [iso(D) || D <- maps:get(disabled_dates, Opts, [])] of
                 [] -> undefined;
                 Ds -> iolist_to_binary(lists:join(<<",">>, Ds))
             end},
            {data_ah_first_day, FirstDay},
            {data_ah_week_numbers, maps:get(week_numbers, Opts, false)},
            {data_ah_weekends, maps:get(weekends, Opts, false)},
            {data_ah_other_month_days,
             case maps:get(other_month_days, Opts, true) of
                 true -> undefined;
                 false -> <<"false">>
             end},
            {data_ah_labels, iolist_to_binary(json:encode(Labels))},
            {aria_disabled, Disabled andalso <<"true">>}],
           Attrs]).

is_range({A, B}) when not is_integer(A); not is_integer(B) -> true;
is_range({undefined, undefined}) -> true;
is_range(_) -> false.

range_value(undefined) -> undefined;
range_value({undefined, undefined}) -> undefined;
range_value({F, T}) ->
    case {iso(F), iso(T)} of
        {A, B} when A =/= undefined, B =/= undefined, A > B -> {B, A};
        Pair -> Pair
    end;
range_value(Single) -> {iso(Single), undefined}.

range_display(F, T, Format, Labels) ->
    iolist_to_binary([fmt_opt(F, Format, Labels),
                      [[<<" - ">>, format(T, Format, Labels)] || T =/= undefined]]).

fmt_opt(undefined, _, _) -> <<>>;
fmt_opt(D, Format, Labels) -> format(D, Format, Labels).

nz(undefined) -> <<>>;
nz(B) -> B.

iso_opt(undefined) -> undefined;
iso_opt(D) -> iso(D).

%% A date as <<"yyyy-mm-dd">>, checked.
iso(undefined) -> undefined;
iso(<<>>) -> undefined;
iso({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    calendar:valid_date(Date) orelse error({aihtml, {bad_date, Date}}),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D]));
iso(B) when is_binary(B) ->
    try
        <<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> = B,
        iso({binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)})
    catch
        _:_ -> error({aihtml, {bad_date, B}})
    end;
iso(L) when is_list(L) -> iso(unicode:characters_to_binary(L));
iso(Other) -> error({aihtml, {bad_date, Other}}).

labels(Custom) when is_map(Custom) ->
    Defaults = #{months => ?MONTHS, months_short => ?MONTHS_SHORT, weekdays => ?WEEKDAYS,
                 title => <<"MMMM yyyy">>, today => <<"Today">>, clear => <<"Clear">>,
                 prev_month => <<"Previous month">>, next_month => <<"Next month">>,
                 prev_year => <<"Previous year">>, next_year => <<"Next year">>},
    maps:foreach(fun(K, _) -> maps:is_key(K, Defaults)
                                  orelse error({aihtml, {bad_datepicker_label, K}})
                 end, Custom),
    M = maps:merge(Defaults, Custom),
    check_len(months, M, 12), check_len(months_short, M, 12), check_len(weekdays, M, 7),
    maps:fold(fun(K, V, Acc) when is_list(V), V =/= [], not is_integer(hd(V)) ->
                      Acc#{atom_to_binary(K) => [text(X) || X <- V]};
                 (K, V, Acc) -> Acc#{atom_to_binary(K) => text(V)}
              end, #{}, M).

check_len(K, M, N) ->
    length(maps:get(K, M)) =:= N orelse error({aihtml, {bad_datepicker_label, K}}).

%% date-fns style display format, the tokens the JS formatter knows too.
format(Iso, Format, Labels) ->
    <<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> = Iso,
    Mi = binary_to_integer(M),
    Di = binary_to_integer(D),
    iolist_to_binary(fmt(Format, #{y => Y, m => Mi, d => Di}, Labels)).

fmt(<<"yyyy", R/binary>>, V, L) -> [maps:get(y, V) | fmt(R, V, L)];
fmt(<<"yy", R/binary>>, V, L) -> [binary:part(maps:get(y, V), 2, 2) | fmt(R, V, L)];
fmt(<<"MMMM", R/binary>>, V, L) ->
    [lists:nth(maps:get(m, V), maps:get(<<"months">>, L)) | fmt(R, V, L)];
fmt(<<"MMM", R/binary>>, V, L) ->
    [lists:nth(maps:get(m, V), maps:get(<<"months_short">>, L)) | fmt(R, V, L)];
fmt(<<"MM", R/binary>>, V, L) -> [pad(maps:get(m, V)) | fmt(R, V, L)];
fmt(<<"M", R/binary>>, V, L) -> [integer_to_binary(maps:get(m, V)) | fmt(R, V, L)];
fmt(<<"dd", R/binary>>, V, L) -> [pad(maps:get(d, V)) | fmt(R, V, L)];
fmt(<<"d", R/binary>>, V, L) -> [integer_to_binary(maps:get(d, V)) | fmt(R, V, L)];
fmt(<<C/utf8, R/binary>>, V, L) -> [<<C/utf8>> | fmt(R, V, L)];
fmt(<<>>, _, _) -> [].

%% The view of templates/datepicker_month.mustache for the month holding
%% the focused day (the value, or today), as the browser's dpView builds
%% it; used for the `inline' first render.
month_view(#{id := Id, value := Value, first_day := First, labels := L} = O) ->
    {Sel, From, To} = case Value of
                          {F, T} -> {undefined, days(F), days(T)};
                          V -> {days(V), undefined, undefined}
                      end,
    Today = calendar:date_to_gregorian_days(date()),
    Focus = case {Sel, From} of
                {undefined, undefined} -> Today;
                {undefined, _} -> From;
                _ -> Sel
            end,
    {Y, M, _} = calendar:gregorian_days_to_date(Focus),
    MonthStart = calendar:date_to_gregorian_days(Y, M, 1),
    MonthEnd = calendar:date_to_gregorian_days(Y, M, calendar:last_day_of_the_month(Y, M)),
    GridStart = start_of_week(MonthStart, First),
    GridEnd = start_of_week(MonthEnd, First) + 7,
    Off = [days(D) || D <- maps:get(off, O)],
    Min = days(maps:get(min, O)),
    Max = days(maps:get(max, O)),
    Day = fun(D) ->
              {Dy, Dm, Dd} = calendar:gregorian_days_to_date(D),
              Other = Dm =/= M,
              case Other andalso not maps:get(other_month, O) of
                  true -> #{empty => true};
                  false ->
                      Dis = (Min =/= undefined andalso D < Min)
                          orelse (Max =/= undefined andalso D > Max)
                          orelse lists:member(D, Off),
                      IsStart = D =:= From, IsEnd = D =:= To,
                      Selected = case Value of {_, _} -> IsStart orelse IsEnd; _ -> D =:= Sel end,
                      InRange = From =/= undefined andalso To =/= undefined
                          andalso D >= From andalso D =< To,
                      Dow = js_dow(D),
                      Iso = iso({Dy, Dm, Dd}),
                      Cls = [<<"ah-datepicker-day">>,
                             [<<" ah-datepicker-day-other-month">> || Other],
                             [<<" ah-datepicker-day-today">> || D =:= Today],
                             [<<" ah-datepicker-day-weekend">>
                              || maps:get(weekends, O), Dow =:= 0 orelse Dow =:= 6],
                             [<<" ah-datepicker-day-disabled">> || Dis],
                             [<<" ah-datepicker-day-selected">> || Selected],
                             [<<" ah-datepicker-day-in-range">> || InRange],
                             [<<" ah-datepicker-day-range-start">> || IsStart],
                             [<<" ah-datepicker-day-range-end">> || IsEnd],
                             [<<" ah-datepicker-day-focused">> || D =:= Focus]],
                      #{empty => false, cls => iolist_to_binary(Cls),
                        id => <<Id/binary, "-d", Iso/binary>>, date => Iso,
                        selected => atom_to_binary(Selected), disabled => atom_to_binary(Dis),
                        label => format(Iso, <<"d MMMM yyyy">>, L),
                        day => integer_to_binary(Dd)}
              end
          end,
    #{title_id => <<Id/binary, "-title">>,
      title => format(iso({Y, M, 1}), maps:get(<<"title">>, L), L),
      prev_year => maps:get(<<"prev_year">>, L), prev_month => maps:get(<<"prev_month">>, L),
      next_month => maps:get(<<"next_month">>, L), next_year => maps:get(<<"next_year">>, L),
      today => maps:get(<<"today">>, L),
      week_numbers => maps:get(week_numbers, O),
      weekdays => [#{label => lists:nth((First + I) rem 7 + 1, maps:get(<<"weekdays">>, L))}
                   || I <- lists:seq(0, 6)],
      weeks => [#{num => integer_to_binary(week_number(W, First)),
                  days => [Day(W + K) || K <- lists:seq(0, 6)]}
                || W <- lists:seq(GridStart, GridEnd - 1, 7)]}.

days(undefined) -> undefined;
days(Iso) ->
    <<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> = Iso,
    calendar:date_to_gregorian_days(binary_to_integer(Y), binary_to_integer(M),
                                    binary_to_integer(D)).

%% 0 = Sunday .. 6 = Saturday, as JS getDay
js_dow(D) -> calendar:day_of_the_week(calendar:gregorian_days_to_date(D)) rem 7.

start_of_week(D, First) -> D - (js_dow(D) - First + 7) rem 7.

%% date-fns getWeek (weekStartsOn = First, firstWeekContainsDate = 1),
%% the same algorithm as weekNumber in form_pickers.js.
week_number(D, First) ->
    {Y, _, _} = calendar:gregorian_days_to_date(D),
    Jan1 = fun(Yr) -> start_of_week(calendar:date_to_gregorian_days(Yr, 1, 1), First) end,
    Next = Jan1(Y + 1),
    This = Jan1(Y),
    WY = if D >= Next -> Y + 1;
            D >= This -> Y;
            true -> Y - 1
         end,
    (start_of_week(D, First) - Jan1(WY)) div 7 + 1.

pad(N) when N < 10 -> <<"0", (integer_to_binary(N))/binary>>;
pad(N) -> integer_to_binary(N).

%%%===================================================================
%%% combobox
%%%===================================================================

%% @doc An editable text field with sigil's filtered popup list. `Value' is
%% the selected item's value (a list of values with `multiple' or
%% `checkboxes'), or `undefined'.
%%
%% Css: `disabled', `no_arrow', `multiple' (tags), `checkboxes' (multiple
%% with check boxes), `free_text' (the typed text is a value too; without
%% it the value is always one of the items).
%% Options (in Attrs): `placeholder', `search_mode' (contains_ignore_case
%% (default), contains, starts_with_ignore_case, starts_with,
%% equals_ignore_case, equals, none), `min_length' (characters before the
%% list opens while typing, default 0), `empty_text' (default "No results
%% found"), `dropdown_height' (px, default 240), `search' (an action ref,
%% see the module doc).
-spec combobox([item()], term() | [term()] | undefined, aihtml_html:css(),
               aihtml_html:attrs()) -> aihtml_html:element().
combobox(Items0, Value, Css, Attrs0) ->
    Entry = aihtml_catalog:entry(?MODULE, combobox),
    {Opts, Attrs1} = aihtml_catalog:split_options(Entry, Attrs0),
    {Name, Id, Attrs} = take_name_id(Attrs1),
    Flags = aihtml_catalog:flags(Entry, Css),
    Multi = lists:member(multiple, Flags) orelse lists:member(checkboxes, Flags),
    Checkboxes = lists:member(checkboxes, Flags),
    Disabled = lists:member(disabled, Flags),
    Items = [item(I) || I <- Items0],
    Selected = case {Multi, Value} of
                   {_, undefined} -> [];
                   {true, []} -> [];
                   {true, [V1 | _] = Vs} when not is_integer(V1) -> [text(V) || V <- Vs];
                   {_, V} -> [text(V)]
               end,
    Placeholder = maps:get(placeholder, Opts, <<>>),
    Mode = maps:get(search_mode, Opts, contains_ignore_case),
    lists:member(Mode, [contains_ignore_case, contains, starts_with_ignore_case,
                        starts_with, equals_ignore_case, equals, none])
        orelse error({aihtml, {bad_search_mode, Mode}}),
    Search = case maps:get(search, Opts, undefined) of
                 undefined -> [];
                 Ref -> aihtml:on(input, Ref, #{debounce => 250})
             end,
    ListId = sub_id(Id, <<"list">>),
    Input = ?H:void(input, [<<"ah-combobox-input">>],
                    [[{type, text}, {id, sub_id(Id, <<"input">>)},
                      {autocomplete, off}, {spellcheck, <<"false">>},
                      {placeholder, case Multi andalso Selected =/= [] of
                                        true -> undefined;
                                        false -> Placeholder
                                    end},
                      {value, case Multi of
                                  true -> <<>>;
                                  false -> case Selected of
                                               [S] -> label_of(S, Items);
                                               [] -> <<>>
                                           end
                              end},
                      {disabled, Disabled},
                      {role, combobox}, {aria_autocomplete, list},
                      {aria_haspopup, listbox}, {aria_expanded, <<"false">>},
                      {aria_controls, ListId},
                      {data_combobox, Id},
                      {data_checkboxes, Checkboxes andalso <<"true">>}],
                     Search]),
    Field = case Multi of
                false -> Input;
                true -> ?H:el('div', [[tag(S, label_of(S, Items)) || S <- Selected], Input],
                              [<<"ah-combobox-tags">>], [])
            end,
    Arrow = ?H:el(span, ?H:el(span, <<"▼"/utf8>>, [<<"ah-combobox-arrow-icon">>], []),
                  [<<"ah-combobox-arrow">>], [{aria_hidden, <<"true">>}]),
    Height = maps:get(dropdown_height, Opts, undefined),
    Popup = ?H:el('div',
                  ?H:el(ul, render_items(Items, Selected, Checkboxes, Id),
                        [<<"ah-combobox-list">>],
                        [{id, ListId}, {role, listbox},
                         {aria_multiselectable, Multi andalso <<"true">>}]),
                  [<<"ah-combobox-popup">>],
                  [{style, [<<"max-height:", (integer_to_binary(Height))/binary, "px">>
                            || is_integer(Height)]}]),
    ?H:el('div',
          [?H:el('div', [Field, Arrow], [<<"ah-combobox-input-area">>], []),
           hidden(Name, join(Selected)),
           Popup],
          aihtml_catalog:classes(Entry, Css),
          [[{id, Id}, {data_ah, <<"combobox">>}, {data_ah_value, join(Selected)},
            {data_ah_search_mode, Mode},
            {data_ah_min_length, maps:get(min_length, Opts, undefined)},
            {data_ah_empty, maps:get(empty_text, Opts, undefined)},
            {data_ah_remote, Search =/= []},
            {data_ah_placeholder, Multi andalso Placeholder},
            {aria_disabled, Disabled andalso <<"true">>}],
           Attrs]).

item(#{value := V} = M) ->
    maps:merge(#{label => text(maps:get(label, M, V))},
               maps:map(fun(disabled, B) when is_boolean(B) -> B;
                           (_, X) -> text(X)
                        end, maps:with([value, description, group, disabled], M)));
item({V, L}) -> #{value => text(V), label => text(L)};
item(V) when is_binary(V); is_atom(V); is_integer(V); is_list(V) ->
    T = text(V), #{value => T, label => T};
item(Other) -> error({aihtml, {bad_combobox_item, Other}}).

label_of(V, Items) ->
    case [L || #{value := V1, label := L} <- Items, V1 =:= V] of
        [L | _] -> L;
        [] -> V
    end.

%% Same markup as tags the browser adds: templates/combobox_tag.mustache
tag(Value, Label) ->
    aihtml_tpl:safe(tpl_combobox_tag(#{value => Value, label => Label})).

%% Grouped like sigil (group-by, in order of first appearance); the JS
%% renders the same markup from the same data.
render_items(Items, Selected, Checkboxes, Id) ->
    Groups = lists:foldl(fun(I, Acc) ->
                                 G = maps:get(group, I, undefined),
                                 case lists:keyfind(G, 1, Acc) of
                                     false -> Acc ++ [{G, [I]}];
                                     {G, Is} -> lists:keyreplace(G, 1, Acc, {G, Is ++ [I]})
                                 end
                         end, [], Items),
    Ordered = lists:append([Is || {_, Is} <- Groups]),
    Indexed = lists:zip(lists:seq(0, length(Ordered) - 1), Ordered),
    [[[?H:el(li, G, [<<"ah-combobox-group-header">>], [{role, presentation}])
       || G =/= undefined],
      [render_item(N, I, Selected, Checkboxes, Id)
       || {N, I} <- Indexed, maps:get(group, I, undefined) =:= G]]
     || {G, _} <- Groups].

render_item(N, #{value := V, label := L} = I, Selected, Checkboxes, Id) ->
    Sel = lists:member(V, Selected),
    Dis = maps:get(disabled, I, false),
    ?H:el(li,
          [[?H:el(span, [?H:el(span, <<"✓"/utf8>>, [<<"ah-combobox-checkbox-icon">>], [])
                         || Sel],
                  [<<"ah-combobox-checkbox">>, [<<"ah-combobox-checkbox-checked">> || Sel]], [])
            || Checkboxes],
           ?H:el('div',
                 [?H:el('div', L, [<<"ah-combobox-item-label">>], []),
                  [?H:el('div', D, [<<"ah-combobox-item-desc">>], [])
                   || #{description := D} <- [I]]],
                 [<<"ah-combobox-item-content">>], [])],
          [<<"ah-combobox-item">>, [<<"ah-combobox-item-selected">> || Sel],
           [<<"ah-combobox-item-disabled">> || Dis]],
          [{id, sub_id(Id, <<"opt-", (integer_to_binary(N))/binary>>)},
           {role, option}, {aria_selected, atom_to_binary(Sel)},
           {aria_disabled, Dis andalso <<"true">>},
           {data_index, N}, {data_value, V}, {data_label, L},
           {data_desc, maps:get(description, I, undefined)},
           {data_group, maps:get(group, I, undefined)}]).

join(Vs) -> iolist_to_binary(lists:join(<<",">>, Vs)).

%%%===================================================================
%%% Server-side search
%%%===================================================================

%% @doc Replace the items of a combobox from inside an action, typically
%% the `search' action: `set_items(Ctx, Event, Items)'. `Target' is the
%% search action's event (whose `data' names the combobox and tells
%% whether it has check boxes) or `{id, RootId}'. Items take the same forms
%% as in `combobox/4'. Sends two operations: the rendered items morphed
%% into `<root id>-list' (morph_inner), and a call of the behaviour method
%% `itemsLoaded' on the root, which re-reads the list, marks the selected
%% items, highlights the query and opens the popup.
-spec set_items(aihtml_action:ctx(), {id, iodata() | atom()} | aihtml_action:event(),
                [item()]) -> ok.
set_items(Ctx, #{data := Data}, Items) ->
    set_items(Ctx, {id, maps:get(<<"combobox">>, Data)}, Items,
              #{checkboxes => maps:get(<<"checkboxes">>, Data, <<>>) =:= <<"true">>});
set_items(Ctx, Target, Items) ->
    set_items(Ctx, Target, Items, #{}).

%% @doc `set_items/3' with options: `checkboxes' (render check boxes,
%% default false) and `selected' (values to mark, default none; the
%% browser marks its current selection anyway).
-spec set_items(aihtml_action:ctx(), {id, iodata() | atom()}, [item()],
                #{checkboxes => boolean(), selected => [term()]}) -> ok.
set_items(Ctx, {id, Id0}, Items, Opts) ->
    Id = text(Id0),
    Html = render_items([item(I) || I <- Items],
                        [text(V) || V <- maps:get(selected, Opts, [])],
                        maps:get(checkboxes, Opts, false), Id),
    aihtml_action:html(Ctx, {id, sub_id(Id, <<"list">>)}, Html, morph_inner),
    aihtml_action:call(Ctx, {id, Id}, itemsLoaded, []).

%% @doc Functions besides the components that the aihtml facade re-exports.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{set_items, 3}, {set_items, 4}].

%%%===================================================================
%%% Shared
%%%===================================================================

%% `name' goes to the hidden input, `id' stays on the root (and derives the
%% ids of the parts). A root without an id gets one: the parts refer to
%% each other by id (aria-controls, the search event's data).
take_name_id(Attrs0) ->
    Attrs = ?H:attrs(Attrs0),
    Name = case lists:keyfind(<<"name">>, 1, Attrs) of
               {_, N} -> N;
               false -> undefined
           end,
    Id = case lists:keyfind(<<"id">>, 1, Attrs) of
             {_, I} -> I;
             false -> new_id()
         end,
    {Name, Id, [A || {K, _} = A <- Attrs, K =/= <<"name">>, K =/= <<"id">>]}.

new_id() ->
    <<"ah-p", (integer_to_binary(erlang:unique_integer([positive])))/binary>>.

sub_id(Id, Part) -> <<Id/binary, "-", Part/binary>>.

hidden(undefined, _) -> [];
hidden(Name, Value) -> ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value}]).

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => datepicker, category => form,
       signature => <<"datepicker(Value, Css, Attrs)">>,
       root => <<"ah-datepicker">>,
       flags => [disabled, readonly, range, clearable, inline],
       classes => #{clearable => [<<"ah-datepicker-clearable">>]},
       options => [placeholder, format, min, max, disabled_dates, first_day,
                   week_numbers, other_month_days, weekends, labels],
       behavior => <<"datepicker">>,
       events => [<<"change">>, <<"ah:open">>, <<"ah:close">>],
       doc => <<"A date field with a month grid popup and full keyboard support; "
                "single dates or ranges, value in data-ah-value as yyyy-mm-dd.">>,
       option_docs =>
           #{disabled => <<"Not editable; the popup does not open.">>,
             readonly => <<"Shows the value; the popup does not open.">>,
             range => <<"Select a range: value \"from,to\" (implied by a {From, To} value).">>,
             clearable => <<"A clear button (and Backspace/Delete) empties the value.">>,
             inline => <<"The month grid is always shown under the field, rendered on the server.">>,
             placeholder => <<"Text of the empty field (default \"Select date...\").">>,
             format => <<"Display format: yyyy yy MMMM MMM MM M dd d (default yyyy-MM-dd).">>,
             min => <<"Earliest selectable date (ISO binary or calendar:date()).">>,
             max => <<"Latest selectable date.">>,
             disabled_dates => <<"List of dates that cannot be picked.">>,
             first_day => <<"First day of the week, 0 = Sunday (default) .. 6.">>,
             week_numbers => <<"Show a week number column.">>,
             other_month_days => <<"Show days of the neighbouring months (default true).">>,
             weekends => <<"Colour Saturdays and Sundays.">>,
             labels => <<"Map of months, months_short, weekdays (from Sunday), title (a format), "
                         "today, clear, prev_month, next_month, prev_year, next_year.">>},
       methods =>
           [#{name => setValue, args => <<"(Iso | \"from,to\" | [From, To])">>,
              doc => <<"Set the value without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
            #{name => clear, args => <<"()">>, doc => <<"Empty the value and fire change.">>},
            #{name => open, args => <<"()">>, doc => <<"Open the calendar.">>},
            #{name => close, args => <<"()">>, doc => <<"Close the calendar.">>}]},
     #{name => combobox, category => form,
       signature => <<"combobox(Items, Value, Css, Attrs)">>,
       root => <<"ah-combobox">>,
       flags => [disabled, no_arrow, multiple, checkboxes, free_text],
       classes => #{no_arrow => [<<"ah-combobox-no-arrow">>],
                    free_text => [<<"ah-combobox-free-text">>]},
       options => [placeholder, search_mode, min_length, empty_text,
                   dropdown_height, search],
       behavior => <<"combobox">>,
       events => [<<"change">>, <<"ah:open">>, <<"ah:close">>],
       doc => <<"An editable field with a filtered, keyboard navigable list; "
                "single or multiple values, local or server-side search.">>,
       option_docs =>
           #{disabled => <<"Not editable.">>,
             no_arrow => <<"Hide the dropdown arrow.">>,
             multiple => <<"Several values, shown as tags; value \"a,b,c\".">>,
             checkboxes => <<"Multiple, with a check box on every row.">>,
             free_text => <<"The typed text becomes the value on Enter or blur.">>,
             placeholder => <<"Text of the empty field.">>,
             search_mode => <<"contains_ignore_case (default), contains, starts_with_ignore_case, "
                              "starts_with, equals_ignore_case, equals or none.">>,
             min_length => <<"Characters typed before the list opens (default 0).">>,
             empty_text => <<"Shown when nothing matches (default \"No results found\").">>,
             dropdown_height => <<"Maximum list height in px (default 240).">>,
             search => <<"Action ref {Module, Action, Args} run (debounced) as the user types; "
                         "Event.value is the query, the action answers with set_items/3.">>},
       methods =>
           [#{name => itemsLoaded, args => <<"()">>,
              doc => <<"Re-read the list after set_items/3 morphed new rows in; "
                       "called by set_items itself.">>},
            #{name => setValue, args => <<"(Value | [Value])">>,
              doc => <<"Set the value without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return data-ah-value.">>},
            #{name => clear, args => <<"()">>, doc => <<"Empty the value and fire change.">>},
            #{name => open, args => <<"()">>, doc => <<"Open the list.">>},
            #{name => close, args => <<"()">>, doc => <<"Close the list.">>}]}].
