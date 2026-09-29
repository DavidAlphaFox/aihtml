%% Tests for aihtml_form_pickers. The module is also the fake action
%% module of the server-side search round trip.
-module(aihtml_form_pickers_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_form_pickers.hrl").

-export([action/4]).

-define(M, aihtml_form_pickers).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

%%%===================================================================
%%% datepicker
%%%===================================================================

datepicker_single_test() ->
    H = r(?M:datepicker(<<"2026-09-29">>, [<<"w-64">>], [{id, dp}, {name, due}, {title, <<"t">>}])),
    ?assert(has(<<"class=\"ah-datepicker w-64\"">>, H)),
    ?assert(has(<<"id=\"dp\"">>, H)),
    ?assert(has(<<"data-ah=\"datepicker\"">>, H)),
    ?assert(has(<<"data-ah-value=\"2026-09-29\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"due\" value=\"2026-09-29\">">>, H)),
    ?assert(has(<<"value=\"2026-09-29\"">>, H)),
    ?assert(has(<<"id=\"dp-input\"">>, H)),
    ?assert(has(<<"role=\"combobox\"">>, H)),
    ?assert(has(<<"aria-haspopup=\"dialog\"">>, H)),
    ?assert(has(<<"class=\"ah-datepicker-popup\"">>, H)),
    ?assert(has(<<"placeholder=\"Select date...\"">>, H)),
    ?assert(has(<<"title=\"t\"">>, H)),
    %% name is not written on the root
    ?assertEqual(1, length(binary:matches(H, <<"name=">>))).

datepicker_value_forms_test() ->
    H = r(?M:datepicker({2026, 1, 5}, [], [{format, <<"d MMM yy">>}])),
    ?assert(has(<<"data-ah-value=\"2026-01-05\"">>, H)),
    ?assert(has(<<"value=\"5 Jan 26\"">>, H)),
    E = r(?M:datepicker(undefined, [], [])),
    ?assert(has(<<"data-ah-value=\"\"">>, E)),
    ?assert(has(<<"id=\"ah-p">>, E)),          % an id is generated
    ?assertError({aihtml, {bad_date, <<"2026-02-30">>}},
                 r(?M:datepicker(<<"2026-02-30">>, [], []))),
    ?assertError({aihtml, {bad_date, _}}, r(?M:datepicker(<<"29/09/2026">>, [], []))).

datepicker_range_test() ->
    H = r(?M:datepicker({<<"2026-09-18">>, {2026, 9, 10}}, [], [{name, p}])),
    ?assert(has(<<"ah-datepicker-range">>, H)),
    ?assert(has(<<"data-ah-range">>, H)),
    %% sorted
    ?assert(has(<<"data-ah-value=\"2026-09-10,2026-09-18\"">>, H)),
    ?assert(has(<<"value=\"2026-09-10 - 2026-09-18\"">>, H)),
    E = r(?M:datepicker(undefined, [range], [])),
    ?assert(has(<<"class=\"ah-datepicker ah-datepicker-range\"">>, E)),
    ?assert(has(<<"data-ah-value=\"\"">>, E)).

datepicker_options_test() ->
    H = r(?M:datepicker(undefined, [disabled, clearable],
                        [{min, {2026, 1, 1}}, {max, <<"2026-12-31">>},
                         {disabled_dates, [<<"2026-05-01">>, {2026, 10, 1}]},
                         {first_day, 1}, {week_numbers, true}, {other_month_days, false},
                         {weekends, true}, {placeholder, <<"Due">>},
                         {labels, #{today => <<"今天"/utf8>>}}])),
    ?assert(has(<<"ah-datepicker-disabled">>, H)),
    ?assert(has(<<"ah-datepicker-clearable">>, H)),
    ?assert(has(<<"data-ah-min=\"2026-01-01\"">>, H)),
    ?assert(has(<<"data-ah-max=\"2026-12-31\"">>, H)),
    ?assert(has(<<"data-ah-disabled-dates=\"2026-05-01,2026-10-01\"">>, H)),
    ?assert(has(<<"data-ah-first-day=\"1\"">>, H)),
    ?assert(has(<<"data-ah-week-numbers">>, H)),
    ?assert(has(<<"data-ah-weekends">>, H)),
    ?assert(has(<<"data-ah-other-month-days=\"false\"">>, H)),
    ?assert(has(<<"placeholder=\"Due\"">>, H)),
    ?assert(has(<<" disabled">>, H)),
    ?assert(has(<<"aria-disabled=\"true\"">>, H)),
    %% no clear button when disabled
    ?assertNot(has_quiet(<<"ah-datepicker-clear\"">>, H)),
    %% labels travel as JSON, escaped in the attribute
    ?assert(has(<<"&quot;today&quot;:&quot;"/utf8>>, H)),
    ?assertError({aihtml, {bad_first_day, 7}}, r(?M:datepicker(undefined, [], [{first_day, 7}]))),
    ?assertError({aihtml, {bad_datepicker_label, months}},
                 r(?M:datepicker(undefined, [], [{labels, #{months => [<<"x">>]}}]))),
    ?assertError({aihtml, {unknown_modifier, datepicker, big, _}},
                 ?M:datepicker(undefined, [big], [])).

datepicker_labels_format_test() ->
    Months = [<<"M", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 12)],
    H = r(?M:datepicker(<<"2026-03-07">>, [], [{format, <<"MMMM/dd">>},
                                                {labels, #{months => Months}}])),
    ?assert(has(<<"value=\"M3/07\"">>, H)).

datepicker_clearable_test() ->
    H = r(?M:datepicker(<<"2026-03-07">>, [clearable], [])),
    ?assert(has(<<"class=\"ah-datepicker-clear\"">>, H)),
    ?assert(has(<<"aria-label=\"Clear\"">>, H)).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

datepicker_inline_test() ->
    H = r(?M:datepicker(<<"2026-09-15">>, [inline], [{id, di}, {first_day, 1}, {week_numbers, true},
                                                      {min, <<"2026-09-05">>},
                                                      {disabled_dates, [<<"2026-09-21">>]}])),
    ?assert(has(<<"ah-datepicker-inline">>, H)),
    ?assert(has(<<"role=\"group\"">>, H)),
    ?assert(has(<<"<div class=\"ah-datepicker-title\" id=\"di-title\" aria-live=\"polite\">September 2026</div>">>, H)),
    %% Monday first: the grid starts on Monday 31 August, week 36
    ?assert(has(<<"<div class=\"ah-datepicker-weekday\">Mo</div><div class=\"ah-datepicker-weekday\">Tu</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-datepicker-week-num\">36</div><div class=\"ah-datepicker-day ah-datepicker-day-other-month ah-datepicker-day-disabled\" role=\"gridcell\" id=\"di-d2026-08-31\"">>, H)),
    ?assert(has(<<"<div class=\"ah-datepicker-week-num\">40</div>">>, H)),
    ?assertNot(has_quiet(<<"-d2026-10-05">>, H)),       % five rows
    ?assert(has(<<"ah-datepicker-day ah-datepicker-day-selected ah-datepicker-day-focused\" role=\"gridcell\" id=\"di-d2026-09-15\" data-date=\"2026-09-15\" aria-selected=\"true\" aria-disabled=\"false\" aria-label=\"15 September 2026\"">>, H)),
    ?assert(has(<<"id=\"di-d2026-09-21\" data-date=\"2026-09-21\" aria-selected=\"false\" aria-disabled=\"true\"">>, H)),
    %% the non-inline popup starts empty
    ?assert(has(<<"<div class=\"ah-datepicker-popup\" role=\"dialog\" aria-label=\"Choose date\"></div>">>,
                r(?M:datepicker(<<"2026-09-15">>, [], [])))).

datepicker_inline_range_week_numbers_test() ->
    %% Sunday first, a range across the turn of the year: week 1 of 2027
    H = r(?M:datepicker({<<"2026-12-30">>, <<"2027-01-02">>}, [inline],
                        [{id, dr}, {week_numbers, true}, {other_month_days, false}])),
    ?assert(has(<<"December 2026">>, H)),
    ?assert(has(<<"ah-datepicker-day-in-range ah-datepicker-day-range-start">>, H)),
    ?assert(has(<<"<div class=\"ah-datepicker-week-num\">1</div>">>, H)),
    ?assert(has(<<"ah-datepicker-day-empty">>, H)).

combobox_tag_template_test() ->
    H = r(?M:combobox([{<<"a&b">>, <<"A <&>">>}], [<<"a&b">>], [multiple], [])),
    ?assert(has(<<"<span class=\"ah-combobox-tag\"><span class=\"ah-combobox-tag-text\">A &lt;&amp;&gt;</span><span class=\"ah-combobox-tag-close\" data-value=\"a&amp;b\" role=\"button\" aria-label=\"Remove A &lt;&amp;&gt;\">&times;</span></span>">>, H)).

%%%===================================================================
%%% combobox
%%%===================================================================

combobox_single_test() ->
    Items = [<<"Apple">>, {b, <<"Banana">>}, #{value => 3, label => <<"Cherry <3>">>,
                                               description => <<"red">>}],
    H = r(?M:combobox(Items, b, [<<"w-56">>], [{id, cb}, {name, fruit},
                                               {placeholder, <<"Pick">>}])),
    ?assert(has(<<"class=\"ah-combobox w-56\"">>, H)),
    ?assert(has(<<"data-ah=\"combobox\"">>, H)),
    ?assert(has(<<"data-ah-value=\"b\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"fruit\" value=\"b\">">>, H)),
    ?assert(has(<<"value=\"Banana\"">>, H)),               % the label is shown
    ?assert(has(<<"placeholder=\"Pick\"">>, H)),
    ?assert(has(<<"aria-controls=\"cb-list\"">>, H)),
    ?assert(has(<<"data-combobox=\"cb\"">>, H)),
    ?assert(has(<<"<ul class=\"ah-combobox-list\" id=\"cb-list\" role=\"listbox\">">>, H)),
    ?assert(has(<<"class=\"ah-combobox-item ah-combobox-item-selected\" id=\"cb-opt-1\" role=\"option\" aria-selected=\"true\"">>, H)),
    ?assert(has(<<"Cherry &lt;3&gt;">>, H)),               % escaped
    ?assert(has(<<"<div class=\"ah-combobox-item-desc\">red</div>">>, H)),
    ?assert(has(<<"data-value=\"3\"">>, H)),
    ?assert(has(<<"ah-combobox-arrow-icon">>, H)),
    ?assertNot(has_quiet(<<"data-ah-on">>, H)).

combobox_value_not_in_items_test() ->
    H = r(?M:combobox([], <<"zed">>, [], [])),
    ?assert(has(<<"value=\"zed\"">>, H)),
    ?assert(has(<<"data-ah-value=\"zed\"">>, H)).

combobox_multi_test() ->
    H = r(?M:combobox([<<"a">>, <<"b">>, <<"c">>], [<<"a">>, <<"c">>], [checkboxes],
                      [{name, n}, {placeholder, <<"P">>}])),
    ?assert(has(<<"ah-combobox-checkboxes">>, H)),
    ?assert(has(<<"data-ah-value=\"a,c\"">>, H)),
    ?assert(has(<<"name=\"n\" value=\"a,c\"">>, H)),
    ?assert(has(<<"aria-multiselectable=\"true\"">>, H)),
    ?assertEqual(2, length(binary:matches(H, <<"class=\"ah-combobox-tag\"">>))),
    ?assertEqual(2, length(binary:matches(H, <<"ah-combobox-checkbox ah-combobox-checkbox-checked">>))),
    ?assertEqual(1, length(binary:matches(H, <<"class=\"ah-combobox-checkbox\"">>))),
    %% the placeholder moves to the root while tags are shown
    ?assert(has(<<"data-ah-placeholder=\"P\"">>, H)),
    ?assertNot(has_quiet(<<" placeholder=">>, H)).

combobox_groups_test() ->
    Items = [#{value => 1, label => <<"A">>, group => <<"G1">>},
             #{value => 2, label => <<"B">>, group => <<"G2">>},
             #{value => 3, label => <<"C">>, group => <<"G1">>, disabled => true}],
    H = r(?M:combobox(Items, undefined, [], [{id, g}])),
    [{P1, _}, {P2, _}] = binary:matches(H, <<"ah-combobox-group-header">>),
    {PA, _} = binary:match(H, <<"data-value=\"1\"">>),
    {PC, _} = binary:match(H, <<"data-value=\"3\"">>),
    {PB, _} = binary:match(H, <<"data-value=\"2\"">>),
    %% G1 (A, C) then G2 (B): grouped in order of first appearance
    ?assert(P1 < PA andalso PA < PC andalso PC < P2 andalso P2 < PB),
    ?assert(has(<<"ah-combobox-item ah-combobox-item-disabled\" id=\"g-opt-1\"">>, H)),
    ?assert(has(<<"data-group=\"G1\"">>, H)).

combobox_flags_options_test() ->
    H = r(?M:combobox([<<"x">>], undefined, [disabled, no_arrow, free_text],
                      [{search_mode, starts_with}, {min_length, 2},
                       {empty_text, <<"Nothing">>}, {dropdown_height, 120}])),
    ?assert(has(<<"class=\"ah-combobox ah-combobox-disabled ah-combobox-free-text ah-combobox-no-arrow\"">>, H)),
    ?assert(has(<<"data-ah-search-mode=\"starts_with\"">>, H)),
    ?assert(has(<<"data-ah-min-length=\"2\"">>, H)),
    ?assert(has(<<"data-ah-empty=\"Nothing\"">>, H)),
    ?assert(has(<<"style=\"max-height:120px\"">>, H)),
    ?assertError({aihtml, {bad_search_mode, fuzzy}},
                 r(?M:combobox([], undefined, [], [{search_mode, fuzzy}]))),
    ?assertError({aihtml, {bad_combobox_item, _}},
                 r(?M:combobox([{1, 2, 3}], undefined, [], []))).

%%%===================================================================
%%% Server-side search: render, verify the token, run the action
%%%===================================================================

action(search, #{source := Source}, #{value := Q} = Ev, Ctx) ->
    Lower = string:lowercase(Q),
    ?M:set_items(Ctx, Ev, [I || I <- Source,
                                binary:match(string:lowercase(I), Lower) =/= nomatch]).

search_round_trip_test() ->
    Ref = {?MODULE, search, #{source => [<<"Apple">>, <<"Apricot">>, <<"Banana">>]}},
    H = r(?M:combobox([], undefined, [], [{id, <<"cb-s">>}, {search, Ref}])),
    ?assert(has(<<"data-ah-remote">>, H)),
    %% the input carries the debounced action: input:TOKEN:250
    {match, [Token]} = re:run(H, <<"data-ah-on=\"input:([^:\"]+):250\"">>,
                              [{capture, all_but_first, binary}]),
    {ok, Ref} = aihtml_action:verify(Token),
    Self = self(),
    Event = #{<<"type">> => <<"input">>, <<"id">> => <<"cb-s-input">>,
              <<"value">> => <<"AP">>, <<"data">> => #{<<"combobox">> => <<"cb-s">>}},
    ok = aihtml_action:execute(Ref, Event, #{emit => fun(E) -> Self ! {ev, E} end}),
    Evs = collect(),
    ?assertEqual([<<"RUN_STARTED">>, <<"CUSTOM">>, <<"RUN_FINISHED">>],
                 [maps:get(<<"type">>, E) || E <- Evs]),
    [#{<<"value">> := [Html, Call]}] = [E || #{<<"type">> := <<"CUSTOM">>} = E <- Evs],
    %% the items, rendered like the first render, morphed into the list
    #{op := html, swap := morph_inner, id := <<"cb-s-list">>, html := Items} = Html,
    ?assertEqual(extract_list(r(?M:combobox([<<"Apple">>, <<"Apricot">>], undefined, [],
                                            [{id, <<"cb-s">>}]))),
                 Items),
    ?assert(has(<<"id=\"cb-s-opt-1\" role=\"option\" aria-selected=\"false\" data-index=\"1\" data-value=\"Apricot\"">>, Items)),
    ?assertNot(has_quiet(<<"Banana">>, Items)),
    ?assertEqual(#{op => call, id => <<"cb-s">>, method => <<"itemsLoaded">>, args => []}, Call),
    %% what the transport sends is valid JSON
    _ = iolist_to_binary(json:encode([Html, Call])).

%% The <ul> content of a rendered combobox.
extract_list(H) ->
    {match, [Inner]} = re:run(H, <<"<ul class=\"ah-combobox-list\"[^>]*>(.*)</ul>">>,
                              [{capture, all_but_first, binary}]),
    Inner.

search_checkboxes_test() ->
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:set_items(Ctx, #{data => #{<<"combobox">> => <<"c">>,
                                                  <<"checkboxes">> => <<"true">>}}, [<<"a">>])
            end),
    [#{html := H}, _] = Ops,
    ?assert(has(<<"class=\"ah-combobox-checkbox\"">>, H)),
    %% the input tells the search action about check boxes
    ?assert(has(<<"data-checkboxes=\"true\"">>,
                r(?M:combobox([], undefined, [checkboxes], [{search, {?MODULE, search, #{}}}])))).

collect() ->
    receive {ev, E} -> [E | collect()]
    after 0 -> []
    end.

set_items_targets_test() ->
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:set_items(Ctx, {id, cb}, [#{value => 1, label => <<"One">>,
                                                    description => <<"d">>, group => g,
                                                    disabled => true}]),
                    ?M:set_items(Ctx, {id, <<"x">>}, [<<"a">>, <<"b">>],
                                 #{selected => [b], checkboxes => true})
            end),
    [#{op := html, swap := morph_inner, id := <<"cb-list">>, html := H1},
     #{op := call, id := <<"cb">>, method := <<"itemsLoaded">>},
     #{op := html, id := <<"x-list">>, html := H2},
     #{op := call, id := <<"x">>}] = Ops,
    ?assert(has(<<"<li class=\"ah-combobox-group-header\" role=\"presentation\">g</li>">>, H1)),
    ?assert(has(<<"ah-combobox-item-disabled">>, H1)),
    ?assert(has(<<"<div class=\"ah-combobox-item-desc\">d</div>">>, H1)),
    ?assert(has(<<"ah-combobox-item ah-combobox-item-selected\" id=\"x-opt-1\"">>, H2)),
    ?assert(has(<<"ah-combobox-checkbox-checked">>, H2)),
    ?assertError(function_clause, aihtml_action:render_ops(
                                    fun(Ctx) -> ?M:set_items(Ctx, <<"#cb">>, []) end)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := datepicker}, #{name := combobox}] = ?M:catalog(),
    [begin
         E = aihtml_catalog:entry(?M, N),
         #{flags := Flags} = E,
         [_ | _] = aihtml_catalog:classes(E, Flags)
     end || N <- [datepicker, combobox]],
    ?assertEqual([{set_items, 3}, {set_items, 4}], ?M:facade_extras()),
    %% every option and flag is documented, every behaviour method listed
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         ?assert(lists:member(setValue, [Name || #{name := Name} <- Ms]))
     end || N <- [datepicker, combobox]],
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    Labels = #{today => <<"Now">>},
    ?assertEqual(r(?M:datepicker(<<"2026-09-15">>, [inline, clearable, <<"w-64">>],
                                 [{id, dp}, {name, due}, {first_day, 1}, {min, {2026, 9, 5}},
                                  {week_numbers, true}, {labels, Labels}, {title, <<"t">>}])),
                 r(#ah_datepicker{value = <<"2026-09-15">>, inline = true, clearable = true,
                                  css = [<<"w-64">>], id = dp, name = due, first_day = 1,
                                  min = {2026, 9, 5}, week_numbers = true, labels = Labels,
                                  attrs = [{title, <<"t">>}]})),
    Items = [<<"a">>, {b, <<"B">>}, #{value => c, group => <<"G">>}],
    ?assertEqual(r(?M:combobox(Items, [a, c], [checkboxes, no_arrow],
                               [{id, <<"cb">>}, {name, n}, {placeholder, <<"P">>},
                                {search_mode, starts_with}, {dropdown_height, 100},
                                {search, {?MODULE, search, #{}}}])),
                 r(#ah_combobox{items = Items, value = [a, c], checkboxes = true,
                                no_arrow = true, id = <<"cb">>, name = n, placeholder = <<"P">>,
                                search_mode = starts_with, dropdown_height = 100,
                                search = {?MODULE, search, #{}}})).

builder_fills_fields_test() ->
    D = ?M:datepicker({<<"2026-01-01">>, undefined}, [range, disabled, <<"x">>],
                      [{id, d}, {format, <<"d MMM">>}, {max, <<"2026-12-31">>},
                       {disabled_dates, [{2026, 5, 1}]}, {other_month_days, false},
                       {title, <<"t">>}]),
    ?assertMatch(#ah_datepicker{value = {<<"2026-01-01">>, undefined}, range = true,
                                disabled = true, readonly = false, id = d,
                                format = <<"d MMM">>, max = <<"2026-12-31">>,
                                disabled_dates = [{2026, 5, 1}], other_month_days = false,
                                css = [<<"x">>], attrs = [{title, <<"t">>}]}, D),
    C = ?M:combobox([<<"a">>], <<"a">>, [free_text], [{min_length, 2}, {empty_text, <<"-">>}]),
    ?assertMatch(#ah_combobox{items = [<<"a">>], value = <<"a">>, free_text = true,
                              min_length = 2, empty_text = <<"-">>, id = undefined,
                              attrs = []}, C),
    ?assertError({aihtml, {record_only_field, ah_combobox, postback}},
                 ?M:combobox([], undefined, [], [{postback, pick}])).

generated_id_test() ->
    %% no id: one is generated at render, and the parts refer to it
    H = r(#ah_combobox{items = [<<"a">>], postback = pick}),
    {match, [Id]} = re:run(H, <<"^<div class=\"ah-combobox\" id=\"(ah-p[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(<<"aria-controls=\"", Id/binary, "-list\"">>, H)),
    ?assertEqual(1, length(binary:matches(H, <<" id=\"", Id/binary, "\"">>))),
    %% each render gets its own id
    R = #ah_datepicker{},
    ?assertNotEqual(r(R), r(R)).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, picked, #{id => 7}}},
                 Token(#ah_datepicker{postback = {picked, #{id => 7}}})),
    ?assertEqual({<<"change">>, {other_mod, pick, #{}}},
                 Token(#ah_combobox{items = [<<"a">>], postback = pick,
                                    delegate = other_mod})),
    %% the id stays first on the root, the postback follows its own attributes
    H = r(#ah_combobox{id = c, postback = pick}),
    ?assertMatch({match, _}, re:run(H, <<"^<div class=\"ah-combobox\" id=\"c\" "
                                         "data-ah=\"combobox\"[^>]* data-ah-on=\"change:">>)).

field_validation_test() ->
    ?assertError({aihtml, {bad_first_day, 9}}, r(#ah_datepicker{first_day = 9})),
    ?assertError({aihtml, {bad_date, <<"soon">>}}, r(#ah_datepicker{min = <<"soon">>})),
    ?assertError({aihtml, {bad_datepicker_label, today_}},
                 r(#ah_datepicker{labels = #{today_ => <<"x">>}})),
    ?assertError({aihtml, {bad_flag, datepicker, inline, yes}},
                 r(#ah_datepicker{inline = yes})),
    ?assertError({aihtml, {bad_search_mode, fuzzy}}, r(#ah_combobox{search_mode = fuzzy})),
    ?assertError({aihtml, {bad_combobox_item, _}}, r(#ah_combobox{items = [{1, 2, 3}]})),
    ?assertError({aihtml, {modifier_in_css, combobox, multiple}},
                 r(#ah_combobox{css = [multiple]})),
    %% modifier names still fail in the builder
    ?assertError({aihtml, {unknown_modifier, combobox, big, _}},
                 ?M:combobox([], undefined, [big], [])).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(default(Tag))))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].

default(ah_datepicker) -> #ah_datepicker{};
default(ah_combobox) -> #ah_combobox{}.
