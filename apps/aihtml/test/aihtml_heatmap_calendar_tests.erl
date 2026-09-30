%% Tests for aihtml_heatmap_calendar.
-module(aihtml_heatmap_calendar_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_heatmap_calendar.hrl").

-define(M, aihtml_heatmap_calendar).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

%%%===================================================================
%%% heatmap_calendar
%%%===================================================================

heatmap_test() ->
    Data = #{<<"2026-09-01">> => 1, {2026, 9, 2} => 4, "2026-09-03" => 7, <<"2026-09-04">> => 2.5},
    H = r(?M:ah_heatmap_calendar(Data, [], [{months, 1}, {end_date, <<"2026-09-29">>}, {id, hm}])),
    ?assert(has(<<"<div class=\"ah-heatmap-calendar\" data-ah=\"heatmap-calendar\" "
                  "data-tip=\"{value} · {date}\" id=\"hm\">"/utf8>>, H)),
    %% 2026-08-29 is a Saturday: the grid starts on Sunday 2026-08-23 and
    %% ends with the week of the 29th of September (a Tuesday): 6 weeks
    ?assertEqual(6, count(<<"class=\"ah-heatmap-calendar__week\"">>, H)),
    ?assertEqual(42, count(<<"class=\"ah-heatmap-calendar__cell\"">>, H)),
    {match, [First]} = re:run(H, <<"data-date=\"([0-9-]+)\"">>, [{capture, all_but_first, binary}]),
    ?assertEqual(<<"2026-08-23">>, First),
    ?assert(has(<<"data-date=\"2026-10-03\"">>, H)),
    ?assert(has(<<"data-level=\"1\" data-date=\"2026-09-01\" data-value=\"1\"">>, H)),
    ?assert(has(<<"data-level=\"3\" data-date=\"2026-09-02\" data-value=\"4\"">>, H)),
    ?assert(has(<<"data-level=\"4\" data-date=\"2026-09-03\" data-value=\"7\"">>, H)),
    ?assert(has(<<"data-level=\"2\" data-date=\"2026-09-04\" data-value=\"2.5\"">>, H)),
    ?assert(has(<<"data-level=\"0\" data-date=\"2026-09-05\" data-value=\"0\"">>, H)),
    %% month labels by the Wednesday of each week: Aug 1 week, Sep 5 weeks
    ?assert(has(<<"<span class=\"ah-heatmap-calendar__month\" style=\"width:15px;\">Aug</span>"
                  "<span class=\"ah-heatmap-calendar__month\" style=\"width:75px;\">Sep</span>">>, H)),
    ?assert(has(<<"<span class=\"ah-heatmap-calendar__weekday\">Mon</span>">>, H)),
    ?assertEqual(5, count(<<"ah-heatmap-calendar__legend-cell">>, H)),
    ?assert(has(<<"<span>Less</span>">>, H)),
    ?assert(has(<<"<div class=\"ah-heatmap-calendar__tooltip\" data-visible=\"false\" role=\"tooltip\"></div>">>, H)).

heatmap_options_test() ->
    Months = [integer_to_binary(M) || M <- lists:seq(1, 12)],
    H = r(?M:ah_heatmap_calendar([{{2026, 3, 1}, 9}], [],
                                 [{months, 2}, {end_date, {2026, 3, 31}}, {thresholds, [0, 10]},
                                  {legend, false}, {month_labels, Months},
                                  {weekday_labels, [<<"S">>, <<"M">>, <<"T">>, <<"W">>, <<"T">>,
                                                    <<"F">>, <<"S">>]},
                                  {tooltip, <<"{date}: {value}">>}])),
    ?assertNot(has_quiet(<<"legend">>, H)),
    ?assert(has(<<"data-level=\"1\" data-date=\"2026-03-01\"">>, H)),
    ?assert(has(<<">1</span><span class=\"ah-heatmap-calendar__month\"">>, H)),
    ?assert(has(<<"<span class=\"ah-heatmap-calendar__weekday\">S</span>">>, H)),
    ?assert(has(<<"data-tip=\"{date}: {value}\"">>, H)),
    %% 31 March minus 2 months: 31 January (no overflow into February)
    ?assert(has(<<"data-date=\"2026-01-25\"">>, H)),
    ?assertNot(has_quiet(<<"data-date=\"2026-01-24\"">>, H)),
    %% without end_date the grid ends in the current week
    Today = iso(date()),
    ?assert(has(<<"data-date=\"", Today/binary, "\"">>, r(?M:ah_heatmap_calendar(#{}, [], [])))).

iso({Y, M, D}) -> iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := heatmap_calendar}] = ?M:catalog(),
    [begin
         #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E =
             aihtml_catalog:entry(?M, N),
         ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
         [_ | _] = aihtml_catalog:classes(E, Fl),
         [?assert(is_binary(D)) || #{doc := D} <- Ms]
     end || #{name := N} <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         ?assert(byte_size(maps:get(doc, E)) > 0),
         [?assert(byte_size(maps:get(K, maps:get(option_docs, E))) > 0)
          || K <- maps:get(options, E, []) ++ maps:get(flags, E, [])]
     end || E <- ?M:catalog()].

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_heatmap_calendar(#{}, [], [{months, 3}, {end_date, {2026, 9, 29}},
                                                    {legend, false}])),
                 r(#ah_heatmap_calendar{months = 3, end_date = {2026, 9, 29}, legend = false})).

builder_fills_fields_test() ->
    ?assertMatch(#ah_heatmap_calendar{data = #{}, months = 6, tooltip = <<"t">>},
                 ?M:ah_heatmap_calendar(#{}, [], [{months, 6}, {tooltip, <<"t">>}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"([a-z:]+):([^\"]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"ah:select">>, {?MODULE, day, #{}}},
                 Token(#ah_heatmap_calendar{end_date = {2026, 1, 1}, postback = day})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, months, 0}},
                 r(#ah_heatmap_calendar{months = 0})),
    ?assertError({aihtml, {bad_option, thresholds, [3, 1]}},
                 r(#ah_heatmap_calendar{thresholds = [3, 1]})),
    ?assertError({aihtml, {bad_option, month_labels, [a]}},
                 r(#ah_heatmap_calendar{month_labels = [a]})),
    ?assertError({aihtml, {bad_option, legend, true}}, r(#ah_heatmap_calendar{legend = true})),
    ?assertError({aihtml, {bad_date, <<"2026-02-30">>}},
                 r(#ah_heatmap_calendar{end_date = <<"2026-02-30">>})),
    ?assertError({aihtml, {bad_heatmap_value, _, x}},
                 r(#ah_heatmap_calendar{end_date = {2026, 1, 1}, data = #{{2026, 1, 1} => x}})).

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

default(ah_heatmap_calendar) -> #ah_heatmap_calendar{}.
