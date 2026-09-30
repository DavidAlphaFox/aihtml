%% Tests for aihtml_datetime_input.
-module(aihtml_datetime_input_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_datetime_input.hrl").

-define(M, aihtml_datetime_input).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

%%%===================================================================
%%% datetime_input
%%%===================================================================

datetime_input_date_test() ->
    H = r(?M:ah_datetime_input(<<"2026-09-29">>, [<<"w-48">>], [{id, d}, {name, due}, {title, <<"t">>}])),
    ?assert(has(<<"<div class=\"ah-dti-group w-48\" id=\"d\" data-ah=\"datetime-input\" "
                  "data-ah-value=\"2026-09-29\" data-ah-format=\"yyyy-MM-dd\"">>, H)),
    ?assert(has(<<"<input class=\"ah-dti-input\" type=\"text\" id=\"d-input\" readonly">>, H)),
    ?assert(has(<<"value=\"2026-09-29\"">>, H)),
    ?assert(has(<<"aria-controls=\"d-dropdown\"">>, H)),
    ?assert(has(<<"<div class=\"ah-dti-cal-btn\" data-action=\"toggle-dropdown\" "
                  "aria-hidden=\"true\">📅</div>"/utf8>>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"due\" value=\"2026-09-29\">">>, H)),
    ?assert(has(<<"<div class=\"ah-dti-dropdown\" id=\"d-dropdown\" role=\"dialog\" "
                  "aria-label=\"Choose date\" hidden></div>">>, H)),
    ?assert(has(<<"<span class=\"ah-dti-live\" aria-live=\"polite\" aria-atomic=\"true\">">>, H)),
    ?assert(has(<<"title=\"t\"">>, H)),
    ?assertEqual(1, count(<<"name=">>, H)).

datetime_input_formats_test() ->
    V = fun(Value, Format) ->
                H = r(?M:ah_datetime_input(Value, [], [{format, Format}])),
                {match, [Iso, Shown]} =
                    re:run(H, <<"data-ah-value=\"([^\"]*)\".* value=\"([^\"]*)\"">>,
                           [{capture, all_but_first, binary}]),
                {Iso, Shown}
        end,
    ?assertEqual({<<"2026-09-29T14:05">>, <<"2026-09-29 02:05 PM">>},
                 V(<<"2026-09-29T14:05:09">>, <<"yyyy-MM-dd hh:mm a">>)),
    ?assertEqual({<<"2026-09-29T09:05:30">>, <<"29/09/26 09:05:30">>},
                 V({{2026, 9, 29}, {9, 5, 30}}, <<"d/M/yy H:m:s">>)),
    ?assertEqual({<<"00:30">>, <<"12:30 AM">>}, V(<<"00:30">>, <<"hh:mm a">>)),
    ?assertEqual({<<"2026-09-01">>, <<"2026年09月01日"/utf8>>},
                 V({2026, 9, 1}, <<"yyyy年MM月dd日"/utf8>>)),
    ?assertEqual({<<>>, <<>>}, V(undefined, <<"yyyy-MM-dd">>)),
    %% a time alone has no calendar
    T = r(?M:ah_datetime_input(<<"10:00">>, [], [{format, <<"HH:mm">>}])),
    ?assertNot(has_quiet(<<"ah-dti-cal-btn">>, T)),
    ?assertNot(has_quiet(<<"ah-dti-dropdown">>, T)).

datetime_input_flags_test() ->
    H = r(?M:ah_datetime_input(undefined, [disabled, readonly, spinner, no_calendar, show_time,
                                           floating_label, no_rounded],
                               [{placeholder, <<"Birthday">>}, {min, {2026, 1, 1}},
                                {max, <<"2026-12-31T08:00">>}])),
    ?assert(has(<<"class=\"ah-dti-group ah-dti-disabled ah-dti-no-rounded ah-dti-readonly\"">>, H)),
    ?assert(has(<<"data-ah-min=\"2026-01-01\" data-ah-max=\"2026-12-31\"">>, H)),
    ?assert(has(<<"data-ah-show-time">>, H)),
    ?assert(has(<<"<div class=\"ah-dti-spinner\" aria-hidden=\"true\">">>, H)),
    ?assert(has(<<"aria-label=\"Increment\"">>, H)),
    ?assert(has(<<"<label class=\"ah-dti-label\" for=\"">>, H)),
    ?assert(has(<<">Birthday</label>">>, H)),
    ?assertNot(has_quiet(<<"placeholder=">>, H)),
    ?assert(has(<<" disabled">>, H)),
    ?assertNot(has_quiet(<<"ah-dti-cal-btn">>, H)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := datetime_input}] = ?M:catalog(),
    %% every option and flag is documented, every behaviour method listed
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
        aihtml_catalog:entry(?M, datetime_input),
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    ?assert(lists:member(setValue, [Name || #{name := Name} <- Ms])).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_datetime_input(<<"2026-09-29T10:00">>, [spinner, show_time, <<"w-60">>],
                                        [{id, d}, {name, at}, {format, <<"yyyy-MM-dd HH:mm">>},
                                         {placeholder, <<"P">>}, {min, <<"2026-01-01T00:00">>},
                                         {first_day, 1}, {labels, #{time => <<"T">>}}])),
                 r(#ah_datetime_input{value = <<"2026-09-29T10:00">>, spinner = true,
                                      show_time = true, css = [<<"w-60">>], id = d, name = at,
                                      format = <<"yyyy-MM-dd HH:mm">>, placeholder = <<"P">>,
                                      min = <<"2026-01-01T00:00">>, first_day = 1,
                                      labels = #{time => <<"T">>}})).

builder_fills_fields_test() ->
    D = ?M:ah_datetime_input(undefined, [no_rounded], [{format, <<"HH:mm">>}, {max, <<"18:00">>}]),
    ?assertMatch(#ah_datetime_input{no_rounded = true, format = <<"HH:mm">>, max = <<"18:00">>,
                                    id = undefined, attrs = []}, D).

generated_id_test() ->
    H = r(#ah_datetime_input{postback = changed}),
    {match, [Id]} = re:run(H, <<"^<div class=\"ah-dti-group\" id=\"(ah-cal[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(<<"aria-controls=\"", Id/binary, "-dropdown\"">>, H)),
    ?assertEqual(1, count(<<" id=\"", Id/binary, "\"">>, H)).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {other_mod, changed, #{}}},
                 Token(#ah_datetime_input{postback = changed, delegate = other_mod})).

field_validation_test() ->
    ?assertError({aihtml, {bad_date, <<"soon">>}}, r(#ah_datetime_input{value = <<"soon">>})),
    ?assertError({aihtml, {bad_datetime_format, <<"--">>}}, r(#ah_datetime_input{format = <<"--">>})),
    ?assertError({aihtml, {bad_datetime_label, now}},
                 r(#ah_datetime_input{labels = #{now => <<"x">>}})),
    ?assertError({aihtml, {modifier_in_css, datetime_input, spinner}},
                 r(#ah_datetime_input{css = [spinner]})).

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

default(ah_datetime_input) -> #ah_datetime_input{}.
