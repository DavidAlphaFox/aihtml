-module(aihtml_scrollview_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_scrollview.hrl").

-define(M, aihtml_scrollview).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([scrollview], [N || #{name := N} <- ?M:catalog()]).

catalog_entries_are_valid_test() ->
    [begin
         E = aihtml_catalog:entry(?M, N),
         ?assertMatch(#{category := layout, root := <<"ah-", _/binary>>}, E),
         ?assert(is_list(aihtml_catalog:classes(E, [])))
     end || #{name := N} <- ?M:catalog()].

catalog_docs_test() ->
    [begin
         ?assertMatch(#{option_docs := #{}, methods := [_ | _]}, E),
         Documented = maps:keys(maps:get(option_docs, E)),
         Opts = maps:get(options, E, []) ++ maps:get(flags, E, []) ++
             lists:append([Ms || {Ms, _} <- maps:values(maps:get(groups, E, #{}))]),
         ?assertEqual([], Opts -- Documented)
     end || E <- ?M:catalog()].

unknown_modifier_fails_test() ->
    ?assertError({aihtml, {unknown_modifier, scrollview, big, _}},
                 ?M:scrollview([], [big], [])).

%%%===================================================================
%%% scrollview
%%%===================================================================

scrollview_structure_test() ->
    H = r(?M:scrollview([<<"a">>, <<"b">>, <<"c">>], [<<"rounded">>],
                        [{id, sv}, {current_page, 1}, {name, page}, {height, 200},
                         {title, <<"t">>}])),
    ?assert(has(<<"<div class=\"ah-scrollview rounded\" id=\"sv\" data-ah=\"scrollview\" "
                  "data-ah-value=\"1\"">>, H)),
    ?assert(has(<<"role=\"region\" aria-roledescription=\"carousel\" aria-label=\"Carousel\" "
                  "tabindex=\"0\" style=\"height:200px;\"">>, H)),
    ?assert(has(<<"<div class=\"ah-scrollview-wrapper\" id=\"sv-pages\" "
                  "style=\"margin-left:-100%;\" aria-live=\"polite\">">>, H)),
    ?assertEqual(3, count(<<"class=\"ah-scrollview-page\"">>, H)),
    ?assertEqual(2, count(<<"aria-hidden=\"true\" inert>">>, H)),
    ?assert(has(<<"aria-label=\"2 / 3\">b</div>">>, H)),
    ?assertEqual(3, count(<<"<span class=\"ah-scrollview-button">>, H)),
    ?assert(has(<<"ah-scrollview-button ah-scrollview-button-active\" role=\"button\" "
                  "tabindex=\"-1\" aria-label=\"Page 2\" aria-current=\"true\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"page\" value=\"1\">">>, H)),
    ?assert(has(<<"title=\"t\"">>, H)),
    ?assertNot(has_quiet(<<"data-slide-show">>, H)).

scrollview_options_test() ->
    H = r(?M:scrollview([<<"a">>, <<"b">>], [disabled],
                        [{current_page, 9}, {show_buttons, false}, {slide_show, true},
                         {slide_duration, 2000}, {animation_duration, 500},
                         {move_threshold, 0.25}, {bounce, false}, {label, <<"Promo">>}])),
    ?assert(has(<<"class=\"ah-scrollview ah-scrollview-disabled\"">>, H)),
    ?assert(has(<<"data-ah-value=\"1\"">>, H)),                  % clamped
    ?assert(has(<<"style=\"margin-left:-100%;transition-duration:500ms;\"">>, H)),
    ?assert(has(<<"style=\"display:none\"">>, H)),
    ?assert(has(<<"tabindex=\"-1\" aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"aria-label=\"Promo\"">>, H)),
    ?assert(has(<<"data-slide-show data-slide-duration=\"2000\" data-duration=\"500\" "
                  "data-threshold=\"0.25\" data-bounce=\"false\"">>, H)),
    %% no aria-live while the pages turn by themselves
    ?assertNot(has_quiet(<<"aria-live">>, H)),
    ?assert(has(<<"data-ah-value=\"0\"">>, r(?M:scrollview([], [], [])))).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:scrollview([<<"a">>, <<"b">>], [disabled, <<"x">>],
                                 [{id, s}, {name, n}, {current_page, 1}, {height, 90},
                                  {bounce, false}, {title, <<"t">>}])),
                 r(#ah_scrollview{body = [<<"a">>, <<"b">>], disabled = true, css = [<<"x">>],
                                  id = s, name = n, current_page = 1, height = 90,
                                  bounce = false, attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    S = ?M:scrollview([<<"a">>], [disabled], [{slide_show, true}, {title, <<"t">>}]),
    ?assertMatch(#ah_scrollview{body = [<<"a">>], disabled = true, slide_show = true,
                                attrs = [{title, <<"t">>}]}, S),
    ?assertError({aihtml, {record_only_field, ah_scrollview, postback}},
                 ?M:scrollview([], [], [{postback, go}])).

generated_id_test() ->
    H = r(#ah_scrollview{body = [<<"a">>]}),
    {match, [Id]} = re:run(H, <<"^<div class=\"ah-scrollview\" id=\"(ah-scrollview-[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(<<"id=\"", Id/binary, "-pages\"">>, H)).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, paged, #{id => 7}}},
                 Token(#ah_scrollview{postback = {paged, #{id => 7}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, current_page, -1}}, r(#ah_scrollview{current_page = -1})),
    ?assertError({aihtml, {bad_option, move_threshold, 2}}, r(#ah_scrollview{move_threshold = 2})),
    ?assertError({aihtml, {bad_option, slide_show, yes}}, r(#ah_scrollview{slide_show = yes})),
    ?assertError({aihtml, {bad_option, body, <<"x">>}}, r(#ah_scrollview{body = <<"x">>})).

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

default(ah_scrollview) -> #ah_scrollview{}.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.
