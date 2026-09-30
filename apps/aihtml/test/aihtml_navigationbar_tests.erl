%% Tests for aihtml_navigationbar. The module is also the fake action
%% module of the postback test.
-module(aihtml_navigationbar_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_navigationbar.hrl").

-define(M, aihtml_navigationbar).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

nav_items() ->
    [{<<"One">>, <<"First">>},
     {#{title => <<"Two">>, subheader => <<"sub">>, extra => <<"x">>}, <<"Second">>,
      [{actions, <<"Act">>}]},
     #{header => <<"Three">>, content => <<"Third">>, disabled => true}].

navigationbar_test() ->
    H = r(?M:ah_navigationbar(nav_items(), 0, [square], [{id, nb}, {name, open}])),
    ?assert(has(<<"<div class=\"ah-navigationbar ah-navigationbar-square ah-navigationbar-vertical "
                  "ah-navigationbar-animate-slide\" id=\"nb\" data-ah=\"navigationbar\" "
                  "data-ah-value=\"0\" data-expand-mode=\"single_fit_height\" "
                  "data-animation=\"slide\" data-toggle-mode=\"click\" "
                  "data-expand-duration=\"250\" data-collapse-duration=\"250\">">>, H)),
    %% hidden input first, so that :last-child is the last item
    ?assert(has(<<"data-collapse-duration=\"250\"><input type=\"hidden\" name=\"open\" "
                  "value=\"0\"><div class=\"ah-navigationbar-item\">">>, H)),
    ?assert(has(<<"<div class=\"ah-navigationbar-header ah-navigationbar-header-expanded\" "
                  "id=\"nb-item-0-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"true\" "
                  "aria-controls=\"nb-item-0-content\"><span class=\"ah-navigationbar-header-text\">One"
                  "</span><span class=\"ah-navigationbar-arrow ah-navigationbar-arrow-up\" "
                  "aria-hidden=\"true\">▼</span></div>"/utf8>>, H)),
    ?assert(has(<<"<div class=\"ah-navigationbar-body\" id=\"nb-item-0-content\" role=\"region\" "
                  "aria-labelledby=\"nb-item-0-header\"><div class=\"ah-navigationbar-content\">"
                  "First</div></div>">>, H)),
    ?assert(has(<<"id=\"nb-item-1-content\" role=\"region\" aria-labelledby=\"nb-item-1-header\" "
                  "style=\"display:none;\"">>, H)),
    ?assert(has(<<"<span class=\"ah-navigationbar-header-text ah-navigationbar-header-text-structured\">"
                  "<span class=\"ah-navigationbar-header-title\">Two</span>"
                  "<span class=\"ah-navigationbar-header-subheader\">sub</span>"
                  "<span class=\"ah-navigationbar-header-extra\">x</span></span>">>, H)),
    ?assert(has(<<"<div class=\"ah-navigationbar-actions\">Act</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-navigationbar-header ah-navigationbar-disabled\" "
                  "id=\"nb-item-2-header\" role=\"button\" tabindex=\"-1\" aria-expanded=\"false\" "
                  "aria-controls=\"nb-item-2-content\" aria-disabled=\"true\">">>, H)).

navigationbar_options_test() ->
    H = r(?M:ah_navigationbar(nav_items(), <<"0,1">>, [disable_gutters, no_arrow],
                              [{expand_mode, multiple}, {animation, none}, {toggle_mode, none},
                               {height, 300}, {width, <<"20rem">>}, {disabled, true}])),
    ?assert(has(<<"class=\"ah-navigationbar ah-navigationbar-no-gutters ah-navigationbar-vertical "
                  "ah-navigationbar-expand-multiple ah-navigationbar-disabled\"">>, H)),
    ?assert(has(<<"style=\"width:20rem;height:300px;\"">>, H)),
    ?assert(has(<<"data-ah-value=\"0,1\"">>, H)),
    ?assert(has(<<"aria-disabled=\"true\"">>, H)),
    ?assertNot(has_quiet(<<"ah-navigationbar-arrow">>, H)),
    ?assert(has(<<"ah-navigationbar-header-no-toggle">>, H)),
    %% only single_fit_height with a height fits the bodies
    ?assertNot(has_quiet(<<"data-fit">>, H)),
    ?assertEqual(3, count(<<"tabindex=\"-1\"">>, H)),
    F = r(?M:ah_navigationbar(nav_items(), [1], [], [{height, 300}])),
    ?assert(has(<<" data-fit">>, F)),
    D = r(?M:ah_navigationbar(nav_items(), undefined, [],
                              [{arrow_position, left}, {expand_icon, <<"+">>},
                               {collapse_icon, <<"-">>}])),
    ?assert(has(<<"<span class=\"ah-navigationbar-arrow ah-navigationbar-arrow-left "
                  "ah-navigationbar-arrow-dual\" aria-hidden=\"true\">"
                  "<span class=\"ah-navigationbar-icon ah-navigationbar-icon-expand\">+</span>"
                  "<span class=\"ah-navigationbar-icon ah-navigationbar-icon-collapse\">-</span>"
                  "</span>">>, D)),
    ?assert(has(<<"data-ah-value=\"\"">>, D)).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := navigationbar}] = ?M:catalog(),
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
        aihtml_catalog:entry(?M, navigationbar),
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    ?assertMatch([_ | _], Ms).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_navigationbar(nav_items(), [0, 2], [square, no_arrow],
                                       [{id, b}, {expand_mode, toggle}, {animation, fade},
                                        {expand_duration, 100}, {disabled, true}])),
                 r(#ah_navigationbar{items = nav_items(), value = [0, 2], square = true,
                                     no_arrow = true, id = b, expand_mode = toggle,
                                     animation = fade, expand_duration = 100,
                                     disabled = true})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z:]+:[^\":]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Tok | Rev] = lists:reverse(binary:split(T, <<":">>, [global])),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {iolist_to_binary(lists:join(<<":">>, lists:reverse(Rev))), Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, open, #{}}},
                 Token(#ah_navigationbar{postback = open})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, expand_mode, all}},
                 r(#ah_navigationbar{expand_mode = all})),
    ?assertError({aihtml, {bad_option, animation, spin}}, r(#ah_navigationbar{animation = spin})),
    ?assertError({aihtml, {bad_option, toggle_mode, hover}},
                 r(#ah_navigationbar{toggle_mode = hover})),
    ?assertError({aihtml, {bad_option, arrow_position, top}},
                 r(#ah_navigationbar{arrow_position = top})),
    ?assertError({aihtml, {bad_option, expand_duration, -1}},
                 r(#ah_navigationbar{expand_duration = -1})),
    ?assertError({aihtml, {bad_value, <<"a">>}}, r(#ah_navigationbar{value = <<"a">>})),
    ?assertError({aihtml, {bad_navigationbar_item, x}}, r(#ah_navigationbar{items = [x]})),
    ?assertError({aihtml, {bad_flag, navigationbar, square, yes}},
                 r(#ah_navigationbar{square = yes})).

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

default(ah_navigationbar) -> #ah_navigationbar{}.
