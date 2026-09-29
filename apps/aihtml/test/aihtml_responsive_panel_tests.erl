-module(aihtml_responsive_panel_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_responsive_panel.hrl").

-define(M, aihtml_responsive_panel).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

%%%===================================================================
%%% catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([responsive_panel], [N || #{name := N} <- ?M:catalog()]).

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

%%%===================================================================
%%% responsive_panel
%%%===================================================================

responsive_panel_test() ->
    H = r(?M:responsive_panel(<<"nav">>, [],
                              [{id, rp}, {breakpoint, 600}, {collapse_width, 240},
                               {height, 300}, {animation, slide}, {auto_close, false},
                               {toggle_button, <<"#menu">>}, {toggle_size, 36},
                               {toggle_label, <<"Menu">>}])),
    ?assert(has(<<"<div class=\"ah-responsive-panel\" id=\"rp\" data-ah=\"responsive-panel\" "
                  "data-breakpoint=\"600\" data-collapse-width=\"240px\" data-animation=\"slide\" "
                  "data-show-duration=\"200\" data-hide-duration=\"200\" data-auto-close=\"false\" "
                  "data-toggle-button=\"#menu\">">>, H)),
    ?assert(has(<<"<div class=\"ah-responsive-panel-toggle\" role=\"button\" tabindex=\"0\" "
                  "title=\"Menu\" aria-label=\"Menu\" aria-expanded=\"false\" "
                  "aria-controls=\"rp-content\" style=\"width:36px;height:36px;\">☰</div>"/utf8>>, H)),
    ?assert(has(<<"<div class=\"ah-responsive-panel-content\" id=\"rp-content\" "
                  "style=\"height:300px;\">nav</div>">>, H)),
    D = r(?M:responsive_panel([], [disabled], [{toggle_content, <<"=">>}])),
    ?assert(has(<<"class=\"ah-responsive-panel ah-responsive-panel-disabled\" id=\"ah-responsive-panel-">>, D)),
    ?assert(has(<<"aria-disabled=\"true\"">>, D)),
    ?assert(has(<<">=</div>">>, D)).

responsive_panel_load_test() ->
    H = r(#ah_responsive_panel{id = rp, load = {?MODULE, load, #{n => 1}}}),
    {match, [Tok]} = re:run(H, <<"id=\"rp-content\" data-ah-on=\"ah:load:([^\"]+)\"">>,
                            [{capture, all_but_first, binary}]),
    ?assertEqual({ok, {?MODULE, load, #{n => 1}}}, aihtml_action:unsign(Tok)).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:responsive_panel(<<"c">>, [], [{id, p}, {breakpoint, 10},
                                                    {animation, none}])),
                 r(#ah_responsive_panel{body = <<"c">>, id = p, breakpoint = 10,
                                        animation = none})).

generated_id_test() ->
    R = #ah_responsive_panel{},
    ?assertNotEqual(r(R), r(R)).

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_responsive_panel}},
                 r(#ah_responsive_panel{postback = go})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, animation, zoom}},
                 r(#ah_responsive_panel{animation = zoom})),
    ?assertError({aihtml, {bad_option, load, fun_ref}}, r(#ah_responsive_panel{load = fun_ref})),
    ?assertError({aihtml, {bad_length, 1.5}}, r(#ah_responsive_panel{height = 1.5})).

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

default(ah_responsive_panel) -> #ah_responsive_panel{}.

