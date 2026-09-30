%% Tests for aihtml_swimlane.
-module(aihtml_swimlane_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_swimlane.hrl").

-define(M, aihtml_swimlane).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

%%%===================================================================
%%% swimlane
%%%===================================================================

lanes() -> [#{id => l1, name => <<"L1">>, color => blue}, #{id => l2, name => <<"L2">>}].
phases() -> [#{id => p1, label => <<"P1">>}, #{id => p2, label => <<"P2">>}].

swimlane_test() ->
    Nodes = [#{id => a, lane => l1, phase => p1, label => <<"A">>, type => start},
             #{id => b, lane => l1, phase => p1, label => <<"B">>, variant => outline,
               color => <<"#ff0000">>},
             #{id => c, lane => l2, phase => p2, label => <<"C">>, type => decision, dimmed => true},
             #{id => d, lane => nowhere, phase => p2}],
    H = r(?M:ah_swimlane(Nodes, [legend, editable],
                         [{id, sw}, {lanes, lanes()}, {phases, phases()}, {selected, a},
                          {flows, [#{from => a, to => c, label => <<"go">>},
                                   #{from => b, to => c, dashed => true, arrow => false},
                                   #{from => a, to => d}]}])),
    ?assert(has(<<"<div class=\"ah-swimlane\" id=\"sw\" data-ah=\"swimlane\" data-axis=\"discrete\"">>, H)),
    ?assert(has(<<"data-editable data-selected=\"a\"">>, H)),
    %% two nodes stacked in the first cell: (110 - 114) / 2 = -2 and 60
    ?assert(has(<<"style=\"left:29px;top:-2px;width:132px;height:52px;background-color:#3B82F6\"">>, H)),
    ?assert(has(<<"style=\"left:29px;top:60px;width:132px;height:52px;color:#ff0000\"">>, H)),
    ?assert(has(<<"data-type=\"start\" data-variant=\"solid\" data-state=\"selected\" tabindex=\"0\" "
                  "role=\"button\" aria-pressed=\"true\"">>, H)),
    ?assert(has(<<"data-dimmed=\"true\"">>, H)),
    %% lane without colour: the default grey
    ?assert(has(<<"background-color:#6B7280\"><span class=\"ah-swimlane-node__label\">C</span>">>, H)),
    ?assertNot(has_quiet(<<"data-id=\"d\"">>, H)),
    ?assert(has(<<"<marker id=\"sw-arrow-active\"">>, H)),
    ?assert(has(<<"<g data-from=\"a\" data-to=\"c\" data-active=\"true\">">>, H)),
    ?assert(has(<<"<g data-from=\"b\" data-to=\"c\" data-dim=\"true\">">>, H)),
    ?assert(has(<<"marker-end=\"url(#sw-arrow-active)\"">>, H)),
    ?assert(has(<<"data-dashed=\"true\">">>, H)),
    ?assert(has(<<"<text class=\"ah-swimlane-flows__label\"">>, H)),
    %% the flow to an unplaced node is not drawn
    ?assertEqual(2, count(<<"<g ">>, H)),
    ?assert(has(<<"<div class=\"ah-swimlane-legend\">">>, H)),
    ?assert(has(<<"<div class=\"ah-swimlane-grid__phase\" data-phase-id=\"p2\" style=\"width:190px\">P2</div>">>, H)).

swimlane_geometry_test() ->
    %% a forward elbow with rounded corners (sigil's flow-points, points->path)
    F = r(?M:ah_swimlane([#{id => a, lane => l1, phase => p1}, #{id => b, lane => l2, phase => p2}],
                         [], [{lanes, lanes()}, {phases, phases()}, {flows, [#{from => a, to => b}]}])),
    ?assert(has(<<"d=\"M161,55 L180,55 Q190,55 190,65 L190,155 Q190,165 200,165 L219,165\"">>, F)),
    H = r(?M:ah_swimlane([#{id => a, lane => l1, value => 0}, #{id => b, lane => l2, value => 10}],
                         [], [{lanes, lanes()}, {axis, continuous}, {axis_width, 400},
                              {flows, [#{from => a, to => b}]}])),
    ?assert(has(<<"left:0px;top:29px;">>, H)),
    ?assert(has(<<"left:268px;top:139px;">>, H)),
    ?assert(has(<<"<div class=\"ah-swimlane-grid__tick-label\" style=\"position:absolute;left:66px;"
                  "transform:translateX(-50%)\">0</div>">>, H)),
    ?assertError({aihtml, {bad_option, axis, round}}, r(?M:ah_swimlane([], [], [{axis, round}]))),
    ?assertError({aihtml, {bad_swimlane_node_type, box}},
                 r(?M:ah_swimlane([#{id => a, lane => l1, type => box}], [], []))),
    ?assertError({aihtml, {bad_color, magenta}},
                 r(?M:ah_swimlane([], [], [{lanes, [#{id => x, color => magenta}]}]))).

swimlane_update_test() ->
    S = ?M:ah_swimlane([], [], []),
    [#{op := html, id := <<"w">>, html := H}] =
        aihtml_action:render_ops(
          fun(Ctx) -> ?M:swimlane_update(Ctx, #{id => <<"w">>, data => #{<<"selected">> => <<"n">>}}, S) end),
    ?assert(has(<<"data-selected=\"n\"">>, H)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := swimlane}] = ?M:catalog(),
    ?assertEqual([{swimlane_update, 3}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()],
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} = E =
        aihtml_catalog:entry(?M, swimlane),
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    [_ | _] = aihtml_catalog:classes(E, Fl),
    [?assert(is_binary(D)) || #{doc := D} <- Ms].

catalog_docs_test() ->
    [begin
         ?assert(byte_size(maps:get(doc, E)) > 0),
         [?assert(byte_size(maps:get(K, maps:get(option_docs, E))) > 0)
          || K <- maps:get(options, E, []) ++ maps:get(flags, E, [])]
     end || E <- ?M:catalog()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_swimlane([#{id => a, lane => l1, phase => p1}], [legend],
                                  [{id, w}, {lanes, lanes()}, {phases, phases()}])),
                 r(#ah_swimlane{items = [#{id => a, lane => l1, phase => p1}], legend = true,
                                id = w, lanes = lanes(), phases = phases()})).

builder_fills_fields_test() ->
    ?assertError({aihtml, {record_only_field, ah_swimlane, postback}},
                 ?M:ah_swimlane([], [], [{postback, x}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z:-]+:[^\" ]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Tok | Rev] = lists:reverse(binary:split(T, <<":">>, [global])),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {iolist_to_binary(lists:join(<<":">>, lists:reverse(Rev))), Ref}
            end,
    ?assertEqual({<<"ah:node-change">>, {?MODULE, moved, #{}}},
                 Token(#ah_swimlane{postback = moved})).

field_validation_test() ->
    ?assertError({aihtml, {modifier_in_css, swimlane, legend}}, r(#ah_swimlane{css = [legend]})).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(default(Tag))))),
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].

default(ah_swimlane) -> #ah_swimlane{}.

generated_id_test() ->
    H = r(#ah_swimlane{flows = []}),
    ?assertMatch({match, _}, re:run(H, <<"^<div class=\"ah-swimlane\" id=\"ah-s[0-9]+\"">>)).
