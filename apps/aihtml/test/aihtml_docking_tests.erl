%% Tests for aihtml_docking.
-module(aihtml_docking_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_docking.hrl").

-define(M, aihtml_docking).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

%% The JSON in data-ah-value, decoded.
value(H) ->
    {match, [V]} = re:run(H, <<"data-ah-value=\"([^\"]*)\"">>, [{capture, all_but_first, binary}]),
    json:decode(unescape(V)).

unescape(B) ->
    lists:foldl(fun({F, T}, Acc) -> binary:replace(Acc, F, T, [global]) end, B,
                [{<<"&quot;">>, <<"\"">>}, {<<"&lt;">>, <<"<">>}, {<<"&gt;">>, <<">">>},
                 {<<"&#39;">>, <<"'">>}, {<<"&amp;">>, <<"&">>}]).

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

%%%===================================================================
%%% docking
%%%===================================================================

panels() ->
    [{a, [{a1, <<"A1">>, <<"one">>}, {a2, <<"A2">>, <<"two">>, #{collapsed => true}}]},
     {b, [{b1, <<"B1">>, <<"three">>, #{pinned => true}}]}].

docking_basic_test() ->
    H = r(?M:docking(panels(), [<<"h-80">>], [{id, dk}, {name, lay}, {title, <<"t">>}])),
    ?assert(has(<<"<div class=\"ah-docking ah-docking-horizontal h-80\" id=\"dk\" data-ah=\"docking\"">>, H)),
    ?assert(has(<<"data-panel-id=\"a\"">>, H)),
    ?assert(has(<<"class=\"ah-docking-window ah-docking-window-docked\" data-window-id=\"a1\" "
                  "role=\"region\" aria-labelledby=\"dk-w-a1-title\"">>, H)),
    ?assert(has(<<"ah-docking-window-docked ah-docking-window-collapsed\" data-window-id=\"a2\"">>, H)),
    ?assert(has(<<"ah-docking-window-docked ah-docking-window-pinned\" data-window-id=\"b1\"">>, H)),
    ?assert(has(<<"<div class=\"ah-docking-window-header\" tabindex=\"0\">">>, H)),
    ?assert(has(<<"aria-controls=\"dk-w-a2-content\" aria-expanded=\"false\"">>, H)),
    ?assert(has(<<"<div class=\"ah-docking-window-content\" id=\"dk-w-a1-content\">one</div>">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"lay\"">>, H)),
    ?assert(has(<<"title=\"t\"">>, H)),
    ?assertEqual(#{<<"panels">> => [#{<<"id">> => <<"a">>, <<"windows">> => [<<"a1">>, <<"a2">>]},
                                    #{<<"id">> => <<"b">>, <<"windows">> => [<<"b1">>]}],
                   <<"floating">> => [], <<"collapsed">> => [<<"a2">>], <<"closed">> => []},
                 value(H)).

docking_options_test() ->
    H = r(?M:docking(panels(), [vertical, disabled],
                     [{offset, 8}, {allow_float, false}, {close_buttons, false},
                      {drag_opacity, 0.5}, {labels, #{collapse => <<"Fold">>}}])),
    ?assert(has(<<"ah-docking ah-docking-vertical ah-docking-disabled">>, H)),
    ?assert(has(<<"style=\"--ah-docking-offset:8px\"">>, H)),
    ?assert(has(<<"data-ah-allow-float=\"false\"">>, H)),
    ?assert(has(<<"data-ah-drag-opacity=\"0.5\"">>, H)),
    ?assert(has(<<"aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"aria-label=\"Fold\"">>, H)),
    ?assertEqual(0, count(<<"ah-docking-window-close-btn">>, H)),
    ?assert(has(<<"id=\"ah-dk">>, H)).                     % an id is generated

docking_floating_test() ->
    H = r(?M:docking([{p, [{f, <<"F">>, <<"x">>, #{floating => {10, 20.4, 300}}},
                           {g, <<"G">>, <<"y">>, #{floating => {1, 2}}}]}], [], [])),
    ?assert(has(<<"ah-docking-window-floating\" data-window-id=\"f\" role=\"region\" "
                  "aria-labelledby=\"">>, H)),
    ?assert(has(<<"style=\"left:10px;top:20.4px;width:300px;\"">>, H)),
    ?assert(has(<<"style=\"left:1px;top:2px;\"">>, H)),
    ?assertMatch(#{<<"panels">> := [#{<<"windows">> := []}],
                   <<"floating">> := [#{<<"id">> := <<"f">>, <<"x">> := 10, <<"y">> := 20,
                                        <<"width">> := 300},
                                      #{<<"id">> := <<"g">>, <<"x">> := 1, <<"y">> := 2}]},
                 value(H)).

docking_layout_applied_test() ->
    Saved = <<"{\"panels\":[{\"id\":\"b\",\"windows\":[\"a2\",\"b1\",\"zz\"]},{\"id\":\"a\",\"windows\":[]}],"
              "\"floating\":[{\"id\":\"a1\",\"x\":5,\"y\":6,\"width\":200}],"
              "\"collapsed\":[],\"closed\":[\"nope\"]}">>,
    H = r(?M:docking(panels(), [], [{id, d}, {layout, Saved}])),
    %% panels keep the page's order, windows follow the layout, unknown ids go
    ?assertEqual(#{<<"panels">> => [#{<<"id">> => <<"a">>, <<"windows">> => []},
                                    #{<<"id">> => <<"b">>, <<"windows">> => [<<"a2">>, <<"b1">>]}],
                   <<"floating">> => [#{<<"id">> => <<"a1">>, <<"x">> => 5, <<"y">> => 6,
                                        <<"width">> => 200}],
                   <<"collapsed">> => [], <<"closed">> => []},
                 value(H)),
    %% the saved value decides collapsed for the windows it names
    ?assertEqual(0, count(<<"ah-docking-window-collapsed">>, H)),
    %% a closed window is not rendered; one the layout does not name stays home
    H2 = r(?M:docking(panels(), [], [{layout, #{<<"panels">> => [#{<<"id">> => <<"a">>,
                                                                   <<"windows">> => [<<"a2">>]}],
                                                <<"closed">> => [<<"a1">>]}}])),
    ?assertEqual(0, count(<<"data-window-id=\"a1\"">>, H2)),
    ?assertMatch(#{<<"panels">> := [#{<<"windows">> := [<<"a2">>]},
                                    #{<<"windows">> := [<<"b1">>]}],
                   <<"closed">> := [<<"a1">>]}, value(H2)),
    %% a2 is named, so its own collapsed option no longer applies
    ?assertEqual(0, count(<<"ah-docking-window-collapsed">>, H2)).

docking_errors_test() ->
    ?assertError({aihtml, {bad_docking_window, _}}, r(?M:docking([{a, [bad]}], [], []))),
    ?assertError({aihtml, {bad_docking_window_option, color}},
                 r(?M:docking([{a, [{w, <<"W">>, <<>>, #{color => red}}]}], [], []))),
    ?assertError({aihtml, {bad_docking_panel, _}}, r(?M:docking([x], [], []))),
    ?assertError({aihtml, {bad_dock_json, _}}, r(?M:docking([], [], [{layout, <<"{">>}]))),
    ?assertError({aihtml, {bad_option, offset, -1}}, r(?M:docking([], [], [{offset, -1}]))),
    ?assertError({aihtml, {bad_option, drag_opacity, 2}},
                 r(?M:docking([], [], [{drag_opacity, 2}]))),
    ?assertError({aihtml, {bad_docking_label, open}},
                 r(?M:docking([], [], [{labels, #{open => <<"x">>}}]))).

docking_add_window_test() ->
    Ops = aihtml_action:render_ops(
            fun(Ctx) -> ?M:docking_add_window(Ctx, {id, dk}, b, {nw, <<"New">>, <<"body">>}) end),
    [#{op := call, id := <<"dk">>, method := <<"addWindow">>, args := [<<"b">>, Html]}] = Ops,
    ?assert(has(<<"data-window-id=\"nw\"">>, Html)),
    ?assert(has(<<"id=\"dk-w-nw-title\"">>, Html)),
    ?assert(has(<<">body</div>">>, Html)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := docking}] = ?M:catalog(),
    E = aihtml_catalog:entry(?M, docking),
    #{flags := Flags} = E,
    [_ | _] = aihtml_catalog:classes(E, Flags),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()].

catalog_docs_test() ->
    [begin
         Docs = maps:get(option_docs, E, #{}),
         ?assertEqual(lists:sort(maps:get(options, E, []) ++ maps:get(flags, E, [])
                                 ++ maps:keys(maps:get(groups, E, #{}))),
                      lists:sort(maps:keys(Docs))),
         [?assert(is_binary(D) andalso D =/= <<>>) || D <- maps:values(Docs)],
         Ms = maps:get(methods, E),
         [#{name := _, args := <<"(", _/binary>>, doc := _} = X || X <- Ms],
         ?assert(lists:member(getValue, [Name || #{name := Name} <- Ms])),
         ?assertEqual(layout, maps:get(category, E))
     end || E <- ?M:catalog()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:docking(panels(), [vertical, disabled, <<"h-80">>],
                              [{id, d}, {name, n}, {offset, 3}, {allow_float, false},
                               {labels, #{close => <<"X">>}}, {title, <<"t">>}])),
                 r(#ah_docking{items = panels(), orientation = vertical, disabled = true,
                               css = [<<"h-80">>], id = d, name = n, offset = 3,
                               allow_float = false, labels = #{close => <<"X">>},
                               attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    D = ?M:docking(panels(), [vertical, <<"x">>], [{layout, <<"{}">>}, {drag_opacity, 0.5},
                                                   {collapse_buttons, false}, {title, <<"t">>}]),
    ?assertMatch(#ah_docking{items = [_, _], orientation = vertical, disabled = false,
                             layout = <<"{}">>, drag_opacity = 0.5, collapse_buttons = false,
                             close_buttons = true, css = [<<"x">>], attrs = [{title, <<"t">>}]}, D).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {?MODULE, saved, #{u => 1}}},
                 Token(#ah_docking{postback = {saved, #{u => 1}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, docking, disabled, yes}}, r(#ah_docking{disabled = yes})),
    ?assertError({aihtml, {bad_option, allow_float, 1}}, r(#ah_docking{allow_float = 1})).

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

default(ah_docking) -> #ah_docking{}.
