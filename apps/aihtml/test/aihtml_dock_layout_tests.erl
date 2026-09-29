%% Tests for aihtml_dock_layout.
-module(aihtml_dock_layout_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_dock_layout.hrl").

-define(M, aihtml_dock_layout).

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
%%% dock_layout
%%%===================================================================

dl_panels() ->
    [{e, <<"Explorer">>, <<"tree">>}, {s, <<"Search">>, <<"find">>},
     {m, <<"main.erl">>, <<"code">>}, {c, <<"Console">>, <<"$">>},
     {i, <<"Inspector">>, <<"dom">>}, {o, <<"Output">>, <<"log">>}].

ide() ->
    [{split, horizontal, [{tabs, [e, s], #{size => 22, id => left}},
                          {split, vertical, [{documents, [m]}, {tabs, [c], #{size => 35}}]}]},
     {float, [i], #{x => 10, y => 20}},
     {autohide, bottom, [o], #{size => 150}}].

dock_layout_structure_test() ->
    H = r(?M:dock_layout(ide(), [<<"x">>], [{id, dl}, {panels, dl_panels()}, {name, lay}])),
    ?assert(has(<<"<div class=\"ah-dl x\" id=\"dl\" data-ah=\"dock_layout\"">>, H)),
    ?assert(has(<<"<div class=\"ah-dl-inner\"><div class=\"ah-dl-group ah-dl-horizontal\" "
                  "style=\"flex:100 1 0px\">">>, H)),
    ?assert(has(<<"data-group-id=\"left\" data-allow-pin=\"true\" data-allow-close=\"true\" "
                  "data-pinned=\"true\" style=\"flex:22 1 0px\">">>, H)),
    ?assert(has(<<"<div class=\"ah-dl-group ah-dl-vertical\" style=\"flex:78 1 0px\">">>, H)),
    %% documents: no auto hide, not closable by default, the rest of the space
    ?assert(has(<<"<div class=\"ah-dl-tabbed ah-dl-document-group\" data-group-id=\"dl-g1\" "
                  "data-allow-pin=\"false\" data-allow-close=\"false\" data-pinned=\"true\" "
                  "data-document=\"true\" style=\"flex:65 1 0px\">">>, H)),
    %% tabs and panels, the first selected
    ?assert(has(<<"<li class=\"ah-tabs-item ah-tabs-item-selected\" id=\"dl-t-e\" role=\"tab\" "
                  "tabindex=\"0\" aria-selected=\"true\" aria-controls=\"dl-p-e\" "
                  "data-panel-id=\"e\">Explorer</li>">>, H)),
    ?assert(has(<<"id=\"dl-p-s\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"dl-t-s\" hidden>"
                  "<div class=\"ah-dl-panel-content\" data-panel-id=\"s\">find</div>">>, H)),
    %% splitbars between the children, oriented across the container
    ?assert(has(<<"<div class=\"ah-dl-splitbar\" role=\"separator\" tabindex=\"0\" "
                  "aria-orientation=\"vertical\"></div>">>, H)),
    ?assert(has(<<"aria-orientation=\"horizontal\"></div>">>, H)),
    ?assertEqual(2, count(<<"class=\"ah-dl-splitbar\"">>, H)),
    %% the float window, its group without a size
    ?assert(has(<<"<div class=\"ah-dl-float-window\" data-group-id=\"dl-g3\" role=\"dialog\" "
                  "aria-label=\"Inspector\" style=\"left:10px;top:20px;width:260px;height:180px;\">">>, H)),
    ?assert(has(<<"data-group-id=\"dl-g3-g\"">>, H)),
    %% the auto hidden group waits in its slot, with a tab on the strip
    ?assert(has(<<"<div class=\"ah-dl-autohide-preview-slot ah-dl-autohide-preview-slot-bottom\">"
                  "<div class=\"ah-dl-tabbed\" data-group-id=\"dl-g4\" data-allow-pin=\"true\" "
                  "data-allow-close=\"true\" data-pinned=\"false\" data-edge=\"bottom\" "
                  "data-size=\"150\">">>, H)),
    ?assert(has(<<"ah-dl-btn-pin ah-dl-unpinned\" aria-label=\"Auto Hide\" title=\"Auto Hide\" "
                  "aria-pressed=\"true\"">>, H)),
    ?assert(has(<<"<div class=\"ah-dl-autohide-strip ah-dl-autohide-strip-bottom\">"
                  "<div class=\"ah-dl-autohide-tab\" data-group-id=\"dl-g4\" role=\"button\" "
                  "tabindex=\"0\" aria-expanded=\"false\">Output</div></div>">>, H)),
    ?assert(has(<<"<div class=\"ah-dl-dock-zone\" data-zone=\"center\"></div>">>, H)),
    ?assert(has(<<"data-zone=\"edge-left\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"lay\"">>, H)),
    %% no stray newline from the templates
    ?assertEqual(nomatch, binary:match(H, <<"\n">>)).

dock_layout_value_test() ->
    H = r(?M:dock_layout(ide(), [], [{id, dl}, {panels, dl_panels()}])),
    ?assertEqual(
       [#{<<"type">> => <<"split">>, <<"orientation">> => <<"horizontal">>, <<"size">> => 100,
          <<"items">> =>
              [#{<<"type">> => <<"tabs">>, <<"id">> => <<"left">>, <<"size">> => 22,
                 <<"items">> => [<<"e">>, <<"s">>], <<"active">> => <<"e">>,
                 <<"pin">> => true, <<"close">> => true},
               #{<<"type">> => <<"split">>, <<"orientation">> => <<"vertical">>, <<"size">> => 78,
                 <<"items">> =>
                     [#{<<"type">> => <<"documents">>, <<"id">> => <<"dl-g1">>, <<"size">> => 65,
                        <<"items">> => [<<"m">>], <<"active">> => <<"m">>, <<"close">> => false},
                      #{<<"type">> => <<"tabs">>, <<"id">> => <<"dl-g2">>, <<"size">> => 35,
                        <<"items">> => [<<"c">>], <<"active">> => <<"c">>,
                        <<"pin">> => true, <<"close">> => true}]}]},
        #{<<"type">> => <<"float">>, <<"id">> => <<"dl-g3">>, <<"items">> => [<<"i">>],
          <<"active">> => <<"i">>, <<"x">> => 10, <<"y">> => 20, <<"width">> => 260,
          <<"height">> => 180},
        #{<<"type">> => <<"autohide">>, <<"id">> => <<"dl-g4">>, <<"edge">> => <<"bottom">>,
          <<"size">> => 150, <<"items">> => [<<"o">>], <<"active">> => <<"o">>,
          <<"pin">> => true, <<"close">> => true}],
       value(H)).

%% What the browser saves renders the same layout again.
dock_layout_round_trip_test() ->
    H1 = r(?M:dock_layout(ide(), [], [{id, dl}, {panels, dl_panels()}])),
    Json = iolist_to_binary(json:encode(value(H1))),
    H2 = r(?M:dock_layout(Json, [], [{id, dl}, {panels, dl_panels()}])),
    ?assertEqual(H1, H2),
    %% the decoded map works too
    ?assertEqual(H1, r(?M:dock_layout(json:decode(Json), [], [{id, dl}, {panels, dl_panels()}]))).

dock_layout_resolve_test() ->
    %% unknown ids are skipped, a panel is shown once, empty groups go and a
    %% split of one child is replaced by it
    L = {split, vertical, [{tabs, [nope, e, e]}, {tabs, [gone]}, {documents, [m], #{active => zz}}]},
    H = r(?M:dock_layout(L, [], [{id, x}, {panels, dl_panels()}])),
    ?assertMatch([#{<<"type">> := <<"split">>, <<"orientation">> := <<"vertical">>,
                    <<"items">> := [#{<<"items">> := [<<"e">>], <<"size">> := 50},
                                    #{<<"type">> := <<"documents">>, <<"active">> := <<"m">>,
                                      <<"size">> := 50}]}],
                 value(H)),
    H2 = r(?M:dock_layout({split, horizontal, [{tabs, [gone]}, {tabs, [c], #{size => 30}}]},
                          [], [{id, y}, {panels, dl_panels()}])),
    ?assertMatch([#{<<"type">> := <<"tabs">>, <<"items">> := [<<"c">>], <<"size">> := 100}],
                 value(H2)),
    %% sizes: given ones kept, the rest share what is left, all add up to 100
    H3 = r(?M:dock_layout({split, horizontal, [{tabs, [e], #{size => <<"20%">>}}, {tabs, [s]},
                                               {tabs, [c]}]},
                          [], [{id, z}, {panels, dl_panels()}])),
    [#{<<"items">> := Kids}] = value(H3),
    ?assertEqual([20, 40, 40], [S || #{<<"size">> := S} <- Kids]),
    %% inline panels and a fixed panel
    H4 = r(?M:dock_layout([{panel, {p, <<"Fixed">>, <<"body">>}, #{size => 30}},
                           {tabs, [{q, <<"Q">>, <<"qq">>}]}], [], [{id, w}])),
    ?assert(has(<<"<div class=\"ah-dl-panel\" data-panel-id=\"p\" style=\"flex:30 1 0px\">"
                  "<div class=\"ah-dl-panel-header\">Fixed</div>"
                  "<div class=\"ah-dl-panel-body\">body</div></div>">>, H4)),
    ?assertMatch([#{<<"type">> := <<"panel">>, <<"item">> := <<"p">>, <<"size">> := 30},
                  #{<<"type">> := <<"tabs">>, <<"size">> := 70}], value(H4)),
    %% an empty layout
    H5 = r(?M:dock_layout([], [], [])),
    ?assert(has(<<"<div class=\"ah-dl-inner\"></div>">>, H5)),
    ?assertEqual([], value(H5)).

dock_layout_options_test() ->
    H = r(?M:dock_layout({tabs, [e]}, [disabled],
                         [{panels, dl_panels()}, {resizable, false}, {resize_mode, feedback},
                          {allow_float, false}, {allow_dock, false}, {min_size, 50},
                          {labels, #{close => <<"Schließen"/utf8>>}}])),
    ?assert(has(<<"class=\"ah-dl ah-dl-disabled\"">>, H)),
    ?assert(has(<<"data-ah-resizable=\"false\"">>, H)),
    ?assert(has(<<"data-ah-resize-mode=\"feedback\"">>, H)),
    ?assert(has(<<"data-ah-allow-float=\"false\"">>, H)),
    ?assert(has(<<"data-ah-allow-dock=\"false\"">>, H)),
    ?assert(has(<<"data-ah-min-size=\"50\"">>, H)),
    ?assert(has(<<"aria-label=\"Schließen\""/utf8>>, H)),
    ?assert(has(<<"aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"id=\"ah-dl">>, H)),
    %% defaults stay out of the markup
    D = r(?M:dock_layout({tabs, [e]}, [], [{panels, dl_panels()}])),
    ?assertEqual(nomatch, binary:match(D, <<"data-ah-resizable">>)),
    ?assertEqual(nomatch, binary:match(D, <<"data-ah-resize-mode">>)).

dock_layout_errors_test() ->
    P = [{panels, dl_panels()}],
    ?assertError({aihtml, {bad_dock_node, _}}, r(?M:dock_layout({grid, [e]}, [], P))),
    ?assertError({aihtml, {bad_dock_node, diagonal}},
                 r(?M:dock_layout({split, diagonal, []}, [], P))),
    ?assertError({aihtml, {bad_dock_node, <<"middle">>}},
                 r(?M:dock_layout(<<"[{\"type\":\"autohide\",\"edge\":\"middle\",\"items\":[]}]">>,
                                  [], P))),
    ?assertError({aihtml, {bad_dock_node, {size, <<"wide">>}}},
                 r(?M:dock_layout({tabs, [e], #{size => <<"wide">>}}, [], P))),
    ?assertError({aihtml, {bad_dock_node, {colour, red}}},
                 r(?M:dock_layout({tabs, [e], #{colour => red}}, [], P))),
    ?assertError({aihtml, {bad_dock_json, _}}, r(?M:dock_layout(<<"[">>, [], P))),
    ?assertError({aihtml, {bad_dock_panel, _}}, r(?M:dock_layout([], [], [{panels, [x]}]))),
    ?assertError({aihtml, {bad_option, resize_mode, slow}},
                 r(?M:dock_layout([], [], [{resize_mode, slow}]))),
    ?assertError({aihtml, {bad_option, min_size, -3}}, r(?M:dock_layout([], [], [{min_size, -3}]))),
    ?assertError({aihtml, {bad_dock_layout_label, pin}},
                 r(?M:dock_layout([], [], [{labels, #{pin => <<"x">>}}]))).

dock_layout_open_test() ->
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:dock_layout_open(Ctx, {id, dl}, {c, <<"Console">>, <<"<$>">>}),
                    ?M:dock_layout_open(Ctx, {id, dl}, {i, <<"I">>, <<>>},
                                        #{float => {10.4, 20}, in => grp}),
                    ?M:dock_layout_open(Ctx, {id, <<"dl">>}, #{id => o, title => <<"O">>},
                                        #{edge => left})
            end),
    [#{op := call, id := <<"dl">>, method := <<"openPanel">>, args := [H1, W1]},
     #{args := [_, W2]}, #{args := [_, W3]}] = Ops,
    ?assert(has(<<"<div class=\"ah-dl-tabbed\" data-group-id=\"dl-o">>, H1)),
    ?assert(has(<<"id=\"dl-t-c\"">>, H1)),
    ?assert(has(<<"&lt;$&gt;">>, H1)),
    ?assertEqual(#{}, W1),
    ?assertEqual(#{float => true, x => 10, y => 20, in => <<"grp">>}, W2),
    ?assertEqual(#{edge => <<"left">>}, W3),
    ?assertError({aihtml, {bad_option, edge, middle}},
                 aihtml_action:render_ops(
                   fun(Ctx) -> ?M:dock_layout_open(Ctx, {id, dl}, {c, <<"C">>, <<>>},
                                                   #{edge => middle}) end)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := dock_layout}] = ?M:catalog(),
    E = aihtml_catalog:entry(?M, dock_layout),
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
    ?assertEqual(r(?M:dock_layout(ide(), [disabled], [{id, dl}, {panels, dl_panels()},
                                                     {resize_mode, feedback}, {min_size, 60}])),
                 r(#ah_dock_layout{layout = ide(), disabled = true, id = dl, panels = dl_panels(),
                                   resize_mode = feedback, min_size = 60})).

builder_fills_fields_test() ->
    L = ?M:dock_layout({tabs, [e]}, [], [{panels, []}, {allow_dock, false}, {id, k}]),
    ?assertMatch(#ah_dock_layout{layout = {tabs, [e]}, panels = [], allow_dock = false,
                                 allow_float = true, id = k, attrs = []}, L),
    ?assertError({aihtml, {record_only_field, ah_dock_layout, postback}},
                 ?M:dock_layout([], [], [{postback, x}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ev, Tok] = binary:split(T, <<":">>),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"change">>, {other, saved, #{}}},
                 Token(#ah_dock_layout{postback = saved, delegate = other})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, resizable, no}}, r(#ah_dock_layout{resizable = no})),
    ?assertError({aihtml, {unknown_modifier, dock_layout, big, _}}, ?M:dock_layout([], [big], [])).

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

default(ah_dock_layout) -> #ah_dock_layout{}.
