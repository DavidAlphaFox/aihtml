%% Tests for aihtml_chart: the markup, the data island, chart_option/1 and
%% chart_update/3, the record and the catalog.
-module(aihtml_chart_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_chart.hrl").
-include_lib("aihtml/include/aihtml_area_chart.hrl").
-include_lib("aihtml/include/aihtml_bar_chart.hrl").
-include_lib("aihtml/include/aihtml_relation_graph.hrl").

-define(M, aihtml_chart).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

%% The option in a rendered chart's data island, decoded.
island(Html) ->
    {match, [Json]} = re:run(r(Html), <<"<script class=\"ah-chart-data\" type=\"application/json\">"
                                        "(.*?)</script>">>,
                             [{capture, all_but_first, binary}, dotall]),
    json:decode(Json).

chart_markup_test() ->
    H = r(?M:chart(#{series => [#{type => line, data => [1, 2]}]}, [<<"mt-2">>],
                   [{id, c}, {height, 300}, {aria_label, <<"Sales">>}])),
    ?assert(has(<<"<div class=\"ah-chart mt-2\" role=\"img\" data-ah=\"chart\" "
                  "style=\"height:300px;\" id=\"c\" aria-label=\"Sales\">"
                  "<script class=\"ah-chart-data\" type=\"application/json\">">>, H)),
    ?assertEqual(#{<<"series">> => [#{<<"type">> => <<"line">>, <<"data">> => [1, 2]}]},
                 island(?M:chart(#{series => [#{type => line, data => [1, 2]}]}, [], []))).

chart_flags_test() ->
    H = r(?M:chart(#{}, [loading, disabled], [{width, <<"50%">>}, {height, 200},
                                               {renderer, svg}])),
    ?assert(has(<<"class=\"ah-chart ah-chart-disabled\"">>, H)),
    ?assert(has(<<"data-ah-renderer=\"svg\" data-ah-loading=\"true\" aria-disabled=\"true\" "
                  "style=\"width:50%;height:200px;\"">>, H)),
    ?assert(has(<<"<div class=\"ah-chart-overlay\"></div></div>">>, H)).

island_escaping_test() ->
    Opt = #{title => #{text => <<"</script><!-- x">>}},
    H = r(?M:chart(Opt, [], [])),
    ?assertEqual(nomatch, binary:match(H, <<"</script><!--">>)),
    ?assertEqual(1, length(binary:matches(H, <<"</script>">>))),
    %% and the data still round-trips
    ?assertEqual(#{<<"title">> => #{<<"text">> => <<"</script><!-- x">>}}, island(?M:chart(Opt, [], []))).

chart_option_test() ->
    ?assertEqual(#{a => 1}, ?M:chart_option(#ah_chart{option = #{a => 1}})),
    #{series := [#{type := bar}]} = ?M:chart_option(#ah_bar_chart{series = [{a, [1]}]}),
    #{series := [#{type := graph}]} = ?M:chart_option(#ah_relation_graph{graph = {[a], []}}),
    ?assertError({aihtml, {not_a_chart, x}}, ?M:chart_option(x)).

chart_update_test() ->
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:chart_update(Ctx, {id, <<"c">>}, #ah_area_chart{series = [{a, [1]}]}),
                    ?M:chart_update(Ctx, {id, <<"g">>}, #ah_relation_graph{graph = {[a], []}}),
                    ?M:chart_update(Ctx, <<"#x">>, #{series => [#{data => [2]}]})
            end),
    Text = iolist_to_binary(io_lib:format("~p", [Ops])),
    ?assert(has(<<"setOption">>, Text)),
    [#{op := call, id := <<"c">>, method := <<"setOption">>, args := [#{series := _}, false]},
     #{op := call, id := <<"g">>, args := [#{series := [#{type := graph}]}, true]},
     #{op := call, sel := <<"#x">>, args := [#{series := [#{data := [2]}]}, false]}] = Ops.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := chart}] = ?M:catalog(),
    ?assertEqual([{chart_option, 1}, {chart_update, 3}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()],
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
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    Opt = #{series => []},
    ?assertEqual(r(?M:chart(Opt, [loading, <<"x">>], [{id, c}, {height, 100}, {title, <<"t">>}])),
                 r(#ah_chart{option = Opt, loading = true, css = [<<"x">>], id = c, height = 100,
                             attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    ?assertError({aihtml, {record_only_field, ah_chart, postback}},
                 ?M:chart(#{}, [], [{postback, x}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"([a-z:-]+):([^\"]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertEqual({<<"ah:chart-click">>, {other, go, #{}}},
                 Token(#ah_chart{postback = go, delegate = other})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, option, x}}, r(#ah_chart{option = x})),
    ?assertError({aihtml, {bad_option, option, _}}, r(#ah_chart{option = #{a => {1, 2}}})),
    ?assertError({aihtml, {bad_option, renderer, webgl}}, r(#ah_chart{renderer = webgl})),
    ?assertError({aihtml, {bad_option, height, 0}}, r(#ah_chart{height = 0})),
    ?assertError({aihtml, {bad_option, width, <<"1px;color:red">>}},
                 r(#ah_chart{width = <<"1px;color:red">>})),
    ?assertError({aihtml, {bad_flag, chart, loading, yes}}, r(#ah_chart{loading = yes})).

records_match_catalog_test() ->
    Base = [module, id, css, attrs, postback, delegate],
    [begin
         Tag = list_to_atom("ah_" ++ atom_to_list(N)),
         Fields = ?M:fields(Tag),
         ?assertEqual(Base, lists:sublist(Fields, 6)),
         Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_chart{})))),
         [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                       {N, G, maps:get(G, Defaults)})
          || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
         [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
          || F <- maps:get(flags, E, [])],
         [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
         ?assertEqual(?M, maps:get(module, Defaults))
     end || #{name := N} = E <- ?M:catalog()].
