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
    ?assert(has(<<"<div class=\"ah-chart mt-2\" role=\"figure\" data-ah=\"chart\" "
                  "aria-describedby=\"c-data\" style=\"height:300px;\" id=\"c\" "
                  "aria-label=\"Sales\">"
                  "<script class=\"ah-chart-data\" type=\"application/json\">">>, H)),
    %% no table for a line without a category axis: the caption alone
    ?assert(has(<<"</script><p class=\"ah-chart-text ah-sr-only\" id=\"c-data\">Sales</p></div>">>, H)),
    ?assertEqual(#{<<"series">> => [#{<<"type">> => <<"line">>, <<"data">> => [1, 2]}]},
                 island(?M:chart(#{series => [#{type => line, data => [1, 2]}]}, [], []))).

chart_flags_test() ->
    H = r(?M:chart(#{}, [loading, disabled], [{width, <<"50%">>}, {height, 200},
                                               {renderer, svg}])),
    ?assert(has(<<"class=\"ah-chart ah-chart-disabled\"">>, H)),
    ?assert(has(<<"data-ah-renderer=\"svg\" data-ah-loading=\"true\" aria-disabled=\"true\" "
                  "style=\"width:50%;height:200px;\"">>, H)),
    ?assert(has(<<"<div class=\"ah-chart-overlay\"></div></div>">>, H)),
    %% nothing to read: no node, no aria-describedby
    ?assertEqual(nomatch, binary:match(H, [<<"ah-chart-text">>, <<"aria-describedby">>])).

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
    %% a record brings the server's readable data table (no id: the
    %% browser keeps the current one's); a map has the browser rebuild it
    [#{op := call, id := <<"c">>, method := <<"setOption">>,
       args := [#{series := _}, false, TextC]},
     #{op := call, id := <<"g">>, args := [#{series := [#{type := graph}]}, true, TextG]},
     #{op := call, sel := <<"#x">>, args := [#{series := [#{data := [2]}]}, false]}] = Ops,
    ?assertEqual(<<"<div class=\"ah-chart-text ah-sr-only\"><table><thead><tr>"
                   "<th scope=\"col\">Category</th><th scope=\"col\">a</th></tr></thead>"
                   "<tbody><tr><th scope=\"row\">1</th><td>1</td></tr></tbody></table></div>">>,
                 TextC),
    ?assert(has(<<"<th scope=\"row\">a</th><td></td></tr>">>, TextG)),
    %% nothing to read: an empty string (the browser keeps the old caption)
    [#{args := [_, false, <<>>]}] =
        aihtml_action:render_ops(fun(Ctx) -> ?M:chart_update(Ctx, {id, <<"c">>}, #ah_chart{}) end).

%%%===================================================================
%%% Readable data (aihtml_lib_chart:data_text/2)
%%%===================================================================

%% {Caption, Head, Rows} of the readable data table of an option, none
%% without a table ({caption, Text} for a caption alone).
tbl(Opt) -> tbl(Opt, []).
tbl(Opt, Attrs) ->
    H = r(?M:chart(Opt, [], [{id, c} | Attrs])),
    case re:run(H, <<"<div class=\"ah-chart-text ah-sr-only\" id=\"c-data\"><table>(.*?)</table></div>">>,
                [{capture, all_but_first, binary}, dotall]) of
        {match, [T]} ->
            Cap = case re:run(T, <<"<caption>(.*?)</caption>">>,
                              [{capture, all_but_first, binary}]) of
                      {match, [C]} -> C;
                      nomatch -> undefined
                  end,
            [Head | Rows] = [[C || [C] <- all_cells(Tr)] || [Tr] <- all_rows(T)],
            {Cap, Head, Rows};
        nomatch ->
            case re:run(H, <<"<p class=\"ah-chart-text ah-sr-only\" id=\"c-data\">(.*?)</p>">>,
                        [{capture, all_but_first, binary}]) of
                {match, [C]} -> {caption, C};
                nomatch -> none
            end
    end.

all_rows(T) ->
    case re:run(T, <<"<tr>(.*?)</tr>">>, [global, {capture, all_but_first, binary}]) of
        {match, L} -> L;
        nomatch -> []
    end.

all_cells(Tr) ->
    case re:run(Tr, <<"<t[hd][^>]*>(.*?)</t[hd]>">>, [global, {capture, all_but_first, binary}]) of
        {match, L} -> L;
        nomatch -> []
    end.

text_axis_test() ->
    Opt = #{title => #{text => <<"Sales">>},
            xAxis => #{type => category, data => [q1, <<"Q2">>, 3]},
            yAxis => #{type => value},
            series => [#{name => <<"Web">>, type => bar, data => [1, 2.5, null]},
                       #{type => line, data => [#{value => 4.0}, <<"-">>, 6, 7]}]},
    ?assertEqual({<<"Sales">>, [<<"Category">>, <<"Web">>, <<"Series 2">>],
                  [[<<"q1">>, <<"1">>, <<"4">>], [<<"Q2">>, <<"2.5">>, <<>>],
                   [<<"3">>, <<>>, <<"6">>], [<<"4">>, <<>>, <<"7">>]]},
                 tbl(Opt)),
    %% categories on the y axis (a horizontal bar chart), named
    ?assertEqual({undefined, [<<"Region">>, <<"s">>], [[<<"N">>, <<"5">>]]},
                 tbl(#{xAxis => #{type => value},
                       yAxis => #{type => category, name => <<"Region">>, data => [<<"N">>]},
                       series => [#{name => s, type => bar, data => [5]}]})),
    %% the title wins over the aria-label, which is the fallback
    {<<"Sales">>, _, _} = tbl(Opt, [{aria_label, <<"L">>}]),
    {<<"L">>, _, _} = tbl(maps:remove(title, Opt), [{aria_label, <<"L">>}]),
    %% binary keys, as from JSON
    ?assertEqual({undefined, [<<"Category">>, <<"Series 1">>], [[<<"a">>, <<"1">>]]},
                 tbl(#{<<"xAxis">> => #{<<"data">> => [<<"a">>]},
                       <<"series">> => [#{<<"type">> => <<"bar">>, <<"data">> => [1]}]})).

text_points_and_heatmap_test() ->
    ?assertEqual({undefined, [<<"cm">>, <<"Y">>], [[<<"160">>, <<"50">>], [<<"170">>, <<"65">>]]},
                 tbl(#{xAxis => #{type => value, name => <<"cm">>}, yAxis => #{type => value},
                       series => [#{type => scatter, data => [[160, 50], #{value => [170, 65]}]}]})),
    ?assertEqual({undefined, [<<"Day">>, <<"9">>, <<"10">>],
                  [[<<"Mon">>, <<"1">>, <<>>], [<<"Tue">>, <<"3">>, <<"4">>]]},
                 tbl(#{xAxis => #{type => category, data => [<<"9">>, <<"10">>]},
                       yAxis => #{type => category, name => <<"Day">>,
                                  data => [<<"Mon">>, <<"Tue">>]},
                       series => [#{type => heatmap,
                                    data => [[0, 0, 1], [0, 1, 3], [<<"10">>, <<"Tue">>, 4]]}]})).

text_pie_radar_dataset_test() ->
    ?assertEqual({undefined, [<<"Name">>, <<"Value">>, <<"Share">>],
                  [[<<"a">>, <<"1">>, <<"25.0%">>], [<<"b">>, <<"3">>, <<"75.0%">>]]},
                 tbl(#{series => [#{type => pie, data => [#{name => a, value => 1},
                                                          #{name => <<"b">>, value => 3}]}]})),
    {undefined, [<<"Series">>, <<"Name">>, <<"Value">>, <<"Share">>], [_, _]} =
        tbl(#{series => [#{type => pie, name => p, data => [#{name => a, value => 1}]},
                         #{type => funnel, data => [#{name => b, value => 2}]}]}),
    ?assertEqual({undefined, [<<"Indicator">>, <<"A">>, <<"Series 2">>],
                  [[<<"x">>, <<"1">>, <<"3">>], [<<"y">>, <<"2">>, <<>>]]},
                 tbl(#{radar => #{indicator => [#{name => x}, #{name => <<"y">>}]},
                       series => [#{type => radar, data => [#{name => <<"A">>, value => [1, 2]},
                                                            #{value => [3]}]}]})),
    ?assertEqual({undefined, [<<"p">>, <<"n">>], [[<<"a">>, <<"1">>]]},
                 tbl(#{dataset => #{source => [[p, n], [a, 1]]},
                       series => [#{type => bar}]})),
    %% rows of maps: the keys sorted (or the dimensions)
    ?assertEqual({undefined, [<<"n">>, <<"p">>], [[<<"1">>, <<"a">>]]},
                 tbl(#{dataset => [#{source => [#{p => a, n => 1}]}]})),
    ?assertEqual({undefined, [<<"p">>], [[<<"a">>]]},
                 tbl(#{dataset => #{dimensions => [#{name => p}], source => [#{p => a, n => 1}]}})).

text_graph_and_tree_test() ->
    ?assertEqual({undefined, [<<"Node">>, <<"Category">>, <<"Links to">>],
                  [[<<"Ann">>, <<"x">>, <<"Bob (knows), zed">>], [<<"Bob">>, <<>>, <<"Ann">>]]},
                 tbl(#{series => [#{type => graph, categories => [#{name => x}],
                                    data => [#{id => a, name => <<"Ann">>, category => 0},
                                             #{name => <<"Bob">>}],
                                    links => [#{source => a, target => <<"Bob">>, value => knows},
                                              #{source => 0, target => zed},
                                              #{source => 1, target => 0}]}]})),
    ?assertEqual({undefined, [<<"Node">>, <<"Children">>],
                  [[<<"r">>, <<"a, b">>], [<<"a">>, <<>>], [<<"b">>, <<"c">>], [<<"c">>, <<>>]]},
                 tbl(#{series => [#{type => tree,
                                    data => [#{name => r, children =>
                                                   [#{name => a},
                                                    #{name => b, children => [#{name => c}]}]}]}]})).

text_free_form_test() ->
    %% a shape without a table: the caption alone, or nothing
    Free = #{series => [#{type => sankey, data => [#{name => a}]}]},
    ?assertEqual({caption, <<"Flow">>}, tbl(Free#{title => [#{text => <<>>}, #{text => <<"Flow">>}]})),
    ?assertEqual(none, tbl(Free)),
    ?assertEqual(none, tbl(#{xAxis => #{type => category, data => [a]},
                             series => [#{type => bar, data => [[1, 2, 3], #{x => 1}]},
                                        #{type => line, data => [#{value => #{}}]}]})),
    %% text is escaped
    {<<"&lt;b&gt;">>, [_, <<"&lt;s&gt;">>], [[<<"&lt;i&gt;">>, _]]} =
        tbl(#{title => #{text => <<"<b>">>}, xAxis => #{data => [<<"<i>">>]},
              series => [#{name => <<"<s>">>, type => bar, data => [1]}]}).

text_ids_and_limit_test() ->
    %% without a root id: a generated one, the same on both ends
    H = r(?M:chart(#{title => #{text => t}}, [], [])),
    {match, [Id, Id]} = re:run(H, <<"aria-describedby=\"(ah-chart-text-[0-9]+)\".*id=\"(ah-chart-text-[0-9]+)\"">>,
                               [{capture, all_but_first, binary}]),
    %% a user's aria-describedby wins
    ?assert(has(<<"aria-describedby=\"mine\"">>,
                r(?M:chart(#{title => #{text => t}}, [], [{aria_describedby, mine}])))),
    %% at most 500 rows, then a line that says how many are left out
    {_, _, Rows} = tbl(#{xAxis => #{type => category},
                         series => [#{type => bar, data => lists:seq(1, 503)}]}),
    ?assertEqual(501, length(Rows)),
    ?assertEqual([<<"And 3 more rows.">>], lists:last(Rows)),
    ?assert(has(<<"<td colspan=\"2\">And 3 more rows.</td>">>,
                r(?M:chart(#{xAxis => #{type => category},
                             series => [#{type => bar, data => lists:seq(1, 503)}]}, [], [])))).

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
