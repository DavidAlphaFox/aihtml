-module(aihtml_progress_circle_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_progress_circle.hrl").

-define(D, aihtml_progress_circle).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := progress_circle, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, ah_progress_circle, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_progress_circle),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_progress_circle{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_progress_circle{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

progress_circle_geometry_test() ->
    C = 2 * math:pi() * 45,
    Offset = fun(H) ->
                     {match, [O]} = re:run(r(H), "stroke-dashoffset=\"([0-9.]+)\"",
                                           [{capture, all_but_first, binary}]),
                     binary_to_float(<<O/binary, (case binary:match(O, <<".">>) of
                                                      nomatch -> <<".0">>; _ -> <<>> end)/binary>>)
             end,
    ?assert(abs(Offset(?D:ah_progress_circle(0, [], [])) - C) < 0.001),
    ?assert(abs(Offset(?D:ah_progress_circle(25, [], [])) - 0.75 * C) < 0.001),
    ?assert(abs(Offset(?D:ah_progress_circle(100, [], []))) < 0.001),
    ?assert(abs(Offset(?D:ah_progress_circle(250, [], []))) < 0.001),
    H = ?D:ah_progress_circle(42.9, [lg, success], [{label, <<"Up<load>">>}]),
    ?assert(has(<<"viewBox=\"0 0 100 100\"">>, H)),
    ?assert(has(<<"cx=\"50\" cy=\"50\" r=\"45\"">>, H)),
    ?assert(has(<<"stroke-dasharray=\"282.7433\"">>, H)),
    ?assert(has(<<"class=\"ah-progress-circle ah-progress-circle--success ah-progress-circle--lg\"">>, H)),
    ?assert(has(<<">42%</span>">>, H)),
    ?assert(has(<<"aria-label=\"Up&lt;load&gt;\"">>, H)),
    ?assert(has(<<"<span class=\"ah-progress-circle-label\">Up&lt;load&gt;</span>">>, H)),
    ?assertNot(has(<<"ah-progress-circle-value">>, ?D:ah_progress_circle(5, [], [{show_value, false}]))),
    ?assert(has(<<"ah-progress-circle-disabled">>, ?D:ah_progress_circle(5, [disabled], []))).

%%%===================================================================
%%% element record (designs/05-records.md)
%%%===================================================================

postback_test() ->
    Token = fun(Html) ->
                    {match, [Ev, Tok]} = re:run(r(Html), <<"data-ah-on=\"((?:ah:)?[a-z-]+):([^\":]+)\"">>,
                                                [{capture, all_but_first, binary}]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {Ev, Ref}
            end,
    ?assertMatch({<<"change">>, _}, Token(#ah_progress_circle{value = 5, postback = p})).
