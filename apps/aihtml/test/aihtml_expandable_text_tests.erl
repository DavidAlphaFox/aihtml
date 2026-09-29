-module(aihtml_expandable_text_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_expandable_text.hrl").

-define(D, aihtml_expandable_text).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Html) ->
    binary:match(r(Html), Needle) =/= nomatch.

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := expandable_text, category := C}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, expandable_text, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs)) || K <- maps:get(options, E, [])],
    [?assert(is_binary(D)) || D <- maps:values(OD)],
    [?assertMatch(#{name := _, args := _, doc := _}, M) || M <- Ms].

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_expandable_text),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_expandable_text{})))),
    [?assertEqual({N, G, case D of none -> undefined; _ -> D end},
                  {N, G, maps:get(G, Defaults)})
     || {G, {_, D}} <- maps:to_list(maps:get(groups, E, #{}))],
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    % the record renders with its defaults
    ?assert(is_binary(r(#ah_expandable_text{}))),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

expandable_text_test() ->
    Long = binary:copy(<<"é"/utf8>>, 12),
    H = ?D:expandable_text(Long, [], [{threshold, 10}]),
    ?assert(has(<<"data-expanded=\"false\" data-truncated=\"true\"">>, H)),
    ?assert(has(<<"<span data-ah-part=\"short\">", (binary:copy(<<"é"/utf8>>, 10))/binary, "…"/utf8,
                  "</span><span data-ah-part=\"full\" hidden>">>, H)),
    ?assert(has(<<"aria-expanded=\"false\"">>, H)),
    ?assert(has(<<">展开</button>"/utf8>>, H)),
    E = ?D:expandable_text(Long, [], [{threshold, 10}, {expanded, true}, {collapse_label, <<"less">>}]),
    ?assert(has(<<"<span data-ah-part=\"short\" hidden>">>, E)),
    ?assert(has(<<">less</button>">>, E)),
    S = ?D:expandable_text(<<"<b>short</b>">>, [], []),
    ?assert(has(<<"data-truncated=\"false\"">>, S)),
    ?assert(has(<<"&lt;b&gt;short&lt;/b&gt;">>, S)),
    ?assertNot(has(<<"<button">>, S)).

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
    ?assertMatch({<<"ah:toggle">>, _},
                 Token(#ah_expandable_text{text = <<"t">>, postback = t})).
