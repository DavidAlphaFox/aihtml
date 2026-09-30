-module(aihtml_markdown_view_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_markdown_view.hrl").

-define(D, aihtml_markdown_view).

r(Html) -> aihtml_html:render_binary(Html).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_are_exported_test() ->
    [#{name := markdown_view, category := C, behavior := none}] = ?D:catalog(),
    ?assert(erlang:function_exported(?D, ah_markdown_view, 3)),
    ?assert(lists:member(C, [form, layout, overlay, data, media, text])).

api_docs_cover_options_and_flags_test() ->
    [#{option_docs := OD, methods := Ms} = E] = ?D:catalog(),
    Docs = maps:keys(OD),
    [?assert(lists:member(K, Docs))
     || K <- maps:get(options, E, []) ++ maps:get(flags, E, [])],
    ?assertEqual([], Ms).

records_match_catalog_test() ->
    [#{name := N} = E] = ?D:catalog(),
    Fields = ?D:fields(ah_markdown_view),
    ?assertEqual([module, id, css, attrs, postback, delegate], lists:sublist(Fields, 6)),
    Defaults = maps:from_list(lists:zip(Fields, tl(tuple_to_list(#ah_markdown_view{})))),
    [?assertEqual({N, F, false}, {N, F, maps:get(F, Defaults)})
     || F <- maps:get(flags, E, [])],
    [?assert(lists:member(O, Fields)) || O <- maps:get(options, E, [])],
    ?assertEqual(<<"<div class=\"ah-markdown-view\"></div>">>, r(#ah_markdown_view{})),
    ?assertEqual(?D, maps:get(module, Defaults)).

%%%===================================================================
%%% Rendering
%%%===================================================================

renders_markdown_test() ->
    ?assertEqual(<<"<div class=\"ah-markdown-view\"><h1>Title</h1>\n"
                   "<p>Some <strong>bold</strong> text.</p>\n</div>">>,
                 r(?D:ah_markdown_view(<<"# Title\n\nSome **bold** text.\n">>, [], []))).

css_and_attrs_go_to_the_root_test() ->
    ?assertEqual(<<"<div class=\"ah-markdown-view max-w-prose\" id=\"doc\" lang=\"en\">"
                   "<p>x</p>\n</div>">>,
                 r(?D:ah_markdown_view("x", [<<"max-w-prose">>], [{id, doc}, {lang, en}]))).

undefined_is_empty_test() ->
    ?assertEqual(<<"<div class=\"ah-markdown-view\"></div>">>,
                 r(?D:ah_markdown_view(undefined, [], []))).

html_is_escaped_test() ->
    Out = r(?D:ah_markdown_view(<<"<script>alert(1)</script>\n\n[x](javascript:alert(1))">>, [], [])),
    ?assertEqual(nomatch, binary:match(Out, [<<"<script">>, <<"href">>])),
    ?assertNotEqual(nomatch, binary:match(Out, <<"&lt;script&gt;">>)).

record_test() ->
    ?assertEqual(r(?D:ah_markdown_view(<<"- [x] done">>, [], [{id, t}])),
                 r(#ah_markdown_view{markdown = <<"- [x] done">>, id = t})).

unknown_modifier_test() ->
    ?assertError(_, r(?D:ah_markdown_view(<<"x">>, [nope], []))).

postback_test() ->
    ?assertError({aihtml, {no_postback_event, ah_markdown_view}},
                 r(#ah_markdown_view{postback = p})).
