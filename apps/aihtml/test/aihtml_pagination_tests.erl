-module(aihtml_pagination_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_pagination.hrl").

-export([action/4]).

-define(M, aihtml_pagination).

r(H) -> aihtml_html:render_binary(H).

has(Bin, Part) -> binary:match(Bin, Part) =/= nomatch.

count(Bin, Part) -> length(binary:matches(Bin, Part)).

%%% catalog

catalog_matches_exports_test() ->
    Exports = ?M:module_info(exports),
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([pagination], Names),
    [?assert(lists:keymember(aihtml_catalog:builder(N), 1, Exports)) || N <- Names],
    [?assertMatch(#{category := layout, root := <<"ah-", _/binary>>, signature := _}, E)
     || E <- ?M:catalog()].

catalog_documents_every_option_test() ->
    [begin
         Docs = maps:get(option_docs, E),
         ?assertEqual({N, []}, {N, [K || K <- maps:get(options, E, []) ++ maps:get(flags, E, []),
                                         not maps:is_key(K, Docs)]}),
         ?assert(is_list(maps:get(methods, E)))
     end || #{name := N} = E <- ?M:catalog()].

%%% pagination

visible_pages_test() ->
    ?assertEqual([1, 2, 3, 4, 5], ?M:visible_pages(3, 5, 7)),
    ?assertEqual([1, 2, 3, 4, 5, gap, 20], ?M:visible_pages(4, 20, 7)),
    ?assertEqual([1, gap, 4, 5, 6, gap, 20], ?M:visible_pages(5, 20, 7)),
    ?assertEqual([1, gap, 16, 17, 18, 19, 20], ?M:visible_pages(17, 20, 7)),
    ?assertEqual([1, gap, 8, 9, 10, 11, 12, gap, 30], ?M:visible_pages(10, 30, 9)),
    [?assert(length(?M:visible_pages(C, 50, 7)) =< 7) || C <- lists:seq(1, 50)],
    [?assert(lists:member(C, ?M:visible_pages(C, 50, 7))) || C <- lists:seq(1, 50)].

pagination_test() ->
    H = r(?M:ah_pagination(95, 3, [], [{name, page}])),
    ?assert(has(H, <<"data-ah=\"pagination\" data-ah-value=\"3\" data-total=\"95\" "
                     "data-page-size=\"10\" data-max-visible=\"7\"">>)),
    ?assert(has(H, <<"aria-current=\"page\">3</li>">>)),
    ?assert(has(H, <<"<option value=\"10\" selected>10 / page</option>">>)),
    ?assert(has(H, <<"<input type=\"hidden\" name=\"page\" value=\"3\">">>)),
    ?assertEqual(0, count(H, <<"ah-pagination-nav-disabled">>)).

pagination_clamps_and_options_test() ->
    H = r(?M:ah_pagination(40, 99, [], [{page_size, 20}, {show_first_last, true}, {show_total, true},
                                        {show_jumper, true}, {show_size_selector, false},
                                        {labels, #{total => <<"{0} rows">>}}])),
    ?assert(has(H, <<"data-ah-value=\"2\"">>)),
    ?assert(has(H, <<"40 rows">>)),
    ?assert(has(H, <<"data-type=\"last\"">>)),
    ?assertEqual(2, count(H, <<"ah-pagination-nav-disabled">>)),
    ?assert(has(H, <<"ah-pagination-jumper-input">>)),
    ?assertNot(has(H, <<"<select">>)).

pagination_simple_and_links_test() ->
    S = r(?M:ah_pagination(30, 2, [simple], [])),
    ?assert(has(S, <<"Page 2 / 3">>)),
    ?assertNot(has(S, <<"ah-pagination-item">>)),
    L = r(?M:ah_pagination(30, 1, [], [{href, <<"?p={page}&s={size}">>}, {show_size_selector, false}])),
    ?assert(has(L, <<"<a class=\"ah-pagination-item\" href=\"?p=2&amp;s=10\"">>)),
    ?assert(has(L, <<"<a class=\"ah-pagination-nav\" href=\"?p=2&amp;s=10\" data-type=\"next\"">>)),
    ?assert(has(L, <<"ah-pagination-nav ah-pagination-nav-disabled\" data-type=\"prev\"">>)).

%% Crawlable navigation: every enabled entry is a real link with the URL of
%% its page; a server binding keeps the links (the browser intercepts
%% them) and carries the template on the root for the pushed URL.
links_for_every_state_test() ->
    H = r(?M:ah_pagination(95, 3, [], [{href, <<"/list?page={page}&size={size}">>},
                                       {page_size, 20}, {show_first_last, true},
                                       aihtml:on(change, {?MODULE, page, #{}})])),
    Hrefs = [U || [U] <- element(2, re:run(H, <<"<a [^>]*href=\"([^\"]*)\"">>,
                                              [global, {capture, all_but_first, binary}]))],
    Url = fun(P) -> <<"/list?page=", (integer_to_binary(P))/binary, "&amp;size=20">> end,
    ?assertEqual([Url(P) || P <- [1, 2, 1, 2, 3, 4, 5, 4, 5]], Hrefs),
    ?assert(has(H, <<"aria-current=\"page\">3</a>">>)),
    ?assert(has(H, <<"data-href=\"/list?page={page}&amp;size={size}\"">>)),
    ?assert(has(H, <<"data-ah-on=\"change:">>)),
    %% the last page: next and last are not links
    Last = r(?M:ah_pagination(95, 5, [], [{href, <<"?p={page}">>}, {page_size, 20}])),
    ?assert(has(Last, <<"ah-pagination-nav ah-pagination-nav-disabled\" data-type=\"next\"">>)),
    ?assertNot(has(Last, <<"data-type=\"next\" aria-label">>)).

%%% CSS: every sigil class the module writes exists in the stylesheets

classes_are_styled_test() ->
    %% the sources when run from the project root, else the installed copy
    Root = "apps/aihtml/priv/css",
    Dir = case filelib:is_dir(Root) of
              true -> Root;
              false -> filename:join(code:priv_dir(aihtml), "css")
          end,
    Css = iolist_to_binary([element(2, file:read_file(F))
                            || F <- filelib:wildcard(filename:join([Dir, "**", "*.css"]))]),
    Html = iolist_to_binary([r(H) || H <- samples()]),
    {ok, Re} = re:compile(<<"class=\"([^\"]*)\"">>),
    {match, Ms} = re:run(Html, Re, [global, {capture, [1], binary}]),
    Classes = lists:usort([C || [Cs] <- Ms, C <- binary:split(Cs, <<" ">>, [global, trim_all]),
                                binary:match(C, <<"ah-">>) =:= {0, 3}]),
    %% state markers with no rules of their own (sigil writes -top too)
    Markers = [<<"ah-pagination-links">>],
    Missing = [C || C <- Classes -- Markers, not has(Css, <<".", C/binary>>)],
    ?assertEqual([], Missing).

%% One render of the component in its main variants and states.
samples() ->
    [?M:ah_pagination(500, 12, [disabled], [{show_first_last, true}, {show_total, true},
                                            {show_jumper, true}]),
     ?M:ah_pagination(95, 3, [simple], []),
     ?M:ah_pagination(95, 3, [], [{href, <<"?p={page}">>}])].

%%% element records (designs/05-records.md)

-spec action(atom(), term(), map(), term()) -> ok.
action(_, _, _, _) -> ok.

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z]+:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_pagination(500, 12, [simple], [{page_size, 20}, {show_total, true},
                                                        {labels, #{prev => <<"<">>}}])),
                 r(#ah_pagination{total = 500, value = 12, simple = true, page_size = 20,
                                  show_total = true, labels = #{prev => <<"<">>}})).

postback_test() ->
    ?assertEqual({<<"change">>, {other_mod, page, 1}},
                 token(#ah_pagination{total = 50, postback = {page, 1}, delegate = other_mod})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, show_jumper, 1}},
                 r(#ah_pagination{show_jumper = 1})).

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

default(ah_pagination) -> #ah_pagination{}.
