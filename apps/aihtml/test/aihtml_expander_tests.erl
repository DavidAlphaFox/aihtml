-module(aihtml_expander_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_expander.hrl").

-export([action/4]).

-define(M, aihtml_expander).

r(H) -> aihtml_html:render_binary(H).

has(Bin, Part) -> binary:match(Bin, Part) =/= nomatch.

count(Bin, Part) -> length(binary:matches(Bin, Part)).

%%% catalog

catalog_matches_exports_test() ->
    Exports = ?M:module_info(exports),
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([expander], Names),
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

%%% expander

expander_test() ->
    H = r(?M:ah_expander(<<"body">>, [bottom, no_gutters], [{id, e}, {header, <<"Head">>},
                                                            {expanded, false}, {name, open}])),
    ?assert(has(H, <<"class=\"ah-expander ah-expander-bottom ah-expander-no-gutters\"">>)),
    ?assert(has(H, <<"data-ah=\"expander\" data-ah-value=\"false\"">>)),
    ?assert(has(H, <<"role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"e-content\"">>)),
    ?assert(has(H, <<"id=\"e-content\" role=\"region\" aria-labelledby=\"e-header\" style=\"display:none\"">>)),
    ?assert(has(H, <<"<input type=\"hidden\" name=\"open\" value=\"false\">">>)).

expander_disabled_dual_icon_test() ->
    H = r(?M:ah_expander(<<"b">>, [disabled], [{id, e}, {expand_icon, <<"+">>},
                                               {collapse_icon, <<"-">>}])),
    ?assert(has(H, <<"tabindex=\"-1\"">>)),
    ?assert(has(H, <<"aria-disabled=\"true\"">>)),
    ?assert(has(H, <<"ah-expander-arrow ah-expander-arrow-expanded ah-expander-arrow-dual">>)),
    ?assert(has(H, <<"ah-expander-header ah-expander-header-expanded">>)).

expander_structured_header_test() ->
    H = r(?M:ah_expander(<<"b">>, [], [{header, #{title => <<"A">>, extra => <<"3">>}},
                                       {toggle_mode, none}, {accordion, g}])),
    ?assert(has(H, <<"ah-expander-header-text-structured">>)),
    ?assert(has(H, <<"<span class=\"ah-expander-header-extra\">3</span>">>)),
    ?assert(has(H, <<"ah-expander-header-no-toggle">>)),
    ?assert(has(H, <<"data-toggle-mode=\"none\"">>)),
    ?assert(has(H, <<"data-accordion=\"g\"">>)).

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
    Markers = [<<"ah-expander-top">>],
    Missing = [C || C <- Classes -- Markers, not has(Css, <<".", C/binary>>)],
    ?assertEqual([], Missing).

%% One render of the component in its main variants and states.
samples() ->
    [?M:ah_expander(<<"c">>, [bottom, square, no_gutters, disabled],
                    [{header, #{title => <<"t">>, subheader => <<"s">>, extra => <<"x">>}},
                     {actions, <<"a">>}, {arrow_position, left}, {toggle_mode, none},
                     {expand_icon, <<"+">>}, {collapse_icon, <<"-">>}]),
     ?M:ah_expander(<<"c">>, [], [])].

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
    ?assertEqual(r(?M:ah_expander(<<"b">>, [bottom, disabled],
                                  [{id, e}, {header, <<"H">>}, {expanded, false},
                                   {toggle_mode, dblclick}, {name, open}])),
                 r(#ah_expander{body = <<"b">>, position = bottom, disabled = true, id = e,
                                header = <<"H">>, expanded = false, toggle_mode = dblclick,
                                name = open})).

generated_ids_test() ->
    %% no id: one is generated and the inner ids derive from it
    E = r(#ah_expander{body = <<"b">>}),
    {match, [Id]} = re:run(E, <<"^<div class=\"ah-expander ah-expander-top\" id=\"(ah-expander-[0-9]+)\"">>,
                           [{capture, all_but_first, binary}]),
    ?assert(has(E, <<"id=\"", Id/binary, "-header\"">>)),
    ?assert(has(E, <<"aria-controls=\"", Id/binary, "-content\"">>)),
    ?assertEqual(1, count(E, <<"id=\"", Id/binary, "\"">>)).

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, toggled, #{}}},
                 token(#ah_expander{id = e, postback = toggled})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, toggle_mode, hold}},
                 r(#ah_expander{id = e, toggle_mode = hold})),
    ?assertError({aihtml, {bad_option, expanded, "no"}},
                 r(#ah_expander{id = e, expanded = "no"})),
    %% values are checked when rendering, not when building
    Bad = ?M:ah_expander(<<"b">>, [], [{id, e}, {arrow_position, up}]),
    ?assertError({aihtml, {bad_option, arrow_position, up}}, r(Bad)).

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

default(ah_expander) -> #ah_expander{}.
