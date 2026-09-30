-module(aihtml_steps_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_steps.hrl").

-export([action/4]).

-define(M, aihtml_steps).

r(H) -> aihtml_html:render_binary(H).

has(Bin, Part) -> binary:match(Bin, Part) =/= nomatch.

count(Bin, Part) -> length(binary:matches(Bin, Part)).

%%% catalog

catalog_matches_exports_test() ->
    Exports = ?M:module_info(exports),
    Names = [N || #{name := N} <- ?M:catalog()],
    ?assertEqual([steps], Names),
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

%%% steps

steps_test() ->
    H = r(?M:ah_steps([<<"A">>, {<<"B">>, <<"desc">>}, #{title => <<"C">>, status => error}], 1,
                      [vertical], [])),
    ?assert(has(H, <<"class=\"ah-steps ah-steps-vertical\" data-ah=\"steps\" data-ah-value=\"1\"">>)),
    ?assert(has(H, <<"ah-steps-item ah-steps-item-completed ah-steps-item-clickable">>)),
    ?assert(has(H, <<"ah-steps-item ah-steps-item-active ah-steps-item-clickable ah-steps-item-selected">>)),
    ?assert(has(H, <<"ah-steps-item ah-steps-item-error">>)),
    ?assert(has(H, <<"ah-steps-connector ah-steps-connector-done">>)),
    ?assert(has(H, <<"aria-current=\"step\"">>)),
    ?assertNot(has(H, <<"ah-steps-panels">>)),
    ?assertNot(has(H, <<"ah-steps-nav">>)).

steps_panels_nav_test() ->
    H = r(?M:ah_steps([#{title => <<"A">>, content => <<"a">>}, #{title => <<"B">>, content => <<"b">>}],
                      0, [], [{clickable, false}])),
    ?assert(has(H, <<"<div class=\"ah-steps-panel ah-steps-panel-active\" data-index=\"0\">a</div>">>)),
    ?assertEqual(2, count(H, <<" disabled>">>)),
    ?assert(has(H, <<"data-clickable=\"false\"">>)),
    ?assertNot(has(H, <<"role=\"button\"">>)).

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
    Markers = [],
    Missing = [C || C <- Classes -- Markers, not has(Css, <<".", C/binary>>)],
    ?assertEqual([], Missing).

%% One render of the component in its main variants and states.
samples() ->
    [?M:ah_steps([<<"A">>, #{title => <<"B">>, status => error, description => <<"d">>,
                             content => <<"c">>}, #{title => <<"C">>, disabled => true}, <<"D">>],
                 1, [vertical, disabled], [])].

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
    ?assertEqual(r(?M:ah_steps([<<"A">>, <<"B">>], 1, [vertical], [{clickable, false}])),
                 r(#ah_steps{items = [<<"A">>, <<"B">>], value = 1, orientation = vertical,
                             clickable = false})).

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, step, #{}}},
                 token(#ah_steps{items = [<<"A">>], postback = step})).

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

default(ah_steps) -> #ah_steps{}.
