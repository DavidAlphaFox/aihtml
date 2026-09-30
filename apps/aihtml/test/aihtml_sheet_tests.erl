-module(aihtml_sheet_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_sheet.hrl").

-define(M, aihtml_sheet).

r(H) -> aihtml_html:render_binary(H).

has(Html, Part) ->
    case binary:match(r(Html), Part) of
        nomatch -> ?assertEqual({missing, Part}, r(Html));
        _ -> ok
    end.

lacks(Html, Part) ->
    ?assertEqual(nomatch, binary:match(r(Html), Part)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_names_test() ->
    ?assertEqual([sheet], [N || #{name := N} <- ?M:catalog()]).

catalog_entries_are_complete_test() ->
    [begin
         ?assert(is_binary(S)), ?assert(is_binary(R)),
         ?assertEqual(overlay, C),
         ?assert(lists:member(<<"ah:open">>, maps:get(events, E)))
     end || #{signature := S, root := R, category := C} = E <- ?M:catalog()].

every_entry_documents_options_and_methods_test() ->
    [begin
         Docs = maps:get(option_docs, E),
         [?assert(maps:is_key(O, Docs)) || O <- maps:get(options, E, [])],
         ?assert(is_list(maps:get(methods, E)))
     end || E <- ?M:catalog()].

%%%===================================================================
%%% Sheet
%%%===================================================================

sheet_test() ->
    H = ?M:ah_sheet(<<"x">>, [], [{title, <<"S">>}]),
    has(H, <<"class=\"ah-sheet__overlay\" data-ah=\"sheet\"">>),
    has(H, <<"data-side=\"right\"">>),
    has(H, <<"style=\"width:380px;\"">>),
    has(H, <<"aria-label=\"S\"">>),
    lacks(H, <<"handle">>),
    ?assertError({aihtml, {unknown_modifier, sheet, middle, _}}, ?M:ah_sheet(<<"x">>, [middle], [])),
    %% attributes are checked when the record is rendered
    ?assertError(_, r(?M:ah_sheet(<<"x">>, [], [{handle, true}, {bogus, 1}, {"bad attr", 1}]))).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_sheet(<<"S">>, [top], [{id, s}, {footer, <<"F">>}, {open, true}])),
                 r(#ah_sheet{body = <<"S">>, side = top, id = s, footer = <<"F">>,
                             open = true})).

builder_fills_fields_test() ->
    %% an option of the drawer is an HTML attribute of the sheet
    ?assertMatch(#ah_sheet{attrs = [{handle, false}]}, ?M:ah_sheet(<<"x">>, [], [{handle, false}])).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"(ah:[a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ah, Ev, Tok] = binary:split(T, <<":">>, [global]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {<<Ah/binary, ":", Ev/binary>>, Ref}
            end,
    Close = <<"ah:close">>,
    ?assertEqual({Close, {?MODULE, closed, #{}}}, Token(#ah_sheet{postback = closed})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, close_on_esc, 1}},
                 r(#ah_sheet{close_on_esc = 1})).

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

default(ah_sheet) -> #ah_sheet{}.
