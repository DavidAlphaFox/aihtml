-module(aihtml_popover_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_popover.hrl").

-define(M, aihtml_popover).

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
    ?assertEqual([popover], [N || #{name := N} <- ?M:catalog()]).

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
%%% Popover
%%%===================================================================

popover_test() ->
    H = ?M:popover(<<"Body">>, [left, no_arrow, <<"w-64">>],
                   [{id, <<"p">>}, {title, <<"T">>}, {closable, true}, {modal, true},
                    {anchor, <<"#btn">>}]),
    has(H, <<"class=\"ah-popover ah-popover-left ah-popover-no-arrow w-64\"">>),
    has(H, <<"data-ah=\"popover\" data-state=\"closed\" role=\"dialog\"">>),
    has(H, <<"data-ah-anchor=\"#btn\"">>),
    has(H, <<"data-ah-modal=\"true\"">>),
    has(H, <<"<div class=\"ah-popover-title\">T<div class=\"ah-popover-close-btn\"">>),
    has(H, <<"<div class=\"ah-popover-content\">Body</div>">>).

popover_defaults_test() ->
    H = ?M:popover(<<"x">>, [], []),
    has(H, <<"ah-popover ah-popover-bottom">>),
    lacks(H, <<"ah-popover-title">>).

%% The declarative triggers live in aihtml_lib_overlay, whose
%% facade_extras/0 the facade re-exports; the aihtml facade has them.
triggers_in_lib_test() ->
    ?assertNot(erlang:function_exported(?M, facade_extras, 0)),
    Extras = aihtml_lib_overlay:facade_extras(),
    ?assertEqual([{opens, 1}, {closes, 0}, {closes, 1}, {closes, 2}, {toggles, 1}], Extras),
    [?assert(erlang:function_exported(aihtml_lib_overlay, F, A)) || {F, A} <- Extras],
    {module, aihtml} = code:ensure_loaded(aihtml),
    [?assert(erlang:function_exported(aihtml, F, A)) || {F, A} <- Extras],
    ?assertEqual(aihtml_lib_overlay:opens({id, d}), aihtml:opens({id, d})),
    ?assertEqual(aihtml_lib_overlay:closes(closest, ok), aihtml:closes(closest, ok)).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:popover(<<"P">>, [top], [{id, <<"p">>}, {title, <<"T">>},
                                               {closable, true}, {modal, false}])),
                 r(#ah_popover{body = <<"P">>, position = top, id = <<"p">>, title = <<"T">>,
                               closable = true, modal = false})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"(ah:[a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ah, Ev, Tok] = binary:split(T, <<":">>, [global]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {<<Ah/binary, ":", Ev/binary>>, Ref}
            end,
    Close = <<"ah:close">>,
    ?assertEqual({Close, {?MODULE, closed, #{}}}, Token(#ah_popover{postback = closed})).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, popover, no_arrow, yes}},
                 r(#ah_popover{no_arrow = yes})).

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

default(ah_popover) -> #ah_popover{}.
