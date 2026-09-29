-module(aihtml_drawer_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_drawer.hrl").

-define(M, aihtml_drawer).

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
    ?assertEqual([drawer], [N || #{name := N} <- ?M:catalog()]).

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
%%% Drawer
%%%===================================================================

drawer_structure_test() ->
    H = ?M:drawer(<<"Body">>, [<<"bg-red-50">>],
                  [{id, <<"d">>}, {title, <<"Title">>}, {description, <<"Desc">>},
                   {footer, <<"F">>}]),
    has(H, <<"<div class=\"ah-drawer__overlay\" data-ah=\"drawer\" data-state=\"closed\" id=\"d\">">>),
    has(H, <<"class=\"ah-drawer__panel bg-red-50\" role=\"dialog\" aria-modal=\"true\" "
             "aria-labelledby=\"d-title\" data-side=\"bottom\" data-state=\"closed\" "
             "style=\"height:50vh;\" tabindex=\"-1\"">>),
    has(H, <<"<div class=\"ah-drawer__handle\" aria-hidden=\"true\">">>),
    has(H, <<"<h2 class=\"ah-drawer__title\" id=\"d-title\">Title</h2>">>),
    has(H, <<"<p class=\"ah-drawer__description\">Desc</p>">>),
    has(H, <<"class=\"ah-drawer__close\" type=\"button\" aria-label=\"close\" data-ah-close=\"\"">>),
    has(H, <<"<div class=\"ah-drawer__body\">Body</div>">>),
    has(H, <<"<div class=\"ah-drawer__footer\">F</div>">>).

drawer_options_test() ->
    H = ?M:drawer(<<"x">>, [right], [{size, 320}, {handle, false}, {closable, false},
                                     {dismissible, false}, {close_on_overlay, false},
                                     {close_on_esc, false}, {open, true}]),
    has(H, <<"data-side=\"right\"">>),
    has(H, <<"style=\"width:320px;\"">>),
    has(H, <<"data-ah-esc=\"false\" data-ah-scrim=\"false\" data-ah-dismissible=\"false\" "
             "data-ah-initial=\"open\"">>),
    lacks(H, <<"__handle">>),
    lacks(H, <<"__header">>),
    lacks(H, <<"__footer">>).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:drawer(<<"D">>, [right, <<"bg-white">>],
                             [{id, <<"d">>}, {title, <<"T">>}, {size, 280},
                              {handle, false}, {close_on_esc, false}])),
                 r(#ah_drawer{body = <<"D">>, side = right, css = [<<"bg-white">>],
                              id = <<"d">>, title = <<"T">>, size = 280, handle = false,
                              close_on_esc = false})).

builder_fills_fields_test() ->
    ?assertError({aihtml, {record_only_field, ah_drawer, postback}},
                 ?M:drawer(<<"x">>, [], [{postback, save}])).

ids_test() ->
    %% the title id follows the id field, also for an atom id
    has(#ah_drawer{id = cart, title = <<"Cart">>},
        <<"aria-labelledby=\"cart-title\"">>),
    lacks(#ah_drawer{title = <<"T">>}, <<"aria-labelledby">>).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"(ah:[a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ah, Ev, Tok] = binary:split(T, <<":">>, [global]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {<<Ah/binary, ":", Ev/binary>>, Ref}
            end,
    Close = <<"ah:close">>,
    ?assertEqual({Close, {?MODULE, closed, #{id => 1}}},
                 Token(#ah_drawer{id = <<"d">>, postback = {closed, #{id => 1}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_modifier, drawer, side, center, _}},
                 r(#ah_drawer{side = center})),
    %% modifier names still fail in the builder
    ?assertError({aihtml, {unknown_modifier, drawer, center, _}},
                 ?M:drawer(<<"x">>, [center], [])).

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

default(ah_drawer) -> #ah_drawer{}.
