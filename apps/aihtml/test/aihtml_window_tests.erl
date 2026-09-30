-module(aihtml_window_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_window.hrl").

-define(M, aihtml_window).

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
    ?assertEqual([window], [N || #{name := N} <- ?M:catalog()]).

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
%%% Window
%%%===================================================================

window_test() ->
    H = ?M:ah_window(<<"Body">>, [<<"shadow-xl">>],
                     [{id, <<"w">>}, {title, <<"Title">>}, {modal, true}, {footer, <<"F">>},
                      {width, 400}, {height, 300}, {collapsible, true}]),
    has(H, <<"class=\"ah-window shadow-xl ah-window-resizable\" data-ah=\"window\" "
             "data-state=\"closed\" role=\"dialog\" tabindex=\"-1\" aria-modal=\"true\" "
             "aria-labelledby=\"w-title\" style=\"display:none;width:400px;height:300px;\"">>),
    has(H, <<"data-ah-modal=\"true\"">>),
    has(H, <<"<div class=\"ah-window-header ah-window-header-draggable\">">>),
    has(H, <<"<div class=\"ah-window-title\" id=\"w-title\">Title</div>">>),
    has(H, <<"class=\"ah-window-collapse-btn\" type=\"button\" aria-label=\"Collapse\" aria-expanded=\"true\"">>),
    has(H, <<"class=\"ah-window-close-btn\" type=\"button\" aria-label=\"Close\" data-ah-close=\"\"">>),
    has(H, <<"<div class=\"ah-window-content\">Body</div><div class=\"ah-window-footer\">F</div>">>),
    ?assertEqual(8, length(binary:matches(r(H), <<"ah-window-resize-handle">>))).

window_plain_test() ->
    H = ?M:ah_window(<<"x">>, [], [{resizable, false}, {draggable, false}, {closable, false},
                                   {collapsed, true}]),
    has(H, <<"class=\"ah-window ah-window-collapsed\"">>),
    has(H, <<"aria-modal=\"false\"">>),
    has(H, <<"aria-expanded=\"false\"">>),
    lacks(H, <<"ah-window-close-btn">>),
    lacks(H, <<"header-draggable">>),
    lacks(H, <<"data-ah-modal">>).

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_window(<<"W">>, [<<"shadow">>],
                                [{id, <<"w">>}, {title, <<"T">>}, {modal, true},
                                 {height, 200}, {collapsed, true}])),
                 r(#ah_window{body = <<"W">>, css = [<<"shadow">>], id = <<"w">>,
                              title = <<"T">>, modal = true, height = 200,
                              collapsed = true})).

builder_fills_fields_test() ->
    W = ?M:ah_window(<<"x">>, [<<"c">>], [{id, <<"w">>}, {width, 400}, {draggable, false},
                                           {close_on_esc, false}, {data_x, 1}]),
    ?assertMatch(#ah_window{id = <<"w">>, css = [<<"c">>], width = 400, draggable = false,
                            close_on_esc = false, closable = true, attrs = [{data_x, 1}]}, W).

ids_test() ->
    has(#ah_window{id = <<"w">>}, <<"<div class=\"ah-window-title\" id=\"w-title\">">>),
    %% without an id a window still gets a unique title id
    ?assertMatch({match, _}, re:run(r(#ah_window{}),
                                    <<"aria-labelledby=\"ah-window-[0-9]+-title\"">>)).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"(ah:[a-z]+:[^\"]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Ah, Ev, Tok] = binary:split(T, <<":">>, [global]),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {<<Ah/binary, ":", Ev/binary>>, Ref}
            end,
    Close = <<"ah:close">>,
    ?assertEqual({Close, {other_mod, closed, #{}}},
                 Token(#ah_window{postback = closed, delegate = other_mod})).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, modal, <<"true">>}},
                 r(#ah_window{modal = <<"true">>})),
    ?assertError({aihtml, {modifier_in_css, window, large}},
                 r(#ah_window{css = [large]})).

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

default(ah_window) -> #ah_window{}.
