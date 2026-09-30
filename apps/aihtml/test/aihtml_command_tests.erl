%% Tests for aihtml_command. The module is also the fake action module
%% of the command palette's server-side search.
-module(aihtml_command_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_command.hrl").

-define(M, aihtml_command).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

cmd_items() ->
    [#{heading => <<"Files">>,
       items => [#{value => new, label => <<"New file">>, shortcut => <<"⌘N"/utf8>>,
                   icon => <<"+">>},
                 #{value => open, label => <<"Open">>, description => <<"Open a file">>,
                   disabled => true}]},
     <<"Loose">>,
     {theme, <<"Toggle theme">>}].

command_test() ->
    H = r(?M:ah_command(cmd_items(), [<<"w-96">>], [{id, cmd}, {placeholder, <<"Search">>}])),
    ?assert(has(<<"<div class=\"ah-command w-96\" id=\"cmd\" data-ah=\"command\" data-ah-query=\"\" "
                  "data-close-on-select=\"true\">">>, H)),
    ?assert(has(<<"<input class=\"ah-command__input\" type=\"text\" id=\"cmd-input\" "
                  "placeholder=\"Search\" value=\"\" autocomplete=\"off\" spellcheck=\"false\" "
                  "aria-label=\"command input\" role=\"combobox\" aria-expanded=\"true\" "
                  "aria-autocomplete=\"list\" aria-controls=\"cmd-list\" data-command=\"cmd\" "
                  "data-empty=\"No results found.\">">>, H)),
    ?assert(has(<<"<div class=\"ah-command__list\" id=\"cmd-list\" role=\"listbox\">">>, H)),
    ?assert(has(<<"<div class=\"ah-command__group\" role=\"group\"><div class=\"ah-command__group-heading\" "
                  "role=\"presentation\">Files</div>">>, H)),
    ?assert(has(<<"<div class=\"ah-command__item\" id=\"cmd-item-0\" role=\"option\" data-value=\"new\" "
                  "data-index=\"0\" data-active=\"true\" data-disabled=\"false\" aria-selected=\"true\">"
                  "<span class=\"ah-command__item-icon\" aria-hidden=\"true\">+</span>"
                  "<div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">New file</span>"
                  "</div><kbd class=\"ah-kbd ah-command__item-shortcut\">⌘N</kbd></div>"/utf8>>, H)),
    ?assert(has(<<"data-value=\"open\" data-index=\"1\" data-active=\"false\" data-disabled=\"true\" "
                  "aria-selected=\"false\" aria-disabled=\"true\">">>, H)),
    ?assert(has(<<"<span class=\"ah-command__item-desc\">Open a file</span>">>, H)),
    %% bare commands in a row share one unnamed group
    ?assert(has(<<"<div class=\"ah-command__group\" role=\"group\"><div class=\"ah-command__item\" "
                  "id=\"cmd-item-2\" role=\"option\" data-value=\"Loose\"">>, H)),
    ?assert(has(<<"id=\"cmd-item-3\" role=\"option\" data-value=\"theme\"">>, H)),
    ?assertEqual(2, count(<<"class=\"ah-command__group\"">>, H)),
    ?assert(has(<<"<div class=\"ah-command__empty\" hidden>No results found.</div>">>, H)).

command_palette_test() ->
    H = r(?M:ah_command([<<"a">>], [palette], [{id, p}, {hotkey, <<"k">>}, {close_on_select, false},
                                                {empty_text, <<"Nothing">>}])),
    ?assert(has(<<"<div class=\"ah-command-overlay\" hidden><div class=\"ah-command ah-command-panel\" "
                  "id=\"p\" role=\"dialog\" aria-modal=\"true\" aria-label=\"Command palette\" "
                  "data-ah=\"command\" data-ah-query=\"\" data-hotkey=\"k\" "
                  "data-close-on-select=\"false\">">>, H)),
    E = r(?M:ah_command([], [auto_focus], [{query, <<"zz">>}])),
    ?assert(has(<<"data-ah-query=\"zz\" data-auto-focus">>, E)),
    ?assert(has(<<"value=\"zz\"">>, E)),
    ?assert(has(<<"<div class=\"ah-command__empty\">No results found.</div>">>, E)),
    ?assertError({aihtml, {bad_command_item, _}}, r(?M:ah_command([{1, 2, 3}], [], []))).

command_search_test() ->
    H = r(?M:ah_command([], [], [{id, c}, {search, {?MODULE, search, #{}}}])),
    ?assert(has(<<"data-ah-remote">>, H)),
    {match, [Tok]} = re:run(H, <<"data-ah-on=\"input:([^\":]+):250\"">>,
                            [{capture, all_but_first, binary}]),
    ?assertEqual({ok, {?MODULE, search, #{}}}, aihtml_action:unsign(Tok)),
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:set_command_items(Ctx, #{data => #{<<"command">> => <<"c">>,
                                                          <<"empty">> => <<"None">>}},
                                         [{a, <<"Alpha">>}])
            end),
    Bin = iolist_to_binary(io_lib:format("~p", [Ops])),
    ?assert(has(<<"c-list">>, Bin)),
    ?assert(has(<<"morph_inner">>, Bin) orelse has(<<"morph-inner">>, Bin)),
    ?assert(has(<<"itemsLoaded">>, Bin)),
    ?assert(has(<<"c-item-0">>, Bin)),
    ?assert(has(<<"None">>, Bin)).

%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := command}] = ?M:catalog(),
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
        aihtml_catalog:entry(?M, command),
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    ?assertMatch([_ | _], Ms),
    ?assertEqual([{set_command_items, 3}], ?M:facade_extras()),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    ?assertEqual(r(?M:ah_command(cmd_items(), [palette], [{id, c}, {hotkey, <<"k">>},
                                                          {search, {?MODULE, s, #{}}}])),
                 r(#ah_command{items = cmd_items(), palette = true, id = c, hotkey = <<"k">>,
                               search = {?MODULE, s, #{}}})).

postback_test() ->
    Token = fun(Html) ->
                    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"([a-z:]+:[^\":]+)\"">>,
                                          [{capture, all_but_first, binary}]),
                    [Tok | Rev] = lists:reverse(binary:split(T, <<":">>, [global])),
                    {ok, Ref} = aihtml_action:unsign(Tok),
                    {iolist_to_binary(lists:join(<<":">>, lists:reverse(Rev))), Ref}
            end,
    ?assertEqual({<<"ah:select">>, {other, run, #{}}},
                 Token(#ah_command{postback = run, delegate = other})),
    ?assertError({aihtml, {record_only_field, ah_command, postback}},
                 ?M:ah_command([], [], [{postback, x}])).

field_validation_test() ->
    ?assertError({aihtml, {bad_option, search, nope}}, r(#ah_command{search = nope})),
    ?assertError({aihtml, {bad_option, close_on_select, 1}},
                 r(#ah_command{close_on_select = 1})),
    ?assertError({aihtml, {unknown_modifier, command, big, _}}, ?M:ah_command([], [big], [])).

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

default(ah_command) -> #ah_command{}.
