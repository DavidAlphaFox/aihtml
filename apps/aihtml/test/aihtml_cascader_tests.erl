%% Tests for aihtml_cascader. The module is also the fake action module
%% of the lazy cascader round trip.
-module(aihtml_cascader_tests).
-behaviour(aihtml_action).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aihtml/include/aihtml_cascader.hrl").

-export([action/4]).

-define(M, aihtml_cascader).

r(Html) -> aihtml_html:render_binary(Html).

has(Needle, Hay) ->
    case binary:match(Hay, Needle) of
        nomatch -> ?debugFmt("~s not in~n~s", [Needle, Hay]), false;
        _ -> true
    end.

has_quiet(Needle, Hay) -> binary:match(Hay, Needle) =/= nomatch.

count(Needle, Hay) -> length(binary:matches(Hay, Needle)).

tree() ->
    [{<<"zj">>, <<"Zhejiang">>,
      [{<<"hz">>, <<"Hangzhou">>, [<<"xihu">>, {<<"bj">>, <<"Binjiang">>}]},
       #{value => <<"nb">>, label => <<"Ningbo">>, disabled => true}]},
     {<<"js">>, <<"Jiangsu">>, lazy},
     <<"hk">>].

%%%===================================================================
%%% cascader
%%%===================================================================

cascader_basic_test() ->
    H = r(?M:cascader(tree(), [<<"zj">>, <<"hz">>, <<"bj">>], [<<"w-64">>],
                      [{id, cs}, {name, region}, {title, <<"t">>}])),
    ?assert(has(<<"<div class=\"ah-cascader w-64\" id=\"cs\" data-ah=\"cascader\" "
                  "data-ah-value=\"zj,hz,bj\"">>, H)),
    ?assert(has(<<"<input type=\"hidden\" name=\"region\" value=\"zj,hz,bj\">">>, H)),
    ?assert(has(<<"value=\"Zhejiang / Hangzhou / Binjiang\"">>, H)),
    ?assert(has(<<"readonly">>, H)),
    ?assert(has(<<"placeholder=\"Please select\"">>, H)),
    ?assert(has(<<"role=\"combobox\"">>, H)),
    ?assert(has(<<"aria-controls=\"cs-menus\"">>, H)),
    ?assert(has(<<"title=\"t\"">>, H)),
    ?assertEqual(1, count(<<"name=">>, H)),
    %% one column for the top level and one per branch, the top one shown
    ?assertEqual(3, count(<<"class=\"ah-cascader-menu-column\"">>, H)),
    ?assert(has(<<"data-level=\"0\" data-parent=\"\"><ul">>, H)),
    ?assert(has(<<"data-level=\"1\" data-parent=\"zj\" hidden>">>, H)),
    ?assert(has(<<"data-level=\"2\" data-parent=\"zj,hz\" hidden>">>, H)),
    %% the path is active, branches have arrows, lazy nodes are marked
    ?assert(has(<<"<li class=\"has-children active\" role=\"option\" aria-selected=\"true\" "
                  "aria-haspopup=\"true\" data-value=\"zj\" data-level=\"0\">">>, H)),
    ?assert(has(<<"<li class=\"active\" role=\"option\" aria-selected=\"true\" "
                  "data-value=\"bj\" data-level=\"2\">">>, H)),
    ?assert(has(<<"data-value=\"js\" data-level=\"0\" data-lazy>">>, H)),
    ?assert(has(<<"aria-disabled=\"true\" data-value=\"nb\"">>, H)),
    ?assert(has(<<"<span class=\"ah-cascader-menu-item-label\">Hangzhou</span>">>, H)),
    ?assert(has(<<"<span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">▶"/utf8>>, H)),
    ?assert(has(<<"style=\"max-height:240px\"">>, H)),
    ?assert(has(<<"<span class=\"ah-cascader-clear\" role=\"button\" aria-label=\"Clear\">">>, H)),
    ?assert(has(<<"ah-cascader-arrow-icon">>, H)),
    ?assertNot(has_quiet(<<"ah-cascader-search-panel">>, H)),
    ?assertNot(has_quiet(<<"ah-cascader-loader">>, H)).

cascader_empty_and_unknown_test() ->
    E = r(?M:cascader(tree(), undefined, [], [])),
    ?assert(has(<<"data-ah-value=\"\"">>, E)),
    ?assert(has(<<"id=\"ah-l">>, E)),
    ?assert(has(<<"<span class=\"ah-cascader-clear\" role=\"button\" aria-label=\"Clear\" hidden>">>, E)),
    ?assertNot(has_quiet(<<"class=\"active\"">>, E)),
    %% a path below a lazy node shows the values it cannot resolve
    U = r(?M:cascader(tree(), [<<"js">>, <<"nj">>], [], [{separator, <<" > ">>}])),
    ?assert(has(<<"value=\"Jiangsu &gt; nj\"">>, U)),
    ?assertError({aihtml, {bad_option, value, <<"zj">>}}, r(?M:cascader(tree(), <<"zj">>, [], []))),
    ?assertError({aihtml, {bad_cascader_node, _}}, r(?M:cascader([{1, 2, 3, 4}], undefined, [], []))),
    ?assertError({aihtml, {bad_cascader_node, _}},
                 r(?M:cascader([#{value => a, kids => []}], undefined, [], []))).

cascader_flags_options_test() ->
    H = r(?M:cascader(tree(), undefined,
                      [lg, disabled, filterable, change_on_select, no_arrow, no_clear],
                      [{id, c}, {placeholder, <<"Pick">>}, {popup_height, 300},
                       {empty_text, <<"None">>}, {separator, <<"/">>}])),
    ?assert(has(<<"class=\"ah-cascader ah-cascader-lg ah-cascader-change-on-select ah-cascader-disabled "
                  "ah-cascader-filterable ah-cascader-no-arrow ah-cascader-no-clear\"">>, H)),
    ?assert(has(<<"data-ah-separator=\"/\" data-ah-empty=\"None\" data-ah-change-on-select">>, H)),
    ?assert(has(<<"aria-disabled=\"true\"">>, H)),
    ?assert(has(<<"placeholder=\"Pick\"">>, H)),
    ?assert(has(<<"max-height:300px">>, H)),
    ?assertNot(has_quiet(<<"readonly">>, H)),
    ?assert(has(<<"aria-autocomplete=\"list\"">>, H)),
    ?assertNot(has_quiet(<<"ah-cascader-clear">>, H)),
    ?assertNot(has_quiet(<<"ah-cascader-arrow">>, H)),
    %% every path (change_on_select), a disabled node disables its path,
    %% lazy nodes are searched as themselves only
    ?assert(has(<<"<ul class=\"ah-cascader-search-panel\" id=\"c-search\" role=\"listbox\" hidden>">>, H)),
    ?assert(has(<<"data-path=\"zj,hz,bj\" data-label=\"Zhejiang/Hangzhou/Binjiang\">"
                  "<span class=\"ah-cascader-search-item-label\">Zhejiang/Hangzhou/Binjiang</span></li>">>, H)),
    ?assert(has(<<"data-path=\"zj\"">>, H)),
    ?assert(has(<<"aria-disabled=\"true\" data-path=\"zj,nb\"">>, H)),
    ?assert(has(<<"data-path=\"js\"">>, H)),
    S = r(?M:cascader(tree(), undefined, [sm, filterable], [])),
    ?assert(has(<<"ah-cascader-sm">>, S)),
    ?assertNot(has_quiet(<<"data-path=\"zj\"">>, S)),
    ?assertNot(has_quiet(<<"data-path=\"js\"">>, S)),
    ?assert(has(<<"data-path=\"hk\"">>, S)),
    ?assertError({aihtml, {bad_option, popup_height, 0}},
                 r(?M:cascader([], undefined, [], [{popup_height, 0}]))),
    ?assertError({aihtml, {conflicting_modifiers, cascader, size, _}},
                 ?M:cascader([], undefined, [sm, lg], [])).

%%%===================================================================
%%% Lazy levels: render, verify the token, run the action
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(load, _Args, #{value := <<"js">>} = Ev, Ctx) ->
    ?M:cascader_children(Ctx, Ev, [{<<"nj">>, <<"Nanjing">>, [<<"gulou">>]},
                                   {<<"sz">>, <<"Suzhou">>, lazy}]);
action(load, _Args, Ev, Ctx) ->
    ?M:cascader_children(Ctx, Ev, []).

lazy_round_trip_test() ->
    Ref = {?MODULE, load, #{}},
    H = r(?M:cascader(tree(), undefined, [], [{id, <<"cz">>}, {load, Ref}])),
    %% the loader carries the queued action and names the cascader
    {match, [Token]} = re:run(H, <<"<span class=\"ah-cascader-loader\" hidden "
                                   "data-cascader=\"cz\" data-ah-on=\"ah:load:([^\"]+)\" "
                                   "data-ah-sync=\"queue\"></span>">>,
                              [{capture, all_but_first, binary}]),
    {ok, Ref} = aihtml_action:verify(Token),
    Event = #{<<"type">> => <<"ah:load">>, <<"id">> => <<"x">>, <<"value">> => <<"js">>,
              <<"data">> => #{<<"cascader">> => <<"cz">>}},
    {ok, [Html, Call]} = aihtml_action:execute(Ref, Event, #{send => fun(_) -> error(unexpected_flush) end}),
    #{op := html, swap := append, id := <<"cz-menus">>, html := Cols} = Html,
    ?assertEqual(2, count(<<"class=\"ah-cascader-menu-column\"">>, Cols)),
    ?assert(has(<<"data-level=\"1\" data-parent=\"js\" hidden><ul">>, Cols)),
    ?assert(has(<<"data-level=\"2\" data-parent=\"js,nj\" hidden>">>, Cols)),
    ?assert(has(<<"data-value=\"sz\" data-level=\"1\" data-lazy>">>, Cols)),
    ?assertEqual(#{op => call, id => <<"cz">>, method => <<"childrenLoaded">>,
                   args => [<<"js">>]}, Call),
    %% no children: only the call, the node becomes a leaf in the browser
    Ops = aihtml_action:render_ops(
            fun(Ctx) -> ?M:cascader_children(Ctx, {id, cz}, [<<"js">>, sz], []) end),
    ?assertEqual([#{op => call, id => <<"cz">>, method => <<"childrenLoaded">>,
                    args => [<<"js,sz">>]}], Ops),
    _ = iolist_to_binary(json:encode([Html, Call])).


%%%===================================================================
%%% Catalog
%%%===================================================================

catalog_test() ->
    [#{name := cascader}] = ?M:catalog(),
    #{flags := Fl, options := Op, option_docs := Docs, methods := Ms} =
        aihtml_catalog:entry(?M, cascader),
    ?assertEqual(lists:sort(Fl ++ Op), lists:sort(maps:keys(Docs))),
    ?assert(lists:member(setValue, [Name || #{name := Name} <- Ms])),
    ?assert(erlang:function_exported(?M, cascader, 4)),
    [?assert(erlang:function_exported(?M, F, A)) || {F, A} <- ?M:facade_extras()].

%%%===================================================================
%%% element records (designs/05-records.md)
%%%===================================================================

record_equals_builder_test() ->
    Ref = {?MODULE, load, #{}},
    ?assertEqual(r(?M:cascader(tree(), [<<"zj">>], [sm, filterable, <<"w-64">>],
                               [{id, c}, {name, n}, {separator, <<">">>}, {load, Ref},
                                {title, <<"t">>}])),
                 r(#ah_cascader{items = tree(), value = [<<"zj">>], size = sm, filterable = true,
                                css = [<<"w-64">>], id = c, name = n, separator = <<">">>,
                                load = Ref, attrs = [{title, <<"t">>}]})).

builder_fills_fields_test() ->
    C = ?M:cascader([a], [a], [lg, no_arrow, <<"x">>], [{popup_height, 100}, {title, <<"t">>}]),
    ?assertMatch(#ah_cascader{items = [a], value = [a], size = lg, no_arrow = true,
                              popup_height = 100, css = [<<"x">>], attrs = [{title, <<"t">>}]}, C).

token(Html) ->
    {match, [T]} = re:run(r(Html), <<"data-ah-on=\"(change:[^\"]+)\"">>,
                          [{capture, all_but_first, binary}]),
    [Ev, Tok] = binary:split(T, <<":">>),
    {ok, Ref} = aihtml_action:unsign(Tok),
    {Ev, Ref}.

postback_test() ->
    ?assertEqual({<<"change">>, {?MODULE, picked, #{id => 7}}},
                 token(#ah_cascader{items = tree(), postback = {picked, #{id => 7}}})).

field_validation_test() ->
    ?assertError({aihtml, {bad_flag, cascader, filterable, yes}},
                 r(#ah_cascader{filterable = yes})),
    ?assertError({aihtml, {bad_modifier, cascader, size, huge, _}}, r(#ah_cascader{size = huge})),
    ?assertError({aihtml, {bad_option, popup_height, tall}}, r(#ah_cascader{popup_height = tall})).

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

default(ah_cascader) -> #ah_cascader{}.

%% A value containing a comma is escaped in data-ah-value (aihtml_value).
vhas(Sub, Bin) -> binary:match(Bin, Sub) =/= nomatch.

comma_values_test() ->
    Tree = [{<<"a,b">>, <<"AB">>, [{<<"c\\d">>, <<"CD">>}]}],
    H = r(?M:cascader(Tree, [<<"a,b">>, <<"c\\d">>], [], [{id, <<"cc">>}, {name, p}])),
    ?assert(vhas(<<"data-ah-value=\"a\\,b,c\\\\d\"">>, H)),
    ?assert(vhas(<<"value=\"a\\,b,c\\\\d\"">>, H)),
    ?assert(vhas(<<"data-parent=\"a\\,b\"">>, H)),
    %% a lazy level's path comes back in the same form
    Ops = aihtml_action:render_ops(
            fun(Ctx) ->
                    ?M:cascader_children(Ctx, #{data => #{<<"cascader">> => <<"cc">>},
                                                value => <<"a\\,b,c\\\\d">>}, [<<"e">>])
            end),
    [#{op := html, html := Cols}, #{op := call, args := [Path]}] = Ops,
    ?assertEqual(<<"a\\,b,c\\\\d">>, Path),
    ?assert(vhas(<<"data-parent=\"a\\,b,c\\\\d\"">>, Cols)).
