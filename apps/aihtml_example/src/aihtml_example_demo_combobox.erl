%% @doc Demos of the combo box (aihtml_combobox), shown on
%% /components/combobox. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
%%
%% The module is also the action module of the server-side search demo.
-module(aihtml_example_demo_combobox).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0, action/4]).
-export([combo_basic/0, combo_free_text/0, combo_groups/0, combo_multiple/0,
         combo_search/0, combo_disabled/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => combobox, title => <<"ComboBox">>,
       summary => <<"可输入过滤的下拉选择，支持分组、多选、勾选框和服务端搜索。"/utf8>>,
       demos => [{<<"输入过滤，只能选列表里的项"/utf8>>, combo_basic},
                 {<<"自由输入、前缀匹配、无箭头"/utf8>>, combo_free_text},
                 {<<"分组、描述、禁用项"/utf8>>, combo_groups},
                 {<<"多选标签与勾选框"/utf8>>, combo_multiple},
                 {<<"服务端搜索"/utf8>>, combo_search},
                 {<<"禁用"/utf8>>, combo_disabled}]}].

%%%===================================================================
%%% ComboBox
%%%===================================================================

-spec combo_basic() -> aihtml:html().
combo_basic() ->
    combobox(fruits(), <<"Cherry">>, [<<"w-56">>],
             [{name, fruit}, {placeholder, <<"选一种水果"/utf8>>}]).

-spec combo_free_text() -> aihtml:html().
combo_free_text() ->
    row([combobox(fruits(), undefined, [free_text, <<"w-56">>],
                  [{placeholder, <<"任意水果"/utf8>>}, {search_mode, starts_with_ignore_case}]),
         combobox(fruits(), undefined, [no_arrow, <<"w-56">>],
                  [{placeholder, <<"没有箭头"/utf8>>}])]).

-spec combo_groups() -> aihtml:html().
combo_groups() ->
    People = [#{value => 1, label => <<"Ada Lovelace">>, description => <<"Analyst">>,
                group => <<"Engineering">>},
              #{value => 2, label => <<"Alan Turing">>, description => <<"Cryptography">>,
                group => <<"Engineering">>},
              #{value => 3, label => <<"Grace Hopper">>, description => <<"Compilers">>,
                group => <<"Engineering">>},
              #{value => 4, label => <<"Joan Clarke">>, description => <<"Cryptanalysis">>,
                group => <<"Research">>},
              #{value => 5, label => <<"Katherine Johnson">>,
                description => <<"Orbital mechanics">>, group => <<"Research">>,
                disabled => true}],
    combobox(People, 3, [<<"w-72">>], [{name, person}]).

-spec combo_multiple() -> aihtml:html().
combo_multiple() ->
    row([combobox(fruits(), [<<"Apple">>, <<"Mango">>], [multiple, <<"w-72">>],
                  [{name, fruits}, {placeholder, <<"水果"/utf8>>}]),
         combobox([<<"Reading">>, <<"Music">>, <<"Sports">>, <<"Travel">>, <<"Coding">>],
                  [<<"Music">>], [checkboxes, <<"w-72">>], [{placeholder, <<"爱好"/utf8>>}])]).

%% Typing calls action(search, ...) below, which answers with set_items.
-spec combo_search() -> aihtml:html().
combo_search() ->
    combobox([], undefined, [<<"w-72">>],
             [{name, city}, {placeholder, <<"输入城市名，如 an"/utf8>>},
              {search, {?MODULE, search, #{}}}]).

-spec combo_disabled() -> aihtml:html().
combo_disabled() ->
    combobox(fruits(), <<"Apple">>, [disabled, <<"w-56">>], []).

%%%===================================================================
%%% Actions
%%%===================================================================

-spec action(atom(), term(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(search, _Args, #{value := Query} = Event, Ctx) ->
    Q = string:lowercase(Query),
    Found = [C || C <- cities(), string:find(string:lowercase(C), Q) =/= nomatch],
    set_items(Ctx, Event, lists:sublist(Found, 8)).

%%%===================================================================
%%% Data
%%%===================================================================

fruits() ->
    [<<"Apple">>, <<"Apricot">>, <<"Banana">>, <<"Blueberry">>, <<"Cherry">>,
     <<"Grape">>, <<"Lemon">>, <<"Mango">>, <<"Orange">>, <<"Peach">>].

cities() ->
    [<<"Amsterdam">>, <<"Athens">>, <<"Bangkok">>, <<"Beijing">>, <<"Berlin">>,
     <<"Dalian">>, <<"Istanbul">>, <<"Jakarta">>, <<"London">>, <<"Madrid">>,
     <<"Milan">>, <<"Nanjing">>, <<"Oslo">>, <<"Paris">>, <<"Santiago">>,
     <<"Seoul">>, <<"Shanghai">>, <<"Stockholm">>, <<"Tokyo">>, <<"Vienna">>].

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
