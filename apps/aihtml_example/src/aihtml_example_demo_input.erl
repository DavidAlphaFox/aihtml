%% @doc Demos of the text input (aihtml_input), shown on
%% /components/input. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_input).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([input_sizes/0, input_states/0, input_addons/0, input_clearable/0, input_label/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => input, title => <<"Input">>,
       summary => <<"单行文本输入，支持尺寸、校验状态、前后缀、清除按钮和浮动标签。"/utf8>>,
       demos => [{<<"尺寸"/utf8>>, input_sizes},
                 {<<"校验状态与禁用"/utf8>>, input_states},
                 {<<"前缀与后缀"/utf8>>, input_addons},
                 {<<"可清除"/utf8>>, input_clearable},
                 {<<"浮动标签"/utf8>>, input_label}]}].

-spec input_sizes() -> aihtml:html().
input_sizes() ->
    row([ah_input(undefined, [sm, <<"w-56">>], [{placeholder, <<"Small">>}]),
         ah_input(undefined, [<<"w-56">>], [{placeholder, <<"Medium">>}, {name, q}]),
         ah_input(undefined, [lg, <<"w-56">>], [{placeholder, <<"Large">>}])]).

-spec input_states() -> aihtml:html().
input_states() ->
    row([ah_input(<<"not-an-email">>, [invalid, <<"w-56">>], [{name, email}]),
         ah_input(<<"ada@example.com">>, [valid, <<"w-56">>], []),
         ah_input(<<"Disabled">>, [disabled, <<"w-56">>], []),
         ah_input(undefined, [no_rounded, <<"w-56">>], [{placeholder, <<"No rounding">>}])]).

-spec input_addons() -> aihtml:html().
input_addons() ->
    row([ah_input(undefined, [<<"w-64">>], [{prefix, <<"https://">>},
                                            {placeholder, <<"example.com">>}]),
         ah_input(<<"42">>, [<<"w-40">>], [{suffix, <<"kg">>}]),
         ah_input(<<"19.99">>, [<<"w-48">>], [{prefix, <<"$">>}, {suffix, <<"USD">>}])]).

-spec input_clearable() -> aihtml:html().
input_clearable() ->
    Search = safe(<<"<svg width=\"14\" height=\"14\" viewBox=\"0 0 24 24\" fill=\"none\" "
                    "stroke=\"currentColor\" stroke-width=\"2\"><circle cx=\"11\" cy=\"11\" "
                    "r=\"7\"/><path d=\"m20 20-3.5-3.5\"/></svg>">>),
    row([ah_input(undefined, [clearable, <<"w-64">>], [{prefix, Search},
                                                      {placeholder, <<"Search">>}]),
         ah_input(<<"Clear me">>, [clearable, <<"w-56">>], [])]).

-spec input_label() -> aihtml:html().
input_label() ->
    row([ah_input(undefined, [<<"w-56">>], [{label, <<"Full name">>}]),
         ah_input(<<"Ada Lovelace">>, [<<"w-56">>], [{label, <<"Full name">>}])]).

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-start gap-4">>], []).
