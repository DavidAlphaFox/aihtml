%% @doc Demos of the password input (aihtml_password_input), shown on
%% /components/password_input. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_password_input).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([password_basic/0, password_strength/0, password_states/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => password_input, title => <<"PasswordInput">>,
       summary => <<"密码输入框，可切换明文显示，可显示强度。"/utf8>>,
       demos => [{<<"显示与隐藏"/utf8>>, password_basic},
                 {<<"强度指示"/utf8>>, password_strength},
                 {<<"尺寸与状态"/utf8>>, password_states}]}].

-spec password_basic() -> aihtml:html().
password_basic() ->
    row([password_input(<<"secret123">>, [<<"w-60">>], [{name, password}]),
         password_input(undefined, [<<"w-60">>], [{label, <<"Password">>}]),
         password_input(undefined, [<<"w-60">>], [{toggle, false},
                                                 {placeholder, <<"No toggle">>}])]).

-spec password_strength() -> aihtml:html().
password_strength() ->
    password_input(undefined, [<<"w-72">>], [{strength, true}, {name, new_password},
                                             {placeholder, <<"New password">>}]).

-spec password_states() -> aihtml:html().
password_states() ->
    row([password_input(<<"x">>, [sm, invalid, <<"w-60">>], []),
         password_input(undefined, [lg, <<"w-60">>], [{placeholder, <<"Large">>}]),
         password_input(<<"hunter2">>, [disabled, <<"w-60">>], [])]).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).
