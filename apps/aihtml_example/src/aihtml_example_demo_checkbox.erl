%% @doc Demos of the checkbox (aihtml_checkbox), shown on
%% /components/checkbox. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_checkbox).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([checkbox_states/0, checkbox_sizes/0, checkbox_three_states/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => checkbox, title => <<"Checkbox">>,
       summary => <<"复选框，支持半选、三态循环和锁定。"/utf8>>,
       demos => [{<<"状态"/utf8>>, checkbox_states},
                 {<<"尺寸"/utf8>>, checkbox_sizes},
                 {<<"三态与锁定"/utf8>>, checkbox_three_states}]}].

-spec checkbox_states() -> aihtml:html().
checkbox_states() ->
    row([checkbox(<<"Unchecked">>, undefined, [], [{name, a}]),
         checkbox(<<"Checked">>, undefined, [], [{name, b}, {checked, true}]),
         checkbox(<<"Indeterminate">>, undefined, [], [{indeterminate, true}]),
         checkbox(<<"Disabled">>, undefined, [], [{disabled, true}]),
         checkbox(<<"Disabled checked">>, undefined, [], [{disabled, true}, {checked, true}])]).

-spec checkbox_sizes() -> aihtml:html().
checkbox_sizes() ->
    row([checkbox(<<"Small">>, undefined, [sm], [{checked, true}]),
         checkbox(<<"Medium">>, undefined, [md], [{checked, true}]),
         checkbox(<<"Large">>, undefined, [lg], [{checked, true}]),
         checkbox(<<"24px box">>, undefined, [], [{box_size, 24}, {checked, true}])]).

-spec checkbox_three_states() -> aihtml:html().
checkbox_three_states() ->
    row([checkbox(<<"Click me: on, mixed, off">>, undefined, [],
                  [{three_states, true}, {checked, true}]),
         checkbox(<<"Locked">>, undefined, [], [{locked, true}, {checked, true}])]).

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-6">>], []).
