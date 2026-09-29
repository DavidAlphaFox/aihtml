%% @doc Demos of steps (aihtml_steps), shown on /components/steps. Each
%% function is one example, written the way an application writes it;
%% the docs page prints its source under it.
-module(aihtml_example_demo_steps).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([steps_basic/0, steps_wizard/0, steps_vertical/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => steps, title => <<"Steps">>,
       summary => <<"分步流程指示，值为当前步骤的序号。"/utf8>>,
       demos => [{<<"步骤与说明"/utf8>>, steps_basic},
                 {<<"带内容面板和上一步、下一步"/utf8>>, steps_wizard},
                 {<<"竖排、出错与禁用的步骤"/utf8>>, steps_vertical}]}].

-spec steps_basic() -> aihtml:html().
steps_basic() ->
    steps([{<<"Account">>, <<"Create an account">>}, {<<"Profile">>, <<"Your details">>},
           {<<"Confirm">>, <<"Check and submit">>}], 1, [], []).

-spec steps_wizard() -> aihtml:html().
steps_wizard() ->
    steps([#{title => <<"Cart">>, content => p(<<"Your cart.">>)},
           #{title => <<"Shipping">>, content => p(<<"Shipping address.">>)},
           #{title => <<"Payment">>, content => p(<<"Payment method.">>)},
           #{title => <<"Done">>, content => p(<<"Thank you.">>)}],
          0, [], [{name, step}]).

-spec steps_vertical() -> aihtml:html().
steps_vertical() ->
    steps([{<<"Draft">>, <<"Written">>},
           #{title => <<"Review">>, status => error, description => <<"Changes requested">>},
           #{title => <<"Publish">>, disabled => true}],
          1, [vertical], [{clickable, false}]).
