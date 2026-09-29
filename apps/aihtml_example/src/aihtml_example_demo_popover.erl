%% @doc Demos of the popover (aihtml_popover), shown on
%% /components/popover. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_popover).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([popover_basic/0, popover_positions/0, popover_modal/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => popover, title => <<"Popover">>,
       summary => <<"锚定在触发元素上的气泡卡片，可放任意内容。"/utf8>>,
       demos => [{<<"标题与关闭按钮"/utf8>>, popover_basic},
                 {<<"方向"/utf8>>, popover_positions},
                 {<<"模态：带遮罩，点外部不关"/utf8>>, popover_modal}]}].

%%%===================================================================
%%% Popover
%%%===================================================================

-spec popover_basic() -> aihtml:html().
popover_basic() ->
    row([button(<<"Show details">>, undefined, [primary], toggles({id, <<"pop-details">>})),
         popover([p(<<"Popovers hold any content: text, links, small forms.">>,
                    [<<"mb-2">>], []),
                  button(<<"Got it">>, undefined, [sm], closes())],
                 [], [{id, <<"pop-details">>}, {title, <<"Details">>},
                      {closable, true}, {width, 260}])]).

-spec popover_positions() -> aihtml:html().
popover_positions() ->
    row([button(<<"Top">>, undefined, [outlined], toggles({id, <<"pop-top">>})),
         popover(<<"Above its trigger.">>, [top], [{id, <<"pop-top">>}]),
         button(<<"Right">>, undefined, [outlined], toggles({id, <<"pop-right">>})),
         popover(<<"To the right, without an arrow.">>, [right, no_arrow],
                 [{id, <<"pop-right">>}])]).

-spec popover_modal() -> aihtml:html().
popover_modal() ->
    row([button(<<"Confirm">>, undefined, [warning], opens({id, <<"pop-modal">>})),
         popover([p(<<"Outside clicks do not close this one.">>, [<<"mb-2">>], []),
                  button(<<"OK">>, undefined, [sm, primary], closes())],
                 [], [{id, <<"pop-modal">>}, {title, <<"Modal popover">>}, {modal, true}])]).

%%%===================================================================
%%% Helpers
%%%===================================================================

row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-center gap-3">>], []).
