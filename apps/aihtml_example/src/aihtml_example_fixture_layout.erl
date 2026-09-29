%% @doc Layout helpers shared by the demos of the layout components
%% (aihtml_example_demo_card, _expander, _tabs, _breadcrumbs, _skeleton,
%% _loader): rows, columns and fixed-size boxes around the examples.
-module(aihtml_example_fixture_layout).

-include_lib("aihtml/include/aihtml.hrl").

-export([row/1, col/1, box/1, frame/1]).

-spec row(aihtml:html()) -> aihtml:html().
row(Children) ->
    'div'(Children, [<<"flex flex-wrap items-start gap-4">>], []).

-spec col(aihtml:html()) -> aihtml:html().
col(Children) ->
    'div'(Children, [<<"flex flex-col gap-2">>], []).

-spec box(aihtml:html()) -> aihtml:html().
box(Child) ->
    'div'(Child, [<<"w-64">>], []).

-spec frame(aihtml:html()) -> aihtml:html().
frame(Children) ->
    'div'(Children, [<<"relative w-56 h-32 p-3 border border-line rounded">>], []).
