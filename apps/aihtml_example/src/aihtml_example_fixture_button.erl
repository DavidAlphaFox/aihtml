%% @doc Shared by the demos of the button components
%% (aihtml_example_demo_button, _link_button, _toggle_button,
%% _button_group, _segmented_control, _dropdown_button, _split_button):
%% the row around the examples and the menu of the menu demos.
-module(aihtml_example_fixture_button).

-include_lib("aihtml/include/aihtml.hrl").

-export([row/1, menu/0]).

-spec row(aihtml:html()) -> aihtml:html().
row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-center gap-3">>], []).

%% Shared by the menu demos.
-spec menu() -> [aihtml_lib_button:item()].
menu() ->
    [{draft, <<"Save as draft">>}, {copy, <<"Save a copy">>}, divider,
     {template, <<"Save as template">>},
     {locked, <<"Publish">>, [{disabled, true}]}].
