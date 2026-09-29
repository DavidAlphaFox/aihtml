%% @doc Demo data shared by the bar demos (aihtml_example_demo_activity_bar,
%% aihtml_example_demo_command): 20px line icons.
-module(aihtml_example_fixture_layout_bars).

-export([icon/1]).

%% 20px line icons (stroke follows the text colour).
-spec icon(atom()) -> {safe, iodata()}.
icon(Name) ->
    {safe, [<<"<svg viewBox=\"0 0 24 24\" width=\"20\" height=\"20\" fill=\"none\" "
              "stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" "
              "stroke-linejoin=\"round\">">>, path(Name), <<"</svg>">>]}.

path(files) -> <<"<path d=\"M14 3H6a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V9z\"/>"
                 "<path d=\"M14 3v6h6\"/>">>;
path(search) -> <<"<circle cx=\"11\" cy=\"11\" r=\"7\"/><path d=\"m20 20-3.5-3.5\"/>">>;
path(git) -> <<"<circle cx=\"6\" cy=\"6\" r=\"2.5\"/><circle cx=\"6\" cy=\"18\" r=\"2.5\"/>"
               "<circle cx=\"18\" cy=\"8\" r=\"2.5\"/><path d=\"M6 8.5v7M18 10.5c0 4-6 3-11 6\"/>">>;
path(run) -> <<"<path d=\"M7 4v16l13-8z\"/>">>;
path(list) -> <<"<path d=\"M8 6h13M8 12h13M8 18h13M3 6h.01M3 12h.01M3 18h.01\"/>">>;
path(chat) -> <<"<path d=\"M21 12a8 8 0 0 1-11.5 7.2L4 20l1-4.6A8 8 0 1 1 21 12z\"/>">>;
path(settings) -> <<"<circle cx=\"12\" cy=\"12\" r=\"3\"/><path d=\"M12 2v3M12 19v3M4.2 4.2l2.1 2.1"
                    "M17.7 17.7l2.1 2.1M2 12h3M19 12h3M4.2 19.8l2.1-2.1M17.7 6.3l2.1-2.1\"/>">>;
path(box) -> <<"<path d=\"M21 8 12 3 3 8v8l9 5 9-5z\"/><path d=\"M3 8l9 5 9-5M12 13v8\"/>">>.
