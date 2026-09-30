%% @doc Every view of the demo, written as aihtml calls. The full page and
%% the fragment endpoints share these functions, so a swapped fragment is
%% byte-for-byte what a full render would produce.
-module(aihtml_example_views).

-include_lib("aihtml/include/aihtml.hrl").

-export([index/0, counter_value/1, greeting/1, todo_section/1, todo_item/1]).

-spec index() -> aihtml:element().
index() ->
    ah_div([top_bar(),
            ah_main([intro(),
                     ah_div([buttons_card(), controls_card(), feedback_card(), tabs_card(),
                             round_trip_card(), todos_card()],
                            [<<"grid gap-6 md:grid-cols-2">>], [])],
                    [<<"mx-auto max-w-5xl px-4 py-8">>], [])],
           [<<"min-h-screen">>], []).

%%%===================================================================
%%% Page sections
%%%===================================================================

top_bar() ->
    [aihtml_example_site:topbar(fetch),
     ah_div([ah_h1(<<"片段模式"/utf8>>, [<<"text-2xl font-bold">>], []),
             ah_theme_switcher([], [])],
            [<<"mx-auto max-w-5xl px-4 pt-8 flex flex-wrap items-end justify-between gap-4">>], [])].

intro() ->
    ah_section([ah_p([<<"Each part of this page is an Erlang function call that renders "
                        "HTML. Prefabs emit semantic classes. The four theme axes above "
                        "restyle them through CSS variables, and Tailwind utilities "
                        "handle layout.">>],
                     [<<"text-muted max-w-3xl">>], [])],
               [<<"mb-8">>], []).

buttons_card() ->
    ah_card([row([ah_button(<<"Primary">>, primary, [], []),
                  ah_button(<<"Secondary">>, secondary, [secondary], []),
                  ah_button(<<"Outline">>, outline, [outlined], []),
                  ah_button(<<"Ghost">>, ghost, [borderless], []),
                  ah_button(<<"Danger">>, danger, [error], [])]),
             row([ah_button(<<"Small">>, s, [sm], []),
                  ah_button(<<"Large">>, l, [lg, outlined], []),
                  ah_button(<<"Disabled">>, d, [], [{disabled, true}])],
                 [<<"mt-4">>]),
             ah_button(<<"Block">>, b, [secondary, <<"w-full mt-4">>], [])],
            [], [{title, <<"Buttons">>}]).

controls_card() ->
    ah_card([ah_div([ah_field(<<"Name">>, ah_input(<<>>, [], [{id, name}, {name, name},
                                                             {placeholder, <<"Ada">>}]),
                              [top], [{for, name}, {help, <<"Shown to other users.">>}]),
                     ah_field(<<"Email">>, ah_input(<<"not-an-email">>, [invalid], [{id, email},
                                                                                    {type, email}]),
                              [top], [{for, email}, {error, <<"Enter a valid address.">>}]),
                     ah_field(<<"Role">>, ah_select([{dev, <<"Developer">>}, {ops, <<"Operations">>},
                                                     {pm, <<"Product">>}],
                                                    ops, [], [{id, role}, {name, role}]),
                              [top], [{for, role}]),
                     ah_field(<<"Bio">>, ah_textarea(<<>>, [], [{id, bio}, {rows, 3}]),
                              [top], [{for, bio}]),
                     row([ah_checkbox(<<"Remember me">>, yes, [], [{name, remember}, {checked, true}]),
                          ah_radiobutton(<<"Monthly">>, monthly, [], [{name, plan}, {checked, true}]),
                          ah_radiobutton(<<"Yearly">>, yearly, [], [{name, plan}]),
                          ah_switch_button(<<"Notifications">>, on, [], [{name, notify}])])],
                    [<<"flex flex-col gap-4">>], [])],
            [], [{title, <<"Form controls">>}]).

feedback_card() ->
    ah_card([ah_div([ah_alert(<<"An informational message.">>, [], []),
                     ah_alert(<<"Saved successfully.">>, [success, dismissible], []),
                     ah_alert(<<"Your trial ends in three days.">>, [warning], []),
                     ah_alert(<<"Something went wrong.">>, [error, dismissible], []),
                     row([ah_chip(<<"neutral">>, [soft], []), ah_chip(<<"primary">>, [primary, soft], []),
                          ah_chip(<<"success">>, [success, soft], []), ah_chip(<<"warning">>, [warning, soft], []),
                          ah_chip(<<"error">>, [error, soft], [])])],
                    [<<"flex flex-col gap-3">>], [])],
            [], [{title, <<"Feedback">>}]).

tabs_card() ->
    ah_card([ah_tabs([{overview, <<"Overview">>,
                       ah_p(<<"Tabs switch on the client. Every panel was rendered by the server.">>)},
                      {axes, <<"Four axes">>,
                       ah_ul([ah_li([ah_strong(<<"appearance">>), <<": neutrals, light or dark">>]),
                              ah_li([ah_strong(<<"palette">>), <<": accent colours">>]),
                              ah_li([ah_strong(<<"typography">>), <<": font families">>]),
                              ah_li([ah_strong(<<"skin">>), <<": radius, borders, shadows">>])],
                             [<<"list-disc pl-5 space-y-1">>], [])},
                      {code, <<"Code">>,
                       ah_pre(ah_code(<<"ah_button(<<\"Save\">>, save, [primary, <<\"mt-4\">>], [])">>),
                              [<<"font-mono text-sm bg-surface-2 rounded-control p-3 overflow-x-auto">>],
                              [])}],
                     overview, [], [{id, demo_tabs}])],
            [], [{title, <<"Tabs">>}]).

round_trip_card() ->
    ah_card([ah_div([row([ah_button(<<"Count on the server">>, bump, [outlined],
                                    [fetch(post, <<"/counter">>, <<"#counter">>)]),
                          counter_value(aihtml_example_store:counter())]),
                     ah_field(<<"Live greeting">>,
                              ah_input(<<>>, [], [{id, greet}, {name, name},
                                                  {placeholder, <<"Type a name">>},
                                                  fetch(get, <<"/greet">>, <<"#greeting">>,
                                                        #{trigger => input})]),
                              [top], [{for, greet}]),
                     greeting(<<>>)],
                    [<<"flex flex-col gap-4">>], [])],
            [], [{title, <<"Server round trips">>}]).

todos_card() ->
    ah_card([ah_div(todo_section(undefined), [], [{id, todos}])],
            [<<"md:col-span-2">>], [{title, <<"Todos">>}]).

%%%===================================================================
%%% Fragments, also returned by aihtml_example_api
%%%===================================================================

-spec counter_value(integer()) -> aihtml:element().
counter_value(N) ->
    ah_span([<<"Clicks: ">>, ah_strong(N)], [<<"text-muted">>], [{id, counter}]).

-spec greeting(binary()) -> aihtml:element().
greeting(<<>>) ->
    ah_p(<<"Hello, stranger.">>, [<<"text-muted">>], [{id, greeting}]);
greeting(Name) ->
    %% Name is user input: it is escaped like every other text child.
    ah_p([<<"Hello, ">>, ah_strong(Name), <<"!">>], [], [{id, greeting}]).

-spec todo_section(binary() | undefined) -> aihtml:html().
todo_section(Error) ->
    Todos = aihtml_example_store:todos(),
    Done = length([T || #{done := true} = T <- Todos]),
    [ah_form(ah_field(<<"New todo">>,
                      ah_div([ah_input(<<>>, [], [{id, todo_text}, {name, text},
                                                  {placeholder, <<"What needs doing?">>},
                                                  {autocomplete, off}]),
                              ah_button(<<"Add">>, add, [], [{type, submit}])],
                             [<<"flex gap-2">>], []),
                      [top], [{for, todo_text}, {error, Error}]),
             [], [fetch(post, <<"/todos">>, <<"#todos">>)]),
     ah_p([ah_chip([Done, <<" / ">>, length(Todos), <<" done">>], [primary, soft], [])],
          [<<"mt-4">>], []),
     ah_ul([todo_item(T) || T <- Todos], [<<"mt-3 divide-y divide-line">>], [])].

-spec todo_item(aihtml_example_store:todo()) -> aihtml:element().
todo_item(#{id := Id, text := Text, done := Done}) ->
    IdB = integer_to_binary(Id),
    ah_li([ah_checkbox(Text, IdB, [<<"flex-1">>, [<<"line-through text-muted">> || Done]],
                       [{checked, Done},
                        fetch(post, <<"/todos/", IdB/binary, "/toggle">>, <<"#todos">>)]),
           ah_button(<<"Delete">>, IdB, [borderless, sm],
                     [fetch(delete, <<"/todos/", IdB/binary>>, <<"#todos">>,
                            #{confirm => <<"Delete this todo?">>})])],
          [<<"flex items-center gap-3 py-2">>], [{id, <<"todo-", IdB/binary>>}]).

%%%===================================================================
%%% Local layout helpers
%%%===================================================================

row(Children) -> row(Children, []).
row(Children, Css) -> ah_div(Children, [<<"flex flex-wrap items-center gap-3">>, Css], []).
