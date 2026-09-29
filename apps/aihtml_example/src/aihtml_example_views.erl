%% @doc Every view of the demo, written as aihtml calls. The full page and
%% the fragment endpoints share these functions, so a swapped fragment is
%% byte-for-byte what a full render would produce.
-module(aihtml_example_views).

-include_lib("aihtml/include/aihtml.hrl").

-export([index/0, counter_value/1, greeting/1, todo_section/1, todo_item/1]).

-spec index() -> aihtml:element().
index() ->
    'div'([top_bar(),
           main([intro(),
                 'div'([buttons_card(), controls_card(), feedback_card(), tabs_card(),
                        round_trip_card(), todos_card()],
                       [<<"grid gap-6 md:grid-cols-2">>], [])],
                [<<"mx-auto max-w-5xl px-4 py-8">>], [])],
          [<<"min-h-screen">>], []).

%%%===================================================================
%%% Page sections
%%%===================================================================

top_bar() ->
    [aihtml_example_site:topbar(fetch),
     'div'([h1(<<"片段模式"/utf8>>, [<<"text-2xl font-bold">>], []),
            theme_switcher([], [])],
           [<<"mx-auto max-w-5xl px-4 pt-8 flex flex-wrap items-end justify-between gap-4">>], [])].

intro() ->
    section([p([<<"Each part of this page is an Erlang function call that renders "
                  "HTML. Prefabs emit semantic classes. The four theme axes above "
                  "restyle them through CSS variables, and Tailwind utilities "
                  "handle layout.">>],
               [<<"text-muted max-w-3xl">>], [])],
            [<<"mb-8">>], []).

buttons_card() ->
    card([row([button(<<"Primary">>, primary, [], []),
               button(<<"Secondary">>, secondary, [secondary], []),
               button(<<"Outline">>, outline, [outlined], []),
               button(<<"Ghost">>, ghost, [borderless], []),
               button(<<"Danger">>, danger, [error], [])]),
          row([button(<<"Small">>, s, [sm], []),
               button(<<"Large">>, l, [lg, outlined], []),
               button(<<"Disabled">>, d, [], [{disabled, true}])],
              [<<"mt-4">>]),
          button(<<"Block">>, b, [secondary, <<"w-full mt-4">>], [])],
         [], [{title, <<"Buttons">>}]).

controls_card() ->
    card(['div'([field(<<"Name">>, input(<<>>, [], [{id, name}, {name, name},
                                                   {placeholder, <<"Ada">>}]),
                       [top], [{for, name}, {help, <<"Shown to other users.">>}]),
                 field(<<"Email">>, input(<<"not-an-email">>, [invalid], [{id, email},
                                                                          {type, email}]),
                       [top], [{for, email}, {error, <<"Enter a valid address.">>}]),
                 field(<<"Role">>, select([{dev, <<"Developer">>}, {ops, <<"Operations">>},
                                           {pm, <<"Product">>}],
                                          ops, [], [{id, role}, {name, role}]),
                       [top], [{for, role}]),
                 field(<<"Bio">>, textarea(<<>>, [], [{id, bio}, {rows, 3}]),
                       [top], [{for, bio}]),
                 row([checkbox(<<"Remember me">>, yes, [], [{name, remember}, {checked, true}]),
                      radiobutton(<<"Monthly">>, monthly, [], [{name, plan}, {checked, true}]),
                      radiobutton(<<"Yearly">>, yearly, [], [{name, plan}]),
                      switch_button(<<"Notifications">>, on, [], [{name, notify}])])],
                [<<"flex flex-col gap-4">>], [])],
         [], [{title, <<"Form controls">>}]).

feedback_card() ->
    card(['div'([alert(<<"An informational message.">>, [], []),
                 alert(<<"Saved successfully.">>, [success, dismissible], []),
                 alert(<<"Your trial ends in three days.">>, [warning], []),
                 alert(<<"Something went wrong.">>, [error, dismissible], []),
                 row([chip(<<"neutral">>, [soft], []), chip(<<"primary">>, [primary, soft], []),
                      chip(<<"success">>, [success, soft], []), chip(<<"warning">>, [warning, soft], []),
                      chip(<<"error">>, [error, soft], [])])],
                [<<"flex flex-col gap-3">>], [])],
         [], [{title, <<"Feedback">>}]).

tabs_card() ->
    card([tabs([{overview, <<"Overview">>,
                 p(<<"Tabs switch on the client. Every panel was rendered by the server.">>)},
                {axes, <<"Four axes">>,
                 ul([li([strong(<<"appearance">>), <<": neutrals, light or dark">>]),
                     li([strong(<<"palette">>), <<": accent colours">>]),
                     li([strong(<<"typography">>), <<": font families">>]),
                     li([strong(<<"skin">>), <<": radius, borders, shadows">>])],
                    [<<"list-disc pl-5 space-y-1">>], [])},
                {code, <<"Code">>,
                 pre(code(<<"button(<<\"Save\">>, save, [primary, <<\"mt-4\">>], [])">>),
                     [<<"font-mono text-sm bg-surface-2 rounded-control p-3 overflow-x-auto">>],
                     [])}],
               overview, [], [{id, demo_tabs}])],
         [], [{title, <<"Tabs">>}]).

round_trip_card() ->
    card(['div'([row([button(<<"Count on the server">>, bump, [outlined],
                             [fetch(post, <<"/counter">>, <<"#counter">>)]),
                      counter_value(aihtml_example_store:counter())]),
                 field(<<"Live greeting">>,
                       input(<<>>, [], [{id, greet}, {name, name},
                                        {placeholder, <<"Type a name">>},
                                        fetch(get, <<"/greet">>, <<"#greeting">>,
                                              #{trigger => input})]),
                       [top], [{for, greet}]),
                 greeting(<<>>)],
                [<<"flex flex-col gap-4">>], [])],
         [], [{title, <<"Server round trips">>}]).

todos_card() ->
    card(['div'(todo_section(undefined), [], [{id, todos}])],
         [<<"md:col-span-2">>], [{title, <<"Todos">>}]).

%%%===================================================================
%%% Fragments, also returned by aihtml_example_api
%%%===================================================================

-spec counter_value(integer()) -> aihtml:element().
counter_value(N) ->
    span([<<"Clicks: ">>, strong(N)], [<<"text-muted">>], [{id, counter}]).

-spec greeting(binary()) -> aihtml:element().
greeting(<<>>) ->
    p(<<"Hello, stranger.">>, [<<"text-muted">>], [{id, greeting}]);
greeting(Name) ->
    %% Name is user input: it is escaped like every other text child.
    p([<<"Hello, ">>, strong(Name), <<"!">>], [], [{id, greeting}]).

-spec todo_section(binary() | undefined) -> aihtml:html().
todo_section(Error) ->
    Todos = aihtml_example_store:todos(),
    Done = length([T || #{done := true} = T <- Todos]),
    [form(field(<<"New todo">>,
                'div'([input(<<>>, [], [{id, todo_text}, {name, text},
                                        {placeholder, <<"What needs doing?">>},
                                        {autocomplete, off}]),
                       button(<<"Add">>, add, [], [{type, submit}])],
                      [<<"flex gap-2">>], []),
                [top], [{for, todo_text}, {error, Error}]),
          [], [fetch(post, <<"/todos">>, <<"#todos">>)]),
     p([chip([Done, <<" / ">>, length(Todos), <<" done">>], [primary, soft], [])],
       [<<"mt-4">>], []),
     ul([todo_item(T) || T <- Todos], [<<"mt-3 divide-y divide-line">>], [])].

-spec todo_item(aihtml_example_store:todo()) -> aihtml:element().
todo_item(#{id := Id, text := Text, done := Done}) ->
    IdB = integer_to_binary(Id),
    li([checkbox(Text, IdB, [<<"flex-1">>, [<<"line-through text-muted">> || Done]],
                 [{checked, Done},
                  fetch(post, <<"/todos/", IdB/binary, "/toggle">>, <<"#todos">>)]),
        button(<<"Delete">>, IdB, [borderless, sm],
               [fetch(delete, <<"/todos/", IdB/binary>>, <<"#todos">>,
                      #{confirm => <<"Delete this todo?">>})])],
       [<<"flex items-center gap-3 py-2">>], [{id, <<"todo-", IdB/binary>>}]).

%%%===================================================================
%%% Local layout helpers
%%%===================================================================

row(Children) -> row(Children, []).
row(Children, Css) -> 'div'(Children, [<<"flex flex-wrap items-center gap-3">>, Css], []).
