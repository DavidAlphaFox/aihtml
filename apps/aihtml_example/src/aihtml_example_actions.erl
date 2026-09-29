%% @doc The action demo. GET / renders the page from the data layer; every
%% button, input and form on it calls action/4 below through one stateless
%% POST, which may land on any node. The counter, the todo list and the
%% clock also follow push topics, so other open pages update live.
-module(aihtml_example_actions).
-behaviour(aihtml_action).

-include_lib("aihtml/include/aihtml.hrl").

-export([init/2, action/4]).

%%%===================================================================
%%% Page
%%%===================================================================

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, State) ->
    {ok, aihtml_cowboy:reply(Req, page(), #{title => <<"aihtml actions">>,
                                            css => [<<"/static/example.css">>]}),
     State}.

page() ->
    'div'([top_bar(),
           main(['div'([counter_card(), greeting_card(), data_card(), include_card(),
                        todos_card()],
                       [<<"grid gap-6 md:grid-cols-2">>], [])],
                [<<"mx-auto max-w-5xl px-4 py-8">>], [])],
          [<<"min-h-screen">>], []).

top_bar() ->
    header('div'(['div'([h1(<<"aihtml actions">>, [<<"text-xl font-bold font-heading">>], []),
                         p([<<"Server time ">>,
                            span(aihtml_example_clock:now_text(), [<<"font-mono">>],
                                 [{id, clock}, subscribe(clock)]),
                            <<" · One stateless request per event · "/utf8>>,
                            a(<<"fragment demo">>, [<<"underline">>], [{href, <<"/fetch">>}])],
                           [<<"text-sm text-muted">>], [])]),
                  theme_switcher([], [])],
                 [<<"mx-auto max-w-5xl px-4 py-4 flex flex-wrap items-end justify-between gap-4">>],
                 []),
           [<<"bg-surface border-b border-line">>], []).

%% The count lives in the data layer, so every visitor shares it.
counter_card() ->
    card([row([button(<<"+1">>, inc, [], [on(click, {?MODULE, bump, #{}})]),
               button(<<"Reset">>, reset, [ghost], [on(click, {?MODULE, reset, #{}})]),
               span([<<"Count: ">>,
                     strong(aihtml_example_store:counter(), [],
                            [{id, count},
                             subscribe(counter, #{refresh => {?MODULE, refresh_counter, #{}}})])],
                    [<<"text-muted">>], [])]),
          p(<<"Stored in the data layer and pushed to every open page.">>,
            [<<"mt-3 text-sm text-muted">>], [])],
         [], [{title, <<"Click handled in Erlang">>}]).

greeting_card() ->
    card(['div'([field(<<"Your name">>,
                       input(<<>>, [], [{id, name}, {placeholder, <<"Type a name">>},
                                        {autocomplete, off},
                                        on(input, {?MODULE, greet, #{}}, #{debounce => 150})]),
                       [], [{for, name}]),
                 p(<<"Hello, stranger.">>, [<<"text-muted">>], [{id, greeting}])],
                [<<"flex flex-col gap-3">>], [])],
         [], [{title, <<"Input events">>}]).

data_card() ->
    card([row([button(<<"Load processes">>, load, [outline],
                      [on(click, {?MODULE, load_processes, #{}})]),
               button(<<"Load system info">>, info, [ghost],
                      [on(click, {?MODULE, load_system, #{}})])]),
          'div'(p(<<"Nothing loaded yet.">>, [<<"text-muted text-sm">>], []),
                [<<"mt-4">>], [{id, data}])],
         [], [{title, <<"Load data, progressively">>}]).

%% The browser sends the values of other controls along with the event.
include_card() ->
    card([row([input(<<"2">>, [sm, <<"w-20">>], [{id, a}, {type, number}]),
               span(<<"+">>),
               input(<<"3">>, [sm, <<"w-20">>], [{id, b}, {type, number}]),
               button(<<"=">>, sum, [secondary, sm],
                      [on(click, {?MODULE, sum, #{}}, #{include => [{id, a}, {id, b}]})]),
               strong(<<"?">>, [], [{id, sum}])]),
          row([button(<<"Dark mode from Erlang">>, dark, [ghost],
                      [on(click, {?MODULE, dark, #{}})])])],
         [], [{title, <<"Values and scripts">>}]).

todos_card() ->
    card([form(field(<<"New todo">>,
                     'div'([input(<<>>, [], [{id, todo_text}, {name, text},
                                             {placeholder, <<"What needs doing?">>},
                                             {autocomplete, off}]),
                            button(<<"Add">>, add, [], [{type, submit}])],
                           [<<"flex gap-2">>], []),
                     [], [{for, todo_text}]),
               [], [on(submit, {?MODULE, add_todo, #{}})]),
          ul([todo_item(T) || T <- aihtml_example_store:todos()],
             [<<"mt-3 divide-y divide-line">>],
             [{id, todo_list},
              subscribe(todos, #{refresh => {?MODULE, refresh_todos, #{}}})])],
         [<<"md:col-span-2">>], [{title, <<"Todos in the data layer">>}]).

todo_item(#{id := Id, text := Text, done := Done}) ->
    li([checkbox(Text, Id, [<<"flex-1">>, [<<"line-through text-muted">> || Done]],
                 [{checked, Done}, on(change, {?MODULE, toggle_todo, #{id => Id}})]),
        button(<<"Delete">>, Id, [ghost, sm],
               [on(click, {?MODULE, delete_todo, #{id => Id}},
                   #{confirm => <<"Delete this todo?">>})])],
       [<<"flex items-center gap-3 py-2">>], [{id, todo_dom(Id)}]).

%%%===================================================================
%%% Actions: stateless, everything comes from the event and the data layer
%%%===================================================================

-spec action(atom(), map(), aihtml_action:event(), aihtml_action:ctx()) -> ok.
action(bump, _, _Ev, Ctx) ->
    show_count(Ctx, aihtml_example_store:bump());
action(reset, _, _Ev, Ctx) ->
    ok = aihtml_example_store:reset(),
    show_count(Ctx, 0);
action(refresh_counter, _, _Ev, Ctx) ->
    aihtml_action:html(Ctx, {id, count}, aihtml_example_store:counter());
action(greet, _, #{value := V}, Ctx) ->
    Html = case string:trim(V) of
               <<>> -> <<"Hello, stranger.">>;
               Name -> [<<"Hello, ">>, strong(Name), <<"!">>]    % escaped
           end,
    aihtml_action:html(Ctx, {id, greeting}, Html);
action(load_processes, _, _Ev, Ctx) ->
    loading(Ctx),
    Top = lists:sublist(
            lists:reverse(lists:keysort(2, [{P, M} || P <- erlang:processes(),
                                                     {memory, M} <- [erlang:process_info(P, memory)]])),
            8),
    Rows = [tr([td(pid_to_list(P), [<<"font-mono py-1">>], []),
                td(proc_name(P), [<<"py-1">>], []),
                td(M div 1024, [<<"py-1 text-right">>], [])])
            || {P, M} <- Top],
    aihtml_action:html(Ctx, {id, data},
                       table([thead(tr([th(<<"pid">>, [<<"text-left">>], []),
                                        th(<<"name">>, [<<"text-left">>], []),
                                        th(<<"KiB">>, [<<"text-right">>], [])])),
                              tbody(Rows)],
                             [<<"w-full text-sm">>], []));
action(load_system, _, _Ev, Ctx) ->
    loading(Ctx),
    Items = [{<<"Node">>, node()},
             {<<"OTP">>, erlang:system_info(otp_release)},
             {<<"Schedulers">>, erlang:system_info(schedulers_online)},
             {<<"Processes">>, erlang:system_info(process_count)},
             {<<"Memory (MiB)">>, erlang:memory(total) div (1024 * 1024)},
             {<<"Request process">>, pid_to_list(self())}],
    aihtml_action:html(Ctx, {id, data},
                       dl([[dt(K, [<<"text-muted">>], []), dd(V, [<<"font-mono">>], [])]
                           || {K, V} <- Items],
                          [<<"grid grid-cols-2 gap-y-1 text-sm">>], []));
action(sum, _, #{values := Vs}, Ctx) ->
    Html = try
               binary_to_integer(maps:get(<<"a">>, Vs)) + binary_to_integer(maps:get(<<"b">>, Vs))
           catch
               error:_ -> <<"not a number">>
           end,
    aihtml_action:html(Ctx, {id, sum}, Html);
action(dark, _, _Ev, Ctx) ->
    aihtml_action:js(Ctx, <<"AH.theme.set('appearance', 'dark')">>);
action(add_todo, _, #{form := Form}, Ctx) ->
    case string:trim(maps:get(<<"text">>, Form, <<>>)) of
        <<>> ->
            aihtml_action:focus(Ctx, {id, todo_text});
        Text ->
            Todo = aihtml_example_store:add_todo(unicode:characters_to_binary(Text)),
            aihtml_action:html(Ctx, {id, todo_list}, todo_item(Todo), append),
            %% every other open page appends it too
            aihtml_push:publish(todos, fun(C) ->
                                           aihtml_action:html(C, {id, todo_list}, todo_item(Todo), append)
                                       end, #{except => Ctx}),
            aihtml_action:set_value(Ctx, {id, todo_text}, <<>>),
            aihtml_action:focus(Ctx, {id, todo_text})
    end;
action(toggle_todo, #{id := Id}, _Ev, Ctx) ->
    Show = case aihtml_example_store:toggle_todo(Id) of
               undefined -> fun(C) -> aihtml_action:remove(C, {id, todo_dom(Id)}) end;
               Todo -> fun(C) -> aihtml_action:html(C, {id, todo_dom(Id)}, todo_item(Todo), outer) end
           end,
    Show(Ctx),
    aihtml_push:publish(todos, Show, #{except => Ctx});
action(delete_todo, #{id := Id}, _Ev, Ctx) ->
    ok = aihtml_example_store:delete_todo(Id),
    Remove = fun(C) -> aihtml_action:remove(C, {id, todo_dom(Id)}) end,
    Remove(Ctx),
    aihtml_push:publish(todos, Remove, #{except => Ctx});
action(refresh_todos, _, _Ev, Ctx) ->
    aihtml_action:html(Ctx, {id, todo_list},
                       [todo_item(T) || T <- aihtml_example_store:todos()]).

%% Update this page through the response and every other page by push.
show_count(Ctx, N) ->
    aihtml_action:html(Ctx, {id, count}, N),
    aihtml_push:publish(counter, fun(C) -> aihtml_action:html(C, {id, count}, N) end,
                        #{except => Ctx}).

%% Progressive update: the loading state is sent at once, the data when
%% the (here artificially slow) query returns, in the same response.
loading(Ctx) ->
    aihtml_action:html(Ctx, {id, data}, p(<<"Loading…"/utf8>>, [<<"text-muted text-sm">>], [])),
    aihtml_action:flush(Ctx),
    timer:sleep(400).

%%%===================================================================
%%% Helpers
%%%===================================================================

todo_dom(Id) -> <<"todo-", (integer_to_binary(Id))/binary>>.

proc_name(P) ->
    case erlang:process_info(P, registered_name) of
        {registered_name, N} -> N;
        _ -> <<>>
    end.

row(Children) -> 'div'(Children, [<<"flex flex-wrap items-center gap-3">>], []).
