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
    %% ?view=processes|system: the URL the load buttons push into the
    %% history renders the same content, so back/forward and bookmarks work.
    View = proplists:get_value(<<"view">>, cowboy_req:parse_qs(Req)),
    {ok, aihtml_cowboy:reply(Req, page(View), #{title => <<"aihtml actions">>,
                                                css => [aihtml_example_site:css()]}),
     State}.

page(View) ->
    ah_div([top_bar(),
            ah_main([ah_div([counter_card(), greeting_card(), data_card(View), include_card(),
                             todos_card()],
                            [<<"grid gap-6 md:grid-cols-2">>], [])],
                    [<<"mx-auto max-w-5xl px-4 py-8">>], [])],
           [<<"min-h-screen">>], []).

top_bar() ->
    [aihtml_example_site:topbar(demo),
     ah_div([ah_div([ah_h1(<<"实时演示"/utf8>>, [<<"text-2xl font-bold">>], []),
                     ah_p([<<"每个事件一次无状态请求 · 服务器时间 "/utf8>>,
                           ah_span(aihtml_example_clock:now_text(), [<<"font-mono">>],
                                   [{id, clock}, subscribe(clock)])],
                          [<<"text-sm text-muted mt-1">>], [])]),
             ah_theme_switcher([], [])],
            [<<"mx-auto max-w-5xl px-4 pt-8 flex flex-wrap items-end justify-between gap-4">>], [])].

%% The count lives in the data layer, so every visitor shares it.
counter_card() ->
    ah_card([row([ah_button(<<"+1">>, inc, [], [on(click, {?MODULE, bump, #{}})]),
                  ah_button(<<"Reset">>, reset, [borderless], [on(click, {?MODULE, reset, #{}})]),
                  ah_span([<<"Count: ">>,
                           ah_strong(aihtml_example_store:counter(), [],
                                     [{id, count},
                                      subscribe(counter, #{refresh => {?MODULE, refresh_counter, #{}}})])],
                          [<<"text-muted">>], [])]),
             ah_p(<<"Stored in the data layer and pushed to every open page.">>,
                  [<<"mt-3 text-sm text-muted">>], [])],
            [], [{title, <<"Click handled in Erlang">>}]).

greeting_card() ->
    ah_card([ah_div([ah_field(<<"Your name">>,
                              ah_input(<<>>, [], [{id, name}, {placeholder, <<"Type a name">>},
                                                  {autocomplete, off},
                                                  on(input, {?MODULE, greet, #{}}, #{debounce => 150})]),
                              [top], [{for, name}]),
                     ah_p(<<"Hello, stranger.">>, [<<"text-muted">>], [{id, greeting}])],
                    [<<"flex flex-col gap-3">>], [])],
            [], [{title, <<"Input events">>}]).

%% The buttons show a spinner (indicator) and are disabled while their
%% request runs; the two share one queue (sync_scope), so a second click
%% waits for the first. Each load pushes ?view=... into the history.
data_card(View) ->
    Req = #{indicator => <<"#data-spin">>, disable => <<"#data-card button">>,
            sync => queue, sync_scope => <<"#data-card">>},
    ah_card([row([ah_button(<<"Load processes">>, load, [outlined],
                            [on(click, {?MODULE, load_processes, #{}}, Req)]),
                  ah_button(<<"Load system info">>, info, [borderless],
                            [on(click, {?MODULE, load_system, #{}}, Req)]),
                  ah_span(<<"Loading…"/utf8>>, [<<"ah-indicator text-sm text-muted">>], [{id, <<"data-spin">>}])]),
             ah_div(data_view(View), [<<"mt-4">>], [{id, data}])],
            [], [{id, <<"data-card">>}, {title, <<"Load data, progressively">>}]).

data_view(<<"processes">>) -> processes_table();
data_view(<<"system">>) -> system_info();
data_view(_) -> ah_p(<<"Nothing loaded yet.">>, [<<"text-muted text-sm">>], []).

%% The browser sends the values of other controls along with the event.
include_card() ->
    ah_card([row([ah_input(<<"2">>, [sm, <<"w-20">>], [{id, a}, {type, number}]),
                  ah_span(<<"+">>),
                  ah_input(<<"3">>, [sm, <<"w-20">>], [{id, b}, {type, number}]),
                  ah_button(<<"=">>, sum, [secondary, sm],
                            [on(click, {?MODULE, sum, #{}}, #{include => [{id, a}, {id, b}]})]),
                  ah_strong(<<"?">>, [], [{id, sum}])]),
             row([ah_button(<<"Dark mode from Erlang">>, dark, [borderless],
                            [on(click, {?MODULE, dark, #{}})])]),
             %% Server-side search: each keystroke (debounced) runs `search'
             %% below, which renders the matching rows in Erlang and morphs
             %% them into the list; the input keeps its focus and caret.
             ah_field(<<"Registered process (server search)">>,
                      ah_combobox([], undefined, [<<"w-72">>],
                                  [{id, <<"proc_search">>}, {placeholder, <<"Type a name, e.g. kernel">>},
                                   {search, {?MODULE, search, #{}}}]),
                      [top, <<"mt-4">>], [])],
            [], [{title, <<"Values and scripts">>}]).

todos_card() ->
    ah_card([ah_form(ah_field(<<"New todo">>,
                              ah_div([ah_input(<<>>, [], [{id, todo_text}, {name, text},
                                                          {placeholder, <<"What needs doing?">>},
                                                          {autocomplete, off}]),
                                      ah_button(<<"Add">>, add, [], [{type, submit}])],
                                     [<<"flex gap-2">>], []),
                              [top], [{for, todo_text}]),
                     [], [on(submit, {?MODULE, add_todo, #{}})]),
             ah_ul([todo_item(T) || T <- aihtml_example_store:todos()],
                   [<<"mt-3 divide-y divide-line">>],
                   [{id, todo_list},
                    subscribe(todos, #{refresh => {?MODULE, refresh_todos, #{}}})])],
            [<<"md:col-span-2">>], [{title, <<"Todos in the data layer">>}]).

todo_item(#{id := Id, text := Text, done := Done}) ->
    ah_li([ah_checkbox(Text, Id, [<<"flex-1">>, [<<"line-through text-muted">> || Done]],
                       [{checked, Done}, on(change, {?MODULE, toggle_todo, #{id => Id}})]),
           ah_button(<<"Delete">>, Id, [borderless, sm],
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
               Name -> [<<"Hello, ">>, ah_strong(Name), <<"!">>]    % escaped
           end,
    aihtml_action:html(Ctx, {id, greeting}, Html);
action(load_processes, _, _Ev, Ctx) ->
    loading(Ctx),
    aihtml_action:html(Ctx, {id, data}, processes_table()),
    aihtml_action:push_url(Ctx, <<"/demo?view=processes">>);
action(load_system, _, _Ev, Ctx) ->
    loading(Ctx),
    aihtml_action:html(Ctx, {id, data}, system_info()),
    aihtml_action:push_url(Ctx, <<"/demo?view=system">>);
action(sum, _, #{values := Vs}, Ctx) ->
    Html = try
               binary_to_integer(maps:get(<<"a">>, Vs)) + binary_to_integer(maps:get(<<"b">>, Vs))
           catch
               error:_ -> <<"not a number">>
           end,
    aihtml_action:html(Ctx, {id, sum}, Html);
action(search, _, #{value := Query} = Ev, Ctx) ->
    Q = string:lowercase(Query),
    Names = lists:sort([atom_to_binary(N) || N <- erlang:registered()]),
    Hits = lists:sublist([N || N <- Names, string:find(N, Q) =/= nomatch], 20),
    aihtml_combobox:set_items(Ctx, Ev, Hits);
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
    aihtml_action:html(Ctx, {id, data}, ah_p(<<"Loading…"/utf8>>, [<<"text-muted text-sm">>], [])),
    aihtml_action:flush(Ctx),
    timer:sleep(400).

%%%===================================================================
%%% Helpers
%%%===================================================================

processes_table() ->
    Top = lists:sublist(
            lists:reverse(lists:keysort(2, [{P, M} || P <- erlang:processes(),
                                                     {memory, M} <- [erlang:process_info(P, memory)]])),
            8),
    Rows = [ah_tr([ah_td(pid_to_list(P), [<<"font-mono py-1">>], []),
                   ah_td(proc_name(P), [<<"py-1">>], []),
                   ah_td(M div 1024, [<<"py-1 text-right">>], [])])
            || {P, M} <- Top],
    ah_table([ah_thead(ah_tr([ah_th(<<"pid">>, [<<"text-left">>], []),
                              ah_th(<<"name">>, [<<"text-left">>], []),
                              ah_th(<<"KiB">>, [<<"text-right">>], [])])),
              ah_tbody(Rows)],
             [<<"w-full text-sm">>], []).

system_info() ->
    Items = [{<<"Node">>, node()},
             {<<"OTP">>, erlang:system_info(otp_release)},
             {<<"Schedulers">>, erlang:system_info(schedulers_online)},
             {<<"Processes">>, erlang:system_info(process_count)},
             {<<"Memory (MiB)">>, erlang:memory(total) div (1024 * 1024)},
             {<<"Request process">>, pid_to_list(self())}],
    ah_dl([[ah_dt(K, [<<"text-muted">>], []), ah_dd(V, [<<"font-mono">>], [])] || {K, V} <- Items],
          [<<"grid grid-cols-2 gap-y-1 text-sm">>], []).

todo_dom(Id) -> <<"todo-", (integer_to_binary(Id))/binary>>.

proc_name(P) ->
    case erlang:process_info(P, registered_name) of
        {registered_name, N} -> N;
        _ -> <<>>
    end.

row(Children) -> ah_div(Children, [<<"flex flex-wrap items-center gap-3">>], []).
