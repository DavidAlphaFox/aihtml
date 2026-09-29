%% @doc Fragment endpoints. Each answers with an HTML fragment that the
%% browser runtime swaps into the page, rendered by the same view functions
%% the full page uses.
-module(aihtml_example_api).

-export([init/2]).

-spec init(cowboy_req:req(), atom()) -> {ok, cowboy_req:req(), atom()}.
init(Req0, Action) ->
    Method = cowboy_req:method(Req0),
    {Status, Html, Req} = handle(Action, Method, Req0),
    Req1 = cowboy_req:reply(Status, #{<<"content-type">> => <<"text/html; charset=utf-8">>},
                            aihtml:render(Html), Req),
    {ok, Req1, Action}.

handle(counter, <<"POST">>, Req) ->
    {200, aihtml_example_views:counter_value(aihtml_example_store:bump()), Req};
handle(greet, <<"GET">>, Req) ->
    Name = proplists:get_value(<<"name">>, cowboy_req:parse_qs(Req), <<>>),
    {200, aihtml_example_views:greeting(string:trim(Name)), Req};
handle(todos, <<"POST">>, Req0) ->
    {ok, Form, Req} = cowboy_req:read_urlencoded_body(Req0),
    case string:trim(proplists:get_value(<<"text">>, Form, <<>>)) of
        <<>> ->
            {200, aihtml_example_views:todo_section(<<"Write something first.">>), Req};
        Text ->
            _ = aihtml_example_store:add_todo(unicode:characters_to_binary(Text)),
            {200, aihtml_example_views:todo_section(undefined), Req}
    end;
handle(toggle, <<"POST">>, Req) ->
    with_todo(Req, fun(Id) -> aihtml_example_store:toggle_todo(Id) end);
handle(todo, <<"DELETE">>, Req) ->
    with_todo(Req, fun(Id) -> T = aihtml_example_store:todo(Id),
                              ok = aihtml_example_store:delete_todo(Id),
                              T
                   end);
handle(_, _, Req) ->
    {405, <<"Method not allowed">>, Req}.

%% Both todo actions answer with the whole section, so the done count stays
%% in step with the list.
with_todo(Req, Change) ->
    try binary_to_integer(cowboy_req:binding(id, Req)) of
        Id ->
            case Change(Id) of
                undefined -> {404, <<"No such todo">>, Req};
                _Todo -> {200, aihtml_example_views:todo_section(undefined), Req}
            end
    catch
        error:badarg -> {400, <<"Bad id">>, Req}
    end.
