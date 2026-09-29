-module(aihtml_example_app).
-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) ->
    %% The data layer first: Mnesia with the demo's tables on this node.
    ok = aihtml_example_store:init(),
    {ok, Sup} = aihtml_example_sup:start_link(),
    Port = application:get_env(aihtml_example, port, 8080),
    %% "/" is the landing page, /components/:name the component docs, and
    %% "/demo" the live action demo (each event one stateless request).
    %% aihtml_cowboy adds the action endpoint and the /aihtml/ assets.
    Actions = [{"/", aihtml_example_home, #{}},
               {"/components", aihtml_example_docs, #{}},
               {"/components/:name", aihtml_example_docs, #{}},
               {"/demo", aihtml_example_actions, #{}} | aihtml_cowboy:routes(#{})],
    %% "/fetch" is the lower-level variant: HTML fragments from plain URLs.
    Fetch = [{"/fetch", aihtml_example_page, #{}},
             {"/counter", aihtml_example_api, counter},
             {"/greet", aihtml_example_api, greet},
             {"/todos", aihtml_example_api, todos},
             {"/todos/:id/toggle", aihtml_example_api, toggle},
             {"/todos/:id", aihtml_example_api, todo}],
    %% the example's own Tailwind build
    Static = [{"/static/[...]", cowboy_static, {priv_dir, aihtml_example, "static"}}],
    Routes = [{'_', Actions ++ Fetch ++ Static}],
    Dispatch = cowboy_router:compile(Routes),
    case cowboy:start_clear(aihtml_example_http, [{port, Port}],
                            #{env => #{dispatch => Dispatch}}) of
        {ok, _} ->
            logger:notice("aihtml example on http://localhost:~b/", [Port]),
            {ok, Sup};
        {error, _} = E -> E
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    ok = cowboy:stop_listener(aihtml_example_http).
