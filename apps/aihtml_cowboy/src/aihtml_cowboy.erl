%%%-------------------------------------------------------------------
%%% @doc Cowboy glue for aihtml.
%%%
%%% ```
%%% Dispatch = cowboy_router:compile(
%%%     [{'_', aihtml_cowboy:routes(#{}) ++ [{"/", my_page, #{}}]}]).
%%%
%%% %% in my_page:init/2
%%% aihtml_cowboy:reply(Req, my_views:index(), #{title => <<"Todos">>})
%%% '''
%%%
%%% `routes/1' gives:
%%%
%%%   ActionPath       POST endpoint for actions, default "/aihtml/action"
%%%   EventsPath       GET push stream (SSE), default "/aihtml/events"
%%%   /aihtml/[...]    the runtime bundle (js/), aihtml.css and jQuery (static => false
%%%                    leaves it out when another route serves them)
%%%
%%% Options:
%%%   action   action path
%%%   events   push stream path
%%%   static   serve the aihtml assets, default true
%%%   origins  extra allowed Origin values; by default only same-host
%%%            requests may run actions or open streams
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_cowboy).

-export([routes/1, reply/3]).

-export_type([opts/0]).

-type opts() :: #{action => string() | binary(),
                  events => string() | binary(),
                  static => boolean(),
                  origins => [binary()]}.

-spec routes(opts()) -> [{binary(), module(), term()}].
routes(Opts) ->
    Action = iolist_to_binary(maps:get(action, Opts, <<"/aihtml/action">>)),
    Events = iolist_to_binary(maps:get(events, Opts, <<"/aihtml/events">>)),
    Origins = #{origins => maps:get(origins, Opts, [])},
    [{Action, aihtml_cowboy_action, Origins},
     {Events, aihtml_cowboy_events, Origins}]
        ++ [{<<"/aihtml/[...]">>, cowboy_static, {priv_dir, aihtml, "static"}}
            || maps:get(static, Opts, true)].

%% @doc Reply 200 with a whole page, see `aihtml_page'.
-spec reply(cowboy_req:req(), aihtml:html(), aihtml_page:opts()) -> cowboy_req:req().
reply(Req, Body, PageOpts) ->
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>},
                     aihtml:page(Body, PageOpts), Req).
