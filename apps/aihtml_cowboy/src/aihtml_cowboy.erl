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
%%%   /aihtml/[...]    the runtime bundle (js/) and the stylesheet (css/)
%%%                    (static => false leaves it out when another route
%%%                    serves them)
%%%
%%% Options:
%%%   action   action path
%%%   events   push stream path
%%%   static   serve the aihtml assets, default true
%%%   origins  extra allowed Origin values; by default only same-host
%%%            requests may run actions or open streams
%%%   authorize  fun((Req) -> ok | {deny, Status}), called before origin
%%%              checks, token verification and stream joins; use it for the
%%%              host's session check
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_cowboy).

-export([routes/1, reply/3, authorize/2]).

-export_type([opts/0]).

-type opts() :: #{action => string() | binary(),
                  events => string() | binary(),
                  static => boolean(),
                  origins => [binary()],
                  authorize => fun((cowboy_req:req()) -> ok | {deny, 100..599})}.

-spec routes(opts()) -> [{binary(), module(), term()}].
routes(Opts) ->
    Action = iolist_to_binary(maps:get(action, Opts, <<"/aihtml/action">>)),
    Events = iolist_to_binary(maps:get(events, Opts, <<"/aihtml/events">>)),
    State = #{origins => maps:get(origins, Opts, []),
              authorize => maps:get(authorize, Opts, undefined)},
    [{Action, aihtml_cowboy_action, State},
     {Events, aihtml_cowboy_events, State}]
        ++ [{<<"/aihtml/[...]">>, cowboy_static, {priv_dir, aihtml, "static"}}
            || maps:get(static, Opts, true)].

%% @doc Reply 200 with a whole page, see `aihtml_page'.
-spec reply(cowboy_req:req(), aihtml:html(), aihtml_page:opts()) -> cowboy_req:req().
reply(Req, Body, PageOpts) ->
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>},
                     aihtml:page(Body, PageOpts), Req).

%% The host's authorization hook, configured with `routes/1'. `ok' lets the
%% request continue; `{deny, Status}' replies with that plain status.
-spec authorize(cowboy_req:req(), map()) -> ok | {deny, 100..599}.
authorize(Req, #{authorize := Authorize}) when is_function(Authorize, 1) ->
    case Authorize(Req) of
        ok -> ok;
        {deny, Status} when is_integer(Status), Status >= 100, Status =< 599 -> {deny, Status};
        Other -> error({aihtml, {bad_authorize_result, Other}})
    end;
authorize(_, _) ->
    ok.
