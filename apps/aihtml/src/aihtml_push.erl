%%%-------------------------------------------------------------------
%%% @doc Server push: publish DOM operations to every page subscribed to a
%%% topic, on every node of the cluster.
%%%
%%% ```
%%% %% in the page: the list follows the `todos' topic
%%% ul(Items, [], [{id, todo_list},
%%%                subscribe(todos, #{refresh => {?MODULE, refresh_todos, #{}}})])
%%%
%%% %% anywhere, typically in an action after the data layer changed
%%% aihtml_push:publish(todos, fun(Ctx) ->
%%%     aihtml_action:html(Ctx, {id, todo_list}, item(Todo), append)
%%% end, #{except => Ctx})
%%% '''
%%%
%%% A page opens one server-sent-events stream carrying the signed tokens
%%% of the topics it shows (`aihtml:subscribe/1,2'). The process holding
%%% the stream joins one `pg' group per topic and only relays: it keeps no
%%% business state, so any node can serve it and a restart just reconnects
%%% it. `pg' spans every connected node, so a publish on one node reaches
%%% subscribers on all of them.
%%%
%%% Delivery is at most once. Events published while a page is
%%% reconnecting are lost; the subscription's `refresh' action runs after
%%% each reconnect to bring the page up to date from the data layer.
%%%
%%% `call/4,5' and `trigger/4,5' publish data rather than HTML: a component
%%% method call (a chart gets new points) or a DOM event with a detail.
%%%
%%% When the topics on a page change (new content subscribes, removed
%%% content unsubscribes), the page sends the new set for its stream id
%%% (`set_topics/2'); the stream process, wherever it runs, joins and leaves
%%% groups without reconnecting.
%%%
%%% Topics are any plain term: `todos', `{room, 42}', `<<"user:7">>'. The
%%% token is signed, so a page can only follow topics the server rendered
%%% for it; decide per user which topics to render.
%%%
%%% Requires the aihtml application to be running (it starts the scope).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_push).

-export([publish/2, publish/3, call/4, call/5, trigger/4, trigger/5, subscribers/1]).
%% For templates and transports.
-export([token/1, verify/1, join/1, leave/1, new_stream_id/0, register_stream/1, set_topics/2,
         max_topics/0]).

-export_type([topic/0]).

-type topic() :: term().

-define(SCOPE, aihtml_push).
%% Topics one stream may follow.
-define(MAX_TOPICS, 32).

-type publish_opts() :: #{except => aihtml_action:ctx() | binary()}.

%% @doc Publish to everyone subscribed to `Topic'. `Fun' receives a ctx
%% and uses the aihtml_action operations; the HTML is rendered once.
-spec publish(topic(), fun((aihtml_action:ctx()) -> any())) -> ok.
publish(Topic, Fun) -> publish(Topic, Fun, #{}).

%% @doc Options: `except' is an action ctx (or a stream id): the page that
%% sent that request is skipped, because the action's own response already
%% updated it.
-spec publish(topic(), fun((aihtml_action:ctx()) -> any()), publish_opts()) -> ok.
publish(Topic, Fun, Opts) ->
    Except = case maps:get(except, Opts, undefined) of
                 Id when is_binary(Id); Id =:= undefined -> Id;
                 Ctx -> aihtml_action:stream_id(Ctx)
             end,
    case aihtml_action:render_ops(Fun) of
        [] -> ok;
        Ops ->
            Json = iolist_to_binary(aihtml_json:encode(Ops)),
            _ = [P ! {aihtml_push, Except, Json} || P <- members(Topic)],
            ok
    end.

%% @doc Call a component method on every page following `Topic' (see
%% aihtml_action:call/4): data for a component, no HTML.
-spec call(topic(), aihtml_action:target() | global, atom() | binary(), [term()]) -> ok.
call(Topic, Target, Method, Args) -> call(Topic, Target, Method, Args, #{}).

-spec call(topic(), aihtml_action:target() | global, atom() | binary(), [term()],
           publish_opts()) -> ok.
call(Topic, Target, Method, Args, Opts) ->
    publish(Topic, fun(Ctx) -> aihtml_action:call(Ctx, Target, Method, Args) end, Opts).

%% @doc Fire a DOM event with `Detail' on every page following `Topic' (see
%% aihtml_action:trigger/4).
-spec trigger(topic(), aihtml_action:target() | document, atom() | binary(), term()) -> ok.
trigger(Topic, Target, Event, Detail) -> trigger(Topic, Target, Event, Detail, #{}).

-spec trigger(topic(), aihtml_action:target() | document, atom() | binary(), term(),
              publish_opts()) -> ok.
trigger(Topic, Target, Event, Detail, Opts) ->
    publish(Topic, fun(Ctx) -> aihtml_action:trigger(Ctx, Target, Event, Detail) end, Opts).

%% @doc Number of streams following `Topic', cluster-wide.
-spec subscribers(topic()) -> non_neg_integer().
subscribers(Topic) -> length(members(Topic)).

%% @doc The signed token that `aihtml:subscribe/1,2' writes into the page.
-spec token(topic()) -> binary().
token(Topic) ->
    aihtml_action:plain(Topic) orelse error({aihtml, {topic_not_data, Topic}}),
    aihtml_action:sign({aihtml_topic, Topic}).

%% @doc Check topic tokens from the browser; all must be valid.
-spec verify([binary()]) -> {ok, [topic()]} | {error, invalid_topic}.
verify(Tokens) when is_list(Tokens), length(Tokens) > ?MAX_TOPICS ->
    {error, invalid_topic};
verify(Tokens) when is_list(Tokens) ->
    Topics = [case aihtml_action:unsign(T) of
                  {ok, {aihtml_topic, Topic}} -> {ok, Topic};
                  _ -> error
              end || T <- Tokens],
    case lists:member(error, Topics) of
        true -> {error, invalid_topic};
        false -> {ok, lists:usort([T || {ok, T} <- Topics])}
    end.

%% @doc Make the calling process a subscriber. It then receives
%% `{aihtml_push, Except, EventJson}' and should write EventJson to its
%% stream unless Except is its own stream id. Leaving is automatic when
%% the process exits.
-spec join([topic()]) -> ok.
join(Topics) ->
    try
        _ = [ok = pg:join(?SCOPE, {topic, T}, self()) || T <- Topics],
        ok
    catch
        error:badarg -> error({aihtml, push_not_started})
    end.

%% @doc Stop following topics (the calling process).
-spec leave([topic()]) -> ok.
leave(Topics) ->
    _ = [pg:leave(?SCOPE, {topic, T}, self()) || T <- Topics],
    ok.

%% @doc Topics one stream may follow.
-spec max_topics() -> pos_integer().
max_topics() -> ?MAX_TOPICS.

%% @doc Make the calling stream process findable by its id, cluster-wide
%% (set_topics/2).
-spec register_stream(binary()) -> ok.
register_stream(Id) ->
    ok = pg:join(?SCOPE, {stream, Id}, self()).

%% @doc Change the topics of the stream `Id' to those of `Tokens': the
%% stream process receives `{aihtml_push_topics, Topics}' and joins and
%% leaves groups itself. `no_stream' when no process holds that id (the
%% page then reopens its stream).
-spec set_topics(binary(), [binary()]) -> ok | {error, invalid_topic | no_stream}.
set_topics(Id, Tokens) ->
    case verify(Tokens) of
        {ok, Topics} ->
            case pg:get_members(?SCOPE, {stream, Id}) of
                [] -> {error, no_stream};
                Pids -> _ = [P ! {aihtml_push_topics, Topics} || P <- Pids], ok
            end;
        {error, _} = E ->
            E
    end.

-spec new_stream_id() -> binary().
new_stream_id() ->
    base64:encode(crypto:strong_rand_bytes(16), #{mode => urlsafe, padding => false}).

members(Topic) ->
    try pg:get_members(?SCOPE, {topic, Topic})
    catch error:badarg -> error({aihtml, push_not_started})
    end.
