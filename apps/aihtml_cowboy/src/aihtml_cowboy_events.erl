%% @doc Endpoint for push streams (designs/03-push.md).
%%
%% GET `?t=Token&t=Token...' opens a server-sent-events stream for the
%% page's signed topic tokens. The process joins the topics and relays
%% what aihtml_push publishes; it holds no application state. Events:
%%
%%   event: hello   data: {"id": StreamId}   first; the page sends the id
%%                                           with its actions (a publish
%%                                           skips the page that caused it)
%%                                           and with topic changes
%%   event: ops     data: [Op, ...]          published operations
%%   : ping                                  every 25 s, keeps proxies open
%%
%% POST `{"stream": StreamId, "topics": [Token, ...]}' replaces the topics
%% of an open stream, on whichever node it runs: 204, 404 when no stream
%% has that id (the page reopens), 403 for a bad token, 400 for a bad body.
%%
%% A stream whose client reads too slowly (its mailbox keeps growing) is
%% closed; the browser reconnects and its refresh actions catch up.
-module(aihtml_cowboy_events).
-behaviour(cowboy_loop).

-export([init/2, info/3]).

-define(PING, 25000).
%% Messages a stream may have waiting before it is closed as too slow.
-define(MAX_QUEUE, 1000).
-define(MAX_BODY, 65536).

-spec init(cowboy_req:req(), map()) ->
          {cowboy_loop, cowboy_req:req(), map(), hibernate} | {ok, cowboy_req:req(), map()}.
init(Req0, #{origins := Origins} = State) ->
    case {cowboy_req:method(Req0), aihtml_cowboy_action:origin_ok(Req0, Origins)} of
        {_, false} -> {ok, error_reply(403, forbidden_origin, Req0), State};
        {<<"GET">>, true} -> open(Req0, State);
        {<<"POST">>, true} -> {ok, topics(Req0), State};
        _ -> {ok, cowboy_req:reply(405, #{<<"allow">> => <<"GET, POST">>}, Req0), State}
    end.

open(Req0, State) ->
    Tokens = [T || {<<"t">>, T} <- cowboy_req:parse_qs(Req0), is_binary(T)],
    Checked = case Tokens =/= [] andalso length(Tokens) =< aihtml_push:max_topics() of
                  true -> aihtml_push:verify(Tokens);
                  false -> {error, bad_request}
              end,
    case Checked of
        {ok, Topics} ->
            Id = aihtml_push:new_stream_id(),
            ok = aihtml_push:join(Topics),
            ok = aihtml_push:register_stream(Id),
            Req = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"text/event-stream">>,
                                                 <<"cache-control">> => <<"no-store">>,
                                                 <<"x-accel-buffering">> => <<"no">>}, Req0),
            %% HTTP/1.1: the stream may stay quiet for long; the pings below
            %% keep proxies from closing it.
            cowboy_req:cast({set_options, #{idle_timeout => infinity}}, Req),
            Hello = event(<<"hello">>, json:encode(#{<<"id">> => Id})),
            ok = cowboy_req:stream_body([<<"retry: 2000\n">>, Hello], nofin, Req),
            _ = erlang:send_after(?PING, self(), aihtml_ping),
            {cowboy_loop, Req, State#{stream_id => Id, topics => Topics}, hibernate};
        {error, Code} ->
            Status = case Code of invalid_topic -> 403; _ -> 400 end,
            {ok, error_reply(Status, Code, Req0), State}
    end.

%% POST: the page's new topic set for its stream.
topics(Req0) ->
    case cowboy_req:read_body(Req0, #{length => ?MAX_BODY}) of
        {ok, Body, Req} ->
            case catch json:decode(Body) of
                #{<<"stream">> := Id, <<"topics">> := Tokens} when is_binary(Id), is_list(Tokens) ->
                    case aihtml_push:set_topics(Id, [T || T <- Tokens, is_binary(T)]) of
                        ok -> cowboy_req:reply(204, #{}, Req);
                        {error, no_stream} -> error_reply(404, no_stream, Req);
                        {error, invalid_topic} -> error_reply(403, invalid_topic, Req)
                    end;
                _ ->
                    error_reply(400, bad_request, Req)
            end;
        {more, _, Req} ->
            error_reply(400, bad_request, Req)
    end.

-spec info(term(), cowboy_req:req(), map()) ->
          {ok, cowboy_req:req(), map(), hibernate} | {stop, cowboy_req:req(), map()}.
info({aihtml_push, Except, Json}, Req, #{stream_id := Id} = State) ->
    case erlang:process_info(self(), message_queue_len) of
        {message_queue_len, N} when N > ?MAX_QUEUE ->
            {stop, Req, State};      % too slow: the browser reconnects and refreshes
        _ ->
            _ = case Except =:= Id of
                    true -> ok;      % the page that caused it updated itself
                    false -> cowboy_req:stream_body(event(<<"ops">>, Json), nofin, Req)
                end,
            {ok, Req, State, hibernate}
    end;
info({aihtml_push_topics, New}, Req, #{topics := Old} = State) ->
    ok = aihtml_push:leave(Old -- New),
    ok = aihtml_push:join(New -- Old),
    {ok, Req, State#{topics => New}, hibernate};
info(aihtml_ping, Req, State) ->
    ok = cowboy_req:stream_body(<<": ping\n\n">>, nofin, Req),
    _ = erlang:send_after(?PING, self(), aihtml_ping),
    {ok, Req, State, hibernate};
info(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.

event(Name, Json) -> [<<"event: ">>, Name, <<"\ndata: ">>, Json, <<"\n\n">>].

error_reply(Status, Code, Req) ->
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>},
                     json:encode(#{error => Code}), Req).
