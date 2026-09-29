%% @doc GET endpoint for push streams (server-sent events).
%%
%% `?t=Token&t=Token...' carries the page's signed topic tokens. The
%% process joins the topics and relays what aihtml_push publishes; it holds
%% no application state. The first event tells the page its stream id,
%% which the page sends with its actions so a publish can skip it.
-module(aihtml_cowboy_events).
-behaviour(cowboy_loop).

-export([init/2, info/3]).

-define(MAX_TOPICS, 32).
-define(PING, 25000).

-spec init(cowboy_req:req(), map()) ->
          {cowboy_loop, cowboy_req:req(), map(), hibernate} | {ok, cowboy_req:req(), map()}.
init(Req0, #{origins := Origins} = State) ->
    Tokens = [T || {<<"t">>, T} <- cowboy_req:parse_qs(Req0), is_binary(T)],
    Checked = case {cowboy_req:method(Req0), aihtml_cowboy_action:origin_ok(Req0, Origins)} of
                  {<<"GET">>, true} when Tokens =/= [], length(Tokens) =< ?MAX_TOPICS ->
                      aihtml_push:verify(Tokens);
                  {<<"GET">>, true} -> {error, bad_request};
                  {<<"GET">>, false} -> {error, forbidden_origin};
                  _ -> {error, method}
              end,
    case Checked of
        {ok, Topics} ->
            Id = aihtml_push:new_stream_id(),
            ok = aihtml_push:join(Topics),
            Req = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"text/event-stream">>,
                                                 <<"cache-control">> => <<"no-store">>,
                                                 <<"x-accel-buffering">> => <<"no">>}, Req0),
            %% HTTP/1.1: the stream may stay quiet for long; the pings below
            %% keep proxies from closing it.
            cowboy_req:cast({set_options, #{idle_timeout => infinity}}, Req),
            Hello = #{<<"type">> => <<"CUSTOM">>, <<"name">> => <<"aihtml.stream">>,
                      <<"value">> => #{<<"id">> => Id}},
            ok = cowboy_req:stream_body([<<"retry: 2000\n">>, frame(json:encode(Hello))], nofin, Req),
            _ = erlang:send_after(?PING, self(), aihtml_ping),
            {cowboy_loop, Req, State#{stream_id => Id}, hibernate};
        {error, method} ->
            {ok, cowboy_req:reply(405, #{<<"allow">> => <<"GET">>}, Req0), State};
        {error, Code} ->
            Status = case Code of forbidden_origin -> 403; invalid_topic -> 403; _ -> 400 end,
            {ok, cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>},
                                  json:encode(#{error => Code}), Req0), State}
    end.

-spec info(term(), cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map(), hibernate}.
info({aihtml_push, Except, Event}, Req, #{stream_id := Id} = State) ->
    _ = case Except =:= Id of
            true -> ok;            % the page that caused it updated itself
            false -> cowboy_req:stream_body(frame(Event), nofin, Req)
        end,
    {ok, Req, State, hibernate};
info(aihtml_ping, Req, State) ->
    ok = cowboy_req:stream_body(<<": ping\n\n">>, nofin, Req),
    _ = erlang:send_after(?PING, self(), aihtml_ping),
    {ok, Req, State, hibernate};
info(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.

frame(Json) -> [<<"data: ">>, Json, <<"\n\n">>].
