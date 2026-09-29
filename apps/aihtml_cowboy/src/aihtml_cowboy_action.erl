%% @doc POST endpoint for aihtml actions.
%%
%% The body is JSON: `{"action": Token, "event": {...}, "threadId": T,
%% "runId": R}'. A bad origin, a bad body or an invalid token is refused
%% with a plain status (403 / 400 / 405) before anything runs; otherwise the
%% action runs in this request process and its AG-UI events are streamed
%% back as server-sent events.
-module(aihtml_cowboy_action).

-export([init/2]).

-define(MAX_BODY, 1048576).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, #{origins := Origins} = State) ->
    Req = case cowboy_req:method(Req0) of
              <<"POST">> ->
                  case origin_ok(Req0, Origins) of
                      true -> handle(Req0);
                      false -> refuse(403, <<"forbidden_origin">>, Req0)
                  end;
              _ ->
                  cowboy_req:reply(405, #{<<"allow">> => <<"POST">>}, Req0)
          end,
    {ok, Req, State}.

handle(Req0) ->
    case read_json(Req0) of
        {ok, #{<<"action">> := Token} = In, Req1} ->
            case aihtml_action:verify(Token) of
                {ok, Ref} -> stream(Ref, In, Req1);
                {error, invalid_action} -> refuse(403, <<"invalid_action">>, Req1)
            end;
        {ok, _, Req1} ->
            refuse(400, <<"bad_request">>, Req1);
        {error, Req1} ->
            refuse(400, <<"bad_request">>, Req1)
    end.

stream(Ref, In, Req0) ->
    Req = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"text/event-stream">>,
                                         <<"cache-control">> => <<"no-store">>,
                                         <<"x-accel-buffering">> => <<"no">>}, Req0),
    Emit = fun(Event) ->
               cowboy_req:stream_body([<<"data: ">>, json:encode(Event), <<"\n\n">>], nofin, Req)
           end,
    _ = aihtml_action:execute(Ref, maps:get(<<"event">>, In, #{}),
                              #{emit => Emit,
                                meta => #{req => Req0},
                                thread_id => bin(maps:get(<<"threadId">>, In, <<>>)),
                                run_id => bin(maps:get(<<"runId">>, In, <<>>))}),
    ok = cowboy_req:stream_body(<<>>, fin, Req),
    Req.

read_json(Req0) ->
    case cowboy_req:read_body(Req0, #{length => ?MAX_BODY}) of
        {ok, Body, Req} ->
            try json:decode(Body) of
                Map when is_map(Map) -> {ok, Map, Req};
                _ -> {error, Req}
            catch
                error:_ -> {error, Req}
            end;
        {more, _, Req} ->
            {error, Req}
    end.

refuse(Status, Code, Req) ->
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>},
                     json:encode(#{error => Code}), Req).

bin(B) when is_binary(B) -> B;
bin(_) -> <<>>.

%% Browsers send Origin on POST. Accept the page's own host, and whatever
%% the application listed; requests without Origin (curl, tests) pass.
origin_ok(Req, Extra) ->
    case cowboy_req:header(<<"origin">>, Req) of
        undefined -> true;
        Origin ->
            Host = cowboy_req:host(Req),
            HostPort = case cowboy_req:port(Req) of
                           P when P =:= 80; P =:= 443 -> Host;
                           P -> <<Host/binary, ":", (integer_to_binary(P))/binary>>
                       end,
            lists:member(Origin, Extra) orelse
                lists:member(Origin, [<<"http://", HostPort/binary>>,
                                      <<"https://", HostPort/binary>>,
                                      <<"http://", Host/binary>>,
                                      <<"https://", Host/binary>>])
    end.
