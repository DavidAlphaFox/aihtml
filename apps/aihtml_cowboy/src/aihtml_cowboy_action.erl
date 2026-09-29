%% @doc POST endpoint for aihtml actions (designs/02-actions.md).
%%
%% The body is JSON: `{"action": Token, "event": {...}, "stream": S}'
%% (stream: the page's push stream id, if any). A bad origin, a bad body or
%% an invalid token is refused with a plain status (403 / 400 / 405) before
%% anything runs. Otherwise the action runs in this request process and the
%% reply is its operations:
%%
%%   200 application/json      {"ops": [...]}
%%   200 application/x-ndjson  one {"ops": [...]} line per flush/1, then
%%                             {"done": true} (or {"error": "action_failed"}
%%                             when the action crashes after a flush)
%%   500 application/json      {"error": "action_failed"}, a crash before
%%                             any flush (logged, not sent)
-module(aihtml_cowboy_action).

-export([init/2, origin_ok/2]).

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
    %% flush/1 turns the reply into an NDJSON stream; the streaming Req is
    %% kept here (this process runs the action).
    Send = fun(Ops) -> line(started(Req0), #{ops => Ops}) end,
    Result = aihtml_action:execute(Ref, maps:get(<<"event">>, In, #{}),
                                   #{send => Send,
                                     meta => #{req => Req0},
                                     stream_id => stream_id(maps:get(<<"stream">>, In, null))}),
    case {erase(aihtml_cowboy_stream), Result} of
        {undefined, {ok, Ops}} ->
            json(200, #{ops => Ops}, Req0);
        {undefined, error} ->
            json(500, #{error => action_failed}, Req0);
        {Req, {ok, Ops}} ->
            _ = [line(Req, #{ops => Ops}) || Ops =/= []],
            finish(Req, #{done => true});
        {Req, error} ->
            finish(Req, #{error => action_failed})
    end.

%% The streaming reply, started on the first flush.
started(Req0) ->
    case get(aihtml_cowboy_stream) of
        undefined ->
            Req = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"application/x-ndjson">>,
                                                 <<"cache-control">> => <<"no-store">>,
                                                 <<"x-accel-buffering">> => <<"no">>}, Req0),
            put(aihtml_cowboy_stream, Req),
            Req;
        Req ->
            Req
    end.

line(Req, Term) ->
    cowboy_req:stream_body([aihtml_json:encode(Term), <<"\n">>], nofin, Req).

finish(Req, Term) ->
    ok = cowboy_req:stream_body([aihtml_json:encode(Term), <<"\n">>], fin, Req),
    Req.

json(Status, Term, Req) ->
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>,
                               <<"cache-control">> => <<"no-store">>},
                     aihtml_json:encode(Term), Req).

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
    json(Status, #{error => Code}, Req).

stream_id(B) when is_binary(B) -> B;
stream_id(_) -> undefined.

%% @doc Browsers send Origin on POST. Accept the page's own host, and whatever
%% the application listed; requests without Origin (curl, tests) pass.
-spec origin_ok(cowboy_req:req(), [binary()]) -> boolean().
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
