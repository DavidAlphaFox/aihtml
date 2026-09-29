%% @doc Upload endpoint of the upload demos (route "/upload"): reads the
%% multipart body that the upload component POSTs, one file per request,
%% and answers JSON describing the file. The bytes are counted and
%% discarded; nothing is stored. Files above the limit get a 413 whose
%% JSON `error' the component shows on the file's row.
%%
%%   {"/upload", aihtml_example_upload, #{}}                    5 MB limit
%%   {"/upload", aihtml_example_upload, #{max_size => Bytes}}
-module(aihtml_example_upload).

-export([init/2]).

-define(MAX_SIZE, 5 * 1024 * 1024).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, Opts) ->
    Max = maps:get(max_size, Opts, ?MAX_SIZE),
    Req = case cowboy_req:method(Req0) of
              <<"POST">> -> handle(Req0, Max);
              _ -> reply(405, #{error => <<"POST a multipart/form-data body">>}, Req0)
          end,
    {ok, Req, Opts}.

handle(Req0, Max) ->
    case cowboy_req:parse_header(<<"content-type">>, Req0) of
        {<<"multipart">>, <<"form-data">>, _} ->
            case parts(Req0, Max, undefined) of
                {ok, undefined, Req} -> reply(400, #{error => <<"No file in the request">>}, Req);
                {ok, File, Req} -> reply(200, File, Req);
                {too_large, Name, Req} ->
                    reply(413, #{error => iolist_to_binary(
                                            [<<"File too large (max ">>,
                                             aihtml_form_upload:format_size(Max), <<")">>]),
                                 name => Name}, Req)
            end;
        _ ->
            reply(415, #{error => <<"Expected multipart/form-data">>}, Req0)
    end.

%% Walks the parts; the first file part is described, other parts skipped.
parts(Req0, Max, Found) ->
    case cowboy_req:read_part(Req0) of
        {done, Req} -> {ok, Found, Req};
        {ok, Headers, Req1} ->
            case cow_multipart:form_data(Headers) of
                {file, _Field, Name, Type} when Found =:= undefined ->
                    case count(Req1, Max, 0) of
                        {ok, Size, Req} ->
                            parts(Req, Max, #{id => erlang:unique_integer([positive]),
                                              name => Name, size => Size, type => Type});
                        {too_large, Req} -> {too_large, Name, Req}
                    end;
                _ ->
                    {ok, _, Req} = count(Req1, infinity, 0),
                    parts(Req, Max, Found)
            end
    end.

%% Reads a part's body in chunks, keeping only its size.
count(Req0, Max, N0) ->
    {More, Data, Req} = cowboy_req:read_part_body(Req0, #{length => 64000}),
    N = N0 + byte_size(Data),
    if
        Max =/= infinity, N > Max -> {too_large, Req};
        More =:= more -> count(Req, Max, N);
        true -> {ok, N, Req}
    end.

reply(Status, Body, Req) ->
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>},
                     json:encode(Body), Req).
