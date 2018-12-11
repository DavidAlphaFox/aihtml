-module(complex).
-compile(export_all).

get_value(<<"header">>,_Ctx) -> "Colors";
get_value(<<"items">>,Ctx) ->
  io:format("items is called: ~p~n",[Ctx]),
  A = maps:from_list([{<<"name">>, "red"}, {<<"current">>, true}, {<<"url">>, "#Red"}]),
  B = maps:from_list([{<<"name">>, "green"}, {<<"current">>, false}, {<<"url">>, "#Green"}]),
  C = maps:from_list([{<<"name">>, "blue"}, {<<"current">>, false}, {<<"url">>, "#Blue"}]),
  [A, B, C];

get_value(<<"link">>,Ctx) ->
  io:format("link is called: ~p~n",[Ctx]),
  ai_mustache_context:get_value(<<"current">>,Ctx);

get_value(<<"list">>,_Ctx) -> true;
get_value(<<"empty">>,_Ctx) -> false.


%%---------------------------------------------------------------------------

start() ->
  code:add_patha("../ebin"),
  code:add_patha("../deps/ailib/ebin"),
  {ok,Body} = file:read_file("./complex.mustache"),
  {ok,Partial} = file:read_file("./_item.mustache"),
  {ok,PartialFun} = ai_mustache:compile(Partial),
  Ctx = ai_mustache_context:new(),
  Ctx0 = ai_mustache_context:module(complex,Ctx),
  Ctx1 = ai_mustache_context:partial(<<"_item">>,PartialFun,Ctx0),
  Output = ai_mustache:render(Body,Ctx1),
  io:format(Output, []).