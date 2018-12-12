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
get_value(<<"empty">>,_Ctx) -> false;
get_value(Any,_Ctx)->
  io:format("get value for ~p~n",[Any]).


%%---------------------------------------------------------------------------

start() ->
  code:add_patha("../ebin"),
  code:add_patha("../deps/ailib/ebin"),
  {ok,Body} = file:read_file("./complex.mustache"),
  {ok,Partial} = file:read_file("./_item.mustache"),
  {module,Module} = ai_mustache_compiler:compile(complex,Body),
  {module,PartialModule} = ai_mustache_compiler:compile(item,Partial),
  Output = Module:render(#{<<"_item">> =>PartialModule}),
  io:format("~ts", [Output]).