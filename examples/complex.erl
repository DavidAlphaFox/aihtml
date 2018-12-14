-module(complex).
-compile(export_all).

-define(COUNT, 500).

get_value(<<"header">>,_Ctx) -> "Colors";
get_value(<<"items">>,_Ctx) ->
  A = maps:from_list([{<<"name">>, "red"}, {<<"current">>, true}, {<<"url">>, "#Red"}]),
  B = maps:from_list([{<<"name">>, "green"}, {<<"current">>, false}, {<<"url">>, "#Green"}]),
  C = maps:from_list([{<<"name">>, "blue"}, {<<"current">>, false}, {<<"url">>, "#Blue"}]),
  [A, B, C];

get_value(<<"link">>,Ctx) ->
  ai_mustache_context:get_value(<<"current">>,Ctx);

get_value(<<"list">>,_Ctx) -> true;
get_value(<<"empty">>,_Ctx) -> false.



%%---------------------------------------------------------------------------

start() ->
  code:add_patha("../ebin"),
  code:add_patha("../deps/ailib/ebin"),
  {ok,Body} = file:read_file("./complex.mustache"),
  {ok,Partial} = file:read_file("./_item.mustache"),
  {module,Module} = ai_mustache_compiler:compile(complex,Body),
  {module,PartialModule} = ai_mustache_compiler:compile(item,Partial),
  Output = Module:render(#{<<"_item">> =>PartialModule}),
  io:format("~ts~n",[Output]),
  T0 = os:timestamp(),
  render(Module, #{<<"_item">> =>PartialModule}, ?COUNT),
  T1 = os:timestamp(),
  Diff = timer:now_diff(T1, T0),
  Mean = Diff / ?COUNT,
  io:format("~nTotal time: ~.2fs~n", [Diff / 1000000]),
  io:format("Mean render time: ~.2fms~n", [Mean / 1000]).


  render(_CT, _Ctx, 0) ->
    ok;
  render(Module, Ctx, N) ->
    Module:render(Ctx),
    render(Module, Ctx, N - 1).