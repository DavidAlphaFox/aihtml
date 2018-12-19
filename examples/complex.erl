-module(complex).
-compile(export_all).

-define(COUNT, 500).

context()->
    A = maps:from_list([{<<"name">>, "red"}, {<<"current">>, true}, {<<"url">>, "#Red"}]),
    B = maps:from_list([{<<"name">>, "green"}, {<<"current">>, false}, {<<"url">>, "#Green"}]),
    C = maps:from_list([{<<"name">>, "blue"}, {<<"current">>, false}, {<<"url">>, "#Blue"}]),
    #{
        <<"items">> => [A,B,C],
        <<"header">> => <<"Colors">>,
        <<"list">> => true,
        <<"empty">> => false
    }.



%%---------------------------------------------------------------------------

start() ->
  code:add_patha("../ebin"),
  code:add_patha("../deps/ailib/ebin"),
  application:start(ailib),
  application:start(aihtml),
  
  {ok,CWD} = file:get_cwd(),
  ai_mustache:prepare(CWD),

  {ok,Body} = file:read_file("./complex.mustache"),
  {IR,Partials} = ai_mustache_parser:parse(Body),
  io:format("Partials ~p~n",[Partials]),
  {ok,Partial} = file:read_file("./_item.mustache"),
  {PartialIR,_RestPartials} =  ai_mustache_parser:parse(Partial),
  Output = ai_mustache_runner:render(IR,#{<<"_item">> => {PartialIR,undefined}},context()),
  io:format("~ts~n",[Output]),
  T0 = os:timestamp(),
  render(IR, context(), #{<<"_item">> => {PartialIR,undefined}}, ?COUNT),
  T1 = os:timestamp(),
  Diff = timer:now_diff(T1, T0),
  Mean = Diff / ?COUNT,
  io:format("~nTotal time: ~.2fs~n", [Diff / 1000000]),
  io:format("Mean render time: ~.2fms~n", [Mean / 1000]).


  render(_CT, _Ctx, _Partial,0) ->
    ok;
  render(IR, Ctx,Partial,N) ->
    ai_mustache_runner:render(IR,Partial,Ctx),
    render(IR, Ctx,Partial,N - 1).