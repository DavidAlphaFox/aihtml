#!/usr/bin/env escript
%%! -noshell
%% Benchmark harness for the current implementation.
%% Templates are compiled to modules exactly as the rebar3 plugin does.
main([EbinDir, BenchDir, NStr, ItemsStr]) ->
    true = code:add_pathz(EbinDir),
    true = code:add_pathz(BenchDir),
    N = list_to_integer(NStr),
    Items = list_to_integer(ItemsStr),
    Views = filename:join([BenchDir, "new", "views"]),
    Base = #{views => list_to_binary(Views), prefix => <<"view_">>,
             suffix => <<".mustache">>, line_map => false},
    [build(Name, Views, Base) || Name <- [<<"page">>, <<"shared/row">>]],
    Ctx = bench_ctx:context(Items),
    Out = view_page:render(Ctx),
    ok = file:write_file(filename:join(BenchDir, "out_new.txt"), Out),
    io:format("bytes ~p~n", [byte_size(Out)]),
    report("render/1        ", fun() -> view_page:render(Ctx) end, N),
    report("render_iolist/1 ", fun() -> view_page:render_iolist(Ctx) end, N).

build(Name, Views, Base) ->
    Path = filename:join(Views, binary_to_list(Name) ++ ".mustache"),
    {ok, Body} = file:read_file(Path),
    Mod = ai_mustache_ast:module_name(Name, Base),
    Opts = Base#{module => Mod, source => list_to_binary(Path),
                 stamp => ai_mustache_compiler:source_hash(Body, Base), mtime => 0},
    {ok, Ast} = ai_mustache_parser:parse(Body, Opts),
    {ok, Forms, _} = ai_mustache_compiler:forms(Ast, Opts),
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors, binary]),
    {module, Mod} = code:load_binary(Mod, Path, Bin).

report(Label, F, N) ->
    loop(F, 1000),                        % warm up
    Runs = [begin {T, _} = timer:tc(fun() -> loop(F, N) end), T end
            || _ <- lists:seq(1, 7)],
    Sorted = lists:sort(Runs),
    Median = lists:nth(4, Sorted),
    io:format("~s median ~10.2f us/iter   (min ~8.2f  max ~8.2f)~n",
              [Label, Median / N, hd(Sorted) / N, lists:last(Sorted) / N]).

loop(_F, 0) -> ok;
loop(F, N)  -> _ = F(), loop(F, N - 1).
