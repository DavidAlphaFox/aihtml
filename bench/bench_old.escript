#!/usr/bin/env escript
%%! -noshell
%% Benchmark harness for aihtml v0.3.7 (commit 82c01eb), which needs ailib.
%% Measured on its steady-state path: the first render populates the process
%% dictionary cache, and every later render hits it, so timing must start
%% after a warm-up or the ets and parse costs would be attributed to it.
main([EbinDir, AilibDir, BenchDir, NStr, ItemsStr]) ->
    true = code:add_pathz(EbinDir),
    true = code:add_pathz(AilibDir),
    true = code:add_pathz(BenchDir),
    N = list_to_integer(NStr),
    Items = list_to_integer(ItemsStr),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = ai_mustache_loader:start_link(),
    Views = list_to_binary(filename:join([BenchDir, "old", "views"])),
    ok = ai_mustache:bootstrap(#{views => Views}),
    Ctx = bench_ctx:context(Items),
    Out = ai_mustache:render("page", Ctx),
    ok = file:write_file(filename:join(BenchDir, "out_old.txt"), Out),
    io:format("bytes ~p~n", [byte_size(Out)]),
    report("render/2        ", fun() -> ai_mustache:render("page", Ctx) end, N).

report(Label, F, N) ->
    loop(F, 1000),
    Runs = [begin {T, _} = timer:tc(fun() -> loop(F, N) end), T end
            || _ <- lists:seq(1, 7)],
    Sorted = lists:sort(Runs),
    Median = lists:nth(4, Sorted),
    io:format("~s median ~10.2f us/iter   (min ~8.2f  max ~8.2f)~n",
              [Label, Median / N, hd(Sorted) / N, lists:last(Sorted) / N]).

loop(_F, 0) -> ok;
loop(F, N)  -> _ = F(), loop(F, N - 1).
