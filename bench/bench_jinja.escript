#!/usr/bin/env escript
%%! -noshell
%% Benchmark harness for the jinja engine.
%%
%% Templates are compiled to modules exactly as the rebar3 plugin does, so what
%% is measured is the code the plugin actually emits -- not render_string/3,
%% which would fold the compile into the first iteration.
%%
%% There is no old implementation to compare against here. What the numbers are
%% for is the two questions the design left open: whether the maps:merge/2 in
%% all_blocks/0 costs anything measurable (designs/11 section 2.1), and whether
%% a fully static template really does fold to one literal.
main([EbinDir, BenchDir, NStr, ItemsStr]) ->
    true = code:add_pathz(EbinDir),
    true = code:add_pathz(BenchDir),
    N = list_to_integer(NStr),
    Items = list_to_integer(ItemsStr),
    Views = filename:join([BenchDir, "jinja", "views"]),
    Base = #{views => list_to_binary(Views), views_abs => list_to_binary(Views),
             prefix => <<"j2_">>, suffix => <<".j2">>, line_map => false},
    [build(Name, Views, Base)
     || Name <- [<<"page.j2">>, <<"shared/row.j2">>, <<"static.j2">>,
                 <<"shared/base.j2">>, <<"inherit.j2">>, <<"flat.j2">>]],
    Ctx = bench_ctx:context(Items),
    Out = j2_page:render(Ctx),
    ok = file:write_file(filename:join(BenchDir, "out_jinja.txt"), Out),
    io:format("bytes ~p~n", [byte_size(Out)]),
    report("page render/1       ", fun() -> j2_page:render(Ctx) end, N),
    report("page render_iolist/1", fun() -> j2_page:render_iolist(Ctx) end, N),
    %% The inheritance question: one maps:merge per render, at the chain entry.
    %% flat.j2 renders the identical bytes without extending anything, so the
    %% difference between the two IS the cost of inheritance.
    Same = j2_inherit:render(Ctx) =:= j2_flat:render(Ctx),
    io:format("inherit and flat render identical bytes: ~p~n", [Same]),
    report("inherit render/1    ", fun() -> j2_inherit:render(Ctx) end, N),
    report("flat render/1       ", fun() -> j2_flat:render(Ctx) end, N),
    report("static render/1     ", fun() -> j2_static:render(Ctx) end, N),
    io:format("~nstatic template folded to one literal: ~p~n",
              [folded(j2_static)]).

build(Name, Views, Base) ->
    Path = filename:join(Views, binary_to_list(Name)),
    {ok, Body} = file:read_file(Path),
    Mod = ai_jinja_ast:module_name(Name, Base),
    Opts = Base#{module => Mod, source => list_to_binary(Path),
                 stamp => ai_jinja_compiler:source_hash(Body, Base), mtime => 0},
    {ok, Ast} = ai_jinja_parser:parse(Body, Opts),
    {ok, Forms, _} = ai_jinja_compiler:forms(Ast, Opts),
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors, binary]),
    {module, Mod} = code:load_binary(Mod, Path, Bin).

%% A template with no dynamic nodes must not build an iolist at all.
folded(Mod) ->
    is_binary(Mod:render(#{})) andalso length(Mod:render_iolist(#{})) =:= 1.

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
