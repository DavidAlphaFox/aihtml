%% Every shared template renders its fixtures to the same bytes in Erlang
%% (beamai_render, via the component module's tpl_<name>/1) and in the
%% browser compiler (scripts/mustache.mjs, run through node).
-module(aihtml_tpl_tests).

-include_lib("eunit/include/eunit.hrl").

templates_match_across_languages_test_() ->
    Names = aihtml_tpl:names(),
    [{binary_to_list(N), fun() -> check(N) end} || N <- Names].

every_template_has_fixtures_and_an_erlang_function_test() ->
    [begin
         ?assert(filelib:is_regular(fixtures_file(N))),
         ?assertNotEqual(none, erlang_fun(N))
     end || N <- aihtml_tpl:names()].

check(Name) ->
    {ok, Json} = file:read_file(fixtures_file(Name)),
    Fixtures = json:decode(Json),
    Fun = erlang_fun(Name),
    Erl = [element(2, aihtml_tpl:safe(Fun(atomize(F)))) || F <- Fixtures],
    Out = os:cmd("cd " ++ project_root(aihtml_tpl:dir())
                 ++ " && node scripts/render-tpl.mjs " ++ binary_to_list(Name)),
    Js = json:decode(unicode:characters_to_binary(Out)),
    ?assertEqual(length(Erl), length(Js)),
    [?assertEqual({Name, I, J}, {Name, I, E})
     || {I, E, J} <- lists:zip3(lists:seq(1, length(Erl)), Erl, Js)].

%% The directory holding scripts/render-tpl.mjs, above the templates.
project_root(Dir) ->
    case filelib:is_regular(filename:join([Dir, "scripts", "render-tpl.mjs"])) of
        true -> Dir;
        false when Dir =/= "/" -> project_root(filename:dirname(Dir));
        false -> error(no_project_root)
    end.

fixtures_file(Name) ->
    filename:join(aihtml_tpl:dir(), <<Name/binary, ".fixtures.json">>).

%% tpl_<name>/1 exported by one of aihtml's modules (a component module,
%% or a shared aihtml_lib_* module when several components use the template)
erlang_fun(Name) ->
    F = binary_to_atom(<<"tpl_", Name/binary>>),
    case [M || M <- aihtml_modules(),
               code:ensure_loaded(M) =:= {module, M},
               erlang:function_exported(M, F, 1)] of
        [M | _] -> fun M:F/1;
        [] -> none
    end.

%% Every aihtml_* module on the code path, the first copy of each name.
aihtml_modules() ->
    lists:usort([list_to_atom(filename:basename(B, ".beam"))
                 || D <- code:get_path(), B <- filelib:wildcard("aihtml_*.beam", D)]).

%% JSON objects -> maps with atom keys (beamai_render looks keys up as
%% atoms); strings stay binaries, null stays null (falsy on both sides).
atomize(M) when is_map(M) -> #{binary_to_atom(K) => atomize(V) || K := V <- M};
atomize(L) when is_list(L) -> [atomize(E) || E <- L];
atomize(V) -> V.
