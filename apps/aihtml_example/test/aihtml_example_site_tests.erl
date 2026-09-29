%% The demo site: every component has demos, every demo renders, its
%% source can be shown, and every docs page and the home page render.
-module(aihtml_example_site_tests).

-include_lib("eunit/include/eunit.hrl").

site_test_() ->
    {setup,
     fun() -> {ok, Apps} = application:ensure_all_started(aihtml), Apps end,
     fun(Apps) -> [application:stop(A) || A <- lists:reverse(Apps)] end,
     [{"every component has demos", fun every_component_has_demos/0},
      {"every demo renders and shows its source", fun demos_render/0},
      {"every docs page renders", fun docs_pages_render/0},
      {"the home page renders", fun home_renders/0},
      {"the API tab shows each component's record", fun records_shown/0}]}.

components() ->
    [N || #{name := N} <- aihtml_example_site:components()].

every_component_has_demos() ->
    Missing = [N || N <- components(), aihtml_example_demos:for(N) =:= []],
    ?assertEqual([], Missing),
    %% and no demo entry names a component the catalog does not know
    Known = components(),
    ?assertEqual([], [C || #{component := C} <- aihtml_example_demos:all(),
                           not lists:member(C, Known)]).

demos_render() ->
    [begin
         Html = aihtml:render_binary(M:F()),
         ?assert(byte_size(Html) > 0),
         Src = aihtml_example_source:function(M, F),
         ?assertMatch({match, _}, re:run(Src, <<"^", (atom_to_binary(F))/binary, "\\(\\)">>)),
         ?assert(is_binary(aihtml:render_binary(aihtml_example_source:highlight(Src))))
     end || N <- components(), {_, M, F} <- aihtml_example_demos:for(N)].

docs_pages_render() ->
    [?assertMatch(<<_/binary>>, aihtml:render_binary(aihtml_example_docs:render(N)))
     || N <- components()].

home_renders() ->
    Html = aihtml:render_binary(aihtml_example_home:render()),
    [?assertMatch({_, _}, binary:match(Html, <<"/components/", (atom_to_binary(N))/binary, "\"">>))
     || N <- components()].

%% Components without an element record: the theme switcher and toast
%% (an action, not an element).
-define(NO_RECORD, [theme_switcher, toast]).

records_shown() ->
    [case lists:member(N, ?NO_RECORD) of
         true ->
             ?assertEqual(undefined, aihtml_example_records:record(N));
         false ->
             #{record := Rec, header := <<"aihtml_", _/binary>>, doc := Doc,
               fields := Fields} = aihtml_example_records:record(N),
             ?assertNotEqual(<<>>, Doc),
             ?assertNotEqual([], Fields),
             [?assertNot(lists:member(F, aihtml_example_records:base_fields()))
              || #{name := F} <- Fields],
             Html = aihtml:render_binary(aihtml_example_docs:render(N)),
             ?assertMatch({_, _}, binary:match(Html, <<"#", (atom_to_binary(Rec))/binary, "{}">>))
     end || N <- components()].
