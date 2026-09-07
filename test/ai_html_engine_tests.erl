%%%-------------------------------------------------------------------
%%% Tests for the engine behaviour and its mustache adapter.
%%%
%%% Two things are worth asserting: the adapter really is nothing but
%%% forwarding, and the engine-neutral header has not started collecting
%%% engine-specific types.
%%%-------------------------------------------------------------------
-module(ai_html_engine_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CALLBACKS,
        [{parse, 2}, {forms, 2}, {source_hash, 2}, {module_name, 2},
         {attribute, 0}, {default_suffix, 0}, {default_prefix, 0},
         {config_key, 0}, {known_keys, 0}, {banner_tag, 0}]).

behaviour_declares_every_callback_test() ->
    ?assertEqual(lists:sort(?CALLBACKS),
                 lists:sort(ai_html_engine:behaviour_info(callbacks))).

adapter_implements_every_callback_test() ->
    Exports = ai_mustache_engine:module_info(exports),
    [?assert(lists:member(C, Exports)) || C <- ?CALLBACKS].

adapter_declares_the_behaviour_test() ->
    Attrs = ai_mustache_engine:module_info(attributes),
    ?assertEqual([ai_html_engine],
                 proplists:get_value(behaviour, Attrs,
                                     proplists:get_value(behavior, Attrs))).

adapter_constants_test() ->
    ?assertEqual(mustache_source, ai_mustache_engine:attribute()),
    ?assertEqual(".mustache",     ai_mustache_engine:default_suffix()),
    ?assertEqual(<<"view_">>,     ai_mustache_engine:default_prefix()),
    ?assertEqual(mustache_opts,   ai_mustache_engine:config_key()),
    ?assertEqual("mustache",      ai_mustache_engine:banner_tag()),
    ?assert(lists:member(views, ai_mustache_engine:known_keys())),
    ?assert(lists:member(ext_opts, ai_mustache_engine:known_keys())).

%% parse/2 -> forms/2 must be a working pipeline through the adapter alone.
adapter_round_trip_test() ->
    Opts = #{module => ai_html_engine_tests_view, source => <<"t.mustache">>},
    {ok, Ast} = ai_mustache_engine:parse(<<"Hello {{name}}!">>, Opts),
    {ok, Forms, Deps} = ai_mustache_engine:forms(Ast, Opts),
    ?assertEqual([], Deps),
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
    {module, Mod} = code:load_binary(Mod, "t.mustache", Bin),
    ?assertEqual(<<"Hello world!">>, Mod:render(#{name => <<"world">>})),
    true = code:delete(Mod), _ = code:purge(Mod),
    ok.

adapter_module_name_and_stamp_test() ->
    ?assertEqual(view_a_b,
                 ai_mustache_engine:module_name(<<"a/b">>, #{})),
    S1 = ai_mustache_engine:source_hash(<<"x">>, #{}),
    S2 = ai_mustache_compiler:source_hash(<<"x">>, #{}),
    ?assertEqual(S2, S1).

%% The neutral header must not grow engine types. Checked textually because
%% that is exactly the drift being guarded against.
neutral_header_stays_neutral_test() ->
    Dir = filename:dirname(code:which(?MODULE)),
    Hrl = find_up(Dir, "include/ai_html.hrl", 6),
    {ok, Bin} = file:read_file(Hrl),
    Txt = unicode:characters_to_list(Bin),
    ?assertEqual(nomatch, string:find(Txt, "ai_mustache_node")),
    ?assertEqual(nomatch, string:find(Txt, "ai_jinja_node")),
    %% types, macros and comments only
    ?assertEqual(nomatch, string:find(Txt, "-export(")).

find_up(_Dir, Rel, 0) -> erlang:error({not_found, Rel});
find_up(Dir, Rel, N) ->
    Candidate = filename:join(Dir, Rel),
    case filelib:is_regular(Candidate) of
        true  -> Candidate;
        false -> find_up(filename:dirname(Dir), Rel, N - 1)
    end.
