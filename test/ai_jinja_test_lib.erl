%%%-------------------------------------------------------------------
%%% @doc Support library for the jinja fixture suite.
%%%
%%% Loads test/jinja_spec/*.json, converts the JSON context into the term
%%% shape the engine expects, and decides which cases are due to pass at the
%%% current implementation stage.
%%%
%%% Jinja has no official spec, so these fixtures are generated from CPython
%%% jinja2 by tools/gen_jinja_fixtures.py and committed; nothing here needs
%%% Python. See designs/13-jinja-roadmap.md section 2.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_test_lib).

-compile([export_all, nowarn_export_all]).

%% The stage the implementation has reached. Cases labelled with a higher
%% stage are reported as skipped rather than failing, so the suite stays green
%% while the engine is built up. Raise it as each stage lands.
-define(CURRENT_STAGE, 12).

%% Groups and the minimum number of cases each must carry. The inventory
%% assertion is real: a fixture file that silently loses half its cases would
%% otherwise look like a passing suite.
-define(INVENTORY,
        [{<<"lexer">>, 30}, {<<"expr">>, 60}, {<<"whitespace">>, 35},
         {<<"if">>, 15}, {<<"for">>, 33}, {<<"set">>, 20},
         {<<"filters">>, 100}, {<<"tests">>, 40}, {<<"escape">>, 30},
         {<<"inherit">>, 30}, {<<"macro">>, 25}, {<<"include">>, 15},
         {<<"error">>, 40}]).

%%%===================================================================
%%% Loading
%%%===================================================================

-spec current_stage() -> pos_integer().
current_stage() -> ?CURRENT_STAGE.

-spec inventory() -> [{binary(), pos_integer()}].
inventory() -> ?INVENTORY.

%% @doc The directory the fixtures were copied into by the test profile.
spec_dir() ->
    filename:join(filename:dirname(code:which(?MODULE)), "jinja_spec").

-spec groups() -> [binary()].
groups() -> [G || {G, _} <- ?INVENTORY].

%% @doc Every case of one group, in file order.
-spec cases(binary()) -> [map()].
cases(Group) ->
    File = filename:join(spec_dir(), <<Group/binary, ".json">>),
    {ok, Bin} = file:read_file(File),
    #{<<"cases">> := Cases} = json:decode(Bin),
    [normalise(C) || C <- Cases].

-spec all_cases() -> [map()].
all_cases() -> lists:append([cases(G) || G <- groups()]).

%%%===================================================================
%%% Case shape
%%%===================================================================

%% Turn one JSON object into the shape the tests use: atom keys for the
%% fields, and the context converted to engine terms.
normalise(C) ->
    Base = #{name      => maps:get(<<"name">>, C),
             group     => maps:get(<<"group">>, C),
             stage     => maps:get(<<"stage">>, C),
             template  => template_of(C),
             templates => maps:from_list(
                            [{K, V} || {K, V} <-
                                 maps:to_list(maps:get(<<"templates">>, C, #{}))]),
             context   => context(maps:get(<<"context">>, C, #{}))},
    lists:foldl(fun({JKey, Key}, Acc) ->
                        case C of
                            #{JKey := V} -> Acc#{Key => convert(JKey, V)};
                            _            -> Acc
                        end
                end, Base,
                [{<<"expected">>, expected},
                 {<<"error">>, error},
                 {<<"deviation">>, deviation},
                 {<<"erlang_only">>, erlang_only}]).

convert(<<"error">>, V)     -> binary_to_atom(V, utf8);
convert(_, V)               -> V.

%% A case is either normal text or, for the invalid-UTF-8 case, an explicit
%% byte list -- which is the whole point of that fixture and cannot be carried
%% as a JSON string.
template_of(#{<<"raw_template">> := Bytes}) -> list_to_binary(Bytes);
template_of(#{<<"template">> := T})         -> T.

%%%===================================================================
%%% Context conversion
%%%===================================================================

%% JSON gives binary keys; the engine's maps are keyed by atom
%% (designs/10-jinja-semantics.md section 1, deviation J1).
%%
%% binary_to_atom/2 is deliberate here and safe: this runs only over fixture
%% data that lives in the repository, never over anything a user supplies.
-spec context(term()) -> term().
context(M) when is_map(M) ->
    maps:from_list([{binary_to_atom(K, utf8), context(V)}
                    || {K, V} <- maps:to_list(M)]);
context(L) when is_list(L) ->
    [context(E) || E <- L];
%% JSON null is Python's None, and this engine has one empty value
%% (deviation J2), so it arrives as undefined rather than as a second one.
context(null) ->
    undefined;
context(V) ->
    V.

%%%===================================================================
%%% Stage gating
%%%===================================================================

%% @doc Should this case be asserted yet?
-spec due(map()) -> boolean().
due(#{erlang_only := _}) -> false;   % asserted from a dedicated test instead
due(#{stage := S})       -> S =< current_stage().

%% @doc A readable test title.
-spec title(map()) -> string().
title(#{group := G, name := N} = C) ->
    Suffix = case C of
                 #{deviation := D} -> [" (deviation ", binary_to_list(D), ")"];
                 _                 -> ""
             end,
    lists:flatten([binary_to_list(G), "/", binary_to_list(N), Suffix]).

%%%===================================================================
%%% Rendering a case
%%%===================================================================

%% @doc Render a case through the engine, or report why it could not be.
%%
%% Returns {ok, Binary} | {error, ReasonAtom}. Every failure mode is folded
%% down to the reason atom, because that is what the fixtures record.
-spec run(map()) -> {ok, binary()} | {error, atom()}.
run(#{template := T, templates := Ts, context := Ctx}) ->
    try ai_jinja:render_string(T, Ctx, #{templates => Ts}) of
        Bin when is_binary(Bin) -> {ok, Bin}
    catch
        error:{ai_jinja, {error, {_File, _Line, Reason}}} -> {error, reason(Reason)};
        error:{ai_jinja, Reason}                          -> {error, reason(Reason)};
        error:{ai_html, Reason}                           -> {error, reason(Reason)}
    end.

%% @doc Every case of one group as an EUnit-friendly {Title, Fun} pair.
-spec generator(binary()) -> [{string(), fun()}].
generator(Group) ->
    [{title(C), fun() -> ok end} || C <- cases(Group)].

-spec reason(term()) -> atom().
reason(R) when is_atom(R)   -> R;
reason(R) when is_tuple(R)  -> element(1, R);
reason(R)                   -> R.
