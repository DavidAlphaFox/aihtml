#!/usr/bin/env escript
%%! -noshell
%% Regenerates the component section of apps/aihtml/src/aihtml.erl and the
%% component imports of apps/aihtml/include/aihtml.hrl from the component
%% group modules (aihtml_catalog:groups/0).
%%
%% For every group module it takes the exported functions named in the
%% module's catalog, plus the functions listed in its `facade_extras/0' if
%% it exports one, and writes a delegating wrapper with the module's own
%% -spec (local types become Module:type() when exported, term() if not).
%%
%%   escript scripts/gen-facade.escript [--out Dir] [ExtraEbinDir...]
%%
%% With --out the sources are left alone: Dir/aihtml.erl and
%% Dir/aihtml/include/ (aihtml.hrl generated, the other headers symlinked)
%% are written instead, for checking a group that is not integrated yet:
%% compile Dir/aihtml.erl with `-I Dir/aihtml/include' and the modules
%% that include aihtml.hrl with `-I Dir'.
main(["--out", Out | Args]) ->
    put(out, filename:absname(Out)),
    main(Args);
main(Args) ->
    Root = filename:dirname(filename:dirname(filename:absname(escript:script_name()))),
    [code:add_patha(D) || D <- filelib:wildcard(filename:join(Root, "_build/default/lib/*/ebin"))],
    [code:add_patha(D) || D <- Args],
    Groups = [M || M <- aihtml_catalog:groups(), M =/= aihtml_theme,
                   code:ensure_loaded(M) =:= {module, M}],
    Entries = lists:append([entries(M) || M <- Groups]),
    check_clashes(Entries),
    Code = [["%% ", atom_to_list(M), "\n",
             [wrapper(M, F, A, Spec) || {M1, F, A, Spec} <- Entries, M1 =:= M], "\n"]
            || M <- Groups],
    Exports = [io_lib:format("~p/~b", [F, A]) || {_, F, A, _} <- Entries],
    ExportAttr = ["-export([", lists:join(",\n         ", chunk(Exports)), "]).\n"],
    {Erl, Hrl} = targets(Root, get(out)),
    replace(Erl, "%% BEGIN GENERATED EXPORTS", "%% END GENERATED EXPORTS", ExportAttr),
    replace(Erl, "%% BEGIN GENERATED COMPONENTS", "%% END GENERATED COMPONENTS", Code),
    replace(Hrl, "%% BEGIN GENERATED COMPONENT IMPORTS", "%% END GENERATED COMPONENT IMPORTS",
            ["-import(aihtml,\n        [", lists:join(",\n         ", chunk(Exports)), "]).\n"]),
    io:format("~b component functions from ~p~n", [length(Entries), Groups]).

entries(M) ->
    Names = [N || #{name := N} <- M:catalog()],
    Extras = case erlang:function_exported(M, facade_extras, 0) of
                 true -> M:facade_extras();
                 false -> []
             end,
    {Specs, ExportedTypes} = specs(M),
    [{M, F, A, fix_types(maps:get({F, A}, Specs, undefined), M, ExportedTypes)}
     || {F, A} <- M:module_info(exports),
        lists:member(F, Names) orelse lists:member({F, A}, Extras)].

specs(M) ->
    {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} =
        beam_lib:chunks(code:which(M), [abstract_code]),
    Specs = maps:from_list([{FA, Types} || {attribute, _, spec, {FA, Types}} <- Forms]),
    Exported = lists:append([Ts || {attribute, _, export_type, Ts} <- Forms]),
    Defs = maps:from_list([{{N, length(Ps)}, {Ps, Def}}
                           || {attribute, _, T, {N, Def, Ps}} <- Forms, T =:= type orelse T =:= opaque]),
    put(type_defs, Defs),
    {Specs, Exported}.

%% Local types become remote (when exported) or term().
fix_types(undefined, _, _) -> undefined;
fix_types(Types, M, Exported) ->
    erl_parse:map_anno(fun(A) -> A end,
                       walk(Types, M, Exported)).

walk({user_type, L, Name, Params}, M, Exported) ->
    Defs = get(type_defs),
    case {lists:member({Name, length(Params)}, Exported), maps:find({Name, length(Params)}, Defs)} of
        {true, _} ->
            {remote_type, L, [{atom, L, M}, {atom, L, Name},
                              [walk(P, M, Exported) || P <- Params]]};
        {false, {ok, {[], Def}}} ->
            %% a local alias without parameters: inline its definition
            %% (guarded against recursive types)
            Seen = case get(inlining) of undefined -> []; S -> S end,
            case lists:member(Name, Seen) of
                true -> {type, L, term, []};
                false ->
                    put(inlining, [Name | Seen]),
                    R = walk(Def, M, Exported),
                    put(inlining, Seen),
                    R
            end;
        _ ->
            {type, L, term, []}
    end;
walk(T, M, Exported) when is_tuple(T) ->
    list_to_tuple([walk(E, M, Exported) || E <- tuple_to_list(T)]);
walk(L, M, Exported) when is_list(L) ->
    [walk(E, M, Exported) || E <- L];
walk(X, _, _) -> X.

wrapper(M, F, A, Spec) ->
    Vars = [lists:flatten(io_lib:format("A~b", [I])) || I <- lists:seq(1, A)],
    SpecText = case Spec of
                   undefined -> "";
                   Types -> erl_pp:attribute({attribute, 0, spec, {{F, A}, Types}})
               end,
    [SpecText,
     io_lib:format("~p(~s) -> ~p:~p(~s).~n",
                   [F, lists:join(", ", Vars), M, F, lists:join(", ", Vars)])].

check_clashes(Entries) ->
    Own = own_functions(),
    Dups = [F || {_, F, _, _} <- Entries, lists:member(F, Own)],
    Dups =:= [] orelse error({clash_with_aihtml_functions, lists:usort(Dups)}),
    Names = [{F, A} || {_, F, A, _} <- Entries],
    length(Names) =:= length(lists:usort(Names))
        orelse error({duplicate_component_functions, Names -- lists:usort(Names)}).

%% Names defined by hand in aihtml.erl (outside the generated section):
%% tags and helpers that component names must not reuse.
own_functions() ->
    {ok, Bin} = file:read_file(code:where_is_file("aihtml.erl") =/= non_existing
                               andalso code:where_is_file("aihtml.erl")
                               orelse source()),
    [Before, Rest] = string:split(Bin, "%% BEGIN GENERATED COMPONENTS"),
    [_, After] = string:split(Rest, "%% END GENERATED COMPONENTS"),
    {match, Ms} = re:run([Before, After], "^('?[a-z][a-z0-9_]*'?)\\(", [multiline, global,
                                                                         {capture, all_but_first, list}]),
    lists:usort([list_to_atom(string:trim(N, both, "'")) || [N] <- Ms]).

source() ->
    Root = filename:dirname(filename:dirname(filename:absname(escript:script_name()))),
    filename:join(Root, "apps/aihtml/src/aihtml.erl").

chunk(L) -> L.

targets(Root, undefined) ->
    {filename:join(Root, "apps/aihtml/src/aihtml.erl"),
     filename:join(Root, "apps/aihtml/include/aihtml.hrl")};
targets(Root, Out) ->
    Inc = filename:join([Out, "aihtml", "include"]),
    ok = filelib:ensure_path(Inc),
    Src = filename:join(Root, "apps/aihtml/include"),
    [begin
         Link = filename:join(Inc, F),
         _ = file:delete(Link),
         case F of
             "aihtml.hrl" -> {ok, _} = file:copy(filename:join(Src, F), Link);
             _ -> ok = file:make_symlink(filename:join(Src, F), Link)
         end
     end || F <- filelib:wildcard("*.hrl", Src)],
    Erl = filename:join(Out, "aihtml.erl"),
    {ok, _} = file:copy(filename:join(Root, "apps/aihtml/src/aihtml.erl"), Erl),
    {Erl, filename:join(Inc, "aihtml.hrl")}.

replace(File, Begin, End, New) ->
    {ok, Bin} = file:read_file(File),
    [Before, Rest] = string:split(Bin, Begin),
    [_, After] = string:split(Rest, End),
    ok = file:write_file(File, [Before, Begin, "\n", New, End, After]).
