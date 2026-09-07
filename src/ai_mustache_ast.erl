%%%-------------------------------------------------------------------
%%% @doc AST post-processing passes.
%%%
%%% Three pure passes run in a fixed order: merge_text, drop_empty,
%%% resolve_partials. Each is exported so it can be tested alone, which the
%%% old implementation could not do -- it folded them into parse/1.
%%%
%%% All passes descend through every body-bearing node via map_body/2. The old
%%% merge_continuous_binary/1 recursed into sections only, so static text
%%% inside a {{+x}} block was never merged (bug P2); routing every pass
%%% through one traversal makes that class of omission impossible.
%%%
%%% Accumulation is always [X | Acc] plus a final lists:reverse/1; the old
%%% `Acc ++ [X]' was the root of the O(n^2) post-processing (bug P1).
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_ast).

-include("ai_mustache.hrl").

-export([postprocess/2, merge_text/1, drop_empty/1, resolve_partials/2]).
-export([module_name/2, default_prefix/0]).

-type nodes() :: [ai_mustache_node()].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Run every pass in order.
%%
%% The order is fixed: drop_empty needs text already merged to judge whether a
%% body is empty, and resolve_partials goes last because it is the only pass
%% that can fail -- running it first would throw away the other two's work.
-spec postprocess(nodes(), map()) ->
          {ok, nodes(), [module()]} | ai_mustache_error().
postprocess(Nodes, Opts0) ->
    Opts = ai_html_text:opts(Opts0),
    resolve_partials(drop_empty(merge_text(Nodes)), Opts).

%%%===================================================================
%%% Traversal
%%%===================================================================

%% The single place that knows which nodes carry a body. A new node type only
%% needs to be added here.
-spec map_body(fun((nodes()) -> nodes()), ai_mustache_node()) -> ai_mustache_node().
map_body(F, {section,  L, K, B})    -> {section,  L, K, F(B)};
map_body(F, {inverted, L, K, B})    -> {inverted, L, K, F(B)};
map_body(F, {has,      L, K, B, P}) -> {has,      L, K, F(B), P};
map_body(F, {ext,      L, C, K, B}) -> {ext,      L, C, K, F(B)};
map_body(_, Node)                   -> Node.

%%%===================================================================
%%% Pass 1: merge adjacent text
%%%===================================================================

%% @doc Collapse runs of adjacent text nodes into one literal.
%%
%% Fewer literals means a shorter iolist at runtime and, more importantly,
%% each surviving literal lands in the module's literal pool as a single
%% binary shared across processes.
-spec merge_text(nodes()) -> nodes().
merge_text(Nodes) -> merge(Nodes, [], []).

-spec merge(nodes(), [{ai_mustache_loc(), binary()}], nodes()) -> nodes().
merge([], Pending, Acc) ->
    lists:reverse(flush(Pending, Acc));
merge([{text, Loc, Bin} | Rest], Pending, Acc) ->
    merge(Rest, [{Loc, Bin} | Pending], Acc);
merge([Node | Rest], Pending, Acc) ->
    merge(Rest, [], [map_body(fun merge_text/1, Node) | flush(Pending, Acc)]).

-spec flush([{ai_mustache_loc(), binary()}], nodes()) -> nodes().
flush([], Acc) ->
    Acc;
flush(Pending, Acc) ->
    [{Loc, _} | _] = Rev = lists:reverse(Pending),
    Bin = iolist_to_binary([B || {_, B} <- Rev]),
    [{text, Loc, Bin} | Acc].

%%%===================================================================
%%% Pass 2: drop empty bodies
%%%===================================================================

%% @doc Remove has and ext nodes whose body is empty.
%%
%% Sections and inverted sections are deliberately NOT dropped. `{{#w}}{{/w}}'
%% with a fun/2 in `w' must still call F(<<>>, Frame) and emit the result, and
%% the compiler cannot prove at build time that a key is not a function. An
%% empty section simply compiles to a body function returning [].
%%
%% The old remove_empty_section/1 also only walked the top level and carried a
%% dead `[<<>>]' clause that could never match; neither is reproduced here.
-spec drop_empty(nodes()) -> nodes().
drop_empty(Nodes) -> drop(Nodes, []).

-spec drop(nodes(), nodes()) -> nodes().
drop([], Acc) ->
    lists:reverse(Acc);
drop([Node | Rest], Acc) ->
    case map_body(fun drop_empty/1, Node) of
        {has, _, _, [], _} -> drop(Rest, Acc);
        {ext, _, _, _, []} = Ext ->
            %% An inline extension legitimately has no body; only a block-form
            %% extension that ended up empty is dropped. The two are
            %% indistinguishable here, so keep it and let the extension decide.
            drop(Rest, [Ext | Acc]);
        Kept -> drop(Rest, [Kept | Acc])
    end.

%%%===================================================================
%%% Pass 3: resolve partials
%%%===================================================================

%% @doc Turn each partial's raw path into its target module and collect deps.
%%
%% No cycle detection: partials compile to cross-module calls, so mutual
%% recursion between templates is legal (designs/02-architecture.md 2.2).
-spec resolve_partials(nodes(), map()) ->
          {ok, nodes(), [module()]} | ai_mustache_error().
resolve_partials(Nodes, Opts) ->
    case resolve(Nodes, Opts, [], []) of
        {error, _} = E    -> E;
        {ok, Out, Deps}   -> {ok, Out, lists:usort(Deps)}
    end.

-spec resolve(nodes(), map(), [module()], nodes()) ->
          {ok, nodes(), [module()]} | ai_mustache_error().
resolve([], _Opts, Deps, Acc) ->
    {ok, lists:reverse(Acc), Deps};
resolve([{partial, Loc, Name, Indent} | Rest], Opts, Deps, Acc) when is_binary(Name) ->
    case exists(Name, Opts) of
        false ->
            {error, {ai_html_text:source(Opts), element(1, Loc),
                     {partial_not_found, Name}}};
        true ->
            Mod = module_name(Name, Opts),
            resolve(Rest, Opts, [Mod | Deps], [{partial, Loc, Mod, Indent} | Acc])
    end;
resolve([Node | Rest], Opts, Deps, Acc) ->
    case body_of(Node) of
        none ->
            resolve(Rest, Opts, Deps, [Node | Acc]);
        Body ->
            case resolve(Body, Opts, Deps, []) of
                {error, _} = E -> E;
                {ok, Body1, Deps1} ->
                    resolve(Rest, Opts, Deps1,
                            [map_body(fun(_) -> Body1 end, Node) | Acc])
            end
    end.

-spec body_of(ai_mustache_node()) -> nodes() | none.
body_of({section,  _, _, B})    -> B;
body_of({inverted, _, _, B})    -> B;
body_of({has,      _, _, B, _}) -> B;
body_of({ext,      _, _, _, B}) -> B;
body_of(_)                      -> none.

%% Existence is only checked when a views root is configured. The inline
%% template path has no views directory; it rejects partials outright (T14).
%%
%% `views_abs' takes precedence over `views' when present. A relative `views'
%% is resolved against the current working directory, which is the project
%% root under rebar3 and therefore wrong for an application inside an
%% umbrella; the caller that knows the app's directory passes the resolved
%% path as `views_abs'. It is kept out of normalize_opts/1 so an absolute path
%% never reaches the build stamp.
-spec exists(binary(), map()) -> boolean().
exists(Name, Opts) ->
    case maps:get(views_abs, Opts, maps:get(views, Opts, undefined)) of
        undefined -> true;
        Views ->
            Suffix = maps:get(suffix, Opts, <<".mustache">>),
            filelib:is_regular(filename:join(Views, <<Name/binary, Suffix/binary>>))
    end.

%%%===================================================================
%%% Module naming
%%%===================================================================

-spec default_prefix() -> binary().
default_prefix() -> <<"view_">>.

%% @doc Map a template path to its generated module name.
%%
%% This is the only implementation of the mapping; the rebar3 plugin calls it
%% too when scanning the views directory, so the compiler's idea of a
%% partial's target and the plugin's idea of an output file cannot drift.
%%
%%   <<"index">>          -> view_index
%%   <<"shared/item">>    -> view_shared_item
%%   <<"layout/default">> -> view_layout_default
-spec module_name(binary(), map()) -> module().
module_name(Name, Opts) ->
    Prefix = maps:get(prefix, Opts, default_prefix()),
    Flat = << <<(norm_char(C))>> || <<C>> <= Name >>,
    binary_to_atom(<<Prefix/binary, Flat/binary>>, utf8).

-spec norm_char(char()) -> char().
norm_char($/) -> $_;
norm_char($-) -> $_;
norm_char($.) -> $_;
norm_char(C)  -> C.

