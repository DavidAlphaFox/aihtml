%%%-------------------------------------------------------------------
%%% @doc Mustache parser: token stream -> AST.
%%%
%%% Recursive descent with an explicit open-block record, so an unclosed
%%% section reports the line of its opening tag rather than the end of the
%%% file. Produces the raw AST only; merging, pruning and partial resolution
%%% are separate passes in ai_mustache_ast (T09) so each can be tested alone.
%%%
%%% Derived in part from bbmustache (MIT, Copyright (c) 2015 Hinagiku Soranoba).
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_parser).

-include("ai_mustache.hrl").

-export([parse/1, parse/2, keys/1, partial_name/1]).

-record(st, {file :: binary(),
             ext  :: #{char() => block | inline}}).

%% An open block: the marker that opened it, its keys, and where it started.
-type open() :: toplevel | {char(), ai_mustache_keys(), ai_mustache_loc()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(unicode:chardata() | [ai_mustache_token()]) ->
          {ok, [ai_mustache_node()]} | ai_mustache_error().
parse(Input) -> parse(Input, #{}).

-spec parse(unicode:chardata() | [ai_mustache_token()], map()) ->
          {ok, [ai_mustache_node()]} | ai_mustache_error().
%% A list of tuples is a token list, already past the scanner. Anything else
%% is a template body: a binary, or the chardata an entry point may still be
%% holding. Chardata is never a list of tuples, so the two cannot be confused.
parse([T | _] = Tokens, Opts) when is_tuple(T) ->
    do_parse(Tokens, Opts);
parse([], Opts) ->
    do_parse([], Opts);
parse(Data, Opts) ->
    case ai_mustache_scanner:scan(Data, Opts) of
        {error, _} = E -> E;
        {ok, Tokens}   -> do_parse(Tokens, Opts)
    end.

-spec do_parse([ai_mustache_token()], map()) ->
          {ok, [ai_mustache_node()]} | ai_mustache_error().
do_parse(Tokens, Opts0) ->
    Opts = ai_html_text:opts(Opts0),
    St = #st{file = ai_html_text:source(Opts), ext = ext_table(Opts)},
    case nodes(Tokens, toplevel, [], St) of
        {error, _} = E   -> E;
        {ok, Nodes, []}  -> {ok, Nodes};
        {ok, _, [T | _]} -> {error, {St#st.file, line_of(T),
                                     {mismatched_close, [], []}}}
    end.

%%%===================================================================
%%% Recursive descent
%%%===================================================================

-spec nodes([ai_mustache_token()], open(), [ai_mustache_node()], #st{}) ->
          {ok, [ai_mustache_node()], [ai_mustache_token()]} | ai_mustache_error().
nodes([], toplevel, Acc, _St) ->
    {ok, lists:reverse(Acc), []};
nodes([], {_M, Keys, Loc}, _Acc, St) ->
    %% Report where the block was opened, not where the input ran out.
    %%
    %% The marker is deliberately not carried in the reason. Widening the
    %% tuple to name it would ripple through the plugin's formatter and every
    %% caller that matches on the shape, and buys only a more precise glyph in
    %% a message whose file, line and key are already correct. The formatter
    %% therefore names the key without claiming which of #/^/+/- opened it.
    {error, {St#st.file, line(Loc), {unclosed_tag, Keys}}};

nodes([{text, Loc, Bin} | Rest], Open, Acc, St) ->
    nodes(Rest, Open, [{text, Loc, Bin} | Acc], St);

nodes([{tag, Loc, $/, Content, _} | Rest], Open, Acc, St) ->
    Got = keys(Content),
    case Open of
        {_M, Got, _OpenLoc} -> {ok, lists:reverse(Acc), Rest};
        {_M, Want, _OpenLoc} ->
            {error, {St#st.file, line(Loc), {mismatched_close, Want, Got}}};
        toplevel ->
            {error, {St#st.file, line(Loc), {mismatched_close, [], Got}}}
    end;

nodes([{tag, Loc, M, Content, _} | Rest], Open, Acc, St)
  when M =:= $#; M =:= $^; M =:= $+; M =:= $- ->
    block(M, Loc, keys(Content), Rest, Open, Acc, St);

nodes([{tag, Loc, none, Content, _} | Rest], Open, Acc, St) ->
    nodes(Rest, Open, [{var, Loc, keys(Content), escape} | Acc], St);
nodes([{tag, Loc, ${, Content, _} | Rest], Open, Acc, St) ->
    nodes(Rest, Open, [{var, Loc, keys(Content), raw} | Acc], St);
nodes([{tag, Loc, $*, Content, _} | Rest], Open, Acc, St) ->
    nodes(Rest, Open, [{lambda, Loc, keys(Content)} | Acc], St);

nodes([{tag, Loc, $>, Content, Indent} | Rest], Open, Acc, St) ->
    case partial_name(Content) of
        {error, Reason} -> {error, {St#st.file, line(Loc), Reason}};
        {ok, Name}      -> nodes(Rest, Open, [{partial, Loc, Name, Indent} | Acc], St)
    end;

nodes([{tag, Loc, M, Content, _} = T | Rest], Open, Acc, St) when is_integer(M) ->
    case maps:get(M, St#st.ext, undefined) of
        block  -> ext_block(M, Loc, keys(Content), Rest, Open, Acc, St);
        inline -> nodes(Rest, Open, [{ext, Loc, M, keys(Content), []} | Acc], St);
        undefined ->
            %% The scanner rejects unregistered punctuation markers, so this
            %% only fires if the two disagree about the extension table.
            _ = T,
            {error, {St#st.file, line(Loc), {unknown_marker, M}}}
    end.

-spec block(char(), ai_mustache_loc(), ai_mustache_keys(), [ai_mustache_token()],
            open(), [ai_mustache_node()], #st{}) ->
          {ok, [ai_mustache_node()], [ai_mustache_token()]} | ai_mustache_error().
block(M, Loc, Keys, Rest, Open, Acc, St) ->
    case nodes(Rest, {M, Keys, Loc}, [], St) of
        {error, _} = E -> E;
        {ok, Body, Rest1} ->
            nodes(Rest1, Open, [node_of(M, Loc, Keys, Body) | Acc], St)
    end.

-spec ext_block(char(), ai_mustache_loc(), ai_mustache_keys(), [ai_mustache_token()],
                open(), [ai_mustache_node()], #st{}) ->
          {ok, [ai_mustache_node()], [ai_mustache_token()]} | ai_mustache_error().
ext_block(M, Loc, Keys, Rest, Open, Acc, St) ->
    case nodes(Rest, {M, Keys, Loc}, [], St) of
        {error, _} = E -> E;
        {ok, Body, Rest1} ->
            nodes(Rest1, Open, [{ext, Loc, M, Keys, Body} | Acc], St)
    end.

-spec node_of(char(), ai_mustache_loc(), ai_mustache_keys(), [ai_mustache_node()]) ->
          ai_mustache_node().
node_of($#, Loc, Keys, Body) -> {section,  Loc, Keys, Body};
node_of($^, Loc, Keys, Body) -> {inverted, Loc, Keys, Body};
node_of($+, Loc, Keys, Body) -> {has,      Loc, Keys, Body, true};
node_of($-, Loc, Keys, Body) -> {has,      Loc, Keys, Body, false}.

%%%===================================================================
%%% Keys
%%%===================================================================

%% @doc Split a tag's content into a dotted key path.
%%
%% A lone `.' (or empty content) yields the sentinel ['.'], which
%% ai_mustache_rt:lookup/2 recognises as the implicit iterator and answers
%% with the top of the context stack.
%%
%% NOTE: this creates atoms. That is safe here because template text comes
%% from the build-time file system, never from runtime input. Do not move
%% keys/1 onto a runtime path.
-spec keys(binary()) -> ai_mustache_keys().
keys(Bin0) ->
    Bin = << <<X:8>> || <<X:8>> <= Bin0,
                        X =/= $\s, X =/= $\t, X =/= $\r, X =/= $\n >>,
    case Bin =:= <<>> orelse Bin =:= <<".">> of
        true  -> [?AI_MUSTACHE_DOT];
        false -> [binary_to_atom(P, utf8)
                  || P <- binary:split(Bin, <<".">>, [global]), P =/= <<>>]
    end.

%% @doc A partial name is a file path, not a key path.
%%
%% Running keys/1 over it is exactly bug B4: `{{> layout.default}}' would
%% split into [layout, default] and the old parser's single-element clause
%% raised function_clause. The `/' and `.' are preserved verbatim; turning the
%% path into a module name is resolve_partials/2's job (T09).
-spec partial_name(binary()) -> {ok, binary()} | {error, ai_mustache_reason()}.
partial_name(Content) ->
    Name = trim(Content),
    Segments = binary:split(Name, <<"/">>, [global]),
    case Name =:= <<>> orelse lists:member(<<"..">>, Segments) of
        true  -> {error, {partial_not_found, Name}};
        false -> {ok, Name}
    end.

%%%===================================================================
%%% Extensions
%%%===================================================================

%% Char => block | inline, built from the extension modules' markers/0 and
%% optional block_markers/0. Modules that cannot be loaded contribute nothing;
%% reporting misconfiguration is the plugin's and the transform's job.
-spec ext_table(map()) -> #{char() => block | inline}.
ext_table(Opts) ->
    lists:foldl(
      fun(Mod, Acc) ->
              _ = code:ensure_loaded(Mod),
              Markers = exported(Mod, markers),
              Blocks  = exported(Mod, block_markers),
              lists:foldl(
                fun(C, A) ->
                        Kind = case lists:member(C, Blocks) of
                                   true  -> block;
                                   false -> inline
                               end,
                        A#{C => Kind}
                end, Acc, Markers)
      end, #{}, maps:get(extensions, Opts, [])).

-spec exported(module(), atom()) -> [char()].
exported(Mod, Fun) ->
    case erlang:function_exported(Mod, Fun, 0) of
        true  -> Mod:Fun();
        false -> []
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec line(ai_mustache_loc()) -> pos_integer().
line({L, _C}) -> L.

-spec line_of(ai_mustache_token()) -> pos_integer().
line_of({text, Loc, _})      -> line(Loc);
line_of({tag, Loc, _, _, _}) -> line(Loc).

-spec trim(binary()) -> binary().
trim(Bin) -> trim_right(trim_left(Bin)).

-spec trim_left(binary()) -> binary().
trim_left(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t -> trim_left(Rest);
trim_left(Bin) -> Bin.

-spec trim_right(binary()) -> binary().
trim_right(Bin) ->
    Sz = byte_size(Bin) - 1,
    case Sz >= 0 andalso Bin of
        <<Head:Sz/binary, C>> when C =:= $\s; C =:= $\t -> trim_right(Head);
        _ -> Bin
    end.
