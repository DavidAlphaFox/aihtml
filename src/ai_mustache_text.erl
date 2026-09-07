%%%-------------------------------------------------------------------
%%% @doc The canonical text representation.
%%%
%%% Template bodies and every path aihtml handles are UTF-8 binaries. Nothing
%%% downstream of the entry points has to ask whether it is holding a binary,
%%% a string, or a deep iolist, and no module carries its own private
%%% conversion.
%%%
%%% Conversion happens exactly twice: inbound, where a caller may hand in any
%%% unicode:chardata() and it is normalised and validated here; and outbound,
%%% at the few OTP calls that insist on a string (code:load_binary/3 and
%%% friends), via to_list/1.
%%%
%%% Splitting a UTF-8 binary on ASCII bytes is safe -- UTF-8 is
%%% self-synchronising, so a byte below 128 never occurs inside a multi-byte
%%% sequence -- and the scanner only ever splits on `{{', `}}' and newlines.
%%% Validating once at the boundary therefore guarantees every text node in
%%% the AST is valid UTF-8 too.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_text).

-include("ai_mustache.hrl").

-export([template/1, path/1, source/1, opts/1, to_list/1]).

%% Option keys whose values name a file or a piece of a module name, and which
%% are therefore held as UTF-8 binaries.
-define(PATH_KEYS, [source, views, views_abs, prefix, suffix]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Normalise and validate a template body.
%%
%% A template that is not valid UTF-8 is rejected rather than quietly passed
%% through as bytes: the declared representation is UTF-8, and accepting
%% anything else would surface much later as mangled output from a generated
%% module.
-spec template(unicode:chardata()) ->
          {ok, binary()} | {error, {invalid_utf8, non_neg_integer()}}.
template(Data) ->
    case unicode:characters_to_binary(Data, utf8, utf8) of
        Bin when is_binary(Bin)   -> {ok, Bin};
        {error, Prefix, _Rest}    -> {error, {invalid_utf8, byte_size(Prefix)}};
        {incomplete, Prefix, _}   -> {error, {invalid_utf8, byte_size(Prefix)}}
    end.

%% @doc Normalise a path or other identifier to a UTF-8 binary.
%%
%% Unlike template/1 this does not reject: paths reach us from the file
%% system and from rebar3, which may hand back raw bytes on a system whose
%% filename encoding is not UTF-8. Such a path is kept verbatim so that it
%% still names the file it came from.
-spec path(unicode:chardata()) -> binary().
path(Bin) when is_binary(Bin) ->
    Bin;
path(Data) ->
    case unicode:characters_to_binary(Data, utf8, utf8) of
        Bin when is_binary(Bin) -> Bin;
        _                       -> iolist_to_binary(Data)
    end.

%% @doc The source path recorded in compile options, for diagnostics.
%%
%% This replaces the four private copies scanner, parser, ast and compiler
%% each used to carry.
-spec source(map()) -> binary().
source(#{source := S}) -> path(S);
source(_)              -> <<"nofile">>.

%% @doc Normalise the path-like values in a compile options map.
%%
%% Called once at each entry point, so every module downstream can rely on
%% these being binaries -- including ai_mustache_ast:module_name/2, which
%% builds a module name by binary concatenation and would fail outright on a
%% string prefix.
-spec opts(map()) -> map().
opts(Opts) ->
    lists:foldl(
      fun(K, Acc) ->
              case Acc of
                  #{K := V} -> Acc#{K => path(V)};
                  _         -> Acc
              end
      end, Opts, ?PATH_KEYS).

%% @doc Back to a string, for the OTP calls that require one.
-spec to_list(unicode:chardata()) -> string().
to_list(Data) ->
    case unicode:characters_to_list(Data, utf8) of
        L when is_list(L) -> L;
        _                 -> binary_to_list(iolist_to_binary(Data))
    end.
