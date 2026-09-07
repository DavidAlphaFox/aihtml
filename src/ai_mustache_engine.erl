%%%-------------------------------------------------------------------
%%% @doc ai_html_engine adapter for the mustache engine.
%%%
%%% Every function is a one-line forward. The adapter exists so that the
%%% plugin can be engine-parameterised without any of the mustache modules
%%% having to know that a behaviour is involved.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_engine).

-behaviour(ai_html_engine).

-include("ai_html.hrl").

-export([parse/2, forms/2, source_hash/2, module_name/2, attribute/0,
         default_suffix/0, default_prefix/0, config_key/0, known_keys/0,
         banner_tag/0]).

-spec parse(binary(), map()) -> {ok, term()} | {error, term()}.
parse(Body, Opts) -> ai_mustache_parser:parse(Body, Opts).

-spec forms(term(), map()) ->
          {ok, [erl_parse:abstract_form()], [module()]} | {error, term()}.
forms(Ast, Opts) -> ai_mustache_compiler:forms(Ast, Opts).

-spec source_hash(binary(), map() | [{atom(), term()}]) -> binary().
source_hash(Body, Opts) -> ai_mustache_compiler:source_hash(Body, Opts).

-spec module_name(binary(), map()) -> module().
module_name(Name, Opts) -> ai_mustache_ast:module_name(Name, Opts).

-spec attribute() -> atom().
attribute() -> mustache_source.

-spec default_suffix() -> string().
default_suffix() -> ".mustache".

-spec default_prefix() -> binary().
default_prefix() -> ai_mustache_ast:default_prefix().

-spec config_key() -> atom().
config_key() -> mustache_opts.

-spec known_keys() -> [atom()].
known_keys() -> ?AI_HTML_MUSTACHE_KEYS.

-spec banner_tag() -> string().
banner_tag() -> "mustache".
