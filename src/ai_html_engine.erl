%%%-------------------------------------------------------------------
%%% @doc The template-engine behaviour.
%%%
%%% The rebar3 plugin, the staleness check and the dev reloader are written
%%% against this and never name a concrete engine. Everything that differs
%%% between mustache and jinja -- the syntax, the semantics, the shape of the
%%% generated module -- is on the far side of these nine callbacks.
%%%
%%% == The AST is opaque ==
%%%
%%% parse/2 hands back a term and forms/2 takes it. That term is the engine's
%%% private business: this behaviour promises only that the output of one can
%%% be fed to the other. Do not be tempted to give it a shared type. A mustache
%%% node list and a jinja node list have almost nothing in common
%%% (designs/08-jinja-architecture.md section 1.2), and an abstraction over
%%% both would be an empty interface that makes every caller cast.
%%%
%%% == Why attribute/0 is a callback ==
%%%
%%% Renaming -mustache_source to a shared -template_source would invalidate
%%% every artefact ever built and put ai_mustache_dev out of step with every
%%% .erl on disk, in exchange for one fewer callback. The two engines' modules
%%% are separate families anyway -- different prefix, different render
%%% contract -- so their self-description is separate too.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_html_engine).

%% @doc Template body -> engine-private AST.
-callback parse(Body :: binary(), Opts :: map()) ->
    {ok, term()} | {error, term()}.

%% @doc Engine-private AST -> a complete module, plus the generated modules it
%% calls (include/extends/import targets, or mustache partials).
-callback forms(Ast :: term(), Opts :: map()) ->
    {ok, [erl_parse:abstract_form()], Deps :: [module()]} | {error, term()}.

%% @doc The build stamp: template content, normalised options and the
%% generated-code version, combined. The single source of staleness.
-callback source_hash(Body :: binary(), Opts :: map()) -> binary().

%% @doc Views-relative template name -> generated module name.
-callback module_name(Name :: binary(), Opts :: map()) -> module().

%% @doc The attribute a generated module describes itself with.
-callback attribute() -> atom().

%% @doc Defaults, used when the options block does not say.
-callback default_suffix() -> string().
-callback default_prefix() -> binary().

%% @doc The rebar.config key holding this engine's options.
-callback config_key() -> atom().

%% @doc Every key that options block understands.
-callback known_keys() -> [atom()].

%% @doc How this engine marks its output, in the banner of a generated .erl.
%%
%% Two engines may share an out_dir, and the orphan collector deletes files it
%% recognises as its own. Without a per-engine mark each would delete the
%% other's output on every build.
-callback banner_tag() -> string().
