%%%-------------------------------------------------------------------
%%% @doc The `rebar3 jinja' provider.
%%%
%%% Nothing but registration: the pipeline is rebar3_aihtml_prv:run/3, which
%%% both engines share. Anything that looked engine-specific enough to belong
%%% here would be a sign that the parameterisation is incomplete.
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_aihtml_jinja).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, jinja).

init(State) ->
    P = providers:create(
          [{name,       ?PROVIDER},
           {module,     ?MODULE},
           {namespace,  default},
           {bare,       true},
           {deps,       [app_discovery, install_deps]},
           {example,    "rebar3 jinja"},
           {opts,       [{force, $f, "force", boolean,
                          "Ignore build stamps and recompile every template"}]},
           {short_desc, "Compile Jinja2 templates to Erlang modules"},
           {desc,       "Scan the views directory of every project app and "
                        "compile each .j2 template into an .erl module in "
                        "out_dir. Templates whose recorded build stamp still "
                        "matches are skipped."}]),
    {ok, rebar_state:add_provider(State, P)}.

do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Force = proplists:get_value(force, Args, false),
    rebar3_aihtml_prv:run(State, ai_jinja_engine, Force).

format_error(Reason) -> rebar3_aihtml_prv:format_error(Reason).
