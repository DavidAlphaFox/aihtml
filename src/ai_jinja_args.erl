%%%-------------------------------------------------------------------
%%% @doc Argument binding for filters, tests and macros.
%%%
%%% A call site has positional and keyword arguments; the callee has named
%%% parameters. This is the single place the two are matched up, so that
%%% `x|replace("a","b")' and `x|replace(new="b", old="a")' cannot end up
%%% meaning different things.
%%%
%%% Leftover positionals are kept under `$positional' rather than dropped:
%%% `|format' takes an arbitrary number of them, and `|map("upper", 1, 2)'
%%% forwards them to the named filter.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_jinja_args).

-export([build/3, params_of/2]).

%% @doc Bind positional and keyword arguments to a parameter list.
-spec build([atom()], [term()], [{atom(), term()}]) -> map().
build(Params, Positional, Keyword) ->
    {Bound, Extra} = bind(Params, Positional, #{}),
    Args = maps:merge(Bound, maps:from_list(Keyword)),
    case Extra of
        [] -> Args;
        _  -> Args#{'$positional' => Extra}
    end.

bind([], Rest, Acc)            -> {Acc, Rest};
bind(_Params, [], Acc)         -> {Acc, []};
bind([P | Ps], [V | Vs], Acc)  -> bind(Ps, Vs, Acc#{P => V}).

%% @doc The positional parameter names a filter or test declares.
-spec params_of(map(), atom()) -> [atom()].
params_of(Table, Name) -> maps:get(Name, Table, []).
