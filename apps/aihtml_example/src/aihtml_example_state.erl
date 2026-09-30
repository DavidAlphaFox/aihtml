%% @doc GET /components/:name/state?...: the docs page of a component with
%% its linked demos in the state the query names (a page, a date, a view).
%%
%% Components whose navigation is a real link (pagination, datagrid,
%% datatable, scheduler and calendar with `href') point that link here, so
%% following it without JavaScript -- a crawler, a new tab, a bookmark, a
%% reload after the script pushed the URL -- renders the page for that
%% state on the server. The demo functions stay zero-arity: they read the
%% state with param/2 and int/2, which fall back to the defaults on the
%% plain docs page (/components/:name), where no query is set.
-module(aihtml_example_state).

-export([init/2, param/2, int/2]).

-define(KEY, {?MODULE, query}).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req0, State) ->
    Name = cowboy_req:binding(name, Req0),
    case [N || #{name := N} <- aihtml_example_site:components(), atom_to_binary(N) =:= Name] of
        [N] ->
            put(?KEY, maps:from_list([{K, V} || {K, V} <- cowboy_req:parse_qs(Req0),
                                                is_binary(V)])),
            Title = <<(aihtml_example_site:display_name(N))/binary, " — aihtml"/utf8>>,
            Body = fun() -> aihtml_example_docs:render(N) end,
            {ok, aihtml_example_site:reply(Req0, Title, Body, #{}), State};
        [] ->
            {ok, cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>},
                                  <<"no such component">>, Req0), State}
    end.

%% @doc A query parameter of the state page being rendered, else `Default'
%% (always on the plain docs page and in tests).
-spec param(binary(), binary()) -> binary().
param(Key, Default) ->
    case get(?KEY) of
        #{Key := V} -> V;
        _ -> Default
    end.

%% @doc An integer query parameter, else `Default' (also when it is not a
%% number).
-spec int(binary(), integer()) -> integer().
int(Key, Default) ->
    try binary_to_integer(param(Key, <<>>))
    catch error:badarg -> Default
    end.
