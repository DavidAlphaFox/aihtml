%%%-------------------------------------------------------------------
%%% @doc Worked example for aihtml.
%%%
%%% There is no application to start, no bootstrap call, no ets table and no
%%% process: aihtml is a library application and the templates in this
%%% directory are turned into modules (view_complex, view_shared_item, ...)
%%% by rebar3_aihtml during `rebar3 compile'.
%%%
%%% Run it with:  rebar3 shell   then  complex:start().
%%% @end
%%%-------------------------------------------------------------------
-module(complex).

-export([context/0, start/0, start_via_facade/0, body_for_cowboy/0, bench/1]).

-define(COUNT, 500).

%%%===================================================================
%%% Context
%%%===================================================================

%% NOTE on lambdas: since the move to standard context-stack semantics a
%% lambda receives the TOP OF THE STACK, not a flat global context. This works
%% here only because {{*yield}} sits at the top level of complex.mustache, so
%% hd(Stack) happens to be the root context. Move the tag inside a section and
%% `header' would no longer resolve -- pass what you need via the fun/2 form
%% instead of relying on the position.
-spec yield(binary(), map()) -> binary().
yield(Name, Frame) ->
    Header = maps:get(header, Frame),
    <<Name/binary, " is ", Header/binary>>.

-spec context() -> map().
context() ->
    A = #{name => <<"red">>,   current => true,  url => <<"#Red">>,   link => true},
    B = #{name => <<"green">>, current => true,  url => <<"#Green">>, link => true},
    C = #{name => <<"blue">>,  current => false, url => <<"#Blue">>,  link => true},
    #{items  => [A, B, C],
      header => <<"Colors">>,
      %% list is `true': the section runs once and does NOT push a scope,
      %% which is why {{header}} is still reachable inside it.
      list   => true,
      %% empty is `false', so only the inverted section runs.
      empty  => false,
      %% 0 is TRUTHY. Only undefined, false, [], <<>> and null are falsy.
      count  => 0,
      user   => #{name => <<"David Gao">>},
      level  => #{name => <<"VIP User">>},
      yield  => [fun yield/2, <<"Test">>]}.

%%%===================================================================
%%% Rendering
%%%===================================================================

%% The template compiled into a module; this is the normal path.
-spec start() -> ok.
start() ->
    io:format("~ts~n", [view_complex:render(context())]),
    bench(?COUNT).

%% The facade, for when the template is only known at run time -- picking a
%% layout by route, say. It is a single call into the generated module, with
%% no lookup table behind it.
-spec start_via_facade() -> ok.
start_via_facade() ->
    Mod = view_complex,
    io:format("~ts~n", [ai_mustache:render(Mod, context())]).

%% Hand this straight to cowboy as a response body: no iolist_to_binary/1 and
%% therefore no copy of the whole page.
-spec body_for_cowboy() -> iolist().
body_for_cowboy() ->
    view_complex:render_iolist(context()).

%%%===================================================================
%%% Smoke timing
%%%===================================================================

%% A rough timing print, not a benchmark. The real measurement, including the
%% comparison against v0.3.7, lives in bench/ (see tasks/T30.md); do not quote
%% these numbers as a performance result.
-spec bench(pos_integer()) -> ok.
bench(N) ->
    Ctx = context(),
    T0 = erlang:monotonic_time(microsecond),
    loop(Ctx, N),
    T1 = erlang:monotonic_time(microsecond),
    Diff = T1 - T0,
    io:format("~nTotal time: ~.2fs~nMean render time: ~.4fms~n",
              [Diff / 1000000, Diff / N / 1000]).

-spec loop(map(), non_neg_integer()) -> ok.
loop(_Ctx, 0) -> ok;
loop(Ctx, N)  -> _ = view_complex:render(Ctx), loop(Ctx, N - 1).
