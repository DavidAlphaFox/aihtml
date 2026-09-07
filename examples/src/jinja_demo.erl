%%%-------------------------------------------------------------------
%%% @doc The jinja half of the example: inheritance, macros, filters.
%%%
%%% j2_index is generated from views/jinja/index.j2 by `rebar3 jinja', which
%%% runs as a pre-compile hook. Nothing needs starting first: aihtml is a
%%% library application and the generated code only calls into its runtime.
%%% @end
%%%-------------------------------------------------------------------
-module(jinja_demo).

-export([render/0, context/0]).

-spec render() -> binary().
render() -> j2_index:render(context()).

-spec context() -> map().
context() ->
    #{title  => <<"Users & guests">>,
      label  => <<"beta">>,
      users  => [#{name => <<"ada">>,  role => <<"admin">>, visible => true},
                 #{name => <<"bo">>,   visible => true},
                 #{name => <<"hidden">>, visible => false}]}.
