%% @doc GET /fetch: the fetch-mode demo page (HTML fragments from plain URLs).
-module(aihtml_example_page).

-export([init/2]).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, State) ->
    {ok, aihtml_example_site:reply(Req, <<"aihtml example">>, fun aihtml_example_views:index/0,
                                   #{theme => #{appearance => light}}),
     State}.
