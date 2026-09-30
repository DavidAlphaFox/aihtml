%% @doc GET / : the demo page.
-module(aihtml_example_page).

-export([init/2]).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req0, State) ->
    Html = aihtml:page(aihtml_example_views:index(),
                       #{title => <<"aihtml example">>,
                         theme => #{appearance => light},
                         css => [aihtml_example_site:css()]}),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>},
                           Html, Req0),
    {ok, Req, State}.
