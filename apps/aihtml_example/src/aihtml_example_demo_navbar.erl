%% @doc Demos of the NavBar component (aihtml_navbar), shown on
%% /components/navbar. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_navbar).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([navbar_basic/0, navbar_vertical/0, navbar_minimized/0, navbar_links/0, navbar_record/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => navbar, title => <<"NavBar">>,
       summary => <<"一排可选中的导航项，带品牌区和右侧内容。"/utf8>>,
       demos => [{<<"品牌、导航项、右侧按钮"/utf8>>, navbar_basic},
                 {<<"竖排"/utf8>>, navbar_vertical},
                 {<<"折叠：汉堡按钮加弹出列表"/utf8>>, navbar_minimized},
                 {<<"链接与自定义列宽"/utf8>>, navbar_links},
                 {<<"record 写法"/utf8>>, navbar_record}]}].

%%%-------------------------------------------------------------------
%%% navbar
%%%-------------------------------------------------------------------

-spec navbar_basic() -> aihtml:html().
navbar_basic() ->
    ah_navbar([{home, <<"Home">>}, {products, <<"Products">>}, {pricing, <<"Pricing">>},
               #{key => docs, label => <<"Docs">>, disabled => true}],
              products, [],
              [{brand, ah_strong(<<"Acme">>)},
               {extra, ah_button(<<"Sign in">>, undefined, [sm], [])},
               {name, section}]).

-spec navbar_vertical() -> aihtml:html().
navbar_vertical() ->
    ah_div(ah_navbar([{profile, <<"Profile">>}, {account, <<"Account">>},
                      {billing, <<"Billing">>}, {security, <<"Security">>}],
                     account, [vertical], []),
           [<<"w-56">>], []).

-spec navbar_minimized() -> aihtml:html().
navbar_minimized() ->
    ah_div(ah_navbar([{home, <<"Home">>}, {products, <<"Products">>}, {pricing, <<"Pricing">>}],
                     pricing, [minimized], [{title, <<"Pricing">>}]),
           [<<"w-72">>], []).

-spec navbar_links() -> aihtml:html().
navbar_links() ->
    ah_navbar([#{key => overview, label => <<"Overview">>, href => <<"#overview">>},
               #{key => api, label => <<"API">>, href => <<"#api">>},
               #{key => faq, label => <<"FAQ">>, href => <<"#faq">>}],
              overview, [], [{columns, [<<"50%">>, <<"30%">>, <<"20%">>]}]).

-spec navbar_record() -> aihtml:html().
navbar_record() ->
    #ah_navbar{items = [{home, <<"Home">>}, {products, <<"Products">>},
                        {pricing, <<"Pricing">>}],
               value = home,
               brand = ah_strong(<<"Acme">>),
               extra = ah_button(<<"Sign in">>, undefined, [sm], []),
               title = <<"Acme">>,
               minimize_width = 480,
               name = section}.
