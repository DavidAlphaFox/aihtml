%% @doc GET /examples and the dashboard-shell applications. Calendar, mail
%% and file manager are sidebar pages of the Sigil dashboard port, not
%% separate cards on the Examples index.
-module(aihtml_example_examples).

-include_lib("aihtml/include/aihtml.hrl").

-export([init/2, render/1]).

-define(SITE, aihtml_example_site).
-define(SUBPAGES, [<<"analytics">>, <<"banking">>, <<"booking">>, <<"file">>,
                   <<"course">>, <<"ecommerce">>, <<"kanban">>, <<"chat">>,
                   <<"user">>, <<"user-profile">>, <<"user-cards">>,
                   <<"user-create">>, <<"user-edit">>, <<"user-account">>,
                   <<"product">>, <<"product-detail">>, <<"product-create">>,
                   <<"product-edit">>, <<"order">>, <<"order-detail">>,
                   <<"invoice">>, <<"invoice-detail">>, <<"invoice-create">>,
                   <<"invoice-edit">>, <<"blog">>, <<"blog-detail">>,
                   <<"blog-create">>, <<"blog-edit">>, <<"job">>,
                   <<"job-detail">>, <<"job-create">>, <<"job-edit">>,
                   <<"tour">>, <<"tour-detail">>, <<"tour-create">>,
                   <<"tour-edit">>]).
-define(is_subpage(Page), lists:member((Page), ?SUBPAGES)).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req0, State) ->
    Page = case cowboy_req:binding(page, Req0) of
               undefined -> <<"index">>;
               Name -> Name
           end,
    case valid(Page) of
        true ->
            Title = case Page of
                        <<"dashboard">> -> <<"Dashboard — aihtml examples"/utf8>>;
                        <<"calendar">> -> <<"Calendar — aihtml examples"/utf8>>;
                        <<"mail">> -> <<"Mail — aihtml examples"/utf8>>;
                        <<"file-manager">> -> <<"File manager — aihtml examples"/utf8>>;
                        _ -> <<"aihtml examples"/utf8>>
                    end,
            {ok, ?SITE:reply(Req0, Title, fun() -> render(Page) end, page_opts(Page)), State};
        false ->
            {ok, cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>},
                                  <<"no such example">>, Req0), State}
    end.

%% @doc Per-page reply options: dashboard needs its board script; the other
%% Sigil shell pages only need the shared drawer and dashboard typography.
page_opts(<<"dashboard">>) ->
    #{head => aihtml_example_dashboard:head()};
page_opts(Page) ->
    ShellPages = [<<"calendar">>, <<"mail">>, <<"file-manager">>],
    case is_subpage(Page) orelse lists:member(Page, ShellPages) of
        true -> #{head => aihtml_example_dashboard:shell_head()};
        false -> #{}
    end.

-spec render(binary()) -> aihtml:html().
render(<<"dashboard">>) -> aihtml_example_dashboard:page();
render(<<"calendar">>) -> aihtml_example_calendar_app:page();
render(<<"mail">>) -> aihtml_example_dashboard:page(mail);
render(<<"file-manager">>) -> aihtml_example_dashboard:page(file_manager);
render(<<"index">>) -> index();

render(Page) ->
    case subpage(Page) of
        undefined -> index();
        Subpage -> aihtml_example_dashboard:page(Subpage)
    end.

valid(<<"dashboard">>) -> true;
valid(<<"calendar">>) -> true;
valid(<<"mail">>) -> true;
valid(<<"file-manager">>) -> true;
valid(<<"index">>) -> true;
valid(Page) -> is_subpage(Page).

%% @doc Sigil template pages are one URL segment under /examples. Keep this
%% explicit so an arbitrary priv file cannot become addressable.
subpage(<<"analytics">>) -> analytics;
subpage(<<"banking">>) -> banking;
subpage(<<"booking">>) -> booking;
subpage(<<"file">>) -> file;
subpage(<<"course">>) -> course;
subpage(<<"ecommerce">>) -> ecommerce;
subpage(<<"kanban">>) -> kanban;
subpage(<<"chat">>) -> chat;
subpage(<<"user">>) -> user_list;
subpage(<<"user-profile">>) -> user_profile;
subpage(<<"user-cards">>) -> user_cards;
subpage(<<"user-create">>) -> user_create;
subpage(<<"user-edit">>) -> user_edit;
subpage(<<"user-account">>) -> user_account;
subpage(<<"product">>) -> product_list;
subpage(<<"product-detail">>) -> product_details;
subpage(<<"product-create">>) -> product_create;
subpage(<<"product-edit">>) -> product_edit;
subpage(<<"order">>) -> order_list;
subpage(<<"order-detail">>) -> order_details;
subpage(<<"invoice">>) -> invoice_list;
subpage(<<"invoice-detail">>) -> invoice_details;
subpage(<<"invoice-create">>) -> invoice_create;
subpage(<<"invoice-edit">>) -> invoice_edit;
subpage(<<"blog">>) -> blog_list;
subpage(<<"blog-detail">>) -> blog_details;
subpage(<<"blog-create">>) -> blog_create;
subpage(<<"blog-edit">>) -> blog_edit;
subpage(<<"job">>) -> job_list;
subpage(<<"job-detail">>) -> job_details;
subpage(<<"job-create">>) -> job_create;
subpage(<<"job-edit">>) -> job_edit;
subpage(<<"tour">>) -> tour_list;
subpage(<<"tour-detail">>) -> tour_details;
subpage(<<"tour-create">>) -> tour_create;
subpage(<<"tour-edit">>) -> tour_edit;

subpage(_) -> undefined.

is_subpage(Page) ->
    lists:member(Page, ?SUBPAGES).

%%%===================================================================
%%% Index
%%%===================================================================

index() ->
    Cards = [example(<<"Dashboard"/utf8>>, <<"/examples/dashboard">>,
                     <<"仪表盘"/utf8>>,
                     <<"侧边导航、应用页面、指标拖拽、图表、消息、任务、订单、转化与区域汇总。"/utf8>>)],
    ah_main([aihtml_example_site:topbar(examples),
             ah_div([ah_h1(<<"Examples"/utf8>>, [<<"text-4xl font-bold">>], []),
                     ah_p(<<"不止是组件沙盒：这些页面按真实业务布局组合组件。"/utf8>>,
                          [<<"mt-3 text-lg text-muted">>], []),
                     ah_div(Cards,
                            [<<"mt-8 grid gap-6 md:grid-cols-2 xl:grid-cols-3">>], [])],
                    [<<"max-w-6xl mx-auto px-6 py-16">>], [])],
            [<<"min-h-screen">>], []).

example(Title, Href, Chip, Desc) ->
    ah_a([ah_div([ah_chip(Chip, [primary, soft, small], [])],
                 [<<"flex items-center justify-between">>], []),
          ah_h2(Title, [<<"mt-4 text-xl font-semibold">>], []),
          ah_p(Desc, [<<"mt-2 text-sm text-muted leading-relaxed">>], []),
          ah_span(<<"Open →"/utf8>>, [<<"mt-4 inline-block text-sm font-semibold text-primary">>], [])],
           [<<"example-card block p-6 rounded-surface border border-line bg-surface hover">>],
           [{href, Href}]).
