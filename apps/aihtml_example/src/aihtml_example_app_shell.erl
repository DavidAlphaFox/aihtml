%% @doc The sidebar shell shared by the application examples. It keeps
%% the application pages next to the dashboard and gives each one the
%% same sticky top bar as sigil's application demos.
-module(aihtml_example_app_shell).

-include_lib("aihtml/include/aihtml.hrl").

-export([page/3, app_nav/1]).

-spec page(atom(), aihtml:html(), aihtml:html()) -> aihtml:html().
page(Active, Head, Body) ->
    ah_div([app_nav(Active),
            ah_main([top_bar(Active), Head,
                     ah_div(Body, [<<"px-4 pb-10 xl:px-8 flex flex-col gap-6">>], [])],
                    [<<"flex-1 min-w-0 min-h-screen overflow-x-hidden bg-surface-2">>], [])],
           [<<"flex min-h-screen bg-surface-2">>], []).

-spec app_nav(atom()) -> aihtml:html().
app_nav(Active) ->
    Groups = [#{label => <<"Workspace"/utf8>>,
                items => [#{key => dashboard, label => <<"Dashboard"/utf8>>,
                            href => <<"/examples/dashboard">>},
                          #{key => calendar, label => <<"Calendar"/utf8>>,
                            href => <<"/examples/calendar">>},
                          #{key => mail, label => <<"Mail"/utf8>>,
                            href => <<"/examples/mail">>},
                          #{key => files, label => <<"Files"/utf8>>,
                            href => <<"/examples/file-manager">>}]}],
    ah_sidenav(Groups, Active, [<<"app-nav shrink-0">>],
               [{brand, #{name => <<"aihtml"/utf8>>, logo => ah_strong(<<"A">>),
                          href => <<"/examples">>}},
                {footer, ah_div([ah_p(<<"Workspace"/utf8>>, [<<"text-sm font-semibold">>], []),
                                 ah_p(<<"Examples · v1.0"/utf8>>,
                                      [<<"mt-1 text-xs text-muted">>], [])],
                                [], [])},
                {collapsible, true},
                {style, <<"--ah-ssn-width:16rem; --ah-ssn-height:100vh">>}])
    .

top_bar(Active) ->
    ah_header(ah_div([ah_breadcrumbs([<<"Home"/utf8>>, <<"Examples"/utf8>>, label(Active)],
                                     [], []),
                      ah_div([ah_button(<<"Help"/utf8>>, undefined, [borderless, sm],
                                        [{type, button}]),
                              ah_theme_switcher([], []),
                              ah_avatar(<<"DA"/utf8>>, [sm], [{title, <<"David"/utf8>>}])],
                             [<<"flex items-center gap-3">>], [])],
                     [<<"flex min-h-20 flex-wrap items-center justify-between gap-4 px-4 py-4 xl:px-8">>],
                     []),
            [<<"sticky top-0 z-20 border-b border-line bg-surface/85 backdrop-blur">>], []).

label(calendar) -> <<"Calendar"/utf8>>;
label(mail) -> <<"Mail"/utf8>>;
label(files) -> <<"File manager"/utf8>>;
label(dashboard) -> <<"Dashboard"/utf8>>.
