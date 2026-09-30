%% @doc Demos of the drawer (aihtml_drawer), shown on
%% /components/drawer. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_drawer).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([drawer_bottom/0, drawer_sides/0, drawer_options/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => drawer, title => <<"Drawer">>,
       summary => <<"从边缘滑出的抽屉，可下滑关闭。"/utf8>>,
       demos => [{<<"底部抽屉"/utf8>>, drawer_bottom},
                 {<<"左右两侧"/utf8>>, drawer_sides},
                 {<<"不可滑动关闭、无把手"/utf8>>, drawer_options}]}].

%%%===================================================================
%%% Drawer
%%%===================================================================

-spec drawer_bottom() -> aihtml:html().
drawer_bottom() ->
    ah_div([ah_button(<<"Open drawer">>, undefined, [primary], opens({id, <<"drawer-bottom">>})),
            ah_drawer(ah_p(<<"Drag the handle down, press Escape or click the scrim to close.">>),
                      [], [{id, <<"drawer-bottom">>}, {title, <<"Filters">>},
                           {description, <<"Swipe down to close.">>},
                           {footer, [ah_button(<<"Reset">>, undefined, [default], closes()),
                                     ah_button(<<"Apply">>, undefined, [primary], closes(closest, apply))]}])]).

-spec drawer_sides() -> aihtml:html().
drawer_sides() ->
    row([ah_button(<<"Left">>, undefined, [outlined], opens({id, <<"drawer-left">>})),
         ah_button(<<"Right">>, undefined, [outlined], opens({id, <<"drawer-right">>})),
         ah_drawer(nav_links(), [left], [{id, <<"drawer-left">>}, {title, <<"Menu">>}, {size, 280}]),
         ah_drawer(nav_links(), [right], [{id, <<"drawer-right">>}, {title, <<"Menu">>}, {size, 280}])]).

-spec drawer_options() -> aihtml:html().
drawer_options() ->
    ah_div([ah_button(<<"Open">>, undefined, [secondary], opens({id, <<"drawer-plain">>})),
            ah_drawer(ah_p(<<"No handle and no swipe; only the close button closes it.">>),
                      [top], [{id, <<"drawer-plain">>}, {title, <<"Announcement">>},
                              {size, 200}, {handle, false}, {dismissible, false},
                              {close_on_overlay, false}, {close_on_esc, false}])]).

%%%===================================================================
%%% Helpers
%%%===================================================================

nav_links() ->
    ah_ul([ah_li(ah_a(<<"Dashboard">>, [], [{href, <<"#">>}])),
           ah_li(ah_a(<<"Projects">>, [], [{href, <<"#">>}])),
           ah_li(ah_a(<<"Settings">>, [], [{href, <<"#">>}]))],
          [<<"flex flex-col gap-2">>], []).

row(Children) ->
    ah_div(Children, [<<"flex flex-wrap items-center gap-3">>], []).
