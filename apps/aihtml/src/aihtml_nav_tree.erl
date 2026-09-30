%%%-------------------------------------------------------------------
%%% @doc The nav tree, ported from sigil (layout/nav_tree). DOM and class
%%% names are sigil's, so the styles in priv/css/sigil apply unchanged.
%%%
%%%   ah_nav_tree(Items, Value, Css, Attrs)   grouped side navigation of links
%%%
%%% The nav tree is value-bearing: the root carries `data-ah-value' (the
%%% active route) and fires `change'.
%%%
%%% ah_nav_tree/4 builds an element record (#ah_nav_tree{}, defined in
%%% include/aihtml_nav_tree.hrl) and render/1 turns it into HTML, so pages
%%% may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_nav_tree).
-behaviour(aihtml_element).

-include("aihtml_nav_tree.hrl").

-export([ah_nav_tree/4, render/1, fields/1, catalog/0]).

-export_type([element/0, item/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().
%% A nav tree entry: a link (`route' or `href'), a collapsible node
%% (`items'), `{Label, Route}' for short, or a group of entries under a
%% small caps heading (`group').
-type item() :: {aihtml_html:html(), iodata()}
              | #{label := aihtml_html:html(), icon => aihtml_html:html(),
                  route => iodata(), href => iodata(),
                  items => [item()]}
              | #{group := aihtml_html:html() | undefined,
                  items := [item()]}.
-type element() :: #ah_nav_tree{}.

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc A grouped side navigation. `Items' are links (`#{label, route}' or
%% `#{label, href}', `{Label, Route}' for short), collapsible nodes
%% (`#{label, items}') and groups (`#{group => Heading, items => [...]}').
%% `Value' is the active route: that link is highlighted and the nodes
%% around it are open. Options: `route_prefix' (prepended to a route to
%% make the href, default "#/").
-spec ah_nav_tree([item()], iodata() | atom() | undefined, css(), attrs()) -> #ah_nav_tree{}.
ah_nav_tree(Items, Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_nav_tree{items = Items, value = Value}, Css, Attrs).

%% @doc The field names of this component's record.
-spec fields(atom()) -> [atom()].
fields(ah_nav_tree) -> record_info(fields, ah_nav_tree).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(element()) -> html().
render(#ah_nav_tree{} = R) -> render_nav_tree(R).

-define(CARET, {safe, <<"<svg class=\"ah-nav-tree__caret-svg\" width=\"16\" height=\"16\" "
                        "viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
                        "stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" "
                        "aria-hidden=\"true\"><path d=\"m6 9 6 6 6-6\"/></svg>">>}).

render_nav_tree(#ah_nav_tree{items = Items, value = Value, route_prefix = Prefix} = R) ->
    Active = case Value of undefined -> undefined; _ -> text(Value) end,
    Groups = nav_groups(Items),
    Cfg = #{active => Active, prefix => text(Prefix)},
    ?H:el(nav,
          [?H:el('div',
                 [case Label of
                      undefined -> [];
                      _ -> ?H:el('div', Label, [<<"ah-nav-tree__group-label">>], [])
                  end,
                  [nav_item(I, Cfg) || I <- GItems]],
                 [<<"ah-nav-tree__group">>], [])
           || {Label, GItems} <- Groups],
          ?E:classes(?MODULE, R),
          [[{data_ah, <<"nav-tree">>}, {data_ah_value, case Active of
                                                        undefined -> <<>>;
                                                        _ -> Active
                                                    end}],
           ?E:root_attrs(R, change)]).

%% Groups as given; loose items between them form groups without a heading.
nav_groups(Items) ->
    Rev = lists:foldl(fun(#{group := L, items := Is}, Acc) -> [{{group, L}, Is} | Acc];
                         (I, [{loose, Is} | Acc]) -> [{loose, Is ++ [I]} | Acc];
                         (I, Acc) -> [{loose, [I]} | Acc]
                      end, [], Items),
    [{case K of loose -> undefined; {group, L} -> L end, Is} || {K, Is} <- lists:reverse(Rev)].

nav_item({Label, Route}, Cfg) -> nav_item(#{label => Label, route => Route}, Cfg);
nav_item(#{label := Label, items := [_ | _] = Kids} = I, Cfg) ->
    Open = nav_contains(Kids, maps:get(active, Cfg)),
    ?H:el(details,
          [?H:el(summary,
                 [nav_icon(I), ?H:el(span, Label, [<<"ah-nav-tree__label">>], []),
                  ?H:el(span, ?CARET, [<<"ah-nav-tree__caret">>], [])],
                 [<<"ah-nav-tree__item">>, <<"ah-nav-tree__item--parent">>,
                  [<<"ah-is-open">> || Open]], []),
           ?H:el('div',
                 ?H:el('div', [nav_item(K, Cfg) || K <- Kids],
                       [<<"ah-nav-tree__children-inner">>], []),
                 [<<"ah-nav-tree__children">>], [])],
          [<<"ah-nav-tree__node">>], [{open, Open}]);
nav_item(#{label := Label} = I, #{active := Active, prefix := Prefix}) ->
    Route = case I of #{route := R0} -> text(R0); _ -> undefined end,
    Href = case I of
               #{href := H} -> H;
               _ when Route =/= undefined -> <<Prefix/binary, Route/binary>>;
               _ -> <<"#">>
           end,
    IsActive = Route =/= undefined andalso Route =:= Active,
    ?H:el(a, [nav_icon(I), ?H:el(span, Label, [<<"ah-nav-tree__label">>], [])],
          [<<"ah-nav-tree__item">>, [<<"ah-is-active">> || IsActive]],
          [{href, Href}, {data_route, Route},
           {aria_current, IsActive andalso <<"page">>}]);
nav_item(Other, _) -> error({aihtml, {bad_nav_tree_item, Other}}).

nav_icon(#{icon := Icon}) when Icon =/= undefined ->
    ?H:el(span, Icon, [<<"ah-nav-tree__icon">>], [{aria_hidden, <<"true">>}]);
nav_icon(_) -> [].

nav_contains(_, undefined) -> false;
nav_contains(Items, Active) ->
    lists:any(fun({_, R}) -> text(R) =:= Active;
                 (#{items := [_ | _] = Kids}) -> nav_contains(Kids, Active);
                 (#{route := R}) -> text(R) =:= Active;
                 (_) -> false
              end, Items).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => nav_tree, category => layout,
       signature => <<"ah_nav_tree(Items, Value, Css, Attrs)">>,
       root => <<"ah-nav-tree">>, options => [route_prefix],
       behavior => <<"nav-tree">>, events => [<<"change">>],
       doc => <<"A grouped side navigation: links, collapsible nodes with connector lines, "
                "the active route highlighted and its nodes open.">>,
       option_docs => #{route_prefix => <<"Prepended to a route to make its href (default \"#/\").">>},
       methods =>
           [#{name => setValue, args => <<"(Route)">>,
              doc => <<"Mark the link of this route active and open its nodes, without firing change.">>},
            #{name => getValue, args => <<"()">>, doc => <<"Return the active route.">>}]}].

%%%===================================================================
%%% Internal
%%%===================================================================

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_text, L}})
    end;
text(X) -> beamai_html_escape:to_binary(X, aihtml).
