%%%-------------------------------------------------------------------
%%% @doc An ancestor path (sigil's breadcrumbs); the last item is the current
%%% page.
%%%
%%% breadcrumbs/3 builds an element record (#ah_breadcrumbs{}, defined in
%%% include/aihtml_breadcrumbs.hrl) and render/1 turns it into HTML, so pages
%%% may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_breadcrumbs).
-behaviour(aihtml_element).

-include("aihtml_breadcrumbs.hrl").

-export([breadcrumbs/3, render/1, fields/1, catalog/0]).
-export_type([crumb/0]).

%% Label | {Label, Href} | #{label, href, icon, attrs}
-type crumb() :: aihtml_html:html() | {aihtml_html:html(), binary() | undefined} | map().

-import(aihtml_html, [el/4]).
-import(aihtml_lib_layout, [maybe_el/3, bool/2, tf/1]).

-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% @doc An ancestor path. `Items' are `Label', `{Label, Href}' or
%% `#{label, href, icon, attrs}'; the last item is the current page.
%% Options: separator (default "/"; none for dots), active_last, max_items.
-spec breadcrumbs([html() | {html(), binary() | undefined} | map()], css(), attrs()) ->
          #ah_breadcrumbs{}.
breadcrumbs(Items, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_breadcrumbs{items = Items}, Css, Attrs).

%% @doc The field names of #ah_breadcrumbs{}.
-spec fields(ah_breadcrumbs) -> [atom()].
fields(ah_breadcrumbs) -> record_info(fields, ah_breadcrumbs).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_breadcrumbs{}) -> html().
render(#ah_breadcrumbs{items = Items, separator = Sep} = R) ->
    HasSep = not lists:member(Sep, [none, undefined, <<>>, ""]),
    ActiveLast = bool(active_last, R#ah_breadcrumbs.active_last),
    Shown = collapse([norm_crumb(I) || I <- Items], R#ah_breadcrumbs.max_items),
    N = length(Shown),
    Lis = lists:append(
            [[crumb(It, I, I =:= N - 1, ActiveLast),
              [el(li, [Sep || HasSep], [<<"ah-breadcrumbs__separator">>],
                  [{role, presentation}, {aria_hidden, <<"true">>}]) || I < N - 1]]
             || {I, It} <- lists:zip(lists:seq(0, N - 1), Shown)]),
    el(nav, el(ol, Lis, [<<"ah-breadcrumbs__list">>], []), aihtml_element:classes(?MODULE, R),
       [[{aria_label, R#ah_breadcrumbs.label}, {data_has_separator, tf(HasSep)}],
        ?E:root_attrs(R, none)]).

norm_crumb(#{} = M) -> M;
norm_crumb({L, H}) -> #{label => L, href => H};
norm_crumb(L) -> #{label => L}.

collapse(Items, Max) when is_integer(Max), length(Items) > Max, length(Items) > 3 ->
    [hd(Items), ellipsis | lists:nthtail(length(Items) - 2, Items)];
collapse(Items, _) -> Items.

crumb(ellipsis, _, _, _) ->
    el(li, <<"\x{2026}"/utf8>>, [<<"ah-breadcrumbs__item ah-breadcrumbs__ellipsis">>],
       [{aria_hidden, <<"true">>}]);
crumb(M, I, Last, ActiveLast) ->
    Current = Last andalso not ActiveLast,
    Href = maps:get(href, M, undefined),
    Inner = [maybe_el(span, maps:get(icon, M, undefined), <<"ah-breadcrumbs__icon">>),
             maps:get(label, M, <<>>)],
    Body = case Href =/= undefined andalso not Current of
               true -> el(a, Inner, [<<"ah-breadcrumbs__link">>],
                          [[{href, Href}], maps:get(attrs, M, [])]);
               false -> el(span, Inner, [<<"ah-breadcrumbs__text">>], [])
           end,
    el(li, Body, [<<"ah-breadcrumbs__item">>],
       [{data_index, I}, {aria_current, Current andalso page}]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => breadcrumbs, category => layout,
       option_docs => #{separator => <<"Separator text (default /); none for dots.">>,
                        active_last => <<"Keep the last item a link.">>,
                        max_items => <<"Collapse the middle to an ellipsis beyond this many items.">>,
                        label => <<"aria-label of the nav (default breadcrumb).">>},
       methods => [],
       signature => <<"breadcrumbs(Items, Css, Attrs)">>, root => <<"ah-breadcrumbs">>,
       options => [separator, active_last, max_items, label],
       doc => <<"An ancestor path; the last item is the current page.">>}].
