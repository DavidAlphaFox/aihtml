%%%-------------------------------------------------------------------
%%% @doc Page navigation (sigil's pagination). The value, the current page, is
%%% in `data-ah-value' on the root and a user change fires `change' there.
%%% The page list is rendered with the shared template pagination_items,
%%% which the behaviour (pagination.ts) re-renders in the browser.
%%%
%%%
%%% == Links ==
%%%
%%% With `href' (a URL template with {page} and {size}) every page is a
%%% real `<a href>' link, so the page of each state has a URL: the server
%%% renders it from the query, crawlers follow it, it opens in a new tab
%%% and works without script. Without a server binding the link simply
%%% loads. With `on(change, Action)' a plain click is handled in place:
%%% the value changes, the action renders the new content (html/4 with
%%% morph) and the browser pushes the link's URL to the history (like
%%% aihtml_action:push_url/2, so the action need not), so back, forward
%%% and reload load that URL from the server. The page handler reads the
%%% page from the query and renders the same component with it.
%%%
%%% pagination/4 builds an element record (#ah_pagination{}, defined in
%%% include/aihtml_pagination.hrl) and render/1 turns it into HTML, so pages
%%% may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_pagination).
-behaviour(aihtml_element).

-include("aihtml_pagination.hrl").

-export([pagination/4, render/1, fields/1, catalog/0]).
-export([visible_pages/3, pagination_view/5]).

%% Shared with the browser (see aihtml_tpl), compiled to AH.tpl.pagination_items.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_pagination_items, "../templates/pagination_items.mustache"}).

-import(aihtml_html, [el/4]).
-import(aihtml_lib_layout, [bool/2, hidden/2, bin/1]).

-define(E, aihtml_element).

-type html() :: aihtml_html:html().

-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% @doc Page navigation for `Total' items, `Page' being current (from 1).
%% Options: page_size (10), page_sizes ([10,20,50,100]),
%% show_size_selector (true), show_jumper, show_first_last, show_total,
%% max_visible (7 slots) or siblings (pages on each side of the current
%% one), href (a template with {page} and {size}: pages become links the
%% server renders; with on(change, ...) a click updates in place and pushes
%% the URL, see the module doc), labels (#{prev, next, first, last, per_page,
%% total, goto, goto_suffix, goto_confirm, page_info, aria_label,
%% per_page_aria}), name. Value: the current page.
-spec pagination(non_neg_integer(), pos_integer(), css(), attrs()) -> #ah_pagination{}.
pagination(Total, Page, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_pagination{total = Total, value = Page}, Css, Attrs).

%% @doc The field names of #ah_pagination{}.
-spec fields(ah_pagination) -> [atom()].
fields(ah_pagination) -> record_info(fields, ah_pagination).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_pagination{}) -> html().
render(#ah_pagination{total = Total, value = Page, href = Href} = R) ->
    Classes = aihtml_element:classes(?MODULE, R),
    Size = max(1, R#ah_pagination.page_size),
    Pages = max(1, (Total + Size - 1) div Size),
    Cur = min(max(1, Page), Pages),
    Max = case R#ah_pagination.siblings of
              undefined -> R#ah_pagination.max_visible;
              S -> 2 * S + 5
          end,
    L = maps:merge(labels(), R#ah_pagination.labels),
    %% What the page list depends on besides the state; pagination.ts
    %% reads it to re-render the list with the same template.
    Cfg = #{labels => maps:map(fun(_, V) -> bin(V) end,
                              maps:with([prev, next, first, last, page_info], L)),
            first_last => bool(show_first_last, R#ah_pagination.show_first_last),
            simple => R#ah_pagination.simple,
            href => case Href of undefined -> null; _ -> bin(Href) end},
    List = el(ul, aihtml_tpl:safe(tpl_pagination_items(pagination_view(Cur, Pages, Max, Size, Cfg))),
              [<<"ah-pagination-pages">>],
              [{data_view, iolist_to_binary(aihtml_json:encode(Cfg))}]),
    Total_ = [el(span, fmt(maps:get(total, L), [Total]), [<<"ah-pagination-total">>], [])
              || bool(show_total, R#ah_pagination.show_total)],
    SizeSel = [el('div',
                  el(select, [el(option, fmt(maps:get(per_page, L), [Sz]), [],
                                 [{value, Sz}, {selected, Sz =:= Size}])
                              || Sz <- R#ah_pagination.page_sizes],
                     [<<"ah-pagination-size-select">>],
                     [{aria_label, maps:get(per_page_aria, L)}]),
                  [<<"ah-pagination-size-selector">>], [])
               || bool(show_size_selector, R#ah_pagination.show_size_selector)],
    Jumper = [el('div', [el(span, maps:get(goto, L), [], []),
                         aihtml_html:void(input, [<<"ah-pagination-jumper-input">>],
                                          [{type, text}, {inputmode, numeric},
                                           {aria_label, maps:get(goto, L)}]),
                         el(span, fmt(maps:get(goto_suffix, L), [Pages]), [],
                            [{data_template, maps:get(goto_suffix, L)}]),
                         el(button, maps:get(goto_confirm, L), [<<"ah-pagination-jumper-btn">>],
                            [{type, button}])],
                 [<<"ah-pagination-jumper">>], [])
              || bool(show_jumper, R#ah_pagination.show_jumper)],
    el('div', [Total_, List, SizeSel, Jumper, hidden(R#ah_pagination.name, Cur)],
       [Classes, [<<"ah-pagination-links">> || Href =/= undefined]],
       [[{role, navigation}, {aria_label, maps:get(aria_label, L)},
         {data_ah, <<"pagination">>}, {data_ah_value, Cur},
         {data_total, Total}, {data_page_size, Size}, {data_max_visible, Max},
         {data_href, Href}], ?E:root_attrs(R, change)]).

%% @doc The page numbers to show, `gap' standing for an ellipsis. At most
%% `Max' slots (at least 5): the first and last pages always, the current
%% page and its neighbours in the middle.
-spec visible_pages(pos_integer(), pos_integer(), pos_integer()) -> [pos_integer() | gap].
visible_pages(Cur, Total, Max0) ->
    Max = max(5, Max0),
    if
        Total =< Max -> lists:seq(1, Total);
        Cur =< Max - 3 -> lists:seq(1, Max - 2) ++ [gap, Total];
        Cur >= Total - (Max - 4) -> [1, gap | lists:seq(Total - (Max - 3), Total)];
        true ->
            H = (Max - 5) div 2,
            [1, gap | lists:seq(Cur - H, Cur + H)] ++ [gap, Total]
    end.

labels() ->
    #{prev => <<"Previous">>, next => <<"Next">>, first => <<"First">>, last => <<"Last">>,
      per_page => <<"{0} / page">>, total => <<"Total {0}">>, goto => <<"Go to">>,
      goto_suffix => <<" / {0} pages">>, goto_confirm => <<"Go">>,
      page_info => <<"Page {0} / {1}">>, aria_label => <<"Pagination">>,
      per_page_aria => <<"Items per page">>}.

%% @doc The view data of templates/pagination_items.mustache: the entries
%% of the page list (first/prev navs, pages and gaps or the simple-mode
%% info, next/last navs). pagination.ts has the same function (pgView).
-spec pagination_view(pos_integer(), pos_integer(), pos_integer(), pos_integer(), map()) ->
          #{entries := [map()]}.
pagination_view(Cur, Pages, Max, Size, #{labels := L, first_last := FL, simple := Simple,
                                         href := Href}) ->
    Link = fun(P) -> href(Href, P, Size) end,
    Nav = fun(Type, Disabled, Target) ->
                  Url = case Disabled of true -> null; false -> Link(Target) end,
                  entry(#{nav => true, type => atom_to_binary(Type), label => maps:get(Type, L),
                          icon => nav_icon(Type), first_last => Type =:= first orelse Type =:= last,
                          disabled => Disabled, tabindex => tabindex(not Disabled),
                          link => Url =/= null, href => url(Url)})
          end,
    Middle = case Simple of
                 true ->
                     [entry(#{info => true, text => fmt(maps:get(page_info, L), [Cur, Pages])})];
                 false ->
                     [case P of
                          gap -> entry(#{gap => true});
                          _ -> Url = Link(P),
                               entry(#{item => true, number => P, active => P =:= Cur,
                                       tabindex => tabindex(P =/= Cur),
                                       link => Url =/= null, href => url(Url)})
                      end || P <- visible_pages(Cur, Pages, Max)]
             end,
    FirstLast = FL andalso not Simple,
    #{entries => [Nav(first, Cur =:= 1, 1) || FirstLast]
                 ++ [Nav(prev, Cur =:= 1, Cur - 1)] ++ Middle ++ [Nav(next, Cur =:= Pages, Cur + 1)]
                 ++ [Nav(last, Cur =:= Pages, Pages) || FirstLast]}.

%% Every entry carries every key, so a section never looks a key up in
%% the enclosing context.
entry(M) ->
    maps:merge(#{gap => false, info => false, item => false, nav => false, link => false,
                 active => false, disabled => false, first_last => false, href => <<>>,
                 number => 0, tabindex => 0, type => <<>>, label => <<>>, icon => <<>>,
                 text => <<>>}, M).

tabindex(true) -> 0;
tabindex(false) -> -1.

url(null) -> <<>>;
url(U) -> U.

nav_icon(prev) -> <<"\x{2039}"/utf8>>;
nav_icon(next) -> <<"\x{203A}"/utf8>>;
nav_icon(first) -> <<"\x{00AB}"/utf8>>;
nav_icon(last) -> <<"\x{00BB}"/utf8>>.

href(null, _, _) -> null;
href(undefined, _, _) -> null;
href(T, P, Size) ->
    B = bin(T),
    B1 = binary:replace(B, <<"{page}">>, integer_to_binary(P), [global]),
    binary:replace(B1, <<"{size}">>, integer_to_binary(Size), [global]).

fmt(T, Args) ->
    {Out, _} = lists:foldl(fun(A, {Acc, I}) ->
                                   {binary:replace(Acc, <<"{", (integer_to_binary(I))/binary, "}">>,
                                                   bin(A), [global]), I + 1}
                           end, {bin(T), 0}, Args),
    Out.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => pagination, category => layout,
       option_docs => #{page_size => <<"Items per page (default 10).">>,
                        page_sizes => <<"Choices of the page-size select (default [10,20,50,100]).">>,
                        show_size_selector => <<"Show the page-size select (default true).">>,
                        show_jumper => <<"Show a go-to-page input.">>,
                        show_first_last => <<"Show first and last buttons.">>,
                        show_total => <<"Show the item count.">>,
                        max_visible => <<"Slots for page numbers and gaps (default 7).">>,
                        siblings => <<"Pages on each side of the current one (instead of max_visible).">>,
                        href => <<"Link template with {page} and {size}: pages become <a href> links the server "
                                  "renders (crawlable, no script needed). With on(change, Action) a "
                                  "plain click updates in place (the action renders the new content) "
                                  "and pushes the link's URL; without a binding the link loads.">>,
                        labels => <<"Map overriding prev, next, first, last, per_page, total, goto, goto_suffix, goto_confirm, page_info, aria_label, per_page_aria.">>,
                        name => <<"Submit the page as a hidden input.">>,
                        simple => <<"Previous, Page x / y, Next only.">>,
                        disabled => <<"Dim and ignore clicks.">>},
       methods => [#{name => setPage, args => <<"(N)">>, doc => <<"Go to page N without firing change.">>},
                   #{name => next, args => <<"()">>, doc => <<"Next page.">>},
                   #{name => prev, args => <<"()">>, doc => <<"Previous page.">>},
                   #{name => first, args => <<"()">>, doc => <<"First page.">>},
                   #{name => last, args => <<"()">>, doc => <<"Last page.">>},
                   #{name => setTotal, args => <<"(Items)">>, doc => <<"Change the item count; the page is clamped.">>},
                   #{name => setPageSize, args => <<"(N)">>, doc => <<"Change the page size.">>}],
       signature => <<"pagination(Total, Page, Css, Attrs)">>, root => <<"ah-pagination">>,
       flags => [simple, disabled],
       options => [page_size, page_sizes, show_size_selector, show_jumper, show_first_last,
                   show_total, max_visible, siblings, href, labels, name],
       behavior => <<"pagination">>, events => [<<"change">>],
       doc => <<"Page navigation; value is the current page, data-page-size the page size. "
                "Methods: setPage(n), next, prev, first, last, setTotal(n), setPageSize(n).">>}].
