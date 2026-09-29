%%%-------------------------------------------------------------------
%%% @doc The alert component (designs/04-components.md): `alert/3'
%%% builds an #ah_alert{} element record (include/aihtml_alert.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% aihtml's own component, styled by priv/css/extra/alert.css.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_alert).
-behaviour(aihtml_element).

-include("aihtml_alert.hrl").

-export([alert/3, render/1, fields/1, catalog/0]).

-import(aihtml_lib_display, [blank/1, method/3, svg/1, circle/3, line/4, polyline/1, path/1]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Alert (aihtml's own): an inline message box. Options: `title',
%% `icon' (true, false or html). `dismissible' adds a close button; the
%% browser fires `ah:dismiss' (cancellable) and removes the alert.
-spec alert(html(), css(), attrs()) -> #ah_alert{}.
alert(Children, Css, Attrs) ->
    ?E:build(?MODULE, #ah_alert{body = Children}, Css, Attrs).

%% @doc The field names of #ah_alert{}.
-spec fields(atom()) -> [atom()].
fields(ah_alert) -> record_info(fields, ah_alert).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_alert{}) -> html().
render(#ah_alert{body = Children, variant = Variant, title = Title} = R) ->
    Cls = ?E:classes(?MODULE, R),
    Icon = case R#ah_alert.icon of
               true -> alert_icon(Variant);
               false -> [];
               Html -> Html
           end,
    ?H:el('div',
          [[?H:el(span, Icon, [<<"ah-alert-icon">>], [{aria_hidden, <<"true">>}]) || Icon =/= []],
           ?H:el('div', [[?H:el('div', Title, [<<"ah-alert-title">>], []) || not blank(Title)],
                         ?H:el('div', Children, [<<"ah-alert-body">>], [])],
                 [<<"ah-alert-content">>], []),
           [?H:el(button, <<"×"/utf8>>, [<<"ah-alert-close">>],
                  [{type, button}, {aria_label, <<"Close">>}])
            || R#ah_alert.dismissible]],
          Cls, [[{role, <<"alert">>}, {data_ah, <<"alert">>}], ?E:root_attrs(R, 'ah:dismiss')]).

alert_icon(Variant) ->
    Shapes = case Variant of
                 info -> [circle(12, 12, 10), line(12, 16, 12, 12), line(12, 8, 12.01, 8)];
                 success -> [circle(12, 12, 10), polyline(<<"9 12 11 14 15 10">>)];
                 warning -> [path(<<"M10.29 3.86 1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 "
                                    "1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z">>),
                             line(12, 9, 12, 13), line(12, 17, 12.01, 17)];
                 error -> [circle(12, 12, 10), line(15, 9, 9, 15), line(9, 9, 15, 15)]
             end,
    svg(Shapes).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => alert, category => text, root => <<"ah-alert">>,
      signature => <<"alert(Children, Css, Attrs)">>,
      groups => #{variant => {[info, success, warning, error], info}},
      flags => [dismissible], options => [title, icon], behavior => <<"alert">>,
      events => [<<"ah:dismiss">>],
      doc => <<"Inline message box; dismissible alerts fire ah:dismiss and remove "
               "themselves. Method dismiss().">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{title => <<"Bold first line.">>,
                       icon => <<"true (the variant's icon, default), false, or custom HTML.">>,
                       dismissible => <<"Close button: fires ah:dismiss (cancellable), then removes "
                                        "the alert.">>},
      methods => [method(dismiss, <<"()">>, <<"Dismiss as if the close button were clicked.">>)]}.
