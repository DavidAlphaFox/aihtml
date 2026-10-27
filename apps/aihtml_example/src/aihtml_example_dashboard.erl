%% @doc GET /examples/dashboard — a faithful port of sigil's demo dashboard
%% (sigil demo.templates.dashboard + app-shell): same green-accent design
%% system (assets/dashboard.css, `.dash-*` classes), same sections, same
%% data, and the same client-side interactions (drag to reorder with the
%% order kept in localStorage, related-apps tabs, checkable tasks, mobile
%% drawer). Charts are aihtml chart components with sigil's ECharts
%% options; drag and drop is the aihtml sortable.
-module(aihtml_example_dashboard).

-include_lib("aihtml/include/aihtml.hrl").

-export([page/0, page/1, head/0, shell/3, shell/4, shell_head/0]).

-define(GREEN, <<"#00A76F">>).
-define(GREEN_SOFT, <<"#5BE49B">>).
-define(TEAL, <<"#007867">>).
-define(CYAN, <<"#00B8D9">>).
-define(AXIS, <<"#919EAB">>).
-define(SPLIT, <<"#F4F6F8">>).


%%====================================================================
%% Page
%%====================================================================

-spec page() -> aihtml:html().
page() ->
    shell(dashboard, top_bar(dashboard), page_head(),
          [stats_grid(), board()]).

%% @doc A page generated from Sigil's dashboard template registry. The
%% rendered template body is stored as an immutable private asset; this
%% function supplies aihtml's shell, active sidebar state and top bar.
-spec page(atom()) -> aihtml:html().
page(Page) ->
    shell(Page, top_bar(Page), [], [{safe, template_html(Page)}]).

template_html(Page) ->
    File = atom_to_binary(route(Page), utf8),
    Path = filename:join([code:priv_dir(aihtml_example), "dashboard_pages",
                          <<File/binary, ".html">>]),
    {ok, Bin} = file:read_file(Path),
    Bin.

%% @doc Sidebar atom -> Sigil template asset. Most names are identical;
%% the few aliases keep the sidebar's semantic keys stable.
route(analytics) -> analytics;
route(banking) -> banking;
route(booking) -> booking;
route(file) -> file;
route(course) -> course;
route(ecommerce) -> ecommerce;
route(kanban) -> kanban;
route(chat) -> chat;
route(mail) -> mail;
route(file_manager) -> 'file-manager';
route(user) -> user;
route(user_profile) -> 'user-profile';
route(user_cards) -> 'user-cards';
route(user_list) -> user;
route(user_create) -> 'user-create';
route(user_edit) -> 'user-edit';
route(user_account) -> 'user-account';
route(product) -> product;
route(product_list) -> product;
route(product_details) -> 'product-detail';
route(product_create) -> 'product-create';
route(product_edit) -> 'product-edit';
route(order) -> order;
route(order_list) -> order;
route(order_details) -> 'order-detail';
route(invoice) -> invoice;
route(invoice_list) -> invoice;
route(invoice_details) -> 'invoice-detail';
route(invoice_create) -> 'invoice-create';
route(invoice_edit) -> 'invoice-edit';
route(blog) -> blog;
route(blog_list) -> blog;
route(blog_details) -> 'blog-detail';
route(blog_create) -> 'blog-create';
route(blog_edit) -> 'blog-edit';
route(job) -> job;
route(job_list) -> job;
route(job_details) -> 'job-detail';
route(job_create) -> 'job-create';
route(job_edit) -> 'job-edit';
route(tour) -> tour;
route(tour_list) -> tour;
route(tour_details) -> 'tour-detail';
route(tour_create) -> 'tour-create';
route(tour_edit) -> 'tour-edit'.

%% @doc The Sigil dashboard shell, shared by the sidebar application pages
%% (calendar, mail and file manager) so every page uses the same nav, top
%% bar, mobile drawer, typography and green dashboard theme.
-spec shell(atom(), aihtml:html(), aihtml:html()) -> aihtml:html().
shell(Active, Head, Body) ->
    shell(Active, top_bar(Active), Head, Body).

-spec shell(atom(), aihtml:html(), aihtml:html(), aihtml:html()) -> aihtml:html().
shell(Active, TopBar, Head, Body) ->
    ah_div([side_nav(Active),
            ah_div([ah_div([TopBar, Head | Body],
                           [<<"dash-content">>], [])],
                   [<<"dash-main">>], []),
            tag('div', [], [<<"dash-backdrop">>], [])],
           [<<"dash">>], []).

%% @doc An arbitrary HTML tag (ah_el/4 is not in the header's imports).
tag(Name, Body, Css, Attrs) ->
    aihtml:ah_el(Name, Body, Css, Attrs).

%% @doc The page's <head> extras: sigil's Google Fonts (Nunito + Space
%% Grotesk + JetBrains Mono, the dashboard's typography, see dashboard.css
%% --dash-font) and the client-side behaviour: drawer toggle, layout order
%% persistence, related-apps tabs, checkable tasks, reset button.
-spec head() -> aihtml:html().
head() ->
    font_head() ++
    [{safe, <<"<script>\n"
             "(function(){\n"
             "  var LS_STATS='aihtml:dashboard:stat-order',\n"
             "      LS_SECTIONS='aihtml:dashboard:section-order';\n"
             "  function applyOrder(id,key){\n"
             "    var el=document.getElementById(id);if(!el)return;\n"
             "    try{\n"
             "      var saved=JSON.parse(localStorage.getItem(key)||'null');\n"
             "      if(!Array.isArray(saved))return;\n"
             "      var byId={};\n"
             "      Array.prototype.forEach.call(el.children,function(c){\n"
             "        var v=c.getAttribute('data-value');if(v)byId[v]=c;\n"
             "      });\n"
             "      var ids=Object.keys(byId);\n"
             "      if(saved.length!==ids.length||!ids.every(function(k){\n"
             "        return saved.indexOf(k)!==-1;\n"
             "      }))return;\n"
             "      saved.forEach(function(k){if(byId[k])el.appendChild(byId[k]);});\n"
             "    }catch(e){}\n"
             "  }\n"
             "  applyOrder('dash-stats',LS_STATS);\n"
             "  applyOrder('dash-board',LS_SECTIONS);\n"
             "  document.addEventListener('change',function(e){\n"
             "    var t=e.target;\n"
             "    if(!t||!t.hasAttribute||!t.hasAttribute('data-ah-value'))return;\n"
             "    try{localStorage.setItem(\n"
             "      t.id==='dash-board'?LS_SECTIONS:LS_STATS,\n"
             "      JSON.stringify(t.getAttribute('data-ah-value').split(','))\n"
             "    );}catch(e2){}\n"
             "  });\n"
             "  document.addEventListener('click',function(e){\n"
             "    var t=e.target;\n"
             "    var dash=document.querySelector('.dash');if(!dash)return;\n"
             "    if(t.closest&&t.closest('.dash-topbar-toggle')){\n"
             "      dash.classList.toggle('is-sidebar-open');return;\n"
             "    }\n"
             "    if(t.closest&&t.closest('.dash-backdrop')){\n"
             "      dash.classList.remove('is-sidebar-open');return;\n"
             "    }\n"
             "    if(t.closest&&t.closest('.dash-sidenav a')&&window.innerWidth<=768){\n"
             "      dash.classList.remove('is-sidebar-open');return;\n"
             "    }\n"
             "    var reset=t.closest&&t.closest('#dash-reset');\n"
             "    if(reset){\n"
             "      try{localStorage.removeItem(LS_STATS);\n"
             "          localStorage.removeItem(LS_SECTIONS);}catch(e3){}\n"
             "      location.reload();return;\n"
             "    }\n"
             "    var tab=t.closest&&t.closest('.dash-tab');\n"
             "    if(tab){\n"
             "      var card=tab.closest('.dash-card');\n"
             "      card.querySelectorAll('.dash-tab').forEach(function(b){\n"
             "        b.classList.toggle('is-active',b===tab);\n"
             "      });\n"
             "      card.querySelectorAll('.dash-apps-panel').forEach(function(p){\n"
             "        p.classList.toggle('is-active',\n"
             "          p.getAttribute('data-panel')===tab.getAttribute('data-tab'));\n"
             "      });\n"
             "      return;\n"
             "    }\n"
             "    var check=t.closest&&t.closest('.dash-task-check');\n"
             "    if(check){\n"
             "      var task=check.closest('.dash-task');\n"
             "      var done=task.getAttribute('data-done')!=='true';\n"
             "      task.setAttribute('data-done',String(done));\n"
             "      check.setAttribute('data-checked',String(done));\n"
             "      check.innerHTML=done\n"
             "        ?'<svg class=\"dash-icon\" width=\"12\" height=\"12\" viewBox=\"0 0 24 24\"'\n"
             "         +' fill=\"none\" stroke=\"currentColor\" stroke-width=\"3\"'\n"
             "         +' stroke-linecap=\"round\" stroke-linejoin=\"round\">'\n"
             "         +'<polyline points=\"20 6 9 17 4 12\"/></svg>'\n"
             "        :'';\n"
             "      var all=task.closest('.dash-card').querySelectorAll('.dash-task');\n"
             "      var n=task.closest('.dash-card')\n"
             "        .querySelectorAll('.dash-task[data-done=\"true\"]').length;\n"
             "      var sub=task.closest('.dash-card').querySelector('.dash-card-subtitle');\n"
             "      if(sub)sub.textContent=n+' of '+all.length+' completed';\n"
             "    }\n"
             "  });\n"
             "})();\n"
             "</script>">>}].

%% @doc Head extras for shell pages without the dashboard board's layout
%% persistence and dashboard-specific interactions.
-spec shell_head() -> aihtml:html().
shell_head() ->
    font_head() ++
        [{safe, <<"<script defer src=\"/static/js/jquery.min.js\"></script>">>},
         {safe, <<"<script defer src=\"/static/js/dashboard-pages.js\"></script>">>},
        {safe, <<"<script>\n"
                 "(function(){\n"
                 "  document.addEventListener('click',function(e){\n"
                 "    var t=e.target;\n"
                 "    var dash=document.querySelector('.dash');if(!dash)return;\n"
                 "    if(t.closest&&t.closest('.dash-topbar-toggle')){\n"
                 "      dash.classList.toggle('is-sidebar-open');return;\n"
                 "    }\n"
                 "    if(t.closest&&t.closest('.dash-backdrop')){\n"
                 "      dash.classList.remove('is-sidebar-open');return;\n"
                 "    }\n"
                 "    if(t.closest&&t.closest('.dash-sidenav a')&&window.innerWidth<=768){\n"
                 "      dash.classList.remove('is-sidebar-open');\n"
                 "    }\n"
                 "  });\n"
                 "})();\n"
                 "</script>">>}].

font_head() ->
    Fonts = <<"https://fonts.googleapis.com/css2?"
              "family=Nunito:wght@400;500;600;700;800"
              "&family=Space+Grotesk:wght@400;500;600;700"
              "&family=JetBrains+Mono:wght@400;500&display=swap">>,
    [aihtml_html:void(link, [], [{rel, <<"preconnect">>},
                                 {href, <<"https://fonts.googleapis.com">>}]),
     aihtml_html:void(link, [], [{rel, <<"preconnect">>},
                                 {href, <<"https://fonts.gstatic.com">>},
                                 {crossorigin, true}]),
     aihtml_html:void(link, [], [{rel, <<"stylesheet">>}, {href, Fonts}])].

%%====================================================================
%% Sidebar + topbar (sigil app-shell look)
%%====================================================================

side_nav(Active) ->
    %% Sigil's sidebar (demo.templates.app-shell/nav-groups): every item now
    %% points at the matching aihtml example route and its Sigil template.
    Groups = [#{label => <<"NAVIGATION">>,
                items => [#{key => home, label => <<"Home">>, icon => home, href => <<"/">>},
                          #{key => components, label => <<"Components">>, icon => components,
                            href => <<"/components">>},
                          #{key => demos, label => <<"Demos">>, icon => play, href => <<"/demo">>},
                          #{key => examples, label => <<"Examples">>, icon => layout_grid,
                            href => <<"/examples">>}]},
              #{label => <<"OVERVIEW">>,
                items => [#{key => dashboard, label => <<"Dashboard">>, icon => dashboard,
                            href => <<"/examples/dashboard">>},
                          #{key => analytics, label => <<"Analytics">>, icon => analytics,
                            href => <<"/examples/analytics">>},
                          #{key => banking, label => <<"Banking">>, icon => banking,
                            href => <<"/examples/banking">>},
                          #{key => booking, label => <<"Booking">>, icon => booking,
                            href => <<"/examples/booking">>},
                          #{key => file, label => <<"File">>, icon => file,
                            href => <<"/examples/file">>},
                          #{key => course, label => <<"Course">>, icon => course,
                            href => <<"/examples/course">>},
                          #{key => ecommerce, label => <<"Ecommerce">>, icon => ecommerce,
                            href => <<"/examples/ecommerce">>}]},
              #{label => <<"APPS">>,
                items => [#{key => calendar_app, label => <<"Calendar">>, icon => calendar,
                            href => <<"/examples/calendar">>},
                          #{key => kanban, label => <<"Kanban">>, icon => kanban,
                            href => <<"/examples/kanban">>},
                          #{key => chat, label => <<"Chat">>, icon => chat,
                            href => <<"/examples/chat">>},
                          #{key => mail, label => <<"Mail">>, icon => mail,
                            href => <<"/examples/mail">>},
                          #{key => file_manager, label => <<"File Manager">>, icon => folder,
                            href => <<"/examples/file-manager">>}]},
              #{label => <<"MANAGEMENT">>,
                items => [#{key => user, label => <<"User">>, icon => user,
                            children => [#{key => user_profile, label => <<"Profile">>,
                                           href => <<"/examples/user-profile">>},
                                         #{key => user_cards, label => <<"Cards">>,
                                           href => <<"/examples/user-cards">>},
                                         #{key => user_list, label => <<"List">>,
                                           href => <<"/examples/user">>},
                                         #{key => user_create, label => <<"Create">>,
                                           href => <<"/examples/user-create">>},
                                         #{key => user_edit, label => <<"Edit">>,
                                           href => <<"/examples/user-edit">>},
                                         #{key => user_account, label => <<"Account">>,
                                           href => <<"/examples/user-account">>}]},
                          #{key => product, label => <<"Product">>, icon => product,
                            children => [#{key => product_list, label => <<"List">>,
                                           href => <<"/examples/product">>},
                                         #{key => product_details, label => <<"Details">>,
                                           href => <<"/examples/product-detail">>},
                                         #{key => product_create, label => <<"Create">>,
                                           href => <<"/examples/product-create">>},
                                         #{key => product_edit, label => <<"Edit">>,
                                           href => <<"/examples/product-edit">>}]},
                          #{key => order, label => <<"Order">>, icon => order,
                            children => [#{key => order_list, label => <<"List">>,
                                           href => <<"/examples/order">>},
                                         #{key => order_details, label => <<"Details">>,
                                           href => <<"/examples/order-detail">>}]},
                          #{key => invoice, label => <<"Invoice">>, icon => invoice,
                            children => [#{key => invoice_list, label => <<"List">>,
                                           href => <<"/examples/invoice">>},
                                         #{key => invoice_details, label => <<"Details">>,
                                           href => <<"/examples/invoice-detail">>},
                                         #{key => invoice_create, label => <<"Create">>,
                                           href => <<"/examples/invoice-create">>},
                                         #{key => invoice_edit, label => <<"Edit">>,
                                           href => <<"/examples/invoice-edit">>}]},
                          #{key => blog, label => <<"Blog">>, icon => blog,
                            children => [#{key => blog_list, label => <<"List">>,
                                           href => <<"/examples/blog">>},
                                         #{key => blog_details, label => <<"Details">>,
                                           href => <<"/examples/blog-detail">>},
                                         #{key => blog_create, label => <<"Create">>,
                                           href => <<"/examples/blog-create">>},
                                         #{key => blog_edit, label => <<"Edit">>,
                                           href => <<"/examples/blog-edit">>}]},
                          #{key => job, label => <<"Job">>, icon => job,
                            children => [#{key => job_list, label => <<"List">>,
                                           href => <<"/examples/job">>},
                                         #{key => job_details, label => <<"Details">>,
                                           href => <<"/examples/job-detail">>},
                                         #{key => job_create, label => <<"Create">>,
                                           href => <<"/examples/job-create">>},
                                         #{key => job_edit, label => <<"Edit">>,
                                           href => <<"/examples/job-edit">>}]},
                          #{key => tour, label => <<"Tour">>, icon => tour,
                            children => [#{key => tour_list, label => <<"List">>,
                                           href => <<"/examples/tour">>},
                                         #{key => tour_details, label => <<"Details">>,
                                           href => <<"/examples/tour-detail">>},
                                         #{key => tour_create, label => <<"Create">>,
                                           href => <<"/examples/tour-create">>},
                                         #{key => tour_edit, label => <<"Edit">>,
                                           href => <<"/examples/tour-edit">>}]}]}],
    tag('aside',
          [ah_a([ah_span(icon(sparkles, 20, 2), [<<"dash-sidenav-mark">>], []),
                 ah_span(<<"aihtml">>, [<<"dash-sidenav-name">>], [])],
                [], [{href, <<"/examples">>}, {class, <<"dash-sidenav-brand">>}]),
           ah_nav([nav_group(G, Active) || G <- Groups],
                  [<<"dash-sidenav-nav">>, <<"dash-nav">>], []),
           ah_div([ah_div([], [<<"dash-upgrade-glow">>], []),
                   ah_div(<<"Go Pro 🚀"/utf8>>, [<<"dash-upgrade-title">>], []),
                   ah_p(<<"Unlock all components & templates."/utf8>>,
                        [<<"dash-upgrade-desc">>], []),
                   tag('button', <<"Upgrade">>, [<<"dash-btn">>, <<"dash-btn-light">>, <<"dash-btn-block">>],
                         [{type, button}])],
                  [<<"dash-upgrade">>], [])],
          [<<"dash-sidenav">>], []).

nav_group(#{label := Label, items := Items}, Active) ->
    ah_div([ah_div(Label, [<<"dash-nav-label">>], [])
            | [nav_item(I, Active) || I <- Items]],
           [<<"dash-nav-group">>], []).

nav_item(#{children := Children} = I, Active) ->
    IsOpen = lists:member(Active, [maps:get(key, C) || C <- Children]),
    %% Sigil expands only the active branch. In aihtml's dashboard this
    %% sidebar is also the examples index, so keep every branch's child
    %% links visible and clickable by default (users may still collapse).
    tag('details',
          [tag('summary', nav_link(I, true),
               [<<"dash-nav-item">>, <<"dash-nav-item--parent">>], []),
           ah_div([ah_div([nav_item(C, Active) || C <- Children],
                          [<<"dash-nav-children-inner">>], [])],
                  [<<"dash-nav-children">>], [])],
          [<<"dash-nav-node">>], [{open, true} || IsOpen orelse Children =/= []]);
nav_item(I, Active) ->
    IsCurrent = maps:get(key, I) =:= Active,
    ah_a(nav_link(I, false), [<<"dash-nav-item">>, [<<"is-active">> || IsCurrent]],
         [{href, maps:get(href, I)}]).

%% Sigil's nav-tree only renders an icon span when the item has an icon;
%% child rows (Profile, List, ...) are text-only with connector lines.
nav_link(I, Parent) ->
    [ [ah_span(icon(maps:get(icon, I)), [<<"dash-nav-icon">>], [])
       || maps:is_key(icon, I)],
      ah_span(maps:get(label, I), [<<"dash-nav-text">>], []),
      [ah_span(icon(chevron_down, 16), [<<"dash-nav-caret">>], []) || Parent] ].

top_bar(Active) ->
    ah_header([tag('button', icon(menu, 22, 2), [<<"dash-topbar-toggle">>],
                     [{type, button}, {aria_label, <<"Toggle sidebar"/utf8>>}]),
               crumbs([<<"Home"/utf8>>, <<"Examples"/utf8>>, active_label(Active)]),
               ah_div([tag('button', icon(search, 20), [<<"dash-icon-btn">>],
                             [{type, button}, {aria_label, <<"Search">>}]),
                       tag('button', [icon(bell, 20), ah_span(<<"4">>, [<<"dash-dot">>], [])],
                             [<<"dash-icon-btn">>, <<"dash-has-dot">>],
                             [{type, button}, {aria_label, <<"Notifications">>}]),
                       tag('button', icon(settings, 20), [<<"dash-icon-btn">>],
                             [{type, button}, {aria_label, <<"Settings">>}]),
                       ah_div(<<"DA">>, [<<"dash-avatar">>], [])],
                      [<<"dash-topbar-actions">>], [])],
              [<<"dash-topbar">>], []).

active_label(dashboard) -> <<"Dashboard"/utf8>>;
active_label(analytics) -> <<"Analytics"/utf8>>;
active_label(banking) -> <<"Banking"/utf8>>;
active_label(booking) -> <<"Booking"/utf8>>;
active_label(file) -> <<"File"/utf8>>;
active_label(course) -> <<"Course"/utf8>>;
active_label(ecommerce) -> <<"Ecommerce"/utf8>>;
active_label(kanban) -> <<"Kanban"/utf8>>;
active_label(chat) -> <<"Chat"/utf8>>;
active_label(user) -> <<"User"/utf8>>;
active_label(user_profile) -> <<"Profile"/utf8>>;
active_label(user_cards) -> <<"Cards"/utf8>>;
active_label(user_list) -> <<"List"/utf8>>;
active_label(user_create) -> <<"Create"/utf8>>;
active_label(user_edit) -> <<"Edit"/utf8>>;
active_label(user_account) -> <<"Account"/utf8>>;
active_label(product) -> <<"Product"/utf8>>;
active_label(product_list) -> <<"List"/utf8>>;
active_label(product_details) -> <<"Details"/utf8>>;
active_label(product_create) -> <<"Create"/utf8>>;
active_label(product_edit) -> <<"Edit"/utf8>>;
active_label(order) -> <<"Order"/utf8>>;
active_label(order_list) -> <<"List"/utf8>>;
active_label(order_details) -> <<"Details"/utf8>>;
active_label(invoice) -> <<"Invoice"/utf8>>;
active_label(invoice_list) -> <<"List"/utf8>>;
active_label(invoice_details) -> <<"Details"/utf8>>;
active_label(invoice_create) -> <<"Create"/utf8>>;
active_label(invoice_edit) -> <<"Edit"/utf8>>;
active_label(blog) -> <<"Blog"/utf8>>;
active_label(blog_list) -> <<"List"/utf8>>;
active_label(blog_details) -> <<"Details"/utf8>>;
active_label(blog_create) -> <<"Create"/utf8>>;
active_label(blog_edit) -> <<"Edit"/utf8>>;
active_label(job) -> <<"Job"/utf8>>;
active_label(job_list) -> <<"List"/utf8>>;
active_label(job_details) -> <<"Details"/utf8>>;
active_label(job_create) -> <<"Create"/utf8>>;
active_label(job_edit) -> <<"Edit"/utf8>>;
active_label(tour) -> <<"Tour"/utf8>>;
active_label(tour_list) -> <<"List"/utf8>>;
active_label(tour_details) -> <<"Details"/utf8>>;
active_label(tour_create) -> <<"Create"/utf8>>;
active_label(tour_edit) -> <<"Edit"/utf8>>;
active_label(calendar_app) -> <<"Calendar"/utf8>>;
active_label(mail) -> <<"Mail"/utf8>>;
active_label(file_manager) -> <<"File Manager"/utf8>>;
active_label(_) -> <<"Examples"/utf8>>.

crumbs(Cs) ->
    {_, Items} = lists:foldl(fun(C, {I, Acc}) ->
                                     Sep = [ah_span(<<"›"/utf8>>, [<<"dash-crumb-sep">>], [])
                                            || I > 0],
                                     Class = [<<"dash-crumb-muted">> || I < length(Cs) - 1],
                                     {I + 1, Acc ++ Sep ++ [ah_span(C, Class, [])]}
                             end, {0, []}, Cs),
    ah_div(Items, [<<"dash-crumbs">>], []).

page_head() ->
    ah_div([ah_div([ah_h1(<<"Hi, Welcome back 👋"/utf8>>, [<<"dash-greeting">>], []),
                    ah_p(<<"Drag the handle on any stat card or block to rearrange — "
                           "your layout saves automatically."/utf8>>,
                         [<<"dash-greeting-sub">>], [])],
                   [], []),
            ah_div([tag('button', [icon(reset, 16), ah_span(<<"Reset layout">>, [], [])],
                          [<<"dash-btn">>, <<"dash-btn-ghost">>],
                          [{type, button}, {id, <<"dash-reset">>}]),
                    tag('button', [icon(sparkles, 16, 2), ah_span(<<"New report">>, [], [])],
                          [<<"dash-btn">>, <<"dash-btn-ink">>], [{type, button}])],
                   [<<"dash-pagehead-actions">>], [])],
           [<<"dash-pagehead">>], []).

%%====================================================================
%% Stat widgets (draggable, order persisted in localStorage)
%%====================================================================

stats_grid() ->
    Stats = [#{key => users, title => <<"Total active users">>, total => <<"18,765">>,
              delta => 2.6, color => success, icon => users,
              spark => [40, 26, 60, 30, 86, 40, 70, 24]},
             #{key => installed, title => <<"Total installed">>, total => <<"4,876">>,
              delta => 0.2, color => info, icon => package,
              spark => [22, 50, 35, 70, 56, 84, 60, 46]},
             #{key => downloads, title => <<"Total downloads">>, total => <<"678">>,
              delta => -0.1, color => error, icon => download,
              spark => [32, 58, 28, 70, 45, 70, 24, 78]},
             #{key => messages, title => <<"Messages">>, total => <<"234">>,
              delta => 3.6, color => warning, icon => message,
              spark => [56, 30, 23, 54, 47, 40, 62, 73]}],
    Items = [{maps:get(key, S), stat_card(S)} || S <- Stats],
    ah_sortable(Items, undefined, [<<"dash-stats-grid">>],
                [{id, <<"dash-stats">>}, {aria_label, <<"Reorder metrics"/utf8>>}]).

stat_card(#{title := Title, total := Total, delta := Delta, color := Color, icon := Ico,
            spark := Spark}) ->
    ah_div([ah_span(icon(grip, 16), [<<"dash-stat-grip">>], []),
            ah_div([ah_div([ah_div(Title, [<<"dash-stat-title">>], []),
                            ah_div(Total, [<<"dash-stat-total">>], []),
                            trend_badge(Delta, <<"last 7 days"/utf8>>)],
                           [<<"dash-stat-text">>], []),
                    ah_div(icon(Ico, 26, 2), [<<"dash-stat-icon">>], [])],
                   [<<"dash-stat-top">>], []),
            spark_bars(Spark),
            {safe, <<"<svg class=\"dash-stat-shape\" viewBox=\"0 0 240 240\" fill=\"currentColor\""
                    " aria-hidden=\"true\"><path d=\"M120,16 L224,120 L120,224 L16,120 Z\"/></svg>">>}],
           [<<"dash-stat">>], [{data_color, Color}]).

trend_pill(Delta) ->
    Up = Delta >= 0,
    Sign = if Up -> <<"+">>; true -> <<>> end,
    ah_span([icon(if Up -> trending_up; true -> trending_down end, 12, 2),
             ah_span([Sign, float_bin(Delta), <<"%">>], [], [])],
            [<<"dash-trend-pill">>], [{data_up, bool_attr(Up)}]).

trend_badge(Delta, Since) ->
    ah_div([trend_pill(Delta), ah_span(Since, [<<"dash-trend-since">>], [])],
           [<<"dash-trend">>], []).

%% 84x56 bar sparkline; Points are 0~100, the last bar is the active one.
spark_bars(Points) ->
    N = length(Points),
    Gap = 2,
    Bw = (84 - Gap * (N - 1)) / N,
    Bars = lists:map(fun({I, V}) ->
                             H = max(4, V / 100 * 56),
                             X = I * (Bw + Gap),
                             Y = 56 - H,
                             Attrs = [{x, fmt(X)}, {y, fmt(Y)}, {width, fmt(Bw)}, {height, fmt(H)},
                                      {rx, <<"1.4">>}]
                                     ++ [[{data_active, <<"true">>} || I =:= N - 1]],
                             svg_el(<<"rect">>, Attrs)
                     end, lists:enumerate(0, Points)),
    {safe, [<<"<svg class=\"dash-spark\" viewBox=\"0 0 84 56\" preserveAspectRatio=\"none\">">>,
            Bars, <<"</svg>">>]}.

svg_el(Tag, Attrs0) ->
    Attrs = lists:flatten(Attrs0),
    iolist_to_binary(
      [<<"<">>, Tag,
       [[<<" ">>, attr_name(K), <<"=\"">>, attr_val(V), <<"\"">>] || {K, V} <- Attrs],
       <<"/>">>]).

attr_name(K) when is_atom(K) ->
    binary:replace(atom_to_binary(K), <<"_">>, <<"-">>, [global]);
attr_name(K) ->
    K.

attr_val(V) when is_binary(V) -> V;
attr_val(V) when is_integer(V) -> integer_to_binary(V);
attr_val(V) when is_float(V) -> fmt(V).

%%====================================================================
%% Section board (12-column grid, drag handle, order persisted)
%%====================================================================

board() ->
    Sections = [{welcome, 12, fun hero/0},
                {goals, 12, fun goals/0},
                {revenue, 12, fun revenue_card/0},
                {download, 4, fun download_card/0},
                {visits, 8, fun visits_card/0},
                {related, 8, fun related_card/0},
                {countries, 4, fun countries_card/0},
                {news, 8, fun news_card/0},
                {authors, 4, fun authors_card/0},
                {inbox, 8, fun inbox_card/0},
                {tasks, 4, fun tasks_card/0},
                {transactions, 8, fun transactions_card/0},
                {timeline, 4, fun timeline_card/0},
                {products, 6, fun products_card/0},
                {traffic, 6, fun traffic_card/0},
                {conversion, 8, fun conversion_card/0},
                {sales, 4, fun sales_card/0},
                {summary, 12, fun summary/0}],
    Items = [{Id, Render(), [{class, cell_class(Span)}]} || {Id, Span, Render} <- Sections],
    ah_sortable(Items, undefined, [<<"dash-board">>, handle],
                [{id, <<"dash-board">>}, {aria_label, <<"Reorder sections"/utf8>>}]).

cell_class(Span) ->
    <<"dash-cell dash-col-", (integer_to_binary(Span))/binary>>.

dash_card(Title, Subtitle, BodyClass, Body) ->
    ah_div([ah_div([ah_div([ah_h3(Title, [<<"dash-card-title">>], []),
                            ah_p(Subtitle, [<<"dash-card-subtitle">>], [])],
                           [], []),
                    tag('button', icon(more, 18), [<<"dash-icon-btn">>, <<"sm">>],
                          [{type, button}, {aria_label, <<"More">>}])],
                   [<<"dash-card-head">>], []),
            ah_div(Body, [<<"dash-card-body">>, [BodyClass || BodyClass =/= <<>>]], [])],
           [<<"dash-card">>], []).

%%====================================================================
%% Welcome hero
%%====================================================================

hero() ->
    ah_div([ah_div([ah_p(<<"WELCOME BACK 👋"/utf8>>, [<<"dash-hero-eyebrow">>], []),
                    ah_h2(<<"Ship faster with aihtml"/utf8>>, [<<"dash-hero-title">>], []),
                    ah_p(<<"Your team closed 32 issues this week — that's 18% above your average. "
                           "Keep the streak going."/utf8>>,
                         [<<"dash-hero-desc">>], []),
                    ah_div([tag('button', <<"Explore tasks">>,
                                  [<<"dash-btn">>, <<"dash-btn-light">>], [{type, button}]),
                            tag('button', <<"View report">>,
                                  [<<"dash-btn">>, <<"dash-btn-ghost-light">>], [{type, button}])],
                           [<<"dash-hero-actions">>], [])],
                   [<<"dash-hero-text">>], []),
            ah_div([ah_div([], [<<"dash-hero-glow">>], []),
                    ah_div([ah_div([], [<<"dash-hero-card-row">>], []),
                            ah_div([], [<<"dash-hero-card-row">>, <<"sm">>], []),
                            hero_spark()],
                           [<<"dash-hero-card">>], [{data_card, <<"a">>}]),
                    ah_div([ah_div([], [<<"dash-hero-card-btn">>], []),
                            ah_div([ah_span([], [<<"dash-hero-chip">>], [{data_tone, <<"p">>}]),
                                    ah_span([], [<<"dash-hero-chip">>], [{data_tone, <<"s">>}]),
                                    ah_span([], [<<"dash-hero-chip">>], [{data_tone, <<"p">>}])],
                                   [<<"dash-hero-card-chips">>], [])],
                           [<<"dash-hero-card">>], [{data_card, <<"b">>}]),
                    ah_div([ah_div([], [<<"dash-hero-card-meta">>], []),
                            ah_div([ah_div([], [<<"dash-hero-card-fill">>], [])],
                                   [<<"dash-hero-card-track">>], [])],
                           [<<"dash-hero-card">>], [{data_card, <<"c">>}]),
                    ah_span(icon(sparkles, 28, 2), [<<"dash-hero-sparkle">>], [])],
                   [<<"dash-hero-illus">>], [])],
           [<<"dash-hero">>], []).

hero_spark() ->
    {safe, <<"<svg class=\"dash-hero-spark\" viewBox=\"0 0 80 28\" preserveAspectRatio=\"none\">"
            "<path d=\"M 0 24 L 12 18 L 24 22 L 36 10 L 48 14 L 60 6 L 80 8\""
            " fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\""
            " stroke-linecap=\"round\" stroke-linejoin=\"round\"/></svg>">>}.

%%====================================================================
%% Goal progress
%%====================================================================

goals() ->
    Goals = [#{label => <<"Quarterly revenue">>, value => <<"$184.3k">>,
              sub => <<"of $250.0k target · 74%"/utf8>>, pct => 74, delta => 6.4,
              color => success, hint => <<"vs Q2">>},
             #{label => <<"Active subscribers">>, value => <<"4.1k">>,
              sub => <<"of 5.0k target · 82%"/utf8>>, pct => 82, delta => 3.2,
              color => info, hint => <<"vs last month">>},
             #{label => <<"Support SLA">>, value => <<"94%">>,
              sub => <<"of 100% target · 94%"/utf8>>, pct => 94, delta => -1.1,
              color => warning, hint => <<"30-day rolling">>}],
    dash_card(<<"Goal progress">>, <<"Track quarterly KPIs"/utf8>>, <<"dash-goals-grid">>,
              [goal(G) || G <- Goals]).

goal(#{label := Label, value := Value, sub := Sub, pct := Pct, delta := Delta,
       color := Color, hint := Hint}) ->
    ah_div([ah_div([ah_div(Label, [<<"dash-goal-label">>], []),
                    trend_pill(Delta)],
                   [<<"dash-goal-head">>], []),
            ah_div(Value, [<<"dash-goal-value">>], []),
            ah_p(Sub, [<<"dash-goal-sub">>], []),
            ah_div([ah_div([], [<<"dash-goal-fill">>], [])],
                   [<<"dash-goal-track">>], [{style, width_style(Pct)}]),
            ah_p(Hint, [<<"dash-goal-hint">>], [])],
           [<<"dash-goal">>], [{data_color, Color}]).

width_style(Pct) ->
    <<"width:", (integer_to_binary(Pct))/binary, "%">>.

%%====================================================================
%% Charts (sigil's ECharts options)
%%====================================================================

v_grad(Top, Bottom) ->
    #{type => linear, x => 0, y => 0, x2 => 0, y2 => 1,
      colorStops => [#{offset => 0, color => Top}, #{offset => 1, color => Bottom}]}.

tooltip_axis() ->
    #{trigger => axis,
      backgroundColor => <<"rgba(33,43,54,0.92)">>, borderWidth => 0,
      textStyle => #{color => <<"#fff">>}, padding => [8, 12]}.

revenue_card() ->
    Option = #{tooltip => tooltip_axis(),
               legend => #{data => [<<"Paid">>, <<"Free">>], bottom => 0, icon => circle,
                           itemWidth => 8, itemHeight => 8,
                           textStyle => #{color => ?AXIS}},
               grid => #{left => 8, right => 16, top => 16, bottom => 36, containLabel => true},
               xAxis => #{type => category, boundaryGap => false,
                          data => [<<"Mon">>, <<"Tue">>, <<"Wed">>, <<"Thu">>, <<"Fri">>,
                                   <<"Sat">>, <<"Sun">>],
                          axisLine => #{show => false}, axisTick => #{show => false},
                          axisLabel => #{color => ?AXIS}},
               yAxis => #{type => value, splitLine => #{lineStyle => #{color => ?SPLIT}},
                          axisLabel => #{color => ?AXIS}},
               series => [#{name => <<"Paid">>, type => line, smooth => true, symbol => none, z => 3,
                            data => [3200, 3800, 3500, 4200, 4800, 5100, 4900],
                            lineStyle => #{width => 3, color => ?GREEN,
                                           shadowColor => <<"rgba(0,167,111,0.30)">>,
                                           shadowBlur => 12, shadowOffsetY => 8},
                            itemStyle => #{color => ?GREEN},
                            areaStyle => #{color => v_grad(<<"rgba(0,167,111,0.32)">>,
                                                            <<"rgba(0,167,111,0.02)">>)}},
                           #{name => <<"Free">>, type => line, smooth => true, symbol => none, z => 2,
                            data => [1200, 1500, 1800, 1600, 2100, 2400, 2200],
                            lineStyle => #{width => 3, color => ?CYAN},
                            itemStyle => #{color => ?CYAN},
                            areaStyle => #{color => v_grad(<<"rgba(0,184,217,0.26)">>,
                                                            <<"rgba(0,184,217,0.02)">>)}}]},
    dash_card(<<"Revenue trend"/utf8>>, <<"Last 7 days · paid + free"/utf8>>, <<>>,
              [ah_chart(Option, [], [{height, 300}, {aria_label, <<"Revenue trend chart">>}])]).

download_card() ->
    Option = #{tooltip => #{trigger => item,
                            formatter => <<"{b}: {c} ({d}%)">>,
                            backgroundColor => <<"rgba(33,43,54,0.92)">>, borderWidth => 0,
                            textStyle => #{color => <<"#fff">>}, padding => [8, 12]},
               legend => #{bottom => 0, icon => circle, itemWidth => 8, itemHeight => 8,
                           itemGap => 16, textStyle => #{color => ?AXIS}},
               graphic => [#{type => text, id => dash_total_label, left => center, top => <<"36%">>,
                            style => #{text => <<"Total">>, textAlign => center, fill => ?AXIS,
                                       font => <<"600 12px Nunito, system-ui, sans-serif">>}},
                           #{type => text, id => dash_total_value, left => center, top => <<"44%">>,
                            style => #{text => <<"188,245">>, textAlign => center, fill => <<"#1C252E">>,
                                       font => <<"800 24px Nunito, system-ui, sans-serif">>}}],
               series => [#{name => <<"Downloads">>, type => pie, radius => [<<"50%">>,
                                                                               <<"70%">>],
                            center => [<<"50%">>, <<"42%">>],
                            avoidLabelOverlap => false, padAngle => 2,
                            label => #{show => false}, labelLine => #{show => false},
                            itemStyle => #{borderColor => <<"#fff">>, borderWidth => 2,
                                           borderRadius => 6},
                            emphasis => #{scale => true, scaleSize => 6,
                                          itemStyle => #{shadowBlur => 14,
                                                         shadowColor => <<"rgba(145,158,171,0.35)">>}},
                            data => [#{name => <<"Mac">>, value => 38566,
                                       itemStyle => #{color => <<"#C8FAD6">>}},
                                     #{name => <<"Window">>, value => 55566,
                                       itemStyle => #{color => ?GREEN_SOFT}},
                                     #{name => <<"iOS">>, value => 47057,
                                       itemStyle => #{color => ?GREEN}},
                                     #{name => <<"Android">>, value => 47056,
                                       itemStyle => #{color => ?TEAL}}]}]},
    dash_card(<<"Current download"/utf8>>, <<"By operating system"/utf8>>, <<>>,
              [ah_chart(Option, [], [{height, 300}, {aria_label, <<"Downloads by OS chart">>}])]).

visits_card() ->
    Option = #{tooltip => #{trigger => axis, axisPointer => #{type => shadow},
                            backgroundColor => <<"rgba(33,43,54,0.92)">>, borderWidth => 0,
                            textStyle => #{color => <<"#fff">>}, padding => [8, 12]},
               legend => #{data => [<<"Team A"/utf8>>, <<"Team B"/utf8>>], bottom => 0,
                           left => center, icon => roundRect, itemWidth => 14, itemHeight => 10,
                           textStyle => #{color => ?AXIS}},
               grid => #{left => 8, right => 8, top => 16, bottom => 36, containLabel => true},
               xAxis => #{type => category,
                          data => [<<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>, <<"May">>,
                                   <<"Jun">>, <<"Jul">>, <<"Aug">>, <<"Sep">>],
                          axisLine => #{lineStyle => #{color => ?SPLIT}}, axisTick => #{show => false},
                          axisLabel => #{color => ?AXIS}},
               yAxis => #{type => value,
                          splitLine => #{lineStyle => #{color => ?SPLIT, type => dashed}},
                          axisLabel => #{color => ?AXIS}},
               series => [#{name => <<"Team A"/utf8>>, type => bar,
                            data => [43, 33, 22, 37, 67, 68, 37, 24, 55],
                            barMaxWidth => 24, itemStyle => #{color => ?GREEN,
                                                              borderRadius => [4, 4, 0, 0]}},
                          #{name => <<"Team B"/utf8>>, type => bar,
                            data => [51, 70, 47, 67, 40, 37, 24, 70, 24],
                            barMaxWidth => 24, itemStyle => #{color => ?CYAN,
                                                              borderRadius => [4, 4, 0, 0]}}]},
    dash_card(<<"Website visits"/utf8>>, <<"(+43%) than last year"/utf8>>, <<>>,
              [ah_chart(Option, [], [{height, 300}, {aria_label, <<"Website visits chart">>}])]).

%%====================================================================
%% Related applications (tabs)
%%====================================================================

related_card() ->
    Tabs = ah_div([tag('button', <<"Top 7 days">>,
                         [<<"dash-tab">>, [<<"is-active">> || K =:= <<"7d">>]],
                         [{type, button}, {data_tab, K}])
                   || K <- [<<"7d">>, <<"30d">>, <<"all">>]],
                  [<<"dash-tabs">>], []),
    Panels = [ah_div([app_row(A) || A <- maps:get(K, related_apps())],
                     [<<"dash-apps-panel">>, [<<"is-active">> || K =:= <<"7d">>]],
                     [{data_panel, K}])
              || K <- [<<"7d">>, <<"30d">>, <<"all">>]],
    dash_card(<<"Related applications"/utf8>>, <<"Most active this period"/utf8>>,
              <<>>, [Tabs | Panels]).

related_apps() ->
    #{<<"7d">> => [#{letter => <<"M">>, name => <<"Microsoft office 365">>, price => free,
                      color => info, dl => <<"9.91k">>, size => <<"9.68 MB">>, rate => <<"9.91k">>},
                    #{letter => <<"O">>, name => <<"Opera">>, price => free,
                      color => error, dl => <<"1.95k">>, size => <<"1.9 MB">>, rate => <<"1.95k">>},
                    #{letter => <<"A">>, name => <<"Adobe acrobat reader DC">>, price => 68.71,
                      color => warning, dl => <<"9.12k">>, size => <<"8.91 MB">>, rate => <<"9.12k">>},
                    #{letter => <<"J">>, name => <<"Joplin">>, price => free,
                      color => success, dl => <<"6.98k">>, size => <<"6.82 MB">>, rate => <<"6.98k">>},
                    #{letter => <<"T">>, name => <<"Topaz photo AI">>, price => 52.17,
                      color => secondary, dl => <<"8.49k">>, size => <<"8.29 MB">>, rate => <<"8.49k">>}],
      <<"30d">> => [#{letter => <<"S">>, name => <<"Slack">>, price => free,
                      color => warning, dl => <<"12.4k">>, size => <<"112 MB">>, rate => <<"8.40k">>},
                    #{letter => <<"N">>, name => <<"Notion">>, price => free,
                      color => secondary, dl => <<"10.8k">>, size => <<"189 MB">>, rate => <<"10.2k">>},
                    #{letter => <<"F">>, name => <<"Figma">>, price => free,
                      color => success, dl => <<"9.50k">>, size => <<"98 MB">>, rate => <<"9.30k">>},
                    #{letter => <<"L">>, name => <<"Linear">>, price => free,
                      color => info, dl => <<"4.12k">>, size => <<"55 MB">>, rate => <<"4.05k">>},
                    #{letter => <<"S">>, name => <<"Sketch">>, price => 99.00,
                      color => error, dl => <<"3.30k">>, size => <<"76 MB">>, rate => <<"3.20k">>}],
      <<"all">> => [#{letter => <<"V">>, name => <<"Visual Studio Code">>, price => free,
                      color => info, dl => <<"184k">>, size => <<"85 MB">>, rate => <<"180k">>},
                    #{letter => <<"C">>, name => <<"Chrome">>, price => free,
                      color => warning, dl => <<"176k">>, size => <<"120 MB">>, rate => <<"170k">>},
                    #{letter => <<"S">>, name => <<"Spotify">>, price => free,
                      color => success, dl => <<"152k">>, size => <<"256 MB">>, rate => <<"150k">>},
                    #{letter => <<"P">>, name => <<"Photoshop">>, price => 239.88,
                      color => secondary, dl => <<"96k">>, size => <<"1.8 GB">>, rate => <<"90k">>},
                    #{letter => <<"F">>, name => <<"Final Cut Pro">>, price => 299.99,
                      color => error, dl => <<"44k">>, size => <<"3.2 GB">>, rate => <<"42k">>}]}.

app_row(#{letter := Letter, name := Name, price := Price, color := Color,
          dl := Dl, size := Size, rate := Rate}) ->
    PriceHtml = case Price of
                    free -> ah_span(<<"Free">>, [<<"dash-app-price">>], [{data_tone, <<"free">>}]);
                    N -> ah_span([<<"$">>, money_bin(N)], [<<"dash-app-price">>],
                                 [{data_tone, <<"paid">>}])
                end,
    ah_div([ah_div(Letter, [<<"dash-app-icon">>], []),
            ah_div([ah_div([ah_p(Name, [<<"dash-app-name">>], []), PriceHtml],
                           [<<"dash-app-head">>], []),
                    ah_div([app_stat(download, Dl), ah_span(<<"·"/utf8>>, [<<"dash-app-dot">>], []),
                            app_stat(hard_drive, Size), ah_span(<<"·"/utf8>>, [<<"dash-app-dot">>], []),
                            app_stat(star, Rate)],
                           [<<"dash-app-stats">>], [])],
                   [<<"dash-app-body">>], []),
            tag('button', icon(more, 16), [<<"dash-icon-btn">>, <<"sm">>],
                  [{type, button}, {aria_label, <<"More">>}])],
           [<<"dash-app">>], [{data_color, Color}]).

app_stat(Ico, Text) ->
    ah_span([icon(Ico, 13), ah_span(Text, [], [])], [<<"dash-app-stat">>], []).

%%====================================================================
%% Top installed countries
%%====================================================================

countries_card() ->
    Countries = [{<<"🇩🇪"/utf8>>, <<"Germany"/utf8>>, <<"9.91k">>, <<"1.95k">>, <<"9.12k">>},
                 {<<"🇬🇧"/utf8>>, <<"England"/utf8>>, <<"1.95k">>, <<"9.12k">>, <<"6.98k">>},
                 {<<"🇫🇷"/utf8>>, <<"France"/utf8>>, <<"9.12k">>, <<"6.98k">>, <<"8.49k">>},
                 {<<"🇰🇷"/utf8>>, <<"Korea"/utf8>>, <<"6.98k">>, <<"8.49k">>, <<"2.03k">>},
                 {<<"🇺🇸"/utf8>>, <<"USA"/utf8>>, <<"8.49k">>, <<"2.03k">>, <<"3.36k">>}],
    Rows = [ah_div([ah_span(Flag, [<<"dash-country-flag">>], []),
                    ah_span(Name, [<<"dash-country-name">>], []),
                    ah_div([country_stat(smartphone, A), country_stat(laptop, W),
                            country_stat(apple, Pp)],
                           [<<"dash-country-stats">>], [])],
                   [<<"dash-country">>], [])
            || {Flag, Name, A, W, Pp} <- Countries],
    dash_card(<<"Top installed countries"/utf8>>, <<"Installs by platform"/utf8>>,
              <<"dash-country-list">>, Rows).

country_stat(Ico, Text) ->
    ah_span([icon(Ico, 13), ah_span(Text, [], [])], [<<"dash-country-stat">>], []).

%%====================================================================
%% News
%%====================================================================

news_card() ->
    News = [#{icon => sparkles, color => success, cat => <<"Release">>, new => true,
              title => <<"Real-time analytics dashboard now live">>,
              desc => <<"Track sessions, funnels, and revenue side-by-side without leaving the home view.">>,
              time => <<"2 h">>},
            #{icon => analytics, color => info, cat => <<"Report">>, new => true,
              title => <<"April performance summary is ready">>,
              desc => <<"Engagement up 18%, churn down to 2.4%. Detailed PDF in your inbox.">>,
              time => <<"5 h">>},
            #{icon => settings, color => warning, cat => <<"Maintenance">>, new => false,
              title => <<"Scheduled DB upgrade this Saturday">>,
              desc => <<"Brief 30-minute window starting 02:00 UTC. Read replicas keep traffic flowing.">>,
              time => <<"1 d">>},
            #{icon => components, color => secondary, cat => <<"API">>, new => false,
              title => <<"API v2 — webhooks + rate limits doubled">>,
              desc => <<"New endpoints for billing, search, and audit logs. Backward compatible.">>,
              time => <<"2 d">>},
            #{icon => calendar, color => info, cat => <<"Schedule">>, new => false,
              title => <<"Holiday support hours next week">>,
              desc => <<"Limited live coverage May 1–3; on-call still rotating per usual.">>,
              time => <<"3 d">>}],
    dash_card(<<"News"/utf8>>, <<"5 latest updates"/utf8>>, <<"dash-news-list">>,
              [news_row(N) || N <- News]).

news_row(#{icon := Ico, color := Color, cat := Cat, new := New, title := Title,
           desc := Desc, time := Time}) ->
    ah_div([ah_div(icon(Ico, 18), [<<"dash-news-avatar">>], []),
            ah_div([ah_div([ah_p(Title, [<<"dash-news-title">>], []),
                            [ah_span(<<"NEW">>, [<<"dash-news-new">>], []) || New]],
                           [<<"dash-news-head">>], []),
                    ah_p(Desc, [<<"dash-news-desc">>], []),
                    ah_div([ah_span(Cat, [<<"dash-news-cat">>], []),
                            ah_span(Time, [], [])],
                           [<<"dash-news-meta">>], [])],
                   [<<"dash-news-body">>], [])],
           [<<"dash-news">>], [{data_color, Color}]).

%%====================================================================
%% Top authors
%%====================================================================

authors_card() ->
    Authors = [#{name => <<"Jayvion Simon">>, hearts => <<"9.91k">>, initial => <<"J">>,
                 color => success, trophy => warning},
               #{name => <<"Deja Brady">>, hearts => <<"9.12k">>, initial => <<"D">>,
                 color => info, trophy => info},
               #{name => <<"Lucian Obrien">>, hearts => <<"1.95k">>, initial => <<"L">>,
                 color => warning, trophy => error}],
    Rows = [ah_div([ah_div(Initial, [<<"dash-author-avatar">>], [{data_color, Color}]),
                    ah_div([ah_p(Name, [<<"dash-author-name">>], []),
                            ah_span([icon(heart, 14), ah_span(Hearts, [], [])],
                                    [<<"dash-author-hearts">>], [])],
                           [<<"dash-author-meta">>], []),
                    ah_div(icon(trophy, 16), [<<"dash-author-trophy">>],
                           [{data_color, Trophy}, {aria_label, [<<"Rank #">>, integer_to_binary(I)]}])],
                   [<<"dash-author">>], [])
            || {I, #{name := Name, hearts := Hearts, initial := Initial,
                     color := Color, trophy := Trophy}} <- lists:enumerate(1, Authors)],
    dash_card(<<"Top authors"/utf8>>, <<"By total reactions"/utf8>>,
              <<"dash-author-list">>, Rows).

%%====================================================================
%% Inbox
%%====================================================================

inbox_card() ->
    Inbox = [#{initial => <<"L">>, name => <<"Lara Quinn">>, color => success,
               time => <<"12 min">>, online => true,
               subject => <<"Re: Q3 launch checklist">>,
               preview => <<"Thanks for the update — let's lock the dates by Friday.">>},
             #{initial => <<"M">>, name => <<"Marco Diaz">>, color => info,
               time => <<"1 h">>, online => true,
               subject => <<"Design review for billing page">>,
               preview => <<"Pushed v3 of the layout. Curious to hear your thoughts on the empty state.">>},
             #{initial => <<"S">>, name => <<"Sienna Ohira">>, color => success,
               time => <<"3 h">>, online => false,
               subject => <<"Customer feedback summary">>,
               preview => <<"Logged 14 new requests this week — top theme is dark mode for billing.">>},
             #{initial => <<"R">>, name => <<"Roman Schultz">>, color => warning,
               time => <<"Yesterday">>, online => false,
               subject => <<"DB migration window">>,
               preview => <<"Ops scheduled the index rebuild for Saturday 02:00 UTC, 30 min downtime.">>},
             #{initial => <<"P">>, name => <<"Priya Banerjee">>, color => secondary,
               time => <<"2 d">>, online => false,
               subject => <<"Welcome to the team!">>,
               preview => <<"Joined yesterday — happy to be working with you all.">>}],
    dash_card(<<"Inbox"/utf8>>, <<"5 latest messages"/utf8>>, <<"dash-inbox-list">>,
              [inbox_row(M) || M <- Inbox]).

inbox_row(#{initial := Initial, name := Name, color := Color, time := Time,
            online := Online, subject := Subject, preview := Preview}) ->
    ah_div([ah_div([ah_span(Initial, [], []),
                    [ah_span([], [<<"dash-inbox-online">>], []) || Online]],
                   [<<"dash-inbox-avatar">>], [{data_color, Color}]),
            ah_div([ah_div([ah_p(Name, [<<"dash-inbox-name">>], []),
                            ah_span(Time, [<<"dash-inbox-time">>], [])],
                           [<<"dash-inbox-head">>], []),
                    ah_p(Subject, [<<"dash-inbox-subject">>], []),
                    ah_p(Preview, [<<"dash-inbox-preview">>], [])],
                   [<<"dash-inbox-body">>], [])],
           [<<"dash-inbox">>], [{data_color, Color}]).

%%====================================================================
%% Tasks (checkable)
%%====================================================================

tasks_card() ->
    Tasks = [#{title => <<"Prepare monthly financial report">>, priority => high,
               due => <<"Today">>, done => true},
             #{title => <<"Design Q3 marketing campaign">>, priority => high,
               due => <<"Tomorrow">>, done => false},
             #{title => <<"Analyze customer feedback survey">>, priority => medium,
               due => <<"Apr 18">>, done => false},
             #{title => <<"Update website hero copy">>, priority => low,
               due => <<"Apr 22">>, done => false},
             #{title => <<"Conduct competitor market research">>, priority => medium,
               due => <<"Apr 25">>, done => false}],
    dash_card(<<"Tasks"/utf8>>, <<"1 of 5 completed"/utf8>>, <<"dash-task-list">>,
              [task(T) || T <- Tasks]).

task(#{title := Title, priority := Priority, due := Due, done := Done}) ->
    Urgent = lists:member(Due, [<<"Today">>, <<"Tomorrow">>]),
    ah_div([ah_span([icon(check, 12, 3) || Done], [<<"dash-task-check">>],
                    [{data_checked, bool_attr(Done)}]),
            ah_span([], [<<"dash-task-pri">>], [{data_priority, Priority}]),
            ah_span(Title, [<<"dash-task-label">>], []),
            ah_span(Due, [<<"dash-task-due">>], [{data_urgent, bool_attr(Urgent)}])],
           [<<"dash-task">>], [{data_done, bool_attr(Done)}]).

%%====================================================================
%% Recent transactions
%%====================================================================

transactions_card() ->
    Txs = [#{id => <<"INV-3412">>, customer => <<"Aimee Chen">>, amount => <<"$1,250.00">>,
             status => <<"Paid">>, color => success},
           #{id => <<"INV-3411">>, customer => <<"Bruno Marais">>, amount => <<"$748.20">>,
             status => <<"Pending">>, color => warning},
           #{id => <<"INV-3410">>, customer => <<"Camille Hong">>, amount => <<"$2,980.00">>,
             status => <<"Paid">>, color => success},
           #{id => <<"INV-3409">>, customer => <<"Dimitri Volkov">>, amount => <<"$316.50">>,
             status => <<"Refunded">>, color => error},
           #{id => <<"INV-3408">>, customer => <<"Emily Park">>, amount => <<"$890.75">>,
             status => <<"Paid">>, color => success}],
    Head = ah_thead(ah_tr([ah_th(<<"Invoice">>, [], []),
                           ah_th(<<"Customer">>, [], []),
                           ah_th(<<"Amount">>, [<<"dash-tx-right">>], []),
                           ah_th(<<"Status">>, [], [])])),
    Body = ah_tbody([ah_tr([ah_td(Id, [<<"dash-tx-id">>], []),
                            ah_td(Customer, [<<"dash-tx-customer">>], []),
                            ah_td(Amount, [<<"dash-tx-amount">>], []),
                            ah_td([ah_span(Status, [<<"dash-tx-status">>], [{data_color, Color}])],
                                  [], [])],
                           [<<"dash-tx-row">>], [])
                     || #{id := Id, customer := Customer, amount := Amount,
                          status := Status, color := Color} <- Txs]),
    Table = ah_table([Head, Body], [<<"dash-tx-table">>], []),
    dash_card(<<"Recent transactions"/utf8>>, <<"Last 5 invoices"/utf8>>, <<"dash-tx">>,
              [Table]).

%%====================================================================
%% Order timeline
%%====================================================================

timeline_card() ->
    Timeline = [#{id => <<"1248">>, status => <<"Completed">>, icon => check,
                  color => success, amount => <<"$1,250.00">>, time => <<"Mar 15 · 2:34 PM"/utf8>>},
                #{id => <<"1247">>, status => <<"Processing">>, icon => loader,
                  color => info, amount => <<"$748.20">>, time => <<"Mar 15 · 11:20 AM"/utf8>>},
                #{id => <<"1246">>, status => <<"Pending">>, icon => clock,
                  color => warning, amount => <<"$2,980.00">>, time => <<"Mar 14 · 5:45 PM"/utf8>>},
                #{id => <<"1245">>, status => <<"Shipped">>, icon => truck,
                  color => secondary, amount => <<"$316.50">>, time => <<"Mar 14 · 3:12 PM"/utf8>>},
                #{id => <<"1244">>, status => <<"Delivered">>, icon => package,
                  color => success, amount => <<"$890.75">>, time => <<"Mar 13 · 9:00 AM"/utf8>>}],
    dash_card(<<"Order timeline"/utf8>>, <<"Last 5 orders"/utf8>>, <<"dash-tl">>,
              [timeline_item(I, T) || {I, T} <- lists:enumerate(1, Timeline)]).

timeline_item(I, #{id := Id, status := Status, icon := Ico, color := Color,
                   amount := Amount, time := Time}) ->
    Last = I =:= 5,
    ah_div([ah_div([ah_div(icon(Ico, 12, 2), [<<"dash-tl-dot">>], []),
                    [ah_div([], [<<"dash-tl-line">>], []) || not Last]],
                   [<<"dash-tl-marker">>], []),
            ah_div([ah_div([ah_span([<<"Order #"/utf8>>, Id], [<<"dash-tl-id">>], []),
                            ah_span(Amount, [<<"dash-tl-amount">>], [])],
                           [<<"dash-tl-head">>], []),
                    ah_div([ah_span(Status, [<<"dash-tl-status">>], []),
                            ah_span(Time, [<<"dash-tl-time">>], [])],
                           [<<"dash-tl-meta">>], [])],
                   [<<"dash-tl-body">>], [])],
           [<<"dash-tl-item">>], [{data_color, Color}]).

%%====================================================================
%% Top products
%%====================================================================

products_card() ->
    Products = [#{rank => 1, name => <<"Atlas Foldable Tablet">>, sales => 1284,
                  delta => 9.2, color => success},
                #{rank => 2, name => <<"Nimbus Pro Headphones">>, sales => 1042,
                  delta => 4.6, color => info},
                #{rank => 3, name => <<"Lumen Smart Lamp">>, sales => 879,
                  delta => 2.1, color => warning},
                #{rank => 4, name => <<"Vega Mechanical KB">>, sales => 712,
                  delta => -1.4, color => error},
                #{rank => 5, name => <<"Orbit Wireless Mouse">>, sales => 638,
                  delta => 0.8, color => secondary}],
    dash_card(<<"Top products"/utf8>>, <<"Best sellers this month"/utf8>>,
              <<"dash-prod-list">>,
              [product(P) || P <- Products]).

product(#{rank := Rank, name := Name, sales := Sales, delta := Delta, color := Color}) ->
    Tier = if Rank =< 3 -> top; true -> rest end,
    ah_div([ah_div([<<"#"/utf8>>, integer_to_binary(Rank)], [<<"dash-prod-rank">>],
                   [{data_color, Color}, {data_tier, Tier}]),
            ah_div([ah_p(Name, [<<"dash-prod-name">>], []),
                    ah_p([integer_to_binary(Sales), <<" sold"/utf8>>], [<<"dash-prod-meta">>], [])],
                   [<<"dash-prod-body">>], []),
            trend_pill(Delta)],
           [<<"dash-prod">>], []).

%%====================================================================
%% Traffic by site
%%====================================================================

traffic_card() ->
    Traffic = [#{value => <<"9.12k">>, label => <<"Google">>, color => error,
                 icon => globe, delta => 18.2},
               #{value => <<"8.49k">>, label => <<"Twitter">>, color => info,
                 icon => message, delta => 5.6},
               #{value => <<"6.98k">>, label => <<"LinkedIn"/utf8>>, color => secondary,
                 icon => users, delta => -2.4},
               #{value => <<"1.95k">>, label => <<"Facebook">>, color => warning,
                 icon => flag, delta => 12.8}],
    dash_card(<<"Traffic by site"/utf8>>, <<"Visits last 7 days"/utf8>>,
              <<"dash-traffic-grid">>,
              [traffic_cell(T) || T <- Traffic]).

traffic_cell(#{value := Value, label := Label, color := Color, icon := Ico, delta := Delta}) ->
    ah_div([ah_div([ah_div(icon(Ico, 20), [<<"dash-traffic-icon">>], []),
                    trend_pill_sm(Delta)],
                   [<<"dash-traffic-head">>], []),
            ah_div(Label, [<<"dash-traffic-label">>], []),
            ah_div(Value, [<<"dash-traffic-value">>], [])],
           [<<"dash-traffic">>], [{data_color, Color}]).

trend_pill_sm(Delta) ->
    Up = Delta >= 0,
    Sign = if Up -> <<"+">>; true -> <<>> end,
    ah_span([icon(if Up -> trending_up; true -> trending_down end, 12, 2),
             ah_span([Sign, float_bin(Delta), <<"%">>], [], [])],
            [<<"dash-trend-pill">>, <<"sm">>], [{data_up, bool_attr(Up)}]).

%%====================================================================
%% Conversion rates + sales by country
%%====================================================================

conversion_card() ->
    Convs = [{<<"Italy"/utf8>>, 44, success}, {<<"Japan"/utf8>>, 55, info},
             {<<"China"/utf8>>, 41, warning}, {<<"Canada"/utf8>>, 64, error},
             {<<"France"/utf8>>, 72, secondary}],
    Rows = [ah_div([ah_div([ah_span(Label, [<<"dash-conv-label">>], []),
                            ah_span([integer_to_binary(V), <<"%">>], [<<"dash-conv-value">>], [])],
                           [<<"dash-conv-row">>], []),
                    ah_div([ah_div([], [<<"dash-conv-fill">>], [])],
                           [<<"dash-conv-track">>], [{style, width_style(V)}])],
                   [<<"dash-conv">>], [{data_color, Color}])
            || {Label, V, Color} <- Convs],
    dash_card(<<"Conversion rates"/utf8>>, <<"(+43%) than last year"/utf8>>,
              <<"dash-conv-list">>, Rows).

sales_card() ->
    Sales = [{<<"🇺🇸"/utf8>>, <<"United States"/utf8>>, <<"$48.2k">>, 100, success},
             {<<"🇩🇪"/utf8>>, <<"Germany"/utf8>>, <<"$28.6k">>, 62, info},
             {<<"🇯🇵"/utf8>>, <<"Japan"/utf8>>, <<"$21.4k">>, 46, warning},
             {<<"🇧🇷"/utf8>>, <<"Brazil"/utf8>>, <<"$15.7k">>, 34, secondary},
             {<<"🇮🇳"/utf8>>, <<"India"/utf8>>, <<"$11.8k">>, 26, error},
             {<<"🇫🇷"/utf8>>, <<"France"/utf8>>, <<"$4.1k">>, 10, success}],
    Rows = [ah_div([ah_div([ah_span(Flag, [<<"dash-sales-flag">>], []),
                            ah_span(Name, [<<"dash-sales-name">>], []),
                            ah_span(Value, [<<"dash-sales-value">>], [])],
                           [<<"dash-sales-head">>], []),
                    ah_div([ah_div([], [<<"dash-sales-fill">>], [])],
                           [<<"dash-sales-track">>], [{style, width_style(Pct)}])],
                   [<<"dash-sales">>], [{data_color, Color}])
            || {Flag, Name, Value, Pct, Color} <- Sales],
    dash_card(<<"Sales by country"/utf8>>, <<"Top regions this month"/utf8>>,
              <<"dash-sales-list">>, Rows).

%%====================================================================
%% Summary banners (radial gauges)
%%====================================================================

summary() ->
    Cards = [#{label => <<"Conversion">>, total => <<"38,566">>, pct => 48,
               delta => <<"12.0%">>, tone => green, icon => trending_up},
             #{label => <<"Applications">>, total => <<"55,566">>, pct => 75,
               delta => <<"8.4%">>, tone => blue, icon => app_window}],
    ah_div([summary_card(C) || C <- Cards], [<<"dash-summary-grid">>], []).

summary_card(#{label := Label, total := Total, pct := Pct, delta := Delta,
               tone := Tone, icon := Ico}) ->
    ah_div([ah_span([], [<<"dash-summary-glow">>], []),
            ah_div([gauge_svg(Tone, Pct),
                    ah_span([integer_to_binary(Pct), <<"%">>], [<<"dash-summary-ring-val">>], [])],
                   [<<"dash-summary-ring">>], []),
            ah_div([ah_p(Label, [<<"dash-summary-label">>], []),
                    ah_div(Total, [<<"dash-summary-total">>], []),
                    ah_div([ah_span([icon(trending_up, 12, 2), ah_span([<<"+"/utf8>>, Delta], [], [])],
                                    [<<"dash-summary-delta">>], []),
                            ah_span(<<"vs last month"/utf8>>, [<<"dash-summary-since">>], [])],
                           [<<"dash-summary-trend">>], [])],
                   [<<"dash-summary-body">>], []),
            ah_span(icon(Ico, 72, 1), [<<"dash-summary-illus">>], [])],
           [<<"dash-summary">>], [{data_tone, Tone}]).

gauge_svg(Tone, Pct) ->
    R = 32,
    C = 2 * math:pi() * R,
    Offset = C * (1 - Pct / 100),
    Gid = <<"dash-gauge-glow-", (atom_to_binary(Tone))/binary>>,
    {safe, iolist_to_binary(
             ["<svg class=\"dash-summary-gauge\" viewBox=\"0 0 80 80\" aria-hidden=\"true\">",
              "<defs><filter id=\"", Gid, "\" x=\"-50%\" y=\"-50%\" width=\"200%\" height=\"200%\">",
              "<feDropShadow dx=\"0\" dy=\"0\" stdDeviation=\"3\"",
              " flood-color=\"var(--dash-gauge-accent)\" flood-opacity=\"0.7\"/>",
              "</filter></defs>",
              "<circle class=\"dash-summary-gauge-track\" cx=\"40\" cy=\"40\" r=\"", fmt(R),
              "\" fill=\"none\"/>",
              "<circle class=\"dash-summary-gauge-fill\" cx=\"40\" cy=\"40\" r=\"", fmt(R),
              "\" fill=\"none\" stroke-dasharray=\"", fmt(C), "\" stroke-dashoffset=\"", fmt(Offset),
              "\" transform=\"rotate(-90 40 40)\" filter=\"url(#", Gid, ")\"/>",
              "</svg>"]) }.

%%====================================================================
%% Icons (lucide-style inline SVG, ported from sigil's app-shell)
%%====================================================================

float_bin(N) when is_integer(N) -> integer_to_binary(N);
float_bin(N) when is_float(N) -> iolist_to_binary(io_lib:format("~.1f", [N])).

%% aihtml renders Erlang booleans as empty HTML attributes. Sigil's
%% dashboard CSS and JS expect explicit "true"/"false" state values.
bool_attr(true) -> <<"true">>;
bool_attr(false) -> <<"false">>.

money_bin(N) when is_float(N) -> iolist_to_binary(io_lib:format("~.2f", [N])).

fmt(N) when is_integer(N) -> integer_to_binary(N);
fmt(N) when is_float(N) -> iolist_to_binary(io_lib:format("~.2f", [N])).

%% @doc An inline SVG icon, `Size' in px, `Sw' the stroke width.
-spec icon(atom()) -> aihtml:html().
icon(Name) -> icon(Name, 20, 1.8).

-spec icon(atom(), pos_integer()) -> aihtml:html().
icon(Name, Size) -> icon(Name, Size, 1.8).

-spec icon(atom(), pos_integer(), number()) -> aihtml:html().
icon(Name, Size, Sw) ->
    {safe, iolist_to_binary(
             ["<svg class=\"dash-icon\" width=\"", integer_to_binary(Size),
              "\" height=\"", integer_to_binary(Size),
              "\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"",
              float_bin(Sw), "\" stroke-linecap=\"round\" stroke-linejoin=\"round\"",
              " aria-hidden=\"true\">", path(Name), "</svg>"]) }.

%% @doc The SVG body of an icon (lucide paths, ported from sigil).
-spec path(atom()) -> iodata().
path(dashboard) ->
    "<rect width=\"7\" height=\"9\" x=\"3\" y=\"3\" rx=\"1\"/><rect width=\"7\" height=\"5\" x=\"14\" y=\"3\" rx=\"1\"/><rect width=\"7\" height=\"9\" x=\"14\" y=\"12\" rx=\"1\"/><rect width=\"7\" height=\"5\" x=\"3\" y=\"16\" rx=\"1\"/>";
path(home) ->
    "<path d=\"m3 9 9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z\"/><polyline points=\"9 22 9 12 15 12 15 22\"/>";
path(layout_grid) ->
    "<rect width=\"7\" height=\"7\" x=\"3\" y=\"3\" rx=\"1\"/><rect width=\"7\" height=\"7\" x=\"14\" y=\"3\" rx=\"1\"/><rect width=\"7\" height=\"7\" x=\"14\" y=\"14\" rx=\"1\"/><rect width=\"7\" height=\"7\" x=\"3\" y=\"14\" rx=\"1\"/>";
path(play) ->
    "<polygon points=\"6 3 20 12 6 21 6 3\"/>";
path(analytics) ->
    "<path d=\"M3 3v18h18\"/><path d=\"M18 17V9\"/><path d=\"M13 17V5\"/><path d=\"M8 17v-3\"/>";
path(banking) ->
    "<line x1=\"3\" x2=\"21\" y1=\"22\" y2=\"22\"/><line x1=\"6\" x2=\"6\" y1=\"18\" y2=\"11\"/><line x1=\"10\" x2=\"10\" y1=\"18\" y2=\"11\"/><line x1=\"14\" x2=\"14\" y1=\"18\" y2=\"11\"/><line x1=\"18\" x2=\"18\" y1=\"18\" y2=\"11\"/><polygon points=\"12 2 20 7 4 7\"/>";
path(booking) ->
    "<path d=\"M17.8 19.2 16 11l3.5-3.5C21 6 21.5 4 21 3c-1-.5-3 0-4.5 1.5L13 8 4.8 6.2c-.5-.1-.9.1-1.1.5l-.3.5c-.2.5-.1 1 .3 1.3L9 12l-2 3H4l-1 1 3 2 2 3 1-1v-3l3-2 3.5 5.3c.3.4.8.5 1.3.3l.5-.2c.4-.3.6-.7.5-1.2z\"/>";
path(file) ->
    "<path d=\"M20 20a2 2 0 0 0 2-2V8a2 2 0 0 0-2-2h-7.9a2 2 0 0 1-1.69-.9L9.6 3.9A2 2 0 0 0 7.93 3H4a2 2 0 0 0-2 2v13a2 2 0 0 0 2 2Z\"/>";
path(course) ->
    "<path d=\"M22 10v6M2 10l10-5 10 5-10 5z\"/><path d=\"M6 12v5c3 3 9 3 12 0v-5\"/>";
path(ecommerce) ->
    "<path d=\"M6 2 3 6v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2V6l-3-4Z\"/><path d=\"M3 6h18\"/><path d=\"M16 10a4 4 0 0 1-8 0\"/>";
path(kanban) ->
    "<path d=\"M6 5v11M12 5v6M18 5v14\"/>";
path(chat) ->
    "<path d=\"M7.9 20A9 9 0 1 0 4 16.1L2 22Z\"/>";
path(mail) ->
    "<rect width=\"20\" height=\"16\" x=\"2\" y=\"4\" rx=\"2\"/><path d=\"m22 7-8.97 5.7a1.94 1.94 0 0 1-2.06 0L2 7\"/>";
path(folder) ->
    "<path d=\"M20 20a2 2 0 0 0 2-2V8a2 2 0 0 0-2-2h-7.9a2 2 0 0 1-1.69-.9L9.6 3.9A2 2 0 0 0 7.93 3H4a2 2 0 0 0-2 2v13a2 2 0 0 0 2 2Z\"/>";
path(product) ->
    "<path d=\"M11 21.73a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73z\"/><path d=\"m3.3 7 8.7 5 8.7-5\"/><path d=\"M12 22V12\"/>";
path(order) ->
    "<path d=\"M16 4h2a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2\"/><rect width=\"8\" height=\"4\" x=\"8\" y=\"2\" rx=\"1\"/><path d=\"m9 14 2 2 4-4\"/>";
path(invoice) ->
    "<path d=\"M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z\"/><path d=\"M14 2v6h6\"/><path d=\"M16 13H8M16 17H8M10 9H8\"/>";
path(blog) ->
    "<path d=\"M4 22h16a2 2 0 0 0 2-2V4a2 2 0 0 0-2-2H8a2 2 0 0 0-2 2v16a2 2 0 0 1-2 2Zm0 0a2 2 0 0 1-2-2v-9c0-1.1.9-2 2-2h2\"/><path d=\"M18 14h-8M15 18h-5M10 6h8v4h-8z\"/>";
path(job) ->
    "<rect width=\"20\" height=\"14\" x=\"2\" y=\"6\" rx=\"2\"/><path d=\"M16 6V4a2 2 0 0 0-2-2h-4a2 2 0 0 0-2 2v2\"/>";
path(tour) ->
    "<path d=\"M12 2a8 8 0 0 0-8 8c0 5.4 7 11.5 7.35 11.76a1 1 0 0 0 1.3 0C13 21.5 20 15.4 20 10a8 8 0 0 0-8-8z\"/><circle cx=\"12\" cy=\"10\" r=\"3\"/>";
path(users) ->
    "<path d=\"M16 21v-2a4 4 0 0 0-4-4H6a4 4 0 0 0-4 4v2\"/><circle cx=\"9\" cy=\"7\" r=\"4\"/><path d=\"M22 21v-2a4 4 0 0 0-3-3.87\"/><path d=\"M16 3.13a4 4 0 0 1 0 7.75\"/>";
path(components) ->
    "<path d=\"M5.5 8.5 9 12l-3.5 3.5L2 12z\"/><path d=\"m12 2 3.5 3.5L12 9 8.5 5.5z\"/><path d=\"M18.5 8.5 22 12l-3.5 3.5L15 12z\"/><path d=\"m12 15 3.5 3.5L12 22l-3.5-3.5z\"/>";
path(credit_card) ->
    "<rect width=\"20\" height=\"14\" x=\"2\" y=\"5\" rx=\"2\"/><line x1=\"2\" x2=\"22\" y1=\"10\" y2=\"10\"/>";
path(globe) ->
    "<circle cx=\"12\" cy=\"12\" r=\"10\"/><path d=\"M12 2a14.5 14.5 0 0 0 0 20 14.5 14.5 0 0 0 0-20\"/><path d=\"M2 12h20\"/>";
path(user) ->
    "<circle cx=\"12\" cy=\"8\" r=\"5\"/><path d=\"M20 21a8 8 0 0 0-16 0\"/>";
path(settings) ->
    "<circle cx=\"12\" cy=\"12\" r=\"3\"/><path d=\"M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 1 1-2.83 2.83l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-4 0v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 1 1-2.83-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1 0-4h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 1 1 2.83-2.83l.06.06a1.65 1.65 0 0 0 1.82.33H9a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 4 0v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 1 1 2.83 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 0 4h-.09a1.65 1.65 0 0 0-1.51 1z\"/>";
path(menu) ->
    "<line x1=\"3\" x2=\"21\" y1=\"6\" y2=\"6\"/><line x1=\"3\" x2=\"21\" y1=\"12\" y2=\"12\"/><line x1=\"3\" x2=\"21\" y1=\"18\" y2=\"18\"/>";
path(search) ->
    "<circle cx=\"11\" cy=\"11\" r=\"8\"/><path d=\"m21 21-4.3-4.3\"/>";
path(bell) ->
    "<path d=\"M6 8a6 6 0 0 1 12 0c0 7 3 9 3 9H3s3-2 3-9\"/><path d=\"M10.3 21a1.94 1.94 0 0 0 3.4 0\"/>";
path(sparkles) ->
    "<path d=\"m12 3-1.9 5.8a2 2 0 0 1-1.3 1.3L3 12l5.8 1.9a2 2 0 0 1 1.3 1.3L12 21l1.9-5.8a2 2 0 0 1 1.3-1.3L21 12l-5.8-1.9a2 2 0 0 1-1.3-1.3z\"/>";
path(reset) ->
    "<path d=\"M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8\"/><path d=\"M3 3v5h5\"/>";
path(chevron_down) ->
    "<path d=\"m6 9 6 6 6-6\"/>";
path(package) ->
    "<path d=\"m16 16 2 2 4-4\"/><path d=\"M21 10V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l2-1.14\"/><path d=\"M3.29 7 12 12l8.71-5\"/><path d=\"M12 22V12\"/>";
path(download) ->
    "<path d=\"M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4\"/><polyline points=\"7 10 12 15 17 10\"/><line x1=\"12\" x2=\"12\" y1=\"15\" y2=\"3\"/>";
path(message) ->
    "<path d=\"M21 15a2 2 0 0 1-2 2H7l-4 4V5a2 2 0 0 1 2-2h14a2 2 0 0 1 2 2z\"/>";
path(trending_up) ->
    "<polyline points=\"22 7 13.5 15.5 8.5 10.5 2 17\"/><polyline points=\"16 7 22 7 22 13\"/>";
path(trending_down) ->
    "<polyline points=\"22 17 13.5 8.5 8.5 13.5 2 7\"/><polyline points=\"16 17 22 17 22 11\"/>";
path(grip) ->
    "<circle cx=\"9\" cy=\"6\" r=\"1\"/><circle cx=\"9\" cy=\"12\" r=\"1\"/><circle cx=\"9\" cy=\"18\" r=\"1\"/><circle cx=\"15\" cy=\"6\" r=\"1\"/><circle cx=\"15\" cy=\"12\" r=\"1\"/><circle cx=\"15\" cy=\"18\" r=\"1\"/>";
path(more) ->
    "<circle cx=\"12\" cy=\"12\" r=\"1\"/><circle cx=\"12\" cy=\"5\" r=\"1\"/><circle cx=\"12\" cy=\"19\" r=\"1\"/>";
path(check) ->
    "<polyline points=\"20 6 9 17 4 12\"/>";
path(loader) ->
    "<path d=\"M21 12a9 9 0 1 1-6.219-8.56\"/>";
path(clock) ->
    "<circle cx=\"12\" cy=\"12\" r=\"10\"/><polyline points=\"12 6 12 12 16 14\"/>";
path(truck) ->
    "<path d=\"M14 18V6a2 2 0 0 0-2-2H4a2 2 0 0 0-2 2v11a1 1 0 0 0 1 1h2\"/><path d=\"M14 9h4l4 4v4a1 1 0 0 1-1 1h-1\"/><circle cx=\"7\" cy=\"18\" r=\"2\"/><circle cx=\"17\" cy=\"18\" r=\"2\"/>";
path(flag) ->
    "<path d=\"M4 15s1-1 4-1 5 2 8 2 4-1 4-1V3s-1 1-4 1-5-2-8-2-4 1-4 1z\"/><line x1=\"4\" x2=\"4\" y1=\"22\" y2=\"15\"/>";
path(hard_drive) ->
    "<line x1=\"22\" x2=\"2\" y1=\"12\" y2=\"12\"/><path d=\"M5.45 5.11 2 12v6a2 2 0 0 0 2 2h16a2 2 0 0 0 2-2v-6l-3.45-6.89A2 2 0 0 0 16.76 4H7.24a2 2 0 0 0-1.79 1.11z\"/><line x1=\"6\" x2=\"6.01\" y1=\"16\" y2=\"16\"/><line x1=\"10\" x2=\"10.01\" y1=\"16\" y2=\"16\"/>";
path(star) ->
    "<polygon points=\"12 2 15.09 8.26 22 9.27 17 14.14 18.18 21.02 12 17.77 5.82 21.02 7 14.14 2 9.27 8.91 8.26 12 2\"/>";
path(heart) ->
    "<path d=\"M19 14c1.49-1.46 3-3.21 3-5.5A5.5 5.5 0 0 0 16.5 3c-1.76 0-3 .5-4.5 2-1.5-1.5-2.74-2-4.5-2A5.5 5.5 0 0 0 2 8.5c0 2.3 1.5 4.05 3 5.5l7 7Z\"/>";
path(trophy) ->
    "<path d=\"M6 9H4.5a2.5 2.5 0 0 1 0-5H6\"/><path d=\"M18 9h1.5a2.5 2.5 0 0 0 0-5H18\"/><path d=\"M4 22h16\"/><path d=\"M10 14.66V17c0 .55-.47.98-.97 1.21C7.85 18.75 7 20.24 7 22\"/><path d=\"M14 14.66V17c0 .55.47.98.97 1.21C16.15 18.75 17 20.24 17 22\"/><path d=\"M18 2H6v7a6 6 0 0 0 12 0V2Z\"/>";
path(smartphone) ->
    "<rect width=\"14\" height=\"20\" x=\"5\" y=\"2\" rx=\"2\" ry=\"2\"/><path d=\"M12 18h.01\"/>";
path(laptop) ->
    "<path d=\"M20 16V7a2 2 0 0 0-2-2H6a2 2 0 0 0-2 2v9m16 0H4m16 0 1.28 2.55a1 1 0 0 1-.9 1.45H3.62a1 1 0 0 1-.9-1.45L4 16\"/>";
path(apple) ->
    "<path d=\"M12 20.94c1.5 0 2.75 1.06 4 1.06 3 0 6-8 6-12.22A4.91 4.91 0 0 0 17 5c-2.22 0-4 1.44-5 2-1-.56-2.78-2-5-2a4.9 4.9 0 0 0-5 4.78C2 14 5 22 8 22c1.25 0 2.5-1.06 4-1.06z\"/><path d=\"M10 2c1 .5 2 2 2 5\"/>";
path(app_window) ->
    "<rect x=\"2\" y=\"4\" width=\"20\" height=\"16\" rx=\"2\"/><path d=\"M10 4v4\"/><path d=\"M2 8h20\"/><path d=\"M6 4v4\"/>";
path(calendar) ->
    "<rect width=\"18\" height=\"18\" x=\"3\" y=\"4\" rx=\"2\"/><path d=\"M3 10h18M8 2v4M16 2v4\"/>";
path(_) ->
    "".
