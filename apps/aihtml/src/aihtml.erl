%%%-------------------------------------------------------------------
%%% @doc aihtml: write HTML pages as Erlang function calls.
%%%
%%% ```
%%% -include_lib("aihtml/include/aihtml.hrl").   % imports this module
%%%
%%% page() ->
%%%     'div'([checkbox(<<"Remember me">>, yes, [], [{name, remember}]),
%%%            button(<<"Save">>, save, [primary, <<"mt-4">>], [{type, submit}])],
%%%           [<<"flex flex-col gap-2">>], [{id, login}]).
%%% '''
%%%
%%% `div' is an Erlang operator, so that one tag is written `'div''.
%%%
%%% Generic tags take `(Children)' or `(Children, Css, Attrs)'. Void tags
%%% take `(Css, Attrs)'. Prefabs are listed in `aihtml_catalog:prefabs/0'.
%%% Everything returns an element; `render/1' turns it into iodata.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml).

%% Tags with children, /1 and /3.
-export(['div'/1, 'div'/3, span/1, span/3, p/1, p/3, a/1, a/3,
         h1/1, h1/3, h2/1, h2/3, h3/1, h3/3, h4/1, h4/3,
         ul/1, ul/3, ol/1, ol/3, li/1, li/3, dl/1, dl/3, dt/1, dt/3, dd/1, dd/3,
         section/1, section/3, article/1, article/3, aside/1, aside/3,
         header/1, header/3, footer/1, footer/3, nav/1, nav/3, main/1, main/3,
         form/1, form/3, fieldset/1, fieldset/3, legend/1, legend/3, label/1, label/3,
         strong/1, strong/3, em/1, em/3, small/1, small/3, code/1, code/3,
         pre/1, pre/3, blockquote/1, blockquote/3,
         table/1, table/3, thead/1, thead/3, tbody/1, tbody/3,
         tr/1, tr/3, th/1, th/3, td/1, td/3]).
%% Void tags.
-export([br/0, hr/2, img/2]).
%% Escape hatches and rendering.
-export([el/4, void/3, text/1, safe/1, render/1, render_binary/1, page/2]).
%% Server round trips for the jQuery runtime.
-export([fetch/3, fetch/4]).
%% Browser events that call Erlang actions (see aihtml_action).
-export([on/2, on/3]).
%% Server push (see aihtml_push).
-export([subscribe/1, subscribe/2]).
%% Prefabs.
-export([button/4, checkbox/4, radio/4, switch/4,
         input/3, textarea/3, select/4, field/4,
         card/3, alert/3, badge/3, tabs/4, theme_switcher/2]).

-export_type([html/0, element/0, css/0, attrs/0]).

-type html() :: aihtml_html:html().
-type element() :: aihtml_html:element().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%% One /1 and one /3 function per tag (a macro cannot define functions).
-spec 'div'(html()) -> element().
'div'(C) -> aihtml_html:el('div', C, [], []).
-spec 'div'(html(), css(), attrs()) -> element().
'div'(C, Css, A) -> aihtml_html:el('div', C, Css, A).

-spec span(html()) -> element().
span(C) -> aihtml_html:el(span, C, [], []).
-spec span(html(), css(), attrs()) -> element().
span(C, Css, A) -> aihtml_html:el(span, C, Css, A).

-spec p(html()) -> element().
p(C) -> aihtml_html:el(p, C, [], []).
-spec p(html(), css(), attrs()) -> element().
p(C, Css, A) -> aihtml_html:el(p, C, Css, A).

-spec a(html()) -> element().
a(C) -> aihtml_html:el(a, C, [], []).
-spec a(html(), css(), attrs()) -> element().
a(C, Css, A) -> aihtml_html:el(a, C, Css, A).

-spec h1(html()) -> element().
h1(C) -> aihtml_html:el(h1, C, [], []).
-spec h1(html(), css(), attrs()) -> element().
h1(C, Css, A) -> aihtml_html:el(h1, C, Css, A).

-spec h2(html()) -> element().
h2(C) -> aihtml_html:el(h2, C, [], []).
-spec h2(html(), css(), attrs()) -> element().
h2(C, Css, A) -> aihtml_html:el(h2, C, Css, A).

-spec h3(html()) -> element().
h3(C) -> aihtml_html:el(h3, C, [], []).
-spec h3(html(), css(), attrs()) -> element().
h3(C, Css, A) -> aihtml_html:el(h3, C, Css, A).

-spec h4(html()) -> element().
h4(C) -> aihtml_html:el(h4, C, [], []).
-spec h4(html(), css(), attrs()) -> element().
h4(C, Css, A) -> aihtml_html:el(h4, C, Css, A).

-spec ul(html()) -> element().
ul(C) -> aihtml_html:el(ul, C, [], []).
-spec ul(html(), css(), attrs()) -> element().
ul(C, Css, A) -> aihtml_html:el(ul, C, Css, A).

-spec ol(html()) -> element().
ol(C) -> aihtml_html:el(ol, C, [], []).
-spec ol(html(), css(), attrs()) -> element().
ol(C, Css, A) -> aihtml_html:el(ol, C, Css, A).

-spec li(html()) -> element().
li(C) -> aihtml_html:el(li, C, [], []).
-spec li(html(), css(), attrs()) -> element().
li(C, Css, A) -> aihtml_html:el(li, C, Css, A).

-spec dl(html()) -> element().
dl(C) -> aihtml_html:el(dl, C, [], []).
-spec dl(html(), css(), attrs()) -> element().
dl(C, Css, A) -> aihtml_html:el(dl, C, Css, A).

-spec dt(html()) -> element().
dt(C) -> aihtml_html:el(dt, C, [], []).
-spec dt(html(), css(), attrs()) -> element().
dt(C, Css, A) -> aihtml_html:el(dt, C, Css, A).

-spec dd(html()) -> element().
dd(C) -> aihtml_html:el(dd, C, [], []).
-spec dd(html(), css(), attrs()) -> element().
dd(C, Css, A) -> aihtml_html:el(dd, C, Css, A).

-spec section(html()) -> element().
section(C) -> aihtml_html:el(section, C, [], []).
-spec section(html(), css(), attrs()) -> element().
section(C, Css, A) -> aihtml_html:el(section, C, Css, A).

-spec article(html()) -> element().
article(C) -> aihtml_html:el(article, C, [], []).
-spec article(html(), css(), attrs()) -> element().
article(C, Css, A) -> aihtml_html:el(article, C, Css, A).

-spec aside(html()) -> element().
aside(C) -> aihtml_html:el(aside, C, [], []).
-spec aside(html(), css(), attrs()) -> element().
aside(C, Css, A) -> aihtml_html:el(aside, C, Css, A).

-spec header(html()) -> element().
header(C) -> aihtml_html:el(header, C, [], []).
-spec header(html(), css(), attrs()) -> element().
header(C, Css, A) -> aihtml_html:el(header, C, Css, A).

-spec footer(html()) -> element().
footer(C) -> aihtml_html:el(footer, C, [], []).
-spec footer(html(), css(), attrs()) -> element().
footer(C, Css, A) -> aihtml_html:el(footer, C, Css, A).

-spec nav(html()) -> element().
nav(C) -> aihtml_html:el(nav, C, [], []).
-spec nav(html(), css(), attrs()) -> element().
nav(C, Css, A) -> aihtml_html:el(nav, C, Css, A).

-spec main(html()) -> element().
main(C) -> aihtml_html:el(main, C, [], []).
-spec main(html(), css(), attrs()) -> element().
main(C, Css, A) -> aihtml_html:el(main, C, Css, A).

-spec form(html()) -> element().
form(C) -> aihtml_html:el(form, C, [], []).
-spec form(html(), css(), attrs()) -> element().
form(C, Css, A) -> aihtml_html:el(form, C, Css, A).

-spec fieldset(html()) -> element().
fieldset(C) -> aihtml_html:el(fieldset, C, [], []).
-spec fieldset(html(), css(), attrs()) -> element().
fieldset(C, Css, A) -> aihtml_html:el(fieldset, C, Css, A).

-spec legend(html()) -> element().
legend(C) -> aihtml_html:el(legend, C, [], []).
-spec legend(html(), css(), attrs()) -> element().
legend(C, Css, A) -> aihtml_html:el(legend, C, Css, A).

-spec label(html()) -> element().
label(C) -> aihtml_html:el(label, C, [], []).
-spec label(html(), css(), attrs()) -> element().
label(C, Css, A) -> aihtml_html:el(label, C, Css, A).

-spec strong(html()) -> element().
strong(C) -> aihtml_html:el(strong, C, [], []).
-spec strong(html(), css(), attrs()) -> element().
strong(C, Css, A) -> aihtml_html:el(strong, C, Css, A).

-spec em(html()) -> element().
em(C) -> aihtml_html:el(em, C, [], []).
-spec em(html(), css(), attrs()) -> element().
em(C, Css, A) -> aihtml_html:el(em, C, Css, A).

-spec small(html()) -> element().
small(C) -> aihtml_html:el(small, C, [], []).
-spec small(html(), css(), attrs()) -> element().
small(C, Css, A) -> aihtml_html:el(small, C, Css, A).

-spec code(html()) -> element().
code(C) -> aihtml_html:el(code, C, [], []).
-spec code(html(), css(), attrs()) -> element().
code(C, Css, A) -> aihtml_html:el(code, C, Css, A).

-spec pre(html()) -> element().
pre(C) -> aihtml_html:el(pre, C, [], []).
-spec pre(html(), css(), attrs()) -> element().
pre(C, Css, A) -> aihtml_html:el(pre, C, Css, A).

-spec blockquote(html()) -> element().
blockquote(C) -> aihtml_html:el(blockquote, C, [], []).
-spec blockquote(html(), css(), attrs()) -> element().
blockquote(C, Css, A) -> aihtml_html:el(blockquote, C, Css, A).

-spec table(html()) -> element().
table(C) -> aihtml_html:el(table, C, [], []).
-spec table(html(), css(), attrs()) -> element().
table(C, Css, A) -> aihtml_html:el(table, C, Css, A).

-spec thead(html()) -> element().
thead(C) -> aihtml_html:el(thead, C, [], []).
-spec thead(html(), css(), attrs()) -> element().
thead(C, Css, A) -> aihtml_html:el(thead, C, Css, A).

-spec tbody(html()) -> element().
tbody(C) -> aihtml_html:el(tbody, C, [], []).
-spec tbody(html(), css(), attrs()) -> element().
tbody(C, Css, A) -> aihtml_html:el(tbody, C, Css, A).

-spec tr(html()) -> element().
tr(C) -> aihtml_html:el(tr, C, [], []).
-spec tr(html(), css(), attrs()) -> element().
tr(C, Css, A) -> aihtml_html:el(tr, C, Css, A).

-spec th(html()) -> element().
th(C) -> aihtml_html:el(th, C, [], []).
-spec th(html(), css(), attrs()) -> element().
th(C, Css, A) -> aihtml_html:el(th, C, Css, A).

-spec td(html()) -> element().
td(C) -> aihtml_html:el(td, C, [], []).
-spec td(html(), css(), attrs()) -> element().
td(C, Css, A) -> aihtml_html:el(td, C, Css, A).

-spec br() -> element().
br() -> aihtml_html:void(br, [], []).

-spec hr(css(), attrs()) -> element().
hr(Css, A) -> aihtml_html:void(hr, Css, A).

-spec img(css(), attrs()) -> element().
img(Css, A) -> aihtml_html:void(img, Css, A).

%% @doc Any element, for tags without a helper.
-spec el(atom() | binary(), html(), css(), attrs()) -> element().
el(Tag, Children, Css, Attrs) -> aihtml_html:el(Tag, Children, Css, Attrs).

-spec void(atom() | binary(), css(), attrs()) -> element().
void(Tag, Css, Attrs) -> aihtml_html:void(Tag, Css, Attrs).

%% @doc Text content. Plain binaries are text already; this exists for
%% values that would otherwise be read as children, such as a list of
%% integers.
-spec text(term()) -> binary().
text(V) when is_list(V) -> unicode:characters_to_binary(V);
text(V) -> beamai_html_escape:to_binary(V, aihtml).

%% @doc Trusted HTML, written without escaping. Never pass user input.
-spec safe(iodata()) -> {safe, iodata()}.
safe(IoData) -> {safe, IoData}.

-spec render(html()) -> iodata().
render(Html) -> aihtml_html:render(Html).

-spec render_binary(html()) -> binary().
render_binary(Html) -> aihtml_html:render_binary(Html).

%% @doc A whole document, see `aihtml_page'.
-spec page(html(), aihtml_page:opts()) -> iodata().
page(Body, Opts) -> aihtml_page:render(Body, Opts).

%% @doc Attributes that make an element fetch HTML from the server and swap
%% it into `Target' (a CSS selector, or `this'). Splice the result into any
%% Attrs list: `button(<<"More">>, more, [], [fetch(get, <<"/more">>, <<"#list">>)])'.
-spec fetch(get | post | put | patch | delete, iodata(), iodata() | this) -> attrs().
fetch(Method, Url, Target) -> fetch(Method, Url, Target, #{}).

%% @doc Options: `swap' (inner | outer | append | prepend | none, default
%% inner), `trigger' (a DOM event name; default submit for forms, change
%% for inputs, click otherwise), `confirm' (a question asked first).
-spec fetch(get | post | put | patch | delete, iodata(), iodata() | this,
            #{swap => inner | outer | append | prepend | none,
              trigger => atom() | binary(), confirm => iodata()}) -> attrs().
fetch(Method, Url, Target, Opts) ->
    lists:member(Method, [get, post, put, patch, delete])
        orelse error({aihtml, {bad_fetch_method, Method}}),
    Swap = maps:get(swap, Opts, inner),
    lists:member(Swap, [inner, outer, append, prepend, none])
        orelse error({aihtml, {bad_fetch_swap, Swap}}),
    [{data_ah_fetch, Method},
     {data_ah_url, iolist_to_binary(Url)},
     {data_ah_target, target(Target)},
     {data_ah_swap, Swap},
     {data_ah_trigger, maps:get(trigger, Opts, undefined)},
     {data_ah_confirm, maps:get(confirm, Opts, undefined)}].

%% @doc Bind an event to an action, spliced into Attrs like `fetch/3':
%% `button(<<"Save">>, save, [], [on(click, {?MODULE, save, #{id => 7}})])'.
%% When the browser reports `Event' (click, change, input, submit, keydown,
%% ...), it POSTs the signed action and the event; `Module:action/4' runs
%% on whichever node receives the request. See `aihtml_action'.
-spec on(atom() | binary(), aihtml_action:ref()) -> attrs().
on(Event, Action) -> on(Event, Action, #{}).

%% @doc Options:
%% `debounce' (ms) waits for the events to pause and sends only the last
%% one, for `input' and `keyup';
%% `include' is a list of selectors (or `{id, Id}') whose controls' values
%% are sent along in the event's `values';
%% `confirm' asks the user first.
-spec on(atom() | binary(), aihtml_action:ref(),
         #{debounce => pos_integer(), include => [iodata() | {id, iodata() | atom()}],
           confirm => iodata()}) -> attrs().
on(Event, Action, Opts) when is_map(Opts) ->
    E = beamai_html_escape:to_binary(Event, aihtml),
    lists:all(fun(C) -> C >= $a andalso C =< $z end, binary_to_list(E)) andalso E =/= <<>>
        orelse error({aihtml, {bad_event_name, Event}}),
    is_function(Action) andalso error({aihtml, {action_must_be_mfa, Action}}),
    [{<<"data-ah-on">>, {actions, [{E, aihtml_action:token(Action), maps:with([debounce], Opts)}]}},
     [{<<"data-ah-include">>, {selectors, [include_sel(S) || S <- Sels]}}
      || #{include := Sels} <- [Opts], Sels =/= []],
     {data_ah_confirm, maps:get(confirm, Opts, undefined)}].

%% @doc Follow a push topic, spliced into the Attrs of the element whose
%% content the topic updates: `ul(Items, [], [{id, list}, subscribe(todos)])'.
%% The page opens one event stream for all its topics. One subscription
%% per element.
-spec subscribe(aihtml_push:topic()) -> attrs().
subscribe(Topic) -> subscribe(Topic, #{}).

%% @doc Options: `refresh' is an action run after every reconnect of the
%% stream (not the first connect), to reload what may have been missed.
-spec subscribe(aihtml_push:topic(), #{refresh => aihtml_action:ref()}) -> attrs().
subscribe(Topic, Opts) when is_map(Opts) ->
    [{data_ah_subscribe, aihtml_push:token(Topic)},
     [{data_ah_refresh, aihtml_action:token(Ref)} || #{refresh := Ref} <- [Opts]]].

include_sel({id, Id}) -> <<"#", (text(Id))/binary>>;
include_sel(Sel) -> text(Sel).

target(this) -> <<"this">>;
target(T) -> iolist_to_binary(T).

%%%===================================================================
%%% Prefabs, see aihtml_prefab and aihtml_catalog
%%%===================================================================

-spec button(html(), term(), css(), attrs()) -> element().
button(Content, Value, Css, Attrs) -> aihtml_prefab:button(Content, Value, Css, Attrs).
-spec checkbox(html(), term(), css(), attrs()) -> element().
checkbox(Content, Value, Css, Attrs) -> aihtml_prefab:checkbox(Content, Value, Css, Attrs).
-spec radio(html(), term(), css(), attrs()) -> element().
radio(Content, Value, Css, Attrs) -> aihtml_prefab:radio(Content, Value, Css, Attrs).
-spec switch(html(), term(), css(), attrs()) -> element().
switch(Content, Value, Css, Attrs) -> aihtml_prefab:switch(Content, Value, Css, Attrs).
-spec input(term(), css(), attrs()) -> element().
input(Value, Css, Attrs) -> aihtml_prefab:input(Value, Css, Attrs).
-spec textarea(term(), css(), attrs()) -> element().
textarea(Value, Css, Attrs) -> aihtml_prefab:textarea(Value, Css, Attrs).
-spec select([{term(), html()} | term()], term(), css(), attrs()) -> element().
select(Options, Value, Css, Attrs) -> aihtml_prefab:select(Options, Value, Css, Attrs).
-spec field(html(), html(), css(), attrs()) -> element().
field(Label, Control, Css, Attrs) -> aihtml_prefab:field(Label, Control, Css, Attrs).
-spec card(html(), css(), attrs()) -> element().
card(Children, Css, Attrs) -> aihtml_prefab:card(Children, Css, Attrs).
-spec alert(html(), css(), attrs()) -> element().
alert(Children, Css, Attrs) -> aihtml_prefab:alert(Children, Css, Attrs).
-spec badge(html(), css(), attrs()) -> element().
badge(Children, Css, Attrs) -> aihtml_prefab:badge(Children, Css, Attrs).
-spec tabs([{term(), html(), html()}], term(), css(), attrs()) -> element().
tabs(Tabs, Active, Css, Attrs) -> aihtml_prefab:tabs(Tabs, Active, Css, Attrs).
-spec theme_switcher(css(), attrs()) -> element().
theme_switcher(Css, Attrs) -> aihtml_prefab:theme_switcher(Css, Attrs).
