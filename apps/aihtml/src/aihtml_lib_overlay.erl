%%%-------------------------------------------------------------------
%%% @doc What the overlay components share (aihtml_tooltip,
%%% aihtml_popover, aihtml_drawer, aihtml_sheet, aihtml_toast,
%%% aihtml_notification, aihtml_window).
%%%
%%% Everything renders server-side, hidden; the behaviours in
%%% assets/js/components (with _lib_overlay.ts) open and close it. Three
%%% ways to drive an overlay:
%%%
%%%   declarative   splice `opens(Target)', `toggles(Target)', `closes()'
%%%                 or `closes(Target)' into any element's Attrs; a click on
%%%                 it runs the target's open/close/toggle behaviour method
%%%   server        inside an action, `open(Ctx, Target)', `close(Ctx,
%%%                 Target)' (this module), `aihtml_toast:ah_toast(Ctx,
%%%                 Message, Opts)', `aihtml_notification:notify(Ctx, Opts)'
%%%   client        `AH.invoke(el, "open")', `AH.invoke(el, "close")'
%%%
%%% `Target' is a CSS selector (binary) or `{id, Id}'.
%%%
%%% The declarative triggers are public API: the aihtml facade re-exports
%%% them (facade_extras/0, read by scripts/gen-facade.escript).
%%% open/2, close/2 and toggle/2 stay module qualified because their names
%%% are too generic to import. The rest of the module is internal: the
%%% notification card, the drawer and sheet markup and field checks.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_overlay).

%% Shared templates (see aihtml_tpl): also compiled to AH.tpl.* for the
%% browser, so a notification or toast card has one source of markup.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_notification, "../templates/notification.mustache"}).

%% Declarative triggers (public, re-exported by the facade)
-export([opens/1, closes/0, closes/1, closes/2, toggles/1, facade_extras/0]).
%% Server-driven helpers, for actions
-export([open/2, close/2, toggle/2]).
%% Internal
-export([card/2, send_card/4, slide/5, root_id/1, bool/2, opt_bool/2, int/2, opt_int/2,
         width_style/1, opt_el/4, opt_bin/2, if_/2, bool/1, css_len/1, dash/1, text/1]).

-export_type([target/0, len/0, side/0, corner/0]).

-type target() :: binary() | string() | {id, iodata() | atom()}.
%% A length: integer px or a CSS length such as <<"50vh">>.
-type len() :: non_neg_integer() | binary() | string().
-type side() :: top | bottom | left | right.
-type corner() :: top_right | top_left | bottom_right | bottom_left.

-define(H, aihtml_html).
-define(E, aihtml_element).

-define(CLOSE_ICON,
        {safe, <<"<svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
                 "stroke-width=\"2\" stroke-linecap=\"round\" aria-hidden=\"true\">"
                 "<line x1=\"18\" y1=\"6\" x2=\"6\" y2=\"18\"></line>"
                 "<line x1=\"6\" y1=\"6\" x2=\"18\" y2=\"18\"></line></svg>">>}).

%%%===================================================================
%%% Declarative triggers
%%%===================================================================

%% @doc Attrs: a click opens `Target' (drawer, sheet, window, popover,
%% tooltip or notification template). A popover anchors to the element.
-spec opens(target()) -> aihtml_html:attrs().
opens(Target) -> trigger_attrs(data_ah_open, Target).

%% @doc Attrs: a click toggles `Target'.
-spec toggles(target()) -> aihtml_html:attrs().
toggles(Target) -> trigger_attrs(data_ah_toggle, Target).

%% @doc Attrs: a click closes the overlay the element is in.
-spec closes() -> aihtml_html:attrs().
closes() -> [{data_ah_close, <<>>}].

%% @doc Attrs: a click closes `Target'.
-spec closes(target()) -> aihtml_html:attrs().
closes(Target) -> [{data_ah_close, selector(Target)}].

%% @doc Attrs: a click closes `Target' (`closest' for the enclosing
%% overlay) and reports `Result' in the `ah:close' event, like sigil's
%% window ok/cancel buttons.
-spec closes(target() | closest, atom() | iodata()) -> aihtml_html:attrs().
closes(closest, Result) -> [{data_ah_close, <<>>}, {data_ah_result, text(Result)}];
closes(Target, Result) -> [{data_ah_close, selector(Target)}, {data_ah_result, text(Result)}].

%% @doc The declarative triggers, re-exported by the aihtml facade
%% (scripts/gen-facade.escript).
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() ->
    [{opens, 1}, {closes, 0}, {closes, 1}, {closes, 2}, {toggles, 1}].

trigger_attrs(Key, Target) ->
    [{Key, selector(Target)}, {aria_haspopup, <<"dialog">>},
     {aria_controls, case Target of {id, Id} -> text(Id); _ -> undefined end}].

%%%===================================================================
%%% Server-driven helpers
%%%===================================================================

%% @doc In an action: open the overlay at `Target'.
-spec open(aihtml_action:ctx(), target()) -> ok.
open(Ctx, Target) -> aihtml_action:call(Ctx, Target, open, []).

%% @doc In an action: close the overlay at `Target'.
-spec close(aihtml_action:ctx(), target()) -> ok.
close(Ctx, Target) -> aihtml_action:call(Ctx, Target, close, []).

%% @doc In an action: toggle the overlay at `Target'.
-spec toggle(aihtml_action:ctx(), target()) -> ok.
toggle(Ctx, Target) -> aihtml_action:call(Ctx, Target, toggle, []).

%%%===================================================================
%%% Notification cards (notification, toast)
%%%===================================================================

%% @doc Internal: a card from templates/notification.mustache. The view
%% is built the same way in _lib_overlay.ts (cardView).
-spec card(map(), iodata()) -> {safe, iodata()}.
card(Opts, ContentHtml) ->
    V = case maps:get(variant, Opts, info) of
            X when X =:= info; X =:= success; X =:= warning; X =:= error -> X;
            B when is_binary(B) -> variant(B);
            _ -> info
        end,
    aihtml_tpl:safe(tpl_notification(
        #{variant => atom_to_binary(V),
          info => V =:= info, success => V =:= success,
          warning => V =:= warning, error => V =:= error,
          clickable => maps:get(close_on_click, Opts, true) =/= false,
          closable => maps:get(closable, Opts, true) =/= false,
          width => case maps:get(width, Opts, undefined) of
                       undefined -> null;
                       W -> css_len(W)
                   end,
          content => ContentHtml})).

variant(<<"success">>) -> success;
variant(<<"warning">>) -> warning;
variant(<<"error">>) -> error;
variant(_) -> info.

%% Server-side cards: aihtml_toast:ah_toast/3 and aihtml_notification:notify/2
%% render the card here, with the same templates the browser uses, and
%% send the finished HTML to AH.fn("notify") as `card'; the browser only
%% places it in its corner (created on demand, so an html operation has no
%% fixed target) and runs its timer. No markup is built from options in
%% the browser for them.

%% @doc Internal: send a card rendered from `Content' to the browser.
-spec send_card(aihtml_action:ctx(), map(), non_neg_integer(), iodata()) -> ok.
send_card(Ctx, Opts, DefaultDuration, Content) ->
    {safe, Card} = card(Opts, Content),
    aihtml_action:call(Ctx, global, notify,
                       [#{card => Card,
                          position => dash(maps:get(position, Opts, top_right)),
                          duration => maps:get(duration, Opts, DefaultDuration)}]).

%%%===================================================================
%%% Drawer and sheet
%%%===================================================================

%% @doc Internal: the markup of a drawer or sheet. `Root' is the class
%% list of the overlay (the component's classes without the literal ones),
%% `Lits' the literal classes, which go on the panel; `O' holds the
%% fields, validated.
-spec slide(drawer | sheet, aihtml_element:element(), aihtml_html:css(), aihtml_html:css(),
            map()) -> aihtml_html:html().
slide(Kind, R, Root, Lits, #{side := Side, title := Title, description := Desc,
                              footer := Footer, closable := Closable, handle := Handle} = O) ->
    P = <<"ah-", (atom_to_binary(Kind))/binary>>,
    Size = css_len(case maps:get(size, O) of
                       undefined -> default_size(Kind, Side);
                       S -> S
                   end),
    Dim = case Side of left -> <<"width:">>; right -> <<"width:">>; _ -> <<"height:">> end,
    TitleId = case root_id(R) of
                  undefined -> undefined;
                  Id -> <<Id/binary, "-title">>
              end,
    Header = case Title =/= undefined orelse Desc =/= undefined orelse Closable of
                 false -> [];
                 true ->
                     ?H:el('div',
                         [?H:el('div',
                              [opt_el(h2, Title, [<<P/binary, "__title">>], [{id, TitleId}]),
                               opt_el(p, Desc, [<<P/binary, "__description">>], [])],
                              [], []),
                          [?H:el(button, ?CLOSE_ICON, [<<P/binary, "__close">>],
                                 [{type, button}, {aria_label, <<"close">>},
                                  {data_ah_close, <<>>}]) || Closable]],
                         [<<P/binary, "__header">>], [])
             end,
    Panel = ?H:el('div',
                [[?H:el('div',
                      ?H:el(span, [], [<<P/binary, "__handle-bar">>], []),
                      [<<P/binary, "__handle">>], [{aria_hidden, <<"true">>}]) || Handle],
                 Header,
                 ?H:el('div', maps:get(body, O), [<<P/binary, "__body">>], []),
                 opt_el('div', Footer, [<<P/binary, "__footer">>], [])],
                [<<P/binary, "__panel">>, Lits],
                [{role, dialog}, {aria_modal, <<"true">>},
                 {aria_labelledby, case Title of undefined -> undefined; _ -> TitleId end},
                 {aria_label, case {TitleId, Title} of
                                  {undefined, T} when is_binary(T) -> T;
                                  _ -> undefined
                              end},
                 {data_side, Side}, {data_state, closed},
                 {style, [Dim, Size, $;]}, {tabindex, -1}]),
    ?H:el('div', Panel, Root,
          [[{data_ah, Kind}, {data_state, closed},
            {data_ah_esc, opt_bin(close_on_esc, O)},
            {data_ah_scrim, opt_bin(close_on_overlay, O)},
            {data_ah_dismissible, opt_bin(dismissible, O)},
            {data_ah_initial, if_(maps:get(open, O), <<"open">>)}],
           ?E:root_attrs(R, 'ah:close')]).

default_size(drawer, Side) when Side =:= left; Side =:= right -> <<"380px">>;
default_size(drawer, _) -> <<"50vh">>;
default_size(sheet, _) -> <<"380px">>.

%%%===================================================================
%%% Field checks and attribute values
%%%===================================================================

%% @doc Internal: the root id as a binary: the `id' field, or an id left
%% in `attrs'.
-spec root_id(aihtml_element:element()) -> binary() | undefined.
root_id(R) ->
    #{id := Id, attrs := Attrs} = ?E:base(R),
    case Id of
        undefined ->
            case lists:keyfind(<<"id">>, 1, ?H:attrs(Attrs)) of
                {_, B} when is_binary(B) -> B;
                _ -> undefined
            end;
        _ -> text(Id)
    end.

%% @doc Internal: `V' when it is a boolean, else a bad_option error.
-spec bool(atom(), term()) -> boolean().
bool(Field, V) ->
    is_boolean(V) orelse error({aihtml, {bad_option, Field, V}}),
    V.

%% @doc Internal: bool/2 that lets undefined through.
-spec opt_bool(atom(), term()) -> boolean() | undefined.
opt_bool(_Field, undefined) -> undefined;
opt_bool(Field, V) -> bool(Field, V).

%% @doc Internal: `V' when it is a non-negative integer, else a
%% bad_option error.
-spec int(atom(), term()) -> non_neg_integer().
int(Field, V) ->
    (is_integer(V) andalso V >= 0) orelse error({aihtml, {bad_option, Field, V}}),
    V.

%% @doc Internal: int/2 that lets undefined through.
-spec opt_int(atom(), term()) -> non_neg_integer() | undefined.
opt_int(_Field, undefined) -> undefined;
opt_int(Field, V) -> int(Field, V).

%% @doc Internal: a `width:' style, or undefined.
-spec width_style(undefined | len()) -> undefined | iodata().
width_style(undefined) -> undefined;
width_style(W) -> [<<"width:">>, css_len(W)].

%% @doc Internal: an element, or nothing when the content is undefined.
-spec opt_el(atom(), aihtml_html:html() | undefined, aihtml_html:css(), aihtml_html:attrs()) ->
          aihtml_html:html().
opt_el(_Tag, undefined, _Css, _Attrs) -> [];
opt_el(Tag, Content, Css, Attrs) -> aihtml_html:el(Tag, Content, Css, Attrs).

%% @doc Internal: an option of `Opts' as an attribute value, or undefined.
-spec opt_bin(atom(), map()) -> binary() | undefined.
opt_bin(Key, Opts) ->
    case maps:get(Key, Opts, undefined) of
        undefined -> undefined;
        true -> <<"true">>;
        false -> <<"false">>;
        V when is_atom(V) -> atom_to_binary(V);
        V when is_integer(V) -> integer_to_binary(V);
        V -> text(V)
    end.

%% @doc Internal: `V' when true, undefined (no attribute) when false.
-spec if_(boolean(), V) -> V | undefined.
if_(true, V) -> V;
if_(false, _) -> undefined.

%% @doc Internal: a boolean as an attribute value.
-spec bool(boolean()) -> binary().
bool(true) -> <<"true">>;
bool(false) -> <<"false">>.

%% @doc Internal: a CSS length: integer px or the text as given.
-spec css_len(len()) -> binary().
css_len(N) when is_integer(N) -> <<(integer_to_binary(N))/binary, "px">>;
css_len(V) -> text(V).

%% @doc Internal: top_right -> <<"top-right">>.
-spec dash(atom() | iodata()) -> binary().
dash(A) when is_atom(A) -> dash(atom_to_binary(A));
dash(B) -> binary:replace(text(B), <<"_">>, <<"-">>, [global]).

selector({id, Id}) -> <<"#", (text(Id))/binary>>;
selector(Sel) -> text(Sel).

%% @doc Internal: text as a binary.
-spec text(atom() | integer() | iodata()) -> binary().
text(A) when is_atom(A) -> atom_to_binary(A);
text(I) when is_integer(I) -> integer_to_binary(I);
text(B) when is_binary(B) -> B;
text(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_text, L}})
    end.
