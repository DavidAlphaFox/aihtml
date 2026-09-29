%%%-------------------------------------------------------------------
%%% @doc Toasts, ported from sigil's overlay/toast: a page-level API, not
%%% an element. toast/3 in an action or shows_toast/2 on a trigger pops a
%%% card in a screen corner; the card is templates/notification.mustache
%%% (see aihtml_lib_overlay:card/2) around templates/toast.mustache. The
%%% browser side is AH.fn("toast") in assets/js/components/toast.js.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_toast).

%% Shared templates (see aihtml_tpl): also compiled to AH.tpl.* for the
%% browser, so a toast card has one source of markup.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_toast, "../templates/toast.mustache"}).

-export([toast/3, shows_toast/2, catalog/0, facade_extras/0]).

-define(L, aihtml_lib_overlay).

%% @doc Attributes that pop a toast when the element is clicked, without a
%% server round trip. Opts: `description', `variant' (info | success |
%% warning | error), `duration' (ms, default 4000, 0 keeps it), `position'
%% (top_right ...), `closable'.
-spec shows_toast(iodata(), map()) -> aihtml_html:attrs().
shows_toast(Message, Opts) when is_map(Opts) ->
    [{data_ah_toast, ?L:text(Message)},
     {data_ah_toast_description, case maps:get(description, Opts, undefined) of
                                     undefined -> undefined;
                                     D -> ?L:text(D)
                                 end},
     {data_ah_toast_variant, ?L:opt_bin(variant, Opts)},
     {data_ah_toast_duration, ?L:opt_bin(duration, Opts)},
     {data_ah_toast_position, case maps:get(position, Opts, undefined) of
                                  undefined -> undefined;
                                  P -> ?L:dash(P)
                              end},
     {data_ah_toast_closable, ?L:opt_bin(closable, Opts)}].

%% @doc In an action: pop a toast (sigil's toast/show!): title `Message'
%% plus Opts `description', `variant', `duration' (ms, default 4000, 0
%% keeps it), `position', `closable', `close_on_click', `width'.
-spec toast(aihtml_action:ctx(), iodata(), map()) -> ok.
toast(Ctx, Message, Opts) when is_map(Opts) ->
    Title = ?L:text(Message),
    Desc = case maps:get(description, Opts, undefined) of
               undefined -> <<>>;
               D -> ?L:text(D)
           end,
    {safe, Content} = aihtml_tpl:safe(tpl_toast(#{has_title => Title =/= <<>>, title => Title,
                                                  has_description => Desc =/= <<>>,
                                                  description => Desc})),
    ?L:send_card(Ctx, Opts, 4000, Content).

%% @doc Attribute helpers re-exported by the aihtml facade.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{shows_toast, 2}].

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    Events = [<<"ah:open">>, <<"ah:close">>],
    [#{name => toast, category => overlay,
       signature => <<"toast(Ctx, Message, Opts)">>,
       root => <<"ah-notify">>,
       option_docs => #{description => <<"Second line under the message.">>,
                       variant => <<"info (default) | success | warning | error.">>,
                       duration => <<"ms before it closes (4000); 0 keeps it.">>,
                       position => <<"top_right (default) | top_left | bottom_right | bottom_left.">>,
                       closable => <<"Close button (true).">>,
                       close_on_click => <<"A click on the card closes it (true).">>,
                       width => <<"Card width.">>},
       methods => [],
       behavior => none, events => Events,
       doc => <<"Page-level API, not an element: toast/3 in an action or "
                "shows_toast/2 on a trigger pops a card built by AH.fn(\"toast\").">>}].
