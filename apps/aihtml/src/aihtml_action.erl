%%%-------------------------------------------------------------------
%%% @doc Actions: Erlang functions that browser events call, one HTTP
%%% request per event, answered with a list of DOM operations.
%%%
%%% ```
%%% -module(todo_page).
%%% -behaviour(aihtml_action).
%%% -include_lib("aihtml/include/aihtml.hrl").
%%% -export([action/4]).
%%%
%%% item(#{id := Id, text := Text}) ->
%%%     ah_li([Text, ah_button(<<"Delete">>, Id, [borderless],
%%%                            [on(click, {?MODULE, delete, #{id => Id}})])],
%%%           [], [{id, [<<"todo-">>, integer_to_binary(Id)]}]).
%%%
%%% action(delete, #{id := Id}, _Event, Ctx) ->
%%%     ok = todo_db:delete(Id),
%%%     aihtml_action:remove(Ctx, {id, [<<"todo-">>, integer_to_binary(Id)]}).
%%% '''
%%%
%%% Nothing is kept between requests. `on/2,3' encodes `{Module, Action,
%%% Args}' into a token signed with the application secret and writes it
%%% into the HTML. When the event fires, the browser POSTs the token and the
%%% event; any node that shares the secret verifies it and calls
%%% `Module:action(Action, Args, Event, Ctx)'. State lives in the data
%%% layer, so requests can go to any node behind any load balancer.
%%%
%%% Only modules that declare `-behaviour(aihtml_action)' can be called,
%%% and the signature stops the browser from forging actions or changing
%%% their arguments. Args are signed, not encrypted: the page can read them.
%%% Authorisation still belongs in the action (who is this user, may they
%%% delete this item), usually from the request in `meta(Ctx)'.
%%%
%%% Operations are buffered and returned when the action returns (the
%%% transport sends them as one reply); `flush/1' sends what is buffered so
%%% far through the transport's `send' callback, for progressive updates (a
%%% loading state first, the data after), which turns the reply into a
%%% stream. The wire format is the transport's (designs/02-actions.md).
%%%
%%% The secret comes from the `secret' environment key of the aihtml
%%% application (at least 32 bytes, the same on every node). Without one a
%%% random secret is generated per node, which only suits development: a
%%% restart or another node would reject the tokens of pages already open.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_action).

%% Rendering and transports.
-export([token/1, verify/1, execute/3]).
%% Shared with aihtml_push.
-export([sign/1, unsign/1, render_ops/1, stream_id/1, plain/1, check_secret/0]).
%% Operations inside an action.
-export([html/3, html/4, remove/2, attr/4, add_class/3, remove_class/3,
         set_value/3, focus/2, title/2, redirect/2, js/2, call/4,
         trigger/4, push_url/2, replace_url/2, flush/1, meta/1]).

-export_type([ref/0, event/0, ctx/0, target/0, run_opts/0, op/0]).

-callback action(Name :: atom(), Args :: term(), event(), ctx()) -> any().

-type ref() :: {module(), atom(), term()}.
%% A CSS selector, or `{id, Id}'.
-type target() :: binary() | string() | {id, iodata() | atom()}.
%% What an action receives. `id' is the element that fired, given an id by
%% the browser when it had none. `form' holds the fields of the enclosing
%% form, `values' those of the elements named by the `include' option, and
%% `data' the element's own data-* attributes.
-type event() :: #{type := binary(), id := binary(), value := term(),
                   checked := boolean() | null, key := binary() | null,
                   form := #{binary() => binary()}, values := #{binary() => term()},
                   data := #{binary() => binary()}}.
-opaque ctx() :: {aihtml_ctx, pid(), fun(([op()]) -> any()), map(), binary() | undefined}.
%% A DOM operation, as sent to the browser (JSON object).
-type op() :: #{atom() => term()}.
%% `send' gets the operations `flush/1' sends before the action returns
%% (the transport then streams its reply). `meta' is handed to the action
%% untouched (the cowboy transport puts the request there). `stream_id'
%% names the page's push stream, so a publish can skip the page that caused
%% it (see aihtml_push:publish/3).
-type run_opts() :: #{send := fun(([op()]) -> any()),
                      meta => map(),
                      stream_id => binary()}.

-define(BUF, aihtml_action_buf).
-define(COLLECT, aihtml_action_collect).
-define(SECRET_KEY, {aihtml, action_secret}).

%%%===================================================================
%%% Tokens
%%%===================================================================

%% @doc The signed token for an action reference. Args must be plain data
%% (no funs, pids, ports or references): the token outlives this process.
-spec token(ref()) -> binary().
token({Mod, Name, Args} = Ref) when is_atom(Mod), is_atom(Name) ->
    plain(Args) orelse error({aihtml, {action_args_not_data, Ref}}),
    sign(Ref);
token(Other) ->
    error({aihtml, {bad_action, Other}}).

%% @doc Check a token from the browser. Fails unless the signature is ours
%% and the module declares the aihtml_action behaviour.
-spec verify(binary()) -> {ok, ref()} | {error, invalid_action}.
verify(Token) ->
    case unsign(Token) of
        {ok, {Mod, Name, _Args} = Ref} when is_atom(Mod), is_atom(Name) ->
            case is_action_module(Mod) of
                true -> {ok, Ref};
                false -> {error, invalid_action}
            end;
        _ ->
            {error, invalid_action}
    end.

%% @doc Sign a term with the application secret:
%% base64url(term_to_binary(T)) "." base64url(HMAC-SHA256).
-spec sign(term()) -> binary().
sign(Term) ->
    Payload = term_to_binary(Term, [{minor_version, 2}]),
    <<(b64(Payload))/binary, ".", (b64(mac(Payload)))/binary>>.

%% @doc The term inside a token made by `sign/1', if the signature holds.
%% The payload is only decoded after the signature checks out.
-spec unsign(term()) -> {ok, term()} | error.
unsign(Token) when is_binary(Token) ->
    try
        [P, M] = binary:split(Token, <<".">>),
        Payload = unb64(P),
        true = crypto:hash_equals(mac(Payload), unb64(M)),
        {ok, binary_to_term(Payload, [safe])}
    catch
        _:_ -> error
    end;
unsign(_) ->
    error.

%%%===================================================================
%%% Running an action
%%%===================================================================

%% @doc Run a verified action in the calling process. Returns the
%% operations still buffered when it returned (all of them unless it called
%% `flush/1', whose batches went to `send'), or `error' when the action
%% crashed (the crash is logged, not sent).
-spec execute(ref(), map(), run_opts()) -> {ok, [op()]} | error.
execute({Mod, Name, Args}, EventJson, #{send := Send} = Opts) ->
    Ctx = {aihtml_ctx, self(), Send, maps:get(meta, Opts, #{}), maps:get(stream_id, Opts, undefined)},
    put(?BUF, []),
    try
        _ = Mod:action(Name, Args, event(EventJson), Ctx),
        {ok, lists:reverse(get(?BUF))}
    catch
        C:R:St ->
            logger:error("aihtml action ~p:~p crashed: ~p:~p~n~p", [Mod, Name, C, R, St]),
            error
    after
        erase(?BUF)
    end.

%%%===================================================================
%%% Operations
%%%===================================================================

%% @doc Replace the content of `Target' with `Html'.
-spec html(ctx(), target(), aihtml:html()) -> ok.
html(Ctx, Target, Html) -> html(Ctx, Target, Html, inner).

%% @doc Put `Html' at `Target': `inner' replaces the content, `outer' the
%% element itself; `append' and `prepend' add to the content. `morph'
%% (the element itself, Html must have one root) and `morph_inner' (its
%% content) patch the existing DOM instead, keeping focus, caret, scroll
%% and component state of everything that survives. Every mode puts the
%% focus back on the element with the same id.
-spec html(ctx(), target(), aihtml:html(),
           inner | outer | append | prepend | morph | morph_inner) -> ok.
html(Ctx, Target, Html, Swap) when Swap =:= inner; Swap =:= outer;
                                   Swap =:= append; Swap =:= prepend;
                                   Swap =:= morph; Swap =:= morph_inner ->
    push(Ctx, target(Target, #{op => html, swap => Swap,
                               html => iolist_to_binary(aihtml_html:render(Html))})).

-spec remove(ctx(), target()) -> ok.
remove(Ctx, Target) -> push(Ctx, target(Target, #{op => remove})).

%% @doc Set an attribute; `false' or `undefined' removes it.
-spec attr(ctx(), target(), atom() | binary(), term()) -> ok.
attr(Ctx, Target, Name, Value) ->
    V = case Value of
            _ when Value =:= false; Value =:= undefined -> null;
            true -> <<>>;
            _ -> text(Value)
        end,
    push(Ctx, target(Target, #{op => attr, name => text(Name), value => V})).

-spec add_class(ctx(), target(), aihtml:css()) -> ok.
add_class(Ctx, Target, Css) ->
    push(Ctx, target(Target, #{op => class, add => aihtml_html:classes(Css)})).

-spec remove_class(ctx(), target(), aihtml:css()) -> ok.
remove_class(Ctx, Target, Css) ->
    push(Ctx, target(Target, #{op => class, remove => aihtml_html:classes(Css)})).

%% @doc Set a form control's value (`true'/`false' set a checkbox).
-spec set_value(ctx(), target(), term()) -> ok.
set_value(Ctx, Target, Value) ->
    V = case is_boolean(Value) of true -> Value; false -> text(Value) end,
    push(Ctx, target(Target, #{op => val, value => V})).

-spec focus(ctx(), target()) -> ok.
focus(Ctx, Target) -> push(Ctx, target(Target, #{op => focus})).

-spec title(ctx(), iodata()) -> ok.
title(Ctx, Title) -> push(Ctx, #{op => title, value => text(Title)}).

-spec redirect(ctx(), iodata()) -> ok.
redirect(Ctx, Url) -> push(Ctx, #{op => redirect, value => text(Url)}).

%% @doc Run JavaScript in the browser. `AH' is in scope (the runtime no
%% longer includes jQuery; a page that loads it itself can use its
%% globals). Never build the code from user input.
-spec js(ctx(), iodata()) -> ok.
js(Ctx, Code) -> push(Ctx, #{op => js, code => text(Code)}).

%% @doc Call a component method in the browser, without writing any
%% JavaScript: `call(Ctx, {id, <<"cart">>}, open, [])' runs the `open'
%% method of the behaviour the element carries (its data-ah), and
%% `call(Ctx, global, toast, [#{message => <<"Saved">>}])' runs a function
%% registered with AH.fn. Args are sent as JSON values.
-spec call(ctx(), target() | global, atom() | binary(), [term()]) -> ok.
call(Ctx, global, Method, Args) when is_list(Args) ->
    push(Ctx, #{op => call, method => text(Method), args => Args});
call(Ctx, Target, Method, Args) when is_list(Args) ->
    push(Ctx, target(Target, #{op => call, method => text(Method), args => Args})).

%% @doc Fire a DOM event in the browser (a bubbling CustomEvent): on
%% `Target', or on the document with `document'. Page scripts can listen
%% for it (`Detail', passed as JSON, is the event's `detail'), and
%% elements can bind actions to it with aihtml:on/2 (`on('ah:saved', ...)').
-spec trigger(ctx(), target() | document, atom() | binary(), term()) -> ok.
trigger(Ctx, document, Event, Detail) ->
    push(Ctx, #{op => trigger, event => text(Event), detail => Detail});
trigger(Ctx, Target, Event, Detail) ->
    push(Ctx, target(Target, #{op => trigger, event => text(Event), detail => Detail})).

%% @doc Add `Url' to the browser history without loading it, so the
%% address bar and bookmarks match what the action just showed (a page of
%% results, an open tab). Going back or forward to it reloads that URL,
%% which the server renders like any other page.
-spec push_url(ctx(), iodata()) -> ok.
push_url(Ctx, Url) -> push(Ctx, #{op => url, mode => push, value => text(Url)}).

%% @doc Like push_url/2 but replaces the current history entry.
-spec replace_url(ctx(), iodata()) -> ok.
replace_url(Ctx, Url) -> push(Ctx, #{op => url, mode => replace, value => text(Url)}).

%% @doc Send the operations buffered so far, before the action returns.
-spec flush(ctx()) -> ok.
flush({aihtml_ctx, _, Send, _, _} = Ctx) ->
    owner(Ctx),
    case put(?BUF, []) of
        [] -> ok;
        Ops ->
            _ = Send(lists:reverse(Ops)),
            ok
    end.

%% @doc What the transport passed along; the cowboy transport gives
%% `#{req => cowboy_req:req()}' for cookies, headers and the peer.
-spec meta(ctx()) -> map().
meta({aihtml_ctx, _, _, Meta, _}) -> Meta.

%% @doc The push stream of the page that sent this request, or
%% `undefined' when it has none.
-spec stream_id(ctx()) -> binary() | undefined.
stream_id({aihtml_ctx, _, _, _, Id}) -> Id.

%% @doc Run `Fun(Ctx)' and return the operations it produced instead of
%% sending them: the same operation functions, rendered once, for
%% aihtml_push to fan out. Safe to call inside an action.
-spec render_ops(fun((ctx()) -> any())) -> [op()].
render_ops(Fun) ->
    Saved = put(?BUF, []),
    put(?COLLECT, []),
    Ctx = {aihtml_ctx, self(), fun(Ops) -> put(?COLLECT, get(?COLLECT) ++ Ops) end,
           #{}, undefined},
    try
        _ = Fun(Ctx),
        flush(Ctx),
        get(?COLLECT)
    after
        erase(?COLLECT),
        case Saved of
            undefined -> erase(?BUF);
            _ -> put(?BUF, Saved)
        end
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

push(Ctx, Op) ->
    owner(Ctx),
    put(?BUF, [Op | get(?BUF)]),
    ok.

%% Operations are buffered in the request process, so they must be called
%% from it.
owner({aihtml_ctx, Pid, _, _, _}) when Pid =:= self() -> ok;
owner(_) -> error({aihtml, action_ctx_used_outside_its_request}).

target({id, Id}, Op) -> Op#{id => text(Id)};
target(Sel, Op) -> Op#{sel => text(Sel)}.

event(Ev) when is_map(Ev) ->
    #{type => maps:get(<<"type">>, Ev, <<>>),
      id => maps:get(<<"id">>, Ev, <<>>),
      value => maps:get(<<"value">>, Ev, null),
      checked => maps:get(<<"checked">>, Ev, null),
      key => maps:get(<<"key">>, Ev, null),
      form => maps:get(<<"form">>, Ev, #{}),
      values => maps:get(<<"values">>, Ev, #{}),
      data => maps:get(<<"data">>, Ev, #{})};
event(_) ->
    event(#{}).

is_action_module(Mod) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            Attrs = Mod:module_info(attributes),
            %% every -behaviour attribute: a component module may also
            %% declare aihtml_element before this one
            lists:member(?MODULE, lists:append(proplists:get_all_values(behaviour, Attrs) ++
                                               proplists:get_all_values(behavior, Attrs)))
                andalso erlang:function_exported(Mod, action, 4);
        _ ->
            false
    end.

%% @doc True for plain data: no funs, pids, ports or references anywhere.
-spec plain(term()) -> boolean().
plain(T) when is_function(T); is_pid(T); is_port(T); is_reference(T) -> false;
plain(T) when is_list(T) -> plain_list(T);
plain(T) when is_tuple(T) -> lists:all(fun plain/1, tuple_to_list(T));
plain(T) when is_map(T) -> lists:all(fun plain/1, maps:keys(T) ++ maps:values(T));
plain(_) -> true.

%% also accepts improper lists
plain_list([H | T]) -> plain(H) andalso plain_list(T);
plain_list([]) -> true;
plain_list(T) -> plain(T).

%% @doc Validate the configured secret, if any: at least 32 bytes.
-spec check_secret() -> ok | {error, {secret_too_short, string()} | {bad_secret, term()}}.
check_secret() ->
    case application:get_env(aihtml, secret) of
        undefined -> ok;
        {ok, Bin} when is_binary(Bin), byte_size(Bin) >= 32 -> ok;
        {ok, Bin} when is_binary(Bin) ->
            {error, {secret_too_short, "aihtml secret must be at least 32 bytes"}};
        {ok, Other} ->
            {error, {bad_secret, Other}}
    end.

mac(Payload) -> crypto:mac(hmac, sha256, secret(), Payload).

secret() ->
    case persistent_term:get(?SECRET_KEY, undefined) of
        undefined ->
            S = case application:get_env(aihtml, secret) of
                    {ok, Bin} when is_binary(Bin), byte_size(Bin) >= 32 ->
                        Bin;
                    {ok, _} ->
                        error({aihtml, {secret_too_short, "use at least 32 bytes"}});
                    undefined ->
                        logger:warning("aihtml: no secret configured, using a random one; "
                                       "action tokens will not survive a restart or work "
                                       "on other nodes"),
                        crypto:strong_rand_bytes(32)
                end,
            persistent_term:put(?SECRET_KEY, S),
            S;
        S ->
            S
    end.

b64(B) -> base64:encode(B, #{mode => urlsafe, padding => false}).
unb64(B) -> base64:decode(B, #{mode => urlsafe, padding => false}).

text(V) when is_binary(V) -> V;
text(V) when is_list(V) -> unicode:characters_to_binary(V);
text(V) -> beamai_html_escape:to_binary(V, aihtml).
