%%%-------------------------------------------------------------------
%%% @doc The language of rendering and the texts of the components
%%% (designs/07-i18n.md).
%%%
%%% Texts live in JSON catalogs, one per language: priv/i18n/<lang>.json
%%% for the bundled ones, plus any the application configures:
%%%
%%% ```
%%% {"format":   {"decimal": ".", "group": ",", "first_day": 0},
%%%  "messages": {"common":   {"close": "Close"},
%%%               "datagrid": {"total": "Total {0}"}}}
%%% '''
%%%
%%% The current language is a property of the rendering process, set by
%%% the entry points (aihtml_page's `lang', the `lang' of an action
%%% request, aihtml_push:publish/3's `lang') or by `with/2'. Records render
%%% lazily, so components look their texts up while rendering. Outside of
%%% any of these it is the application's `default_locale' (default "en").
%%%
%%% Lookups fall back text by text: "zh-tw", then "zh", then "en", so a
%%% partly translated catalog still works.
%%%
%%% What the browser builds itself (menus, validation messages ...) takes
%%% its texts from the scopes en.json lists under "client": a page in
%%% another language carries them (`client_script/0', written by
%%% aihtml_page), and the browser falls back to the English text each call
%%% site gives (assets/js/runtime/i18n.ts).
%%%
%%% Application environment:
%%%
%%%   default_locale  the language when none is set, default <<"en">>
%%%   locales         #{Lang => Source}: more catalogs, or entries that
%%%                   override a bundled one (merged key by key). Source is
%%%                   a file name or {priv_dir, App, RelativePath}.
%%%
%%% Catalogs are read once, on first use; `reload/0' reads them again.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_i18n).

-export([locale/0, with/2, normalize/1, locales/0,
         text/2, text/3, texts/1, texts/2, format/1, formats/1, client/0, client_script/0,
         reload/0]).

-export_type([lang/0, scope/0]).

%% A language tag: <<"en">>, <<"zh-CN">>, zh, "zh_TW" ... (normalised to
%% lower case with hyphens).
-type lang() :: binary() | atom() | string().
%% A group of texts: a component name, or `common' for shared ones.
-type scope() :: atom().

-define(PD, aihtml_i18n_locale).
-define(KEY, {?MODULE, catalogs}).
-define(FALLBACK, <<"en">>).

%%%===================================================================
%%% The current language
%%%===================================================================

%% @doc The language rendering uses now.
-spec locale() -> binary().
locale() ->
    case get(?PD) of
        undefined -> default_locale();
        L -> L
    end.

%% @doc Run `Fun' with `Lang' as the current language, and restore the
%% previous one afterwards (also when Fun raises).
-spec with(lang() | undefined, fun(() -> R)) -> R.
with(Lang, Fun) ->
    Old = put(?PD, normalize(Lang)),
    try Fun()
    after
        case Old of
            undefined -> erase(?PD);
            _ -> put(?PD, Old)
        end
    end.

%% @doc A language tag in canonical form: lower case, `-' between parts.
%% Anything that is not a well-formed tag (it may come from a browser)
%% gives the default language.
-spec normalize(lang() | undefined | null) -> binary().
normalize(L) ->
    case canonical(L) of
        {ok, Tag} -> Tag;
        error -> default_locale()
    end.

%% @doc The languages that have a catalog, sorted.
-spec locales() -> [binary()].
locales() -> lists:sort(maps:keys(catalogs())).

%%%===================================================================
%%% Texts
%%%===================================================================

%% @doc One text of `Scope' in the current language.
-spec text(scope(), atom()) -> binary().
text(Scope, Key) ->
    S = atom_to_binary(Scope),
    K = atom_to_binary(Key),
    case first(fun(C) -> find([<<"messages">>, S, K], C) end) of
        {ok, T} -> T;
        error -> error({aihtml, {no_text, Scope, Key}})
    end.

%% @doc A text with its placeholders `{0}', `{1}' ... replaced by `Args'.
-spec text(scope(), atom(), [term()]) -> binary().
text(Scope, Key, Args) ->
    subst(text(Scope, Key), Args).

%% @doc All texts of `Scope' in the current language, missing ones taken
%% from the fallback languages.
-spec texts(scope()) -> #{atom() => binary()}.
texts(Scope) ->
    S = atom_to_binary(Scope),
    lists:foldl(fun(C, Acc) ->
                        case find([<<"messages">>, S], C) of
                            {ok, M} when is_map(M) ->
                                maps:fold(fun(K, V, A) -> A#{binary_to_atom(K) => V} end, Acc, M);
                            _ -> Acc
                        end
                end, #{}, lists:reverse(chain())).

%% @doc `texts(Scope)' with some of them replaced, as a component's
%% `labels' option does. A key the catalog does not know is an error, so a
%% misspelt label fails instead of being ignored.
-spec texts(scope(), #{atom() => unicode:chardata()}) -> #{atom() => binary()}.
texts(Scope, Over) ->
    Base = texts(Scope),
    maps:fold(fun(K, V, Acc) ->
                      is_map_key(K, Base) orelse error({aihtml, {unknown_label, Scope, K}}),
                      Acc#{K => unicode:characters_to_binary(V)}
              end, Base, Over).

%% @doc A formatting setting of the current language (decimal, group,
%% first_day ...).
-spec format(atom()) -> term().
format(Key) ->
    K = atom_to_binary(Key),
    case first(fun(C) -> find([<<"format">>, K], C) end) of
        {ok, V} -> V;
        error -> error({aihtml, {no_format, Key}})
    end.

%% @doc Several formatting settings at once, as a map (the month and
%% weekday names a date component merges into its texts).
-spec formats([atom()]) -> #{atom() => term()}.
formats(Keys) -> maps:from_list([{K, format(K)} || K <- Keys]).

%% @doc What the browser needs in the current language: the texts of the
%% scopes en.json lists under "client", and all formatting settings.
-spec client() -> #{messages := #{atom() => #{atom() => binary()}}, format := map()}.
client() ->
    Scopes = case find([<<"client">>], maps:get(?FALLBACK, catalogs(), #{})) of
                 {ok, L} when is_list(L) -> [binary_to_atom(S) || S <- L];
                 _ -> []
             end,
    Format = lists:foldl(fun(C, Acc) ->
                                 case find([<<"format">>], C) of
                                     {ok, F} when is_map(F) -> maps:merge(Acc, F);
                                     _ -> Acc
                                 end
                         end, #{}, lists:reverse(chain())),
    #{messages => maps:from_list([{S, texts(S)} || S <- Scopes]), format => Format}.

%% @doc The `<script type="application/json" id="ah-labels">' a page
%% carries for the browser, or nothing when `client()' is the same as in
%% English (the browser's own fallbacks are the English texts).
-spec client_script() -> aihtml_html:html().
client_script() ->
    C = client(),
    case C =:= with(?FALLBACK, fun client/0) of
        true -> [];
        false ->
            Json = iolist_to_binary(aihtml_json:encode(C)),
            aihtml_html:el(script, {safe, binary:replace(Json, <<"<">>, <<"\\u003c">>, [global])},
                           [], [{type, <<"application/json">>}, {id, <<"ah-labels">>}])
    end.

%% @doc Read the catalogs again (after changing their files or the
%% `locales' setting).
-spec reload() -> ok.
reload() ->
    _ = persistent_term:erase(?KEY),
    ok.

%%%===================================================================
%%% Internal
%%%===================================================================

%% {ok, Tag} for a well-formed language tag, in canonical form.
canonical(L) when is_atom(L), L =/= undefined, L =/= null -> canonical(atom_to_binary(L));
canonical(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        B when is_binary(B) -> canonical(B);
        _ -> error
    end;
canonical(L) when is_binary(L), byte_size(L) =< 35 ->
    Tag = binary:replace(string:lowercase(L), <<"_">>, <<"-">>, [global]),
    case re:run(Tag, <<"^[a-z]{2,3}(-[a-z0-9]{1,8})*$">>, [{capture, none}]) of
        match -> {ok, Tag};
        nomatch -> error
    end;
canonical(_) ->
    error.

default_locale() ->
    case canonical(application:get_env(aihtml, default_locale, ?FALLBACK)) of
        {ok, Tag} -> Tag;
        error -> ?FALLBACK
    end.

%% The catalogs to look in, most specific first: "zh-tw", "zh", "en".
chain() ->
    Cs = catalogs(),
    Tags = prefixes(locale()) ++ [?FALLBACK],
    [maps:get(T, Cs) || T <- dedup(Tags), is_map_key(T, Cs)].

dedup([]) -> [];
dedup([X | Xs]) -> [X | dedup([Y || Y <- Xs, Y =/= X])].

prefixes(Tag) ->
    Parts = binary:split(Tag, <<"-">>, [global]),
    [iolist_to_binary(lists:join(<<"-">>, lists:sublist(Parts, N)))
     || N <- lists:seq(length(Parts), 1, -1)].

first(Fun) -> first(Fun, chain()).
first(_, []) -> error;
first(Fun, [C | Cs]) ->
    case Fun(C) of
        {ok, _} = Found -> Found;
        error -> first(Fun, Cs)
    end.

find([], V) -> {ok, V};
find([K | Ks], M) when is_map(M) ->
    case M of
        #{K := V} -> find(Ks, V);
        _ -> error
    end;
find(_, _) -> error.

subst(Pattern, Args) ->
    {Out, _} = lists:foldl(fun(A, {P, N}) ->
                                   {binary:replace(P, <<"{", (integer_to_binary(N))/binary, "}">>,
                                                   to_text(A), [global]), N + 1}
                           end, {Pattern, 0}, Args),
    Out.

to_text(B) when is_binary(B) -> B;
to_text(I) when is_integer(I) -> integer_to_binary(I);
to_text(F) when is_float(F) -> float_to_binary(F, [short]);
to_text(A) when is_atom(A) -> atom_to_binary(A);
to_text(L) -> unicode:characters_to_binary(L).

catalogs() ->
    case persistent_term:get(?KEY, undefined) of
        undefined ->
            Cs = load(),
            persistent_term:put(?KEY, Cs),
            Cs;
        Cs -> Cs
    end.

%% Bundled catalogs, then the configured ones merged over them.
load() ->
    Dir = filename:join(code:priv_dir(aihtml), "i18n"),
    Bundled = maps:from_list([{list_to_binary(filename:basename(F, ".json")), read(F)}
                              || F <- filelib:wildcard(filename:join(Dir, "*.json"))]),
    Extra = application:get_env(aihtml, locales, #{}),
    maps:fold(fun(Lang, Source, Acc) ->
                      Tag = case canonical(Lang) of
                                {ok, T} -> T;
                                error -> error({aihtml, {bad_locale, Lang}})
                            end,
                      Acc#{Tag => merge(maps:get(Tag, Acc, #{}), read(source(Source)))}
              end, Bundled, Extra).

source({priv_dir, App, Rel}) -> filename:join(code:priv_dir(App), Rel);
source(File) -> File.

read(File) ->
    case file:read_file(File) of
        {ok, Bin} -> json:decode(Bin);
        {error, Reason} -> error({aihtml, {i18n_catalog, File, Reason}})
    end.

%% Deep merge: B's leaves win.
merge(A, B) when is_map(A), is_map(B) ->
    maps:fold(fun(K, V, Acc) -> Acc#{K => merge(maps:get(K, Acc, undefined), V)} end, A, B);
merge(_, B) -> B.
