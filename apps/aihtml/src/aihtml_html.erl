%%%-------------------------------------------------------------------
%%% @doc The element tree and its renderer.
%%%
%%% Every builder in aihtml returns an `element()'. Classes and attributes
%%% are normalised when the element is built, so a bad attribute name fails
%%% at the call site that wrote it, and `render/1' only concatenates.
%%%
%%% Children are rendered by these rules:
%%%
%%%   binary / integer / float / atom   text, HTML-escaped
%%%   printable unicode charlist        text, HTML-escaped
%%%   other list                        a sequence of children
%%%   {safe, iodata()}                  written verbatim (already HTML)
%%%   element()                         rendered recursively
%%%   element record (#ah_button{} ...) its module's render/1, rendered
%%%                                     recursively (aihtml_element)
%%%   undefined / null                  nothing
%%%
%%% Escaping is `beamai_html_escape:escape/1', the same five-character set
%%% the beamai_render engines use. `{safe, iodata()}' is also the marker the
%%% beamai_jinja runtime understands, so a rendered element can be passed to
%%% a jinja template unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_html).

-export([el/4, void/3, render/1, render_binary/1,
         classes/1, attrs/1, merge_attrs/2]).

-export_type([html/0, element/0, css/0, attrs/0, attr/0, action/0]).

-record(el, {tag :: binary(),
             attrs :: [attr()],
             children :: html() | void}).

%% Transparent: element records (aihtml_element) share html() with it, and
%% dialyzer rejects an opaque type in a union with tuple().
-type element() :: #el{}.
-type html() :: element() | aihtml_element:element() | binary() | number() | atom()
              | {safe, iodata()} | [html()] | string().
-type css() :: [binary() | atom() | string() | css()].
%% Attributes are a proplist or a map. Nested lists are flattened, which
%% lets helpers such as `aihtml:fetch/3' return a list that is spliced in.
-type attrs() :: [{atom() | binary(), term()} | attrs()] | #{atom() | binary() => term()}.
-type attr() :: {binary(), binary() | true | {actions, [action()]}}.
%% {Event, SignedToken, Opts}, built by aihtml:on/2,3.
-type action() :: {binary(), binary(), #{debounce => pos_integer()}}.

%% HTML elements that never have content or a closing tag.
-define(IS_VOID(T), (T =:= <<"area">> orelse T =:= <<"base">> orelse
                     T =:= <<"br">> orelse T =:= <<"col">> orelse
                     T =:= <<"embed">> orelse T =:= <<"hr">> orelse
                     T =:= <<"img">> orelse T =:= <<"input">> orelse
                     T =:= <<"link">> orelse T =:= <<"meta">> orelse
                     T =:= <<"source">> orelse T =:= <<"track">> orelse
                     T =:= <<"wbr">>)).

%%%===================================================================
%%% Building
%%%===================================================================

%% @doc Build an element with children. `Css' becomes the `class'
%% attribute; a class given in `Attrs' is appended after it.
-spec el(atom() | binary(), html(), css(), attrs()) -> element().
el(Tag, Children, Css, Attrs) ->
    T = tag(Tag),
    case ?IS_VOID(T) of
        true  -> error({aihtml, {void_element_with_children, T}});
        false -> #el{tag = T, attrs = with_class(Css, Attrs),
                     children = Children}
    end.

%% @doc Build a void element such as `input' or `img'.
-spec void(atom() | binary(), css(), attrs()) -> element().
void(Tag, Css, Attrs) ->
    T = tag(Tag),
    case ?IS_VOID(T) of
        true  -> #el{tag = T, attrs = with_class(Css, Attrs), children = void};
        false -> error({aihtml, {not_a_void_element, T}})
    end.

%% @doc Normalise a class list into one space separated binary.
%% Atoms are written literally; binaries may hold several classes.
-spec classes(css()) -> binary().
classes(Css) ->
    Parts = [C || C <- [class_bin(X) || X <- flatten_css(Css)], C =/= <<>>],
    iolist_to_binary(lists:join(<<" ">>, Parts)).

%% @doc Normalise attributes. Later keys override earlier ones but keep the
%% position of the first occurrence; `class' values accumulate instead.
%% `true' renders a bare attribute; `false', `undefined' and `null' drop it.
%% `{data, #{k => v}}' expands to `data-k' attributes, and `_' in an
%% attribute name becomes `-', so `aria_label' is written `aria-label'.
-spec attrs(attrs()) -> [attr()].
attrs(Attrs) ->
    finish(lists:foldl(fun add_attr/2, [], expand(Attrs))).

%% @doc Merge two attribute sets, `Over' wins except for `class'.
-spec merge_attrs(attrs(), attrs()) -> [attr()].
merge_attrs(Base, Over) ->
    attrs([to_list(Base), to_list(Over)]).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(html()) -> iodata().
render(#el{tag = T, attrs = A, children = void}) ->
    [$<, T, render_attrs(A), $>];
render(#el{tag = T, attrs = A, children = C}) ->
    [$<, T, render_attrs(A), $>, render(C), "</", T, $>];
render({safe, IoData}) ->
    IoData;
render(B) when is_binary(B) ->
    beamai_html_escape:escape(B);
render(undefined) -> [];
render(null) -> [];
render(X) when is_atom(X); is_number(X) ->
    beamai_html_escape:escape(X);
render([]) -> [];
render(L) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true  -> beamai_html_escape:escape(unicode:characters_to_binary(L));
        false -> [render(C) || C <- L]
    end;
render(T) when is_tuple(T) ->
    case aihtml_element:is_element(T) of
        true  -> render(aihtml_element:render(T));
        false -> error({aihtml, {not_renderable, T}})
    end;
render(Other) ->
    error({aihtml, {not_renderable, Other}}).

-spec render_binary(html()) -> binary().
render_binary(Html) ->
    iolist_to_binary(render(Html)).

%%%===================================================================
%%% Internal
%%%===================================================================

render_attrs(Attrs) ->
    [render_attr(A) || A <- Attrs].

render_attr({K, true}) -> [$\s, K];
render_attr({K, {actions, As}}) ->
    %% Actions bound with aihtml:on/2,3: event:token[:debounce]
    [$\s, K, "=\"", lists:join($\s, [action_spec(E, Tok, Opts) || {E, Tok, Opts} <- As]), $"];
render_attr({K, V}) -> [$\s, K, "=\"", beamai_html_escape:escape(V), $"].

action_spec(Event, Token, #{debounce := Ms}) ->
    [Event, $:, Token, $:, integer_to_binary(Ms)];
action_spec(Event, Token, #{}) ->
    [Event, $:, Token].

with_class(Css, Attrs) ->
    case classes(Css) of
        <<>> -> attrs(Attrs);
        C    -> attrs([{class, C}, to_list(Attrs)])
    end.

to_list(M) when is_map(M) -> lists:sort(maps:to_list(M));
to_list(L) when is_list(L) -> L.

expand(M) when is_map(M) -> expand(to_list(M));
expand(L) when is_list(L) -> lists:flatmap(fun expand_one/1, L).

expand_one(L) when is_list(L); is_map(L) -> expand(L);
expand_one({data, M}) when is_map(M) ->
    [{<<"data-", (name(K))/binary>>, V} || {K, V} <- to_list(M)];
expand_one({K, V}) -> [{name(K), V}];
expand_one(Other) -> error({aihtml, {bad_attribute, Other}}).

add_attr({_K, V}, Acc) when V =:= false; V =:= undefined; V =:= null ->
    Acc;
add_attr({<<"class">>, V}, Acc) ->
    C = classes([V]),
    case lists:keyfind(<<"class">>, 1, Acc) of
        false -> [{<<"class">>, C} | Acc];
        {_, Old} -> lists:keyreplace(<<"class">>, 1, Acc,
                                     {<<"class">>, classes([Old, C])})
    end;
add_attr({<<"data-ah-on">>, {actions, As}}, Acc) ->
    case lists:keyfind(<<"data-ah-on">>, 1, Acc) of
        false -> [{<<"data-ah-on">>, {actions, As}} | Acc];
        {_, {actions, Old}} ->
            lists:keyreplace(<<"data-ah-on">>, 1, Acc,
                             {<<"data-ah-on">>, {actions, Old ++ As}})
    end;
add_attr({<<"data-ah-include">>, {selectors, Sels}}, Acc) ->
    case lists:keyfind(<<"data-ah-include">>, 1, Acc) of
        false -> [{<<"data-ah-include">>, iolist_to_binary(lists:join(<<", ">>, Sels))} | Acc];
        {_, Old} -> lists:keyreplace(<<"data-ah-include">>, 1, Acc,
                                     {<<"data-ah-include">>,
                                      iolist_to_binary([Old, <<", ">>,
                                                        lists:join(<<", ">>, Sels)])})
    end;
add_attr({K, V}, Acc) ->
    Val = value(K, V),
    case lists:keymember(K, 1, Acc) of
        true  -> lists:keyreplace(K, 1, Acc, {K, Val});
        false -> [{K, Val} | Acc]
    end.

finish(RevAcc) ->
    [A || {K, V} = A <- lists:reverse(RevAcc),
          not (K =:= <<"class">> andalso V =:= <<>>)].

value(_K, true) -> true;
value(_K, V) when is_binary(V) -> V;
value(_K, V) when is_atom(V); is_number(V) ->
    beamai_html_escape:to_binary(V, aihtml);
value(K, V) when is_list(V) ->
    case unicode:characters_to_binary(V) of
        B when is_binary(B) -> B;
        _ -> error({aihtml, {bad_value, K, V}})
    end;
value(K, V) -> error({aihtml, {bad_value, K, V}}).

flatten_css(L) when is_list(L) ->
    case L =/= [] andalso io_lib:printable_unicode_list(L) of
        true  -> [L];
        false -> lists:flatmap(fun flatten_css/1, L)
    end;
flatten_css(X) -> [X].

class_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
class_bin(B) when is_binary(B) -> normalize_space(B);
class_bin(L) when is_list(L) -> normalize_space(unicode:characters_to_binary(L));
class_bin(Other) -> error({aihtml, {bad_class, Other}}).

normalize_space(B) ->
    iolist_to_binary(lists:join(<<" ">>, binary:split(B, [<<" ">>, <<"\t">>, <<"\n">>],
                                                      [global, trim_all]))).

tag(A) when is_atom(A) -> tag(atom_to_binary(A, utf8));
tag(B) when is_binary(B) ->
    case B =/= <<>> andalso valid(B, fun tag_char/1) of
        true  -> B;
        false -> error({aihtml, {bad_tag, B}})
    end.

name(A) when is_atom(A) -> name(atom_to_binary(A, utf8));
name(B) when is_binary(B) ->
    N = binary:replace(B, <<"_">>, <<"-">>, [global]),
    case N =/= <<>> andalso valid(N, fun attr_char/1) of
        true  -> N;
        false -> error({aihtml, {bad_attribute_name, B}})
    end;
name(Other) -> error({aihtml, {bad_attribute_name, Other}}).

valid(B, Pred) -> lists:all(Pred, binary_to_list(B)).

tag_char(C) -> (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z)
                   orelse (C >= $0 andalso C =< $9) orelse C =:= $-.

attr_char(C) -> tag_char(C) orelse C =:= $: orelse C =:= $. orelse C =:= $@.
