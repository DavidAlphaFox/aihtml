%%%-------------------------------------------------------------------
%%% @doc The aspect_ratio component (designs/04-components.md): `ah_aspect_ratio/3'
%%% builds an #ah_aspect_ratio{} element record (include/aihtml_aspect_ratio.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_aspect_ratio).
-behaviour(aihtml_element).

-include("aihtml_aspect_ratio.hrl").

-export([ah_aspect_ratio/3, render/1, fields/1, catalog/0]).

-import(aihtml_lib_display, [num/1]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Fixed aspect ratio box. Option `ratio': `<<"16/9">>' (default),
%% `<<"4:3">>', a number or `{W, H}'. A `style' attribute is kept.
-spec ah_aspect_ratio(html(), css(), attrs()) -> #ah_aspect_ratio{}.
ah_aspect_ratio(Children, Css, Attrs) ->
    ?E:build(?MODULE, #ah_aspect_ratio{body = Children}, Css, Attrs).

%% @doc The field names of #ah_aspect_ratio{}.
-spec fields(atom()) -> [atom()].
fields(ah_aspect_ratio) -> record_info(fields, ah_aspect_ratio).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_aspect_ratio{}) -> html().
render(#ah_aspect_ratio{body = Children, ratio = Ratio, style = Style0} = R) ->
    Cls = ?E:classes(?MODULE, R),
    %% a style attribute is kept after the ratio, also when given with a
    %% binary key (which stays in attrs)
    {Styles, Rest} = lists:partition(fun({K, _}) -> K =:= <<"style">>;
                                        (_) -> false
                                     end, flat_attrs(R#ah_aspect_ratio.attrs)),
    Style = [<<"aspect-ratio: ">>, ratio_css(Ratio), <<";">>
             | [[<<" ">>, S] || S <- [Style0 || Style0 =/= undefined] ++ [V || {_, V} <- Styles]]],
    ?H:el('div', Children, Cls,
          [[{style, iolist_to_binary(Style)}],
           ?E:root_attrs(R#ah_aspect_ratio{attrs = Rest}, none)]).

ratio_css(undefined) -> <<"16 / 9">>;
ratio_css(N) when is_number(N), N > 0 -> num(N);
ratio_css({W, H}) when is_number(W), is_number(H), W > 0, H > 0 ->
    [num(W), <<" / ">>, num(H)];
ratio_css(S) when is_binary(S); is_list(S) ->
    B = unicode:characters_to_binary(S),
    Parts = [string:trim(X) || X <- binary:split(B, [<<"/">>, <<":">>])],
    case [parse_num(X) || X <- Parts] of
        [N] when is_number(N), N > 0 -> num(N);
        [W, H] when is_number(W), is_number(H), W > 0, H > 0 -> [num(W), <<" / ">>, num(H)];
        _ -> error({aihtml, {bad_ratio, S}})
    end;
ratio_css(Other) -> error({aihtml, {bad_ratio, Other}}).

parse_num(B) ->
    case string:to_integer(B) of
        {I, <<>>} -> I;
        _ -> case string:to_float(B) of
                 {F, <<>>} -> F;
                 _ -> error
             end
    end.

flat_attrs(M) when is_map(M) -> lists:sort(maps:to_list(M));
flat_attrs(L) when is_list(L) ->
    lists:flatmap(fun(X) when is_list(X); is_map(X) -> flat_attrs(X);
                     (X) -> [X]
                  end, L).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => aspect_ratio, category => media, root => <<"ah-aspect-ratio">>,
      signature => <<"ah_aspect_ratio(Children, Css, Attrs)">>,
      options => [ratio],
      doc => <<"Locks Children to a ratio (\"16/9\", \"4:3\", 1.5 or {W, H}).">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{ratio => <<"\"16/9\" (default), \"4:3\", a number such as 1.5, or {W, H}.">>},
      methods => []}.
