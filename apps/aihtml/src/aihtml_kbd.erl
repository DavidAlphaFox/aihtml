%%%-------------------------------------------------------------------
%%% @doc The kbd component (designs/04-components.md): `kbd/3'
%%% builds an #ah_kbd{} element record (include/aihtml_kbd.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_kbd).
-behaviour(aihtml_element).

-include("aihtml_kbd.hrl").

-export([kbd/3, render/1, fields/1, catalog/0]).

-import(aihtml_lib_display, [none_for/1]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Keyboard key. `Keys' is one key (`<<"Esc">>') or a list of keys
%% (`[<<"Ctrl">>, <<"K">>]'), rendered as nested `<kbd>'s joined by
%% the `separator' option (default "+").
-spec kbd(html() | [html()], css(), attrs()) -> #ah_kbd{}.
kbd(Keys, Css, Attrs) ->
    ?E:build(?MODULE, #ah_kbd{keys = Keys}, Css, Attrs).

%% @doc The field names of #ah_kbd{}.
-spec fields(atom()) -> [atom()].
fields(ah_kbd) -> record_info(fields, ah_kbd).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_kbd{}) -> html().
render(#ah_kbd{keys = Keys, size = Size, separator = Separator} = R) ->
    Cls = ?E:classes(?MODULE, R),
    [Root | Literal] = Cls,
    case is_list(Keys) andalso Keys =/= [] andalso not io_lib:printable_unicode_list(Keys) of
        false ->
            ?H:el(kbd, Keys, [Root | Literal], [[{data_size, Size}], ?E:root_attrs(R, none)]);
        true ->
            Sep = ?H:el(span, Separator, [<<"ah-kbd-combo__sep">>],
                        [{aria_hidden, <<"true">>}]),
            Ks = [?H:el(kbd, K, [Root], [{data_size, Size}]) || K <- Keys],
            ?H:el(kbd, lists:join(Sep, Ks), [<<"ah-kbd-combo">> | Literal],
                  [[{data_size, Size}], ?E:root_attrs(R, none)])
    end.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => kbd, category => text, root => <<"ah-kbd">>,
      signature => <<"kbd(Keys, Css, Attrs)">>,
      groups => #{size => {[md, lg], md}},
      classes => none_for([md, lg]), options => [separator],
      doc => <<"Keyboard key, or a key combination when Keys is a list.">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{separator => <<"Text between the keys of a combination (default \"+\").">>},
      methods => []}.
