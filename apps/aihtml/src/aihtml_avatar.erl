%%%-------------------------------------------------------------------
%%% @doc The avatar component (designs/04-components.md): `avatar/3'
%%% builds an #ah_avatar{} element record (include/aihtml_avatar.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%%
%%% The DOM and class names follow sigil so the ported stylesheets in
%%% priv/css/sigil/components apply unchanged.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_avatar).
-behaviour(aihtml_element).

-include("aihtml_avatar.hrl").

-export([avatar/3, render/1, fields/1, catalog/0]).

-import(aihtml_lib_display, [blank/1, none_for/1]).

-define(H, aihtml_html).
-define(E, aihtml_element).
-define(COLORS, aihtml_lib_color:theme_colors()).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%%%===================================================================
%%% Builder
%%%===================================================================

%% @doc Avatar: an image with an initials (or icon) fallback that shows
%% when there is no `src' or the image fails to load. `Content' is the
%% fallback, `"?"' when empty.
-spec avatar(html(), css(), attrs()) -> #ah_avatar{}.
avatar(Content, Css, Attrs) ->
    ?E:build(?MODULE, #ah_avatar{body = Content}, Css, Attrs).

%% @doc The field names of #ah_avatar{}.
-spec fields(atom()) -> [atom()].
fields(ah_avatar) -> record_info(fields, ah_avatar).

%%%===================================================================
%%% Rendering
%%%===================================================================

-spec render(#ah_avatar{}) -> html().
render(#ah_avatar{body = Content, src = Src, alt = Alt} = R) ->
    Cls = ?E:classes(?MODULE, R),
    Img = case blank(Src) of
              true -> [];
              false -> ?H:void(img, [<<"ah-avatar__image">>], [{src, Src}, {alt, Alt}])
          end,
    Fallback = case blank(Content) of true -> <<"?">>; false -> Content end,
    %% Without an image the fallback is aria-hidden, so name the avatar.
    Named = blank(Src) andalso not blank(Alt),
    ?H:el(span,
          [Img, ?H:el(span, Fallback, [<<"ah-avatar__fallback">>], [{aria_hidden, <<"true">>}])],
          Cls,
          [[{data_size, R#ah_avatar.size}, {data_shape, R#ah_avatar.shape},
            {data_color, R#ah_avatar.color}, {data_ah, <<"avatar">>},
            {role, Named andalso <<"img">>}, {aria_label, Named andalso Alt}],
           ?E:root_attrs(R, none)]).

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [maps:merge(entry(), api())].

entry() ->
    #{name => avatar, category => media, root => <<"ah-avatar">>,
      signature => <<"avatar(Content, Css, Attrs)">>,
      groups => #{size => {[sm, md, lg, xl], md},
                  shape => {[circle, square, rounded], circle},
                  color => {?COLORS, primary}},
      classes => none_for([sm, md, lg, xl, circle, square, rounded | ?COLORS]),
      options => [src, alt], behavior => <<"avatar">>,
      doc => <<"Image avatar with an initials fallback (Content) when src is "
               "missing or fails to load.">>}.

%% The API tab of the docs page: options, flags and client methods.
api() ->
    #{option_docs => #{src => <<"Image URL; the fallback shows when it is missing or fails to load.">>,
                       alt => <<"Alt text of the image; without an image it becomes the aria-label.">>},
      methods => []}.
