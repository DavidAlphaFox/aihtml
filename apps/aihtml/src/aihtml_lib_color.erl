%%%-------------------------------------------------------------------
%%% @doc Internal colour helpers of the display components: the theme
%%% colour atoms and colours for inline styles. Not part of the public API.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_color).

-export([theme_colors/0, css/1]).

-export_type([theme_color/0, css_color/0]).

-type theme_color() :: primary | secondary | success | warning | error | info.
%% A theme colour atom or a CSS colour (#hex, rgb(...), a name).
-type css_color() :: theme_color() | iodata().

%% @doc The theme colour atoms, the modifiers of a `color' group.
-spec theme_colors() -> [theme_color()].
theme_colors() -> [primary, secondary, success, warning, error, info].

%% @doc A colour for an inline style: a theme colour atom becomes its
%% custom property, anything else must look like a plain CSS colour.
-spec css(css_color()) -> binary().
css(A) when is_atom(A) ->
    lists:member(A, [primary, secondary, success, warning, error, info])
        orelse error({aihtml, {bad_color, A}}),
    <<"var(--ah-color-", (atom_to_binary(A))/binary, ")">>;
css(C) ->
    B = aihtml_lib_display:to_bin(C),
    Ok = B =/= <<>> andalso
        lists:all(fun(X) -> (X >= $a andalso X =< $z) orelse (X >= $A andalso X =< $Z)
                                orelse (X >= $0 andalso X =< $9)
                                orelse lists:member(X, "#(),.% -") end,
                  binary_to_list(B)),
    Ok orelse error({aihtml, {bad_color, C}}),
    B.
