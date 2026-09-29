%%%-------------------------------------------------------------------
%%% @doc Internal helpers shared by the display components (avatar,
%%% badge, chip, aspect_ratio, kbd, time_ago, expandable_text, alert,
%%% progressbar, progress_circle, meter, statistic, kpi_card, timeline,
%%% ranking_list, tag_cloud): value tests and formatting, catalog helpers
%%% and the stroke icons of alert and kpi_card. Not part of the public API.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_display).

-export([tf/1, blank/1, num/1, to_bin/1, none_for/1, method/3,
         svg/1, circle/3, line/4, polyline/1, path/1]).

-define(H, aihtml_html).

%% @doc A boolean as a data-* attribute value.
-spec tf(boolean()) -> binary().
tf(true) -> <<"true">>;
tf(false) -> <<"false">>.

%% @doc Whether an optional value is missing or empty.
-spec blank(term()) -> boolean().
blank(undefined) -> true;
blank(null) -> true;
blank(<<>>) -> true;
blank([]) -> true;
blank(_) -> false.

%% @doc A number for CSS or SVG: integers as is, floats with at most four
%% decimals and no trailing zeros.
-spec num(number()) -> binary().
num(N) when is_integer(N) -> integer_to_binary(N);
num(F) when is_float(F) ->
    case float_to_binary(F, [{decimals, 4}, compact]) of
        <<"-0.0">> -> <<"0">>;
        B -> case binary:split(B, <<".0">>) of
                 [I, <<>>] -> I;
                 _ -> B
             end
    end.

%% @doc A binary, atom, number or character list as a binary.
-spec to_bin(binary() | atom() | number() | unicode:chardata()) -> binary().
to_bin(B) when is_binary(B) -> B;
to_bin(A) when is_atom(A) -> atom_to_binary(A);
to_bin(I) when is_integer(I) -> integer_to_binary(I);
to_bin(F) when is_float(F) -> num(F);
to_bin(L) when is_list(L) -> unicode:characters_to_binary(L).

%% @doc Catalog `classes' mapping each modifier to no class (the component
%% writes a data-* attribute instead).
-spec none_for([atom()]) -> #{atom() => []}.
none_for(Mods) -> maps:from_list([{M, []} || M <- Mods]).

%% @doc A catalog `methods' entry.
-spec method(atom(), binary(), binary()) -> #{name := atom(), args := binary(), doc := binary()}.
method(Name, Args, Doc) -> #{name => Name, args => Args, doc => Doc}.

%% @doc A 20px stroke icon on a 24 x 24 grid.
-spec svg(aihtml_html:html()) -> aihtml_html:html().
svg(Shapes) ->
    ?H:el(svg, Shapes, [],
          [{xmlns, <<"http://www.w3.org/2000/svg">>}, {width, 20}, {height, 20},
           {<<"viewBox">>, <<"0 0 24 24">>}, {fill, none}, {stroke, <<"currentColor">>},
           {stroke_width, 2}, {stroke_linecap, round}, {stroke_linejoin, round}]).

-spec circle(number(), number(), number()) -> aihtml_html:html().
circle(Cx, Cy, R) -> ?H:el(circle, [], [], [{cx, Cx}, {cy, Cy}, {r, R}]).

-spec line(number(), number(), number(), number()) -> aihtml_html:html().
line(X1, Y1, X2, Y2) -> ?H:el(line, [], [], [{x1, X1}, {y1, Y1}, {x2, X2}, {y2, Y2}]).

-spec polyline(binary()) -> aihtml_html:html().
polyline(Points) -> ?H:el(polyline, [], [], [{points, Points}]).

-spec path(binary()) -> aihtml_html:html().
path(D) -> ?H:el(path, [], [], [{d, D}]).
