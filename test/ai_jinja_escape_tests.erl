%%%-------------------------------------------------------------------
%%% Autoescape and the safe marker.
%%%
%%% The @safe annotation on every filter is checked mechanically here.
%%% Getting one of those wrong is an XSS, which is not something to leave to
%%% review.
%%%-------------------------------------------------------------------
-module(ai_jinja_escape_tests).

-include_lib("eunit/include/eunit.hrl").

r(T)     -> ai_jinja:render_string(T, #{}, #{}).
r(T, C)  -> ai_jinja:render_string(T, C, #{}).

%%%===================================================================
%%% The meta-test
%%%===================================================================

%% Every exported filter must declare how it treats the safe marker, in one of
%% exactly four ways. A filter that grows without one is a filter nobody has
%% thought about, and that is how an escaping bug gets in.
every_filter_declares_its_safe_class_test() ->
    Src = read_source("src/ai_jinja_filters.erl"),
    Classes = ["transparent", "escaping", "generating", "none"],
    Missing = [N || N <- maps:keys(ai_jinja_filters:filters()),
                    not has_annotation(Src, N, Classes)],
    ?assertEqual([], Missing).

has_annotation(Src, Name, Classes) ->
    %% The annotation sits in the comment block immediately above the spec.
    Marker = "-spec " ++ atom_to_list(Name) ++ "(",
    case string:split(Src, Marker) of
        [Before, _] -> lists:any(fun(C) -> ends_with_annotation(Before, C) end,
                                 Classes);
        _           -> false
    end.

ends_with_annotation(Before, Class) ->
    Tag = "@safe " ++ Class,
    case string:find(Before, Tag, trailing) of
        nomatch -> false;
        Tail    -> %% nothing but comment lines between the tag and the spec
                   Between = string:slice(Tail, length(Tag)),
                   lists:all(fun(Line) ->
                                     T = string:trim(Line),
                                     T =:= "" orelse lists:prefix("%%", T)
                             end, string:split(Between, "\n", all))
    end.

read_source(Rel) ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok, Bin} = file:read_file(find_up(Dir, Rel, 6)),
    unicode:characters_to_list(Bin).

find_up(_Dir, Rel, 0) -> erlang:error({not_found, Rel});
find_up(Dir, Rel, N) ->
    C = filename:join(Dir, Rel),
    case filelib:is_regular(C) of
        true  -> C;
        false -> find_up(filename:dirname(Dir), Rel, N - 1)
    end.

%%%===================================================================
%%% Behaviour
%%%===================================================================

autoescape_is_on_by_default_test() ->
    ?assertEqual(<<"&lt;b&gt;">>, r(<<"{{ x }}">>, #{x => <<"<b>">>})).

escape_can_be_turned_off_test() ->
    ?assertEqual(<<"<b>">>,
                 ai_jinja:render_string(<<"{{ x }}">>, #{x => <<"<b>">>},
                                        #{escape => false})).

safe_marker_survives_transparent_filters_test() ->
    ?assertEqual(<<"<B>">>, r(<<"{{ '<b>'|safe|upper }}">>)),
    ?assertEqual(<<"&lt;B&gt;">>, r(<<"{{ '<b>'|upper }}">>)).

forceescape_breaks_the_marker_test() ->
    ?assertEqual(<<"<b>">>, r(<<"{{ '<b>'|safe }}">>)),
    ?assertEqual(<<"&lt;b&gt;">>, r(<<"{{ '<b>'|safe|forceescape }}">>)).

%% A {% filter %} or {% set %} block wraps output that has ALREADY been
%% rendered -- literal text is never escaped, and an interpolation inside it
%% has been escaped once. Escaping the block again gives `&amp;amp;', which is
%% the classic symptom.
block_bodies_are_not_escaped_twice_test() ->
    ?assertEqual(<<"A & B">>, r(<<"{% filter upper %}a & b{% endfilter %}">>)),
    ?assertEqual(<<"a & b">>, r(<<"{% set v %}a & b{% endset %}{{ v }}">>)),
    %% An interpolation inside the block is escaped exactly once.
    ?assertEqual(<<"a &amp; b">>,
                 r(<<"{% set v %}{{ x }}{% endset %}{{ v }}">>, #{x => <<"a & b">>})),
    ?assertEqual(<<"A &AMP; B">>,
                 r(<<"{% filter upper %}{{ x }}{% endfilter %}">>,
                   #{x => <<"a & b">>})).

macro_output_is_safe_test() ->
    ?assertEqual(<<"<b>">>, r(<<"{% macro m() %}<b>{% endmacro %}{{ m() }}">>)),
    %% ... but what the macro interpolates is still escaped.
    ?assertEqual(<<"&lt;i&gt;">>,
                 r(<<"{% macro m(v) %}{{ v }}{% endmacro %}{{ m('<i>') }}">>)).

super_and_include_output_is_safe_test() ->
    ?assertEqual(<<"[<b>]">>,
                 ai_jinja:render_string(
                   <<"{% extends \"b.j2\" %}{% block t %}[{{ super() }}]{% endblock %}">>,
                   #{},
                   #{templates => #{<<"b.j2">> => <<"{% block t %}<b>{% endblock %}">>}})).

tojson_escapes_for_html_test() ->
    ?assertEqual(<<"{\"a\": \"\\u003c/script\\u003e\"}">>,
                 r(<<"{{ {'a': '</script>'}|tojson }}">>)).
