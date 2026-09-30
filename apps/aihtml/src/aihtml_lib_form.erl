%%%-------------------------------------------------------------------
%%% @doc Internal helpers shared by the form components: the labelled row
%%% of ah_field/4 and ah_form_layout/4 (sigil's form markup), and the value and
%%% size formatting of the selection controls (dropdownlist, select,
%%% slider, field, form_layout). Not part of the public API.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_form).

-export([row_body/3, bin/1, text/1, num/1, tidy/1, px/1, css_size/1]).

-export_type([size/0, label_position/0, row_opts/0]).

%% px (integer) or a CSS length
-type size() :: integer() | binary().
-type label_position() :: left | top | right | bottom.
%% The row options ah_field/4 and the form_layout rows share (a form_layout
%% field map may hold more keys).
-type row_opts() :: #{for => term(), help => aihtml_html:html(),
                      error => aihtml_html:html(), required => boolean(),
                      info => aihtml_html:html(), label_width => size(),
                      atom() => term()}.

%%%===================================================================
%%% Form rows
%%%===================================================================

%% @doc The label and body of one form row: the label (with `for',
%% `required', `label_width'), the control with an optional info icon,
%% and a help or error line under it.
-spec row_body(undefined | aihtml_html:html(), aihtml_html:html(), row_opts()) ->
          aihtml_html:html().
row_body(Label, Control, Opts) ->
    [label(Label, Opts),
     el('div',
        [el('div', [el('div', Control, [], []),
                    case maps:find(info, Opts) of
                        {ok, Info} -> el('div', <<"ⓘ"/utf8>>, [<<"ah-form-info">>],
                                         [{title, text(Info)}, {aria_label, text(Info)}]);
                        error -> []
                    end],
            [<<"ah-form-field">>], []),
         case maps:find(help, Opts) of
             {ok, Help} -> el('div', Help, [<<"ah-form-help">>], []);
             error -> []
         end,
         case maps:find(error, Opts) of
             {ok, Err} -> el('div', Err, [<<"ah-form-error">>, <<"ah-validator-error-label">>],
                             [{role, alert}]);
             error -> []
         end],
        [<<"ah-form-body">>], [])].

label(undefined, _Opts) -> [];
label(Label, Opts) ->
    Style = case maps:find(label_width, Opts) of
                {ok, W} -> S = css_size(W), [<<"width:">>, S, <<";min-width:">>, S];
                error -> undefined
            end,
    el(label, [el(span, Label, [], []),
               case maps:get(required, Opts, false) of
                   true -> el(span, <<"*">>, [<<"ah-form-required">>], [{aria_hidden, <<"true">>}]);
                   false -> []
               end],
       [<<"ah-form-label">>],
       [{for, maps:get(for, Opts, undefined)},
        {style, case Style of undefined -> undefined; _ -> iolist_to_binary(Style) end}]).

%%%===================================================================
%%% Values and sizes
%%%===================================================================

%% @doc A value as text: binaries as they are, atoms by name, numbers as
%% num/1 writes them, strings as UTF-8.
-spec bin(binary() | atom() | number() | string()) -> binary().
bin(B) when is_binary(B) -> B;
bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(N) when is_number(N) -> num(N);
bin(L) when is_list(L) -> unicode:characters_to_binary(L).

%% @doc Label text for an attribute (title, label); non-text labels give "".
-spec text(term()) -> binary().
text(B) when is_binary(B) -> B;
text(A) when is_atom(A); is_number(A) -> bin(A);
text(L) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true -> unicode:characters_to_binary(L);
        false -> <<>>
    end;
text(_) -> <<>>.

%% @doc A number as short text: whole floats without a fraction.
-spec num(number()) -> binary().
num(I) when is_integer(I) -> integer_to_binary(I);
num(F) when is_float(F) ->
    case tidy(F) of
        I when is_integer(I) -> integer_to_binary(I);
        G -> float_to_binary(G, [short])
    end.

%% @doc A float rounded to 6 decimals, or the integer it is (almost) equal to.
-spec tidy(number()) -> number().
tidy(V) when is_float(V) ->
    R = round(V),
    case abs(V - R) < 1.0e-9 of
        true -> R;
        false -> round(V * 1.0e6) / 1.0e6
    end;
tidy(V) -> V.

%% @doc Pixels (an integer) or a CSS length, as iodata.
-spec px(size()) -> iodata().
px(N) when is_integer(N) -> [integer_to_binary(N), <<"px">>];
px(B) when is_binary(B) -> B.

%% @doc px/1 as a binary.
-spec css_size(size()) -> binary().
css_size(N) when is_integer(N) -> iolist_to_binary(px(N));
css_size(B) when is_binary(B) -> B.

el(Tag, Children, Css, Attrs) -> aihtml_html:el(Tag, Children, Css, Attrs).
