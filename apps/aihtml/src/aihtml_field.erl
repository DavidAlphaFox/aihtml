%%%-------------------------------------------------------------------
%%% @doc One labelled form row in sigil's form markup, and client-side
%%% validation (sigil.components.form.{form, validator}).
%%%
%%%   ah_field(Label, Control, Css, Attrs)   one labelled form row
%%%   validate(Rules)                        client validation attrs
%%%
%%% validate/1 lives here because its messages show in ah_field/4 rows (and
%%% ah_form_layout/4 rows, which share the markup, see aihtml_lib_form); the
%%% aihtml facade re-exports it. The validator behaviour is in
%%% assets/js/components/field.ts.
%%%
%%% == validate/1 rules ==
%%%
%%%   required | email | number | integer | phone | zip_code | ssn
%%%   | not_number | starts_with_letter
%%%   | {min_length, N} | {max_length, N} | {length, Min, Max}
%%%   | {min, N} | {max, N} | {range, Min, Max}
%%%   | {pattern, Regex}        whole value must match (like HTML pattern)
%%%   | {same_as, Selector}     equal to another field (confirm password)
%%%   | {Rule, Message}         any of the above with its own message
%%%
%%% and these options in the same list:
%%%   {hint, auto | tooltip | label}   auto: label inside ah_field/4 rows,
%%%                                    sigil's tooltip bubble elsewhere
%%%   {position, right | left | top | bottom}   tooltip side (right)
%%%   {on, blur | input | change | [Event]}     when to check (blur)
%%%
%%% The component function builds an #ah_field{} record (include/
%%% aihtml_field.hrl) and render/1 turns it into HTML, so pages may also
%%% write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_field).
-behaviour(aihtml_element).

-include("aihtml_field.hrl").

-export([ah_field/4, validate/1, render/1, fields/1, catalog/0, facade_extras/0]).

-export_type([rule/0]).

-define(E, aihtml_element).
-define(F, aihtml_lib_form).

-type base_rule() :: atom() | {atom(), term()} | {atom(), term(), term()}.
-type rule() :: base_rule() | {base_rule(), binary()}.

-define(SIMPLE_RULES, [required, email, number, integer, phone, zip_code, ssn,
                       not_number, starts_with_letter]).
-define(ARG_RULES, [min_length, max_length, min, max, pattern, same_as]).

%%%===================================================================
%%% field
%%%===================================================================

%% @doc One form row in sigil's form markup: a label, the control, and an
%% optional help or error line under the control.
-spec ah_field(aihtml_html:html(), aihtml_html:html(), aihtml_html:css(),
               aihtml_html:attrs()) -> #ah_field{}.
ah_field(Label, Control, Css, Attrs) ->
    ?E:build(?MODULE, #ah_field{label = Label, body = Control}, Css, Attrs).

-spec render(#ah_field{}) -> aihtml_html:html().
render(#ah_field{label = Label, body = Control, error = Error} = R) ->
    %% the row options that row_body/3 and form_layout rows share
    Opts = maps:from_list([{K, V} || {K, V} <- [{for, R#ah_field.for},
                                                  {help, R#ah_field.help},
                                                  {error, Error},
                                                  {required, R#ah_field.required},
                                                  {info, R#ah_field.info},
                                                  {label_width, R#ah_field.label_width}],
                                     V =/= undefined]),
    aihtml_html:el('div', ?F:row_body(Label, Control, Opts),
                   [?E:classes(?MODULE, R), [<<"ah-form-row-invalid">> || Error =/= undefined]],
                   ?E:root_attrs(R, none)).

%%%===================================================================
%%% validate
%%%===================================================================

%% @doc Attributes that make a control validate on the client (sigil's
%% Validator). Put them in the control's Attrs, e.g.
%% `ah_input(..., [{name, email}, validate([required, email])])'. A form
%% holding such controls is checked on submit; if anything fails the
%% submit is stopped before actions (on/2) or data-ah-fetch see it.
-spec validate([rule() | {hint | position | on, term()}]) -> aihtml_html:attrs().
validate(Rules) ->
    {Options, RuleList} = lists:partition(
                            fun({K, _}) -> lists:member(K, [hint, position, on]);
                               (_) -> false
                            end, Rules),
    Json = [rule_json(R) || R <- RuleList],
    Opts = maps:from_list(Options),
    On = case maps:get(on, Opts, blur) of
             L when is_list(L) -> iolist_to_binary(lists:join(<<" ">>, [?F:bin(X) || X <- L]));
             X -> ?F:bin(X)
         end,
    [{data_ah_validate, iolist_to_binary(aihtml_json:encode(Json))},
     {data_ah_validate_on, On},
     {data_ah_validate_hint, atom_opt(hint, Opts, [auto, tooltip, label])},
     {data_ah_validate_position, atom_opt(position, Opts, [right, left, top, bottom])},
     {aria_required, aria(lists:any(fun(#{rule := R}) -> R =:= required end, Json))}].

atom_opt(K, Opts, Allowed) ->
    case maps:find(K, Opts) of
        error -> undefined;
        {ok, V} ->
            lists:member(V, Allowed) orelse error({aihtml, {bad_validate_option, K, V}}),
            atom_to_binary(V)
    end.

rule_json({R, Msg}) when is_binary(Msg), is_tuple(R) ->
    (rule_json(R))#{msg => Msg};
rule_json({R, Msg}) when is_binary(Msg), is_atom(R) ->
    case lists:member(R, ?SIMPLE_RULES) of
        true -> #{rule => R, msg => Msg};
        false -> arg_rule(R, [Msg])            % {pattern, Regex}, {same_as, Sel}
    end;
rule_json(R) when is_atom(R) ->
    lists:member(R, ?SIMPLE_RULES) orelse error({aihtml, {unknown_rule, R}}),
    #{rule => R};
rule_json({length, Min, Max}) when is_integer(Min), is_integer(Max) ->
    #{rule => length, args => [Min, Max]};
rule_json({range, Min, Max}) when is_number(Min), is_number(Max) ->
    #{rule => range, args => [Min, Max]};
rule_json({R, Arg}) when is_atom(R) ->
    arg_rule(R, [Arg]);
rule_json(Other) ->
    error({aihtml, {unknown_rule, Other}}).

arg_rule(R, [Arg]) ->
    lists:member(R, ?ARG_RULES) orelse error({aihtml, {unknown_rule, R}}),
    A = case R of
            pattern -> ?F:bin(Arg);
            same_as -> ?F:bin(Arg);
            _ when is_number(Arg) -> Arg;
            _ -> error({aihtml, {bad_rule_argument, R, Arg}})
        end,
    #{rule => R, args => [A]}.

aria(true) -> <<"true">>;
aria(false) -> undefined.

%% @doc validate/1 builds Attrs, so the aihtml facade re-exports it.
-spec facade_extras() -> [{atom(), arity()}].
facade_extras() -> [{validate, 1}].

%%%===================================================================
%%% Record and catalog
%%%===================================================================

%% @doc The field names of #ah_field{}.
-spec fields(atom()) -> [atom()].
fields(ah_field) -> record_info(fields, ah_field).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => field, category => form,
       signature => <<"ah_field(Label, Control, Css, Attrs)">>,
       root => <<"ah-form-row">>,
       groups => #{label_position => {[left, top, right, bottom], left}},
       classes => #{left => []},
       options => [for, help, error, required, info, label_width],
       option_docs => #{left => <<"Label left of the control (default).">>,
                        top => <<"Label above the control.">>,
                        right => <<"Label right of the control (checkboxes).">>,
                        bottom => <<"Label under the control.">>,
                        for => <<"Id of the control the label points to.">>,
                        help => <<"Help line under the control.">>,
                        error => <<"Error line under the control; marks the row invalid.">>,
                        required => <<"true: a red asterisk after the label.">>,
                        info => <<"Tooltip text of an info icon after the control.">>,
                        label_width => <<"Label width, px or CSS length.">>},
       methods => [],
       doc => <<"A labelled form row: label (with `for', `required', `label_width'), the "
                "control, and a `help' or `error' line under it; `info' adds a hint icon. "
                "Controls given validate(Rules) show their errors in this row; page "
                "functions AH.fn validate(Target) and clearValidation(Target).">>}].
