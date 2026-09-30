%%%-------------------------------------------------------------------
%%% @doc sigil's declarative Form (sigil.components.form.form), rendered
%%% on the server.
%%%
%%%   ah_form_layout(Fields, Values, Css, Attrs)
%%%
%%% == Fields ==
%%%
%%% Fields is a list of rows, each one of
%%%
%%%   {Label, Control}                a labelled row
%%%   #{label => Label, control => Control, key => Key,
%%%     for => Id, help => Text, error => Text, required => true,
%%%     info => Text, label_position => top, label_width => 120,
%%%     hidden => true}
%%%   {columns, [Field]}              several fields on one row
%%%   {text, Text}                    a line of static text
%%%   blank | {blank, HeightPx}       vertical space
%%%
%%% Control is any html(), or a fun((Value) -> html()) that receives
%%% `maps:get(Key, Values, undefined)', so one Values map fills the form.
%%% The rows share ah_field/4's markup (aihtml_lib_form); controls given
%%% aihtml_field:validate/1 are checked on submit.
%%%
%%% The component function builds an #ah_form_layout{} record (include/
%%% aihtml_form_layout.hrl) and render/1 turns it into HTML, so pages may
%%% also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_form_layout).
-behaviour(aihtml_element).

-include("aihtml_form_layout.hrl").

-export([ah_form_layout/4, render/1, fields/1, catalog/0]).

-export_type([field_spec/0, control/0]).

-define(E, aihtml_element).
-define(F, aihtml_lib_form).

-type control() :: aihtml_html:html() | fun((term()) -> aihtml_html:html()).
-type field_spec() :: {aihtml_html:html(), control()}
                    | #{label => aihtml_html:html(), control := control(), atom() => term()}
                    | {columns, [field_spec()]}
                    | {text, aihtml_html:html()}
                    | blank | {blank, pos_integer()}.

%% @doc sigil's declarative Form rendered on the server: rows of labelled
%% controls (see the module doc for the field shapes). The root is a
%% `<form>' (option `tag => div' for a plain container); Attrs go to it.
-spec ah_form_layout([field_spec()], #{term() => term()}, aihtml_html:css(),
                     aihtml_html:attrs()) -> #ah_form_layout{}.
ah_form_layout(Fields, Values, Css, Attrs) ->
    ?E:build(?MODULE, #ah_form_layout{fields = Fields, values = Values}, Css, Attrs).

-spec render(#ah_form_layout{}) -> aihtml_html:html().
render(#ah_form_layout{fields = Fields, values = Values, tag = Tag} = R) ->
    Global = maps:from_list([{K, V} || {K, V} <- [{label_position, R#ah_form_layout.label_position},
                                                  {label_width, R#ah_form_layout.label_width}],
                                       V =/= undefined]),
    Rows = [form_row(F, Values, Global) || F <- Fields],
    Pad = case R#ah_form_layout.padding of
              {T, Rt, B, L} -> [?F:px(T), $\s, ?F:px(Rt), $\s, ?F:px(B), $\s, ?F:px(L)];
              P -> ?F:px(P)
          end,
    el(Tag, Rows, ?E:classes(?MODULE, R),
       [[{style, iolist_to_binary([<<"padding:">>, Pad])}],
        ?E:root_attrs(R, case Tag of form -> submit; _ -> none end)]).

form_row(blank, _Values, _G) -> form_row({blank, 16}, _Values, _G);
form_row({blank, H}, _Values, _G) ->
    el('div', [], [<<"ah-form-row">>, <<"ah-form-row-blank">>],
       [{style, iolist_to_binary([<<"height:">>, ?F:px(H)])}, {aria_hidden, <<"true">>}]);
form_row({text, Text}, _Values, _G) ->
    el('div', el('div', Text, [<<"ah-form-label-text">>], []),
       [<<"ah-form-row">>, <<"ah-form-row-label">>], []);
form_row({columns, Cols}, Values, G) ->
    el('div', [form_cell(<<"ah-form-col">>, C, Values, G) || C <- Cols],
       [<<"ah-form-row">>, <<"ah-form-columns">>], []);
form_row(F, Values, G) ->
    form_cell(<<"ah-form-row">>, F, Values, G).

form_cell(Class, {Label, Control}, Values, G) ->
    form_cell(Class, #{label => Label, control => Control}, Values, G);
form_cell(Class, #{control := Control0} = F, Values, G) ->
    Opts = maps:merge(G, F),
    Control = case Control0 of
                  Fun when is_function(Fun, 1) ->
                      Fun(maps:get(maps:get(key, F, undefined), Values, undefined));
                  Html -> Html
              end,
    Pos = case maps:get(label_position, Opts, left) of
              left -> [];
              P when P =:= top; P =:= right; P =:= bottom ->
                  [<<"ah-form-row-", (atom_to_binary(P))/binary>>];
              P -> error({aihtml, {bad_label_position, P}})
          end,
    el('div', ?F:row_body(maps:get(label, F, undefined), Control, Opts),
       [Class, Pos, [<<"ah-form-row-invalid">> || maps:is_key(error, F)]],
       [{data_ah_key, case maps:find(key, F) of {ok, K} -> ?F:bin(K); error -> undefined end},
        {hidden, maps:get(hidden, F, false)}]);
form_cell(_Class, Other, _Values, _G) ->
    error({aihtml, {bad_form_field, Other}}).

%%%===================================================================
%%% Record and catalog
%%%===================================================================

%% @doc The field names of #ah_form_layout{}.
-spec fields(atom()) -> [atom()].
fields(ah_form_layout) -> record_info(fields, ah_form_layout).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => form_layout, category => form,
       signature => <<"ah_form_layout(Fields, Values, Css, Attrs)">>,
       root => <<"ah-form">>,
       flags => [bordered, bg, disabled],
       options => [label_position, label_width, padding, tag],
       option_docs => #{bordered => <<"A border round the form.">>,
                        bg => <<"Paper background.">>,
                        disabled => <<"Dim the form and ignore the pointer.">>,
                        label_position => <<"Default label position of every field: left | top | right | bottom.">>,
                        label_width => <<"Default label width, px or CSS length.">>,
                        padding => <<"Inner padding in px, or {Top, Right, Bottom, Left} (default 10).">>,
                        tag => <<"form (default) or div.">>},
       methods => [],
       doc => <<"sigil's declarative form, rendered on the server. Fields: {Label, Control} "
                "| #{label, control, key, for, help, error, required, info, label_position, "
                "label_width, hidden} | {columns, [Field]} | {text, Text} | blank | "
                "{blank, Px}. A control may be fun(Value) -> html(), called with "
                "maps:get(Key, Values, undefined). Options: label_position (left | top | "
                "right | bottom), label_width, padding (px or {T,R,B,L}), tag (form | div).">>}].

%%%===================================================================
%%% Internal
%%%===================================================================

el(Tag, Children, Css, Attrs) -> aihtml_html:el(Tag, Children, Css, Attrs).
