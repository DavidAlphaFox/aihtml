%%%-------------------------------------------------------------------
%%% @doc Prefab implementations. Use them through the `aihtml' facade.
%%%
%%% Conventions shared by every prefab:
%%%
%%%   * The last two arguments are always `Css' and `Attrs'.
%%%   * `Css' styles the prefab's root element. Atoms are semantic
%%%     modifiers checked by `aihtml_catalog'; binaries are literal
%%%     (Tailwind) classes appended after the semantic ones.
%%%   * `Attrs' override the prefab's own attributes. For a control wrapped
%%%     in a `<label>' (checkbox, radio, switch) they go to the native
%%%     `<input>', because that is where `name', `checked' and `required'
%%%     belong.
%%%   * A prefab with client behaviour marks its root `data-ah="<behavior>"';
%%%     priv/static/aihtml.js attaches to that marker.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_prefab).

-export([button/4, checkbox/4, radio/4, switch/4,
         input/3, textarea/3, select/4, field/4,
         card/3, alert/3, badge/3, tabs/4, theme_switcher/2]).

-import(aihtml_html, [el/4, void/3]).

-type html() :: aihtml_html:html().
-type css() :: aihtml_html:css().
-type attrs() :: aihtml_html:attrs().

%%%===================================================================
%%% Form
%%%===================================================================

-spec button(html(), term(), css(), attrs()) -> aihtml_html:element().
button(Content, Value, Css, Attrs) ->
    el(button, Content, cls(button, Css), [[{type, button}, {value, Value}], Attrs]).

-spec checkbox(html(), term(), css(), attrs()) -> aihtml_html:element().
checkbox(Content, Value, Css, Attrs) ->
    check(checkbox, checkbox, Content, Value, Css, Attrs, [], []).

-spec radio(html(), term(), css(), attrs()) -> aihtml_html:element().
radio(Content, Value, Css, Attrs) ->
    check(radio, radio, Content, Value, Css, Attrs, [], []).

-spec switch(html(), term(), css(), attrs()) -> aihtml_html:element().
switch(Content, Value, Css, Attrs) ->
    Track = el(span, el(span, [], [<<"ah-switch-thumb">>], []),
               [<<"ah-switch-track">>], [{aria_hidden, true}]),
    check(switch, checkbox, Content, Value, Css, Attrs, [{role, switch}], [Track]).

-spec input(term(), css(), attrs()) -> aihtml_html:element().
input(Value, Css, Attrs) ->
    void(input, cls(input, Css),
         [[{type, text}, {value, Value}], invalid(input, Css), Attrs]).

-spec textarea(term(), css(), attrs()) -> aihtml_html:element().
textarea(Value, Css, Attrs) ->
    el(textarea, text(Value), cls(textarea, Css), [invalid(textarea, Css), Attrs]).

-spec select([{term(), html()} | term()], term(), css(), attrs()) ->
          aihtml_html:element().
select(Options, Value, Css, Attrs) ->
    Selected = text(Value),
    Opts = [begin
                {V, Label} = case O of
                                 {_, _} -> O;
                                 _      -> {O, O}
                             end,
                VB = text(V),
                el(option, Label, [], [{value, VB}, {selected, VB =:= Selected}])
            end || O <- Options],
    el(select, Opts, cls(select, Css), [invalid(select, Css), Attrs]).

-spec field(html(), html(), css(), attrs()) -> aihtml_html:element().
field(Label, Control, Css, Attrs) ->
    {Opts, Rest} = aihtml_catalog:split_options(field, Attrs),
    Error = maps:get(error, Opts, undefined),
    Help = maps:get(help, Opts, undefined),
    Children =
        [el(label, Label, [<<"ah-field-label">>], [{for, maps:get(for, Opts, undefined)}]),
         Control,
         [el(p, Help, [<<"ah-field-help">>], []) || Help =/= undefined, Error =:= undefined],
         [el(p, Error, [<<"ah-field-error">>], [{role, alert}]) || Error =/= undefined]],
    Invalid = [<<"ah-field-invalid">> || Error =/= undefined],
    el('div', Children, cls(field, [Css, Invalid]), Rest).

%%%===================================================================
%%% Display
%%%===================================================================

-spec card(html(), css(), attrs()) -> aihtml_html:element().
card(Children, Css, Attrs) ->
    {Opts, Rest} = aihtml_catalog:split_options(card, Attrs),
    Header = case Opts of
                 #{title := T} ->
                     [el('div', el(h3, T, [<<"ah-card-title">>], []),
                         [<<"ah-card-header">>], [])];
                 #{} -> []
             end,
    el('div', [Header, el('div', Children, [<<"ah-card-body">>], [])],
       cls(card, Css), Rest).

-spec alert(html(), css(), attrs()) -> aihtml_html:element().
alert(Children, Css, Attrs) ->
    Close = [el(button, <<"×"/utf8>>, [<<"ah-alert-close">>],
                [{type, button}, {aria_label, <<"Close">>}, {data_ah_dismiss, true}])
             || lists:member(dismissible, aihtml_catalog:flags(alert, Css))],
    el('div', [el('div', Children, [<<"ah-alert-body">>], []), Close],
       cls(alert, Css), [behavior(alert), [{role, alert}], Attrs]).

-spec badge(html(), css(), attrs()) -> aihtml_html:element().
badge(Children, Css, Attrs) ->
    el(span, Children, cls(badge, Css), Attrs).

%% @doc `Tabs' is `[{Key, Label, Panel}]'; `Active' is the key shown first
%% (the first tab when `undefined'). Give the root an `id' in `Attrs' for
%% stable ids; otherwise one is generated.
-spec tabs([{term(), html(), html()}], term(), css(), attrs()) -> aihtml_html:element().
tabs(Tabs, Active0, Css, Attrs) ->
    Id = case lists:keyfind(<<"id">>, 1, aihtml_html:attrs(Attrs)) of
             {_, I} -> I;
             false -> <<"ah-tabs-", (integer_to_binary(
                                        erlang:unique_integer([positive])))/binary>>
         end,
    Active = case {Active0, Tabs} of
                 {undefined, [{K, _, _} | _]} -> text(K);
                 _ -> text(Active0)
             end,
    List = [begin
                KB = text(K),
                On = KB =:= Active,
                el(button, Label, [<<"ah-tab">>],
                   [{type, button}, {role, tab}, {id, <<Id/binary, "-tab-", KB/binary>>},
                    {aria_controls, <<Id/binary, "-panel-", KB/binary>>},
                    {aria_selected, atom_to_binary(On, utf8)},
                    {tabindex, case On of true -> 0; false -> -1 end},
                    {data_ah_tab, KB}])
            end || {K, Label, _} <- Tabs],
    Panels = [begin
                  KB = text(K),
                  el('div', Panel, [<<"ah-tab-panel">>],
                     [{role, tabpanel}, {id, <<Id/binary, "-panel-", KB/binary>>},
                      {aria_labelledby, <<Id/binary, "-tab-", KB/binary>>},
                      {data_ah_panel, KB}, {hidden, KB =/= Active}])
              end || {K, _, Panel} <- Tabs],
    el('div', [el('div', List, [<<"ah-tabs-list">>], [{role, tablist}]), Panels],
       cls(tabs, Css), [behavior(tabs), [{id, Id}], Attrs]).

%%%===================================================================
%%% Theme
%%%===================================================================

-spec theme_switcher(css(), attrs()) -> aihtml_html:element().
theme_switcher(Css, Attrs) ->
    Items = [el(label,
                [el(span, atom_to_binary(Axis, utf8), [<<"ah-theme-switcher-label">>], []),
                 select(Values, Default, [sm], [{data_ah_axis, Axis}, {aria_label, Axis}])],
                [<<"ah-theme-switcher-item">>], [])
             || {Axis, _Attr, Values, Default} <- aihtml_theme:axes()],
    el('div', Items, cls(theme_switcher, Css), [behavior(theme_switcher), Attrs]).

%%%===================================================================
%%% Internal
%%%===================================================================

check(Name, Type, Content, Value, Css, Attrs, Extra, After) ->
    #{root := Root} = aihtml_catalog:prefab(Name),
    Input = void(input, [<<Root/binary, "-input">>],
                 [[{type, Type}, {value, Value}], Extra, Attrs]),
    el(label, [Input, After, el(span, Content, [<<Root/binary, "-label">>], [])],
       cls(Name, Css), behavior(Name)).

cls(Name, Css) -> aihtml_catalog:classes(Name, Css).

behavior(Name) ->
    case aihtml_catalog:prefab(Name) of
        #{behavior := none} -> [];
        #{behavior := B} -> [{data_ah, B}]
    end.

invalid(Name, Css) ->
    [{aria_invalid, <<"true">>} || lists:member(invalid, aihtml_catalog:flags(Name, Css))].

text(V) -> beamai_html_escape:to_binary(V, aihtml).
