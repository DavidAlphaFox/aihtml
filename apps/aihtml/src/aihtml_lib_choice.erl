%%%-------------------------------------------------------------------
%%% @doc Internal: what the choice components share (aihtml_checkbox,
%%% aihtml_radiobutton, aihtml_switch_button, aihtml_checkbox_group,
%%% aihtml_radiobutton_group, aihtml_radio_cards, aihtml_rating_group).
%%%
%%% The single controls wrap a real, visually hidden `<input>' in a
%%% `<label>' that carries sigil's markup; the groups render one native
%%% input per item. Group items are
%%%   Value | {Value, Label} | {Value, Label, Opts}
%%% with Opts (a map or proplist): disabled, class; radio_cards also
%%% description and icon (html).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_choice).

-export([single_input/4, group_attrs/5, input/4, check_box/3, radio_box/2,
         label_span/2, group/12, norm_items/1, enumerate/1, opt/2, opt_class/1,
         selected_value/2, join_values/1, check_opt/4, to_bin/1, opt_bin/1, num/1,
         bool/1, state/2, truthy/1, attr/2,
         set_checked/1, get_value/1, set_disabled/0, set_value/1, group_get_value/1,
         group_set_disabled/0, group_api/1]).

-export_type([value/0, item/0, size/0, layout/0, name/0]).

-type value() :: binary() | atom() | integer() | float() | string().
%% Value | {Value, Label} | {Value, Label, Opts}. Opts (a map or proplist):
%% disabled, class; radio_cards also description and icon (html).
-type item() :: value() | {value(), aihtml_html:html()}
              | {value(), aihtml_html:html(), map() | list()}.
-type size() :: sm | md | lg.
-type layout() :: vertical | horizontal.
-type name() :: undefined | atom() | iodata().

-define(INPUT_CLASS, <<"ah-choice-input">>).
%% Attributes of a group that go to every native input instead of the root.
-define(INPUT_ATTRS, [<<"name">>, <<"disabled">>, <<"required">>, <<"form">>]).

%%%===================================================================
%%% Attributes
%%%===================================================================

%% @doc The native input's attributes of a single control: the `attrs'
%% field, then disabled / checked, then id and postback (`Bare' is the
%% record without its attrs). A binary "checked" / "disabled" key in attrs
%% still counts, as it did before these were fields.
-spec single_input(aihtml_html:attrs(), boolean(), boolean(), aihtml_element:element()) ->
          {boolean(), boolean(), list()}.
single_input(Attrs0, Checked, Disabled, Bare) ->
    Attrs = flat(Attrs0),
    {truthy(attr(<<"checked">>, [{checked, Checked} | Attrs])),
     truthy(attr(<<"disabled">>, [{disabled, Disabled} | Attrs])),
     [Attrs, {disabled, Disabled}, {checked, Checked}, aihtml_element:root_attrs(Bare, change)]}.

%% @doc A group's attributes for every input (the name / disabled /
%% required / form fields, then such keys left in attrs) and for the root
%% (the rest).
-spec group_attrs(aihtml_html:attrs(), name(), boolean(), boolean(), name()) ->
          {list(), list()}.
group_attrs(Attrs, Name, Disabled, Required, Form) ->
    {Input, Root} = split_attrs(?INPUT_ATTRS, Attrs),
    {[{name, Name}, {disabled, Disabled}, {required, Required}, {form, Form} | Input], Root}.

%%%===================================================================
%%% Shared markup
%%%===================================================================

%% @doc The native input. Attrs from the caller come after type/value so
%% that name, id, checked, on/2 ... apply; Forced wins over both.
-spec input(checkbox | radio, value() | undefined, term(), term()) -> aihtml_html:html().
input(Type, Value, Attrs, Forced) ->
    aihtml_html:void(input, [?INPUT_CLASS],
                     [{type, Type}, {value, opt_bin(Value)}, Attrs, Forced,
                      {type, Type}]).

-spec check_box(boolean(), boolean(), undefined | pos_integer()) -> aihtml_html:html().
check_box(Checked, Indet, BoxSize) ->
    Check = [<<"ah-checkbox-check">>,
             state(Checked andalso not Indet, <<"ah-checkbox-check-checked">>),
             state(Indet, <<"ah-checkbox-check-indeterminate">>)],
    aihtml_html:el(span, aihtml_html:el(span, [], Check, []),
                   [<<"ah-checkbox-box">>], [{style, box_style(BoxSize)}, {aria_hidden, <<"true">>}]).

-spec radio_box(boolean(), undefined | pos_integer()) -> aihtml_html:html().
radio_box(Checked, BoxSize) ->
    Check = [<<"ah-radiobutton-check">>, state(Checked, <<"ah-radiobutton-check-checked">>)],
    aihtml_html:el(span, aihtml_html:el(span, [], Check, []),
                   [<<"ah-radiobutton-box">>], [{style, box_style(BoxSize)}, {aria_hidden, <<"true">>}]).

box_style(undefined) -> undefined;
box_style(N) when is_integer(N), N > 0 ->
    Px = [integer_to_binary(N), <<"px">>],
    iolist_to_binary([<<"width:">>, Px, <<";height:">>, Px, <<";">>]);
box_style(N) -> error({aihtml, {bad_option, box_size, N}}).

-spec label_span(binary(), aihtml_html:html()) -> aihtml_html:html().
label_span(_Class, undefined) -> [];
label_span(_Class, <<>>) -> [];
label_span(_Class, []) -> [];
label_span(Class, Content) -> aihtml_html:el(span, Content, [Class], []).

%% @doc checkbox_group and radiobutton_group: sigil's group markup, one
%% item per option, each wrapping the inner control (without its own
%% label) and the item label. `Classes' are the root's catalog classes,
%% `RootAttrs' its id, postback and attrs.
-spec group(checkbox | radio, binary(), binary(), binary(), [item()],
            fun((binary()) -> boolean()), binary(), undefined | size(), boolean(),
            aihtml_html:css(), list(), aihtml_html:attrs()) -> aihtml_html:html().
group(Kind, Prefix, Role, Behavior, Items, IsOn, DataValue, Size, Before0, Classes,
      InputAttrs, RootAttrs) ->
    GroupDisabled = truthy(attr(<<"disabled">>, InputAttrs)),
    Before = Before0 =:= true,
    Inner = case Kind of
                checkbox -> <<"ah-checkbox">>;
                radio -> <<"ah-radiobutton">>
            end,
    ItemEls =
        [begin
             D = GroupDisabled orelse truthy(opt(disabled, IOpts)),
             On = IsOn(V),
             Box = case Kind of checkbox -> check_box(On, false, undefined);
                                radio -> radio_box(On, undefined)
                   end,
             Control = aihtml_html:el(span,
                 [input(Kind, V, InputAttrs, [{checked, On}, {disabled, D}]), Box],
                 [Inner, size_class(Inner, Size), state(D, <<Inner/binary, "-disabled">>),
                  state(On, <<Inner/binary, "-checked">>)], []),
             Lbl = aihtml_html:el(span, Label, [<<Prefix/binary, "-label">>], []),
             aihtml_html:el(label,
                 case Before of true -> [Lbl, Control]; false -> [Control, Lbl] end,
                 [<<Prefix/binary, "-item">>, opt_class(IOpts),
                  state(D, <<Prefix/binary, "-item-disabled">>)],
                 [{data_value, V}, {data_index, I - 1},
                  {data_ah_item_disabled, truthy(opt(disabled, IOpts))}])
         end || {I, {V, Label, IOpts}} <- enumerate(norm_items(Items))],
    aihtml_html:el('div', ItemEls,
        [Classes, state(GroupDisabled, <<Prefix/binary, "-disabled">>)],
        [{data_ah, Behavior}, {role, Role}, {data_ah_value, DataValue},
         {data_label_position, case Before of true -> before; false -> 'after' end},
         {aria_disabled, GroupDisabled andalso <<"true">>},
         RootAttrs]).

size_class(_Inner, undefined) -> [];
size_class(Inner, S) ->
    <<Inner/binary, "-", (atom_to_binary(S, utf8))/binary>>.

%%%===================================================================
%%% Values and attributes
%%%===================================================================

-spec norm_items([item()]) -> [{binary(), aihtml_html:html(), map()}].
norm_items(Items) -> [norm_item(I) || I <- Items].

norm_item({V, L}) -> {to_bin(V), L, #{}};
norm_item({V, L, O}) when is_map(O) -> {to_bin(V), L, O};
norm_item({V, L, O}) when is_list(O) -> {to_bin(V), L, maps:from_list(O)};
norm_item(V) when is_binary(V); is_atom(V); is_number(V) -> {to_bin(V), to_bin(V), #{}};
norm_item(V) when is_list(V) -> {to_bin(V), to_bin(V), #{}};
norm_item(Other) -> error({aihtml, {bad_item, Other}}).

-spec enumerate([T]) -> [{pos_integer(), T}].
enumerate(L) -> lists:zip(lists:seq(1, length(L)), L).

-spec opt(atom(), map()) -> term().
opt(K, Opts) -> maps:get(K, Opts, undefined).

-spec opt_class(map()) -> term().
opt_class(Opts) ->
    case opt(class, Opts) of undefined -> []; C -> C end.

-spec selected_value(binary() | undefined, [item()]) -> binary().
selected_value(undefined, _Items) -> <<>>;
selected_value(Sel, Items) ->
    case lists:keymember(Sel, 1, norm_items(Items)) of
        true -> Sel;
        false -> <<>>
    end.

-spec join_values([binary()]) -> binary().
join_values(Vs) -> iolist_to_binary(lists:join(<<",">>, Vs)).

-spec check_opt(atom(), atom(), binary(), [binary()]) -> binary().
check_opt(Name, Key, V, Allowed) ->
    case lists:member(V, Allowed) of
        true -> V;
        false -> error({aihtml, {bad_option, Name, Key, V}})
    end.

-spec to_bin(value()) -> binary().
to_bin(B) when is_binary(B) -> B;
to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_bin(I) when is_integer(I) -> integer_to_binary(I);
to_bin(F) when is_float(F) -> num(F);
to_bin(L) when is_list(L) -> unicode:characters_to_binary(L).

-spec opt_bin(value() | undefined) -> binary() | undefined.
opt_bin(undefined) -> undefined;
opt_bin(V) -> to_bin(V).

-spec num(number()) -> binary().
num(I) when is_integer(I) -> integer_to_binary(I);
num(F) when is_float(F), F == trunc(F) -> integer_to_binary(trunc(F));
num(F) when is_float(F) -> float_to_binary(F, [short]).

-spec bool(boolean()) -> binary().
bool(true) -> <<"true">>;
bool(false) -> <<"false">>.

-spec state(boolean(), binary()) -> binary() | [].
state(true, Class) -> Class;
state(false, _Class) -> [].

-spec truthy(term()) -> boolean().
truthy(V) -> not (V =:= false orelse V =:= undefined orelse V =:= null
                  orelse V =:= <<"false">>).

%% @doc Attribute lookup with aihtml_html's key rules ("_" is "-").
-spec attr(binary(), term()) -> term().
attr(Name, Attrs) ->
    case [V || {K, V} <- flat(Attrs), key(K) =:= Name] of
        [] -> undefined;
        Vs -> lists:last(Vs)
    end.

split_attrs(Names, Attrs) ->
    lists:partition(fun({K, _}) -> lists:member(key(K), Names); (_) -> false end,
                    flat(Attrs)).

key(A) when is_atom(A) -> key(atom_to_binary(A, utf8));
key(B) when is_binary(B) -> binary:replace(B, <<"_">>, <<"-">>, [global]);
key(Other) -> Other.

flat(M) when is_map(M) -> lists:sort(maps:to_list(M));
flat(L) when is_list(L) ->
    lists:flatmap(fun(X) when is_list(X); is_map(X) -> flat(X); (X) -> [X] end, L).

%%%===================================================================
%%% API docs (methods) shared by the catalog entries
%%%===================================================================

-type method() :: #{name := atom(), args := binary(), doc := binary()}.

-spec set_checked(binary()) -> method().
set_checked(Args) ->
    #{name => setChecked, args => <<"(", Args/binary, ")">>,
      doc => <<"Set the state; no change event.">>}.
-spec get_value(binary()) -> method().
get_value(Ret) ->
    #{name => getValue, args => <<"()">>, doc => <<"The current state: ", Ret/binary, ".">>}.
-spec set_disabled() -> method().
set_disabled() ->
    #{name => setDisabled, args => <<"(bool)">>, doc => <<"Disable or enable the input.">>}.
-spec set_value(boolean()) -> method().
set_value(true) ->
    #{name => setValue, args => <<"(values | \"a,b\")">>,
      doc => <<"Check exactly these values; no change event.">>};
set_value(false) ->
    #{name => setValue, args => <<"(value)">>,
      doc => <<"Select this value (\"\" for none); no change event.">>}.
-spec group_get_value(boolean()) -> method().
group_get_value(true) ->
    #{name => getValue, args => <<"()">>, doc => <<"The checked values, an array.">>};
group_get_value(false) ->
    #{name => getValue, args => <<"()">>, doc => <<"The selected value, \"\" if none.">>}.
-spec group_set_disabled() -> method().
group_set_disabled() ->
    #{name => setDisabled, args => <<"(bool)">>,
      doc => <<"Disable or enable the whole group; items disabled on their own stay disabled.">>}.

%% @doc option_docs and methods of checkbox_group (Multi) and
%% radiobutton_group.
-spec group_api(boolean()) -> #{option_docs := #{atom() => binary()}, methods := [method()]}.
group_api(Multi) ->
    #{option_docs =>
          #{vertical => <<"One item per line (default).">>,
            horizontal => <<"Items in a wrapping row.">>,
            sm => <<"Small controls.">>, md => <<"Default size.">>, lg => <<"Large controls.">>,
            label_before => <<"Put each label before its control.">>},
      methods => [set_value(Multi), group_get_value(Multi), group_set_disabled()]}.
