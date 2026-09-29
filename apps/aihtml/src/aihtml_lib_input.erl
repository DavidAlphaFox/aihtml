%%%-------------------------------------------------------------------
%%% @doc Internal: what the text entry components share (aihtml_input,
%%% aihtml_textarea, aihtml_password_input, aihtml_number_input,
%%% aihtml_input_otp, aihtml_tag_input).
%%%
%%% input, textarea, password_input and number_input keep a native
%%% control: `Attrs' (name, placeholder, on(...), ...) go to the
%%% `<input>'/`<textarea>', `Css' to the wrapper. input_otp and tag_input
%%% are value-bearing components: `Attrs' go to the root, which carries
%%% `data-ah-value' and fires `change'; `name' goes to a hidden input.
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_lib_input).

-export([classes/2, native_attrs/2, input_id/2, float_label/4, has_value/1, text/1,
         hidden/3, family/2, sizes/0, states/0, field_docs/0]).

-export_type([size/0, state/0, value/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type size() :: sm | lg.
-type state() :: invalid | valid.
%% The text of a field; numbers and atoms are written as text.
-type value() :: iodata() | number() | atom().

%% @doc The class list: catalog root, modifier and flag fields, `css'.
%% Flags are sorted so the classes come out in the order the Css builders
%% always wrote them (aihtml_catalog:classes/2 sorts flags too).
-spec classes(module(), aihtml_element:element()) -> aihtml_html:css().
classes(Mod, R) ->
    Tag = element(1, R),
    #{flags := Flags} = Entry = aihtml_catalog:entry(Mod, ?E:component_name(Tag)),
    ?E:classes(R, Mod:fields(Tag), Entry#{flags := lists:sort(Flags)}).

%% @doc The native control's attributes after the component's own: id,
%% postback and `attrs', without the placeholder when a label replaces it.
-spec native_attrs(aihtml_element:element(), term()) -> aihtml_html:attrs().
native_attrs(R, Label) ->
    no_placeholder(Label, ?E:root_attrs(R, change)).

%% @doc The native control's id: the element's own (or one written as a
%% binary key in `attrs'), or a generated one when a label must point at it.
-spec input_id(aihtml_element:element(), term()) -> term().
input_id(R, Label) ->
    #{id := Id0, attrs := Attrs} = ?E:base(R),
    Id = case Id0 of
             undefined ->
                 case lists:keyfind(<<"id">>, 1, ?H:attrs(Attrs)) of
                     {_, V} -> V;
                     false -> undefined
                 end;
             _ -> Id0
         end,
    case Id of
        undefined when Label =:= undefined -> undefined;
        undefined -> <<"ah-in-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
        _ -> Id
    end.

%% @doc The floating label of a field whose classes start with `Prefix'.
-spec float_label(binary(), term(), term(), term()) -> aihtml_html:html().
float_label(_Prefix, undefined, _Id, _Value) -> [];
float_label(Prefix, Label, Id, Value) ->
    ?H:el(label, Label,
          [<<Prefix/binary, "-label">>,
           [<<Prefix/binary, "-label-float">> || has_value(Value)]],
          [{for, Id}]).

-spec has_value(term()) -> boolean().
has_value(undefined) -> false;
has_value(V) -> iolist_size(text(V)) > 0.

-spec text(undefined | value()) -> binary().
text(undefined) -> <<>>;
text(V) when is_binary(V) -> V;
text(V) when is_list(V) -> unicode:characters_to_binary(V);
text(V) when is_number(V); is_atom(V) -> beamai_html_escape:to_binary(V, aihtml).

%% @doc The hidden input of a value-bearing component (none without a name).
-spec hidden(undefined | atom() | iodata(), iodata(), boolean()) -> aihtml_html:html().
hidden(undefined, _Value, _Disabled) -> [];
hidden(Name, Value, Disabled) ->
    ?H:void(input, [], [{type, hidden}, {name, Name}, {value, Value},
                        {disabled, Disabled}]).

%% @doc Modifier classes of the sigil input family: `<prefix>-<modifier>',
%% underscores written as dashes (no_rounded -> ah-input-no-rounded).
-spec family(binary(), [atom()]) -> #{atom() => [binary()]}.
family(Prefix, Mods) ->
    maps:from_list(
      [{M, [<<Prefix/binary, "-",
              (binary:replace(atom_to_binary(M), <<"_">>, <<"-">>, [global]))/binary>>]}
       || M <- Mods]).

%% @doc The catalog group `size' of the native fields.
-spec sizes() -> {[size()], none}.
sizes() -> {[sm, lg], none}.

%% @doc The catalog group `state' of the native fields.
-spec states() -> {[state()], none}.
states() -> {[invalid, valid], none}.

%% @doc The option docs the native fields share.
-spec field_docs() -> #{atom() => binary()}.
field_docs() ->
    #{sm => <<"Small field.">>, lg => <<"Large field.">>,
      invalid => <<"Error border; sets aria-invalid=\"true\" on the control.">>,
      valid => <<"Success border.">>,
      disabled => <<"Disable the native control.">>,
      label => <<"Floating label text; it replaces the placeholder.">>}.

%%%===================================================================
%%% Internal
%%%===================================================================

%% A floating label takes the placeholder's place (sigil does the same).
no_placeholder(undefined, Attrs) -> Attrs;
no_placeholder(_Label, Attrs) ->
    lists:keydelete(<<"placeholder">>, 1, ?H:attrs(Attrs)).
