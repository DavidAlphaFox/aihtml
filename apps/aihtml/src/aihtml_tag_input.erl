%%%-------------------------------------------------------------------
%%% @doc Chips plus a text field, ported from sigil (form/tag_input). See
%%% designs/04-components.md.
%%%
%%% A value-bearing component: `Attrs' go to the root, which carries
%%% `data-ah-value' and fires `change'; `name' goes to a hidden input.
%%%
%%% tag_input/3 builds an #ah_tag_input{} (include/aihtml_tag_input.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_tag_input).
-behaviour(aihtml_element).

-include("aihtml_tag_input.hrl").

-export([tag_input/3, render/1, fields/1, catalog/0]).

-export_type([chip_color/0, chip_variant/0]).

-define(H, aihtml_html).
-define(L, aihtml_lib_input).
-define(M(Name, Args, Doc), #{name => Name, args => Args, doc => Doc}).

-type chip_color() :: primary | secondary | success | warning | error | info.
-type chip_variant() :: soft | filled | outlined.

%% Shared templates (see aihtml_tpl): also compiled to AH.tpl.* for the browser.
-compile({parse_transform, beamai_mustache_transform}).
-mustache_template({tpl_tag_input_chip, "../templates/tag_input_chip.mustache"}).

%% @doc Chips plus a text field (sigil tag_input). Enter or comma adds the
%% typed tag, Backspace in the empty field removes the last one, leaving
%% the field adds what was typed, a pasted list is split on commas and
%% new lines; duplicates are dropped. Options: `placeholder' (default
%% "Add tag…"), `max_tags', `allow_duplicates' (default false),
%% `chip_color' (default primary), `chip_variant' (default soft).
%% `data-ah-value' is the tags joined with commas (aihtml_value:join/1: a
%% comma inside a tag, from `Tags' or setTags, is escaped as `\,').
-spec tag_input([binary()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_tag_input{}.
tag_input(Tags, Css, Attrs) when is_list(Tags) ->
    aihtml_element:build(?MODULE, #ah_tag_input{value = Tags}, Css, Attrs).

%% @doc The field names of #ah_tag_input{}.
-spec fields(atom()) -> [atom()].
fields(ah_tag_input) -> record_info(fields, ah_tag_input).

-spec render(#ah_tag_input{}) -> aihtml_html:html().
render(#ah_tag_input{value = Tags0, name = Name, disabled = Disabled,
                     placeholder = Placeholder, chip_color = Color,
                     chip_variant = Variant} = R) ->
    Classes = ?L:classes(?MODULE, R),
    Tags = [unicode:characters_to_binary(T) || T <- Tags0],
    Chips = [aihtml_tpl:safe(tpl_tag_input_chip(#{variant => Variant, color => Color,
                                                  index => I - 1, label => T,
                                                  disabled => Disabled}))
             || {I, T} <- lists:zip(lists:seq(1, length(Tags)), Tags)],
    Field = ?H:void(input, [<<"ah-tag-input__field">>],
                    [{type, text},
                     {placeholder, default(Placeholder, <<"Add tag…"/utf8>>)},
                     {aria_label, default(Placeholder, <<"Add tag">>)},
                     {disabled, Disabled}]),
    Value = aihtml_value:join(Tags),
    ?H:el('div', [Chips, Field, ?L:hidden(Name, Value, Disabled)],
          Classes,
          [[{data_ah, <<"tag-input">>}, {role, group},
            {data_ah_value, Value},
            {data_disabled, atom_to_binary(Disabled)},
            {data_chip_color, Color}, {data_chip_variant, Variant},
            {data_max_tags, R#ah_tag_input.max_tags},
            {data_allow_duplicates, R#ah_tag_input.allow_duplicates =:= true}],
           aihtml_element:root_attrs(R, change)]).

default(undefined, D) -> D;
default(V, _) -> V.

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => tag_input, category => form,
       signature => <<"tag_input(Tags, Css, Attrs)">>,
       root => <<"ah-tag-input">>,
       flags => [disabled],
       classes => #{disabled => []},
       options => [placeholder, max_tags, allow_duplicates, chip_color, chip_variant],
       behavior => <<"tag-input">>,
       events => [<<"change">>],
       doc => <<"Chips plus a text field: Enter or comma adds a tag, "
                "Backspace removes the last.">>,
       option_docs => #{disabled => <<"Read-only chips; the hidden input is disabled too.">>,
                        placeholder => <<"Placeholder of the text field (default \"Add tag...\").">>,
                        max_tags => <<"Ignore new tags past this count.">>,
                        allow_duplicates => <<"Keep repeated tags (default false).">>,
                        chip_color => <<"Chip colour: primary (default), secondary, success, "
                                        "warning, error or info.">>,
                        chip_variant => <<"Chip style: soft (default), filled or outlined.">>,
                        name => <<"Name of the hidden input; its value is the tags joined "
                                  "with commas (a comma inside a tag is escaped as \\,; "
                                  "aihtml_value:split/1 reads it).">>},
       methods => [?M(getTags, <<"()">>, <<"Return the tags as an array.">>),
                   ?M(setTags, <<"(Tags)">>, <<"Replace the tags, firing change.">>),
                   ?M(add, <<"(Tag)">>, <<"Add a tag (subject to max_tags and duplicates).">>),
                   ?M(remove, <<"(Tag)">>, <<"Remove the first tag equal to Tag.">>),
                   ?M(clear, <<"()">>, <<"Remove every tag, firing change.">>),
                   ?M(focus, <<"()">>, <<"Focus the text field.">>)]}].
