%%%-------------------------------------------------------------------
%%% @doc A native select wrapped to look like the dropdownlist.
%%%
%%%   select(Options, Value, Css, Attrs)
%%%
%%% Options are items as in aihtml_lib_select; groups become optgroups.
%%% The markup and classes follow sigil's dropdownlist, so its ported
%%% stylesheets apply.
%%%
%%% The component function builds an #ah_select{} record (include/
%%% aihtml_select.hrl) and render/1 turns it into HTML, so pages may also
%%% write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_select).
-behaviour(aihtml_element).

-include("aihtml_select.hrl").

-export([select/4, render/1, fields/1, catalog/0]).

-define(E, aihtml_element).
-define(F, aihtml_lib_form).

%% @doc A native select wrapped to look like the dropdownlist. Css styles
%% the wrapper; Attrs (name, id, multiple, disabled, on/2 ...) go to the
%% select itself. Value may be a list when the select is `multiple'.
-spec select([aihtml_lib_select:item()],
             aihtml_lib_select:value() | [aihtml_lib_select:value()] | undefined,
             aihtml_html:css(), aihtml_html:attrs()) -> #ah_select{}.
select(Options, Value, Css, Attrs) ->
    %% {size, N} is the select's HTML attribute, not the size modifier;
    %% a binary key keeps it (in place) among the HTML attributes
    Html = [case A of {size, N} -> {<<"size">>, N}; _ -> A end || A <- flat_attrs(Attrs)],
    ?E:build(?MODULE, #ah_select{items = Options, value = Value}, Css, Html).

-spec render(#ah_select{}) -> aihtml_html:html().
render(#ah_select{items = Options, value = Value, placeholder = P0} = R) ->
    Classes = ?E:classes(?MODULE, R),
    Vals = case Value of
               undefined -> [];
               L when is_list(L), not is_integer(hd(L)) -> [?F:bin(V) || V <- L];
               V -> [?F:bin(V)]
           end,
    Placeholder = case P0 of
                      undefined -> [];
                      P -> el(option, P, [], [{value, <<>>}, {selected, Vals =:= []}])
                  end,
    OptionEls = [select_option(I, Vals) || I <- aihtml_lib_select:norm_items(Options)],
    el(span, [el(select, [Placeholder, OptionEls], [<<"ah-select-control">>],
                 ?E:root_attrs(R, change)),
              el(span, el(span, <<"▼"/utf8>>, [<<"ah-dropdownlist-arrow-icon">>], []),
                 [<<"ah-select-arrow">>], [{aria_hidden, <<"true">>}])],
       Classes, []).

select_option({group, Label, Sub}, Vals) ->
    el(optgroup, [select_option(I, Vals) || I <- Sub], [], [{label, ?F:text(Label)}]);
select_option({item, V, L, Dis}, Vals) ->
    el(option, L, [], [{value, V}, {selected, lists:member(V, Vals)}, {disabled, Dis}]).

%%%===================================================================
%%% Record and catalog
%%%===================================================================

%% @doc The field names of #ah_select{}.
-spec fields(atom()) -> [atom()].
fields(ah_select) -> record_info(fields, ah_select).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => select, category => form,
       signature => <<"select(Options, Value, Css, Attrs)">>,
       root => <<"ah-select">>,
       groups => #{size => {[sm, lg], none},
                   template => {[primary, success, warning, danger], none}},
       flags => [block],
       options => [placeholder],
       events => [<<"change">>],
       option_docs => #{sm => <<"Compact height (28px).">>,
                        lg => <<"Large height (42px).">>,
                        primary => <<"Primary-coloured border.">>,
                        success => <<"Success-coloured border.">>,
                        warning => <<"Warning-coloured border.">>,
                        danger => <<"Danger-coloured border.">>,
                        block => <<"Fill the container width.">>,
                        placeholder => <<"An empty first option with this label, selected when Value is undefined.">>},
       methods => [],
       doc => <<"Native <select> styled like the dropdownlist; Attrs go to the select. "
                "Options as for dropdownlist, groups become optgroups; Value may be a list "
                "with `multiple'. Option `placeholder' adds an empty first option.">>}].

%%%===================================================================
%%% Internal
%%%===================================================================

flat_attrs(M) when is_map(M) -> lists:sort(maps:to_list(M));
flat_attrs(L) when is_list(L) ->
    lists:flatmap(fun(X) when is_list(X); is_map(X) -> flat_attrs(X);
                     (X) -> [X]
                  end, L).

el(Tag, Children, Css, Attrs) -> aihtml_html:el(Tag, Children, Css, Attrs).
