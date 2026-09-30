%%%-------------------------------------------------------------------
%%% @doc An `<a>' styled as a button, ported from sigil's form components.
%%%
%%% ah_link_button/4 builds an #ah_link_button{} (include/aihtml_link_button.hrl)
%%% and render/1 turns it into HTML, so pages may also write the record
%%% directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_link_button).
-behaviour(aihtml_element).

-include("aihtml_link_button.hrl").

-export([ah_link_button/4, render/1, fields/1, catalog/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

%% @doc An `<a href=Href>' with button styling. `{disabled, true}' in
%% `Attrs' removes the href and marks it `aria-disabled'.
-spec ah_link_button(aihtml_html:html(), iodata() | undefined, aihtml_html:css(),
                     aihtml_html:attrs()) -> #ah_link_button{}.
ah_link_button(Content, Href, Css, Attrs) ->
    ?E:build(?MODULE, #ah_link_button{body = Content, href = Href}, Css, Attrs).

%% @doc The field names of #ah_link_button{}.
-spec fields(atom()) -> [atom()].
fields(ah_link_button) -> record_info(fields, ah_link_button).

-spec render(#ah_link_button{}) -> aihtml_html:html().
render(#ah_link_button{body = Content, href = Href, disabled = Disabled} = B) ->
    State = case Disabled of
                true  -> [{aria_disabled, <<"true">>}, {tabindex, <<"-1">>}];
                false -> [{href, Href}]
            end,
    ?H:el(a, Content,
          [?E:classes(?MODULE, B), <<"ah-link-btn">>, [<<"ah-btn-disabled">> || Disabled]],
          [[{role, link} | State], ?E:root_attrs(B, click)]).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => link_button, category => form,
       signature => <<"ah_link_button(Content, Href, Css, Attrs)">>,
       root => <<"ah-btn">>, groups => aihtml_lib_button:btn_groups(), flags => [round],
       classes => #{md => []},
       doc => <<"A link that looks like a button.">>,
       option_docs => #{round => <<"Pill-shaped corners.">>},
       methods => []}].
