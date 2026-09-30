%%%-------------------------------------------------------------------
%%% @doc Markdown shown as HTML, rendered on the server: the reading side
%%% of markdown_editor (see designs/04-components.md).
%%%
%%%   ah_markdown_view(Markdown, Css, Attrs)
%%%
%%% The Markdown is parsed and rendered by aihtml_lib_markdown, a port of
%%% markdown-it configured as the editor configures it, so a document
%%% written in the editor reads the same here; readers and search engines
%%% get real headings, paragraphs, lists, tables and links in the page,
%%% with no JavaScript. The HTML is wrapped in
%%% `<div class="ah-markdown-view">', whose stylesheet
%%% (priv/css/extra/markdown_view.css) sets the typography from the theme
%%% tokens.
%%%
%%% Safe for Markdown from users: raw HTML in the Markdown is shown as
%%% text, never passed through; every text and attribute is escaped; links
%%% and images keep only http:, https:, mailto: and relative URLs (a link
%%% with any other scheme stays as its Markdown text). See
%%% aihtml_lib_markdown for the details.
%%%
%%% To show what an editor holds as the user edits, re-render the view in
%%% the editor's change postback with aihtml_action:html/4 (the demo does).
%%%
%%% ah_markdown_view/3 builds an #ah_markdown_view{}
%%% (include/aihtml_markdown_view.hrl) and render/1 turns it into HTML,
%%% so pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_markdown_view).
-behaviour(aihtml_element).

-include("aihtml_markdown_view.hrl").

-export([ah_markdown_view/3, render/1, fields/1, catalog/0]).

-export_type([element/0]).

-type element() :: #ah_markdown_view{}.

%% @doc The HTML of `Markdown' (a binary or string of UTF-8 Markdown;
%% `undefined' is empty). No modifiers or options: Css adds classes
%% (for instance Tailwind's `max-w-prose'), Attrs go to the root.
-spec ah_markdown_view(undefined | unicode:chardata(), aihtml_html:css(), aihtml_html:attrs()) ->
          element().
ah_markdown_view(Markdown, Css, Attrs) ->
    aihtml_element:build(?MODULE, #ah_markdown_view{markdown = Markdown}, Css, Attrs).

%% @doc The field names of #ah_markdown_view{}.
-spec fields(atom()) -> [atom()].
fields(ah_markdown_view) -> record_info(fields, ah_markdown_view).

-spec render(element()) -> aihtml_html:html().
render(#ah_markdown_view{markdown = Markdown} = R) ->
    Html = case Markdown of
               undefined -> <<>>;
               _ -> aihtml_lib_markdown:render(Markdown)
           end,
    aihtml_html:el('div', {safe, Html}, aihtml_element:classes(?MODULE, R),
                   aihtml_element:root_attrs(R, none)).

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => markdown_view, category => text,
       signature => <<"ah_markdown_view(Markdown, Css, Attrs)">>,
       root => <<"ah-markdown-view">>,
       behavior => none,
       doc => <<"Markdown rendered to HTML on the server (as markdown_editor's markdown-it "
                "reads it): headings, lists, task lists, tables, code, links and images, "
                "readable without JavaScript. Raw HTML shows as text; only http(s), mailto "
                "and relative URLs are linked.">>,
       option_docs => #{},
       methods => []}].
