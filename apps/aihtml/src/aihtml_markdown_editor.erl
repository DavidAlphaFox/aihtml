%%%-------------------------------------------------------------------
%%% @doc A WYSIWYG Markdown editor, ported from sigil
%%% (text/markdown_editor, built on its prose_editor). See
%%% designs/04-components.md.
%%%
%%%   markdown_editor(Value, Css, Attrs)      Value is the Markdown text
%%%
%%% The editor is ProseMirror with markdown-it, imported by the
%%% `markdown-editor' behaviour (assets/js/components/markdown_editor.js)
%%% and so part of its lazily loaded chunk. Typing Markdown formats in place
%%% (`# ' a heading, `**bold**', `- ' a list, `> ' a quote, ``` a code
%%% block, `[text](url)' a link ...); `/' on an empty line opens the block
%%% menu; the handle left of a block adds a block or drags it elsewhere.
%%%
%%% A value-bearing component: the root carries the Markdown in
%%% `data-ah-value' and fires `input' on every edit and `change' when the
%%% editor loses focus with an edited value (as a native textarea), so
%%% `on(change, ...)' and postbacks receive the Markdown in Event.value.
%%%
%%% The server renders everything but ProseMirror's own document: the
%%% block menu, the block handle and the statistics footer are in the
%%% page from the start, hidden until used. The Markdown itself is also
%%% in a <textarea> inside the root, which is what the page shows until
%%% the editor has loaded (and all it shows without JavaScript): the
%%% plain Markdown, editable, with the component's `name', so a form
%%% submits the Markdown either way. The editor starts from what the
%%% textarea holds (text typed before it loaded is kept); then the textarea is
%%% hidden and kept in sync with the document.
%%%
%%% markdown_editor/3 builds an #ah_markdown_editor{}
%%% (include/aihtml_markdown_editor.hrl) and render/1 turns it into HTML,
%%% so pages may also write the record directly (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_markdown_editor).
-behaviour(aihtml_element).

-include("aihtml_markdown_editor.hrl").

-export([markdown_editor/3, render/1, fields/1, catalog/0]).

-export_type([label_key/0, labels/0, element/0]).

-define(H, aihtml_html).
-define(E, aihtml_element).

-type label_key() :: editor | group_text | group_list | group_other
                   | paragraph | heading_1 | heading_2 | heading_3 | blockquote
                   | horizontal_rule | bullet_list | ordered_list | task_list
                   | image | code_block | table | block_add | block_drag
                   | chars | words | paragraphs | limit | exceeded
                   | enter_url | enter_image_url.
%% Texts of the editor (sigil's prose-editor locale keys).
-type labels() :: #{label_key() => unicode:chardata()}.
-type element() :: #ah_markdown_editor{}.

-define(LABELS, #{editor => <<"Markdown editor">>,
                  group_text => <<"Heading">>, group_list => <<"List">>,
                  group_other => <<"Other">>,
                  paragraph => <<"Paragraph">>, heading_1 => <<"Heading 1">>,
                  heading_2 => <<"Heading 2">>, heading_3 => <<"Heading 3">>,
                  blockquote => <<"Quote">>, horizontal_rule => <<"Horizontal Rule">>,
                  bullet_list => <<"Bullet List">>, ordered_list => <<"Ordered List">>,
                  task_list => <<"Task List">>, image => <<"Image">>,
                  code_block => <<"Code Block">>, table => <<"Table">>,
                  block_add => <<"Add Block">>, block_drag => <<"Drag to Sort">>,
                  chars => <<"Chars">>, words => <<"Words">>,
                  paragraphs => <<"Paragraphs">>, limit => <<"Limit">>,
                  exceeded => <<"Limit exceeded">>,
                  enter_url => <<"Enter URL:">>, enter_image_url => <<"Image URL:">>}).

%% The labels the behaviour needs itself (the rest is rendered here).
-define(JS_LABELS, [editor, enter_url, enter_image_url]).

%% sigil's slash menu (prose_editor/plugins/slash_menu/items.cljs) without
%% the maths item: {group, type, level, label key}.
-define(GROUPS, [{<<"Text">>, group_text}, {<<"List">>, group_list},
                 {<<"Advanced">>, group_other}]).
-define(ITEMS, [{<<"Text">>, <<"paragraph">>, undefined, paragraph},
                {<<"Text">>, <<"heading">>, 1, heading_1},
                {<<"Text">>, <<"heading">>, 2, heading_2},
                {<<"Text">>, <<"heading">>, 3, heading_3},
                {<<"Text">>, <<"blockquote">>, undefined, blockquote},
                {<<"Text">>, <<"horizontal_rule">>, undefined, horizontal_rule},
                {<<"List">>, <<"bullet_list">>, undefined, bullet_list},
                {<<"List">>, <<"ordered_list">>, undefined, ordered_list},
                {<<"List">>, <<"task_list">>, undefined, task_list},
                {<<"Advanced">>, <<"image">>, undefined, image},
                {<<"Advanced">>, <<"code_block">>, undefined, code_block},
                {<<"Advanced">>, <<"table">>, undefined, table}]).

%% @doc A WYSIWYG Markdown editor holding `Value' (Markdown text, a
%% binary or a string; `undefined' is empty).
%%
%% Css: `disabled', `readonly' (the document is shown but cannot be
%% edited; no block menu or handle).
%% Options (in Attrs): `placeholder' (shown in an empty document, default
%% "Start typing..."), `show_stats' (a footer with the number of
%% characters, words and paragraphs), `max_chars' (a limit shown in the
%% footer, which turns red beyond it; implies the footer; typing is not
%% blocked, as in sigil), `height' (pixels or a CSS length: a fixed height,
%% the document scrolls inside), `labels' (a map of texts, see
%% label_key()). `name' names the textarea that carries the Markdown in
%% a form.
-spec markdown_editor(undefined | unicode:chardata(), aihtml_html:css(), aihtml_html:attrs()) ->
          #ah_markdown_editor{}.
markdown_editor(Value, Css, Attrs) ->
    ?E:build(?MODULE, #ah_markdown_editor{value = Value}, Css, Attrs).

%% @doc The field names of #ah_markdown_editor{}.
-spec fields(atom()) -> [atom()].
fields(ah_markdown_editor) -> record_info(fields, ah_markdown_editor).

-spec render(element()) -> aihtml_html:html().
render(#ah_markdown_editor{name = Name, disabled = Disabled, readonly = Readonly,
                           show_stats = ShowStats, max_chars = Max} = R0) ->
    {Id, R} = ensure_id(R0),
    Classes = ?E:classes(?MODULE, R),               % checks the flag fields first
    is_boolean(ShowStats) orelse error({aihtml, {bad_show_stats, ShowStats}}),
    Max =:= undefined orelse (is_integer(Max) andalso Max > 0)
        orelse error({aihtml, {bad_max_chars, Max}}),
    Value = text(R#ah_markdown_editor.value),
    L = labels(R#ah_markdown_editor.labels),
    Placeholder = text(R#ah_markdown_editor.placeholder),
    Editable = not (Disabled orelse Readonly),
    %% the HTML parser drops a newline right after <textarea>: keep one
    Source = ?H:el(textarea, [[$\n || binary:first(<<Value/binary, " ">>) =:= $\n], Value],
                   [<<"ah-md-editor-source">>],
                   [{id, sub_id(Id, <<"source">>)}, {name, Name},
                    {aria_label, maps:get(editor, L)},
                    {placeholder, Placeholder}, {rows, 10}, {spellcheck, <<"false">>},
                    {readonly, Readonly}, {disabled, Disabled}]),
    Content = ?H:el('div',
                    [Source | case Editable of
                                  true -> [handle(L), drag_indicator(), caret(),
                                           slash_menu(Id, L)];
                                  false -> []
                              end],
                    [<<"ah-pm-content">>], []),
    Stats = case ShowStats orelse Max =/= undefined of
                true -> [stats(Max, L)];
                false -> []
            end,
    ?H:el('div', [Content | Stats], Classes,
          [[{id, Id}, {data_ah, <<"markdown-editor">>}, {data_ah_value, Value},
            {data_ah_placeholder, Placeholder},
            {data_ah_max_chars, Max},
            {data_ah_readonly, Readonly},
            {data_ah_labels, iolist_to_binary(aihtml_json:encode(maps:with(?JS_LABELS, L)))},
            {aria_disabled, Disabled andalso <<"true">>},
            {style, height(R#ah_markdown_editor.height)}],
           ?E:root_attrs(R, change)]).

%% The block handle: add (+) and drag (grip) buttons beside the hovered
%% block (sigil's plugins/handle.cljs). Mouse affordances: the keyboard has
%% the block menu (/) and Alt-ArrowUp/Down to move a block.
handle(L) ->
    Btn = fun(Cls, Key, Svg) ->
                  ?H:el(button, {safe, Svg}, [Cls],
                        [{type, button}, {tabindex, <<"-1">>},
                         {title, maps:get(Key, L)}, {aria_label, maps:get(Key, L)}])
          end,
    ?H:el('div', [Btn(<<"ah-pm-block-handle-add">>, block_add, icon(plus)),
                  Btn(<<"ah-pm-block-handle-drag">>, block_drag, icon(grip))],
          [<<"ah-pm-block-handle">>], [{aria_hidden, <<"true">>}]).

drag_indicator() ->
    ?H:el('div', [], [<<"ah-pm-drag-indicator">>], [{aria_hidden, <<"true">>}]).

%% A zero-width mark the behaviour moves to the caret: the anchor the
%% slash menu floats at (AH.float needs an element).
caret() ->
    ?H:el(span, [], [<<"ah-md-editor-caret">>], [{aria_hidden, <<"true">>}]).

%% The slash menu (sigil's slash_menu/view.cljs): group tabs, then the
%% items of each group. A listbox of options for assistive technology;
%% the editor points at the selected option with aria-activedescendant.
slash_menu(Id, L) ->
    Menu = sub_id(Id, <<"menu">>),
    Tabs = [?H:el(button, maps:get(Key, L),
                  [<<"ah-pm-slash-menu-tab">>, [<<"active">> || G =:= <<"Text">>]],
                  [{type, button}, {tabindex, <<"-1">>}, {data_group, G}])
            || {G, Key} <- ?GROUPS],
    Numbered = lists:zip(lists:seq(0, length(?ITEMS) - 1), ?ITEMS),
    Groups = [?H:el('div',
                    [?H:el('div', maps:get(Key, L), [<<"ah-pm-slash-menu-group-heading">>],
                           [{id, <<Menu/binary, "-", G/binary>>}]),
                     ?H:el('div',
                           [item(Menu, N, Type, Level, maps:get(Label, L), N =:= 0)
                            || {N, {IG, Type, Level, Label}} <- Numbered, IG =:= G],
                           [<<"ah-pm-slash-menu-group-items">>], [{role, presentation}])],
                    [<<"ah-pm-slash-menu-group">>],
                    [{data_group, G}, {role, group},
                     {aria_labelledby, <<Menu/binary, "-", G/binary>>}])
              || {G, Key} <- ?GROUPS],
    ?H:el('div',
          [?H:el('div', Tabs, [<<"ah-pm-slash-menu-tabs">>], [{aria_hidden, <<"true">>}]),
           ?H:el('div', Groups, [<<"ah-pm-slash-menu-content">>],
                 [{id, <<Menu/binary, "-list">>}, {role, listbox},
                  {aria_label, maps:get(editor, L)}])],
          [<<"ah-pm-slash-menu">>], [{id, Menu}]).

item(Menu, N, Type, Level, Label, Selected) ->
    ?H:el('div',
          [?H:el(span, {safe, icon({Type, Level})}, [<<"ah-pm-slash-menu-item-icon">>],
                 [{aria_hidden, <<"true">>}]),
           ?H:el(span, Label, [<<"ah-pm-slash-menu-item-label">>], [])],
          [<<"ah-pm-slash-menu-item">>, [<<"selected">> || Selected]],
          [{id, <<Menu/binary, "-", (integer_to_binary(N))/binary>>}, {role, option},
           {data_type, Type}, {data_level, Level},
           {aria_selected, atom_to_binary(Selected)}]).

%% The statistics footer (sigil's plugins/stats_footer.cljs). The numbers
%% are filled in by the behaviour once the document is parsed.
stats(Max, L) ->
    Item = fun(Key, Stat, V) ->
                   ?H:el(span, [?H:el(span, maps:get(Key, L), [<<"ah-pm-stats__label">>], []),
                                ?H:el(span, V, [<<"ah-pm-stats__value">>],
                                      [{data_stat, Stat}])],
                         [<<"ah-pm-stats__item">>], [])
           end,
    Sep = ?H:el(span, <<"|">>, [<<"ah-pm-stats__sep">>], [{aria_hidden, <<"true">>}]),
    Dash = <<"–"/utf8>>,
    ?H:el('div',
          [Item(chars, chars, Dash), Sep, Item(words, words, Dash), Sep,
           Item(paragraphs, paragraphs, Dash)
           | case Max of
                 undefined -> [];
                 _ -> [Sep, Item(limit, limit, [Dash, <<"/">>, integer_to_binary(Max)])]
             end]
          ++ [?H:el(span, maps:get(exceeded, L), [<<"ah-pm-stats__warning">>],
                    [{hidden, true}]) || Max =/= undefined],
          [<<"ah-pm-stats">>], []).

labels(Custom) ->
    is_map(Custom) orelse error({aihtml, {bad_markdown_editor_labels, Custom}}),
    maps:foreach(fun(K, _) -> maps:is_key(K, ?LABELS)
                                  orelse error({aihtml, {bad_markdown_editor_label, K}})
                 end, Custom),
    maps:map(fun(_, V) -> text(V) end, maps:merge(?LABELS, Custom)).

height(undefined) -> undefined;
height(N) when is_integer(N), N > 0 -> <<"height:", (integer_to_binary(N))/binary, "px">>;
height(H) when is_binary(H); is_list(H) ->
    V = text(H),
    re:run(V, <<"^[0-9.]+(px|em|rem|vh|%)?$|^calc\\([^;{}]*\\)$">>) =/= nomatch
        orelse error({aihtml, {bad_height, H}}),
    <<"height:", V/binary>>;
height(H) -> error({aihtml, {bad_height, H}}).

%% `name' goes to the textarea, `id' stays on the root (and derives the
%% ids of the parts). A root without an id gets one.
ensure_id(R) ->
    Id = case R#ah_markdown_editor.id of
             undefined -> <<"ah-md", (integer_to_binary(erlang:unique_integer([positive])))/binary>>;
             Id0 -> text(Id0)
         end,
    {Id, R#ah_markdown_editor{id = Id}}.

sub_id(Id, Part) -> <<Id/binary, "-", Part/binary>>.

text(undefined) -> <<>>;
text(B) when is_binary(B) -> B;
text(L) when is_list(L) -> unicode:characters_to_binary(L);
text(X) -> beamai_html_escape:to_binary(X, aihtml).

%% sigil's icons (slash_menu/items.cljs, plugins/handle.cljs).
-define(SVG(Body), <<"<svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" "
                     "stroke=\"currentColor\" stroke-width=\"2\">", Body, "</svg>">>).
icon(plus) ->
    <<"<svg width=\"16\" height=\"16\" viewBox=\"0 0 16 16\" fill=\"none\" stroke=\"currentColor\" "
      "stroke-width=\"2\" stroke-linecap=\"round\"><line x1=\"8\" y1=\"3\" x2=\"8\" y2=\"13\"/>"
      "<line x1=\"3\" y1=\"8\" x2=\"13\" y2=\"8\"/></svg>">>;
icon(grip) ->
    <<"<svg width=\"16\" height=\"16\" viewBox=\"0 0 16 16\" fill=\"currentColor\">"
      "<circle cx=\"6\" cy=\"4\" r=\"1.2\"/><circle cx=\"10\" cy=\"4\" r=\"1.2\"/>"
      "<circle cx=\"6\" cy=\"8\" r=\"1.2\"/><circle cx=\"10\" cy=\"8\" r=\"1.2\"/>"
      "<circle cx=\"6\" cy=\"12\" r=\"1.2\"/><circle cx=\"10\" cy=\"12\" r=\"1.2\"/></svg>">>;
icon({<<"paragraph">>, _}) ->
    ?SVG("<path d=\"M13 4v16\"/><path d=\"M3 4h18\"/><path d=\"M7 4v4\"/><path d=\"M17 4v4\"/>");
icon({<<"heading">>, 1}) ->
    ?SVG("<path d=\"M4 12h8\"/><path d=\"M4 4v16\"/><path d=\"M12 4v16\"/>"
         "<path d=\"M17 12l3-2v8\"/>");
icon({<<"heading">>, 2}) ->
    ?SVG("<path d=\"M4 12h8\"/><path d=\"M4 4v16\"/><path d=\"M12 4v16\"/>"
         "<path d=\"M21 18h-4c0-4 4-3 4-6 0-1.5-2-2.5-4-1\"/>");
icon({<<"heading">>, 3}) ->
    ?SVG("<path d=\"M4 12h8\"/><path d=\"M4 4v16\"/><path d=\"M12 4v16\"/>"
         "<path d=\"M17.5 15.5c1 1 2.5 1 3.5 0 .5-.5.5-1.5 0-2-.5-.5-1.5-.5-2 0\"/>"
         "<path d=\"M17 10.5c1-1 2.5-1 3.5 0 .5.5.5 1.5 0 2\"/>");
icon({<<"blockquote">>, _}) ->
    ?SVG("<path d=\"M3 21c3 0 7-1 7-8V5c0-1.25-.756-2.017-2-2H4c-1.25 0-2 .75-2 1.972V11c0 "
         "1.25.75 2 2 2 1 0 1 0 1 1v1c0 1-1 2-2 2s-1 .008-1 1.031V21z\"/><path d=\"M15 21c3 "
         "0 7-1 7-8V5c0-1.25-.757-2.017-2-2h-4c-1.25 0-2 .75-2 1.972V11c0 1.25.75 2 2 "
         "2h.75c0 2.25.25 4-2.75 4v3z\"/>");
icon({<<"horizontal_rule">>, _}) ->
    ?SVG("<line x1=\"3\" y1=\"12\" x2=\"21\" y2=\"12\"/>");
icon({<<"bullet_list">>, _}) ->
    ?SVG("<line x1=\"9\" y1=\"6\" x2=\"20\" y2=\"6\"/><line x1=\"9\" y1=\"12\" x2=\"20\" y2=\"12\"/>"
         "<line x1=\"9\" y1=\"18\" x2=\"20\" y2=\"18\"/><circle cx=\"4\" cy=\"6\" r=\"1\" "
         "fill=\"currentColor\"/><circle cx=\"4\" cy=\"12\" r=\"1\" fill=\"currentColor\"/>"
         "<circle cx=\"4\" cy=\"18\" r=\"1\" fill=\"currentColor\"/>");
icon({<<"ordered_list">>, _}) ->
    ?SVG("<line x1=\"10\" y1=\"6\" x2=\"21\" y2=\"6\"/><line x1=\"10\" y1=\"12\" x2=\"21\" "
         "y2=\"12\"/><line x1=\"10\" y1=\"18\" x2=\"21\" y2=\"18\"/><text x=\"2\" y=\"8\" "
         "font-size=\"7\" fill=\"currentColor\" stroke=\"none\" font-weight=\"bold\">1</text>"
         "<text x=\"2\" y=\"14\" font-size=\"7\" fill=\"currentColor\" stroke=\"none\" "
         "font-weight=\"bold\">2</text><text x=\"2\" y=\"20\" font-size=\"7\" "
         "fill=\"currentColor\" stroke=\"none\" font-weight=\"bold\">3</text>");
icon({<<"task_list">>, _}) ->
    ?SVG("<rect x=\"3\" y=\"5\" width=\"6\" height=\"6\" rx=\"1\"/><path d=\"M3 17l2 2 4-4\"/>"
         "<line x1=\"13\" y1=\"6\" x2=\"21\" y2=\"6\"/><line x1=\"13\" y1=\"12\" x2=\"21\" "
         "y2=\"12\"/><line x1=\"13\" y1=\"18\" x2=\"21\" y2=\"18\"/>");
icon({<<"image">>, _}) ->
    ?SVG("<rect x=\"3\" y=\"3\" width=\"18\" height=\"18\" rx=\"2\" ry=\"2\"/><circle "
         "cx=\"8.5\" cy=\"8.5\" r=\"1.5\"/><polyline points=\"21 15 16 10 5 21\"/>");
icon({<<"code_block">>, _}) ->
    ?SVG("<polyline points=\"16 18 22 12 16 6\"/><polyline points=\"8 6 2 12 8 18\"/>");
icon({<<"table">>, _}) ->
    ?SVG("<rect x=\"3\" y=\"3\" width=\"18\" height=\"18\" rx=\"2\"/><line x1=\"3\" y1=\"9\" "
         "x2=\"21\" y2=\"9\"/><line x1=\"3\" y1=\"15\" x2=\"21\" y2=\"15\"/><line x1=\"9\" "
         "y1=\"3\" x2=\"9\" y2=\"21\"/><line x1=\"15\" y1=\"3\" x2=\"15\" y2=\"21\"/>").

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => markdown_editor, category => form,
       signature => <<"markdown_editor(Value, Css, Attrs)">>,
       root => <<"ah-md-editor">>,
       flags => [disabled, readonly],
       options => [placeholder, show_stats, max_chars, height, labels],
       behavior => <<"markdown-editor">>,
       events => [<<"input">>, <<"change">>],
       doc => <<"A WYSIWYG Markdown editor (ProseMirror, loaded on demand): Markdown "
                "shortcuts format as you type, / opens the block menu, blocks can be "
                "dragged; the Markdown is in data-ah-value and a form field.">>,
       option_docs =>
           #{disabled => <<"Not editable, dimmed; the textarea is disabled too.">>,
             readonly => <<"The document is shown but cannot be edited.">>,
             placeholder => <<"Text of an empty document (default \"Start typing...\").">>,
             show_stats => <<"A footer counting characters, words and paragraphs.">>,
             max_chars => <<"A character limit shown in the footer (implies it); exceeding "
                            "it is flagged, not blocked.">>,
             height => <<"Fixed height (pixels or a CSS length); the document scrolls inside.">>,
             labels => <<"Map of texts: editor (aria-label), group_text, group_list, "
                         "group_other, paragraph, heading_1..3, blockquote, horizontal_rule, "
                         "bullet_list, ordered_list, task_list, image, code_block, table, "
                         "block_add, block_drag, chars, words, paragraphs, limit, exceeded, "
                         "enter_url, enter_image_url.">>},
       methods =>
           [#{name => getValue, args => <<"()">>, doc => <<"Return the Markdown.">>},
            #{name => setValue, args => <<"(Markdown)">>,
              doc => <<"Replace the document without firing change.">>},
            #{name => getHtml, args => <<"()">>, doc => <<"Return the document as HTML.">>},
            #{name => getJson, args => <<"()">>,
              doc => <<"Return the document as ProseMirror JSON.">>},
            #{name => focus, args => <<"()">>, doc => <<"Focus the editor.">>},
            #{name => blur, args => <<"()">>, doc => <<"Remove the focus (fires change if edited).">>},
            #{name => exec, args => <<"(Command, Opts)">>,
              doc => <<"Run a command on the selection: bold, italic, code, strikethrough, "
                       "link {href}, heading {level}, paragraph, blockquote, code_block "
                       "{language}, bullet_list, ordered_list, task_list, horizontal_rule, "
                       "undo, redo. Fires change unless the editor has focus.">>},
            #{name => stats, args => <<"()">>,
              doc => <<"Return {chars, words, paragraphs}.">>},
            #{name => view, args => <<"()">>,
              doc => <<"The ProseMirror EditorView (browser side).">>}]}].
