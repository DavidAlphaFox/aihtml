%%%-------------------------------------------------------------------
%%% @doc Status bar with left and right segments, ported from sigil's
%%% status-bar (DOM and class names are sigil's, so the ported styles
%%% under priv/css/sigil/components apply unchanged). The behaviour (the
%%% count details float above their segment) is in
%%% assets/js/components/status_bar.ts, aihtml's additions in
%%% priv/css/extra/status_bar.css.
%%%
%%% status_bar/3 builds an #ah_status_bar{} record
%%% (include/aihtml_status_bar.hrl) and render/1 turns it into HTML
%%% (designs/05-records.md).
%%% @end
%%%-------------------------------------------------------------------
-module(aihtml_status_bar).
-behaviour(aihtml_element).

-include("aihtml_status_bar.hrl").

-export([status_bar/3]).
-export([render/1, fields/1, catalog/0]).

-export_type([segment/0]).

-type segment() :: #{content := aihtml_html:html(), align => left | right}
                 | #{count := integer(), label => aihtml_html:html(),
                     details => [{aihtml_html:html(), aihtml_html:html()}],
                     align => left | right}
                 | aihtml_html:html().

-define(EL, aihtml_element).

%% @doc Status bar with left and right segments (sigil status-bar).
%% A segment is html (left), `#{content, align}', or a count segment
%% `#{count, label, details => [{Label, Value}]}' whose details show on
%% hover. Options: `content' (text: adds sigil's CJK-aware word count
%% segment), `dirty' (true | false: adds the saved/unsaved dot on the
%% right), `labels' (map overriding the default English labels).
-spec status_bar([segment()], aihtml_html:css(), aihtml_html:attrs()) -> #ah_status_bar{}.
status_bar(Segments, Css, Attrs) ->
    ?EL:build(?MODULE, #ah_status_bar{segments = Segments}, Css, Attrs).

%% @doc The field names of the record.
-spec fields(ah_status_bar) -> [atom()].
fields(ah_status_bar) -> record_info(fields, ah_status_bar).

-spec render(#ah_status_bar{}) -> aihtml_html:html().
render(#ah_status_bar{segments = Segments, dirty = Dirty} = R) ->
    Classes = ?EL:classes(?MODULE, R),
    lists:member(Dirty, [undefined, true, false])
        orelse error({aihtml, {bad_option, dirty, Dirty}}),
    Labels = maps:merge(default_labels(), R#ah_status_bar.labels),
    Words = case R#ah_status_bar.content of
                undefined -> [];
                Text -> [word_count_segment(Text, Labels)]
            end,
    Save = case Dirty of
               undefined -> [];
               _ -> [#{align => right,
                       content => {save, Dirty}}]
           end,
    All = [seg(S) || S <- Words ++ Segments ++ Save],
    Left = [segment(S, Labels) || S <- All, maps:get(align, S, left) =:= left],
    Right = [segment(S, Labels) || S <- All, maps:get(align, S, left) =:= right],
    aihtml_html:el('div',
        [aihtml_html:el('div', Left, [<<"ah-status-bar__side">>], []),
         aihtml_html:el('div', Right,
                        [<<"ah-status-bar__side">>, <<"ah-status-bar__side--right">>], [])],
        Classes,
        [[{role, status},
          {data_ah, <<"status-bar">>},
          {data_dirty, Dirty =/= undefined andalso atom_to_binary(Dirty =:= true)}],
         ?EL:root_attrs(R, none)]).

seg(#{content := _} = S) -> S;
seg(#{count := _} = S) -> S;
seg(Html) -> #{content => Html}.

segment(#{content := {save, Dirty}}, Labels) ->
    aihtml_html:el('div',
        [aihtml_html:el(span, [], [<<"ah-status-bar__dot">>], [{aria_hidden, <<"true">>}]),
         aihtml_html:el(span, case Dirty of
                                  true -> maps:get(unsaved, Labels);
                                  false -> maps:get(saved, Labels)
                              end, [], [])],
        [<<"ah-status-bar__save">>], []);
segment(#{count := N} = S, _Labels) ->
    Details = maps:get(details, S, []),
    aihtml_html:el('div',
        [aihtml_html:el(span, N, [<<"ah-status-bar__count-num">>], []),
         case maps:get(label, S, undefined) of
             undefined -> [];
             L -> aihtml_html:el(span, [<<" ">>, L], [], [])
         end,
         case Details of
             [] -> [];
             _ ->
                 aihtml_html:el('div',
                     [aihtml_html:el('div',
                          [aihtml_html:el(span, K, [<<"ah-status-bar__row-label">>], []),
                           aihtml_html:el(span, V, [<<"ah-status-bar__row-val">>], [])],
                          [<<"ah-status-bar__row">>], [])
                      || {K, V} <- Details],
                     [<<"ah-status-bar__popover">>], [{role, tooltip}])
         end],
        [<<"ah-status-bar__count">>], [{tabindex, 0}]);
segment(#{content := Html}, _Labels) ->
    aihtml_html:el('div', Html, [<<"ah-status-bar__extra">>], []).

default_labels() ->
    #{count => <<"words">>, cjk => <<"CJK">>, words => <<"EN words">>,
      chars => <<"Chars">>, chars_no_space => <<"No spaces">>, lines => <<"Lines">>,
      paragraphs => <<"Paragraphs">>, saved => <<"Saved">>, unsaved => <<"Unsaved">>}.

%% sigil's compute-stats: count = CJK characters + English words.
word_count_segment(Text0, Labels) ->
    Text = unicode:characters_to_binary(Text0),
    Cs = unicode:characters_to_list(Text),
    Cjk = length([C || C <- Cs, (C >= 16#4E00 andalso C =< 16#9FFF)
                                orelse (C >= 16#3400 andalso C =< 16#4DBF)
                                orelse (C >= 16#F900 andalso C =< 16#FAFF)]),
    Words = length(re:split(Text, <<"[^A-Za-z]+">>, [trim]) -- [<<>>]),
    Blank = string:trim(Text) =:= <<>>,
    Lines = case Blank of true -> 0; false -> length(binary:matches(Text, <<"\n">>)) + 1 end,
    Paras = length([P || P <- re:split(Text, <<"\n{2,}">>), string:trim(P) =/= <<>>]),
    NoSpace = length([C || C <- Cs, not lists:member(C, " \t\n\r\f\v")]),
    #{count => Cjk + Words, label => maps:get(count, Labels),
      details => [{maps:get(K, Labels), V}
                  || {K, V} <- [{cjk, Cjk}, {words, Words}, {chars, length(Cs)},
                                {chars_no_space, NoSpace}, {lines, Lines},
                                {paragraphs, Paras}]]}.

%%%===================================================================
%%% Catalog
%%%===================================================================

-spec catalog() -> [aihtml_catalog:entry()].
catalog() ->
    [#{name => status_bar, category => layout,
       signature => <<"status_bar(Segments, Css, Attrs)">>,
       root => <<"ah-status-bar">>,
       options => [content, dirty, labels],
       behavior => <<"status-bar">>,
       option_docs => #{content => <<"Text to count: adds a CJK-aware word count with details on hover.">>,
                       dirty => <<"true or false: adds the unsaved (amber) or saved (green) dot.">>,
                       labels => <<"Map overriding the English labels (count, cjk, words, chars, chars_no_space, lines, paragraphs, saved, unsaved).">>},
       methods => [],
       doc => <<"Bottom status bar with left and right segments, word count "
                "details and a saved/unsaved dot.">>}].
