%% @doc Demos of the expandable_text component (aihtml_expandable_text), shown on
%% /components/expandable_text. Each function is one example, written the way an
%% application writes it; the docs page prints its source under it.
-module(aihtml_example_demo_expandable_text).

-include_lib("aihtml/include/aihtml.hrl").

-export([demos/0]).
-export([expandable_default/0, expandable_labels/0]).

-spec demos() -> [map()].
demos() ->
    [#{component => expandable_text, title => <<"ExpandableText">>,
       summary => <<"长文本截断，点「展开」看全文。"/utf8>>,
       demos => [{<<"默认"/utf8>>, expandable_default},
                 {<<"自定义按钮文案、初始展开"/utf8>>, expandable_labels}]}].

%%% ExpandableText

-spec expandable_default() -> aihtml:html().
expandable_default() ->
    expandable_text(<<"aihtml renders every page on the server as Erlang function calls; "
                      "the browser only adds behaviour. This paragraph is long enough to be cut "
                      "at the threshold and shows a toggle to read the rest.">>,
                    [], [{threshold, 60}]).

-spec expandable_labels() -> aihtml:html().
expandable_labels() ->
    'div'([expandable_text(<<"Release notes: morph swaps keep focus, shared Mustache "
                             "templates render the same bytes on both ends, and popups "
                             "follow their anchor.">>,
                           [], [{threshold, 40}, {expand_label, <<"Show more">>},
                                {collapse_label, <<"Show less">>}]),
           expandable_text(<<"This one starts expanded, so the whole text is visible "
                             "and the toggle folds it.">>,
                           [], [{threshold, 30}, {expanded, true}]),
           expandable_text(<<"Short text is shown as is.">>, [], [])],
          [<<"flex flex-col gap-3">>], []).
