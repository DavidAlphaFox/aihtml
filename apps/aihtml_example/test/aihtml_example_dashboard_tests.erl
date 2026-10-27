%% @doc Regression coverage for the sigil dashboard port at
%% /examples/dashboard. These assertions protect both every migrated board
%% section and the client-side capabilities promised by the page.
-module(aihtml_example_dashboard_tests).

-include_lib("eunit/include/eunit.hrl").

dashboard_page_test() ->
    Html = aihtml:render_binary(aihtml_example_dashboard:page()),

    %% Shell + every section registered in sigil's section-defs.
    [?assertMatch({_, _}, binary:match(Html, Needle))
     || Needle <- [<<"dash-stats">>,
                   <<"data-value=\"welcome\""/utf8>>,
                   <<"Goal progress"/utf8>>,
                   <<"data-value=\"revenue\""/utf8>>,
                   <<"Current download"/utf8>>,
                   <<"Website visits"/utf8>>,
                   <<"Related applications"/utf8>>,
                   <<"Top installed countries"/utf8>>,
                   <<"News"/utf8>>,
                   <<"Top authors"/utf8>>,
                   <<"Inbox"/utf8>>,
                   <<"Tasks"/utf8>>,
                   <<"Recent transactions"/utf8>>,
                   <<"Order timeline"/utf8>>,
                   <<"Top products"/utf8>>,
                   <<"Traffic by site"/utf8>>,
                   <<"Conversion rates"/utf8>>,
                   <<"Sales by country"/utf8>>,
                   <<"dash-summary">>],
       true],

    %% Migrated interactions: drag handles, persisted order, tabs, task
    %% checking, and layout reset.
    [?assertMatch({_, _}, binary:match(Html, Needle))
     || Needle <- [<<"data-ah=\"sortable\""/utf8>>,
                   <<"data-tab=\"7d\""/utf8>>,
                   <<"data-up=\"true\""/utf8>>,
                   <<"data-up=\"false\""/utf8>>,
                   <<"data-done=\"true\""/utf8>>,
                   <<"data-done=\"false\""/utf8>>,
                   <<"dash-task-check">>,
                   <<"id=\"dash-reset\"">>],
       true],
    Head = aihtml:render_binary(aihtml_example_dashboard:head()),
    [?assertMatch({_, _}, binary:match(Head, Needle))
     || Needle <- [<<"aihtml:dashboard:stat-order">>,
                   <<"aihtml:dashboard:section-order">>,
                   <<"applyOrder('dash-board'">>,
                   <<"if(saved.length!==ids.length">>],
       true].
