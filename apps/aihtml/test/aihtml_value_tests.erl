-module(aihtml_value_tests).
-include_lib("eunit/include/eunit.hrl").

plain_test() ->
    ?assertEqual(<<"a,b,c">>, aihtml_value:join([<<"a">>, b, "c"])),
    ?assertEqual(<<"1,2.5,x">>, aihtml_value:join([1, 2.5, <<"x">>])),
    ?assertEqual(<<>>, aihtml_value:join([])),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], aihtml_value:split(<<"a,b,c">>)),
    ?assertEqual([<<"a">>, <<"b">>], aihtml_value:split("a,b")).

empty_test() ->
    ?assertEqual([], aihtml_value:split(<<>>)),
    ?assertEqual([], aihtml_value:split("")),
    ?assertEqual([], aihtml_value:split(undefined)),
    ?assertEqual([<<"a">>, <<>>, <<"b">>], aihtml_value:split(<<"a,,b">>)),
    ?assertEqual([<<>>, <<>>], aihtml_value:split(<<",">>)).

escape_test() ->
    ?assertEqual(<<"a\\,b,c">>, aihtml_value:join([<<"a,b">>, <<"c">>])),
    ?assertEqual(<<"x\\\\y">>, aihtml_value:join([<<"x\\y">>])),
    ?assertEqual(<<"\\\\\\,">>, aihtml_value:join([<<"\\,">>])),
    ?assertEqual([<<"a,b">>, <<"c">>], aihtml_value:split(<<"a\\,b,c">>)),
    ?assertEqual([<<"x\\y">>], aihtml_value:split(<<"x\\\\y">>)),
    %% a lone backslash is kept; one before another character drops
    ?assertEqual([<<"ab">>], aihtml_value:split(<<"a\\b">>)),
    ?assertEqual([<<"a\\">>], aihtml_value:split(<<"a\\">>)).

round_trip_test_() ->
    Cases = [[<<"a">>],
             [<<"a,b">>, <<"c">>],
             [<<"1,000">>, <<"2,000,000">>],
             [<<"\\">>, <<",">>, <<"\\,">>, <<",\\">>],
             [<<"a">>, <<>>, <<"b">>],
             [<<>>, <<"x">>],
             [<<"x">>, <<>>],
             [<<>>, <<>>],
             [<<"北京,上海"/utf8>>, <<"東京"/utf8>>, <<"é,ü\\ñ"/utf8>>],
             [<<"emoji 😀,🎉"/utf8>>],
             [<<" spaced , value ">>, <<"trailing\\">>]],
    [?_assertEqual(C, aihtml_value:split(aihtml_value:join(C))) || C <- Cases].

unicode_join_test() ->
    ?assertEqual(<<"北京\\,上海,東京"/utf8>>,
                 aihtml_value:join([<<"北京,上海"/utf8>>, "東京"])),
    ?assertEqual([<<"北京"/utf8>>, <<"上海"/utf8>>], aihtml_value:split("北京,上海")).
