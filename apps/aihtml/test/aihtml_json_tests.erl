-module(aihtml_json_tests).
-include_lib("eunit/include/eunit.hrl").

enc(T) -> iolist_to_binary(aihtml_json:encode(T)).

sorted_keys_test() ->
    ?assertEqual(<<"{\"a\":1,\"b\":2,\"c\":3}">>, enc(#{c => 3, a => 1, b => 2})),
    %% atoms, binaries and integers sort together by their text
    ?assertEqual(<<"{\"1\":\"i\",\"a\":\"atom\",\"b\":\"bin\",\"z\":null}">>,
                 enc(#{<<"b">> => <<"bin">>, a => <<"atom">>, 1 => <<"i">>, z => null})),
    ?assertEqual(<<"{}">>, enc(#{})).

nested_test() ->
    T = #{z => [#{y => 1, x => #{q => true, p => false}}], m => #{b => [1, 2.5], a => <<"é"/utf8>>}},
    ?assertEqual(<<"{\"m\":{\"a\":\"é\",\"b\":[1,2.5]},"
                   "\"z\":[{\"x\":{\"p\":false,\"q\":true},\"y\":1}]}"/utf8>>, enc(T)).

same_as_json_test() ->
    %% other than the key order the output is json:encode/1's
    T = #{<<"list">> => [1, <<"two">>, null, true, 3.0], <<"s">> => <<"a\"b</c>\n">>},
    ?assertEqual(json:decode(iolist_to_binary(json:encode(T))), json:decode(enc(T))),
    ?assertEqual(iolist_to_binary(json:encode([1, <<"x">>, false])), enc([1, <<"x">>, false])).

atom_order_independent_test() ->
    %% keys made in a different order give the same text
    A = maps:from_list([{list_to_atom("k" ++ integer_to_list(I)), I} || I <- lists:seq(1, 40)]),
    B = maps:from_list(lists:reverse(maps:to_list(A))),
    ?assertEqual(enc(A), enc(B)),
    Text = enc(A),
    Pos = [{element(1, binary:match(Text, <<"\"", K/binary, "\":">>)), K}
           || K <- [atom_to_binary(Ka) || Ka := _ <- A]],
    ?assertEqual(lists:sort([K || {_, K} <- Pos]), [K || {_, K} <- lists:sort(Pos)]).
