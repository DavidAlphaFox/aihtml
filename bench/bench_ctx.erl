%% Shared benchmark context. Values deliberately avoid the characters whose
%% escaping changed (/ = and backtick) and the ampersand whose old encoding was
%% missing its semicolon, so both versions must emit identical bytes.
-module(bench_ctx).
-export([context/1]).

context(N) ->
    Items = [#{id => I, url => <<"#anchor">>,
               name => item_name(I),
               note => <<"plain text, no special chars">>}
             || I <- lists:seq(1, N)],
    #{header  => <<"Benchmark Page">>,
      raw     => <<"<em>raw html is not escaped</em>">>,
      items   => Items,
      %% v0.3.7's inverted section only fires on [], not on a missing key, so
      %% the key is present and empty to make both versions take the branch.
      missing => []}.

item_name(I) ->
    <<"item ", (integer_to_binary(I))/binary>>.
