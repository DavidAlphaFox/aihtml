%% A second extension claiming the same name, used by ai_jinja_ext_tests.
-module(ai_jinja_ext_tests_ext2).
-behaviour(ai_jinja_ext).
-export([filters/0, tests/0, money/2]).

filters() -> #{money => {?MODULE, money}}.
tests()   -> #{}.

money(V, _Args) -> ai_jinja_rt:to_binary(V).
