%% An extension that shadows a builtin, used by ai_jinja_ext_tests.
-module(ai_jinja_ext_tests_clash).
-behaviour(ai_jinja_ext).
-export([filters/0, tests/0, upper/2]).

filters() -> #{upper => {?MODULE, upper}}.
tests()   -> #{}.

upper(V, _Args) -> V.
