# rebar3_aihtml fixtures

Whole rebar3 projects driven by `test/rebar3_aihtml_SUITE.erl`. Each case
copies one of them into its own `priv_dir` and runs a real `rebar3` subprocess
against the copy, so nothing here is ever written to.

Erlang sources carry a `.erl.in` extension and are renamed on copy. They are
sources of the *fixture* projects, not of aihtml, but `rebar3 eunit` scans the
whole of `test/` for `.erl` files and would report them as test modules it
cannot find. The extension is the only thing that keeps them out of that scan;
`{extra_src_dirs, [{"test", [{recursive, false}]}]}` stops them being compiled
but not being discovered.

`@PLUGIN@`-style placeholders are not used: `aihtml` and `rebar3_aihtml` reach
each fixture through a `_checkouts/` directory that the suite fills in, because
rebar3 has no `path` resource for deps or plugins.
