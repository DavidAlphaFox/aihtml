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

## The projects

| | |
|---|---|
| `plugin_basic` | one app, four mustache templates, partials and the render API |
| `plugin_errors` | templates that must fail, one per diagnostic |
| `plugin_legacy` | a generated file from an older version, for the healing path |
| `plugin_umbrella` | two apps whose templates collide on a module name |
| `plugin_jinja` | inheritance, `super()`, an imported macro and an include |
| `plugin_dual` | both engines writing into one `out_dir` -- the arrangement in which each provider's orphan collector used to delete the other's output |
