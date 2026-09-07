# The Jinja2 engine

aihtml ships two template engines side by side. They are peers: neither
replaces the other, and a single project can use both. This document covers
the jinja one; for mustache see the [project README](../README.md).

Everything here is checked by 520 fixtures in `test/jinja_spec/`, generated
from CPython's `jinja2` 3.1 by `tools/gen_jinja_fixtures.py`. Where this
engine deliberately differs, the fixture says so and the difference is listed
in [Deviations](#deviations) below.

---

## Quick start

```erlang
%% rebar.config
{plugins, [rebar3_aihtml]}.
{deps, [aihtml]}.
{provider_hooks, [{pre, [{compile, jinja}]}]}.

{jinja_opts, [{views, "views"}, {suffix, ".j2"}, {prefix, "j2_"}]}.
{erl_opts, [debug_info, {src_dirs, ["src", "_gen"]}]}.
```

`views/page.j2` becomes the module `j2_page`:

```erlang
j2_page:render(#{title => <<"Hi">>, users => [#{name => <<"ada">>}]}).
%% => <<"...">>

j2_page:render_iolist(Ctx).   %% hand straight to cowboy, no final copy
```

A template is compiled to Erlang code at build time. There is no template
cache, no ets table, no process: the BEAM code server holds the compiled
module, and static text lives in its literal pool, shared across processes by
reference.

---

## Supported syntax

### Expressions

The full expression language: literals, arithmetic (`+ - * / // % ** ~`),
comparison, `and`/`or`/`not`, `X if C else Y`, attribute access, subscripts
and slices, calls, filters (`|`) and tests (`is`).

```jinja
{{ users|selectattr("active")|map(attribute="name")|join(", ") }}
{{ (a + b) * 2 if ready else 0 }}
{{ items[1:5:2] }}
{{ n is divisibleby(3) }}
```

Two precedence rules are worth knowing because they surprise people, and both
match the reference implementation:

* **`**` associates to the LEFT**, unlike Python: `2 ** 3 ** 2` is 64.
* **A filter binds tighter than any arithmetic operator, but a leading sign is
  inside it**: `1 + -2|abs` is `1 + abs(-2)` = 3, and `-a|abs` is `(-a)|abs`.

### Statements

```jinja
{% if %} {% elif %} {% else %} {% endif %}
{% for x in xs %} {% else %} {% endfor %}        {# with loop, if, recursive #}
{% set x = ... %}   {% set x %}...{% endset %}
{% with a = 1 %}...{% endwith %}
{% filter upper %}...{% endfilter %}
{% raw %}...{% endraw %}
{% do expr %}
{% include "p.j2" [ignore missing] [with|without context] %}
{% extends "base.j2" %}  {% block name [scoped] [required] %}  {{ super() }}
{% macro m(a, b=1) %}...{% endmacro %}  {% call(x) m() %}...{% endcall %}
{% import "lib.j2" as lib %}  {% from "lib.j2" import a, b as c %}
```

The `loop` variable carries `index index0 revindex revindex0 first last length
depth depth0 previtem nextitem cycle(...) changed(...)`.

**Not implemented:** `{% autoescape %}`, `{% trans %}`, `{% debug %}`,
`{% break %}`/`{% continue %}`, and custom delimiters. Each is explained in
[Deviations](#deviations); using one is a build error, not a silent no-op.

### Whitespace control

`{{- -}}`, `{%- -%}`, `{#- -#}`, plus `trim_blocks`, `lstrip_blocks` and
`keep_trailing_newline`. **`trim_blocks` and `lstrip_blocks` default to
`true` here**, unlike the reference implementation — see deviation J8.

---

## Values

| Jinja concept | Erlang term |
|---|---|
| dict / mapping | `map()` with `atom()` keys |
| list / sequence | `list()` |
| tuple | `tuple()` |
| string | `binary()`, UTF-8 |
| int / float | `integer()` / `float()` |
| true / false | `true` / `false` |
| none, and undefined | `undefined` |
| safe (Markup) string | `{safe, iodata()}` |
| callable | `fun/N` |

Output follows the reference implementation rather than Erlang's own
conventions, because that is what a template ported from jinja2 expects:
`true` prints as `True`, a list as `[1, 2]`, a map as `{'a': 1}`, and a string
inside a container gains quotes while a bare one does not.

Truthiness is Python's, **not mustache's**: `0`, `0.0`, `#{}` and `{}` are all
false here and all true there.

---

## Autoescaping

On by default. `{{ x }}` escapes `& < > " '`; `|safe` marks a value as
already-safe and `|forceescape` escapes even a safe one.

Filters are classified by how they treat the safe marker — transparent,
escaping, generating, or not-text — and every one of them carries that
classification as an annotation in `src/ai_jinja_filters.erl`. A test asserts
mechanically that none is missing, because getting one wrong is an XSS.

`|tojson` escapes `< > & '` as `\uXXXX`, so a value containing `</script>`
cannot end the script element it is embedded in.

---

## Configuration

`jinja_opts` in `rebar.config`:

| Key | Default | In the build stamp |
|---|---|---|
| `views` | `"views"` | no |
| `suffix` | `".j2"` | no |
| `out_dir` | `"_gen"` | no |
| `prefix` | `"j2_"` | **yes** |
| `extensions` | `[]` | **yes** |
| `escape` | `true` | **yes** |
| `trim_blocks` | `true` | **yes** |
| `lstrip_blocks` | `true` | **yes** |
| `keep_trailing_newline` | `false` | **yes** |
| `strict_undefined` | `false` | **yes** |
| `line_map` | `true` | **yes** |
| `warnings_as_errors` | `false` | no |

"In the build stamp" means changing it rebuilds every template. The keys that
are not in it cannot change the generated code, so folding them in would make
renaming `out_dir` recompile the world.

---

## Custom filters and tests

```erlang
-module(my_filters).
-behaviour(ai_jinja_ext).
-export([filters/0, tests/0, params/0, money/2]).

filters() -> #{money => {?MODULE, money}}.
tests()   -> #{}.
params()  -> #{money => [currency]}.       %% positional parameter names

money(V, Args) ->
    Cur = maps:get(currency, Args, <<"USD">>),
    <<(ai_jinja_rt:to_binary(V))/binary, " ", Cur/binary>>.
```

```erlang
{jinja_opts, [{extensions, [my_filters]}]}.
```

`{{ 9|money("EUR") }}` and `{{ 9|money(currency="EUR") }}` both reach the
function identically. A filter that shadows a builtin is a build error, not an
override: a template that silently gets a different `join` because a
dependency registered one is a bug nobody finds.

Custom **statements** are not offered. The interface a `{% mytag %}` would
need is much wider than a filter's, and designing it without a real
requirement driving it would almost certainly have to be redone.

---

## The parse_transform

```erlang
-module(my_views).
-compile({parse_transform, ai_jinja_transform}).

-jinja_ext(my_filters).                      % (a) declare and validate
-jinja_template({page, "views/page.j2"}).    % (c) compile into page/1

greet(Name) ->                               % (b) expand in place
    ai_jinja:inline(~"Hello {{ name }}!", #{name => Name}).
```

Form (b) works with or without the transform — without it the same template is
compiled at run time, with identical output *and identical failures*: both
paths refuse the same statements, through one shared predicate. Form (c)
requires the transform; without it the function is simply not there.

An inline template cannot carry `{% include %}`, `{% extends %}`,
`{% import %}`, `{% from %}` or `{% block %}` — there is no views directory to
resolve them against — and its macros cannot be mutually recursive, because an
expansion compiles them to anonymous funs.

See [Using the parse_transform](parse-transform.md) for the mustache
equivalent; the two are the same three forms and can be used in one module.

---

## Development-time reloading

```erlang
ai_jinja_dev:check().        %% one-shot self test
ai_jinja_dev:reload(all).    %% recompile and reload every stale template
ai_html_dev:reload_all().    %% both engines
```

There is no switch and nothing is watched: with no ets, no persistent_term and
no process there is nowhere to keep an "enabled" flag. Call `reload(all)` at
the top of a request handler in your dev profile, or from an editor save hook.

Staleness is decided by a content stamp, never by mtime: POSIX mtime has
one-second granularity, and the edit-then-reload loop this exists to serve
routinely happens inside one second.

---

## Deviations

Every one of these is deliberate, and every one has a fixture.

| # | Deviation | Why |
|---|---|---|
| J1 | Map keys are `atom()`, not strings | Follows the mustache side; avoids binary key comparison on the hot path |
| J2 | `none` and undefined are one value | Erlang has one idiomatic empty value. Consequence: `x is defined` is **false** for a context that explicitly passes `none` |
| J3 | Chained comparison `1 < x < 3` is refused | Better a build error than a confidently wrong `(1 < x) < 3` |
| J4 | An unknown filter or test is a **build** error | A typo should not wait until the page is served |
| J5 | Delimiters are not configurable | They would have to enter the build stamp and parameterise the raw scanner, for very little |
| J6 | No `{% autoescape %}` block | Autoescape is a compile-time constant; a block would need the safe marker propagated at run time |
| J7 | No `{% trans %}`, `{% debug %}`, `{% break %}`, `{% continue %}` | Extensions of the reference implementation, not the language. Use a custom filter |
| J8 | `trim_blocks` and `lstrip_blocks` default to **on** | The reference default fills real templates with blank lines; nearly every project turns them on |
| J9 | Iterating a string is an error | In a template it is almost always a typo |
| J10 | Identifiers are ASCII only | A non-ASCII name would need `binary_to_atom/2`, an unbounded atom table |
| J11 | `extends`/`include`/`import` targets must be literal | Compile-time resolution to a module is what makes them direct cross-module calls |
| J12 | No sandbox | Templates compile to Erlang code; a sandbox would have to live in code generation |
| J13 | Attribute access does not fall back to methods | Erlang maps have none. `.items()`, `.keys()` and `.values()` are special-cased |
| J14 | Undefined is chainable | `a.b.c` renders as nothing instead of raising, equivalent to `ChainableUndefined`. `strict_undefined` gives the strict behaviour |
| J15 | No nested destructuring in `{% for %}` | Flat `{% for k, v in ... %}` covers the real uses |
| J16 | `{% set ns.x = ... %}` is not supported | Erlang has no mutable container; supporting it would demote every enclosing loop from a comprehension to a fold |
| J17 | A macro sees template-level names regardless of definition order | The reference captures the scope at the definition point; a module-level function cannot |

---

## Using both engines

```erlang
{provider_hooks, [{pre, [{compile, mustache}, {compile, jinja}]}]}.
{mustache_opts, [{views, "views"}, {prefix, "view_"}]}.
{jinja_opts,    [{views, "views"}, {suffix, ".j2"}, {prefix, "j2_"}]}.
```

They may share one `views` and one `out_dir`; each generated file names its
engine in its banner, and the orphan collector only removes its own. Module
names are global, so a clash between the two is reported like any other.

The one thing to keep in mind is that the two languages disagree:

| | mustache | jinja |
|---|---|---|
| Variable lookup | dynamic, walks the context stack | lexical scope |
| `0` and `#{}` | true | false |
| Missing variable | empty | empty (`strict_undefined` changes it) |
| Escaping off | `{{{x}}}` | `\|safe` |
| Reuse | `{{> p}}` | include / extends / macro / import |
| `true` prints as | `true` | `True` |
| A list prints as | its characters | `[1, 2]` |
