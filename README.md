# aihtml

[English](README.md) · [中文](README.zh-CN.md)

A Mustache template engine for Erlang that compiles templates into Erlang modules.

Templates are turned into `.erl` files at build time by a rebar3 plugin, so at
run time rendering is a plain function call: no process, no ETS table, no
lookup of any kind.

- Passes all 136 cases of the six required [mustache spec](https://github.com/mustache/spec) modules
- Zero dependencies: the library and its test suite need nothing but OTP
- Static template text lives in the module's literal pool and is shared across
  processes by reference

---

## Incompatible Changes in 0.4.0

0.4.0 is a full rewrite. Templates and calling code both need changes.

### Standard context stack semantics

Previously a template used a flat context with full-path lookups. It now uses
the standard Mustache context stack, which is what every other Mustache
implementation does.

```diff
- {{#user}}{{user.name}}{{/user}}
+ {{#user}}{{name}}{{/user}}

- {{#items}}{{+ items.current}}<li>{{items.name}}</li>{{/ items.current}}{{/items}}
+ {{#items}}{{+ current}}<li>{{name}}</li>{{/ current}}{{/items}}
```

The rule: inside `{{#X}}`, drop the `X.` prefix from references to `X`'s own
fields. `rebar3 mustache migrate` does the mechanical part and reports what it
could not decide.

There is deliberately no compatibility switch. Supporting both would fork the
compiler's scope resolution, which costs more over time than migrating once.

### The API

| Removed | Replacement |
|---|---|
| `ai_mustache:bootstrap/0,1` | nothing -- templates are compiled by the build |
| `ai_mustache:reload/0` | `ai_mustache_dev:reload/1` (dev only) |
| `ai_mustache:render(Name, Ctx)` where `Name` is a string | `view_name:render(Ctx)`, or `ai_mustache:render(view_name, Ctx)` |
| `application:start(aihtml)` | nothing -- aihtml is a library application |
| `ai_dom_node`, `ai_dom_render` | removed; they were unrelated to Mustache |

### HTML escaping now covers five characters, not eight

`&` `<` `>` `"` `'` are escaped. `/`, `=` and `` ` `` are **not**.

The old set corrupted URLs: `href="/a/b"` came out as `href="&#x2F;a&#x2F;b"`.
The old `&` replacement was also missing its semicolon, producing `&amp`
instead of `&amp;`; that is fixed.

### Partial indentation applies to every line

When `{{> partial}}` sits alone on an indented line, that indent is applied to
every line the partial emits, as the spec requires. The old implementation
indented only the first line.

### ailib is no longer a dependency

Everything aihtml used from it is now implemented in `ai_mustache_rt`, which
depends only on OTP.

### Other fixed behaviour

- `{{.}}`, the implicit iterator, works. It previously always resolved to nothing.
- `{{> a.b}}` and other partial paths containing dots no longer crash the parser.
- `{{#a.b}}` no longer discards `a`'s other keys while iterating.

---

## How it works

```
views/index.mustache
   |
   +- ai_mustache_scanner     lexer: text and tag tokens, standalone lines, delimiters
   +- ai_mustache_parser      recursive descent -> AST
   +- ai_mustache_ast         merge text, drop empty bodies, resolve partials
   |
   +- ai_mustache_compiler    AST -> Erlang abstract forms      (the only such implementation)
         |
         +- rebar3_aihtml     erl_prettypr -> _gen/view_index.erl -> .beam
         +- parse_transform   forms injected into the calling module
```

`views/index.mustache` becomes roughly this:

```erlang
-module(view_index).

-mustache_source(#{path => <<"views/index.mustache">>, stamp => <<...>>,
                   mtime => 1757203845, vsn => 1, opts => #{...}}).

-export([render/1, render_iolist/1, render_stack/1, render_stack/2, partials/0]).

render(Ctx) -> erlang:iolist_to_binary(render_stack([Ctx], <<>>)).
partials()  -> [view_shared_item].

render_stack(S, I) ->
    [I, <<"<h1>">>,
     ai_mustache_rt:escape(ai_mustache_rt:lookup([header], S)),
     <<"</h1>\n">>,
     sec_1(S, I)].

sec_1(S, I) ->
    case ai_mustache_rt:lookup([items], S) of
        []                       -> [];
        L when is_list(L)        -> [sec_1_body([E | S], I) || E <- L];
        M when is_map(M)         -> sec_1_body([M | S], I);
        true                     -> sec_1_body(S, I);
        F when is_function(F, 2) -> F(erlang:iolist_to_binary(sec_1_body(S, I)), hd(S));
        F when is_function(F, 1) -> ai_mustache_rt:section(F(hd(S)), fun sec_1_body/2, S, I);
        V                        -> ai_mustache_rt:section(V, fun sec_1_body/2, S, I)
    end.

sec_1_body(S, I) -> [<<"  ">>, view_shared_item:render_stack(S, I)].
```

Three consequences worth naming:

**Static text is a literal.** It goes into the module's literal pool and is
shared between processes by reference. The previous design kept the parsed
template in ETS, and every `ets:lookup/2` deep-copied the whole tree into the
calling process.

**A partial is a cross-module call.** Editing a partial therefore does not
require recompiling the templates that include it, and two templates may
include each other -- the old tree-walking interpreter would have recursed
forever.

**Iterating a section pushes onto a list.** `[Item | Stack]` allocates one
cons cell. The old runner built a fresh context with `maps:merge/2` on every
iteration, which copied the whole map and, for a dotted section name, silently
dropped the sibling keys.

---

## Installation

You need **two** entries: the plugin, which compiles the templates at build
time, and the `aihtml` dependency, whose `ai_mustache_rt` the generated
modules call at run time.

```erlang
%% rebar.config -- the whole of it, for a single-app project
{erl_opts, [debug_info, {src_dirs, ["src", "_gen"]}]}.

{deps, [{aihtml, {git, "https://github.com/DavidAlphaFox/aihtml.git",
                  {tag, "v0.4.0"}}}]}.

{plugins, [{rebar3_aihtml, {git_subdir, "https://github.com/DavidAlphaFox/aihtml.git",
                            {tag, "v0.4.0"}, "rebar3_aihtml"}}]}.

{provider_hooks, [{pre, [{compile, mustache}]}]}.
```

That is genuinely all of it -- there is no `mustache_opts` above because every
option has a working default. The plugin lives in a subdirectory of the aihtml
repository, hence `git_subdir` rather than `git`.

Add `_gen/` to `.gitignore`; it is build output.

### Project layout

```
myapp/
  rebar.config
  src/
    myapp.app.src
    myapp.erl              %% calls view_index:render/1
  views/                   %% <- {views, "views"}, the default
    index.mustache         %% -> view_index
    layout/
      head.mustache        %% -> view_layout_head
    shared/
      post.mustache        %% -> view_shared_post
  _gen/                    %% generated, gitignored
    view_index.erl
    view_layout_head.erl
    view_shared_post.erl
```

`{src_dirs, ["src", "_gen"]}` is the one line that is easy to forget: without
it rebar3 never compiles the generated modules and `view_index` is undefined.

Partials are named by their path under `views/`, without the suffix:

```mustache
{{> layout/head}}
{{> shared/post}}
```

### Overriding the defaults

```erlang
{mustache_opts, [
    {views,    "views"},        % template root, relative to the app
    {suffix,   ".mustache"},
    {out_dir,  "_gen"},         % where the generated .erl files go
    {prefix,   "view_"},        % module name prefix
    {line_map, true},           % map generated line numbers back to the template
    {extensions, []},           % custom tag modules, see parse_transform below
    {ext_opts, #{}},            % configuration passed to those modules
    {warnings_as_errors, false}
]}.
```

An unrecognised key is reported rather than ignored, so a typo does not
silently do nothing.

### Umbrella projects

Put the plugin, the dep and the hook at the project root; each application
keeps its own `views/` and generates into its own `_gen/`.

```
myproject/
  rebar.config             %% plugins, deps, provider_hooks, shared mustache_opts
  apps/
    web/
      rebar.config         %% {erl_opts, [{src_dirs, ["src", "_gen"]}]} + any overrides
      views/ ...
      _gen/ ...
    admin/
      rebar.config
      views/ ...
      _gen/ ...
```

Two things to know:

- **`{src_dirs, ["src", "_gen"]}` has to be in each application's own
  `rebar.config`**, not just the root one.
- **Give each application its own `prefix`** if they have templates with the
  same path. `web/views/index.mustache` and `admin/views/index.mustache` both
  map to `view_index`, which is one module name for two modules. The plugin
  detects the clash across applications and tells you which prefixes are
  involved; `{prefix, "admin_"}` in one of them resolves it, and the module
  generated under the old name is collected as an orphan on the next build.

`rebar3 compile` now runs the `mustache` provider first. It only recompiles
templates whose content, options or compiler version actually changed -- the
check reads the `-mustache_source` attribute out of the previously generated
`.erl`, so there is no cache file to go stale.

### Why generate `.erl` rather than `.beam` directly

The generated source is readable, greppable and visible to dialyzer, and stack
traces from a template error point at real line numbers.

### Module names

```
views/index.mustache          ->  view_index
views/shared/item.mustache    ->  view_shared_item
views/layout/default.mustache ->  view_layout_default
```

`/`, `-` and `.` all become `_`, so `shared/item.mustache` and
`shared_item.mustache` would collide. The plugin detects that and fails rather
than silently overwriting one with the other.

### Migrating 0.3.x templates

```sh
rebar3 mustache migrate           # print a diff
rebar3 mustache migrate --write   # apply it
```

It rewrites in place, preserving comments, whitespace and custom delimiters.
Anything it cannot decide -- a reference to a sibling section's variable, say
-- is left alone and listed in the report for you to handle.

### erlang.mk

```makefile
BUILD_DEPS = rebar3_aihtml
DEP_PLUGINS = rebar3_aihtml
dep_rebar3_aihtml = git https://github.com/DavidAlphaFox/aihtml.git v0.4.0
```

Supported, but not the primary path; CI builds with rebar3.

---

## Rendering

```erlang
view_index:render(Ctx)         -> binary().
view_index:render_iolist(Ctx)  -> iolist().

%% When the template is only known at run time -- picking a layout by route:
ai_mustache:render(view_index, Ctx)         -> binary().
ai_mustache:render_iolist(view_index, Ctx)  -> iolist().

%% An inline template (see parse_transform below):
ai_mustache:inline(~"Hello {{name}}!", #{name => Name}) -> binary().
```

`render_iolist/1` can go straight into a cowboy response body, which skips
building the flat binary entirely.

The context may be any term, not just a map: the spec has a case whose entire
data is the integer `85`, reachable as `{{.}}`.

### Non-ASCII in your own Erlang code

Template files are read as UTF-8 and need nothing special. But a literal in
your own source does:

```erlang
%% WRONG: each codepoint is truncated to 8 bits and the text is destroyed
#{title => <<"我的博客">>}

%% Right
#{title => <<"我的博客"/utf8>>}
```

This is ordinary Erlang, not something aihtml introduces, but it is the most
common way to end up with mojibake in a page whose templates are fine.

### Development-time reloading

```erlang
ai_mustache_dev:check()          % one-shot environment self-test
ai_mustache_dev:stale(view_index)
ai_mustache_dev:reload(view_index)
ai_mustache_dev:reload(all)
```

**`check/0` is not a switch.** This module holds no state at all -- no ETS, no
persistent_term, no process, no cache file -- so there is nowhere for an
"enabled" flag to live. Nothing watches your files. You call `reload/1`
yourself, from a dev-only middleware, an editor hook, or the top of a request
handler.

Staleness is decided by hashing the file, never by its mtime: POSIX mtime has
one-second resolution and the edit-then-refresh loop happens well inside one
second.

---

## Template semantics

Keys are **atoms**, in the context and in the template:

```erlang
#{user => #{name => <<"David Gao">>, level => 1}, stars => 10}
```

Template bodies and paths are **UTF-8 binaries**. An entry point will accept
any `unicode:chardata()` and normalise it once, but a template that is not
valid UTF-8 is rejected with `{invalid_utf8, ByteOffset}` rather than passed
through as bytes -- that failure is much easier to diagnose at the door than
as mangled output from a generated module later. Paths are normalised but
never rejected, since a non-UTF-8 filesystem can still hand back a path that
names a real file.

### Name resolution

`{{name}}` walks the context stack from the top outwards and takes the value
from the first frame that has that key.

`{{a.b}}` resolves `a` by walking the stack, then takes `b` **strictly inside**
`a`. If `a` has no `b`, the result is empty; it does not keep searching
outwards.

`{{.}}` is the implicit iterator: the value on top of the stack.

### Falsy values

Exactly five: `undefined`, `false`, `[]`, `<<>>`, `null`.

### 0 is truthy. So is `#{}`.

This trips people up, so it is worth being explicit:

```erlang
%% {{#count}}You have {{.}} messages{{/count}}
#{count => 0}   %% the section RUNS and renders "You have 0 messages"
#{count => []}  %% the section is skipped
```

If you want "zero means hide", test it in your code and pass a boolean.

### Section dispatch

`{{#x}}` behaves according to the run-time type of `x`:

| `x` | Behaviour | Pushes a scope |
|---|---|---|
| `[]` | skipped | -- |
| non-empty list | body runs once per element | yes, per element |
| map | body runs once | yes |
| `true` | body runs once | no |
| `fun/2` | called as `F(RenderedBody, CurrentFrame)` | no |
| `fun/1` | called as `F(CurrentFrame)`, result dispatched again | depends on the result |
| falsy | skipped | -- |
| anything else | body runs once | yes, so `{{.}}` works |

`{{^x}}` runs the body when `x` is falsy and never pushes a scope.

---

## `{{#}}` versus `{{+}}`

Both look like conditionals. The difference is scope.

```mustache
{{#user}}{{name}}{{/user}}        renders the user's name -- {{#}} pushes user onto the stack
{{+user}}{{user.name}}{{/user}}   renders the same thing -- {{+}} does not push, so the full path is needed
```

`{{#}}` means *with* / *for each*: it enters a scope and iterates lists.
`{{+}}` means *if*: it tests truthiness and runs the body once, in the
surrounding scope. `{{-}}` is `{{+}}` negated.

Use `{{+}}` when you want a condition without changing what the names inside
refer to:

```mustache
{{#items}}
  {{+ current}}<li class="on">{{name}}</li>{{/ current}}
  {{- current}}<li>{{name}}</li>{{/ current}}
{{/items}}
```

`{{name}}` refers to the item in both branches. With `{{#current}}` it would
refer to whatever is inside `current`.

Both accept a `fun/1`, called with the current frame:

```erlang
#{has_friends => fun(Frame) -> maps:get(friends, Frame, []) =/= [] end}
```

---

## Rendering a partial conditionally

`{{> x}}` is unconditional on its own; wrap it in a section:

```mustache
{{#user}}
  {{> shared/card}}
{{/user}}

{{+is_admin}}
  {{> shared/panel}}
{{/is_admin}}

{{^items}}
  {{> shared/empty}}
{{/items}}
```

Which section you choose decides how names resolve inside the partial:
`{{#}}` pushes a scope, `{{+}}` does not.

**Give the partial a line of its own.** Squeezed onto one line it is not
standalone and loses its indentation:

```mustache
{{#show}}
  {{> row}}          correct: every line of row gets the 2-space indent
{{/show}}

{{#show}}{{> row}}{{/show}}   indentation lost, plus a stray blank line
```

To choose *which* partial at run time (the spec's Dynamic Names are not
implemented): mutually exclusive sections, or a lambda calling the module
directly -- partials compile to real modules, so Erlang can call them:

```erlang
Pick = fun(Frame) ->
    Mod = case maps:get(kind, Frame) of
              text  -> view_shared_text;
              image -> view_shared_image
          end,
    Mod:render_iolist(Frame)     %% an iolist, and not escaped
end.
```

The template says `{{*body}}`.

---

## Lambdas

`{{*name}}` is an aihtml extension. Its output is **not** escaped -- producing
markup is the point.

```erlang
%% fun/1: receives the current frame
#{yield => fun(Frame) -> render_something(Frame) end}

%% fun/2 plus a value: called as Fun(Value, Frame)
#{yield => [fun render_layout/2, <<"index">>]}
```

Since 0.4.0 the fun receives the **top of the stack**, not a flat global
context. A lambda at the top level of a template still sees the root context,
but move it inside a section and it will not. Pass what you need through the
`fun/2` form rather than relying on where the tag sits.

---

## Partials

```mustache
{{> shared/user}}
```

resolves to `view_shared_user` and is compiled into a direct call. Partials
render with the **current stack**, so a partial used inside `{{#items}}` sees
the item:

```mustache
{{! views/index.mustache }}
<ul>{{#items}}{{> shared/row}}{{/items}}</ul>

{{! views/shared/row.mustache }}
<li>{{name}}</li>
```

A partial alone on an indented line has that indent applied to every line it
emits. Interpolated values are not re-indented, so a value containing newlines
keeps its own shape.

---

## parse_transform

Full guide: **[docs/parse-transform.md](docs/parse-transform.md)**.

```erlang
-module(my_views).
-compile({parse_transform, ai_mustache_transform}).

%% (a) declare a custom tag; see below
-mustache_tag({$@, my_i18n}).

%% (b) inline template, expanded at compile time
greet(Name) -> ai_mustache:inline(~"Hello {{name}}!", #{name => Name}).

%% (c) compile a template file into index/1 and index_iolist/1
-mustache_template({index, "views/index.mustache"}).
```

### There is no `~mustache` sigil

Erlang's sigils are a fixed set; a custom one does not lex. Inline templates
are therefore recognised as calls to `ai_mustache:inline/2` whose first
argument is a binary literal. `~"..."` is OTP 27's ordinary string sigil and
produces exactly that.

If the transform is not applied, or the first argument is not a literal, the
call still works -- it falls back to compiling at run time. Same output, just
slower. The one exception is custom tags: those are expanded by compile-time
callbacks and cannot run in the fallback path.

A non-literal first argument warns, since writing `inline` usually means you
wanted the zero-cost version; `nowarn_mustache_inline` in `erl_opts` turns that
off. Inline templates cannot use `{{> partial}}` -- there is no views directory
to resolve one against -- and that is a compile error.

### Custom tags

```erlang
-module(my_i18n).
-behaviour(ai_mustache_ext).
-export([markers/0, compile_tag/4]).

markers() -> [$@].

compile_tag($@, [Key], _Body, Opts) ->
    %% Return an abstract expression evaluating to iodata()
    ...
```

`# ^ / > ! = & { } + - *` are taken; a custom marker must be something else,
and two extensions may not claim the same character.

`-mustache_tag` **declares and checks**; it does not wire anything up. A
parse_transform only sees one module, so the module compiling your templates
cannot learn about the declaration from it. Actual registration goes in
`rebar.config`:

```erlang
{mustache_opts, [{extensions, [my_i18n]},
                 {ext_opts,   #{my_i18n => #{default_locale => en}}}]}.
```

What the attribute buys you is that a mistake -- a module that does not
implement the behaviour, or a marker it does not claim -- becomes a compile
error in the module that declared it instead of a puzzling failure later.

There is one exception to "declares but does not wire up": forms (b) and (c)
compile inside the very module that carries the attribute, so a `-mustache_tag`
there **is** used for that module's own inline and `-mustache_template`
templates. It still says nothing about `.mustache` files the plugin compiles.
If `extensions` is configured and a declared module is missing from it, the
transform warns.

### Known limitation of form (c)

rebar3 cannot see that `my_views.erl` depends on `views/index.mustache`; a
parse_transform has no way to register an extra file dependency. The plugin
compensates by scanning for `-mustache_template` attributes and touching the
`.erl` when the template changes. Forms (a) and (b) are unaffected.

---

## Differences from the spec

The required modules -- Comments, Delimiters, Interpolation, Inverted,
Partials, Sections -- all pass. These are deliberate deviations:

| | |
|---|---|
| `{{+x}}` / `{{-x}}` | aihtml extensions; the spec has no such tags |
| `{{*x}}` | aihtml extension |
| Key type | atoms, where the spec uses strings |
| Lambdas | return values are not re-parsed as templates |
| Dynamic Names, Blocks | not implemented (optional spec modules) |
| `'` escaping | escaped as `&#39;`, which the spec does not require |

---

## Performance

`bench/run.sh` compares against v0.3.7 on a page both versions render
byte-identically -- the harness refuses to report timings if the outputs ever
diverge. Median of seven runs on OTP 28, Linux x86-64:

| Page | v0.3.7 | 0.4.0 `render/1` | 0.4.0 `render_iolist/1` |
|---|---|---|---|
| 20 items, 1843 bytes | 632.5 us | 11.8 us (**53x**) | 10.0 us |
| 100 items, 8885 bytes | 3235.0 us | 51.9 us (**62x**) | 46.0 us |

The gap grows with the number of section iterations: the old runner rebuilt
the context with `maps:merge/2` for every element, which copies a map whose
size does not shrink, while pushing onto the stack is one cons cell.

See [bench/README.md](bench/README.md) for the methodology and where the rest
of the difference comes from.

---

## A worked example

`examples/` is a complete project: templates under `examples/views/`, a driver
in `examples/src/complex.erl`, and the plugin wired up in
`examples/rebar.config`.

```sh
sh examples/run.sh
```

It builds in a temporary copy, because aihtml and rebar3_aihtml have to reach
the example through `_checkouts` (rebar3 has no `path` resource) and
symlinking the repository root into a directory inside that repository would
be circular. The script prints the generated module list and the rendered
page.

The example is deliberately dense: it covers partial indentation, `{{+}}` and
`{{-}}`, a `true` section that does not push a scope, an inverted section, a
`0` that is truthy, `{{.}}`, and a `fun/2` lambda. `test/examples_tests.erl`
pins its output byte for byte.

## Projects using aihtml

- [aiwiki](https://github.com/DavidAlphaFox/aiwiki) -- a very simple blog.
  Its templates predate 0.4.0 and need migrating.

---

## Documentation

- [Using the parse_transform](docs/parse-transform.md) -- extension tags, inline templates, file templates
- [Benchmark](bench/README.md) -- methodology and results
- [Design notes](designs/README.md) -- why the engine is built this way

## Credit

The scanner's tag-splitting logic derives from
[bbmustache](https://github.com/soranoba/bbmustache) by Hinagiku Soranoba,
used under the MIT licence.

## Licence

MIT. See [LICENSE](LICENSE).
