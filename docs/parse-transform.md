# Using the parse_transform

[English](parse-transform.md) · [中文](parse-transform.zh-CN.md) · [back to README](../README.md)

`ai_mustache_transform` is a parse_transform that handles three separate
things. You opt in per module:

```erlang
-module(my_views).
-compile({parse_transform, ai_mustache_transform}).
```

| Form | Syntax | What it does | Without the transform |
|---|---|---|---|
| **(a)** Extension tags | `-mustache_tag(my_i18n).` | Declares and **validates** an extension module | Nothing is validated; the extension still works if listed in `mustache_opts` |
| **(b)** Inline templates | `ai_mustache:inline(~"Hi {{name}}!", Ctx)` | Expands the template in place at compile time | Same output, parsed and interpreted at run time |
| **(c)** File templates | `-mustache_template({index, "views/index.mustache"}).` | Compiles a `.mustache` into `index/1` and `index_iolist/1` | **Hard failure** -- the functions do not exist |

Only (c) requires the transform. (a) and (b) degrade gracefully, which is
deliberate: forgetting the `-compile` line should not change behaviour, only
performance.

---

## There is no `~mustache` sigil

OTP's sigils are a closed set -- `~"..."`, `~b`, `~B`, `~s`, `~S` -- and an
unknown one is a **lexical** error, so `~mustache"..."` never reaches a
parse_transform at all. Form (b) therefore keys off an ordinary remote call
whose first argument is a binary literal.

`~"Hello {{x}}"` works fine: it is the standard string sigil (OTP 27+) and
produces a plain binary literal, exactly like `<<"Hello {{x}}">>`.

---

## (a) Extension tags

A custom marker lets you add syntax mustache does not have. The classic case
is compile-time i18n: `{{@ hello}}`.

### Writing an extension

```erlang
-module(my_i18n).
-behaviour(ai_mustache_ext).

-export([markers/0, compile_tag/4]).

markers() -> [$@].

table() ->
    #{hello => #{<<"en">> => <<"Hello">>, <<"fr">> => <<"Bonjour">>},
      bye   => #{<<"en">> => <<"Bye">>,   <<"fr">> => <<"Au revoir">>}}.

%% {{@ hello}} becomes:
%%   case ai_mustache_rt:lookup([locale], S) of
%%       <<"en">> -> <<"Hello">>;
%%       <<"fr">> -> <<"Bonjour">>;
%%       _        -> <<"hello">>
%%   end
compile_tag($@, [Key], _Body, Opts) ->
    A = ai_mustache_ext:anno(Opts),
    Table = ai_mustache_ext:ext_opt(?MODULE, Opts, table()),
    Locales = maps:get(Key, Table, #{}),
    Clauses = [{clause, A, [bin(A, L)], [], [bin(A, T)]}
               || {L, T} <- lists:sort(maps:to_list(Locales))]
        ++ [{clause, A, [{var, A, '_'}], [], [bin(A, atom_to_binary(Key, utf8))]}],
    {'case', A, ai_mustache_ext:lookup_expr([locale], Opts), Clauses}.

bin(A, <<>>) -> {bin, A, []};
bin(A, B)    -> {bin, A, [{bin_element, A, {string, A, binary_to_list(B)},
                           default, default}]}.
```

`compile_tag/4` returns an **abstract expression** that must evaluate to
`iodata()`. It runs at compile time, so the translation table above is baked
into the generated module and nothing is looked up while rendering.

### Do not hand-write `{var, 0, 'S'}`

The context stack's variable is renamed when a template is expanded inline, so
its name must come from `Opts`. `ai_mustache_ext` provides builders so you
never touch abstract forms for the common cases:

| Helper | Gives you |
|---|---|
| `anno(Opts)` | the `erl_anno:anno()` for the current tag, so errors point at the template line |
| `lookup_expr(Keys, Opts)` | `ai_mustache_rt:lookup(Keys, S)` |
| `escape_expr(Expr, Opts)` | `ai_mustache_rt:escape(Expr)` |
| `to_binary_expr(Expr, Opts)` | `ai_mustache_rt:to_binary(Expr)` |
| `stack_var(Opts)` / `indent_var(Opts)` | the variable names, if you need them raw |
| `stack_expr(Opts)` / `indent_expr(Opts)` | those as expressions |
| `ext_opt(Module, Opts, Default)` | your own configuration from `{ext_opts, #{...}}` |

### Block extensions

Add `block_markers/0` and the tag takes a body, which arrives **already
compiled** as a list of expressions:

```erlang
-module(my_wrap).
-behaviour(ai_mustache_ext).
-export([markers/0, block_markers/0, compile_tag/4]).

markers()       -> [$%].
block_markers() -> [$%].

%% {{%b}}text{{/b}}  ->  <b>text</b>
compile_tag($%, [Tag], Body, Opts) ->
    A = ai_mustache_ext:anno(Opts),
    T = atom_to_binary(Tag, utf8),
    mklist(A, [bin(A, <<"<", T/binary, ">">>)] ++ Body
              ++ [bin(A, <<"</", T/binary, ">">>)]).

mklist(A, [])      -> {nil, A};
mklist(A, [H | T]) -> {cons, A, H, mklist(A, T)}.

bin(A, B) -> {bin, A, [{bin_element, A, {string, A, binary_to_list(B)},
                        default, default}]}.
```

Without `block_markers/0` every marker the module claims is inline and `Body`
is `[]`.

### Declaring versus assembling -- the part that surprises people

```erlang
-mustache_tag(my_i18n).
```

This **declares and validates**. It does not assemble. A parse_transform sees
one module, so a declaration in `my_views.erl` says nothing about how the
rebar3 plugin compiles `views/*.mustache`. Registration lives in
`rebar.config`:

```erlang
{mustache_opts, [{extensions, [my_i18n]},
                 {ext_opts,   #{my_i18n => #{default_locale => en}}}]}.
```

Declare it anyway. The attribute turns a class of mistakes into compile errors
in the module that made them:

- the module does not exist, or does not `-behaviour(ai_mustache_ext)`
- it does not export `markers/0` or `compile_tag/4`
- its marker collides with a builtin (`# ^ / > ! = & { } + - *`)
- two extensions claim the same character
- `block_markers/0` returns a character `markers/0` does not claim

You also get a warning if you declare an extension that `mustache_opts` does
not list -- the commonest way to have an extension that quietly never runs.

The declaration **is** used within the same module: forms (b) and (c) compile
right there, so they see it.

### The extension must be loadable while its user compiles

`compile_tag/4` is called during compilation, so the extension module has to be
loadable in the compiler's VM by then -- the same requirement any
parse_transform module has. If it is not, expansion fails with
`{unknown_marker, Char}`.

Under rebar3, force the order:

```erlang
{erl_first_files, ["src/my_i18n.erl"]}.
```

Validation is more forgiving than expansion: when the beam does not exist yet,
`-mustache_tag` falls back to reading the extension's `.erl` to check the
behaviour and the callbacks. That catches a misspelled module or a missing
export on a clean tree, but it cannot execute `compile_tag/4` -- for that the
module must really be there.

`-mustache_tag({$@, my_i18n})` is accepted too, but the marker is redundant --
`markers/0` already says. `-mustache_tag({$@, {my_i18n, my_fun}})` warns: the
behaviour fixes the entry point at `compile_tag/4` and that is what gets
called.

---

## (b) Inline templates

```erlang
greet(Name) ->
    ai_mustache:inline(~"Hello {{name}}!", #{name => Name}).
```

With the transform this becomes the iolist the template builds, inlined into
`greet/1`. No parsing, no interpretation, no module lookup at run time.

**It degrades.** Without the transform the identical call still works -- it
parses and renders at run time, producing the same bytes. The test suite
asserts this byte for byte, because a fallback that quietly differs from the
compiled path is worse than no fallback.

Two things do not degrade:

- **Partials are rejected.** `{{> x}}` in an inline template is a compile error
  (`inline_partial_unsupported`) with the transform, and raises
  `partial_in_inline_template` without it. An inline template has no views
  directory to resolve against, and the two paths agree about that.
- **Custom tags need the transform.** `compile_tag/4` returns an abstract
  expression, which means nothing at run time. An inline template using a
  custom marker raises `{unknown_marker, Char}` on the interpreted path.

If the first argument is not a literal the call is left alone and you get a
warning:

```
the first argument of ai_mustache:inline/2 is not a binary literal, so it
cannot be expanded at compile time; this call will parse and interpret the
template on every invocation. Use <<"...">> or ~"..." to get the compiled
path, or add nowarn_mustache_inline to silence this.
```

Silence it per module with `-compile(nowarn_mustache_inline).` when the
template genuinely is dynamic.

---

## (c) File templates

```erlang
-mustache_template({index, "views/index.mustache"}).
-mustache_template("views/greet.mustache").          %% name taken from the file
```

Generates and exports:

```erlang
index/1         %% -> binary()
index_iolist/1  %% -> iolist()
greet/1
greet_iolist/1
```

The path is resolved against the module's own directory, then
`{mustache_opts, {views, ...}}`, then the compiler's `i` include path.

Two templates that would generate the same function name are a compile error
(`duplicate_template_name`); give one of them an explicit name.

### When to use this instead of the plugin

The rebar3 plugin is the normal way to compile templates -- it gives you one
module per template, incremental builds, orphan collection and cross-template
partials. Form (c) is for when you want a template's rendering to live as a
function of *your* module, or when you are not using the plugin at all.

### Known limitation

**rebar3 cannot see that your module depends on the template file.** A
parse_transform has no way to register an extra file dependency; only
`-include` is tracked. Editing `views/index.mustache` therefore does not on its
own rebuild `my_views.erl`.

The plugin closes this: it scans for `-mustache_template` attributes and
touches the `.erl` when the template's hash changes. Without the plugin, run
`rebar3 clean` or touch the module. Forms (a) and (b) are unaffected --
inline templates live in the `.erl` itself.

Partials inside a form-(c) template need the plugin, since the partial modules
have to exist (`template_partial_needs_plugin`).

---

## All three together

```erlang
-module(tf_all).
-compile({parse_transform, ai_mustache_transform}).

-mustache_tag(my_i18n).
-mustache_template({index, "views/index.mustache"}).
-mustache_template("views/greet.mustache").

-export([hi/2]).

hi(Name, Locale) ->
    ai_mustache:inline(~"{{@hello}}, {{name}}!",
                       #{name => Name, locale => Locale}).
```

`hi/2` uses the `{{@}}` marker `my_i18n` claims, and it resolves because the
declaration and the inline template are in the same module.

---

## Diagnostics

Errors and warnings go through the standard `{error, [{File, [{Line, Module,
Reason}]}], []}` channel with a `format_error/1`, so they look like any other
compile error and your editor can jump to them. For an inline template the line
is the **template's** line inside the literal, not the line the call starts on.

| Reason | Meaning |
|---|---|
| `{bad_mustache_tag, Term}` | the attribute's argument is not a module or `{Marker, Module}` |
| `{bad_mustache_template, Term}` | not a path or `{Name, Path}` |
| `{template_not_found, Path}` / `{template_unreadable, Path}` | resolution failed |
| `{duplicate_template_name, Name}` | two templates generate the same function |
| `{template_name_clash, Name}` | a generated name collides with one you wrote |
| `{inline_partial_unsupported, Name}` | `{{> x}}` in an inline template |
| `{template_partial_needs_plugin, Name}` | a partial in a form-(c) template |
| `{unclosed_tag, Keys}` / `{mismatched_close, _, _}` / `{partial_not_found, _}` | ordinary template syntax errors |
| `ext_not_assembled` (warning) | declared but missing from `mustache_opts` |
| `{ext_callback_name_ignored, M, F}` (warning) | `{Marker, {Mod, Fun}}` -- the name is ignored |
| `inline_not_literal` (warning) | see form (b) |

Extension validation failures come from `ai_mustache_ext` and are reported the
same way: `not_an_ext_module`, `ext_missing_callback`, `marker_reserved`,
`marker_conflict`, and so on.
