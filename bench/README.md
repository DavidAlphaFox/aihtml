# Benchmark

Compares rendering throughput between the current implementation and v0.3.7
(commit `82c01eb`, the last release of the ETS-and-interpreter design).

```sh
sh bench/run.sh [ITERATIONS] [ITEMS]      # defaults: 20000 iterations, 20 items
AILIB=/path/to/ailib/ebin sh bench/run.sh # skip cloning ailib
```

## Why it is set up this way

**Two VMs.** Both versions define `ai_mustache`, so they cannot be loaded into
one node. Each harness runs in its own `erl`.

**The outputs are compared before the timings are.** This matters more than it
sounds. Three changes in 0.4.0 alter what a template renders, and any of them
would silently turn the benchmark into a comparison of two different jobs:

- the escape set shrank from eight characters to five, and the old `&`
  replacement was missing its semicolon
- a standalone partial now indents every line of its output, not just the first
- `{{+}}` / `{{-}}` resolve differently under a context stack

The benchmark templates avoid all three: no `/`, `=` or backtick in escaped
values, no `&`, the partial is not standalone, and no has-sections. The two
template files differ only in the way section fields are addressed
(`{{items.name}}` versus `{{name}}`), which is the semantic change itself.
`run.sh` fails loudly if the two outputs stop matching byte for byte.

There is one more trap: v0.3.7's `{{^x}}` fires only when the value is `[]`,
not when the key is missing, so the context carries `missing => []` to make
both versions take that branch.

**The old version is measured on its fast path.** v0.3.7 caches the parsed
template in the process dictionary on first render, so a cold measurement
would be dominated by parsing and ETS work that a real server pays once.
Both harnesses warm up for 1000 iterations before timing, then take the median
of seven runs.

## Results

Measured on OTP 28 / erts 16.4.0.3, Linux x86-64.

| Page | v0.3.7 `render/2` | 0.4.0 `render/1` | 0.4.0 `render_iolist/1` | Speedup |
|---|---|---|---|---|
| 20 items, 1843 bytes | 632.5 us | 11.8 us | 10.0 us | **53x** |
| 100 items, 8885 bytes | 3235.0 us | 51.9 us | 46.0 us | **62x** |

Median of seven runs, after a 1000-iteration warm-up. The gap widens with the
number of section iterations, which is what you would expect: the per-element
`maps:merge/2` the old runner performed grows with the size of the context,
while consing onto the stack does not.

`render_iolist/1` is the number that matters for a web server: handing the
iolist to cowboy skips the final `iolist_to_binary/1` entirely.

## Where the difference comes from

Roughly in order of magnitude:

1. **No ETS deep copy.** v0.3.7 stored the parsed template in ETS, and every
   `ets:lookup/2` copied the whole structure into the calling process. In
   0.4.0 the static text is in the module's literal pool and is shared by
   reference. (v0.3.7 also cached in the process dictionary, which avoided the
   repeated copy at the cost of one full copy per worker process -- the
   measurement above gives it that benefit.)
2. **No `maps:merge/2` per iteration.** The old runner built a fresh context
   map for every element of a section. 0.4.0 conses the element onto the
   stack.
3. **Escaping is one pass.** The old `html_escape/1` ran eight global
   `re:replace/4` calls over every interpolated value; 0.4.0 scans the binary
   once and returns the original unchanged when nothing needs escaping.
4. **iolist output.** The old runner appended to a binary accumulator and
   restarted the accumulator inside sections and lambdas, defeating the
   runtime's append optimisation.
5. **No interpretation.** Dispatch that the old runner did by matching on IR
   tuples at render time is expanded into case clauses at compile time.

---

# The jinja engine

There is no older implementation to compare against here, so this benchmark
answers the two questions the design left open rather than measuring a
speed-up.

```
sh bench/run_jinja.sh [ITERATIONS] [ITEMS]
```

Same machine, same page shape, 30000 iterations over 20 items:

| | µs/render |
|---|---|
| jinja `page render/1` | 12.72 |
| mustache `render/1` (the same page) | 12.36 |
| jinja `page render_iolist/1` | 11.00 |
| mustache `render_iolist/1` | 10.88 |

The two engines cost within 3% of each other on equivalent templates, which is
what one would expect: they emit the same kind of code and share the escape
and formatting routines.

## Does inheritance cost anything?

`designs/11-jinja-codegen.md` section 2.1 chose to merge the block table at
run time rather than flatten it at compile time, so that a base template
gaining a block does not force a rebuild of every descendant. The price is one
`maps:merge/2` and one extra cross-module call, once per render.

`inherit.j2` extends a base; `flat.j2` produces byte-identical output without
extending anything. The harness asserts they match, and then times both:

| | µs/render |
|---|---|
| `inherit render/1` | 2.43 |
| `flat render/1` | 2.33 |

**0.10 µs**, and it does not grow with the template: the merge happens once at
the entry of the chain, not per block or per node. The design's assumption
holds, and the increment stays correct.

## Does a static template really fold?

`static.j2` has no dynamic nodes at all. It renders in 0.01 µs -- the cost of
returning a literal -- and the harness asserts that `render_iolist/1` comes
back as a one-element list, so the fold is real and not just fast.

## Files

| | |
|---|---|
| `bench_ctx.erl` | the shared context, compiled into every VM |
| `bench_new.escript` | current mustache implementation; compiles templates the way the plugin does |
| `bench_old.escript` | v0.3.7; boots its loader gen_server and calls `bootstrap/1` |
| `bench_jinja.escript` | the jinja engine: page, inheritance, and the static fold |
| `new/views/`, `old/views/` | the same page written in the new and old mustache semantics |
| `jinja/views/` | the same page in jinja, plus the inheritance and static probes |
| `run.sh` | builds both mustache versions, runs both, and verifies the outputs match |
| `run_jinja.sh` | builds and runs the jinja benchmark |
