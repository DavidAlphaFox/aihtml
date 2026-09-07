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

## Files

| | |
|---|---|
| `bench_ctx.erl` | the shared context, compiled into both VMs |
| `bench_new.escript` | current implementation; compiles templates the way the plugin does |
| `bench_old.escript` | v0.3.7; boots its loader gen_server and calls `bootstrap/1` |
| `new/views/`, `old/views/` | the same page written in the new and old semantics |
| `run.sh` | builds both, runs both, and verifies the outputs match |
