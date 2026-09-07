#!/bin/sh
# Benchmark the jinja engine.
#
# Unlike run.sh there is no older implementation to compare against, so this
# answers the two questions designs/11-jinja-codegen.md left open instead:
# what inheritance costs per render, and whether a static template really
# folds to a single literal. See bench/README.md.
#
#   sh bench/run_jinja.sh [ITERATIONS] [ITEMS]
set -e

cd "$(dirname "$0")/.."
N=${1:-30000}
ITEMS=${2:-20}

echo "==> building"
rebar3 compile >/dev/null
erlc -o bench bench/bench_ctx.erl

echo
echo "==> jinja"
escript bench/bench_jinja.escript _build/default/lib/aihtml/ebin bench "$N" "$ITEMS"

echo
echo "==> mustache, the same page, for scale"
escript bench/bench_new.escript _build/default/lib/aihtml/ebin bench "$N" "$ITEMS"
