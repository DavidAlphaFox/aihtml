#!/bin/sh
# Compare rendering throughput between the current implementation and v0.3.7
# (commit 82c01eb, the last release of the old design).
#
# The two versions cannot share a VM -- both define ai_mustache -- so each runs
# in its own erl, and both write their rendered page so the harness can prove
# they produced identical bytes before any timing is compared.
#
# v0.3.7 needs ailib, which this refactor removed. Point AILIB at a checkout of
# it, or let the script build one from git.
#
#   sh bench/run.sh [ITERATIONS] [ITEMS]
set -e

cd "$(dirname "$0")/.."
ROOT=$(pwd)
N=${1:-20000}
ITEMS=${2:-20}
WORK=${WORK:-$(mktemp -d)}
OLD_REV=82c01eb

echo "==> building the current implementation"
rebar3 compile >/dev/null
erlc -o bench bench/bench_ctx.erl

echo "==> extracting aihtml $OLD_REV into $WORK/old"
mkdir -p "$WORK/old/ebin"
git archive "$OLD_REV" | tar -x -C "$WORK/old"

if [ -z "$AILIB" ]; then
    echo "==> fetching ailib v0.4.5 (set AILIB=<path>/ebin to skip)"
    git clone -q --depth 1 -b v0.4.5 https://github.com/DavidAlphaFox/ailib.git "$WORK/ailib"
    mkdir -p "$WORK/ailib/ebin"
    erlc -o "$WORK/ailib/ebin" $(find "$WORK/ailib/src" -name '*.erl')
    AILIB="$WORK/ailib/ebin"
fi

echo "==> building aihtml $OLD_REV"
erlc -o "$WORK/old/ebin" -pa "$AILIB" "$ROOT"/../aihtml/bench/../src/*.erl 2>/dev/null || \
    erlc -o "$WORK/old/ebin" -pa "$AILIB" "$WORK"/old/src/*.erl

echo
echo "==> current (templates compiled to modules)"
escript bench/bench_new.escript _build/default/lib/aihtml/ebin bench "$N" "$ITEMS"
echo
echo "==> v0.3.7 (ets + process dictionary cache, tree-walking interpreter)"
escript bench/bench_old.escript "$WORK/old/ebin" "$AILIB" bench "$N" "$ITEMS"
echo

if cmp -s bench/out_old.txt bench/out_new.txt; then
    echo "==> outputs are byte-identical ($(wc -c < bench/out_new.txt) bytes) -- the comparison is like for like"
else
    echo "==> WARNING: outputs differ; the timings above are NOT comparable"
    diff bench/out_old.txt bench/out_new.txt | head
    exit 1
fi
