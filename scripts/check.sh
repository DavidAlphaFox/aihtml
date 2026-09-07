#!/bin/sh
# Run every check aihtml is expected to pass. Used by CI and before handing work over.
#
# The first dialyzer run has to build the PLT and can take several minutes;
# subsequent runs are fast.
set -e

cd "$(dirname "$0")/.."

echo "==> compile"
rebar3 compile

echo "==> compile (test profile)"
rebar3 as test compile

echo "==> eunit"
rebar3 eunit

echo "==> ct (drives real rebar3 subprocesses; slower)"
rebar3 ct

echo "==> xref"
rebar3 xref

echo "==> dialyzer (first run builds the PLT, this can take a few minutes)"
rebar3 dialyzer

# The jinja fixtures are generated from CPython jinja2 and committed, so the
# suite above needs no Python. This only re-checks that what is committed is
# what the generator produces, and is skipped where jinja2 is not installed.
if python3 -c "import jinja2" 2>/dev/null; then
    echo "==> jinja fixtures are up to date"
    python3 tools/gen_jinja_fixtures.py --check
else
    echo "==> skipping the jinja fixture check (no python3 jinja2)"
fi

echo "==> all checks passed"
