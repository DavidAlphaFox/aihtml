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

echo "==> xref"
rebar3 xref

echo "==> dialyzer (first run builds the PLT, this can take a few minutes)"
rebar3 dialyzer

echo "==> all checks passed"
