#!/bin/sh
# Build and run the example end to end through the rebar3 plugin.
#
# The build happens in a temporary copy. Two reasons:
#
#   - aihtml and rebar3_aihtml must be wired in through _checkouts, because
#     rebar3 has no `path' resource: {path, Dir} deps and plugins fail with
#     fetch_fail. A real project would use the git form from the README.
#   - symlinking the repository root into examples/_checkouts would be
#     circular, since examples/ lives inside the repository. Copying only
#     src/ and include/ -- the same thing rebar3_aihtml_SUITE does for its
#     fixtures -- avoids both the cycle and dragging a _build tree along.
set -e

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

APP="$WORK/example"
cp -r "$ROOT/examples" "$APP"
rm -rf "$APP/_build" "$APP/_gen" "$APP/_checkouts" "$APP/rebar.lock"

seed() {  # seed <name> <dest>
    mkdir -p "$2"
    cp "$1/rebar.config" "$2/" 2>/dev/null || true
    for d in src include; do
        [ -d "$1/$d" ] && cp -r "$1/$d" "$2/"
    done
}
mkdir -p "$APP/_checkouts"
seed "$ROOT" "$APP/_checkouts/aihtml"
seed "$ROOT/rebar3_aihtml" "$APP/_checkouts/rebar3_aihtml"

cd "$APP"
echo "==> compiling (the mustache provider runs first and writes _gen/*.erl)"
rebar3 compile

echo
echo "==> generated modules"
ls _gen

echo
echo "==> rendering"
erl -noshell -pa _build/default/lib/*/ebin -pa _build/default/checkouts/*/ebin \
    -eval 'complex:start(), halt().'
