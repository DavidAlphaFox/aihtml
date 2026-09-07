#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Generate test/jinja_spec/*.json from tools/jinja_cases.py.

Expected output comes from the real CPython jinja2, never from a person:
designs/13-jinja-roadmap.md section 2 explains why.  The generated JSON is
committed, so running the test suite needs neither Python nor jinja2.

    python3 tools/gen_jinja_fixtures.py [--check]

--check regenerates into memory and fails if anything differs from what is on
disk, which is what CI should run.

Pinned to jinja2 3.1.x.  Changing that version means regenerating and
reviewing the diff.
"""

import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import jinja2                                       # noqa: E402
import jinja_cases                                  # noqa: E402

EXPECTED_JINJA = "3.1"
OUT_DIR = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                       "test", "jinja_spec")

# Must match the defaults in include/ai_jinja.hrl.  These are NOT jinja2's own
# defaults -- see designs/10-jinja-semantics.md, deviation J8.
ENV_OPTS = dict(
    autoescape=True,
    trim_blocks=True,
    lstrip_blocks=True,
    keep_trailing_newline=False,
    undefined=jinja2.Undefined,
)

# {% do %} is part of the language here, but an extension in CPython.
EXTENSIONS = ["jinja2.ext.do"]


def render(c):
    """Render one case with CPython jinja2.

    Returns ("ok", text) or ("raised", "ExceptionClass: message").
    """
    if c.get("raw_template") is not None or c["template"] is None:
        return ("skipped", "not renderable by the reference implementation")
    sources = dict(c.get("templates") or {})
    sources["__main__"] = c["template"]
    env = jinja2.Environment(loader=jinja2.DictLoader(sources),
                             extensions=EXTENSIONS, **ENV_OPTS)
    try:
        return ("ok", env.get_template("__main__").render(c.get("context") or {}))
    except Exception as e:                          # noqa: BLE001
        return ("raised", "%s: %s" % (type(e).__name__, e))


def build(c):
    out = {"name": c["name"], "group": c["group"], "stage": c["stage"]}
    if c.get("raw_template") is not None:
        out["raw_template"] = c["raw_template"]
    else:
        out["template"] = c["template"]
    if c.get("templates"):
        out["templates"] = c["templates"]
    if c.get("context"):
        out["context"] = c["context"]

    kind, result = render(c)

    if c.get("erlang_only"):
        out["error"] = c["error"]
        out["erlang_only"] = c["erlang_only"]
        return out, None

    if c.get("deviation"):
        out["deviation"] = c["deviation"]
        if "error" in c:
            out["error"] = c["error"]
        else:
            out["expected"] = c["ours"]
        out["cpython"] = result if kind != "ok" else repr(result)
        return out, None

    if c.get("error"):
        out["error"] = c["error"]
        out["cpython"] = result if kind != "ok" else repr(result)
        # A case we expect to reject that CPython happily rendered is worth a
        # note: it means we are stricter, which should be a declared deviation.
        note = None
        if kind == "ok":
            note = "%s/%s expects %s but CPython rendered %r" % (
                c["group"], c["name"], c["error"], result)
        return out, note

    if kind != "ok":
        return None, "%s/%s failed to render: %s" % (c["group"], c["name"], result)
    out["expected"] = result
    return out, None


def main(argv):
    check = "--check" in argv
    if not jinja2.__version__.startswith(EXPECTED_JINJA):
        sys.stderr.write("expected jinja2 %s.x, found %s\n"
                         % (EXPECTED_JINJA, jinja2.__version__))
        return 2

    groups = jinja_cases.all_cases()
    notes, failures, differ = [], [], []
    os.makedirs(OUT_DIR, exist_ok=True)

    for name, _fn in jinja_cases.GROUPS:
        built = []
        for c in groups[name]:
            rec, note = build(c)
            if rec is None:
                failures.append(note)
                continue
            if note:
                notes.append(note)
            built.append(rec)
        payload = {"group": name,
                   "generator": "jinja2 " + jinja2.__version__,
                   "env": dict({k: (v.__name__ if isinstance(v, type) else v)
                                for k, v in ENV_OPTS.items()},
                               extensions=EXTENSIONS),
                   "cases": built}
        text = json.dumps(payload, indent=1, sort_keys=True,
                          ensure_ascii=False) + "\n"
        path = os.path.join(OUT_DIR, name + ".json")
        if check:
            current = open(path, encoding="utf-8").read() if os.path.exists(path) else ""
            if current != text:
                differ.append(path)
        else:
            with open(path, "w", encoding="utf-8") as f:
                f.write(text)

    for n in notes:
        sys.stderr.write("note: %s\n" % n)
    for f in failures:
        sys.stderr.write("FAILED: %s\n" % f)
    if differ:
        for d in differ:
            sys.stderr.write("out of date: %s\n" % d)
        return 1
    total = sum(len(groups[g]) for g, _ in jinja_cases.GROUPS)
    sys.stderr.write("%d cases in %d groups, %d notes, %d failures\n"
                     % (total, len(jinja_cases.GROUPS), len(notes), len(failures)))
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
