# -*- coding: utf-8 -*-
"""Case sources for the aihtml jinja fixture suite.

A case is a dict:

    name       required, unique within its group
    template   required, the main template source
    templates  optional, {name: source} for include/extends/import targets
    context    optional, the render context (JSON-shaped)
    stage      required, the implementation stage that should light it up
    deviation  optional, a J-number from designs/10-jinja-semantics.md.
               Present means "do NOT take CPython's answer here".
    ours       optional, our expected output.  Only meaningful with deviation.
    error      optional, the reason atom we expect instead of output.

Expected output is filled in by tools/gen_jinja_fixtures.py from the real
CPython jinja2, never by hand -- see designs/13-jinja-roadmap.md section 2.
"""

# Stages, matching designs/13-jinja-roadmap.md.
S_EXPR = 9      # scanner / lexer / expression parser
S_STMT = 10     # statement parser + AST
S_RT = 11       # runtime, filters, tests
S_GEN = 12      # code generation


def case(name, template, stage, **kw):
    c = {"name": name, "template": template, "stage": stage}
    c.update(kw)
    return c


# ---------------------------------------------------------------- lexer

def lexer():
    out = []
    nums = [
        ("int", "{{ 123 }}"), ("int-underscore", "{{ 1_000 }}"),
        ("hex", "{{ 0x1f }}"), ("octal", "{{ 0o17 }}"),
        ("binary", "{{ 0b1010 }}"), ("float", "{{ 1.5 }}"),
        ("float-exp", "{{ 1e3 }}"), ("float-exp-neg", "{{ 1.5e-3 }}"),
        ("float-underscore", "{{ 1_000.5 }}"), ("zero", "{{ 0 }}"),
        ("negative", "{{ -7 }}"), ("float-negative", "{{ -1.5 }}"),
    ]
    for n, t in nums:
        out.append(case("number-" + n, t, S_EXPR))

    strs = [
        ("double", '{{ "abc" }}'), ("single", "{{ 'abc' }}"),
        ("escape-n", '{{ "a\\nb" }}'), ("escape-t", '{{ "a\\tb" }}'),
        ("escape-r", '{{ "a\\rb" }}'), ("escape-backslash", '{{ "a\\\\b" }}'),
        ("escape-quote", '{{ "a\\"b" }}'), ("escape-apos", "{{ 'a\\'b' }}"),
        ("escape-hex", '{{ "a\\x41b" }}'), ("escape-unicode", '{{ "a\\u4e2db" }}'),
        ("adjacent", '{{ "a" "b" }}'), ("empty", '{{ "" }}'),
        ("utf8", '{{ "中文" }}'),
    ]
    for n, t in strs:
        out.append(case("string-" + n, t, S_EXPR))

    out += [
        case("bool-true", "{{ true }}", S_EXPR),
        case("bool-True", "{{ True }}", S_EXPR),
        case("bool-false", "{{ false }}", S_EXPR),
        case("bool-False", "{{ False }}", S_EXPR),
        case("none-renders-empty", "[{{ none }}]", S_EXPR,
             deviation="J2", ours="[]"),
        case("None-renders-empty", "[{{ None }}]", S_EXPR,
             deviation="J2", ours="[]"),
        # The end delimiter may not be found inside a string literal.
        case("stop-inside-string", '{{ "%}" }}', S_EXPR),
        case("stop-inside-string-2", '{{ "}}" }}', S_EXPR),
        case("brace-depth", '{{ {"a": 1}["a"] }}', S_EXPR),
        case("comment-dropped", "a{# {{ x }} {% if %} #}b", S_EXPR),
        case("comment-multiline", "a{#\nstill\na comment\n#}b", S_EXPR),
        case("text-only", "no tags at all", S_EXPR),
        case("brace-not-a-tag", "{ not a tag }", S_EXPR),
        case("single-brace-then-tag", "{ {{ 1 }}", S_EXPR),
    ]
    return out


# ----------------------------------------------------------------- expr

def expr():
    ctx = {"x": 2, "y": 3, "s": "ab", "l": [1, 2, 3],
           "d": {"k": "v", "n": {"deep": 1}}, "t": True, "f": False}
    out = []
    binops = [
        ("add", "{{ x + y }}"), ("sub", "{{ x - y }}"), ("mul", "{{ x * y }}"),
        ("div", "{{ y / x }}"), ("floordiv", "{{ y // x }}"),
        ("mod", "{{ y % x }}"), ("pow", "{{ x ** y }}"),
        ("concat", "{{ s ~ y }}"), ("concat-num", "{{ x ~ y }}"),
        ("add-strings", '{{ s + "c" }}'), ("add-lists", "{{ l + [4] }}"),
        ("mod-negative", "{{ -7 % 3 }}"), ("div-exact", "{{ 4 / 2 }}"),
        ("floordiv-negative", "{{ -7 // 2 }}"),
    ]
    for n, t in binops:
        out.append(case("binop-" + n, t, S_EXPR, context=ctx))

    cmps = [("eq", "=="), ("ne", "!="), ("lt", "<"), ("gt", ">"),
            ("le", "<="), ("ge", ">=")]
    for n, op in cmps:
        out.append(case("compare-" + n, "{{ x %s y }}" % op, S_EXPR, context=ctx))
    out += [
        case("compare-int-float", "{{ 1 == 1.0 }}", S_EXPR),
        case("compare-strings", '{{ "a" < "b" }}', S_EXPR),
        case("compare-in-list", "{{ 2 in l }}", S_EXPR, context=ctx),
        case("compare-not-in-list", "{{ 9 not in l }}", S_EXPR, context=ctx),
        case("compare-in-string", '{{ "a" in s }}', S_EXPR, context=ctx),
        case("compare-in-map", '{{ "k" in d }}', S_EXPR, context=ctx),
    ]

    out += [
        case("bool-and", "{{ t and f }}", S_EXPR, context=ctx),
        case("bool-or", "{{ t or f }}", S_EXPR, context=ctx),
        case("bool-not", "{{ not t }}", S_EXPR, context=ctx),
        case("bool-and-value", "{{ 1 and 2 }}", S_EXPR),
        case("bool-or-value", "{{ 0 or 2 }}", S_EXPR),
        case("bool-or-short-circuit", "{{ 1 or 2 }}", S_EXPR),
        case("bool-and-short-circuit", "{{ 0 and 2 }}", S_EXPR),
        case("bool-precedence", "{{ true or false and false }}", S_EXPR),
        case("not-precedence", "{{ not false and true }}", S_EXPR),
        case("cond-true", "{{ 1 if t else 2 }}", S_EXPR, context=ctx),
        case("cond-false", "{{ 1 if f else 2 }}", S_EXPR, context=ctx),
        case("cond-no-else", "[{{ 1 if f }}]", S_EXPR, context=ctx),
        case("cond-nested", "{{ 1 if f else 2 if t else 3 }}", S_EXPR, context=ctx),
    ]

    # Precedence and associativity -- the answers CPython gives, not the ones
    # Python would.  designs/09-jinja-syntax.md section 3.2.
    out += [
        case("assoc-pow", "{{ 2 ** 3 ** 2 }}", S_EXPR),
        case("assoc-sub", "{{ 10 - 3 - 2 }}", S_EXPR),
        case("assoc-div", "{{ 16 / 4 / 2 }}", S_EXPR),
        case("prec-add-mul", "{{ 2 + 3 * 4 }}", S_EXPR),
        case("prec-paren", "{{ (2 + 3) * 4 }}", S_EXPR),
        case("prec-unary-pow", "{{ -2 ** 2 }}", S_EXPR),
        case("prec-filter-vs-add", "{{ 1 + -2|abs }}", S_EXPR),
        case("prec-filter-vs-unary", "{{ -3|abs }}", S_EXPR),
        case("prec-filter-vs-pow", "{{ 2 ** 3|abs }}", S_EXPR),
        case("prec-filter-chain", '{{ "  a  "|trim|upper }}', S_EXPR),
        case("prec-concat-vs-mul", "{{ 2 * 3 ~ 4 }}", S_EXPR),
        case("assoc-concat", '{{ "a" ~ "b" ~ "c" }}', S_EXPR),
    ]

    out += [
        case("attr", "{{ d.k }}", S_EXPR, context=ctx),
        case("attr-nested", "{{ d.n.deep }}", S_EXPR, context=ctx),
        case("attr-missing", "[{{ d.nope }}]", S_EXPR, context=ctx),
        case("attr-chain-on-missing", "[{{ d.nope.deeper }}]", S_EXPR, context=ctx,
             deviation="J14", ours="[]"),
        case("subscript-string-key", '{{ d["k"] }}', S_EXPR, context=ctx),
        case("subscript-index", "{{ l[0] }}", S_EXPR, context=ctx),
        case("subscript-negative", "{{ l[-1] }}", S_EXPR, context=ctx),
        case("subscript-expr", "{{ l[x - 1] }}", S_EXPR, context=ctx),
        case("slice", "{{ l[1:] }}", S_EXPR, context=ctx),
        case("slice-both", "{{ l[0:2] }}", S_EXPR, context=ctx),
        case("slice-step", "{{ [1,2,3,4,5][0:5:2] }}", S_EXPR),
        case("list-literal", "{{ [1, 2, 3] }}", S_EXPR),
        case("list-trailing-comma", "{{ [1, 2,] }}", S_EXPR),
        case("list-empty", "{{ [] }}", S_EXPR),
        case("map-literal", '{{ {"a": 1}["a"] }}', S_EXPR),
        case("tuple-literal", "{{ (1, 2) }}", S_EXPR),
        case("paren-is-not-a-tuple", "{{ (1) + 1 }}", S_EXPR),
        case("range-call", "{{ range(3)|list }}", S_EXPR),
        case("range-two-args", "{{ range(1, 4)|list }}", S_EXPR),
        case("dict-call", '{{ dict(a=1)["a"] }}', S_EXPR),
    ]
    out += [
        case("chained-comparison", "{{ 1 < x < 3 }}", S_EXPR, context=ctx,
             deviation="J3", error="chained_comparison"),
        case("identifier-ascii-only", "{{ 中 }}", S_EXPR,
             deviation="J10", error="unexpected_token"),
    ]
    return out


# ---------------------------------------------------------- whitespace

def whitespace():
    """The full explicit-marker x global-option matrix.

    designs/09-jinja-syntax.md section 2.  The awkward cases are the ones
    where a `-' crosses several blank lines and the one where interpolated
    text keeps its own padding; both have burnt other implementations.
    """
    out = []
    for left in ("{%", "{%-", "{%+"):
        for right in ("%}", "-%}", "+%}"):
            name = "block-%s-%s" % (
                {"{%": "plain", "{%-": "minus", "{%+": "plus"}[left],
                {"%}": "plain", "-%}": "minus", "+%}": "plus"}[right])
            tpl = "a\n  %s if true %s X %s endif %s\nb" % (
                left, right, left, right)
            out.append(case("trim-" + name, tpl, S_EXPR))
    for left in ("{{", "{{-"):
        for right in ("}}", "-}}"):
            name = "expr-%s-%s" % ("minus" if left == "{{-" else "plain",
                                    "minus" if right == "-}}" else "plain")
            out.append(case("trim-" + name,
                            "a\n   %s 1 %s\nb" % (left, right), S_EXPR))
    for left in ("{#", "{#-"):
        for right in ("#}", "-#}"):
            name = "comment-%s-%s" % ("minus" if left == "{#-" else "plain",
                                       "minus" if right == "-#}" else "plain")
            out.append(case("trim-" + name,
                            "a\n   %s c %s\nb" % (left, right), S_EXPR))
    out += [
        case("trim-across-blank-lines", "a\n\n\n   {{- 1 }}", S_EXPR),
        case("trim-right-across-blank-lines", "{{ 1 -}}\n\n\n   b", S_EXPR),
        case("trim-left-tabs", "a\t\t{{- 1 }}", S_EXPR),
        case("lstrip-needs-only-whitespace-before",
             "a\n  x {% if true %}Y{% endif %}\nb", S_EXPR),
        case("lstrip-at-line-start", "a\n{% if true %}Y{% endif %}\nb", S_EXPR),
        case("trailing-newline-dropped", "abc\n", S_EXPR),
        case("trailing-newlines-only-one-dropped", "abc\n\n", S_EXPR),
        case("no-trailing-newline", "abc", S_EXPR),
        case("empty-template", "", S_EXPR),
        case("only-a-newline", "\n", S_EXPR),
        case("raw-keeps-everything",
             "{% raw %}  {{ x }}\n {% if %} {% endraw %}", S_EXPR),
        case("raw-empty", "a{% raw %}{% endraw %}b", S_EXPR),
        case("raw-newlines", "{% raw %}\na\n{% endraw %}", S_EXPR),
        case("raw-trim", "a\n  {%- raw -%}  x  {%- endraw -%}\nb", S_EXPR),
        case("interpolated-whitespace-kept", "[{{- s -}}]", S_EXPR,
             context={"s": "  padded  "}),
        case("nested-block-indent",
             "{% for i in [1,2] %}\n  {% if i == 1 %}\n    one\n"
             "  {% endif %}\n{% endfor %}", S_EXPR),
        case("text-between-blocks",
             "{% if true %}a{% endif %}b{% if true %}c{% endif %}", S_EXPR),
        case("newline-inside-tag", "{{\n  1\n}}", S_EXPR),
        case("multiline-statement", "{% if\n  true\n%}x{% endif %}", S_EXPR),
        case("crlf-body", "a\r\n{{ 1 }}\r\nb", S_EXPR),
    ]
    return out


# ------------------------------------------------------------------- if

def if_():
    ctx = {"a": 1, "b": 0, "s": "", "l": [], "m": {}, "n": None}
    return [
        case("true", "{% if true %}yes{% endif %}", S_STMT),
        case("false", "[{% if false %}yes{% endif %}]", S_STMT),
        case("else", "{% if false %}a{% else %}b{% endif %}", S_STMT),
        case("elif", "{% if false %}a{% elif true %}b{% else %}c{% endif %}", S_STMT),
        case("elif-chain",
             "{% if false %}1{% elif false %}2{% elif false %}3{% elif true %}4"
             "{% else %}5{% endif %}", S_STMT),
        case("elif-none-match",
             "[{% if false %}1{% elif false %}2{% endif %}]", S_STMT),
        case("nested", "{% if true %}{% if true %}deep{% endif %}{% endif %}", S_STMT),
        case("truthy-nonzero", "{% if a %}y{% else %}n{% endif %}", S_STMT, context=ctx),
        case("falsy-zero", "{% if b %}y{% else %}n{% endif %}", S_STMT, context=ctx),
        case("falsy-empty-string", "{% if s %}y{% else %}n{% endif %}", S_STMT, context=ctx),
        case("falsy-empty-list", "{% if l %}y{% else %}n{% endif %}", S_STMT, context=ctx),
        case("falsy-empty-map", "{% if m %}y{% else %}n{% endif %}", S_STMT, context=ctx),
        case("falsy-none", "{% if n %}y{% else %}n{% endif %}", S_STMT, context=ctx),
        case("falsy-undefined", "{% if nope %}y{% else %}n{% endif %}", S_STMT),
        case("condition-with-filter",
             "{% if l|length == 0 %}empty{% endif %}", S_STMT, context=ctx),
        case("condition-with-test",
             "{% if a is odd %}odd{% endif %}", S_STMT, context=ctx),
        case("set-escapes-an-if",
             "{% if true %}{% set x = 1 %}{% endif %}{{ x }}", S_GEN),
    ]


# ------------------------------------------------------------------ for

def for_():
    ctx = {"xs": [10, 20, 30], "empty": [], "m": {"a": 1, "b": 2},
           "people": [{"name": "ann", "age": 30}, {"name": "bo", "age": 20}]}
    out = [
        case("basic", "{% for x in xs %}{{ x }},{% endfor %}", S_STMT, context=ctx),
        case("empty-sequence", "[{% for x in empty %}{{ x }}{% endfor %}]",
             S_STMT, context=ctx),
        case("else-runs-when-empty",
             "{% for x in empty %}a{% else %}none{% endfor %}", S_STMT, context=ctx),
        case("else-skipped-when-not-empty",
             "{% for x in xs %}a{% else %}none{% endfor %}", S_STMT, context=ctx),
        case("undefined-is-empty", "[{% for x in nope %}a{% endfor %}]", S_STMT),
        case("literal-list", "{% for x in [1,2] %}{{ x }}{% endfor %}", S_STMT),
        case("range", "{% for i in range(3) %}{{ i }}{% endfor %}", S_STMT),
        case("nested",
             "{% for i in [1,2] %}{% for j in [1,2] %}{{ i }}{{ j }} {% endfor %}"
             "{% endfor %}", S_STMT),
        case("destructure-items",
             "{% for k, v in m.items() %}{{ k }}={{ v }};{% endfor %}",
             S_STMT, context=ctx),
        case("map-iterates-keys",
             "{% for k in m %}{{ k }}{% endfor %}", S_STMT, context=ctx),
        case("map-keys", "{% for k in m.keys() %}{{ k }}{% endfor %}",
             S_STMT, context=ctx),
        case("map-values", "{% for v in m.values() %}{{ v }}{% endfor %}",
             S_STMT, context=ctx),
        case("inline-filter",
             "{% for x in xs if x > 10 %}{{ x }},{% endfor %}", S_STMT, context=ctx),
        case("inline-filter-with-loop-index",
             "{% for x in xs if x > 10 %}{{ loop.index }}:{{ x }},{% endfor %}",
             S_GEN, context=ctx),
        case("body-scope-does-not-escape",
             "{% for x in xs %}{% set y = x %}{% endfor %}[{{ y }}]", S_GEN, context=ctx),
        case("outer-binding-visible",
             "{% set p = '!' %}{% for x in xs %}{{ x }}{{ p }}{% endfor %}",
             S_GEN, context=ctx),
    ]
    loopfields = ["index", "index0", "revindex", "revindex0", "first", "last",
                  "length", "depth", "depth0"]
    for f in loopfields:
        out.append(case("loop-" + f,
                        "{%% for x in xs %%}{{ loop.%s }},{%% endfor %%}" % f,
                        S_GEN, context=ctx))
    out += [
        case("loop-previtem",
             "{% for x in xs %}[{{ loop.previtem }}]{% endfor %}", S_GEN, context=ctx),
        case("loop-nextitem",
             "{% for x in xs %}[{{ loop.nextitem }}]{% endfor %}", S_GEN, context=ctx),
        case("loop-cycle",
             "{% for x in xs %}{{ loop.cycle('a','b') }}{% endfor %}", S_GEN, context=ctx),
        case("loop-changed",
             "{% for x in [1,1,2] %}{{ loop.changed(x) }},{% endfor %}", S_GEN),
        case("loop-in-nested-belongs-to-inner",
             "{% for i in [1,2] %}{% for j in [7,8] %}{{ loop.index }}{% endfor %}"
             "{% endfor %}", S_GEN),
        case("loop-recursive",
             "{% for n in tree recursive %}{{ n.name }}"
             "({{ loop(n.children) }}){% endfor %}", S_GEN,
             context={"tree": [{"name": "a", "children": [
                 {"name": "b", "children": []}]}]}),
        case("loop-recursive-depth",
             "{% for n in tree recursive %}{{ loop.depth }}"
             "{{ loop(n.children) }}{% endfor %}", S_GEN,
             context={"tree": [{"name": "a", "children": [
                 {"name": "b", "children": []}]}]}),
        case("sort-then-iterate",
             "{% for p in people|sort(attribute='age') %}{{ p.name }},{% endfor %}",
             S_RT, context=ctx),
        case("iterate-tuple", "{% for x in (1,2) %}{{ x }}{% endfor %}", S_STMT),
        case("iterate-string-rejected", "{% for c in s %}{{ c }}{% endfor %}",
             S_RT, context={"s": "ab"}, deviation="J9", error="not_iterable"),
    ]
    return out


# ------------------------------------------------------------------ set

def set_():
    return [
        case("inline", "{% set x = 1 %}{{ x }}", S_GEN),
        case("expression", "{% set x = 1 + 2 %}{{ x }}", S_GEN),
        case("rebind", "{% set x = 1 %}{{ x }}{% set x = 2 %}{{ x }}", S_GEN),
        case("from-context", "{% set y = x * 2 %}{{ y }}", S_GEN, context={"x": 4}),
        case("shadows-context", "{% set x = 9 %}{{ x }}", S_GEN, context={"x": 1}),
        case("block-form", "{% set x %}hi{% endset %}{{ x }}", S_GEN),
        case("block-form-not-double-escaped",
             "{% set x %}a & b{% endset %}{{ x }}", S_GEN),
        case("block-form-with-tags",
             "{% set x %}{% for i in [1,2] %}{{ i }}{% endfor %}{% endset %}{{ x }}",
             S_GEN),
        case("used-later-only", "[{{ x }}]{% set x = 1 %}[{{ x }}]", S_GEN),
        case("with-block", "{% with a = 1 %}{{ a }}{% endwith %}[{{ a }}]", S_GEN),
        case("with-two-bindings",
             "{% with a = 1, b = 2 %}{{ a }}{{ b }}{% endwith %}", S_GEN),
        case("with-shadowing",
             "{% set a = 1 %}{% with a = 2 %}{{ a }}{% endwith %}{{ a }}", S_GEN),
        case("filter-block", "{% filter upper %}abc{% endfilter %}", S_GEN),
        case("filter-block-not-double-escaped",
             "{% filter upper %}a & b{% endfilter %}", S_GEN),
        case("filter-block-with-args",
             "{% filter replace('a','z') %}abc{% endfilter %}", S_GEN),
        case("filter-block-nested-tags",
             "{% filter upper %}{% for i in ['a'] %}{{ i }}{% endfor %}{% endfilter %}",
             S_GEN),
        case("do-produces-nothing", "[{% do 1 + 1 %}]", S_GEN),
        case("set-in-loop-body-per-iteration",
             "{% for i in [1,2] %}{% set j = i %}{{ j }}{% endfor %}", S_GEN),
        case("set-list", "{% set l = [1,2] %}{{ l|length }}", S_GEN),
        case("set-map", "{% set d = {'a': 1} %}{{ d['a'] }}", S_GEN),
    ]


# -------------------------------------------------------------- filters

# {filter: [(suffix, template, context), ...]} -- at least two per filter,
# one ordinary and one boundary, per tasks/T43.md.
FILTER_CASES = [
    ("upper",   [("basic", "{{ 'abc'|upper }}", None),
                 ("empty", "{{ ''|upper }}", None)]),
    ("lower",   [("basic", "{{ 'ABC'|lower }}", None),
                 ("utf8", "{{ 'ÄBC'|lower }}", None)]),
    ("capitalize", [("basic", "{{ 'hello world'|capitalize }}", None),
                    ("empty", "{{ ''|capitalize }}", None)]),
    ("title",   [("basic", "{{ 'hello world'|title }}", None),
                 ("mixed", "{{ 'hELLO'|title }}", None)]),
    ("trim",    [("basic", "[{{ '  a  '|trim }}]", None),
                 ("chars", "[{{ 'xxaxx'|trim('x') }}]", None)]),
    ("striptags", [("basic", "{{ '<b>hi</b>'|striptags }}", None),
                   ("nested", "{{ '<a><b>x</b></a>'|striptags }}", None)]),
    ("truncate", [("basic", "{{ s|truncate(10) }}",
                   {"s": "the quick brown fox jumps"}),
                  ("short", "{{ 'abc'|truncate(10) }}", None)]),
    ("wordwrap", [("basic", "{{ s|wordwrap(10) }}", {"s": "aaa bbb ccc ddd"}),
                  ("short", "{{ 'a'|wordwrap(10) }}", None)]),
    ("wordcount", [("basic", "{{ 'a b c'|wordcount }}", None),
                   ("empty", "{{ ''|wordcount }}", None)]),
    ("center",  [("basic", "[{{ 'a'|center(5) }}]", None),
                 ("too-wide", "[{{ 'abcdef'|center(3) }}]", None)]),
    ("indent",  [("basic", "{{ s|indent(2) }}", {"s": "a\nb"}),
                 ("first", "{{ s|indent(2, true) }}", {"s": "a\nb"})]),
    ("replace", [("basic", "{{ 'aaa'|replace('a','b') }}", None),
                 ("count", "{{ 'aaa'|replace('a','b',2) }}", None)]),
    ("format",  [("basic", "{{ '%s-%s'|format('a','b') }}", None),
                 ("number", "{{ '%d'|format(3) }}", None)]),
    ("urlencode", [("string", "{{ 'a b&c'|urlencode }}", None),
                   ("map", "{{ {'a': 1}|urlencode }}", None)]),
    ("escape",  [("basic", "{{ '<b>'|escape }}", None),
                 ("already-safe", "{{ '<b>'|safe|escape }}", None)]),
    ("e",       [("alias", "{{ '<b>'|e }}", None),
                 ("amp", "{{ 'a&b'|e }}", None)]),
    ("forceescape", [("breaks-safe", "{{ '<b>'|safe|forceescape }}", None),
                     ("plain", "{{ '<b>'|forceescape }}", None)]),
    ("safe",    [("basic", "{{ '<b>'|safe }}", None),
                 ("empty", "[{{ ''|safe }}]", None)]),
    ("string",  [("int", "{{ 3|string }}", None),
                 ("list", "{{ [1]|string }}", None)]),
    ("first",   [("basic", "{{ [1,2]|first }}", None),
                 ("empty", "[{{ []|first }}]", None)]),
    ("last",    [("basic", "{{ [1,2]|last }}", None),
                 ("empty", "[{{ []|last }}]", None)]),
    ("length",  [("list", "{{ [1,2]|length }}", None),
                 ("string", "{{ 'abc'|length }}", None)]),
    ("count",   [("alias", "{{ [1,2]|count }}", None),
                 ("map", "{{ {'a':1}|count }}", None)]),
    ("list",    [("string", "{{ 'ab'|list }}", None),
                 ("range", "{{ range(2)|list }}", None)]),
    ("join",    [("basic", "{{ [1,2]|join('-') }}", None),
                 ("attribute", "{{ ps|join(', ', attribute='n') }}",
                  {"ps": [{"n": "a"}, {"n": "b"}]})]),
    ("reverse", [("list", "{{ [1,2,3]|reverse|list }}", None),
                 ("string", "{{ 'abc'|reverse }}", None)]),
    ("sort",    [("basic", "{{ [3,1,2]|sort|list }}", None),
                 ("attribute", "{{ ps|sort(attribute='n')|map(attribute='n')|list }}",
                  {"ps": [{"n": "b"}, {"n": "a"}]})]),
    ("sum",     [("basic", "{{ [1,2,3]|sum }}", None),
                 ("empty", "{{ []|sum }}", None)]),
    ("min",     [("basic", "{{ [3,1,2]|min }}", None),
                 ("attribute", "{{ ps|min(attribute='n')|attr('n') }}",
                  {"ps": [{"n": 2}, {"n": 1}]})]),
    ("max",     [("basic", "{{ [3,1,2]|max }}", None),
                 ("strings", "{{ ['a','c','b']|max }}", None)]),
    ("unique",  [("basic", "{{ [1,1,2]|unique|list }}", None),
                 ("strings", "{{ ['a','A','a']|unique|list }}", None)]),
    ("slice",   [("basic", "{{ [1,2,3,4]|slice(2)|list }}", None),
                 ("fill", "{{ [1,2,3]|slice(2, 'x')|list }}", None)]),
    ("batch",   [("basic", "{{ [1,2,3]|batch(2)|list }}", None),
                 ("fill", "{{ [1,2,3]|batch(2, 0)|list }}", None)]),
    ("groupby", [("basic",
                  "{% for g, items in ps|groupby('k') %}{{ g }}:{{ items|length }};"
                  "{% endfor %}", {"ps": [{"k": "a"}, {"k": "a"}, {"k": "b"}]}),
                 ("empty", "[{% for g, i in []|groupby('k') %}x{% endfor %}]", None)]),
    ("map",     [("attribute", "{{ ps|map(attribute='n')|list }}",
                  {"ps": [{"n": 1}, {"n": 2}]}),
                 ("filter-name", "{{ ['a','b']|map('upper')|list }}", None)]),
    ("select",  [("test-name", "{{ [1,2,3]|select('odd')|list }}", None),
                 ("no-test", "{{ [0,1,2]|select|list }}", None)]),
    ("reject",  [("test-name", "{{ [1,2,3]|reject('odd')|list }}", None),
                 ("no-test", "{{ [0,1,2]|reject|list }}", None)]),
    ("selectattr", [("truthy", "{{ ps|selectattr('ok')|map(attribute='n')|list }}",
                     {"ps": [{"n": 1, "ok": True}, {"n": 2, "ok": False}]}),
                    ("with-test", "{{ ps|selectattr('n','odd')|map(attribute='n')|list }}",
                     {"ps": [{"n": 1}, {"n": 2}]})]),
    ("rejectattr", [("truthy", "{{ ps|rejectattr('ok')|map(attribute='n')|list }}",
                     {"ps": [{"n": 1, "ok": True}, {"n": 2, "ok": False}]}),
                    ("with-test", "{{ ps|rejectattr('n','odd')|map(attribute='n')|list }}",
                     {"ps": [{"n": 1}, {"n": 2}]})]),
    ("dictsort", [("by-key", "{{ {'b':1,'a':2}|dictsort|list }}", None),
                  ("by-value", "{{ {'b':1,'a':2}|dictsort(by='value')|list }}", None)]),
    ("items",   [("basic", "{% for k,v in {'a':1}|items %}{{ k }}{{ v }}{% endfor %}",
                  None),
                 ("empty", "[{% for k,v in {}|items %}x{% endfor %}]", None)]),
    ("attr",    [("basic", "{{ d|attr('a') }}", {"d": {"a": 1}}),
                 ("missing", "[{{ d|attr('z') }}]", {"d": {"a": 1}})]),
    ("tojson",  [("map", "{{ {'a': 1}|tojson }}", None),
                 ("script-safe", "{{ {'a': '</script>'}|tojson }}", None)]),
    ("abs",     [("negative", "{{ -3|abs }}", None),
                 ("float", "{{ -1.5|abs }}", None)]),
    ("round",   [("half-up", "{{ 0.5|round }}", None),
                 ("precision", "{{ 2.345|round(2) }}", None)]),
    ("int",     [("string", "{{ '42'|int }}", None),
                 ("bad", "{{ 'x'|int }}", None)]),
    ("float",   [("string", "{{ '1.5'|float }}", None),
                 ("bad", "{{ 'x'|float }}", None)]),
    ("filesizeformat", [("decimal", "{{ 1000|filesizeformat }}", None),
                        ("binary", "{{ 1024|filesizeformat(true) }}", None)]),
    ("default", [("undefined", "{{ nope|default('d') }}", None),
                 ("boolean", "{{ ''|default('d', true) }}", None)]),
    ("d",       [("alias", "{{ nope|d('d') }}", None),
                 ("defined", "{{ x|d('d') }}", {"x": "v"})]),
    ("pprint",  [("list", "{{ [1,2]|pprint }}", None),
                 ("map", "{{ {'a':1}|pprint }}", None)]),
    ("urlize",  [("basic", "{{ 'see http://x.test now'|urlize }}", None),
                 ("plain", "{{ 'no links'|urlize }}", None)]),
    ("random",  [("single-element", "{{ [7]|random }}", None),
                 ("single-element-again", "{{ ['a']|random }}", None)]),
]


def filters():
    out = []
    for name, cases in FILTER_CASES:
        for suffix, tpl, ctx in cases:
            kw = {"context": ctx} if ctx else {}
            out.append(case("%s-%s" % (name, suffix), tpl, S_RT, **kw))
    out += [
        case("undefined-input-survives-chain",
             "[{{ nope|upper|trim|default('d') }}]", S_RT),
        case("chain-order", "{{ '  ab '|trim|upper|replace('A','z') }}", S_RT),
    ]
    return out


# ---------------------------------------------------------------- tests

TEST_CASES = [
    ("defined", "{{ x is defined }}", {"x": 1}),
    ("defined-missing", "{{ nope is defined }}", None),
    ("undefined", "{{ nope is undefined }}", None),
    ("none", "{{ x is none }}", {"x": None}),
    ("boolean", "{{ true is boolean }}", None),
    ("false", "{{ false is false }}", None),
    ("true", "{{ true is true }}", None),
    ("integer", "{{ 1 is integer }}", None),
    ("float", "{{ 1.0 is float }}", None),
    ("number", "{{ 1 is number }}", None),
    ("string", "{{ 'a' is string }}", None),
    ("sequence", "{{ [1] is sequence }}", None),
    ("mapping", "{{ {'a':1} is mapping }}", None),
    ("iterable", "{{ [1] is iterable }}", None),
    ("callable", "{{ range is callable }}", None),
    ("sameas", "{{ true is sameas true }}", None),
    ("escaped", "{{ '<b>'|safe is escaped }}", None),
    ("not-escaped", "{{ '<b>' is escaped }}", None),
    ("in", "{{ 1 is in([1,2]) }}", None),
    ("eq", "{{ 1 is eq(1) }}", None),
    ("ne", "{{ 1 is ne(2) }}", None),
    ("lt", "{{ 1 is lt(2) }}", None),
    ("le", "{{ 1 is le(1) }}", None),
    ("gt", "{{ 2 is gt(1) }}", None),
    ("ge", "{{ 2 is ge(2) }}", None),
    ("odd", "{{ 3 is odd }}", None),
    ("even", "{{ 2 is even }}", None),
    ("divisibleby", "{{ 6 is divisibleby(3) }}", None),
    ("lower", "{{ 'ab' is lower }}", None),
    ("upper", "{{ 'AB' is upper }}", None),
    ("filter", "{{ 'upper' is filter }}", None),
    ("test", "{{ 'odd' is test }}", None),
    ("negated", "{{ 1 is not string }}", None),
    ("in-if", "{% if 3 is odd %}y{% endif %}", None),
    ("in-select", "{{ [1,2,3]|select('divisibleby', 3)|list }}", None),
    ("string-on-number", "{{ 1 is string }}", None),
    ("mapping-on-list", "{{ [1] is mapping }}", None),
    ("sequence-on-int", "{{ 1 is sequence }}", None),
    ("callable-on-int", "{{ 1 is callable }}", None),
    ("even-on-odd", "{{ 3 is even }}", None),
]


def tests():
    out = []
    for name, tpl, ctx in TEST_CASES:
        kw = {"context": ctx} if ctx else {}
        out.append(case(name, tpl, S_RT, **kw))
    # J2: none and undefined are one value here, so `is defined' answers
    # differently for an explicit none.  designs/10-jinja-semantics.md.
    out.append(case("defined-on-explicit-none", "{{ x is defined }}",
                    S_RT, context={"x": None}, deviation="J2", ours="False"))
    return out


# --------------------------------------------------------------- escape

def escape():
    ctx = {"raw": "<b>a & b</b>", "s": "plain"}
    transparent = ["upper", "lower", "trim", "center(9)", "indent(0)"]
    escaping = ["join", "title", "wordwrap(99)"]
    generating = ["tojson", "urlencode"]
    out = [
        case("autoescape-on", "{{ raw }}", S_RT, context=ctx),
        case("safe-turns-it-off", "{{ raw|safe }}", S_RT, context=ctx),
        case("escape-is-idempotent-through-safe",
             "{{ raw|safe|escape }}", S_RT, context=ctx),
        case("forceescape-breaks-safe",
             "{{ raw|safe|forceescape }}", S_RT, context=ctx),
        case("literal-in-template-is-not-escaped", "<b>{{ s }}</b>",
             S_RT, context=ctx),
        case("apostrophe", "{{ \"it's\" }}", S_RT),
        case("quote", '{{ \'say "hi"\' }}', S_RT),
        case("ampersand-once", "{{ 'a & b' }}", S_RT),
        case("slash-not-escaped", "{{ '/a/b' }}", S_RT),
        case("filter-block-single-escape",
             "{% filter upper %}a & b{% endfilter %}", S_GEN),
        case("set-block-single-escape",
             "{% set v %}a & b{% endset %}{{ v }}", S_GEN),
        case("set-block-then-filter",
             "{% set v %}a & b{% endset %}{{ v|upper }}", S_GEN),
        case("concat-escapes-both", "{{ raw ~ raw }}", S_RT, context=ctx),
        case("join-escapes-elements", "{{ ['<a>','<b>']|join(',') }}", S_RT),
        case("join-keeps-safe", "{{ ['<a>'|safe]|join(',') }}", S_RT),
        case("tojson-escapes-script", "{{ {'k': '</script>'}|tojson }}", S_RT),
        case("tojson-escapes-angle", "{{ '<'|tojson }}", S_RT),
        case("default-value-is-escaped", "{{ nope|default('<b>') }}", S_RT),
    ]
    for f in transparent:
        n = f.split("(")[0]
        out.append(case("transparent-%s-plain" % n,
                        "{{ raw|%s }}" % f, S_RT, context=ctx))
        out.append(case("transparent-%s-safe" % n,
                        "{{ raw|safe|%s }}" % f, S_RT, context=ctx))
    for f in escaping:
        n = f.split("(")[0]
        arg = "['<a>','<b>']" if n == "join" else "raw"
        out.append(case("escaping-%s" % n, "{{ %s|%s }}" % (arg, f),
                        S_RT, context=ctx))
    for f in generating:
        out.append(case("generating-%s" % f, "{{ '<x>'|%s }}" % f, S_RT))
    return out


# -------------------------------------------------------------- inherit

BASE = ("<html>{% block title %}base title{% endblock %}"
        "|{% block body %}base body{% endblock %}</html>")


def inherit():
    mid = ('{% extends "base.j2" %}'
           '{% block title %}mid{% endblock %}')
    leaf = ('{% extends "mid.j2" %}'
            '{% block body %}leaf{% endblock %}')
    return [
        case("no-override", '{% extends "base.j2" %}', S_GEN,
             templates={"base.j2": BASE}),
        case("override-one", '{% extends "base.j2" %}'
             '{% block title %}mine{% endblock %}', S_GEN,
             templates={"base.j2": BASE}),
        case("override-both", '{% extends "base.j2" %}'
             '{% block title %}t{% endblock %}{% block body %}b{% endblock %}',
             S_GEN, templates={"base.j2": BASE}),
        case("named-endblock", '{% extends "base.j2" %}'
             '{% block title %}t{% endblock title %}', S_GEN,
             templates={"base.j2": BASE}),
        case("super", '{% extends "base.j2" %}'
             '{% block title %}[{{ super() }}]{% endblock %}', S_GEN,
             templates={"base.j2": BASE}),
        case("three-levels", leaf, S_GEN,
             templates={"base.j2": BASE, "mid.j2": mid}),
        case("three-levels-super",
             '{% extends "mid2.j2" %}{% block title %}L({{ super() }}){% endblock %}',
             S_GEN,
             templates={"base.j2": BASE,
                        "mid2.j2": '{% extends "base.j2" %}'
                                   '{% block title %}M({{ super() }}){% endblock %}'}),
        case("block-uses-context", '{% extends "base.j2" %}'
             '{% block title %}{{ who }}{% endblock %}', S_GEN,
             context={"who": "me"}, templates={"base.j2": BASE}),
        case("top-level-content-dropped",
             'ignored{% extends "base.j2" %}also ignored', S_GEN,
             templates={"base.j2": BASE}),
        case("set-before-extends-visible",
             '{% extends "base.j2" %}{% set v = "V" %}'
             '{% block title %}{{ v }}{% endblock %}', S_GEN,
             templates={"base.j2": BASE}),
        case("block-inside-for-in-parent",
             '{% extends "loopbase.j2" %}{% block item %}<{{ i }}>{% endblock %}',
             S_GEN,
             templates={"loopbase.j2":
                        "{% for i in [1,2] %}{% block item %}{{ i }}{% endblock %}"
                        "{% endfor %}"}),
        case("scoped-block",
             '{% extends "sbase.j2" %}{% block item scoped %}<{{ i }}>{% endblock %}',
             S_GEN,
             templates={"sbase.j2":
                        "{% for i in [1,2] %}{% block item scoped %}{{ i }}"
                        "{% endblock %}{% endfor %}"}),
        case("block-in-the-middle",
             '{% extends "b2.j2" %}{% block m %}M{% endblock %}', S_GEN,
             templates={"b2.j2": "a{% block m %}x{% endblock %}b"}),
        case("empty-block-override",
             '{% extends "base.j2" %}{% block title %}{% endblock %}', S_GEN,
             templates={"base.j2": BASE}),
        case("blocks-render-in-parent-order",
             '{% extends "base.j2" %}{% block body %}B{% endblock %}'
             '{% block title %}T{% endblock %}', S_GEN,
             templates={"base.j2": BASE}),
        case("parent-with-include",
             '{% extends "b3.j2" %}{% block m %}M{% endblock %}', S_GEN,
             templates={"b3.j2": '{% include "frag.j2" %}'
                                 '{% block m %}x{% endblock %}',
                        "frag.j2": "F"}),
        case("child-block-with-loop",
             '{% extends "base.j2" %}'
             '{% block body %}{% for i in [1,2] %}{{ i }}{% endfor %}{% endblock %}',
             S_GEN, templates={"base.j2": BASE}),
        case("super-in-nested-block",
             '{% extends "b4.j2" %}{% block inner %}[{{ super() }}]{% endblock %}',
             S_GEN,
             templates={"b4.j2": "{% block outer %}O{% block inner %}I{% endblock %}"
                                 "{% endblock %}"}),
        case("escaping-inside-block",
             '{% extends "base.j2" %}{% block title %}{{ v }}{% endblock %}', S_GEN,
             context={"v": "<b>"}, templates={"base.j2": BASE}),
        case("filter-inside-block",
             '{% extends "base.j2" %}{% block title %}{{ v|upper }}{% endblock %}',
             S_GEN, context={"v": "hi"}, templates={"base.j2": BASE}),
        case("deep-chain-four",
             '{% extends "l3.j2" %}{% block title %}D{% endblock %}', S_GEN,
             templates={"base.j2": BASE,
                        "l2.j2": '{% extends "base.j2" %}',
                        "l3.j2": '{% extends "l2.j2" %}'}),
        case("parent-block-default-used-in-chain",
             '{% extends "l2b.j2" %}{% block body %}B{% endblock %}', S_GEN,
             templates={"base.j2": BASE, "l2b.j2": '{% extends "base.j2" %}'}),
        case("block-with-macro-call",
             '{% extends "base.j2" %}{% macro m() %}Mac{% endmacro %}'
             '{% block title %}{{ m() }}{% endblock %}', S_GEN,
             templates={"base.j2": BASE}),
        case("two-blocks-same-parent-body",
             '{% extends "b5.j2" %}{% block a %}A{% endblock %}'
             '{% block b %}B{% endblock %}', S_GEN,
             templates={"b5.j2": "{% block a %}1{% endblock %}-"
                                 "{% block b %}2{% endblock %}"}),
        case("whitespace-around-blocks",
             '{% extends "b6.j2" %}\n{% block m %}M{% endblock %}\n', S_GEN,
             templates={"b6.j2": "a\n  {% block m %}x{% endblock %}\nb"}),
        case("extends-then-include",
             '{% extends "base.j2" %}'
             '{% block title %}{% include "frag.j2" %}{% endblock %}', S_GEN,
             templates={"base.j2": BASE, "frag.j2": "F"}),
        case("required-block-provided",
             '{% extends "b7.j2" %}{% block m %}M{% endblock %}', S_GEN,
             templates={"b7.j2": "a{% block m required %}{% endblock %}b"}),
        case("block-name-mismatch",
             '{% extends "base.j2" %}{% block title %}t{% endblock body %}', S_GEN,
             templates={"base.j2": BASE}, error="block_name_mismatch"),
        case("duplicate-block",
             '{% extends "base.j2" %}{% block title %}a{% endblock %}'
             '{% block title %}b{% endblock %}', S_GEN,
             templates={"base.j2": BASE}, error="duplicate_block"),
        case("dynamic-extends", '{% extends parent %}', S_GEN,
             context={"parent": "base.j2"}, templates={"base.j2": BASE},
             deviation="J11", error="dynamic_target_unsupported"),
    ]


# ---------------------------------------------------------------- macro

def macro():
    lib = ("{% macro input(name, value='', type='text') %}"
           "<input name=\"{{ name }}\" value=\"{{ value }}\" type=\"{{ type }}\">"
           "{% endmacro %}"
           "{% macro shout(s) %}{{ s|upper }}!{% endmacro %}")
    return [
        case("define-and-call",
             "{% macro m() %}X{% endmacro %}{{ m() }}", S_GEN),
        case("positional-args",
             "{% macro m(a, b) %}{{ a }}-{{ b }}{% endmacro %}{{ m(1, 2) }}", S_GEN),
        case("keyword-args",
             "{% macro m(a, b) %}{{ a }}-{{ b }}{% endmacro %}{{ m(b=2, a=1) }}",
             S_GEN),
        case("default-values",
             "{% macro m(a, b=9) %}{{ a }}-{{ b }}{% endmacro %}{{ m(1) }}", S_GEN),
        case("default-overridden",
             "{% macro m(a, b=9) %}{{ a }}-{{ b }}{% endmacro %}{{ m(1, 2) }}", S_GEN),
        case("varargs",
             "{% macro m() %}{{ varargs|join(',') }}{% endmacro %}{{ m(1,2,3) }}",
             S_GEN),
        case("kwargs",
             "{% macro m() %}{{ kwargs['a'] }}{% endmacro %}{{ m(a=1) }}", S_GEN),
        case("escaping-inside",
             "{% macro m(v) %}{{ v }}{% endmacro %}{{ m('<b>') }}", S_GEN),
        case("macro-output-is-safe",
             "{% macro m() %}<b>{% endmacro %}{{ m() }}", S_GEN),
        case("cannot-see-caller-locals",
             "{% macro m() %}[{{ local }}]{% endmacro %}"
             "{% set local = 'x' %}{{ m() }}", S_GEN),
        case("sees-template-globals",
             "{% set g = 'G' %}{% macro m() %}{{ g }}{% endmacro %}{{ m() }}", S_GEN),
        case("loop-inside-macro",
             "{% macro m(l) %}{% for x in l %}{{ x }}{% endfor %}{% endmacro %}"
             "{{ m([1,2]) }}", S_GEN),
        case("macro-calls-macro",
             "{% macro a() %}A{% endmacro %}{% macro b() %}{{ a() }}B{% endmacro %}"
             "{{ b() }}", S_GEN),
        case("call-block",
             "{% macro m() %}[{{ caller() }}]{% endmacro %}"
             "{% call m() %}inner{% endcall %}", S_GEN),
        case("call-block-with-args",
             "{% macro m() %}{{ caller('x') }}{% endmacro %}"
             "{% call(v) m() %}<{{ v }}>{% endcall %}", S_GEN),
        case("call-block-nested",
             "{% macro m() %}[{{ caller() }}]{% endmacro %}"
             "{% call m() %}{% call m() %}deep{% endcall %}{% endcall %}", S_GEN),
        case("import-as",
             '{% import "lib.j2" as lib %}{{ lib.shout("hi") }}', S_GEN,
             templates={"lib.j2": lib}),
        case("import-as-with-args",
             '{% import "lib.j2" as lib %}{{ lib.input("n", value="v") }}', S_GEN,
             templates={"lib.j2": lib}),
        case("from-import",
             '{% from "lib.j2" import shout %}{{ shout("hi") }}', S_GEN,
             templates={"lib.j2": lib}),
        case("from-import-as",
             '{% from "lib.j2" import shout as s %}{{ s("hi") }}', S_GEN,
             templates={"lib.j2": lib}),
        case("from-import-two",
             '{% from "lib.j2" import shout, input %}{{ shout("a") }}'
             '{{ input("n") }}', S_GEN, templates={"lib.j2": lib}),
        case("import-with-context",
             '{% import "who.j2" as m with context %}{{ m.who() }}', S_GEN,
             context={"name": "ann"},
             templates={"who.j2": "{% macro who() %}{{ name }}{% endmacro %}"}),
        case("import-without-context",
             '{% import "who.j2" as m %}[{{ m.who() }}]', S_GEN,
             context={"name": "ann"},
             templates={"who.j2": "{% macro who() %}{{ name }}{% endmacro %}"}),
        case("macro-in-loop",
             "{% macro m(x) %}<{{ x }}>{% endmacro %}"
             "{% for i in [1,2] %}{{ m(i) }}{% endfor %}", S_GEN),
        case("recursive-macro",
             "{% macro m(n) %}{% if n > 0 %}{{ n }}{{ m(n - 1) }}{% endif %}"
             "{% endmacro %}{{ m(3) }}", S_GEN),
    ]


# -------------------------------------------------------------- include

def include():
    frag = "F({{ v }})"
    return [
        case("basic", 'a{% include "frag.j2" %}b', S_GEN,
             context={"v": 1}, templates={"frag.j2": frag}),
        case("twice", '{% include "frag.j2" %}{% include "frag.j2" %}', S_GEN,
             context={"v": 1}, templates={"frag.j2": frag}),
        case("in-loop",
             '{% for v in [1,2] %}{% include "frag.j2" %}{% endfor %}', S_GEN,
             templates={"frag.j2": frag}),
        case("nested", '{% include "outer.j2" %}', S_GEN, context={"v": 1},
             templates={"outer.j2": 'O[{% include "frag.j2" %}]',
                        "frag.j2": frag}),
        case("with-context-default", '{% include "frag.j2" %}', S_GEN,
             context={"v": "ctx"}, templates={"frag.j2": frag}),
        case("without-context", '[{% include "frag.j2" without context %}]', S_GEN,
             context={"v": "ctx"}, templates={"frag.j2": frag}),
        case("with-context-explicit",
             '{% include "frag.j2" with context %}', S_GEN,
             context={"v": "ctx"}, templates={"frag.j2": frag}),
        case("ignore-missing", '[{% include "gone.j2" ignore missing %}]', S_GEN),
        case("sees-set-binding",
             '{% set v = "set" %}{% include "frag.j2" %}', S_GEN,
             templates={"frag.j2": frag}),
        case("escaping-in-included",
             '{% include "frag.j2" %}', S_GEN, context={"v": "<b>"},
             templates={"frag.j2": frag}),
        case("included-has-its-own-blocks-ignored",
             '{% include "b.j2" %}', S_GEN,
             templates={"b.j2": "{% block x %}X{% endblock %}"}),
        case("include-a-template-that-includes",
             '{% include "l1.j2" %}', S_GEN,
             templates={"l1.j2": '1{% include "l2.j2" %}', "l2.j2": "2"}),
        case("include-inside-if",
             '{% if true %}{% include "frag.j2" %}{% endif %}', S_GEN,
             context={"v": 1}, templates={"frag.j2": frag}),
        case("include-missing-is-an-error", '{% include "gone.j2" %}', S_GEN,
             error="template_not_found"),
        case("dynamic-include", '{% include name %}', S_GEN,
             context={"name": "frag.j2"}, templates={"frag.j2": frag},
             deviation="J11", error="dynamic_target_unsupported"),
    ]


# ---------------------------------------------------------------- error

def error():
    """One case per reason in designs/11-jinja-codegen.md section 9.

    Reasons already exercised from another group (chained_comparison,
    dynamic_target_unsupported, duplicate_block, block_name_mismatch,
    template_not_found, not_iterable) are repeated here only where a second
    shape is worth pinning.
    """
    return [
        case("unclosed-if", "{% if true %}x", S_STMT, error="unclosed_block"),
        case("unclosed-for", "{% for x in [1] %}x", S_STMT, error="unclosed_block"),
        case("unclosed-block", "{% block a %}x", S_STMT, error="unclosed_block"),
        case("unclosed-macro", "{% macro m() %}x", S_STMT, error="unclosed_block"),
        case("unclosed-raw", "{% raw %}x", S_EXPR, error="unclosed_block"),
        case("unclosed-expr", "{{ x ", S_EXPR, error="unclosed_block"),
        case("unclosed-statement", "{% if true ", S_EXPR, error="unclosed_block"),
        case("unclosed-comment", "{# c ", S_EXPR, error="unclosed_block"),
        case("mismatched-end", "{% if true %}{% endfor %}", S_STMT,
             error="mismatched_end"),
        case("mismatched-end-block", "{% block a %}{% endif %}", S_STMT,
             error="mismatched_end"),
        case("orphan-else", "{% else %}", S_STMT, error="orphan_clause"),
        case("orphan-elif", "{% elif true %}", S_STMT, error="orphan_clause"),
        case("orphan-end", "{% endif %}", S_STMT, error="orphan_clause"),
        case("two-else-in-if", "{% if true %}a{% else %}b{% else %}c{% endif %}",
             S_STMT, error="orphan_clause"),
        case("elif-after-else",
             "{% if true %}a{% else %}b{% elif false %}c{% endif %}", S_STMT,
             error="orphan_clause"),
        case("unknown-statement", "{% autoescape true %}x{% endautoescape %}",
             S_STMT, deviation="J6", error="unknown_statement"),
        case("unknown-statement-trans", "{% trans %}x{% endtrans %}", S_STMT,
             deviation="J7", error="unknown_statement"),
        case("unknown-statement-debug", "{% debug %}", S_STMT,
             deviation="J7", error="unknown_statement"),
        case("unknown-statement-break",
             "{% for x in [1] %}{% break %}{% endfor %}", S_STMT,
             deviation="J7", error="unknown_statement"),
        case("unexpected-token-operator", "{{ + }}", S_EXPR,
             error="unexpected_token"),
        case("unexpected-token-juxtaposed", "{{ 1 2 }}", S_EXPR,
             error="unexpected_token"),
        case("unbalanced-paren", "{{ (1 }}", S_EXPR, error="unexpected_token"),
        case("extra-close-paren", "{{ 1) }}", S_EXPR, error="unexpected_token"),
        case("keyword-then-positional", "{{ range(stop=3, 1) }}", S_EXPR,
             error="unexpected_token"),
        case("nested-destructure",
             "{% for (a, b), c in xs %}x{% endfor %}", S_STMT,
             context={"xs": [[[1, 2], 3]]},
             deviation="J15", error="unexpected_token"),
        case("unknown-filter", "{{ 'a'|nosuchfilter }}", S_GEN,
             deviation="J4", error="unknown_filter"),
        case("unknown-filter-mid-chain", "{{ 'a'|upper|nosuch|lower }}", S_GEN,
             deviation="J4", error="unknown_filter"),
        case("unknown-test", "{{ 1 is nosuchtest }}", S_GEN,
             deviation="J4", error="unknown_test"),
        case("unknown-filter-in-block",
             "{% filter nosuchfilter %}a{% endfilter %}", S_GEN,
             deviation="J4", error="unknown_filter"),
        case("super-outside-block", "{{ super() }}", S_GEN,
             error="super_outside_block"),
        case("namespace-assignment",
             "{% set ns = namespace(t=0) %}{% set ns.t = 1 %}{{ ns.t }}", S_GEN,
             deviation="J16", error="namespace_assignment_unsupported"),
        case("import-missing-macro",
             '{% from "lib.j2" import nope %}{{ nope() }}', S_GEN,
             templates={"lib.j2": "{% macro m() %}x{% endmacro %}"},
             error="macro_not_found"),
        case("extends-self", '{% include "self.j2" %}', S_GEN,
             templates={"self.j2": '{% extends "self.j2" %}x'},
             error="extends_cycle"),
        case("include-missing", '{% include "gone.j2" %}', S_GEN,
             error="template_not_found"),
        case("extends-missing", '{% extends "gone.j2" %}', S_GEN,
             error="template_not_found"),
        case("import-missing", '{% import "gone.j2" as m %}', S_GEN,
             error="template_not_found"),
        case("invalid-utf8", None, S_EXPR, error="invalid_utf8",
             raw_template=[0x7b, 0x7b, 0x20, 0xff, 0x20, 0x7d, 0x7d]),
        case("divide-by-zero", "{{ 1 / 0 }}", S_RT, error="division_by_zero"),
        case("undefined-arithmetic", "{{ nope + 1 }}", S_RT,
             error="undefined_operation"),
        case("unsupported-operands", "{{ 'a' + 1 }}", S_RT,
             error="unsupported_operands"),
        case("not-callable", "{{ x() }}", S_RT, context={"x": 1},
             error="not_callable"),
        case("not-renderable", None, S_RT, error="not_renderable",
             erlang_only="a pid cannot come from JSON; asserted in Erlang"),
        case("filter-name-conflict", None, S_GEN, error="filter_name_conflict",
             erlang_only="needs a registered extension module"),
        case("unexpected-remote-calls", None, S_GEN,
             error="unexpected_remote_calls",
             erlang_only="only reachable by injecting a bad form"),
        case("target-in-inline-template", '{% include "x.j2" %}', S_GEN,
             error="target_in_inline_template",
             erlang_only="only applies on the inline path"),
        case("mutual-macro-in-inline",
             "{% macro a() %}{{ b() }}{% endmacro %}"
             "{% macro b() %}{{ a() }}{% endmacro %}", S_GEN,
             error="mutual_macro_in_inline",
             erlang_only="only applies on the inline path"),
        case("required-block-not-provided", '{% extends "b.j2" %}', S_GEN,
             templates={"b.j2": "{% block m required %}{% endblock %}"},
             error="required_block_not_provided"),
    ]


GROUPS = [
    ("lexer", lexer), ("expr", expr), ("whitespace", whitespace),
    ("if", if_), ("for", for_), ("set", set_),
    ("filters", filters), ("tests", tests), ("escape", escape),
    ("inherit", inherit), ("macro", macro), ("include", include),
    ("error", error),
]


def all_cases():
    out = {}
    for name, fn in GROUPS:
        cases = fn()
        seen = set()
        for c in cases:
            if c["name"] in seen:
                raise ValueError("duplicate case %s/%s" % (name, c["name"]))
            seen.add(c["name"])
            c["group"] = name
        out[name] = cases
    return out
