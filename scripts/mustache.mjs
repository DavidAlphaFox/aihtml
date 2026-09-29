// A build-time Mustache -> JavaScript compiler, the browser half of the
// shared templates in apps/aihtml/templates (the Erlang half is
// beamai_render's -mustache_template). Its semantics follow beamai_render
// so both sides render a template to the same bytes:
//
//   {{x}} escaped (& < > " ' -> &amp; &lt; &gt; &quot; &#39;), {{{x}}} and
//   {{&x}} raw, {{#x}} / {{^x}} sections, {{! }} comments, dotted names,
//   {{.}}, standalone tag lines removed as the spec says.
//   falsy: undefined, null, false, "", [] (0 is truthy, as in Erlang).
//   section value: array -> once per element, element pushed; object ->
//   once, pushed; true -> once; other scalar -> once, pushed.
//
// Not supported (a compile error): partials, set delimiters, lambdas.
//
// Template FILES lose one trailing newline before compiling (readTemplate),
// matching aihtml_tpl:safe/1 on the Erlang side; compile() itself follows
// the spec exactly.

export const RUNTIME = `{
  esc: function (v) {
    return this.str(v).replace(/[&<>"']/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c];
    });
  },
  str: function (v) {
    return v === undefined || v === null ? "" : String(v);
  },
  falsy: function (v) {
    return v === undefined || v === null || v === false || v === "" ||
      (Array.isArray(v) && v.length === 0);
  },
  lookup: function (stack, keys) {
    if (keys.length === 0) { return stack[0]; }
    var v, found = false;
    for (var i = 0; i < stack.length; i++) {
      var f = stack[i];
      if (f !== null && typeof f === "object" && !Array.isArray(f) &&
          Object.prototype.hasOwnProperty.call(f, keys[0])) {
        v = f[keys[0]]; found = true; break;
      }
    }
    if (!found) { return undefined; }
    for (var k = 1; k < keys.length; k++) {
      if (v === null || typeof v !== "object" || Array.isArray(v) ||
          !Object.prototype.hasOwnProperty.call(v, keys[k])) { return undefined; }
      v = v[keys[k]];
    }
    return v;
  }
}`;

function tokenize(src, name) {
  const toks = [];
  let i = 0;
  const pushText = (t) => {
    // split text into text and newline tokens (for standalone detection)
    const parts = t.split(/(\r?\n)/);
    for (const p of parts) {
      if (p === "") { continue; }
      toks.push(p === "\n" || p === "\r\n" ? { t: "nl", v: p } : { t: "text", v: p });
    }
  };
  while (i < src.length) {
    const open = src.indexOf("{{", i);
    if (open < 0) { pushText(src.slice(i)); break; }
    pushText(src.slice(i, open));
    let close, type, body;
    if (src[open + 2] === "{") {
      close = src.indexOf("}}}", open);
      if (close < 0) { throw new Error(`${name}: unclosed {{{`); }
      type = "&"; body = src.slice(open + 3, close); i = close + 3;
    } else {
      close = src.indexOf("}}", open);
      if (close < 0) { throw new Error(`${name}: unclosed {{`); }
      body = src.slice(open + 2, close); i = close + 2;
      const c = body.trim()[0];
      if ("#^/!&>=".includes(c)) { type = c; body = body.trim().slice(1); }
      else { type = "name"; }
    }
    if (type === ">") { throw new Error(`${name}: partials are not supported ({{>${body}}})`); }
    if (type === "=") { throw new Error(`${name}: set delimiters are not supported`); }
    toks.push({ t: type, v: body.trim() });
  }
  return toks;
}

// Remove standalone lines: a line whose only non-whitespace token is one
// section / inverted / close / comment tag loses its whitespace and its
// newline.
function standalone(toks) {
  const out = [];
  let line = [];
  const flush = (nl) => {
    const tags = line.filter((x) => x.t !== "text");
    const blank = line.every((x) => x.t !== "text" || /^[ \t]*$/.test(x.v));
    if (tags.length === 1 && "#^/!".includes(tags[0].t) && blank) {
      out.push(tags[0]);          // drop the whitespace and the newline
    } else {
      out.push(...line);
      if (nl) { out.push(nl); }
    }
    line = [];
  };
  for (const x of toks) {
    if (x.t === "nl") { flush(x); } else { line.push(x); }
  }
  flush(null);
  return out;
}

function parse(toks, name) {
  const root = [];
  const stack = [{ kids: root, v: null }];
  for (const x of toks) {
    const top = stack[stack.length - 1];
    if (x.t === "#" || x.t === "^") {
      const node = { t: x.t, v: x.v, kids: [] };
      top.kids.push(node);
      stack.push(node);
    } else if (x.t === "/") {
      if (stack.length === 1 || top.v !== x.v) {
        throw new Error(`${name}: {{/${x.v}}} does not close ${top.v ? "{{#" + top.v + "}}" : "anything"}`);
      }
      stack.pop();
    } else if (x.t !== "!") {
      top.kids.push(x);
    }
  }
  if (stack.length > 1) { throw new Error(`${name}: unclosed {{#${stack[stack.length - 1].v}}}`); }
  return root;
}

function keys(v) {
  return v === "." ? "[]" : JSON.stringify(v.split("."));
}

function gen(nodes, depth) {
  let code = "";
  for (const n of nodes) {
    if (n.t === "text" || n.t === "nl") {
      code += `o+=${JSON.stringify(n.v)};`;
    } else if (n.t === "name") {
      code += `o+=R.esc(R.lookup(S,${keys(n.v)}));`;
    } else if (n.t === "&") {
      code += `o+=R.str(R.lookup(S,${keys(n.v)}));`;
    } else if (n.t === "#") {
      const v = "v" + depth, i = "i" + depth, body = gen(n.kids, depth + 1);
      code += `var ${v}=R.lookup(S,${keys(n.v)});` +
        `if(!R.falsy(${v})){` +
        `if(Array.isArray(${v})){for(var ${i}=0;${i}<${v}.length;${i}++){S.unshift(${v}[${i}]);${body}S.shift();}}` +
        `else if(${v}===true){${body}}` +
        `else if(typeof ${v}==="function"){throw new Error("lambdas are not supported");}` +
        `else{S.unshift(${v});${body}S.shift();}}`;
    } else if (n.t === "^") {
      code += `if(R.falsy(R.lookup(S,${keys(n.v)}))){${gen(n.kids, depth + 1)}}`;
    }
  }
  return code;
}

// JavaScript source of a function(data) -> string. `R` must be in scope.
export function compile(src, name = "template") {
  const tree = parse(standalone(tokenize(src, name)), name);
  return `function(d){var S=[d],o="";${gen(tree, 0)}return o;}`;
}

// For node tests and tools: a callable template.
export function compileFn(src, name) {
  const R = new Function(`return ${RUNTIME};`)();
  return new Function("R", `return ${compile(src, name)};`)(R);
}

// A template file's source, without the file's trailing newline.
export function templateSource(text) {
  return text.replace(/\r?\n$/, "");
}
