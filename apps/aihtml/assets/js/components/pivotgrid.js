/* Pivot table behaviour (aihtml_pivotgrid), ported from sigil's
 * data/pivotgrid. The server renders the first view; this file re-renders
 * on every view change:
 *
 *   local mode   the engine below aggregates the rows of the root's JSON
 *                data island (script.ah-pg-data) with the same algorithm
 *                as aihtml_pivotgrid.erl and renders the shared templates
 *                AH.tpl.pivotgrid_grid / pivotgrid_fields, so the tables
 *                are byte for byte what the server renders;
 *   remote mode  (data-ah-remote) the new view is written to the root
 *                (data-ah-value, data-view) and 'ah:view' is fired; the
 *                action bound to it answers with pivotgrid_rows/3, which
 *                morphs the server-rendered tables in and calls viewLoaded.
 *
 * Everything else (selection, keyboard, context menu, field list,
 * column resizing, export) reads the rendered DOM, so it works the same
 * in both modes. */
import $ from "jquery";
import AH from "../core.js";
import "virtual:ah-tpl/pivotgrid_fields";
import "virtual:ah-tpl/pivotgrid_grid";

// ------------------------------------------------------------------
// Engine: the same steps as the engine of aihtml_pivotgrid.erl
// ------------------------------------------------------------------

var ROW = {};                 // the "count the record" marker of a measure without field

function key(p) { return JSON.stringify(p); }

// numbers, then texts by code point (Erlang's binary order), then blanks
function rank(k) { return typeof k === "number" ? 0 : k === null ? 2 : 1; }

function cmpStr(a, b) {
  if (!/[\uD800-\uDFFF]/.test(a + b)) { return a < b ? -1 : a > b ? 1 : 0; }
  var i = 0, j = 0;
  while (i < a.length && j < b.length) {
    var ca = a.codePointAt(i), cb = b.codePointAt(j);
    if (ca !== cb) { return ca < cb ? -1 : 1; }
    i += ca > 0xffff ? 2 : 1;
    j += cb > 0xffff ? 2 : 1;
  }
  return (a.length - i) - (b.length - j) < 0 ? -1 : (a.length - i) === (b.length - j) ? 0 : 1;
}

function kcmp(a, b) {
  var ra = rank(a), rb = rank(b);
  if (ra !== rb) { return ra < rb ? -1 : 1; }
  if (ra === 0) { return a < b ? -1 : a > b ? 1 : 0; }
  if (ra === 1) { return cmpStr(a, b); }
  return 0;
}

// a value as the server normalises it
function cell(v) {
  if (v === null || v === undefined) { return null; }
  if (typeof v === "number") { return isFinite(v) ? v : null; }
  if (typeof v === "boolean") { return v ? "true" : "false"; }
  return String(v);
}

function effValues(M) {
  return M.layout.values.length ? M.layout.values
    : [{ field: null, agg: "count", label: null }];
}

function prefixes(k) {
  var out = [];
  for (var n = 0; n <= k.length; n++) { out.push(key(k.slice(0, n))); }
  return out;
}

function addKids(k, map) {
  for (var n = 0; n < k.length; n++) {
    var pk = key(k.slice(0, n));
    var set = map.get(pk);
    if (!set) { set = new Map(); map.set(pk, set); }
    set.set(key(k[n]), k[n]);
  }
}

function add(a, v) {
  if (v === ROW) { a[2]++; return; }
  if (v === null || v === undefined) { return; }
  if (typeof v === "number") {
    a[0] = a[0] + v; a[1]++; a[2]++;
    if (a[3] === null || v < a[3]) { a[3] = v; }
    if (a[4] === null || v > a[4]) { a[4] = v; }
    a[5] = a[5] * v;
    return;
  }
  a[2]++;
}

function result(agg, a) {
  if (agg === "count" && a[2] > 0) { return a[2]; }
  if (a[1] === 0) { return null; }
  switch (agg) {
    case "sum": return a[0];
    case "avg": return a[0] / a[1];
    case "min": return a[3];
    case "max": return a[4];
    case "product": return a[5];
    default: return null;
  }
}

function aggregate(M, data, vs) {
  var idx = {};
  M.fieldNames.forEach(function (n, i) { idx[n] = i; });
  var ri = M.layout.rows.map(function (f) { return idx[f]; });
  var ci = M.layout.columns.map(function (f) { return idx[f]; });
  var vi = vs.map(function (v) { return v.field === null ? -1 : idx[v.field]; });
  var accs = new Map(), rk = new Map(), ck = new Map();
  function at(t, i) { var v = t[i]; return v === undefined ? null : v; }
  data.forEach(function (t) {
    var rkey = ri.map(function (i) { return at(t, i); });
    var ckey = ci.map(function (i) { return at(t, i); });
    var vals = vi.map(function (i) { return i < 0 ? ROW : at(t, i); });
    var rps = prefixes(rkey), cps = prefixes(ckey);
    for (var a = 0; a < rps.length; a++) {
      for (var b = 0; b < cps.length; b++) {
        var k = rps[a] + "\u0001" + cps[b];
        var acc = accs.get(k);
        if (!acc) {
          acc = vals.map(function () { return [0, 0, 0, null, null, 1]; });
          accs.set(k, acc);
        }
        for (var n = 0; n < vals.length; n++) { add(acc[n], vals[n]); }
      }
    }
    addKids(rkey, rk);
    addKids(ckey, ck);
  });
  return { accs: accs, rkids: rk, ckids: ck };
}

function valueAt(E, vs, rp, cp, v) {
  var acc = E.accs.get(key(rp) + "\u0001" + key(cp));
  return acc ? result(vs[v].agg, acc[v]) : null;
}

function keyCmp(dir) {
  return dir === "desc" ? function (a, b) { return kcmp(b, a); } : kcmp;
}

function valueCmp(E, vs, parent, s) {
  var v = Math.min(s.vi, vs.length - 1);
  return function (a, b) {
    var va = valueAt(E, vs, parent.concat([a]), s.col, v);
    var vb = valueAt(E, vs, parent.concat([b]), s.col, v);
    if (va === null && vb === null) { return kcmp(a, b); }
    if (va === null) { return 1; }
    if (vb === null) { return -1; }
    if (va < vb) { return s.dir === "asc" ? -1 : 1; }
    if (va > vb) { return s.dir === "asc" ? 1 : -1; }
    return kcmp(a, b);
  };
}

function kidsOf(kids, parent) {
  var set = kids.get(key(parent));
  return set ? Array.from(set.values()) : null;
}

function flatten(kids, exp, sortFun, parent, depth, out) {
  var ks = kidsOf(kids, parent);
  if (!ks) { return out; }
  sortFun(parent, ks).forEach(function (k) {
    var p = parent.concat([k]), pk = key(p);
    var has = kids.has(pk), open = has && exp.has(pk);
    out.push({ path: p, key: k, depth: depth, kids: has, exp: open });
    if (open) { flatten(kids, exp, sortFun, p, depth + 1, out); }
  });
  return out;
}

function expandedSet(paths, kids) {
  var s = new Set();
  if (paths === "all") {
    kids.forEach(function (_, k) { if (k !== "[]") { s.add(k); } });
  } else {
    (paths || []).forEach(function (p) { s.add(key(p)); });
  }
  return s;
}

function span(n) { return n <= 1 ? "" : String(n); }
function tot(b) { return b ? " ah-pg-total" : ""; }
function leafCls(l) {
  return l.kind === "grand" ? " ah-pg-grand-total" : l.kind === "subtotal" ? " ah-pg-total" : "";
}

function fieldOf(M, name) {
  for (var i = 0; i < M.fields.length; i++) {
    if (M.fields[i].name === name) { return M.fields[i]; }
  }
  return null;
}

function fieldLabel(M, f) { return fieldOf(M, f).label; }

function valueLabel(M, v) {
  if (v.label !== null && v.label !== undefined) { return v.label; }
  if (v.field === null) { return M.labels.count; }
  return fieldLabel(M, v.field) + " (" + M.labels[v.agg] + ")";
}

function keyLabel(M, k) {
  return k === null ? M.labels.blank : typeof k === "number" ? String(k) : k;
}

function raw(v) {
  if (v === null) { return ""; }
  return String(v);
}

function thousands(int, sep) {
  if (!sep || int.length <= 3) { return int; }
  var first = int.length % 3 || 3, out = int.slice(0, first);
  for (var i = first; i < int.length; i += 3) { out += sep + int.slice(i, i + 3); }
  return out;
}

function formatNumber(v, f) {
  var a = Math.abs(v), str;
  if (f.decimals === null || f.decimals === undefined) {
    str = a === Math.trunc(a) ? a.toFixed(0) : a.toFixed(2);
  } else {
    str = a.toFixed(f.decimals);
  }
  var parts = str.split(".");
  return f.prefix + (v < 0 ? "-" : "") + thousands(parts[0], f.thousands) +
    (parts.length > 1 ? f.decimal + parts[1] : "") + f.suffix;
}

function formatValue(M, v, spec) {
  if (v === null) { return ""; }
  var fmt;
  if (spec.agg === "count") {
    fmt = $.extend({}, M.format, { decimals: null, prefix: "", suffix: "" });
  } else {
    var f = spec.field === null ? null : fieldOf(M, spec.field);
    fmt = (f && f.format) || M.format;
  }
  return formatNumber(v, fmt);
}

// The column tree below parent: its leaves and header cells (tagged
// with their level and, for a leaf cell, the leaf).
function colWalk(M, E, exp, sortFun, parent, level, D, vcEff) {
  var leaves = [], cells = [];
  sortFun(parent, kidsOf(E.ckids, parent)).forEach(function (k) {
    var p = parent.concat([k]), pk = key(p);
    var has = E.ckids.has(pk), open = has && exp.has(pk);
    var label = keyLabel(M, k);
    var base = {
      path: key(p), label: label, lvl: level,
      toggle: !has ? "" : open ? "ah-pg-toggle ah-pg-toggle-open" : "ah-pg-toggle ah-pg-toggle-closed",
      expanded: !has ? "" : open ? "true" : "false"
    };
    if (open) {
      var sub = colWalk(M, E, exp, sortFun, p, level + 1, D, vcEff);
      var st = { kind: "subtotal", agg: p };
      var ls = sub.leaves.concat(M.col_subtotals ? [st] : []);
      cells.push($.extend(base, { cls: "ah-pg-col-th", colspan: span(ls.length * vcEff), rowspan: "" }));
      cells.push.apply(cells, sub.cells);
      if (M.col_subtotals) {
        cells.push({ cls: "ah-pg-col-th ah-pg-total", colspan: span(vcEff), rowspan: span(D - level),
                     path: "", toggle: "", expanded: "", lvl: level + 1, leaf: st,
                     label: label + " " + M.labels.subtotal });
      }
      leaves.push.apply(leaves, ls);
    } else {
      var leaf = { kind: "member", agg: p };
      cells.push($.extend(base, { cls: "ah-pg-col-th", colspan: span(vcEff),
                                  rowspan: span(D - level + 1), leaf: leaf }));
      leaves.push(leaf);
    }
  });
  return { leaves: leaves, cells: cells };
}

function noSort() { return { sort: "", ci: "", sort_icon: "", aria_sort: "" }; }

function omit(o, ks) {
  var r = {};
  Object.keys(o).forEach(function (k) { if (ks.indexOf(k) < 0) { r[k] = o[k]; } });
  return r;
}

function gridView(M, vs, E, rowList, colList, cexp, csort) {
  var L = M.labels, vc = vs.length;
  var vor = M.values_on_rows && vc > 1, vcEff = vor ? 1 : vc;
  var gt = M.grand_totals, hasCols = M.layout.columns.length > 0;
  var hasRows = M.layout.rows.length > 0, grand = L.grand_total;
  var rowSort = M.view.row_sort;
  var D = colList.reduce(function (m, c) { return Math.max(m, c.path.length); }, 0);
  var walk = hasCols ? colWalk(M, E, cexp, csort, [], 1, D, vcEff) : { leaves: [], cells: [] };
  var leaves = walk.leaves.concat(gt || !hasCols ? [{ kind: "grand", agg: [] }] : []);
  var phys = [];
  leaves.forEach(function (l) { for (var v = 0; v < vcEff; v++) { phys.push([l, v]); } });
  function sortAttrs(leaf, v, ci) {
    var dir = rowSort && rowSort.by === "value" && key(rowSort.col) === key(leaf.agg) &&
      rowSort.vi === v ? rowSort.dir : "";
    return {
      sort: JSON.stringify([leaf.agg, v]), ci: String(ci),
      sort_icon: dir === "asc" ? "▲" : dir === "desc" ? "▼" : "",
      aria_sort: dir === "asc" ? "ascending" : dir === "desc" ? "descending" : ""
    };
  }
  var valueRow = vcEff > 1 || !hasCols;
  function leafCells(level) {
    return walk.cells.filter(function (c) { return c.lvl === level; }).map(function (c) {
      var extra;
      if (!valueRow && c.leaf) {
        var ci = leaves.indexOf(c.leaf);
        extra = sortAttrs(leaves[ci], 0, ci);
      } else {
        extra = noSort();
      }
      return $.extend(omit(c, ["leaf", "lvl"]), extra);
    });
  }
  function grandCell() {
    var base = { cls: "ah-pg-col-th ah-pg-grand-total", colspan: span(vcEff), rowspan: span(D),
                 path: "", toggle: "", expanded: "", label: grand };
    return $.extend(base, valueRow ? noSort()
      : sortAttrs(leaves[leaves.length - 1], 0, leaves.length - 1));
  }
  var hrows = [];
  for (var lv = 1; lv <= D; lv++) {
    hrows.push({ cls: "ah-pg-col-header-row",
                 cells: leafCells(lv).concat(lv === 1 && gt ? [grandCell()] : []) });
  }
  if (valueRow) {
    hrows.push({
      cls: "ah-pg-col-header-row ah-pg-value-label-row",
      cells: phys.map(function (pv, ci) {
        return $.extend({
          cls: "ah-pg-col-th ah-pg-value-label" + leafCls(pv[0]),
          colspan: "", rowspan: "", path: "", toggle: "", expanded: "",
          label: !hasCols && vor ? grand : valueLabel(M, vs[pv[1]])
        }, sortAttrs(pv[0], pv[1], ci));
      })
    });
  }
  var entries = hasRows ? rowList.concat(gt ? [{ grand: true }] : []) : [{ grand: true }];
  var visRows = [];
  entries.forEach(function (e) {
    if (vor) { for (var v = 0; v < vc; v++) { visRows.push([e, v]); } } else { visRows.push([e, 0]); }
  });
  function isTotal(e) { return e.grand ? true : e.exp && M.row_subtotals; }
  var rows = visRows.map(function (ev) {
    var e = ev[0], v = ev[1], total = isTotal(e);
    var head = v !== 0 ? [] : [e.grand
      ? { rowspan: vor ? span(vc) : "", expanded: "", indent: "0", toggle: "", label: grand }
      : { rowspan: vor ? span(vc) : "",
          expanded: !e.kids ? "" : e.exp ? "true" : "false",
          indent: String(e.depth * 20),
          toggle: !e.kids ? "ah-pg-toggle ah-pg-toggle-leaf"
            : e.exp ? "ah-pg-toggle ah-pg-toggle-open" : "ah-pg-toggle ah-pg-toggle-closed",
          label: keyLabel(M, e.key) }];
    return {
      cls: "ah-pg-row-header" + tot(total), path: e.grand ? "[]" : key(e.path),
      vi: vor ? String(v) : "", head: head, vlabel: vor ? valueLabel(M, vs[v]) : ""
    };
  });
  var body = visRows.map(function (ev, ri) {
    var e = ev[0], total = isTotal(e), rp = e.grand ? [] : e.path;
    var blank = !!e.exp && !M.row_subtotals;
    return {
      cls: "ah-pg-body-row" + tot(total),
      cells: phys.map(function (pv, ci) {
        var leaf = pv[0], v = vor ? ev[1] : pv[1];
        var val = blank ? null : valueAt(E, vs, rp, leaf.agg, v);
        return {
          id: M.id + "-c" + ri + "-" + ci,
          cls: "ah-pg-cell" + (leaf.kind === "grand" ? " ah-pg-grand-total"
            : leaf.kind === "subtotal" ? " ah-pg-total" : tot(total)),
          v: raw(val), text: formatValue(M, val, vs[v])
        };
      })
    };
  });
  return {
    empty: "",
    corner: M.layout.rows.map(function (f) { return fieldLabel(M, f); }).join(" / "),
    cols: phys.map(function () { return {}; }),
    hrows: hrows, rows: rows, body: body
  };
}

function fieldsView(M) {
  var L = M.labels, used = M.layout.rows.concat(M.layout.columns);
  function zone(z, chips) {
    return {
      zone: z, label: L[z], empty: L.drop,
      chips: chips.map(function (c, i) {
        return { id: M.id + "-chip-" + z + "-" + i, zone: z, index: String(i),
                 field: c[0], label: c[1] };
      })
    };
  }
  return {
    zones: [
      zone("fields", M.fields.filter(function (f) { return used.indexOf(f.name) < 0; })
        .map(function (f) { return [f.name, f.label]; })),
      zone("rows", M.layout.rows.map(function (f) { return [f, fieldLabel(M, f)]; })),
      zone("columns", M.layout.columns.map(function (f) { return [f, fieldLabel(M, f)]; })),
      zone("values", M.layout.values.map(function (v) {
        return [v.field === null ? "" : v.field, valueLabel(M, v)];
      }))
    ]
  };
}

// The template data of both templates for model M and rows, and the
// view with "all" expansions resolved.
function views(M, rows) {
  var vs = effValues(M);
  var E = aggregate(M, rows, vs);
  var rexp = expandedSet(M.view.expanded_rows, E.rkids);
  var cexp = expandedSet(M.view.expanded_cols, E.ckids);
  var rs = M.view.row_sort;
  var rsort = rs && rs.by === "value"
    ? function (parent, ks) { return ks.sort(valueCmp(E, vs, parent, rs)); }
    : function (parent, ks) { return ks.sort(keyCmp(rs ? rs.dir : "asc")); };
  var csort = function (parent, ks) { return ks.sort(keyCmp(M.view.col_sort)); };
  var rowList = flatten(E.rkids, rexp, rsort, [], 0, []);
  var colList = flatten(E.ckids, cexp, csort, [], 0, []);
  var view = $.extend({}, M.view, {
    expanded_rows: Array.from(rexp).map(function (k) { return JSON.parse(k); }),
    expanded_cols: Array.from(cexp).map(function (k) { return JSON.parse(k); })
  });
  var grid = rows.length ? gridView(M, vs, E, rowList, colList, cexp, csort)
    : { empty: M.labels.empty };
  return { grid: grid, fields: fieldsView(M), view: view };
}

// ------------------------------------------------------------------
// Behaviour
// ------------------------------------------------------------------

var AGGS = ["sum", "count", "avg", "min", "max", "product"];

function st(el) { return $.data(el, "ah-pg"); }

function parse(el, attr, dflt) {
  try { return JSON.parse(el.getAttribute(attr)); } catch (e) { return dflt; }
}

function model(s) {
  var c = s.config;
  return {
    id: s.el.id, fields: c.fields,
    fieldNames: c.fields.map(function (f) { return f.name; }),
    layout: s.layout, view: s.view,
    row_subtotals: c.row_subtotals, col_subtotals: c.col_subtotals,
    grand_totals: c.grand_totals, values_on_rows: c.values_on_rows,
    format: c.format, labels: c.labels
  };
}

function $content(s) { return $(document.getElementById(s.el.id + "-content")); }

// ---- layout of the four quadrants ------------------------------

// Give the columns of the header and body tables the same widths (the
// wider of the two natural ones, or the width the user dragged), the
// row header and body rows the same heights, and leave room for the
// body's scroll bars in the header panes.
function syncLayout(s) {
  var c = document.getElementById(s.el.id + "-content");
  if (!c) { return; }
  var colT = c.querySelector(".ah-pg-col-table"), bodyT = c.querySelector(".ah-pg-body-table");
  var body = c.querySelector(".ah-pg-body");
  if (!colT || !bodyT) { return; }
  var hcols = colT.querySelectorAll("col"), bcols = bodyT.querySelectorAll("col");
  [colT, bodyT].forEach(function (t) {
    t.style.width = ""; t.style.tableLayout = ""; t.style.minWidth = "0";
  });
  for (var i = 0; i < hcols.length; i++) { hcols[i].style.width = ""; bcols[i].style.width = ""; }
  var n = hcols.length, w = new Array(n).fill(0), keys = new Array(n).fill("");
  $(colT).find("th[data-ci]").each(function () {
    var ci = +this.getAttribute("data-ci");
    w[ci] = Math.max(w[ci], this.getBoundingClientRect().width);
    keys[ci] = this.getAttribute("data-sort");
  });
  var first = bodyT.rows[0];
  if (first) {
    for (var j = 0; j < first.cells.length && j < n; j++) {
      w[j] = Math.max(w[j], first.cells[j].getBoundingClientRect().width);
    }
  }
  for (var k = 0; k < n; k++) {
    if (s.widths[keys[k]]) { w[k] = s.widths[keys[k]]; }
    w[k] = Math.ceil(w[k]);
  }
  var sum = w.reduce(function (a, b) { return a + b; }, 0);
  var avail = body.clientWidth;
  if (sum < avail && n) {
    var extra = Math.floor((avail - sum) / n);
    w = w.map(function (x, i2) { return x + extra + (i2 === n - 1 ? (avail - sum) - extra * n : 0); });
    sum = avail;
  }
  for (var m = 0; m < n; m++) { hcols[m].style.width = w[m] + "px"; bcols[m].style.width = w[m] + "px"; }
  [colT, bodyT].forEach(function (t) {
    t.style.tableLayout = "fixed"; t.style.width = sum + "px"; t.style.minWidth = "";
  });
  var rtrs = c.querySelectorAll(".ah-pg-row-table > tbody > tr");
  var btrs = bodyT.tBodies[0] ? bodyT.tBodies[0].rows : [];
  for (var r = 0; r < rtrs.length && r < btrs.length; r++) {
    rtrs[r].style.height = ""; btrs[r].style.height = "";
  }
  for (var q = 0; q < rtrs.length && q < btrs.length; q++) {
    var h1 = rtrs[q].getBoundingClientRect().height, h2 = btrs[q].getBoundingClientRect().height;
    if (Math.abs(h1 - h2) > 0.5) {
      var h = Math.max(h1, h2) + "px";
      rtrs[q].style.height = h; btrs[q].style.height = h;
    }
  }
  var rowT = c.querySelector(".ah-pg-row-table");
  if (rowT) { rowT.style.marginBottom = (body.offsetHeight - body.clientHeight) + "px"; }
  colT.style.marginRight = (body.offsetWidth - body.clientWidth) + "px";
  syncScroll(s);
}

function syncScroll(s) {
  var c = document.getElementById(s.el.id + "-content");
  var body = c && c.querySelector(".ah-pg-body");
  if (!body) { return; }
  c.querySelector(".ah-pg-col-headers").scrollLeft = body.scrollLeft;
  c.querySelector(".ah-pg-row-headers").scrollTop = body.scrollTop;
}

// ---- rendering and view changes --------------------------------

function writeState(s) {
  var layout = JSON.stringify(s.layout);
  s.el.setAttribute("data-ah-value", layout);
  s.el.setAttribute("data-view", JSON.stringify(s.view));
  $(s.el).children("input[type=hidden]").val(layout);
}

function render(s) {
  var c = document.getElementById(s.el.id + "-content");
  var body = c.querySelector(".ah-pg-body");
  var sl = body ? body.scrollLeft : 0, stp = body ? body.scrollTop : 0;
  var focused = document.activeElement && s.el.contains(document.activeElement)
    ? document.activeElement.id : null;
  var out = views(model(s), s.rows);
  s.view = out.view;
  c.innerHTML = AH.tpl.pivotgrid_grid(out.grid);
  var f = document.getElementById(s.el.id + "-fields");
  if (f) { f.innerHTML = AH.tpl.pivotgrid_fields(out.fields); }
  afterRender(s, sl, stp, focused);
}

function afterRender(s, sl, stp, focused) {
  s.sel = []; s.focus = null; s.anchor = null;
  $content(s).removeAttr("aria-activedescendant");
  syncLayout(s);
  var body = document.querySelector("#" + CSS.escape(s.el.id + "-content") + " .ah-pg-body");
  if (body && sl !== undefined) { body.scrollLeft = sl; body.scrollTop = stp; syncScroll(s); }
  if (focused) {
    var again = document.getElementById(focused);
    if (again) { again.focus(); }
  }
}

// A view change: re-render here (local) or ask the server (remote);
// 'ah:view' fires in both cases, `change' too when the layout changed.
function update(s, layoutChanged) {
  if (s.remote) {
    writeState(s);
    $(s.el).addClass("ah-pg-loading").attr("aria-busy", "true");
  } else {
    render(s);
    writeState(s);
  }
  if (layoutChanged) { $(s.el).trigger("change"); }
  $(s.el).trigger("ah:view", [{ layout: s.layout, view: s.view }]);
}

function setView(s, patch) {
  s.view = $.extend({}, s.view, patch);
  update(s, false);
}

function hasPath(list, p) {
  var k = key(p);
  return Array.isArray(list) && list.some(function (q) { return key(q) === k; });
}

function toggle(s, axis, p, open) {
  var prop = axis === "row" ? "expanded_rows" : "expanded_cols";
  var list = s.view[prop] === "all" ? allPaths(s, axis) : (s.view[prop] || []).slice();
  var has = hasPath(list, p);
  if (open === undefined) { open = !has; }
  if (open === has) { return; }
  list = open ? list.concat([p]) : list.filter(function (q) { return key(q) !== key(p); });
  var patch = {};
  patch[prop] = list;
  setView(s, patch);
}

// every expandable path (local: from the data; remote: the view asks
// the server for "all")
function allPaths(s, axis) {
  if (s.remote) {
    var sel = axis === "row" ? ".ah-pg-row-headers td[aria-expanded]" : ".ah-pg-col-headers th[aria-expanded]";
    return $content(s).find(sel).filter("[aria-expanded=true]").map(function () {
      return [JSON.parse($(this).closest("[data-path]").attr("data-path"))];
    }).get();
  }
  var E = aggregate(model(s), s.rows, effValues(model(s)));
  return Array.from(expandedSet("all", axis === "row" ? E.rkids : E.ckids))
    .map(function (k) { return JSON.parse(k); });
}

function expandAll(s, open) {
  setView(s, open ? { expanded_rows: s.remote ? "all" : allPaths(s, "row"),
                      expanded_cols: s.remote ? "all" : allPaths(s, "col") }
                  : { expanded_rows: [], expanded_cols: [] });
}

function sortByValue(s, col, vi, dir) {
  setView(s, { row_sort: dir ? { by: "value", dir: dir, col: col, vi: vi } : null });
}

// clicking a column's bottom header: ascending, descending, unsorted
function cycleSort(s, col, vi) {
  var cur = s.view.row_sort;
  var same = cur && cur.by === "value" && key(cur.col) === key(col) && cur.vi === vi;
  sortByValue(s, col, vi, !same ? "asc" : cur.dir === "asc" ? "desc" : null);
}

// ---- layout (the field list) -----------------------------------

function defaultAgg(s, field) {
  var f = null;
  s.config.fields.forEach(function (x) { if (x.name === field) { f = x; } });
  return f ? f.agg : "sum";
}

function setLayout(s, layout, fire) {
  var old = s.layout;
  var norm = {
    rows: (layout.rows || []).slice(),
    columns: (layout.columns || []).slice(),
    values: (layout.values || []).map(function (v) {
      if (typeof v === "string") { v = { field: v }; }
      var field = v.field === undefined ? null : v.field;
      return { label: v.label === undefined ? null : v.label,
               agg: v.agg || (field === null ? "count" : defaultAgg(s, field)),
               field: field };
    })
  };
  var patch = {};
  if (key(norm.rows) !== key(old.rows)) {
    patch.expanded_rows = [];
    if (s.view.row_sort && s.view.row_sort.by === "value") { patch.row_sort = null; }
  }
  if (key(norm.columns) !== key(old.columns)) {
    patch.expanded_cols = [];
    if (s.view.row_sort && s.view.row_sort.by === "value") { patch.row_sort = null; }
  }
  if (s.view.row_sort && s.view.row_sort.by === "value" &&
      s.view.row_sort.vi >= Math.max(1, norm.values.length)) {
    patch.row_sort = null;
  }
  s.layout = norm;
  s.view = $.extend({}, s.view, patch);
  if (fire === false) {
    if (!s.remote) { render(s); }
    writeState(s);
  } else {
    update(s, true);
  }
}

// Move the chip `index' of zone `from' to position `to' of zone
// `into' (to = -1: at the end).
function moveField(s, from, index, into, to) {
  var L = { rows: s.layout.rows.slice(), columns: s.layout.columns.slice(),
            values: s.layout.values.slice() };
  var field, spec = null;
  if (from === "fields") {
    field = $("#" + CSS.escape(s.el.id + "-chip-fields-" + index)).attr("data-field");
  } else if (from === "values") {
    spec = L.values.splice(index, 1)[0];
    field = spec.field;
  } else {
    field = L[from].splice(index, 1)[0];
  }
  if (into === "fields") { return setLayout(s, L); }
  function ins(list, item) {
    var at = to < 0 || to > list.length ? list.length : to;
    if (from === into && index < at && to >= 0) { at--; }
    list.splice(at, 0, item);
  }
  if (into === "values") {
    if (field === null && !spec) { return; }
    ins(L.values, spec && from === "values" ? spec
      : { field: field, agg: defaultAgg(s, field), label: null });
  } else {
    if (field === null) { return; }
    ["rows", "columns"].forEach(function (z) {
      var i = L[z].indexOf(field);
      if (i >= 0) { L[z].splice(i, 1); }
    });
    ins(L[into], field);
  }
  setLayout(s, L);
}

// ---- selection, focus, cell events -----------------------------

function bodyRows(s) {
  var t = $content(s).find(".ah-pg-body-table")[0];
  return t && t.tBodies[0] ? t.tBodies[0].rows : [];
}

function cellAt(s, r, c) {
  var rows = bodyRows(s);
  return rows[r] ? rows[r].cells[c] || null : null;
}

// The members, measure and value of a body cell, read from the DOM.
function cellInfo(s, td) {
  var tr = td.parentNode, r = tr.sectionRowIndex, c = td.cellIndex;
  var rh = $content(s).find(".ah-pg-row-table > tbody > tr")[r];
  var th = $content(s).find('.ah-pg-col-table th[data-ci="' + c + '"]')[0];
  var row = rh ? JSON.parse(rh.getAttribute("data-path")) : [];
  var sort = th ? JSON.parse(th.getAttribute("data-sort")) : [[], 0];
  var vi = rh && rh.hasAttribute("data-vi") ? +rh.getAttribute("data-vi") : sort[1];
  var vs = s.layout.values.length ? s.layout.values : [{ field: null, agg: "count" }];
  var filter = {};
  row.forEach(function (k, i) { filter[s.layout.rows[i]] = k; });
  sort[0].forEach(function (k, i) { filter[s.layout.columns[i]] = k; });
  var v = td.getAttribute("data-v");
  return { row: row, col: sort[0], vi: vi, r: r, c: c, filter: filter,
           field: vs[vi] ? vs[vi].field : null, agg: vs[vi] ? vs[vi].agg : "count",
           value: v === null ? null : +v, text: td.textContent };
}

function paint(s) {
  var $c = $content(s);
  $c.find(".ah-pg-cell-selected").removeClass("ah-pg-cell-selected").removeAttr("aria-selected");
  $c.find(".ah-pg-cell-focused").removeClass("ah-pg-cell-focused").removeAttr("aria-label");
  s.sel.forEach(function (rc) {
    var td = cellAt(s, rc[0], rc[1]);
    if (td) { $(td).addClass("ah-pg-cell-selected").attr("aria-selected", "true"); }
  });
  if (s.focus) {
    var td = cellAt(s, s.focus[0], s.focus[1]);
    if (td) {
      var info = cellInfo(s, td), M = model(s);
      var rl = info.row.length ? info.row.map(function (k) { return keyLabel(M, k); }).join(" / ")
        : M.labels.grand_total;
      var cl = info.col.length ? info.col.map(function (k) { return keyLabel(M, k); }).join(" / ")
        : M.labels.grand_total;
      var vs = effValues(M);
      $(td).addClass("ah-pg-cell-focused")
        .attr("aria-label", rl + ", " + cl + ", " + valueLabel(M, vs[Math.min(info.vi, vs.length - 1)]) +
              ": " + (info.text || "-"));
      $c.attr("aria-activedescendant", td.id);
      scrollIntoView(s, td);
    }
  } else {
    $c.removeAttr("aria-activedescendant");
  }
}

function scrollIntoView(s, td) {
  var body = $content(s).find(".ah-pg-body")[0];
  var top = td.parentNode.offsetTop, h = td.parentNode.offsetHeight;
  if (top < body.scrollTop) { body.scrollTop = top; }
  if (top + h > body.scrollTop + body.clientHeight) { body.scrollTop = top + h - body.clientHeight; }
  var left = td.offsetLeft, w = td.offsetWidth;
  if (left < body.scrollLeft) { body.scrollLeft = left; }
  if (left + w > body.scrollLeft + body.clientWidth) { body.scrollLeft = left + w - body.clientWidth; }
}

function rect(a, b) {
  var out = [];
  for (var r = Math.min(a[0], b[0]); r <= Math.max(a[0], b[0]); r++) {
    for (var c = Math.min(a[1], b[1]); c <= Math.max(a[1], b[1]); c++) { out.push([r, c]); }
  }
  return out;
}

function select(s, rc, mode) {
  if (mode === "range" && s.anchor) {
    s.sel = rect(s.anchor, rc);
  } else if (mode === "toggle") {
    var i = s.sel.findIndex(function (x) { return x[0] === rc[0] && x[1] === rc[1]; });
    if (i >= 0) { s.sel.splice(i, 1); } else { s.sel.push(rc); }
    s.anchor = rc;
  } else {
    s.sel = [rc];
    s.anchor = rc;
  }
  s.focus = rc;
  paint(s);
  $(s.el).trigger("ah:selection", [getSelection(s)]);
}

function getSelection(s) {
  return s.sel.map(function (rc) {
    var td = cellAt(s, rc[0], rc[1]);
    if (!td) { return null; }
    var i = cellInfo(s, td);
    return { row: i.row, col: i.col, vi: i.vi, value: i.value };
  }).filter(Boolean);
}

// 'ah:cell-click': the cell's JSON goes into data-cell for the action's
// Event.data while the event runs.
function fireCell(s, td) {
  var i = cellInfo(s, td);
  var detail = { row: i.row, col: i.col, filter: i.filter, field: i.field, agg: i.agg,
                 value: i.value, text: i.text };
  s.el.setAttribute("data-cell", JSON.stringify(detail));
  $(s.el).trigger("ah:cell-click", [detail]);
  s.el.removeAttribute("data-cell");
}

// ---- context menu ----------------------------------------------

function openMenu(s, ctx, target, at) {
  var $m = $(document.getElementById(s.el.id + "-menu"));
  s.menu = { ctx: ctx, target: target, invoker: document.activeElement };
  $m.children().each(function () {
    var ctxs = (this.getAttribute("data-ctx") || "").split(" ");
    var show = ctxs.indexOf(ctx) >= 0;
    var act = this.getAttribute("data-action");
    if (show && ctx === "values" && act === "agg") {
      var spec = s.layout.values[+target.getAttribute("data-index")];
      this.setAttribute("aria-checked", spec && spec.agg === this.getAttribute("data-agg") ? "true" : "false");
      if (spec && spec.field === null && this.getAttribute("data-agg") !== "count") { show = false; }
    }
    if (show && (act === "move-left" || act === "move-right")) {
      var idx = +target.getAttribute("data-index");
      var len = $(target).parent().children(".ah-pg-chip").length;
      show = act === "move-left" ? idx > 0 : idx < len - 1;
    }
    if (show && ctx === "col" && /^sort-value/.test(act)) { show = target.hasAttribute("data-sort"); }
    this.hidden = !show;
  });
  $m.addClass("ah-pg-context-menu-open");
  if (at) {
    var w = $m.outerWidth(), h = $m.outerHeight();
    var left = at.x + w > window.innerWidth ? Math.max(0, at.x - w) : at.x;
    var top = at.y + h > window.innerHeight ? Math.max(0, at.y - h) : at.y;
    $m.css({ position: "fixed", left: left + "px", top: top + "px" });
  } else {
    s.float = AH.float($m[0], target, { placement: "bottom", align: "start", offset: 2 });
  }
  var first = $m.children(":not([hidden])")[0];
  if (first) { first.focus(); }
}

function closeMenu(s, refocus) {
  if (!s.menu) { return; }
  if (s.float) { s.float.stop(); s.float = null; }
  $(document.getElementById(s.el.id + "-menu")).removeClass("ah-pg-context-menu-open");
  var inv = s.menu.invoker;
  s.menu = null;
  if (refocus && inv && document.contains(inv)) { inv.focus(); }
}

function menuAction(s, item) {
  var m = s.menu, act = item.getAttribute("data-action"), t = m.target;
  closeMenu(s, true);
  var info = m.ctx === "cell" ? cellInfo(s, t) : null;
  var sort = m.ctx === "col" && t.hasAttribute("data-sort") ? JSON.parse(t.getAttribute("data-sort"))
    : info ? [info.col, info.vi] : null;
  switch (act) {
    case "sort-asc": case "sort-desc":
      return setView(s, { row_sort: { by: "key", dir: act === "sort-asc" ? "asc" : "desc" } });
    case "sort-value-asc": case "sort-value-desc":
      if (sort) { sortByValue(s, sort[0], sort[1], act === "sort-value-asc" ? "asc" : "desc"); }
      return;
    case "sort-cols-asc": return setView(s, { col_sort: "asc" });
    case "sort-cols-desc": return setView(s, { col_sort: "desc" });
    case "sort-clear": return setView(s, { row_sort: null, col_sort: "asc" });
    case "expand-all": return expandAll(s, true);
    case "collapse-all": return expandAll(s, false);
    case "export-xlsx": return exportXlsx(s, {});
    case "export-csv": return exportCsv(s, {});
    case "agg": {
      var L = $.extend(true, {}, s.layout), i = +t.getAttribute("data-index");
      L.values[i].agg = item.getAttribute("data-agg");
      return setLayout(s, L);
    }
    default: {
      var zone = t.getAttribute("data-zone"), idx = +t.getAttribute("data-index");
      if (act === "remove") { return moveField(s, zone, idx, "fields", -1); }
      if (act === "move-left") { return moveField(s, zone, idx, zone, idx - 1); }
      if (act === "move-right") { return moveField(s, zone, idx, zone, idx + 2); }
      var into = act.replace("move-", "");
      return moveField(s, zone, idx, into, -1);
    }
  }
}

// ---- export (what the table shows, in both modes) ---------------

function exportData(s) {
  var $c = $content(s), M = model(s);
  var nr = Math.max(1, s.layout.rows.length);
  var vor = !!s.config.values_on_rows && effValues(M).length > 1;
  var off = nr + (vor ? 1 : 0);
  var aoa = [], merges = [], occupied = {};
  var htrs = $c.find(".ah-pg-col-table > thead > tr");
  var H = htrs.length;
  htrs.each(function (r) {
    aoa[r] = aoa[r] || [];
    var c = 0;
    $(this).children("th").each(function () {
      while (occupied[r + "," + c]) { c++; }
      var cs = +(this.getAttribute("colspan") || 1), rs = +(this.getAttribute("rowspan") || 1);
      for (var i = 0; i < rs; i++) {
        for (var j = 0; j < cs; j++) { occupied[(r + i) + "," + (c + j)] = true; }
      }
      aoa[r][off + c] = $(this).find(".ah-pg-col-label").text();
      if (cs > 1 || rs > 1) {
        merges.push({ s: { r: r, c: off + c }, e: { r: r + rs - 1, c: off + c + cs - 1 } });
      }
      c += cs;
    });
  });
  for (var r0 = 0; r0 < H; r0++) {
    for (var k = 0; k < off; k++) { aoa[r0][k] = ""; }
  }
  if (H) {
    s.layout.rows.forEach(function (f, i) { aoa[H - 1][i] = fieldLabel(M, f); });
    if (vor) { aoa[H - 1][nr] = M.labels.values; }
  }
  var rtrs = $c.find(".ah-pg-row-table > tbody > tr");
  $(bodyRows(s)).each(function (i) {
    var rh = rtrs[i], line = [];
    var path = JSON.parse(rh.getAttribute("data-path"));
    for (var j = 0; j < nr; j++) { line.push(""); }
    if (!path.length) { line[0] = M.labels.grand_total; }
    path.forEach(function (key0, j2) { line[j2] = keyLabel(M, key0); });
    if (vor) { line.push($(rh).children(".ah-pg-value-label-cell").text()); }
    $(this.cells).each(function () {
      var v = this.getAttribute("data-v");
      line.push(v === null || v === "" ? this.textContent : +v);
    });
    aoa.push(line);
  });
  var width = aoa.reduce(function (m, l) { return Math.max(m, l.length); }, 0);
  for (var x = 0; x < aoa.length; x++) {
    for (var y = 0; y < width; y++) { if (aoa[x][y] === undefined) { aoa[x][y] = ""; } }
  }
  return { aoa: aoa, merges: merges };
}

function download(blob, filename) {
  var url = URL.createObjectURL(blob), a = document.createElement("a");
  a.href = url; a.download = filename;
  document.body.appendChild(a); a.click(); document.body.removeChild(a);
  setTimeout(function () { URL.revokeObjectURL(url); }, 0);
}

function exportXlsx(s, opts) {
  opts = opts || {};
  var d = exportData(s);
  return AH.vendor("xlsx").then(function (XLSX) {
    var ws = XLSX.utils.aoa_to_sheet(d.aoa);
    if (d.merges.length) { ws["!merges"] = d.merges; }
    var wb = XLSX.utils.book_new();
    XLSX.utils.book_append_sheet(wb, ws, opts.sheetName || "Pivot");
    var buf = XLSX.write(wb, { bookType: "xlsx", type: "array" });
    var blob = new Blob([buf], { type: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" });
    if (opts.download !== false) { download(blob, opts.filename || "pivot.xlsx"); }
    return blob;
  });
}

function exportCsv(s, opts) {
  opts = opts || {};
  var sep = opts.separator || ",";
  var text = exportData(s).aoa.map(function (line) {
    return line.map(function (v) {
      var t = v === null || v === undefined ? "" : String(v);
      return /[",\r\n]/.test(t) || t.indexOf(sep) >= 0 ? '"' + t.replace(/"/g, '""') + '"' : t;
    }).join(sep);
  }).join("\r\n");
  if (opts.download !== false) {
    download(new Blob(["﻿" + text], { type: "text/csv;charset=utf-8" }), opts.filename || "pivot.csv");
  }
  return text;
}

// ---- column resizing -------------------------------------------

function startResize(s, e, th) {
  e.preventDefault(); e.stopPropagation();
  var $line = $(s.el).children(".ah-pg-resize-line");
  var root = s.el.getBoundingClientRect(), cr = th.getBoundingClientRect();
  var x0 = e.clientX, w0 = cr.width, width = w0;
  $line.css({ display: "block", left: (cr.right - root.left) + "px", top: 0, height: root.height + "px" });
  var ns = ".ah-pg-rsz" + AH.NS;
  $(document).on("mousemove" + ns, function (me) {
    width = Math.max(40, w0 + me.clientX - x0);
    $line.css("left", (cr.left - root.left + width) + "px");
  }).on("mouseup" + ns, function () {
    $(document).off(ns);
    $line.css("display", "none");
    s.widths[th.getAttribute("data-sort")] = Math.round(width);
    syncLayout(s);
  });
}

// ---- keyboard ----------------------------------------------------

function onKey(s, e) {
  var rows = bodyRows(s);
  if (!rows.length) { return; }
  var maxR = rows.length - 1, maxC = rows[0].cells.length - 1;
  var f = s.focus || [0, 0], r = f[0], c = f[1], k = e.key;
  var nav = { ArrowDown: [r + 1, c], ArrowUp: [r - 1, c], ArrowRight: [r, c + 1], ArrowLeft: [r, c - 1],
              Home: [e.ctrlKey ? 0 : r, 0], End: [e.ctrlKey ? maxR : r, maxC],
              PageDown: [r + 10, c], PageUp: [r - 10, c] };
  if (e.altKey && (k === "ArrowRight" || k === "ArrowLeft" || k === "ArrowDown" || k === "ArrowUp")) {
    e.preventDefault();
    if (!s.focus) { return; }
    var info = cellInfo(s, cellAt(s, r, c));
    if (k === "ArrowRight" || k === "ArrowLeft") {
      if (info.row.length) { toggle(s, "row", info.row, k === "ArrowRight"); }
    } else if (info.col.length) {
      toggle(s, "col", info.col, k === "ArrowDown");
    }
    return;
  }
  if (nav[k]) {
    e.preventDefault();
    var to = [Math.max(0, Math.min(maxR, nav[k][0])), Math.max(0, Math.min(maxC, nav[k][1]))];
    select(s, to, e.shiftKey ? "range" : null);
    if (e.shiftKey) { s.focus = to; paint(s); }
    return;
  }
  switch (k) {
    case "Tab":
      if (!s.focus) { return; }
      var next = e.shiftKey ? (c > 0 ? [r, c - 1] : r > 0 ? [r - 1, maxC] : null)
        : (c < maxC ? [r, c + 1] : r < maxR ? [r + 1, 0] : null);
      if (next) { e.preventDefault(); select(s, next, null); }
      return;
    case "Enter": case " ":
      e.preventDefault();
      if (!s.focus) { select(s, [0, 0], null); return; }
      fireCell(s, cellAt(s, r, c));
      return;
    case "+": case "-":
      if (!s.focus) { return; }
      e.preventDefault();
      var i2 = cellInfo(s, cellAt(s, r, c));
      if (i2.row.length) { toggle(s, "row", i2.row, k === "+"); }
      return;
    case "Escape":
      if (s.sel.length) { e.preventDefault(); s.sel = []; s.focus = null; paint(s);
                          $(s.el).trigger("ah:selection", [[]]); }
      return;
    case "ContextMenu": case "F10":
      if (k === "F10" && !e.shiftKey) { return; }
      e.preventDefault();
      if (!s.focus) { select(s, [0, 0], null); }
      openMenu(s, "cell", cellAt(s, s.focus[0], s.focus[1]), null);
      return;
    default:
      if ((e.ctrlKey || e.metaKey) && k === "a") {
        e.preventDefault();
        s.anchor = [0, 0];
        s.sel = rect([0, 0], [maxR, maxC]);
        s.focus = s.focus || [0, 0];
        paint(s);
        $(s.el).trigger("ah:selection", [getSelection(s)]);
      }
  }
}

function onMenuKey(s, e) {
  var items = $(document.getElementById(s.el.id + "-menu")).children(":not([hidden])").get();
  var i = items.indexOf(document.activeElement);
  switch (e.key) {
    case "ArrowDown": e.preventDefault(); items[(i + 1) % items.length].focus(); break;
    case "ArrowUp": e.preventDefault(); items[(i - 1 + items.length) % items.length].focus(); break;
    case "Home": e.preventDefault(); items[0].focus(); break;
    case "End": e.preventDefault(); items[items.length - 1].focus(); break;
    case "Enter": case " ":
      e.preventDefault();
      if (i >= 0) { menuAction(s, items[i]); }
      break;
    case "Escape": e.preventDefault(); e.stopPropagation(); closeMenu(s, true); break;
    case "Tab": closeMenu(s, false); break;
    default: break;
  }
}

// ---- field list drag and drop ------------------------------------

function dropIndex(zone, x, y) {
  var chips = $(zone).children(".ah-pg-chip").get();
  for (var i = 0; i < chips.length; i++) {
    var r = chips[i].getBoundingClientRect();
    if (y < r.top) { return i; }
    if (y <= r.bottom && x < r.left + r.width / 2) { return i; }
  }
  return chips.length;
}

// ------------------------------------------------------------------

AH.define("pivotgrid", {
  init: function (el, $el) {
    var island = $el.children("script.ah-pg-data")[0];
    var s = {
      el: el, remote: el.hasAttribute("data-ah-remote"),
      config: parse(el, "data-config", {}),
      layout: parse(el, "data-ah-value", { rows: [], columns: [], values: [] }),
      view: parse(el, "data-view", {}),
      rows: [], sel: [], focus: null, anchor: null, widths: {}, menu: null, float: null
    };
    if (island) {
      try {
        s.rows = JSON.parse(island.textContent).rows;
      } catch (err) {
        s.rows = [];
      }
    }
    $.data(el, "ah-pg", s);
    var $c = $content(s);

    $c.on("click" + AH.NS, ".ah-pg-toggle", function (e) {
      e.stopPropagation();
      var $t = $(this);
      if ($t.hasClass("ah-pg-toggle-leaf")) { return; }
      var holder = $t.closest("[data-path]")[0];
      toggle(s, $t.closest(".ah-pg-row-headers").length ? "row" : "col",
             JSON.parse(holder.getAttribute("data-path")));
    });
    $c.on("click" + AH.NS, "th[data-sort]", function (e) {
      if ($(e.target).closest(".ah-pg-resize-handle, .ah-pg-toggle").length) { return; }
      var sort = JSON.parse(this.getAttribute("data-sort"));
      cycleSort(s, sort[0], sort[1]);
    });
    $c.on("click" + AH.NS, "td.ah-pg-cell", function (e) {
      var rc = [this.parentNode.sectionRowIndex, this.cellIndex];
      select(s, rc, e.shiftKey ? "range" : (e.ctrlKey || e.metaKey) ? "toggle" : null);
      fireCell(s, this);
    });
    $c.on("mousedown" + AH.NS, ".ah-pg-resize-handle", function (e) {
      startResize(s, e, $(this).closest("th")[0]);
    });
    $c.on("contextmenu" + AH.NS, "th.ah-pg-col-th, .ah-pg-row-header, td.ah-pg-cell", function (e) {
      e.preventDefault();
      var ctx = $(this).is("td.ah-pg-cell") ? "cell" : $(this).is("th") ? "col" : "row";
      if (ctx === "cell") { select(s, [this.parentNode.sectionRowIndex, this.cellIndex], null); }
      openMenu(s, ctx, this, { x: e.clientX, y: e.clientY });
    });
    $c.on("keydown" + AH.NS, function (e) {
      if (e.target === $c[0]) { onKey(s, e); }
    });
    $c.on("focus" + AH.NS, function () {
      if (!s.focus && bodyRows(s).length) { s.focus = [0, 0]; paint(s); }
    });
    $c.on("wheel" + AH.NS, ".ah-pg-row-headers, .ah-pg-col-headers", function (e) {
      var body = $c.find(".ah-pg-body")[0], oe = e.originalEvent;
      if (!body) { return; }
      var t0 = body.scrollTop, l0 = body.scrollLeft;
      body.scrollTop += oe.deltaY;
      body.scrollLeft += oe.deltaX;
      if (body.scrollTop !== t0 || body.scrollLeft !== l0) { e.preventDefault(); }
    });
    s.onScroll = function (e) {
      if (e.target.classList && e.target.classList.contains("ah-pg-body")) { syncScroll(s); }
    };
    $c[0].addEventListener("scroll", s.onScroll, true);

    // context menu
    var $m = $(document.getElementById(el.id + "-menu"));
    $m.on("click" + AH.NS, ".ah-pg-context-menu-item", function () { menuAction(s, this); });
    $m.on("keydown" + AH.NS, function (e) { onMenuKey(s, e); });
    $(document).on("mousedown.ah-pg-" + el.id + AH.NS, function (e) {
      if (s.menu && !$m[0].contains(e.target)) { closeMenu(s, false); }
    });

    // field list
    var $f = $(document.getElementById(el.id + "-fields"));
    $f.on("click" + AH.NS, ".ah-pg-chip", function () { openMenu(s, this.getAttribute("data-zone"), this, null); });
    $f.on("keydown" + AH.NS, ".ah-pg-chip", function (e) {
      var zone = this.getAttribute("data-zone"), idx = +this.getAttribute("data-index");
      if (e.key === "Enter" || e.key === " " || e.key === "ContextMenu" || (e.key === "F10" && e.shiftKey)) {
        e.preventDefault();
        openMenu(s, zone, this, null);
      } else if ((e.key === "Delete" || e.key === "Backspace") && zone !== "fields") {
        e.preventDefault();
        moveField(s, zone, idx, "fields", -1);
      } else if (e.key === "ArrowRight" || e.key === "ArrowLeft") {
        e.preventDefault();
        var chips = $f.find(".ah-pg-chip").get(), i = chips.indexOf(this);
        var next = chips[i + (e.key === "ArrowRight" ? 1 : -1)];
        if (next) { next.focus(); }
      }
    });
    $f.on("dragstart" + AH.NS, ".ah-pg-chip", function (e) {
      s.drag = { zone: this.getAttribute("data-zone"), index: +this.getAttribute("data-index") };
      var dt = e.originalEvent.dataTransfer;
      if (dt) { dt.effectAllowed = "move"; dt.setData("text/plain", this.getAttribute("data-field")); }
      $(this).addClass("ah-pg-chip-dragging");
    });
    $f.on("dragend" + AH.NS, ".ah-pg-chip", function () {
      s.drag = null;
      $f.find(".ah-pg-chip-dragging").removeClass("ah-pg-chip-dragging");
      $f.find(".ah-pg-zone-over").removeClass("ah-pg-zone-over");
    });
    $f.on("dragover" + AH.NS, ".ah-pg-zone", function (e) {
      if (!s.drag) { return; }
      e.preventDefault();
      $f.find(".ah-pg-zone-over").not(this).removeClass("ah-pg-zone-over");
      $(this).addClass("ah-pg-zone-over");
    });
    $f.on("dragleave" + AH.NS, ".ah-pg-zone", function (e) {
      if (!this.contains(e.originalEvent.relatedTarget)) { $(this).removeClass("ah-pg-zone-over"); }
    });
    $f.on("drop" + AH.NS, ".ah-pg-zone", function (e) {
      if (!s.drag) { return; }
      e.preventDefault();
      var d = s.drag, oe = e.originalEvent;
      s.drag = null;
      $(this).removeClass("ah-pg-zone-over");
      var into = this.getAttribute("data-zone");
      if (into === "fields" && d.zone === "fields") { return; }
      moveField(s, d.zone, d.index, into, dropIndex(this, oe.clientX, oe.clientY));
    });

    if (window.ResizeObserver) {
      var lastW = -1;
      s.ro = new ResizeObserver(function (entries) {
        var w = Math.round(entries[0].contentRect.width);
        if (w !== lastW) { lastW = w; syncLayout(s); }
      });
      s.ro.observe($c[0]);
    }
    syncLayout(s);
  },
  destroy: function (el) {
    var s = st(el);
    if (!s) { return; }
    $(document).off("mousedown.ah-pg-" + el.id + AH.NS).off(".ah-pg-rsz" + AH.NS);
    var c = document.getElementById(el.id + "-content");
    if (c) { c.removeEventListener("scroll", s.onScroll, true); }
    if (s.ro) { s.ro.disconnect(); }
    if (s.float) { s.float.stop(); }
    $.removeData(el, "ah-pg");
  },
  methods: {
    expandAll: function (el) { expandAll(st(el), true); },
    collapseAll: function (el) { expandAll(st(el), false); },
    expandRow: function (el, $el, p) { toggle(st(el), "row", p, true); },
    collapseRow: function (el, $el, p) { toggle(st(el), "row", p, false); },
    expandColumn: function (el, $el, p) { toggle(st(el), "col", p, true); },
    collapseColumn: function (el, $el, p) { toggle(st(el), "col", p, false); },
    sortRows: function (el, $el, dir) {
      setView(st(el), { row_sort: dir ? { by: "key", dir: dir } : null });
    },
    sortByValue: function (el, $el, col, vi, dir) { sortByValue(st(el), col || [], vi || 0, dir || "asc"); },
    sortColumns: function (el, $el, dir) { setView(st(el), { col_sort: dir === "desc" ? "desc" : "asc" }); },
    setLayout: function (el, $el, layout) { setLayout(st(el), layout, false); },
    getLayout: function (el) { return st(el).layout; },
    getView: function (el) { return st(el).view; },
    setData: function (el, $el, rows) {
      var s = st(el);
      var names = s.config.fields.map(function (f) { return f.name; });
      s.rows = (rows || []).map(function (o) {
        return names.map(function (n) { return cell(o[n]); });
      });
      if (!s.remote) { render(s); }
    },
    getSelection: function (el) { return getSelection(st(el)); },
    clearSelection: function (el) { var s = st(el); s.sel = []; s.focus = null; paint(s); },
    exportXlsx: function (el, $el, opts) { return exportXlsx(st(el), opts); },
    exportCsv: function (el, $el, opts) { return exportCsv(st(el), opts); },
    exportData: function (el) { return exportData(st(el)); },
    refresh: function (el) { update(st(el), false); },
    // the HTML of both templates for the current state (local mode)
    viewHtml: function (el) {
      var s = st(el), out = views(model(s), s.rows);
      return { grid: AH.tpl.pivotgrid_grid(out.grid), fields: AH.tpl.pivotgrid_fields(out.fields) };
    },
    viewLoaded: function (el) {
      var s = st(el);
      s.view = parse(el, "data-view", s.view);
      $(el).removeClass("ah-pg-loading").removeAttr("aria-busy");
      afterRender(s, undefined, undefined, document.activeElement && el.contains(document.activeElement)
        ? document.activeElement.id : null);
    }
  }
});
