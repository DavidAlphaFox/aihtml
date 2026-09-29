/* Shared by the treegrid and datatable controllers (treegrid.js,
 * datatable.js): selection by row keys, sorting order, roving focus, the
 * header / body scroll and gutter sync and column resizing. Ported from
 * sigil (data/treegrid, data/datatable). Native DOM: every helper takes
 * elements (arrays of rows), never jQuery objects; the helpers that bind
 * listeners take the AH.Controller, so teardown removes them.
 */
import AH from "../core.js";
import "./_lib_values.js";

var seq = 0;

// A fresh id per table instance.
function nextId() { return ++seq; }

// The element children of el matching selector.
function kids(el, selector) {
  if (!el) { return []; }
  return Array.prototype.filter.call(el.children, function (c) { return c.matches(selector); });
}

function kid(el, selector) { return kids(el, selector)[0] || null; }

// Keys are joined by AH.lib.values (a comma in a key is escaped).
function keysOf(el) {
  return AH.lib.values.split(el.getAttribute("data-ah-value") || "");
}

function toKeys(v) {
  if (v === null || v === undefined) { return []; }
  return AH.lib.values.split(Array.isArray(v) ? v : String(v));
}

function writeValue(el, keys) {
  var v = AH.lib.values.join(keys);
  el.setAttribute("data-ah-value", v);
  kids(el, "input[type=hidden][data-ah-input]").forEach(function (i) { i.value = v; });
}

function sameKeys(a, b) {
  return a.length === b.length && a.every(function (k, i) { return k === b[i]; });
}

// The raw value of a cell: data-value when the server wrote one (numbers,
// rendered cells), its text otherwise.
function raw(td) {
  if (!td) { return ""; }
  var v = td.getAttribute("data-value");
  return v !== null ? v : td.textContent;
}

function isNum(s) { return /^\s*-?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?\s*$/.test(s); }

// Numbers before texts, texts in the locale's order.
function compare(a, b) {
  var na = isNum(a), nb = isNum(b);
  if (na && nb) { return parseFloat(a) - parseFloat(b); }
  if (na !== nb) { return na ? -1 : 1; }
  return a.localeCompare(b, undefined, { numeric: true, sensitivity: "base" });
}

function ord(tr) { return parseInt(tr.getAttribute("data-i"), 10) || 0; }

// Selection after a click on a row; null: the click does not select.
function clickSelect(mode, keys, key, e, anchor, order) {
  if (mode === "single") {
    if (keys.length === 1 && keys[0] === key) { return e.detail > 1 ? keys : []; }
    return [key];
  }
  if (mode === "multiple") {
    if (e.shiftKey && anchor !== null) {
      var a = order.indexOf(anchor), b = order.indexOf(key);
      if (a >= 0 && b >= 0) { return order.slice(Math.min(a, b), Math.max(a, b) + 1); }
    }
    if (e.ctrlKey || e.metaKey) { return toggleKey(keys, key); }
    return [key];
  }
  return null;
}

function toggleKey(keys, key) {
  return keys.indexOf(key) >= 0 ? keys.filter(function (k) { return k !== key; }) : keys.concat([key]);
}

// Enter / Space on a row.
function keySelect(mode, keys, key) {
  if (mode === "single") { return keys.length === 1 && keys[0] === key ? [] : [key]; }
  if (mode === "multiple" || mode === "checkbox") { return toggleKey(keys, key); }
  return null;
}

function markRows(rows, pre, keys, mode) {
  rows.forEach(function (tr) {
    var on = keys.indexOf(tr.getAttribute("data-key")) >= 0;
    tr.classList.toggle(pre + "-row-selected", on);
    if (mode !== "none") { tr.setAttribute("aria-selected", String(on)); }
    kids(tr, "td").forEach(function (td) {
      kids(td, "." + pre + "-row-checkbox").forEach(function (cb) { cb.checked = on; });
    });
  });
}

function headerCheck(box, keys, visibleKeys) {
  if (!box) { return; }
  var n = visibleKeys.filter(function (k) { return keys.indexOf(k) >= 0; }).length;
  box.checked = n > 0 && n === visibleKeys.length;
  box.indeterminate = n > 0 && n < visibleKeys.length;
}

// Roving tabindex: one row of the table is in the tab order.
function focusRow(rows, tr, move) {
  if (!tr) { return; }
  rows.forEach(function (r) { r.setAttribute("tabindex", "-1"); });
  tr.setAttribute("tabindex", "0");
  if (move) {
    tr.focus();
    if (tr.scrollIntoView) { tr.scrollIntoView({ block: "nearest" }); }
  }
}

// The header / body part of the table (null when missing).
function part(el, pre, name) {
  return kid(kid(el, "." + pre + "-content"), "." + pre + "-" + name);
}

// The table of a part (header or body).
function partTable(el, pre, name) { return kid(part(el, pre, name), "table"); }

// Body scroll moves the header along. Scroll does not bubble: the
// listener captures on the root, so a body the server morphed in keeps
// working.
function bindScroll(ctl, el, pre) {
  ctl.listen(el, "scroll", function (e) {
    var body = part(el, pre, "body");
    if (e.target !== body) { return; }
    var header = part(el, pre, "header");
    if (header) { header.scrollLeft = body.scrollLeft; }
  }, { capture: true });
}

// The header leaves room for the body's vertical scrollbar, so the
// columns of both tables line up (sigil reserves it with
// scrollbar-gutter, which leaves an empty strip when nothing scrolls).
function syncGutter(el, pre) {
  var body = part(el, pre, "body"), header = part(el, pre, "header");
  if (!body || !header) { return; }
  var w = body.offsetWidth - body.clientWidth;
  header.style.paddingRight = w > 0 ? w + "px" : "";
}

function watchGutter(el, pre, st) {
  syncGutter(el, pre);
  var body = part(el, pre, "body");
  if (body && window.ResizeObserver) {
    st.ro = new ResizeObserver(function () { syncGutter(el, pre); });
    st.ro.observe(body);
    var table = kid(body, "table");
    if (table) { st.ro.observe(table); }
  }
}

// Dragging a header edge resizes the column in both tables; fires
// ah:column-resize (detail {field, width}) on the root.
function bindResize(ctl, el, pre, st) {
  ctl.delegate("mousedown", "." + pre + "-resize-handle", function (e, handle) {
    if (e.button !== 0) { return; }
    e.preventDefault();
    e.stopPropagation();
    var th = handle.parentNode;
    var idx = Array.prototype.indexOf.call(th.parentNode.children, th);
    var cols = [partTable(el, pre, "header"), partTable(el, pre, "body")]
      .map(function (t) { return kid(t, "colgroup"); })
      .filter(Boolean)
      .map(function (g) { return g.children[idx]; })
      .filter(Boolean);
    var x0 = e.pageX, w0 = th.getBoundingClientRect().width, w = w0;
    var stop = new AbortController();
    function move(ev) {
      w = Math.max(40, Math.round(w0 + ev.pageX - x0));
      cols.forEach(function (c) { c.style.width = w + "px"; c.style.minWidth = w + "px"; });
    }
    function up() {
      stop.abort();
      st.stopResize = null;
      ctl.fire("ah:column-resize", { field: th.getAttribute("data-field"), width: w });
    }
    document.addEventListener("mousemove", move, { signal: stop.signal });
    document.addEventListener("mouseup", up, { signal: stop.signal });
    st.stopResize = function () { stop.abort(); };
  });
}

// Sort state after a header click: asc -> desc -> none.
function nextSort(el, field) {
  var cur = el.getAttribute("data-sort-field"), dir = el.getAttribute("data-sort-dir");
  if (cur !== field) { return "asc"; }
  return dir === "asc" ? "desc" : (dir === "desc" ? null : "asc");
}

function writeSort(el, pre, field, dir) {
  if (field && dir) {
    el.setAttribute("data-sort-field", field);
    el.setAttribute("data-sort-dir", dir);
  } else {
    el.removeAttribute("data-sort-field");
    el.removeAttribute("data-sort-dir");
  }
  var thead = kid(partTable(el, pre, "header"), "thead");
  kids(thead, "tr").forEach(function (tr) {
    kids(tr, "th[data-field]").forEach(function (th) {
      var on = !!dir && th.getAttribute("data-field") === field;
      th.classList.toggle(pre + "-sort-asc", on && dir === "asc");
      th.classList.toggle(pre + "-sort-desc", on && dir === "desc");
      if (on) { th.setAttribute("aria-sort", dir === "asc" ? "ascending" : "descending"); }
      else { th.removeAttribute("aria-sort"); }
    });
  });
}

function isOff(el) { return el.getAttribute("aria-disabled") === "true"; }

// Elements inside a cell that keep their own clicks.
var OWN = "a, button, input, select, textarea, label";

AH.lib = AH.lib || {};
AH.lib.table = {
  nextId: nextId,
  kids: kids,
  kid: kid,
  keysOf: keysOf,
  toKeys: toKeys,
  writeValue: writeValue,
  sameKeys: sameKeys,
  raw: raw,
  isNum: isNum,
  compare: compare,
  ord: ord,
  clickSelect: clickSelect,
  toggleKey: toggleKey,
  keySelect: keySelect,
  markRows: markRows,
  headerCheck: headerCheck,
  focusRow: focusRow,
  part: part,
  partTable: partTable,
  bindScroll: bindScroll,
  syncGutter: syncGutter,
  watchGutter: watchGutter,
  bindResize: bindResize,
  nextSort: nextSort,
  writeSort: writeSort,
  isOff: isOff,
  OWN: OWN
};
