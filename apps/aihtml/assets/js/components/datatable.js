/* Controller of the data table (designs/04-components.md).
 *
 * Ported from sigil (data/datatable). The server renders every row,
 * header and pager; this file moves state around in that DOM. Local mode:
 * sort, filter row / search / advanced filters and paging over the
 * rendered rows (the pager comes from the shared template
 * datatable_pager); remote mode (data-mode "remote"): each view change
 * writes the state on the root (data-sort-field, data-sort-dir,
 * data-page, data-page-size, data-search, data-filters as JSON) and fires
 * ah:query, whose action answers with datatable_rows/3 (a morph of the
 * whole table). Selection, row details, inline editing (ah:cell-edit,
 * sent to the edit action, data-edit), column resize and the column
 * chooser work in both modes. Shared helpers are in _lib_table.js
 * (AH.lib.table).
 *
 * Links: with the href option the pager's prev / next / page buttons are
 * <a href> (crawlable; the page at the URL renders that state). A plain
 * left click is intercepted: the table pages as above and the link's URL
 * is pushed to the history (core's url op; back / forward reload it).
 * The pager re-rendered here fills the template (the pager's data-href)
 * from the current sort and search.
 *
 * The view state lives in the DOM (root data-* attributes, filter inputs,
 * row attributes), so a morph keeps it; the server calls refresh after
 * one. The selection is the root's data-ah-value (keys joined with
 * commas), mirrored into a hidden input; "change" fires when the user
 * changes it, never from methods.
 *
 * Events (native CustomEvents, bubbling; the detail in e.detail):
 *   ah:query          {sort, dir, page, pageSize, search, filters}
 *   ah:sort           {field, dir}
 *   ah:page           {page, pageSize}
 *   ah:filter         {filters, search}
 *   ah:row-click, ah:row-dblclick, ah:row-expand, ah:row-collapse  {key}
 *                     (the key is also the root's data-key)
 *   ah:cell-edit      {key, field, value, old}, fired on the cell
 *   ah:columns        {hidden}
 *   ah:column-resize  {field, width}
 */
import AH from "../core.js";
import "./_lib_table.js";
import "./_lib_values.js";
import "virtual:ah-tpl/datatable_pager";

var L = AH.lib.table;
var V = AH.lib.values;
var kids = L.kids;
var kid = L.kid;
var keysOf = L.keysOf;
var toKeys = L.toKeys;
var writeValue = L.writeValue;
var sameKeys = L.sameKeys;
var raw = L.raw;
var isNum = L.isNum;
var compare = L.compare;
var ord = L.ord;
var clickSelect = L.clickSelect;
var keySelect = L.keySelect;
var markRows = L.markRows;
var headerCheck = L.headerCheck;
var focusRow = L.focusRow;
var part = L.part;
var partTable = L.partTable;
var nextSort = L.nextSort;
var writeSort = L.writeSort;
var isOff = L.isOff;
var OWN = L.OWN;

function qa(root, sel) { return root ? Array.from(root.querySelectorAll(sel)) : []; }
function header(el) { return part(el, "ah-dt", "header"); }

function dtBody(el) { return document.getElementById(el.id + "-rows"); }
function dtRows(el) { return kids(dtBody(el), "tr.ah-dt-row"); }
function dtShown(el) { return dtRows(el).filter(function (r) { return !r.hidden; }); }
function dtRemote(el) { return el.getAttribute("data-mode") === "remote"; }
function dtByKey(el, key) {
  key = String(key);
  return dtRows(el).filter(function (r) { return r.getAttribute("data-key") === key; })[0] || null;
}
function dtDetail(el, key) {
  return kids(dtBody(el), "tr.ah-dt-row-details").filter(function (r) {
    return r.getAttribute("data-key") === key;
  })[0] || null;
}
function dtHeaderRow(el) {
  return kid(kid(partTable(el, "ah-dt", "header"), "thead"), "tr.ah-dt-header-row");
}
function dtTh(el, field) {
  return kids(dtHeaderRow(el), "th[data-field]").filter(function (th) {
    return th.getAttribute("data-field") === field;
  })[0] || null;
}
function dtCell(tr, field) {
  return kids(tr, "td[data-field]").filter(function (td) {
    return td.getAttribute("data-field") === field;
  })[0] || null;
}
function num(v, d) { var n = parseInt(v, 10); return n > 0 ? n : d; }
function keyOf(r) { return r.getAttribute("data-key"); }
function siblingInput(sel) {
  return sel.parentNode ? kid(sel.parentNode, ".ah-dt-adv-filter-input") : null;
}

// Column filters from the filter row: text, or {condition, value}.
function dtFilters(el) {
  var f = {};
  var h = header(el);
  qa(h, ".ah-dt-filter-input").forEach(function (i) {
    if (i.value) { f[i.getAttribute("data-field")] = i.value; }
  });
  qa(h, ".ah-dt-adv-filter-select").forEach(function (s) {
    var cond = s.value;
    var input = siblingInput(s);
    var v = (input && input.value) || "";
    if (cond === "empty" || cond === "not_empty") { f[s.getAttribute("data-field")] = { condition: cond, value: "" }; }
    else if (v) { f[s.getAttribute("data-field")] = { condition: cond, value: v }; }
  });
  return f;
}

function dtSearch(el) {
  var s = qa(header(el), ".ah-dt-search-input")[0];
  return s ? s.value : (el.getAttribute("data-search") || "");
}

function lower(s) { return String(s).toLowerCase(); }

function matchCond(cond, v, f, number) {
  var t = lower(v), q = lower(f);
  var nv = parseFloat(v), nf = parseFloat(f), both = isNum(v) && isNum(f);
  switch (cond) {
    case "empty": return v === "";
    case "not_empty": return v !== "";
    case "contains": return t.indexOf(q) >= 0;
    case "not_contains": return t.indexOf(q) < 0;
    case "starts_with": return t.indexOf(q) === 0;
    case "ends_with": return t.length >= q.length && t.slice(t.length - q.length) === q;
    case "equals": return number ? both && nv === nf : t === q;
    case "not_equals": return number ? !(both && nv === nf) : t !== q;
    case "gt": return both && nv > nf;
    case "gte": return both && nv >= nf;
    case "lt": return both && nv < nf;
    case "lte": return both && nv <= nf;
    default: return true;
  }
}

function dtMatches(tr, fields, search, filters, types) {
  if (search) {
    var hit = fields.some(function (f) { return lower(raw(dtCell(tr, f))).indexOf(search) >= 0; });
    if (!hit) { return false; }
  }
  return Object.keys(filters).every(function (f) {
    var flt = filters[f], v = raw(dtCell(tr, f));
    if (typeof flt === "string") { return lower(v).indexOf(lower(flt)) >= 0; }
    return matchCond(flt.condition, v, flt.value, types[f] === "number");
  });
}

// The pager's view data, as aihtml_datatable:pager_view/6 builds it;
// base is the href template with {sort} and {search} filled in, or null.
function pagerView(page, size, total, sizes, t, base) {
  var pages = Math.max(1, Math.ceil(total / size));
  var start = total === 0 ? 0 : (page - 1) * size + 1;
  var end = Math.min(page * size, total);
  var info = (t.info || "")
    .split("{start}").join(start).split("{end}").join(end).split("{total}").join(total)
    .split("{page}").join(page).split("{pages}").join(pages);
  var btns;
  if (pages <= 7) {
    btns = [];
    for (var i = 1; i <= pages; i++) { btns.push(i); }
  } else {
    btns = [1];
    if (page > 3) { btns.push(0); }
    for (var m = Math.max(2, page - 1); m <= Math.min(page + 1, pages - 1); m++) { btns.push(m); }
    if (page < pages - 2) { btns.push(0); }
    btns.push(pages);
  }
  var link = base != null;
  var url = function (p) {
    return link ? base.split("{page}").join(String(p)).split("{size}").join(String(size)) : "";
  };
  var all = sizes.concat([size]).filter(function (s, j, a) { return a.indexOf(s) === j; })
    .sort(function (a, b) { return a - b; });
  return {
    info: info, prev_label: t.prev, next_label: t.next, size_label: t.size,
    prev_disabled: page <= 1, next_disabled: page >= pages,
    prev_link: link && page > 1, prev_href: url(page - 1),
    next_link: link && page < pages, next_href: url(page + 1),
    buttons: btns.map(function (b) {
      return b === 0 ? { gap: true, page: 0, active: false, link: false, href: "" }
        : { gap: false, page: b, active: b === page, link: link && b !== page, href: url(b) };
    }),
    has_sizes: sizes.length > 0,
    sizes: all.map(function (s) { return { size: s, selected: s === size }; })
  };
}

function dtPager(el, page, size, total) {
  var c = kid(el, ".ah-dt-pager-container");
  if (!c || !AH.tpl || !AH.tpl.datatable_pager) { return; }
  var sizes = (c.getAttribute("data-sizes") || "").split(",").filter(Boolean).map(Number);
  var t = { info: c.getAttribute("data-info"), prev: c.getAttribute("data-prev"),
            next: c.getAttribute("data-next"), size: c.getAttribute("data-size") };
  c.innerHTML = AH.tpl.datatable_pager(pagerView(page, size, total, sizes, t, linkBase(el, c)));
}

// The href template (data-href of the pager) with {sort} and {search}
// filled in from the current view, as aihtml_datatable:link_base/3 does.
function linkBase(el, c) {
  var tpl = c.getAttribute("data-href");
  if (tpl == null) { return null; }
  var field = el.getAttribute("data-sort-field"), dir = el.getAttribute("data-sort-dir");
  var sort = field && dir ? field + ":" + dir : "";
  return tpl.split("{sort}").join(encodeURIComponent(sort))
    .split("{search}").join(encodeURIComponent(dtSearch(el)));
}

// A plain left click on a pager link is handled in place; any other
// click (new tab, window, download) is left to the browser.
function plainClick(e) {
  return e.button === 0 && !e.metaKey && !e.ctrlKey && !e.shiftKey && !e.altKey;
}

function pushUrl(url) { AH.apply([{ op: "url", mode: "push", value: url }]); }

function headerBox(el) { return qa(header(el), ".ah-dt-header-checkbox")[0] || null; }

function dtStripes(el, shown) {
  var alt = el.getAttribute("data-alt-rows") === "true";
  shown.forEach(function (r, i) { r.classList.toggle("ah-dt-row-alt", alt && i % 2 === 1); });
  kids(dtBody(el), "tr.ah-dt-row-empty").forEach(function (r) { r.hidden = shown.length > 0; });
  var rows = dtRows(el);
  var stop = rows.filter(function (r) { return r.getAttribute("tabindex") === "0"; })[0];
  if (!stop || stop.hidden) { focusRow(rows, shown[0], false); }
  headerCheck(headerBox(el), keysOf(el), shown.map(keyOf));
}

// Local mode: search, filters, sort and page over the rendered rows.
function dtApply(el) {
  var body = dtBody(el);
  var rows = dtRows(el);
  var fields = [], types = {};
  kids(dtHeaderRow(el), "th[data-field]").forEach(function (th) {
    fields.push(th.getAttribute("data-field"));
    types[th.getAttribute("data-field")] = th.getAttribute("data-type");
  });
  var search = lower(String(dtSearch(el)).trim());
  var filters = dtFilters(el);
  var field = el.getAttribute("data-sort-field"), dir = el.getAttribute("data-sort-dir");
  var sign = dir === "desc" ? -1 : 1;
  var match = rows.filter(function (r) { return dtMatches(r, fields, search, filters, types); });
  match.sort(function (a, b) {
    var c = field && dir ? sign * compare(raw(dtCell(a, field)), raw(dtCell(b, field))) : 0;
    return c || ord(a) - ord(b);
  });
  var rest = rows.filter(function (r) { return match.indexOf(r) < 0; })
    .sort(function (a, b) { return ord(a) - ord(b); });
  var end = kid(body, "tr.ah-dt-row-empty");
  match.concat(rest).forEach(function (r) {
    body.insertBefore(r, end);
    var d = dtDetail(el, keyOf(r));
    if (d) { body.insertBefore(d, end); }
  });
  var size = num(el.getAttribute("data-page-size"), 0);
  var total = match.length;
  var page = num(el.getAttribute("data-page"), 1);
  if (size) {
    page = Math.min(page, Math.max(1, Math.ceil(total / size)));
    el.setAttribute("data-page", page);
  }
  var shown = size ? match.slice((page - 1) * size, page * size) : match;
  rows.forEach(function (r) {
    var on = shown.indexOf(r) >= 0;
    r.hidden = !on;
    var d = dtDetail(el, keyOf(r));
    if (d) { d.hidden = !on; }
  });
  dtStripes(el, shown);
  if (size) { dtPager(el, page, size, total); }
}

// Remote mode: the state goes on the root and the server answers.
function dtQuery(ctl, el) {
  var f = dtFilters(el);
  if (Object.keys(f).length) { el.setAttribute("data-filters", JSON.stringify(f)); }
  else { el.removeAttribute("data-filters"); }
  var s = dtSearch(el);
  if (s) { el.setAttribute("data-search", s); } else { el.removeAttribute("data-search"); }
  kids(el, ".ah-dt-content").forEach(function (c) { c.classList.add("ah-dt-loading"); });
  ctl.fire("ah:query", { sort: el.getAttribute("data-sort-field"), dir: el.getAttribute("data-sort-dir"),
                         page: num(el.getAttribute("data-page"), 1),
                         pageSize: num(el.getAttribute("data-page-size"), 0) || null,
                         search: s, filters: f });
}

function dtView(ctl, el) {
  if (dtRemote(el)) { dtQuery(ctl, el); } else { dtApply(el); }
}

function dtSelect(ctl, el, keys, user) {
  var prev = keysOf(el);
  markRows(dtRows(el), "ah-dt", keys, el.getAttribute("data-selection"));
  writeValue(el, keys);
  headerCheck(headerBox(el), keys, dtShown(el).map(keyOf));
  if (user && !sameKeys(prev, keys)) { ctl.fire("change"); }
}

function dtEvent(ctl, el, name, key, extra) {
  el.setAttribute("data-key", key);
  ctl.fire(name, Object.assign({ key: key }, extra || {}));
}

function expandBtns(tr) {
  var out = [];
  kids(tr, "td").forEach(function (td) { out.push.apply(out, kids(td, ".ah-dt-expand-btn")); });
  return out;
}

function dtSetDetails(ctl, el, key, open, user) {
  var tr = dtByKey(el, key), d = dtDetail(el, key);
  if (!tr || !d) { return; }
  var was = !d.classList.contains("ah-dt-row-details-hidden");
  if (was === open) { return; }
  d.classList.toggle("ah-dt-row-details-hidden", !open);
  expandBtns(tr).forEach(function (b) {
    b.classList.toggle("ah-dt-expand-btn-open", open);
    b.setAttribute("aria-expanded", String(open));
  });
  var list = V.split(el.getAttribute("data-expanded")).filter(function (k) { return k && k !== key; });
  if (open) { list.push(key); }
  if (list.length) { el.setAttribute("data-expanded", V.join(list)); } else { el.removeAttribute("data-expanded"); }
  if (user) { dtEvent(ctl, el, open ? "ah:row-expand" : "ah:row-collapse", key); }
}

function dtSetHidden(ctl, el, field, hide) {
  var th = dtTh(el, field);
  if (!th) { return; }
  var row = th.parentNode;
  var idx = Array.prototype.indexOf.call(row.children, th);
  [partTable(el, "ah-dt", "header"), partTable(el, "ah-dt", "body")].forEach(function (t) {
    kids(t, "colgroup").forEach(function (g) {
      var c = g.children[idx];
      if (c) { c.hidden = hide; }
    });
  });
  th.hidden = hide;
  kids(kid(partTable(el, "ah-dt", "header"), "thead"), "tr.ah-dt-filter-row").forEach(function (tr) {
    if (tr.children[idx]) { tr.children[idx].hidden = hide; }
  });
  dtRows(el).forEach(function (r) { var c = dtCell(r, field); if (c) { c.hidden = hide; } });
  var hidden = kids(row, "th[data-field]").filter(function (h) { return h.hidden; })
    .map(function (h) { return h.getAttribute("data-field"); });
  if (hidden.length) { el.setAttribute("data-hidden", V.join(hidden)); } else { el.removeAttribute("data-hidden"); }
  var span = kids(row, "th").filter(function (h) { return !h.hidden; }).length;
  kids(dtBody(el), "tr").forEach(function (tr) {
    kids(tr, "td.ah-dt-cell-empty, td.ah-dt-row-details-cell").forEach(function (td) {
      td.setAttribute("colspan", span);
    });
  });
  kids(el, ".ah-dt-chooser-panel").forEach(function (p) {
    qa(p, ".ah-dt-chooser-checkbox").forEach(function (cb) {
      if (cb.getAttribute("data-field") === field) { cb.checked = !hide; }
    });
  });
  ctl.fire("ah:columns", { hidden: hidden });
}

// ---- inline editing ------------------------------------------------

function dtEditable(el) { return el.getAttribute("data-editable") === "true"; }

function dtBeginEdit(ctl, el, st, td) {
  if (!td || !dtEditable(el) || isOff(el)) { return; }
  if (st.edit) { dtEndEdit(ctl, el, st, true); }
  var tr = td.parentNode;
  var field = td.getAttribute("data-field");
  var th = dtTh(el, field);
  var type = (th && th.getAttribute("data-type")) || "text";
  var old = raw(td);
  var input = document.createElement("input");
  input.className = "ah-dt-editor ah-dt-editor-" + type;
  if (type === "checkbox") {
    input.type = "checkbox";
    input.checked = old === "true";
  } else {
    input.type = type === "number" ? "number" : (type === "date" ? "date" : "text");
    input.value = old;
  }
  st.edit = { td: td, tr: tr, field: field, type: type, old: old, html: td.innerHTML, input: input };
  td.innerHTML = "";
  td.appendChild(input);
  input.focus();
  if (type !== "checkbox" && input.select) { input.select(); }
}

// Commit (or cancel) the open editor; returns the edited cell.
function dtEndEdit(ctl, el, st, commit) {
  var ed = st.edit;
  if (!ed) { return null; }
  st.edit = null;
  var value = ed.type === "checkbox" ? String(ed.input.checked) : ed.input.value;
  if (!commit || value === ed.old) {
    ed.td.innerHTML = ed.html;
    return ed.td;
  }
  ed.td.innerHTML = "";
  var span = document.createElement("span");
  span.textContent = value;
  ed.td.appendChild(span);
  ed.td.setAttribute("data-value", value);
  var key = ed.tr.getAttribute("data-key");
  ed.td.setAttribute("data-key", key);
  ed.td.setAttribute("data-old", ed.old);
  ed.td.setAttribute("data-table", el.id);
  var token = el.getAttribute("data-edit");
  if (token && !ed.td.hasAttribute("data-ah-on")) {
    ed.td.setAttribute("data-ah-on", "ah:cell-edit:" + token);
    AH.mount(ed.td);
  }
  ctl.fire("ah:cell-edit", { key: key, field: ed.field, value: value, old: ed.old }, ed.td);
  return ed.td;
}

function dtEditKey(ctl, el, st, e) {
  var ed = st.edit;
  if (!ed || e.target !== ed.input) { return; }
  e.stopPropagation();
  if (e.key === "Escape") {
    e.preventDefault();
    dtEndEdit(ctl, el, st, false);
    focusRow(dtRows(el), ed.tr, true);
  } else if (e.key === "Enter") {
    e.preventDefault();
    dtEndEdit(ctl, el, st, true);
    focusRow(dtRows(el), ed.tr, true);
  } else if (e.key === "Tab") {
    e.preventDefault();
    var cells = [];
    dtShown(el).forEach(function (r) {
      kids(r, "td.ah-dt-cell-editable").forEach(function (td) { if (!td.hidden) { cells.push(td); } });
    });
    var i = cells.indexOf(ed.td);
    var next = cells[i + (e.shiftKey ? -1 : 1)];
    dtEndEdit(ctl, el, st, true);
    if (next) { dtBeginEdit(ctl, el, st, next); } else { focusRow(dtRows(el), ed.tr, true); }
  }
}

// ---- column chooser -------------------------------------------------

function dtChooser(el, st, open) {
  var p = kid(el, ".ah-dt-chooser-panel");
  var btn = qa(header(el), ".ah-dt-chooser-btn")[0];
  if (!p || !btn) { return; }
  if (st.float) { st.float.stop(); st.float = null; }
  if (st.chooserOff) { st.chooserOff.abort(); st.chooserOff = null; }
  p.classList.toggle("ah-dt-chooser-panel-open", open);
  btn.setAttribute("aria-expanded", String(open));
  if (!open) { return; }
  st.float = AH.float(p, btn, { placement: "bottom", align: "end", offset: 4 });
  var off = st.chooserOff = new AbortController();
  document.addEventListener("mousedown", function (e) {
    if (!p.contains(e.target) && !btn.contains(e.target)) { dtChooser(el, st, false); }
  }, { signal: off.signal });
  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape") { dtChooser(el, st, false); btn.focus(); }
  }, { signal: off.signal });
}

// ---- events -----------------------------------------------------------

function dtKeydown(ctl, el, st, e, tr) {
  if (e.target !== tr || tr.parentNode !== dtBody(el) || isOff(el) || e.altKey || e.metaKey) { return; }
  var vis = dtShown(el);
  var i = vis.indexOf(tr);
  var key = keyOf(tr);
  var to = null;
  switch (e.key) {
    case "ArrowDown": to = vis[Math.min(i + 1, vis.length - 1)]; break;
    case "ArrowUp": to = vis[Math.max(i - 1, 0)]; break;
    case "Home": to = vis[0]; break;
    case "End": to = vis[vis.length - 1]; break;
    case "PageDown": to = vis[Math.min(i + 10, vis.length - 1)]; break;
    case "PageUp": to = vis[Math.max(i - 10, 0)]; break;
    case "ArrowRight": dtSetDetails(ctl, el, key, true, true); break;
    case "ArrowLeft": dtSetDetails(ctl, el, key, false, true); break;
    case "F2":
    case "Enter":
      if (dtEditable(el)) {
        var cell = kids(tr, "td.ah-dt-cell-editable").filter(function (td) { return !td.hidden; })[0];
        if (cell) { dtBeginEdit(ctl, el, st, cell); break; }
      }
      if (e.key === "F2") { break; }
      /* falls through */
    case " ":
      var next = keySelect(el.getAttribute("data-selection"), keysOf(el), key);
      if (next) { dtSelect(ctl, el, next, true); st.anchor = key; }
      break;
    default:
      return;
  }
  e.preventDefault();
  if (to) { focusRow(dtRows(el), to, true); }
}

function dtClick(ctl, el, st, e, tr) {
  if (tr.parentNode !== dtBody(el) || isOff(el)) { return; }
  var key = keyOf(tr);
  var btn = e.target.closest(".ah-dt-expand-btn");
  if (btn && tr.contains(btn)) {
    dtSetDetails(ctl, el, key, !btn.classList.contains("ah-dt-expand-btn-open"), true);
    return;
  }
  if (st.edit && st.edit.td.contains(e.target) && st.edit.td !== e.target) { return; }
  focusRow(dtRows(el), tr, false);
  var own = e.target.closest(OWN);
  if (own && tr.contains(own)) { return; }
  var next = clickSelect(el.getAttribute("data-selection"), keysOf(el), key, e, st.anchor,
                         dtShown(el).map(keyOf));
  if (next) {
    dtSelect(ctl, el, next, true);
    if (!e.shiftKey) { st.anchor = key; }
  }
  dtEvent(ctl, el, "ah:row-click", key);
}

function dtGoTo(ctl, el, page) {
  el.setAttribute("data-page", Math.max(1, page));
  dtView(ctl, el);
  ctl.fire("ah:page", { page: num(el.getAttribute("data-page"), 1),
                        pageSize: num(el.getAttribute("data-page-size"), 0) });
}

function dtFiltered(ctl, el) {
  el.setAttribute("data-page", 1);
  dtView(ctl, el);
  ctl.fire("ah:filter", { filters: dtFilters(el), search: dtSearch(el) });
}

var FILTER_INPUTS = ".ah-dt-filter-input, .ah-dt-adv-filter-input, .ah-dt-search-input";

AH.register("datatable", class extends AH.Controller {
  setup() {
    var el = this.element, ctl = this;
    var st = this.st = { id: L.nextId(), anchor: null, edit: null, float: null, timer: null,
                         stopResize: null, chooserOff: null, ro: null };
    var debounced = function () {
      clearTimeout(st.timer);
      st.timer = setTimeout(function () { dtFiltered(ctl, el); }, 200);
    };
    var ROW = "tbody > tr.ah-dt-row";
    this.delegate("click", ROW, function (e, tr) { dtClick(ctl, el, st, e, tr); });
    this.delegate("dblclick", ROW, function (e, tr) {
      if (tr.parentNode !== dtBody(el)) { return; }
      var td = e.target.closest("td.ah-dt-cell-editable");
      if (td && td.parentNode === tr) { dtBeginEdit(ctl, el, st, td); }
      dtEvent(ctl, el, "ah:row-dblclick", keyOf(tr));
    });
    this.listen(el, "keydown", function (e) {
      if (!(e.target instanceof Element)) { return; }
      if (e.target.matches(".ah-dt-editor")) { dtEditKey(ctl, el, st, e); return; }
      var tr = e.target.closest(ROW);
      if (tr && el.contains(tr)) { dtKeydown(ctl, el, st, e, tr); return; }
      var th = e.target.closest(".ah-dt-th-sortable");
      if (th && el.contains(th) && (e.key === "Enter" || e.key === " ")) {
        e.preventDefault();
        th.click();
      }
    });
    this.delegate("focusout", ".ah-dt-editor", function () {
      var ed = st.edit;
      setTimeout(function () { if (st.edit === ed && ed) { dtEndEdit(ctl, el, st, true); } }, 0);
    });
    // Inner controls (row / header boxes, filters, pager, chooser) report
    // to the table, not as its own change: stopImmediatePropagation also
    // keeps them from listeners on the root added after this one, as
    // jQuery's delegated stopPropagation did.
    this.listen(el, "change", function (e) {
      var t = e.target;
      if (!(t instanceof Element) || t === el) { return; }
      if (t.matches(".ah-dt-row-checkbox")) {
        e.stopImmediatePropagation();
        var k = keyOf(t.closest("tr"));
        var rest = keysOf(el).filter(function (x) { return x !== k; });
        dtSelect(ctl, el, t.checked ? rest.concat([k]) : rest, true);
      } else if (t.matches(".ah-dt-header-checkbox")) {
        e.stopImmediatePropagation();
        var page = dtShown(el).map(keyOf);
        var others = keysOf(el).filter(function (x) { return page.indexOf(x) < 0; });
        dtSelect(ctl, el, t.checked ? others.concat(page) : others, true);
      } else if (t.matches(FILTER_INPUTS)) {
        e.stopImmediatePropagation();
      } else if (t.matches(".ah-dt-adv-filter-select")) {
        e.stopImmediatePropagation();
        var none = t.value === "empty" || t.value === "not_empty";
        var input = siblingInput(t);
        if (input) {
          input.disabled = none;
          if (none) { input.value = ""; }
        }
        dtFiltered(ctl, el);
      } else if (t.matches(".ah-dt-pager-size-select")) {
        e.stopImmediatePropagation();
        el.setAttribute("data-page-size", num(t.value, 10));
        dtGoTo(ctl, el, 1);
      } else if (t.matches(".ah-dt-chooser-checkbox")) {
        e.stopImmediatePropagation();
        dtSetHidden(ctl, el, t.getAttribute("data-field"), !t.checked);
      }
    });
    this.delegate("click", ".ah-dt-th-sortable", function (e, th) {
      if (e.target.closest(".ah-dt-resize-handle") || isOff(el)) { return; }
      var field = th.getAttribute("data-field");
      var dir = nextSort(el, field);
      writeSort(el, "ah-dt", dir ? field : null, dir);
      el.setAttribute("data-page", 1);
      dtView(ctl, el);
      ctl.fire("ah:sort", { field: field, dir: dir });
    });
    this.delegate("input", FILTER_INPUTS, function (e) {
      e.stopImmediatePropagation();
      debounced();
    });
    // Pager buttons; with href they are links: a plain click pages in
    // place and pushes the link's URL, other clicks go to the browser.
    var pagerClick = function (e, b, page) {
      var href = b.tagName === "A" ? b.getAttribute("href") : null;
      if (href !== null) {
        if (!plainClick(e)) { return; }
        e.preventDefault();
      }
      dtGoTo(ctl, el, page);
      if (href !== null) { pushUrl(href); }
    };
    this.delegate("click", ".ah-dt-pager-btn-num", function (e, b) { pagerClick(e, b, num(b.getAttribute("data-page"), 1)); });
    this.delegate("click", ".ah-dt-pager-btn-prev", function (e, b) { pagerClick(e, b, num(el.getAttribute("data-page"), 1) - 1); });
    this.delegate("click", ".ah-dt-pager-btn-next", function (e, b) { pagerClick(e, b, num(el.getAttribute("data-page"), 1) + 1); });
    this.delegate("click", ".ah-dt-chooser-btn", function () {
      var p = kid(el, ".ah-dt-chooser-panel");
      dtChooser(el, st, !(p && p.classList.contains("ah-dt-chooser-panel-open")));
    });
    this.listen(el, "ah:error", function () {
      kids(el, ".ah-dt-content").forEach(function (c) { c.classList.remove("ah-dt-loading"); });
    });
    L.bindScroll(this, el, "ah-dt");
    L.bindResize(this, el, "ah-dt", st);
    L.watchGutter(el, "ah-dt", st);
    if (dtRemote(el)) { dtStripes(el, dtShown(el)); } else { dtApply(el); }
  }

  teardown() {
    var st = this.st;
    clearTimeout(st.timer);
    if (st.float) { st.float.stop(); st.float = null; }
    if (st.stopResize) { st.stopResize(); }
    if (st.ro) { st.ro.disconnect(); }
    if (st.chooserOff) { st.chooserOff.abort(); st.chooserOff = null; }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  setValue(v) {
    var el = this.element;
    dtSelect(this, el, toKeys(v).filter(function (k) { return dtByKey(el, k); }), false);
  }
  clearSelection() { dtSelect(this, this.element, [], false); }
  sort(field, dir) {
    var el = this.element;
    dir = dir === "asc" || dir === "desc" ? dir : null;
    writeSort(el, "ah-dt", dir ? field : null, dir);
    el.setAttribute("data-page", 1);
    dtView(this, el);
  }
  goToPage(page) { this.element.setAttribute("data-page", num(page, 1)); dtView(this, this.element); }
  setPageSize(size) {
    var el = this.element;
    el.setAttribute("data-page-size", num(size, 10));
    el.setAttribute("data-page", 1);
    dtView(this, el);
  }
  setSearch(text) {
    var el = this.element;
    qa(header(el), ".ah-dt-search-input").forEach(function (i) { i.value = text || ""; });
    if (text) { el.setAttribute("data-search", text); } else { el.removeAttribute("data-search"); }
    el.setAttribute("data-page", 1);
    dtView(this, el);
  }
  clearFilters() {
    var el = this.element, h = header(el);
    qa(h, FILTER_INPUTS).forEach(function (i) { i.value = ""; });
    qa(h, ".ah-dt-adv-filter-select").forEach(function (s) { s.value = "contains"; });
    qa(h, ".ah-dt-adv-filter-input").forEach(function (i) { i.disabled = false; });
    el.removeAttribute("data-search");
    el.setAttribute("data-page", 1);
    dtView(this, el);
  }
  showColumn(field) { dtSetHidden(this, this.element, String(field), false); }
  hideColumn(field) { dtSetHidden(this, this.element, String(field), true); }
  expandRow(key) { dtSetDetails(this, this.element, String(key), true, false); }
  collapseRow(key) { dtSetDetails(this, this.element, String(key), false, false); }
  refresh() {
    var el = this.element;
    markRows(dtRows(el), "ah-dt", keysOf(el), el.getAttribute("data-selection"));
    V.split(el.getAttribute("data-hidden")).filter(Boolean).forEach(function (f) {
      dtRows(el).forEach(function (r) { var c = dtCell(r, f); if (c) { c.hidden = true; } });
    });
    V.split(el.getAttribute("data-expanded")).filter(Boolean).forEach(function (k) {
      var d = dtDetail(el, k);
      if (d && d.classList.contains("ah-dt-row-details-hidden")) {
        d.classList.remove("ah-dt-row-details-hidden");
        var tr = dtByKey(el, k);
        if (tr) {
          expandBtns(tr).forEach(function (b) {
            b.classList.add("ah-dt-expand-btn-open");
            b.setAttribute("aria-expanded", "true");
          });
        }
      }
    });
    kids(el, ".ah-dt-content").forEach(function (c) { c.classList.remove("ah-dt-loading"); });
    if (dtRemote(el)) { dtStripes(el, dtShown(el)); } else { dtApply(el); }
  }
});
