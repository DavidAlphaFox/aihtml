/* Controller of the data grid (designs/04-components.md).
 *
 * Ported from sigil (data/datagrid). The server renders the header, the
 * rows, the pager and the status bar (aihtml_datagrid); this file keeps
 * the view state and moves that DOM around:
 *
 *   local mode   all rows are in the body; sorting, filtering (filter row,
 *                search box), paging and grouping reorder and hide them
 *                (.ah-dg-row-off), group rows and the pager come from the
 *                shared templates (datagrid_group_row, datagrid_pager),
 *                the status bar aggregates are recomputed
 *   remote mode  (data-ah-remote) the server rendered the first page; every
 *                view change fires ah:query on the grid's hidden
 *                .ah-dg-query element (its data-* carry the view); the
 *                server answers with datagrid_rows/4, which morphs the rows
 *                and the pager in and calls rowsLoaded. Nothing is asked
 *                for on mount (refresh() does that on demand).
 *
 * Pager links (data-ah-href, the `href' option): the pager's buttons are
 * <a href> links for crawlers and new tabs; a plain left click stays in
 * the page (the same re-page or query as a button) and pushes the link's
 * URL to the history (core's url operation: going back reloads it).
 *
 * Also: selection (single / multi / checkbox, data-ah-value + hidden input
 * + change), keyboard navigation over cells (roving tabindex, ARIA grid),
 * column resize, pinning (sticky), hiding, the column menu (template
 * datagrid_column_menu, positioned with AH.float), inline editing
 * (ah:edit), and CSV / Excel / PDF export (xlsx, jspdf and jspdf-autotable
 * by dynamic import(): separate chunks, loaded on the first export).
 *
 * Component events are native CustomEvents (bubbling). Their details are
 * in e.detail ({key, field, value, old, name, expanded}, whichever apply)
 * and, while they are dispatched, also data-* attributes of the root, so
 * postbacks bound with on/2 receive them in Event.data:
 *   ah:sort {field, value: "asc" | "desc" | ""}, ah:filter {field, value},
 *   ah:page {value}, ah:group-toggle {value, expanded},
 *   ah:column-resize {field, value}, ah:edit {key, field, value, old},
 *   ah:row-click / ah:row-dblclick {key, field}, ah:command {key, field,
 *   name}, ah:toolbar {name}; ah:query (no detail) on .ah-dg-query.
 */
import AH from "../core.js";
import "./_lib_values.js";
import "virtual:ah-tpl/datagrid_column_menu";
import "virtual:ah-tpl/datagrid_group_row";
import "virtual:ah-tpl/datagrid_pager";

var seq = 0;
var CHECK = "__checkbox";
var DETAIL = ["key", "field", "value", "old", "name", "expanded"];
var NUM_RE = /^-?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?$/;
var PAGER_BUTTONS = 7;

// ------------------------------------------------------------------
// Values: raw text (data-v, else the text), comparison, formatting
// ------------------------------------------------------------------

function raw(cell) {
  if (!cell) { return ""; }
  var v = cell.getAttribute("data-v");
  return v === null ? cell.textContent : v;
}
function text(cell) { return cell ? cell.textContent : ""; }

function toNumber(s) { return NUM_RE.test(s) ? Number(s) : null; }

// The same order as aihtml_datagrid:compare/2.
function compare(a, b) {
  var na = toNumber(a), nb = toNumber(b);
  if (na !== null && nb !== null) { return na < nb ? -1 : (na > nb ? 1 : 0); }
  a = a.toLowerCase();
  b = b.toLowerCase();
  return a < b ? -1 : (a > b ? 1 : 0);
}

function thousands(n, d) {
  var s = n.toFixed(d), sign = "";
  if (s.charAt(0) === "-") { sign = "-"; s = s.slice(1); }
  var parts = s.split(".");
  return sign + parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, ",") + (parts[1] ? "." + parts[1] : "");
}

function pad(n, w) {
  n = String(n);
  while (n.length < w) { n = "0" + n; }
  return n;
}

function dateFormat(v, spec) {
  if (!/yyyy|MM|dd|HH|mm|ss/.test(spec)) { return v; }
  var m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2})(?::(\d{2}))?)?/.exec(v);
  if (!m) { return v; }
  var parts = [["yyyy", m[1], 4], ["MM", m[2], 2], ["dd", m[3], 2],
               ["HH", m[4] || 0, 2], ["mm", m[5] || 0, 2], ["ss", m[6] || 0, 2]];
  return parts.reduce(function (acc, p) {
    return acc.split(p[0]).join(pad(parseInt(p[1], 10), p[2]));
  }, spec);
}

// aihtml_datagrid:format_value/3 for a raw text (the browser only
// formats what the user typed).
function format(v, c) {
  if (v === "") { return ""; }
  var spec = c.format;
  if (!spec) { return v; }
  var ch = spec.charAt(0);
  if ("nNcCpP".indexOf(ch) >= 0) {
    var d = parseInt(spec.slice(1), 10);
    if (isNaN(d) || d < 0) { d = 0; }
    var n = toNumber(v);
    if (n === null) { return dateFormat(v, spec); }
    if (ch === "n" || ch === "N") { return thousands(n, d); }
    if (ch === "c" || ch === "C") { return (c.currency || "") + thousands(n, d); }
    return (n * 100).toFixed(d) + "%";
  }
  return dateFormat(v, spec);
}

function display(s, c, v) {
  if (v === "") { return ""; }
  if (c.type === "bool") {
    return v === "true" ? s.labels.yes : (v === "false" ? s.labels.no : v);
  }
  if (c.type === "select") {
    for (var i = 0; i < c.options.length; i++) {
      if (String(c.options[i][0]) === v) { return String(c.options[i][1]); }
    }
    return v;
  }
  return format(v, c);
}

function aggregate(a, nums) {
  if (a === "count") { return nums.length; }
  if (!nums.length) { return null; }
  var sum = nums.reduce(function (x, y) { return x + y; }, 0);
  if (a === "sum") { return sum; }
  if (a === "avg") { return sum / nums.length; }
  return a === "min" ? Math.min.apply(null, nums) : Math.max.apply(null, nums);
}
function intOrFixed(v, d) { return Number.isInteger(v) ? String(v) : v.toFixed(d); }
function statusText(a, v) {
  if (v === null) { return ""; }
  if (a === "count") { return String(Math.trunc(v)); }
  if (a === "avg") { return v.toFixed(2); }
  return intOrFixed(v, 2);
}

function subst(p, n) { return String(p).split("{0}").join(String(n)); }
function json(s, d) {
  try { return s ? JSON.parse(s) : d; } catch (e) { return d; }
}

// ------------------------------------------------------------------
// DOM helpers
// ------------------------------------------------------------------

function kids(el, sel) {
  if (!el) { return []; }
  return Array.prototype.filter.call(el.children, function (c) { return c.matches(sel); });
}
function kid(el, sel) { return kids(el, sel)[0] || null; }
function qa(el, sel) { return el ? Array.from(el.querySelectorAll(sel)) : []; }
function parseOne(html) {
  var t = document.createElement("template");
  t.innerHTML = html;
  return t.content.firstElementChild;
}
function setStyle(node, props) { Object.assign(node.style, props); }

// ------------------------------------------------------------------
// State
// ------------------------------------------------------------------

function readCols(el, s) {
  s.cols = [];
  s.byField = Object.create(null);
  kids(el.querySelector(".ah-dg-header-row"), ".ah-dg-header-cell").forEach(function (h) {
    var f = h.getAttribute("data-field");
    var content = kid(h, ".ah-dg-header-cell-content");
    var c = {
      field: f, head: h, check: f === CHECK,
      title: content ? content.textContent : "",
      type: h.getAttribute("data-type") || "text",
      editable: h.getAttribute("data-editable") === "true",
      sortable: f !== CHECK && h.getAttribute("data-sortable") !== "false",
      groupable: f !== CHECK && h.getAttribute("data-groupable") !== "false",
      pinned: h.getAttribute("data-pinned") === "true" ||
        (f === CHECK && h.style.position === "sticky"),
      hidden: h.classList.contains("ah-dg-col-hidden"),
      width: parseInt(h.style.width, 10) || h.offsetWidth || 100,
      minWidth: parseInt(h.getAttribute("data-min-width") || "40", 10),
      format: h.getAttribute("data-format"),
      currency: h.getAttribute("data-currency") || "",
      options: json(h.getAttribute("data-options"), []),
      aggs: (h.getAttribute("data-aggs") || "").split(",").filter(Boolean)
    };
    s.cols.push(c);
    s.byField[f] = c;
  });
}

function readRecs(s) {
  var rows = kids(s.body, ".ah-dg-row");
  if (rows.length && rows[0].hasAttribute("data-i")) {
    rows.sort(function (a, b) {
      return parseInt(a.getAttribute("data-i"), 10) - parseInt(b.getAttribute("data-i"), 10);
    });
  }
  s.recs = rows.map(function (r) { return rec(r); });
}

function rec(r) { return { el: r, key: r.getAttribute("data-key"), cells: null }; }

function cellOf(r, f) {
  if (!r.cells) {
    r.cells = Object.create(null);
    kids(r.el, ".ah-dg-cell").forEach(function (c) {
      r.cells[c.getAttribute("data-field")] = c;
    });
  }
  return r.cells[f] || null;
}

function visibleCols(s) { return s.cols.filter(function (c) { return !c.hidden; }); }
function dataCols(s) { return s.cols.filter(function (c) { return !c.check; }); }

function writeState(el, s) {
  el.setAttribute("data-sort", JSON.stringify(s.sort));
  el.setAttribute("data-filter", JSON.stringify(s.filters));
  el.setAttribute("data-page", String(s.page));
  el.setAttribute("data-page-size", String(s.pageSize));
  el.setAttribute("data-group-by", JSON.stringify(s.groupBy));
}

// Fire a component event with its details as data-* of the root, so
// a postback bound on the root receives them in Event.data.
function fire(el, s, name, detail) {
  detail = detail || {};
  DETAIL.forEach(function (k) {
    if (detail[k] !== undefined && detail[k] !== null) { el.setAttribute("data-" + k, String(detail[k])); }
  });
  try {
    s.ctl.fire(name, detail);
  } finally {
    DETAIL.forEach(function (k) { el.removeAttribute("data-" + k); });
  }
}

// ------------------------------------------------------------------
// The view
// ------------------------------------------------------------------

function view(el, s) {
  writeState(el, s);
  paintHeader(el, s);
  if (s.remote) {
    query(el, s, null);
  } else {
    viewLocal(el, s);
  }
}

function matches(s, r, filters, q) {
  for (var i = 0; i < filters.length; i++) {
    var c = cellOf(r, filters[i][0]);
    if (!c) { continue; }
    var n = filters[i][1];
    if (raw(c).toLowerCase().indexOf(n) < 0 && text(c).toLowerCase().indexOf(n) < 0) { return false; }
  }
  if (!q) { return true; }
  return dataCols(s).some(function (col) {
    var c = cellOf(r, col.field);
    return c && (raw(c).toLowerCase().indexOf(q) >= 0 || text(c).toLowerCase().indexOf(q) >= 0);
  });
}

function sortRecs(s, list) {
  if (!s.sort.length) { return list.slice(); }
  var keyed = list.map(function (r, i) {
    return { r: r, i: i, k: s.sort.map(function (sd) { return raw(cellOf(r, sd[0])); }) };
  });
  keyed.sort(function (a, b) {
    for (var j = 0; j < s.sort.length; j++) {
      var x = compare(a.k[j], b.k[j]);
      if (x) { return s.sort[j][1] === "desc" ? -x : x; }
    }
    return a.i - b.i;
  });
  return keyed.map(function (x) { return x.r; });
}

function groupRecs(s, list, fields, level, path, out) {
  if (!fields.length) {
    list.forEach(function (r) { out.push({ row: r }); });
    return;
  }
  var f = fields[0], keys = [], by = Object.create(null);
  list.forEach(function (r) {
    var k = raw(cellOf(r, f));
    if (!(k in by)) { by[k] = []; keys.push(k); }
    by[k].push(r);
  });
  keys.sort(compare);
  keys.forEach(function (k) {
    var p = path.concat([f + ":" + k]);
    var id = p.join("|");
    var members = by[k];
    var collapsed = !!s.collapsed[id];
    out.push({ group: { id: id, level: level, title: text(cellOf(members[0], f)),
                        count: members.length, recs: members, collapsed: collapsed } });
    if (!collapsed) { groupRecs(s, members, fields.slice(1), level + 1, p, out); }
  });
}

function numbers(list, f) {
  var out = [];
  list.forEach(function (r) {
    var n = toNumber(raw(cellOf(r, f)));
    if (n !== null) { out.push(n); }
  });
  return out;
}

function groupEl(s, g) {
  var aggs = [];
  visibleCols(s).forEach(function (c) {
    if (!c.aggs.length) { return; }
    var nums = numbers(g.recs, c.field);
    var parts = [];
    c.aggs.forEach(function (a) {
      var v = aggregate(a, nums);
      if (v !== null) { parts.push(a + "=" + intOrFixed(v, 1)); }
    });
    if (parts.length) { aggs.push({ label: c.title, text: parts.join(", ") }); }
  });
  return parseOne(AH.tpl.datagrid_group_row({
    id: g.id, level: g.level, aria_level: g.level + 1,
    expanded: g.collapsed ? "false" : "true", open: !g.collapsed,
    indent: g.level * 20, colspan: visibleCols(s).length,
    title: g.title, count: g.count, has_aggs: aggs.length > 0, aggs: aggs
  }));
}

function statusbar(el, s, list) {
  var bar = el.querySelector(".ah-dg-statusbar");
  if (!bar) { return; }
  s.cols.forEach(function (c) {
    if (!c.aggs.length) { return; }
    var nums = numbers(list, c.field);
    qa(bar, ".ah-dg-statusbar-cell").filter(function (cell) {
      return cell.getAttribute("data-field") === c.field;
    }).forEach(function (cell) {
      qa(cell, ".ah-dg-statusbar-item").forEach(function (item) {
        var a = item.getAttribute("data-agg");
        kids(item, ".ah-dg-statusbar-value").forEach(function (v) {
          v.textContent = statusText(a, aggregate(a, nums));
        });
      });
    });
  });
}

function isOff(node) { return !!node.closest(".ah-dg-row-off"); }

function viewLocal(el, s) {
  var active = document.activeElement;
  var hadFocus = !!active && active !== s.body && s.body.contains(active);
  var activeGroupRow = hadFocus ? active.closest(".ah-dg-group-row") : null;
  var activeGroup = activeGroupRow ? activeGroupRow.getAttribute("data-group-id") : null;
  var filters = Object.keys(s.filters).filter(function (f) {
    return s.filters[f] !== "" && s.byField[f];
  }).map(function (f) { return [f, String(s.filters[f]).toLowerCase()]; });
  var q = s.search.toLowerCase();
  var kept = [];
  s.recs.forEach(function (r) { if (matches(s, r, filters, q)) { kept.push(r); } });
  statusbar(el, s, kept);
  var sorted = sortRecs(s, kept);
  s.lastView = sorted;
  var flat = [];
  if (s.groupBy.length) { groupRecs(s, sorted, s.groupBy, 0, [], flat); }
  else { sorted.forEach(function (r) { flat.push({ row: r }); }); }
  var total = flat.length;
  var pages = Math.max(1, Math.ceil(total / s.pageSize));
  s.page = s.pageable ? Math.min(Math.max(1, s.page), pages) : 1;
  var lo = s.pageable ? (s.page - 1) * s.pageSize : 0;
  var hi = s.pageable ? lo + s.pageSize : total;
  kids(s.body, ".ah-dg-group-row").forEach(function (g) { g.remove(); });
  var frag = document.createDocumentFragment();
  var stamp = ++seq;
  var focusGroup = null;
  flat.forEach(function (it, i) {
    var on = i >= lo && i < hi;
    if (it.group) {
      if (on) {
        var g = groupEl(s, it.group);
        if (activeGroup === it.group.id) { focusGroup = g; }
        frag.appendChild(g);
      }
      return;
    }
    it.row.stamp = stamp;
    it.row.el.classList.toggle("ah-dg-row-off", !on);
    frag.appendChild(it.row.el);
  });
  s.recs.forEach(function (r) {
    if (r.stamp !== stamp) {
      r.el.classList.add("ah-dg-row-off");
      frag.appendChild(r.el);
    }
  });
  s.body.insertBefore(frag, s.empty);
  s.total = total;
  paintRows(el, s, lo);
  renderPager(el, s);
  paintSelection(el, s);
  if (hadFocus) {
    if (focusGroup) { activate(el, s, kid(focusGroup, ".ah-dg-group-title"), true); }
    else if (active.isConnected && !isOff(active)) { active.focus(); }
    else { resetActive(el, s, true); }
  } else {
    resetActive(el, s, false);
  }
  el.setAttribute("aria-rowcount", String(total + s.headerRows));
}

// Stripes and aria-rowindex of the rows on the page; the empty message.
function paintRows(el, s, offset) {
  var idx = 0, ri = s.headerRows + offset + 1;
  kids(s.body, ".ah-dg-row, .ah-dg-group-row").forEach(function (r) {
    if (r.classList.contains("ah-dg-row-off")) { r.removeAttribute("aria-rowindex"); return; }
    if (r.classList.contains("ah-dg-row")) {
      r.classList.toggle("ah-dg-row-even", idx % 2 === 0);
      r.classList.toggle("ah-dg-row-odd", idx % 2 === 1);
      r.setAttribute("aria-rowindex", String(ri++));
    }
    idx++;
  });
  var empty = idx === 0;
  s.body.classList.toggle("ah-dg-body-empty", empty);
  if (s.empty) { s.empty.hidden = !empty; }
}

function pagerView(s) {
  var L = s.labels, total = s.total || 0, ps = s.pageSize;
  var pages = Math.ceil(total / ps);
  var page = Math.max(1, Math.min(s.page, Math.max(1, pages)));
  var end = Math.min(pages, Math.max(1, page - Math.floor(PAGER_BUTTONS / 2)) + PAGER_BUTTONS - 1);
  var start = Math.max(1, end - PAGER_BUTTONS + 1);
  var list = [];
  for (var p = start; pages > 0 && p <= end; p++) { list.push({ page: p, active: p === page }); }
  var sizes = s.pageSizes.slice();
  if (sizes.indexOf(ps) < 0) { sizes.push(ps); }
  sizes.sort(function (a, b) { return a - b; });
  return {
    label: L.pages, info: subst(L.total, total),
    prev: Math.max(1, page - 1), next: Math.min(Math.max(1, pages), page + 1), last: Math.max(1, pages),
    at_start: page <= 1, at_end: page >= pages,
    first_label: L.first_page, prev_label: L.prev_page, next_label: L.next_page, last_label: L.last_page,
    pages: list, size_label: L.page_size,
    sizes: sizes.map(function (n) { return { size: n, text: subst(L.per_page, n), selected: n === ps }; })
  };
}

// The grid's href with the view filled in but {page}; mirrors
// aihtml_datagrid:link_template/4.
function linkTemplate(s) {
  var sort = s.sort.map(function (sd) { return encodeURIComponent(sd[0]) + ":" + sd[1]; }).join(",");
  return s.href.replace(/\{size\}/g, String(s.pageSize)).replace(/\{sort\}/g, sort)
    .replace(/\{search\}/g, encodeURIComponent(s.search || ""));
}

function withLinks(v, t) {
  var url = function (skip, p) { return skip ? "" : t.replace(/\{page\}/g, String(p)); };
  v.link = true;
  v.first_href = url(v.at_start, 1);
  v.prev_href = url(v.at_start, v.prev);
  v.next_href = url(v.at_end, v.next);
  v.last_href = url(v.at_end, v.last);
  v.pages.forEach(function (p) { p.link = true; p.href = url(false, p.page); });
  return v;
}

// A plain left click follows a pager link in the page; any other (a new
// tab, a download) is the browser's.
function plainClick(e) {
  return e.button === 0 && !e.ctrlKey && !e.metaKey && !e.shiftKey && !e.altKey;
}

function renderPager(el, s) {
  if (!s.pageable || !s.pagerWrap) { return; }
  var focused = document.activeElement;
  var refocus = focused && focused !== s.pagerWrap && s.pagerWrap.contains(focused) ?
    (focused.getAttribute("data-page") ? "[aria-current=page]" : "select") : null;
  var v = pagerView(s);
  s.pagerWrap.innerHTML = AH.tpl.datagrid_pager(s.href ? withLinks(v, linkTemplate(s)) : v);
  if (refocus) {
    var again = s.pagerWrap.querySelector(refocus);
    if (again) { again.focus(); }
  }
}

function paintHeader(el, s) {
  s.cols.forEach(function (c) {
    if (c.check || !c.sortable) { return; }
    var i = -1;
    s.sort.forEach(function (sd, j) { if (sd[0] === c.field) { i = j; } });
    var dir = i >= 0 ? s.sort[i][1] : null;
    c.head.classList.toggle("ah-dg-header-cell-sorted", !!dir);
    c.head.setAttribute("aria-sort", dir === "asc" ? "ascending" : (dir === "desc" ? "descending" : "none"));
    var icon = kid(c.head, ".ah-dg-header-sort-icon");
    if (!icon) { return; }
    icon.textContent = dir === "asc" ? "▲" : (dir === "desc" ? "▼" : "");
    if (dir && s.sort.length > 1) {
      var badge = document.createElement("span");
      badge.className = "ah-dg-header-sort-badge";
      badge.textContent = String(i + 1);
      icon.appendChild(badge);
    }
  });
  if (s.filterRow) {
    kids(s.filterRow, ".ah-dg-filter-cell").forEach(function (cell) {
      var f = cell.getAttribute("data-field");
      cell.classList.toggle("ah-dg-filter-cell-active", !!s.filters[f]);
      var input = kid(cell, ".ah-dg-filter-input");
      if (input && input !== document.activeElement && input.value !== (s.filters[f] || "")) {
        input.value = s.filters[f] || "";
      }
    });
  }
}

// ------------------------------------------------------------------
// Remote mode
// ------------------------------------------------------------------

function query(el, s, exportFormat) {
  var q = s.q;
  if (!q) { return; }
  q.setAttribute("data-render", el.getAttribute("data-render") || "");
  q.setAttribute("data-sort", JSON.stringify(s.sort));
  q.setAttribute("data-filter", JSON.stringify(s.filters));
  q.setAttribute("data-search", s.search);
  q.setAttribute("data-page", String(s.page));
  q.setAttribute("data-page-size", String(s.pageSize));
  q.setAttribute("data-header-rows", String(s.headerRows));
  if (exportFormat) {
    q.setAttribute("data-export", exportFormat);
  } else {
    loading(el, s, true);
  }
  try {
    s.ctl.fire("ah:query", null, q);
  } finally {
    q.removeAttribute("data-export");
  }
}

function loading(el, s, on) {
  kids(el, ".ah-dg-loading-overlay").forEach(function (o) { o.style.display = on ? "flex" : "none"; });
  el.setAttribute("aria-busy", on ? "true" : "false");
  s.busy = on;
}

// After the server morphed rows in (rowsLoaded, rowUpdated).
function adoptRows(el, s) {
  if (s.remote) {
    s.recs = kids(s.body, ".ah-dg-row").map(rec);
  } else {
    var byKey = Object.create(null);
    kids(s.body, ".ah-dg-row").forEach(function (r) { byKey[r.getAttribute("data-key")] = r; });
    s.recs.forEach(function (r) {
      if (byKey[r.key] && byKey[r.key] !== r.el) { r.el = byKey[r.key]; }
      r.cells = null;
    });
  }
  s.empty = kid(s.body, ".ah-dg-empty-message");
  layout(el, s, s.body);
}

// ------------------------------------------------------------------
// Columns: widths, hidden, pinned
// ------------------------------------------------------------------

var CELLS = ".ah-dg-header-cell, .ah-dg-filter-cell, .ah-dg-cell, .ah-dg-statusbar-cell";

function layout(el, s, scope) {
  var left = 0, lastPinned = null, offsets = {};
  visibleCols(s).forEach(function (c) {
    if (c.pinned) { offsets[c.field] = left; left += c.width; lastPinned = c.field; }
  });
  qa(scope || el, CELLS).forEach(function (cell) {
    var c = s.byField[cell.getAttribute("data-field")];
    if (!c || cell.closest(".ah-dg") !== el) { return; }
    cell.style.width = c.width + "px";
    cell.classList.toggle("ah-dg-col-hidden", c.hidden);
    cell.classList.toggle("ah-dg-cell-pinned-last", c.field === lastPinned && !c.hidden);
    if (c.pinned && !c.hidden) {
      setStyle(cell, { position: "sticky", left: offsets[c.field] + "px", zIndex: "2" });
    } else if (cell.style.position === "sticky") {
      setStyle(cell, { position: "", left: "", zIndex: "" });
    }
  });
  el.setAttribute("aria-colcount", String(visibleCols(s).length));
}

function setHidden(el, s, c, hidden) {
  if (hidden && visibleCols(s).length <= 1) { return; }
  c.hidden = hidden;
  layout(el, s);
  if (!s.remote && s.groupBy.length) { viewLocal(el, s); }
  if (s.active && s.active.classList.contains("ah-dg-col-hidden")) { resetActive(el, s, false); }
}

// ------------------------------------------------------------------
// Selection
// ------------------------------------------------------------------

function pageRows(s) {
  return kids(s.body, ".ah-dg-row").filter(function (r) { return !r.classList.contains("ah-dg-row-off"); });
}

function paintSelection(el, s) {
  var set = Object.create(null);
  s.sel.forEach(function (k) { set[k] = true; });
  kids(s.body, ".ah-dg-row").forEach(function (r) {
    var on = !!set[r.getAttribute("data-key")];
    r.classList.toggle("ah-dg-row-selected", on);
    if (s.mode !== "none") { r.setAttribute("aria-selected", on ? "true" : "false"); }
    var cb = kid(kid(r, ".ah-dg-cell-checkbox"), "input");
    if (cb) { cb.checked = on; }
  });
  var all = el.querySelector(".ah-dg-select-all");
  if (all) {
    var rows = pageRows(s);
    var n = rows.filter(function (r) { return set[r.getAttribute("data-key")]; }).length;
    all.checked = rows.length > 0 && n === rows.length;
    all.indeterminate = n > 0 && n < rows.length;
  }
}

function writeValue(el, s, user) {
  var before = el.getAttribute("data-ah-value") || "";
  var v = AH.lib.values.join(s.sel);
  el.setAttribute("data-ah-value", v);
  kids(el, "input[type=hidden][data-ah-input]").forEach(function (i) { i.value = v; });
  paintSelection(el, s);
  if (user && v !== before) { s.ctl.fire("change"); }
}

function toggleKey(s, key) {
  var i = s.sel.indexOf(key);
  if (i >= 0) { s.sel.splice(i, 1); } else { s.sel.push(key); }
}

function selectRow(el, s, row, e, toggle) {
  if (s.mode === "none" || !row) { return; }
  var key = row.getAttribute("data-key");
  if (s.mode === "single") {
    s.sel = [key];
  } else if (s.mode === "checkbox" || toggle || (e && (e.ctrlKey || e.metaKey))) {
    toggleKey(s, key);
  } else if (e && e.shiftKey && s.anchor !== null) {
    var keys = pageRows(s).map(function (r) { return r.getAttribute("data-key"); });
    var a = keys.indexOf(s.anchor), b = keys.indexOf(key);
    if (a < 0) { s.sel = [key]; } else { s.sel = keys.slice(Math.min(a, b), Math.max(a, b) + 1); }
  } else {
    s.sel = [key];
  }
  if (!(e && e.shiftKey)) { s.anchor = key; }
  writeValue(el, s, true);
}

function selectAll(el, s, on) {
  var keys = pageRows(s).map(function (r) { return r.getAttribute("data-key"); });
  if (on === undefined) {
    on = !keys.every(function (k) { return s.sel.indexOf(k) >= 0; });
  }
  keys.forEach(function (k) {
    var i = s.sel.indexOf(k);
    if (on && i < 0) { s.sel.push(k); }
    if (!on && i >= 0) { s.sel.splice(i, 1); }
  });
  writeValue(el, s, true);
}

// ------------------------------------------------------------------
// Keyboard: an active cell with the only tabindex="0" (roving)
// ------------------------------------------------------------------

function navRows(s) {
  var rows = [s.headerRow];
  kids(s.body, ".ah-dg-row, .ah-dg-group-row").forEach(function (r) {
    if (!r.classList.contains("ah-dg-row-off")) { rows.push(r); }
  });
  return rows;
}

function cellAt(s, row, field) {
  if (!row) { return null; }
  if (row.classList.contains("ah-dg-group-row")) { return kid(row, ".ah-dg-group-title"); }
  var sel = row === s.headerRow ? ".ah-dg-header-cell" : ".ah-dg-cell";
  return kids(row, sel).filter(function (c) { return c.getAttribute("data-field") === field; })[0] || null;
}

function activate(el, s, cell, focus) {
  if (!cell) { return; }
  if (s.active && s.active !== cell) {
    s.active.removeAttribute("tabindex");
    s.active.classList.remove("ah-dg-cell-focused");
  }
  s.active = cell;
  cell.setAttribute("tabindex", "0");
  var f = cell.getAttribute("data-field");
  if (f) { s.activeField = f; }
  if (focus) {
    cell.classList.add("ah-dg-cell-focused");
    cell.focus({ preventScroll: false });
  }
}

// Keep a tab stop: the active cell, else the header cell of its column.
function resetActive(el, s, focus) {
  var a = s.active;
  if (a && a.isConnected && !isOff(a) && !a.classList.contains("ah-dg-col-hidden")) {
    if (focus) { activate(el, s, a, true); }
    return;
  }
  var cols = visibleCols(s);
  var c = s.byField[s.activeField];
  if (!c || c.hidden) { c = cols[0]; }
  if (c) { activate(el, s, c.head, focus); }
}

function position(s, cell) {
  var row = cell.closest(".ah-dg-row, .ah-dg-group-row, .ah-dg-header-row");
  var rows = navRows(s);
  return { rows: rows, r: rows.indexOf(row), row: row };
}

function move(el, s, cell, dr, dc, e) {
  var p = position(s, cell);
  var cols = visibleCols(s);
  var f = cell.getAttribute("data-field") || s.activeField;
  var ci = cols.map(function (c) { return c.field; }).indexOf(f);
  if (ci < 0) { ci = 0; }
  var r = p.r, key = e.key;
  if (key === "Home" && !(e.ctrlKey || e.metaKey)) { ci = 0; }
  else if (key === "End" && !(e.ctrlKey || e.metaKey)) { ci = cols.length - 1; }
  else if (key === "Home") { r = 0; }
  else if (key === "End") { r = p.rows.length - 1; }
  else { r += dr; ci += dc; }
  r = Math.max(0, Math.min(p.rows.length - 1, r));
  ci = Math.max(0, Math.min(cols.length - 1, ci));
  activate(el, s, cellAt(s, p.rows[r], cols[ci].field), true);
}

function keydown(el, s, e) {
  if (s.editing) { return; }
  var cell = e.target;
  if (!(cell instanceof Element) ||
      !cell.matches(".ah-dg-cell, .ah-dg-header-cell, .ah-dg-group-title") ||
      cell.closest(".ah-dg") !== el) { return; }
  var header = cell.classList.contains("ah-dg-header-cell");
  var group = cell.closest(".ah-dg-group-row");
  var row = cell.closest(".ah-dg-row");
  var field = cell.getAttribute("data-field");
  var c = field ? s.byField[field] : null;
  switch (e.key) {
    case "ArrowUp": move(el, s, cell, -1, 0, e); break;
    case "ArrowDown":
      if (header && e.altKey) { openMenu(el, s, field); break; }
      move(el, s, cell, 1, 0, e);
      break;
    case "ArrowLeft":
      if (group) { toggleGroup(el, s, group, false); break; }
      move(el, s, cell, 0, -1, e);
      break;
    case "ArrowRight":
      if (group) { toggleGroup(el, s, group, true); break; }
      move(el, s, cell, 0, 1, e);
      break;
    case "Home": case "End": move(el, s, cell, 0, 0, e); break;
    case "PageUp": case "PageDown":
      if (s.pageable) {
        goToPage(el, s, s.page + (e.key === "PageDown" ? 1 : -1));
        var rows = navRows(s);
        activate(el, s, cellAt(s, rows[Math.min(1, rows.length - 1)], s.activeField), true);
      } else {
        move(el, s, cell, e.key === "PageDown" ? 10 : -10, 0, e);
      }
      break;
    case "Enter":
      if (header) { if (c && c.check) { selectAll(el, s); } else { headerSort(el, s, c, e.shiftKey); } break; }
      if (group) { toggleGroup(el, s, group); break; }
      if (c && c.editable) { beginEdit(el, s, cell); break; }
      selectRow(el, s, row, null, s.mode === "multi" || s.mode === "checkbox");
      break;
    case "F2":
      if (c && c.editable && row) { beginEdit(el, s, cell); }
      break;
    case " ":
      if (header) { if (c && c.check) { selectAll(el, s); } break; }
      if (group) { toggleGroup(el, s, group); break; }
      selectRow(el, s, row, null, s.mode === "multi" || s.mode === "checkbox");
      break;
    case "ContextMenu":
      if (header && field !== CHECK) { openMenu(el, s, field); break; }
      return;
    case "a": case "A":
      if ((e.ctrlKey || e.metaKey) && (s.mode === "multi" || s.mode === "checkbox")) { selectAll(el, s, true); break; }
      return;
    default:
      return;
  }
  e.preventDefault();
}

// ------------------------------------------------------------------
// Sorting, filtering, paging, grouping
// ------------------------------------------------------------------

function headerSort(el, s, c, multi) {
  if (!c || !c.sortable) { return; }
  var i = -1;
  s.sort.forEach(function (sd, j) { if (sd[0] === c.field) { i = j; } });
  var cur = i >= 0 ? s.sort[i][1] : null;
  var next = cur === null ? "asc" : (cur === "asc" ? "desc" : null);
  if (multi) {
    if (i >= 0) {
      if (next) { s.sort[i] = [c.field, next]; } else { s.sort.splice(i, 1); }
    } else {
      s.sort.push([c.field, "asc"]);
    }
  } else {
    s.sort = next ? [[c.field, next]] : [];
  }
  view(el, s);
  fire(el, s, "ah:sort", { field: c.field, value: next || "" });
}

function setSort(el, s, field, dir) {
  s.sort = dir ? [[field, dir]] : s.sort.filter(function (sd) { return sd[0] !== field; });
  view(el, s);
  fire(el, s, "ah:sort", { field: field, value: dir || "" });
}

function setFilter(el, s, field, v) {
  if (v) { s.filters[field] = v; } else { delete s.filters[field]; }
  s.page = 1;
  view(el, s);
  fire(el, s, "ah:filter", { field: field, value: v });
}

function goToPage(el, s, p) {
  var pages = Math.max(1, Math.ceil((s.total || 0) / s.pageSize));
  p = Math.max(1, Math.min(pages, p));
  if (p === s.page) { return; }
  s.page = p;
  view(el, s);
  fire(el, s, "ah:page", { value: p });
}

function toggleGroup(el, s, groupRow, open) {
  var id = groupRow.getAttribute("data-group-id");
  var collapsed = !!s.collapsed[id];
  if (open === true && !collapsed) { return; }
  if (open === false && collapsed) { return; }
  if (collapsed) { delete s.collapsed[id]; } else { s.collapsed[id] = true; }
  viewLocal(el, s);
  fire(el, s, "ah:group-toggle", { value: id, expanded: collapsed ? "true" : "false" });
}

function setGroupBy(el, s, fields) {
  s.groupBy = fields.filter(function (f) { return s.byField[f] && !s.byField[f].check; });
  s.collapsed = Object.create(null);
  s.page = 1;
  view(el, s);
}

// ------------------------------------------------------------------
// Column resize
// ------------------------------------------------------------------

function startResize(el, s, e, handle) {
  var head = handle.closest(".ah-dg-header-cell");
  var c = head && s.byField[head.getAttribute("data-field")];
  if (!c) { return; }
  e.preventDefault();
  e.stopPropagation();
  var line = kid(el, ".ah-dg-resize-line");
  var rootLeft = el.getBoundingClientRect().left;
  var startX = e.clientX, startW = head.getBoundingClientRect().width, w = startW;
  var edge = head.getBoundingClientRect().right - rootLeft;
  if (line) { setStyle(line, { display: "block", left: edge + "px", top: "0", height: el.offsetHeight + "px" }); }
  if (s.resizeOff) { s.resizeOff.abort(); }
  var off = s.resizeOff = new AbortController();
  var opts = { signal: off.signal };
  document.addEventListener("pointermove", function (me) {
    w = Math.max(c.minWidth, startW + me.clientX - startX);
    if (line) { line.style.left = (edge + w - startW) + "px"; }
  }, opts);
  var end = function () {
    off.abort();
    s.resizeOff = null;
    if (line) { line.style.display = "none"; }
    s.resized = Date.now();
    w = Math.round(w);
    if (w !== c.width) {
      c.width = w;
      layout(el, s);
      fire(el, s, "ah:column-resize", { field: c.field, value: w });
    }
  };
  document.addEventListener("pointerup", end, opts);
  document.addEventListener("pointercancel", end, opts);
}

// ------------------------------------------------------------------
// Column menu
// ------------------------------------------------------------------

function menuItems(s, c) {
  var L = s.labels, items = [];
  function item(action, label, opts) {
    opts = opts || {};
    return { action: action, field: opts.field || c.field, label: label,
             role: opts.role || "menuitem", checkable: opts.checked !== undefined,
             checked: opts.checked ? "true" : "false", mark: opts.checked ? "✓" : "" };
  }
  if (c.sortable) {
    var dir = null;
    s.sort.forEach(function (sd) { if (sd[0] === c.field) { dir = sd[1]; } });
    items.push(item("sort-asc", L.sort_asc, { role: "menuitemradio", checked: dir === "asc" }));
    items.push(item("sort-desc", L.sort_desc, { role: "menuitemradio", checked: dir === "desc" }));
    if (dir) { items.push(item("sort-clear", L.sort_clear)); }
    items.push({ sep: true });
  }
  items.push(item(c.pinned ? "unpin" : "pin", c.pinned ? L.unpin : L.pin));
  if (visibleCols(s).length > 1) { items.push(item("hide", L.hide_column)); }
  if (!s.remote && c.groupable) {
    items.push({ sep: true });
    var grouped = s.groupBy.indexOf(c.field) >= 0;
    items.push(item(grouped ? "ungroup" : "group", grouped ? L.ungroup : L.group_by));
    if (s.groupBy.length) { items.push(item("clear-groups", L.clear_groups)); }
  }
  items.push({ sep: true });
  items.push({ heading: L.columns });
  dataCols(s).forEach(function (col) {
    items.push(item("toggle-column", col.title, { field: col.field, role: "menuitemcheckbox",
                                                  checked: !col.hidden }));
  });
  return items;
}

function menuEntries(s) { return kids(s.menu, ".ah-dg-column-menu-item"); }

function openMenu(el, s, field, focusIndex) {
  var c = s.byField[field];
  if (!c || c.check || !s.menu) { return; }
  closeMenu(el, s, false);
  s.menu.innerHTML = AH.tpl.datagrid_column_menu({ items: menuItems(s, c) });
  s.menu.setAttribute("aria-label", s.labels.column_menu + ": " + c.title);
  s.menu.classList.add("ah-dg-column-menu-open");
  s.menuField = field;
  // the header cell: its ⋮ button is only displayed while hovered
  s.menuFloat = AH.float(s.menu, c.head, { placement: "bottom", align: "end", offset: 2 });
  var items = menuEntries(s);
  var first = items[Math.min(focusIndex || 0, items.length - 1)];
  if (first) { first.focus(); }
  var off = s.menuOff = new AbortController();
  document.addEventListener("mousedown", function (e) {
    if (!s.menu.contains(e.target) && !(e.target.closest && e.target.closest(".ah-dg-column-menu-btn"))) {
      closeMenu(el, s, false);
    }
  }, { signal: off.signal });
}

function closeMenu(el, s, refocus) {
  if (!s.menu || !s.menu.classList.contains("ah-dg-column-menu-open")) { return; }
  if (s.menuOff) { s.menuOff.abort(); s.menuOff = null; }
  if (s.menuFloat) { s.menuFloat.stop(); s.menuFloat = null; }
  s.menu.classList.remove("ah-dg-column-menu-open");
  setStyle(s.menu, { position: "", left: "", top: "" });
  s.menu.innerHTML = "";
  var c = s.byField[s.menuField];
  s.menuField = null;
  if (refocus && c) { activate(el, s, c.head, true); }
}

function menuAction(el, s, item) {
  var action = item.getAttribute("data-action");
  var field = item.getAttribute("data-field");
  var c = s.byField[field];
  if (!c) { return; }
  var menuField = s.menuField;
  switch (action) {
    case "sort-asc": setSort(el, s, field, "asc"); break;
    case "sort-desc": setSort(el, s, field, "desc"); break;
    case "sort-clear": setSort(el, s, field, null); break;
    case "pin": case "unpin": c.pinned = action === "pin"; layout(el, s); break;
    case "hide": setHidden(el, s, c, true); break;
    case "group": setGroupBy(el, s, s.groupBy.concat([field])); break;
    case "ungroup": setGroupBy(el, s, s.groupBy.filter(function (f) { return f !== field; })); break;
    case "clear-groups": setGroupBy(el, s, []); break;
    case "toggle-column":
      // stays open: several columns are usually toggled in a row
      var index = menuEntries(s).indexOf(item);
      setHidden(el, s, c, !c.hidden);
      openMenu(el, s, s.byField[menuField].hidden ? visibleCols(s)[0].field : menuField, index);
      return;
  }
  closeMenu(el, s, true);
}

function menuKey(el, s, e) {
  var items = menuEntries(s);
  var i = items.indexOf(document.activeElement);
  var n = items.length;
  switch (e.key) {
    case "ArrowDown": if (n) { items[(i + 1) % n].focus(); } break;
    case "ArrowUp": if (n) { items[(i - 1 + n) % n].focus(); } break;
    case "Home": if (n) { items[0].focus(); } break;
    case "End": if (n) { items[n - 1].focus(); } break;
    case "Enter": case " ": if (i >= 0) { menuAction(el, s, items[i]); } break;
    case "Escape": case "Tab": closeMenu(el, s, true); break;
    default: return;
  }
  e.preventDefault();
  e.stopPropagation();
}

// ------------------------------------------------------------------
// Editing
// ------------------------------------------------------------------

function setCellValue(s, cell, c, v) {
  var t = display(s, c, v);
  var span = kid(cell, ".ah-dg-cell-content");
  if (!span) {
    cell.textContent = "";
    span = document.createElement("span");
    span.className = "ah-dg-cell-content";
    cell.appendChild(span);
  }
  span.textContent = t;
  if (t !== v) { cell.setAttribute("data-v", v); } else { cell.removeAttribute("data-v"); }
}

function commit(el, s, cell, c, old, v) {
  var row = cell.closest(".ah-dg-row");
  setCellValue(s, cell, c, v);
  fire(el, s, "ah:edit", { key: row.getAttribute("data-key"), field: c.field, value: v, old: old });
  if (!s.remote) { viewLocal(el, s); }
}

function beginEdit(el, s, cell) {
  var c = s.byField[cell.getAttribute("data-field")];
  if (!c || !c.editable || s.editing || !cell.classList.contains("ah-dg-cell")) { return; }
  var old = raw(cell);
  activate(el, s, cell, false);
  if (c.type === "bool") {
    commit(el, s, cell, c, old, old === "true" ? "false" : "true");
    activate(el, s, cell, true);
    return;
  }
  var ed, kind;
  if (c.type === "select") {
    ed = document.createElement("select");
    kind = "select";
    c.options.forEach(function (o) {
      ed.add(new Option(String(o[1]), String(o[0]), false, String(o[0]) === old));
    });
  } else if (c.type === "textarea") {
    ed = document.createElement("textarea");
    kind = "textarea";
    ed.value = old;
  } else {
    ed = document.createElement("input");
    kind = c.type === "number" ? "number" : (c.type === "date" ? "date" : "text");
    ed.type = kind;
    ed.value = kind === "date" ? old.slice(0, 10) : old;
  }
  ed.className = "ah-dg-editor ah-dg-editor-" + kind;
  ed.setAttribute("aria-label", c.title);
  var content = document.createDocumentFragment();
  while (cell.firstChild) { content.appendChild(cell.firstChild); }
  var off = new AbortController();
  var editing = s.editing = { cell: cell, col: c, old: old, ed: ed, content: content, off: off };
  cell.classList.add("ah-dg-cell-editing");
  cell.appendChild(ed);
  ed.focus();
  if (kind === "text" || kind === "textarea") { ed.select(); }
  ed.addEventListener("blur", function () {
    if (s.editing === editing) { endEdit(el, s, true, false); }
  }, { signal: off.signal });
  ed.addEventListener("keydown", function (e) { editorKey(el, s, e); }, { signal: off.signal });
}

function endEdit(el, s, save, refocus) {
  var ed = s.editing;
  if (!ed) { return; }
  s.editing = null;
  var v = String(ed.ed.value);
  ed.off.abort();
  ed.ed.remove();
  ed.cell.appendChild(ed.content);
  ed.cell.classList.remove("ah-dg-cell-editing");
  if (save && v !== ed.old) { commit(el, s, ed.cell, ed.col, ed.old, v); }
  if (refocus) { activate(el, s, ed.cell, true); }
}

function nextEditable(s, cell, dir) {
  var cols = visibleCols(s);
  var i = cols.indexOf(s.byField[cell.getAttribute("data-field")]);
  for (i += dir; i >= 0 && i < cols.length; i += dir) {
    if (cols[i].editable) { return cellAt(s, cell.parentNode, cols[i].field); }
  }
  return null;
}

function editorKey(el, s, e) {
  var ed = s.editing;
  if (!ed) { return; }
  e.stopPropagation();
  if (e.key === "Enter" && (ed.col.type !== "textarea" || e.ctrlKey || e.metaKey)) {
    e.preventDefault();
    endEdit(el, s, true, true);
  } else if (e.key === "Escape") {
    e.preventDefault();
    endEdit(el, s, false, true);
  } else if (e.key === "Tab") {
    e.preventDefault();
    var cell = ed.cell;
    var next = nextEditable(s, cell, e.shiftKey ? -1 : 1);
    endEdit(el, s, true, !next);
    if (next && next.isConnected) { beginEdit(el, s, next); }
  }
}

// ------------------------------------------------------------------
// Export
// ------------------------------------------------------------------

function download(blob, name) {
  var url = URL.createObjectURL(blob);
  var a = document.createElement("a");
  a.href = url;
  a.download = name;
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(function () { URL.revokeObjectURL(url); }, 0);
}

function csvCell(v) {
  v = v === null || v === undefined ? "" : String(v);
  return /[",\n\r]/.test(v) ? '"' + v.replace(/"/g, '""') + '"' : v;
}

function writeFile(el, s, fmt, headers, rows) {
  var name = s.exportName || "data";
  if (fmt === "csv") {
    var lines = [headers].concat(rows).map(function (r) { return r.map(csvCell).join(","); });
    download(new Blob(["﻿" + lines.join("\n")], { type: "text/csv;charset=utf-8;" }), name + ".csv");
    return Promise.resolve();
  }
  if (fmt === "xlsx") {
    return import("xlsx").then(function (m) {
      var XLSX = m.utils ? m : m.default;
      var ws = XLSX.utils.aoa_to_sheet([headers].concat(rows));
      ws["!cols"] = headers.map(function (h, i) {
        var w = String(h).length;
        rows.forEach(function (r) { w = Math.max(w, String(r[i] === undefined ? "" : r[i]).length); });
        return { wch: w + 2 };
      });
      var wb = XLSX.utils.book_new();
      XLSX.utils.book_append_sheet(wb, ws, "Sheet1");
      var bytes = XLSX.write(wb, { bookType: "xlsx", type: "array" });
      download(new Blob([bytes], { type: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" }),
               name + ".xlsx");
    });
  }
  if (fmt === "pdf") {
    return Promise.all([import("jspdf"), import("jspdf-autotable")]).then(function (libs) {
      var JsPDF = libs[0].jsPDF || libs[0].default.jsPDF;
      var autoTable = libs[1].autoTable || libs[1].default;
      var doc = new JsPDF({ orientation: headers.length > 6 ? "landscape" : "portrait", format: "a4" });
      autoTable(doc, { head: [headers], body: rows, startY: 14, styles: { fontSize: 9 },
                       headStyles: { fillColor: [66, 139, 202] } });
      download(doc.output("blob"), name + ".pdf");
    });
  }
  return Promise.reject(new Error("aihtml: unknown export format " + fmt));
}

function exportLocal(el, s, fmt) {
  var cols = dataCols(s).filter(function (c) { return !c.hidden && c.type !== "command"; });
  var list = s.lastView || s.recs;
  var rows = list.map(function (r) {
    return cols.map(function (c) {
      var cell = cellOf(r, c.field);
      var t = text(cell);
      return t === "" ? raw(cell) : t;
    });
  });
  return writeFile(el, s, fmt, cols.map(function (c) { return c.title; }), rows);
}

function exportGrid(el, s, fmt) {
  if (s.remote) {
    query(el, s, fmt);
    return Promise.resolve();
  }
  return exportLocal(el, s, fmt);
}

// ------------------------------------------------------------------
// Controller
// ------------------------------------------------------------------

function debounce(s, key, ms, f) {
  clearTimeout(s.timers[key]);
  s.timers[key] = setTimeout(f, ms);
}

// The nearest element matching sel from the event target, inside root.
function hit(e, sel, root) {
  var t = e.target;
  var m = t instanceof Element ? t.closest(sel) : null;
  return m && root.contains(m) ? m : null;
}

AH.register("datagrid", class extends AH.Controller {
  setup() {
    var el = this.element;
    var s = this.s = {
      ctl: this,
      id: ++seq,
      remote: el.hasAttribute("data-ah-remote"),
      mode: el.getAttribute("data-ah-selection") || "single",
      editMode: el.getAttribute("data-ah-edit-mode") || "dblclick",
      pageable: el.hasAttribute("data-ah-pageable"),
      headerRows: parseInt(el.getAttribute("data-ah-header-rows") || "1", 10),
      labels: json(el.getAttribute("data-ah-labels"), {}),
      exportName: el.getAttribute("data-ah-export-name") || "data",
      href: el.getAttribute("data-ah-href"),
      sort: json(el.getAttribute("data-sort"), []),
      filters: json(el.getAttribute("data-filter"), {}),
      groupBy: json(el.getAttribute("data-group-by"), []),
      page: parseInt(el.getAttribute("data-page") || "1", 10) || 1,
      pageSize: parseInt(el.getAttribute("data-page-size") || "10", 10) || 10,
      search: "",
      collapsed: Object.create(null),
      sel: AH.lib.values.split(el.getAttribute("data-ah-value")).filter(Boolean),
      anchor: null, active: null, activeField: null, editing: null, timers: {},
      menuOff: null, resizeOff: null, menuFloat: null,
      body: el.querySelector(".ah-dg-body"),
      headerRow: el.querySelector(".ah-dg-header-row"),
      filterRow: el.querySelector(".ah-dg-header-filter-row"),
      pagerWrap: el.querySelector(".ah-dg-pager-wrap"),
      menu: kid(el, ".ah-dg-column-menu"),
      q: kid(el, ".ah-dg-query")
    };
    if (Array.isArray(s.filters) || typeof s.filters !== "object" || !s.filters) { s.filters = {}; }
    s.pageSizes = qa(el, ".ah-dg-pager-size-select option").map(function (o) {
      return parseInt(o.value, 10);
    });
    if (!s.pageSizes.length) { s.pageSizes = [10, 20, 50, 100]; }
    s.empty = kid(s.body, ".ah-dg-empty-message");
    readCols(el, s);
    readRecs(s);
    s.total = s.remote ? parseInt(el.getAttribute("aria-rowcount") || "0", 10) - s.headerRows : s.recs.length;
    el.removeAttribute("tabindex");

    // the body scrolls the header and the status bar along (scroll does
    // not bubble: captured on the root)
    this.listen(el, "scroll", function (e) {
      var wrap = el.querySelector(".ah-dg-body-wrap");
      if (e.target !== wrap) { return; }
      var x = wrap.scrollLeft;
      qa(el, ".ah-dg-header-wrap, .ah-dg-statusbar-wrap").forEach(function (w) { w.scrollLeft = x; });
    }, { capture: true });

    // header: resize
    this.delegate("pointerdown", ".ah-dg-resize-handle", function (e, h) { startResize(el, s, e, h); });

    // clicks, the innermost target first (as jQuery's delegated
    // handlers ran, with stopPropagation between them)
    this.listen(el, "click", function (e) {
      var m;
      if ((m = hit(e, ".ah-dg-column-menu-btn", el))) {
        e.preventDefault();
        e.stopPropagation();
        var f = m.getAttribute("data-field");
        if (s.menuField === f) { closeMenu(el, s, true); } else { openMenu(el, s, f); }
      } else if ((m = hit(e, ".ah-dg-header-cell", el))) {
        if (m.closest(".ah-dg") !== el) { return; }
        if (hit(e, ".ah-dg-resize-handle, .ah-dg-column-menu-btn, input", m)) { return; }
        if (s.resized && Date.now() - s.resized < 300) { return; }
        activate(el, s, m, true);
        headerSort(el, s, s.byField[m.getAttribute("data-field")], e.shiftKey);
      } else if ((m = hit(e, ".ah-dg-row-checkbox", el))) {
        e.stopPropagation();
        var row = m.closest(".ah-dg-row");
        var key = row.getAttribute("data-key");
        if ((s.sel.indexOf(key) >= 0) !== m.checked) { toggleKey(s, key); }
        s.anchor = key;
        writeValue(el, s, true);
      } else if ((m = hit(e, ".ah-dg-command-btn", el))) {
        e.stopPropagation();
        var crow = m.closest(".ah-dg-row");
        fire(el, s, "ah:command", { key: crow && crow.getAttribute("data-key"),
                                    field: m.getAttribute("data-field"),
                                    name: m.getAttribute("data-command") });
      } else if ((m = hit(e, ".ah-dg-row", el))) {
        if (m.closest(".ah-dg") !== el || s.editing) { return; }
        var cell = hit(e, ".ah-dg-cell", m);
        var field = cell ? cell.getAttribute("data-field") : null;
        if (cell && !hit(e, "a, button, input, select, textarea", m)) {
          activate(el, s, cell, true);
        }
        fire(el, s, "ah:row-click", { key: m.getAttribute("data-key"), field: field });
        if (!hit(e, "a", m)) { selectRow(el, s, m, e, false); }
        if (cell && s.editMode === "click" && cell.classList.contains("ah-dg-cell-editable")) {
          beginEdit(el, s, cell);
        }
      } else if ((m = hit(e, ".ah-dg-group-row", el))) {
        activate(el, s, kid(m, ".ah-dg-group-title"), true);
        toggleGroup(el, s, m);
      } else if ((m = hit(e, ".ah-dg-pager-button", el))) {
        if (m.disabled) { return; }
        var link = m.tagName === "A" ? m.getAttribute("href") : null;
        if (link !== null && !plainClick(e)) { return; }
        if (link !== null) { e.preventDefault(); }
        var before = s.page;
        goToPage(el, s, parseInt(m.getAttribute("data-page"), 10));
        if (link && s.page !== before) { AH.apply([{ op: "url", mode: "push", value: link }]); }
      } else if ((m = hit(e, ".ah-dg-toolbar-btn[data-export]", el))) {
        exportGrid(el, s, m.getAttribute("data-export"));
      } else if ((m = hit(e, ".ah-dg-toolbar-btn[data-name]", el))) {
        fire(el, s, "ah:toolbar", { name: m.getAttribute("data-name") });
      }
    });
    this.delegate("dblclick", ".ah-dg-row", function (e, row) {
      if (row.closest(".ah-dg") !== el || s.editing) { return; }
      var cell = hit(e, ".ah-dg-cell", row);
      fire(el, s, "ah:row-dblclick", { key: row.getAttribute("data-key"),
                                       field: cell ? cell.getAttribute("data-field") : null });
      if (cell && s.editMode === "dblclick" && cell.classList.contains("ah-dg-cell-editable")) {
        beginEdit(el, s, cell);
      }
    });

    // select all, pager size
    this.delegate("change", ".ah-dg-select-all", function (e, box) { selectAll(el, s, box.checked); });
    this.delegate("change", ".ah-dg-pager-size-select", function (e, sel) {
      s.pageSize = parseInt(sel.value, 10) || s.pageSize;
      s.page = 1;
      view(el, s);
      fire(el, s, "ah:page", { value: 1 });
    });

    // filter row, search box (debounced)
    this.delegate("input", ".ah-dg-filter-input", function (e, input) {
      var f = input.getAttribute("data-field");
      debounce(s, "f:" + f, s.remote ? 300 : 200, function () { setFilter(el, s, f, input.value); });
    });
    this.delegate("input", ".ah-dg-search-input", function (e, input) {
      debounce(s, "search", s.remote ? 300 : 200, function () {
        s.search = input.value;
        s.page = 1;
        view(el, s);
        fire(el, s, "ah:filter", { field: "", value: input.value });
      });
    });

    // keyboard, focus ring
    this.listen(el, "keydown", function (e) {
      if (s.menu && s.menu.contains(e.target)) { menuKey(el, s, e); return; }
      keydown(el, s, e);
    });
    this.delegate("focusin", ".ah-dg-cell, .ah-dg-header-cell, .ah-dg-group-title", function (e, cell) {
      if (cell === s.active) { cell.classList.add("ah-dg-cell-focused"); }
    });
    this.listen(el, "focusout", function (e) {
      var to = e.relatedTarget;
      if (s.active && !(to && to !== el && el.contains(to))) { s.active.classList.remove("ah-dg-cell-focused"); }
    });
    if (s.menu) {
      this.delegate("click", ".ah-dg-column-menu-item", function (e, item) {
        e.stopPropagation();
        menuAction(el, s, item);
      }, s.menu);
    }
    if (s.q) {
      this.listen(s.q, "ah:error", function () { loading(el, s, false); });
    }

    // inner controls report to the grid, not as the grid's own change /
    // input; registered last so the grid's own handlers above still run,
    // and stopImmediatePropagation also keeps them from listeners added
    // to the root later (as jQuery's delegated stopPropagation did)
    var inner = function (e) {
      var t = e.target;
      if (t !== el && t instanceof Element && t.matches("input, select, textarea")) {
        e.stopImmediatePropagation();
      }
    };
    this.listen(el, "change", inner);
    this.listen(el, "input", inner);

    layout(el, s);
    paintSelection(el, s);
    if (s.remote) {
      paintHeader(el, s);
      writeState(el, s);
      resetActive(el, s, false);
    } else {
      view(el, s);
    }
  }

  teardown() {
    var s = this.s;
    Object.keys(s.timers).forEach(function (k) { clearTimeout(s.timers[k]); });
    if (s.resizeOff) { s.resizeOff.abort(); s.resizeOff = null; }
    if (s.menuOff) { s.menuOff.abort(); s.menuOff = null; }
    if (s.menuFloat) { s.menuFloat.stop(); s.menuFloat = null; }
    if (s.editing) { s.editing.off.abort(); }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v) {
    var s = this.s;
    s.sel = AH.lib.values.split(Array.isArray(v) ? v : v == null ? "" : String(v)).filter(Boolean);
    writeValue(this.element, s, false);
  }
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  sort(field, dir) { setSort(this.element, this.s, String(field), dir || null); }
  filter(field, v) { setFilter(this.element, this.s, String(field), v ? String(v) : ""); }
  search(v) {
    var s = this.s;
    s.search = v ? String(v) : "";
    qa(this.element, ".ah-dg-search-input").forEach(function (i) { i.value = s.search; });
    s.page = 1;
    view(this.element, s);
  }
  goToPage(p) { goToPage(this.element, this.s, parseInt(p, 10) || 1); }
  groupBy(fields) {
    if (!this.s.remote) { setGroupBy(this.element, this.s, (fields || []).map(String)); }
  }
  showColumn(f) { var s = this.s; if (s.byField[f]) { setHidden(this.element, s, s.byField[f], false); } }
  hideColumn(f) { var s = this.s; if (s.byField[f]) { setHidden(this.element, s, s.byField[f], true); } }
  pinColumn(f, pinned) {
    var s = this.s;
    if (s.byField[f]) { s.byField[f].pinned = pinned !== false; layout(this.element, s); }
  }
  setColumnWidth(f, w) {
    var s = this.s, c = s.byField[f];
    if (c) { c.width = Math.max(c.minWidth, parseInt(w, 10) || c.width); layout(this.element, s); }
  }
  exportData(fmt, headers, rows) {
    var s = this.s;
    return headers ? writeFile(this.element, s, String(fmt), headers, rows || [])
      : exportGrid(this.element, s, String(fmt));
  }
  refresh() {
    var s = this.s;
    if (s.remote) { query(this.element, s, null); } else { view(this.element, s); }
  }
  rowsLoaded(total, page) {
    var el = this.element, s = this.s;
    s.total = parseInt(total, 10) || 0;
    if (page) { s.page = parseInt(page, 10) || s.page; }
    var pages = Math.ceil(s.total / s.pageSize);
    if (s.pageable && pages > 0 && s.page > pages) {
      s.page = pages;
      writeState(el, s);
      query(el, s, null);
      return;
    }
    loading(el, s, false);
    el.setAttribute("data-ah-loaded", "true");
    el.setAttribute("aria-rowcount", String(s.total + s.headerRows));
    adoptRows(el, s);
    paintRows(el, s, s.pageable ? (s.page - 1) * s.pageSize : 0);
    paintSelection(el, s);
    var focusLost = document.activeElement === document.body;
    resetActive(el, s, focusLost);
  }
  rowUpdated(rowId) {
    var el = this.element, s = this.s;
    adoptRows(el, s);
    if (s.remote) {
      paintRows(el, s, s.pageable ? (s.page - 1) * s.pageSize : 0);
      paintSelection(el, s);
    } else {
      viewLocal(el, s);
    }
    var row = document.getElementById(rowId);
    if (row && s.active && !s.active.isConnected) {
      activate(el, s, cellAt(s, row, s.activeField), false);
    }
  }
});
