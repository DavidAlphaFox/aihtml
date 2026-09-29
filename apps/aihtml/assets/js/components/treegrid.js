/* Controller of the tree grid (designs/04-components.md).
 *
 * Ported from sigil (data/treegrid). The server renders every row and
 * header; this file moves state around in that DOM: expand / collapse
 * (rows under a closed node are hidden), sorting within each level,
 * selection (single, multiple with Ctrl / Shift, checkbox), keyboard
 * (treegrid pattern: roving tabindex over rows, arrows, Home/End,
 * Enter/Space), lazy rows loaded through the load action (data-load, a
 * signed token) which answers with treegrid_children/3 -> childrenLoaded.
 * Shared helpers are in _lib_table.js (AH.lib.table).
 *
 * The view state lives in the DOM (root data-* attributes, row
 * attributes), so a morph keeps it. The selection is the root's
 * data-ah-value (keys joined with commas), mirrored into a hidden input;
 * "change" fires when the user changes it, never from methods.
 *
 * Events (native CustomEvents, bubbling; the detail in e.detail):
 *   ah:expand, ah:collapse, ah:row-click, ah:row-dblclick  {key, level}
 *                     (the key is also the root's data-key)
 *   ah:load           {key, level}, fired on the lazy row
 *   ah:sort           {field, dir}
 *   ah:column-resize  {field, width}
 */
import AH from "../core.js";
import "./_lib_table.js";

var L = AH.lib.table;
var kids = L.kids;
var kid = L.kid;
var keysOf = L.keysOf;
var toKeys = L.toKeys;
var writeValue = L.writeValue;
var sameKeys = L.sameKeys;
var raw = L.raw;
var compare = L.compare;
var ord = L.ord;
var clickSelect = L.clickSelect;
var keySelect = L.keySelect;
var markRows = L.markRows;
var headerCheck = L.headerCheck;
var focusRow = L.focusRow;
var part = L.part;
var writeSort = L.writeSort;
var nextSort = L.nextSort;
var isOff = L.isOff;
var OWN = L.OWN;

function tgBody(el) { return document.getElementById(el.id + "-rows"); }
function tgRows(el) { return kids(tgBody(el), "tr.ah-tg-row"); }
function tgVisible(el) { return tgRows(el).filter(function (r) { return !r.hidden; }); }
function tgKeys(el) { return tgRows(el).map(function (r) { return r.getAttribute("data-key"); }); }
function tgByKey(el, key) {
  if (key === null || key === undefined) { return null; }
  key = String(key);
  return tgRows(el).filter(function (r) { return r.getAttribute("data-key") === key; })[0] || null;
}
function tgOpen(tr) { return tr.getAttribute("aria-expanded") === "true"; }
function tgParent(el, tr) { return tgByKey(el, tr.getAttribute("data-parent") || null); }
function tgInfo(tr) { return { key: tr.getAttribute("data-key"), level: parseInt(tr.getAttribute("data-level"), 10) }; }
function tgHeaderBox(el) {
  var h = part(el, "ah-tg", "header");
  return h ? h.querySelector(".ah-tg-header-checkbox") : null;
}

// Rows show when every ancestor is open; stripes follow the shown rows.
function tgLayout(el) {
  var open = {};
  var alt = el.getAttribute("data-alt-rows") === "true";
  var n = 0;
  var rows = tgRows(el);
  var stop = rows.filter(function (r) { return r.getAttribute("tabindex") === "0"; })[0];
  var hadFocus = !!stop && document.activeElement === stop;
  rows.forEach(function (r) {
    var p = r.getAttribute("data-parent");
    var shown = !p || !(p in open) || open[p];
    r.hidden = !shown;
    open[r.getAttribute("data-key")] = shown && tgOpen(r);
    if (shown) {
      r.classList.toggle("ah-tg-row-alt", alt && n % 2 === 1);
      n++;
    }
  });
  kids(tgBody(el), "tr.ah-tg-row-empty").forEach(function (r) { r.hidden = rows.length > 0; });
  // the tab stop never stays on a hidden row
  if (!stop || stop.hidden) {
    var to = stop;
    while (to && to.hidden) { to = tgParent(el, to); }
    focusRow(rows, to || tgVisible(el)[0], hadFocus);
  }
}

function tgEvent(ctl, el, name, tr, extra) {
  var d = Object.assign(tgInfo(tr), extra || {});
  el.setAttribute("data-key", d.key);
  ctl.fire(name, d);
}

function tgSetOpen(ctl, el, tr, open) {
  if (!tr || !tr.hasAttribute("aria-expanded") || tgOpen(tr) === open) { return; }
  if (open && tr.getAttribute("data-lazy") === "true") {
    tgLoad(ctl, el, tr);
    return;
  }
  tr.setAttribute("aria-expanded", String(open));
  tgToggleIcon(tr, open ? "open" : "closed");
  tgLayout(el);
  tgEvent(ctl, el, open ? "ah:expand" : "ah:collapse", tr);
}

function tgToggleIcon(tr, state) {
  kids(tr, "td").forEach(function (td) {
    kids(td, ".ah-tg-tree-indent").forEach(function (ind) {
      kids(ind, ".ah-tg-toggle").forEach(function (t) {
        t.classList.remove("ah-tg-toggle-open", "ah-tg-toggle-closed", "ah-tg-toggle-leaf");
        t.classList.add("ah-tg-toggle-" + state);
        t.textContent = state === "leaf" ? "" : "▶";
      });
    });
  });
}

// A lazy row asks the server for its children: the load token is bound
// to the row (ah:load), so each row has its own request.
function tgLoad(ctl, el, tr) {
  var token = el.getAttribute("data-load");
  if (!token || tr.classList.contains("ah-tg-row-loading")) { return; }
  tr.classList.add("ah-tg-row-loading");
  tr.setAttribute("aria-busy", "true");
  tr.setAttribute("data-value", el.getAttribute("data-ah-value") || "");
  if (!tr.hasAttribute("data-ah-on")) {
    tr.setAttribute("data-ah-on", "ah:load:" + token);
    AH.mount(tr);                   // registers the ah:load listener
  }
  ctl.fire("ah:load", tgInfo(tr), tr);
}

function tgChildrenLoaded(ctl, el, tr) {
  if (!tr) { return; }
  var prefix = tr.id + "-";
  var after = tr;
  tgRows(el).filter(function (r) { return r.id.indexOf(prefix) === 0; }).forEach(function (r) {
    after.parentNode.insertBefore(r, after.nextSibling);
    after = r;
  });
  tr.classList.remove("ah-tg-row-loading");
  ["aria-busy", "data-lazy", "data-ah-on", "data-value"].forEach(function (a) { tr.removeAttribute(a); });
  if (after === tr) {
    tr.removeAttribute("aria-expanded");
    tr.classList.add("ah-tg-row-leaf");
    tgToggleIcon(tr, "leaf");
    tgLayout(el);
    return;
  }
  tr.setAttribute("aria-expanded", "true");
  tgToggleIcon(tr, "open");
  var f = el.getAttribute("data-sort-field");
  if (f) { tgSort(el, f, el.getAttribute("data-sort-dir")); }
  markRows(tgRows(el), "ah-tg", keysOf(el), el.getAttribute("data-selection"));
  tgLayout(el);
  tgEvent(ctl, el, "ah:expand", tr);
}

// Siblings in the order of a column (or the original order), then the
// rows re-laid depth first.
function tgSort(el, field, dir) {
  var rows = tgRows(el);
  var keys = {};
  rows.forEach(function (r) { keys[r.getAttribute("data-key")] = true; });
  var byParent = { "": [] };
  rows.forEach(function (r) {
    var p = r.getAttribute("data-parent") || "";
    if (!keys[p]) { p = ""; }
    (byParent[p] = byParent[p] || []).push(r);
  });
  var sign = dir === "desc" ? -1 : 1;
  var cell = function (r) {
    return raw(kids(r, "td[data-field]").filter(function (td) {
      return td.getAttribute("data-field") === field; })[0]);
  };
  Object.keys(byParent).forEach(function (p) {
    byParent[p].sort(function (a, b) {
      var c = dir && field ? sign * compare(cell(a), cell(b)) : 0;
      return c || ord(a) - ord(b);
    });
  });
  var body = tgBody(el);
  var end = kid(body, "tr.ah-tg-row-empty");
  (function walk(list) {
    list.forEach(function (r) {
      body.insertBefore(r, end);
      walk(byParent[r.getAttribute("data-key")] || []);
    });
  })(byParent[""]);
  writeSort(el, "ah-tg", field, dir);
  tgLayout(el);
}

function tgSelect(ctl, el, keys, user) {
  var prev = keysOf(el);
  var mode = el.getAttribute("data-selection");
  markRows(tgRows(el), "ah-tg", keys, mode);
  writeValue(el, keys);
  headerCheck(tgHeaderBox(el), keys, tgKeys(el));
  if (user && !sameKeys(prev, keys)) { ctl.fire("change"); }
}

function tgKeydown(ctl, el, st, e, tr) {
  if (e.target !== tr || tr.parentNode !== tgBody(el) || isOff(el) || e.altKey || e.metaKey) { return; }
  var vis = tgVisible(el);
  var i = vis.indexOf(tr);
  var to = null;
  var key = tr.getAttribute("data-key");
  switch (e.key) {
    case "ArrowDown": to = vis[Math.min(i + 1, vis.length - 1)]; break;
    case "ArrowUp": to = vis[Math.max(i - 1, 0)]; break;
    case "Home": to = vis[0]; break;
    case "End": to = vis[vis.length - 1]; break;
    case "PageDown": to = vis[Math.min(i + 10, vis.length - 1)]; break;
    case "PageUp": to = vis[Math.max(i - 10, 0)]; break;
    case "ArrowRight":
      if (tr.hasAttribute("aria-expanded") && !tgOpen(tr)) { tgSetOpen(ctl, el, tr, true); }
      else if (tgOpen(tr) && vis[i + 1] && vis[i + 1].getAttribute("data-parent") === key) { to = vis[i + 1]; }
      break;
    case "ArrowLeft":
      if (tgOpen(tr)) { tgSetOpen(ctl, el, tr, false); } else { to = tgParent(el, tr); }
      break;
    case "Enter":
    case " ":
      var next = keySelect(el.getAttribute("data-selection"), keysOf(el), key);
      if (next) { tgSelect(ctl, el, next, true); st.anchor = key; }
      break;
    default:
      return;
  }
  e.preventDefault();
  if (to) { focusRow(tgRows(el), to, true); }
}

function tgClick(ctl, el, st, e, tr) {
  if (tr.parentNode !== tgBody(el) || isOff(el)) { return; }
  var key = tr.getAttribute("data-key");
  focusRow(tgRows(el), tr, false);
  var toggle = e.target.closest(".ah-tg-toggle");
  if (toggle && tr.contains(toggle)) {
    tgSetOpen(ctl, el, tr, !tgOpen(tr));
    return;
  }
  var own = e.target.closest(OWN);
  if (own && tr.contains(own)) { return; }
  var mode = el.getAttribute("data-selection");
  var next = clickSelect(mode, keysOf(el), key, e, st.anchor,
                         tgVisible(el).map(function (r) { return r.getAttribute("data-key"); }));
  if (next) {
    tgSelect(ctl, el, next, true);
    if (!e.shiftKey) { st.anchor = key; }
  }
  tgEvent(ctl, el, "ah:row-click", tr);
}

AH.register("treegrid", class extends AH.Controller {
  setup() {
    var el = this.element, ctl = this;
    var st = this.st = { id: L.nextId(), anchor: null, stopResize: null, ro: null };
    var ROW = "tbody > tr.ah-tg-row";
    this.delegate("click", ROW, function (e, tr) { tgClick(ctl, el, st, e, tr); });
    this.delegate("dblclick", ROW, function (e, tr) {
      if (tr.parentNode === tgBody(el) && !e.target.closest(".ah-tg-toggle")) {
        tgEvent(ctl, el, "ah:row-dblclick", tr);
      }
    });
    this.listen(el, "keydown", function (e) {
      if (!(e.target instanceof Element)) { return; }
      var tr = e.target.closest(ROW);
      if (tr && el.contains(tr)) { tgKeydown(ctl, el, st, e, tr); return; }
      var th = e.target.closest(".ah-tg-th-sortable");
      if (th && el.contains(th) && (e.key === "Enter" || e.key === " ")) {
        e.preventDefault();
        th.click();
      }
    });
    // Inner controls (row / header boxes, filters, pager, chooser) report
    // to the table, not as its own change: stopImmediatePropagation also
    // keeps them from listeners on the root added after this one, as
    // jQuery's delegated stopPropagation did.
    this.listen(el, "change", function (e) {
      var t = e.target;
      if (!(t instanceof Element) || t === el) { return; }
      if (t.matches(".ah-tg-row-checkbox")) {
        e.stopImmediatePropagation();
        var k = t.closest("tr").getAttribute("data-key");
        var rest = keysOf(el).filter(function (x) { return x !== k; });
        tgSelect(ctl, el, t.checked ? rest.concat([k]) : rest, true);
      } else if (t.matches(".ah-tg-header-checkbox")) {
        e.stopImmediatePropagation();
        tgSelect(ctl, el, t.checked ? tgKeys(el) : [], true);
      }
    });
    this.delegate("click", ".ah-tg-th-sortable", function (e, th) {
      if (e.target.closest(".ah-tg-resize-handle") || isOff(el)) { return; }
      var field = th.getAttribute("data-field");
      var dir = nextSort(el, field);
      tgSort(el, dir ? field : null, dir);
      ctl.fire("ah:sort", { field: field, dir: dir });
    });
    L.bindScroll(this, el, "ah-tg");
    L.bindResize(this, el, "ah-tg", st);
    L.watchGutter(el, "ah-tg", st);
    headerCheck(tgHeaderBox(el), keysOf(el), tgKeys(el));
    var rows = tgRows(el);
    if (!rows.some(function (r) { return r.getAttribute("tabindex") === "0"; })) {
      focusRow(rows, tgVisible(el)[0], false);
    }
  }

  teardown() {
    var st = this.st;
    if (st.stopResize) { st.stopResize(); }
    if (st.ro) { st.ro.disconnect(); }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  setValue(v) {
    var el = this.element;
    tgSelect(this, el, toKeys(v).filter(function (k) { return tgByKey(el, k); }), false);
  }
  clearSelection() { tgSelect(this, this.element, [], false); }
  expand(k) { tgSetOpen(this, this.element, tgByKey(this.element, k), true); }
  collapse(k) { tgSetOpen(this, this.element, tgByKey(this.element, k), false); }
  toggle(k) {
    var tr = tgByKey(this.element, k);
    if (tr) { tgSetOpen(this, this.element, tr, !tgOpen(tr)); }
  }
  expandAll() {
    tgRows(this.element).forEach(function (r) {
      if (r.hasAttribute("aria-expanded") && !r.hasAttribute("data-lazy")) {
        r.setAttribute("aria-expanded", "true");
        tgToggleIcon(r, "open");
      }
    });
    tgLayout(this.element);
  }
  collapseAll() {
    tgRows(this.element).forEach(function (r) {
      if (r.hasAttribute("aria-expanded")) {
        r.setAttribute("aria-expanded", "false");
        tgToggleIcon(r, "closed");
      }
    });
    tgLayout(this.element);
  }
  ensureVisible(k) {
    var el = this.element;
    var tr = tgByKey(el, k);
    for (var p = tr && tgParent(el, tr); p; p = tgParent(el, p)) {
      if (!tgOpen(p)) {
        p.setAttribute("aria-expanded", "true");
        tgToggleIcon(p, "open");
      }
    }
    tgLayout(el);
    if (tr && tr.scrollIntoView) { tr.scrollIntoView({ block: "nearest" }); }
  }
  sort(field, dir) {
    dir = dir === "asc" || dir === "desc" ? dir : null;
    tgSort(this.element, dir ? field : null, dir);
  }
  childrenLoaded(id) { tgChildrenLoaded(this, this.element, document.getElementById(id)); }
});
