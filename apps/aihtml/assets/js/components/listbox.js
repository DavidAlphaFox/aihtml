/* Behaviour of listbox (designs/04-components.md), ported from sigil's
 * form/listbox. The rows are rendered on the server (aihtml_listbox);
 * the behaviour marks, filters and selects them. Shared helpers:
 * _lib_list.js. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_list.js";

var NS = AH.NS;
var LIST = AH.lib.list;
var ensureId = LIST.ensureId, publish = LIST.publish, split = LIST.split, join = LIST.join,
  shown = LIST.shown, enabled = LIST.enabled, scrollInto = LIST.scrollInto;

// ==================================================================
// listbox
// ==================================================================

function lbState(el) { return $.data(el, "ah-lb"); }

function lbItems(st) { return st.$list.children(".ah-listbox-item").get(); }
// Rows keyboard and check-all work on: shown and enabled.
function lbRows(st) { return lbItems(st).filter(function (li) { return shown(li) && enabled(li); }); }

function lbMark(el) {
  var st = lbState(el);
  lbItems(st).forEach(function (li) {
    var sel = st.selected.indexOf(li.getAttribute("data-value")) >= 0;
    $(li).toggleClass("ah-listbox-item-selected", sel).attr("aria-selected", String(sel))
      .children(".ah-listbox-checkbox").toggleClass("ah-listbox-checkbox-checked", sel);
  });
  if (st.$checkAll.length) {
    var rows = lbRows(st);
    var n = rows.filter(function (li) { return st.selected.indexOf(li.getAttribute("data-value")) >= 0; }).length;
    var all = rows.length > 0 && n === rows.length;
    st.$checkAll.attr("aria-pressed", all ? "true" : (n ? "mixed" : "false"))
      .children(".ah-listbox-checkbox")
      .toggleClass("ah-listbox-checkbox-checked", all)
      .toggleClass("ah-listbox-checkbox-indeterminate", n > 0 && !all);
  }
}

function lbSet(el, $el, values, fire) {
  var st = lbState(el);
  st.selected = st.multi ? values.slice() : values.slice(0, 1);
  lbMark(el);
  publish(el, $el, join(st.selected, !st.multi), fire);
}

function lbCursor(el, li) {
  var st = lbState(el);
  $(lbItems(st)).removeClass("ah-listbox-item-focused");
  st.cursor = li || null;
  if (!li) { $(el).removeAttr("aria-activedescendant"); return; }
  $(li).addClass("ah-listbox-item-focused");
  el.setAttribute("aria-activedescendant", li.id);
  scrollInto(st.$content[0], li);
}

function lbValue(li) { return li.getAttribute("data-value"); }

function lbRange(st, a, b) {
  var rows = lbRows(st), i = rows.indexOf(a), j = rows.indexOf(b);
  if (i < 0) { i = j; }
  return rows.slice(Math.min(i, j), Math.max(i, j) + 1).map(lbValue);
}

function lbToggle(el, $el, li) {
  var st = lbState(el);
  var v = lbValue(li), next = st.selected.slice(), i = next.indexOf(v);
  if (i >= 0) { next.splice(i, 1); } else { next.push(v); }
  lbSet(el, $el, next, true);
}

// A click: sigil's select-item! (single, Ctrl toggle, Shift range) and
// toggle-checkbox! (check boxes).
function lbClick(el, $el, li, e) {
  var st = lbState(el);
  if (!enabled(li)) { return; }
  if (st.checkboxes || (st.multi && (e.ctrlKey || e.metaKey))) {
    lbToggle(el, $el, li);
    st.anchor = li;
  } else if (st.multi && e.shiftKey && st.anchor) {
    var add = lbRange(st, st.anchor, li);
    lbSet(el, $el, st.selected.concat(add.filter(function (v) { return st.selected.indexOf(v) < 0; })), true);
  } else {
    lbSet(el, $el, [lbValue(li)], true);
    st.anchor = li;
  }
  lbCursor(el, li);
}

// Arrow keys and friends: move the cursor and select like sigil (a
// single row), or extend (Shift) or only move (Ctrl, check boxes).
function lbGo(el, $el, li, e) {
  var st = lbState(el);
  if (!li) { return; }
  var from = st.anchor || st.cursor || li;
  lbCursor(el, li);
  if (st.checkboxes || (st.multi && (e.ctrlKey || e.metaKey))) { return; }
  if (st.multi && e.shiftKey) {
    st.anchor = from;
    lbSet(el, $el, lbRange(st, from, li), true);
    return;
  }
  st.anchor = li;
  lbSet(el, $el, [lbValue(li)], true);
}

function lbKey(el, $el, e) {
  var st = lbState(el);
  var inFilter = e.target !== el;
  var rows = lbRows(st);
  if (!rows.length) { return; }
  var i = rows.indexOf(st.cursor);
  var PAGE = 10;
  switch (e.key) {
    case "ArrowDown": e.preventDefault(); lbGo(el, $el, rows[Math.min(i + 1, rows.length - 1)], e); break;
    case "ArrowUp": e.preventDefault(); lbGo(el, $el, rows[Math.max(i - 1, 0)], e); break;
    case "PageDown": e.preventDefault(); lbGo(el, $el, rows[Math.min(Math.max(i, 0) + PAGE, rows.length - 1)], e); break;
    case "PageUp": e.preventDefault(); lbGo(el, $el, rows[Math.max(i - PAGE, 0)], e); break;
    case "Home": if (!inFilter) { e.preventDefault(); lbGo(el, $el, rows[0], e); } break;
    case "End": if (!inFilter) { e.preventDefault(); lbGo(el, $el, rows[rows.length - 1], e); } break;
    case " ":
      if (inFilter || !st.cursor) { break; }
      e.preventDefault();
      if (st.multi) { lbToggle(el, $el, st.cursor); st.anchor = st.cursor; } else { lbSet(el, $el, [lbValue(st.cursor)], true); }
      break;
    case "Enter":
      if (!st.cursor) { break; }
      e.preventDefault();
      if (st.checkboxes) { lbToggle(el, $el, st.cursor); } else if (!st.multi) { lbSet(el, $el, [lbValue(st.cursor)], true); }
      break;
    default:
      if (inFilter) { break; }
      if ((e.key === "a" || e.key === "A") && (e.ctrlKey || e.metaKey) && st.multi) {
        e.preventDefault();
        lbSet(el, $el, rows.map(lbValue), true);
        break;
      }
      // sigil's incremental search: typed letters within 800 ms
      if (e.key && e.key.length === 1 && !e.ctrlKey && !e.altKey && !e.metaKey) {
        var now = Date.now();
        st.typed = (now - (st.typedAt || 0) > 800 ? "" : st.typed) + e.key.toLowerCase();
        st.typedAt = now;
        var hit = rows.filter(function (li) {
          return $(li).children(".ah-listbox-label").text().toLowerCase().indexOf(st.typed) === 0;
        })[0];
        if (hit) { e.preventDefault(); lbGo(el, $el, hit, {}); }
      }
  }
}

// sigil's filter-items!: hide rows without the text, and empty groups.
function lbFilter(el, text) {
  var st = lbState(el);
  var q = String(text || "").trim().toLowerCase();
  if (!st.remote) {
    lbItems(st).forEach(function (li) {
      var hit = !q || $(li).children(".ah-listbox-label").text().toLowerCase().indexOf(q) >= 0;
      li.style.display = hit ? "" : "none";
    });
  }
  lbGroups(el);
}

function lbGroups(el) {
  var st = lbState(el);
  st.$list.children(".ah-listbox-group").each(function () {
    var $rows = $(this).nextUntil(".ah-listbox-group");
    this.style.display = $rows.filter(function () { return shown(this); }).length ? "" : "none";
  });
  var any = lbItems(st).some(shown);
  st.$empty.prop("hidden", any);
  if (st.cursor && (!st.cursor.isConnected || !shown(st.cursor))) { lbCursor(el, null); }
  lbMark(el);
}

AH.define("listbox", {
  init: function (el, $el) {
    ensureId(el, "ah-lb");
    var st = {
      $list: $el.find(".ah-listbox-list"),
      $content: $el.children(".ah-listbox-content"),
      $empty: $el.find(".ah-listbox-empty"),
      $checkAll: $el.children(".ah-listbox-check-all"),
      $filter: $el.find(".ah-listbox-filter-input"),
      checkboxes: $el.hasClass("ah-listbox-checkboxes"),
      remote: $el.hasClass("ah-listbox-remote"),
      cursor: null, anchor: null, typed: ""
    };
    st.multi = st.checkboxes || $el.hasClass("ah-listbox-multiple");
    st.selected = split(el.getAttribute("data-ah-value"), !st.multi);
    $.data(el, "ah-lb", st);
    var blocked = function () { return $el.hasClass("ah-listbox-disabled"); };
    $el.on("mousedown" + NS, ".ah-listbox-item, .ah-listbox-check-all", function (e) {
      if (e.shiftKey) { e.preventDefault(); }  // no text selection on Shift+click
    });
    $el.on("click" + NS, ".ah-listbox-item", function (e) {
      if (!blocked()) { lbClick(el, $el, this, e); }
    });
    $el.on("click" + NS, ".ah-listbox-check-all", function () {
      if (blocked()) { return; }
      var rows = lbRows(st).map(lbValue);
      var all = rows.length && rows.every(function (v) { return st.selected.indexOf(v) >= 0; });
      var rest = st.selected.filter(function (v) { return rows.indexOf(v) < 0; });
      lbSet(el, $el, all ? rest : rest.concat(rows), true);
    });
    $el.on("keydown" + NS, function (e) { if (!blocked()) { lbKey(el, $el, e); } });
    $el.on("focus" + NS, function () {
      if (!st.cursor) {
        var rows = lbRows(st);
        var sel = rows.filter(function (li) { return st.selected.indexOf(lbValue(li)) >= 0; })[0];
        if (sel || rows[0]) { lbCursor(el, sel || rows[0]); }
      }
    });
    st.$filter
      .on("input" + NS, function () { lbFilter(el, this.value); })
      .on("change" + NS, function (e) { e.stopPropagation(); });
  },
  methods: {
    // A value or a list (multiple); no change event (the server set it).
    setValue: function (el, $el, v) { lbSet(el, $el, split(v, !lbState(el).multi), false); },
    getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
    clear: function (el, $el) { lbSet(el, $el, [], true); },
    filter: function (el, $el, text) {
      lbState(el).$filter.val(text);
      lbFilter(el, text);
    },
    // Called by aihtml_listbox:listbox_items/3 after it morphed the
    // server-rendered rows into the list.
    itemsLoaded: function (el) {
      lbState(el).anchor = null;
      lbGroups(el);
    }
  }
});
