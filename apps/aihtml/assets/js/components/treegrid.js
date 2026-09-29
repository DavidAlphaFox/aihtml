/* Behaviour of the tree grid (designs/04-components.md).
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
 * attributes), so a morph that re-initialises the behaviour keeps it.
 * The selection is the root's data-ah-value (keys joined with commas),
 * mirrored into a hidden input; "change" fires when the user changes it,
 * never from methods.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.table;
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
  var bindScroll = L.bindScroll;
  var watchGutter = L.watchGutter;
  var bindResize = L.bindResize;
  var nextSort = L.nextSort;
  var writeSort = L.writeSort;
  var isOff = L.isOff;
  var OWN = L.OWN;

  function tgBody(el) { return document.getElementById(el.id + "-rows"); }
  function tgRows(el) { return $(tgBody(el)).children("tr.ah-tg-row"); }
  function tgVisible(el) { return tgRows(el).filter(function () { return !this.hidden; }).toArray(); }
  function tgByKey(el, key) {
    if (key === null || key === undefined) { return null; }
    key = String(key);
    return tgRows(el).filter(function () { return this.getAttribute("data-key") === key; })[0] || null;
  }
  function tgOpen(tr) { return tr.getAttribute("aria-expanded") === "true"; }
  function tgParent(el, tr) { return tgByKey(el, tr.getAttribute("data-parent") || null); }
  function tgInfo(tr) { return { key: tr.getAttribute("data-key"), level: parseInt(tr.getAttribute("data-level"), 10) }; }

  // Rows show when every ancestor is open; stripes follow the shown rows.
  function tgLayout(el) {
    var open = {};
    var alt = el.getAttribute("data-alt-rows") === "true";
    var n = 0;
    var $rows = tgRows(el);
    var stop = $rows.filter("[tabindex='0']")[0];
    var hadFocus = !!stop && document.activeElement === stop;
    $rows.each(function () {
      var p = this.getAttribute("data-parent");
      var shown = !p || !(p in open) || open[p];
      this.hidden = !shown;
      open[this.getAttribute("data-key")] = shown && tgOpen(this);
      if (shown) {
        $(this).toggleClass("ah-tg-row-alt", alt && n % 2 === 1);
        n++;
      }
    });
    $(tgBody(el)).children("tr.ah-tg-row-empty").prop("hidden", $rows.length > 0);
    // the tab stop never stays on a hidden row
    if (!stop || stop.hidden) {
      var to = stop;
      while (to && to.hidden) { to = tgParent(el, to); }
      focusRow($rows, to || tgVisible(el)[0], hadFocus);
    }
  }

  function tgEvent(el, name, tr, extra) {
    var d = $.extend(tgInfo(tr), extra || {});
    el.setAttribute("data-key", d.key);
    $(el).trigger(name, [d]);
  }

  function tgSetOpen(el, tr, open) {
    if (!tr || !tr.hasAttribute("aria-expanded") || tgOpen(tr) === open) { return; }
    if (open && tr.getAttribute("data-lazy") === "true") {
      tgLoad(el, tr);
      return;
    }
    tr.setAttribute("aria-expanded", String(open));
    tgToggleIcon(tr, open ? "open" : "closed");
    tgLayout(el);
    tgEvent(el, open ? "ah:expand" : "ah:collapse", tr);
  }

  function tgToggleIcon(tr, state) {
    $(tr).find("> td > .ah-tg-tree-indent > .ah-tg-toggle")
      .removeClass("ah-tg-toggle-open ah-tg-toggle-closed ah-tg-toggle-leaf")
      .addClass("ah-tg-toggle-" + state)
      .text(state === "leaf" ? "" : "▶");
  }

  // A lazy row asks the server for its children: the load token is bound
  // to the row (ah:load), so each row has its own request.
  function tgLoad(el, tr) {
    var token = el.getAttribute("data-load");
    if (!token || $(tr).hasClass("ah-tg-row-loading")) { return; }
    $(tr).addClass("ah-tg-row-loading").attr("aria-busy", "true");
    tr.setAttribute("data-value", el.getAttribute("data-ah-value") || "");
    if (!tr.hasAttribute("data-ah-on")) {
      tr.setAttribute("data-ah-on", "ah:load:" + token);
      AH.mount(tr);                   // registers the ah:load listener
    }
    $(tr).trigger("ah:load", [tgInfo(tr)]);
  }

  function tgChildrenLoaded(el, tr) {
    if (!tr) { return; }
    var prefix = tr.id + "-";
    var after = tr;
    tgRows(el).filter(function () { return this.id.indexOf(prefix) === 0; }).each(function () {
      after.parentNode.insertBefore(this, after.nextSibling);
      after = this;
    });
    $(tr).removeClass("ah-tg-row-loading").removeAttr("aria-busy data-lazy data-ah-on data-value");
    if (after === tr) {
      tr.removeAttribute("aria-expanded");
      $(tr).addClass("ah-tg-row-leaf");
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
    tgEvent(el, "ah:expand", tr);
  }

  // Siblings in the order of a column (or the original order), then the
  // rows re-laid depth first.
  function tgSort(el, field, dir) {
    var rows = tgRows(el).toArray();
    var keys = {};
    rows.forEach(function (r) { keys[r.getAttribute("data-key")] = true; });
    var kids = { "": [] };
    rows.forEach(function (r) {
      var p = r.getAttribute("data-parent") || "";
      if (!keys[p]) { p = ""; }
      (kids[p] = kids[p] || []).push(r);
    });
    var sign = dir === "desc" ? -1 : 1;
    var cell = function (r) { return raw($(r).children("td[data-field]").filter(function () {
      return this.getAttribute("data-field") === field; })[0]); };
    Object.keys(kids).forEach(function (p) {
      kids[p].sort(function (a, b) {
        var c = dir && field ? sign * compare(cell(a), cell(b)) : 0;
        return c || ord(a) - ord(b);
      });
    });
    var body = tgBody(el);
    var end = $(body).children("tr.ah-tg-row-empty")[0] || null;
    (function walk(list) {
      list.forEach(function (r) {
        body.insertBefore(r, end);
        walk(kids[r.getAttribute("data-key")] || []);
      });
    })(kids[""]);
    writeSort(el, "ah-tg", field, dir);
    tgLayout(el);
  }

  function tgSelect(el, keys, user) {
    var prev = keysOf(el);
    var mode = el.getAttribute("data-selection");
    markRows(tgRows(el), "ah-tg", keys, mode);
    writeValue(el, keys);
    headerCheck(part(el, "ah-tg", "header").find(".ah-tg-header-checkbox"), keys,
                tgRows(el).map(function () { return this.getAttribute("data-key"); }).get());
    if (user && !sameKeys(prev, keys)) { $(el).trigger("change"); }
  }

  function tgKeydown(el, st, e) {
    var tr = $(e.target).closest("tr.ah-tg-row")[0];
    if (!tr || e.target !== tr || tr.parentNode !== tgBody(el) || isOff(el) ||
        e.altKey || e.metaKey) { return; }
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
        if (tr.hasAttribute("aria-expanded") && !tgOpen(tr)) { tgSetOpen(el, tr, true); }
        else if (tgOpen(tr) && vis[i + 1] && vis[i + 1].getAttribute("data-parent") === key) { to = vis[i + 1]; }
        break;
      case "ArrowLeft":
        if (tgOpen(tr)) { tgSetOpen(el, tr, false); } else { to = tgParent(el, tr); }
        break;
      case "Enter":
      case " ":
        var next = keySelect(el.getAttribute("data-selection"), keysOf(el), key);
        if (next) { tgSelect(el, next, true); st.anchor = key; }
        break;
      default:
        return;
    }
    e.preventDefault();
    if (to) { focusRow(tgRows(el), to, true); }
  }

  function tgClick(el, st, e) {
    var tr = $(e.target).closest("tr.ah-tg-row")[0];
    if (!tr || tr.parentNode !== tgBody(el) || isOff(el)) { return; }
    var key = tr.getAttribute("data-key");
    focusRow(tgRows(el), tr, false);
    if ($(e.target).closest(".ah-tg-toggle").length) {
      tgSetOpen(el, tr, !tgOpen(tr));
      return;
    }
    if ($(e.target).closest(OWN).length) { return; }
    var mode = el.getAttribute("data-selection");
    var next = clickSelect(mode, keysOf(el), key, e, st.anchor,
                           tgVisible(el).map(function (r) { return r.getAttribute("data-key"); }));
    if (next) {
      tgSelect(el, next, true);
      if (!e.shiftKey) { st.anchor = key; }
    }
    tgEvent(el, "ah:row-click", tr);
  }

  AH.define("treegrid", {
    init: function (el, $el) {
      var st = { id: L.nextId(), anchor: null, stopResize: null };
      $.data(el, "ah-treegrid", st);
      $el.on("click" + NS, "tbody > tr.ah-tg-row", function (e) { tgClick(el, st, e); });
      $el.on("dblclick" + NS, "tbody > tr.ah-tg-row", function (e) {
        if (this.parentNode === tgBody(el) && !$(e.target).closest(".ah-tg-toggle").length) {
          tgEvent(el, "ah:row-dblclick", this);
        }
      });
      $el.on("keydown" + NS, "tbody > tr.ah-tg-row", function (e) { tgKeydown(el, st, e); });
      $el.on("change" + NS, ".ah-tg-row-checkbox", function (e) {
        e.stopPropagation();
        var k = $(this).closest("tr")[0].getAttribute("data-key");
        var rest = keysOf(el).filter(function (x) { return x !== k; });
        tgSelect(el, this.checked ? rest.concat([k]) : rest, true);
      });
      $el.on("change" + NS, ".ah-tg-header-checkbox", function (e) {
        e.stopPropagation();
        tgSelect(el, this.checked ? tgRows(el).map(function () { return this.getAttribute("data-key"); }).get()
                                  : [], true);
      });
      $el.on("click" + NS, ".ah-tg-th-sortable", function (e) {
        if ($(e.target).closest(".ah-tg-resize-handle").length || isOff(el)) { return; }
        var field = this.getAttribute("data-field");
        var dir = nextSort(el, field);
        tgSort(el, dir ? field : null, dir);
        $el.trigger("ah:sort", [{ field: field, dir: dir }]);
      });
      $el.on("keydown" + NS, ".ah-tg-th-sortable", function (e) {
        if (e.key === "Enter" || e.key === " ") { e.preventDefault(); $(this).trigger("click"); }
      });
      bindScroll(el, "ah-tg");
      bindResize(el, $el, "ah-tg", st);
      watchGutter(el, "ah-tg", st);
      headerCheck(part(el, "ah-tg", "header").find(".ah-tg-header-checkbox"), keysOf(el),
                  tgRows(el).map(function () { return this.getAttribute("data-key"); }).get());
      if (!tgRows(el).filter("[tabindex='0']").length) { focusRow(tgRows(el), tgVisible(el)[0], false); }
    },
    destroy: function (el) {
      var st = $.data(el, "ah-treegrid");
      if (st && st.stopResize) { st.stopResize(); }
      if (st && st.ro) { st.ro.disconnect(); }
      part(el, "ah-tg", "body").off(NS);
    },
    methods: {
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      setValue: function (el, $el, v) {
        var keys = toKeys(v);
        tgSelect(el, keys.filter(function (k) { return tgByKey(el, k); }), false);
      },
      clearSelection: function (el) { tgSelect(el, [], false); },
      expand: function (el, $el, k) { tgSetOpen(el, tgByKey(el, k), true); },
      collapse: function (el, $el, k) { tgSetOpen(el, tgByKey(el, k), false); },
      toggle: function (el, $el, k) {
        var tr = tgByKey(el, k);
        if (tr) { tgSetOpen(el, tr, !tgOpen(tr)); }
      },
      expandAll: function (el) {
        tgRows(el).filter("[aria-expanded]").not("[data-lazy]").attr("aria-expanded", "true")
          .each(function () { tgToggleIcon(this, "open"); });
        tgLayout(el);
      },
      collapseAll: function (el) {
        tgRows(el).filter("[aria-expanded]").attr("aria-expanded", "false")
          .each(function () { tgToggleIcon(this, "closed"); });
        tgLayout(el);
      },
      ensureVisible: function (el, $el, k) {
        var tr = tgByKey(el, k);
        for (var p = tr && tgParent(el, tr); p; p = tgParent(el, p)) {
          if (!tgOpen(p)) {
            p.setAttribute("aria-expanded", "true");
            tgToggleIcon(p, "open");
          }
        }
        tgLayout(el);
        if (tr && tr.scrollIntoView) { tr.scrollIntoView({ block: "nearest" }); }
      },
      sort: function (el, $el, field, dir) {
        dir = dir === "asc" || dir === "desc" ? dir : null;
        tgSort(el, dir ? field : null, dir);
      },
      childrenLoaded: function (el, $el, id) { tgChildrenLoaded(el, document.getElementById(id)); }
    }
  });
})(window.jQuery, window.AH);
