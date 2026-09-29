/* Behaviours of the data_tables components (designs/04-components.md).
 *
 * Ported from sigil (data/treegrid, data/datatable). The server renders
 * every row, header and pager; this file moves state around in that DOM:
 *
 *   treegrid   expand / collapse (rows under a closed node are hidden),
 *              sorting within each level, selection (single, multiple
 *              with Ctrl / Shift, checkbox), keyboard (treegrid pattern:
 *              roving tabindex over rows, arrows, Home/End, Enter/Space),
 *              lazy rows loaded through the load action (data-load, a
 *              signed token) which answers with treegrid_children/3 ->
 *              childrenLoaded
 *   datatable  local mode: sort, filter row / search / advanced filters and
 *              paging over the rendered rows (the pager comes from the
 *              shared template datatable_pager); remote mode (data-mode
 *              "remote"): each view change writes the state on the root
 *              (data-sort-field, data-sort-dir, data-page, data-page-size,
 *              data-search, data-filters as JSON) and fires ah:query,
 *              whose action answers with datatable_rows/3 (a morph of the
 *              whole table). Selection, row details, inline editing
 *              (ah:cell-edit, sent to the edit action, data-edit), column
 *              resize and the column chooser work in both modes.
 *
 * The view state lives in the DOM (root data-* attributes, filter inputs,
 * row attributes), so a morph that re-initialises the behaviour keeps it.
 * The selection is the root's data-ah-value (keys joined with commas),
 * mirrored into a hidden input; "change" fires when the user changes it,
 * never from methods.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  // ------------------------------------------------------------------
  // Shared
  // ------------------------------------------------------------------

  function keysOf(el) {
    var v = el.getAttribute("data-ah-value") || "";
    return v ? v.split(",") : [];
  }

  function toKeys(v) {
    if (v === null || v === undefined) { return []; }
    if (Array.isArray(v)) { return v.map(String); }
    v = String(v);
    return v ? v.split(",") : [];
  }

  function writeValue(el, keys) {
    var v = keys.join(",");
    el.setAttribute("data-ah-value", v);
    $(el).children("input[type=hidden][data-ah-input]").val(v);
  }

  function sameKeys(a, b) {
    return a.length === b.length && a.every(function (k, i) { return k === b[i]; });
  }

  // The raw value of a cell: data-value when the server wrote one (numbers,
  // rendered cells), its text otherwise.
  function raw(td) {
    if (!td) { return ""; }
    var v = td.getAttribute("data-value");
    return v !== null ? v : $(td).text();
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

  function markRows($rows, pre, keys, mode) {
    $rows.each(function () {
      var on = keys.indexOf(this.getAttribute("data-key")) >= 0;
      $(this).toggleClass(pre + "-row-selected", on);
      if (mode !== "none") { this.setAttribute("aria-selected", String(on)); }
      $(this).children("td").children("." + pre + "-row-checkbox").prop("checked", on);
    });
  }

  function headerCheck($box, keys, visibleKeys) {
    if (!$box.length) { return; }
    var n = visibleKeys.filter(function (k) { return keys.indexOf(k) >= 0; }).length;
    $box.prop("checked", n > 0 && n === visibleKeys.length)
      .prop("indeterminate", n > 0 && n < visibleKeys.length);
  }

  // Roving tabindex: one row of the table is in the tab order.
  function focusRow($rows, tr, move) {
    if (!tr) { return; }
    $rows.attr("tabindex", "-1");
    tr.setAttribute("tabindex", "0");
    if (move) {
      tr.focus();
      if (tr.scrollIntoView) { tr.scrollIntoView({ block: "nearest" }); }
    }
  }

  function part(el, pre, name) {
    return $(el).children("." + pre + "-content").children("." + pre + "-" + name);
  }

  // Body scroll moves the header along; bound on the body (scroll does
  // not bubble), so destroy unbinds it there.
  function bindScroll(el, pre) {
    var header = part(el, pre, "header")[0];
    part(el, pre, "body").on("scroll" + NS, function () {
      if (header) { header.scrollLeft = this.scrollLeft; }
    });
  }

  // The header leaves room for the body's vertical scrollbar, so the
  // columns of both tables line up (sigil reserves it with
  // scrollbar-gutter, which leaves an empty strip when nothing scrolls).
  function syncGutter(el, pre) {
    var body = part(el, pre, "body")[0], header = part(el, pre, "header")[0];
    if (!body || !header) { return; }
    var w = body.offsetWidth - body.clientWidth;
    header.style.paddingRight = w > 0 ? w + "px" : "";
  }

  function watchGutter(el, pre, st) {
    syncGutter(el, pre);
    var body = part(el, pre, "body")[0];
    if (body && window.ResizeObserver) {
      st.ro = new ResizeObserver(function () { syncGutter(el, pre); });
      st.ro.observe(body);
      var table = $(body).children("table")[0];
      if (table) { st.ro.observe(table); }
    }
  }

  // Dragging a header edge resizes the column in both tables.
  function bindResize(el, $el, pre, st) {
    $el.on("mousedown" + NS, "." + pre + "-resize-handle", function (e) {
      if (e.button !== 0) { return; }
      e.preventDefault();
      e.stopPropagation();
      var th = this.parentNode;
      var idx = Array.prototype.indexOf.call(th.parentNode.children, th);
      var cols = part(el, pre, "header").add(part(el, pre, "body"))
        .find("> table > colgroup").map(function () { return this.children[idx]; }).get();
      var x0 = e.pageX, w0 = th.getBoundingClientRect().width, w = w0;
      var dns = ".ahdtbrs" + st.id;
      function move(ev) {
        w = Math.max(40, Math.round(w0 + ev.pageX - x0));
        cols.forEach(function (c) { c.style.width = w + "px"; c.style.minWidth = w + "px"; });
      }
      function up() {
        $(document).off(dns);
        st.stopResize = null;
        $(el).trigger("ah:column-resize", [{ field: th.getAttribute("data-field"), width: w }]);
      }
      $(document).on("mousemove" + dns, move).on("mouseup" + dns, up);
      st.stopResize = function () { $(document).off(dns); };
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
    part(el, pre, "header").find("> table > thead > tr > th[data-field]").each(function () {
      var on = !!dir && this.getAttribute("data-field") === field;
      $(this).toggleClass(pre + "-sort-asc", on && dir === "asc")
        .toggleClass(pre + "-sort-desc", on && dir === "desc");
      if (on) { this.setAttribute("aria-sort", dir === "asc" ? "ascending" : "descending"); }
      else { this.removeAttribute("aria-sort"); }
    });
  }

  function isOff(el) { return el.getAttribute("aria-disabled") === "true"; }

  // Elements inside a cell that keep their own clicks.
  var OWN = "a, button, input, select, textarea, label";

  // ------------------------------------------------------------------
  // TreeGrid
  // ------------------------------------------------------------------

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
      var st = { id: ++seq, anchor: null, stopResize: null };
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

  // ------------------------------------------------------------------
  // DataTable
  // ------------------------------------------------------------------

  function dtBody(el) { return document.getElementById(el.id + "-rows"); }
  function dtRows(el) { return $(dtBody(el)).children("tr.ah-dt-row"); }
  function dtShown(el) { return dtRows(el).filter(function () { return !this.hidden; }).toArray(); }
  function dtRemote(el) { return el.getAttribute("data-mode") === "remote"; }
  function dtByKey(el, key) {
    key = String(key);
    return dtRows(el).filter(function () { return this.getAttribute("data-key") === key; })[0] || null;
  }
  function dtDetail(el, key) {
    return $(dtBody(el)).children("tr.ah-dt-row-details").filter(function () {
      return this.getAttribute("data-key") === key;
    })[0] || null;
  }
  function dtHeaderRow(el) { return part(el, "ah-dt", "header").find("> table > thead > tr.ah-dt-header-row")[0]; }
  function dtTh(el, field) {
    return $(dtHeaderRow(el)).children("th[data-field]").filter(function () {
      return this.getAttribute("data-field") === field;
    })[0] || null;
  }
  function dtCell(tr, field) {
    return $(tr).children("td[data-field]").filter(function () {
      return this.getAttribute("data-field") === field;
    })[0] || null;
  }
  function num(v, d) { var n = parseInt(v, 10); return n > 0 ? n : d; }

  // Column filters from the filter row: text, or {condition, value}.
  function dtFilters(el) {
    var f = {};
    var $h = part(el, "ah-dt", "header");
    $h.find(".ah-dt-filter-input").each(function () {
      if (this.value) { f[this.getAttribute("data-field")] = this.value; }
    });
    $h.find(".ah-dt-adv-filter-select").each(function () {
      var cond = this.value;
      var v = $(this).siblings(".ah-dt-adv-filter-input").val() || "";
      if (cond === "empty" || cond === "not_empty") { f[this.getAttribute("data-field")] = { condition: cond, value: "" }; }
      else if (v) { f[this.getAttribute("data-field")] = { condition: cond, value: v }; }
    });
    return f;
  }

  function dtSearch(el) {
    var $s = part(el, "ah-dt", "header").find(".ah-dt-search-input");
    return $s.length ? $s.val() : (el.getAttribute("data-search") || "");
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

  // The pager's view data, as aihtml_data_tables:pager_view/5 builds it.
  function pagerView(page, size, total, sizes, t) {
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
    var all = sizes.concat([size]).filter(function (s, j, a) { return a.indexOf(s) === j; })
      .sort(function (a, b) { return a - b; });
    return {
      info: info, prev_label: t.prev, next_label: t.next, size_label: t.size,
      prev_disabled: page <= 1, next_disabled: page >= pages,
      buttons: btns.map(function (b) {
        return b === 0 ? { gap: true, page: 0, active: false } : { gap: false, page: b, active: b === page };
      }),
      has_sizes: sizes.length > 0,
      sizes: all.map(function (s) { return { size: s, selected: s === size }; })
    };
  }

  function dtPager(el, page, size, total) {
    var $c = $(el).children(".ah-dt-pager-container");
    if (!$c.length || !AH.tpl || !AH.tpl.datatable_pager) { return; }
    var sizes = ($c.attr("data-sizes") || "").split(",").filter(Boolean).map(Number);
    var t = { info: $c.attr("data-info"), prev: $c.attr("data-prev"), next: $c.attr("data-next"),
              size: $c.attr("data-size") };
    $c.html(AH.tpl.datatable_pager(pagerView(page, size, total, sizes, t)));
  }

  function dtStripes(el, shown) {
    var alt = el.getAttribute("data-alt-rows") === "true";
    shown.forEach(function (r, i) { $(r).toggleClass("ah-dt-row-alt", alt && i % 2 === 1); });
    $(dtBody(el)).children("tr.ah-dt-row-empty").prop("hidden", shown.length > 0);
    var $rows = dtRows(el);
    var stop = $rows.filter("[tabindex='0']")[0];
    if (!stop || stop.hidden) { focusRow($rows, shown[0], false); }
    headerCheck(part(el, "ah-dt", "header").find(".ah-dt-header-checkbox"), keysOf(el),
                shown.map(function (r) { return r.getAttribute("data-key"); }));
  }

  // Local mode: search, filters, sort and page over the rendered rows.
  function dtApply(el) {
    var body = dtBody(el);
    var rows = dtRows(el).toArray();
    var fields = [], types = {};
    $(dtHeaderRow(el)).children("th[data-field]").each(function () {
      fields.push(this.getAttribute("data-field"));
      types[this.getAttribute("data-field")] = this.getAttribute("data-type");
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
    var end = $(body).children("tr.ah-dt-row-empty")[0] || null;
    match.concat(rest).forEach(function (r) {
      body.insertBefore(r, end);
      var d = dtDetail(el, r.getAttribute("data-key"));
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
      var d = dtDetail(el, r.getAttribute("data-key"));
      if (d) { d.hidden = !on; }
    });
    dtStripes(el, shown);
    if (size) { dtPager(el, page, size, total); }
  }

  // Remote mode: the state goes on the root and the server answers.
  function dtQuery(el) {
    var f = dtFilters(el);
    if (Object.keys(f).length) { el.setAttribute("data-filters", JSON.stringify(f)); }
    else { el.removeAttribute("data-filters"); }
    var s = dtSearch(el);
    if (s) { el.setAttribute("data-search", s); } else { el.removeAttribute("data-search"); }
    $(el).children(".ah-dt-content").addClass("ah-dt-loading");
    $(el).trigger("ah:query", [{ sort: el.getAttribute("data-sort-field"), dir: el.getAttribute("data-sort-dir"),
                                 page: num(el.getAttribute("data-page"), 1),
                                 pageSize: num(el.getAttribute("data-page-size"), 0) || null,
                                 search: s, filters: f }]);
  }

  function dtView(el) {
    if (dtRemote(el)) { dtQuery(el); } else { dtApply(el); }
  }

  function dtSelect(el, keys, user) {
    var prev = keysOf(el);
    markRows(dtRows(el), "ah-dt", keys, el.getAttribute("data-selection"));
    writeValue(el, keys);
    headerCheck(part(el, "ah-dt", "header").find(".ah-dt-header-checkbox"), keys,
                dtShown(el).map(function (r) { return r.getAttribute("data-key"); }));
    if (user && !sameKeys(prev, keys)) { $(el).trigger("change"); }
  }

  function dtEvent(el, name, key, extra) {
    el.setAttribute("data-key", key);
    $(el).trigger(name, [$.extend({ key: key }, extra || {})]);
  }

  function dtSetDetails(el, key, open, user) {
    var tr = dtByKey(el, key), d = dtDetail(el, key);
    if (!tr || !d) { return; }
    var was = !$(d).hasClass("ah-dt-row-details-hidden");
    if (was === open) { return; }
    $(d).toggleClass("ah-dt-row-details-hidden", !open);
    $(tr).find("> td > .ah-dt-expand-btn").toggleClass("ah-dt-expand-btn-open", open)
      .attr("aria-expanded", String(open));
    var list = (el.getAttribute("data-expanded") || "").split(",").filter(function (k) { return k && k !== key; });
    if (open) { list.push(key); }
    if (list.length) { el.setAttribute("data-expanded", list.join(",")); } else { el.removeAttribute("data-expanded"); }
    if (user) { dtEvent(el, open ? "ah:row-expand" : "ah:row-collapse", key); }
  }

  function dtSetHidden(el, field, hide) {
    var th = dtTh(el, field);
    if (!th) { return; }
    var row = th.parentNode;
    var idx = Array.prototype.indexOf.call(row.children, th);
    var $both = part(el, "ah-dt", "header").add(part(el, "ah-dt", "body"));
    $both.find("> table > colgroup").each(function () {
      var c = this.children[idx];
      if (c) { c.hidden = hide; }
    });
    th.hidden = hide;
    part(el, "ah-dt", "header").find("> table > thead > tr.ah-dt-filter-row").each(function () {
      if (this.children[idx]) { this.children[idx].hidden = hide; }
    });
    dtRows(el).each(function () { var c = dtCell(this, field); if (c) { c.hidden = hide; } });
    var hidden = $(row).children("th[data-field]").filter(function () { return this.hidden; })
      .map(function () { return this.getAttribute("data-field"); }).get();
    if (hidden.length) { el.setAttribute("data-hidden", hidden.join(",")); } else { el.removeAttribute("data-hidden"); }
    var span = $(row).children("th").filter(function () { return !this.hidden; }).length;
    $(dtBody(el)).find("> tr > td.ah-dt-cell-empty, > tr > td.ah-dt-row-details-cell").attr("colspan", span);
    $(el).children(".ah-dt-chooser-panel").find(".ah-dt-chooser-checkbox").each(function () {
      if (this.getAttribute("data-field") === field) { this.checked = !hide; }
    });
    $(el).trigger("ah:columns", [{ hidden: hidden }]);
  }

  // ---- inline editing ------------------------------------------------

  function dtEditable(el) { return el.getAttribute("data-editable") === "true"; }

  function dtBeginEdit(el, st, td) {
    if (!td || !dtEditable(el) || isOff(el)) { return; }
    if (st.edit) { dtEndEdit(el, st, true); }
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
  function dtEndEdit(el, st, commit) {
    var ed = st.edit;
    if (!ed) { return null; }
    st.edit = null;
    var value = ed.type === "checkbox" ? String(ed.input.checked) : ed.input.value;
    if (!commit || value === ed.old) {
      ed.td.innerHTML = ed.html;
      return ed.td;
    }
    ed.td.innerHTML = "";
    $("<span>").text(value).appendTo(ed.td);
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
    $(ed.td).trigger("ah:cell-edit", [{ key: key, field: ed.field, value: value, old: ed.old }]);
    return ed.td;
  }

  function dtEditKey(el, st, e) {
    var ed = st.edit;
    if (!ed || e.target !== ed.input) { return; }
    e.stopPropagation();
    if (e.key === "Escape") {
      e.preventDefault();
      dtEndEdit(el, st, false);
      focusRow(dtRows(el), ed.tr, true);
    } else if (e.key === "Enter") {
      e.preventDefault();
      dtEndEdit(el, st, true);
      focusRow(dtRows(el), ed.tr, true);
    } else if (e.key === "Tab") {
      e.preventDefault();
      var cells = [];
      dtShown(el).forEach(function (r) {
        $(r).children("td.ah-dt-cell-editable").each(function () { if (!this.hidden) { cells.push(this); } });
      });
      var i = cells.indexOf(ed.td);
      var next = cells[i + (e.shiftKey ? -1 : 1)];
      dtEndEdit(el, st, true);
      if (next) { dtBeginEdit(el, st, next); } else { focusRow(dtRows(el), ed.tr, true); }
    }
  }

  // ---- column chooser -------------------------------------------------

  function dtChooser(el, st, open) {
    var $p = $(el).children(".ah-dt-chooser-panel");
    var btn = part(el, "ah-dt", "header").find(".ah-dt-chooser-btn")[0];
    if (!$p.length || !btn) { return; }
    if (st.float) { st.float.stop(); st.float = null; }
    $(document).off(".ahdtbch" + st.id);
    $p.toggleClass("ah-dt-chooser-panel-open", open);
    btn.setAttribute("aria-expanded", String(open));
    if (!open) { return; }
    st.float = AH.float($p[0], btn, { placement: "bottom", align: "end", offset: 4 });
    $(document).on("mousedown.ahdtbch" + st.id, function (e) {
      if (!$.contains($p[0], e.target) && !$.contains(btn, e.target) && e.target !== btn) {
        dtChooser(el, st, false);
      }
    }).on("keydown.ahdtbch" + st.id, function (e) {
      if (e.key === "Escape") { dtChooser(el, st, false); btn.focus(); }
    });
  }

  // ---- events -----------------------------------------------------------

  function dtKeydown(el, st, e) {
    var tr = e.target;
    if (!$(tr).is("tr.ah-dt-row") || tr.parentNode !== dtBody(el) || isOff(el) ||
        e.altKey || e.metaKey) { return; }
    var vis = dtShown(el);
    var i = vis.indexOf(tr);
    var key = tr.getAttribute("data-key");
    var to = null;
    switch (e.key) {
      case "ArrowDown": to = vis[Math.min(i + 1, vis.length - 1)]; break;
      case "ArrowUp": to = vis[Math.max(i - 1, 0)]; break;
      case "Home": to = vis[0]; break;
      case "End": to = vis[vis.length - 1]; break;
      case "PageDown": to = vis[Math.min(i + 10, vis.length - 1)]; break;
      case "PageUp": to = vis[Math.max(i - 10, 0)]; break;
      case "ArrowRight": dtSetDetails(el, key, true, true); break;
      case "ArrowLeft": dtSetDetails(el, key, false, true); break;
      case "F2":
      case "Enter":
        if (dtEditable(el)) {
          var cell = $(tr).children("td.ah-dt-cell-editable").filter(function () { return !this.hidden; })[0];
          if (cell) { dtBeginEdit(el, st, cell); break; }
        }
        if (e.key === "F2") { break; }
        /* falls through */
      case " ":
        var next = keySelect(el.getAttribute("data-selection"), keysOf(el), key);
        if (next) { dtSelect(el, next, true); st.anchor = key; }
        break;
      default:
        return;
    }
    e.preventDefault();
    if (to) { focusRow(dtRows(el), to, true); }
  }

  function dtClick(el, st, e) {
    var tr = $(e.target).closest("tr.ah-dt-row")[0];
    if (!tr || tr.parentNode !== dtBody(el) || isOff(el)) { return; }
    var key = tr.getAttribute("data-key");
    if ($(e.target).closest(".ah-dt-expand-btn").length) {
      dtSetDetails(el, key, !$(e.target).closest(".ah-dt-expand-btn").hasClass("ah-dt-expand-btn-open"), true);
      return;
    }
    if (st.edit && $.contains(st.edit.td, e.target)) { return; }
    focusRow(dtRows(el), tr, false);
    if ($(e.target).closest(OWN).length) { return; }
    var next = clickSelect(el.getAttribute("data-selection"), keysOf(el), key, e, st.anchor,
                           dtShown(el).map(function (r) { return r.getAttribute("data-key"); }));
    if (next) {
      dtSelect(el, next, true);
      if (!e.shiftKey) { st.anchor = key; }
    }
    dtEvent(el, "ah:row-click", key);
  }

  function dtGoTo(el, page) {
    el.setAttribute("data-page", Math.max(1, page));
    dtView(el);
    $(el).trigger("ah:page", [{ page: num(el.getAttribute("data-page"), 1),
                                pageSize: num(el.getAttribute("data-page-size"), 0) }]);
  }

  function dtFiltered(el) {
    el.setAttribute("data-page", 1);
    dtView(el);
    $(el).trigger("ah:filter", [{ filters: dtFilters(el), search: dtSearch(el) }]);
  }

  AH.define("datatable", {
    init: function (el, $el) {
      var st = { id: ++seq, anchor: null, edit: null, float: null, timer: null, stopResize: null };
      $.data(el, "ah-datatable", st);
      var debounced = function () {
        clearTimeout(st.timer);
        st.timer = setTimeout(function () { dtFiltered(el); }, 200);
      };
      $el.on("click" + NS, "tbody > tr.ah-dt-row", function (e) { dtClick(el, st, e); });
      $el.on("dblclick" + NS, "tbody > tr.ah-dt-row", function (e) {
        if (this.parentNode !== dtBody(el)) { return; }
        var td = $(e.target).closest("td.ah-dt-cell-editable")[0];
        if (td && td.parentNode === this) { dtBeginEdit(el, st, td); }
        dtEvent(el, "ah:row-dblclick", this.getAttribute("data-key"));
      });
      $el.on("keydown" + NS, "tbody > tr.ah-dt-row", function (e) { dtKeydown(el, st, e); });
      $el.on("keydown" + NS, ".ah-dt-editor", function (e) { dtEditKey(el, st, e); });
      $el.on("focusout" + NS, ".ah-dt-editor", function () {
        var ed = st.edit;
        setTimeout(function () { if (st.edit === ed && ed) { dtEndEdit(el, st, true); } }, 0);
      });
      $el.on("change" + NS, ".ah-dt-row-checkbox", function (e) {
        e.stopPropagation();
        var k = $(this).closest("tr")[0].getAttribute("data-key");
        var rest = keysOf(el).filter(function (x) { return x !== k; });
        dtSelect(el, this.checked ? rest.concat([k]) : rest, true);
      });
      $el.on("change" + NS, ".ah-dt-header-checkbox", function (e) {
        e.stopPropagation();
        var page = dtShown(el).map(function (r) { return r.getAttribute("data-key"); });
        var rest = keysOf(el).filter(function (k) { return page.indexOf(k) < 0; });
        dtSelect(el, this.checked ? rest.concat(page) : rest, true);
      });
      $el.on("click" + NS, ".ah-dt-th-sortable", function (e) {
        if ($(e.target).closest(".ah-dt-resize-handle").length || isOff(el)) { return; }
        var field = this.getAttribute("data-field");
        var dir = nextSort(el, field);
        writeSort(el, "ah-dt", dir ? field : null, dir);
        el.setAttribute("data-page", 1);
        dtView(el);
        $el.trigger("ah:sort", [{ field: field, dir: dir }]);
      });
      $el.on("keydown" + NS, ".ah-dt-th-sortable", function (e) {
        if (e.key === "Enter" || e.key === " ") { e.preventDefault(); $(this).trigger("click"); }
      });
      $el.on("input" + NS, ".ah-dt-filter-input, .ah-dt-adv-filter-input, .ah-dt-search-input", function (e) {
        e.stopPropagation();
        debounced();
      });
      $el.on("change" + NS, ".ah-dt-filter-input, .ah-dt-adv-filter-input, .ah-dt-search-input", function (e) {
        e.stopPropagation();
      });
      $el.on("change" + NS, ".ah-dt-adv-filter-select", function (e) {
        e.stopPropagation();
        var none = this.value === "empty" || this.value === "not_empty";
        var $in = $(this).siblings(".ah-dt-adv-filter-input").prop("disabled", none);
        if (none) { $in.val(""); }
        dtFiltered(el);
      });
      $el.on("click" + NS, ".ah-dt-pager-btn-num", function () { dtGoTo(el, num(this.getAttribute("data-page"), 1)); });
      $el.on("click" + NS, ".ah-dt-pager-btn-prev", function () { dtGoTo(el, num(el.getAttribute("data-page"), 1) - 1); });
      $el.on("click" + NS, ".ah-dt-pager-btn-next", function () { dtGoTo(el, num(el.getAttribute("data-page"), 1) + 1); });
      $el.on("change" + NS, ".ah-dt-pager-size-select", function (e) {
        e.stopPropagation();
        el.setAttribute("data-page-size", num(this.value, 10));
        dtGoTo(el, 1);
      });
      $el.on("click" + NS, ".ah-dt-chooser-btn", function () {
        dtChooser(el, st, !$el.children(".ah-dt-chooser-panel").hasClass("ah-dt-chooser-panel-open"));
      });
      $el.on("change" + NS, ".ah-dt-chooser-checkbox", function (e) {
        e.stopPropagation();
        dtSetHidden(el, this.getAttribute("data-field"), !this.checked);
      });
      $el.on("ah:error" + NS, function () { $el.children(".ah-dt-content").removeClass("ah-dt-loading"); });
      bindScroll(el, "ah-dt");
      bindResize(el, $el, "ah-dt", st);
      watchGutter(el, "ah-dt", st);
      if (dtRemote(el)) { dtStripes(el, dtShown(el)); } else { dtApply(el); }
    },
    destroy: function (el) {
      var st = $.data(el, "ah-datatable");
      if (st) {
        clearTimeout(st.timer);
        if (st.float) { st.float.stop(); }
        if (st.stopResize) { st.stopResize(); }
        if (st.ro) { st.ro.disconnect(); }
        $(document).off(".ahdtbch" + st.id);
      }
      part(el, "ah-dt", "body").off(NS);
    },
    methods: {
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      setValue: function (el, $el, v) {
        dtSelect(el, toKeys(v).filter(function (k) { return dtByKey(el, k); }), false);
      },
      clearSelection: function (el) { dtSelect(el, [], false); },
      sort: function (el, $el, field, dir) {
        dir = dir === "asc" || dir === "desc" ? dir : null;
        writeSort(el, "ah-dt", dir ? field : null, dir);
        el.setAttribute("data-page", 1);
        dtView(el);
      },
      goToPage: function (el, $el, page) { el.setAttribute("data-page", num(page, 1)); dtView(el); },
      setPageSize: function (el, $el, size) {
        el.setAttribute("data-page-size", num(size, 10));
        el.setAttribute("data-page", 1);
        dtView(el);
      },
      setSearch: function (el, $el, text) {
        part(el, "ah-dt", "header").find(".ah-dt-search-input").val(text || "");
        if (text) { el.setAttribute("data-search", text); } else { el.removeAttribute("data-search"); }
        el.setAttribute("data-page", 1);
        dtView(el);
      },
      clearFilters: function (el) {
        var $h = part(el, "ah-dt", "header");
        $h.find(".ah-dt-filter-input, .ah-dt-adv-filter-input, .ah-dt-search-input").val("");
        $h.find(".ah-dt-adv-filter-select").val("contains");
        $h.find(".ah-dt-adv-filter-input").prop("disabled", false);
        el.removeAttribute("data-search");
        el.setAttribute("data-page", 1);
        dtView(el);
      },
      showColumn: function (el, $el, field) { dtSetHidden(el, String(field), false); },
      hideColumn: function (el, $el, field) { dtSetHidden(el, String(field), true); },
      expandRow: function (el, $el, key) { dtSetDetails(el, String(key), true, false); },
      collapseRow: function (el, $el, key) { dtSetDetails(el, String(key), false, false); },
      refresh: function (el) {
        markRows(dtRows(el), "ah-dt", keysOf(el), el.getAttribute("data-selection"));
        (el.getAttribute("data-hidden") || "").split(",").filter(Boolean).forEach(function (f) {
          dtRows(el).each(function () { var c = dtCell(this, f); if (c) { c.hidden = true; } });
        });
        (el.getAttribute("data-expanded") || "").split(",").filter(Boolean).forEach(function (k) {
          var d = dtDetail(el, k);
          if (d && $(d).hasClass("ah-dt-row-details-hidden")) {
            $(d).removeClass("ah-dt-row-details-hidden");
            $(dtByKey(el, k)).find("> td > .ah-dt-expand-btn").addClass("ah-dt-expand-btn-open")
              .attr("aria-expanded", "true");
          }
        });
        if (dtRemote(el)) { dtStripes(el, dtShown(el)); } else { dtApply(el); }
      }
    }
  });
})(window.jQuery, window.AH);
