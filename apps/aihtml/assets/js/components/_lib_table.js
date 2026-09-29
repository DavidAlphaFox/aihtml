/* Shared by the treegrid and datatable behaviours (treegrid.js,
 * datatable.js): selection by row keys, sorting order, roving focus, the
 * header / body scroll and gutter sync and column resizing. Ported from
 * sigil (data/treegrid, data/datatable). Loaded before the component
 * files (build-js concatenates in name order).
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  // A fresh id per table instance (namespaces its document handlers).
  function nextId() { return ++seq; }

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

  AH.lib = AH.lib || {};
  AH.lib.table = {
    nextId: nextId,
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
    bindScroll: bindScroll,
    syncGutter: syncGutter,
    watchGutter: watchGutter,
    bindResize: bindResize,
    nextSort: nextSort,
    writeSort: writeSort,
    isOff: isOff,
    OWN: OWN
  };
})(window.jQuery, window.AH);
