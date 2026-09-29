/* Behaviour of the gantt (designs/04-components.md). Ported from sigil:
 * data/gantt (+ timeline, rendering, interactions).
 *
 * aihtml_gantt renders every row, bar, summary bar and dependency
 * path on the server. The behaviour syncs the scroll areas, collapses rows
 * (hiding rows and bars, moving bars up, showing summary bars), drags bars
 * and recomputes the positions and dependency paths of the existing
 * elements; it builds no HTML.
 *
 * Times are minutes since 1970-01-01 in UTC arithmetic: task times are
 * local wall times without a zone, so there is no DST shifting. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var DAY = 1440;

  function pad(n) { return (n < 10 ? "0" : "") + n; }
  // ISO date or date-time -> { t: minutes, dateOnly }
  function parseT(s) {
    var m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2}))?/.exec(String(s || ""));
    if (!m) { return null; }
    var t = Math.round(Date.UTC(+m[1], +m[2] - 1, +m[3]) / 60000);
    return m[4] === undefined ? { t: t, dateOnly: true } : { t: t + (+m[4]) * 60 + (+m[5]), dateOnly: false };
  }
  function fmtT(t, dateOnly) {
    var d = new Date(t * 60000);
    var s = d.getUTCFullYear() + "-" + pad(d.getUTCMonth() + 1) + "-" + pad(d.getUTCDate());
    return dateOnly && t % DAY === 0 ? s : s + "T" + pad(d.getUTCHours()) + ":" + pad(d.getUTCMinutes());
  }
  function num(x) { return Math.round(x * 1000) / 1000; }

  function conf(el) {
    return {
      cw: parseFloat(el.getAttribute("data-column-width")) || 60,
      rh: parseFloat(el.getAttribute("data-row-height")) || 40,
      origin: (parseT(el.getAttribute("data-origin")) || { t: 0 }).t,
      editable: el.hasAttribute("data-editable")
    };
  }

  function sidebarRows(el) { return $(el).find(".ah-gantt-sidebar-row"); }
  function barOf(el, id) {
    return $(el).find(".ah-gantt-task-bar").filter(function () {
      return this.getAttribute("data-taskid") === String(id);
    })[0];
  }
  function times(bar) {
    var s = parseT(bar.getAttribute("data-start")), e = parseT(bar.getAttribute("data-end"));
    return { s: s.t, e: e.t, sd: s.dateOnly, ed: e.dateOnly };
  }
  function pos(c, s, e) {
    return { left: (s - c.origin) / DAY * c.cw, width: Math.max((e - s) / DAY * c.cw, c.cw) };
  }

  // Row visibility from the expanded state of the parents (tree order):
  // hides rows, grid rows and bars, moves bars, shows summary bars of
  // collapsed rows, then redraws the dependency lines.
  function layout(el) {
    var c = conf(el), open = {}, index = {}, parent = {}, n = 0, collapsed = [];
    sidebarRows(el).each(function () {
      var id = this.getAttribute("data-rowid"), p = this.getAttribute("data-parent");
      var vis = !p || open[p] === true;
      parent[id] = p;
      this.hidden = !vis;
      var expanded = this.getAttribute("aria-expanded");
      if (expanded === "false") { collapsed.push(id); }
      open[id] = vis && expanded !== "false";
      if (vis) { index[id] = n++; }
    });
    el.setAttribute("data-collapsed", AH.lib.values.join(collapsed));
    $(el).find(".ah-gantt-grid-row").each(function () {
      this.hidden = !(this.getAttribute("data-rowid") in index);
    });
    var $bars = $(el).find(".ah-gantt-task-bar");
    $bars.each(function () {
      var r = this.getAttribute("data-rowid"), t = times(this), p = pos(c, t.s, t.e);
      this.hidden = !(r in index);
      this.style.left = num(p.left) + "px";
      this.style.width = num(p.width) + "px";
      if (r in index) { this.style.top = (index[r] * c.rh + 4) + "px"; }
    });
    function under(r, anc) {
      for (var p = parent[r]; p; p = parent[p]) { if (p === anc) { return true; } }
      return false;
    }
    $(el).find(".ah-gantt-summary-bar").each(function () {
      var r = this.getAttribute("data-rowid"), s = Infinity, e = -Infinity, k = 0;
      $bars.each(function () {
        if (under(this.getAttribute("data-rowid"), r)) {
          var t = times(this);
          s = Math.min(s, t.s); e = Math.max(e, t.e); k++;
        }
      });
      var show = k > 0 && (r in index) && collapsed.indexOf(r) >= 0;
      this.hidden = !show;
      if (show) {
        var p = pos(c, s, e);
        this.style.left = num(p.left) + "px";
        this.style.width = num(p.width) + "px";
        this.style.top = (index[r] * c.rh + 4) + "px";
      }
    });
    var h = n * c.rh;
    $(el).find(".ah-gantt-tasks-layer").css("height", h + "px");
    var svg = $(el).find(".ah-gantt-deps-layer")[0];
    if (svg) {
      var w = parseFloat($(el).find(".ah-gantt-tasks-layer")[0].style.width) || 0;
      svg.style.height = h + "px";
      svg.setAttribute("viewBox", "0 0 " + w + " " + h);
      drawDeps(el, c, index);
    }
  }

  // sigil's render-dependency-line (twin of dep_d/5 in the Erlang module)
  function depD(x1, y1, x2, y2, same) {
    if (same) { return "M" + num(x1) + "," + num(y1) + " L" + num(x2) + "," + num(y2); }
    if (x1 >= x2) {
      var ext = 30 + (x1 - x2) / 3;
      return "M" + num(x1) + "," + num(y1) + " C" + num(x1 + ext) + "," + num(y1) + " " +
        num(x2 - ext) + "," + num(y2) + " " + num(x2) + "," + num(y2);
    }
    var mx = (x1 + x2) / 2;
    return "M" + num(x1) + "," + num(y1) + " C" + num(mx) + "," + num(y1) + " " +
      num(mx) + "," + num(y2) + " " + num(x2) + "," + num(y2);
  }

  function drawDeps(el, c, index) {
    $(el).find(".ah-gantt-dep-line").each(function () {
      var a = barOf(el, this.getAttribute("data-from")), b = barOf(el, this.getAttribute("data-to"));
      var ra = a && a.getAttribute("data-rowid"), rb = b && b.getAttribute("data-rowid");
      if (!a || !b || !(ra in index) || !(rb in index)) { this.setAttribute("d", ""); return; }
      var ta = times(a), tb = times(b), pa = pos(c, ta.s, ta.e), pb = pos(c, tb.s, tb.e);
      this.setAttribute("d", depD(pa.left + pa.width, index[ra] * c.rh + c.rh / 2,
                                  pb.left, index[rb] * c.rh + c.rh / 2, ra === rb));
    });
  }

  // ---- rows ---------------------------------------------------------

  function visibleRows(el) { return sidebarRows(el).filter(function () { return !this.hidden; }); }

  function setExpanded(el, row, open, notify) {
    if (!row || row.getAttribute("aria-expanded") === null) { return; }
    if ((row.getAttribute("aria-expanded") === "true") === open) { return; }
    row.setAttribute("aria-expanded", open ? "true" : "false");
    $(row).find(".ah-gantt-expand-icon").toggleClass("ah-gantt-expand-icon-expanded", open);
    layout(el);
    if (notify) {
      el.setAttribute("data-row", row.getAttribute("data-rowid"));
      el.setAttribute("data-expanded", open ? "true" : "false");
      $(el).trigger("ah:row-expand", [{ row: row.getAttribute("data-rowid"), expanded: open }]);
    }
  }

  function rowById(el, id) {
    return sidebarRows(el).filter(function () { return this.getAttribute("data-rowid") === String(id); })[0];
  }

  function focusRow(el, row) {
    if (!row) { return; }
    sidebarRows(el).attr("tabindex", "-1");
    row.setAttribute("tabindex", "0");
    row.focus();
  }

  function rowKey(el, row, e) {
    var $vis = visibleRows(el), i = $vis.index(row), open = row.getAttribute("aria-expanded");
    switch (e.key) {
      case "ArrowDown": focusRow(el, $vis[i + 1]); break;
      case "ArrowUp": focusRow(el, $vis[i - 1]); break;
      case "Home": focusRow(el, $vis[0]); break;
      case "End": focusRow(el, $vis[$vis.length - 1]); break;
      case "ArrowRight":
        if (open === "false") { setExpanded(el, row, true, true); }
        else if (open === "true") { focusRow(el, $vis[i + 1]); }
        break;
      case "ArrowLeft":
        if (open === "true") { setExpanded(el, row, false, true); }
        else { focusRow(el, rowById(el, row.getAttribute("data-parent"))); }
        break;
      case "Enter": case " ": rowClick(el, row); break;
      default: return;
    }
    e.preventDefault();
  }

  function rowClick(el, row) {
    el.setAttribute("data-row", row.getAttribute("data-rowid"));
    $(el).trigger("ah:row-click", [{ row: row.getAttribute("data-rowid") }]);
  }

  // ---- tasks --------------------------------------------------------

  function taskClick(el, bar) {
    el.setAttribute("data-task", bar.getAttribute("data-taskid"));
    $(el).trigger("ah:task-click", [{ task: bar.getAttribute("data-taskid") }]);
  }

  // Apply a change to a bar and fire ah:task-change.
  function change(el, bar, s, e, row, kind, days, notify) {
    var t = times(bar);
    bar.setAttribute("data-start", fmtT(s, t.sd));
    bar.setAttribute("data-end", fmtT(e, t.ed));
    if (row) { bar.setAttribute("data-rowid", row); }
    layout(el);
    if (!notify) { return; }
    var d = {
      task: bar.getAttribute("data-taskid"), from: bar.getAttribute("data-start"),
      to: bar.getAttribute("data-end"), row: bar.getAttribute("data-rowid"), kind: kind, days: days
    };
    $.each(d, function (k, v) { el.setAttribute("data-" + k, String(v)); });
    $(el).trigger("ah:task-change", [d]);
  }

  function taskKey(el, bar, e) {
    var c = conf(el);
    if (e.key === "Enter" || e.key === " ") { e.preventDefault(); taskClick(el, bar); return; }
    if (!c.editable) { return; }
    var t = times(bar), step = 0;
    if (e.key === "ArrowLeft") { step = -1; } else if (e.key === "ArrowRight") { step = 1; }
    if (step && e.shiftKey) {
      e.preventDefault();
      if (t.e + step * DAY > t.s) { change(el, bar, t.s, t.e + step * DAY, null, "resize", step, true); }
    } else if (step) {
      e.preventDefault();
      change(el, bar, t.s + step * DAY, t.e + step * DAY, null, "move", step, true);
    } else if (e.altKey && (e.key === "ArrowUp" || e.key === "ArrowDown")) {
      e.preventDefault();
      var $vis = visibleRows(el), i = $vis.index(rowById(el, bar.getAttribute("data-rowid")));
      var target = $vis[i + (e.key === "ArrowUp" ? -1 : 1)];
      if (target) { change(el, bar, t.s, t.e, target.getAttribute("data-rowid"), "move", 0, true); }
    }
  }

  // One drag at a time: mousedown on a bar decides move or resize; the
  // document's mousemove/mouseup finish it (sigil's setup-task-drag!).
  function dragStart(el, e) {
    var c = conf(el), $t = $(e.target), bar = $t.closest(".ah-gantt-task-bar")[0];
    if (!bar || e.button !== 0 || !c.editable) { return; }
    e.preventDefault();
    var st = $.data(el, "ah-gantt"), rect = bar.getBoundingClientRect();
    var d = { bar: bar, x: e.pageX, y: e.pageY, moved: false,
              left: parseFloat(bar.style.left), width: rect.width };
    if ($t.hasClass("ah-gantt-resize-handle")) {
      d.mode = "resize";
      d.side = $t.hasClass("ah-gantt-resize-left") ? "left" : "right";
    } else {
      d.mode = "move";
      d.ox = e.clientX - rect.left;
      d.oy = e.clientY - rect.top;
    }
    st.drag = d;
    $(document).on("mousemove" + st.ns, function (me) { dragMove(el, c, me); })
      .on("mouseup" + st.ns, function (ue) { dragEnd(el, c, ue); });
  }

  function dragMove(el, c, e) {
    var d = $.data(el, "ah-gantt").drag;
    if (!d) { return; }
    var dx = e.pageX - d.x, dy = e.pageY - d.y;
    if (!d.moved && Math.abs(dx) + Math.abs(dy) < 4) { return; }
    d.moved = true;
    if (d.mode === "move") {
      if (!d.ghost) {
        d.ghost = $(d.bar).clone().removeAttr("tabindex id").addClass("ah-gantt-task-ghost")
          .css({ position: "fixed", zIndex: 9999, opacity: 0.6, pointerEvents: "none", margin: 0,
                 width: d.width + "px", height: d.bar.offsetHeight + "px" })
          .appendTo(document.body);
      }
      d.ghost.css({ left: (e.clientX - d.ox) + "px", top: (e.clientY - d.oy) + "px" });
    } else if (d.side === "right") {
      d.bar.style.width = Math.max(c.cw, d.width + dx) + "px";
      d.dx = dx;
    } else {
      var dxl = Math.min(dx, d.width - c.cw);
      d.bar.style.left = (d.left + dxl) + "px";
      d.bar.style.width = (d.width - dxl) + "px";
      d.dx = dxl;
    }
  }

  function dragEnd(el, c, e) {
    var st = $.data(el, "ah-gantt"), d = st.drag;
    $(document).off("mousemove" + st.ns).off("mouseup" + st.ns);
    st.drag = null;
    if (!d) { return; }
    if (d.ghost) { d.ghost.remove(); }
    if (!d.moved) { return; }
    st.noClick = true;
    setTimeout(function () { st.noClick = false; }, 0);
    var t = times(d.bar);
    if (d.mode === "move") {
      var days = Math.round((e.pageX - d.x) / c.cw);
      var $vis = visibleRows(el), i = $vis.index(rowById(el, d.bar.getAttribute("data-rowid")));
      var j = Math.max(0, Math.min($vis.length - 1, i + Math.round((e.pageY - d.y) / c.rh)));
      var row = $vis[j] && $vis[j].getAttribute("data-rowid");
      var rowChanged = row && row !== d.bar.getAttribute("data-rowid");
      if (days || rowChanged) {
        change(el, d.bar, t.s + days * DAY, t.e + days * DAY, rowChanged ? row : null, "move", days, true);
      }
    } else {
      var n = Math.round((d.dx || 0) / c.cw);
      if (d.side === "right" && t.e + n * DAY <= t.s) { n = Math.ceil((t.s - t.e) / DAY) + 1; }
      if (d.side === "left" && t.s + n * DAY >= t.e) { n = Math.floor((t.e - t.s) / DAY) - 1; }
      if (n) {
        change(el, d.bar, d.side === "left" ? t.s + n * DAY : t.s,
               d.side === "right" ? t.e + n * DAY : t.e, null, "resize", n, true);
      } else {
        layout(el);
      }
    }
  }

  function scrollToTime(el, t) {
    var c = conf(el), body = $(el).find(".ah-gantt-timeline-body")[0];
    if (!body) { return; }
    body.scrollLeft = Math.max(0, (t - c.origin) / DAY * c.cw - 100);
    $(el).find(".ah-gantt-timeline-header")[0].scrollLeft = body.scrollLeft;
  }

  AH.define("gantt", {
    init: function (el, $el) {
      var st = { ns: NS + "-gantt" + Math.random().toString(36).slice(2) };
      $.data(el, "ah-gantt", st);
      var $body = $el.find(".ah-gantt-timeline-body"), $side = $el.find(".ah-gantt-sidebar-body");
      var $head = $el.find(".ah-gantt-timeline-header");
      $body.on("scroll" + NS, function () {
        $side[0].scrollTop = this.scrollTop;
        $head[0].scrollLeft = this.scrollLeft;
      });
      $side.on("scroll" + NS, function () { $body[0].scrollTop = this.scrollTop; });
      $el.on("click" + NS, ".ah-gantt-expand-icon", function (e) {
        e.stopPropagation();
        var row = $(this).closest(".ah-gantt-sidebar-row")[0];
        setExpanded(el, row, row.getAttribute("aria-expanded") === "false", true);
      });
      $el.on("click" + NS, ".ah-gantt-sidebar-row", function () {
        focusRow(el, this);
        rowClick(el, this);
      });
      $el.on("keydown" + NS, ".ah-gantt-sidebar-row", function (e) { rowKey(el, this, e); });
      $el.on("click" + NS, ".ah-gantt-task-bar", function (e) {
        e.stopPropagation();
        if (!st.noClick) { taskClick(el, this); }
      });
      $el.on("keydown" + NS, ".ah-gantt-task-bar", function (e) { taskKey(el, this, e); });
      $body.on("mousedown" + NS, function (e) { dragStart(el, e); });
      // sigil scrolls to the first task
      var first = Infinity;
      $el.find(".ah-gantt-task-bar").each(function () { first = Math.min(first, times(this).s); });
      // re-initialised after a morph (gantt_update): keep the scroll position
      if (el.ahKeepScroll) { delete el.ahKeepScroll; } else if (first < Infinity) { scrollToTime(el, first); }
    },
    destroy: function (el) {
      var st = $.data(el, "ah-gantt");
      if (st) {
        $(document).off(st.ns);
        if (st.drag && st.drag.ghost) { st.drag.ghost.remove(); }
      }
      $(el).find(".ah-gantt-timeline-body, .ah-gantt-sidebar-body").off(NS);
      $.removeData(el, "ah-gantt");
      el.ahKeepScroll = true;
    },
    methods: {
      expandRow: function (el, $el, id) { setExpanded(el, rowById(el, id), true, false); },
      collapseRow: function (el, $el, id) { setExpanded(el, rowById(el, id), false, false); },
      scrollToDate: function (el, $el, iso) { var p = parseT(iso); if (p) { scrollToTime(el, p.t); } },
      setTask: function (el, $el, id, from, to, row) {
        var bar = barOf(el, id), a = parseT(from), b = parseT(to);
        if (bar && a && b) {
          bar.setAttribute("data-start", fmtT(a.t, a.dateOnly));
          bar.setAttribute("data-end", fmtT(b.t, b.dateOnly));
          if (row) { bar.setAttribute("data-rowid", row); }
          layout(el);
        }
      }
    }
  });
})(window.jQuery, window.AH);
