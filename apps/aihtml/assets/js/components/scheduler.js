/* Behaviour of the scheduler (designs/04-components.md). Ported from
 * sigil: data/scheduler (+ core, toolbar, drag, contextmenu, the +more
 * popover of month_view).
 *
 * Every view is rendered on the server (aihtml_scheduler). Navigation
 * does not render here: the toolbar writes the new date, view and range
 * to the root and fires change; the `source' action answers with
 * scheduler_update/3, which morphs the new view in. Drags and keyboard
 * moves reposition the existing elements (time grid columns, month week
 * rows, timeline rows), recompute the overlap columns and fire
 * ah:event-change; the context menu and the "+n more" popover are cloned
 * from server-rendered templates.
 *
 * Times are minutes since 1970-01-01, days are day numbers, both in UTC
 * arithmetic (local wall times without a zone; AH.lib.date). */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var DAY = 1440;
  var EVENT_SEL = ".ah-scheduler-timegrid-event, .ah-scheduler-allday-event, .ah-scheduler-month-event, " +
    ".ah-scheduler-timeline-event, .ah-scheduler-agenda-event, .ah-scheduler-more-popover-item";
  var COL_SEL = ".ah-scheduler-dayview-col, .ah-scheduler-dayview-res-col";

  var D = AH.lib.date;
  var pad = D.pad, dnum = D.dnum, ymd = D.ymd, sow = D.sow,
      firstOfMonth = D.firstOfMonth, lastOfMonth = D.lastOfMonth, addMonths = D.addMonths,
      todayNum = D.todayNum;
  // (the year is not padded to four digits here, unlike D.isoDate)
  function isoDate(n) { var p = ymd(n); return p[0] + "-" + pad(p[1]) + "-" + pad(p[2]); }
  function parseDay(s) {
    var m = /^(\d{4})-(\d{2})-(\d{2})/.exec(s || "");
    return m ? dnum(+m[1], +m[2], +m[3]) : null;
  }
  function parseT(s) {
    var m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2}))?/.exec(String(s || ""));
    if (!m) { return null; }
    return dnum(+m[1], +m[2], +m[3]) * DAY + (m[4] === undefined ? 0 : (+m[4]) * 60 + (+m[5]));
  }
  function isoTime(t, allDay) {
    var s = isoDate(Math.floor(t / DAY));
    return allDay && t % DAY === 0 ? s : s + "T" + pad(Math.floor((t % DAY) / 60)) + ":" + pad(t % 60);
  }
  function num(x) { return Math.round(x * 1000) / 1000; }

  function conf(el) {
    var a = function (k, d) { var v = parseInt(el.getAttribute("data-" + k), 10); return isNaN(v) ? d : v; };
    return {
      view: el.getAttribute("data-view") || "week",
      date: parseDay(el.getAttribute("data-ah-value")) || todayNum(),
      first: a("first-day", 1), agenda: a("agenda-days", 30),
      sd: a("slot-duration", 30), sh: a("slot-height", 20),
      ds: a("day-start", 0), de: a("day-end", 24), hf: a("hour-format", 12),
      am: el.getAttribute("data-am") || "AM", pm: el.getAttribute("data-pm") || "PM",
      editable: el.hasAttribute("data-editable")
    };
  }

  function clock(t, c) {
    var h = Math.floor((t % DAY) / 60), m = pad(t % 60);
    if (c.hf === 24) { return pad(h) + ":" + m; }
    return ((h % 12) || 12) + ":" + m + " " + (h < 12 ? c.am : c.pm);
  }

  // The visible days [start, end) of a view (twin of profile/4).
  function profile(view, d, first, agenda) {
    switch (view) {
      case "day": case "timeline_day": return [d, d + 1];
      case "week": case "timeline_week": var s = sow(d, first); return [s, s + 7];
      case "month": return [sow(firstOfMonth(d), first), sow(lastOfMonth(d), first) + 7];
      case "timeline_month": return [firstOfMonth(d), lastOfMonth(d) + 1];
      default: return [d, d + agenda];
    }
  }

  function step(view, d, dir, agenda) {
    switch (view) {
      case "day": case "timeline_day": return d + dir;
      case "week": case "timeline_week": return d + 7 * dir;
      case "month": case "timeline_month": return addMonths(d, dir);
      default: return d + agenda * dir;
    }
  }

  function live(el, msg) { $(el).children(".ah-scheduler-live").text(msg); }

  // ---- navigation (the server renders the new range) -----------------

  function show(el, d, view) {
    var c = conf(el), r = profile(view, d, c.first, c.agenda);
    el.setAttribute("data-ah-value", isoDate(d));
    el.setAttribute("data-view", view);
    el.setAttribute("data-start", isoDate(r[0]));
    el.setAttribute("data-end", isoDate(r[1]));
    $(el).children("input[type=hidden]").val(isoDate(d));
    $(el).find(".ah-scheduler-view-btn").each(function () {
      var on = this.getAttribute("data-view") === view;
      $(this).toggleClass("ah-scheduler-view-btn-active", on).attr("aria-pressed", on ? "true" : "false");
    });
    closePopups(el);
    $(el).trigger("change");
  }

  function navigate(el, dir) {
    var c = conf(el);
    show(el, dir === "today" ? todayNum() : step(c.view, c.date, dir === "prev" ? -1 : 1, c.agenda), c.view);
  }

  // ---- appointment data ----------------------------------------------

  function info(ev) {
    return {
      event: ev.getAttribute("data-eventid"), source: ev.getAttribute("data-source"),
      from: ev.getAttribute("data-start"), to: ev.getAttribute("data-end"),
      resource: ev.getAttribute("data-resourceid") || "",
      allDay: ev.hasAttribute("data-all-day")
    };
  }

  function fire(el, name, data) {
    $.each(data, function (k, v) {
      el.setAttribute("data-" + k.replace(/[A-Z]/g, function (x) { return "-" + x.toLowerCase(); }), String(v));
    });
    $(el).trigger(name, [data]);
  }

  // ---- time grid ------------------------------------------------------

  function colWindow(col, c) {
    var d = parseDay(col.getAttribute("data-date"));
    return [d * DAY + c.ds * 60, d * DAY + c.de * 60];
  }

  // sigil's assign-columns over the events of one column
  function relayoutCol(col, c) {
    var w = colWindow(col, c);
    var items = $(col).children(".ah-scheduler-timegrid-event").map(function () {
      var s = Math.max(parseT(this.getAttribute("data-start")), w[0]);
      var e = Math.max(Math.min(parseT(this.getAttribute("data-end")), w[1]), s + c.sd);
      return { el: this, s: s, e: e };
    }).get().sort(function (a, b) { return a.s - b.s; });
    var cols = [];
    items.forEach(function (it) {
      var i = 0;
      for (; i < cols.length; i++) {
        if (!cols[i].some(function (o) { return it.s < o.e && it.e > o.s; })) { break; }
      }
      if (i === cols.length) { cols.push([]); }
      cols[i].push(it);
      it.col = i;
    });
    items.forEach(function (it) {
      $(it.el).css({
        top: num((it.s - w[0]) / c.sd * c.sh) + "px", height: num((it.e - it.s) / c.sd * c.sh) + "px",
        left: num(it.col * 100 / cols.length) + "%", width: num(100 / cols.length) + "%"
      });
    });
  }

  function findCol(el, day, res) {
    return $(el).find(COL_SEL).filter(function () {
      return this.getAttribute("data-date") === isoDate(day) &&
        (!res || this.getAttribute("data-resourceid") === res);
    })[0];
  }

  function at(sel, $scope, x, y) {
    var hit = null;
    $scope.find(sel).each(function () {
      var r = this.getBoundingClientRect();
      if (x >= r.left && x < r.right && (y === undefined || (y >= r.top && y < r.bottom))) { hit = this; return false; }
    });
    return hit;
  }

  // Put an appointment at new times (and resource) in the current view.
  function place(el, ev, s, e, res) {
    var c = conf(el), allDay = ev.hasAttribute("data-all-day");
    ev.setAttribute("data-start", isoTime(s, allDay));
    ev.setAttribute("data-end", isoTime(e, allDay));
    if (res !== undefined && res !== null) { ev.setAttribute("data-resourceid", res); }
    var focused = document.activeElement === ev;
    var $ev = $(ev);
    $ev.find(".ah-scheduler-event-time").first().text(
      $ev.hasClass("ah-scheduler-month-event") ? clock(s, c) : clock(s, c) + " – " + clock(e, c));
    if ($ev.hasClass("ah-scheduler-timegrid-event")) {
      var old = $ev.parent()[0];
      var col = findCol(el, Math.floor(s / DAY), ev.getAttribute("data-resourceid")) || old;
      if (col !== old) { col.appendChild(ev); relayoutCol(old, c); }
      relayoutCol(col, c);
    } else if ($ev.hasClass("ah-scheduler-month-event")) {
      var d0 = Math.floor(s / DAY), d1 = allDay ? Math.max(d0 + 1, Math.ceil(e / DAY)) : d0 + 1;
      var cell = $(el).find('.ah-scheduler-day[data-date="' + isoDate(d0) + '"]')[0];
      if (cell) {
        var row = $(cell).closest(".ah-scheduler-week-row")[0];
        var ws = parseDay($(row).find(".ah-scheduler-day").first().attr("data-date"));
        var sc = d0 - ws + 1, ec = Math.min(8, d1 - ws + 1);
        var content = $(row).children(".ah-scheduler-week-content")[0];
        if (ev.parentNode !== content) { content.appendChild(ev); }
        var used = {};
        $(content).children(".ah-scheduler-month-event").not(ev).each(function () {
          var g = /grid-column:\s*(\d+)\s*\/\s*(\d+).*grid-row:\s*(\d+)/.exec(this.getAttribute("style") || "");
          if (g && sc < +g[2] && ec > +g[1]) { used[g[3]] = true; }
        });
        var r = 2;
        while (used[r]) { r++; }
        ev.style.gridColumn = sc + "/" + ec;
        ev.style.gridRow = String(r);
      }
    } else if ($ev.hasClass("ah-scheduler-timeline-event")) {
      var grid = $(el).find(".ah-scheduler-timeline-grid")[0];
      var ts = parseT(grid.getAttribute("data-from")), te = parseT(grid.getAttribute("data-to")), tot = te - ts;
      var left = Math.max(0, s - ts) * 100 / tot, right = Math.min(tot, e - ts) * 100 / tot;
      ev.style.left = num(left) + "%";
      ev.style.width = num(Math.max(0.5, right - left)) + "%";
      var rid = ev.getAttribute("data-resourceid");
      var target = $(grid).find(".ah-scheduler-timeline-row").filter(function () {
        return (this.getAttribute("data-resourceid") || "") === (rid || "");
      }).children(".ah-scheduler-timeline-row-events")[0];
      if (target && ev.parentNode !== target) { target.appendChild(ev); }
    }
    if (focused) { ev.focus(); }
  }

  function changed(el, ev, kind) {
    var d = info(ev);
    d.kind = kind;
    live(el, $(ev).find(".ah-scheduler-event-title, .ah-scheduler-timeline-event-title").first().text() +
         ": " + d.from + " – " + d.to);
    fire(el, "ah:event-change", d);
  }

  // ---- drag (sigil's drag.cljs): one at a time ---------------------------

  function ghost(ev, e) {
    var r = ev.getBoundingClientRect();
    return {
      ox: e.clientX - r.left, oy: e.clientY - r.top,
      $g: $(ev).clone().removeAttr("tabindex").addClass("ah-scheduler-event-ghost")
        .css({ position: "fixed", zIndex: 9999, opacity: 0.7, pointerEvents: "none", margin: 0,
               width: r.width + "px", height: r.height + "px", left: r.left + "px", top: r.top + "px" })
    };
  }

  function snap(min, unit) { return Math.round(min / unit) * unit; }

  function dragStart(el, e) {
    var c = conf(el);
    if (e.button !== 0 || !c.editable) { return; }
    var $t = $(e.target), st = $.data(el, "ah-scheduler");
    var ev = $t.closest(".ah-scheduler-timegrid-event, .ah-scheduler-month-event, .ah-scheduler-timeline-event")[0];
    var d = { x: e.clientX, y: e.clientY, moved: false, ev: ev };
    if (ev) {
      d.s = parseT(ev.getAttribute("data-start"));
      d.e = parseT(ev.getAttribute("data-end"));
      if ($t.hasClass("ah-scheduler-timegrid-resize-handle")) {
        d.mode = "resize";
      } else if ($t.hasClass("ah-scheduler-timeline-resize-handle")) {
        d.mode = "tl-resize";
        d.side = $t.hasClass("ah-scheduler-timeline-resize-left") ? "left" : "right";
      } else {
        d.mode = "move";
      }
    } else {
      var col = $t.closest(COL_SEL)[0];
      if (!col) { return; }
      d.mode = "create";
      d.col = col;
    }
    e.preventDefault();
    st.drag = d;
    $(document).on("mousemove" + st.ns, function (me) { dragMove(el, c, me); })
      .on("mouseup" + st.ns, function (ue) { dragEnd(el, c, ue); });
  }

  function gridMinute(col, c, y) {
    var w = colWindow(col, c), r = col.getBoundingClientRect();
    return Math.max(w[0], Math.min(w[1], w[0] + snap((y - r.top) / c.sh * c.sd, c.sd)));
  }

  function tlTime(el, x, unit) {
    var grid = $(el).find(".ah-scheduler-timeline-grid")[0], r = grid.getBoundingClientRect();
    var ts = parseT(grid.getAttribute("data-from")), te = parseT(grid.getAttribute("data-to"));
    var t = ts + (x - r.left) / r.width * (te - ts);
    return ts + snap(Math.max(0, Math.min(te - ts, t - ts)), unit);
  }

  function tlUnit(el, c, ev) {
    if (c.view === "timeline_day") { return c.sd; }
    return ev && ev.hasAttribute("data-all-day") ? DAY : 60;
  }

  function dragMove(el, c, e) {
    var d = $.data(el, "ah-scheduler").drag;
    if (!d) { return; }
    if (!d.moved && Math.abs(e.clientX - d.x) + Math.abs(e.clientY - d.y) < 4) { return; }
    if (!d.moved && d.mode === "move") { d.g = ghost(d.ev, { clientX: d.x, clientY: d.y }); d.g.$g.appendTo(document.body); }
    d.moved = true;
    if (d.mode === "move") {
      d.g.$g.css({ left: (e.clientX - d.g.ox) + "px", top: (e.clientY - d.g.oy) + "px" });
    } else if (d.mode === "resize") {
      var col = d.ev.parentNode, w = colWindow(col, c);
      var end = Math.max(Math.max(d.s, w[0]) + c.sd, gridMinute(col, c, e.clientY));
      d.ev.style.height = num((end - Math.max(d.s, w[0])) / c.sd * c.sh) + "px";
      d.newEnd = end;
    } else if (d.mode === "tl-resize") {
      var t = tlTime(el, e.clientX, tlUnit(el, c, d.ev));
      if (d.side === "left") { d.ns = Math.min(t, d.e - tlUnit(el, c, d.ev)); d.ne = d.e; }
      else { d.ns = d.s; d.ne = Math.max(t, d.s + tlUnit(el, c, d.ev)); }
      var grid = $(el).find(".ah-scheduler-timeline-grid")[0];
      var ts = parseT(grid.getAttribute("data-from")), te = parseT(grid.getAttribute("data-to"));
      d.ev.style.left = num(Math.max(0, d.ns - ts) * 100 / (te - ts)) + "%";
      d.ev.style.width = num(Math.max(0.5, (Math.min(te, d.ne) - Math.max(ts, d.ns)) * 100 / (te - ts))) + "%";
    } else if (d.mode === "create") {
      var a = gridMinute(d.col, c, d.y), b = gridMinute(d.col, c, e.clientY);
      d.from = Math.min(a, b);
      d.to = Math.max(a, b) + c.sd;
      var w0 = colWindow(d.col, c)[0];
      if (!d.$ph) { d.$ph = $('<div class="ah-scheduler-create-placeholder"></div>').appendTo(d.col); }
      d.$ph.css({ left: 0, right: 0, top: num((d.from - w0) / c.sd * c.sh) + "px",
                  height: num((d.to - d.from) / c.sd * c.sh) + "px" });
    }
  }

  function dragEnd(el, c, e) {
    var st = $.data(el, "ah-scheduler"), d = st.drag;
    $(document).off("mousemove" + st.ns).off("mouseup" + st.ns);
    st.drag = null;
    if (!d) { return; }
    if (d.g) { d.g.$g.remove(); }
    if (d.$ph) { d.$ph.remove(); }
    if (!d.moved) {
      if (d.mode === "create") { return; }
      return;
    }
    st.noClick = true;
    setTimeout(function () { st.noClick = false; }, 0);
    var dur = d.e - d.s;
    if (d.mode === "move") {
      var $ev = $(d.ev);
      if ($ev.hasClass("ah-scheduler-timegrid-event")) {
        var col = at(COL_SEL, $(el), e.clientX);
        if (!col) { return; }
        var start = gridMinute(col, c, e.clientY - d.g.oy);
        place(el, d.ev, start, start + dur, col.getAttribute("data-resourceid"));
      } else if ($ev.hasClass("ah-scheduler-month-event")) {
        var day = at(".ah-scheduler-day", $(el), e.clientX, e.clientY);
        if (!day) { return; }
        var delta = parseDay(day.getAttribute("data-date")) - Math.floor(d.s / DAY);
        if (!delta) { return; }
        place(el, d.ev, d.s + delta * DAY, d.e + delta * DAY);
      } else {
        var s = tlTime(el, e.clientX - d.g.ox, tlUnit(el, c, d.ev));
        var row = at(".ah-scheduler-timeline-row", $(el), e.clientX, e.clientY);
        var res = row ? row.getAttribute("data-resourceid") : null;
        place(el, d.ev, s, s + dur, res);
      }
      changed(el, d.ev, "move");
    } else if (d.mode === "resize") {
      if (d.newEnd === undefined || d.newEnd === d.e) { relayoutCol(d.ev.parentNode, c); return; }
      place(el, d.ev, d.s, d.newEnd);
      changed(el, d.ev, "resize");
    } else if (d.mode === "tl-resize") {
      if (d.ns === undefined) { return; }
      place(el, d.ev, d.ns, d.ne);
      changed(el, d.ev, "resize");
    } else if (d.mode === "create" && d.from !== undefined) {
      fire(el, "ah:select", { from: isoTime(d.from), to: isoTime(d.to),
                              resource: d.col.getAttribute("data-resourceid") || "", allDay: false });
    }
  }

  // ---- keyboard moves ------------------------------------------------

  function eventKey(el, ev, e) {
    var c = conf(el), $ev = $(ev);
    if (e.key === "Enter" || e.key === " ") { e.preventDefault(); fire(el, "ah:event-click", info(ev)); return; }
    if ((e.key === "F10" && e.shiftKey) || e.key === "ContextMenu") {
      if (c.editable) {
        e.preventDefault();
        var r = ev.getBoundingClientRect();
        openMenu(el, "event", r.left + 4, r.bottom, ev);
      }
      return;
    }
    if (!c.editable || $ev.hasClass("ah-scheduler-more-popover-item")) { return; }
    if (e.key === "Delete") { e.preventDefault(); fire(el, "ah:event-delete", info(ev)); return; }
    var s = parseT(ev.getAttribute("data-start")), en = parseT(ev.getAttribute("data-end"));
    var k = e.key, ds = 0, de = 0, res = null;
    if ($ev.hasClass("ah-scheduler-timegrid-event")) {
      if (k === "ArrowUp" || k === "ArrowDown") {
        var n = (k === "ArrowUp" ? -1 : 1) * c.sd;
        if (e.shiftKey) { de = en + n > s ? n : 0; } else { ds = de = n; }
      } else if (k === "ArrowLeft" || k === "ArrowRight") { ds = de = (k === "ArrowLeft" ? -1 : 1) * DAY; }
    } else if ($ev.hasClass("ah-scheduler-month-event")) {
      var m = { ArrowLeft: -1, ArrowRight: 1, ArrowUp: -7, ArrowDown: 7 }[k];
      if (m) { ds = de = m * DAY; }
    } else if ($ev.hasClass("ah-scheduler-timeline-event")) {
      var u = tlUnit(el, c, ev);
      if (k === "ArrowLeft" || k === "ArrowRight") {
        var dir = k === "ArrowLeft" ? -1 : 1;
        if (e.shiftKey) { de = en + dir * u > s ? dir * u : 0; } else { ds = de = dir * u; }
      } else if (e.altKey && (k === "ArrowUp" || k === "ArrowDown")) {
        var $rows = $(el).find(".ah-scheduler-timeline-row");
        var i = $rows.index($ev.closest(".ah-scheduler-timeline-row"));
        var target = $rows[i + (k === "ArrowUp" ? -1 : 1)];
        if (target) { res = target.getAttribute("data-resourceid"); }
      }
    }
    if (!ds && !de && !res) { return; }
    e.preventDefault();
    place(el, ev, s + ds, en + de, res);
    changed(el, ev, ds === de ? "move" : "resize");
  }

  // ---- context menu and +more popover (cloned from templates) ----------

  function closePopups(el) {
    var st = $.data(el, "ah-scheduler");
    if (!st) { return; }
    if (st.menu) { st.menu.remove(); st.menu = null; }
    if (st.pop) {
      st.popFloat.stop();
      st.pop.remove();
      st.pop = null;
      if (st.popAnchor && document.contains(st.popAnchor)) { st.popAnchor.focus(); }
    }
  }

  function openMenu(el, kind, x, y, target) {
    closePopups(el);
    var tpl = $(el).children(".ah-scheduler-menu-" + kind)[0];
    if (!tpl) { return; }
    var st = $.data(el, "ah-scheduler");
    var $m = $('<div class="ah-scheduler-contextmenu" role="menu"></div>')
      .append(tpl.content.cloneNode(true))
      .css({ position: "fixed", left: x + "px", top: y + "px", zIndex: 10000 })
      .appendTo(el);
    st.menu = $m;
    st.menuTarget = target;
    $m.find(".ah-scheduler-contextmenu-item").first().trigger("focus");
  }

  function menuAction(el, item) {
    var st = $.data(el, "ah-scheduler"), action = item.getAttribute("data-action"), t = st.menuTarget;
    closePopups(el);
    if (action === "create") {
      fire(el, "ah:select", t);
    } else if (t) {
      fire(el, "ah:event-" + action, info(t));
      if (document.contains(t)) { t.focus(); }
    }
  }

  function openMore(el, more) {
    closePopups(el);
    var st = $.data(el, "ah-scheduler"), tpl = $(more).children("template")[0];
    if (!tpl) { return; }
    var $p = $('<div class="ah-scheduler-more-popover" role="dialog"></div>')
      .attr("aria-label", more.getAttribute("data-date"))
      .append(tpl.content.cloneNode(true)).appendTo(el);
    st.pop = $p;
    st.popAnchor = more;
    st.popFloat = AH.float($p[0], more, { placement: "bottom", align: "start", offset: 2 });
    $p.find(EVENT_SEL).first().trigger("focus");
    fire(el, "ah:more-click", { date: more.getAttribute("data-date") });
  }

  // ---- now indicator and first scroll ----------------------------------

  function nowLine(el) {
    var c = conf(el), $line = $(el).find(".ah-scheduler-dayview-now-indicator");
    if (!$line.length) { return; }
    var t = new Date(), today = isoDate(todayNum()), min = t.getHours() * 60 + t.getMinutes();
    var hasToday = $(el).find(COL_SEL).filter(function () {
      return this.getAttribute("data-date") === today;
    }).length > 0;
    if (hasToday && min >= c.ds * 60 && min < c.de * 60) {
      $line.css({ display: "block", top: num((min - c.ds * 60) / c.sd * c.sh) + "px" });
    } else {
      $line.css("display", "none");
    }
  }

  function firstScroll(el) {
    var c = conf(el), sc = $(el).find(".ah-scheduler-dayview-hscroll")[0];
    if (!sc) { return; }
    var t = new Date(), today = isoDate(todayNum()), target;
    if ($(el).find(COL_SEL + '[data-date="' + today + '"]').length) {
      target = t.getHours() * 60 + t.getMinutes() - 60;
    } else {
      target = Infinity;
      $(el).find(".ah-scheduler-timegrid-event").each(function () {
        target = Math.min(target, parseT(this.getAttribute("data-start")) % DAY - 30);
      });
      if (target === Infinity) { target = 8 * 60; }
    }
    sc.scrollTop = Math.max(0, (target - c.ds * 60) / c.sd * c.sh);
  }

  AH.define("scheduler", {
    init: function (el, $el) {
      var st = { ns: NS + "-sched" + Math.random().toString(36).slice(2) };
      $.data(el, "ah-scheduler", st);
      $el.on("click" + NS, ".ah-scheduler-btn-prev", function () { navigate(el, "prev"); });
      $el.on("click" + NS, ".ah-scheduler-btn-next", function () { navigate(el, "next"); });
      $el.on("click" + NS, ".ah-scheduler-btn-today", function () { navigate(el, "today"); });
      $el.on("click" + NS, ".ah-scheduler-view-btn", function () {
        show(el, conf(el).date, this.getAttribute("data-view"));
      });
      $el.on("click" + NS, EVENT_SEL, function (e) {
        e.stopPropagation();
        if (!st.noClick) { fire(el, "ah:event-click", info(this)); }
      });
      $el.on("dblclick" + NS, EVENT_SEL, function (e) {
        e.stopPropagation();
        if (conf(el).editable) { closePopups(el); fire(el, "ah:event-edit", info(this)); }
      });
      $el.on("keydown" + NS, EVENT_SEL, function (e) { eventKey(el, this, e); });
      $el.on("dblclick" + NS, COL_SEL, function (e) {
        if (!conf(el).editable || $(e.target).closest(EVENT_SEL).length) { return; }
        var c = conf(el), s = gridMinute(this, c, e.clientY);
        fire(el, "ah:select", { from: isoTime(s), to: isoTime(s + 60),
                                resource: this.getAttribute("data-resourceid") || "", allDay: false });
      });
      $el.on("contextmenu" + NS, ".ah-scheduler-view-container", function (e) {
        var c = conf(el);
        if (!c.editable) { return; }
        var ev = $(e.target).closest(EVENT_SEL)[0];
        var col = $(e.target).closest(COL_SEL)[0];
        if (!ev && !col) { return; }
        e.preventDefault();
        if (ev) {
          openMenu(el, "event", e.clientX, e.clientY, ev);
        } else {
          var s = gridMinute(col, c, e.clientY);
          openMenu(el, "cell", e.clientX, e.clientY,
                   { from: isoTime(s), to: isoTime(s + 60),
                     resource: col.getAttribute("data-resourceid") || "", allDay: false });
        }
      });
      $el.on("click" + NS, ".ah-scheduler-contextmenu-item", function (e) {
        e.stopPropagation();
        menuAction(el, this);
      });
      $el.on("keydown" + NS, ".ah-scheduler-contextmenu", function (e) {
        var $items = $(this).find(".ah-scheduler-contextmenu-item"), i = $items.index(document.activeElement);
        if (e.key === "ArrowDown") { $items.eq((i + 1) % $items.length).trigger("focus"); }
        else if (e.key === "ArrowUp") { $items.eq((i - 1 + $items.length) % $items.length).trigger("focus"); }
        else if (e.key === "Enter" || e.key === " ") { if (i >= 0) { menuAction(el, $items[i]); } }
        else if (e.key === "Escape" || e.key === "Tab") {
          var t = st.menuTarget;
          closePopups(el);
          if (t && t.nodeType === 1) { t.focus(); }
        } else { return; }
        e.preventDefault();
        e.stopPropagation();
      });
      $el.on("click" + NS, ".ah-scheduler-day-more", function (e) {
        e.stopPropagation();
        openMore(el, this);
      });
      $el.on("keydown" + NS, ".ah-scheduler-day-more", function (e) {
        if (e.key === "Enter" || e.key === " ") { e.preventDefault(); openMore(el, this); }
      });
      $el.on("click" + NS, ".ah-scheduler-more-popover-close", function (e) {
        e.stopPropagation();
        closePopups(el);
      });
      $el.on("keydown" + NS, ".ah-scheduler-more-popover", function (e) {
        if (e.key === "Escape") { e.preventDefault(); e.stopPropagation(); closePopups(el); }
        if (e.key === "Enter" && $(e.target).hasClass("ah-scheduler-more-popover-close")) { closePopups(el); }
      });
      $(document).on("mousedown" + st.ns, function (e) {
        if ((st.menu && !$.contains(st.menu[0], e.target)) ||
            (st.pop && !$.contains(st.pop[0], e.target))) { closePopups(el); }
      });
      $el.find(".ah-scheduler-view-container").on("mousedown" + NS, function (e) {
        if ($(e.target).closest(".ah-scheduler-contextmenu, .ah-scheduler-more-popover").length) { return; }
        dragStart(el, e);
      });
      var $tl = $el.find(".ah-scheduler-timeline-scroll");
      $tl.on("scroll" + NS, function () {
        $el.find(".ah-scheduler-timeline-slots-header")[0].style.transform = "translateX(" + (-this.scrollLeft) + "px)";
        $el.find(".ah-scheduler-timeline-resource-panel")[0].scrollTop = this.scrollTop;
      });
      // re-initialised after a morph (scheduler_update): the scroll areas
      // were kept, keep their position
      if (el.ahKeepScroll) { delete el.ahKeepScroll; } else { firstScroll(el); }
      nowLine(el);
      st.timer = setInterval(function () { nowLine(el); }, 60000);
    },
    destroy: function (el, $el) {
      var st = $.data(el, "ah-scheduler");
      if (st) {
        closePopups(el);
        clearInterval(st.timer);
        $(document).off(st.ns);
        if (st.drag && st.drag.g) { st.drag.g.$g.remove(); }
      }
      $el.find(".ah-scheduler-view-container, .ah-scheduler-timeline-scroll").off(NS);
      $.removeData(el, "ah-scheduler");
      el.ahKeepScroll = true;
    },
    methods: {
      navigate: function (el, $el, dir) { navigate(el, dir); },
      setView: function (el, $el, view) { show(el, conf(el).date, view); },
      gotoDate: function (el, $el, iso) { var d = parseDay(iso); if (d !== null) { show(el, d, conf(el).view); } },
      getValue: function (el) { return el.getAttribute("data-ah-value"); }
    }
  });
})(window.jQuery, window.AH);
