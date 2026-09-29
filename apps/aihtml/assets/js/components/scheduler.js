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
 * With the href option the toolbar entries are links (<a href>) to the
 * state they lead to (data-href on the root holds the template, {date}
 * and {view}). When the scheduler is bound to an action (a data-ah-on
 * change binding: `source' or a postback) a plain click navigates here as
 * the buttons do and pushes the link's URL (AH.apply url op; going back
 * reloads it, so the server renders that state); modified clicks and
 * unbound schedulers leave the link to the browser. After a navigation
 * the links are pointed at the new neighbours.
 *
 * Times are minutes since 1970-01-01, days are day numbers, both in UTC
 * arithmetic (local wall times without a zone; AH.lib.date).
 *
 * Events (detail, also written to the root as data-*): `change`;
 * ah:event-click / ah:event-edit / ah:event-delete / ah:event-copy
 * ({event, source, from, to, resource, allDay}); ah:event-change (the
 * same plus kind: "move" | "resize"); ah:select ({from, to, resource,
 * allDay}); ah:more-click ({date}). */
import AH from "../core.js";
import "./_lib_date.js";

var DAY = 1440;
var EVENT_SEL = ".ah-scheduler-timegrid-event, .ah-scheduler-allday-event, .ah-scheduler-month-event, " +
  ".ah-scheduler-timeline-event, .ah-scheduler-agenda-event, .ah-scheduler-more-popover-item";
var COL_SEL = ".ah-scheduler-dayview-col, .ah-scheduler-dayview-res-col";

var D = AH.lib.date;
var pad = D.pad, sow = D.sow,
    firstOfMonth = D.firstOfMonth, lastOfMonth = D.lastOfMonth, addMonths = D.addMonths,
    todayNum = D.todayNum;
var isoDate = D.isoDate, isoTime = D.isoTime;
// The day or the minute of an ISO date or date-time (the server writes
// them with aihtml_lib_date); null when it is not a valid one.
function parseT(s) { var r = D.parseTime(s); return r ? r.t : null; }
function parseDay(s) { var t = parseT(s); return t === null ? null : Math.floor(t / DAY); }
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

function live(el, msg) {
  el.querySelectorAll(":scope > .ah-scheduler-live").forEach(function (l) { l.textContent = msg; });
}

// ---- navigation (the server renders the new range) -----------------

function show(c, d, view) {
  var el = c.element, cf = conf(el), r = profile(view, d, cf.first, cf.agenda);
  el.setAttribute("data-ah-value", isoDate(d));
  el.setAttribute("data-view", view);
  el.setAttribute("data-start", isoDate(r[0]));
  el.setAttribute("data-end", isoDate(r[1]));
  var hidden = el.querySelector(":scope > input[type=hidden]");
  if (hidden) { hidden.value = isoDate(d); }
  el.querySelectorAll(".ah-scheduler-view-btn").forEach(function (b) {
    var on = b.getAttribute("data-view") === view;
    b.classList.toggle("ah-scheduler-view-btn-active", on);
    if (b.tagName === "A") {
      if (on) { b.setAttribute("aria-current", "true"); } else { b.removeAttribute("aria-current"); }
    } else {
      b.setAttribute("aria-pressed", on ? "true" : "false");
    }
  });
  links(el);
  closePopups(c);
  c.fire("change");
}

// ---- links (href option) --------------------------------------------

// Point the toolbar links at the states they lead to from the shown date
// and view (the twin of nav_url/3).
function links(el) {
  var tpl = el.getAttribute("data-href");
  if (!tpl) { return; }
  var cf = conf(el);
  var url = function (d, view) {
    return tpl.replace(/\{date\}/g, isoDate(d)).replace(/\{view\}/g, view);
  };
  var set = function (sel, d) {
    el.querySelectorAll("a" + sel).forEach(function (a) { a.setAttribute("href", url(d, cf.view)); });
  };
  set(".ah-scheduler-btn-prev", step(cf.view, cf.date, -1, cf.agenda));
  set(".ah-scheduler-btn-next", step(cf.view, cf.date, 1, cf.agenda));
  set(".ah-scheduler-btn-today", todayNum());
  el.querySelectorAll("a.ah-scheduler-view-btn").forEach(function (a) {
    a.setAttribute("href", url(cf.date, a.getAttribute("data-view")));
  });
}

// Whether a click on a toolbar entry is handled here. Buttons always are;
// a link only on a plain left click of a bound scheduler (a change
// action renders the new state), else the browser follows it. A handled
// link's default is prevented and its URL returned, to push after the
// navigation (which rewrites the links).
function claim(c, e, t) {
  if (t.tagName !== "A") { return ""; }
  var bound = /(^|\s)change:/.test(c.element.getAttribute("data-ah-on") || "");
  if (!bound || e.button !== 0 || e.ctrlKey || e.metaKey || e.shiftKey || e.altKey) { return null; }
  e.preventDefault();
  return t.getAttribute("href");
}

function pushUrl(url) {
  if (url) { AH.apply([{ op: "url", mode: "push", value: url }]); }
}

function navigate(c, dir) {
  var cf = conf(c.element);
  show(c, dir === "today" ? todayNum() : step(cf.view, cf.date, dir === "prev" ? -1 : 1, cf.agenda), cf.view);
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

function fire(c, name, data) {
  var el = c.element;
  Object.keys(data).forEach(function (k) {
    el.setAttribute("data-" + k.replace(/[A-Z]/g, function (x) { return "-" + x.toLowerCase(); }), String(data[k]));
  });
  c.fire(name, data);
}

function children(node, sel) {
  return Array.prototype.filter.call(node.children, function (k) { return k.matches(sel); });
}

// ---- time grid ------------------------------------------------------

function colWindow(col, cf) {
  var d = parseDay(col.getAttribute("data-date"));
  return [d * DAY + cf.ds * 60, d * DAY + cf.de * 60];
}

// sigil's assign-columns over the events of one column
function relayoutCol(col, cf) {
  var w = colWindow(col, cf);
  var items = children(col, ".ah-scheduler-timegrid-event").map(function (ev) {
    var s = Math.max(parseT(ev.getAttribute("data-start")), w[0]);
    var e = Math.max(Math.min(parseT(ev.getAttribute("data-end")), w[1]), s + cf.sd);
    return { el: ev, s: s, e: e };
  }).sort(function (a, b) { return a.s - b.s; });
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
    var s = it.el.style;
    s.top = num((it.s - w[0]) / cf.sd * cf.sh) + "px";
    s.height = num((it.e - it.s) / cf.sd * cf.sh) + "px";
    s.left = num(it.col * 100 / cols.length) + "%";
    s.width = num(100 / cols.length) + "%";
  });
}

function findCol(el, day, res) {
  return Array.prototype.find.call(el.querySelectorAll(COL_SEL), function (col) {
    return col.getAttribute("data-date") === isoDate(day) &&
      (!res || col.getAttribute("data-resourceid") === res);
  });
}

function at(sel, scope, x, y) {
  var all = scope.querySelectorAll(sel);
  for (var i = 0; i < all.length; i++) {
    var r = all[i].getBoundingClientRect();
    if (x >= r.left && x < r.right && (y === undefined || (y >= r.top && y < r.bottom))) { return all[i]; }
  }
  return null;
}

// Put an appointment at new times (and resource) in the current view.
function place(el, ev, s, e, res) {
  var cf = conf(el), allDay = ev.hasAttribute("data-all-day");
  ev.setAttribute("data-start", isoTime(s, allDay));
  ev.setAttribute("data-end", isoTime(e, allDay));
  if (res !== undefined && res !== null) { ev.setAttribute("data-resourceid", res); }
  var focused = document.activeElement === ev;
  var cl = ev.classList;
  var time = ev.querySelector(".ah-scheduler-event-time");
  if (time) {
    time.textContent = cl.contains("ah-scheduler-month-event") ? clock(s, cf) : clock(s, cf) + " – " + clock(e, cf);
  }
  if (cl.contains("ah-scheduler-timegrid-event")) {
    var old = ev.parentNode;
    var col = findCol(el, Math.floor(s / DAY), ev.getAttribute("data-resourceid")) || old;
    if (col !== old) { col.appendChild(ev); relayoutCol(old, cf); }
    relayoutCol(col, cf);
  } else if (cl.contains("ah-scheduler-month-event")) {
    var d0 = Math.floor(s / DAY), d1 = allDay ? Math.max(d0 + 1, Math.ceil(e / DAY)) : d0 + 1;
    var cell = el.querySelector('.ah-scheduler-day[data-date="' + isoDate(d0) + '"]');
    if (cell) {
      var row = cell.closest(".ah-scheduler-week-row");
      var ws = parseDay(row.querySelector(".ah-scheduler-day").getAttribute("data-date"));
      var sc = d0 - ws + 1, ec = Math.min(8, d1 - ws + 1);
      var content = children(row, ".ah-scheduler-week-content")[0];
      if (ev.parentNode !== content) { content.appendChild(ev); }
      var used = {};
      children(content, ".ah-scheduler-month-event").forEach(function (o) {
        if (o === ev) { return; }
        var g = /grid-column:\s*(\d+)\s*\/\s*(\d+).*grid-row:\s*(\d+)/.exec(o.getAttribute("style") || "");
        if (g && sc < +g[2] && ec > +g[1]) { used[g[3]] = true; }
      });
      var r = 2;
      while (used[r]) { r++; }
      ev.style.gridColumn = sc + "/" + ec;
      ev.style.gridRow = String(r);
    }
  } else if (cl.contains("ah-scheduler-timeline-event")) {
    var grid = el.querySelector(".ah-scheduler-timeline-grid");
    var ts = parseT(grid.getAttribute("data-from")), te = parseT(grid.getAttribute("data-to")), tot = te - ts;
    var left = Math.max(0, s - ts) * 100 / tot, right = Math.min(tot, e - ts) * 100 / tot;
    ev.style.left = num(left) + "%";
    ev.style.width = num(Math.max(0.5, right - left)) + "%";
    var rid = ev.getAttribute("data-resourceid");
    var trow = Array.prototype.find.call(grid.querySelectorAll(".ah-scheduler-timeline-row"), function (x) {
      return (x.getAttribute("data-resourceid") || "") === (rid || "");
    });
    var target = trow && children(trow, ".ah-scheduler-timeline-row-events")[0];
    if (target && ev.parentNode !== target) { target.appendChild(ev); }
  }
  if (focused) { ev.focus(); }
}

function changed(c, ev, kind) {
  var d = info(ev);
  d.kind = kind;
  var title = ev.querySelector(".ah-scheduler-event-title, .ah-scheduler-timeline-event-title");
  live(c.element, (title ? title.textContent : "") + ": " + d.from + " – " + d.to);
  fire(c, "ah:event-change", d);
}

// ---- drag (sigil's drag.cljs): one at a time ---------------------------

function ghost(ev, e) {
  var r = ev.getBoundingClientRect();
  var g = ev.cloneNode(true);
  g.removeAttribute("tabindex");
  g.classList.add("ah-scheduler-event-ghost");
  Object.assign(g.style, { position: "fixed", zIndex: 9999, opacity: 0.7, pointerEvents: "none", margin: 0,
                           width: r.width + "px", height: r.height + "px", left: r.left + "px", top: r.top + "px" });
  return { ox: e.clientX - r.left, oy: e.clientY - r.top, g: g };
}

function snap(min, unit) { return Math.round(min / unit) * unit; }

function dragStart(c, e) {
  var el = c.element, cf = conf(el), st = c.st;
  if (e.button !== 0 || !cf.editable) { return; }
  var t = e.target;
  var ev = t.closest(".ah-scheduler-timegrid-event, .ah-scheduler-month-event, .ah-scheduler-timeline-event");
  var d = { x: e.clientX, y: e.clientY, moved: false, ev: ev };
  if (ev) {
    d.s = parseT(ev.getAttribute("data-start"));
    d.e = parseT(ev.getAttribute("data-end"));
    if (t.classList.contains("ah-scheduler-timegrid-resize-handle")) {
      d.mode = "resize";
    } else if (t.classList.contains("ah-scheduler-timeline-resize-handle")) {
      d.mode = "tl-resize";
      d.side = t.classList.contains("ah-scheduler-timeline-resize-left") ? "left" : "right";
    } else {
      d.mode = "move";
    }
  } else {
    var col = t.closest(COL_SEL);
    if (!col) { return; }
    d.mode = "create";
    d.col = col;
  }
  e.preventDefault();
  st.drag = d;
  var ac = st.dragAc = new AbortController();
  document.addEventListener("mousemove", function (me) { dragMove(c, cf, me); }, { signal: ac.signal });
  document.addEventListener("mouseup", function (ue) { dragEnd(c, cf, ue); }, { signal: ac.signal });
}

function gridMinute(col, cf, y) {
  var w = colWindow(col, cf), r = col.getBoundingClientRect();
  return Math.max(w[0], Math.min(w[1], w[0] + snap((y - r.top) / cf.sh * cf.sd, cf.sd)));
}

function tlTime(el, x, unit) {
  var grid = el.querySelector(".ah-scheduler-timeline-grid"), r = grid.getBoundingClientRect();
  var ts = parseT(grid.getAttribute("data-from")), te = parseT(grid.getAttribute("data-to"));
  var t = ts + (x - r.left) / r.width * (te - ts);
  return ts + snap(Math.max(0, Math.min(te - ts, t - ts)), unit);
}

function tlUnit(el, cf, ev) {
  if (cf.view === "timeline_day") { return cf.sd; }
  return ev && ev.hasAttribute("data-all-day") ? DAY : 60;
}

function dragMove(c, cf, e) {
  var el = c.element, d = c.st.drag;
  if (!d) { return; }
  if (!d.moved && Math.abs(e.clientX - d.x) + Math.abs(e.clientY - d.y) < 4) { return; }
  if (!d.moved && d.mode === "move") { d.g = ghost(d.ev, { clientX: d.x, clientY: d.y }); document.body.appendChild(d.g.g); }
  d.moved = true;
  if (d.mode === "move") {
    d.g.g.style.left = (e.clientX - d.g.ox) + "px";
    d.g.g.style.top = (e.clientY - d.g.oy) + "px";
  } else if (d.mode === "resize") {
    var col = d.ev.parentNode, w = colWindow(col, cf);
    var end = Math.max(Math.max(d.s, w[0]) + cf.sd, gridMinute(col, cf, e.clientY));
    d.ev.style.height = num((end - Math.max(d.s, w[0])) / cf.sd * cf.sh) + "px";
    d.newEnd = end;
  } else if (d.mode === "tl-resize") {
    var t = tlTime(el, e.clientX, tlUnit(el, cf, d.ev));
    if (d.side === "left") { d.ns = Math.min(t, d.e - tlUnit(el, cf, d.ev)); d.ne = d.e; }
    else { d.ns = d.s; d.ne = Math.max(t, d.s + tlUnit(el, cf, d.ev)); }
    var grid = el.querySelector(".ah-scheduler-timeline-grid");
    var ts = parseT(grid.getAttribute("data-from")), te = parseT(grid.getAttribute("data-to"));
    d.ev.style.left = num(Math.max(0, d.ns - ts) * 100 / (te - ts)) + "%";
    d.ev.style.width = num(Math.max(0.5, (Math.min(te, d.ne) - Math.max(ts, d.ns)) * 100 / (te - ts))) + "%";
  } else if (d.mode === "create") {
    var a = gridMinute(d.col, cf, d.y), b = gridMinute(d.col, cf, e.clientY);
    d.from = Math.min(a, b);
    d.to = Math.max(a, b) + cf.sd;
    var w0 = colWindow(d.col, cf)[0];
    if (!d.ph) {
      d.ph = document.createElement("div");
      d.ph.className = "ah-scheduler-create-placeholder";
      d.col.appendChild(d.ph);
    }
    Object.assign(d.ph.style, { left: "0px", right: "0px", top: num((d.from - w0) / cf.sd * cf.sh) + "px",
                                height: num((d.to - d.from) / cf.sd * cf.sh) + "px" });
  }
}

function dragStop(st) {
  if (st.dragAc) { st.dragAc.abort(); st.dragAc = null; }
  var d = st.drag;
  st.drag = null;
  if (d && d.g) { d.g.g.remove(); }
  if (d && d.ph) { d.ph.remove(); }
  return d;
}

function dragEnd(c, cf, e) {
  var el = c.element, st = c.st, d = dragStop(st);
  if (!d || !d.moved) { return; }
  st.noClick = true;
  setTimeout(function () { st.noClick = false; }, 0);
  var dur = d.e - d.s;
  if (d.mode === "move") {
    var cl = d.ev.classList;
    if (cl.contains("ah-scheduler-timegrid-event")) {
      var col = at(COL_SEL, el, e.clientX);
      if (!col) { return; }
      var start = gridMinute(col, cf, e.clientY - d.g.oy);
      place(el, d.ev, start, start + dur, col.getAttribute("data-resourceid"));
    } else if (cl.contains("ah-scheduler-month-event")) {
      var day = at(".ah-scheduler-day", el, e.clientX, e.clientY);
      if (!day) { return; }
      var delta = parseDay(day.getAttribute("data-date")) - Math.floor(d.s / DAY);
      if (!delta) { return; }
      place(el, d.ev, d.s + delta * DAY, d.e + delta * DAY);
    } else {
      var s = tlTime(el, e.clientX - d.g.ox, tlUnit(el, cf, d.ev));
      var row = at(".ah-scheduler-timeline-row", el, e.clientX, e.clientY);
      var res = row ? row.getAttribute("data-resourceid") : null;
      place(el, d.ev, s, s + dur, res);
    }
    changed(c, d.ev, "move");
  } else if (d.mode === "resize") {
    if (d.newEnd === undefined || d.newEnd === d.e) { relayoutCol(d.ev.parentNode, cf); return; }
    place(el, d.ev, d.s, d.newEnd);
    changed(c, d.ev, "resize");
  } else if (d.mode === "tl-resize") {
    if (d.ns === undefined) { return; }
    place(el, d.ev, d.ns, d.ne);
    changed(c, d.ev, "resize");
  } else if (d.mode === "create" && d.from !== undefined) {
    fire(c, "ah:select", { from: isoTime(d.from), to: isoTime(d.to),
                           resource: d.col.getAttribute("data-resourceid") || "", allDay: false });
  }
}

// ---- keyboard moves ------------------------------------------------

function eventKey(c, ev, e) {
  var el = c.element, cf = conf(el), cl = ev.classList;
  if (e.key === "Enter" || e.key === " ") { e.preventDefault(); fire(c, "ah:event-click", info(ev)); return; }
  if ((e.key === "F10" && e.shiftKey) || e.key === "ContextMenu") {
    if (cf.editable) {
      e.preventDefault();
      var r = ev.getBoundingClientRect();
      openMenu(c, "event", r.left + 4, r.bottom, ev);
    }
    return;
  }
  if (!cf.editable || cl.contains("ah-scheduler-more-popover-item")) { return; }
  if (e.key === "Delete") { e.preventDefault(); fire(c, "ah:event-delete", info(ev)); return; }
  var s = parseT(ev.getAttribute("data-start")), en = parseT(ev.getAttribute("data-end"));
  var k = e.key, ds = 0, de = 0, res = null;
  if (cl.contains("ah-scheduler-timegrid-event")) {
    if (k === "ArrowUp" || k === "ArrowDown") {
      var n = (k === "ArrowUp" ? -1 : 1) * cf.sd;
      if (e.shiftKey) { de = en + n > s ? n : 0; } else { ds = de = n; }
    } else if (k === "ArrowLeft" || k === "ArrowRight") { ds = de = (k === "ArrowLeft" ? -1 : 1) * DAY; }
  } else if (cl.contains("ah-scheduler-month-event")) {
    var m = { ArrowLeft: -1, ArrowRight: 1, ArrowUp: -7, ArrowDown: 7 }[k];
    if (m) { ds = de = m * DAY; }
  } else if (cl.contains("ah-scheduler-timeline-event")) {
    var u = tlUnit(el, cf, ev);
    if (k === "ArrowLeft" || k === "ArrowRight") {
      var dir = k === "ArrowLeft" ? -1 : 1;
      if (e.shiftKey) { de = en + dir * u > s ? dir * u : 0; } else { ds = de = dir * u; }
    } else if (e.altKey && (k === "ArrowUp" || k === "ArrowDown")) {
      var rows = Array.from(el.querySelectorAll(".ah-scheduler-timeline-row"));
      var i = rows.indexOf(ev.closest(".ah-scheduler-timeline-row"));
      var target = i < 0 ? null : rows[i + (k === "ArrowUp" ? -1 : 1)];
      if (target) { res = target.getAttribute("data-resourceid"); }
    }
  }
  if (!ds && !de && !res) { return; }
  e.preventDefault();
  place(el, ev, s + ds, en + de, res);
  changed(c, ev, ds === de ? "move" : "resize");
}

// ---- context menu and +more popover (cloned from templates) ----------

function closePopups(c) {
  var st = c.st;
  if (!st) { return; }
  if (st.menu) { st.menu.remove(); st.menu = null; }
  if (st.pop) {
    st.popFloat.stop();
    st.pop.remove();
    st.pop = null;
    if (st.popAnchor && document.contains(st.popAnchor)) { st.popAnchor.focus(); }
  }
}

function openMenu(c, kind, x, y, target) {
  var el = c.element, st = c.st;
  closePopups(c);
  var tpl = el.querySelector(":scope > .ah-scheduler-menu-" + kind);
  if (!tpl) { return; }
  var m = document.createElement("div");
  m.className = "ah-scheduler-contextmenu";
  m.setAttribute("role", "menu");
  m.appendChild(tpl.content.cloneNode(true));
  Object.assign(m.style, { position: "fixed", left: x + "px", top: y + "px", zIndex: 10000 });
  el.appendChild(m);
  st.menu = m;
  st.menuTarget = target;
  var first = m.querySelector(".ah-scheduler-contextmenu-item");
  if (first) { first.focus(); }
}

function menuAction(c, item) {
  var st = c.st, action = item.getAttribute("data-action"), t = st.menuTarget;
  closePopups(c);
  if (action === "create") {
    fire(c, "ah:select", t);
  } else if (t) {
    fire(c, "ah:event-" + action, info(t));
    if (document.contains(t)) { t.focus(); }
  }
}

function openMore(c, more) {
  var el = c.element, st = c.st;
  closePopups(c);
  var tpl = children(more, "template")[0];
  if (!tpl) { return; }
  var p = document.createElement("div");
  p.className = "ah-scheduler-more-popover";
  p.setAttribute("role", "dialog");
  p.setAttribute("aria-label", more.getAttribute("data-date"));
  p.appendChild(tpl.content.cloneNode(true));
  el.appendChild(p);
  st.pop = p;
  st.popAnchor = more;
  st.popFloat = AH.float(p, more, { placement: "bottom", align: "start", offset: 2 });
  var first = p.querySelector(EVENT_SEL);
  if (first) { first.focus(); }
  fire(c, "ah:more-click", { date: more.getAttribute("data-date") });
}

// ---- now indicator and first scroll ----------------------------------

function nowLine(el) {
  var cf = conf(el), lines = el.querySelectorAll(".ah-scheduler-dayview-now-indicator");
  if (!lines.length) { return; }
  var t = new Date(), today = isoDate(todayNum()), min = t.getHours() * 60 + t.getMinutes();
  var hasToday = Array.prototype.some.call(el.querySelectorAll(COL_SEL), function (col) {
    return col.getAttribute("data-date") === today;
  });
  lines.forEach(function (line) {
    if (hasToday && min >= cf.ds * 60 && min < cf.de * 60) {
      line.style.display = "block";
      line.style.top = num((min - cf.ds * 60) / cf.sd * cf.sh) + "px";
    } else {
      line.style.display = "none";
    }
  });
}

function firstScroll(el) {
  var cf = conf(el), sc = el.querySelector(".ah-scheduler-dayview-hscroll");
  if (!sc) { return; }
  var t = new Date(), today = isoDate(todayNum()), target;
  if (el.querySelector(".ah-scheduler-dayview-col[data-date=\"" + today + "\"], " +
                      ".ah-scheduler-dayview-res-col[data-date=\"" + today + "\"]")) {
    target = t.getHours() * 60 + t.getMinutes() - 60;
  } else {
    target = Infinity;
    el.querySelectorAll(".ah-scheduler-timegrid-event").forEach(function (ev) {
      target = Math.min(target, parseT(ev.getAttribute("data-start")) % DAY - 30);
    });
    if (target === Infinity) { target = 8 * 60; }
  }
  sc.scrollTop = Math.max(0, (target - cf.ds * 60) / cf.sd * cf.sh);
}

// The server morphs a new range into the kept root (scheduler_update):
// the controller stays, its listeners are delegated from the root, and
// the scroll areas keep their position; only the now line is redrawn.
AH.register("scheduler", class extends AH.Controller {
  setup() {
    var c = this, el = this.element;
    var st = this.st = { drag: null, dragAc: null, menu: null, pop: null, noClick: false };
    var toolbar = function (sel, go) {
      c.delegate("click", sel, function (e, t) {
        var url = claim(c, e, t);
        if (url === null) { return; }
        go(t);
        pushUrl(url);
      });
    };
    toolbar(".ah-scheduler-btn-prev", function () { navigate(c, "prev"); });
    toolbar(".ah-scheduler-btn-next", function () { navigate(c, "next"); });
    toolbar(".ah-scheduler-btn-today", function () { navigate(c, "today"); });
    toolbar(".ah-scheduler-view-btn", function (b) { show(c, conf(el).date, b.getAttribute("data-view")); });
    this.delegate("click", EVENT_SEL, function (e, ev) {
      e.stopPropagation();
      if (!st.noClick) { fire(c, "ah:event-click", info(ev)); }
    });
    this.delegate("dblclick", EVENT_SEL, function (e, ev) {
      e.stopPropagation();
      if (conf(el).editable) { closePopups(c); fire(c, "ah:event-edit", info(ev)); }
    });
    this.delegate("keydown", EVENT_SEL, function (e, ev) { eventKey(c, ev, e); });
    this.delegate("dblclick", COL_SEL, function (e, col) {
      if (!conf(el).editable || e.target.closest(EVENT_SEL)) { return; }
      var cf = conf(el), s = gridMinute(col, cf, e.clientY);
      fire(c, "ah:select", { from: isoTime(s), to: isoTime(s + 60),
                             resource: col.getAttribute("data-resourceid") || "", allDay: false });
    });
    this.delegate("contextmenu", ".ah-scheduler-view-container", function (e) {
      var cf = conf(el);
      if (!cf.editable) { return; }
      var ev = e.target.closest(EVENT_SEL);
      var col = e.target.closest(COL_SEL);
      if (!ev && !col) { return; }
      e.preventDefault();
      if (ev) {
        openMenu(c, "event", e.clientX, e.clientY, ev);
      } else {
        var s = gridMinute(col, cf, e.clientY);
        openMenu(c, "cell", e.clientX, e.clientY,
                 { from: isoTime(s), to: isoTime(s + 60),
                   resource: col.getAttribute("data-resourceid") || "", allDay: false });
      }
    });
    this.delegate("click", ".ah-scheduler-contextmenu-item", function (e, item) {
      e.stopPropagation();
      menuAction(c, item);
    });
    this.delegate("keydown", ".ah-scheduler-contextmenu", function (e, menu) {
      var items = Array.from(menu.querySelectorAll(".ah-scheduler-contextmenu-item"));
      var i = items.indexOf(document.activeElement), n = items.length;
      if (e.key === "ArrowDown") { if (n) { items[(i + 1) % n].focus(); } }
      else if (e.key === "ArrowUp") { if (n) { items[(i - 1 + n) % n].focus(); } }
      else if (e.key === "Enter" || e.key === " ") { if (i >= 0) { menuAction(c, items[i]); } }
      else if (e.key === "Escape" || e.key === "Tab") {
        var t = st.menuTarget;
        closePopups(c);
        if (t && t.nodeType === 1) { t.focus(); }
      } else { return; }
      e.preventDefault();
      e.stopPropagation();
    });
    this.delegate("click", ".ah-scheduler-day-more", function (e, more) {
      e.stopPropagation();
      openMore(c, more);
    });
    this.delegate("keydown", ".ah-scheduler-day-more", function (e, more) {
      if (e.key === "Enter" || e.key === " ") { e.preventDefault(); openMore(c, more); }
    });
    this.delegate("click", ".ah-scheduler-more-popover-close", function (e) {
      e.stopPropagation();
      closePopups(c);
    });
    this.delegate("keydown", ".ah-scheduler-more-popover", function (e) {
      if (e.key === "Escape") { e.preventDefault(); e.stopPropagation(); closePopups(c); }
      if (e.key === "Enter" && e.target.classList.contains("ah-scheduler-more-popover-close")) { closePopups(c); }
    });
    this.listen(document, "mousedown", function (e) {
      if ((st.menu && !st.menu.contains(e.target)) ||
          (st.pop && !st.pop.contains(e.target))) { closePopups(c); }
    });
    this.delegate("mousedown", ".ah-scheduler-view-container", function (e) {
      if (e.target.closest(".ah-scheduler-contextmenu, .ah-scheduler-more-popover")) { return; }
      dragStart(c, e);
    });
    // scroll does not bubble: listen in the capture phase
    this.listen(el, "scroll", function (e) {
      var sc = e.target;
      if (!sc.classList || !sc.classList.contains("ah-scheduler-timeline-scroll")) { return; }
      var head = el.querySelector(".ah-scheduler-timeline-slots-header");
      var panel = el.querySelector(".ah-scheduler-timeline-resource-panel");
      if (head) { head.style.transform = "translateX(" + (-sc.scrollLeft) + "px)"; }
      if (panel) { panel.scrollTop = sc.scrollTop; }
    }, { capture: true });
    links(el);               // today in the browser's own date
    firstScroll(el);
    nowLine(el);
    // a morph from the server replaces the view: redraw the now line
    this.observer = new MutationObserver(function () { nowLine(el); });
    var cont = el.querySelector(".ah-scheduler-view-container");
    this.observer.observe(cont || el, { childList: true, subtree: true });
    st.timer = setInterval(function () { nowLine(el); }, 60000);
  }

  teardown() {
    var st = this.st;
    closePopups(this);
    clearInterval(st.timer);
    dragStop(st);
    if (this.observer) { this.observer.disconnect(); }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  navigate(dir) { navigate(this, dir); }
  setView(view) { show(this, conf(this.element).date, view); }
  gotoDate(iso) { var d = parseDay(iso); if (d !== null) { show(this, d, conf(this.element).view); } }
  getValue() { return this.element.getAttribute("data-ah-value"); }
});
