/* Behaviour of the calendar (designs/04-components.md). Ported from
 * sigil: form/calendar (+ daygrid, timegrid, list, shared, recurrence,
 * util).
 *
 * Dates are day numbers (days since 1970-01-01) and times minutes since
 * day 0 (AH.lib.date), computed with UTC arithmetic: event times are local
 * wall times without a zone, so there is no DST or zone shifting. The view
 * builders (calMonth, calTimegrid, calList) are the twins of month_view/4,
 * timegrid_view/4 and list_view/4 in aihtml_calendar.erl: both feed the
 * same templates (calendar_month, calendar_timegrid, calendar_list).
 * Recurring events are expanded by AH.lib.rrule.
 *
 * Events: `change` when the date or view changed; ah:event-click (detail
 * {event, raw}), ah:more-click ({date}; cancelable: preventDefault keeps
 * the view), ah:event-drop / ah:event-resize ({event, from, to, allDay,
 * days?}), ah:select ({from, to, allDay}). Their fields are also written
 * to the root as data-* (Event.data of a postback).
 *
 * With the href option (data-ah-href) the toolbar entries are links to
 * the state they lead to; a plain click still navigates here and pushes
 * the link's URL, modified clicks are left to the browser. */
import AH from "../core.js";
import "./_lib_date.js";
import "./_lib_rrule.js";
import "virtual:ah-tpl/calendar_list";
import "virtual:ah-tpl/calendar_month";
import "virtual:ah-tpl/calendar_timegrid";

var D = AH.lib.date, RR = AH.lib.rrule;
var DAY = D.DAY, dnum = D.dnum, ymd = D.ymd, dow = D.dow, sow = D.sow, lastDay = D.lastDay,
    addMonths = D.addMonths, pad = D.pad, isoDate = D.isoDate, todayNum = D.todayNum,
    parseDate = D.parseDate, parseTime = D.parseTime, isoTime = D.isoTime;
var seq = 0;

function ensureId(el, prefix) {
  if (!el.id) { el.id = prefix + (++seq); }
  return el.id;
}

// Display formats: yyyy yy MMMM MMM MM M dd d EEEE EEE (fmt/3 in Erlang).
function fmtDate(n, f, L) {
  var p = ymd(n);
  return f.replace(/yyyy|yy|MMMM|MMM|MM|M|dd|d|EEEE|EEE/g, function (t) {
    switch (t) {
      case "yyyy": return String(p[0]);
      case "yy": return pad(p[0] % 100);
      case "MMMM": return L.months[p[1] - 1];
      case "MMM": return L.months_short[p[1] - 1];
      case "MM": return pad(p[1]);
      case "M": return String(p[1]);
      case "dd": return pad(p[2]);
      case "d": return String(p[2]);
      case "EEEE": return L.weekdays[dow(n)];
      default: return L.weekdays_short[dow(n)];
    }
  });
}

function readJson(el, attr, dflt) {
  try { return JSON.parse(el.getAttribute(attr) || "null") || dflt; } catch (err) { return dflt; }
}

// ==================================================================
// calendar

var CAL_LABELS = {
  today: "Today", prev: "Previous", next: "Next",
  month: "Month", week: "Week", day: "Day", list: "Agenda",
  all_day: "All day", all_day_short: "all-day", more: "+{n} more",
  no_events: "No events in this period",
  no_events_hint: "Try navigating to a different date range",
  am: "AM", pm: "PM",
  months: ["January", "February", "March", "April", "May", "June", "July",
           "August", "September", "October", "November", "December"],
  months_short: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
  weekdays: ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"],
  weekdays_short: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
  title_month: "MMMM yyyy", title_day: "EEEE, MMMM d, yyyy",
  range_start: "MMM d", range_end: "MMM d, yyyy", list_date: "MMMM d, yyyy"
};
var DEFAULT_COLOR = "var(--ah-color-primary)";
var COLOR_RE = /^[#a-zA-Z0-9(),.%\s-]+$/;
var STATUS_COLORS = { confirmed: "var(--ah-color-success)", tentative: "var(--ah-color-warning)",
                      cancelled: "var(--ah-color-error)" };

// An event as the server sends it (normalize_event/2), from any event
// object a page passes: start required, end and allDay defaulted.
function calNormalize(e, n) {
  var s = parseTime(e.start);
  if (!s) { throw new Error("calendar: bad event start " + e.start); }
  var allDayFlag = e.allDay === true || e.all_day === true || s.dateOnly;
  var end = e.end !== undefined && e.end !== null ? parseTime(e.end) : null;
  var et = end ? end.t : s.t + (allDayFlag ? DAY : 60);
  var allDay = allDayFlag || (s.t % DAY === 0 && et % DAY === 0 && s.t !== et);
  var out = {
    id: e.id !== undefined && e.id !== null ? String(e.id) : "ev" + n,
    title: e.title === undefined || e.title === null ? "" : String(e.title),
    start: isoTime(s.t, allDay), end: isoTime(et, allDay), allDay: allDay
  };
  if (e.color && COLOR_RE.test(e.color)) { out.color = String(e.color); }
  if (e.rrule) { out.rrule = String(e.rrule); }
  if (e.exdates) { out.exdates = e.exdates.slice(); }
  if (e.status) { out.status = String(e.status); }
  return out;
}

// ---- recurrence (AH.lib.rrule) ----

// Instances overlapping [rs, re), in event order.
function instances(events, rs, re) {
  var out = [];
  events.forEach(function (ev) {
    var s = parseTime(ev.start).t, e = parseTime(ev.end).t;
    if (!ev.rrule) {
      if (s < re && e > rs) { out.push({ id: ev.id, src: ev, s: s, e: e, allDay: ev.allDay }); }
      return;
    }
    RR.expand(s, e, RR.parse(ev.rrule), rs, re, ev.exdates || []).forEach(function (p) {
      out.push({ id: ev.id + "_" + RR.stamp(p[0]), src: ev, s: p[0], e: p[1], allDay: ev.allDay });
    });
  });
  return out;
}

function inRange(insts, from, to) {
  return insts.filter(function (i) { return i.s < to && i.e > from; });
}

// ---- views ----

function cls(base, opts) {
  return base + opts.filter(function (o) { return o[0]; }).map(function (o) { return " " + o[1]; }).join("");
}

function h12(h) { return h === 0 ? 12 : (h > 12 ? h - 12 : h); }
function fmtClock(t, st) {
  var r = ((t % DAY) + DAY) % DAY, h = Math.floor(r / 60), m = pad(r % 60);
  return st.hour24 ? pad(h) + ":" + m : h12(h) + ":" + m + " " + (h < 12 ? st.L.am : st.L.pm);
}
function srcColor(src) { return src.color || DEFAULT_COLOR; }

// [rs, re) in days and the title
function calProfile(st) {
  var L = st.L, cur = st.cur, p;
  switch (st.view) {
    case "week":
      p = sow(cur, st.first);
      return [p, p + 7, fmtDate(p, L.range_start, L) + " – " + fmtDate(p + 6, L.range_end, L)];
    case "day":
      return [cur, cur + 1, fmtDate(cur, L.title_day, L)];
    case "list":
      return [cur, cur + st.agendaDays,
              fmtDate(cur, L.range_start, L) + " – " + fmtDate(cur + st.agendaDays - 1, L.range_end, L)];
    default: {
      var y = ymd(cur);
      return [sow(dnum(y[0], y[1], 1), st.first), sow(dnum(y[0], y[1], lastDay(y[0], y[1])), st.first) + 7,
              fmtDate(cur, L.title_month, L)];
    }
  }
}

// sigil's compute-week-segments (week_segments/2 in Erlang)
function weekSegments(w, insts) {
  var segs = inRange(insts, w * DAY, (w + 7) * DAY).map(function (i, n) {
    var vs = Math.floor(i.s / DAY);
    var ve = i.allDay ? Math.max(vs + 1, Math.floor((i.e + DAY - 1) / DAY)) : vs + 1;
    var cs = Math.max(vs, w), ce = Math.min(ve, w + 7);
    return { inst: i, sc: cs - w + 1, ec: ce - w + 1, span: ce - cs, multi: ve - vs > 1,
             cont: vs < w, conts: ve > w + 7, n: n };
  }).filter(function (g) { return g.span > 0; });
  segs.sort(function (a, b) {
    return ((a.multi ? 0 : 1) - (b.multi ? 0 : 1)) || (b.span - a.span) ||
      (a.inst.s % DAY - b.inst.s % DAY) || (a.n - b.n);
  });
  var rows = [];
  segs.forEach(function (g) {
    var r = 0;
    for (; r < rows.length; r++) {
      if (!rows[r].some(function (x) { return g.sc < x[1] && g.ec > x[0]; })) { break; }
    }
    if (r === rows.length) { rows.push([]); }
    rows[r].push([g.sc, g.ec]);
    g.row = r;
  });
  return segs;
}

function calMonth(st, rs, re, insts) {
  var L = st.L, today = todayNum(), curM = ymd(st.cur)[1], max = st.maxEvents;
  var headers = [];
  for (var i = 0; i < 7; i++) { headers.push({ label: L.weekdays_short[(st.first + i) % 7] }); }
  var weeks = [];
  for (var w = rs; w < re; w += 7) {
    var segs = weekSegments(w, insts), over = {};
    segs.forEach(function (g) {
      if (g.row >= max) { for (var c = g.sc; c < g.ec; c++) { over[c] = (over[c] || 0) + 1; } }
    });
    var days = [];
    for (var k = 0; k < 7; k++) {
      var d = w + k, p = ymd(d), other = p[1] !== curM;
      days.push({
        bg_cls: cls("ah-calendar-day", [[d === today, "ah-calendar-day-today"], [other, "ah-calendar-day-other"]]),
        num_cls: cls("ah-calendar-day-num", [[d === today, "ah-calendar-day-num-today"],
                                             [other, "ah-calendar-day-num-other"]]),
        date: isoDate(d), col: String(k + 1), num: String(p[2])
      });
    }
    weeks.push({
      days: days,
      events: segs.filter(function (g) { return g.row < max; }).map(function (g) {
        return {
          cls: cls("ah-calendar-event ah-calendar-daygrid-event",
                   [[g.multi, "ah-calendar-daygrid-event-multi"], [g.cont, "ah-calendar-daygrid-event-start"],
                    [g.conts, "ah-calendar-daygrid-event-end"]]),
          id: g.inst.id, sc: String(g.sc), ec: String(g.ec), row: String(g.row + 2),
          color: srcColor(g.inst.src), title: g.inst.src.title,
          has_time: !g.inst.allDay && !g.multi, time: fmtClock(g.inst.s, st)
        };
      }),
      more: Object.keys(over).map(Number).sort(function (a, b) { return a - b; })
        .filter(function (c) { return over[c] > 0; }).map(function (c) {
          return { date: isoDate(w + c - 1), col: String(c), row: String(max + 2),
                   label: L.more.split("{n}").join(String(over[c])) };
        })
    });
  }
  return { headers: headers, weeks: weeks };
}

// sigil's assign-columns (columns/2 in Erlang)
function columns(d, insts) {
  var items = insts.map(function (i, n) {
    var ts = Math.max(i.s - d * DAY, 0), te = Math.min(i.e - d * DAY, DAY);
    return { inst: i, ts: ts, te: te <= ts ? ts + 30 : te, n: n };
  });
  items.sort(function (a, b) { return (a.ts - b.ts) || (a.n - b.n); });
  var cols = [];
  items.forEach(function (it) {
    var k = 0;
    for (; k < cols.length; k++) {
      if (!cols[k].some(function (o) { return it.ts < o[1] && it.te > o[0]; })) { break; }
    }
    if (k === cols.length) { cols.push([]); }
    cols[k].push([it.ts, it.te]);
    it.col = k;
  });
  items.forEach(function (it) { it.cols = cols.length; });
  return items;
}

function calTimegrid(st, rs, re, insts) {
  var L = st.L, today = todayNum(), dur = st.slotDur, sh = st.slotH;
  var total = Math.round(DAY * sh / dur);
  var slots = [];
  for (var h = 0; h < 24; h++) {
    slots.push({ slot_height: String(Math.round(60 * sh / dur)),
                 label: st.hour24 ? pad(h) + ":00" : h12(h) + " " + (h < 12 ? L.am : L.pm) });
  }
  var days = [];
  for (var d = rs; d < re; d++) {
    var dayI = inRange(insts, d * DAY, (d + 1) * DAY);
    days.push({
      date: isoDate(d), dow: L.weekdays_short[dow(d)], num: String(ymd(d)[2]),
      head_cls: cls("ah-calendar-timegrid-header-cell", [[d === today, "ah-calendar-timegrid-header-today"]]),
      col_cls: cls("ah-calendar-timegrid-day-col", [[d === today, "ah-calendar-timegrid-day-today"]]),
      col_height: String(total),
      allday: dayI.filter(function (i) { return i.allDay; }).map(function (i) {
        return { id: i.id, color: srcColor(i.src), title: i.src.title };
      }),
      timed: columns(d, dayI.filter(function (i) { return !i.allDay; })).map(function (it) {
        return {
          id: it.inst.id, color: srcColor(it.inst.src), title: it.inst.src.title,
          top: String(Math.round(it.ts * sh / dur)), height: String(Math.round((it.te - it.ts) * sh / dur)),
          left: "calc(100% * " + it.col + " / " + it.cols + ")", width: "calc(100% / " + it.cols + ")",
          time: fmtClock(it.inst.s, st) + " – " + fmtClock(it.inst.e, st),
          resizable: st.editable
        };
      })
    });
  }
  return { all_day: L.all_day_short, slots: slots, days: days };
}

function calList(st, rs, re, insts) {
  var L = st.L, groups = [];
  for (var d = rs; d < re; d++) {
    var evs = inRange(insts, d * DAY, (d + 1) * DAY).map(function (i, n) { return [i, n]; });
    evs.sort(function (a, b) { return (a[0].s - b[0].s) || (a[1] - b[1]); });
    if (!evs.length) { continue; }
    groups.push({
      name: L.weekdays[dow(d)], date: fmtDate(d, L.list_date, L),
      events: evs.map(function (p) {
        var i = p[0], src = i.src;
        return {
          id: i.id, color: srcColor(src), title: src.title,
          time: i.allDay ? L.all_day : fmtClock(i.s, st) + " – " + fmtClock(i.e, st),
          recurring: !!src.rrule, has_status: src.status !== undefined,
          status_color: STATUS_COLORS[src.status] || "var(--ah-color-grey-300)"
        };
      })
    });
  }
  return { empty: !groups.length, no_events: L.no_events, no_events_hint: L.no_events_hint,
           groups: groups };
}

// The view HTML of the current state (calView), as the server renders it.
function calView(st) {
  var p = calProfile(st);
  var insts = instances(st.events, p[0] * DAY, p[1] * DAY);
  var html = st.view === "month" ? AH.tpl.calendar_month(calMonth(st, p[0], p[1], insts))
    : st.view === "list" ? AH.tpl.calendar_list(calList(st, p[0], p[1], insts))
    : AH.tpl.calendar_timegrid(calTimegrid(st, p[0], p[1], insts));
  return { rs: p[0], re: p[1], title: p[2], insts: insts, html: html };
}

// ---- behaviour ----

function calRender(c) {
  var st = c.st, el = c.element;
  var sc = st.container.querySelector(".ah-calendar-timegrid-scroll");
  var scroll = sc && st.renderedView === st.view ? sc.scrollTop : null;
  var v = calView(st);
  st.insts = {};
  v.insts.forEach(function (i) { st.insts[i.id] = i; });
  st.range = [v.rs, v.re];
  st.container.innerHTML = v.html;
  st.titles.forEach(function (t) { t.textContent = v.title; });
  el.querySelectorAll(".ah-calendar-view-btn").forEach(function (b) {
    var on = b.getAttribute("data-view") === st.view;
    b.classList.toggle("ah-calendar-view-btn-active", on);
    if (b.tagName === "A") {
      if (on) { b.setAttribute("aria-current", "true"); } else { b.removeAttribute("aria-current"); }
    } else {
      b.setAttribute("aria-pressed", String(on));
    }
  });
  calLinks(c);
  var iso = isoDate(st.cur);
  el.setAttribute("data-ah-value", iso);
  var hidden = el.querySelector(":scope > input[type=hidden]");
  if (hidden) { hidden.value = iso; }
  el.setAttribute("data-view", st.view);
  el.setAttribute("data-start", isoDate(v.rs));
  el.setAttribute("data-end", isoDate(v.re));
  clearInterval(st.timer);
  st.timer = null;
  sc = st.container.querySelector(".ah-calendar-timegrid-scroll");
  if (sc) {
    // keep the scroll position while the view stays, else show the morning
    sc.scrollTop = scroll !== null ? scroll : Math.max(Math.round(7 * 60 * st.slotH / st.slotDur) - 10, 0);
    calNow(st);
    st.timer = setInterval(function () { calNow(st); }, 60000);
  }
  st.renderedView = st.view;
}

// The current time line, when today is in the visible range.
function calNow(st) {
  var now = new Date(), t = todayNum();
  st.container.querySelectorAll(".ah-calendar-timegrid-now-indicator").forEach(function (ind) {
    if (t >= st.range[0] && t < st.range[1]) {
      ind.style.display = "block";
      ind.style.top = Math.round((now.getHours() * 60 + now.getMinutes()) * st.slotH / st.slotDur) + "px";
    } else {
      ind.style.display = "none";
    }
  });
}

// Navigation re-renders and fires change when the date or view changed.
function calGo(c, cur, view) {
  var st = c.st;
  var changed = cur !== st.cur || view !== st.view;
  st.cur = cur;
  st.view = view;
  calRender(c);
  if (changed) { c.fire("change"); }
}

// The day prev (-1) or next (1) shows; the twin of step/4.
function calStepDay(st, dir) {
  switch (st.view) {
    case "month": return addMonths(st.cur, dir);
    case "week": return st.cur + 7 * dir;
    case "day": return st.cur + dir;
    default: return st.cur + st.agendaDays * dir;
  }
}

function calStep(c, dir) {
  calGo(c, calStepDay(c.st, dir), c.st.view);
}

// With the href option (data-ah-href: a template with {date} and {view})
// the toolbar entries are links; point them at the states they lead to
// from the shown date (the twin of nav_url/3).
function calLinks(c) {
  var st = c.st, el = c.element, tpl = el.getAttribute("data-ah-href");
  if (!tpl) { return; }
  var url = function (d, view) {
    return tpl.replace(/\{date\}/g, isoDate(d)).replace(/\{view\}/g, view);
  };
  var set = function (sel, d) {
    el.querySelectorAll("a" + sel).forEach(function (a) { a.setAttribute("href", url(d, st.view)); });
  };
  set(".ah-calendar-btn-prev", calStepDay(st, -1));
  set(".ah-calendar-btn-next", calStepDay(st, 1));
  set(".ah-calendar-btn-today", todayNum());
  el.querySelectorAll("a.ah-calendar-view-btn").forEach(function (a) {
    a.setAttribute("href", url(st.cur, a.getAttribute("data-view")));
  });
}

// A click on a toolbar entry: buttons always navigate here; a link only
// on a plain left click (the browser opens modified clicks), whose
// default is then prevented and whose URL is pushed after the navigation
// (a reload or going back asks the server for that state).
function calToolbar(c, sel, go) {
  c.delegate("click", sel, function (e, t) {
    var url = "";
    if (t.tagName === "A") {
      if (e.button !== 0 || e.ctrlKey || e.metaKey || e.shiftKey || e.altKey) { return; }
      e.preventDefault();
      url = t.getAttribute("href");
    }
    go(t);
    if (url) { AH.apply([{ op: "url", mode: "push", value: url }]); }
  });
}

// Details of an interaction as data-* on the root (Event.data of a
// postback), then the component event (detail: the details). Returns
// false when a listener called preventDefault.
var DETAIL_ATTRS = ["data-event", "data-from", "data-to", "data-days", "data-all-day", "data-date"];
function calFire(c, name, detail) {
  var el = c.element;
  DETAIL_ATTRS.forEach(function (a) { el.removeAttribute(a); });
  Object.keys(detail).forEach(function (k) {
    if (k === "raw") { return; }
    el.setAttribute("data-" + k.replace(/[A-Z]/g, function (x) { return "-" + x.toLowerCase(); }), String(detail[k]));
  });
  return c.fire(name, detail);
}

function calEventClick(c, target) {
  var inst = c.st.insts[target.getAttribute("data-eventid")];
  if (!inst) { return; }
  calFire(c, "ah:event-click", { event: inst.src.id, raw: Object.assign({}, inst.src,
    { start: isoTime(inst.s, inst.allDay), end: isoTime(inst.e, inst.allDay) }) });
}

function calMore(c, target) {
  var st = c.st;
  var date = target.getAttribute("data-date");
  var go = calFire(c, "ah:more-click", { date: date });
  if (go && st.views.indexOf("day") >= 0) {
    calGo(c, parseDate(date), "day");
  }
}

// Shift an event (the whole series for a recurring one) and re-render.
function calMove(c, src, dStart, dEnd, name, days) {
  var st = c.st;
  var s = parseTime(src.start).t + dStart, e = parseTime(src.end).t + dEnd;
  if (e <= s) { return; }
  var allDay = src.allDay && s % DAY === 0 && e % DAY === 0;
  src.start = isoTime(s, allDay);
  src.end = isoTime(e, allDay);
  src.allDay = allDay;
  calRender(c);
  var detail = { event: src.id, from: src.start, to: src.end, allDay: allDay };
  if (days !== undefined) { detail.days = days; }
  calFire(c, name, detail);
  st.justDragged = true;
}

// Hit testing by rectangles (the event layer covers the day cells).
function cellAt(cells, x, y) {
  for (var i = 0; i < cells.length; i++) {
    var r = cells[i].getBoundingClientRect();
    if (x >= r.left && x <= r.right && (y === null || (y >= r.top && y <= r.bottom))) { return cells[i]; }
  }
  return null;
}

function slotMinutes(st, col, y) {
  var rel = y - col.getBoundingClientRect().top;
  var m = Math.min(Math.max(Math.round(rel / st.slotH * st.slotDur), 0), DAY);
  return Math.round(m / st.slotDur) * st.slotDur;
}

function ghost(evEl, e) {
  var r = evEl.getBoundingClientRect();
  var g = evEl.cloneNode(true);
  g.classList.add("ah-calendar-event-ghost");
  g.removeAttribute("tabindex");
  g.removeAttribute("role");
  Object.assign(g.style, { position: "fixed", zIndex: 9999, opacity: 0.7, pointerEvents: "none", margin: 0,
                           width: r.width + "px", height: r.height + "px", left: r.left + "px", top: r.top + "px" });
  document.body.appendChild(g);
  return { g: g, dx: e.clientX - r.left, dy: e.clientY - r.top };
}

// One drag at a time: mousedown decides the mode, document mousemove
// and mouseup (removed when the drag ends) carry it out.
function calDragStart(c, e) {
  var st = c.st, cont = st.container;
  if (e.button !== 0) { return; }
  var t = e.target;
  var evEl = t.closest(".ah-calendar-daygrid-event, .ah-calendar-timegrid-event, .ah-calendar-allday-event");
  var d = null;
  if (t.classList.contains("ah-calendar-timegrid-resize-handle") && st.editable) {
    var rEv = t.closest(".ah-calendar-timegrid-event");
    d = { mode: "resize", ev: rEv, inst: st.insts[rEv.getAttribute("data-eventid")],
          col: t.closest(".ah-calendar-timegrid-day-col") };
  } else if (evEl && st.editable) {
    var kind = evEl.classList.contains("ah-calendar-daygrid-event") ? "month"
      : (evEl.classList.contains("ah-calendar-allday-event") ? "allday" : "timed");
    d = { mode: "move", kind: kind, ev: evEl, inst: st.insts[evEl.getAttribute("data-eventid")],
          x0: e.clientX, y0: e.clientY, started: false };
    if (!d.inst) { return; }
    if (kind === "month") {
      var c0 = cellAt(cont.querySelectorAll(".ah-calendar-day"), e.clientX, e.clientY);
      d.origin = c0 ? parseDate(c0.getAttribute("data-date")) : Math.floor(d.inst.s / DAY);
    } else if (kind === "allday") {
      var ac = evEl.closest(".ah-calendar-timegrid-allday-cell");
      d.origin = parseDate(ac ? ac.getAttribute("data-date") : null);
    }
  } else if (!evEl && st.selectable && t.closest(".ah-calendar-daygrid-body") &&
             !t.closest(".ah-calendar-day-more")) {
    var cell = cellAt(cont.querySelectorAll(".ah-calendar-day"), e.clientX, e.clientY);
    if (cell) { d = { mode: "select", from: parseDate(cell.getAttribute("data-date")) }; d.to = d.from; }
  } else if (!evEl && st.selectable && t.closest(".ah-calendar-timegrid-day-col")) {
    var col = t.closest(".ah-calendar-timegrid-day-col");
    var m0 = Math.min(slotMinutes(st, col, e.clientY), DAY - st.slotDur);
    var ph = document.createElement("div");
    ph.className = "ah-calendar-timegrid-create-placeholder";
    ph.style.left = "0px";
    ph.style.right = "0px";
    col.appendChild(ph);
    d = { mode: "create", col: col, day: parseDate(col.getAttribute("data-date")), m0: m0,
          top: m0, bot: m0 + st.slotDur, ph: ph };
  }
  if (!d || !d.inst && (d.mode === "move" || d.mode === "resize")) { return; }
  e.preventDefault();
  st.drag = d;
  calDragPaint(c, e);
  var ac2 = st.dragAc = new AbortController(), o = { signal: ac2.signal };
  document.addEventListener("mousemove", function (me) { calDragPaint(c, me); }, o);
  document.addEventListener("mouseup", function (ue) { calDragEnd(c, ue); }, o);
  document.addEventListener("keydown", function (ke) {
    if (ke.key === "Escape") { calDragCancel(c); }
  }, o);
}

function calDragPaint(c, e) {
  var st = c.st, d = st.drag, cont = st.container;
  if (!d) { return; }
  switch (d.mode) {
    case "move":
      if (!d.started) {
        if (Math.abs(e.clientX - d.x0) + Math.abs(e.clientY - d.y0) < 4) { return; }
        d.started = true;
        d.g = ghost(d.ev, e);
      }
      d.g.g.style.left = e.clientX - d.g.dx + "px";
      d.g.g.style.top = e.clientY - d.g.dy + "px";
      break;
    case "resize": {
      var m = Math.max(slotMinutes(st, d.col, e.clientY), d.inst.s % DAY + st.slotDur);
      d.end = m;
      d.ev.style.height = Math.round((m - d.inst.s % DAY) * st.slotH / st.slotDur) + "px";
      break;
    }
    case "select": {
      var days = cont.querySelectorAll(".ah-calendar-day");
      var cell = cellAt(days, e.clientX, e.clientY);
      if (cell) { d.to = parseDate(cell.getAttribute("data-date")); }
      var a = Math.min(d.from, d.to), b = Math.max(d.from, d.to);
      days.forEach(function (x) {
        var n = parseDate(x.getAttribute("data-date"));
        x.classList.toggle("ah-calendar-day-selected", n >= a && n <= b);
      });
      break;
    }
    case "create": {
      var cur = slotMinutes(st, d.col, e.clientY);
      d.top = Math.min(d.m0, cur);
      d.bot = Math.min(Math.max(d.m0 + st.slotDur, cur + st.slotDur), DAY);
      d.ph.style.top = Math.round(d.top * st.slotH / st.slotDur) + "px";
      d.ph.style.height = Math.round((d.bot - d.top) * st.slotH / st.slotDur) + "px";
      break;
    }
    default: break;
  }
}

function calDragCancel(c) {
  var st = c.st, d = st.drag;
  if (st.dragAc) { st.dragAc.abort(); st.dragAc = null; }
  st.drag = null;
  if (!d) { return; }
  if (d.g) { d.g.g.remove(); }
  if (d.ph) { d.ph.remove(); }
  st.container.querySelectorAll(".ah-calendar-day-selected").forEach(function (x) {
    x.classList.remove("ah-calendar-day-selected");
  });
  if (d.mode === "resize") { calRender(c); }
}

function calDragEnd(c, e) {
  var st = c.st, d = st.drag, cont = st.container;
  calDragCancel(c);
  if (!d) { return; }
  var src = d.inst ? d.inst.src : null;
  switch (d.mode) {
    case "move": {
      if (!d.started) { return; }                    // a click
      st.justDragged = true;
      if (d.kind === "timed") {
        var col = cellAt(cont.querySelectorAll(".ah-calendar-timegrid-day-col"), e.clientX, null);
        if (!col) { return; }
        var day = parseDate(col.getAttribute("data-date"));
        var top = e.clientY - d.g.dy;                // the ghost's top edge
        var start = day * DAY + Math.min(slotMinutes(st, col, top), DAY - st.slotDur);
        var delta = start - d.inst.s;
        if (delta) {
          calMove(c, src, delta, delta, "ah:event-drop", day - Math.floor(d.inst.s / DAY));
        }
      } else {
        var sel = d.kind === "month" ? ".ah-calendar-day" : ".ah-calendar-timegrid-allday-cell";
        var cell = cellAt(cont.querySelectorAll(sel), e.clientX, d.kind === "month" ? e.clientY : null);
        if (!cell) { return; }
        var days = parseDate(cell.getAttribute("data-date")) - d.origin;
        if (days) { calMove(c, src, days * DAY, days * DAY, "ah:event-drop", days); }
      }
      break;
    }
    case "resize": {
      st.justDragged = true;
      var end = Math.floor(d.inst.s / DAY) * DAY + (d.end === undefined ? d.inst.e % DAY : d.end);
      if (d.end !== undefined && end !== d.inst.e) {
        calMove(c, src, 0, end - d.inst.e, "ah:event-resize");
      }
      break;
    }
    case "select": {
      var a = Math.min(d.from, d.to), b = Math.max(d.from, d.to);
      calFire(c, "ah:select", { from: isoDate(a), to: isoDate(b + 1), allDay: true });
      break;
    }
    case "create":
      st.justDragged = true;
      calFire(c, "ah:select", { from: isoTime(d.day * DAY + d.top, false),
                                to: isoTime(d.day * DAY + d.bot, false), allDay: false });
      break;
    default: break;
  }
}

function calFind(st, id) {
  for (var i = 0; i < st.events.length; i++) {
    if (st.events[i].id === String(id)) { return i; }
  }
  return -1;
}

AH.register("calendar", class extends AH.Controller {
  setup() {
    var c = this, el = this.element;
    ensureId(el, "ah-cal");
    var num = function (a, dflt) {
      var n = parseInt(el.getAttribute(a) || "", 10);
      return n > 0 || (n === 0 && dflt === 0) ? n : dflt;
    };
    var st = this.st = {
      container: el.querySelector(":scope > .ah-calendar-view-container"),
      titles: Array.from(el.querySelectorAll(".ah-calendar-title")),
      events: readJson(el, "data-ah-events", []).map(calNormalize),
      view: el.getAttribute("data-ah-view") || "month",
      views: Array.from(el.querySelectorAll(".ah-calendar-view-btn"), function (b) {
        return b.getAttribute("data-view"); }),
      cur: parseDate(el.getAttribute("data-ah-value")) || todayNum(),
      first: Math.min(num("data-ah-first-day", 0), 6),
      agendaDays: num("data-ah-agenda-days", 30),
      maxEvents: num("data-ah-day-max-events", 3),
      slotDur: num("data-ah-slot-duration", 30),
      slotH: num("data-ah-slot-height", 20),
      hour24: el.getAttribute("data-ah-hour-format") === "24",
      L: Object.assign({}, CAL_LABELS, readJson(el, "data-ah-labels", {})),
      editable: el.classList.contains("ah-calendar-editable"),
      selectable: el.classList.contains("ah-calendar-selectable"),
      insts: {}, range: [0, 0], timer: null, drag: null, dragAc: null, justDragged: false
    };
    // the server rendered the same view; render again for the browser's today
    calRender(this);

    calToolbar(c, ".ah-calendar-btn-prev", function () { calStep(c, -1); });
    calToolbar(c, ".ah-calendar-btn-next", function () { calStep(c, 1); });
    calToolbar(c, ".ah-calendar-btn-today", function () { calGo(c, todayNum(), st.view); });
    calToolbar(c, ".ah-calendar-view-btn", function (b) { calGo(c, st.cur, b.getAttribute("data-view")); });
    this.delegate("click", ".ah-calendar-event, .ah-calendar-list-event", function (e, ev) {
      e.stopPropagation();
      if (st.justDragged) { st.justDragged = false; return; }
      calEventClick(c, ev);
    });
    this.delegate("click", ".ah-calendar-day-more", function (e, more) {
      e.stopPropagation();
      calMore(c, more);
    });
    this.delegate("keydown", ".ah-calendar-event, .ah-calendar-list-event, .ah-calendar-day-more", function (e, t) {
      if (e.key !== "Enter" && e.key !== " ") { return; }
      e.preventDefault();
      if (t.classList.contains("ah-calendar-day-more")) { calMore(c, t); } else { calEventClick(c, t); }
    });
    this.listen(st.container, "mousedown", function (e) {
      st.justDragged = false;
      calDragStart(c, e);
    });
  }

  teardown() {
    var st = this.st;
    if (!st) { return; }
    if (st.drag) { calDragCancel(this); }
    clearInterval(st.timer);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  prev() { calStep(this, -1); }
  next() { calStep(this, 1); }
  today() { calGo(this, todayNum(), this.st.view); }
  changeView(v) {
    if (["month", "week", "day", "list"].indexOf(v) >= 0) { calGo(this, this.st.cur, v); }
  }
  setValue(v) {
    var d = parseDate(String(v || "").slice(0, 10));
    if (d !== null) { this.st.cur = d; calRender(this); }
  }
  getValue() { return this.element.getAttribute("data-ah-value"); }
  setEvents(evs) {
    this.st.events = (evs || []).map(calNormalize);
    calRender(this);
  }
  addEvent(ev) {
    var st = this.st, n = calNormalize(ev, st.events.length + 1), i = calFind(st, n.id);
    if (i >= 0) { st.events[i] = n; } else { st.events.push(n); }
    calRender(this);
  }
  updateEvent(id, changes) {
    var st = this.st, i = calFind(st, id);
    if (i < 0) { return; }
    // fields set to undefined are not copied (as before)
    var merged = Object.assign({}, st.events[i]);
    Object.keys(changes || {}).forEach(function (k) {
      if (changes[k] !== undefined) { merged[k] = changes[k]; }
    });
    if (changes && (changes.start || changes.end) && changes.allDay === undefined) { delete merged.allDay; }
    st.events[i] = calNormalize(merged, i + 1);
    calRender(this);
  }
  removeEvent(id) {
    var st = this.st, i = calFind(st, id);
    if (i >= 0) { st.events.splice(i, 1); calRender(this); }
  }
  getEvents() {
    return this.st.events.map(function (e) { return Object.assign({}, e); });
  }
});
