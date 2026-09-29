/* Behaviours of the form_calendar components (designs/04-components.md).
 * Ported from sigil: form/calendar (+ daygrid, timegrid, list, shared,
 * recurrence, util) and form/datetime_input (+ format, editor, dropdown).
 *
 * Dates are day numbers (days since 1970-01-01) and times minutes since
 * day 0, computed with UTC arithmetic: event times are local wall times
 * without a zone, so there is no DST or zone shifting. The view builders
 * (calMonth, calTimegrid, calList) are the twins of month_view/4,
 * timegrid_view/4 and list_view/4 in aihtml_form_calendar.erl: both feed
 * the same templates (calendar_month, calendar_timegrid, calendar_list). */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var DAY = 1440;
  var MAX_ITERS = 5000;
  var seq = 0;

  function ensureId(el, prefix) {
    if (!el.id) { el.id = prefix + (++seq); }
    return el.id;
  }

  function outside(el, e) {
    return e.target.isConnected !== false && !$.contains(el, e.target) && e.target !== el;
  }

  // ------------------------------------------------------------------
  // Day numbers
  // ------------------------------------------------------------------

  function dnum(y, m, d) { return Math.round(Date.UTC(y, m - 1, d) / 864e5); }   // m 1..12
  function ymd(n) {
    var d = new Date(n * 864e5);
    return [d.getUTCFullYear(), d.getUTCMonth() + 1, d.getUTCDate()];
  }
  function dow(n) { return ((n + 4) % 7 + 7) % 7; }                               // 0 = Sunday
  function sow(n, first) { return n - (dow(n) - first + 7) % 7; }
  function lastDay(y, m) { return new Date(Date.UTC(y, m, 0)).getUTCDate(); }
  // date-fns addMonths: the day clamped to the target month's length
  function addMonths(n, k) {
    var p = ymd(n), t = p[0] * 12 + (p[1] - 1) + k;
    var y = Math.floor(t / 12), m = t - y * 12 + 1;
    return dnum(y, m, Math.min(p[2], lastDay(y, m)));
  }
  function pad(n) { return (n < 10 ? "0" : "") + n; }
  function pad4(n) { return ("000" + n).slice(-4); }
  function isoDate(n) { var p = ymd(n); return pad4(p[0]) + "-" + pad(p[1]) + "-" + pad(p[2]); }
  function todayNum() { var t = new Date(); return dnum(t.getFullYear(), t.getMonth() + 1, t.getDate()); }
  function validYmd(y, m, d) { return m >= 1 && m <= 12 && d >= 1 && d <= lastDay(y, m); }
  function parseDate(s) {
    var m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(s || "");
    return m && validYmd(+m[1], +m[2], +m[3]) ? dnum(+m[1], +m[2], +m[3]) : null;
  }
  // ISO date or date-time -> { t: minutes, dateOnly }
  function parseTime(s) {
    var m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2}))?/.exec(String(s || ""));
    if (!m || !validYmd(+m[1], +m[2], +m[3])) { return null; }
    var t = dnum(+m[1], +m[2], +m[3]) * DAY;
    if (m[4] === undefined) { return { t: t, dateOnly: true }; }
    if (+m[4] > 23 || +m[5] > 59) { return null; }
    return { t: t + (+m[4]) * 60 + (+m[5]), dateOnly: false };
  }
  function isoTime(t, allDay) {
    var d = Math.floor(t / DAY), r = t - d * DAY;
    if (allDay && r === 0) { return isoDate(d); }
    return isoDate(d) + "T" + pad(Math.floor(r / 60)) + ":" + pad(r % 60);
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
  // ==================================================================

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

  // ---- recurrence (sigil's calendar/recurrence.cljs) ----

  var RDAYS = ["SU", "MO", "TU", "WE", "TH", "FR", "SA"];

  function parseRRule(str) {
    var r = {};
    String(str).replace(/^RRULE:/, "").split(";").forEach(function (part) {
      if (!part) { return; }
      var i = part.indexOf("=");
      var k = part.slice(0, i).toUpperCase(), v = part.slice(i + 1);
      switch (k) {
        case "FREQ": r.freq = v.toLowerCase(); break;
        case "INTERVAL": r.interval = parseInt(v, 10); break;
        case "COUNT": r.count = parseInt(v, 10); break;
        case "UNTIL": {
          var m = /^(\d{4})(\d{2})(\d{2})(?:T(\d{2})(\d{2}))?/.exec(v);
          if (m) { r.until = dnum(+m[1], +m[2], +m[3]) * DAY + (m[4] ? +m[4] * 60 + (+m[5]) : 0); }
          break;
        }
        case "BYDAY": r.byday = v.split(",").map(function (d) { return RDAYS.indexOf(d.toUpperCase()); }); break;
        case "BYMONTHDAY": r.bymonthday = v.split(",").map(function (d) { return parseInt(d, 10); }); break;
        case "BYMONTH": r.bymonth = v.split(",").map(function (d) { return parseInt(d, 10); }); break;
        default: break;
      }
    });
    return r;
  }

  function advance(c, freq, i) {
    var d = Math.floor(c / DAY), r = c - d * DAY;
    switch (freq) {
      case "weekly": return c + 7 * i * DAY;
      case "monthly": return addMonths(d, i) * DAY + r;
      case "yearly": return addMonths(d, 12 * i) * DAY + r;
      default: return c + i * DAY;
    }
  }

  function matches(c, rule) {
    var d = Math.floor(c / DAY), p = ymd(d);
    return (!rule.byday || rule.byday.indexOf(dow(d)) >= 0) &&
      (!rule.bymonthday || rule.bymonthday.indexOf(p[2]) >= 0) &&
      (!rule.bymonth || rule.bymonth.indexOf(p[1]) >= 0);
  }

  // The occurrences [start, end] of a series overlapping [rs, re), minutes.
  function expand(s, e, rule, rs, re, ex) {
    if (!rule.freq) { return []; }
    var dur = e - s, I = rule.interval || 1, out = [], iter = 0, count = 0;
    var countOk = function () { return rule.count === undefined || count < rule.count; };
    var untilOk = function (t) { return rule.until === undefined || t <= rule.until; };
    var add = function (c) {
      if (ex.indexOf(isoDate(Math.floor(c / DAY))) < 0 && c < re && c + dur > rs) { out.push([c, c + dur]); }
    };
    if (rule.freq === "weekly" && rule.byday && rule.byday.length) {
      var byday = rule.byday.slice().sort(function (a, b) { return a - b; });
      var offset = s % DAY;
      for (var w = sow(Math.floor(s / DAY), 1);
           iter < MAX_ITERS && w * DAY < re && untilOk(w * DAY) && countOk(); w += 7 * I) {
        var wd = dow(w);
        var cands = byday.map(function (d) { return (w + (d - wd + 7) % 7) * DAY + offset; })
          .sort(function (a, b) { return a - b; });
        for (var k = 0; k < cands.length; k++) {
          var c = cands[k];
          if (countOk() && iter < MAX_ITERS && c >= s && untilOk(c) && c < re) {
            iter++; count++; add(c);
          }
        }
      }
    } else {
      for (var t = s; iter < MAX_ITERS && t < re && untilOk(t) && countOk(); t = advance(t, rule.freq, I)) {
        iter++;
        if (matches(t, rule)) { count++; add(t); }
      }
    }
    return out;
  }

  function stamp(t) {
    var d = Math.floor(t / DAY), p = ymd(d), r = t - d * DAY;
    return pad4(p[0]) + pad(p[1]) + pad(p[2]) + "T" + pad(Math.floor(r / 60)) + pad(r % 60) + "00";
  }

  // Instances overlapping [rs, re), in event order.
  function instances(events, rs, re) {
    var out = [];
    events.forEach(function (ev) {
      var s = parseTime(ev.start).t, e = parseTime(ev.end).t;
      if (!ev.rrule) {
        if (s < re && e > rs) { out.push({ id: ev.id, src: ev, s: s, e: e, allDay: ev.allDay }); }
        return;
      }
      expand(s, e, parseRRule(ev.rrule), rs, re, ev.exdates || []).forEach(function (p) {
        out.push({ id: ev.id + "_" + stamp(p[0]), src: ev, s: p[0], e: p[1], allDay: ev.allDay });
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

  function calState(el) { return $.data(el, "ah-cal"); }

  function calRender(el, $el) {
    var st = calState(el);
    var $scroll = st.$container.find(".ah-calendar-timegrid-scroll");
    var scroll = $scroll.length && st.renderedView === st.view ? $scroll[0].scrollTop : null;
    var v = calView(st);
    st.insts = {};
    v.insts.forEach(function (i) { st.insts[i.id] = i; });
    st.range = [v.rs, v.re];
    st.$container.html(v.html);
    st.$title.text(v.title);
    $el.find(".ah-calendar-view-btn").each(function () {
      var on = this.getAttribute("data-view") === st.view;
      $(this).toggleClass("ah-calendar-view-btn-active", on).attr("aria-pressed", String(on));
    });
    var iso = isoDate(st.cur);
    el.setAttribute("data-ah-value", iso);
    $el.children("input[type=hidden]").val(iso);
    el.setAttribute("data-view", st.view);
    el.setAttribute("data-start", isoDate(v.rs));
    el.setAttribute("data-end", isoDate(v.re));
    clearInterval(st.timer);
    st.timer = null;
    $scroll = st.$container.find(".ah-calendar-timegrid-scroll");
    if ($scroll.length) {
      // keep the scroll position while the view stays, else show the morning
      $scroll[0].scrollTop = scroll !== null ? scroll : Math.max(Math.round(7 * 60 * st.slotH / st.slotDur) - 10, 0);
      calNow(el);
      st.timer = setInterval(function () { calNow(el); }, 60000);
    }
    st.renderedView = st.view;
  }

  // The current time line, when today is in the visible range.
  function calNow(el) {
    var st = calState(el);
    var $ind = st.$container.find(".ah-calendar-timegrid-now-indicator");
    var now = new Date(), t = todayNum();
    if (t >= st.range[0] && t < st.range[1]) {
      $ind.css({ display: "block",
                 top: Math.round((now.getHours() * 60 + now.getMinutes()) * st.slotH / st.slotDur) + "px" });
    } else {
      $ind.css({ display: "none" });
    }
  }

  // Navigation re-renders and fires change when the date or view changed.
  function calGo(el, $el, cur, view) {
    var st = calState(el);
    var changed = cur !== st.cur || view !== st.view;
    st.cur = cur;
    st.view = view;
    calRender(el, $el);
    if (changed) { $el.trigger("change"); }
  }

  function calStep(el, $el, dir) {
    var st = calState(el), c = st.cur;
    switch (st.view) {
      case "month": c = addMonths(c, dir); break;
      case "week": c += 7 * dir; break;
      case "day": c += dir; break;
      default: c += st.agendaDays * dir; break;
    }
    calGo(el, $el, c, st.view);
  }

  // Details of an interaction as data-* on the root (Event.data of a
  // postback), then the component event.
  var DETAIL_ATTRS = ["data-event", "data-from", "data-to", "data-days", "data-all-day", "data-date"];
  function calFire(el, $el, name, detail) {
    DETAIL_ATTRS.forEach(function (a) { el.removeAttribute(a); });
    $.each(detail, function (k, v) {
      if (k === "raw") { return; }
      el.setAttribute("data-" + k.replace(/[A-Z]/g, function (c) { return "-" + c.toLowerCase(); }), String(v));
    });
    var ev = $.Event(name);
    $el.trigger(ev, [detail]);
    return ev;
  }

  function calSource(st, instId) {
    var inst = st.insts[instId];
    return inst ? inst.src : null;
  }

  function calEventClick(el, $el, target) {
    var st = calState(el);
    var inst = st.insts[target.getAttribute("data-eventid")];
    if (!inst) { return; }
    calFire(el, $el, "ah:event-click", { event: inst.src.id, raw: $.extend({}, inst.src,
      { start: isoTime(inst.s, inst.allDay), end: isoTime(inst.e, inst.allDay) }) });
  }

  function calMore(el, $el, target) {
    var st = calState(el);
    var date = target.getAttribute("data-date");
    var ev = calFire(el, $el, "ah:more-click", { date: date });
    if (!ev.isDefaultPrevented() && st.views.indexOf("day") >= 0) {
      calGo(el, $el, parseDate(date), "day");
    }
  }

  // Shift an event (the whole series for a recurring one) and re-render.
  function calMove(el, $el, src, dStart, dEnd, name, days) {
    var st = calState(el);
    var s = parseTime(src.start).t + dStart, e = parseTime(src.end).t + dEnd;
    if (e <= s) { return; }
    var allDay = src.allDay && s % DAY === 0 && e % DAY === 0;
    src.start = isoTime(s, allDay);
    src.end = isoTime(e, allDay);
    src.allDay = allDay;
    calRender(el, $el);
    var detail = { event: src.id, from: src.start, to: src.end, allDay: allDay };
    if (days !== undefined) { detail.days = days; }
    calFire(el, $el, name, detail);
    st.justDragged = true;
  }

  // Hit testing by rectangles (the event layer covers the day cells).
  function cellAt($cells, x, y) {
    var hit = null;
    $cells.each(function () {
      var r = this.getBoundingClientRect();
      if (x >= r.left && x <= r.right && (y === null || (y >= r.top && y <= r.bottom))) { hit = this; return false; }
    });
    return hit;
  }

  function slotMinutes(st, col, y) {
    var rel = y - col.getBoundingClientRect().top;
    var m = Math.min(Math.max(Math.round(rel / st.slotH * st.slotDur), 0), DAY);
    return Math.round(m / st.slotDur) * st.slotDur;
  }

  function ghost(evEl, e) {
    var r = evEl.getBoundingClientRect();
    var $g = $(evEl).clone().addClass("ah-calendar-event-ghost").removeAttr("tabindex role")
      .css({ position: "fixed", zIndex: 9999, opacity: 0.7, pointerEvents: "none", margin: 0,
             width: r.width + "px", height: r.height + "px", left: r.left + "px", top: r.top + "px" })
      .appendTo(document.body);
    return { $g: $g, dx: e.clientX - r.left, dy: e.clientY - r.top };
  }

  // One drag at a time: mousedown decides the mode, document mousemove
  // and mouseup (namespaced per calendar) carry it out.
  function calDragStart(el, $el, e) {
    var st = calState(el);
    if (e.which !== 1) { return; }
    var $t = $(e.target), $c = st.$container;
    var evEl = $t.closest(".ah-calendar-daygrid-event, .ah-calendar-timegrid-event, .ah-calendar-allday-event")[0];
    var d = null;
    if ($t.hasClass("ah-calendar-timegrid-resize-handle") && st.editable) {
      var rEv = $t.closest(".ah-calendar-timegrid-event")[0];
      d = { mode: "resize", ev: rEv, inst: st.insts[rEv.getAttribute("data-eventid")],
            col: $t.closest(".ah-calendar-timegrid-day-col")[0] };
    } else if (evEl && st.editable) {
      var kind = $(evEl).hasClass("ah-calendar-daygrid-event") ? "month"
        : ($(evEl).hasClass("ah-calendar-allday-event") ? "allday" : "timed");
      d = { mode: "move", kind: kind, ev: evEl, inst: st.insts[evEl.getAttribute("data-eventid")],
            x0: e.clientX, y0: e.clientY, started: false };
      if (kind === "month") {
        var c0 = cellAt($c.find(".ah-calendar-day"), e.clientX, e.clientY);
        d.origin = c0 ? parseDate(c0.getAttribute("data-date")) : Math.floor(d.inst.s / DAY);
      } else if (kind === "allday") {
        d.origin = parseDate($(evEl).closest(".ah-calendar-timegrid-allday-cell").attr("data-date"));
      }
    } else if (!evEl && st.selectable && $t.closest(".ah-calendar-daygrid-body").length &&
               !$t.closest(".ah-calendar-day-more").length) {
      var cell = cellAt($c.find(".ah-calendar-day"), e.clientX, e.clientY);
      if (cell) { d = { mode: "select", from: parseDate(cell.getAttribute("data-date")) }; d.to = d.from; }
    } else if (!evEl && st.selectable && $t.closest(".ah-calendar-timegrid-day-col").length) {
      var col = $t.closest(".ah-calendar-timegrid-day-col")[0];
      var m0 = Math.min(slotMinutes(st, col, e.clientY), DAY - st.slotDur);
      d = { mode: "create", col: col, day: parseDate(col.getAttribute("data-date")), m0: m0,
            top: m0, bot: m0 + st.slotDur,
            $ph: $('<div class="ah-calendar-timegrid-create-placeholder"></div>')
              .css({ left: 0, right: 0 }).appendTo(col) };
    }
    if (!d || !d.inst && (d.mode === "move" || d.mode === "resize")) { return; }
    e.preventDefault();
    st.drag = d;
    calDragPaint(el, e);
    $(document).on("mousemove" + st.ns, function (me) { calDragPaint(el, me); })
      .on("mouseup" + st.ns, function (ue) { calDragEnd(el, $el, ue); })
      .on("keydown" + st.ns, function (ke) {
        if (ke.key === "Escape") { calDragCancel(el); }
      });
  }

  function calDragPaint(el, e) {
    var st = calState(el), d = st.drag, $c = st.$container;
    if (!d) { return; }
    switch (d.mode) {
      case "move":
        if (!d.started) {
          if (Math.abs(e.clientX - d.x0) + Math.abs(e.clientY - d.y0) < 4) { return; }
          d.started = true;
          d.g = ghost(d.ev, e);
        }
        d.g.$g.css({ left: e.clientX - d.g.dx + "px", top: e.clientY - d.g.dy + "px" });
        break;
      case "resize": {
        var m = Math.max(slotMinutes(st, d.col, e.clientY), d.inst.s % DAY + st.slotDur);
        d.end = m;
        $(d.ev).css("height", Math.round((m - d.inst.s % DAY) * st.slotH / st.slotDur) + "px");
        break;
      }
      case "select": {
        var cell = cellAt($c.find(".ah-calendar-day"), e.clientX, e.clientY);
        if (cell) { d.to = parseDate(cell.getAttribute("data-date")); }
        var a = Math.min(d.from, d.to), b = Math.max(d.from, d.to);
        $c.find(".ah-calendar-day").each(function () {
          var n = parseDate(this.getAttribute("data-date"));
          $(this).toggleClass("ah-calendar-day-selected", n >= a && n <= b);
        });
        break;
      }
      case "create": {
        var cur = slotMinutes(st, d.col, e.clientY);
        d.top = Math.min(d.m0, cur);
        d.bot = Math.min(Math.max(d.m0 + st.slotDur, cur + st.slotDur), DAY);
        d.$ph.css({ top: Math.round(d.top * st.slotH / st.slotDur) + "px",
                    height: Math.round((d.bot - d.top) * st.slotH / st.slotDur) + "px" });
        break;
      }
      default: break;
    }
  }

  function calDragCancel(el) {
    var st = calState(el), d = st.drag;
    $(document).off(st.ns);
    st.drag = null;
    if (!d) { return; }
    if (d.g) { d.g.$g.remove(); }
    if (d.$ph) { d.$ph.remove(); }
    st.$container.find(".ah-calendar-day-selected").removeClass("ah-calendar-day-selected");
    if (d.mode === "resize") { calRender(el, $(el)); }
  }

  function calDragEnd(el, $el, e) {
    var st = calState(el), d = st.drag, $c = st.$container;
    calDragCancel(el);
    if (!d) { return; }
    var src = d.inst ? d.inst.src : null;
    switch (d.mode) {
      case "move": {
        if (!d.started) { return; }                    // a click
        st.justDragged = true;
        if (d.kind === "timed") {
          var col = cellAt($c.find(".ah-calendar-timegrid-day-col"), e.clientX, null);
          if (!col) { return; }
          var day = parseDate(col.getAttribute("data-date"));
          var top = e.clientY - d.g.dy;                // the ghost's top edge
          var start = day * DAY + Math.min(slotMinutes(st, col, top), DAY - st.slotDur);
          var delta = start - d.inst.s;
          if (delta) {
            calMove(el, $el, src, delta, delta, "ah:event-drop", day - Math.floor(d.inst.s / DAY));
          }
        } else {
          var sel = d.kind === "month" ? ".ah-calendar-day" : ".ah-calendar-timegrid-allday-cell";
          var cell = cellAt($c.find(sel), e.clientX, d.kind === "month" ? e.clientY : null);
          if (!cell) { return; }
          var days = parseDate(cell.getAttribute("data-date")) - d.origin;
          if (days) { calMove(el, $el, src, days * DAY, days * DAY, "ah:event-drop", days); }
        }
        break;
      }
      case "resize": {
        st.justDragged = true;
        var end = Math.floor(d.inst.s / DAY) * DAY + (d.end === undefined ? d.inst.e % DAY : d.end);
        if (d.end !== undefined && end !== d.inst.e) {
          calMove(el, $el, src, 0, end - d.inst.e, "ah:event-resize");
        }
        break;
      }
      case "select": {
        var a = Math.min(d.from, d.to), b = Math.max(d.from, d.to);
        calFire(el, $el, "ah:select", { from: isoDate(a), to: isoDate(b + 1), allDay: true });
        break;
      }
      case "create":
        st.justDragged = true;
        calFire(el, $el, "ah:select", { from: isoTime(d.day * DAY + d.top, false),
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

  AH.define("calendar", {
    init: function (el, $el) {
      ensureId(el, "ah-cal");
      var num = function (a, dflt) {
        var n = parseInt(el.getAttribute(a) || "", 10);
        return n > 0 || (n === 0 && dflt === 0) ? n : dflt;
      };
      var st = {
        ns: ".ahcal" + (++seq),
        $container: $el.children(".ah-calendar-view-container"),
        $title: $el.find(".ah-calendar-title"),
        events: readJson(el, "data-ah-events", []).map(calNormalize),
        view: el.getAttribute("data-ah-view") || "month",
        views: $el.find(".ah-calendar-view-btn").map(function () {
          return this.getAttribute("data-view"); }).get(),
        cur: parseDate(el.getAttribute("data-ah-value")) || todayNum(),
        first: Math.min(num("data-ah-first-day", 0), 6),
        agendaDays: num("data-ah-agenda-days", 30),
        maxEvents: num("data-ah-day-max-events", 3),
        slotDur: num("data-ah-slot-duration", 30),
        slotH: num("data-ah-slot-height", 20),
        hour24: el.getAttribute("data-ah-hour-format") === "24",
        L: $.extend({}, CAL_LABELS, readJson(el, "data-ah-labels", {})),
        editable: $el.hasClass("ah-calendar-editable"),
        selectable: $el.hasClass("ah-calendar-selectable"),
        insts: {}, range: [0, 0], timer: null, drag: null, justDragged: false
      };
      $.data(el, "ah-cal", st);
      // the server rendered the same view; render again for the browser's today
      calRender(el, $el);

      $el.on("click" + NS, ".ah-calendar-btn-prev", function () { calStep(el, $el, -1); })
        .on("click" + NS, ".ah-calendar-btn-next", function () { calStep(el, $el, 1); })
        .on("click" + NS, ".ah-calendar-btn-today", function () { calGo(el, $el, todayNum(), st.view); })
        .on("click" + NS, ".ah-calendar-view-btn", function () {
          calGo(el, $el, st.cur, this.getAttribute("data-view"));
        })
        .on("click" + NS, ".ah-calendar-event, .ah-calendar-list-event", function (e) {
          e.stopPropagation();
          if (st.justDragged) { st.justDragged = false; return; }
          calEventClick(el, $el, this);
        })
        .on("click" + NS, ".ah-calendar-day-more", function (e) {
          e.stopPropagation();
          calMore(el, $el, this);
        })
        .on("keydown" + NS, ".ah-calendar-event, .ah-calendar-list-event, .ah-calendar-day-more", function (e) {
          if (e.key !== "Enter" && e.key !== " ") { return; }
          e.preventDefault();
          if ($(this).hasClass("ah-calendar-day-more")) { calMore(el, $el, this); } else { calEventClick(el, $el, this); }
        });
      st.$container.on("mousedown" + NS, function (e) {
        st.justDragged = false;
        calDragStart(el, $el, e);
      });
    },
    destroy: function (el) {
      var st = calState(el);
      if (!st) { return; }
      if (st.drag) { calDragCancel(el); }
      $(document).off(st.ns);
      clearInterval(st.timer);
    },
    methods: {
      prev: function (el, $el) { calStep(el, $el, -1); },
      next: function (el, $el) { calStep(el, $el, 1); },
      today: function (el, $el) { calGo(el, $el, todayNum(), calState(el).view); },
      changeView: function (el, $el, v) {
        if (["month", "week", "day", "list"].indexOf(v) >= 0) { calGo(el, $el, calState(el).cur, v); }
      },
      setValue: function (el, $el, v) {
        var d = parseDate(String(v || "").slice(0, 10));
        if (d !== null) { calState(el).cur = d; calRender(el, $el); }
      },
      getValue: function (el) { return el.getAttribute("data-ah-value"); },
      setEvents: function (el, $el, evs) {
        calState(el).events = (evs || []).map(calNormalize);
        calRender(el, $el);
      },
      addEvent: function (el, $el, ev) {
        var st = calState(el), n = calNormalize(ev, st.events.length + 1), i = calFind(st, n.id);
        if (i >= 0) { st.events[i] = n; } else { st.events.push(n); }
        calRender(el, $el);
      },
      updateEvent: function (el, $el, id, changes) {
        var st = calState(el), i = calFind(st, id);
        if (i < 0) { return; }
        var merged = $.extend({}, st.events[i], changes || {});
        if (changes && (changes.start || changes.end) && changes.allDay === undefined) { delete merged.allDay; }
        st.events[i] = calNormalize(merged, i + 1);
        calRender(el, $el);
      },
      removeEvent: function (el, $el, id) {
        var st = calState(el), i = calFind(st, id);
        if (i >= 0) { st.events.splice(i, 1); calRender(el, $el); }
      },
      getEvents: function (el) {
        return calState(el).events.map(function (e) { return $.extend({}, e); });
      }
    }
  });

  // ==================================================================
  // datetime_input
  // ==================================================================

  var DTI_LABELS = {
    months: CAL_LABELS.months,
    weekdays: ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"],
    title: "MMMM yyyy", time: "Time",
    prev_month: "Previous month", next_month: "Next month"
  };
  // [pattern, type, min, max]; single letters are two digits wide, as in
  // segments/1 of the Erlang side.
  var TOKENS = [["yyyy", "year", 1900, 2100], ["yy", "year2", 0, 99], ["MM", "month", 1, 12],
                ["M", "month", 1, 12], ["dd", "day", 1, 31], ["d", "day", 1, 31],
                ["HH", "hour", 0, 23], ["H", "hour", 0, 23], ["hh", "hour12", 1, 12],
                ["h", "hour12", 1, 12], ["mm", "minute", 0, 59], ["m", "minute", 0, 59],
                ["ss", "second", 0, 59], ["s", "second", 0, 59], ["aa", "ampm", 0, 1],
                ["a", "ampm", 0, 1]];
  var SEG_NAMES = { year: "Year", year2: "Year", month: "Month", day: "Day", hour: "Hour",
                    hour12: "Hour", minute: "Minute", second: "Second", ampm: "AM/PM" };

  // format.cljs parse-format: [{type, start, end, len, min, max, editable}]
  function dtiSegments(f) {
    var segs = [], pos = 0, i = 0;
    while (i < f.length) {
      var tok = null;
      for (var k = 0; k < TOKENS.length; k++) {
        if (f.substr(i, TOKENS[k][0].length) === TOKENS[k][0]) { tok = TOKENS[k]; break; }
      }
      if (tok) {
        var len = tok[1] === "year" ? 4 : 2;
        segs.push({ type: tok[1], start: pos, end: pos + len, len: len, min: tok[2], max: tok[3],
                    editable: true });
        pos += len;
        i += tok[0].length;
      } else {
        var last = segs[segs.length - 1];
        if (last && !last.editable) { last.text += f[i]; last.end++; last.len++; } else {
          segs.push({ type: "literal", text: f[i], start: pos, end: pos + 1, len: 1, editable: false });
        }
        pos++;
        i++;
      }
    }
    return segs;
  }

  function dtiKind(segs) {
    var date = false, time = false, sec = false;
    segs.forEach(function (s) {
      if (/^(year|year2|month|day)$/.test(s.type)) { date = true; }
      if (/^(hour|hour12|minute|second|ampm)$/.test(s.type)) { time = true; }
      if (s.type === "second") { sec = true; }
    });
    return { date: date || !time, time: time, sec: sec };
  }

  // values are {y, mo, d, h, mi, s}
  function dtiParse(s, kind) {
    s = String(s || "");
    var m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2})(?::(\d{2}))?)?$/.exec(s);
    if (m && validYmd(+m[1], +m[2], +m[3])) {
      return { y: +m[1], mo: +m[2], d: +m[3], h: +(m[4] || 0), mi: +(m[5] || 0), s: +(m[6] || 0) };
    }
    m = /^(\d{2}):(\d{2})(?::(\d{2}))?$/.exec(s);
    if (m && kind.time) {
      var t = ymd(todayNum());
      return { y: t[0], mo: t[1], d: t[2], h: +m[1], mi: +m[2], s: +(m[3] || 0) };
    }
    return null;
  }

  function dtiIso(v, kind) {
    if (!v) { return ""; }
    var date = pad4(v.y) + "-" + pad(v.mo) + "-" + pad(v.d);
    var time = pad(v.h) + ":" + pad(v.mi) + (kind.sec ? ":" + pad(v.s) : "");
    return !kind.time ? date : (kind.date ? date + "T" + time : time);
  }

  // A number that orders values of this kind.
  function dtiOrd(v, kind) {
    var t = v.h * 3600 + v.mi * 60 + v.s, d = dnum(v.y, v.mo, v.d);
    return !kind.time ? d : (kind.date ? d * 86400 + t : t);
  }

  function segValue(v, seg) {
    switch (seg.type) {
      case "year": return v.y;
      case "year2": return v.y % 100;
      case "month": return v.mo;
      case "day": return v.d;
      case "hour": return v.h;
      case "hour12": return h12(v.h);
      case "minute": return v.mi;
      case "second": return v.s;
      case "ampm": return v.h < 12 ? 0 : 1;
      default: return null;
    }
  }

  // The day is clamped to the month's length (sigil's js/Date rolls over).
  function setSeg(v0, seg, val) {
    var v = $.extend({}, v0);
    switch (seg.type) {
      case "year": v.y = val; break;
      case "year2": v.y = Math.floor(v.y / 100) * 100 + val; break;
      case "month": v.mo = val; break;
      case "day": v.d = val; break;
      case "hour": v.h = val; break;
      case "hour12": {
        var pm = v.h >= 12;
        v.h = pm ? (val === 12 ? 12 : val + 12) : (val === 12 ? 0 : val);
        break;
      }
      case "minute": v.mi = val; break;
      case "second": v.s = val; break;
      case "ampm": v.h = val === 0 ? (v.h >= 12 ? v.h - 12 : v.h) : (v.h < 12 ? v.h + 12 : v.h); break;
      default: break;
    }
    v.d = Math.min(v.d, lastDay(v.y, v.mo));
    return v;
  }

  function segText(v, seg) {
    if (!seg.editable) { return seg.text; }
    if (seg.type === "ampm") { return v.h < 12 ? "AM" : "PM"; }
    var n = segValue(v, seg);
    return seg.len === 4 ? pad4(n) : pad(n);
  }

  function segMax(v, seg) { return seg.type === "day" ? lastDay(v.y, v.mo) : seg.max; }

  function dtiState(el) { return $.data(el, "ah-dti"); }

  function dtiEditable(st) { return st.segs.map(function (s, i) { return s.editable ? i : -1; })
    .filter(function (i) { return i >= 0; }); }

  function dtiDisplay(st) {
    return st.value ? st.segs.map(function (s) { return segText(st.value, s); }).join("") : "";
  }

  function dtiSelect(st) {
    var seg = st.segs[st.active];
    if (!seg || !st.value || document.activeElement !== st.input) { return; }
    try { st.input.setSelectionRange(seg.start, seg.end); } catch (err) { /* not focused */ }
  }

  // Show the value, publish it (data-ah-value, hidden input) and fire
  // `input' when it changed; `change' fires on leaving (dtiCommit).
  function dtiShow(el, $el) {
    var st = dtiState(el);
    st.input.value = dtiDisplay(st);
    dtiSelect(st);
    var iso = dtiIso(st.value, st.kind), old = el.getAttribute("data-ah-value") || "";
    el.setAttribute("data-ah-value", iso);
    $el.children("input[type=hidden]").val(iso);
    $el.find(".ah-dti-label").toggleClass("ah-dti-label-float", !!st.value || $el.hasClass("ah-dti-focused"));
    if (iso !== old) { $el.trigger("input"); }
    if (st.open) { dtiRenderCal(el); }
  }

  function dtiClamp(st, v) {
    if (!v) { return v; }
    var k = dtiOrd(v, st.kind);
    if (st.min && k < dtiOrd(st.min, st.kind)) { return $.extend({}, st.min); }
    if (st.max && k > dtiOrd(st.max, st.kind)) { return $.extend({}, st.max); }
    return v;
  }

  function dtiCommit(el, $el) {
    var st = dtiState(el);
    dtiFlush(st);
    st.value = dtiClamp(st, st.value);
    dtiShow(el, $el);
    var iso = dtiIso(st.value, st.kind);
    if (iso !== st.committed) {
      st.committed = iso;
      $el.trigger("change");
    }
  }

  function dtiEnsure(st) {
    if (!st.value) {
      var n = new Date();
      st.value = { y: n.getFullYear(), mo: n.getMonth() + 1, d: n.getDate(),
                   h: n.getHours(), mi: n.getMinutes(), s: 0 };
    }
  }

  function dtiFlush(st) {
    if (st.active === null || !st.buf) { return; }
    var seg = st.segs[st.active], n = parseInt(st.buf, 10);
    st.buf = "";
    if (st.value && !isNaN(n)) { st.value = setSeg(st.value, seg, Math.max(seg.min, Math.min(seg.max, n))); }
  }

  function dtiFocusSeg(el, $el, i) {
    var st = dtiState(el);
    dtiFlush(st);
    st.active = i;
    dtiShow(el, $el);
  }

  function dtiAnnounce(el, $el) {
    var st = dtiState(el), seg = st.segs[st.active];
    if (seg && st.value) { $el.find(".ah-dti-live").text(SEG_NAMES[seg.type] + " " + segText(st.value, seg)); }
  }

  // editor.cljs handle-digit!: buffer until the part is full, then commit
  // and move on
  function dtiDigit(el, $el, ch) {
    var st = dtiState(el), seg = st.segs[st.active];
    if (!seg || !seg.editable || seg.type === "ampm") { return; }
    dtiEnsure(st);
    st.buf += ch;
    if (st.buf.length >= seg.len) {
      var n = parseInt(st.buf, 10);
      st.buf = "";
      st.value = setSeg(st.value, seg, Math.max(seg.min, Math.min(segMax(st.value, seg), n)));
      var next = dtiEditable(st).filter(function (i) { return i > st.active; })[0];
      if (next !== undefined) { st.active = next; }
      dtiShow(el, $el);
    } else {
      // preview: the typed digits right-aligned in the part
      var shown = dtiDisplay(st), p = (new Array(seg.len - st.buf.length + 1)).join(" ") + st.buf;
      st.input.value = shown.slice(0, seg.start) + p + shown.slice(seg.end);
      dtiSelect(st);
    }
  }

  function dtiStep(el, $el, delta, big) {
    var st = dtiState(el), seg = st.segs[st.active];
    if (!seg || !seg.editable) { return; }
    dtiEnsure(st);
    dtiFlush(st);
    var v = st.value;
    if (seg.type === "ampm") {
      st.value = setSeg(v, seg, segValue(v, seg) ? 0 : 1);
    } else {
      var mn = seg.min, mx = segMax(v, seg), cur = segValue(v, seg), n;
      if (big) {
        n = mn + (((cur - mn + delta) % (mx - mn + 1)) + (mx - mn + 1)) % (mx - mn + 1);
      } else {
        n = cur + delta;
        n = n < mn ? mx : (n > mx ? mn : n);
      }
      st.value = setSeg(v, seg, n);
    }
    dtiShow(el, $el);
    dtiAnnounce(el, $el);
  }

  function dtiMoveSeg(el, $el, dir) {
    var st = dtiState(el), eds = dtiEditable(st);
    var next = dir > 0 ? eds.filter(function (i) { return i > st.active; })[0]
      : eds.filter(function (i) { return i < st.active; }).pop();
    if (next === undefined) { dtiFlush(st); return false; }
    dtiFocusSeg(el, $el, next);
    return true;
  }

  function dtiBlocked($el) { return $el.hasClass("ah-dti-disabled") || $el.hasClass("ah-dti-readonly"); }

  // editor.cljs on-keydown
  function dtiKey(el, $el, e) {
    var st = dtiState(el), k = e.key;
    if (k === "Tab") {
      if (!dtiBlocked($el) && dtiMoveSeg(el, $el, e.shiftKey ? -1 : 1)) { e.preventDefault(); } else { dtiClose(el, $el); }
      return;
    }
    if (e.ctrlKey || e.metaKey) { return; }
    if (k === "Escape") {
      if (st.open) { e.preventDefault(); dtiClose(el, $el); }
      return;
    }
    e.preventDefault();
    if (dtiBlocked($el)) { return; }
    if ((k === "ArrowDown" && e.altKey) || k === "F4") {
      if (st.open) { dtiClose(el, $el); } else { dtiOpen(el, $el); }
      return;
    }
    if (/^[0-9]$/.test(k)) { dtiDigit(el, $el, k); return; }
    var eds = dtiEditable(st), seg = st.segs[st.active];
    switch (k) {
      case "ArrowUp": dtiStep(el, $el, 1, false); break;
      case "ArrowDown": dtiStep(el, $el, -1, false); break;
      case "PageUp": dtiStep(el, $el, 10, true); break;
      case "PageDown": dtiStep(el, $el, -10, true); break;
      case "ArrowLeft": dtiMoveSeg(el, $el, -1); break;
      case "ArrowRight": dtiMoveSeg(el, $el, 1); break;
      case "Home": dtiFocusSeg(el, $el, eds[0]); break;
      case "End": dtiFocusSeg(el, $el, eds[eds.length - 1]); break;
      case "Backspace":
      case "Delete":
        if (seg && seg.editable && st.value) {
          st.buf = "";
          st.value = setSeg(st.value, seg, seg.min);
          dtiShow(el, $el);
        }
        break;
      case "a": case "A": case "p": case "P":
        if (seg && seg.type === "ampm" && st.value) {
          st.value = setSeg(st.value, seg, /a/i.test(k) ? 0 : 1);
          dtiShow(el, $el);
        }
        break;
      default: break;
    }
  }

  // format.cljs segment-at-cursor: the part under the caret, or the nearest
  function dtiSegAt(st, pos) {
    var eds = dtiEditable(st), best = eds[0], dist = Infinity;
    for (var i = 0; i < eds.length; i++) {
      var s = st.segs[eds[i]];
      if (pos >= s.start && pos < s.end) { return eds[i]; }
      var dd = Math.min(Math.abs(pos - s.start), Math.abs(pos - s.end));
      if (dd < dist) { dist = dd; best = eds[i]; }
    }
    return best;
  }

  // ---- drop-down calendar ----

  function dtiCalView(st) {
    var L = st.L, y = st.navY, m = st.navM, today = todayNum();
    var start = sow(dnum(y, m, 1), st.first);
    var sel = st.value ? dnum(st.value.y, st.value.mo, st.value.d) : null;
    var min = st.min && st.kind.date ? dnum(st.min.y, st.min.mo, st.min.d) : null;
    var max = st.max && st.kind.date ? dnum(st.max.y, st.max.mo, st.max.d) : null;
    var days = [];
    for (var i = 0; i < 42; i++) {
      var d = start + i, p = ymd(d);
      var dis = (min !== null && d < min) || (max !== null && d > max);
      days.push({
        cls: cls("ah-dti-cal-day", [[d === today, "ah-dti-cal-day-today"], [d === sel, "ah-dti-cal-day-selected"],
                                    [p[1] !== m, "ah-dti-cal-day-other"], [dis, "ah-dti-cal-day-disabled"]]),
        date: isoDate(d), day: String(p[2]), disabled: dis, selected: d === sel
      });
    }
    var weekdays = [];
    for (var k = 0; k < 7; k++) { weekdays.push({ label: L.weekdays[(st.first + k) % 7] }); }
    var title = L.title.replace(/yyyy|MMMM|MM|M/g, function (t) {
      return t === "yyyy" ? String(y) : t === "MMMM" ? L.months[m - 1] : t === "MM" ? pad(m) : String(m);
    });
    return { title: title, prev_month: L.prev_month, next_month: L.next_month, weekdays: weekdays,
             days: days, show_time: st.showTime && !!st.value, time_label: L.time,
             hours: st.value ? pad(st.value.h) : "", minutes: st.value ? pad(st.value.mi) : "" };
  }

  function dtiRenderCal(el) {
    var st = dtiState(el);
    var focused = document.activeElement;
    var field = focused && $.contains(st.$dd[0], focused) ? focused.getAttribute("data-field") : null;
    st.$dd.html(AH.tpl.datetime_input_calendar(dtiCalView(st)));
    if (field) { st.$dd.find('[data-field="' + field + '"]').trigger("focus"); }
    if (st.float) { st.float.update(); }
  }

  function dtiOpen(el, $el) {
    var st = dtiState(el);
    if (st.open || !st.$dd.length || dtiBlocked($el)) { return; }
    var v = st.value;
    var t = ymd(todayNum());
    st.navY = v ? v.y : t[0];
    st.navM = v ? v.mo : t[1];
    st.open = true;
    st.$dd.prop("hidden", false);
    dtiRenderCal(el);
    st.float = AH.float(st.$dd[0], $el.children(".ah-dti-row")[0], { offset: 2 });
    $(st.input).attr("aria-expanded", "true");
    $(document).on("mousedown" + st.ns, function (e) { if (outside(el, e)) { dtiClose(el, $el); } });
    $el.trigger("ah:open");
  }

  function dtiClose(el, $el) {
    var st = dtiState(el);
    if (!st.open) { return; }
    st.open = false;
    st.$dd.prop("hidden", true).empty();
    if (st.float) { st.float.stop(); st.float = null; }
    $(st.input).attr("aria-expanded", "false");
    $(document).off(st.ns);
    $el.trigger("ah:close");
  }

  // dropdown.cljs on-day-click: keep the time, clamp, fire change
  function dtiPickDay(el, $el, iso) {
    var st = dtiState(el), d = parseDate(iso);
    if (d === null) { return; }
    var p = ymd(d), v = st.value || { h: 0, mi: 0, s: 0 };
    st.value = dtiClamp(st, { y: p[0], mo: p[1], d: p[2], h: v.h, mi: v.mi, s: v.s });
    st.navY = st.value.y;
    st.navM = st.value.mo;
    dtiShow(el, $el);
    dtiCommit(el, $el);
    if (!st.showTime) { dtiClose(el, $el); st.input.focus(); }
  }

  function dtiSpinStop(st) { clearTimeout(st.spin); st.spin = null; }

  AH.define("datetime_input", {
    init: function (el, $el) {
      ensureId(el, "ah-dti");
      var segs = dtiSegments(el.getAttribute("data-ah-format") || "yyyy-MM-dd");
      var kind = dtiKind(segs);
      var first = parseInt(el.getAttribute("data-ah-first-day") || "0", 10);
      var st = {
        ns: ".ahdti" + (++seq),
        segs: segs, kind: kind,
        input: $el.find("input.ah-dti-input")[0],
        $dd: $el.children(".ah-dti-dropdown"),
        value: dtiParse(el.getAttribute("data-ah-value"), kind),
        min: dtiParse(el.getAttribute("data-ah-min"), kind),
        max: dtiParse(el.getAttribute("data-ah-max"), kind),
        first: first >= 0 && first <= 6 ? first : 0,
        showTime: el.hasAttribute("data-ah-show-time"),
        L: $.extend({}, DTI_LABELS, readJson(el, "data-ah-labels", {})),
        active: null, buf: "", open: false, float: null, spin: null
      };
      st.committed = dtiIso(st.value, kind);
      $.data(el, "ah-dti", st);
      var $in = $(st.input);
      $in.on("focus" + NS, function () {
        $el.addClass("ah-dti-focused");
        if (st.active === null) { st.active = dtiEditable(st)[0]; }
        $el.find(".ah-dti-label").addClass("ah-dti-label-float");
        setTimeout(function () { dtiSelect(st); }, 0);
      }).on("mouseup" + NS, function () {
        if (!st.value) { return; }
        dtiFocusSeg(el, $el, dtiSegAt(st, st.input.selectionStart || 0));
      }).on("keydown" + NS, function (e) { dtiKey(el, $el, e); })
        // the text field is internal: only the root reports changes
        .on("change" + NS + " input" + NS, function (e) { e.stopPropagation(); });
      // leaving the component (the time fields of the drop-down are inside)
      $el.on("focusout" + NS, function (e) {
        if (e.relatedTarget && $.contains(el, e.relatedTarget)) { return; }
        setTimeout(function () {
          if ($.contains(el, document.activeElement)) { return; }
          $el.removeClass("ah-dti-focused");
          dtiCommit(el, $el);
          dtiClose(el, $el);
        }, 0);
      });
      $el.on("click" + NS, ".ah-dti-cal-btn", function () {
        if (dtiBlocked($el)) { return; }
        st.input.focus();
        if (st.open) { dtiClose(el, $el); } else { dtiOpen(el, $el); }
      });
      $el.on("mousedown" + NS, ".ah-dti-cal-btn", function (e) { e.preventDefault(); });
      // spinner: step, then repeat after 400ms every 120ms while held
      $el.on("mousedown" + NS, ".ah-dti-spin", function (e) {
        e.preventDefault();
        if (dtiBlocked($el)) { return; }
        var delta = $(this).hasClass("ah-dti-spin-up") ? 1 : -1;
        if (st.active === null) { st.active = dtiEditable(st)[0]; }
        st.input.focus();
        dtiSpinStop(st);
        dtiStep(el, $el, delta, false);
        var rep = function () { dtiStep(el, $el, delta, false); st.spin = setTimeout(rep, 120); };
        st.spin = setTimeout(rep, 400);
      }).on("mouseup" + NS + " mouseleave" + NS, ".ah-dti-spin", function () { dtiSpinStop(st); });
      // drop-down: keep the focus in the field, except for the time inputs
      st.$dd.on("mousedown" + NS, function (e) {
        if (!$(e.target).is("input")) { e.preventDefault(); }
      }).on("click" + NS, ".ah-dti-cal-day", function () {
        if (!$(this).hasClass("ah-dti-cal-day-disabled")) { dtiPickDay(el, $el, this.getAttribute("data-date")); }
      }).on("click" + NS, "[data-action]", function () {
        var n = dnum(st.navY, st.navM, 1);
        var t = ymd(addMonths(n, this.getAttribute("data-action") === "prev-month" ? -1 : 1));
        st.navY = t[0];
        st.navM = t[1];
        dtiRenderCal(el);
      }).on("change" + NS, ".ah-dti-time-input", function (e) {
        e.stopPropagation();
        var n = parseInt(this.value, 10);
        if (isNaN(n)) { return; }
        dtiEnsure(st);
        st.value = $.extend({}, st.value);
        if (this.getAttribute("data-field") === "hours") { st.value.h = Math.max(0, Math.min(23, n)); }
        else { st.value.mi = Math.max(0, Math.min(59, n)); }
        dtiShow(el, $el);
        dtiCommit(el, $el);
      }).on("input" + NS, ".ah-dti-time-input", function (e) { e.stopPropagation(); })
        .on("keydown" + NS, ".ah-dti-time-input", function (e) {
          if (e.key === "Escape") { e.preventDefault(); dtiClose(el, $el); st.input.focus(); }
          if (e.key === "Enter") { e.preventDefault(); $(this).trigger("change"); }
        });
    },
    destroy: function (el, $el) {
      var st = dtiState(el);
      if (!st) { return; }
      dtiSpinStop(st);
      dtiClose(el, $el);
    },
    methods: {
      setValue: function (el, $el, v) {
        var st = dtiState(el);
        st.value = v ? dtiParse(v, st.kind) : null;
        st.buf = "";
        st.committed = dtiIso(st.value, st.kind);
        dtiShow(el, $el);
      },
      getValue: function (el) { return el.getAttribute("data-ah-value"); },
      clear: function (el, $el) {
        var st = dtiState(el);
        st.value = null;
        st.buf = "";
        dtiShow(el, $el);
        dtiCommit(el, $el);
      },
      open: function (el, $el) { dtiOpen(el, $el); },
      close: function (el, $el) { dtiClose(el, $el); }
    }
  });
})(window.jQuery, window.AH);
