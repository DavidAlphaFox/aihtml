/* Behaviour of the datepicker (designs/04-components.md). Ported from
 * sigil: form/datepicker (+ calendar, popup, util). Date math is plain JS
 * on local dates.
 *
 * Value-bearing: data-ah-value and the hidden input, then `change` on the
 * root; `ah:open` / `ah:close` when the popup shows / hides. */
import AH from "../core.js";
import "virtual:ah-tpl/datepicker_month";

var seq = 0;

function ensureId(el, prefix) {
  if (!el.id) { el.id = prefix + (++seq); }
  return el.id;
}

// A mousedown outside the component. A target that is gone was inside
// a list re-rendered by this very mousedown (picking in multiple mode).
function outside(el, e) {
  return e.target.isConnected !== false && !el.contains(e.target);
}

// Value-bearing contract: data-ah-value + hidden input, then `change`.
function publish(c, value, fire) {
  var el = c.element;
  var old = el.getAttribute("data-ah-value") || "";
  el.setAttribute("data-ah-value", value);
  var hidden = el.querySelector(":scope > input[type=hidden]");
  if (hidden) { hidden.value = value; }
  if (fire && old !== value) { c.fire("change"); }
}

// ==================================================================
// datepicker
// ==================================================================

var DAY = 86400000;

function pad(n) { return (n < 10 ? "0" : "") + n; }
function mk(y, m, d) { return new Date(y, m, d); }          // m: 0..11, overflow ok
function iso(d) { return d ? d.getFullYear() + "-" + pad(d.getMonth() + 1) + "-" + pad(d.getDate()) : ""; }
function parse(s) {
  var m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(s || "");
  return m ? mk(+m[1], +m[2] - 1, +m[3]) : null;
}
function today() { var n = new Date(); return mk(n.getFullYear(), n.getMonth(), n.getDate()); }
function same(a, b) { return !!(a && b) && a.getTime() === b.getTime(); }
function addDays(d, n) { return mk(d.getFullYear(), d.getMonth(), d.getDate() + n); }
// Month arithmetic keeps the day, clamped to the target month (date-fns addMonths).
function addMonths(d, n) {
  var t = mk(d.getFullYear(), d.getMonth() + n, 1);
  var last = mk(t.getFullYear(), t.getMonth() + 1, 0).getDate();
  return mk(t.getFullYear(), t.getMonth(), Math.min(d.getDate(), last));
}
function startOfWeek(d, first) { return addDays(d, -((d.getDay() - first + 7) % 7)); }
// date-fns getWeek with weekStartsOn = first, firstWeekContainsDate = 1.
function weekNumber(d, first) {
  var y = d.getFullYear();
  var wy = d >= startOfWeek(mk(y + 1, 0, 1), first) ? y + 1
    : (d >= startOfWeek(mk(y, 0, 1), first) ? y : y - 1);
  var diff = startOfWeek(d, first) - startOfWeek(mk(wy, 0, 1), first);
  return Math.round(diff / (7 * DAY)) + 1;
}

// The display formats the Erlang side knows: yyyy yy MMMM MMM MM M dd d.
function format(d, fmt, L) {
  if (!d) { return ""; }
  return fmt.replace(/yyyy|yy|MMMM|MMM|MM|M|dd|d/g, function (t) {
    switch (t) {
      case "yyyy": return String(d.getFullYear());
      case "yy": return String(d.getFullYear()).slice(2);
      case "MMMM": return L.months[d.getMonth()];
      case "MMM": return L.months_short[d.getMonth()];
      case "MM": return pad(d.getMonth() + 1);
      case "M": return String(d.getMonth() + 1);
      case "dd": return pad(d.getDate());
      default: return String(d.getDate());
    }
  });
}

var DEFAULT_LABELS = {
  months: ["January", "February", "March", "April", "May", "June", "July",
           "August", "September", "October", "November", "December"],
  months_short: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
  weekdays: ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"],
  title: "MMMM yyyy", today: "Today", clear: "Clear",
  prev_month: "Previous month", next_month: "Next month",
  prev_year: "Previous year", next_year: "Next year"
};

function dpDisabled(st, d) {
  return (st.min && d < st.min) || (st.max && d > st.max) || !!st.off[iso(d)];
}

function dpDisplay(st) {
  if (st.range) {
    if (!st.from) { return ""; }
    return format(st.from, st.fmt, st.L) + (st.to ? " - " + format(st.to, st.fmt, st.L) : "");
  }
  return format(st.value, st.fmt, st.L);
}

function dpIsoValue(st) {
  return st.range ? (st.from ? iso(st.from) + "," + iso(st.to) : "") : iso(st.value);
}

// The view of templates/datepicker_month.mustache: every class, label
// and id computed here (the Erlang month_view/1 builds the same view for
// the server's `inline' render).
function dpView(el, st) {
  var L = st.L;
  var y = st.viewY, m = st.viewM;
  var first = st.firstDay;
  var monthStart = mk(y, m, 1);
  var gridStart = startOfWeek(monthStart, first);
  var gridEnd = addDays(startOfWeek(mk(y, m + 1, 0), first), 7);
  var now = today();
  var from = st.range ? (st.pending || st.from) : null;
  var to = st.range ? (st.pending ? null : st.to) : null;
  var weeks = [];
  for (var w = gridStart; w < gridEnd; w = addDays(w, 7)) {
    var days = [];
    for (var k = 0; k < 7; k++) {
      var d = addDays(w, k);
      var other = d.getMonth() !== m;
      if (other && !st.otherMonth) { days.push({ empty: true }); continue; }
      var dis = !!dpDisabled(st, d);
      var isStart = same(d, from), isEnd = same(d, to);
      var sel = st.range ? (isStart || isEnd) : same(d, st.value);
      var inRange = !!(st.range && from && to && d >= from && d <= to);
      var dow = d.getDay();
      days.push({
        empty: false,
        cls: "ah-datepicker-day" +
          (other ? " ah-datepicker-day-other-month" : "") +
          (same(d, now) ? " ah-datepicker-day-today" : "") +
          (st.weekends && (dow === 0 || dow === 6) ? " ah-datepicker-day-weekend" : "") +
          (dis ? " ah-datepicker-day-disabled" : "") +
          (sel ? " ah-datepicker-day-selected" : "") +
          (inRange ? " ah-datepicker-day-in-range" : "") +
          (isStart ? " ah-datepicker-day-range-start" : "") +
          (isEnd ? " ah-datepicker-day-range-end" : "") +
          (same(d, st.focus) ? " ah-datepicker-day-focused" : ""),
        id: el.id + "-d" + iso(d),
        date: iso(d),
        selected: String(sel),
        disabled: String(dis),
        label: format(d, "d MMMM yyyy", L),
        day: String(d.getDate())
      });
    }
    weeks.push({ num: String(weekNumber(w, first)), days: days });
  }
  var weekdays = [];
  for (var i = 0; i < 7; i++) { weekdays.push({ label: L.weekdays[(first + i) % 7] }); }
  return {
    title_id: el.id + "-title",
    title: format(monthStart, L.title, L),
    prev_year: L.prev_year, prev_month: L.prev_month,
    next_month: L.next_month, next_year: L.next_year,
    today: L.today,
    week_numbers: st.weekNumbers,
    weekdays: weekdays,
    weeks: weeks
  };
}

function dpRender(c) {
  var st = c.st, el = c.element;
  st.popup.innerHTML = AH.tpl.datepicker_month(dpView(el, st));
  if (st.focus) {
    st.input.setAttribute("aria-activedescendant", el.id + "-d" + iso(st.focus));
  } else {
    st.input.removeAttribute("aria-activedescendant");
  }
}

// popup.cljs position!: at least 280px wide (the viewport minus a
// margin on small screens); AH.float keeps it fixed under the field (or
// above, when there is no room), inside the viewport and following
// scrolls, so clipping ancestors do not cut it.
function dpPosition(c) {
  var st = c.st;
  var w = Math.min(Math.max(c.element.getBoundingClientRect().width, 280), window.innerWidth - 16);
  st.popup.style.width = w + "px";
  if (st.float) { st.float.update(); } else { st.float = AH.float(st.popup, c.element); }
}

function dpBlocked(el) {
  return el.classList.contains("ah-datepicker-disabled") || el.classList.contains("ah-datepicker-readonly");
}

function dpOpen(c) {
  var st = c.st, el = c.element;
  if (st.open || (dpBlocked(el) && !st.inline)) { return; }
  st.pending = null;
  st.focus = (st.range ? st.from : st.value) || today();
  st.viewY = st.focus.getFullYear();
  st.viewM = st.focus.getMonth();
  st.open = true;
  dpRender(c);
  if (st.inline) { return; }        // always shown, in the flow
  st.popup.style.display = "block";
  dpPosition(c);
  el.classList.add("ah-datepicker-open");
  st.input.setAttribute("aria-expanded", "true");
  st.outside = new AbortController();
  document.addEventListener("mousedown", function (e) {
    if (outside(el, e)) { dpClose(c); }
  }, { signal: st.outside.signal });
  c.fire("ah:open");
}

function dpStop(st) {
  if (st.float) { st.float.stop(); st.float = null; }
  if (st.outside) { st.outside.abort(); st.outside = null; }
}

function dpClose(c) {
  var st = c.st, el = c.element;
  if (!st.open || st.inline) { return; }
  st.open = false;
  st.pending = null;
  st.popup.style.display = "none";
  el.classList.remove("ah-datepicker-open");
  st.input.setAttribute("aria-expanded", "false");
  st.input.removeAttribute("aria-activedescendant");
  dpStop(st);
  c.fire("ah:close");
}

function dpCommit(c, fire) {
  var st = c.st;
  st.input.value = dpDisplay(st);
  publish(c, dpIsoValue(st), fire);
}

function dpSetFocus(c, d) {
  var st = c.st;
  st.focus = d;
  st.viewY = d.getFullYear();
  st.viewM = d.getMonth();
  dpRender(c);
  if (st.open && !st.inline) { dpPosition(c); }
}

function dpNavMonths(c, n) {
  var st = c.st;
  var f = st.focus && st.focus.getMonth() === st.viewM ? st.focus : mk(st.viewY, st.viewM, 1);
  dpSetFocus(c, addMonths(f, n));
}

// calendar.cljs handle-day-click: single selects and closes; range takes
// a start, then an end (sorted), then closes.
function dpPick(c, d) {
  var st = c.st;
  if (!d || dpDisabled(st, d) || dpBlocked(c.element)) { return; }
  if (!st.range) {
    st.value = d;
    st.focus = d;
    dpCommit(c, true);
    dpClose(c);
    if (st.inline) { dpRender(c); }
  } else if (!st.pending) {
    st.pending = d;
    st.focus = d;
    dpRender(c);
  } else {
    var a = st.pending;
    st.from = d < a ? d : a;
    st.to = d < a ? a : d;
    st.pending = null;
    dpCommit(c, true);
    dpClose(c);
    if (st.inline) { dpRender(c); }
  }
}

function dpHover(c, d) {
  var st = c.st;
  if (!st.range || !st.pending) { return; }
  var a = st.pending < d ? st.pending : d;
  var b = st.pending < d ? d : st.pending;
  st.popup.querySelectorAll(".ah-datepicker-day[data-date]").forEach(function (cell) {
    var x = parse(cell.getAttribute("data-date"));
    cell.classList.toggle("ah-datepicker-day-hover-range", x >= a && x <= b);
  });
}

// popup.cljs handle-keydown. Up/Down move a week (sigil moves a day),
// Shift+PageUp/PageDown a year.
function dpKey(c, e) {
  var st = c.st, el = c.element;
  if (dpBlocked(el)) { return; }
  var open = st.open;
  var f = st.focus || today();
  switch (e.key) {
    case "ArrowDown":
      e.preventDefault();
      if (e.altKey || !open) { dpOpen(c); } else { dpSetFocus(c, addDays(f, 7)); }
      break;
    case "ArrowUp":
      if (!open) { return; }
      e.preventDefault();
      if (e.altKey) { dpClose(c); } else { dpSetFocus(c, addDays(f, -7)); }
      break;
    case "ArrowLeft":
      if (open) { e.preventDefault(); dpSetFocus(c, addDays(f, -1)); }
      break;
    case "ArrowRight":
      if (open) { e.preventDefault(); dpSetFocus(c, addDays(f, 1)); }
      break;
    case "Enter":
    case " ":
      e.preventDefault();
      if (open) { dpPick(c, f); } else { dpOpen(c); }
      break;
    case "Escape":
      if (open) { e.preventDefault(); dpClose(c); }
      break;
    case "Tab":
      dpClose(c);
      break;
    case "PageUp":
      if (open) { e.preventDefault(); dpNavMonths(c, e.shiftKey ? -12 : -1); }
      break;
    case "PageDown":
      if (open) { e.preventDefault(); dpNavMonths(c, e.shiftKey ? 12 : 1); }
      break;
    case "Home":
      if (open) { e.preventDefault(); dpSetFocus(c, mk(st.viewY, st.viewM, 1)); }
      break;
    case "End":
      if (open) { e.preventDefault(); dpSetFocus(c, mk(st.viewY, st.viewM + 1, 0)); }
      break;
    case "Backspace":
    case "Delete":
      if (el.classList.contains("ah-datepicker-clearable")) { e.preventDefault(); dpClear(c, true); }
      break;
    default:
      break;
  }
}

function dpClear(c, fire) {
  var st = c.st;
  st.value = st.from = st.to = st.pending = null;
  dpCommit(c, fire);
  if (st.open) { dpRender(c); }
}

function dpSet(c, v) {
  var st = c.st;
  if (Array.isArray(v)) { v = v.join(","); }
  v = v || "";
  if (st.range) {
    var p = v.split(",");
    var a = parse(p[0]), b = parse(p[1]);
    st.from = a && b && b < a ? b : a;
    st.to = a && b && b < a ? a : b;
  } else {
    st.value = parse(v);
  }
  st.pending = null;
  dpCommit(c, false);
  if (st.open) { dpRender(c); }
}

AH.register("datepicker", class extends AH.Controller {
  setup() {
    var c = this, el = this.element;
    ensureId(el, "ah-dp");
    var labels = {};
    try { labels = JSON.parse(el.getAttribute("data-ah-labels") || "{}"); } catch (err) { labels = {}; }
    var off = {};
    (el.getAttribute("data-ah-disabled-dates") || "").split(",").forEach(function (d) {
      if (d) { off[d] = true; }
    });
    var st = this.st = {
      input: el.querySelector("input.ah-datepicker-input"),
      popup: el.querySelector(":scope > .ah-datepicker-popup"),
      range: el.hasAttribute("data-ah-range"),
      fmt: el.getAttribute("data-ah-format") || "yyyy-MM-dd",
      min: parse(el.getAttribute("data-ah-min")),
      max: parse(el.getAttribute("data-ah-max")),
      off: off,
      firstDay: parseInt(el.getAttribute("data-ah-first-day") || "0", 10) || 0,
      weekNumbers: el.hasAttribute("data-ah-week-numbers"),
      weekends: el.hasAttribute("data-ah-weekends"),
      otherMonth: el.getAttribute("data-ah-other-month-days") !== "false",
      L: Object.assign({}, DEFAULT_LABELS, labels),
      inline: el.classList.contains("ah-datepicker-inline"),
      open: false, value: null, from: null, to: null, pending: null, focus: null,
      float: null, outside: null
    };
    st.popup.id = el.id + "-popup";
    st.input.setAttribute("aria-controls", el.id + "-popup");
    var v = el.getAttribute("data-ah-value") || "";
    if (st.range) {
      var p = v.split(",");
      st.from = parse(p[0]);
      st.to = parse(p[1]);
    } else {
      st.value = parse(v);
    }

    var input = st.input, popup = st.popup;
    this.listen(input, "focus", function () { el.classList.add("ah-datepicker-focused"); });
    this.listen(input, "blur", function () { el.classList.remove("ah-datepicker-focused"); });
    this.listen(input, "click", function (e) {
      e.preventDefault();
      if (st.open) { dpClose(c); } else { dpOpen(c); }
    });
    this.listen(input, "keydown", function (e) { dpKey(c, e); });
    // the text field is internal: only the root reports changes
    var stop = function (e) { e.stopPropagation(); };
    this.listen(input, "change", stop);
    this.listen(input, "input", stop);
    this.delegate("click", ".ah-datepicker-trigger", function (e) {
      e.preventDefault();
      e.stopPropagation();
      input.focus();
      if (st.open) { dpClose(c); } else { dpOpen(c); }
    });
    this.delegate("mousedown", ".ah-datepicker-clear", function (e) { e.preventDefault(); });
    this.delegate("click", ".ah-datepicker-clear", function (e) {
      e.preventDefault();
      e.stopPropagation();
      dpClear(c, true);
    });
    // Keep the focus in the text field while using the calendar.
    this.listen(popup, "mousedown", function (e) {
      e.preventDefault();
      if (st.inline && !dpBlocked(el)) { input.focus(); }
    });
    // The popup's own delegated clicks run first, then it stops the click.
    this.delegate("click", ".ah-datepicker-day[data-date]", function (e, day) {
      dpPick(c, parse(day.getAttribute("data-date")));
    }, popup);
    this.delegate("click", "[data-nav]", function (e, b) {
      dpNavMonths(c, parseInt(b.getAttribute("data-nav"), 10));
    }, popup);
    this.delegate("click", ".ah-datepicker-today-btn", function () { dpSetFocus(c, today()); }, popup);
    this.listen(popup, "click", function (e) { e.stopPropagation(); });
    // mouseenter on a day: mouseover from outside that day
    this.delegate("mouseover", ".ah-datepicker-day[data-date]", function (e, day) {
      if (e.relatedTarget && day.contains(e.relatedTarget)) { return; }
      dpHover(c, parse(day.getAttribute("data-date")));
    }, popup);
    if (st.inline) { dpOpen(this); }
  }

  teardown() { if (this.st) { dpStop(this.st); } }

  // methods (aihtml_action:call/4, AH.invoke)
  // setValue("2026-09-29"), setValue("from,to") or setValue([from, to]);
  // no change event (the server set it).
  setValue(v) { dpSet(this, v); }
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  clear() { dpClear(this, true); }
  open() { dpOpen(this); }
  close() { dpClose(this); }
});
