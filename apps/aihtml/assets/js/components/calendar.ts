/* Behaviour of the calendar (designs/04-components.md). Ported from
 * sigil: form/calendar (+ daygrid, timegrid, list, shared, recurrence,
 * util).
 *
 * Dates are day numbers (days since 1970-01-01) and times minutes since
 * day 0 (_lib_date), computed with UTC arithmetic: event times are local
 * wall times without a zone, so there is no DST or zone shifting. The view
 * builders (calMonth, calTimegrid, calList) are the twins of month_view/4,
 * timegrid_view/4 and list_view/4 in aihtml_calendar.erl: both feed the
 * same templates (calendar_month, calendar_timegrid, calendar_list).
 * Recurring events are expanded by _lib_rrule.
 *
 * Events: `change` when the date or view changed; ah:event-click (detail
 * CalendarEventClick: {event, raw}), ah:more-click (CalendarMoreClick:
 * {date}; cancelable: preventDefault keeps the view), ah:event-drop /
 * ah:event-resize (CalendarEventDrop: {event, from, to, allDay, days?}),
 * ah:select (CalendarSelect: {from, to, allDay}). Their fields are also
 * written to the root as data-* (Event.data of a postback).
 *
 * With the href option (data-ah-href) the toolbar entries are links to
 * the state they lead to; a plain click still navigates here and pushes
 * the link's URL, modified clicks are left to the browser. */
import AH from "../core.ts";
import {
  DAY, addMonths, dnum, dow, isoDate, isoTime, lastDay, pad, parseDate, parseTime, sow, todayNum, ymd
} from "./_lib_date.ts";
import type { DayNum, Minutes } from "./_lib_date.ts";
import { expand, parse as parseRule, stamp } from "./_lib_rrule.ts";
import "virtual:ah-tpl/calendar_list";
import "virtual:ah-tpl/calendar_month";
import "virtual:ah-tpl/calendar_timegrid";

/** An event as the calendar keeps it (and getEvents returns it): the
 *  server's normalize_event/2. */
export interface CalendarEvent {
  id: string;
  title: string;
  start: string;
  end: string;
  allDay: boolean;
  color?: string;
  rrule?: string;
  exdates?: string[];
  status?: string;
}

/** Detail of ah:event-click: the series id and the clicked occurrence. */
export interface CalendarEventClick { event: string; raw: CalendarEvent; }
/** Detail of ah:more-click (cancelable). */
export interface CalendarMoreClick { date: string | null; }
/** Detail of ah:event-drop and ah:event-resize. */
export interface CalendarEventDrop { event: string; from: string; to: string; allDay: boolean; days?: number; }
/** Detail of ah:select: a day range (to exclusive) or a time range. */
export interface CalendarSelect { from: string; to: string; allDay: boolean; }

type LabelValue = string | string[];

const CAL_LABELS = {
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
type Labels = typeof CAL_LABELS;

const DEFAULT_COLOR = "var(--ah-color-primary)";
const COLOR_RE = /^[#a-zA-Z0-9(),.%\s-]+$/;
const STATUS_COLORS: Record<string, string> = {
  confirmed: "var(--ah-color-success)", tentative: "var(--ah-color-warning)",
  cancelled: "var(--ah-color-error)"
};
const VIEWS = ["month", "week", "day", "list"];

function readJson(el: Element, attr: string): unknown {
  try { return JSON.parse(el.getAttribute(attr) || "null"); } catch (err) { return null; }
}

function isRecord(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null;
}

// The defaults, overridden by the labels of data-ah-labels of the same
// kind (a text or a list of texts).
function readLabels<T extends Record<string, LabelValue>>(el: Element, dflt: T): T {
  const out: Record<string, LabelValue> = { ...dflt };
  const raw = readJson(el, "data-ah-labels");
  if (isRecord(raw)) {
    Object.keys(raw).forEach((k) => {
      const v = raw[k], d = out[k];
      if (typeof v === "string" && (d === undefined || typeof d === "string")) { out[k] = v; }
      if (Array.isArray(v) && (d === undefined || Array.isArray(d))) { out[k] = v.map(String); }
    });
  }
  return out as T;
}

// Display formats: yyyy yy MMMM MMM MM M dd d EEEE EEE (fmt/3 in Erlang).
function fmtDate(n: DayNum, f: string, L: Labels): string {
  const p = ymd(n);
  return f.replace(/yyyy|yy|MMMM|MMM|MM|M|dd|d|EEEE|EEE/g, (t) => {
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

// The minutes of a normalized event's start or end.
function mins(s: string): Minutes {
  const p = parseTime(s);
  if (!p) { throw new Error("calendar: bad time " + s); }
  return p.t;
}

// An event as the server sends it (normalize_event/2), from any event
// object a page passes: start required, end and allDay defaulted.
function calNormalize(raw: unknown, n: number): CalendarEvent {
  const e = isRecord(raw) ? raw : {};
  const s = parseTime(e.start);
  if (!s) { throw new Error("calendar: bad event start " + String(e.start)); }
  const allDayFlag = e.allDay === true || e.all_day === true || s.dateOnly;
  const end = e.end !== undefined && e.end !== null ? parseTime(e.end) : null;
  const et = end ? end.t : s.t + (allDayFlag ? DAY : 60);
  const allDay = allDayFlag || (s.t % DAY === 0 && et % DAY === 0 && s.t !== et);
  const out: CalendarEvent = {
    id: e.id !== undefined && e.id !== null ? String(e.id) : "ev" + n,
    title: e.title === undefined || e.title === null ? "" : String(e.title),
    start: isoTime(s.t, allDay), end: isoTime(et, allDay), allDay
  };
  if (e.color && COLOR_RE.test(String(e.color))) { out.color = String(e.color); }
  if (e.rrule) { out.rrule = String(e.rrule); }
  if (Array.isArray(e.exdates)) { out.exdates = e.exdates.map(String); }
  if (e.status) { out.status = String(e.status); }
  return out;
}

// ---- recurrence (_lib_rrule) ----

/** One occurrence of an event in the shown range. */
interface Inst { id: string; src: CalendarEvent; s: Minutes; e: Minutes; allDay: boolean; }

// Instances overlapping [rs, re), in event order.
function instances(events: readonly CalendarEvent[], rs: Minutes, re: Minutes): Inst[] {
  const out: Inst[] = [];
  events.forEach((ev) => {
    const s = mins(ev.start), e = mins(ev.end);
    if (!ev.rrule) {
      if (s < re && e > rs) { out.push({ id: ev.id, src: ev, s, e, allDay: ev.allDay }); }
      return;
    }
    expand(s, e, parseRule(ev.rrule), rs, re, ev.exdates || []).forEach((p) => {
      out.push({ id: ev.id + "_" + stamp(p[0]), src: ev, s: p[0], e: p[1], allDay: ev.allDay });
    });
  });
  return out;
}

function inRange(insts: readonly Inst[], from: Minutes, to: Minutes): Inst[] {
  return insts.filter((i) => i.s < to && i.e > from);
}

// ---- views ----

/** The settings the views are built from (read once in setup). */
interface CalConf {
  first: number;
  agendaDays: number;
  maxEvents: number;
  slotDur: number;
  slotH: number;
  hour24: boolean;
  L: Labels;
  editable: boolean;
}

function cls(base: string, opts: [boolean, string][]): string {
  return base + opts.filter((o) => o[0]).map((o) => " " + o[1]).join("");
}

function h12(h: number): number { return h === 0 ? 12 : (h > 12 ? h - 12 : h); }
function fmtClock(t: Minutes, cf: CalConf): string {
  const r = ((t % DAY) + DAY) % DAY, h = Math.floor(r / 60), m = pad(r % 60);
  return cf.hour24 ? pad(h) + ":" + m : h12(h) + ":" + m + " " + (h < 12 ? cf.L.am : cf.L.pm);
}
function srcColor(src: CalendarEvent): string { return src.color || DEFAULT_COLOR; }

// [rs, re) in days and the title
function calProfile(cf: CalConf, view: string, cur: DayNum): [DayNum, DayNum, string] {
  const L = cf.L;
  switch (view) {
    case "week": {
      const p = sow(cur, cf.first);
      return [p, p + 7, fmtDate(p, L.range_start, L) + " – " + fmtDate(p + 6, L.range_end, L)];
    }
    case "day":
      return [cur, cur + 1, fmtDate(cur, L.title_day, L)];
    case "list":
      return [cur, cur + cf.agendaDays,
              fmtDate(cur, L.range_start, L) + " – " + fmtDate(cur + cf.agendaDays - 1, L.range_end, L)];
    default: {
      const y = ymd(cur);
      return [sow(dnum(y[0], y[1], 1), cf.first), sow(dnum(y[0], y[1], lastDay(y[0], y[1])), cf.first) + 7,
              fmtDate(cur, L.title_month, L)];
    }
  }
}

/** An event's piece in one week row of the month view. */
interface Segment {
  inst: Inst; sc: number; ec: number; span: number; multi: boolean;
  cont: boolean; conts: boolean; n: number; row: number;
}

// sigil's compute-week-segments (week_segments/2 in Erlang)
function weekSegments(w: DayNum, insts: readonly Inst[]): Segment[] {
  const segs: Segment[] = inRange(insts, w * DAY, (w + 7) * DAY).map((i, n) => {
    const vs = Math.floor(i.s / DAY);
    const ve = i.allDay ? Math.max(vs + 1, Math.floor((i.e + DAY - 1) / DAY)) : vs + 1;
    const cs = Math.max(vs, w), ce = Math.min(ve, w + 7);
    return { inst: i, sc: cs - w + 1, ec: ce - w + 1, span: ce - cs, multi: ve - vs > 1,
             cont: vs < w, conts: ve > w + 7, n, row: 0 };
  }).filter((g) => g.span > 0);
  segs.sort((a, b) =>
    ((a.multi ? 0 : 1) - (b.multi ? 0 : 1)) || (b.span - a.span) ||
    (a.inst.s % DAY - b.inst.s % DAY) || (a.n - b.n));
  const rows: [number, number][][] = [];
  segs.forEach((g) => {
    let r = 0;
    for (; r < rows.length; r++) {
      if (!rows[r].some((x) => g.sc < x[1] && g.ec > x[0])) { break; }
    }
    if (r === rows.length) { rows.push([]); }
    rows[r].push([g.sc, g.ec]);
    g.row = r;
  });
  return segs;
}

function calMonth(cf: CalConf, cur: DayNum, rs: DayNum, re: DayNum, insts: readonly Inst[]): unknown {
  const L = cf.L, today = todayNum(), curM = ymd(cur)[1], max = cf.maxEvents;
  const headers: { label: string }[] = [];
  for (let i = 0; i < 7; i++) { headers.push({ label: L.weekdays_short[(cf.first + i) % 7] }); }
  const weeks: unknown[] = [];
  for (let w = rs; w < re; w += 7) {
    const segs = weekSegments(w, insts), over: Record<number, number> = {};
    segs.forEach((g) => {
      if (g.row >= max) { for (let c = g.sc; c < g.ec; c++) { over[c] = (over[c] || 0) + 1; } }
    });
    const days: unknown[] = [];
    for (let k = 0; k < 7; k++) {
      const d = w + k, p = ymd(d), other = p[1] !== curM;
      days.push({
        bg_cls: cls("ah-calendar-day", [[d === today, "ah-calendar-day-today"], [other, "ah-calendar-day-other"]]),
        num_cls: cls("ah-calendar-day-num", [[d === today, "ah-calendar-day-num-today"],
                                             [other, "ah-calendar-day-num-other"]]),
        date: isoDate(d), col: String(k + 1), num: String(p[2])
      });
    }
    weeks.push({
      days,
      events: segs.filter((g) => g.row < max).map((g) => ({
        cls: cls("ah-calendar-event ah-calendar-daygrid-event",
                 [[g.multi, "ah-calendar-daygrid-event-multi"], [g.cont, "ah-calendar-daygrid-event-start"],
                  [g.conts, "ah-calendar-daygrid-event-end"]]),
        id: g.inst.id, sc: String(g.sc), ec: String(g.ec), row: String(g.row + 2),
        color: srcColor(g.inst.src), title: g.inst.src.title,
        has_time: !g.inst.allDay && !g.multi, time: fmtClock(g.inst.s, cf)
      })),
      more: Object.keys(over).map(Number).sort((a, b) => a - b)
        .filter((c) => over[c] > 0).map((c) => ({
          date: isoDate(w + c - 1), col: String(c), row: String(max + 2),
          label: L.more.split("{n}").join(String(over[c]))
        }))
    });
  }
  return { headers, weeks };
}

/** A timed event placed in a day column. */
interface Placed { inst: Inst; ts: Minutes; te: Minutes; n: number; col: number; cols: number; }

// sigil's assign-columns (columns/2 in Erlang)
function columns(d: DayNum, insts: readonly Inst[]): Placed[] {
  const items: Placed[] = insts.map((i, n) => {
    const ts = Math.max(i.s - d * DAY, 0), te = Math.min(i.e - d * DAY, DAY);
    return { inst: i, ts, te: te <= ts ? ts + 30 : te, n, col: 0, cols: 0 };
  });
  items.sort((a, b) => (a.ts - b.ts) || (a.n - b.n));
  const cols: [number, number][][] = [];
  items.forEach((it) => {
    let k = 0;
    for (; k < cols.length; k++) {
      if (!cols[k].some((o) => it.ts < o[1] && it.te > o[0])) { break; }
    }
    if (k === cols.length) { cols.push([]); }
    cols[k].push([it.ts, it.te]);
    it.col = k;
  });
  items.forEach((it) => { it.cols = cols.length; });
  return items;
}

function calTimegrid(cf: CalConf, rs: DayNum, re: DayNum, insts: readonly Inst[]): unknown {
  const L = cf.L, today = todayNum(), dur = cf.slotDur, sh = cf.slotH;
  const total = Math.round(DAY * sh / dur);
  const slots: unknown[] = [];
  for (let h = 0; h < 24; h++) {
    slots.push({ slot_height: String(Math.round(60 * sh / dur)),
                 label: cf.hour24 ? pad(h) + ":00" : h12(h) + " " + (h < 12 ? L.am : L.pm) });
  }
  const days: unknown[] = [];
  for (let d = rs; d < re; d++) {
    const dayI = inRange(insts, d * DAY, (d + 1) * DAY);
    days.push({
      date: isoDate(d), dow: L.weekdays_short[dow(d)], num: String(ymd(d)[2]),
      head_cls: cls("ah-calendar-timegrid-header-cell", [[d === today, "ah-calendar-timegrid-header-today"]]),
      col_cls: cls("ah-calendar-timegrid-day-col", [[d === today, "ah-calendar-timegrid-day-today"]]),
      col_height: String(total),
      allday: dayI.filter((i) => i.allDay).map((i) => ({ id: i.id, color: srcColor(i.src), title: i.src.title })),
      timed: columns(d, dayI.filter((i) => !i.allDay)).map((it) => ({
        id: it.inst.id, color: srcColor(it.inst.src), title: it.inst.src.title,
        top: String(Math.round(it.ts * sh / dur)), height: String(Math.round((it.te - it.ts) * sh / dur)),
        left: "calc(100% * " + it.col + " / " + it.cols + ")", width: "calc(100% / " + it.cols + ")",
        time: fmtClock(it.inst.s, cf) + " – " + fmtClock(it.inst.e, cf),
        resizable: cf.editable
      }))
    });
  }
  return { all_day: L.all_day_short, slots, days };
}

function calList(cf: CalConf, rs: DayNum, re: DayNum, insts: readonly Inst[]): unknown {
  const L = cf.L, groups: unknown[] = [];
  for (let d = rs; d < re; d++) {
    const evs = inRange(insts, d * DAY, (d + 1) * DAY).map((i, n): [Inst, number] => [i, n]);
    evs.sort((a, b) => (a[0].s - b[0].s) || (a[1] - b[1]));
    if (!evs.length) { continue; }
    groups.push({
      name: L.weekdays[dow(d)], date: fmtDate(d, L.list_date, L),
      events: evs.map((p) => {
        const i = p[0], src = i.src;
        return {
          id: i.id, color: srcColor(src), title: src.title,
          time: i.allDay ? L.all_day : fmtClock(i.s, cf) + " – " + fmtClock(i.e, cf),
          recurring: !!src.rrule, has_status: src.status !== undefined,
          status_color: (src.status !== undefined && STATUS_COLORS[src.status]) || "var(--ah-color-grey-300)"
        };
      })
    });
  }
  return { empty: !groups.length, no_events: L.no_events, no_events_hint: L.no_events_hint, groups };
}

/** The view of a state, as the server renders it. */
interface View { rs: DayNum; re: DayNum; title: string; insts: Inst[]; html: string; }

function calView(cf: CalConf, view: string, cur: DayNum, events: readonly CalendarEvent[]): View {
  const p = calProfile(cf, view, cur);
  const insts = instances(events, p[0] * DAY, p[1] * DAY);
  const html = view === "month" ? AH.tpl.calendar_month(calMonth(cf, cur, p[0], p[1], insts))
    : view === "list" ? AH.tpl.calendar_list(calList(cf, p[0], p[1], insts))
    : AH.tpl.calendar_timegrid(calTimegrid(cf, p[0], p[1], insts));
  return { rs: p[0], re: p[1], title: p[2], insts, html };
}

// ---- drags ----

/** A dragged copy of an event and the pointer's offset in it. */
interface Ghost { g: HTMLElement; dx: number; dy: number; }

interface MoveDrag {
  mode: "move"; kind: "month" | "allday" | "timed"; ev: HTMLElement; inst: Inst;
  x0: number; y0: number; started: boolean; origin: DayNum | null; g: Ghost | null;
}
interface ResizeDrag { mode: "resize"; ev: HTMLElement; inst: Inst; col: HTMLElement; end?: Minutes; }
interface SelectDrag { mode: "select"; from: DayNum; to: DayNum; }
interface CreateDrag {
  mode: "create"; col: HTMLElement; day: DayNum; m0: Minutes; top: Minutes; bot: Minutes; ph: HTMLElement;
}
type Drag = MoveDrag | ResizeDrag | SelectDrag | CreateDrag;

// Hit testing by rectangles (the event layer covers the day cells).
function cellAt(cells: NodeListOf<HTMLElement>, x: number, y: number | null): HTMLElement | null {
  for (const cell of Array.from(cells)) {
    const r = cell.getBoundingClientRect();
    if (x >= r.left && x <= r.right && (y === null || (y >= r.top && y <= r.bottom))) { return cell; }
  }
  return null;
}

// The day of a rendered cell (the templates write valid dates).
function cellDay(cell: Element | null): DayNum {
  return parseDate(cell ? cell.getAttribute("data-date") : null) ?? 0;
}

function ghost(evEl: HTMLElement, e: MouseEvent): Ghost {
  const r = evEl.getBoundingClientRect();
  const g = evEl.cloneNode(true) as HTMLElement;
  g.classList.add("ah-calendar-event-ghost");
  g.removeAttribute("tabindex");
  g.removeAttribute("role");
  Object.assign(g.style, { position: "fixed", zIndex: "9999", opacity: "0.7", pointerEvents: "none", margin: "0",
                           width: r.width + "px", height: r.height + "px", left: r.left + "px", top: r.top + "px" });
  document.body.appendChild(g);
  return { g, dx: e.clientX - r.left, dy: e.clientY - r.top };
}

// Details of an interaction as data-* on the root (Event.data of a
// postback), then the component event (detail: the details).
const DETAIL_ATTRS = ["data-event", "data-from", "data-to", "data-days", "data-all-day", "data-date"];

class CalendarController extends AH.Controller {
  static #seq = 0;

  // server-rendered parts (setup reads them)
  #container!: HTMLElement;
  #titles: HTMLElement[] = [];
  #views: string[] = [];
  #conf!: CalConf;
  #selectable = false;
  // state
  #events: CalendarEvent[] = [];
  #view = "month";
  #cur: DayNum = 0;
  #insts = new Map<string, Inst>();
  #range: [DayNum, DayNum] = [0, 0];
  #renderedView: string | null = null;
  #timer: number | undefined = undefined;
  #drag: Drag | null = null;
  #dragAc: AbortController | null = null;
  #justDragged = false;

  override setup(): void {
    const el = this.element;
    if (!el.id) { el.id = "ah-cal" + (++CalendarController.#seq); }
    const num = (a: string, dflt: number): number => {
      const n = parseInt(el.getAttribute(a) || "", 10);
      return n > 0 || (n === 0 && dflt === 0) ? n : dflt;
    };
    // the server always renders the view container
    this.#container = el.querySelector<HTMLElement>(":scope > .ah-calendar-view-container")!;
    this.#titles = Array.from(el.querySelectorAll<HTMLElement>(".ah-calendar-title"));
    const evs = readJson(el, "data-ah-events");
    this.#events = Array.isArray(evs) ? evs.map(calNormalize) : [];
    this.#view = el.getAttribute("data-ah-view") || "month";
    this.#views = Array.from(el.querySelectorAll(".ah-calendar-view-btn"), (b) => b.getAttribute("data-view") || "");
    this.#cur = parseDate(el.getAttribute("data-ah-value")) || todayNum();
    this.#conf = {
      first: Math.min(num("data-ah-first-day", 0), 6),
      agendaDays: num("data-ah-agenda-days", 30),
      maxEvents: num("data-ah-day-max-events", 3),
      slotDur: num("data-ah-slot-duration", 30),
      slotH: num("data-ah-slot-height", 20),
      hour24: el.getAttribute("data-ah-hour-format") === "24",
      L: readLabels(el, CAL_LABELS),
      editable: el.classList.contains("ah-calendar-editable")
    };
    this.#selectable = el.classList.contains("ah-calendar-selectable");
    this.#insts = new Map();
    this.#range = [0, 0];
    this.#drag = null;
    this.#justDragged = false;
    // the server rendered the same view; render again for the browser's today
    this.render();

    this.toolbar(".ah-calendar-btn-prev", () => { this.step(-1); });
    this.toolbar(".ah-calendar-btn-next", () => { this.step(1); });
    this.toolbar(".ah-calendar-btn-today", () => { this.go(todayNum(), this.#view); });
    this.toolbar(".ah-calendar-view-btn", (b) => { this.go(this.#cur, b.getAttribute("data-view") || ""); });
    this.delegate<MouseEvent, HTMLElement>("click", ".ah-calendar-event, .ah-calendar-list-event", (e, ev) => {
      e.stopPropagation();
      if (this.#justDragged) { this.#justDragged = false; return; }
      this.eventClick(ev);
    });
    this.delegate<MouseEvent, HTMLElement>("click", ".ah-calendar-day-more", (e, more) => {
      e.stopPropagation();
      this.more(more);
    });
    this.delegate<KeyboardEvent, HTMLElement>("keydown",
      ".ah-calendar-event, .ah-calendar-list-event, .ah-calendar-day-more", (e, t) => {
        if (e.key !== "Enter" && e.key !== " ") { return; }
        e.preventDefault();
        if (t.classList.contains("ah-calendar-day-more")) { this.more(t); } else { this.eventClick(t); }
      });
    this.listen(this.#container, "mousedown", (e) => {
      this.#justDragged = false;
      this.dragStart(e);
    });
  }

  override teardown(): void {
    if (this.#drag) { this.dragCancel(); }
    clearInterval(this.#timer);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  prev(): void { this.step(-1); }
  next(): void { this.step(1); }
  today(): void { this.go(todayNum(), this.#view); }
  changeView(v: string): void {
    if (VIEWS.indexOf(v) >= 0) { this.go(this.#cur, v); }
  }
  setValue(v: unknown): void {
    const d = parseDate(String(v || "").slice(0, 10));
    if (d !== null) { this.#cur = d; this.render(); }
  }
  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }
  setEvents(evs: unknown): void {
    this.#events = (Array.isArray(evs) ? evs : []).map(calNormalize);
    this.render();
  }
  addEvent(ev: unknown): void {
    const n = calNormalize(ev, this.#events.length + 1), i = this.find(n.id);
    if (i >= 0) { this.#events[i] = n; } else { this.#events.push(n); }
    this.render();
  }
  updateEvent(id: unknown, changes: unknown): void {
    const i = this.find(id);
    if (i < 0) { return; }
    // fields set to undefined are not copied (as before)
    const merged: Record<string, unknown> = { ...this.#events[i] };
    const ch = isRecord(changes) ? changes : null;
    if (ch) {
      Object.keys(ch).forEach((k) => {
        if (ch[k] !== undefined) { merged[k] = ch[k]; }
      });
      if ((ch.start || ch.end) && ch.allDay === undefined) { delete merged.allDay; }
    }
    this.#events[i] = calNormalize(merged, i + 1);
    this.render();
  }
  removeEvent(id: unknown): void {
    const i = this.find(id);
    if (i >= 0) { this.#events.splice(i, 1); this.render(); }
  }
  getEvents(): CalendarEvent[] {
    return this.#events.map((e) => ({ ...e }));
  }

  // ---- rendering and navigation ----

  private find(id: unknown): number {
    return this.#events.findIndex((e) => e.id === String(id));
  }

  private render(): void {
    const el = this.element, cf = this.#conf, cont = this.#container;
    let sc = cont.querySelector<HTMLElement>(".ah-calendar-timegrid-scroll");
    const scroll = sc && this.#renderedView === this.#view ? sc.scrollTop : null;
    const v = calView(cf, this.#view, this.#cur, this.#events);
    this.#insts = new Map();
    v.insts.forEach((i) => { this.#insts.set(i.id, i); });
    this.#range = [v.rs, v.re];
    cont.innerHTML = v.html;
    this.#titles.forEach((t) => { t.textContent = v.title; });
    el.querySelectorAll(".ah-calendar-view-btn").forEach((b) => {
      const on = b.getAttribute("data-view") === this.#view;
      b.classList.toggle("ah-calendar-view-btn-active", on);
      if (b.tagName === "A") {
        if (on) { b.setAttribute("aria-current", "true"); } else { b.removeAttribute("aria-current"); }
      } else {
        b.setAttribute("aria-pressed", String(on));
      }
    });
    this.links();
    const iso = isoDate(this.#cur);
    el.setAttribute("data-ah-value", iso);
    const hidden = el.querySelector<HTMLInputElement>(":scope > input[type=hidden]");
    if (hidden) { hidden.value = iso; }
    el.setAttribute("data-view", this.#view);
    el.setAttribute("data-start", isoDate(v.rs));
    el.setAttribute("data-end", isoDate(v.re));
    clearInterval(this.#timer);
    this.#timer = undefined;
    sc = cont.querySelector<HTMLElement>(".ah-calendar-timegrid-scroll");
    if (sc) {
      // keep the scroll position while the view stays, else show the morning
      sc.scrollTop = scroll !== null ? scroll : Math.max(Math.round(7 * 60 * cf.slotH / cf.slotDur) - 10, 0);
      this.nowLine();
      this.#timer = window.setInterval(() => { this.nowLine(); }, 60000);
    }
    this.#renderedView = this.#view;
  }

  // The current time line, when today is in the visible range.
  private nowLine(): void {
    const now = new Date(), t = todayNum(), cf = this.#conf, range = this.#range;
    this.#container.querySelectorAll<HTMLElement>(".ah-calendar-timegrid-now-indicator").forEach((ind) => {
      if (t >= range[0] && t < range[1]) {
        ind.style.display = "block";
        ind.style.top = Math.round((now.getHours() * 60 + now.getMinutes()) * cf.slotH / cf.slotDur) + "px";
      } else {
        ind.style.display = "none";
      }
    });
  }

  // Navigation re-renders and fires change when the date or view changed.
  private go(cur: DayNum, view: string): void {
    const changed = cur !== this.#cur || view !== this.#view;
    this.#cur = cur;
    this.#view = view;
    this.render();
    if (changed) { this.fire("change"); }
  }

  // The day prev (-1) or next (1) shows; the twin of step/4.
  private stepDay(dir: number): DayNum {
    switch (this.#view) {
      case "month": return addMonths(this.#cur, dir);
      case "week": return this.#cur + 7 * dir;
      case "day": return this.#cur + dir;
      default: return this.#cur + this.#conf.agendaDays * dir;
    }
  }

  private step(dir: number): void {
    this.go(this.stepDay(dir), this.#view);
  }

  // With the href option (data-ah-href: a template with {date} and {view})
  // the toolbar entries are links; point them at the states they lead to
  // from the shown date (the twin of nav_url/3).
  private links(): void {
    const el = this.element, tpl = el.getAttribute("data-ah-href");
    if (!tpl) { return; }
    const url = (d: DayNum, view: string): string =>
      tpl.replace(/\{date\}/g, isoDate(d)).replace(/\{view\}/g, view);
    const set = (sel: string, d: DayNum): void => {
      el.querySelectorAll("a" + sel).forEach((a) => { a.setAttribute("href", url(d, this.#view)); });
    };
    set(".ah-calendar-btn-prev", this.stepDay(-1));
    set(".ah-calendar-btn-next", this.stepDay(1));
    set(".ah-calendar-btn-today", todayNum());
    el.querySelectorAll("a.ah-calendar-view-btn").forEach((a) => {
      a.setAttribute("href", url(this.#cur, a.getAttribute("data-view") || ""));
    });
  }

  // A click on a toolbar entry: buttons always navigate here; a link only
  // on a plain left click (the browser opens modified clicks), whose
  // default is then prevented and whose URL is pushed after the navigation
  // (a reload or going back asks the server for that state).
  private toolbar(sel: string, go: (t: HTMLElement) => void): void {
    this.delegate<MouseEvent, HTMLElement>("click", sel, (e, t) => {
      let url = "";
      if (t.tagName === "A") {
        if (e.button !== 0 || e.ctrlKey || e.metaKey || e.shiftKey || e.altKey) { return; }
        e.preventDefault();
        url = t.getAttribute("href") || "";
      }
      go(t);
      if (url) { AH.apply([{ op: "url", mode: "push", value: url }]); }
    });
  }

  // ---- component events ----

  // The details as data-* on the root, then the event; false when a
  // listener called preventDefault.
  private report<D extends object>(name: string, detail: D): boolean {
    const el = this.element;
    DETAIL_ATTRS.forEach((a) => { el.removeAttribute(a); });
    Object.entries(detail).forEach(([k, v]) => {
      if (k === "raw") { return; }
      el.setAttribute("data-" + k.replace(/[A-Z]/g, (x) => "-" + x.toLowerCase()), String(v));
    });
    return this.fire<D>(name, detail);
  }

  private eventClick(target: HTMLElement): void {
    const inst = this.#insts.get(target.getAttribute("data-eventid") || "");
    if (!inst) { return; }
    this.report<CalendarEventClick>("ah:event-click", {
      event: inst.src.id,
      raw: { ...inst.src, start: isoTime(inst.s, inst.allDay), end: isoTime(inst.e, inst.allDay) }
    });
  }

  private more(target: HTMLElement): void {
    const date = target.getAttribute("data-date");
    const go = this.report<CalendarMoreClick>("ah:more-click", { date });
    const d = parseDate(date);
    if (go && this.#views.indexOf("day") >= 0 && d !== null) {
      this.go(d, "day");
    }
  }

  // Shift an event (the whole series for a recurring one) and re-render.
  private move(src: CalendarEvent, dStart: Minutes, dEnd: Minutes, name: string, days?: number): void {
    const s = mins(src.start) + dStart, e = mins(src.end) + dEnd;
    if (e <= s) { return; }
    const allDay = src.allDay && s % DAY === 0 && e % DAY === 0;
    src.start = isoTime(s, allDay);
    src.end = isoTime(e, allDay);
    src.allDay = allDay;
    this.render();
    const detail: CalendarEventDrop = { event: src.id, from: src.start, to: src.end, allDay };
    if (days !== undefined) { detail.days = days; }
    this.report<CalendarEventDrop>(name, detail);
    this.#justDragged = true;
  }

  private slotMinutes(col: Element, y: number): Minutes {
    const cf = this.#conf;
    const rel = y - col.getBoundingClientRect().top;
    const m = Math.min(Math.max(Math.round(rel / cf.slotH * cf.slotDur), 0), DAY);
    return Math.round(m / cf.slotDur) * cf.slotDur;
  }

  // ---- drags: one at a time; mousedown decides the mode, document
  // mousemove and mouseup (removed when the drag ends) carry it out ----

  private dragStart(e: MouseEvent): void {
    const cf = this.#conf, cont = this.#container;
    if (e.button !== 0 || !(e.target instanceof Element)) { return; }
    const t = e.target;
    const evEl = t.closest<HTMLElement>(
      ".ah-calendar-daygrid-event, .ah-calendar-timegrid-event, .ah-calendar-allday-event");
    let d: Drag | null = null;
    if (t.classList.contains("ah-calendar-timegrid-resize-handle") && cf.editable) {
      // the handle sits in a timed event in a day column
      const rEv = t.closest<HTMLElement>(".ah-calendar-timegrid-event")!;
      const inst = this.#insts.get(rEv.getAttribute("data-eventid") || "");
      if (!inst) { return; }
      d = { mode: "resize", ev: rEv, inst, col: t.closest<HTMLElement>(".ah-calendar-timegrid-day-col")! };
    } else if (evEl && cf.editable) {
      const kind = evEl.classList.contains("ah-calendar-daygrid-event") ? "month"
        : (evEl.classList.contains("ah-calendar-allday-event") ? "allday" : "timed");
      const inst = this.#insts.get(evEl.getAttribute("data-eventid") || "");
      if (!inst) { return; }
      let origin: DayNum | null = null;
      if (kind === "month") {
        const c0 = cellAt(cont.querySelectorAll<HTMLElement>(".ah-calendar-day"), e.clientX, e.clientY);
        origin = c0 ? parseDate(c0.getAttribute("data-date")) : Math.floor(inst.s / DAY);
      } else if (kind === "allday") {
        const ac = evEl.closest(".ah-calendar-timegrid-allday-cell");
        origin = parseDate(ac ? ac.getAttribute("data-date") : null);
      }
      d = { mode: "move", kind, ev: evEl, inst, x0: e.clientX, y0: e.clientY, started: false, origin, g: null };
    } else if (!evEl && this.#selectable && t.closest(".ah-calendar-daygrid-body") &&
               !t.closest(".ah-calendar-day-more")) {
      const cell = cellAt(cont.querySelectorAll<HTMLElement>(".ah-calendar-day"), e.clientX, e.clientY);
      if (cell) { const from = cellDay(cell); d = { mode: "select", from, to: from }; }
    } else if (!evEl && this.#selectable && t.closest(".ah-calendar-timegrid-day-col")) {
      const col = t.closest<HTMLElement>(".ah-calendar-timegrid-day-col")!;
      const m0 = Math.min(this.slotMinutes(col, e.clientY), DAY - cf.slotDur);
      const ph = document.createElement("div");
      ph.className = "ah-calendar-timegrid-create-placeholder";
      ph.style.left = "0px";
      ph.style.right = "0px";
      col.appendChild(ph);
      d = { mode: "create", col, day: cellDay(col), m0, top: m0, bot: m0 + cf.slotDur, ph };
    }
    if (!d) { return; }
    e.preventDefault();
    this.#drag = d;
    this.dragPaint(e);
    const ac = this.#dragAc = new AbortController(), o = { signal: ac.signal };
    document.addEventListener("mousemove", (me) => { this.dragPaint(me); }, o);
    document.addEventListener("mouseup", (ue) => { this.dragEnd(ue); }, o);
    document.addEventListener("keydown", (ke) => {
      if (ke.key === "Escape") { this.dragCancel(); }
    }, o);
  }

  private dragPaint(e: MouseEvent): void {
    const d = this.#drag, cf = this.#conf, cont = this.#container;
    if (!d) { return; }
    switch (d.mode) {
      case "move": {
        if (!d.started) {
          if (Math.abs(e.clientX - d.x0) + Math.abs(e.clientY - d.y0) < 4) { return; }
          d.started = true;
          d.g = ghost(d.ev, e);
        }
        const g = d.g;
        if (g) {
          g.g.style.left = e.clientX - g.dx + "px";
          g.g.style.top = e.clientY - g.dy + "px";
        }
        break;
      }
      case "resize": {
        const m = Math.max(this.slotMinutes(d.col, e.clientY), d.inst.s % DAY + cf.slotDur);
        d.end = m;
        d.ev.style.height = Math.round((m - d.inst.s % DAY) * cf.slotH / cf.slotDur) + "px";
        break;
      }
      case "select": {
        const days = cont.querySelectorAll<HTMLElement>(".ah-calendar-day");
        const cell = cellAt(days, e.clientX, e.clientY);
        if (cell) { d.to = cellDay(cell); }
        const a = Math.min(d.from, d.to), b = Math.max(d.from, d.to);
        days.forEach((x) => {
          const n = cellDay(x);
          x.classList.toggle("ah-calendar-day-selected", n >= a && n <= b);
        });
        break;
      }
      case "create": {
        const cur = this.slotMinutes(d.col, e.clientY);
        d.top = Math.min(d.m0, cur);
        d.bot = Math.min(Math.max(d.m0 + cf.slotDur, cur + cf.slotDur), DAY);
        d.ph.style.top = Math.round(d.top * cf.slotH / cf.slotDur) + "px";
        d.ph.style.height = Math.round((d.bot - d.top) * cf.slotH / cf.slotDur) + "px";
        break;
      }
      default: break;
    }
  }

  private dragCancel(): void {
    const d = this.#drag;
    if (this.#dragAc) { this.#dragAc.abort(); this.#dragAc = null; }
    this.#drag = null;
    if (!d) { return; }
    if (d.mode === "move" && d.g) { d.g.g.remove(); }
    if (d.mode === "create") { d.ph.remove(); }
    this.#container.querySelectorAll(".ah-calendar-day-selected").forEach((x) => {
      x.classList.remove("ah-calendar-day-selected");
    });
    if (d.mode === "resize") { this.render(); }
  }

  private dragEnd(e: MouseEvent): void {
    const d = this.#drag, cont = this.#container, cf = this.#conf;
    this.dragCancel();
    if (!d) { return; }
    switch (d.mode) {
      case "move": {
        if (!d.started || !d.g) { return; }                    // a click
        this.#justDragged = true;
        const src = d.inst.src;
        if (d.kind === "timed") {
          const col = cellAt(cont.querySelectorAll<HTMLElement>(".ah-calendar-timegrid-day-col"), e.clientX, null);
          if (!col) { return; }
          const day = cellDay(col);
          const top = e.clientY - d.g.dy;                     // the ghost's top edge
          const start = day * DAY + Math.min(this.slotMinutes(col, top), DAY - cf.slotDur);
          const delta = start - d.inst.s;
          if (delta) {
            this.move(src, delta, delta, "ah:event-drop", day - Math.floor(d.inst.s / DAY));
          }
        } else {
          const sel = d.kind === "month" ? ".ah-calendar-day" : ".ah-calendar-timegrid-allday-cell";
          const cell = cellAt(cont.querySelectorAll<HTMLElement>(sel), e.clientX,
                              d.kind === "month" ? e.clientY : null);
          if (!cell) { return; }
          const days = cellDay(cell) - (d.origin ?? 0);
          if (days) { this.move(src, days * DAY, days * DAY, "ah:event-drop", days); }
        }
        break;
      }
      case "resize": {
        this.#justDragged = true;
        const end = Math.floor(d.inst.s / DAY) * DAY + (d.end === undefined ? d.inst.e % DAY : d.end);
        if (d.end !== undefined && end !== d.inst.e) {
          this.move(d.inst.src, 0, end - d.inst.e, "ah:event-resize");
        }
        break;
      }
      case "select": {
        const a = Math.min(d.from, d.to), b = Math.max(d.from, d.to);
        this.report<CalendarSelect>("ah:select", { from: isoDate(a), to: isoDate(b + 1), allDay: true });
        break;
      }
      case "create":
        this.#justDragged = true;
        this.report<CalendarSelect>("ah:select", { from: isoTime(d.day * DAY + d.top, false),
                                                   to: isoTime(d.day * DAY + d.bot, false), allDay: false });
        break;
      default: break;
    }
  }
}

AH.register("calendar", CalendarController);
