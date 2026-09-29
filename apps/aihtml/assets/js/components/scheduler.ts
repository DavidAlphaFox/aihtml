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
 * arithmetic (local wall times without a zone; _lib_date).
 *
 * Events (detail, also written to the root as data-*): `change`;
 * ah:event-click / ah:event-edit / ah:event-delete / ah:event-copy
 * (SchedulerEventInfo: {event, source, from, to, resource, allDay});
 * ah:event-change (SchedulerEventChange: the same plus kind: "move" |
 * "resize"); ah:select (SchedulerSelect: {from, to, resource, allDay});
 * ah:more-click (SchedulerMoreClick: {date}). */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import {
  DAY, addMonths, firstOfMonth, isoDate, isoTime, lastOfMonth, pad, parseTime, sow, todayNum
} from "./_lib_date.ts";
import type { DayNum, Minutes } from "./_lib_date.ts";

/** Detail of ah:event-click, -edit, -delete and -copy: the appointment. */
export interface SchedulerEventInfo {
  event: string | null;
  source: string | null;
  from: string | null;
  to: string | null;
  resource: string;
  allDay: boolean;
}
/** Detail of ah:event-change. */
export interface SchedulerEventChange extends SchedulerEventInfo { kind: "move" | "resize"; }
/** Detail of ah:select: a time range in a column (resource "" when none). */
export interface SchedulerSelect { from: string; to: string; resource: string; allDay: boolean; }
/** Detail of ah:more-click. */
export interface SchedulerMoreClick { date: string | null; }

const EVENT_SEL = ".ah-scheduler-timegrid-event, .ah-scheduler-allday-event, .ah-scheduler-month-event, " +
  ".ah-scheduler-timeline-event, .ah-scheduler-agenda-event, .ah-scheduler-more-popover-item";
const COL_SEL = ".ah-scheduler-dayview-col, .ah-scheduler-dayview-res-col";
const MONTH_KEYS: Record<string, number> = { ArrowLeft: -1, ArrowRight: 1, ArrowUp: -7, ArrowDown: 7 };

// The day or the minute of an ISO date or date-time (the server writes
// them with aihtml_lib_date); null when it is not a valid one.
function parseT(s: unknown): Minutes | null { const r = parseTime(s); return r ? r.t : null; }
function parseDay(s: unknown): DayNum | null { const t = parseT(s); return t === null ? null : Math.floor(t / DAY); }
// The minute of a time attribute the server wrote (0 when missing).
function attrT(el: Element, name: string): Minutes { return parseT(el.getAttribute(name)) ?? 0; }
function num(x: number): number { return Math.round(x * 1000) / 1000; }

/** The settings on the root (read on every use: the server re-renders). */
interface Conf {
  view: string; date: DayNum; first: number; agenda: number;
  sd: number; sh: number; ds: number; de: number; hf: number;
  am: string; pm: string; editable: boolean;
}

function conf(el: Element): Conf {
  const a = (k: string, d: number): number => {
    const v = parseInt(el.getAttribute("data-" + k) || "", 10);
    return isNaN(v) ? d : v;
  };
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

function clock(t: Minutes, c: Conf): string {
  const h = Math.floor((t % DAY) / 60), m = pad(t % 60);
  if (c.hf === 24) { return pad(h) + ":" + m; }
  return ((h % 12) || 12) + ":" + m + " " + (h < 12 ? c.am : c.pm);
}

// The visible days [start, end) of a view (twin of profile/4).
function profile(view: string, d: DayNum, first: number, agenda: number): [DayNum, DayNum] {
  switch (view) {
    case "day": case "timeline_day": return [d, d + 1];
    case "week": case "timeline_week": { const s = sow(d, first); return [s, s + 7]; }
    case "month": return [sow(firstOfMonth(d), first), sow(lastOfMonth(d), first) + 7];
    case "timeline_month": return [firstOfMonth(d), lastOfMonth(d) + 1];
    default: return [d, d + agenda];
  }
}

function step(view: string, d: DayNum, dir: number, agenda: number): DayNum {
  switch (view) {
    case "day": case "timeline_day": return d + dir;
    case "week": case "timeline_week": return d + 7 * dir;
    case "month": case "timeline_month": return addMonths(d, dir);
    default: return d + agenda * dir;
  }
}

function live(el: Element, msg: string): void {
  el.querySelectorAll(":scope > .ah-scheduler-live").forEach((l) => { l.textContent = msg; });
}

// Point the toolbar links at the states they lead to from the shown date
// and view (the twin of nav_url/3).
function links(el: Element): void {
  const tpl = el.getAttribute("data-href");
  if (!tpl) { return; }
  const cf = conf(el);
  const url = (d: DayNum, view: string): string =>
    tpl.replace(/\{date\}/g, isoDate(d)).replace(/\{view\}/g, view);
  const set = (sel: string, d: DayNum): void => {
    el.querySelectorAll("a" + sel).forEach((a) => { a.setAttribute("href", url(d, cf.view)); });
  };
  set(".ah-scheduler-btn-prev", step(cf.view, cf.date, -1, cf.agenda));
  set(".ah-scheduler-btn-next", step(cf.view, cf.date, 1, cf.agenda));
  set(".ah-scheduler-btn-today", todayNum());
  el.querySelectorAll("a.ah-scheduler-view-btn").forEach((a) => {
    a.setAttribute("href", url(cf.date, a.getAttribute("data-view") || ""));
  });
}

function pushUrl(url: string): void {
  if (url) { AH.apply([{ op: "url", mode: "push", value: url }]); }
}

function info(ev: Element): SchedulerEventInfo {
  return {
    event: ev.getAttribute("data-eventid"), source: ev.getAttribute("data-source"),
    from: ev.getAttribute("data-start"), to: ev.getAttribute("data-end"),
    resource: ev.getAttribute("data-resourceid") || "",
    allDay: ev.hasAttribute("data-all-day")
  };
}

function children(node: Element, sel: string): HTMLElement[] {
  return Array.from(node.children).filter((k): k is HTMLElement => k instanceof HTMLElement && k.matches(sel));
}

// ---- time grid ------------------------------------------------------

function colWindow(col: Element, cf: Conf): [Minutes, Minutes] {
  const d = parseDay(col.getAttribute("data-date")) ?? 0;
  return [d * DAY + cf.ds * 60, d * DAY + cf.de * 60];
}

// sigil's assign-columns over the events of one column
function relayoutCol(col: Element, cf: Conf): void {
  const w = colWindow(col, cf);
  const items = children(col, ".ah-scheduler-timegrid-event").map((ev) => {
    const s = Math.max(attrT(ev, "data-start"), w[0]);
    const e = Math.max(Math.min(attrT(ev, "data-end"), w[1]), s + cf.sd);
    return { el: ev, s, e, col: 0 };
  }).sort((a, b) => a.s - b.s);
  const cols: (typeof items)[] = [];
  items.forEach((it) => {
    let i = 0;
    for (; i < cols.length; i++) {
      if (!cols[i].some((o) => it.s < o.e && it.e > o.s)) { break; }
    }
    if (i === cols.length) { cols.push([]); }
    cols[i].push(it);
    it.col = i;
  });
  items.forEach((it) => {
    const s = it.el.style;
    s.top = num((it.s - w[0]) / cf.sd * cf.sh) + "px";
    s.height = num((it.e - it.s) / cf.sd * cf.sh) + "px";
    s.left = num(it.col * 100 / cols.length) + "%";
    s.width = num(100 / cols.length) + "%";
  });
}

function findCol(el: Element, day: DayNum, res: string | null): HTMLElement | undefined {
  return Array.from(el.querySelectorAll<HTMLElement>(COL_SEL)).find((col) =>
    col.getAttribute("data-date") === isoDate(day) &&
    (!res || col.getAttribute("data-resourceid") === res));
}

function at(sel: string, scope: Element, x: number, y?: number): HTMLElement | null {
  for (const node of Array.from(scope.querySelectorAll<HTMLElement>(sel))) {
    const r = node.getBoundingClientRect();
    if (x >= r.left && x < r.right && (y === undefined || (y >= r.top && y < r.bottom))) { return node; }
  }
  return null;
}

/** The timeline's grid and the time range it shows. */
interface Timeline { grid: HTMLElement; ts: Minutes; te: Minutes; }

// Timeline events and resize handles only exist inside the grid.
function timeline(el: Element): Timeline {
  const grid = el.querySelector<HTMLElement>(".ah-scheduler-timeline-grid")!;
  return { grid, ts: attrT(grid, "data-from"), te: attrT(grid, "data-to") };
}

// Put an appointment at new times (and resource) in the current view.
function place(el: Element, ev: HTMLElement, s: Minutes, e: Minutes, res?: string | null): void {
  const cf = conf(el), allDay = ev.hasAttribute("data-all-day");
  ev.setAttribute("data-start", isoTime(s, allDay));
  ev.setAttribute("data-end", isoTime(e, allDay));
  if (res !== undefined && res !== null) { ev.setAttribute("data-resourceid", res); }
  const focused = document.activeElement === ev;
  const cl = ev.classList;
  const time = ev.querySelector(".ah-scheduler-event-time");
  if (time) {
    time.textContent = cl.contains("ah-scheduler-month-event") ? clock(s, cf) : clock(s, cf) + " – " + clock(e, cf);
  }
  if (cl.contains("ah-scheduler-timegrid-event")) {
    const old = ev.parentElement;
    const col = findCol(el, Math.floor(s / DAY), ev.getAttribute("data-resourceid")) || old;
    if (col && old && col !== old) { col.appendChild(ev); relayoutCol(old, cf); }
    if (col) { relayoutCol(col, cf); }
  } else if (cl.contains("ah-scheduler-month-event")) {
    const d0 = Math.floor(s / DAY), d1 = allDay ? Math.max(d0 + 1, Math.ceil(e / DAY)) : d0 + 1;
    const cell = el.querySelector('.ah-scheduler-day[data-date="' + isoDate(d0) + '"]');
    const row = cell && cell.closest(".ah-scheduler-week-row");
    const first = row && row.querySelector(".ah-scheduler-day");
    const content = row && children(row, ".ah-scheduler-week-content")[0];
    if (first && content) {
      const ws = parseDay(first.getAttribute("data-date")) ?? 0;
      const sc = d0 - ws + 1, ec = Math.min(8, d1 - ws + 1);
      if (ev.parentNode !== content) { content.appendChild(ev); }
      const used: Record<string, boolean> = {};
      children(content, ".ah-scheduler-month-event").forEach((o) => {
        if (o === ev) { return; }
        const g = /grid-column:\s*(\d+)\s*\/\s*(\d+).*grid-row:\s*(\d+)/.exec(o.getAttribute("style") || "");
        if (g && sc < +g[2] && ec > +g[1]) { used[g[3]] = true; }
      });
      let r = 2;
      while (used[r]) { r++; }
      ev.style.gridColumn = sc + "/" + ec;
      ev.style.gridRow = String(r);
    }
  } else if (cl.contains("ah-scheduler-timeline-event")) {
    const tl = timeline(el), tot = tl.te - tl.ts;
    const left = Math.max(0, s - tl.ts) * 100 / tot, right = Math.min(tot, e - tl.ts) * 100 / tot;
    ev.style.left = num(left) + "%";
    ev.style.width = num(Math.max(0.5, right - left)) + "%";
    const rid = ev.getAttribute("data-resourceid");
    const trow = Array.from(tl.grid.querySelectorAll(".ah-scheduler-timeline-row")).find((x) =>
      (x.getAttribute("data-resourceid") || "") === (rid || ""));
    const target = trow && children(trow, ".ah-scheduler-timeline-row-events")[0];
    if (target && ev.parentNode !== target) { target.appendChild(ev); }
  }
  if (focused) { ev.focus(); }
}

// ---- drag (sigil's drag.cljs): one at a time ---------------------------

/** A dragged copy of an appointment and the pointer's offset in it. */
interface Ghost { ox: number; oy: number; g: HTMLElement; }

function ghost(ev: HTMLElement, x: number, y: number): Ghost {
  const r = ev.getBoundingClientRect();
  const g = ev.cloneNode(true) as HTMLElement;
  g.removeAttribute("tabindex");
  g.classList.add("ah-scheduler-event-ghost");
  Object.assign(g.style, { position: "fixed", zIndex: "9999", opacity: "0.7", pointerEvents: "none", margin: "0",
                           width: r.width + "px", height: r.height + "px", left: r.left + "px", top: r.top + "px" });
  return { ox: x - r.left, oy: y - r.top, g };
}

function snap(min: number, unit: number): number { return Math.round(min / unit) * unit; }

function gridMinute(col: Element, cf: Conf, y: number): Minutes {
  const w = colWindow(col, cf), r = col.getBoundingClientRect();
  return Math.max(w[0], Math.min(w[1], w[0] + snap((y - r.top) / cf.sh * cf.sd, cf.sd)));
}

function tlTime(el: Element, x: number, unit: number): Minutes {
  const tl = timeline(el), r = tl.grid.getBoundingClientRect();
  const t = tl.ts + (x - r.left) / r.width * (tl.te - tl.ts);
  return tl.ts + snap(Math.max(0, Math.min(tl.te - tl.ts, t - tl.ts)), unit);
}

function tlUnit(cf: Conf, ev: Element | null): number {
  if (cf.view === "timeline_day") { return cf.sd; }
  return ev && ev.hasAttribute("data-all-day") ? DAY : 60;
}

/** A press on an appointment: its times when it started. */
interface EvDragBase { x: number; y: number; moved: boolean; ev: HTMLElement; s: Minutes; e: Minutes; }
interface MoveDrag extends EvDragBase { mode: "move"; g: Ghost | null; }
interface ResizeDrag extends EvDragBase { mode: "resize"; newEnd?: Minutes; }
interface TlResizeDrag extends EvDragBase { mode: "tl-resize"; side: "left" | "right"; ns?: Minutes; ne?: Minutes; }
interface CreateDrag {
  mode: "create"; x: number; y: number; moved: boolean; col: HTMLElement;
  from?: Minutes; to?: Minutes; ph: HTMLElement | null;
}
type Drag = MoveDrag | ResizeDrag | TlResizeDrag | CreateDrag;

// The server morphs a new range into the kept root (scheduler_update):
// the controller stays, its listeners are delegated from the root, and
// the scroll areas keep their position; only the now line is redrawn.
class SchedulerController extends AH.Controller {
  #drag: Drag | null = null;
  #dragAc: AbortController | null = null;
  #menu: HTMLElement | null = null;
  #menuTarget: HTMLElement | SchedulerSelect | null = null;
  #pop: HTMLElement | null = null;
  #popAnchor: HTMLElement | null = null;
  #popFloat: FloatHandle | null = null;
  #noClick = false;
  #timer: number | undefined = undefined;
  #observer: MutationObserver | null = null;

  override setup(): void {
    const el = this.element;
    this.#drag = this.#menu = this.#pop = null;
    this.#noClick = false;
    this.toolbar(".ah-scheduler-btn-prev", () => { this.go("prev"); });
    this.toolbar(".ah-scheduler-btn-next", () => { this.go("next"); });
    this.toolbar(".ah-scheduler-btn-today", () => { this.go("today"); });
    this.toolbar(".ah-scheduler-view-btn", (b) => { this.show(conf(el).date, b.getAttribute("data-view") || ""); });
    this.delegate<MouseEvent, HTMLElement>("click", EVENT_SEL, (e, ev) => {
      e.stopPropagation();
      if (!this.#noClick) { this.report<SchedulerEventInfo>("ah:event-click", info(ev)); }
    });
    this.delegate<MouseEvent, HTMLElement>("dblclick", EVENT_SEL, (e, ev) => {
      e.stopPropagation();
      if (conf(el).editable) { this.closePopups(); this.report<SchedulerEventInfo>("ah:event-edit", info(ev)); }
    });
    this.delegate<KeyboardEvent, HTMLElement>("keydown", EVENT_SEL, (e, ev) => { this.eventKey(ev, e); });
    this.delegate<MouseEvent, HTMLElement>("dblclick", COL_SEL, (e, col) => {
      if (!conf(el).editable || (e.target instanceof Element && e.target.closest(EVENT_SEL))) { return; }
      const cf = conf(el), s = gridMinute(col, cf, e.clientY);
      this.report<SchedulerSelect>("ah:select", { from: isoTime(s), to: isoTime(s + 60),
                                                  resource: col.getAttribute("data-resourceid") || "",
                                                  allDay: false });
    });
    this.delegate<MouseEvent, HTMLElement>("contextmenu", ".ah-scheduler-view-container", (e) => {
      const cf = conf(el);
      if (!cf.editable || !(e.target instanceof Element)) { return; }
      const ev = e.target.closest<HTMLElement>(EVENT_SEL);
      const col = e.target.closest<HTMLElement>(COL_SEL);
      if (!ev && !col) { return; }
      e.preventDefault();
      if (ev) {
        this.openMenu("event", e.clientX, e.clientY, ev);
      } else if (col) {
        const s = gridMinute(col, cf, e.clientY);
        this.openMenu("cell", e.clientX, e.clientY,
                      { from: isoTime(s), to: isoTime(s + 60),
                        resource: col.getAttribute("data-resourceid") || "", allDay: false });
      }
    });
    this.delegate<MouseEvent, HTMLElement>("click", ".ah-scheduler-contextmenu-item", (e, item) => {
      e.stopPropagation();
      this.menuAction(item);
    });
    this.delegate<KeyboardEvent, HTMLElement>("keydown", ".ah-scheduler-contextmenu", (e, menu) => {
      const items = Array.from(menu.querySelectorAll<HTMLElement>(".ah-scheduler-contextmenu-item"));
      const i = items.findIndex((x) => x === document.activeElement), n = items.length;
      if (e.key === "ArrowDown") { if (n) { items[(i + 1) % n].focus(); } }
      else if (e.key === "ArrowUp") { if (n) { items[(i - 1 + n) % n].focus(); } }
      else if (e.key === "Enter" || e.key === " ") { if (i >= 0) { this.menuAction(items[i]); } }
      else if (e.key === "Escape" || e.key === "Tab") {
        const t = this.#menuTarget;
        this.closePopups();
        if (t instanceof HTMLElement) { t.focus(); }
      } else { return; }
      e.preventDefault();
      e.stopPropagation();
    });
    this.delegate<MouseEvent, HTMLElement>("click", ".ah-scheduler-day-more", (e, more) => {
      e.stopPropagation();
      this.openMore(more);
    });
    this.delegate<KeyboardEvent, HTMLElement>("keydown", ".ah-scheduler-day-more", (e, more) => {
      if (e.key === "Enter" || e.key === " ") { e.preventDefault(); this.openMore(more); }
    });
    this.delegate<MouseEvent, HTMLElement>("click", ".ah-scheduler-more-popover-close", (e) => {
      e.stopPropagation();
      this.closePopups();
    });
    this.delegate<KeyboardEvent, HTMLElement>("keydown", ".ah-scheduler-more-popover", (e) => {
      if (e.key === "Escape") { e.preventDefault(); e.stopPropagation(); this.closePopups(); }
      if (e.key === "Enter" && e.target instanceof Element &&
          e.target.classList.contains("ah-scheduler-more-popover-close")) { this.closePopups(); }
    });
    this.listen(document, "mousedown", (e) => {
      const t = e.target instanceof Node ? e.target : null;
      if ((this.#menu && !this.#menu.contains(t)) ||
          (this.#pop && !this.#pop.contains(t))) { this.closePopups(); }
    });
    this.delegate<MouseEvent, HTMLElement>("mousedown", ".ah-scheduler-view-container", (e) => {
      if (e.target instanceof Element && e.target.closest(".ah-scheduler-contextmenu, .ah-scheduler-more-popover")) {
        return;
      }
      this.dragStart(e);
    });
    // scroll does not bubble: listen in the capture phase
    this.listen(el, "scroll", (e) => {
      const sc = e.target;
      if (!(sc instanceof Element) || !sc.classList.contains("ah-scheduler-timeline-scroll")) { return; }
      const head = el.querySelector<HTMLElement>(".ah-scheduler-timeline-slots-header");
      const panel = el.querySelector<HTMLElement>(".ah-scheduler-timeline-resource-panel");
      if (head) { head.style.transform = "translateX(" + (-sc.scrollLeft) + "px)"; }
      if (panel) { panel.scrollTop = sc.scrollTop; }
    }, { capture: true });
    links(el);               // today in the browser's own date
    this.firstScroll();
    this.nowLine();
    // a morph from the server replaces the view: redraw the now line
    this.#observer = new MutationObserver(() => { this.nowLine(); });
    const cont = el.querySelector(".ah-scheduler-view-container");
    this.#observer.observe(cont || el, { childList: true, subtree: true });
    this.#timer = window.setInterval(() => { this.nowLine(); }, 60000);
  }

  override teardown(): void {
    this.closePopups();
    clearInterval(this.#timer);
    this.dragStop();
    if (this.#observer) { this.#observer.disconnect(); }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  navigate(dir: unknown): void { this.go(dir); }
  setView(view: unknown): void { this.show(conf(this.element).date, String(view)); }
  gotoDate(iso: unknown): void {
    const d = parseDay(iso);
    if (d !== null) { this.show(d, conf(this.element).view); }
  }
  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }

  // ---- navigation (the server renders the new range) -----------------

  private show(d: DayNum, view: string): void {
    const el = this.element, cf = conf(el), r = profile(view, d, cf.first, cf.agenda);
    el.setAttribute("data-ah-value", isoDate(d));
    el.setAttribute("data-view", view);
    el.setAttribute("data-start", isoDate(r[0]));
    el.setAttribute("data-end", isoDate(r[1]));
    const hidden = el.querySelector<HTMLInputElement>(":scope > input[type=hidden]");
    if (hidden) { hidden.value = isoDate(d); }
    el.querySelectorAll(".ah-scheduler-view-btn").forEach((b) => {
      const on = b.getAttribute("data-view") === view;
      b.classList.toggle("ah-scheduler-view-btn-active", on);
      if (b.tagName === "A") {
        if (on) { b.setAttribute("aria-current", "true"); } else { b.removeAttribute("aria-current"); }
      } else {
        b.setAttribute("aria-pressed", on ? "true" : "false");
      }
    });
    links(el);
    this.closePopups();
    this.fire("change");
  }

  private go(dir: unknown): void {
    const cf = conf(this.element);
    this.show(dir === "today" ? todayNum() : step(cf.view, cf.date, dir === "prev" ? -1 : 1, cf.agenda), cf.view);
  }

  // Whether a click on a toolbar entry is handled here. Buttons always are;
  // a link only on a plain left click of a bound scheduler (a change
  // action renders the new state), else the browser follows it. A handled
  // link's default is prevented and its URL returned, to push after the
  // navigation (which rewrites the links); null when not handled.
  private claim(e: MouseEvent, t: HTMLElement): string | null {
    if (t.tagName !== "A") { return ""; }
    const bound = /(^|\s)change:/.test(this.element.getAttribute("data-ah-on") || "");
    if (!bound || e.button !== 0 || e.ctrlKey || e.metaKey || e.shiftKey || e.altKey) { return null; }
    e.preventDefault();
    return t.getAttribute("href");
  }

  private toolbar(sel: string, go: (t: HTMLElement) => void): void {
    this.delegate<MouseEvent, HTMLElement>("click", sel, (e, t) => {
      const url = this.claim(e, t);
      if (url === null) { return; }
      go(t);
      pushUrl(url);
    });
  }

  // ---- component events ------------------------------------------------

  // The details as data-* on the root (Event.data of a postback), then
  // the event.
  private report<D extends object>(name: string, data: D): void {
    const el = this.element;
    Object.entries(data).forEach(([k, v]) => {
      el.setAttribute("data-" + k.replace(/[A-Z]/g, (x) => "-" + x.toLowerCase()), String(v));
    });
    this.fire<D>(name, data);
  }

  private changed(ev: HTMLElement, kind: "move" | "resize"): void {
    const d: SchedulerEventChange = { ...info(ev), kind };
    const title = ev.querySelector(".ah-scheduler-event-title, .ah-scheduler-timeline-event-title");
    live(this.element, (title ? title.textContent : "") + ": " + d.from + " – " + d.to);
    this.report<SchedulerEventChange>("ah:event-change", d);
  }

  // ---- drag --------------------------------------------------------------

  private dragStart(e: MouseEvent): void {
    const el = this.element, cf = conf(el);
    if (e.button !== 0 || !cf.editable || !(e.target instanceof Element)) { return; }
    const t = e.target;
    const ev = t.closest<HTMLElement>(".ah-scheduler-timegrid-event, .ah-scheduler-month-event, .ah-scheduler-timeline-event");
    let d: Drag;
    if (ev) {
      const base = { x: e.clientX, y: e.clientY, moved: false, ev,
                     s: attrT(ev, "data-start"), e: attrT(ev, "data-end") };
      if (t.classList.contains("ah-scheduler-timegrid-resize-handle")) {
        d = { ...base, mode: "resize" };
      } else if (t.classList.contains("ah-scheduler-timeline-resize-handle")) {
        d = { ...base, mode: "tl-resize",
              side: t.classList.contains("ah-scheduler-timeline-resize-left") ? "left" : "right" };
      } else {
        d = { ...base, mode: "move", g: null };
      }
    } else {
      const col = t.closest<HTMLElement>(COL_SEL);
      if (!col) { return; }
      d = { mode: "create", x: e.clientX, y: e.clientY, moved: false, col, ph: null };
    }
    e.preventDefault();
    this.#drag = d;
    const ac = this.#dragAc = new AbortController();
    document.addEventListener("mousemove", (me) => { this.dragMove(cf, me); }, { signal: ac.signal });
    document.addEventListener("mouseup", (ue) => { this.dragEnd(cf, ue); }, { signal: ac.signal });
  }

  private dragMove(cf: Conf, e: MouseEvent): void {
    const el = this.element, d = this.#drag;
    if (!d) { return; }
    if (!d.moved && Math.abs(e.clientX - d.x) + Math.abs(e.clientY - d.y) < 4) { return; }
    if (!d.moved && d.mode === "move") { d.g = ghost(d.ev, d.x, d.y); document.body.appendChild(d.g.g); }
    d.moved = true;
    if (d.mode === "move") {
      if (d.g) {
        d.g.g.style.left = (e.clientX - d.g.ox) + "px";
        d.g.g.style.top = (e.clientY - d.g.oy) + "px";
      }
    } else if (d.mode === "resize") {
      const col = d.ev.parentElement;
      if (!col) { return; }
      const w = colWindow(col, cf);
      const end = Math.max(Math.max(d.s, w[0]) + cf.sd, gridMinute(col, cf, e.clientY));
      d.ev.style.height = num((end - Math.max(d.s, w[0])) / cf.sd * cf.sh) + "px";
      d.newEnd = end;
    } else if (d.mode === "tl-resize") {
      const u = tlUnit(cf, d.ev);
      const t = tlTime(el, e.clientX, u);
      if (d.side === "left") { d.ns = Math.min(t, d.e - u); d.ne = d.e; }
      else { d.ns = d.s; d.ne = Math.max(t, d.s + u); }
      const tl = timeline(el);
      d.ev.style.left = num(Math.max(0, d.ns - tl.ts) * 100 / (tl.te - tl.ts)) + "%";
      d.ev.style.width = num(Math.max(0.5, (Math.min(tl.te, d.ne) - Math.max(tl.ts, d.ns)) * 100 / (tl.te - tl.ts))) + "%";
    } else {
      const a = gridMinute(d.col, cf, d.y), b = gridMinute(d.col, cf, e.clientY);
      const from = d.from = Math.min(a, b);
      const to = d.to = Math.max(a, b) + cf.sd;
      const w0 = colWindow(d.col, cf)[0];
      if (!d.ph) {
        d.ph = document.createElement("div");
        d.ph.className = "ah-scheduler-create-placeholder";
        d.col.appendChild(d.ph);
      }
      Object.assign(d.ph.style, { left: "0px", right: "0px", top: num((from - w0) / cf.sd * cf.sh) + "px",
                                  height: num((to - from) / cf.sd * cf.sh) + "px" });
    }
  }

  private dragStop(): Drag | null {
    if (this.#dragAc) { this.#dragAc.abort(); this.#dragAc = null; }
    const d = this.#drag;
    this.#drag = null;
    if (d && d.mode === "move" && d.g) { d.g.g.remove(); }
    if (d && d.mode === "create" && d.ph) { d.ph.remove(); }
    return d;
  }

  private dragEnd(cf: Conf, e: MouseEvent): void {
    const el = this.element, d = this.dragStop();
    if (!d || !d.moved) { return; }
    this.#noClick = true;
    setTimeout(() => { this.#noClick = false; }, 0);
    if (d.mode === "move") {
      const dur = d.e - d.s, cl = d.ev.classList, g = d.g;
      if (!g) { return; }
      if (cl.contains("ah-scheduler-timegrid-event")) {
        const col = at(COL_SEL, el, e.clientX);
        if (!col) { return; }
        const start = gridMinute(col, cf, e.clientY - g.oy);
        place(el, d.ev, start, start + dur, col.getAttribute("data-resourceid"));
      } else if (cl.contains("ah-scheduler-month-event")) {
        const day = at(".ah-scheduler-day", el, e.clientX, e.clientY);
        if (!day) { return; }
        const delta = (parseDay(day.getAttribute("data-date")) ?? 0) - Math.floor(d.s / DAY);
        if (!delta) { return; }
        place(el, d.ev, d.s + delta * DAY, d.e + delta * DAY);
      } else {
        const s = tlTime(el, e.clientX - g.ox, tlUnit(cf, d.ev));
        const row = at(".ah-scheduler-timeline-row", el, e.clientX, e.clientY);
        const res = row ? row.getAttribute("data-resourceid") : null;
        place(el, d.ev, s, s + dur, res);
      }
      this.changed(d.ev, "move");
    } else if (d.mode === "resize") {
      if (d.newEnd === undefined || d.newEnd === d.e) {
        if (d.ev.parentElement) { relayoutCol(d.ev.parentElement, cf); }
        return;
      }
      place(el, d.ev, d.s, d.newEnd);
      this.changed(d.ev, "resize");
    } else if (d.mode === "tl-resize") {
      if (d.ns === undefined || d.ne === undefined) { return; }
      place(el, d.ev, d.ns, d.ne);
      this.changed(d.ev, "resize");
    } else if (d.from !== undefined && d.to !== undefined) {
      this.report<SchedulerSelect>("ah:select", { from: isoTime(d.from), to: isoTime(d.to),
                                                  resource: d.col.getAttribute("data-resourceid") || "",
                                                  allDay: false });
    }
  }

  // ---- keyboard moves ------------------------------------------------

  private eventKey(ev: HTMLElement, e: KeyboardEvent): void {
    const el = this.element, cf = conf(el), cl = ev.classList;
    if (e.key === "Enter" || e.key === " ") {
      e.preventDefault();
      this.report<SchedulerEventInfo>("ah:event-click", info(ev));
      return;
    }
    if ((e.key === "F10" && e.shiftKey) || e.key === "ContextMenu") {
      if (cf.editable) {
        e.preventDefault();
        const r = ev.getBoundingClientRect();
        this.openMenu("event", r.left + 4, r.bottom, ev);
      }
      return;
    }
    if (!cf.editable || cl.contains("ah-scheduler-more-popover-item")) { return; }
    if (e.key === "Delete") {
      e.preventDefault();
      this.report<SchedulerEventInfo>("ah:event-delete", info(ev));
      return;
    }
    const s = attrT(ev, "data-start"), en = attrT(ev, "data-end");
    const k = e.key;
    let ds = 0, de = 0, res: string | null = null;
    if (cl.contains("ah-scheduler-timegrid-event")) {
      if (k === "ArrowUp" || k === "ArrowDown") {
        const n = (k === "ArrowUp" ? -1 : 1) * cf.sd;
        if (e.shiftKey) { de = en + n > s ? n : 0; } else { ds = de = n; }
      } else if (k === "ArrowLeft" || k === "ArrowRight") { ds = de = (k === "ArrowLeft" ? -1 : 1) * DAY; }
    } else if (cl.contains("ah-scheduler-month-event")) {
      const m = MONTH_KEYS[k];
      if (m) { ds = de = m * DAY; }
    } else if (cl.contains("ah-scheduler-timeline-event")) {
      const u = tlUnit(cf, ev);
      if (k === "ArrowLeft" || k === "ArrowRight") {
        const dir = k === "ArrowLeft" ? -1 : 1;
        if (e.shiftKey) { de = en + dir * u > s ? dir * u : 0; } else { ds = de = dir * u; }
      } else if (e.altKey && (k === "ArrowUp" || k === "ArrowDown")) {
        const rows = Array.from(el.querySelectorAll(".ah-scheduler-timeline-row"));
        const own = ev.closest(".ah-scheduler-timeline-row");
        const i = own ? rows.indexOf(own) : -1;
        const target = i < 0 ? null : rows[i + (k === "ArrowUp" ? -1 : 1)];
        if (target) { res = target.getAttribute("data-resourceid"); }
      }
    }
    if (!ds && !de && !res) { return; }
    e.preventDefault();
    place(el, ev, s + ds, en + de, res);
    this.changed(ev, ds === de ? "move" : "resize");
  }

  // ---- context menu and +more popover (cloned from templates) ----------

  private closePopups(): void {
    if (this.#menu) { this.#menu.remove(); this.#menu = null; }
    if (this.#pop) {
      if (this.#popFloat) { this.#popFloat.stop(); this.#popFloat = null; }
      this.#pop.remove();
      this.#pop = null;
      const a = this.#popAnchor;
      if (a && document.contains(a)) { a.focus(); }
    }
  }

  private openMenu(kind: "event" | "cell", x: number, y: number, target: HTMLElement | SchedulerSelect): void {
    const el = this.element;
    this.closePopups();
    const tpl = el.querySelector<HTMLTemplateElement>(":scope > .ah-scheduler-menu-" + kind);
    if (!tpl) { return; }
    const m = document.createElement("div");
    m.className = "ah-scheduler-contextmenu";
    m.setAttribute("role", "menu");
    m.appendChild(tpl.content.cloneNode(true));
    Object.assign(m.style, { position: "fixed", left: x + "px", top: y + "px", zIndex: "10000" });
    el.appendChild(m);
    this.#menu = m;
    this.#menuTarget = target;
    const first = m.querySelector<HTMLElement>(".ah-scheduler-contextmenu-item");
    if (first) { first.focus(); }
  }

  private menuAction(item: HTMLElement): void {
    const action = item.getAttribute("data-action"), t = this.#menuTarget;
    this.closePopups();
    if (action === "create") {
      if (t && !(t instanceof HTMLElement)) { this.report<SchedulerSelect>("ah:select", t); }
    } else if (t instanceof HTMLElement) {
      this.report<SchedulerEventInfo>("ah:event-" + action, info(t));
      if (document.contains(t)) { t.focus(); }
    }
  }

  private openMore(more: HTMLElement): void {
    const el = this.element;
    this.closePopups();
    const tpl = children(more, "template")[0];
    if (!(tpl instanceof HTMLTemplateElement)) { return; }
    const p = document.createElement("div");
    p.className = "ah-scheduler-more-popover";
    p.setAttribute("role", "dialog");
    p.setAttribute("aria-label", more.getAttribute("data-date") || "");
    p.appendChild(tpl.content.cloneNode(true));
    el.appendChild(p);
    this.#pop = p;
    this.#popAnchor = more;
    this.#popFloat = AH.float(p, more, { placement: "bottom", align: "start", offset: 2 });
    const first = p.querySelector<HTMLElement>(EVENT_SEL);
    if (first) { first.focus(); }
    this.report<SchedulerMoreClick>("ah:more-click", { date: more.getAttribute("data-date") });
  }

  // ---- now indicator and first scroll ----------------------------------

  private nowLine(): void {
    const el = this.element;
    const cf = conf(el), lines = el.querySelectorAll<HTMLElement>(".ah-scheduler-dayview-now-indicator");
    if (!lines.length) { return; }
    const t = new Date(), today = isoDate(todayNum()), min = t.getHours() * 60 + t.getMinutes();
    const hasToday = Array.from(el.querySelectorAll(COL_SEL)).some((col) => col.getAttribute("data-date") === today);
    lines.forEach((line) => {
      if (hasToday && min >= cf.ds * 60 && min < cf.de * 60) {
        line.style.display = "block";
        line.style.top = num((min - cf.ds * 60) / cf.sd * cf.sh) + "px";
      } else {
        line.style.display = "none";
      }
    });
  }

  private firstScroll(): void {
    const el = this.element;
    const cf = conf(el), sc = el.querySelector(".ah-scheduler-dayview-hscroll");
    if (!sc) { return; }
    const t = new Date(), today = isoDate(todayNum());
    let target: number;
    if (el.querySelector(".ah-scheduler-dayview-col[data-date=\"" + today + "\"], " +
                        ".ah-scheduler-dayview-res-col[data-date=\"" + today + "\"]")) {
      target = t.getHours() * 60 + t.getMinutes() - 60;
    } else {
      target = Infinity;
      el.querySelectorAll(".ah-scheduler-timegrid-event").forEach((ev) => {
        target = Math.min(target, attrT(ev, "data-start") % DAY - 30);
      });
      if (target === Infinity) { target = 8 * 60; }
    }
    sc.scrollTop = Math.max(0, (target - cf.ds * 60) / cf.sd * cf.sh);
  }
}

AH.register("scheduler", SchedulerController);
