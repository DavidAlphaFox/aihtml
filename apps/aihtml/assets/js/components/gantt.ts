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
 * local wall times without a zone, so there is no DST shifting.
 *
 * Events (detail, also written to the root as data-*): ah:row-expand
 * ({row, expanded}), ah:row-click ({row}), ah:task-click ({task}),
 * ah:task-change ({task, from, to, row, kind: "move" | "resize", days}). */
import AH from "../core.ts";
import { join } from "./_lib_values.ts";

/** Detail of ah:row-expand. */
export interface GanttRowExpand { row: string | null; expanded: boolean; }
/** Detail of ah:row-click. */
export interface GanttRowClick { row: string | null; }
/** Detail of ah:task-click. */
export interface GanttTaskClick { task: string | null; }
/** Detail of ah:task-change. */
export interface GanttTaskChange {
  task: string | null;
  from: string | null;
  to: string | null;
  row: string | null;
  kind: "move" | "resize";
  days: number;
}

/** A time: minutes since the epoch, and whether it was a bare date. */
interface Time { t: number; dateOnly: boolean; }
/** The start and end of a bar. */
interface Span { s: number; e: number; sd: boolean; ed: boolean; }

/** The settings the server renders on the root. */
interface Conf {
  cw: number;
  rh: number;
  origin: number;
  editable: boolean;
}

/** A bar being dragged (moved, or resized on one side). */
interface Drag {
  bar: HTMLElement;
  x: number;
  y: number;
  moved: boolean;
  left: number;
  width: number;
  mode: "move" | "resize";
  side?: "left" | "right";
  ox?: number;
  oy?: number;
  dx?: number;
  ghost?: HTMLElement;
}

const DAY = 1440;

function pad(n: number): string { return (n < 10 ? "0" : "") + n; }
// ISO date or date-time -> { t: minutes, dateOnly }
function parseT(s: unknown): Time | null {
  const m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2}))?/.exec(String(s || ""));
  if (!m) { return null; }
  const t = Math.round(Date.UTC(+(m[1] as string), +(m[2] as string) - 1, +(m[3] as string)) / 60000);
  return m[4] === undefined ? { t, dateOnly: true } : { t: t + (+m[4]) * 60 + (+(m[5] as string)), dateOnly: false };
}
function fmtT(t: number, dateOnly: boolean): string {
  const d = new Date(t * 60000);
  const s = d.getUTCFullYear() + "-" + pad(d.getUTCMonth() + 1) + "-" + pad(d.getUTCDate());
  return dateOnly && t % DAY === 0 ? s : s + "T" + pad(d.getUTCHours()) + ":" + pad(d.getUTCMinutes());
}
function num(x: number): number { return Math.round(x * 1000) / 1000; }

function conf(el: Element): Conf {
  return {
    cw: parseFloat(el.getAttribute("data-column-width") || "") || 60,
    rh: parseFloat(el.getAttribute("data-row-height") || "") || 40,
    origin: (parseT(el.getAttribute("data-origin")) || { t: 0 }).t,
    editable: el.hasAttribute("data-editable")
  };
}

function sidebarRows(el: Element): HTMLElement[] { return Array.from(el.querySelectorAll<HTMLElement>(".ah-gantt-sidebar-row")); }
function barOf(el: Element, id: unknown): HTMLElement | undefined {
  return Array.from(el.querySelectorAll<HTMLElement>(".ah-gantt-task-bar")).find((b) =>
    b.getAttribute("data-taskid") === String(id));
}
// the server writes data-start and data-end on every bar
function times(bar: Element): Span {
  const s = parseT(bar.getAttribute("data-start")) || { t: 0, dateOnly: true };
  const e = parseT(bar.getAttribute("data-end")) || { t: 0, dateOnly: true };
  return { s: s.t, e: e.t, sd: s.dateOnly, ed: e.dateOnly };
}
function pos(c: Conf, s: number, e: number): { left: number; width: number } {
  return { left: (s - c.origin) / DAY * c.cw, width: Math.max((e - s) / DAY * c.cw, c.cw) };
}

// Row visibility from the expanded state of the parents (tree order):
// hides rows, grid rows and bars, moves bars, shows summary bars of
// collapsed rows, then redraws the dependency lines.
function layout(el: Element): void {
  const c = conf(el);
  const open: Record<string, boolean> = {}, index: Record<string, number> = {};
  const parent: Record<string, string | null> = {}, collapsed: string[] = [];
  let n = 0;
  sidebarRows(el).forEach((row) => {
    const id = String(row.getAttribute("data-rowid")), p = row.getAttribute("data-parent");
    const vis = !p || open[p] === true;
    parent[id] = p;
    row.hidden = !vis;
    const expanded = row.getAttribute("aria-expanded");
    if (expanded === "false") { collapsed.push(id); }
    open[id] = vis && expanded !== "false";
    if (vis) { index[id] = n++; }
  });
  el.setAttribute("data-collapsed", join(collapsed));
  el.querySelectorAll<HTMLElement>(".ah-gantt-grid-row").forEach((g) => {
    g.hidden = !(String(g.getAttribute("data-rowid")) in index);
  });
  const bars = Array.from(el.querySelectorAll<HTMLElement>(".ah-gantt-task-bar"));
  bars.forEach((bar) => {
    const r = String(bar.getAttribute("data-rowid")), t = times(bar), p = pos(c, t.s, t.e);
    const at = index[r];
    bar.hidden = at === undefined;
    bar.style.left = num(p.left) + "px";
    bar.style.width = num(p.width) + "px";
    if (at !== undefined) { bar.style.top = (at * c.rh + 4) + "px"; }
  });
  const under = (r: string, anc: string): boolean => {
    for (let p = parent[r]; p; p = parent[p]) { if (p === anc) { return true; } }
    return false;
  };
  el.querySelectorAll<HTMLElement>(".ah-gantt-summary-bar").forEach((sum) => {
    const r = String(sum.getAttribute("data-rowid"));
    let s = Infinity, e = -Infinity, k = 0;
    bars.forEach((bar) => {
      if (under(String(bar.getAttribute("data-rowid")), r)) {
        const t = times(bar);
        s = Math.min(s, t.s); e = Math.max(e, t.e); k++;
      }
    });
    const at = index[r];
    const show = k > 0 && at !== undefined && collapsed.indexOf(r) >= 0;
    sum.hidden = !show;
    if (show && at !== undefined) {
      const p = pos(c, s, e);
      sum.style.left = num(p.left) + "px";
      sum.style.width = num(p.width) + "px";
      sum.style.top = (at * c.rh + 4) + "px";
    }
  });
  const h = n * c.rh;
  const layers = el.querySelectorAll<HTMLElement>(".ah-gantt-tasks-layer");
  layers.forEach((l) => { l.style.height = h + "px"; });
  const svg = el.querySelector<SVGSVGElement>(".ah-gantt-deps-layer");
  if (svg) {
    const w = parseFloat(layers[0] ? layers[0].style.width : "") || 0;
    svg.style.height = h + "px";
    svg.setAttribute("viewBox", "0 0 " + w + " " + h);
    drawDeps(el, c, index);
  }
}

// sigil's render-dependency-line (twin of dep_d/5 in the Erlang module)
function depD(x1: number, y1: number, x2: number, y2: number, same: boolean): string {
  if (same) { return "M" + num(x1) + "," + num(y1) + " L" + num(x2) + "," + num(y2); }
  if (x1 >= x2) {
    const ext = 30 + (x1 - x2) / 3;
    return "M" + num(x1) + "," + num(y1) + " C" + num(x1 + ext) + "," + num(y1) + " " +
      num(x2 - ext) + "," + num(y2) + " " + num(x2) + "," + num(y2);
  }
  const mx = (x1 + x2) / 2;
  return "M" + num(x1) + "," + num(y1) + " C" + num(mx) + "," + num(y1) + " " +
    num(mx) + "," + num(y2) + " " + num(x2) + "," + num(y2);
}

function drawDeps(el: Element, c: Conf, index: Record<string, number>): void {
  el.querySelectorAll(".ah-gantt-dep-line").forEach((line) => {
    const a = barOf(el, line.getAttribute("data-from")), b = barOf(el, line.getAttribute("data-to"));
    const ra = a ? String(a.getAttribute("data-rowid")) : "", rb = b ? String(b.getAttribute("data-rowid")) : "";
    const ia = a ? index[ra] : undefined, ib = b ? index[rb] : undefined;
    if (!a || !b || ia === undefined || ib === undefined) { line.setAttribute("d", ""); return; }
    const ta = times(a), tb = times(b), pa = pos(c, ta.s, ta.e), pb = pos(c, tb.s, tb.e);
    line.setAttribute("d", depD(pa.left + pa.width, ia * c.rh + c.rh / 2,
                                pb.left, ib * c.rh + c.rh / 2, ra === rb));
  });
}

function visibleRows(el: Element): HTMLElement[] { return sidebarRows(el).filter((r) => !r.hidden); }

function rowById(el: Element, id: unknown): HTMLElement | undefined {
  return sidebarRows(el).find((r) => r.getAttribute("data-rowid") === String(id));
}

function focusRow(el: Element, row: HTMLElement | undefined): void {
  if (!row) { return; }
  sidebarRows(el).forEach((r) => { r.setAttribute("tabindex", "-1"); });
  row.setAttribute("tabindex", "0");
  row.focus();
}

function scrollToTime(el: Element, t: number): void {
  const c = conf(el), body = el.querySelector(".ah-gantt-timeline-body");
  if (!body) { return; }
  body.scrollLeft = Math.max(0, (t - c.origin) / DAY * c.cw - 100);
  const head = el.querySelector(".ah-gantt-timeline-header");
  if (head) { head.scrollLeft = body.scrollLeft; }
}

// The server morphs updates into the kept root (gantt_update): the
// controller stays, its listeners are delegated from the root (scroll in
// the capture phase), and the scroll areas keep their position.
class GanttController extends AH.Controller {
  #drag: Drag | null = null;
  #dragAc: AbortController | null = null;
  #noClick = false;

  override setup(): void {
    const el = this.element;
    // scroll does not bubble: listen in the capture phase
    this.listen(el, "scroll", (e) => {
      const sc = e.target;
      if (!(sc instanceof Element)) { return; }
      if (sc.classList.contains("ah-gantt-timeline-body")) {
        const side = el.querySelector(".ah-gantt-sidebar-body"), head = el.querySelector(".ah-gantt-timeline-header");
        if (side) { side.scrollTop = sc.scrollTop; }
        if (head) { head.scrollLeft = sc.scrollLeft; }
      } else if (sc.classList.contains("ah-gantt-sidebar-body")) {
        const body = el.querySelector(".ah-gantt-timeline-body");
        if (body) { body.scrollTop = sc.scrollTop; }
      }
    }, { capture: true });
    this.delegate("click", ".ah-gantt-expand-icon", (e, icon) => {
      e.stopPropagation();
      const row = icon.closest<HTMLElement>(".ah-gantt-sidebar-row");
      if (row) { this.setExpanded(row, row.getAttribute("aria-expanded") === "false", true); }
    });
    this.delegate("click", ".ah-gantt-sidebar-row", (_e, row) => {
      focusRow(el, row);
      this.rowClick(row);
    });
    this.delegate("keydown", ".ah-gantt-sidebar-row", (e, row) => { this.rowKey(row, e); });
    this.delegate("click", ".ah-gantt-task-bar", (e, bar) => {
      e.stopPropagation();
      if (!this.#noClick) { this.taskClick(bar); }
    });
    this.delegate("keydown", ".ah-gantt-task-bar", (e, bar) => { this.taskKey(bar, e); });
    this.delegate("mousedown", ".ah-gantt-timeline-body", (e) => { this.dragStart(e); });
    // sigil scrolls to the first task
    let first = Infinity;
    el.querySelectorAll(".ah-gantt-task-bar").forEach((bar) => { first = Math.min(first, times(bar).s); });
    if (first < Infinity) { scrollToTime(el, first); }
  }

  override teardown(): void { this.dragStop(); }

  // methods (aihtml_action:call/4, AH.invoke)
  expandRow(id: string | number): void { this.setExpanded(rowById(this.element, id), true, false); }
  collapseRow(id: string | number): void { this.setExpanded(rowById(this.element, id), false, false); }
  scrollToDate(iso: string): void { const p = parseT(iso); if (p) { scrollToTime(this.element, p.t); } }
  setTask(id: string | number, from: string, to: string, row?: string | number | null): void {
    const el = this.element, bar = barOf(el, id), a = parseT(from), b = parseT(to);
    if (bar && a && b) {
      bar.setAttribute("data-start", fmtT(a.t, a.dateOnly));
      bar.setAttribute("data-end", fmtT(b.t, b.dateOnly));
      if (row) { bar.setAttribute("data-rowid", String(row)); }
      layout(el);
    }
  }

  // ---- rows ---------------------------------------------------------

  private setExpanded(row: HTMLElement | undefined, open: boolean, notify: boolean): void {
    const el = this.element;
    if (!row || row.getAttribute("aria-expanded") === null) { return; }
    if ((row.getAttribute("aria-expanded") === "true") === open) { return; }
    row.setAttribute("aria-expanded", open ? "true" : "false");
    row.querySelectorAll(".ah-gantt-expand-icon").forEach((i) => {
      i.classList.toggle("ah-gantt-expand-icon-expanded", open);
    });
    layout(el);
    if (notify) {
      el.setAttribute("data-row", String(row.getAttribute("data-rowid")));
      el.setAttribute("data-expanded", open ? "true" : "false");
      this.fire<GanttRowExpand>("ah:row-expand", { row: row.getAttribute("data-rowid"), expanded: open });
    }
  }

  private rowKey(row: HTMLElement, e: KeyboardEvent): void {
    const el = this.element, vis = visibleRows(el), i = vis.indexOf(row), open = row.getAttribute("aria-expanded");
    switch (e.key) {
      case "ArrowDown": focusRow(el, vis[i + 1]); break;
      case "ArrowUp": focusRow(el, vis[i - 1]); break;
      case "Home": focusRow(el, vis[0]); break;
      case "End": focusRow(el, vis[vis.length - 1]); break;
      case "ArrowRight":
        if (open === "false") { this.setExpanded(row, true, true); }
        else if (open === "true") { focusRow(el, vis[i + 1]); }
        break;
      case "ArrowLeft":
        if (open === "true") { this.setExpanded(row, false, true); }
        else { focusRow(el, rowById(el, row.getAttribute("data-parent"))); }
        break;
      case "Enter": case " ": this.rowClick(row); break;
      default: return;
    }
    e.preventDefault();
  }

  private rowClick(row: HTMLElement): void {
    this.element.setAttribute("data-row", String(row.getAttribute("data-rowid")));
    this.fire<GanttRowClick>("ah:row-click", { row: row.getAttribute("data-rowid") });
  }

  // ---- tasks --------------------------------------------------------

  private taskClick(bar: HTMLElement): void {
    this.element.setAttribute("data-task", String(bar.getAttribute("data-taskid")));
    this.fire<GanttTaskClick>("ah:task-click", { task: bar.getAttribute("data-taskid") });
  }

  // Apply a change to a bar and fire ah:task-change.
  private change(bar: HTMLElement, s: number, e: number, row: string | null,
                 kind: "move" | "resize", days: number, notify: boolean): void {
    const el = this.element, t = times(bar);
    bar.setAttribute("data-start", fmtT(s, t.sd));
    bar.setAttribute("data-end", fmtT(e, t.ed));
    if (row) { bar.setAttribute("data-rowid", row); }
    layout(el);
    if (!notify) { return; }
    const d: GanttTaskChange = {
      task: bar.getAttribute("data-taskid"), from: bar.getAttribute("data-start"),
      to: bar.getAttribute("data-end"), row: bar.getAttribute("data-rowid"), kind, days
    };
    (Object.keys(d) as (keyof GanttTaskChange)[]).forEach((k) => { el.setAttribute("data-" + k, String(d[k])); });
    this.fire<GanttTaskChange>("ah:task-change", d);
  }

  private taskKey(bar: HTMLElement, e: KeyboardEvent): void {
    const el = this.element, c = conf(el);
    if (e.key === "Enter" || e.key === " ") { e.preventDefault(); this.taskClick(bar); return; }
    if (!c.editable) { return; }
    const t = times(bar);
    let step = 0;
    if (e.key === "ArrowLeft") { step = -1; } else if (e.key === "ArrowRight") { step = 1; }
    if (step && e.shiftKey) {
      e.preventDefault();
      if (t.e + step * DAY > t.s) { this.change(bar, t.s, t.e + step * DAY, null, "resize", step, true); }
    } else if (step) {
      e.preventDefault();
      this.change(bar, t.s + step * DAY, t.e + step * DAY, null, "move", step, true);
    } else if (e.altKey && (e.key === "ArrowUp" || e.key === "ArrowDown")) {
      e.preventDefault();
      const vis = visibleRows(el), r = rowById(el, bar.getAttribute("data-rowid"));
      const i = r ? vis.indexOf(r) : -1;
      const target = i < 0 ? null : vis[i + (e.key === "ArrowUp" ? -1 : 1)];
      if (target) { this.change(bar, t.s, t.e, target.getAttribute("data-rowid"), "move", 0, true); }
    }
  }

  // One drag at a time: mousedown on a bar decides move or resize; the
  // document's mousemove/mouseup finish it (sigil's setup-task-drag!).
  private dragStart(e: MouseEvent): void {
    const el = this.element, c = conf(el), t = e.target;
    const bar = t instanceof Element ? t.closest<HTMLElement>(".ah-gantt-task-bar") : null;
    if (!bar || !(t instanceof Element) || e.button !== 0 || !c.editable) { return; }
    e.preventDefault();
    const rect = bar.getBoundingClientRect();
    const d: Drag = { bar, x: e.pageX, y: e.pageY, moved: false,
                      left: parseFloat(bar.style.left), width: rect.width, mode: "move" };
    if (t.classList.contains("ah-gantt-resize-handle")) {
      d.mode = "resize";
      d.side = t.classList.contains("ah-gantt-resize-left") ? "left" : "right";
    } else {
      d.ox = e.clientX - rect.left;
      d.oy = e.clientY - rect.top;
    }
    this.#drag = d;
    const ac = this.#dragAc = new AbortController();
    document.addEventListener("mousemove", (me) => { this.dragMove(c, me); }, { signal: ac.signal });
    document.addEventListener("mouseup", (ue) => { this.dragEnd(c, ue); }, { signal: ac.signal });
  }

  private dragMove(c: Conf, e: MouseEvent): void {
    const d = this.#drag;
    if (!d) { return; }
    const dx = e.pageX - d.x, dy = e.pageY - d.y;
    if (!d.moved && Math.abs(dx) + Math.abs(dy) < 4) { return; }
    d.moved = true;
    if (d.mode === "move") {
      if (!d.ghost) {
        const g = d.ghost = d.bar.cloneNode(true) as HTMLElement;
        g.removeAttribute("tabindex");
        g.removeAttribute("id");
        g.classList.add("ah-gantt-task-ghost");
        Object.assign(g.style, { position: "fixed", zIndex: "9999", opacity: "0.6", pointerEvents: "none", margin: "0",
                                 width: d.width + "px", height: d.bar.offsetHeight + "px" });
        document.body.appendChild(g);
      }
      d.ghost.style.left = (e.clientX - (d.ox || 0)) + "px";
      d.ghost.style.top = (e.clientY - (d.oy || 0)) + "px";
    } else if (d.side === "right") {
      d.bar.style.width = Math.max(c.cw, d.width + dx) + "px";
      d.dx = dx;
    } else {
      const dxl = Math.min(dx, d.width - c.cw);
      d.bar.style.left = (d.left + dxl) + "px";
      d.bar.style.width = (d.width - dxl) + "px";
      d.dx = dxl;
    }
  }

  private dragStop(): Drag | null {
    if (this.#dragAc) { this.#dragAc.abort(); this.#dragAc = null; }
    const d = this.#drag;
    this.#drag = null;
    if (d && d.ghost) { d.ghost.remove(); }
    return d;
  }

  private dragEnd(c: Conf, e: MouseEvent): void {
    const el = this.element, d = this.dragStop();
    if (!d || !d.moved) { return; }
    this.#noClick = true;
    setTimeout(() => { this.#noClick = false; }, 0);
    const t = times(d.bar);
    if (d.mode === "move") {
      const days = Math.round((e.pageX - d.x) / c.cw);
      const vis = visibleRows(el), r = rowById(el, d.bar.getAttribute("data-rowid"));
      const i = r ? vis.indexOf(r) : -1;
      const j = Math.max(0, Math.min(vis.length - 1, i + Math.round((e.pageY - d.y) / c.rh)));
      const vj = vis[j];
      const row = vj ? vj.getAttribute("data-rowid") : null;
      const rowChanged = !!row && row !== d.bar.getAttribute("data-rowid");
      if (days || rowChanged) {
        this.change(d.bar, t.s + days * DAY, t.e + days * DAY, rowChanged ? row : null, "move", days, true);
      }
    } else {
      let n = Math.round((d.dx || 0) / c.cw);
      if (d.side === "right" && t.e + n * DAY <= t.s) { n = Math.ceil((t.s - t.e) / DAY) + 1; }
      if (d.side === "left" && t.s + n * DAY >= t.e) { n = Math.floor((t.e - t.s) / DAY) - 1; }
      if (n) {
        this.change(d.bar, d.side === "left" ? t.s + n * DAY : t.s,
                    d.side === "right" ? t.e + n * DAY : t.e, null, "resize", n, true);
      } else {
        layout(el);
      }
    }
  }
}

AH.register("gantt", GanttController);
