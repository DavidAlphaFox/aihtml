/* Behaviour of the swimlane (designs/04-components.md). Ported from
 * sigil: data/swimlane (+ layout).
 *
 * The grid, the nodes and the flow lines (SVG paths and labels) are
 * rendered by aihtml_swimlane. The behaviour selects nodes
 * (highlighting their flows), syncs the scroll areas and, with `editable',
 * drags nodes to another cell: it restacks the cells and recomputes the
 * flow paths of the existing SVG elements (twins of flow_points/2 and
 * points_path/2 in the Erlang module), then fires ah:node-change.
 *
 * Events (detail, also written to the root as data-*): ah:select
 * ({node}, node null when cleared), ah:node-click ({node}),
 * ah:lane-click ({lane}), ah:node-change ({node, lane, phase, oldLane,
 * oldPhase}). */
import AH from "../core.ts";

/** Detail of ah:select (node null when cleared) and ah:node-click. */
export interface SwimlaneNodeEvent { node: string | null; }
/** Detail of ah:lane-click. */
export interface SwimlaneLaneEvent { lane: string | null; }
/** Detail of ah:node-change. */
export interface SwimlaneNodeChange {
  node: string;
  lane: string;
  phase: string;
  oldLane: string;
  oldPhase: string;
}

type Point = [number, number];
interface Box { x: number; y: number; w: number; h: number; }

/** The settings the server renders on the root. */
interface Conf {
  lh: number;
  pw: number;
  nw: number;
  nh: number;
  continuous: boolean;
  editable: boolean;
}

interface Drag {
  node: HTMLElement;
  x: number;
  y: number;
  x0: number;
  y0: number;
  moved: boolean;
}

const GAP = 10;

const DIRS: Record<string, Point> = {
  ArrowLeft: [-1, 0], ArrowRight: [1, 0], ArrowUp: [0, -1], ArrowDown: [0, 1]
};

function conf(el: Element): Conf {
  const a = (k: string, d: number): number => parseFloat(el.getAttribute("data-" + k) || "") || d;
  return {
    lh: a("lane-height", 110), pw: a("phase-width", 190),
    nw: a("node-width", 132), nh: a("node-height", 52),
    continuous: el.getAttribute("data-axis") === "continuous",
    editable: el.hasAttribute("data-editable")
  };
}

function ids(els: NodeListOf<Element>, attr: string): (string | null)[] {
  return Array.from(els, (n) => n.getAttribute(attr));
}
function lanes(el: Element): (string | null)[] { return ids(el.querySelectorAll(".ah-swimlane-lanes__row"), "data-lane-id"); }
function phases(el: Element): (string | null)[] { return ids(el.querySelectorAll(".ah-swimlane-grid__phase"), "data-phase-id"); }
function nodes(el: Element): HTMLElement[] { return Array.from(el.querySelectorAll<HTMLElement>(".ah-swimlane-node")); }
function nodeById(el: Element, id: unknown): HTMLElement | undefined {
  return nodes(el).find((n) => n.getAttribute("data-id") === String(id));
}
function kids(node: Element, sel: string): Element[] {
  return Array.from(node.children).filter((k) => k.matches(sel));
}
function box(n: HTMLElement): Box {
  return { x: parseFloat(n.style.left), y: parseFloat(n.style.top),
           w: parseFloat(n.style.width), h: parseFloat(n.style.height) };
}
function fmt(n: number): number { return Math.round(n * 100) / 100; }
function clamp(v: number, lo: number, hi: number): number { return Math.max(lo, Math.min(hi, v)); }

// Stack the nodes of each cell, centred in the lane (sigil's assign-boxes).
function layout(el: Element): void {
  const c = conf(el);
  const li: Record<string, number> = {}, pi: Record<string, number> = {};
  const count: Record<string, number> = {}, seen: Record<string, number> = {};
  lanes(el).forEach((id, i) => { li[String(id)] = i; });
  phases(el).forEach((id, i) => { pi[String(id)] = i; });
  const cell = (n: Element): string => n.getAttribute("data-lane") + "\u0000" + n.getAttribute("data-phase");
  nodes(el).forEach((node) => { count[cell(node)] = (count[cell(node)] || 0) + 1; });
  nodes(el).forEach((node) => {
    const l = li[String(node.getAttribute("data-lane"))];
    if (l === undefined) { return; }
    if (c.continuous) {
      node.style.top = fmt(l * c.lh + (c.lh - c.nh) / 2) + "px";
      return;
    }
    const p = pi[String(node.getAttribute("data-phase"))], k = cell(node), i = seen[k] || 0, n = count[k] || 0;
    seen[k] = i + 1;
    if (p === undefined) { return; }
    node.style.left = fmt(p * c.pw + (c.pw - c.nw) / 2) + "px";
    node.style.top = fmt(l * c.lh + (c.lh - (n * c.nh + (n - 1) * GAP)) / 2 + i * (c.nh + GAP)) + "px";
  });
  drawFlows(el);
}

// sigil's flow-points (twin of flow_points/2)
function flowPoints(s: Box, t: Box): Point[] {
  const sx = s.x + s.w, sy = s.y + s.h / 2, tx = t.x, ty = t.y + t.h / 2;
  const scx = s.x + s.w / 2, sty = s.y, sby = s.y + s.h;
  const tcx = t.x + t.w / 2, tty = t.y, tby = t.y + t.h;
  const overlap = Math.abs(scx - tcx) < Math.max(s.w, t.w);
  const below = tty >= sby, above = tby <= sty;
  const gap = below ? tty - sby : above ? sty - tby : 0;
  const tight = overlap && (below || above) && gap < 24;
  if (tx >= sx + 8) { const mx = (sx + tx) / 2; return [[sx, sy], [mx, sy], [mx, ty], [tx, ty]]; }
  if (tight) { const lx = Math.min(s.x, t.x) - 24; return [[s.x, sy], [lx, sy], [lx, ty], [t.x, ty]]; }
  if (overlap && below) { const my = (sby + tty) / 2; return [[scx, sby], [scx, my], [tcx, my], [tcx, tty]]; }
  if (overlap && above) { const my = (sty + tby) / 2; return [[scx, sty], [scx, my], [tcx, my], [tcx, tby]]; }
  const txr = t.x + t.w;
  const mx = (s.x + txr) / 2;
  return [[s.x, sy], [mx, sy], [mx, ty], [txr, ty]];
}

function len(a: Point, b: Point): number { return Math.hypot(b[0] - a[0], b[1] - a[1]); }
function lerp(a: Point, b: Point, t: number): Point { return [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t]; }

// sigil's points->path: rounded corners (twin of points_path/2)
function pointsPath(pts: Point[], radius: number): string {
  const p0 = pts[0] as Point;
  const out = ["M" + fmt(p0[0]) + "," + fmt(p0[1])];
  for (let i = 1; i < pts.length; i++) {
    const p = pts[i] as Point;
    if (i === pts.length - 1) { out.push("L" + fmt(p[0]) + "," + fmt(p[1])); continue; }
    const prev = pts[i - 1] as Point, next = pts[i + 1] as Point, l1 = len(prev, p), l2 = len(p, next);
    const r1 = Math.min(radius, l1 / 2), r2 = Math.min(radius, l2 / 2);
    const a = l1 > 0 ? lerp(prev, p, (l1 - r1) / l1) : p, b = l2 > 0 ? lerp(p, next, r2 / l2) : p;
    out.push("L" + fmt(a[0]) + "," + fmt(a[1]) + " Q" + fmt(p[0]) + "," + fmt(p[1]) +
             " " + fmt(b[0]) + "," + fmt(b[1]));
  }
  return out.join(" ");
}

function drawFlows(el: Element): void {
  el.querySelectorAll(".ah-swimlane-flows g").forEach((g) => {
    const a = nodeById(el, g.getAttribute("data-from")), b = nodeById(el, g.getAttribute("data-to"));
    if (!a || !b) { return; }
    const pts = flowPoints(box(a), box(b)), n = pts.length, i = Math.floor((n - 1) / 2);
    const pi = pts[i] as Point, pj = pts[Math.min(i + 1, n - 1)] as Point;
    const m: Point = [(pi[0] + pj[0]) / 2, (pi[1] + pj[1]) / 2];
    kids(g, ".ah-swimlane-flows__line").forEach((l) => { l.setAttribute("d", pointsPath(pts, 10)); });
    const bg = kids(g, ".ah-swimlane-flows__label-bg");
    const bg0 = bg[0];
    if (bg0) {
      const w = parseFloat(bg0.getAttribute("width") || "");
      bg.forEach((x) => {
        x.setAttribute("x", String(fmt(m[0] - w / 2)));
        x.setAttribute("y", String(fmt(m[1] - 9)));
      });
      kids(g, ".ah-swimlane-flows__label").forEach((x) => {
        x.setAttribute("x", String(fmt(m[0])));
        x.setAttribute("y", String(fmt(m[1])));
      });
    }
  });
}

function select(el: Element, id: unknown): void {
  const sid = id === null || id === undefined ? null : String(id);
  if (sid) { el.setAttribute("data-selected", sid); } else { el.removeAttribute("data-selected"); }
  nodes(el).forEach((node) => {
    const on = node.getAttribute("data-id") === sid;
    if (on) { node.setAttribute("data-state", "selected"); } else if (node.getAttribute("data-state") === "selected") { node.removeAttribute("data-state"); }
    node.setAttribute("aria-pressed", on ? "true" : "false");
  });
  el.querySelectorAll(".ah-swimlane-flows g").forEach((g) => {
    const active = !!sid && (g.getAttribute("data-from") === sid || g.getAttribute("data-to") === sid);
    if (active) { g.setAttribute("data-active", "true"); } else { g.removeAttribute("data-active"); }
    if (sid && !active) { g.setAttribute("data-dim", "true"); } else { g.removeAttribute("data-dim"); }
    const path = kids(g, ".ah-swimlane-flows__line")[0], m = path && path.getAttribute("marker-end");
    if (path && m) { path.setAttribute("marker-end", m.replace(/-arrow(-active)?\)$/, active ? "-arrow-active)" : "-arrow)")); }
  });
}

class SwimlaneController extends AH.Controller {
  #dragAc: AbortController | null = null;

  override setup(): void {
    const el = this.element;
    const body = el.querySelector(".ah-swimlane-grid__body");
    if (body) {
      this.listen(body, "scroll", () => {
        const head = el.querySelector(".ah-swimlane-grid__header"), lanesBody = el.querySelector(".ah-swimlane-lanes__body");
        if (head) { head.scrollLeft = body.scrollLeft; }
        if (lanesBody) { lanesBody.scrollTop = body.scrollTop; }
      });
    }
    this.delegate("pointerdown", ".ah-swimlane-node", (e, node) => { this.dragStart(node, e); });
    this.delegate("click", ".ah-swimlane-node", (e, node) => {
      // pointer clicks are handled on pointerup when editable; keyboard
      // and script clicks (detail 0) select here
      if (!conf(el).editable || e.detail === 0) { this.pick(node); }
    });
    this.delegate("keydown", ".ah-swimlane-node", (e, node) => { this.nodeKey(node, e); });
    this.delegate("click", ".ah-swimlane-lanes__row", (_e, row) => {
      el.setAttribute("data-lane", row.getAttribute("data-lane-id") || "");
      this.fire<SwimlaneLaneEvent>("ah:lane-click", { lane: row.getAttribute("data-lane-id") });
    });
  }

  override teardown(): void {
    if (this.#dragAc) { this.#dragAc.abort(); this.#dragAc = null; }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  select(id: string | number | null): void { select(this.element, id); }
  moveNode(id: string | number, lane: string | number, phase?: string | number | null): void {
    const n = nodeById(this.element, id);
    if (n) { this.move(n, String(lane), phase === undefined || phase === null ? null : String(phase), false); }
  }

  private pick(node: HTMLElement): void {
    const el = this.element, id = node.getAttribute("data-id");
    select(el, id);
    el.setAttribute("data-node", id || "");
    this.fire<SwimlaneNodeEvent>("ah:select", { node: id });
    this.fire<SwimlaneNodeEvent>("ah:node-click", { node: id });
  }

  // Move a node to another cell; fires ah:node-change when asked to.
  private move(node: HTMLElement, lane: string, phase: string | null | undefined, notify: boolean): void {
    const el = this.element;
    const old = { lane: node.getAttribute("data-lane"), phase: node.getAttribute("data-phase") };
    node.setAttribute("data-lane", lane);
    if (phase !== undefined && phase !== null) { node.setAttribute("data-phase", phase); }
    layout(el);
    if (notify && (old.lane !== lane || old.phase !== node.getAttribute("data-phase"))) {
      const d: SwimlaneNodeChange = {
        node: node.getAttribute("data-id") || "", lane, phase: node.getAttribute("data-phase") || "",
        oldLane: old.lane || "", oldPhase: old.phase || ""
      };
      el.setAttribute("data-node", d.node);
      el.setAttribute("data-lane", d.lane);
      el.setAttribute("data-phase", d.phase);
      el.setAttribute("data-old-lane", d.oldLane);
      el.setAttribute("data-old-phase", d.oldPhase);
      this.fire<SwimlaneNodeChange>("ah:node-change", d);
    }
  }

  private dragStart(node: HTMLElement, e: PointerEvent): void {
    const el = this.element, c = conf(el);
    if (!c.editable || e.button !== 0) { return; }
    e.preventDefault();
    const d: Drag = { node, x: e.clientX, y: e.clientY, x0: parseFloat(node.style.left),
                      y0: parseFloat(node.style.top), moved: false };
    if (this.#dragAc) { this.#dragAc.abort(); }
    const ac = this.#dragAc = new AbortController(), o = { signal: ac.signal };
    document.addEventListener("pointermove", (me) => {
      const dx = me.clientX - d.x, dy = me.clientY - d.y;
      if (!d.moved && Math.abs(dx) + Math.abs(dy) < 4) { return; }
      d.moved = true;
      node.setAttribute("data-state", "dragging");
      node.style.left = (d.x0 + dx) + "px";
      node.style.top = (d.y0 + dy) + "px";
    }, o);
    const up = (ue: PointerEvent): void => {
      ac.abort();
      this.#dragAc = null;
      if (!d.moved) { this.pick(node); node.focus(); return; }
      node.removeAttribute("data-state");
      if (node.getAttribute("aria-pressed") === "true") { node.setAttribute("data-state", "selected"); }
      const ls = lanes(el), ps = phases(el);
      const li = clamp(ls.indexOf(node.getAttribute("data-lane")) + Math.round((ue.clientY - d.y) / c.lh), 0, ls.length - 1);
      let phase: string | null | undefined = null;
      if (!c.continuous) {
        phase = ps[clamp(ps.indexOf(node.getAttribute("data-phase")) + Math.round((ue.clientX - d.x) / c.pw),
                         0, ps.length - 1)];
      }
      this.move(node, String(ls[li]), phase, true);
    };
    document.addEventListener("pointerup", up, o);
    document.addEventListener("pointercancel", up, o);
  }

  private nodeKey(node: HTMLElement, e: KeyboardEvent): void {
    const el = this.element, c = conf(el), k = e.key;
    if (k === "Enter" || k === " ") { e.preventDefault(); this.pick(node); return; }
    if (k === "Escape") {
      select(el, null);
      el.removeAttribute("data-node");
      this.fire<SwimlaneNodeEvent>("ah:select", { node: null });
      return;
    }
    const dir = DIRS[k];
    if (!dir) { return; }
    e.preventDefault();
    if (e.shiftKey && c.editable) {
      const ls = lanes(el), ps = phases(el);
      const li = clamp(ls.indexOf(node.getAttribute("data-lane")) + dir[1], 0, ls.length - 1);
      const phase = c.continuous ? null
        : ps[clamp(ps.indexOf(node.getAttribute("data-phase")) + dir[0], 0, ps.length - 1)];
      this.move(node, String(ls[li]), phase, true);
      node.focus();
      return;
    }
    // focus the nearest node in that direction
    const b = box(node);
    let best: HTMLElement | null = null, bestD = Infinity;
    nodes(el).forEach((other) => {
      if (other === node) { return; }
      const o = box(other), dx = (o.x + o.w / 2) - (b.x + b.w / 2), dy = (o.y + o.h / 2) - (b.y + b.h / 2);
      const along = dir[0] ? dx * dir[0] : dy * dir[1], across = dir[0] ? Math.abs(dy) : Math.abs(dx);
      if (along <= 0) { return; }
      const dist = along + across * 2;
      if (dist < bestD) { bestD = dist; best = other; }
    });
    if (best) { (best as HTMLElement).focus(); }
  }
}

AH.register("swimlane", SwimlaneController);
