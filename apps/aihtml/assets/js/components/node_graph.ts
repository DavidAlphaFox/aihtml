/* node_graph: the node-graph behaviour (sigil data/node_graph), on markup
 * rendered by aihtml_node_graph:node_graph/3.
 *
 * The root's data-ah-value holds the graph as JSON ({nodes, links,
 * groups}); the server rendered the cards, links and groups with the
 * shared templates node_graph_{node,link,group}. This file keeps the graph
 * in memory, redraws with the same templates (AH.tpl.node_graph_*) and
 * after every edit writes data-ah-value, sets data-op / data-changed /
 * data-removed on the root and fires change. Selection changes fire
 * ah:selection-change (data-selection), a link dropped on empty canvas
 * without a node library fires ah:link-drop (data-origin, data-point).
 *
 * Events (native CustomEvents on the root, bubbling):
 *   change               detail: NodeGraphChange ({op, changed, removed,
 *                        graph})
 *   ah:selection-change  detail: the selected node ids (array)
 *   ah:link-drop         detail: NodeGraphLinkDrop ({origin: {node, kind,
 *                        index, detach}, point: [x, y]})
 *
 * Coordinates: graph units are the nodes' world coordinates; the camera
 * {x, y, z} maps them to the viewport: screen = (graph + [x, y]) * z. A
 * card's slots sit 30 (header) + 4 + 20 * i + 10 below its top, as in
 * node_graph.css and the Erlang module.
 */
import AH from "../core.ts";
import { join } from "./_lib_values.ts";
import "virtual:ah-tpl/node_graph_group";
import "virtual:ah-tpl/node_graph_link";
import "virtual:ah-tpl/node_graph_menu";
import "virtual:ah-tpl/node_graph_minimap";
import "virtual:ah-tpl/node_graph_node";
import "virtual:ah-tpl/node_graph_search";

// ------------------------------------------------------------------
// Types
// ------------------------------------------------------------------

export type Point = [number, number];
export type SlotKind = "input" | "output";

/** A slot of a node (other fields are kept as they are). */
export interface GraphSlot {
  name?: unknown;
  label?: unknown;
  type?: unknown;
  optional?: unknown;
  shape?: unknown;
  [key: string]: unknown;
}

/** Widget and body HTML of a card (trusted: templates, the server). */
export interface NodeHtml { widgets: string[]; body: string | null; }

export interface GraphNode {
  id: string;
  pos: Point;
  inputs: GraphSlot[];
  outputs: GraphSlot[];
  title?: unknown;
  type?: unknown;
  width?: number;
  height?: number;
  collapsed?: boolean;
  color?: unknown;
  /** HTML for a new card (setGraph, a library entry): taken out on load */
  html?: { widgets?: string[]; body?: string | null };
  [key: string]: unknown;
}

export interface GraphLink {
  id: string;
  /** [node id, output index] */
  source: [string, number];
  /** [node id, input index] */
  target: [string, number];
  points?: Point[];
  [key: string]: unknown;
}

export interface GraphGroup {
  id: string;
  title?: unknown;
  /** [x, y, width, height] */
  bounds: [number, number, number, number];
  color?: unknown;
  [key: string]: unknown;
}

export interface Graph { nodes: GraphNode[]; links: GraphLink[]; groups: GraphGroup[]; }

/** Where a link drag started (detach: the id of the link picked up). */
export interface LinkOrigin { node: string; kind: SlotKind; index: number; detach: string | null; }

/** Detail of change. */
export interface NodeGraphChange {
  op: string;
  changed: { nodes: GraphNode[]; links: GraphLink[]; groups: GraphGroup[] };
  removed: { nodes: string[]; links: string[]; groups: string[] };
  graph: Graph;
}
/** Detail of ah:link-drop. */
export interface NodeGraphLinkDrop { origin: LinkOrigin; point: Point; }
/** Detail of ah:selection-change: the selected node ids. */
export type NodeGraphSelection = string[];

/** An entry of the node library (the root's JSON data island). */
interface LibraryItem {
  label?: unknown;
  type?: unknown;
  category?: unknown;
  node?: unknown;
  kind?: unknown;
}

type Rec = Record<string, unknown>;
type SlotRef = [string, number];              // [node id, slot index]
type Candidate = [string, SlotKind, number];  // [node id, kind, index]
interface Camera { x: number; y: number; z: number; }
interface Box { x: number; y: number; w: number; h: number; }
interface Shown extends Box { id: string; collapsed: boolean; }
interface Conn { in: Record<number, boolean>; out: Record<number, boolean>; }

interface Options {
  mode: string;
  snap: number;
  readOnly: boolean;
  allowCycles: boolean;
}

/** The node drag of this frame: moved nodes (and group), or a resize. */
interface NodeDrag {
  ids?: Record<string, boolean>;
  group?: string;
  dx?: number;
  dy?: number;
  resize?: { id: string; w: number; h: number };
  groupResize?: { id: string; bounds: [number, number, number, number] };
}

interface LinkDrag {
  origin: LinkOrigin;
  type: unknown;
  compatible: Record<string, boolean>;
  point: Point;
  candidate: Candidate | null;
}

interface WaypointDrag { link: string; index: number; point: Point; }

interface Clip { graph: { nodes: GraphNode[]; links: GraphLink[] }; html: Record<string, NodeHtml>; }

interface Overlay { el: HTMLElement; cleanup(): void; }

interface MenuItem { label: string; hint?: string; danger?: boolean; run(): void; }

/** A pointer position during a drag (client coordinates, and the offset
 *  from the start). */
interface DragAt { x: number; y: number; dx: number; dy: number; }
interface DragSpec {
  threshold?: number;
  start?(s: DragAt): void;
  move?(s: DragAt): void;
  end?(s: DragAt): void;
  cancel?(): void;
  click?(s: DragAt): void;
}

type Handler = (e: Event, match: Element) => void;

const TITLE_H = 30, SLOT_H = 20, PAD_TOP = 4, PAD_BOTTOM = 8;
const DEFAULT_W = 240, MIN_W = 225, MIN_H = 60;
const GRID_DISPLAY = 50, MIN_Z = 0.1, MAX_Z = 4;
const SNAP_RADIUS = 22, MINI_W = 176, MINI_H = 120, MINI_PAD = 8;
const FALLBACK = "var(--ah-datatype-default, #aaa)";
const TWO_SLICES = ["M0 50 A 50 50 0 0 1 100 50", "M100 50 A 50 50 0 0 1 0 50"];
const THREE_SLICES = ["M0 50A50 50 0 0 0 75 93L50 50", "M75 93A50 50 0 0 0 75 7L50 50",
                      "M75 7A50 50 0 0 0 0 50L50 50"];
const SVG_NS = "http://www.w3.org/2000/svg";

function isMap(x: unknown): x is Rec { return !!x && typeof x === "object" && !Array.isArray(x); }
function list(x: unknown): unknown[] { return Array.isArray(x) ? x : []; }
function index(v: unknown): number { return typeof v === "number" ? v : Number(v); }

// ------------------------------------------------------------------
// Model (sigil node_graph/model): pure functions on {nodes, links,
// groups}; nodes are in z order, the last on top.
// ------------------------------------------------------------------

function normType(t: unknown): string {
  const s = t === undefined || t === null ? "" : String(t).trim();
  return s === "" ? "*" : s.toUpperCase();
}

function typeSet(t: unknown): string[] {
  const s = normType(t);
  if (s === "*") { return ["*"]; }
  return s.split(",").map((x) => x.trim()).filter(Boolean);
}

function typeCompatible(a: unknown, b: unknown): boolean {
  const sa = typeSet(a), sb = typeSet(b);
  if (sa.indexOf("*") >= 0 || sb.indexOf("*") >= 0) { return true; }
  return sa.some((x) => sb.indexOf(x) >= 0);
}

function findNode(g: Graph, id: string | null): GraphNode | null {
  return g.nodes.find((n) => n.id === id) || null;
}

function findLink(g: Graph, id: string | null): GraphLink | null {
  return g.links.find((l) => l.id === id) || null;
}

function findGroup(g: Graph, id: string | null): GraphGroup | null {
  return g.groups.find((gr) => gr.id === id) || null;
}

function slotOf(g: Graph, id: string, kind: SlotKind, i: number): GraphSlot | null {
  const n = findNode(g, id);
  if (!n) { return null; }
  return (kind === "input" ? n.inputs : n.outputs)[i] || null;
}

function inputLink(g: Graph, id: string, i: number): GraphLink | null {
  return g.links.find((l) => l.target[0] === id && l.target[1] === i) || null;
}

function slotKey(id: string, kind: SlotKind, i: number): string { return id + ":" + (kind === "input" ? "i" : "o") + i; }

// Can we walk the links from `from' to `to' (cycle check)?
function downstream(g: Graph, from: string, to: string): boolean {
  const todo = [from], seen: Record<string, boolean> = {};
  while (todo.length) {
    const cur = todo.shift() as string;
    if (cur === to) { return true; }
    if (seen[cur]) { continue; }
    seen[cur] = true;
    g.links.forEach((l) => { if (l.source[0] === cur) { todo.push(l.target[0]); } });
  }
  return false;
}

// null when output `from' [id, i] may connect to input `to', else why not
function connectProblem(g: Graph, from: SlotRef, to: SlotRef, allowCycles: boolean): string | null {
  const out = slotOf(g, from[0], "output", from[1]);
  const inp = slotOf(g, to[0], "input", to[1]);
  if (!out) { return "no-such-output"; }
  if (!inp) { return "no-such-input"; }
  if (from[0] === to[0]) { return "self-connection"; }
  if (!typeCompatible(out.type, inp.type)) { return "type-mismatch"; }
  if (!allowCycles && downstream(g, to[0], from[0])) { return "cycle"; }
  return null;
}

function freshId(used: Record<string, boolean>, prefix: string): string {
  for (let i = 1; ; i++) {
    if (!used[prefix + i]) { return prefix + i; }
  }
}

function ids(xs: readonly { id: string }[]): Record<string, boolean> {
  const o: Record<string, boolean> = {};
  xs.forEach((x) => { o[x.id] = true; });
  return o;
}

// Connect (mutating g); an input's previous link is replaced (ComfyUI).
function connect(g: Graph, from: SlotRef, to: SlotRef, allowCycles: boolean): string | null {
  if (connectProblem(g, from, to, allowCycles)) { return null; }
  const id = freshId(ids(g.links), "l");
  const old = inputLink(g, to[0], to[1]);
  if (old) { g.links = g.links.filter((l) => l !== old); }
  g.links.push({ id, source: [from[0], from[1]], target: [to[0], to[1]] });
  return id;
}

function removeNodes(g: Graph, xs: string[]): void {
  const dead: Record<string, boolean> = {};
  xs.forEach((id) => { dead[id] = true; });
  g.nodes = g.nodes.filter((n) => !dead[n.id]);
  g.links = g.links.filter((l) => !dead[l.source[0]] && !dead[l.target[0]]);
}

function extract(g: Graph, xs: string[]): { nodes: GraphNode[]; links: GraphLink[] } {
  const s: Record<string, boolean> = {};
  xs.forEach((id) => { s[id] = true; });
  return {
    nodes: g.nodes.filter((n) => s[n.id]),
    links: g.links.filter((l) => s[l.source[0]] && s[l.target[0]])
  };
}

// Insert a sub-graph with fresh ids, offset by [dx, dy]; returns the id map.
function insert(g: Graph, sub: { nodes: GraphNode[]; links: GraphLink[] }, dx: number, dy: number): Record<string, string> {
  const map: Record<string, string> = {};
  sub.nodes.forEach((n) => {
    const c = clone(n);
    c.id = freshId(ids(g.nodes), "n");
    c.pos = [Math.round(n.pos[0] + dx), Math.round(n.pos[1] + dy)];
    map[n.id] = c.id;
    g.nodes.push(c);
  });
  sub.links.forEach((l) => {
    const s = map[l.source[0]], t = map[l.target[0]];
    if (s && t) { connect(g, [s, l.source[1]], [t, l.target[1]], true); }
  });
  return map;
}

// A deep copy of JSON data (the graph and its parts).
function clone<T>(x: T): T { return JSON.parse(JSON.stringify(x)) as T; }

// The graph from JSON: ids as strings, missing parts filled in.
function normalize(raw: unknown): Graph {
  const g = isMap(raw) ? raw : {};
  return {
    nodes: list(g["nodes"]).map((n, i) => {
      const c: Rec = Object.assign({}, isMap(n) ? n : {});
      c["id"] = String(c["id"] === undefined ? "n" + i : c["id"]);
      c["pos"] = c["pos"] || [0, 0];
      c["inputs"] = c["inputs"] || [];
      c["outputs"] = c["outputs"] || [];
      return c as GraphNode;
    }),
    links: list(g["links"]).map((l, i) => {
      const c: Rec = Object.assign({}, isMap(l) ? l : {});
      const s = list(c["source"]), t = list(c["target"]);
      c["id"] = String(c["id"] === undefined ? "l" + i : c["id"]);
      c["source"] = [String(s[0]), index(s[1])];
      c["target"] = [String(t[0]), index(t[1])];
      return c as GraphLink;
    }),
    groups: list(g["groups"]).map((gr, i) => {
      const c: Rec = Object.assign({}, isMap(gr) ? gr : {});
      c["id"] = String(c["id"] === undefined ? "g" + (i + 1) : c["id"]);
      return c as GraphGroup;
    })
  };
}

// ------------------------------------------------------------------
// Colours (sigil node_graph/colors): data type colours are user data
// and deliberately do not follow the palette.
// ------------------------------------------------------------------

function typeColor(t: unknown): string {
  const n = normType(t);
  if (n === "*") { return FALLBACK; }
  return "var(--ah-datatype-" + n.replace(/[^A-Z0-9_]/g, "_") + ", " + FALLBACK + ")";
}

function slotColors(t: unknown): string[] {
  const n = normType(t);
  if (n === "*") { return [FALLBACK]; }
  const parts = n.split(",").map((x) => x.trim()).filter(Boolean).slice(0, 3);
  return parts.length ? parts.map(typeColor) : [FALLBACK];
}

function linkColor(t: unknown): string { return slotColors(t)[0] as string; }

function safeColor(c: unknown): string | null {
  return c && /^[#a-zA-Z0-9(),.% -]+$/.test(String(c)) ? String(c) : null;
}

// ------------------------------------------------------------------
// Geometry (sigil node_graph/geometry)
// ------------------------------------------------------------------

function r2(n: number): number { return Math.round(n * 100) / 100; }
function num(n: number): string { return String(r2(n)); }
function pt(p: Point): string { return num(p[0]) + "," + num(p[1]); }
function clamp(v: number, lo: number, hi: number): number { return Math.min(hi, Math.max(lo, v)); }

function nodeWidth(n: GraphNode): number { return n.width ? Math.max(MIN_W, n.width) : DEFAULT_W; }

function nodeHeight(n: GraphNode, measured?: number): number {
  if (n.collapsed) { return TITLE_H; }
  if (n.height) { return Math.max(TITLE_H, n.height); }
  if (measured) { return measured; }
  return TITLE_H + PAD_TOP + SLOT_H * Math.max(n.inputs.length, n.outputs.length) + PAD_BOTTOM;
}

function slotPos(s: Shown, kind: SlotKind, i: number): Point {
  const x = kind === "input" ? s.x : s.x + s.w;
  return s.collapsed ? [x, s.y + TITLE_H / 2]
    : [x, s.y + TITLE_H + PAD_TOP + SLOT_H * i + SLOT_H / 2];
}

function screenToGraph(cam: Camera, sx: number, sy: number): Point { return [sx / cam.z - cam.x, sy / cam.z - cam.y]; }

function pan(cam: Camera, dx: number, dy: number): Camera { return { x: cam.x + dx / cam.z, y: cam.y + dy / cam.z, z: cam.z }; }

function zoomAt(cam: Camera, f: number, sx: number, sy: number): Camera {
  const z = clamp(cam.z * f, MIN_Z, MAX_Z);
  const gp = screenToGraph(cam, sx, sy);
  return { z, x: sx / z - gp[0], y: sy / z - gp[1] };
}

function contentBounds(boxes: readonly Box[]): Box | null {
  if (!boxes.length) { return null; }
  let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
  boxes.forEach((b) => {
    x0 = Math.min(x0, b.x); y0 = Math.min(y0, b.y);
    x1 = Math.max(x1, b.x + b.w); y1 = Math.max(y1, b.y + b.h);
  });
  return { x: x0, y: y0, w: x1 - x0, h: y1 - y0 };
}

function fitView(boxes: readonly Box[], vw: number, vh: number, padding: number): Camera {
  const b = contentBounds(boxes);
  if (!b) { return { x: 0, y: 0, z: 1 }; }
  const z = clamp(Math.min(Math.max(1, vw - 2 * padding) / Math.max(1, b.w),
                           Math.max(1, vh - 2 * padding) / Math.max(1, b.h)), MIN_Z, 1);
  return { z, x: (vw - b.w * z) / 2 / z - b.x, y: (vh - b.h * z) / 2 / z - b.y };
}

function normRect(a: Point, b: Point): Box {
  return { x: Math.min(a[0], b[0]), y: Math.min(a[1], b[1]),
           w: Math.abs(b[0] - a[0]), h: Math.abs(b[1] - a[1]) };
}

function intersects(a: Box, b: Box): boolean {
  return a.x < b.x + b.w && a.x + a.w > b.x && a.y < b.y + b.h && a.y + a.h > b.y;
}

function contains(a: Box, b: Box): boolean {
  return a.x <= b.x && a.y <= b.y && a.x + a.w >= b.x + b.w && a.y + a.h >= b.y + b.h;
}

function snap(v: number, size: number): number { return size > 0 ? size * Math.round(v / size) : v; }

// One segment after the moveto; each segment leaves its left end to the
// right and enters its right end from the left.
function pathBody(mode: string, a: Point, b: Point): string {
  if (mode === "linear") {
    return "L" + pt([a[0] + 15, a[1]]) + " L" + pt([b[0] - 15, b[1]]) + " L" + pt(b);
  }
  if (mode === "straight") {
    const ia: Point = [a[0] + 10, a[1]], ib: Point = [b[0] - 10, b[1]];
    const mid = num(0.5 * (ia[0] + ib[0]));
    return "L" + pt(ia) + " L" + mid + "," + num(ia[1]) + " L" + mid + "," + num(ib[1]) +
      " L" + pt(ib) + " L" + pt(b);
  }
  const d = Math.max(30, Math.hypot(b[0] - a[0], b[1] - a[1]) * 0.25);
  return "C" + pt([a[0] + d, a[1]]) + " " + pt([b[0] - d, b[1]]) + " " + pt(b);
}

function chainPath(mode: string, pts: Point[]): string {
  const parts: string[] = [];
  for (let i = 0; i + 1 < pts.length; i++) { parts.push(pathBody(mode, pts[i] as Point, pts[i + 1] as Point)); }
  return "M" + pt(pts[0] as Point) + " " + parts.join(" ");
}

function nearestSegment(pts: Point[], p: Point): number {
  let best = -1, dist = Infinity;
  for (let i = 0; i + 1 < pts.length; i++) {
    const a = pts[i] as Point, b = pts[i + 1] as Point;
    const cx = (a[0] + b[0]) / 2, cy = (a[1] + b[1]) / 2;
    const d = Math.hypot(cx - p[0], cy - p[1]);
    if (d < dist) { dist = d; best = i; }
  }
  return best;
}

// ------------------------------------------------------------------
// Template views: the same fields aihtml_node_graph.erl builds.
// ------------------------------------------------------------------

function nodeStyle(n: GraphNode): string {
  const c = safeColor(n.color);
  return "transform:translate3d(" + num(n.pos[0]) + "px," + num(n.pos[1]) + "px,0);" +
    "--ah-ng-node-width:" + num(nodeWidth(n)) + "px;" +
    (n.height ? "--ah-ng-node-height:" + num(n.height) + "px;" : "") +
    (c ? "--ah-ng-node-accent:" + c + ";" : "");
}

function slotView(id: string, kind: SlotKind, i: number, s: GraphSlot, connected: boolean | undefined): Rec {
  const label = s.label !== undefined && s.label !== null ? String(s.label) : String(s.name);
  const colors = slotColors(s.type);
  const multi = colors.length > 1;
  const paths = colors.length === 2 ? TWO_SLICES : THREE_SLICES;
  return {
    kind, key: slotKey(id, kind, i), index: String(i),
    connected: !!connected, optional: !!s.optional, tip: normType(s.type),
    shape: s.shape || "circle", multi, color: colors[0],
    slices: multi ? colors.map((c, k) => ({ d: paths[k], fill: c })) : [],
    has_label: label !== "", label
  };
}

function nodeView(n: GraphNode, conn: Record<string, Conn>, readOnly: boolean, hw: NodeHtml): Rec {
  const c = conn[n.id] || { in: {}, out: {} };
  const collapsed = !!n.collapsed;
  const type = n.type !== undefined && n.type !== null ? String(n.type) : "";
  const in0 = n.inputs[0], out0 = n.outputs[0];
  return {
    id: n.id,
    title: String(n.title || n.type || n.id),
    has_type: type !== "", type,
    style: nodeStyle(n),
    collapsed,
    sized: !!n.height && !collapsed,
    expanded: collapsed ? "false" : "true",
    toggle_label: collapsed ? AH.t("node_graph", "expand_node", "Expand node")
                            : AH.t("node_graph", "collapse_node", "Collapse node"),
    stub_in: collapsed && in0 ? [{ color: linkColor(in0.type) }] : [],
    stub_out: collapsed && out0 ? [{ color: linkColor(out0.type) }] : [],
    inputs: n.inputs.map((s, i) => slotView(n.id, "input", i, s, c.in[i])),
    outputs: n.outputs.map((s, i) => slotView(n.id, "output", i, s, c.out[i])),
    has_widgets: hw.widgets.length > 0,
    widgets: hw.widgets.map((h) => ({ html: h })),
    has_body: hw.body !== null && hw.body !== undefined,
    body: hw.body || "",
    resizable: !readOnly
  };
}

function groupView(gr: GraphGroup, readOnly: boolean): Rec {
  const b = gr.bounds, c = safeColor(gr.color);
  return {
    id: gr.id,
    title: gr.title ? String(gr.title) : AH.t("node_graph", "group", "Group"),
    style: "transform:translate3d(" + num(b[0]) + "px," + num(b[1]) + "px,0);" +
      "width:" + num(b[2]) + "px;height:" + num(b[3]) + "px;" +
      (c ? "--ah-ng-group-color:" + c + ";" : ""),
    editable: !readOnly,
    txt_delete_group: AH.t("node_graph", "delete_group", "Delete group"),
    txt_delete_group_title: AH.t("node_graph", "delete_group_title", "Delete group frame (nodes stay)")
  };
}

function connIndex(g: Graph): Record<string, Conn> {
  const c: Record<string, Conn> = {};
  g.links.forEach((l) => {
    const s = c[l.source[0]] = c[l.source[0]] || { in: {}, out: {} };
    s.out[l.source[1]] = true;
    const t = c[l.target[0]] = c[l.target[0]] || { in: {}, out: {} };
    t.in[l.target[1]] = true;
  });
  return c;
}

function outType(g: Graph, l: GraphLink): unknown {
  const s = slotOf(g, l.source[0], "output", l.source[1]);
  return s ? s.type : null;
}

// ------------------------------------------------------------------
// DOM helpers
// ------------------------------------------------------------------

// Direct children of el matching selector (kids), the first one (kid).
function kids<E extends Element = HTMLElement>(el: Element | null, selector: string): E[] {
  return el ? Array.from(el.children).filter((c): c is E => c.matches(selector)) : [];
}

function kid<E extends Element = HTMLElement>(el: Element | null, selector: string): E | null {
  return kids<E>(el, selector)[0] || null;
}

// Trusted HTML (templates, literals) -> its top-level elements.
function parse(html: string): HTMLElement[] {
  const t = document.createElement("template");
  t.innerHTML = html;
  return Array.from(t.content.children) as HTMLElement[];
}

// A literal's one element.
function parseOne<E extends HTMLElement>(html: string): E {
  return parse(html)[0] as E;
}

function graphJson(g: Graph): string { return JSON.stringify(g); }

// Widget HTML of a live card (to copy it); mounted markers dropped so
// the copy mounts again.
function cardHtml(card: Element | null): NodeHtml {
  const out: NodeHtml = { widgets: [], body: null };
  if (!card) { return out; }
  const body = kid(card, ".ah-node-graph-node-body");
  kids(kid(body, ".ah-node-graph-widgets"), ".ah-node-graph-widget").forEach((w) => {
    out.widgets.push(cleanHtml(w));
  });
  const custom = kid(body, ".ah-node-graph-custom");
  if (custom) { out.body = cleanHtml(custom); }
  return out;
}

function cleanHtml(el: Element): string {
  const c = el.cloneNode(true) as Element;
  c.querySelectorAll("[data-ah-mounted]").forEach((m) => { m.removeAttribute("data-ah-mounted"); });
  return c.innerHTML;
}

function diffList<T extends { id: string }>(a: T[], b: T[]): { changed: T[]; removed: string[] } {
  const before: Record<string, string> = {}, after: Record<string, boolean> = {};
  a.forEach((x) => { before[x.id] = JSON.stringify(x); });
  b.forEach((x) => { after[x.id] = true; });
  return {
    changed: b.filter((x) => before[x.id] !== JSON.stringify(x)),
    removed: a.filter((x) => !after[x.id]).map((x) => x.id)
  };
}

function miniTransform(content: Box | null, view: Box): { scale: number; ox: number; oy: number } {
  const boxes = [content, view].filter((b): b is Box => !!b);
  const b = contentBounds(boxes) as Box;   // view is always there
  const s = Math.min((MINI_W - 2 * MINI_PAD) / Math.max(1, b.w), (MINI_H - 2 * MINI_PAD) / Math.max(1, b.h));
  return { scale: s, ox: b.x - MINI_PAD / s, oy: b.y - MINI_PAD / s };
}

function filterItems(items: LibraryItem[], q: string): LibraryItem[] {
  const needle = String(q || "").trim().toLowerCase();
  if (!needle) { return items.slice(); }
  return items.filter((it) => [it.label, it.type, it.category].some((v) =>
    v !== undefined && v !== null && String(v).toLowerCase().indexOf(needle) >= 0));
}

// ------------------------------------------------------------------
// Link drag helpers (sigil node_graph/link_drag)
// ------------------------------------------------------------------

// Dragging from a connected input picks its link up: the real start is
// the output at its other end.
function originOf(g: Graph, id: string, kind: SlotKind, i: number): LinkOrigin {
  if (kind === "input") {
    const l = inputLink(g, id, i);
    if (l) { return { node: l.source[0], kind: "output", index: l.source[1], detach: l.id }; }
  }
  return { node: id, kind, index: i, detach: null };
}

function pairFor(origin: LinkOrigin, cand: Candidate): [SlotRef, SlotRef] {
  return origin.kind === "output"
    ? [[origin.node, origin.index], [cand[0], cand[2]]]
    : [[cand[0], cand[2]], [origin.node, origin.index]];
}

function compatibleMap(g: Graph, origin: LinkOrigin, allowCycles: boolean): Record<string, boolean> {
  const want: SlotKind = origin.kind === "output" ? "input" : "output", out: Record<string, boolean> = {};
  g.nodes.forEach((n) => {
    (want === "input" ? n.inputs : n.outputs).forEach((_, i) => {
      const p = pairFor(origin, [n.id, want, i]);
      if (!connectProblem(g, p[0], p[1], allowCycles)) { out[slotKey(n.id, want, i)] = true; }
    });
  });
  return out;
}

// ------------------------------------------------------------------
// Misc
// ------------------------------------------------------------------

function nodeIdOf(el: Element): string | null {
  const card = el.closest(".ah-node-graph-node");
  return card ? card.getAttribute("data-node-id") : null;
}

function linkIdOf(el: Element): string | null {
  const g = el.closest(".ah-node-graph-link");
  return g ? g.getAttribute("data-link-id") : null;
}

function groupIdOf(el: Element): string | null {
  const g = el.closest(".ah-node-graph-group");
  return g ? g.getAttribute("data-group-id") : null;
}

function isEditable(t: Element): boolean {
  return /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName) || (t instanceof HTMLElement && t.isContentEditable) ||
    !!t.closest(".ah-node-graph-widget, .ah-node-graph-custom, .ah-node-graph-search, .ah-node-graph-ctxmenu");
}

function stop(e: Event): void { e.stopPropagation(); }

function point(e: MouseEvent): Point {
  return [e.clientX, e.clientY];
}

function wheelFactor(e: WheelEvent): number {
  const unit = e.deltaMode === 1 ? 16 : e.deltaMode === 2 ? 400 : 1;
  return Math.pow(1.0015, -e.deltaY * unit);
}

function readLibrary(el: Element): LibraryItem[] {
  const s = kid(el, "script.ah-node-graph-data");
  if (!s) { return []; }
  try {
    const data: unknown = JSON.parse(s.textContent || "");
    return list(isMap(data) ? data["library"] : null).filter(isMap);
  } catch (_x) { return []; }
}

// ------------------------------------------------------------------
// Behaviour
// ------------------------------------------------------------------

class NodeGraphController extends AH.Controller {
  /** element ids and search list ids */
  static #seq = 0;

  // the server's markup (aihtml_node_graph) has these parts
  #vp!: HTMLElement;
  #canvasEl!: HTMLElement;
  #groupsEl!: HTMLElement;
  #linksEl!: SVGElement;
  #nodesEl!: HTMLElement;
  #marqueeEl: HTMLElement | null = null;

  #handlers: Record<string, [string, Handler][]> = {};
  #opts: Options = { mode: "spline", snap: 0, readOnly: false, allowCycles: false };
  #library: LibraryItem[] = [];
  #g: Graph = { nodes: [], links: [], groups: [] };
  #cam: Camera = { x: 0, y: 0, z: 1 };
  #sel: string[] = [];
  #lsel: string[] = [];
  /** measured card heights */
  #sizes: Record<string, number> = {};
  #past: string[] = [];
  #future: string[] = [];
  #clip: Clip | null = null;
  /** HTML for the next render of these cards */
  #pending: Record<string, NodeHtml> = {};
  /** each card's signature: it is redrawn when that changes */
  #sigs = new WeakMap<Element, string | null>();
  #nodeDrag: NodeDrag | null = null;
  #linkDrag: LinkDrag | null = null;
  #wpDrag: WaypointDrag | null = null;
  #marquee: [Point, Point] | null = null;
  #dragLinkEl: SVGGElement | null = null;
  #hand = false;
  #space = false;
  #drags: (() => void)[] = [];
  #overlay: Overlay | null = null;
  // touch pointers of the viewport, a pinch, the viewport's own drag
  #pointers = new Map<number, Point>();
  #pinch: { dist: number; mid: Point; cam: Camera } | null = null;
  #vpCancel: (() => void) | null = null;

  override setup(): void {
    const el = this.element;
    if (!el.id) { el.id = "ah-g" + (++NodeGraphController.#seq) + Date.now().toString(36); }
    this.#vp = kid(el, ".ah-node-graph-viewport") as HTMLElement;
    this.#canvasEl = kid(this.#vp, ".ah-node-graph-canvas") as HTMLElement;
    this.#groupsEl = kid(this.#canvasEl, ".ah-node-graph-groups") as HTMLElement;
    this.#linksEl = kid<SVGElement>(this.#canvasEl, ".ah-node-graph-links") as SVGElement;
    this.#nodesEl = kid(this.#canvasEl, ".ah-node-graph-nodes") as HTMLElement;
    this.#marqueeEl = kid(this.#canvasEl, ".ah-node-graph-marquee");
    this.#handlers = {};
    this.#opts = {
      mode: el.getAttribute("data-ah-link-mode") || "spline",
      snap: parseInt(el.getAttribute("data-ah-snap") || "0", 10) || 0,
      readOnly: el.getAttribute("data-read-only") === "true",
      allowCycles: el.getAttribute("data-ah-allow-cycles") === "true"
    };
    this.#library = readLibrary(el);
    this.#g = { nodes: [], links: [], groups: [] };
    this.#cam = { x: 0, y: 0, z: 1 };
    this.#sel = []; this.#lsel = []; this.#sizes = {}; this.#past = []; this.#future = [];
    this.#clip = null; this.#pending = {};
    this.#nodeDrag = null; this.#linkDrag = null; this.#wpDrag = null; this.#marquee = null;
    this.#dragLinkEl = null; this.#hand = false; this.#space = false; this.#drags = [];
    this.#overlay = null; this.#pointers = new Map(); this.#pinch = null; this.#vpCancel = null;
    try { this.#g = normalize(JSON.parse(el.getAttribute("data-ah-value") || "{}")); } catch (_x) { /* empty */ }
    // adopt the server's cards: same template, same data
    const conn = connIndex(this.#g);
    this.#g.nodes.forEach((n) => {
      const card = this.cardOf(n.id);
      if (card) { this.#sigs.set(card, this.cardSig(n, conn)); }
    });
    this.measure();
    this.wireNodes();
    this.wireLinks();
    this.wireGroups();
    this.wireViewport();
    this.wireToolbar();
    this.applyCamera();
    if (el.getAttribute("data-ah-auto-fit") === "true") { this.fit(); }
  }

  override teardown(): void {
    this.#drags.slice().forEach((f) => { f(); });
    this.closeOverlay();
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getGraph(): Graph { return clone(this.#g); }
  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }
  setGraph(graph: Graph | string): void {
    const g = normalize(typeof graph === "string" ? JSON.parse(graph) : graph);
    g.nodes.forEach((n) => { this.takeHtml(n); });
    this.#g = g;
    this.#past = [];
    this.#future = [];
    this.pruneSelection();
    this.#lsel = [];
    this.renderAll();
    this.writeValue();
  }
  fitView(): void { this.fit(); }
  undo(): void { this.travel(this.#past, this.#future, "undo"); }
  redo(): void { this.travel(this.#future, this.#past, "redo"); }
  getSelection(): string[] { return this.#sel.slice(); }
  selectNodes(xs?: unknown[] | null): void { this.select((xs || []).map(String), "replace"); }
  deleteSelection(): void {
    const nodes = this.#sel.slice(), links = this.#lsel.slice();
    if (!nodes.length && !links.length) { return; }
    this.mutate(links.length && !nodes.length ? "disconnect" : "remove", (g) => {
      g.links = g.links.filter((l) => links.indexOf(l.id) < 0);
      removeNodes(g, nodes);
    });
  }

  // ------------------------------------------------------------------
  // State helpers
  // ------------------------------------------------------------------

  // Delegated listeners in bubbling order, which the wiring below relies
  // on: per event type one listener on the root; handlers run from the
  // target up to the root, in binding order at each level, with the
  // matched element as their second argument, and an inner handler's
  // stopPropagation keeps the outer ones from running (a slot's
  // pointerdown and its card's, a card's and the viewport's).
  private on<K extends keyof HTMLElementEventMap>(type: K, selector: string,
                                                   fn: (e: HTMLElementEventMap[K], match: Element) => void): void {
    let handlers = this.#handlers[type];
    if (!handlers) {
      const hs: [string, Handler][] = handlers = this.#handlers[type] = [];
      const root = this.element;
      this.listen(root, type, (e: Event) => { NodeGraphController.dispatch(root, hs, e); });
    }
    handlers.push([selector, (e, m) => { fn(e as HTMLElementEventMap[K], m); }]);
  }

  private static dispatch(root: Element, handlers: [string, Handler][], e: Event): void {
    if (e.type === "click" && (e as MouseEvent).button >= 1) { return; }
    for (let cur = e.target as Node | null; cur && cur !== root; cur = cur.parentNode) {
      if (!(cur instanceof Element)) { continue; }
      for (const [sel, fn] of handlers) {
        if (cur.matches(sel)) { fn(e, cur); }
      }
      if (e.cancelBubble) { return; }
    }
  }

  // The node as drawn this frame: drag offset and live resize applied.
  private shown(n: GraphNode, live: boolean): Shown {
    const d = live ? this.#nodeDrag : null;
    let x = n.pos[0], y = n.pos[1], w = nodeWidth(n);
    let h = nodeHeight(n, this.#sizes[n.id]);
    if (d) {
      if (d.ids && d.ids[n.id]) { x += d.dx || 0; y += d.dy || 0; }
      if (d.resize && d.resize.id === n.id) { w = d.resize.w; h = d.resize.h; }
    }
    return { id: n.id, x, y, w, h, collapsed: !!n.collapsed };
  }

  private shownMap(live: boolean): Record<string, Shown> {
    const m: Record<string, Shown> = {};
    this.#g.nodes.forEach((n) => { m[n.id] = this.shown(n, live); });
    return m;
  }

  private linkPoints(l: GraphLink, sm: Record<string, Shown>, live: boolean): Point[] | null {
    const a = sm[l.source[0]], b = sm[l.target[0]];
    if (!a || !b) { return null; }
    const pts = (l.points || []).map((p) => p.slice() as Point);
    const w = live ? this.#wpDrag : null;
    if (w && w.link === l.id && pts[w.index]) { pts[w.index] = w.point; }
    return [slotPos(a, "output", l.source[1])].concat(pts, [slotPos(b, "input", l.target[1])]);
  }

  private takeHtml(n: GraphNode): void {
    const html = n.html;
    if (html) {
      this.#pending[n.id] = { widgets: html.widgets || [], body: html.body === undefined ? null : html.body };
      delete n.html;
    }
  }

  private cardOf(id: string): HTMLElement | null {
    return kids(this.#nodesEl, ".ah-node-graph-node").filter((c) => c.getAttribute("data-node-id") === id)[0] || null;
  }

  // ------------------------------------------------------------------
  // Drawing
  // ------------------------------------------------------------------

  private cardSig(n: GraphNode, conn: Record<string, Conn>): string {
    const c = conn[n.id] || { in: {}, out: {} };
    return JSON.stringify([n.title, n.type, n.width, n.height, !!n.collapsed, n.color,
                           n.inputs, n.outputs, Object.keys(c.in).sort(),
                           Object.keys(c.out).sort(), this.#opts.readOnly]);
  }

  private renderNodes(): void {
    const box = this.#nodesEl, conn = connIndex(this.#g), cards: Record<string, HTMLElement> = {};
    kids(box, ".ah-node-graph-node").forEach((c) => {
      cards[c.getAttribute("data-node-id") || ""] = c;
    });
    let prev: HTMLElement | null = null;
    this.#g.nodes.forEach((n) => {
      let card = cards[n.id];
      delete cards[n.id];
      const sig = this.cardSig(n, conn);
      if (!card || this.#sigs.get(card) !== sig || this.#pending[n.id]) {
        card = this.renderCard(n, conn, card || null);
        this.#sigs.set(card, sig);
      } else {
        card.style.transform = "translate3d(" + num(n.pos[0]) + "px," + num(n.pos[1]) + "px,0)";
        card.removeAttribute("data-dragging");
      }
      const want: ChildNode | null = prev ? prev.nextSibling : box.firstChild;
      if (want !== card) { box.insertBefore(card, want); }
      prev = card;
    });
    // removed cards: their widgets' behaviours are torn down by the runtime
    Object.keys(cards).forEach((k) => { const c = cards[k]; if (c) { c.remove(); } });
    this.measure();
  }

  private renderCard(n: GraphNode, conn: Record<string, Conn>, old: HTMLElement | null): HTMLElement {
    const pending = this.#pending[n.id];
    delete this.#pending[n.id];
    const body = old && kid(old, ".ah-node-graph-node-body");
    const oldW = !pending && body ? kid(body, ".ah-node-graph-widgets") : null;
    const oldB = !pending && body ? kid(body, ".ah-node-graph-custom") : null;
    const hw: NodeHtml = pending || {
      widgets: oldW ? Array.from(oldW.children).map(() => "") : [],
      body: oldB ? "" : null
    };
    // the template renders one card
    const card = parse((AH.tpl["node_graph_node"] as (v: unknown) => string)(nodeView(n, conn, this.#opts.readOnly, hw)))
      .filter((x) => x.matches(".ah-node-graph-node"))[0] as HTMLElement;
    const nb = kid(card, ".ah-node-graph-node-body");
    const slotW = oldW && kid(nb, ".ah-node-graph-widgets");
    if (oldW && slotW) { slotW.replaceWith(oldW); }
    const slotB = oldB && kid(nb, ".ah-node-graph-custom");
    if (oldB && slotB) { slotB.replaceWith(oldB); }
    const act = document.activeElement;
    const refocus = old && act && old.contains(act) && act.classList.contains("ah-node-graph-collapse");
    // widgets moved over keep their behaviours; new widget HTML (pending)
    // is mounted, and the old card's torn down, by the runtime
    if (old && old.parentNode) {
      old.parentNode.replaceChild(card, old);
    } else {
      this.#nodesEl.appendChild(card);
    }
    if (refocus) {
      const btn = card.querySelector<HTMLElement>(".ah-node-graph-collapse");
      if (btn) { btn.focus(); }
    }
    return card;
  }

  // Card heights only the browser knows (widget rows): for marquee hits,
  // fit and the minimap. Nodes with their own height keep it.
  private measure(): void {
    kids(this.#nodesEl, ".ah-node-graph-node").forEach((card) => {
      const id = card.getAttribute("data-node-id") || "";
      const h = card.offsetHeight;
      if (h > 0) { this.#sizes[id] = h; }
    });
  }

  private renderLinks(): void {
    const sm = this.shownMap(true), sel = this.#lsel, g = this.#g;
    const detached = this.#linkDrag && this.#linkDrag.origin.detach;
    const tpl = AH.tpl["node_graph_link"] as (v: unknown) => string;
    this.#linksEl.innerHTML = g.links.map((l) => {
      const pts = l.id === detached ? null : this.linkPoints(l, sm, true);
      if (!pts) { return ""; }
      const color = linkColor(outType(g, l));
      return tpl({
        id: l.id, selected: sel.indexOf(l.id) >= 0,
        d: chainPath(this.#opts.mode, pts), color,
        points: (l.points || []).map((_p, i) => {
          const q = pts[i + 1] as Point;
          return { index: String(i), x: num(q[0]), y: num(q[1]), color };
        })
      });
    }).join("");
    this.#dragLinkEl = null;
    this.applyLinkDrag();
  }

  // Per frame while dragging: only the path data and reroute points move.
  private updateLinkPaths(): void {
    const sm = this.shownMap(true), mode = this.#opts.mode;
    const detached = this.#linkDrag && this.#linkDrag.origin.detach;
    kids<SVGElement>(this.#linksEl, ".ah-node-graph-link").forEach((link) => {
      const l = findLink(this.#g, link.getAttribute("data-link-id"));
      const pts = l && l.id !== detached ? this.linkPoints(l, sm, true) : null;
      if (!pts) { link.style.display = "none"; return; }
      link.style.display = "";
      const d = chainPath(mode, pts);
      kids(link, "path").forEach((p) => { p.setAttribute("d", d); });
      kids(link, ".ah-node-graph-waypoint").forEach((w) => {
        const q = pts[parseInt(w.getAttribute("data-index") || "", 10) + 1];
        if (q) { w.setAttribute("cx", num(q[0])); w.setAttribute("cy", num(q[1])); }
      });
    });
  }

  private renderGroups(): void {
    const ro = this.#opts.readOnly;
    const tpl = AH.tpl["node_graph_group"] as (v: unknown) => string;
    this.#groupsEl.innerHTML = this.#g.groups.map((gr) => tpl(groupView(gr, ro))).join("");
  }

  private renderAll(): void {
    this.renderGroups();
    this.renderNodes();
    this.renderLinks();
    this.applySelection();
    this.applyToolbar();
    this.renderMinimap();
  }

  // Drag frame: cards, group frames and link paths follow #nodeDrag.
  private applyNodeDrag(): void {
    const d = this.#nodeDrag;
    kids(this.#nodesEl, ".ah-node-graph-node").forEach((card) => {
      const n = findNode(this.#g, card.getAttribute("data-node-id"));
      if (!n) { return; }
      const s = this.shown(n, true);
      const moved = d && d.ids && d.ids[n.id];
      card.style.transform = "translate3d(" + num(s.x) + "px," + num(s.y) + "px,0)";
      if (moved) { card.setAttribute("data-dragging", "true"); } else { card.removeAttribute("data-dragging"); }
      if (d && d.resize && d.resize.id === n.id) {
        card.style.setProperty("--ah-ng-node-width", num(d.resize.w) + "px");
        card.style.setProperty("--ah-ng-node-height", num(d.resize.h) + "px");
        card.setAttribute("data-sized", "true");
      }
    });
    kids(this.#groupsEl, ".ah-node-graph-group").forEach((frame) => {
      const gr = findGroup(this.#g, frame.getAttribute("data-group-id"));
      if (!gr) { return; }
      const b = d && d.groupResize && d.groupResize.id === gr.id ? d.groupResize.bounds : gr.bounds;
      const mv: Point = d && d.group === gr.id ? [d.dx || 0, d.dy || 0] : [0, 0];
      frame.style.transform = "translate3d(" + num(b[0] + mv[0]) + "px," + num(b[1] + mv[1]) + "px,0)";
      frame.style.width = num(b[2]) + "px";
      frame.style.height = num(b[3]) + "px";
    });
    this.updateLinkPaths();
    this.renderMinimap();
  }

  // Link drag frame: slot states and the link following the pointer.
  private applyLinkDrag(): void {
    const drag = this.#linkDrag;
    this.#nodesEl.querySelectorAll(".ah-node-graph-slot").forEach((slot) => {
      if (!drag) { slot.removeAttribute("data-drag-state"); return; }
      const k = slot.getAttribute("data-slot-key") || "";
      const c = drag.candidate;
      slot.setAttribute("data-drag-state",
                        c && k === slotKey(c[0], c[1], c[2]) ? "candidate"
                        : drag.compatible[k] ? "compatible" : "dimmed");
    });
    if (this.#dragLinkEl) { this.#dragLinkEl.remove(); this.#dragLinkEl = null; }
    kids<SVGElement>(this.#linksEl, ".ah-node-graph-link").forEach((link) => {
      if (drag && drag.origin.detach === link.getAttribute("data-link-id")) {
        link.style.display = "none";
      }
    });
    if (!drag) { return; }
    const sm = this.shownMap(true), o = drag.origin;
    const from = sm[o.node];
    const anchor = from && slotPos(from, o.kind, o.index);
    if (!anchor) { return; }
    const c = drag.candidate;
    const to = c ? sm[c[0]] : undefined;
    const target = c && to ? slotPos(to, c[1], c[2]) : drag.point;
    const a = o.kind === "output" ? anchor : target, b = o.kind === "output" ? target : anchor;
    const gEl = document.createElementNS(SVG_NS, "g");
    gEl.setAttribute("class", "ah-node-graph-drag-link");
    const p = document.createElementNS(SVG_NS, "path");
    p.setAttribute("class", "ah-node-graph-link-line");
    p.setAttribute("d", "M" + pt(a) + " " + pathBody(this.#opts.mode, a, b));
    p.setAttribute("style", "stroke:" + linkColor(drag.type));
    gEl.appendChild(p);
    this.#linksEl.appendChild(gEl);
    this.#dragLinkEl = gEl;
  }

  private applyMarquee(): void {
    const el = this.#marqueeEl, r = this.#marquee;
    if (!el) { return; }
    if (!r) { el.hidden = true; return; }
    const b = normRect(r[0], r[1]);
    el.hidden = false;
    el.style.transform = "translate3d(" + num(b.x) + "px," + num(b.y) + "px,0)";
    el.style.width = num(b.w) + "px";
    el.style.height = num(b.h) + "px";
  }

  private applySelection(): void {
    kids(this.#nodesEl, ".ah-node-graph-node").forEach((card) => {
      if (this.#sel.indexOf(card.getAttribute("data-node-id") || "") >= 0) {
        card.setAttribute("data-selected", "true");
      } else { card.removeAttribute("data-selected"); }
    });
    kids<SVGElement>(this.#linksEl, ".ah-node-graph-link").forEach((link) => {
      if (this.#lsel.indexOf(link.getAttribute("data-link-id") || "") >= 0) {
        link.setAttribute("data-selected", "true");
      } else { link.removeAttribute("data-selected"); }
    });
    this.applyToolbar();
  }

  private applyCamera(): void {
    const c = this.#cam;
    this.#canvasEl.style.transform = "scale3d(" + c.z + "," + c.z + ",1) translate3d(" +
      c.x + "px," + c.y + "px,0)";
    const s = this.#vp.style;
    s.setProperty("--ah-ng-grid-size", GRID_DISPLAY * c.z + "px");
    s.setProperty("--ah-ng-grid-x", c.x * c.z + "px");
    s.setProperty("--ah-ng-grid-y", c.y * c.z + "px");
    this.applyToolbar();
    this.renderMinimap();
  }

  private applyToolbar(): void {
    const bar = kid(this.element, ".ah-node-graph-toolbar");
    if (!bar) { return; }
    const each = (sel: string, f: (b: HTMLButtonElement) => void): void => {
      bar.querySelectorAll<HTMLButtonElement>(sel).forEach(f);
    };
    each(".ah-node-graph-zoom", (z) => { z.textContent = Math.round(this.#cam.z * 100) + "%"; });
    each('[data-action="undo"]', (b) => { b.disabled = !this.#past.length; });
    each('[data-action="redo"]', (b) => { b.disabled = !this.#future.length; });
    each('[data-action="delete"]', (b) => { b.disabled = !this.#sel.length && !this.#lsel.length; });
  }

  private viewBox(): Box {
    const r = this.#vp.getBoundingClientRect(), c = this.#cam;
    return { x: -c.x, y: -c.y, w: Math.max(1, r.width) / c.z, h: Math.max(1, r.height) / c.z };
  }

  private renderMinimap(): void {
    const mini = kid(this.element, ".ah-node-graph-minimap");
    if (!mini) { return; }
    const boxes = this.#g.nodes.map((n) => this.shown(n, true));
    const view = this.viewBox();
    const t = miniTransform(contentBounds(boxes), view);
    const style = (b: Box): string =>
      "left:" + num((b.x - t.ox) * t.scale) + "px;top:" + num((b.y - t.oy) * t.scale) +
      "px;width:" + num(Math.max(1, b.w * t.scale)) + "px;height:" +
      num(Math.max(1, b.h * t.scale)) + "px;";
    mini.innerHTML = (AH.tpl["node_graph_minimap"] as (v: unknown) => string)({
      nodes: boxes.map((b) => ({ selected: this.#sel.indexOf(b.id) >= 0, style: style(b) })),
      view: style(view)
    });
  }

  // ------------------------------------------------------------------
  // Edits, history, events
  // ------------------------------------------------------------------

  private writeValue(): void {
    const json = graphJson(this.#g);
    this.element.setAttribute("data-ah-value", json);
    kids<HTMLInputElement>(this.element, "input[type=hidden]").forEach((h) => { h.value = json; });
  }

  private emit(op: string, old: Graph): void {
    this.writeValue();
    const g = this.#g;
    const dn = diffList(old.nodes, g.nodes), dl = diffList(old.links, g.links);
    const dg = diffList(old.groups, g.groups);
    const changed = { nodes: dn.changed, links: dl.changed, groups: dg.changed };
    const removed = { nodes: dn.removed, links: dl.removed, groups: dg.removed };
    this.element.setAttribute("data-op", op);
    this.element.setAttribute("data-changed", JSON.stringify(changed));
    this.element.setAttribute("data-removed", JSON.stringify(removed));
    this.fire<NodeGraphChange>("change", { op, changed, removed, graph: clone(g) });
  }

  // The one way to change the graph: fn edits a copy; a real change goes
  // into the history, is drawn and fires change.
  private mutate(op: string, fn: (g: Graph) => boolean | void): boolean {
    if (this.#opts.readOnly) { return false; }
    const before = graphJson(this.#g);
    const g = JSON.parse(before) as Graph;   // a copy of our own graph
    if (fn(g) === false) { return false; }
    if (graphJson(g) === before) { return false; }
    this.#past.push(before);
    if (this.#past.length > 200) { this.#past.shift(); }
    this.#future = [];
    const old = this.#g;
    this.#g = g;
    this.pruneSelection();
    this.renderAll();
    this.emit(op, old);
    return true;
  }

  private travel(from: string[], to: string[], op: string): void {
    const json = from.pop();
    if (json === undefined) { return; }
    to.push(graphJson(this.#g));
    const old = this.#g;
    this.#g = JSON.parse(json) as Graph;   // a graph from the history
    this.pruneSelection();
    this.renderAll();
    this.emit(op, old);
  }

  private pruneSelection(): void {
    const n = ids(this.#g.nodes), l = ids(this.#g.links);
    const sel = this.#sel.filter((id) => n[id]);
    this.#lsel = this.#lsel.filter((id) => l[id]);
    if (sel.length !== this.#sel.length) { this.setSelection(sel, this.#lsel); }
  }

  private setSelection(nodes: string[], links: string[]): void {
    const before = join(this.#sel);
    this.#sel = nodes;
    this.#lsel = links;
    this.applySelection();
    this.renderMinimap();
    if (join(this.#sel) !== before) {
      this.element.setAttribute("data-selection", join(this.#sel));
      this.fire<NodeGraphSelection>("ah:selection-change", this.#sel.slice());
    }
  }

  private select(xs: string[], mode: "replace" | "add" | "toggle"): void {
    const cur = this.#sel.slice();
    xs.forEach((id) => {
      const i = cur.indexOf(id);
      if (mode === "toggle" && i >= 0) { cur.splice(i, 1); } else if (i < 0) { cur.push(id); }
    });
    this.setSelection(mode === "replace" ? xs.slice() : cur, []);
  }

  // Raise nodes in the z order: no history, no change event (a click on a
  // card is not an edit); the value follows.
  private bringToFront(xs: string[]): void {
    const s: Record<string, boolean> = {};
    xs.forEach((id) => { s[id] = true; });
    const g = this.#g;
    const order = g.nodes.filter((n) => !s[n.id]).concat(g.nodes.filter((n) => s[n.id]));
    const same = order.every((n, i) => n === g.nodes[i]);
    if (same) { return; }
    g.nodes = order;
    order.forEach((n) => {
      const card = this.cardOf(n.id);
      if (card) { this.#nodesEl.appendChild(card); }
    });
    this.writeValue();
  }

  private copy(xs: string[]): void {
    if (!xs.length) { return; }
    const sub = extract(this.#g, xs);
    const clip: Clip = this.#clip = { graph: clone(sub), html: {} };
    sub.nodes.forEach((n) => { clip.html[n.id] = cardHtml(this.cardOf(n.id)); });
  }

  private paste(clip: Clip | null, op: string): void {
    if (!clip || !clip.graph.nodes.length) { return; }
    let map: Record<string, string> | null = null;
    const ok = this.mutate(op, (g) => {
      const m: Record<string, string> = map = insert(g, clip.graph, 20, 20);
      Object.keys(m).forEach((from) => {
        const html = clip.html[from];
        if (html) { this.#pending[m[from] as string] = html; }
      });
    });
    const m: Record<string, string> | null = map;
    if (ok && m) {
      this.select(clip.graph.nodes.map((n) => m[n.id] as string), "replace");
    } else if (m) {
      Object.keys(m).forEach((from) => { delete this.#pending[m[from] as string]; });
    }
  }

  private duplicate(xs: string[]): void {
    const saved = this.#clip;
    this.copy(xs);
    const clip = this.#clip;
    this.#clip = saved;
    this.paste(clip, "duplicate");
  }

  private fit(): void {
    const r = this.#vp.getBoundingClientRect();
    if (r.width <= 0 || r.height <= 0) { return; }
    this.#cam = fitView(this.#g.nodes.map((n) => this.shown(n, false)), r.width, r.height, 40);
    this.applyCamera();
  }

  private setCamera(cam: Camera): void {
    this.#cam = cam;
    this.applyCamera();
  }

  private zoomBy(f: number, sx?: number, sy?: number): void {
    if (sx === undefined || sy === undefined) {
      const r = this.#vp.getBoundingClientRect();
      sx = r.width / 2; sy = r.height / 2;
    }
    this.setCamera(zoomAt(this.#cam, f, sx, sy));
  }

  // ------------------------------------------------------------------
  // Pointer drags (sigil begin-drag!): capture on the viewport, end on
  // pointerup, a move without buttons or a window blur; Escape cancels.
  // ------------------------------------------------------------------

  private beginDrag(e: PointerEvent, spec: DragSpec): () => void {
    const x0 = e.clientX, y0 = e.clientY, pid: number | undefined = e.pointerId;
    const mouse = e.pointerType !== "touch";
    const threshold = spec.threshold || 0;
    let started = threshold === 0, done = false;
    let last: DragAt = { x: x0, y: y0, dx: 0, dy: 0 };
    const cap = this.#vp;
    const at = (ev: PointerEvent): DragAt => ({ x: ev.clientX, y: ev.clientY, dx: ev.clientX - x0, dy: ev.clientY - y0 });
    const mine = (ev: PointerEvent): boolean =>
      pid === undefined || ev.pointerId === undefined || ev.pointerId === pid;
    const cleanup = (): void => {
      document.removeEventListener("pointermove", move);
      document.removeEventListener("pointerup", up);
      document.removeEventListener("pointercancel", up);
      document.removeEventListener("keydown", key, true);
      window.removeEventListener("blur", blur);
      try { if (pid !== undefined && cap.hasPointerCapture(pid)) { cap.releasePointerCapture(pid); } } catch (_x) { /* gone */ }
      this.#drags = this.#drags.filter((f) => f !== cancel);
    };
    const finish = (f: ((arg: DragAt) => void) | null | undefined, arg: DragAt): void => {
      if (done) { return; }
      done = true;
      cleanup();
      if (f) { f(arg); }
    };
    const cancelled = (): void => { if (spec.cancel) { spec.cancel(); } };
    const lost = (): void => { if (started) { finish(spec.end, last); } else { finish(null, last); } };
    const move = (ev: PointerEvent): void => {
      if (!mine(ev)) { return; }
      if (mouse && ev.buttons === 0) { lost(); return; }
      const s = at(ev);
      last = s;
      if (!started && Math.hypot(s.dx, s.dy) > threshold) {
        started = true;
        if (spec.start) { spec.start(s); }
      }
      if (started && spec.move) { spec.move(s); }
    };
    const up = (ev: PointerEvent): void => {
      if (!mine(ev)) { return; }
      if (started) { finish(spec.end, at(ev)); } else { finish(spec.click || null, at(ev)); }
    };
    const key = (ev: KeyboardEvent): void => {
      if (ev.key === "Escape") {
        ev.stopPropagation();
        finish(cancelled, last);
      }
    };
    const blur = (): void => { lost(); };
    const cancel = (): void => { finish(cancelled, last); };
    document.addEventListener("pointermove", move);
    document.addEventListener("pointerup", up);
    document.addEventListener("pointercancel", up);
    document.addEventListener("keydown", key, true);
    window.addEventListener("blur", blur);
    try { if (pid !== undefined) { cap.setPointerCapture(pid); } } catch (_x) { /* synthetic */ }
    this.#drags.push(cancel);
    if (started && spec.start) { spec.start(last); }
    return cancel;
  }

  private clientToScreen(cx: number, cy: number): Point {
    const r = this.#vp.getBoundingClientRect();
    return [cx - r.left, cy - r.top];
  }

  private clientToGraph(cx: number, cy: number): Point {
    const s = this.clientToScreen(cx, cy);
    return screenToGraph(this.#cam, s[0], s[1]);
  }

  private clientToHost(cx: number, cy: number): Point {
    const r = this.element.getBoundingClientRect();
    return [cx - r.left, cy - r.top];
  }

  // ------------------------------------------------------------------
  // Overlays: context menu, node search menu
  // ------------------------------------------------------------------

  private closeOverlay(refocus?: boolean): void {
    const o = this.#overlay;
    if (!o) { return; }
    this.#overlay = null;
    o.cleanup();
    o.el.remove();
    if (refocus) { this.#vp.focus(); }
  }

  private openOverlay(el: HTMLElement, at: Point): void {
    this.closeOverlay();
    this.element.appendChild(el);
    // keep it inside the component
    const host = this.element.getBoundingClientRect();
    const x = Math.max(4, Math.min(at[0], host.width - el.offsetWidth - 4));
    const y = Math.max(4, Math.min(at[1], host.height - el.offsetHeight - 4));
    el.style.left = x + "px";
    el.style.top = y + "px";
    const outside = (e: PointerEvent): void => { if (!el.contains(e.target as Node | null)) { this.closeOverlay(); } };
    const key = (e: KeyboardEvent): void => { if (e.key === "Escape") { e.preventDefault(); this.closeOverlay(true); } };
    const t = setTimeout(() => { document.addEventListener("pointerdown", outside, true); }, 0);
    el.addEventListener("keydown", key);
    this.#overlay = {
      el,
      cleanup: () => {
        clearTimeout(t);
        document.removeEventListener("pointerdown", outside, true);
        el.removeEventListener("keydown", key);
      }
    };
    el.addEventListener("contextmenu", (e) => { e.preventDefault(); });
  }

  private contextMenu(at: Point, entries: (MenuItem | null)[]): void {
    const items = entries.filter((it): it is MenuItem => !!it);
    const el = parseOne('<div class="ah-node-graph-ctxmenu" role="menu"></div>');
    el.setAttribute("aria-label", AH.t("node_graph", "actions", "Actions"));
    el.innerHTML = (AH.tpl["node_graph_menu"] as (v: unknown) => string)({
      items: items.map((it, i) => ({ index: String(i), label: it.label, danger: !!it.danger,
                                     has_hint: !!it.hint, hint: it.hint || "" }))
    });
    this.openOverlay(el, at);
    const buttons = kids(el, ".ah-node-graph-ctxmenu-item");
    el.addEventListener("click", (e) => {
      const b = e.target instanceof Element ? e.target.closest(".ah-node-graph-ctxmenu-item") : null;
      if (!b || !el.contains(b)) { return; }
      const it = items[parseInt(b.getAttribute("data-index") || "", 10)];
      this.closeOverlay(true);
      if (it && it.run) { it.run(); }
    });
    el.addEventListener("keydown", (e) => {
      const i = buttons.indexOf(document.activeElement as HTMLElement), n = buttons.length;
      const moves: Record<string, number> = { ArrowDown: (i + 1) % n, ArrowUp: (i - 1 + n) % n, Home: 0, End: n - 1 };
      const next = moves[e.key];
      const b = next !== undefined ? buttons[next] : undefined;
      if (b) { e.preventDefault(); b.focus(); }
      if (e.key === "Tab") { e.preventDefault(); this.closeOverlay(true); }
    });
    const b0 = buttons[0];
    if (b0) { b0.focus(); }
  }

  // The node search menu: right click on the canvas, or a link dropped on
  // empty canvas (origin set: the new node gets connected).
  private searchMenu(at: Point, graphPt: Point, origin: LinkOrigin | null): void {
    const canvas: LibraryItem[] = origin ? [] : [{ kind: "group", label: AH.t("node_graph", "new_group_frame", "New group frame"),
                                                               category: AH.t("node_graph", "canvas", "Canvas") }];
    const items = this.#library;
    const listId = this.element.id + "-search-" + (++NodeGraphController.#seq);
    const s = { q: "", active: 0 };
    const el = parseOne('<div class="ah-node-graph-search" role="dialog"></div>');
    el.setAttribute("aria-label", AH.t("node_graph", "add_node", "Add node"));
    const input = parseOne<HTMLInputElement>('<input class="ah-node-graph-search-input" type="text" role="combobox" ' +
                                             'aria-autocomplete="list" aria-expanded="true">');
    input.setAttribute("placeholder", AH.t("node_graph", "search_nodes", "Search nodes…"));
    input.setAttribute("aria-controls", listId);
    const body = parseOne('<div class="ah-node-graph-search-body"></div>');
    el.appendChild(input);
    el.appendChild(body);
    const entries = (): LibraryItem[] => filterItems(canvas, s.q).concat(filterItems(items, s.q));
    const paint = (): void => {
      const l = entries(), n = l.length, idx = n ? ((s.active % n) + n) % n : 0;
      body.innerHTML = (AH.tpl["node_graph_search"] as (v: unknown) => string)({
        list_id: listId, empty: n === 0, empty_text: AH.t("node_graph", "no_match", "No matching node"),
        items: l.map((it, i) => ({ index: String(i), label: String(it.label || it.type), active: i === idx,
                                   has_category: !!it.category, category: it.category ? String(it.category) : "" }))
      });
      if (n) { input.setAttribute("aria-activedescendant", listId + "-" + idx); }
      else { input.removeAttribute("aria-activedescendant"); }
      const act = body.querySelector('[data-active="true"]');
      if (act && act.scrollIntoView) { act.scrollIntoView({ block: "nearest" }); }
    };
    const pick = (i: number): void => {
      const it = entries()[i];
      if (!it) { return; }
      this.closeOverlay(true);
      const x = Math.round(graphPt[0]), y = Math.round(graphPt[1]);
      if (it.kind === "group") {
        this.mutate("group-add", (g) => {
          g.groups.push({ id: freshId(ids(g.groups), "g"), title: AH.t("node_graph", "group", "Group"),
                          bounds: [x, y, 340, 260] });
        });
        return;
      }
      this.addNode(it, [x, y], origin);
    };
    this.openOverlay(el, at);
    paint();
    input.addEventListener("input", () => { s.q = input.value; s.active = 0; paint(); });
    input.addEventListener("keydown", (e) => {
      const n = entries().length;
      if (e.key === "ArrowDown") { e.preventDefault(); s.active++; paint(); }
      else if (e.key === "ArrowUp") { e.preventDefault(); s.active--; paint(); }
      else if (e.key === "Enter") { e.preventDefault(); pick(n ? ((s.active % n) + n) % n : 0); }
    });
    // pointerdown, not click: the input's blur comes before click
    body.addEventListener("pointerdown", (e) => {
      const item = e.target instanceof Element ? e.target.closest(".ah-node-graph-search-item") : null;
      if (!item || !body.contains(item)) { return; }
      e.preventDefault();
      pick(parseInt(item.getAttribute("data-index") || "", 10));
    });
    input.focus();
  }

  // A library entry dropped at `pos'; connected to `origin' when it came
  // from a link drag. One history entry for both.
  private addNode(item: LibraryItem, pos: Point, origin: LinkOrigin | null): void {
    let id: string | null = null;
    const src: Rec = clone(isMap(item.node) ? item.node : {});
    const html = isMap(src["html"]) ? src["html"] : null;
    delete src["html"];
    this.mutate("add", (g) => {
      const nid = id = freshId(ids(g.nodes), "n");
      const n = Object.assign(src, { id: nid, pos, title: item.label || item.type, type: item.type }) as GraphNode;
      n.inputs = n.inputs || [];
      n.outputs = n.outputs || [];
      g.nodes.push(n);
      if (html) {
        this.#pending[nid] = { widgets: list(html["widgets"]).map(String),
                               body: html["body"] === undefined ? null : html["body"] === null ? null : String(html["body"]) };
      }
      if (origin) {
        const want: SlotKind = origin.kind === "output" ? "input" : "output";
        const slots = want === "input" ? n.inputs : n.outputs;
        const allow = this.#opts.allowCycles;
        for (let i = 0; i < slots.length; i++) {
          const pair = pairFor(origin, [nid, want, i]);
          if (!connectProblem(g, pair[0], pair[1], allow)) {
            if (origin.detach) { g.links = g.links.filter((l) => l.id !== origin.detach); }
            connect(g, pair[0], pair[1], allow);
            break;
          }
        }
      }
    });
    const added: string | null = id;
    if (added && findNode(this.#g, added)) { this.select([added], "replace"); }
  }

  private nearestCandidate(compat: Record<string, boolean>, origin: LinkOrigin, p: Point, radius: number): Candidate | null {
    const want: SlotKind = origin.kind === "output" ? "input" : "output";
    let best: Candidate | null = null, dist = Infinity;
    const sm = this.shownMap(false);
    this.#g.nodes.forEach((n) => {
      const s = sm[n.id] as Shown;
      (want === "input" ? n.inputs : n.outputs).forEach((_, i) => {
        if (!compat[slotKey(n.id, want, i)]) { return; }
        const q = slotPos(s, want, i), d = Math.hypot(q[0] - p[0], q[1] - p[1]);
        if (d <= radius && d < dist) { dist = d; best = [n.id, want, i]; }
      });
    });
    return best;
  }

  // ------------------------------------------------------------------
  // Title editing
  // ------------------------------------------------------------------

  private editTitle(span: Element, commit: (v: string) => void): void {
    const old = span.textContent || "";
    const input = document.createElement("input");
    input.type = "text";
    input.className = span.className + "-input";
    input.value = old;
    input.setAttribute("aria-label", AH.t("node_graph", "title", "Title"));
    if (span.parentNode) { span.parentNode.replaceChild(input, span); }
    input.focus();
    input.select();
    let done = false;
    const restore = (v: string): void => {
      if (done) { return; }
      done = true;
      span.textContent = v;
      if (input.parentNode) { input.parentNode.replaceChild(span, input); }
      this.#vp.focus();
    };
    input.addEventListener("pointerdown", (e) => { e.stopPropagation(); });
    input.addEventListener("blur", () => {
      const v = input.value.trim();
      restore(v || old);
      if (v && v !== old) { commit(v); }
    });
    input.addEventListener("keydown", (e) => {
      e.stopPropagation();
      if (e.key === "Enter") { e.preventDefault(); input.blur(); }
      if (e.key === "Escape") { e.preventDefault(); restore(old); }
    });
  }

  // ------------------------------------------------------------------
  // Wiring
  // ------------------------------------------------------------------

  private dragDelta(s: DragAt): Point {
    const sn = this.#opts.snap;
    return [snap(s.dx / this.#cam.z, sn), snap(s.dy / this.#cam.z, sn)];
  }

  private wireNodes(): void {
    this.on("pointerdown", ".ah-node-graph-collapse, .ah-node-graph-widget, .ah-node-graph-custom, " +
            ".ah-node-graph-tool, .ah-node-graph-group-delete", stop);
    this.on("click", ".ah-node-graph-collapse", (e, btn) => {
      e.stopPropagation();
      const id = nodeIdOf(btn);
      this.mutate("collapse", (g) => {
        const n = findNode(g, id);
        if (!n) { return false; }
        n.collapsed = !n.collapsed;
        if (!n.collapsed) { delete n.collapsed; }
        return undefined;
      });
    });
    // Tabbing onto a card's collapse button selects the card, so the
    // keyboard reaches nodes (Delete, arrows, Ctrl+D, ...).
    this.on("focusin", ".ah-node-graph-collapse", (_e, btn) => {
      const id = nodeIdOf(btn) || "";
      if (this.#sel.length !== 1 || this.#sel[0] !== id) { this.select([id], "replace"); }
    });
    this.on("dblclick", ".ah-node-graph-node-title", (e, span) => {
      if (this.#opts.readOnly) { return; }
      e.stopPropagation();
      const id = nodeIdOf(span);
      this.editTitle(span, (v) => {
        this.mutate("rename", (g) => { const n = findNode(g, id); if (n) { n.title = v; } });
      });
    });

    // resize handle
    this.on("pointerdown", ".ah-node-graph-node-resize", (e, handle) => {
      e.stopPropagation();
      if (e.button !== 0 || this.#opts.readOnly) { return; }
      const card = handle.closest<HTMLElement>(".ah-node-graph-node");
      const id = card ? card.getAttribute("data-node-id") || "" : "", n = findNode(this.#g, id);
      if (!card || !n) { return; }
      const w0 = nodeWidth(n), h0 = n.height || card.offsetHeight || nodeHeight(n);
      const hMin = Math.max(MIN_H, TITLE_H + PAD_TOP + SLOT_H * Math.max(n.inputs.length, n.outputs.length) + PAD_BOTTOM);
      const size = (s: DragAt): { id: string; w: number; h: number } =>
        ({ id, w: Math.max(MIN_W, Math.round(w0 + s.dx / this.#cam.z)),
           h: Math.max(hMin, Math.round(h0 + s.dy / this.#cam.z)) });
      this.beginDrag(e, {
        move: (s) => { this.#nodeDrag = { resize: size(s) }; this.applyNodeDrag(); },
        end: (s) => {
          const z = size(s);
          this.#nodeDrag = null;
          this.#sigs.set(card, null);
          if (!this.mutate("resize", (g) => {
            const m = findNode(g, id);
            if (m) { m.width = z.w; m.height = z.h; }
          })) { this.renderAll(); }
        },
        cancel: () => { this.#nodeDrag = null; this.#sigs.set(card, null); this.renderAll(); }
      });
    });

    // slots: drag a link out
    this.on("pointerdown", ".ah-node-graph-slot-hit", (e, hit) => {
      e.stopPropagation();
      if (e.button !== 0 || this.#opts.readOnly) { return; }
      const slotEl = hit.closest(".ah-node-graph-slot");
      if (!slotEl) { return; }
      const origin = originOf(this.#g, nodeIdOf(hit) || "",
                              slotEl.getAttribute("data-kind") === "input" ? "input" : "output",
                              parseInt(slotEl.getAttribute("data-slot-index") || "", 10));
      const compat = compatibleMap(this.#g, origin, this.#opts.allowCycles);
      const slot = slotOf(this.#g, origin.node, origin.kind, origin.index);
      const p0 = point(e);
      this.#vp.focus();
      const drag: LinkDrag = this.#linkDrag = { origin, type: slot && slot.type, compatible: compat,
                                                point: this.clientToGraph(p0[0], p0[1]), candidate: null };
      this.applyLinkDrag();
      this.beginDrag(e, {
        move: (s) => {
          const p = this.clientToGraph(s.x, s.y);
          drag.point = p;
          drag.candidate = this.nearestCandidate(compat, origin, p, SNAP_RADIUS / this.#cam.z);
          this.applyLinkDrag();
        },
        end: (s) => {
          const cand = this.#linkDrag && this.#linkDrag.candidate;
          this.#linkDrag = null;
          if (!cand && Math.hypot(s.dx, s.dy) < 3) {
            this.renderLinks();                  // a click on a slot is not a drop
          } else if (cand) {
            this.dropLink(origin, pairFor(origin, cand));
          } else {
            const gp = this.clientToGraph(s.x, s.y);
            if (this.#library.length) {
              this.renderLinks();
              this.applyLinkDrag();
              this.searchMenu(this.clientToHost(s.x, s.y), gp, origin);
            } else {
              this.element.setAttribute("data-origin", origin.node + ":" + origin.kind + ":" + origin.index);
              this.element.setAttribute("data-point", Math.round(gp[0]) + "," + Math.round(gp[1]));
              if (!origin.detach || !this.dropLink(origin, null)) { this.renderLinks(); }
              this.fire<NodeGraphLinkDrop>("ah:link-drop", { origin, point: gp });
            }
          }
        },
        cancel: () => { this.#linkDrag = null; this.renderLinks(); },
        click: () => { this.#linkDrag = null; this.renderLinks(); }
      });
    });

    // the card itself: select and move
    this.on("pointerdown", ".ah-node-graph-node", (e, card) => {
      if (e.button !== 0 || this.#hand) { return; }
      e.stopPropagation();
      const id = card.getAttribute("data-node-id") || "";
      const additive = e.shiftKey || e.ctrlKey || e.metaKey;
      const sel = this.#sel;
      const moving = additive ? sel.concat(sel.indexOf(id) < 0 ? [id] : [])
        : (sel.indexOf(id) >= 0 ? sel.slice() : [id]);
      this.#vp.focus({ preventScroll: true });
      if (additive) { this.select([id], "add"); }
      else if (this.#sel.indexOf(id) < 0) { this.select([id], "replace"); }
      this.bringToFront(moving);
      if (this.#opts.readOnly) { return; }
      const set: Record<string, boolean> = {};
      moving.forEach((x) => { set[x] = true; });
      this.beginDrag(e, {
        threshold: 3,
        move: (s) => {
          const d = this.dragDelta(s);
          this.#nodeDrag = { ids: set, dx: d[0], dy: d[1] };
          this.applyNodeDrag();
        },
        end: (s) => {
          const d = this.dragDelta(s);
          this.#nodeDrag = null;
          if (!(d[0] || d[1]) || !this.moveNodes(moving, d[0], d[1])) { this.applyNodeDrag(); }
        },
        cancel: () => { this.#nodeDrag = null; this.applyNodeDrag(); },
        click: () => {
          // a plain click on a selected card among several selects it alone
          if (!additive && this.#sel.length > 1) { this.select([id], "replace"); }
        }
      });
    });

    this.on("contextmenu", ".ah-node-graph-node", (e, card) => {
      e.preventDefault();
      e.stopPropagation();
      const p = point(e);
      this.nodeMenu(card.getAttribute("data-node-id") || "", this.clientToHost(p[0], p[1]));
    });
  }

  private moveNodes(xs: string[], dx: number, dy: number): boolean {
    return this.mutate("move", (g) => {
      xs.forEach((id) => {
        const n = findNode(g, id);
        if (n) { n.pos = [Math.round(n.pos[0] + dx), Math.round(n.pos[1] + dy)]; }
      });
    });
  }

  private dropLink(origin: LinkOrigin, pair: [SlotRef, SlotRef] | null): boolean {
    // the same link again (or the one picked up put back) changes nothing
    if (pair && this.#g.links.some((l) =>
      l.source[0] === pair[0][0] && l.source[1] === pair[0][1] &&
      l.target[0] === pair[1][0] && l.target[1] === pair[1][1])) {
      this.renderLinks();
      return false;
    }
    const ok = this.mutate(pair ? "connect" : "disconnect", (g) => {
      if (origin.detach) { g.links = g.links.filter((x) => x.id !== origin.detach); }
      if (pair) { connect(g, pair[0], pair[1], this.#opts.allowCycles); }
    });
    if (!ok) { this.renderLinks(); }
    return ok;
  }

  private nodeMenu(id: string, at: Point): void {
    const n = findNode(this.#g, id);
    if (!n) { return; }
    const xs = this.#sel.indexOf(id) >= 0 ? this.#sel.slice() : [id];
    if (this.#sel.indexOf(id) < 0) { this.select([id], "replace"); }
    const many = xs.length > 1, ro = this.#opts.readOnly;
    this.contextMenu(at, [
      ro ? null : { label: n.collapsed ? AH.t("node_graph", "expand", "Expand") : AH.t("node_graph", "collapse", "Collapse"),
                    run: () => {
                      const card = this.cardOf(id);
                      const btn = card && card.querySelector<HTMLElement>(".ah-node-graph-collapse");
                      if (btn) { btn.click(); }
                    } },
      ro ? null : { label: many ? AH.t("node_graph", "duplicate_nodes", "Duplicate {0} nodes", [xs.length])
                                : AH.t("node_graph", "duplicate_node", "Duplicate node"), hint: "Ctrl+D",
                    run: () => { this.duplicate(xs); } },
      { label: many ? AH.t("node_graph", "copy_nodes", "Copy {0} nodes", [xs.length])
                    : AH.t("node_graph", "copy_node", "Copy node"), hint: "Ctrl+C",
        run: () => { this.copy(xs); } },
      ro ? null : { label: many ? AH.t("node_graph", "delete_nodes", "Delete {0} nodes", [xs.length])
                                : AH.t("node_graph", "delete_node", "Delete node"), hint: "Del", danger: true,
                    run: () => { this.mutate("remove", (g) => { removeNodes(g, xs); }); } }
    ]);
  }

  private wireLinks(): void {
    this.on("pointerdown", ".ah-node-graph-link-hit", (e, hit) => {
      e.stopPropagation();
      const id = linkIdOf(hit) || "";
      this.#vp.focus({ preventScroll: true });
      const lsel = this.#lsel;
      this.setSelection([], e.shiftKey ? lsel.concat(lsel.indexOf(id) < 0 ? [id] : []) : [id]);
    });
    // double click adds a reroute point
    this.on("dblclick", ".ah-node-graph-link-hit", (e, hit) => {
      e.stopPropagation();
      if (this.#opts.readOnly) { return; }
      const p = point(e);
      this.addWaypoint(linkIdOf(hit), this.clientToGraph(p[0], p[1]));
    });
    this.on("contextmenu", ".ah-node-graph-link-hit", (e, hit) => {
      e.preventDefault();
      e.stopPropagation();
      const id = linkIdOf(hit) || "", p = point(e), gp = this.clientToGraph(p[0], p[1]);
      this.setSelection([], [id]);
      if (this.#opts.readOnly) { return; }
      this.contextMenu(this.clientToHost(p[0], p[1]), [
        { label: AH.t("node_graph", "add_reroute", "Add reroute point here"), run: () => { this.addWaypoint(id, gp); } },
        { label: AH.t("node_graph", "delete_link", "Delete link"), hint: "Del", danger: true,
          run: () => { this.mutate("disconnect", (g) => {
            g.links = g.links.filter((l) => l.id !== id);
          }); } }
      ]);
    });
    // reroute points: drag to move, double click to remove
    this.on("pointerdown", ".ah-node-graph-waypoint", (e, wp) => {
      e.stopPropagation();
      if (e.button !== 0 || this.#opts.readOnly) { return; }
      const id = linkIdOf(wp) || "", i = parseInt(wp.getAttribute("data-index") || "", 10);
      this.beginDrag(e, {
        threshold: 2,
        move: (s) => {
          const p = this.clientToGraph(s.x, s.y);
          this.#wpDrag = { link: id, index: i, point: [Math.round(p[0]), Math.round(p[1])] };
          this.updateLinkPaths();
        },
        end: () => {
          const w = this.#wpDrag;
          this.#wpDrag = null;
          if (!w || !this.mutate("reroute", (g) => {
            const l = findLink(g, id);
            if (!l || !l.points || !l.points[i]) { return false; }
            l.points[i] = w.point;
            return undefined;
          })) { this.updateLinkPaths(); }
        },
        cancel: () => { this.#wpDrag = null; this.updateLinkPaths(); }
      });
    });
    this.on("dblclick", ".ah-node-graph-waypoint", (e, wp) => {
      e.stopPropagation();
      const id = linkIdOf(wp), i = parseInt(wp.getAttribute("data-index") || "", 10);
      this.mutate("reroute", (g) => {
        const l = findLink(g, id);
        if (!l || !l.points) { return false; }
        l.points.splice(i, 1);
        if (!l.points.length) { delete l.points; }
        return undefined;
      });
    });
  }

  private addWaypoint(id: string | null, p: Point): void {
    const l = findLink(this.#g, id);
    if (!l) { return; }
    const pts = this.linkPoints(l, this.shownMap(false), false);
    const q: Point = [Math.round(p[0]), Math.round(p[1])];
    const i = pts ? nearestSegment(pts, q) : -1;
    if (i < 0) { return; }
    this.mutate("reroute", (g) => {
      const m = findLink(g, id);
      if (!m) { return false; }
      m.points = (m.points || []).slice();
      m.points.splice(i, 0, q);
      return undefined;
    });
  }

  private wireGroups(): void {
    this.on("pointerdown", ".ah-node-graph-group-header", (e, header) => {
      if (e.button !== 0 || this.#opts.readOnly || this.#hand) { return; }
      e.stopPropagation();
      const id = groupIdOf(header), gr = findGroup(this.#g, id);
      if (!id || !gr) { return; }
      const b = { x: gr.bounds[0], y: gr.bounds[1], w: gr.bounds[2], h: gr.bounds[3] };
      // the nodes it holds are fixed when the drag starts
      const inside: Record<string, boolean> = {}, held: string[] = [];
      this.#g.nodes.forEach((n) => {
        const s = this.shown(n, false);
        if (contains(b, s)) { inside[n.id] = true; held.push(n.id); }
      });
      this.#vp.focus({ preventScroll: true });
      this.beginDrag(e, {
        threshold: 3,
        move: (s) => {
          const d = this.dragDelta(s);
          this.#nodeDrag = { ids: inside, group: id, dx: d[0], dy: d[1] };
          this.applyNodeDrag();
        },
        end: (s) => {
          const d = this.dragDelta(s);
          this.#nodeDrag = null;
          if (!(d[0] || d[1]) || !this.mutate("group-move", (g) => {
            const m = findGroup(g, id);
            if (!m) { return false; }
            m.bounds = [Math.round(m.bounds[0] + d[0]), Math.round(m.bounds[1] + d[1]), m.bounds[2], m.bounds[3]];
            held.forEach((nid) => {
              const n = findNode(g, nid);
              if (n) { n.pos = [Math.round(n.pos[0] + d[0]), Math.round(n.pos[1] + d[1])]; }
            });
            return undefined;
          })) { this.applyNodeDrag(); }
        },
        cancel: () => { this.#nodeDrag = null; this.applyNodeDrag(); }
      });
    });
    this.on("dblclick", ".ah-node-graph-group-title", (e, span) => {
      if (this.#opts.readOnly) { return; }
      e.stopPropagation();
      const id = groupIdOf(span);
      this.editTitle(span, (v) => {
        this.mutate("group-rename", (g) => { const gr = findGroup(g, id); if (gr) { gr.title = v; } });
      });
    });
    this.on("click", ".ah-node-graph-group-delete", (e, btn) => {
      e.stopPropagation();
      this.removeGroup(groupIdOf(btn));
    });
    this.on("pointerdown", ".ah-node-graph-group-resize", (e, handle) => {
      e.stopPropagation();
      if (e.button !== 0 || this.#opts.readOnly) { return; }
      const id = groupIdOf(handle), gr = findGroup(this.#g, id);
      if (!id || !gr) { return; }
      const b0 = gr.bounds;
      const bounds = (s: DragAt): [number, number, number, number] =>
        [b0[0], b0[1], Math.max(120, Math.round(b0[2] + s.dx / this.#cam.z)),
         Math.max(80, Math.round(b0[3] + s.dy / this.#cam.z))];
      this.beginDrag(e, {
        move: (s) => { this.#nodeDrag = { groupResize: { id, bounds: bounds(s) } }; this.applyNodeDrag(); },
        end: (s) => {
          const b = bounds(s);
          this.#nodeDrag = null;
          if (!this.mutate("group-resize", (g) => {
            const m = findGroup(g, id);
            if (m) { m.bounds = b; }
          })) { this.applyNodeDrag(); }
        },
        cancel: () => { this.#nodeDrag = null; this.applyNodeDrag(); }
      });
    });
    this.on("contextmenu", ".ah-node-graph-group", (e, frame) => {
      e.preventDefault();
      e.stopPropagation();
      if (this.#opts.readOnly) { return; }
      const id = groupIdOf(frame), p = point(e);
      this.contextMenu(this.clientToHost(p[0], p[1]), [
        { label: AH.t("node_graph", "delete_group_frame", "Delete group frame"),
          hint: AH.t("node_graph", "frame_only", "frame only"),
          danger: true,
          run: () => { this.removeGroup(id); } }
      ]);
    });
  }

  private removeGroup(id: string | null): void {
    this.mutate("group-remove", (g) => {
      g.groups = g.groups.filter((x) => x.id !== id);
    });
  }

  private touches(): Point[] { return Array.from(this.#pointers.values()); }

  private wireViewport(): void {
    const vp = this.#vp;

    // non-passive, or the page scrolls too
    this.listen(vp, "wheel", (e) => {
      e.preventDefault();
      const s = this.clientToScreen(e.clientX, e.clientY);
      this.zoomBy(wheelFactor(e), s[0], s[1]);
    }, { passive: false });

    this.on("pointerdown", ".ah-node-graph-viewport", (e) => {
      if (e.pointerType === "touch") { this.#pointers.set(e.pointerId, [e.clientX, e.clientY]); }
      const tp = this.touches();
      const t0 = tp[0], t1 = tp[1];
      if (tp.length === 2 && t0 && t1) {
        if (this.#vpCancel) { this.#vpCancel(); this.#vpCancel = null; }
        this.#pinch = { dist: Math.hypot(t1[0] - t0[0], t1[1] - t0[1]),
                        mid: [(t0[0] + t1[0]) / 2, (t0[1] + t1[1]) / 2], cam: this.#cam };
        return;
      }
      if ((e.button !== 0 && e.button !== 1) || this.#pinch) { return; }
      if (e.button === 1) { e.preventDefault(); }
      vp.focus({ preventScroll: true });
      const panning = e.button === 1 || this.#space || this.#hand || e.pointerType === "touch";
      if (panning) {
        const start = this.#cam;
        vp.setAttribute("data-panning", "true");
        const done = (): void => { vp.removeAttribute("data-panning"); this.#vpCancel = null; };
        this.#vpCancel = this.beginDrag(e, {
          move: (s) => { this.setCamera(pan(start, s.dx, s.dy)); },
          end: done,
          cancel: done,
          click: done
        });
        return;
      }
      const additive = e.shiftKey || e.ctrlKey || e.metaKey;
      const origin = this.clientToGraph(e.clientX, e.clientY);
      if (!additive) { this.setSelection([], []); }
      this.#vpCancel = this.beginDrag(e, {
        threshold: 3,
        move: (s) => { this.#marquee = [origin, this.clientToGraph(s.x, s.y)]; this.applyMarquee(); },
        end: () => {
          if (this.#marquee) {
            const r = normRect(this.#marquee[0], this.#marquee[1]);
            const hit = this.#g.nodes.filter((n) => intersects(r, this.shown(n, false))).map((n) => n.id);
            this.select(hit, additive ? "add" : "replace");
          }
          this.#marquee = null;
          this.applyMarquee();
          this.#vpCancel = null;
        },
        cancel: () => { this.#marquee = null; this.applyMarquee(); this.#vpCancel = null; }
      });
    });

    this.on("pointermove", ".ah-node-graph-viewport", (e) => {
      if (!this.#pointers.has(e.pointerId)) { return; }
      this.#pointers.set(e.pointerId, [e.clientX, e.clientY]);
      const tp = this.touches(), pinch = this.#pinch;
      const t0 = tp[0], t1 = tp[1];
      if (!pinch || tp.length !== 2 || !t0 || !t1) { return; }
      const d = Math.hypot(t1[0] - t0[0], t1[1] - t0[1]);
      const m: Point = [(t0[0] + t1[0]) / 2, (t0[1] + t1[1]) / 2];
      const a = this.clientToScreen(pinch.mid[0], pinch.mid[1]);
      const z = zoomAt(pinch.cam, d / Math.max(1, pinch.dist), a[0], a[1]);
      this.setCamera(pan(z, m[0] - pinch.mid[0], m[1] - pinch.mid[1]));
    });

    // fingers lifted outside the component still count
    const lift = (e: PointerEvent): void => {
      this.#pointers.delete(e.pointerId);
      if (this.touches().length < 2) { this.#pinch = null; }
    };
    this.listen(document, "pointerup", lift);
    this.listen(document, "pointercancel", lift);

    this.on("contextmenu", ".ah-node-graph-viewport", (e) => {
      e.preventDefault();
      if (this.#opts.readOnly) { return; }
      const p = point(e);
      this.searchMenu(this.clientToHost(p[0], p[1]), this.clientToGraph(p[0], p[1]), null);
    });

    this.on("keydown", ".ah-node-graph-viewport", (e) => {
      const target = e.target as Element;   // a keydown's target is an element
      if (isEditable(target)) { return; }
      const k = e.key, mod = e.ctrlKey || e.metaKey, lower = k.length === 1 ? k.toLowerCase() : k;
      const ro = this.#opts.readOnly, onButton = target.tagName === "BUTTON";
      if (k === " " && !onButton) {
        e.preventDefault();
        this.#space = true;
        vp.setAttribute("data-hand", "true");
      } else if (mod && lower === "z") {
        e.preventDefault();
        if (e.shiftKey) { this.redo(); } else { this.undo(); }
      } else if (mod && lower === "y") {
        e.preventDefault(); this.redo();
      } else if (mod && lower === "a") {
        e.preventDefault(); this.select(this.#g.nodes.map((n) => n.id), "replace");
      } else if (mod && lower === "c") {
        e.preventDefault(); this.copy(this.#sel);
      } else if (mod && lower === "v") {
        e.preventDefault(); if (!ro) { this.paste(this.#clip, "paste"); }
      } else if (mod && lower === "x") {
        e.preventDefault();
        const cut = this.#sel.slice();
        this.copy(cut);
        if (!ro && cut.length) { this.mutate("remove", (g) => { removeNodes(g, cut); }); }
      } else if (mod && lower === "d") {
        e.preventDefault(); if (!ro) { this.duplicate(this.#sel); }
      } else if (k === "Delete" || k === "Backspace") {
        if (!ro) { e.preventDefault(); this.deleteSelection(); }
      } else if (k === "Escape") {
        this.setSelection([], []);
      } else if (/^Arrow/.test(k)) {
        e.preventDefault();
        const step = e.shiftKey ? 50 : (this.#opts.snap || 10);
        const dx = k === "ArrowLeft" ? -step : k === "ArrowRight" ? step : 0;
        const dy = k === "ArrowUp" ? -step : k === "ArrowDown" ? step : 0;
        if (this.#sel.length && !ro) { this.moveNodes(this.#sel.slice(), dx, dy); }
        else {
          const by = e.shiftKey ? 160 : 40;
          this.setCamera(pan(this.#cam, -Math.sign(dx) * by, -Math.sign(dy) * by));
        }
      } else if (!mod && (k === "+" || k === "=")) {
        this.zoomBy(1.25);
      } else if (!mod && k === "-") {
        this.zoomBy(0.8);
      } else if (k === "ContextMenu" || (e.shiftKey && k === "F10")) {
        e.preventDefault();
        const r = this.#vp.getBoundingClientRect();
        const at: Point = [r.width / 2, r.height / 2];
        if (this.#sel.length) {
          const n = findNode(this.#g, this.#sel[this.#sel.length - 1] || null);
          if (!n) { return; }
          const c = this.#cam;
          const sc = [(n.pos[0] + c.x) * c.z, (n.pos[1] + c.y) * c.z] as Point;
          this.nodeMenu(n.id, [sc[0] + 20, sc[1] + 20]);
        } else if (!ro) {
          this.searchMenu(at, screenToGraph(this.#cam, at[0], at[1]), null);
        }
      }
    });
    this.on("keyup", ".ah-node-graph-viewport", (e) => {
      if (e.key === " ") {
        this.#space = false;
        if (!this.#hand) { vp.removeAttribute("data-hand"); }
      }
    });
  }

  private wireToolbar(): void {
    this.on("click", ".ah-node-graph-tool", (e, tool) => {
      e.stopPropagation();
      switch (tool.getAttribute("data-action")) {
        case "zoom-in": this.zoomBy(1.25); break;
        case "zoom-out": this.zoomBy(0.8); break;
        case "hand":
          this.#hand = !this.#hand;
          if (this.#hand) { this.#vp.setAttribute("data-hand", "true"); tool.setAttribute("data-active", "true"); }
          else { this.#vp.removeAttribute("data-hand"); tool.removeAttribute("data-active"); }
          tool.setAttribute("aria-pressed", String(this.#hand));
          break;
        case "fit": this.fit(); break;
        case "undo": this.undo(); break;
        case "redo": this.redo(); break;
        case "delete": this.deleteSelection(); break;
      }
    });
    // minimap: click to centre the view there
    this.on("pointerdown", ".ah-node-graph-minimap", (e, mini) => {
      e.stopPropagation();
      const view = this.viewBox();
      const t = miniTransform(contentBounds(this.#g.nodes.map((n) => this.shown(n, false))), view);
      const r = mini.getBoundingClientRect(), p = point(e);
      const gx = t.ox + (p[0] - r.left) / t.scale, gy = t.oy + (p[1] - r.top) / t.scale;
      const z = this.#cam.z;
      this.setCamera({ z, x: view.w / 2 - gx, y: view.h / 2 - gy });
    });
  }
}

AH.register("node-graph", NodeGraphController);
