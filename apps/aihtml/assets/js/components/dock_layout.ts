/* The dock_layout behaviour (designs/04-components.md), ported from
 * sigil's layout/dock_layout (+ common, dock, pin, menu, serialize):
 * splits, tab groups and documents: tabs dragged onto the dock targets (a
 * side of a group, its centre, an edge of the layout) or out to float;
 * auto hide at an edge; splitbars; a context menu (right click, the
 * context menu key or Shift+F10 on a tab or a float's title bar).
 *
 * The arrangement is the value: JSON in data-ah-value (and the hidden
 * input), rewritten after every change, and `change' on the root when it
 * changed (user actions and the rearranging methods). Sizes are flex
 * weights (style "flex: W 1 0px"), written as percentages of the parent.
 * Elements are only ever moved, never rebuilt, so panel contents keep
 * their state; new tab groups, float windows and the menu come from the
 * shared templates (AH.tpl.dock_layout_*). The drag tracking is shared
 * with docking.ts (_lib_dock.ts).
 *
 * Events: "ah:panel-close" on the root with the closed panel ids (a
 * multi-value) in data-panels, and as the detail (PanelCloseEvent).
 */
import AH from "../core.ts";
import { inside, parse, commit, fireData, kids, parseHTML, drop, DockIds, DockDrag } from "./_lib_dock.ts";
import { join } from "./_lib_values.ts";
import "virtual:ah-tpl/dock_layout_float";
import "virtual:ah-tpl/dock_layout_group";
import "virtual:ah-tpl/dock_layout_menu";

/** Detail of ah:panel-close: the closed panel ids, joined (_lib_values). */
export type PanelCloseEvent = string;

const CROSS = 128;           // the dock cross: 3 * 40 + 2 * 4
const SHOW_DELAY = 200, HIDE_DELAY = 400;   // auto hide preview on hover

// ------------------------------------------------------------------
// value shapes (the server's aihtml_dock_layout)
// ------------------------------------------------------------------

interface GroupValue {
  type: "tabs" | "documents";
  id: string | null;
  size?: number;
  items: (string | null)[];
  active: string | null;
  pin?: boolean;
  close: boolean;
}
interface SplitValue {
  type: "split";
  orientation: "horizontal" | "vertical";
  size: number;
  items: NodeValue[];
}
interface PanelValue { type: "panel"; size: number; item: string | null; }
type NodeValue = GroupValue | SplitValue | PanelValue;
interface FloatValue {
  type: "float"; id: string | null; items: (string | null)[]; active: string | null;
  x: number; y: number; width: number; height: number;
}
interface AutoHideValue {
  type: "autohide"; id: string | null; edge: string | null; size: number;
  items: (string | null)[]; active: string | null; pin?: boolean; close: boolean;
}
type LayoutValue = NodeValue | FloatValue | AutoHideValue;

/** The menu labels (data-ah-labels). */
interface Labels { auto_hide: string; float: string; dock: string; close: string; }

function parseLabels(v: unknown): Labels {
  const out: Labels = { auto_hide: "Auto Hide", float: "Float", dock: "Dock", close: "Close" };
  if (v && typeof v === "object") {
    const o = v as Record<string, unknown>;
    (["auto_hide", "float", "dock", "close"] as const).forEach((k) => {
      const s = o[k];
      if (typeof s === "string") { out[k] = s; }
    });
  }
  return out;
}

/** Where openPanel puts a panel. */
interface OpenWhere { in?: string; float?: boolean; x?: number; y?: number; edge?: string; }

function num(v: unknown): number | undefined {
  if (typeof v === "number") { return v; }
  if (typeof v === "string" && v.trim() !== "" && !isNaN(Number(v))) { return Number(v); }
  return undefined;
}

function parseWhere(v: unknown): OpenWhere {
  if (!v || typeof v !== "object") { return {}; }
  const o = v as Record<string, unknown>;
  const w: OpenWhere = {};
  if ((typeof o["in"] === "string" || typeof o["in"] === "number") && o["in"] !== "") { w.in = String(o["in"]); }
  if (o.float) { w.float = true; }
  w.x = num(o.x);
  w.y = num(o.y);
  if (typeof o.edge === "string" && o.edge) { w.edge = o.edge; }
  return w;
}

/** An auto hidden group's old place in the layout. */
interface Memo { parent: HTMLElement; index: number; weight: number; }
/** The open context menu. */
interface MenuState {
  g: HTMLElement; win: HTMLElement | null; li: HTMLElement | null;
  x: number; y: number; focus: Element | null;
}
interface MenuItem { action?: string; label?: string; disabled?: boolean; divider: boolean; }
/** A tab taken out of its group. */
interface TabItem { li: HTMLElement; panel: HTMLElement | undefined; from: HTMLElement; }
interface GroupFlags { pin: boolean; close: boolean; active?: HTMLElement; }
/** A dock zone under the pointer. */
interface Hit { zone: string; target: HTMLElement | null; el: HTMLElement; }
/** The two neighbours of a splitbar at the start of a resize. */
interface PairBase { horiz: boolean; sp: number; sn: number; wp: number; wn: number; line?: number; }

function round2(x: number): number { return Math.round(x * 100) / 100; }

function div(cls: string, attrs?: Record<string, string>): HTMLDivElement {
  const d = document.createElement("div");
  d.className = cls;
  Object.keys(attrs || {}).forEach((k) => { d.setAttribute(k, (attrs as Record<string, string>)[k]); });
  return d;
}

function px(v: number | string): string { return typeof v === "number" ? v + "px" : v; }
function css(node: HTMLElement, props: Record<string, number | string>): void {
  Object.keys(props).forEach((k) => { node.style.setProperty(k, px(props[k])); });
}

// ------------------------------------------------------------------
// structure
// ------------------------------------------------------------------

// The server renders .ah-dl-middle > .ah-dl-inner in every layout.
function inner(el: Element): HTMLElement {
  return el.querySelector(":scope > .ah-dl-middle > .ah-dl-inner") as HTMLElement;
}
function floats(el: Element): HTMLElement[] { return kids(el, ".ah-dl-float-container"); }
function floatWins(el: Element): HTMLElement[] {
  const out: HTMLElement[] = [];
  floats(el).forEach((f) => { out.push(...kids(f, ".ah-dl-float-window")); });
  return out;
}
function slots(el: Element): HTMLElement[] {
  return Array.from(el.querySelectorAll<HTMLElement>(":scope > .ah-dl-autohide-preview-slot, " +
                                                     ":scope > .ah-dl-middle > .ah-dl-autohide-preview-slot"));
}
function strips(el: Element): HTMLElement[] {
  return Array.from(el.querySelectorAll<HTMLElement>(":scope > .ah-dl-autohide-strip, " +
                                                     ":scope > .ah-dl-middle > .ah-dl-autohide-strip"));
}
function slotGroups(el: Element): HTMLElement[] {
  const out: HTMLElement[] = [];
  slots(el).forEach((s) => { out.push(...kids(s, ".ah-dl-tabbed")); });
  return out;
}
// The server renders a slot and a strip for each of the four edges.
function slotOf(el: Element, edge: string): HTMLElement {
  return slots(el).find((s) => s.matches(".ah-dl-autohide-preview-slot-" + edge)) as HTMLElement;
}
function stripOf(el: Element, edge: string): HTMLElement {
  return strips(el).find((s) => s.matches(".ah-dl-autohide-strip-" + edge)) as HTMLElement;
}
function layoutGroups(el: Element): HTMLElement[] {
  return Array.from(inner(el).querySelectorAll<HTMLElement>(".ah-dl-tabbed"));
}

function isBar(n: Element): boolean { return n.classList.contains("ah-dl-splitbar"); }
function real(c: Element): HTMLElement[] { return kids(c).filter((n) => !isBar(n)); }
function isContainer(c: Element | null | undefined): c is HTMLElement {
  return !!c && !!c.matches && c.matches(".ah-dl-group, .ah-dl-inner");
}
function isHoriz(c: Element): boolean {
  return c.classList.contains("ah-dl-inner") ? !c.classList.contains("ah-dl-vertical")
                                             : c.classList.contains("ah-dl-horizontal");
}
function weight(n: HTMLElement): number {
  const g = parseFloat(n.style.flexGrow);
  return isNaN(g) || g <= 0 ? 1 : g;
}
function setWeight(n: HTMLElement, w: number): void { n.style.flex = round2(w) + " 1 0px"; }

function header(g: Element): HTMLElement | null {
  return g.querySelector<HTMLElement>(":scope > .ah-dl-tabs > .ah-tabs-header");
}
function tabsOf(g: Element): HTMLElement[] { return kids(header(g), ".ah-tabs-item"); }
// Every tab group has its content box (server render and template).
function content(g: Element): HTMLElement {
  return g.querySelector(":scope > .ah-dl-tabs > .ah-tabs-content") as HTMLElement;
}
function panelsOf(g: Element): HTMLElement[] { return kids(content(g), ".ah-tabs-panel"); }
function pid(li: Element | undefined): string | null { return li ? li.getAttribute("data-panel-id") : null; }
function panelFor(g: Element, li: Element): HTMLElement | undefined {
  return panelsOf(g)[tabsOf(g).indexOf(li as HTMLElement)];
}
function activeTab(g: Element): HTMLElement | undefined {
  const t = tabsOf(g);
  return t.find((li) => li.classList.contains("ah-tabs-item-selected")) || t[0];
}
// Tabs, their buttons and panels always sit inside a tab group.
function groupOf(node: Element): HTMLElement { return node.closest(".ah-dl-tabbed") as HTMLElement; }
function winOf(node: Element): HTMLElement | null { return node.closest<HTMLElement>(".ah-dl-float-window"); }
// A float window always holds its tab group.
function winGroup(win: Element): HTMLElement { return win.querySelector(".ah-dl-tabbed") as HTMLElement; }
function inSlot(node: Element): boolean { return !!node.closest(".ah-dl-autohide-preview-slot"); }
function inLayout(el: Element, node: Element): boolean { const i = inner(el); return i !== node && i.contains(node); }

function flag(g: Element, name: string): boolean { return g.getAttribute("data-allow-" + name) === "true"; }
function opt(el: Element, name: string): boolean { return el.getAttribute("data-ah-" + name) !== "false"; }

function select(g: Element, li: HTMLElement | undefined): void {
  const t = tabsOf(g), i = li ? t.indexOf(li) : -1;
  if (!li || i < 0) { return; }
  t.forEach((n) => {
    n.classList.remove("ah-tabs-item-selected");
    n.setAttribute("aria-selected", "false");
    n.setAttribute("tabindex", "-1");
  });
  li.classList.add("ah-tabs-item-selected");
  li.setAttribute("aria-selected", "true");
  li.setAttribute("tabindex", "0");
  panelsOf(g).forEach((p, j) => {
    p.classList.toggle("ah-tabs-panel-active", j === i);
    p.hidden = j !== i;
  });
  const win = winOf(g);
  if (win) {
    const text = li.textContent || "";
    win.setAttribute("aria-label", text);
    const title = win.querySelector(".ah-dl-float-title");
    if (title) { title.textContent = text; }
  }
}

function newContainer(horiz: boolean): HTMLDivElement {
  return div("ah-dl-group " + (horiz ? "ah-dl-horizontal" : "ah-dl-vertical"));
}

// Take a tab and its panel out of their group (selecting a neighbour).
function extract(li: HTMLElement): TabItem {
  const g = groupOf(li), panel = panelFor(g, li);
  const i = tabsOf(g).indexOf(li), was = li.classList.contains("ah-tabs-item-selected");
  li.remove();
  if (panel) { panel.remove(); }
  const t = tabsOf(g);
  if (was && t.length) { select(g, t[Math.min(i, t.length - 1)]); }
  return { li: li, panel: panel, from: g };
}

function insertTab(g: HTMLElement, it: TabItem, before?: HTMLElement | null): void {
  const h = header(g) as HTMLElement, ref = before || kids(h, ".ah-dl-tabbed-actions")[0] || null;
  h.insertBefore(it.li, ref);
  const refPanel = before ? panelFor(g, before) : null;
  if (it.panel) { content(g).insertBefore(it.panel, refPanel || null); }
  select(g, it.li);
}

function splitbar(el: Element): HTMLDivElement {
  const bar = div("ah-dl-splitbar", { role: "separator" });
  if (opt(el, "resizable")) { bar.setAttribute("tabindex", "0"); }
  return bar;
}

function canPin(g: HTMLElement): boolean {
  const p = g.parentElement;
  if (!isContainer(p) || g.getAttribute("data-document") === "true") { return false; }
  const ks = real(p), i = ks.indexOf(g);
  return ks.length >= 2 && (i === 0 || i === ks.length - 1);
}

function refresh(el: Element): void {
  const inn = inner(el);
  inn.querySelectorAll(".ah-dl-splitbar").forEach((bar) => {
    bar.setAttribute("aria-orientation", isHoriz(bar.parentElement as Element) ? "vertical" : "horizontal");
  });
  inn.querySelectorAll<HTMLElement>(".ah-dl-tabbed").forEach((g) => {
    const off = !canPin(g), h = header(g);
    if (!h) { return; }
    h.querySelectorAll(".ah-dl-btn-pin").forEach((b) => {
      b.classList.toggle("ah-dl-btn-disabled", off);
      if (off) { b.setAttribute("aria-disabled", "true"); } else { b.removeAttribute("aria-disabled"); }
    });
  });
}

// ------------------------------------------------------------------
// value
// ------------------------------------------------------------------

function groupValue(g: Element, size?: number): GroupValue {
  const doc = g.getAttribute("data-document") === "true";
  const base = { type: doc ? "documents" as const : "tabs" as const, id: g.getAttribute("data-group-id"),
                 size: size, items: tabsOf(g).map(pid), active: pid(activeTab(g)) };
  // key order as before: pin (not for documents), then close
  return doc ? { ...base, close: flag(g, "close") }
             : { ...base, pin: flag(g, "pin"), close: flag(g, "close") };
}

function nodeValue(n: HTMLElement, size: number): NodeValue | null {
  if (n.classList.contains("ah-dl-group")) {
    return { type: "split", orientation: isHoriz(n) ? "horizontal" : "vertical", size: size,
             items: kidsValue(n) };
  }
  if (n.classList.contains("ah-dl-tabbed")) { return groupValue(n, size); }
  if (n.classList.contains("ah-dl-panel")) { return { type: "panel", size: size, item: pid(n) }; }
  return null;
}

function kidsValue(c: Element): NodeValue[] {
  const ks = real(c);
  let total = 0;
  ks.forEach((k) => { total += weight(k); });
  return ks.map((k) => nodeValue(k, round2(weight(k) * 100 / total)))
    .filter((v): v is NodeValue => v !== null);
}

function serialize(el: Element): string {
  const inn = inner(el);
  let out: LayoutValue[];
  if (!isHoriz(inn) && real(inn).length > 1) {
    out = [{ type: "split", orientation: "vertical", size: 100, items: kidsValue(inn) }];
  } else {
    out = kidsValue(inn);
  }
  floatWins(el).forEach((w) => {
    const g = winGroup(w);
    if (!g) { return; }
    const v = groupValue(g);
    out.push({ type: "float", id: w.getAttribute("data-group-id"), items: v.items, active: v.active,
               x: Math.round(parseFloat(w.style.left) || 0), y: Math.round(parseFloat(w.style.top) || 0),
               width: Math.round(parseFloat(w.style.width) || w.offsetWidth),
               height: Math.round(parseFloat(w.style.height) || w.offsetHeight) });
  });
  slotGroups(el).forEach((g) => {
    const v = groupValue(g);
    out.push({ type: "autohide", id: v.id, edge: g.getAttribute("data-edge"),
               size: Math.round(parseFloat(g.getAttribute("data-size") || "") || 250),
               items: v.items, active: v.active, pin: v.pin, close: v.close });
  });
  return JSON.stringify(out);
}

// ------------------------------------------------------------------
// docking
// ------------------------------------------------------------------

// Put a new group beside a target (splitting the target's space), or in
// a new container around the target when it runs the other way.
function insertBeside(target: HTMLElement, g: HTMLElement, zone: string): void {
  const horiz = zone === "left" || zone === "right", before = zone === "left" || zone === "top";
  const p = target.parentElement as HTMLElement;
  if (isContainer(p) && isHoriz(p) === horiz) {
    const w = weight(target);
    setWeight(target, w / 2);
    setWeight(g, w / 2);
    p.insertBefore(g, before ? target : target.nextSibling);
  } else {
    const wrap = newContainer(horiz);
    wrap.style.flex = target.style.flex || "1 1 0px";
    p.insertBefore(wrap, target);
    setWeight(target, 50);
    setWeight(g, 50);
    if (before) { wrap.appendChild(g); wrap.appendChild(target); }
    else { wrap.appendChild(target); wrap.appendChild(g); }
  }
}

// Put a group at an edge of the whole layout, a quarter of its size.
function insertEdge(el: Element, g: HTMLElement, zone: string, size?: number): void {
  const inn = inner(el), horiz = zone === "edge-left" || zone === "edge-right";
  const first = zone === "edge-left" || zone === "edge-top";
  let ks = real(inn);
  if (ks.length > 1 && isHoriz(inn) !== horiz) {
    const wrap = newContainer(isHoriz(inn));
    while (inn.firstChild) { wrap.appendChild(inn.firstChild); }
    setWeight(wrap, 100);
    inn.appendChild(wrap);
    ks = [wrap];
  }
  inn.classList.toggle("ah-dl-vertical", !horiz);
  let total = 0;
  ks.forEach((k) => { total += weight(k); });
  let share = 0.25;
  if (size && ks.length) {
    const full = horiz ? inn.clientWidth : inn.clientHeight;
    if (full > size) { share = Math.min(0.75, size / full); }
  }
  setWeight(g, ks.length ? total * share / (1 - share) : 100);
  inn.insertBefore(g, first ? inn.firstChild : null);
}

function defaultTarget(el: Element): HTMLElement | undefined {
  const gs = layoutGroups(el);
  return gs.find((g) => g.getAttribute("data-document") === "true") || gs[0];
}

function raise(win: HTMLElement): void {
  let max = 0;
  kids(win.parentElement, ".ah-dl-float-window").forEach((w) => {
    if (w !== win) { max = Math.max(max, parseInt(w.style.zIndex, 10) || 0); }
  });
  win.style.zIndex = String(max + 1);
}

function sourceFlags(g: Element): GroupFlags {
  return { pin: flag(g, "pin") || g.getAttribute("data-document") === "true",
           close: flag(g, "close") || g.getAttribute("data-document") === "true" };
}

// Dock a float window's tabs back (the whole group for a side or edge).
function dockFloat(el: Element, win: HTMLElement, zone?: string, target?: HTMLElement | null): void {
  const g = winGroup(win);
  if (!zone) {
    target = defaultTarget(el);
    zone = target ? "center" : "edge-right";
  }
  if (zone === "center") {
    const t = target as HTMLElement;   // the centre zone is always on a group
    const act = activeTab(g);
    tabsOf(g).forEach((li) => { insertTab(t, extract(li)); });
    select(t, act);
  } else {
    g.removeAttribute("style");
    if (/^edge-/.test(zone)) { insertEdge(el, g, zone); } else { insertBeside(target as HTMLElement, g, zone); }
  }
  win.remove();
}

// ------------------------------------------------------------------
// auto hide
// ------------------------------------------------------------------

function detectEdge(g: HTMLElement): string {
  for (let node: HTMLElement = g; ; node = node.parentElement as HTMLElement) {
    const p = node.parentElement;
    if (!isContainer(p)) { return "left"; }
    const ks = real(p), i = ks.indexOf(node), h = isHoriz(p);
    if (ks.length > 1 && i === 0) { return h ? "left" : "top"; }
    if (ks.length > 1 && i === ks.length - 1) { return h ? "right" : "bottom"; }
    if (p.classList.contains("ah-dl-inner")) { return "left"; }
  }
}

function stripTab(el: Element, gid: string | null): HTMLElement | null {
  for (const s of strips(el)) {
    const t = kids(s, ".ah-dl-autohide-tab").find((n) => n.getAttribute("data-group-id") === gid);
    if (t) { return t; }
  }
  return null;
}

function setPinned(g: HTMLElement, pinned: boolean): void {
  g.setAttribute("data-pinned", pinned ? "true" : "false");
  const h = header(g);
  if (!h) { return; }
  h.querySelectorAll(".ah-dl-btn-pin").forEach((b) => {
    b.classList.toggle("ah-dl-unpinned", !pinned);
    b.setAttribute("aria-pressed", pinned ? "false" : "true");
  });
}

// ------------------------------------------------------------------
// dragging: the dock overlay
// ------------------------------------------------------------------

function overlay(el: Element): HTMLElement | null { return kids(el, ".ah-dl-dock-overlay")[0] || null; }

function clearZones(ov: HTMLElement | null): void {
  if (!ov) { return; }
  ov.querySelectorAll(".ah-dl-dock-zone-active").forEach((n) => { n.classList.remove("ah-dl-dock-zone-active"); });
  ov.querySelectorAll(".ah-dl-dock-edge-active").forEach((n) => { n.classList.remove("ah-dl-dock-edge-active"); });
  kids(ov, ".ah-dl-dock-preview").forEach((n) => { n.classList.remove("ah-dl-dock-preview-visible"); });
}

// The smallest tab group of the layout under the pointer.
function groupAt(el: Element, x: number, y: number, skip: HTMLElement | null): HTMLElement | null {
  let best: HTMLElement | null = null, area = Infinity;
  layoutGroups(el).forEach((g) => {
    if (g === skip) { return; }
    const r = g.getBoundingClientRect();
    if (inside(x, y, r) && r.width * r.height < area) { area = r.width * r.height; best = g; }
  });
  return best;
}

// Place the cross over the group under the pointer and find the zone
// under it: {zone, target} or null; shows the preview rectangle.
function trackZones(el: Element, x: number, y: number, skip: HTMLElement | null): Hit | null {
  const ov = overlay(el);
  if (!ov) { return null; }
  const rr = el.getBoundingClientRect(), cross = kids(ov, ".ah-dl-dock-cross")[0];
  const target = groupAt(el, x, y, skip);
  if (cross) {
    if (target) {
      const tr = target.getBoundingClientRect();
      css(cross, { display: "grid",
                   left: Math.round(tr.left - rr.left + tr.width / 2 - CROSS / 2),
                   top: Math.round(tr.top - rr.top + tr.height / 2 - CROSS / 2) });
    } else {
      cross.style.display = "none";
    }
  }
  clearZones(ov);
  // the edges of the layout first: the cross of a group at the rim may
  // reach under them
  let hit: Hit | null = null;
  kids(ov, ".ah-dl-dock-edge").forEach((n) => {
    if (!hit && inside(x, y, n.getBoundingClientRect())) {
      hit = { zone: n.getAttribute("data-zone") || "", target: null, el: n };
    }
  });
  if (!hit && target && cross) {
    kids(cross, ".ah-dl-dock-zone").forEach((n) => {
      if (!hit && inside(x, y, n.getBoundingClientRect())) {
        hit = { zone: n.getAttribute("data-zone") || "", target: target, el: n };
      }
    });
  }
  const h = hit as Hit | null;
  if (!h) { return null; }
  h.el.classList.add(h.target ? "ah-dl-dock-zone-active" : "ah-dl-dock-edge-active");
  const box = (h.target || inner(el)).getBoundingClientRect();
  const l = box.left - rr.left, t = box.top - rr.top, w = box.width, ht = box.height;
  const rects: Record<string, [number, number, number, number]> = {
    top: [l, t, w, ht / 2], bottom: [l, t + ht / 2, w, ht / 2], left: [l, t, w / 2, ht],
    right: [l + w / 2, t, w / 2, ht], center: [l, t, w, ht],
    "edge-top": [l, t, w, ht / 4], "edge-bottom": [l, t + ht * 0.75, w, ht / 4],
    "edge-left": [l, t, w / 4, ht], "edge-right": [l + w * 0.75, t, w / 4, ht] };
  const p = rects[h.zone];
  if (p) {
    kids(ov, ".ah-dl-dock-preview").forEach((n) => {
      n.classList.add("ah-dl-dock-preview-visible");
      css(n, { left: p[0], top: p[1], width: p[2], height: p[3] });
    });
  }
  return h;
}

function showOverlay(el: Element): void {
  const ov = overlay(el);
  if (ov) { ov.classList.add("ah-dl-dock-overlay-visible"); }
}
function hideOverlay(el: Element): void {
  const ov = overlay(el);
  if (!ov) { return; }
  clearZones(ov);
  ov.classList.remove("ah-dl-dock-overlay-visible");
  kids(ov, ".ah-dl-dock-cross").forEach((n) => { n.style.display = "none"; });
}

// ------------------------------------------------------------------
// splitbars
// ------------------------------------------------------------------

// The neighbours of a splitbar: tidy() keeps a real node on each side.
function prevOf(bar: Element): HTMLElement { return bar.previousElementSibling as HTMLElement; }
function nextOf(bar: Element): HTMLElement { return bar.nextElementSibling as HTMLElement; }

// Resize the two neighbours of a splitbar by `delta' px from sizes
// `sp', `sn' (px) and weights `wp', `wn'.
function resizePair(el: Element, bar: Element, delta: number, base: PairBase): number {
  const prev = prevOf(bar), next = nextOf(bar);
  const min = Math.min(parseFloat(el.getAttribute("data-ah-min-size") || "") || 0, (base.sp + base.sn) / 2);
  const np = Math.max(min, Math.min(base.sp + delta, base.sp + base.sn - min));
  const total = base.wp + base.wn;
  setWeight(prev, total * np / (base.sp + base.sn));
  setWeight(next, total - total * np / (base.sp + base.sn));
  return np - base.sp;
}

function pairBase(bar: Element): PairBase {
  const prev = prevOf(bar), next = nextOf(bar);
  const horiz = isHoriz(bar.parentElement as Element);
  const a = prev.getBoundingClientRect(), b = next.getBoundingClientRect();
  return { horiz: horiz, sp: horiz ? a.width : a.height, sn: horiz ? b.width : b.height,
           wp: weight(prev), wn: weight(next) };
}

// ------------------------------------------------------------------
// context menu and lookup helpers
// ------------------------------------------------------------------

function menuEl(el: Element): HTMLElement | null { return kids(el, ".ah-dl-context-menu")[0] || null; }
function menuEntries(el: Element): HTMLElement[] {
  const m = menuEl(el);
  return m ? kids(m, ".ah-dl-menu-item").filter((n) => !n.classList.contains("ah-dl-menu-item-disabled")) : [];
}

function menuAt(node: Element): { x: number; y: number } {
  const r = node.getBoundingClientRect();
  return { x: r.left, y: r.bottom };
}

const TAB = ".ah-dl-tabs > .ah-tabs-header > .ah-tabs-item";

function findTab(el: Element, id: unknown): HTMLElement | undefined {
  return Array.from(el.querySelectorAll<HTMLElement>(TAB)).find((li) =>
    pid(li) === String(id) && li.closest(".ah-dl") === el);
}

function dlDisabled(el: Element): boolean { return el.classList.contains("ah-dl-disabled"); }

// ------------------------------------------------------------------
// behaviour
// ------------------------------------------------------------------

class DockLayoutController extends AH.Controller {
  /** Old places of the auto hidden groups, by group id. */
  #memo = new Map<string | null, Memo>();
  /** The auto hide show / hide delay. */
  #timer: ReturnType<typeof setTimeout> | undefined = undefined;
  /** The group id whose auto hide preview is open. */
  #preview: string | null = null;
  #labels: Labels = parseLabels(null);
  #menu: MenuState | null = null;
  /** The document listener of the open menu. */
  #menuAc: AbortController | null = null;

  override setup(): void {
    const el = this.element;
    this.#memo = new Map();
    this.#preview = null;
    this.#labels = parseLabels(parse(el.getAttribute("data-ah-labels")));
    const mine = (node: Element): boolean => node.closest(".ah-dl") === el && !dlDisabled(el);
    // mouseenter / mouseleave of the matches of a selector, from the
    // delegated over / out
    const hover = (selector: string, enter: (n: HTMLElement) => void, leave: (n: HTMLElement) => void): void => {
      this.delegate("mouseover", selector, (e, n) => {
        if (!(e.relatedTarget instanceof Node && n.contains(e.relatedTarget))) { enter(n); }
      });
      this.delegate("mouseout", selector, (e, n) => {
        if (!(e.relatedTarget instanceof Node && n.contains(e.relatedTarget))) { leave(n); }
      });
    };

    // tabs: select, keyboard, drag, context menu
    this.delegate("click", TAB, (_e, li) => {
      if (!mine(li)) { return; }
      const g = groupOf(li);
      if (!li.classList.contains("ah-tabs-item-selected")) {
        select(g, li);
        this.save(true);
      }
    });
    this.delegate("keydown", TAB, (e, li) => {
      if (!mine(li)) { return; }
      const g = groupOf(li), t = tabsOf(g), i = t.indexOf(li);
      let n = 0;
      switch (e.key) {
        case "ArrowRight": n = (i + 1) % t.length; break;
        case "ArrowLeft": n = (i - 1 + t.length) % t.length; break;
        case "Home": n = 0; break;
        case "End": n = t.length - 1; break;
        case "Delete":
          if (flag(g, "close") && !winOf(g)) {
            e.preventDefault();
            this.closeTab(li, true);
            this.done(true);
          }
          return;
        case "ContextMenu": case "F10": {
          if (e.key === "F10" && !e.shiftKey) { return; }
          e.preventDefault();
          const p = menuAt(li);
          this.openMenu(g, winOf(g), li, p.x, p.y);
          return;
        }
        default: return;
      }
      e.preventDefault();
      select(g, t[n]);
      t[n].focus();
      this.save(true);
    });
    this.delegate("pointerdown", TAB, (e, li) => {
      if (e.button !== 0 || !mine(li)) { return; }
      const g = groupOf(li);
      if (winOf(g) || inSlot(g) || (!opt(el, "allow-float") && !opt(el, "allow-dock"))) { return; }
      this.dragTab(li, e);
    });
    this.delegate<MouseEvent, HTMLElement>("contextmenu", TAB + ", .ah-dl-tabbed-actions, .ah-dl-float-titlebar",
                                           (e, n) => {
      if (!mine(n)) { return; }
      e.preventDefault();
      const win = winOf(n);
      const g = win ? winGroup(win) : groupOf(n);
      this.openMenu(g, win, n.classList.contains("ah-tabs-item") ? n : null, e.clientX, e.clientY);
    });
    this.delegate("click", ".ah-dl-context-menu > .ah-dl-menu-item", (e, n) => {
      e.stopPropagation();
      if (!n.classList.contains("ah-dl-menu-item-disabled")) { this.runMenu(n.getAttribute("data-action")); }
    });
    this.delegate("keydown", ".ah-dl-context-menu", (e) => { this.menuKey(e); });

    // group buttons
    this.delegate("click", ".ah-dl-tabbed-actions > .ah-dl-btn-pin", (e, b) => {
      if (!mine(b) || b.classList.contains("ah-dl-btn-disabled")) { return; }
      e.stopPropagation();
      const g = groupOf(b);
      if (inSlot(g)) {
        this.hidePreview();
        this.repin(g);
        this.done(true);
        const a = activeTab(g);
        if (a) { a.focus(); }
      } else if (this.autoHide(g)) {
        this.done(true);
      }
    });
    this.delegate("click", ".ah-dl-tabbed-actions > .ah-dl-btn-close", (e, b) => {
      if (!mine(b)) { return; }
      e.stopPropagation();
      this.closeGroup(groupOf(b), true);
      this.done(true);
    });

    // float windows
    this.delegate("pointerdown", ".ah-dl-float-window", (_e, w) => {
      if (mine(w)) { raise(w); }
    });
    this.delegate("pointerdown", ".ah-dl-float-titlebar", (e, bar) => {
      const t = e.target as Element;
      if (e.button !== 0 || !mine(bar) || t.closest(".ah-dl-btn")) { return; }
      this.dragFloat(winOf(bar) as HTMLElement, e);
    });
    this.delegate("pointerdown", ".ah-dl-float-resize-se", (e, n) => {
      if (e.button !== 0 || !mine(n)) { return; }
      e.preventDefault();
      e.stopPropagation();
      this.resizeFloat(winOf(n) as HTMLElement, e);
    });
    this.delegate("click", ".ah-dl-float-actions > .ah-dl-btn-close", (e, b) => {
      if (!mine(b)) { return; }
      e.stopPropagation();
      this.closeGroup(winGroup(winOf(b) as HTMLElement), true);
      this.done(true);
    });
    this.delegate("keydown", ".ah-dl-float-titlebar", (e, bar) => {
      if (e.target !== bar || !mine(bar)) { return; }
      const win = winOf(bar) as HTMLElement, step = 10;   // a title bar is in its window
      const dx = e.key === "ArrowLeft" ? -step : e.key === "ArrowRight" ? step : 0;
      const dy = e.key === "ArrowUp" ? -step : e.key === "ArrowDown" ? step : 0;
      if (e.key === "ContextMenu" || (e.key === "F10" && e.shiftKey)) {
        e.preventDefault();
        const p = menuAt(bar);
        this.openMenu(winGroup(win), win, null, p.x, p.y);
        return;
      }
      if (!dx && !dy) { return; }
      e.preventDefault();
      if (e.shiftKey) {
        win.style.width = Math.max(150, win.offsetWidth + dx) + "px";
        win.style.height = Math.max(100, win.offsetHeight + dy) + "px";
      } else {
        win.style.left = ((parseFloat(win.style.left) || 0) + dx) + "px";
        win.style.top = ((parseFloat(win.style.top) || 0) + dy) + "px";
      }
      this.save(true);
    });

    // splitbars
    this.delegate("pointerdown", ".ah-dl-splitbar", (e, bar) => {
      if (e.button !== 0 || !mine(bar) || !opt(el, "resizable")) { return; }
      e.preventDefault();
      this.dragSplitbar(bar, e);
    });
    this.delegate("keydown", ".ah-dl-splitbar", (e, bar) => {
      if (!mine(bar) || !opt(el, "resizable")) { return; }
      const horiz = isHoriz(bar.parentElement as Element), step = e.shiftKey ? 50 : 10;
      const steps: Record<string, number> = { ArrowLeft: horiz ? -step : 0, ArrowRight: horiz ? step : 0,
                                              ArrowUp: horiz ? 0 : -step, ArrowDown: horiz ? 0 : step };
      const d = steps[e.key];
      if (!d) { return; }
      e.preventDefault();
      resizePair(el, bar, d, pairBase(bar));
      this.save(true);
    });

    // auto hide strips and previews
    hover(".ah-dl-autohide-tab", (tab) => {
      if (!mine(tab)) { return; }
      const gid = tab.getAttribute("data-group-id");
      clearTimeout(this.#timer);
      this.#timer = setTimeout(() => { this.showPreview(gid); }, SHOW_DELAY);
    }, (tab) => {
      if (mine(tab)) { this.scheduleHide(); }
    });
    hover(".ah-dl-autohide-preview-slot", (slot) => {
      if (mine(slot)) { clearTimeout(this.#timer); }
    }, (slot) => {
      if (mine(slot)) { this.scheduleHide(); }
    });
    this.delegate("click", ".ah-dl-autohide-tab", (_e, tab) => {
      if (!mine(tab)) { return; }
      const gid = tab.getAttribute("data-group-id");
      if (this.#preview === gid) { this.hidePreview(); } else { this.showPreview(gid); }
    });
    this.delegate("keydown", ".ah-dl-autohide-tab", (e, tab) => {
      if (!mine(tab) || (e.key !== "Enter" && e.key !== " ")) { return; }
      e.preventDefault();
      const gid = tab.getAttribute("data-group-id");
      if (this.#preview === gid) { this.hidePreview(); return; }
      this.showPreview(gid);
      let g: HTMLElement | null = null;
      slots(el).forEach((s) => { g = g || kids(s, ".ah-dl-autohide-current")[0] || null; });
      const a = g ? activeTab(g) : undefined;
      if (a) { a.focus(); }
    });
    this.delegate("keydown", ".ah-dl-autohide-preview-slot", (e, slot) => {
      if (e.key !== "Escape" || !mine(slot) || !this.#preview) { return; }
      e.preventDefault();
      const gid = this.#preview;
      this.hidePreview();
      const tab = stripTab(el, gid);
      if (tab) { tab.focus(); }
    });

    refresh(el);
    this.save(false);
  }

  override teardown(): void {
    DockDrag.stop(this.element);
    clearTimeout(this.#timer);
    this.closeMenu(false);
    this.#memo.clear();
    this.#preview = null;
  }

  // methods (aihtml_action:call/4, AH.invoke)

  /** Show a panel: select its tab, open its auto hide group, raise its float. */
  activate(id: string | number): void {
    const el = this.element, li = findTab(el, id);
    if (!li) { return; }
    const g = groupOf(li), win = winOf(g);
    if (inSlot(g)) { this.showPreview(g.getAttribute("data-group-id")); }
    if (win) { raise(win); }
    if (!li.classList.contains("ah-tabs-item-selected")) {
      select(g, li);
      this.save(true);
    }
  }

  float(id: string | number): void {
    const el = this.element, li = findTab(el, id);
    if (!li || winOf(li)) { return; }
    const n = floatWins(el).length;
    this.floatTab(li, 40 + 24 * n, 40 + 24 * n);
    this.done(true);
  }

  dock(id: string | number): void {
    const el = this.element, li = findTab(el, id);
    if (!li) { return; }
    const g = groupOf(li), win = winOf(g);
    if (win) { dockFloat(el, win); }
    else if (inSlot(g)) { this.hidePreview(); this.repin(g); }
    else { return; }
    this.done(true);
  }

  close(id: string | number): void {
    const li = findTab(this.element, id);
    if (!li) { return; }
    this.closeTab(li, false);
    this.done(true);
  }

  /** Open a server-rendered tab group's first panel; where: {in: group
   *  id, float, x, y, edge}. An open panel is activated instead. */
  openPanel(html: string, where?: unknown): void {
    const el = this.element;
    const w = parseWhere(where);
    const g = parseHTML(html).find((n) => n.matches(".ah-dl-tabbed"));
    let li = g && tabsOf(g)[0];
    if (!g || !li) { return; }
    const id = pid(li);
    if (findTab(el, id)) { this.activate(id || ""); return; }
    const byId = (n: Element): boolean => n.getAttribute("data-group-id") === String(w.in);
    let target = w.in ? (layoutGroups(el).find(byId) || slotGroups(el).find(byId)) : undefined;
    if (w.float) {
      const n = floatWins(el).length;
      this.floatItems([extract(li)], w.x !== undefined ? w.x : 40 + 24 * n,
                      w.y !== undefined ? w.y : 40 + 24 * n, sourceFlags(g));
    } else if (w.edge) {
      insertEdge(el, g, "edge-" + w.edge);
    } else if (target || (target = defaultTarget(el))) {
      insertTab(target, extract(li));
    } else {
      insertEdge(el, g, "edge-right");
    }
    li = findTab(el, id);
    const panel = li ? panelFor(groupOf(li), li) : undefined;
    if (panel) { AH.mount(panel); }
    this.done(true);
  }

  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }

  // ---- value ----

  private save(fire: boolean): boolean { return commit(this.element, serialize(this.element), fire); }

  private done(fire: boolean): boolean {
    this.tidy();
    return this.save(fire);
  }

  // Keep the tree regular: no empty groups, a splitbar between each pair
  // of siblings, groups of one child replaced by it, a group inside a
  // container of the same orientation merged into it.
  private tidy(): void {
    const el = this.element;
    const walk = (c: HTMLElement): void => {
      kids(c, ".ah-dl-group").forEach(walk);
      kids(c, ".ah-dl-tabbed").forEach((g) => { if (!tabsOf(g).length) { g.remove(); } });
      kids(c, ".ah-dl-group").forEach((g) => {
        const ks = real(g);
        if (!ks.length) { g.remove(); return; }
        if (ks.length === 1) {
          ks[0].style.flex = g.style.flex || "1 1 0px";
          c.insertBefore(ks[0], g);
          c.removeChild(g);
        } else if (isHoriz(g) === isHoriz(c)) {
          const gw = weight(g);
          let sum = 0;
          ks.forEach((k) => { sum += weight(k); });
          ks.forEach((k) => { setWeight(k, weight(k) * gw / sum); c.insertBefore(k, g); });
          c.removeChild(g);
        }
      });
      const nodes = kids(c);
      let prevReal = false;
      nodes.forEach((n, i) => {
        if (isBar(n)) {
          const nextReal = i + 1 < nodes.length && !isBar(nodes[i + 1]);
          if (!prevReal || !nextReal) { n.remove(); } else { prevReal = false; }
        } else {
          if (prevReal) { c.insertBefore(splitbar(el), n); }
          prevReal = true;
        }
      });
    };
    walk(inner(el));
    // memos of auto hidden groups whose old parent is gone
    this.#memo.forEach((m, gid) => {
      if (!m.parent.isConnected) { this.#memo.delete(gid); }
    });
    refresh(el);
  }

  // ---- groups, tabs, floats ----

  private newGroup(pin: boolean, close: boolean): HTMLElement {
    const lb = this.#labels;
    // the template renders one root element
    return parseHTML(AH.tpl.dock_layout_group({
      gid: this.element.id + "-n" + DockIds.next(), document: false, pin: pin ? "true" : "false",
      close: close ? "true" : "false", pinned: "true", unpinned: false, edge: false, size: 0,
      style: false, pin_label: lb.auto_hide, close_label: lb.close, tabs: [] }))[0];
  }

  // A group left without tabs goes away with its float window or its auto
  // hide tab; one in the layout is removed by tidy().
  private dropEmpty(g: HTMLElement | null): void {
    if (!g || tabsOf(g).length) { return; }
    const win = winOf(g);
    if (win) { win.remove(); return; }
    if (inSlot(g)) { this.discardHidden(g); }
    g.remove();
  }

  // Dock tab items (from a drag, a float or a server-rendered group) at a
  // zone: "center" of a target group, a side of it, or an edge.
  private dockItems(items: TabItem[], zone: string, target: HTMLElement | null, flags: GroupFlags): void {
    if (zone === "center") {
      if (target) { items.forEach((it) => { insertTab(target, it); }); }
      return;
    }
    const g = this.newGroup(flags.pin, flags.close);
    items.forEach((it) => { insertTab(g, it); });
    if (flags.active) { select(g, flags.active); }
    if (/^edge-/.test(zone)) { insertEdge(this.element, g, zone); }
    else if (target) { insertBeside(target, g, zone); }
  }

  private floatItems(items: TabItem[], x: number, y: number, flags: GroupFlags): HTMLElement {
    const el = this.element, rr = el.getBoundingClientRect();
    const w = 300, h = 200;
    x = Math.max(0, Math.min(Math.round(x), Math.round(rr.width) - 120));
    y = Math.max(0, Math.min(Math.round(y), Math.round(rr.height) - 60));
    // the template renders one root element with its body
    const win = parseHTML(AH.tpl.dock_layout_float({
      fid: el.id + "-f" + DockIds.next(), title: "", x: x, y: y, width: w, height: h,
      close_label: this.#labels.close, body: "" }))[0];
    const g = this.newGroup(flags.pin, flags.close);
    (win.querySelector(":scope > .ah-dl-float-body") as HTMLElement).appendChild(g);
    floats(el)[0].appendChild(win);
    items.forEach((it) => { insertTab(g, it); });
    if (flags.active) { select(g, flags.active); }
    raise(win);
    return win;
  }

  private floatTab(li: HTMLElement, x: number, y: number): HTMLElement {
    const g = groupOf(li), flags = sourceFlags(g), it = extract(li);
    this.dropEmpty(g);
    return this.floatItems([it], x, y, flags);
  }

  // ---- auto hide ----

  private autoHide(g: HTMLElement): boolean {
    if (!canPin(g)) { return false; }
    const el = this.element, gid = g.getAttribute("data-group-id"), edge = detectEdge(g);
    const p = g.parentElement as HTMLElement, side = edge === "left" || edge === "right";
    const size = Math.round(side ? g.offsetWidth : g.offsetHeight) || 250;
    this.#memo.set(gid, { parent: p, index: real(p).indexOf(g), weight: weight(g) });
    setPinned(g, false);
    g.setAttribute("data-edge", edge);
    g.setAttribute("data-size", String(size));
    g.removeAttribute("style");
    const focus = g !== document.activeElement && g.contains(document.activeElement);
    slotOf(el, edge).appendChild(g);
    const tab = div("ah-dl-autohide-tab", { role: "button", tabindex: "0", "aria-expanded": "false",
                                            "data-group-id": gid || "" });
    const first = tabsOf(g)[0];
    tab.textContent = first ? first.textContent : "";
    stripOf(el, edge).appendChild(tab);
    if (focus) { tab.focus(); }
    return true;
  }

  private showPreview(gid: string | null): void {
    const el = this.element;
    clearTimeout(this.#timer);
    const g = slotGroups(el).find((n) => n.getAttribute("data-group-id") === gid);
    if (!g) { return; }
    if (this.#preview && this.#preview !== gid) { this.hidePreview(); }
    const edge = g.getAttribute("data-edge"), slot = g.parentElement as HTMLElement;
    kids(slot, ".ah-dl-tabbed").forEach((n) => { n.classList.remove("ah-dl-autohide-current"); });
    g.classList.add("ah-dl-autohide-current");
    slot.style.display = "flex";
    slot.style.flex = "0 0 " + (parseFloat(g.getAttribute("data-size") || "") ||
                                (edge === "top" || edge === "bottom" ? 200 : 280)) + "px";
    const tab = stripTab(el, gid);
    if (tab) {
      tab.classList.add("ah-dl-autohide-tab-active");
      tab.setAttribute("aria-expanded", "true");
    }
    this.#preview = gid;
  }

  private hidePreview(): void {
    const el = this.element;
    clearTimeout(this.#timer);
    slots(el).forEach((s) => {
      s.style.display = "";
      s.style.flex = "";
      kids(s, ".ah-dl-tabbed").forEach((n) => { n.classList.remove("ah-dl-autohide-current"); });
    });
    strips(el).forEach((s) => {
      kids(s, ".ah-dl-autohide-tab").forEach((t) => {
        t.classList.remove("ah-dl-autohide-tab-active");
        t.setAttribute("aria-expanded", "false");
      });
    });
    this.#preview = null;
  }

  private discardHidden(g: HTMLElement): void {
    const gid = g.getAttribute("data-group-id");
    if (this.#preview === gid) { this.hidePreview(); }
    const tab = stripTab(this.element, gid);
    if (tab) { tab.remove(); }
    this.#memo.delete(gid);
  }

  // Back from the edge: into its old place if that still exists, else at
  // its edge of the layout.
  private repin(g: HTMLElement): void {
    const el = this.element, gid = g.getAttribute("data-group-id");
    const edge = g.getAttribute("data-edge") || "left", size = parseFloat(g.getAttribute("data-size") || "");
    const m = this.#memo.get(gid);
    this.discardHidden(g);
    g.classList.remove("ah-dl-autohide-current");
    g.removeAttribute("data-edge");
    g.removeAttribute("data-size");
    setPinned(g, true);
    if (m && m.parent.isConnected && inLayout(el, m.parent) || m && m.parent === inner(el)) {
      const ks = real(m.parent);
      m.parent.insertBefore(g, m.index < ks.length ? ks[m.index] : null);
      setWeight(g, m.weight);
    } else {
      insertEdge(el, g, "edge-" + edge, size);
    }
  }

  private scheduleHide(): void {
    const el = this.element;
    clearTimeout(this.#timer);
    this.#timer = setTimeout(() => {
      const a = document.activeElement;
      if (!kids(el, ".ah-dl-context-menu").length &&
          !slots(el).some((s) => s !== a && s.contains(a))) { this.hidePreview(); }
    }, HIDE_DELAY);
  }

  // ---- closing ----

  private closed(ids: (string | null)[], user: boolean): void {
    if (user && ids.length) { fireData(this.element, "ah:panel-close", "panels", join(ids)); }
  }

  private closeGroup(g: HTMLElement, user: boolean): void {
    const ids = tabsOf(g).map(pid);
    const win = winOf(g);
    if (inSlot(g)) { this.discardHidden(g); }
    drop(win || g);
    this.closed(ids, user);
  }

  private closeTab(li: HTMLElement, user: boolean): void {
    const g = groupOf(li), focus = g.contains(document.activeElement) && g !== document.activeElement;
    const it = extract(li);
    drop(it.panel);
    li.remove();
    const a = activeTab(g);
    if (focus && a) { a.focus(); }
    this.dropEmpty(g);
    this.closed([pid(li)], user);
  }

  // ---- dragging ----

  private dragTab(li: HTMLElement, e: PointerEvent): void {
    const el = this.element, g = groupOf(li);
    const allowDock = opt(el, "allow-dock"), allowFloat = opt(el, "allow-float");
    let ghost: HTMLElement | null = null, zone: Hit | null = null;
    // a group of one tab is not a target for its own tab
    const skip = (): HTMLElement | null => (tabsOf(g).length === 1 ? g : null);
    DockDrag.track(el, e, {
      start: (ev) => {
        ghost = div("ah-dl-drag-ghost", { "aria-hidden": "true" });
        ghost.textContent = li.textContent;
        css(ghost, { left: ev.clientX, top: ev.clientY });
        el.appendChild(ghost);
        li.classList.add("ah-dl-tab-dragging");
        if (allowDock) { showOverlay(el); }
      },
      move: (ev) => {
        if (ghost) { css(ghost, { left: ev.clientX, top: ev.clientY }); }
        zone = allowDock ? trackZones(el, ev.clientX, ev.clientY, skip()) : null;
      },
      end: (ev, cancelled) => {
        if (ghost) { ghost.remove(); }
        li.classList.remove("ah-dl-tab-dragging");
        hideOverlay(el);
        if (cancelled || !ev) { return; }
        const h = header(g) as HTMLElement;
        if (zone && !(zone.zone === "center" && zone.target === g)) {
          const flags = sourceFlags(g), it = extract(li);
          this.dropEmpty(g);
          this.dockItems([it], zone.zone, zone.target, flags);
        } else if (inside(ev.clientX, ev.clientY, h.getBoundingClientRect())) {
          // reorder within the header
          let over: HTMLElement | null = null;
          tabsOf(g).forEach((t) => {
            const r = t.getBoundingClientRect();
            if (!over && t !== li && ev.clientX < r.left + r.width / 2) { over = t; }
          });
          if (over !== li.nextSibling) { insertTab(g, extract(li), over); }
        } else if (allowFloat && !(zone && zone.target === g)) {
          const rr = el.getBoundingClientRect();
          this.floatTab(li, ev.clientX - rr.left - 20, ev.clientY - rr.top - 12);
        } else {
          return;
        }
        if (this.done(true) && el.contains(li)) { li.focus(); }
      }
    });
  }

  private dragFloat(win: HTMLElement, e: PointerEvent): void {
    const el = this.element, allowDock = opt(el, "allow-dock");
    let x0 = 0, y0 = 0, zone: Hit | null = null;
    DockDrag.track(el, e, {
      start: () => {
        x0 = parseFloat(win.style.left) || 0;
        y0 = parseFloat(win.style.top) || 0;
        if (allowDock) { showOverlay(el); }
      },
      move: (ev) => {
        win.style.left = Math.round(x0 + ev.clientX - e.clientX) + "px";
        win.style.top = Math.round(y0 + ev.clientY - e.clientY) + "px";
        zone = allowDock ? trackZones(el, ev.clientX, ev.clientY, null) : null;
      },
      end: (_ev, cancelled) => {
        hideOverlay(el);
        if (cancelled) {
          win.style.left = x0 + "px";
          win.style.top = y0 + "px";
          return;
        }
        if (zone) { dockFloat(el, win, zone.zone, zone.target); }
        this.done(true);
      }
    }, 3);
  }

  private resizeFloat(win: HTMLElement, e: PointerEvent): void {
    const w0 = win.offsetWidth, h0 = win.offsetHeight;
    DockDrag.track(this.element, e, {
      start: () => {},
      move: (ev) => {
        win.style.width = Math.max(150, Math.round(w0 + ev.clientX - e.clientX)) + "px";
        win.style.height = Math.max(100, Math.round(h0 + ev.clientY - e.clientY)) + "px";
      },
      end: (_ev, cancelled) => {
        if (cancelled) { win.style.width = w0 + "px"; win.style.height = h0 + "px"; }
        this.done(!cancelled);
      }
    }, 0);
  }

  private dragSplitbar(bar: HTMLElement, e: PointerEvent): void {
    const el = this.element;
    const feedback = el.getAttribute("data-ah-resize-mode") === "feedback";
    let base: PairBase = { horiz: true, sp: 0, sn: 0, wp: 1, wn: 1 };
    let line: HTMLElement | null = null, delta = 0;
    DockDrag.track(el, e, {
      start: () => {
        base = pairBase(bar);
        if (feedback) {
          const rr = el.getBoundingClientRect(), br = bar.getBoundingClientRect();
          line = div("ah-dl-resize-feedback " +
                     (base.horiz ? "ah-dl-resize-feedback-h" : "ah-dl-resize-feedback-v"));
          css(line, base.horiz ? { left: br.left - rr.left, top: br.top - rr.top, bottom: "auto", height: br.height }
                               : { top: br.top - rr.top, left: br.left - rr.left, right: "auto", width: br.width });
          el.appendChild(line);
          base.line = base.horiz ? br.left - rr.left : br.top - rr.top;
        }
      },
      move: (ev) => {
        const d = base.horiz ? ev.clientX - e.clientX : ev.clientY - e.clientY;
        if (feedback && line) {
          const min = Math.min(parseFloat(el.getAttribute("data-ah-min-size") || "") || 0, (base.sp + base.sn) / 2);
          delta = Math.max(min - base.sp, Math.min(d, base.sn - min));
          line.style.setProperty(base.horiz ? "left" : "top", ((base.line || 0) + delta) + "px");
        } else {
          resizePair(el, bar, d, base);
        }
      },
      end: (_ev, cancelled) => {
        if (line) { line.remove(); }
        if (cancelled) {
          setWeight(prevOf(bar), base.wp);
          setWeight(nextOf(bar), base.wn);
          return;
        }
        if (feedback) { resizePair(el, bar, delta, base); }
        this.done(true);
      }
    }, 0);
  }

  // ---- context menu ----

  private menuItems(g: HTMLElement, win: HTMLElement | null): MenuItem[] {
    const el = this.element, lb = this.#labels, items: MenuItem[] = [];
    const item = (action: string, label: string, disabled?: boolean): void => {
      items.push({ action: action, label: label, disabled: !!disabled, divider: false });
    };
    const allowFloat = opt(el, "allow-float");
    if (win) {
      if (opt(el, "allow-dock")) { item("dock", lb.dock); }
      item("close-float", lb.close);
    } else if (g.getAttribute("data-document") === "true") {
      if (allowFloat) { item("float", lb.float); }
      if (flag(g, "close")) { items.push({ divider: true }); item("close", lb.close); }
    } else if (inSlot(g)) {
      item("unpin", lb.dock);
      if (allowFloat) { item("float", lb.float); }
      if (flag(g, "close")) { items.push({ divider: true }); item("close", lb.close); }
    } else {
      if (flag(g, "pin")) { item("pin", lb.auto_hide, !canPin(g)); }
      if (allowFloat) { item("float", lb.float); }
      if (flag(g, "close")) { items.push({ divider: true }); item("close", lb.close); }
    }
    if (items.length && items[0].divider) { items.shift(); }
    return items;
  }

  private closeMenu(refocus: boolean): void {
    const el = this.element, m = menuEl(el);
    if (!m) { return; }
    kids(el, ".ah-dl-context-menu").forEach((n) => { n.remove(); });
    if (this.#menuAc) { this.#menuAc.abort(); this.#menuAc = null; }
    const st = this.#menu;
    if (refocus && st && st.focus instanceof HTMLElement && el.contains(st.focus) && st.focus !== el) {
      st.focus.focus();
    }
    this.#menu = null;
  }

  private openMenu(g: HTMLElement, win: HTMLElement | null, li: HTMLElement | null, x: number, y: number): void {
    const el = this.element;
    this.closeMenu(false);
    const items = this.menuItems(g, win);
    if (!items.length) { return; }
    const rr = el.getBoundingClientRect();
    // the template renders one root element
    const m = parseHTML(AH.tpl.dock_layout_menu({
      x: Math.round(x - rr.left), y: Math.round(y - rr.top), items: items }))[0];
    el.appendChild(m);
    if (m.offsetLeft + m.offsetWidth > el.clientWidth) { m.style.left = Math.max(0, el.clientWidth - m.offsetWidth) + "px"; }
    if (m.offsetTop + m.offsetHeight > el.clientHeight) { m.style.top = Math.max(0, el.clientHeight - m.offsetHeight) + "px"; }
    this.#menu = { g: g, win: win, li: li, x: x, y: y, focus: document.activeElement };
    this.#menuAc = new AbortController();
    document.addEventListener("pointerdown", (e) => {
      if (!m.contains(e.target as Node | null)) { this.closeMenu(false); }
    }, { signal: this.#menuAc.signal });
    const first = menuEntries(el)[0];
    if (first) { first.focus(); }
  }

  private runMenu(action: string | null): void {
    const el = this.element, m = this.#menu;
    if (!m) { return; }
    this.closeMenu(false);
    const rr = el.getBoundingClientRect(), g = m.g;
    let focus: HTMLElement | null | undefined = null;
    switch (action) {
      case "pin":
        if (this.autoHide(g)) { focus = stripTab(el, g.getAttribute("data-group-id")); }
        break;
      case "unpin":
        this.repin(g);
        focus = activeTab(g);
        break;
      case "float": {
        const li = m.li && groupOf(m.li) === g ? m.li : activeTab(g);
        if (li) { this.floatTab(li, m.x - rr.left, m.y - rr.top); }
        focus = li;
        break;
      }
      case "dock":
        if (m.win) {
          focus = activeTab(winGroup(m.win));
          dockFloat(el, m.win);
        }
        break;
      case "close-float":
        if (m.win) { this.closeGroup(winGroup(m.win), true); }
        break;
      case "close":
        this.closeGroup(g, true);
        break;
      default:
        return;
    }
    this.done(true);
    if (focus && el.contains(focus)) { focus.focus(); }
    else if (m.focus instanceof HTMLElement && el.contains(m.focus)) { m.focus.focus(); }
  }

  private menuKey(e: KeyboardEvent): void {
    const items = menuEntries(this.element);
    const i = items.findIndex((n) => n === document.activeElement);
    const focusAt = (n: number): void => { if (items[n]) { items[n].focus(); } };
    switch (e.key) {
      case "ArrowDown": focusAt((i + 1) % items.length); break;
      case "ArrowUp": focusAt((i - 1 + items.length) % items.length); break;
      case "Home": focusAt(0); break;
      case "End": focusAt(items.length - 1); break;
      case "Enter": case " ":
        if (i >= 0) { this.runMenu(items[i].getAttribute("data-action")); }
        break;
      case "Escape": case "Tab": this.closeMenu(true); break;
      default: return;
    }
    e.preventDefault();
  }
}

AH.register("dock-layout", DockLayoutController);
