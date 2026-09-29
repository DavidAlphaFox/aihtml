/* The tile layout behaviour (designs/04-components.md), ported from sigil's
 * layout/tile_layout: splitbars resize panes, tabs are dragged between
 * tab groups or to an edge of a pane (which splits it), tabs close.
 *
 * The browser moves the live nodes the server rendered, so tile contents
 * keep their state and behaviours. New shells (a tab group, a group, a
 * splitbar) are single elements; a tab for a tile that becomes a tab
 * group comes from the shared template AH.tpl.tile_layout_tab.
 *
 * The arrangement is kept in data-ah-value as JSON (TileLayoutValue: the
 * same shape and key order the server writes, see aihtml_tile_layout)
 * and mirrored into the hidden input; every resize, move, close and tab
 * switch fires "change" on the root. "ah:tab-select" and "ah:tab-close"
 * carry the tab id as their detail (TileTabEvent).
 */
import AH from "../core.ts";
import "virtual:ah-tpl/tile_layout_tab";

/** Detail of ah:tab-select and ah:tab-close: the tab id. */
export type TileTabEvent = string | null;

/** A node of the arrangement (keys in the order the server's JSON
 *  encoder uses). */
export type TileNode =
  | { id: string | null; items: TileNode[]; size?: string; type: "columns" | "rows" }
  | { active: string; id: string | null; size?: string; tabs: (string | null)[]; type: "tabs" }
  | { id: string | null; size?: string; type: "item" };

/** The value (data-ah-value). */
export interface TileLayoutValue { closed: string[]; root: TileNode | null; }

type Zone = "top" | "bottom" | "left" | "right" | "center";

/** A tab drag in progress. */
interface TabDrag {
  tab: HTMLElement;
  x: number;
  y: number;
  started: boolean;
  feedback?: HTMLElement;
  overlay?: HTMLElement;
  target?: HTMLElement | null;
  zone?: Zone | null;
}

/** A splitbar resize in progress. */
interface Resize {
  bar: HTMLElement;
  group: HTMLElement;
  start: number;
  px: number[];
  i: number;
  moved: boolean;
  saved: (string | null)[];
  minPrev: number;
  minNext: number;
}

const MIN = 20;

function mine(el: Element, node: Element): boolean { return node.closest(".ah-tl") === el; }
function own(el: Element, selector: string): HTMLElement[] {
  return Array.from(el.querySelectorAll<HTMLElement>(selector)).filter((n) => mine(el, n));
}

function disabled(el: Element): boolean {
  return el.classList.contains("ah-tl-disabled");
}

class Ids {
  private static seq = 0;
  static next(): string { return "tl-" + Date.now().toString(36) + "-" + (++Ids.seq); }
}

function barSize(el: Element): number {
  const n = parseInt(el.getAttribute("data-splitbar-size") || "", 10);
  return isNaN(n) ? 4 : n;
}

function div(cls: string, attrs?: Record<string, string>): HTMLElement {
  const d = document.createElement("div");
  d.className = cls;
  Object.keys(attrs || {}).forEach((k) => { d.setAttribute(k, (attrs || {})[k]); });
  return d;
}

function emit(el: Element, type: string, detail?: unknown): void {
  el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
}

// ------------------------------------------------------------------
// Groups: children, grid tracks, sizes
// ------------------------------------------------------------------

function isGroup(n: Node | null): n is HTMLElement {
  return n instanceof HTMLElement && n.getAttribute("data-type") === "layout-group";
}
function isTabGroup(n: Node | null): n is HTMLElement {
  return n instanceof HTMLElement && n.getAttribute("data-type") === "tab-group";
}

function kids(group: Element): HTMLElement[] {
  return (Array.from(group.children) as HTMLElement[]).filter((n) => !n.classList.contains("ah-tl-splitbar"));
}

function vert(group: Element): boolean {
  return group.getAttribute("data-orientation") === "vertical";
}

function applyTemplate(el: Element, group: HTMLElement): void {
  const sizes = kids(group).map((n) => n.getAttribute("data-size") || "1fr");
  const tpl = sizes.join(" " + barSize(el) + "px ");
  group.style.gridTemplateColumns = vert(group) ? tpl : "";
  group.style.gridTemplateRows = vert(group) ? "" : tpl;
}

function measure(node: Element, v: boolean): number {
  const r = node.getBoundingClientRect();
  return v ? r.width : r.height;
}

function fr(px: number, total: number): string {
  return total > 0 ? (Math.round(px / total * 10000) / 100) + "fr" : "1fr";
}

// Write sizes (px, one per child) as proportional fr tracks.
function setSizes(el: Element, group: HTMLElement, px: number[]): void {
  const total = px.reduce((a, b) => a + b, 0);
  kids(group).forEach((n, i) => { n.setAttribute("data-size", fr(px[i], total)); });
  applyTemplate(el, group);
}

function sizesOf(group: Element): number[] {
  const v = vert(group);
  return kids(group).map((n) => measure(n, v));
}

function splitbar(v: boolean): HTMLElement {
  return div("ah-tl-splitbar " + (v ? "ah-tl-splitbar-v" : "ah-tl-splitbar-h"),
             { role: "separator", "aria-orientation": v ? "vertical" : "horizontal",
               "aria-label": "Resize", tabindex: "0" });
}

function newGroup(v: boolean): HTMLElement {
  return div("ah-tl-group " + (v ? "ah-tl-vertical" : "ah-tl-horizontal"),
             { "data-id": Ids.next(), "data-type": "layout-group",
               "data-orientation": v ? "vertical" : "horizontal" });
}

function newTabGroup(): HTMLElement {
  const tg = div("ah-tl-tab-group", { "data-id": Ids.next(), "data-type": "tab-group" });
  tg.appendChild(div("ah-tl-tab-strip", { role: "tablist", "aria-orientation": "horizontal" }));
  return tg;
}

// ------------------------------------------------------------------
// Tabs
// ------------------------------------------------------------------

// A tab group has its strip (server markup, newTabGroup).
function strip(tg: Element): HTMLElement {
  return tg.querySelector<HTMLElement>(":scope > .ah-tl-tab-strip") as HTMLElement;
}
function tabsOf(tg: Element): HTMLElement[] {
  const s = tg.querySelector(":scope > .ah-tl-tab-strip");
  return s ? (Array.from(s.children) as HTMLElement[]).filter((n) => n.classList.contains("ah-tl-tab")) : [];
}
function tabId(tab: Element): string | null { return tab.getAttribute("data-tab-id"); }
// A tab is inside its tab group (server markup).
function tabGroupOf(tab: Element): HTMLElement { return tab.closest<HTMLElement>(".ah-tl-tab-group") as HTMLElement; }
function contents(tg: Element): HTMLElement[] {
  return (Array.from(tg.children) as HTMLElement[]).filter((n) => n.classList.contains("ah-tl-tab-content"));
}

function panelOf(tg: Element, id: string | null): HTMLElement | undefined {
  return contents(tg).find((n) => n.getAttribute("data-id") === id);
}

function allows(node: Element, what: string): boolean {
  const m = node.getAttribute("data-modifiers");
  return m === null || m.split(",").indexOf(what) >= 0;
}

function selectTab(tg: Element, tab: Element | null): void {
  tabsOf(tg).forEach((t) => {
    const on = t === tab;
    t.classList.toggle("ah-tl-tab-selected", on);
    t.setAttribute("aria-selected", String(on));
    t.setAttribute("tabindex", on ? "0" : "-1");
  });
  const id = tab && tabId(tab);
  contents(tg).forEach((c) => {
    c.classList.toggle("ah-tl-tab-content-active", c.getAttribute("data-id") === id);
  });
}

function findTab(el: Element, id: unknown): HTMLElement | undefined {
  return own(el, ".ah-tl-tab").find((t) => tabId(t) === String(id));
}

// ------------------------------------------------------------------
// The arrangement (keys in the order the server's JSON encoder uses)
// ------------------------------------------------------------------

function nodeValue(n: HTMLElement): TileNode {
  const t = n.getAttribute("data-type");
  const size = n.getAttribute("data-size");
  const id = n.getAttribute("data-id");
  if (t === "layout-group") {
    const items = kids(n).map(nodeValue);
    const type = vert(n) ? "columns" : "rows";
    return size ? { id: id, items: items, size: size, type: type } : { id: id, items: items, type: type };
  }
  if (t === "tab-group") {
    const sel = tabsOf(n).find((x) => x.classList.contains("ah-tl-tab-selected"));
    const active = sel ? tabId(sel) || "" : "";
    const tabs = tabsOf(n).map(tabId);
    return size ? { active: active, id: id, size: size, tabs: tabs, type: "tabs" }
                : { active: active, id: id, tabs: tabs, type: "tabs" };
  }
  return size ? { id: id, size: size, type: "item" } : { id: id, type: "item" };
}

/** The closed tab ids from the value the server rendered. */
function parseClosed(json: string | null): string[] {
  try {
    const v: unknown = JSON.parse(json || "{}");
    if (typeof v === "object" && v !== null && "closed" in v && Array.isArray(v.closed)) {
      return v.closed.map(String);
    }
    return [];
  } catch (e) {
    return [];
  }
}

// ------------------------------------------------------------------
// Dropping a tab
// ------------------------------------------------------------------

// sigil's five zones: the outer quarter of each side, else the centre.
function zoneAt(x: number, y: number, r: DOMRect): Zone {
  const rx = x - r.left, ry = y - r.top;
  if (ry < r.height * 0.25) { return "top"; }
  if (ry > r.height * 0.75) { return "bottom"; }
  if (rx < r.width * 0.25) { return "left"; }
  if (rx > r.width * 0.75) { return "right"; }
  return "center";
}

function targetAt(el: Element, x: number, y: number): HTMLElement | null {
  const hits = own(el, ".ah-tl-item, .ah-tl-tab-group").filter((n) => {
    const r = n.getBoundingClientRect();
    return x >= r.left && x <= r.right && y >= r.top && y <= r.bottom;
  });
  return hits[hits.length - 1] || null;
}

function showIndicator(el: Element, zone: Zone, target: Element): void {
  hideIndicator(el);
  const r = target.getBoundingClientRect(), o = el.getBoundingClientRect();
  let left = r.left - o.left, top = r.top - o.top, w = r.width, h = r.height;
  if (zone === "top") { h = h / 2; }
  if (zone === "bottom") { top += h / 2; h = h / 2; }
  if (zone === "left") { w = w / 2; }
  if (zone === "right") { left += w / 2; w = w / 2; }
  const area = div("ah-tl-drop-area");
  Object.assign(area.style, { left: left + "px", top: top + "px", width: w + "px", height: h + "px" });
  el.appendChild(area);
}

function hideIndicator(el: Element): void {
  el.querySelectorAll(":scope > .ah-tl-drop-area").forEach((n) => { n.remove(); });
}

// A plain tile becomes a tab group holding it as its first tab.
function wrapItem(el: Element, item: HTMLElement): HTMLElement {
  const tg = newTabGroup();
  const id = item.getAttribute("data-id") || "";
  ["data-size", "data-min", "data-resize"].forEach((a) => {
    const v = item.getAttribute(a);
    if (v !== null) { tg.setAttribute(a, v); }
  });
  strip(tg).insertAdjacentHTML("beforeend", AH.tpl.tile_layout_tab({
    selected: false, dom_id: el.id + "-tab-" + id, id: id, modifiers: "drag,close",
    panel_id: el.id + "-panel-" + id, aria_selected: "false", tabindex: "-1",
    label: item.getAttribute("data-label") || id, close: true
  }));
  const panel = div("ah-tl-tab-content",
                    { id: el.id + "-panel-" + id, "data-id": id, role: "tabpanel",
                      "aria-labelledby": el.id + "-tab-" + id });
  while (item.firstChild) { panel.appendChild(item.firstChild); }
  tg.appendChild(panel);
  item.replaceWith(tg);
  return tg;
}

// Resizing: the two panes around a splitbar trade room, within their
// minimum sizes; the group's tracks become proportional fr values.
function resizeStart(el: Element, bar: HTMLElement, coord: number): Resize | null {
  const group = bar.parentElement;
  const prev = bar.previousElementSibling, next = bar.nextElementSibling;
  if (disabled(el) || !isGroup(group) || !prev || !next ||
      prev.getAttribute("data-resize") === "false" || next.getAttribute("data-resize") === "false") {
    return null;
  }
  const k = kids(group);
  return {
    bar: bar, group: group, start: coord, px: sizesOf(group),
    i: k.indexOf(prev as HTMLElement), moved: false,
    saved: k.map((n) => n.getAttribute("data-size")),
    minPrev: parseInt(prev.getAttribute("data-min") || "", 10) || MIN,
    minNext: parseInt(next.getAttribute("data-min") || "", 10) || MIN
  };
}

function resizeTo(el: Element, r: Resize, delta: number): void {
  const a = r.px[r.i], b = r.px[r.i + 1];
  delta = Math.max(-(a - r.minPrev), Math.min(delta, b - r.minNext));
  const px = r.px.slice();
  px[r.i] = a + delta;
  px[r.i + 1] = b - delta;
  setSizes(el, r.group, px);
  r.moved = r.moved || delta !== 0;
}

function resizeCancel(el: Element, r: Resize): void {
  kids(r.group).forEach((n, i) => {
    const s = r.saved[i];
    if (s === null || s === undefined) { n.removeAttribute("data-size"); }
    else { n.setAttribute("data-size", s); }
  });
  applyTemplate(el, r.group);
  r.bar.classList.remove("ah-tl-splitbar-active");
}

// ------------------------------------------------------------------
// Behaviour
// ------------------------------------------------------------------

class TileLayoutController extends AH.Controller {
  /** Ids of the closed tabs (part of the value). */
  #closed: string[] = [];
  #drag: TabDrag | null = null;
  #resize: Resize | null = null;

  override setup(): void {
    const el = this.element;
    this.#closed = parseClosed(el.getAttribute("data-ah-value"));
    this.#drag = null;
    this.#resize = null;
    this.delegate("click", ".ah-tl-tab-close", (e, btn) => {
      e.stopPropagation();
      if (disabled(el)) { return; }
      const tab = btn.closest<HTMLElement>(".ah-tl-tab");
      if (tab && mine(el, tab) && allows(tab, "close")) { this.closeTab(tab); }
    });
    this.delegate("click", ".ah-tl-tab", (_e, tab) => {
      if (!mine(el, tab) || disabled(el)) { return; }
      const tg = tabGroupOf(tab);
      const changed = !tab.classList.contains("ah-tl-tab-selected");
      selectTab(tg, tab);
      if (changed) {
        this.commit();
        this.fire<TileTabEvent>("ah:tab-select", tabId(tab));
      }
    });
    this.delegate("keydown", ".ah-tl-tab", (e, tab) => {
      if (!mine(el, tab) || disabled(el)) { return; }
      const tg = tabGroupOf(tab);
      const side = /\bah-tl-tab-group-(left|right)\b/.test(tg.className);
      const ts = tabsOf(tg), i = ts.indexOf(tab);
      let next = 0;
      switch (e.key) {
        case side ? "ArrowUp" : "ArrowLeft": next = (i - 1 + ts.length) % ts.length; break;
        case side ? "ArrowDown" : "ArrowRight": next = (i + 1) % ts.length; break;
        case "Home": next = 0; break;
        case "End": next = ts.length - 1; break;
        case "Delete":
          if (allows(tab, "close")) { e.preventDefault(); this.closeTab(tab); }
          return;
        case "Enter": case " ": next = i; break;
        default: return;
      }
      e.preventDefault();
      const t = ts[next];
      t.focus();
      if (!t.classList.contains("ah-tl-tab-selected")) {
        selectTab(tg, t);
        this.commit();
        this.fire<TileTabEvent>("ah:tab-select", tabId(t));
      }
    });
    // the pane last pressed is outlined (sigil's selection)
    this.listen(el, "pointerdown", (e) => {
      const pane = e.target instanceof Element ? e.target.closest(".ah-tl-item, .ah-tl-tab-group") : null;
      if (!pane || !mine(el, pane)) { return; }
      own(el, "[data-tl-selected]").forEach((n) => { n.removeAttribute("data-tl-selected"); });
      pane.setAttribute("data-tl-selected", "true");
    });
    this.delegate("pointerdown", ".ah-tl-tab", (e, tab) => {
      if (mine(el, tab)) { this.tabDown(tab, e); }
    });
    this.delegate("pointerdown", ".ah-tl-splitbar", (e, bar) => {
      const group = bar.parentElement;
      if (!mine(el, bar) || e.button !== 0 || !group) { return; }
      const r = resizeStart(el, bar, vert(group) ? e.clientX : e.clientY);
      if (!r) { return; }
      e.preventDefault();
      bar.classList.add("ah-tl-splitbar-active");
      this.#resize = r;
    });
    this.delegate("keydown", ".ah-tl-splitbar", (e, bar) => {
      const group = bar.parentElement;
      if (!mine(el, bar) || !group) { return; }
      const v = vert(group);
      const step = e.shiftKey ? 50 : 10;
      const keys: Record<string, number> = { ArrowLeft: v ? -step : 0, ArrowRight: v ? step : 0,
                                             ArrowUp: v ? 0 : -step, ArrowDown: v ? 0 : step };
      const d = keys[e.key];
      if (!d) { return; }
      e.preventDefault();
      const r = resizeStart(el, bar, 0);
      if (!r) { return; }
      resizeTo(el, r, d);
      if (r.moved) { this.commit(); }
    });
    this.listen(document, "pointermove", (e) => {
      const r = this.#resize;
      if (r) {
        resizeTo(el, r, (vert(r.group) ? e.clientX : e.clientY) - r.start);
      } else {
        this.dragMove(e);
      }
    });
    const up = (e: PointerEvent): void => {
      const r = this.#resize;
      if (r) {
        this.#resize = null;
        r.bar.classList.remove("ah-tl-splitbar-active");
        if (r.moved) { this.commit(); }
      } else {
        this.dragEnd(e.type === "pointercancel");
      }
    };
    this.listen(document, "pointerup", up);
    this.listen(document, "pointercancel", up);
    this.listen(document, "keydown", (e) => {
      if (e.key !== "Escape") { return; }
      if (this.#resize) { resizeCancel(el, this.#resize); this.#resize = null; }
      if (this.#drag) { this.dragEnd(true); }
    });
  }

  override teardown(): void {
    if (this.#drag) { this.dragEnd(true); }
    this.#resize = null;
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue(): TileLayoutValue { return this.value(); }
  select(id: string): void {
    const tab = findTab(this.element, id);
    if (tab) {
      selectTab(tabGroupOf(tab), tab);
      this.sync();
    }
  }
  close(id: string): void {
    const tab = findTab(this.element, id);
    if (tab) { this.closeTab(tab); }
  }

  // ---- the value ----

  private value(): TileLayoutValue {
    const root = this.element.querySelector<HTMLElement>(":scope > [data-type]");
    const closed = this.#closed.slice().sort();
    return { closed: closed, root: root ? nodeValue(root) : null };
  }

  private sync(): void {
    const el = this.element;
    const v = JSON.stringify(this.value());
    el.setAttribute("data-ah-value", v);
    el.querySelectorAll<HTMLInputElement>(":scope > input[type=hidden]").forEach((i) => { i.value = v; });
  }

  private commit(): void {
    this.sync();
    emit(this.element, "change");
  }

  // ---- removing nodes and tidying up ----

  private removeNode(n: HTMLElement): void {
    const el = this.element, parent = n.parentNode;
    if (isGroup(parent)) {
      setSizes(el, parent, sizesOf(parent));
      const prev = n.previousElementSibling, next = n.nextElementSibling;
      if (prev && prev.classList.contains("ah-tl-splitbar")) {
        prev.remove();
      } else if (next && next.classList.contains("ah-tl-splitbar")) {
        next.remove();
      }
      n.remove();
      applyTemplate(el, parent);
    } else {
      n.remove();
    }
  }

  // Empty tab groups and groups go; a group with one child gives way to it.
  private cleanup(): void {
    const el = this.element;
    let again = true;
    while (again) {
      again = false;
      own(el, ".ah-tl-tab-group").forEach((tg) => {
        if (!tabsOf(tg).length) { this.removeNode(tg); again = true; }
      });
      own(el, ".ah-tl-group").forEach((g) => {
        if (!g.isConnected) { return; }
        const k = kids(g);
        if (k.length === 0) {
          this.removeNode(g);
          again = true;
        } else if (k.length === 1) {
          const child = k[0];
          const size = g.getAttribute("data-size");
          if (size) { child.setAttribute("data-size", size); } else { child.removeAttribute("data-size"); }
          const parent = g.parentNode;
          g.replaceWith(child);
          if (isGroup(parent)) { applyTemplate(el, parent); }
          again = true;
        }
      });
    }
  }

  private closeTab(tab: HTMLElement): void {
    const tg = tabGroupOf(tab);
    const id = tabId(tab);
    const wasSel = tab.classList.contains("ah-tl-tab-selected");
    const i = tabsOf(tg).indexOf(tab);
    const panel = panelOf(tg, id);
    if (panel) { panel.remove(); }
    tab.remove();
    if (id !== null && this.#closed.indexOf(id) < 0) { this.#closed.push(id); }
    const rest = tabsOf(tg);
    if (wasSel && rest.length) {
      const next = rest[Math.min(i, rest.length - 1)];
      selectTab(tg, next);
      const a = document.activeElement;
      if ((a !== tg && tg.contains(a)) || a === document.body) {
        next.focus();
      }
    }
    this.cleanup();
    this.commit();
    emit(this.element, "ah:tab-close", id);
  }

  // ---- dropping a tab ----

  private drop(tab: HTMLElement, zone: Zone, target: HTMLElement): boolean {
    const el = this.element;
    const src = tabGroupOf(tab);
    if (target === src && (zone === "center" || tabsOf(src).length === 1)) { return false; }
    const id = tabId(tab);
    const panel = panelOf(src, id);
    const wasSel = tab.classList.contains("ah-tl-tab-selected");
    tab.remove();
    if (panel) { panel.remove(); }
    if (wasSel && tabsOf(src).length) { selectTab(src, tabsOf(src)[0]); }
    let into: HTMLElement;
    if (zone === "center") {
      into = isTabGroup(target) ? target : wrapItem(el, target);
    } else {
      into = newTabGroup();
      const needVert = zone === "left" || zone === "right";
      const before = zone === "left" || zone === "top";
      const parent = target.parentNode;
      if (isGroup(parent) && vert(parent) === needVert) {
        // same axis: the new group takes half of the target's room
        const px = sizesOf(parent);
        const at = kids(parent).indexOf(target);
        const half = px[at] / 2;
        px.splice(at, 1, half, half);
        if (before) {
          target.before(into, splitbar(needVert));
        } else {
          target.after(splitbar(needVert), into);
          px.splice(at, 2, half, half);
        }
        setSizes(el, parent, px);
      } else {
        // across: target and new group share a new group in its place
        const g = newGroup(needVert);
        const size = target.getAttribute("data-size");
        if (size !== null) {
          g.setAttribute("data-size", size);
          target.removeAttribute("data-size");
        }
        target.replaceWith(g);
        if (before) { g.append(into, splitbar(needVert), target); }
        else { g.append(target, splitbar(needVert), into); }
        applyTemplate(el, g);
        if (isGroup(parent)) { applyTemplate(el, parent); }
      }
    }
    strip(into).appendChild(tab);
    if (panel) { into.appendChild(panel); }
    selectTab(into, tab);
    this.cleanup();
    this.commit();
    return true;
  }

  // ---- pointer interactions ----

  private tabDown(tab: HTMLElement, e: PointerEvent): void {
    if (disabled(this.element) || e.button !== 0 ||
        (e.target instanceof Element && e.target.closest(".ah-tl-tab-close"))) { return; }
    const tg = tabGroupOf(tab);
    if (!allows(tab, "drag") || !allows(tg, "drag")) { return; }
    this.#drag = { tab: tab, x: e.clientX, y: e.clientY, started: false };
  }

  private dragMove(e: PointerEvent): void {
    const el = this.element, d = this.#drag;
    if (!d) { return; }
    if (!d.started) {
      if (Math.sqrt(Math.pow(e.clientX - d.x, 2) + Math.pow(e.clientY - d.y, 2)) <= 5) { return; }
      d.started = true;
      d.feedback = div("ah-tl-feedback");
      const lbl = d.tab.querySelector(".ah-tl-tab-label");
      d.feedback.textContent = lbl ? lbl.textContent : "";
      document.body.appendChild(d.feedback);
      d.overlay = div("ah-tl-overlay");
      document.body.appendChild(d.overlay);
    }
    if (d.feedback) {
      d.feedback.style.left = (e.clientX + 10) + "px";
      d.feedback.style.top = (e.clientY + 10) + "px";
    }
    const t = targetAt(el, e.clientX, e.clientY);
    if (t) {
      d.target = t;
      d.zone = zoneAt(e.clientX, e.clientY, t.getBoundingClientRect());
      showIndicator(el, d.zone, t);
    } else {
      d.target = d.zone = null;
      hideIndicator(el);
    }
  }

  private dragEnd(cancel: boolean): void {
    const d = this.#drag;
    this.#drag = null;
    if (!d) { return; }
    if (d.feedback) { d.feedback.remove(); }
    if (d.overlay) { d.overlay.remove(); }
    hideIndicator(this.element);
    if (!cancel && d.started && d.target && d.zone) { this.drop(d.tab, d.zone, d.target); }
  }
}

AH.register("tile-layout", TileLayoutController);
