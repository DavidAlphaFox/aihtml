/* The docking behaviour (designs/04-components.md), ported from sigil's
 * layout/docking (+ docking/drag): windows dragged by their header between
 * panels, left floating, collapsed, closed; Alt+arrows move the focused
 * window from the keyboard.
 *
 * The arrangement is the value: JSON in data-ah-value (and the hidden
 * input), rewritten after every change, and `change' on the root when it
 * changed (user actions and the rearranging methods; setLayout excepted).
 * Windows are only ever moved, never rebuilt, so their contents keep
 * their state. The drag tracking is shared with dock_layout.ts
 * (_lib_dock.ts).
 *
 * Events: "ah:window-close", "ah:window-collapse", "ah:window-expand" on
 * the root with the window id in data-window (and as the detail, a
 * string: DockingWindowEvent).
 */
import AH from "../core.ts";
import { commit, DockDrag, drop, fireData, inside, kids, own, parse, parseHTML } from "./_lib_dock.ts";

/** Detail of ah:window-close / -collapse / -expand: the window id. */
export type DockingWindowEvent = string;

/** The value (data-ah-value). */
export interface DockingValue {
  panels: { id: string | null; windows: (string | null)[] }[];
  floating: { id: string | null; x: number; y: number; width: number }[];
  collapsed: (string | null)[];
  closed: string[];
}

/** An arrangement for setLayout: every part optional. */
interface LayoutInput {
  panels: { id: string; windows: string[] }[];
  floating: { id: string; x: number; y: number; width: number | undefined }[];
  collapsed: string[];
  closed: string[];
}

/** Where a dragged window came from (to put it back on a cancel). */
interface Origin { parent: ParentNode | null; next: ChildNode | null; floating: boolean; style: string | null; }

const WIN = ".ah-docking-window";

function isObj(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}

function list(v: unknown): unknown[] { return Array.isArray(v) ? v : []; }

function num(v: unknown): number {
  return typeof v === "number" ? v : (typeof v === "string" && v !== "" && !isNaN(Number(v)) ? Number(v) : 0);
}

/** A layout from JSON (or an object from the server); what does not fit
 *  is skipped. */
function parseLayout(v: unknown): LayoutInput | null {
  if (!isObj(v)) { return null; }
  return {
    panels: list(v.panels).filter(isObj).map((p) => ({ id: String(p.id), windows: list(p.windows).map(String) })),
    floating: list(v.floating).filter(isObj).map((f) => ({
      id: String(f.id), x: num(f.x), y: num(f.y),
      width: f.width === undefined || f.width === null ? undefined : num(f.width) })),
    collapsed: list(v.collapsed).map(String),
    closed: list(v.closed).map(String)
  };
}

function dkPanels(el: Element): HTMLElement[] { return kids(el, ".ah-docking-panel"); }
function dkWid(win: Element): string | null { return win.getAttribute("data-window-id"); }
function panelWins(panel: Element, skip?: Element): HTMLElement[] {
  return kids(panel, WIN).filter((w) => w !== skip);
}

function dkWindow(el: Element, id: unknown): HTMLElement | null {
  return Array.from(el.querySelectorAll<HTMLElement>(WIN)).find((w) =>
    dkWid(w) === String(id) && w.closest(".ah-docking") === el) || null;
}

function dkPanel(el: Element, id: unknown): HTMLElement | null {
  return dkPanels(el).find((p) => p.getAttribute("data-panel-id") === String(id)) || null;
}

function dkDock(win: HTMLElement): void {
  win.classList.remove("ah-docking-window-floating");
  win.classList.add("ah-docking-window-docked");
  Object.assign(win.style, { left: "", top: "", width: "", zIndex: "", opacity: "" });
}

function dkFloat(el: Element, win: HTMLElement, x: number, y: number, w?: number): void {
  win.classList.remove("ah-docking-window-docked");
  win.classList.add("ah-docking-window-floating");
  win.style.left = Math.round(x) + "px";
  win.style.top = Math.round(y) + "px";
  if (w) { win.style.width = Math.round(w) + "px"; }
  if (win.parentNode !== el) { el.appendChild(win); }
}

// Insert a window into a panel at an index (-1 or past the end: last).
function dkInsert(panel: Element, win: HTMLElement, index: number): void {
  const ws = panelWins(panel, win);
  dkDock(win);
  if (index < 0 || index >= ws.length) { panel.appendChild(win); }
  else { panel.insertBefore(win, ws[index]); }
}

function dkIndexAt(panel: Element, y: number, skip: Element): number {
  const ws = panelWins(panel, skip);
  for (let i = 0; i < ws.length; i++) {
    const r = ws[i].getBoundingClientRect();
    if (y < r.top + r.height / 2) { return i; }
  }
  return ws.length;
}

function setCollapsed(win: Element, on: boolean): void {
  win.classList.toggle("ah-docking-window-collapsed", on);
  const btn = win.querySelector(".ah-docking-window-collapse-btn");
  if (btn) { btn.setAttribute("aria-expanded", on ? "false" : "true"); }
}

class DockingController extends AH.Controller {
  /** Ids of the closed windows (part of the value). */
  #closed: string[] = [];

  override setup(): void {
    const el = this.element;
    const v = parse(el.getAttribute("data-ah-value"));
    this.#closed = isObj(v) && Array.isArray(v.closed) ? v.closed.map(String) : [];
    const live = (node: Element): boolean => own(el, node, ".ah-docking") && !this.disabled();
    this.delegate("pointerdown", ".ah-docking-window-header", (e, hd) => {
      // the header is a child of its window (server markup)
      const win = hd.parentElement as HTMLElement;
      if (e.button !== 0 || !live(hd) || win.classList.contains("ah-docking-window-pinned") ||
          (e.target as Element).closest("button, a, input, select, textarea")) { return; }
      this.drag(win, e);
    });
    this.delegate("click", ".ah-docking-window-collapse-btn", (e, btn) => {
      const win = btn.closest<HTMLElement>(WIN);
      if (!live(btn) || !win) { return; }
      e.stopPropagation();
      this.setCollapse(win, !win.classList.contains("ah-docking-window-collapsed"), true);
    });
    this.delegate("click", ".ah-docking-window-close-btn", (e, btn) => {
      const win = btn.closest<HTMLElement>(WIN);
      if (!live(btn) || !win) { return; }
      e.stopPropagation();
      this.closeWin(win, true);
    });
    this.delegate("keydown", ".ah-docking-window-header", (e, hd) => {
      if (e.target !== hd || !live(hd)) { return; }
      this.key(hd.parentElement as HTMLElement, e);
    });
    this.commit(false);
  }

  override teardown(): void { DockDrag.stop(this.element); }

  // methods (aihtml_action:call/4, AH.invoke)
  collapse(id: string): void { const w = dkWindow(this.element, id); if (w) { this.setCollapse(w, true, false); } }
  expand(id: string): void { const w = dkWindow(this.element, id); if (w) { this.setCollapse(w, false, false); } }
  close(id: string): void { const w = dkWindow(this.element, id); if (w) { this.closeWin(w, false); } }
  move(id: string, panel: string | number, index?: number): void {
    const el = this.element, w = dkWindow(el, id);
    const p = typeof panel === "number" ? dkPanels(el)[panel] : dkPanel(el, panel);
    if (!w || !p) { return; }
    dkInsert(p, w, index === undefined ? -1 : index);
    this.commit(true);
  }
  pin(id: string): void { const w = dkWindow(this.element, id); if (w) { w.classList.add("ah-docking-window-pinned"); } }
  unpin(id: string): void { const w = dkWindow(this.element, id); if (w) { w.classList.remove("ah-docking-window-pinned"); } }
  addWindow(panel: string, html: string): void {
    const el = this.element, p = dkPanel(el, panel);
    const w = parseHTML(html).find((n) => n.matches(WIN));
    if (!p || !w) { return; }
    const old = dkWindow(el, dkWid(w));
    if (old) { drop(old); }
    p.appendChild(w);
    AH.mount(w);
    const i = this.#closed.indexOf(String(dkWid(w)));
    if (i >= 0) { this.#closed.splice(i, 1); }
    this.commit(true);
  }
  setLayout(json: string | object): void {
    this.apply(parseLayout(typeof json === "string" ? parse(json) : json));
    this.commit(false);
  }
  disable(): void {
    this.element.classList.add("ah-docking-disabled");
    this.element.setAttribute("aria-disabled", "true");
  }
  enable(): void {
    this.element.classList.remove("ah-docking-disabled");
    this.element.removeAttribute("aria-disabled");
  }
  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }

  // internals
  private disabled(): boolean { return this.element.classList.contains("ah-docking-disabled"); }
  private commit(fire: boolean): boolean { return commit(this.element, this.serialize(), fire); }

  private serialize(): string {
    const el = this.element;
    const v: DockingValue = { panels: [], floating: [], collapsed: [], closed: this.#closed };
    dkPanels(el).forEach((p) => {
      v.panels.push({ id: p.getAttribute("data-panel-id"), windows: kids(p, WIN).map(dkWid) });
    });
    kids(el, ".ah-docking-window-floating").forEach((w) => {
      v.floating.push({ id: dkWid(w), x: Math.round(parseFloat(w.style.left) || 0),
                        y: Math.round(parseFloat(w.style.top) || 0),
                        width: Math.round(parseFloat(w.style.width) || w.offsetWidth) });
    });
    // in document order
    const coll: HTMLElement[] = [];
    dkPanels(el).forEach((p) => { coll.push(...kids(p, ".ah-docking-window-collapsed")); });
    coll.push(...kids(el, ".ah-docking-window-floating.ah-docking-window-collapsed"));
    coll.sort((a, b) => (a.compareDocumentPosition(b) & Node.DOCUMENT_POSITION_FOLLOWING) ? -1 : 1);
    coll.forEach((w) => { v.collapsed.push(dkWid(w)); });
    return JSON.stringify(v);
  }

  private announce(text: string): void {
    const live = kids(this.element, ".ah-docking-live");
    live.forEach((n) => { n.textContent = ""; });
    setTimeout(() => { live.forEach((n) => { n.textContent = text; }); }, 20);
  }

  private setCollapse(win: HTMLElement, on: boolean, user: boolean): void {
    if (win.classList.contains("ah-docking-window-collapsed") === on) { return; }
    setCollapsed(win, on);
    if (user) {
      fireData(this.element, on ? "ah:window-collapse" : "ah:window-expand", "window", String(dkWid(win)));
    }
    this.commit(true);
  }

  private closeWin(win: HTMLElement, user: boolean): void {
    const el = this.element, id = String(dkWid(win));
    const focus = win.contains(document.activeElement);
    let next = win.nextElementSibling;
    while (next && !next.matches(WIN)) { next = next.nextElementSibling; }
    if (!next) {
      next = win.previousElementSibling;
      while (next && !next.matches(WIN)) { next = next.previousElementSibling; }
    }
    if (user) { fireData(el, "ah:window-close", "window", id); }
    drop(win);
    if (this.#closed.indexOf(id) < 0) { this.#closed.push(id); }
    if (focus && next) {
      const hd = next.querySelector<HTMLElement>(":scope > .ah-docking-window-header");
      if (hd) { hd.focus(); }
    }
    this.commit(true);
  }

  private drag(win: HTMLElement, e: PointerEvent): void {
    const root = this.element;
    let origin: Origin | null = null;
    let offX = 0, offY = 0, target: HTMLElement | null = null, index = 0;
    const ind = document.createElement("div");
    ind.className = "ah-docking-drop-indicator";
    ind.setAttribute("aria-hidden", "true");
    const opacity = parseFloat(root.getAttribute("data-ah-drag-opacity") || "");
    DockDrag.track(root, e, {
      start: () => {
        const r = win.getBoundingClientRect(), rr = root.getBoundingClientRect();
        origin = { parent: win.parentNode, next: win.nextSibling,
                   floating: win.classList.contains("ah-docking-window-floating"),
                   style: win.getAttribute("style") };
        offX = e.clientX - r.left;
        offY = e.clientY - r.top;
        dkFloat(root, win, r.left - rr.left - root.clientLeft, r.top - rr.top - root.clientTop, r.width);
        win.classList.add("ah-docking-window-dragging");
        win.style.zIndex = "9999";
        win.style.opacity = isNaN(opacity) ? "" : String(opacity);
      },
      move: (ev) => {
        const rr = root.getBoundingClientRect();
        win.style.left = Math.round(ev.clientX - offX - rr.left - root.clientLeft) + "px";
        win.style.top = Math.round(ev.clientY - offY - rr.top - root.clientTop) + "px";
        // the last panel under the pointer
        const hit = dkPanels(root).filter((p) => inside(ev.clientX, ev.clientY, p.getBoundingClientRect())).pop();
        target = hit || null;
        ind.remove();
        if (hit) {
          index = dkIndexAt(hit, ev.clientY, win);
          const ws = panelWins(hit, win);
          if (index < ws.length) { hit.insertBefore(ind, ws[index]); }
          else { hit.appendChild(ind); }
        }
      },
      end: (_ev, cancelled) => {
        ind.remove();
        win.classList.remove("ah-docking-window-dragging");
        win.style.zIndex = "";
        win.style.opacity = "";
        const allowFloat = root.getAttribute("data-ah-allow-float") !== "false";
        const o: Origin | null = origin;
        if (!cancelled && target) {
          dkInsert(target, win, index);
        } else if (!cancelled && allowFloat) {
          // keep the floating window (its header at least) inside the container
          win.style.left = Math.max(0, Math.min(parseFloat(win.style.left) || 0,
                                                root.clientWidth - 60)) + "px";
          win.style.top = Math.max(0, Math.min(parseFloat(win.style.top) || 0,
                                               root.clientHeight - 40)) + "px";
        } else if (o && o.floating) {
          win.setAttribute("style", o.style || "");
        } else if (o && o.parent) {
          const parent = o.parent;
          dkDock(win);
          parent.insertBefore(win, o.next && o.next.parentNode === parent ? o.next : null);
        }
        this.commit(true);
      }
    });
  }

  // Alt+arrows: move the focused window within its panel or to the next
  // panel; plain arrows move a floating window.
  private key(win: HTMLElement, e: KeyboardEvent): void {
    const el = this.element;
    const header = win.querySelector<HTMLElement>(":scope > .ah-docking-window-header");
    const k = e.key;
    let moved = false;
    if (k === "Delete" && win.querySelector(".ah-docking-window-close-btn")) {
      e.preventDefault();
      this.closeWin(win, true);
      return;
    }
    if (!/^Arrow/.test(k) || win.classList.contains("ah-docking-window-pinned")) { return; }
    const panels = dkPanels(el);
    if (win.classList.contains("ah-docking-window-floating")) {
      if (e.altKey) {
        dkInsert(panels[0], win, -1);
      } else {
        const dx = k === "ArrowLeft" ? -10 : k === "ArrowRight" ? 10 : 0;
        const dy = k === "ArrowUp" ? -10 : k === "ArrowDown" ? 10 : 0;
        win.style.left = ((parseFloat(win.style.left) || 0) + dx) + "px";
        win.style.top = ((parseFloat(win.style.top) || 0) + dy) + "px";
      }
      moved = true;
    } else if (e.altKey) {
      // a docked window is a child of its panel (server markup)
      const panel = win.parentElement as HTMLElement, pi = panels.indexOf(panel);
      const i = kids(panel, WIN).indexOf(win);
      const vertical = el.classList.contains("ah-docking-vertical");
      const along = vertical ? { prev: "ArrowLeft", next: "ArrowRight" } : { prev: "ArrowUp", next: "ArrowDown" };
      const across = vertical ? { prev: "ArrowUp", next: "ArrowDown" } : { prev: "ArrowLeft", next: "ArrowRight" };
      if (k === along.prev && i > 0) { dkInsert(panel, win, i - 1); moved = true; }
      else if (k === along.next) { dkInsert(panel, win, i + 1); moved = true; }
      else if (k === across.prev && pi > 0) { dkInsert(panels[pi - 1], win, i); moved = true; }
      else if (k === across.next && pi < panels.length - 1) { dkInsert(panels[pi + 1], win, i); moved = true; }
    }
    if (!moved) { return; }
    e.preventDefault();
    if (header) { header.focus(); }
    if (!win.classList.contains("ah-docking-window-floating")) {
      const p = win.parentElement as HTMLElement, title = win.querySelector(".ah-docking-window-title");
      this.announce((title ? title.textContent : "") + ": " +
                    (panels.indexOf(p) + 1) + " / " + (kids(p, WIN).indexOf(win) + 1));
    }
    this.commit(true);
  }

  private apply(v: LayoutInput | null): void {
    const el = this.element;
    if (!v) { return; }
    v.closed.forEach((id) => {
      const w = dkWindow(el, id);
      if (w) { drop(w); }
      if (this.#closed.indexOf(id) < 0) { this.#closed.push(id); }
    });
    v.panels.forEach((p) => {
      const panel = dkPanel(el, p.id);
      if (!panel) { return; }
      p.windows.forEach((id) => {
        const w = dkWindow(el, id);
        if (w) { dkInsert(panel, w, -1); }
      });
    });
    v.floating.forEach((f) => {
      const w = dkWindow(el, f.id);
      if (w) { dkFloat(el, w, f.x || 0, f.y || 0, f.width); }
    });
    const collapsed = v.collapsed;
    el.querySelectorAll(WIN).forEach((w) => {
      if (w.closest(".ah-docking") !== el) { return; }
      setCollapsed(w, collapsed.indexOf(String(dkWid(w))) >= 0);
    });
  }
}

AH.register("docking", DockingController);
