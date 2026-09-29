/* Internal: what the docking behaviours share (docking.ts,
 * dock_layout.ts), ported from sigil's layout/docking and
 * layout/dock_layout. Pointer events (mouse, pen, touch) instead of
 * sigil's mouse sequences; one drag at a time per component (DockDrag),
 * tracked on the document and unbound when it ends or the component is
 * torn down. The arrangement is the value: JSON in data-ah-value (and
 * the hidden input), rewritten after every change (commit).
 */
import AH from "../core.ts";

const DISTANCE = 5;          // px of movement before a press becomes a drag

/** A rectangle (DOMRect or the like). */
export interface Box { left: number; right: number; top: number; bottom: number; }

export function inside(x: number, y: number, r: Box): boolean {
  return x >= r.left && x <= r.right && y >= r.top && y <= r.bottom;
}

/** JSON.parse, null for nothing or bad JSON. */
export function parse(json: string | null | undefined): unknown {
  try { return JSON.parse(json || "null") as unknown; } catch (e) { return null; }
}

/** A native bubbling, cancelable event on el. */
export function emit(el: EventTarget, type: string, detail?: unknown): boolean {
  return el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
}

/** The element children of node matching selector. */
export function kids<E extends Element = HTMLElement>(node: Element | null | undefined, selector?: string): E[] {
  return node ? (Array.from(node.children) as E[]).filter((c) => !selector || c.matches(selector)) : [];
}

/** Write the value (data-ah-value and the hidden input); change on el
 *  when fire. False when it did not change. */
export function commit(el: Element, json: string, fire: boolean): boolean {
  if (json === el.getAttribute("data-ah-value")) { return false; }
  el.setAttribute("data-ah-value", json);
  kids<HTMLInputElement>(el, "input[type=hidden]").forEach((i) => { i.value = json; });
  if (fire) { emit(el, "change"); }
  return true;
}

/** A component event with its payload in data-* attributes of the root,
 *  which the action receives as Event.data (the value is also the
 *  event's detail). */
export function fireData(el: Element, type: string, key: string, value: string): void {
  el.setAttribute("data-" + key, value);
  emit(el, type, value);
}

/** node belongs to el and not to a nested component matching rootSel. */
export function own(el: Element, node: Element, rootSel: string): boolean {
  return node.closest(rootSel) === el;
}

/** Parse trusted HTML (a server render or a shared template) into nodes. */
export function parseHTML(html: string): HTMLElement[] {
  const t = document.createElement("template");
  t.innerHTML = String(html).trim();
  return Array.from(t.content.children) as HTMLElement[];
}

/** Remove an element the way the runtime does (behaviours destroyed first). */
export function drop(node: Element | null | undefined): void {
  if (!node) { return; }
  AH.destroy(node);
  node.remove();
}

/** One counter for the ids of new groups, float windows and menus. */
export class DockIds {
  private static seq = 0;
  static next(): number { return ++DockIds.seq; }
}

/** What a drag does. start returns false to give up; end gets the
 *  release (null for Escape) and whether the drag was cancelled. */
export interface DragHandlers {
  start(ev: PointerEvent): boolean | void;
  move(ev: PointerEvent): void;
  end(ev: PointerEvent | null, cancelled: boolean): void;
}

/**
 * One pointer press followed on the document: `start' after DISTANCE px
 * (or at once with threshold 0), then `move', then `end' on release,
 * pointercancel or Escape. One drag per owner (the component root): a
 * new one cancels the old.
 */
export class DockDrag {
  private static drags = new WeakMap<Element, DockDrag>();   // owner -> its drag

  private readonly ac = new AbortController();
  private started = false;

  private constructor(private readonly owner: Element, private readonly h: DragHandlers) {}

  static track(owner: Element, e: PointerEvent, h: DragHandlers, threshold?: number): void {
    const prev = DockDrag.drags.get(owner);
    if (prev) { prev.cancel(); }
    const d = new DockDrag(owner, h);
    DockDrag.drags.set(owner, d);
    d.follow(e, threshold === undefined ? DISTANCE : threshold);
  }

  /** Cancel the drag of owner, if any. */
  static stop(owner: Element): void {
    const d = DockDrag.drags.get(owner);
    if (d) { d.cancel(); }
  }

  private follow(e: PointerEvent, min: number): void {
    const sx = e.clientX, sy = e.clientY;
    const opts = { signal: this.ac.signal };
    if (min === 0 && !this.begin(e)) { return; }
    document.addEventListener("pointermove", (ev) => {
      if (!this.started) {
        if (Math.abs(ev.clientX - sx) < min && Math.abs(ev.clientY - sy) < min) { return; }
        if (!this.begin(ev)) { return; }
      }
      ev.preventDefault();
      this.h.move(ev);
    }, opts);
    const up = (ev: PointerEvent): void => {
      this.release();
      if (this.started) { this.h.end(ev, ev.type === "pointercancel"); }
    };
    document.addEventListener("pointerup", up, opts);
    document.addEventListener("pointercancel", up, opts);
    document.addEventListener("keydown", (ev) => {
      if (ev.key === "Escape" && this.started) {
        ev.preventDefault();
        this.release();
        this.h.end(null, true);
      }
    }, opts);
  }

  private begin(ev: PointerEvent): boolean {
    this.started = true;
    if (this.h.start(ev) === false) { this.release(); return false; }
    document.documentElement.classList.add("ah-dock-dragging");
    return true;
  }

  private release(): void {
    this.ac.abort();
    document.documentElement.classList.remove("ah-dock-dragging");
    if (DockDrag.drags.get(this.owner) === this) { DockDrag.drags.delete(this.owner); }
  }

  private cancel(): void {
    this.release();
    if (this.started) { this.h.end(null, true); }
  }
}
