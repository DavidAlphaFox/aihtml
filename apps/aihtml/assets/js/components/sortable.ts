/* The sortable behaviour (designs/04-components.md), ported from sigil's
 * layout/sortable (+ sortable/geometry): reorder by dragging (or from the
 * keyboard); the value is the order of the item keys (data-value),
 * change after a drop that changed it; lists with the same data-ah-group
 * exchange items. The drag machinery is shared with dragdrop.ts
 * (_lib_dnd.ts).
 *
 * Events (native, bubbling) on the lists: ah:sort-start and ah:sort-stop
 * (detail SortPosition {key, index}), ah:sort-change, ah:sort-remove,
 * ah:sort-receive, ah:sort-cancel (detail SortKey {key}), change.
 */
import AH from "../core.ts";
import { announce, autoScroll, DISTANCE, Drag, editable, fire, floatingCopy, inside, label, pageRect,
         px, py, scrollParent, swallowClick } from "./_lib_dnd.ts";
import { join, split } from "./_lib_values.ts";

/** Detail of ah:sort-start and ah:sort-stop. */
export interface SortPosition { key: string; index: number; }
/** Detail of ah:sort-change, -remove, -receive and -cancel. */
export interface SortKey { key: string; }

type Layout = "grid" | "horizontal" | "vertical";

function soItems(list: Element): HTMLElement[] {
  return Array.from(list.querySelectorAll<HTMLElement>(":scope > .ah-sortable-item"));
}

function soKey(item: Element): string { return item.getAttribute("data-value") || ""; }

function soOrder(list: Element): string {
  return join(soItems(list).map(soKey));
}

function soDisabled(list: Element): boolean {
  return list.classList.contains("ah-sortable-disabled");
}

function soLive(list: Element): HTMLElement | null {
  return list.querySelector<HTMLElement>(":scope > .ah-sortable-live");
}

function soPublish(list: Element, notify: boolean): void {
  const v = soOrder(list);
  const old = list.getAttribute("data-ah-value") || "";
  list.setAttribute("data-ah-value", v);
  const hidden = list.querySelector<HTMLInputElement>(":scope > input[type=hidden]");
  if (hidden) { hidden.value = v; }
  if (notify && v !== old) { fire(list, "change"); }
}

// Roving tabindex: `item' (or the first item) is the one in the tab order.
function soRove(list: Element, item: HTMLElement | null): void {
  const items = soItems(list);
  if (!item || items.indexOf(item) < 0) {
    item = items.filter((it) => it.getAttribute("tabindex") === "0")[0] || items[0];
  }
  const off = soDisabled(list);
  items.forEach((it) => { it.setAttribute("tabindex", "-1"); });
  if (item && !off) { item.setAttribute("tabindex", "0"); }
}

function soLayout(list: Element): Layout {
  return list.classList.contains("ah-sortable-grid") ? "grid"
    : list.classList.contains("ah-sortable-horizontal") ? "horizontal" : "vertical";
}

// Where the placeholder goes for the pointer at (x, y): the item to insert
// before, or null for the end. Vertical lists compare with the middle
// of each item; horizontal lists and grids go in reading order (sigil's
// find-grid-insertion), which also handles wrapped rows.
function soInsertion(items: HTMLElement[], layout: Layout, x: number, y: number): HTMLElement | null {
  for (let i = 0; i < items.length; i++) {
    const r = pageRect(items[i]);
    if (layout === "vertical") {
      if (y < r.top + r.height / 2) { return items[i]; }
    } else if (y < r.top || (y <= r.bottom && x < r.left + r.width / 2)) {
      return items[i];
    }
  }
  return null;
}

// Put `node' before `ref', or after the last item of the list.
function soPlace(list: Element, node: HTMLElement, ref: HTMLElement | null): void {
  if (ref) {
    if (ref.previousSibling !== node) { list.insertBefore(node, ref); }
    return;
  }
  const items = Array.from(list.querySelectorAll<HTMLElement>(
    ":scope > .ah-sortable-item, :scope > .ah-sortable-placeholder"))
    .filter((n) => n !== node && n.style.display !== "none");
  const last = items[items.length - 1];
  const after = last ? last.nextSibling : list.firstChild;
  if (after !== node) { list.insertBefore(node, after); }
}

function soPos(list: Element, item: HTMLElement): string {
  const items = soItems(list);
  return (items.indexOf(item) + 1) + " of " + items.length;
}

function sibling(item: Element, dir: number): Element | null {
  let n = dir < 0 ? item.previousElementSibling : item.nextElementSibling;
  while (n && !n.classList.contains("ah-sortable-item")) {
    n = dir < 0 ? n.previousElementSibling : n.nextElementSibling;
  }
  return n;
}

// Move `item' by `delta' places (or to the start / end for -/+Infinity)
// by moving its neighbours, so the item itself keeps the focus.
function soShift(list: Element, item: Element, delta: number): boolean {
  let moved = false;
  while (delta < 0) {
    const prev = sibling(item, -1);
    if (!prev) { break; }
    list.insertBefore(prev, item.nextSibling);
    moved = true; delta++;
  }
  while (delta > 0) {
    const next = sibling(item, 1);
    if (!next) { break; }
    list.insertBefore(next, item);
    moved = true; delta--;
  }
  return moved;
}

function soSetOrder(list: Element, keys: readonly string[]): void {
  const items = soItems(list);
  const byKey = new Map<string, HTMLElement>();
  items.forEach((it) => { byKey.set(soKey(it), it); });
  const named: HTMLElement[] = [];
  keys.forEach((k) => {
    const it = byKey.get(k);
    if (it && named.indexOf(it) < 0) { named.push(it); }
  });
  const rest = items.filter((it) => named.indexOf(it) < 0);
  const lastItem = items[items.length - 1];
  let anchor = lastItem ? lastItem.nextSibling : list.firstChild;
  named.concat(rest).forEach((it) => {
    if (it !== anchor) { list.insertBefore(it, anchor); } else { anchor = it.nextSibling; }
  });
}

function soKeys(layout: Layout): { prev: string[]; next: string[] } {
  return layout === "vertical" ? { prev: ["ArrowUp"], next: ["ArrowDown"] }
    : layout === "horizontal" ? { prev: ["ArrowLeft"], next: ["ArrowRight"] }
    : { prev: ["ArrowLeft", "ArrowUp"], next: ["ArrowRight", "ArrowDown"] };
}

/** One pointer drag of an item of the list el (to list, maybe another
 *  list of its group). */
class SortDrag extends Drag {
  started = false;
  list: HTMLElement;
  private helper: HTMLElement | null = null;
  private ph: HTMLElement | null = null;
  private offX = 0;
  private offY = 0;
  private box: HTMLElement | null = null;
  private display = "";
  // the latest pointer position (page and client coordinates)
  private px = 0;
  private py = 0;
  private cx = 0;
  private cy = 0;

  constructor(el: HTMLElement, readonly item: HTMLElement, private readonly x0: number,
              private readonly y0: number) {
    super(el);
    this.list = el;
  }

  /** A pointer move: past DISTANCE the drag starts. */
  pointer(e: PointerEvent): void {
    if (!this.started) {
      if (Math.abs(px(e) - this.x0) + Math.abs(py(e) - this.y0) <= DISTANCE) { return; }
      this.start();
    }
    this.move(e);
  }

  private start(): void {
    const item = this.item;
    const r = pageRect(item);
    const cs = window.getComputedStyle(item);
    const ph = document.createElement(item.tagName);
    ph.className = "ah-sortable-placeholder";
    Object.assign(ph.style, { width: r.width + "px", height: r.height + "px", margin: cs.margin,
                              flex: "none" });
    this.helper = floatingCopy(item, "ah-sortable-helper", 0.85);
    this.offX = this.x0 - r.left;
    this.offY = this.y0 - r.top;
    this.ph = ph;
    const index = soItems(this.el).indexOf(item);
    this.list = this.el;
    this.box = scrollParent(this.el);
    this.started = true;
    // the item is a child of the list (server markup)
    (item.parentNode as ParentNode).insertBefore(ph, item.nextSibling);
    this.display = item.style.display;
    item.style.display = "none";
    this.el.classList.add("ah-sortable-active");
    document.body.classList.add("ah-disableselect");
    fire(this.el, "ah:sort-start", { key: soKey(item), index: index } satisfies SortPosition);
  }

  // Connected lists under the pointer: the innermost (smallest) one wins,
  // as in sigil's check-connected-containers!.
  private target(x: number, y: number): HTMLElement {
    const group = this.el.getAttribute("data-ah-group");
    if (!group) { return this.list; }
    let best: HTMLElement | null = null, area = Infinity;
    document.querySelectorAll<HTMLElement>(".ah-sortable[data-ah-group]").forEach((l) => {
      if (l.getAttribute("data-ah-group") !== group || (soDisabled(l) && l !== this.el)) {
        return;
      }
      const r = pageRect(l);
      if (inside(x, y, r) && r.width * r.height < area) { best = l; area = r.width * r.height; }
    });
    return best || this.list;
  }

  private move(e: PointerEvent): void {
    this.px = px(e); this.py = py(e); this.cx = e.clientX; this.cy = e.clientY;
    if (this.raf) { return; }
    this.raf = requestAnimationFrame(() => {
      this.raf = 0;
      if (Drag.current !== this || !this.helper || !this.ph) { return; }
      const key: SortKey = { key: soKey(this.item) };
      this.helper.style.left = (this.px - this.offX) + "px";
      this.helper.style.top = (this.py - this.offY) + "px";
      autoScroll(this.box, this.cx, this.cy);
      const target = this.target(this.px, this.py);
      if (target !== this.list) {
        this.list.classList.remove("ah-sortable-receiving");
        if (this.list !== this.el) { this.list.classList.remove("ah-sortable-active"); }
        fire(this.list, "ah:sort-remove", key);
        this.list = target;
        if (target !== this.el) { target.classList.add("ah-sortable-receiving", "ah-sortable-active"); }
        fire(target, "ah:sort-receive", key);
      }
      const items = soItems(this.list).filter((n) => n !== this.item);
      const ref = soInsertion(items, soLayout(this.list), this.px, this.py);
      const before = this.ph.nextSibling, parent = this.ph.parentNode;
      soPlace(this.list, this.ph, ref);
      if (this.ph.nextSibling !== before || this.ph.parentNode !== parent) {
        fire(this.list, "ah:sort-change", key);
      }
    });
  }

  private cleanup(): void {
    this.item.style.display = this.display;
    if (this.ph) { this.ph.remove(); }
    if (this.helper) { this.helper.remove(); }
    [this.el, this.list].forEach((l) => { l.classList.remove("ah-sortable-active", "ah-sortable-receiving"); });
    document.body.classList.remove("ah-disableselect");
  }

  /** The release: the item goes where the placeholder is. */
  end(): void {
    const item = this.item, from = this.el, to = this.list;
    to.insertBefore(item, this.ph);
    this.cleanup();
    swallowClick();
    soRove(to, item);
    if (from !== to) { soRove(from, null); }
    const index = soItems(to).indexOf(item);
    fire(to, "ah:sort-stop", { key: soKey(item), index: index } satisfies SortPosition);
    soPublish(from, true);
    if (from !== to) { soPublish(to, true); }
    try { item.focus({ preventScroll: true }); } catch (err) { /* detached */ }
  }

  protected override cancelled(): void {
    if (!this.started) { return; }
    this.cleanup();
    fire(this.el, "ah:sort-cancel", { key: soKey(this.item) } satisfies SortKey);
  }
}

class SortableController extends AH.Controller {
  // keyboard: the picked-up item and the order before it moved
  #grabbed: HTMLElement | null = null;
  #order = "";

  override setup(): void {
    const el = this.element;
    this.#grabbed = null;
    this.#order = "";
    soRove(el, null);
    this.delegate("pointerdown", ".ah-sortable-item", (e, item) => {
      if (item.parentNode !== el || Drag.current || soDisabled(el) ||
          (e.pointerType === "mouse" && e.button !== 0) || editable(e.target)) {
        return;
      }
      if (item.classList.contains("ah-sortable-handle-mode") &&
          !(e.target instanceof Element && e.target.closest(".ah-sortable-handle"))) {
        return;
      }
      e.preventDefault();
      this.release(true);
      soRove(el, item);
      try { item.focus({ preventScroll: true }); } catch (err) { /* ignore */ }
      const d = new SortDrag(el, item, px(e), py(e));
      d.track(e.pointerId, (me) => { d.pointer(me); }, () => {
        if (d.started) { d.end(); }
      });
    });
    this.delegate("keydown", ".ah-sortable-item", (e, item) => {
      if (e.target === item && item.parentNode === el) { this.keydown(item, e); }
    });
    this.delegate("focusout", ".ah-sortable-item", (_e, item) => {
      // a Tab away (or a click elsewhere) drops the picked-up item
      setTimeout(() => {
        if (this.#grabbed === item && document.activeElement !== item) {
          this.release(true);
        }
      }, 0);
    });
    this.delegate("focusin", ".ah-sortable-item", (_e, item) => {
      if (item.parentNode === el && !soDisabled(el)) { soRove(el, item); }
    });
  }

  override teardown(): void {
    Drag.cancelFor(this.element);
    this.#grabbed = null;
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue(): string { return soOrder(this.element); }

  setValue(order: string | readonly string[] | null): void {
    const keys = split(Array.isArray(order) ? order : String(order || "")).filter(Boolean);
    soSetOrder(this.element, keys);
    soPublish(this.element, false);
  }

  enable(): void {
    this.element.classList.remove("ah-sortable-disabled");
    this.element.removeAttribute("aria-disabled");
    soRove(this.element, null);
  }

  disable(): void {
    Drag.cancelFor(this.element);
    this.release(true);
    this.element.classList.add("ah-sortable-disabled");
    this.element.setAttribute("aria-disabled", "true");
    soRove(this.element, null);
  }

  cancel(): void {
    Drag.cancelFor(this.element);
    if (this.#grabbed) { this.release(false); }
  }

  // ---- keyboard: a picked-up item moves with the arrows ----

  private grab(item: HTMLElement): void {
    const list = this.element;
    this.#grabbed = item;
    this.#order = soOrder(list);
    item.classList.add("ah-sortable-item-grabbed");
    item.setAttribute("aria-pressed", "true");
    announce(soLive(list), AH.t("sortable", "picked",
                                  "Picked up {0}, position {1}. Arrow keys move it, Space drops it, Escape cancels.",
                                  [label(item), soPos(list, item)]));
  }

  private release(commit: boolean): void {
    const list = this.element, item = this.#grabbed;
    if (!item) { return; }
    this.#grabbed = null;
    item.classList.remove("ah-sortable-item-grabbed");
    item.removeAttribute("aria-pressed");
    const live = soLive(list);
    if (commit) {
      announce(live, AH.t("sortable", "dropped", "{0} dropped at position {1}.", [label(item), soPos(list, item)]));
      soPublish(list, true);
    } else {
      soSetOrder(list, split(this.#order));
      item.focus();
      announce(live, AH.t("sortable", "cancelled", "Cancelled, {0} is back at position {1}.", [label(item), soPos(list, item)]));
    }
  }

  private keydown(item: HTMLElement, e: KeyboardEvent): void {
    const list = this.element;
    const keys = soKeys(soLayout(list));
    const dir = keys.prev.indexOf(e.key) >= 0 ? -1 : keys.next.indexOf(e.key) >= 0 ? 1
      : e.key === "Home" ? -Infinity : e.key === "End" ? Infinity : 0;
    const off = soDisabled(list);
    if (this.#grabbed === item) {
      if (dir) {
        e.preventDefault();
        if (soShift(list, item, dir)) {
          announce(soLive(list), label(item) + ", position " + soPos(list, item) + ".");
        }
      } else if (e.key === " " || e.key === "Enter") {
        e.preventDefault();
        this.release(true);
      } else if (e.key === "Escape") {
        e.preventDefault();
        this.release(false);
      }
      return;
    }
    if (dir && e.altKey && !off) {
      e.preventDefault();
      if (soShift(list, item, dir)) { soPublish(list, true); }
      return;
    }
    if (dir) {
      e.preventDefault();
      const items = soItems(list), i = items.indexOf(item);
      const j = dir === -Infinity ? 0 : dir === Infinity ? items.length - 1
        : Math.max(0, Math.min(items.length - 1, i + dir));
      soRove(list, items[j]);
      items[j].focus();
    } else if ((e.key === " " || e.key === "Enter") && !off) {
      e.preventDefault();
      this.grab(item);
    }
  }
}

AH.register("sortable", SortableController);
