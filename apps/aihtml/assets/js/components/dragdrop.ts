/* The dragdrop behaviour (designs/04-components.md), ported from sigil's
 * layout/dragdrop: [data-ah-drag] items dropped on [data-ah-drop] zones
 * fire ah:drop on the zone; the root carries data-drag, data-drop and
 * data-from for the action payload. The drag machinery is shared with
 * sortable.ts (_lib_dnd.ts).
 *
 * Events (native, bubbling): on the item ah:drag-start (detail DragStart
 * {key}), ah:dragging (detail Dragging {pageX, pageY}), ah:drag-end,
 * ah:drag-cancel; on the zone ah:drop-target-enter, ah:drop-target-leave
 * and ah:drop (detail DropEvent {drag, drop, from}).
 */
import AH from "../core.ts";
import { announce, autoScroll, DISTANCE, Drag, editable, fire, floatingCopy, inside, label, pageRect,
         px, py, scrollParent, swallowClick } from "./_lib_dnd.ts";

/** Detail of ah:drag-start. */
export interface DragStart { key: string | null; }
/** Detail of ah:dragging (page coordinates of the pointer). */
export interface Dragging { pageX: number; pageY: number; }
/** Detail of ah:drop: the item's key, the zone's and the zone it came from. */
export interface DropEvent { drag: string | null; drop: string | null; from: string; }

function ddScope(el: Element, node: Element): boolean {
  return node.closest("[data-ah=dragdrop]") === el;
}

function ddUsable(el: Element, item: Element): boolean {
  return !el.classList.contains("ah-dragdrop-disabled") &&
    !item.classList.contains("ah-draggable-disabled");
}

// The zones of this scope that take `item' (its data-ah-drag-type must
// be in a zone's data-ah-drop-accept, when the zone has one).
function ddZones(el: Element, item: Element): HTMLElement[] {
  const type = item.getAttribute("data-ah-drag-type") || "";
  return Array.from(el.querySelectorAll<HTMLElement>("[data-ah-drop]")).filter((z) => {
    if (!ddScope(el, z) || z.getAttribute("data-ah-drop-disabled") === "true" ||
        item.contains(z)) {
      return false;
    }
    const accept = z.getAttribute("data-ah-drop-accept");
    return !accept || accept.split(",").indexOf(type) >= 0;
  });
}

function ddLive(el: Element): HTMLElement {
  let live = el.querySelector<HTMLElement>(":scope > .ah-dnd-live");
  if (!live) {
    live = document.createElement("span");
    live.className = "ah-sortable-live ah-dnd-live";
    live.setAttribute("aria-live", "assertive");
    live.setAttribute("aria-atomic", "true");
    el.appendChild(live);
  }
  return live;
}

/** One drag of an item of the scope el onto its zones: by the pointer or
 *  from the keyboard (keyboard, walking the zones with pos). */
class DropDrag extends Drag {
  started = false;
  zones: HTMLElement[] = [];
  target: HTMLElement | null = null;
  pos = -1;
  private from = "";
  private copy: HTMLElement | null = null;
  private orig: { left: number; top: number } | null = null;
  private offX = 0;
  private offY = 0;
  private box: HTMLElement | null = null;
  // the latest pointer position (page and client coordinates)
  private px = 0;
  private py = 0;
  private cx = 0;
  private cy = 0;

  constructor(el: HTMLElement, readonly item: HTMLElement, readonly keyboard: boolean,
              private readonly x0 = 0, private readonly y0 = 0) {
    super(el);
  }

  begin(): void {
    const item = this.item;
    this.zones = ddZones(this.el, item);
    const from = item.parentElement && item.parentElement.closest("[data-ah-drop]");
    this.from = from && ddScope(this.el, from) ? from.getAttribute("data-ah-drop") || "" : "";
    this.target = null;
    item.classList.add("ah-dragging");
    this.zones.forEach((z) => { z.classList.add("ah-drop-zone-accepting"); });
    this.el.classList.add("ah-dragdrop-active");
    fire(item, "ah:drag-start", { key: item.getAttribute("data-ah-drag") } satisfies DragStart);
  }

  // sigil's hit-test: the last zone (in document order, so an inner zone
  // beats its container) that the copy overlaps (intersect), lies in
  // (fit), or that the pointer is on (pointer).
  private hit(x: number, y: number): HTMLElement | null {
    if (!this.copy) { return null; }
    const tol = this.el.getAttribute("data-ah-tolerance") || "intersect";
    const f = pageRect(this.copy);
    let hit: HTMLElement | null = null;
    this.zones.forEach((z) => {
      const r = pageRect(z);
      const ok = tol === "pointer" ? inside(x, y, r)
        : tol === "fit" ? f.left >= r.left && f.right <= r.right && f.top >= r.top && f.bottom <= r.bottom
        : f.left < r.right && f.right > r.left && f.top < r.bottom && f.bottom > r.top;
      if (ok) { hit = z; }
    });
    return hit;
  }

  setTarget(zone: HTMLElement | null): void {
    if (zone === this.target) { return; }
    if (this.target) {
      this.target.classList.remove("ah-drop-target-active");
      fire(this.target, "ah:drop-target-leave");
    }
    this.target = zone;
    if (zone) {
      zone.classList.add("ah-drop-target-active");
      fire(zone, "ah:drop-target-enter");
    }
  }

  finish(dropped: boolean): void {
    const item = this.item;
    item.classList.remove("ah-dragging");
    this.zones.forEach((z) => { z.classList.remove("ah-drop-zone-accepting", "ah-drop-target-active"); });
    this.el.classList.remove("ah-dragdrop-active");
    document.body.classList.remove("ah-disableselect");
    const copy = this.copy, orig = this.orig;
    if (copy) {
      if (!dropped && this.el.hasAttribute("data-ah-revert") && orig) {
        copy.style.transition = "left .2s ease, top .2s ease";
        copy.style.left = orig.left + "px";
        copy.style.top = orig.top + "px";
        setTimeout(() => { copy.remove(); }, 220);
      } else {
        copy.remove();
      }
    }
    fire(item, "ah:drag-end");
  }

  /** Drop on the current target. */
  drop(): void {
    const zone = this.target, item = this.item, el = this.el;
    if (!zone) { return; }
    const key = item.getAttribute("data-ah-drag");
    const dest = zone.getAttribute("data-ah-drop");
    const focused = document.activeElement === item;
    zone.classList.remove("ah-drop-target-active");
    if (el.hasAttribute("data-ah-move") && item.parentNode !== zone) {
      zone.appendChild(item);
      if (focused) { item.focus(); }
    }
    this.finish(true);
    el.setAttribute("data-drag", key || "");
    el.setAttribute("data-drop", dest || "");
    el.setAttribute("data-from", this.from);
    fire(zone, "ah:drop", { drag: key, drop: dest, from: this.from } satisfies DropEvent);
    announce(ddLive(el), label(item) + " dropped on " + label(zone) + ".");
  }

  protected override cancelled(): void {
    if (!this.keyboard && !this.started) { return; }
    this.setTarget(null);
    this.finish(false);
    fire(this.item, "ah:drag-cancel");
  }

  /** A pointer move: past DISTANCE the drag starts. */
  pointer(me: PointerEvent): void {
    const item = this.item;
    if (!this.started) {
      if (Math.abs(px(me) - this.x0) + Math.abs(py(me) - this.y0) <= DISTANCE) { return; }
      this.started = true;
      const r = pageRect(item);
      this.orig = { left: r.left, top: r.top };
      this.offX = this.x0 - r.left;
      this.offY = this.y0 - r.top;
      this.box = scrollParent(this.el);
      this.copy = floatingCopy(item, "ah-drag-feedback", 0.6);
      document.body.classList.add("ah-disableselect");
      this.begin();
    }
    this.px = px(me); this.py = py(me); this.cx = me.clientX; this.cy = me.clientY;
    if (this.raf) { return; }
    this.raf = requestAnimationFrame(() => {
      this.raf = 0;
      if (Drag.current !== this || !this.copy) { return; }
      this.copy.style.left = (this.px - this.offX) + "px";
      this.copy.style.top = (this.py - this.offY) + "px";
      autoScroll(this.box, this.cx, this.cy);
      this.setTarget(this.hit(this.px, this.py));
      fire(item, "ah:dragging", { pageX: this.px, pageY: this.py } satisfies Dragging);
    });
  }

  /** The release of a pointer drag. */
  release(ue: PointerEvent): void {
    if (!this.started || !this.copy) { return; }
    swallowClick();
    // the last frame may not have run: test where the pointer let go
    this.copy.style.left = (px(ue) - this.offX) + "px";
    this.copy.style.top = (py(ue) - this.offY) + "px";
    this.setTarget(this.hit(px(ue), py(ue)));
    if (this.target) { this.drop(); } else { this.finish(false); }
  }
}

class DragdropController extends AH.Controller {
  override setup(): void {
    const el = this.element;
    this.delegate("pointerdown", "[data-ah-drag]", (e, item) => {
      if (Drag.current || !ddScope(el, item) || !ddUsable(el, item) ||
          (e.pointerType === "mouse" && e.button !== 0) || editable(e.target)) {
        return;
      }
      e.preventDefault();
      try { item.focus({ preventScroll: true }); } catch (err) { /* ignore */ }
      const d = new DropDrag(el, item, false, px(e), py(e));
      d.track(e.pointerId, (me) => { d.pointer(me); }, (ue) => { d.release(ue); });
    });
    this.delegate("keydown", "[data-ah-drag]", (e, item) => {
      if (e.target === item && ddScope(el, item)) { this.keydown(item, e); }
    });
    this.delegate("focusout", "[data-ah-drag]", (_e, item) => {
      setTimeout(() => {
        const d = Drag.current;
        if (d instanceof DropDrag && d.keyboard && d.item === item &&
            document.activeElement !== item) {
          d.cancel();
        }
      }, 0);
    });
  }

  override teardown(): void {
    Drag.cancelFor(this.element);
    const live = this.element.querySelector(":scope > .ah-dnd-live");
    if (live) { live.remove(); }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  enable(): void {
    this.element.classList.remove("ah-dragdrop-disabled");
    this.element.removeAttribute("aria-disabled");
  }

  disable(): void {
    Drag.cancelFor(this.element);
    this.element.classList.add("ah-dragdrop-disabled");
    this.element.setAttribute("aria-disabled", "true");
  }

  cancel(): void { Drag.cancelFor(this.element); }

  // Keyboard: Space/Enter picks up, arrows walk the zones, Space/Enter
  // drops, Escape (or leaving the item) cancels.
  private keydown(item: HTMLElement, e: KeyboardEvent): void {
    const el = this.element;
    const cur = Drag.current;
    const d = cur instanceof DropDrag && cur.keyboard && cur.item === item ? cur : null;
    const pick = e.key === " " || e.key === "Enter";
    if (!d) {
      if (pick && !Drag.current && ddUsable(el, item)) {
        e.preventDefault();
        const k = new DropDrag(el, item, true);
        k.begin();
        if (!k.zones.length) {
          k.finish(false);
          announce(ddLive(el), "No drop zone takes " + label(item) + ".");
          return;
        }
        k.claim();
        k.pos = -1;
        announce(ddLive(el), "Picked up " + label(item) + ". Arrow keys choose one of " +
                 k.zones.length + " drop zones, Space drops, Escape cancels.");
      }
      return;
    }
    const n = d.zones.length;
    if (/^Arrow/.test(e.key)) {
      e.preventDefault();
      const step = e.key === "ArrowUp" || e.key === "ArrowLeft" ? -1 : 1;
      d.pos = d.pos < 0 ? (step > 0 ? 0 : n - 1) : (d.pos + step + n) % n;
      d.setTarget(d.zones[d.pos]);
      announce(ddLive(el), label(d.zones[d.pos]) + ", drop zone " + (d.pos + 1) + " of " + n + ".");
    } else if (pick) {
      e.preventDefault();
      if (d.target) { Drag.untrack(); d.drop(); } else { d.cancel(); }
    } else if (e.key === "Escape") {
      e.preventDefault();
      d.cancel();
      announce(ddLive(el), "Cancelled.");
    }
  }
}

AH.register("dragdrop", DragdropController);
