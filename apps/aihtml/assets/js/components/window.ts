/* The window behaviour (designs/04-components.md), ported from sigil's
 * overlay/window/*: a draggable, resizable window, optionally modal.
 * Shared machinery: _lib_overlay.ts. Events on the root: ah:opening
 * (cancelable), ah:open, ah:closing (cancelable, detail OverlayClose),
 * ah:close (detail OverlayClose), ah:collapse, ah:expand, ah:moving and
 * ah:moved (detail WindowPosition), ah:resize (detail WindowSize). */
import AH from "../core.ts";
import { closest, fades, flag, overlays } from "./_lib_overlay.ts";
import type { OverlayClose } from "./_lib_overlay.ts";

export type { OverlayClose } from "./_lib_overlay.ts";

/** Detail of ah:moving / ah:moved: the window's left / top in px. */
export interface WindowPosition { x: number; y: number; }
/** Detail of ah:resize: the window's size in px. */
export interface WindowSize { width: number; height: number; }

const MIN_W = 100;
const MIN_H = 60;

class WindowController extends AH.Controller {
  #backdrop: HTMLElement | null = null;
  #dragging: AbortController | null = null;      // the drag in progress

  override setup(): void {
    const el = this.element;
    this.#backdrop = null;
    this.#dragging = null;
    const front = (): void => { if (this.isOpen()) { this.bringToFront(); } };
    this.listen(el, "mousedown", front);
    this.listen(el, "pointerdown", front);
    this.delegate("click", ".ah-window-collapse-btn", (e) => {
      e.stopPropagation();
      this.#setCollapsed(!el.classList.contains("ah-window-collapsed"));
    });
    // drag by the title bar
    this.delegate("pointerdown", ".ah-window-header", (e) => {
      if (!flag(el, "draggable", true) || closest(e.target, "button") || e.button !== 0) { return; }
      e.preventDefault();
      const l0 = el.offsetLeft;
      const t0 = el.offsetTop;
      this.#drag(e, (dx, dy) => {
        const x = Math.min(Math.max(0, l0 + dx), window.innerWidth - el.offsetWidth);
        const y = Math.min(Math.max(0, t0 + dy), window.innerHeight - el.offsetHeight);
        el.style.left = Math.max(0, x) + "px";
        el.style.top = Math.max(0, y) + "px";
        el.setAttribute("data-ah-placed", "");
        this.fire<WindowPosition>("ah:moving", { x: x, y: y });
      }, () => {
        this.fire<WindowPosition>("ah:moved", { x: el.offsetLeft, y: el.offsetTop });
      });
    });
    // eight resize handles
    this.delegate("pointerdown", ".ah-window-resize-handle", (e, handle) => {
      if (!el.classList.contains("ah-window-resizable") || e.button !== 0) { return; }
      e.preventDefault();
      e.stopPropagation();
      const dir = handle.getAttribute("data-dir") || "";
      const w0 = el.offsetWidth;
      const h0 = el.offsetHeight;
      const l0 = el.offsetLeft;
      const t0 = el.offsetTop;
      this.#drag(e, (dx, dy) => {
        let w = w0;
        let h = h0;
        if (dir.indexOf("e") >= 0) { w = w0 + dx; }
        if (dir.indexOf("w") >= 0) { w = w0 - dx; }
        if (dir.indexOf("s") >= 0) { h = h0 + dy; }
        if (dir.indexOf("n") >= 0) { h = h0 - dy; }
        w = Math.max(MIN_W, w);
        h = Math.max(MIN_H, h);
        el.style.width = w + "px";
        el.style.height = h + "px";
        if (dir.indexOf("w") >= 0) { el.style.left = (l0 + w0 - w) + "px"; }
        if (dir.indexOf("n") >= 0) { el.style.top = (t0 + h0 - h) + "px"; }
        el.setAttribute("data-ah-placed", "");
      }, () => {
        this.fire<WindowSize>("ah:resize", { width: el.offsetWidth, height: el.offsetHeight });
      });
    });
    // arrows move, Ctrl+arrows resize (only when the window itself or
    // its title bar has focus, so inputs keep their arrow keys)
    this.listen(el, "keydown", (e) => {
      if (e.target !== el && !closest(e.target, ".ah-window-header")) { return; }
      const k = e.key;
      if (k !== "ArrowLeft" && k !== "ArrowRight" && k !== "ArrowUp" && k !== "ArrowDown") {
        return;
      }
      e.preventDefault();
      const dx = k === "ArrowLeft" ? -10 : k === "ArrowRight" ? 10 : 0;
      const dy = k === "ArrowUp" ? -10 : k === "ArrowDown" ? 10 : 0;
      if (e.ctrlKey) {
        this.resize(Math.max(MIN_W, el.offsetWidth + dx), Math.max(MIN_H, el.offsetHeight + dy));
      } else {
        this.move(el.offsetLeft + dx, el.offsetTop + dy);
      }
    });
    if (el.getAttribute("data-ah-initial") === "open") { this.open(); }
  }

  override teardown(): void {
    if (this.#dragging) { this.#dragging.abort(); }
    if (this.#backdrop) {
      this.#backdrop.remove();
      this.#backdrop = null;
      overlays.unlock();
    }
    overlays.pull(this.element, false);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open(): void {
    const el = this.element;
    if (this.isOpen()) { return; }
    if (!this.fire("ah:opening")) { return; }
    const modal = flag(el, "modal", false);
    fades.stop(el, true);
    if (!el.hasAttribute("data-ah-placed")) {
      // centre in the viewport on first open (sigil: position :center)
      el.style.visibility = "hidden";
      el.style.display = "flex";
      const w = el.offsetWidth;
      const h = el.offsetHeight;
      el.style.left = Math.max(0, (window.innerWidth - w) / 2) + "px";
      el.style.top = Math.max(0, (window.innerHeight - h) / 2) + "px";
      el.style.display = "none";
      el.style.visibility = "";
      el.setAttribute("data-ah-placed", "");
    }
    if (modal && el.parentNode) {
      const bd = document.createElement("div");
      bd.className = "ah-window-modal-backdrop";
      bd.style.display = "none";
      el.parentNode.insertBefore(bd, el);
      fades.fadeIn(bd, 250);
      bd.addEventListener("mousedown", () => {
        if (flag(el, "scrim", false)) { this.close(); }
      });
      this.#backdrop = bd;
      overlays.lock();
    }
    this.bringToFront();
    el.setAttribute("data-state", "open");
    overlays.push({
      el: el, trap: modal ? el : null,
      esc: () => {
        if (!flag(el, "esc", true)) { return false; }
        if (!modal && !el.contains(document.activeElement)) { return false; }
        this.close();
        return true;
      }
    });
    fades.fadeIn(el, 250);
    el.focus({ preventScroll: true });
    this.fire("ah:open");
  }

  close(result?: unknown): void {
    const el = this.element;
    if (!this.isOpen()) { return; }
    if (!this.fire<OverlayClose>("ah:closing", { result: result || null })) { return; }
    el.setAttribute("data-state", "closed");
    const bd = this.#backdrop;
    if (bd) {
      this.#backdrop = null;
      fades.fadeOut(bd, 250, () => { bd.remove(); });
      overlays.unlock();
    }
    overlays.pull(el, true);
    fades.stop(el, true);
    fades.fadeOut(el, 250);
    this.fire<OverlayClose>("ah:close", { result: result || null });
  }

  toggle(): void { if (this.isOpen()) { this.close(); } else { this.open(); } }
  collapse(): void { this.#setCollapsed(true); }
  expand(): void { this.#setCollapsed(false); }

  move(x: number, y: number): void {
    const el = this.element;
    el.style.left = x + "px";
    el.style.top = y + "px";
    el.setAttribute("data-ah-placed", "");
    this.fire<WindowPosition>("ah:moved", { x: x, y: y });
  }

  resize(w: number, h: number): void {
    this.element.style.width = w + "px";
    this.element.style.height = h + "px";
    this.fire<WindowSize>("ah:resize", { width: w, height: h });
  }

  bringToFront(): void {
    const zi = overlays.nextZ();
    this.element.style.zIndex = String(zi);
    if (this.#backdrop) { this.#backdrop.style.zIndex = String(zi - 1); }
  }

  isOpen(): boolean { return this.element.getAttribute("data-state") === "open"; }

  #setCollapsed(collapsed: boolean): void {
    const el = this.element;
    el.classList.toggle("ah-window-collapsed", collapsed);
    el.querySelectorAll(".ah-window-collapse-btn").forEach((b) => {
      b.setAttribute("aria-expanded", collapsed ? "false" : "true");
    });
    this.fire(collapsed ? "ah:collapse" : "ah:expand");
  }

  // One pointer drag: move(dx, dy) while the pointer moves, end() once.
  #drag(e: PointerEvent, move: (dx: number, dy: number) => void, end: () => void): void {
    if (this.#dragging) { this.#dragging.abort(); }
    const ac = this.#dragging = new AbortController();
    const x0 = e.clientX;
    const y0 = e.clientY;
    const o = { signal: ac.signal };
    document.addEventListener("pointermove", (me) => { move(me.clientX - x0, me.clientY - y0); }, o);
    const stop = (): void => {
      ac.abort();
      if (this.#dragging === ac) { this.#dragging = null; }
      end();
    };
    document.addEventListener("pointerup", stop, o);
    document.addEventListener("pointercancel", stop, o);
  }
}

AH.register("window", WindowController);
