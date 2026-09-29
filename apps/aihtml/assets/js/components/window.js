/* The window behaviour (designs/04-components.md), ported from sigil's
 * overlay/window/*: a draggable, resizable window, optionally modal.
 * Shared machinery: _lib_overlay.js. Events on the root: ah:opening
 * (cancelable), ah:open, ah:closing (cancelable, detail {result}),
 * ah:close (detail {result}), ah:collapse, ah:expand, ah:moving and
 * ah:moved (detail {x, y}), ah:resize (detail {width, height}). */
import AH from "../core.js";
import "./_lib_overlay.js";

var L = AH.lib.overlay;
var flag = L.flag,
    nextZ = L.nextZ,
    lock = L.lock,
    unlock = L.unlock,
    pushStack = L.pushStack,
    pullStack = L.pullStack;

var MIN_W = 100;
var MIN_H = 60;

AH.register("window", class extends AH.Controller {
  setup() {
    var el = this.element;
    this.backdrop = null;
    this.dragging = null;               // AbortController of the drag in progress
    var front = () => { if (this.isOpen()) { this.bringToFront(); } };
    this.listen(el, "mousedown", front);
    this.listen(el, "pointerdown", front);
    this.delegate("click", ".ah-window-collapse-btn", (e) => {
      e.stopPropagation();
      this.setCollapsed(!el.classList.contains("ah-window-collapsed"));
    });
    // drag by the title bar
    this.delegate("pointerdown", ".ah-window-header", (e) => {
      if (!flag(el, "draggable", true) || e.target.closest("button") || e.button !== 0) { return; }
      e.preventDefault();
      var l0 = el.offsetLeft;
      var t0 = el.offsetTop;
      this.drag(e, (dx, dy) => {
        var x = Math.min(Math.max(0, l0 + dx), window.innerWidth - el.offsetWidth);
        var y = Math.min(Math.max(0, t0 + dy), window.innerHeight - el.offsetHeight);
        el.style.left = Math.max(0, x) + "px";
        el.style.top = Math.max(0, y) + "px";
        el.setAttribute("data-ah-placed", "");
        this.fire("ah:moving", { x: x, y: y });
      }, () => {
        this.fire("ah:moved", { x: el.offsetLeft, y: el.offsetTop });
      });
    });
    // eight resize handles
    this.delegate("pointerdown", ".ah-window-resize-handle", (e, handle) => {
      if (!el.classList.contains("ah-window-resizable") || e.button !== 0) { return; }
      e.preventDefault();
      e.stopPropagation();
      var dir = handle.getAttribute("data-dir") || "";
      var w0 = el.offsetWidth;
      var h0 = el.offsetHeight;
      var l0 = el.offsetLeft;
      var t0 = el.offsetTop;
      this.drag(e, (dx, dy) => {
        var w = w0;
        var h = h0;
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
        this.fire("ah:resize", { width: el.offsetWidth, height: el.offsetHeight });
      });
    });
    // arrows move, Ctrl+arrows resize (only when the window itself or
    // its title bar has focus, so inputs keep their arrow keys)
    this.listen(el, "keydown", (e) => {
      if (e.target !== el && !e.target.closest(".ah-window-header")) { return; }
      var k = e.key;
      if (k !== "ArrowLeft" && k !== "ArrowRight" && k !== "ArrowUp" && k !== "ArrowDown") {
        return;
      }
      e.preventDefault();
      var dx = k === "ArrowLeft" ? -10 : k === "ArrowRight" ? 10 : 0;
      var dy = k === "ArrowUp" ? -10 : k === "ArrowDown" ? 10 : 0;
      if (e.ctrlKey) {
        this.resize(Math.max(MIN_W, el.offsetWidth + dx), Math.max(MIN_H, el.offsetHeight + dy));
      } else {
        this.move(el.offsetLeft + dx, el.offsetTop + dy);
      }
    });
    if (el.getAttribute("data-ah-initial") === "open") { this.open(); }
  }

  teardown() {
    if (this.dragging) { this.dragging.abort(); }
    if (this.backdrop) {
      this.backdrop.remove();
      this.backdrop = null;
      unlock();
    }
    pullStack(this.element, false);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open() {
    var el = this.element;
    if (this.isOpen()) { return; }
    if (!this.fire("ah:opening")) { return; }
    var modal = flag(el, "modal", false);
    L.stopFade(el, true);
    if (!el.hasAttribute("data-ah-placed")) {
      // centre in the viewport on first open (sigil: position :center)
      el.style.visibility = "hidden";
      el.style.display = "flex";
      var w = el.offsetWidth;
      var h = el.offsetHeight;
      el.style.left = Math.max(0, (window.innerWidth - w) / 2) + "px";
      el.style.top = Math.max(0, (window.innerHeight - h) / 2) + "px";
      el.style.display = "none";
      el.style.visibility = "";
      el.setAttribute("data-ah-placed", "");
    }
    if (modal) {
      var bd = document.createElement("div");
      bd.className = "ah-window-modal-backdrop";
      bd.style.display = "none";
      el.parentNode.insertBefore(bd, el);
      L.fadeIn(bd, 250);
      bd.addEventListener("mousedown", () => {
        if (flag(el, "scrim", false)) { this.close(); }
      });
      this.backdrop = bd;
      lock();
    }
    this.bringToFront();
    el.setAttribute("data-state", "open");
    pushStack({
      el: el, trap: modal ? el : null,
      esc: () => {
        if (!flag(el, "esc", true)) { return false; }
        if (!modal && !el.contains(document.activeElement)) { return false; }
        this.close();
        return true;
      }
    });
    L.fadeIn(el, 250);
    el.focus({ preventScroll: true });
    this.fire("ah:open");
  }

  close(result) {
    var el = this.element;
    if (!this.isOpen()) { return; }
    if (!this.fire("ah:closing", { result: result || null })) { return; }
    el.setAttribute("data-state", "closed");
    var bd = this.backdrop;
    if (bd) {
      this.backdrop = null;
      L.fadeOut(bd, 250, function () { bd.remove(); });
      unlock();
    }
    pullStack(el, true);
    L.stopFade(el, true);
    L.fadeOut(el, 250);
    this.fire("ah:close", { result: result || null });
  }

  toggle() { if (this.isOpen()) { this.close(); } else { this.open(); } }
  collapse() { this.setCollapsed(true); }
  expand() { this.setCollapsed(false); }

  move(x, y) {
    var el = this.element;
    el.style.left = x + "px";
    el.style.top = y + "px";
    el.setAttribute("data-ah-placed", "");
    this.fire("ah:moved", { x: x, y: y });
  }

  resize(w, h) {
    this.element.style.width = w + "px";
    this.element.style.height = h + "px";
    this.fire("ah:resize", { width: w, height: h });
  }

  bringToFront() {
    var zi = nextZ();
    this.element.style.zIndex = zi;
    if (this.backdrop) { this.backdrop.style.zIndex = zi - 1; }
  }

  isOpen() { return this.element.getAttribute("data-state") === "open"; }

  setCollapsed(collapsed) {
    var el = this.element;
    el.classList.toggle("ah-window-collapsed", collapsed);
    el.querySelectorAll(".ah-window-collapse-btn").forEach(function (b) {
      b.setAttribute("aria-expanded", collapsed ? "false" : "true");
    });
    this.fire(collapsed ? "ah:collapse" : "ah:expand");
  }

  // One pointer drag: move(dx, dy) while the pointer moves, end() once.
  drag(e, move, end) {
    if (this.dragging) { this.dragging.abort(); }
    var ac = this.dragging = new AbortController();
    var x0 = e.clientX;
    var y0 = e.clientY;
    var o = { signal: ac.signal };
    document.addEventListener("pointermove", function (me) { move(me.clientX - x0, me.clientY - y0); }, o);
    var stop = () => {
      ac.abort();
      if (this.dragging === ac) { this.dragging = null; }
      end();
    };
    document.addEventListener("pointerup", stop, o);
    document.addEventListener("pointercancel", stop, o);
  }
});
