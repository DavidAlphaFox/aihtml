/* The popover behaviour (designs/04-components.md), ported from sigil's
 * overlay/popover: a bubble anchored to the element that opened it or to
 * its data-ah-anchor selector. Shared machinery: _lib_overlay.js.
 * Events: ah:opening (cancelable), ah:open, ah:close (detail {result}). */
import AH from "../core.js";
import "./_lib_overlay.js";

var L = AH.lib.overlay;
var flag = L.flag,
    nextZ = L.nextZ,
    pushStack = L.pushStack,
    pullStack = L.pullStack,
    floatWithArrow = L.floatWithArrow;

var POP_POSITIONS = "ah-popover-top ah-popover-bottom ah-popover-left ah-popover-right";

AH.register("popover", class extends AH.Controller {
  setup() {
    var el = this.element;
    this.anchor = null;
    this.float = null;
    this.backdrop = null;
    var anchorSel = el.getAttribute("data-ah-anchor");
    if (anchorSel) {
      // sigil's `selector' prop: the anchor toggles the popover
      this.listen(document, "click", (e) => {
        var hits;
        try { hits = L.matching(e.target, anchorSel); } catch (err) { return; }
        hits.forEach((a) => {
          if (a.matches("[data-ah-open],[data-ah-toggle]")) { return; }
          e.preventDefault();
          AH.invoke(el, "toggle", { invoker: a });
        });
      });
    }
    this.listen(document, "click", (e) => {
      if (!this.isOpen() || !flag(el, "auto-close", true) || flag(el, "modal", false)) { return; }
      var anchor = this.anchor;
      var t = e.target;
      if (el.contains(t) || (anchor && anchor.contains(t))) { return; }
      this.close();
    });
  }

  teardown() {
    this.unfloat();
    if (this.backdrop) { this.backdrop.remove(); }
    pullStack(this.element, false);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open(opts) {
    var el = this.element;
    if (this.isOpen()) { return; }
    var sel = el.getAttribute("data-ah-anchor");
    var anchor = (opts && opts.invoker) || (sel ? document.querySelector(sel) : null);
    if (!anchor) {
      console.error("aihtml: popover has no anchor", el);
      return;
    }
    if (!this.fire("ah:opening")) { return; }
    this.anchor = anchor;
    var zi = nextZ();
    L.stopFade(el, true);
    this.unfloat();
    Object.assign(el.style, { zIndex: zi, display: "block", visibility: "hidden", opacity: "0" });
    this.float = floatWithArrow(el, anchor, el.getAttribute("data-ah-position") || "bottom",
                                8, "ah-popover-", POP_POSITIONS);
    if (flag(el, "modal", false)) {
      var bd = document.createElement("div");
      bd.className = "ah-popover-modal-backdrop";
      bd.style.zIndex = zi - 1;
      el.parentNode.insertBefore(bd, el);
      this.backdrop = bd;
    }
    el.style.visibility = "visible";
    L.fade(el, 1, 200);
    el.setAttribute("data-state", "open");
    anchor.setAttribute("aria-expanded", "true");
    pushStack({ el: el, trap: null, esc: () => { this.close(); return true; } });
    this.fire("ah:open");
  }

  close(result) {
    var el = this.element;
    if (!this.isOpen()) { return; }
    var anchor = this.anchor;
    el.setAttribute("data-state", "closed");
    if (anchor) { anchor.setAttribute("aria-expanded", "false"); }
    if (this.backdrop) {
      this.backdrop.remove();
      this.backdrop = null;
    }
    var focusInside = el !== document.activeElement && el.contains(document.activeElement);
    pullStack(el, false);
    if (focusInside && anchor) { anchor.focus(); }
    L.fadeOut(el, 200, () => { this.unfloat(); });
    this.fire("ah:close", { result: result || null });
  }

  toggle(opts) {
    if (this.isOpen()) { this.close(); } else { this.open(opts); }
  }

  isOpen() { return this.element.getAttribute("data-state") === "open"; }

  unfloat() {
    if (this.float) {
      this.float.stop();
      this.float = null;
    }
  }
});
