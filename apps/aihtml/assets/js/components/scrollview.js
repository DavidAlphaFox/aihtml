/* Behaviour of the scrollview component (designs/04-components.md),
 * ported from sigil's scrollview (cljs + jQuery). The value is kept in
 * data-ah-value on the root, mirrored into a hidden input when there is
 * one, and "change" fires on the root when the user changes it. Methods
 * called by the server (AH.invoke / aihtml_action:call) update the value
 * without firing "change". Drags use pointer capture, so nothing is bound
 * on document. ah:page-changed fires on the root when the page changes,
 * detail {page, old}.
 */
import AH from "../core.js";
import "./_lib_scroll.js";

const setValue = AH.lib.scroll.setValue, num = AH.lib.scroll.num, capture = AH.lib.scroll.capture;

// ------------------------------------------------------------------
// ScrollView: a horizontal pager (sigil layout/scrollview)
// ------------------------------------------------------------------
//
// Pages are 100% wide and the wrapper moves by margin-left in percent,
// so the layout follows the container without measuring; drags move it
// in pixels and the page change animates back to a percentage.

const SV = "ah-scrollview";
const SV_DEAD_ZONE = 15;

function svWrapper(el) { return el.querySelector(":scope > ." + SV + "-wrapper"); }
function svPages(el) {
  const w = svWrapper(el);
  return w ? Array.from(w.querySelectorAll(":scope > ." + SV + "-page")) : [];
}
function svIndex(el) { return parseInt(el.getAttribute("data-ah-value"), 10) || 0; }
function svDisabled(el) { return el.classList.contains(SV + "-disabled"); }
function svDuration(el) { return num(el, "data-duration", 300); }

// content width (jQuery's .width())
function svWidth(el) {
  const cs = getComputedStyle(el);
  return el.clientWidth - (parseFloat(cs.paddingLeft) || 0) - (parseFloat(cs.paddingRight) || 0);
}

function svPlace(el, idx) {
  svWrapper(el).style.marginLeft = idx ? (-idx * 100) + "%" : "";
}

function svMark(el, idx) {
  svPages(el).forEach(function (pg, i) {
    if (i === idx) {
      pg.removeAttribute("aria-hidden");
      pg.removeAttribute("inert");
    } else {
      pg.setAttribute("aria-hidden", "true");
      pg.setAttribute("inert", "");
    }
  });
  el.querySelectorAll(":scope > ." + SV + "-buttons > ." + SV + "-button").forEach(function (b, i) {
    const on = i === idx;
    b.classList.toggle(SV + "-button-active", on);
    if (on) { b.setAttribute("aria-current", "true"); } else { b.removeAttribute("aria-current"); }
  });
}

AH.register("scrollview", class extends AH.Controller {
  setup() {
    const el = this.element;
    const self = this;
    this.timer = null;
    this.anim = null;
    this.paused = false;
    const w = svWrapper(el);
    let drag = null;
    svMark(el, svIndex(el));

    this.listen(w, "pointerdown", function (e) {
      if (svDisabled(el) || (e.button !== undefined && e.button !== 0)) { return; }
      el.classList.remove(SV + "-animating");
      drag = { x: e.clientX, ml: parseFloat(getComputedStyle(w).marginLeft) || 0,
               moving: false, e: e };
    });
    this.listen(w, "pointermove", function (e) {
      if (!drag) { return; }
      const dx = e.clientX - drag.x;
      if (!drag.moving) {
        if (Math.abs(dx) <= SV_DEAD_ZONE) { return; }
        drag.moving = true;
        capture(w, drag.e);
        el.classList.add(SV + "-dragging");
      }
      e.preventDefault();
      const cw = svWidth(el), cnt = svPages(el).length;
      let ml = drag.ml + dx;
      if (el.getAttribute("data-bounce") === "false") {
        ml = Math.max(-(cnt - 1) * cw, Math.min(0, ml));
      }
      w.style.marginLeft = ml + "px";
    });
    const end = function () {
      if (!drag) { return; }
      const d = drag;
      drag = null;
      if (!d.moving) { return; }
      el.classList.remove(SV + "-dragging");
      // swallow the click that ends a drag, so links in a page stay put
      const swallow = function (c) { c.preventDefault(); c.stopPropagation(); };
      w.addEventListener("click", swallow, { once: true, capture: true });
      setTimeout(function () { w.removeEventListener("click", swallow, { capture: true }); }, 0);
      const dx = (parseFloat(w.style.marginLeft) || 0) - d.ml;
      const cw = svWidth(el), threshold = num(el, "data-threshold", 0.5) * cw;
      const cur = svIndex(el);
      let target = cur;
      if (dx < -threshold) { target = cur + 1; } else if (dx > threshold) { target = cur - 1; }
      self.go(target, "user");
    };
    this.listen(w, "pointerup", end);
    this.listen(w, "pointercancel", end);
    this.listen(w, "dragstart", function (e) { e.preventDefault(); });

    this.delegate("click", "." + SV + "-button", function (e, b) {
      if (b.closest("." + SV) !== el || svDisabled(el)) { return; }
      self.go(Array.prototype.indexOf.call(b.parentNode.children, b), "user");
    });

    this.listen(el, "keydown", function (e) {
      if (e.target !== el || svDisabled(el)) { return; }
      const cur = svIndex(el), cnt = svPages(el).length;
      let to = null;
      switch (e.key) {
        case "ArrowLeft": case "ArrowUp": case "PageUp": to = cur - 1; break;
        case "ArrowRight": case "ArrowDown": case "PageDown": to = cur + 1; break;
        case "Home": to = 0; break;
        case "End": to = cnt - 1; break;
        default: return;
      }
      e.preventDefault();
      self.go(to, "user");
    });

    // the slide show waits while the pointer or the focus is inside
    const pause = function () { self.paused = true; };
    this.listen(el, "mouseenter", pause);
    this.listen(el, "focusin", pause);
    this.listen(el, "mouseleave", function () { self.paused = false; });
    this.listen(el, "focusout", function (e) {
      if (e.relatedTarget && el.contains(e.relatedTarget)) { return; }
      self.paused = false;
    });
    if (el.hasAttribute("data-slide-show")) { this.startSlideShow(); }
  }

  teardown() {
    this.stopSlideShow();
    clearTimeout(this.anim);
    this.anim = null;
    this.element.classList.remove(SV + "-animating");
  }

  // Turn the transition on for one move and off again when it ends.
  animate() {
    const el = this.element;
    const self = this;
    el.classList.add(SV + "-animating");
    clearTimeout(this.anim);
    this.anim = setTimeout(function () {
      self.anim = null;
      el.classList.remove(SV + "-animating");
    }, svDuration(el) + 50);
  }

  // how: "user" fires change, "api" and "auto" only ah:page-changed.
  go(idx, how) {
    const el = this.element;
    const cnt = svPages(el).length;
    if (!cnt) { return; }
    idx = Math.max(0, Math.min(cnt - 1, idx));
    const old = svIndex(el);
    this.animate();
    svPlace(el, idx);
    if (idx === old) { return; }
    svMark(el, idx);
    setValue(el, String(idx));
    this.fire("ah:page-changed", { page: idx, old: old });
    if (how === "user") { this.fire("change"); }
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(v) { this.go(parseInt(v, 10) || 0, "api"); }
  getValue() { return svIndex(this.element); }
  forward() {
    const cur = svIndex(this.element);
    if (cur + 1 < svPages(this.element).length) { this.go(cur + 1, "api"); }
  }
  back() {
    const cur = svIndex(this.element);
    if (cur > 0) { this.go(cur - 1, "api"); }
  }
  startSlideShow() {
    const el = this.element;
    const self = this;
    if (this.timer) { return; }
    this.timer = setInterval(function () {
      if (self.paused || svDisabled(el)) { return; }
      const cnt = svPages(el).length, cur = svIndex(el);
      self.go(cur + 1 >= cnt ? 0 : cur + 1, "auto");
    }, num(el, "data-slide-duration", 3000));
  }
  stopSlideShow() {
    clearInterval(this.timer);
    this.timer = null;
  }
  refresh() {
    const el = this.element;
    const cnt = svPages(el).length, cur = Math.min(svIndex(el), Math.max(0, cnt - 1));
    setValue(el, String(cur));
    svPlace(el, cur);
    svMark(el, cur);
  }
});
