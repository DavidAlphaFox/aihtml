/* Behaviour of the responsive_panel component (designs/04-components.md),
 * ported from sigil's responsive-panel. The only listener on document is
 * the click-outside one, removed on teardown. Events on the root (no
 * detail): ah:collapse / ah:expand (folding at the breakpoint), ah:open /
 * ah:close (the folded content); ah:load fires once on the content.
 */
import AH from "../core.js";
import "./_lib_scroll.js";
import "./_lib_layout.js";

const num = AH.lib.scroll.num;
const L = AH.lib.layout;

// ------------------------------------------------------------------
// Responsive panel (sigil layout/responsive_panel)
// ------------------------------------------------------------------
//
// Folded when the parent is at most data-breakpoint px wide: the content
// then floats below the toggle (AH.float) while open.

const RP = "ah-responsive-panel";

function rpSpeed(el, name) { return num(el, name, 200); }

function rpClearStyles(c) {
  if (!c) { return; }
  L.stop(c);
  c.style.display = c.style.opacity = c.style.width = "";
}

function parentWidth(el) {
  const p = el.parentElement;
  if (!p) { return 0; }
  const cs = getComputedStyle(p);
  return p.clientWidth - (parseFloat(cs.paddingLeft) || 0) - (parseFloat(cs.paddingRight) || 0);
}

AH.register("responsive-panel", class extends AH.Controller {
  setup() {
    const el = this.element;
    const self = this;
    this.st = { collapsed: false, open: false, loaded: false, float: null, ro: null, ext: [] };
    const st = this.st;
    const t = this.toggleEl();
    if (t) {
      this.listen(t, "click", function () {
        if (!self.disabled()) { self.flip(); }
      });
      this.listen(t, "keydown", function (e) {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          if (!self.disabled()) { self.flip(); }
        }
      });
    }
    this.listen(el, "keydown", function (e) {
      if (e.key === "Escape" && st.open) {
        e.stopPropagation();
        self.close();
        const tg = self.toggleEl();
        if (tg) { tg.focus(); }
      }
    });
    const sel = el.getAttribute("data-toggle-button");
    if (sel) {
      st.ext = Array.from(document.querySelectorAll(sel));
      st.ext.forEach(function (b) {
        self.listen(b, "click", function () {
          if (!self.disabled()) { self.flip(); }
        });
      });
    }
    this.listen(document, "click", function (e) {
      if (st.open && el.getAttribute("data-auto-close") !== "false" &&
          !el.contains(e.target) &&
          !st.ext.some(function (b) { return b.contains(e.target); })) {
        self.close();
      }
    });
    const check = function () { self.check(); };
    if (window.ResizeObserver && el.parentNode) {
      st.ro = new ResizeObserver(check);
      st.ro.observe(el.parentNode);
    }
    this.listen(window, "resize", check);
    check();
    if (!st.collapsed) { this.load(); }
  }

  teardown() {
    const st = this.st;
    if (!st) { return; }
    if (st.float) { st.float.stop(); }
    if (st.ro) { st.ro.disconnect(); }
    const c = this.content();
    if (c) { L.stop(c); }
    this.st = null;
  }

  toggleEl() { return this.element.querySelector(":scope > ." + RP + "-toggle"); }
  content() { return this.element.querySelector(":scope > ." + RP + "-content"); }
  disabled() { return this.element.classList.contains(RP + "-disabled"); }

  load() {
    const st = this.st;
    const c = this.content();
    if (st.loaded) { return; }
    st.loaded = true;
    if (c && /(^|\s)ah:load:/.test(c.getAttribute("data-ah-on") || "")) { this.fire("ah:load", undefined, c); }
  }

  flip() {
    if (this.st.open) { this.close(); } else { this.open(); }
  }

  check() {
    const el = this.element;
    const st = this.st;
    if (!st) { return; }
    const bp = num(el, "data-breakpoint", 1000);
    const pw = parentWidth(el);
    if (!st.collapsed && pw <= bp) {
      if (st.open) { this.close(true); }
      st.collapsed = true;
      el.classList.add(RP + "-collapsed");
      this.fire("ah:collapse");
    } else if (st.collapsed && pw > bp) {
      this.close(true);
      st.collapsed = false;
      el.classList.remove(RP + "-collapsed", RP + "-open");
      rpClearStyles(this.content());
      this.fire("ah:expand");
      this.load();
    }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open() {
    const el = this.element;
    const st = this.st;
    const self = this;
    if (!st.collapsed || st.open || this.disabled()) { return; }
    const c = this.content();
    const t = this.toggleEl();
    if (!c || !t) { return; }
    const anim = el.getAttribute("data-animation") || "fade";
    const speed = rpSpeed(el, "data-show-duration");
    const cw = el.getAttribute("data-collapse-width");
    rpClearStyles(c);
    if (cw) { c.style.width = /^\d+(\.\d+)?$/.test(cw) ? cw + "px" : cw; }
    st.open = true;
    el.classList.add(RP + "-open");
    t.setAttribute("aria-expanded", "true");
    st.float = AH.float(c, t, { placement: "bottom", align: "start", offset: 4 });
    const shown = function () {
      if (st.float) { st.float.update(); }
      self.fire("ah:open");
    };
    if (anim === "fade") {
      c.style.opacity = "0";
      L.animate(c, [{ opacity: 0 }, { opacity: 1 }], speed, function () {
        c.style.opacity = "1";
        shown();
      });
    } else if (anim === "slide") {
      L.hide(c);
      L.slide(c, true, speed, shown);
    } else {
      shown();
    }
    this.load();
  }

  close(instant) {
    const el = this.element;
    const st = this.st;
    const self = this;
    if (!st.open) { return; }
    const c = this.content();
    const t = this.toggleEl();
    const anim = instant === true ? "none" : (el.getAttribute("data-animation") || "fade");
    const speed = rpSpeed(el, "data-hide-duration");
    st.open = false;
    if (t) { t.setAttribute("aria-expanded", "false"); }
    if (c && c.contains(document.activeElement) && t) { t.focus(); }
    const hidden = function () {
      el.classList.remove(RP + "-open");
      if (st.float) { st.float.stop(); st.float = null; }
      if (c) { c.style.display = c.style.opacity = ""; }
      if (instant !== true) { self.fire("ah:close"); }
    };
    if (c) { L.stop(c); }
    if (c && anim === "fade") { L.fade(c, false, speed, hidden); }
    else if (c && anim === "slide") { L.slide(c, false, speed, hidden); }
    else { hidden(); }
  }

  toggle() { if (this.st.collapsed) { this.flip(); } }
  refresh() { this.check(); }
  isCollapsed() { return !!this.st.collapsed; }
  isOpen() { return !!this.st.open; }
});
