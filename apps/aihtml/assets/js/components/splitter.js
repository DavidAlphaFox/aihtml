/* Behaviour of the splitter component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.js.
   Value contract: data-ah-value "<pane0 %>,<pane1 %>", the hidden input,
   "input" while dragging and "change" when a change ends. Other events
   on the root (no detail): ah:resize-start, ah:resize, ah:collapsed,
   ah:expanded. */
import AH from "../core.js";
import "./_lib_nav.js";

const setValue = AH.lib.nav.setValue;

// ------------------------------------------------------------------
// splitter (sigil splitter.cljs)
// ------------------------------------------------------------------
//
// The first pane's flex-basis is a fraction of the space left by the
// bar, so the split keeps its proportion when the container resizes.

function spState(el) {
  const mins = (el.getAttribute("data-ah-min") || "0,0").split(",").map(function (x) {
    return parseFloat(x) || 0;
  });
  return {
    horiz: el.classList.contains("ah-splitter-horizontal"),
    p: Array.from(el.querySelectorAll(":scope > .ah-splitter-panel")),
    bar: el.querySelector(":scope > .ah-splitter-splitbar"),
    min0: mins[0],
    min1: mins[1] || 0,
    frac: null,
    saved: null
  };
}

function spDim(st, node) { return st.horiz ? node.offsetHeight : node.offsetWidth; }
function spAvail(el, st) { return spDim(st, el) - spDim(st, st.bar); }

function spFormat(f) {
  const a = Math.round(f * 1000) / 10;
  const b = Math.round((100 - a) * 10) / 10;
  return a + "," + b;
}

function spApply(el, st, frac) {
  st.frac = Math.max(0, Math.min(1, frac));
  const bar = spDim(st, st.bar);
  st.p[0].style.flex = "0 0 calc((100% - " + bar + "px) * " + st.frac.toFixed(5) + ")";
  st.bar.setAttribute("aria-valuenow", String(Math.round(st.frac * 100)));
}

// Resize pane 0 to px (clamped to the minimum sizes).
function spResize(el, st, px) {
  const avail = spAvail(el, st);
  if (avail <= 0) { return; }
  const clamped = Math.max(st.min0, Math.min(avail - st.min1, px));
  spApply(el, st, clamped / avail);
}

function spCollapsed(el, st, on) {
  el.classList.toggle("ah-splitter-collapsed", on);
  st.p[0].style[st.horiz ? "minHeight" : "minWidth"] = on ? "0px" : st.min0 + "px";
}

function fire(el, type) {
  el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true }));
}

function spToggle(el, st) {
  if (el.classList.contains("ah-splitter-collapsed")) {
    spCollapsed(el, st, false);
    spApply(el, st, st.saved !== null ? st.saved : 0.5);
    fire(el, "ah:expanded");
  } else {
    st.saved = st.frac;
    spCollapsed(el, st, true);
    spApply(el, st, 0);
    fire(el, "ah:collapsed");
  }
  setValue(el, spFormat(st.frac), "change");
}

function spEnabled(el) {
  return !el.classList.contains("ah-splitter-disabled") && el.getAttribute("data-ah-resizable") !== "false";
}

AH.register("splitter", class extends AH.Controller {
  setup() {
    const el = this.element;
    const st = this.st = spState(el);
    if (!st.bar || st.p.length < 2) { return; }
    const bar = st.bar;
    const avail = spAvail(el, st);
    // measure the initial split (pixels or percent) as a fraction
    if (avail > 0) {
      const v = el.getAttribute("data-ah-value");
      spApply(el, st, v ? parseFloat(v) / 100 : spDim(st, st.p[0]) / avail);
    }
    let drag = null;
    this.listen(bar, "pointerdown", function (e) {
      if (e.button !== 0 || !spEnabled(el) || e.target.closest(".ah-splitter-collapse-btn")) {
        return;
      }
      e.preventDefault();
      if (bar.setPointerCapture) {
        try { bar.setPointerCapture(e.pointerId); } catch (err) { /* synthetic event */ }
      }
      drag = { start: st.horiz ? e.clientY : e.clientX, size: spDim(st, st.p[0]),
               value: el.getAttribute("data-ah-value") };
      if (el.classList.contains("ah-splitter-collapsed")) { spCollapsed(el, st, false); }
      el.classList.add("ah-splitter-dragging");
      fire(el, "ah:resize-start");
    });
    this.listen(bar, "pointermove", function (e) {
      if (!drag) { return; }
      const want = drag.size + (st.horiz ? e.clientY : e.clientX) - drag.start;
      const max = spAvail(el, st) - st.min1;
      bar.classList.toggle("ah-splitbar-invalid", want <= st.min0 || want >= max);
      spResize(el, st, want);
      setValue(el, spFormat(st.frac), "input");
    });
    const end = function () {
      if (!drag) { return; }
      const before = drag.value;
      drag = null;
      bar.classList.remove("ah-splitbar-invalid");
      el.classList.remove("ah-splitter-dragging");
      const v = spFormat(st.frac);
      if (v !== before) { setValue(el, v, "change"); }
      fire(el, "ah:resize");
    };
    this.listen(bar, "pointerup", end);
    this.listen(bar, "pointercancel", end);
    this.delegate("click", ".ah-splitter-collapse-btn", function (e) {
      e.stopPropagation();
      if (spEnabled(el) || el.classList.contains("ah-splitter-collapsed")) { spToggle(el, st); }
    }, bar);
    this.listen(bar, "keydown", function (e) {
      if (e.target !== bar || !spEnabled(el)) { return; }
      const step = (parseInt(el.getAttribute("data-ah-step"), 10) || 10) * (e.shiftKey ? 5 : 1);
      const cur = spDim(st, st.p[0]);
      const dec = st.horiz ? "ArrowUp" : "ArrowLeft";
      const inc = st.horiz ? "ArrowDown" : "ArrowRight";
      let px;
      switch (e.key) {
        case dec: px = cur - step; break;
        case inc: px = cur + step; break;
        case "Home": px = 0; break;
        case "End": px = Infinity; break;
        case "Enter": e.preventDefault(); spToggle(el, st); return;
        default: return;
      }
      e.preventDefault();
      if (el.classList.contains("ah-splitter-collapsed")) { spCollapsed(el, st, false); }
      spResize(el, st, px);
      const v = spFormat(st.frac);
      if (v !== el.getAttribute("data-ah-value")) { setValue(el, v, "change"); }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  // sizes: pane 0 in percent (a number or "30"); fires no change
  setSizes(pct) {
    const st = this.st;
    spCollapsed(this.element, st, false);
    spApply(this.element, st, parseFloat(pct) / 100);
    setValue(this.element, spFormat(st.frac));
  }
  getSizes() {
    const st = this.st;
    return [spDim(st, st.p[0]), spDim(st, st.p[1])];
  }
  collapse() {
    if (!this.element.classList.contains("ah-splitter-collapsed")) { spToggle(this.element, this.st); }
  }
  expand() {
    if (this.element.classList.contains("ah-splitter-collapsed")) { spToggle(this.element, this.st); }
  }
});
