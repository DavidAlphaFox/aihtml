/* Behaviour of slider (designs/04-components.md). Ported from sigil
   (sigil.components.form.slider). The root keeps data-ah-value and the
   hidden input in step and fires "input" / "change" (native events, no
   detail). */
import AH from "../core.js";

// ------------------------------------------------------------------
// slider
// ------------------------------------------------------------------
//
// Positions are written as calc() fractions of the track (as the server
// renders them), so nothing needs re-measuring on resize. Keyboard, per
// sigil: Left/Down decrease, Right/Up increase, Home/End; plus
// PageUp/PageDown by ten steps. Buttons step like sigil's; the wheel
// steps while the slider has focus.

var THUMB = 18;

function frac(r) {
  return "calc((100% - " + THUMB + "px) * " + (+r.toFixed(4)) + ")";
}

function fracCenter(r) {
  return "calc((100% - " + THUMB + "px) * " + (+r.toFixed(4)) + " + " + THUMB / 2 + "px)";
}

function css(node, props) {
  if (node) { Object.assign(node.style, props); }
}

AH.register("slider", class extends AH.Controller {
  setup() {
    var el = this.element, c = this.conf();
    var state = this.state = { dragging: false, thumb: undefined, startValue: null };

    this.delegate("pointerdown", ".ah-slider-content", (e) => {
      if (this.disabled() || e.button !== 0) { return; }
      e.preventDefault();
      var thumb = e.target.closest(".ah-slider-thumb");
      var v = this.fromPointer(e);
      var which = "end";
      if (c.range) {
        if (thumb) {
          which = thumb.classList.contains("ah-slider-thumb-start") ? "start" : "end";
        } else {
          var vals = this.values();
          which = Math.abs(v - vals[0]) <= Math.abs(v - vals[1]) ? "start" : "end";
        }
      }
      state.dragging = true;
      state.thumb = which;
      state.startValue = el.getAttribute("data-ah-value");
      var t = el.querySelector(".ah-slider-thumb-" + which);
      if (t) { t.classList.add("ah-slider-thumb-dragging"); }
      var target = c.range ? t : el;
      if (target) { target.focus({ preventScroll: true }); }
      this.showTip(true);
      try { el.setPointerCapture(e.pointerId); } catch (err) { /* synthetic event */ }
      // Pressing the track jumps the nearest thumb there.
      if (!thumb && this.set(which, v)) { this.fire("input"); }
      this.tooltip();
    });
    this.listen(el, "pointermove", (e) => {
      if (!state.dragging) { return; }
      if (this.set(state.thumb, this.fromPointer(e))) { this.fire("input"); }
    });
    var up = (e) => {
      if (!state.dragging) { return; }
      state.dragging = false;
      el.querySelectorAll(".ah-slider-thumb").forEach((t) => { t.classList.remove("ah-slider-thumb-dragging"); });
      try { el.releasePointerCapture(e.pointerId); } catch (err) { /* not captured */ }
      if (!el.contains(document.activeElement)) { this.showTip(false); }
      if (el.getAttribute("data-ah-value") !== state.startValue) { this.fire("change"); }
    };
    this.listen(el, "pointerup", up);
    this.listen(el, "pointercancel", up);

    this.delegate("click", ".ah-slider-button", (e, btn) => {
      if (this.disabled()) { return; }
      var inc = btn.classList.contains("ah-slider-button-next");
      // sigil: in range mode "+" moves the end thumb, "-" the start one
      this.stepBy(c.range ? (inc ? "end" : "start") : "end", inc ? c.step : -c.step);
    });

    this.listen(el, "keydown", (e) => {
      if (this.disabled()) { return; }
      var which = "end";
      if (c.range) {
        var t = e.target.closest && e.target.closest(".ah-slider-thumb");
        if (!t) { return; }
        which = t.classList.contains("ah-slider-thumb-start") ? "start" : "end";
      }
      state.thumb = which;
      var big = c.step * Math.max(1, Math.round((c.max - c.min) / c.step / 10));
      var delta = { ArrowRight: c.step, ArrowUp: c.step, ArrowLeft: -c.step, ArrowDown: -c.step,
                    PageUp: big, PageDown: -big }[e.key];
      if (delta !== undefined) {
        e.preventDefault();
        this.showTip(true);
        this.stepBy(which, delta);
      } else if (e.key === "Home" || e.key === "End") {
        e.preventDefault();
        this.showTip(true);
        if (this.set(which, e.key === "Home" ? c.min : c.max)) {
          this.fire("input");
          this.fire("change");
        }
      }
    });

    this.listen(el, "wheel", (e) => {
      if (this.disabled() || !el.contains(document.activeElement)) { return; }
      e.preventDefault();
      var which = c.range && document.activeElement.classList.contains("ah-slider-thumb-start")
        ? "start" : "end";
      this.stepBy(which, e.deltaY < 0 ? c.step : -c.step);
    }, { passive: false });

    this.listen(el, "focusin", (e) => {
      el.classList.add("ah-slider-focused");
      if (c.range) {
        state.thumb = e.target.classList.contains("ah-slider-thumb-start") ? "start" : "end";
      }
      this.tooltip();
    });
    this.listen(el, "focusout", (e) => {
      if (!e.relatedTarget || !el.contains(e.relatedTarget)) {
        el.classList.remove("ah-slider-focused");
        if (!state.dragging) { this.showTip(false); }
      }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue() { return this.element.getAttribute("data-ah-value"); }
  // setValue(v | [lo, hi] | "lo,hi"[, silent])
  setValue(v, silent) {
    var c = this.conf();
    var vals = Array.isArray(v) ? v : String(v).split(",");
    vals = vals.map((x) => this.snap(parseFloat(x)));
    if (c.range) { vals = [Math.min(vals[0], vals[1]), Math.max(vals[0], vals[1])]; }
    var old = this.element.getAttribute("data-ah-value");
    this.render(vals);
    if (!silent && this.element.getAttribute("data-ah-value") !== old) { this.fire("change"); }
  }

  conf() {
    if (!this.c) {
      var el = this.element;
      var step = parseFloat(el.getAttribute("data-ah-step")) || 1;
      var c = {
        min: parseFloat(el.getAttribute("data-ah-min")) || 0,
        max: parseFloat(el.getAttribute("data-ah-max")),
        step: step,
        decimals: (String(step).split(".")[1] || "").length,
        minRange: parseFloat(el.getAttribute("data-ah-min-range")) || 0,
        vertical: el.classList.contains("ah-slider-vertical"),
        range: el.classList.contains("ah-slider-range-slider")
      };
      if (isNaN(c.max)) { c.max = 100; }
      this.c = c;
    }
    return this.c;
  }

  values() {
    return String(this.element.getAttribute("data-ah-value") || "").split(",").map(parseFloat);
  }

  snap(v) {
    var c = this.conf();
    var n = Math.round((v - c.min) / c.step);
    var snapped = c.min + n * c.step;
    snapped = Math.max(c.min, Math.min(c.max, snapped));
    return parseFloat(snapped.toFixed(c.decimals));
  }

  render(vals) {
    var el = this.element, c = this.conf();
    var ratio = (v) => (v - c.min) / (c.max - c.min);
    var range = el.querySelector(".ah-slider-range");
    var end = el.querySelector(".ah-slider-thumb-end");
    var start = el.querySelector(".ah-slider-thumb-start");
    var pos = (t, v) => {
      if (!t) { return; }
      if (c.vertical) { t.style.top = frac(1 - ratio(v)); } else { t.style.left = frac(ratio(v)); }
    };
    var aria = (node, v) => {
      if (!node) { return; }
      node.setAttribute("aria-valuenow", v);
      node.setAttribute("aria-valuetext", v);
    };
    if (c.range) {
      pos(start, vals[0]);
      pos(end, vals[1]);
      if (c.vertical) {
        css(range, { bottom: fracCenter(ratio(vals[0])), height: frac(ratio(vals[1]) - ratio(vals[0])) });
      } else {
        css(range, { left: fracCenter(ratio(vals[0])), width: frac(ratio(vals[1]) - ratio(vals[0])) });
      }
      aria(start, vals[0]);
      aria(end, vals[1]);
    } else {
      pos(end, vals[0]);
      if (c.vertical) { css(range, { bottom: "0px", height: fracCenter(ratio(vals[0])) }); }
      else { css(range, { left: "0px", width: fracCenter(ratio(vals[0])) }); }
      aria(el, vals[0]);
    }
    // Value-bearing contract: data-ah-value + hidden input
    var v = vals.join(",");
    el.setAttribute("data-ah-value", v);
    el.querySelectorAll(":scope > input[type=hidden]").forEach((h) => { h.value = v; });
    this.tooltip();
  }

  tooltip() {
    var el = this.element;
    var tip = el.querySelector(":scope > .ah-slider-tooltip");
    var s = this.state || {};
    if (!tip) { return; }
    var thumb = s.thumb === "start" ? el.querySelector(".ah-slider-thumb-start")
      : el.querySelector(".ah-slider-thumb-end");
    var vals = this.values();
    tip.textContent = s.thumb === "start" ? vals[0] : vals[vals.length - 1];
    if (!thumb) { return; }
    var root = el.getBoundingClientRect();
    var r = thumb.getBoundingClientRect();
    if (this.conf().vertical) {
      tip.style.top = (r.top + r.height / 2 - root.top) + "px";
    } else {
      tip.style.left = (r.left + r.width / 2 - root.left) + "px";
    }
  }

  // Set one thumb ("start" | "end") to v, keeping the range ordered.
  set(which, v) {
    var c = this.conf();
    var vals = this.values();
    v = this.snap(v);
    if (c.range) {
      if (which === "start") { vals[0] = Math.min(v, vals[1] - c.minRange); }
      else { vals[1] = Math.max(v, vals[0] + c.minRange); }
      vals[0] = Math.max(c.min, vals[0]);
      vals[1] = Math.min(c.max, vals[1]);
    } else {
      vals = [v];
    }
    var old = this.element.getAttribute("data-ah-value");
    this.render(vals);
    return this.element.getAttribute("data-ah-value") !== old;
  }

  fromPointer(e) {
    var c = this.conf();
    var r = this.element.querySelector(".ah-slider-track").getBoundingClientRect();
    var ratio = c.vertical
      ? 1 - (e.clientY - r.top - THUMB / 2) / (r.height - THUMB)
      : (e.clientX - r.left - THUMB / 2) / (r.width - THUMB);
    ratio = Math.max(0, Math.min(1, ratio));
    return c.min + ratio * (c.max - c.min);
  }

  disabled() {
    var el = this.element;
    return el.classList.contains("ah-slider-disabled") || el.getAttribute("aria-disabled") === "true";
  }

  showTip(on) {
    var tip = this.element.querySelector(":scope > .ah-slider-tooltip");
    if (tip) { tip.classList.toggle("ah-slider-tooltip-visible", on); }
  }

  stepBy(which, delta) {
    var c = this.conf();
    var vals = this.values();
    var cur = c.range ? (which === "start" ? vals[0] : vals[1]) : vals[0];
    if (this.set(which, cur + delta)) {
      this.fire("input");
      this.fire("change");
    }
  }
});
