/* Behaviour of steps (value: the current index; fires change). */
import AH from "../core.js";
import "./_lib_layout.js";
import "virtual:ah-tpl/steps_indicator";

const L = AH.lib.layout;

const STEP_STATES = ["ah-steps-item-pending", "ah-steps-item-active", "ah-steps-item-completed",
                     "ah-steps-item-error"];

function stepItems(el) {
  return Array.from(el.querySelectorAll(":scope > .ah-steps-header > .ah-steps-item"));
}

// Status class and indicator, the indicator from the server's template
// (templates/steps_indicator.mustache).
function stepIndicator(it, i, status) {
  it.classList.add("ah-steps-item-" + status);
  it.querySelectorAll(":scope > .ah-steps-indicator").forEach(function (ind) {
    ind.innerHTML = AH.tpl.steps_indicator({
      check: status === "completed", error: status === "error",
      plain: status !== "completed" && status !== "error", number: i + 1
    });
  });
}

function connectorDone(it, on) {
  it.querySelectorAll(":scope > .ah-steps-connector").forEach(function (c) {
    c.classList.toggle("ah-steps-connector-done", on);
  });
}

function stepCur(el) {
  return parseInt(el.getAttribute("data-ah-value"), 10) || 0;
}

function stepSelect(el, idx, user) {
  const items = stepItems(el);
  const cur = stepCur(el);
  const n = items.length;
  if (idx < 0 || idx >= n || idx === cur || items[idx].classList.contains("ah-steps-item-disabled")) {
    return false;
  }
  const clickable = el.getAttribute("data-clickable") !== "false";
  items.forEach(function (it) {
    it.classList.remove("ah-steps-item-selected");
    it.removeAttribute("aria-current");
  });
  items[idx].classList.add("ah-steps-item-selected");
  items[idx].setAttribute("aria-current", "step");
  items.forEach(function (it, i) {
    if (it.getAttribute("role") === "button") { it.setAttribute("tabindex", i === idx ? "0" : "-1"); }
    if (it.classList.contains("ah-steps-item-disabled") || it.classList.contains("ah-steps-item-error")) {
      return;
    }
    it.classList.remove.apply(it.classList, STEP_STATES);
    stepIndicator(it, i, i < idx ? "completed" : (i === idx ? "active" : "pending"));
  });
  items.forEach(function (it) {
    connectorDone(it, it.classList.contains("ah-steps-item-completed"));
  });
  el.querySelectorAll(":scope > .ah-steps-panels > .ah-steps-panel").forEach(function (p, i) {
    p.classList.toggle("ah-steps-panel-active", i === idx);
  });
  const toggle = function (action, on) {
    el.querySelectorAll(":scope > .ah-steps-nav > [data-action=" + action + "]").forEach(function (b) {
      b.disabled = !on;
      b.classList.toggle("ah-steps-btn-disabled", !on);
    });
  };
  toggle("prev", clickable && idx > 0);
  toggle("next", clickable && idx < n - 1);
  L.setValue(el, String(idx));
  if (user) {
    el.dispatchEvent(new CustomEvent("change", { bubbles: true, cancelable: true }));
  }
  return true;
}

// Next non-disabled step from cur in direction dir, or cur.
function stepMove(el, cur, dir) {
  const items = stepItems(el);
  for (let i = cur + dir; i >= 0 && i < items.length; i += dir) {
    if (!items[i].classList.contains("ah-steps-item-disabled")) { return i; }
  }
  return cur;
}

AH.register("steps", class extends AH.Controller {
  setup() {
    const el = this.element;
    const header = el.querySelector(":scope > .ah-steps-header");
    if (header) {
      this.delegate("click", ".ah-steps-item-clickable", function (e, item) {
        if (!el.classList.contains("ah-steps-disabled")) {
          stepSelect(el, stepItems(el).indexOf(item), true);
        }
      }, header);
      this.delegate("keydown", ".ah-steps-item-clickable", function (e, item) {
        const items = stepItems(el);
        const vertical = el.classList.contains("ah-steps-vertical");
        const t = L.listKeys(e, items, items.indexOf(item), vertical, "ah-steps-item-disabled");
        if (t === null) { return; }
        e.preventDefault();
        stepSelect(el, t, true);
        items[t].focus();
      }, header);
    }
    this.delegate("click", ".ah-steps-btn", function (e, btn) {
      if (btn.parentNode.parentNode !== el || btn.disabled) { return; }
      const dir = btn.getAttribute("data-action") === "prev" ? -1 : 1;
      stepSelect(el, stepMove(el, stepCur(el), dir), true);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  select(i) { stepSelect(this.element, parseInt(i, 10), false); }
  next() { stepSelect(this.element, stepMove(this.element, stepCur(this.element), 1), false); }
  prev() { stepSelect(this.element, stepMove(this.element, stepCur(this.element), -1), false); }
  first() { stepSelect(this.element, 0, false); }
  last() { stepSelect(this.element, stepItems(this.element).length - 1, false); }
  setStatus(i, status) {
    const it = stepItems(this.element)[parseInt(i, 10)];
    if (!it) { return; }
    it.classList.remove.apply(it.classList, STEP_STATES.concat(["ah-steps-item-disabled"]));
    stepIndicator(it, parseInt(i, 10), status);
    connectorDone(it, status === "completed");
  }
  value() { return stepCur(this.element); }
});
