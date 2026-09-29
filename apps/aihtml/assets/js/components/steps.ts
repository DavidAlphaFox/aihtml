/* Behaviour of steps (value: the current index; fires change). */
import AH from "../core.ts";
import { listKeys, setValue } from "./_lib_layout.ts";
import "virtual:ah-tpl/steps_indicator";

const STEP_STATES = ["ah-steps-item-pending", "ah-steps-item-active", "ah-steps-item-completed",
                     "ah-steps-item-error"];

// Status class and indicator, the indicator from the server's template
// (templates/steps_indicator.mustache).
function stepIndicator(it: Element, i: number, status: string): void {
  it.classList.add("ah-steps-item-" + status);
  it.querySelectorAll(":scope > .ah-steps-indicator").forEach((ind) => {
    ind.innerHTML = AH.tpl.steps_indicator({
      check: status === "completed", error: status === "error",
      plain: status !== "completed" && status !== "error", number: i + 1
    });
  });
}

function connectorDone(it: Element, on: boolean): void {
  it.querySelectorAll(":scope > .ah-steps-connector").forEach((c) => {
    c.classList.toggle("ah-steps-connector-done", on);
  });
}

class StepsController extends AH.Controller {
  override setup(): void {
    const el = this.element;
    const header = el.querySelector(":scope > .ah-steps-header");
    if (header) {
      this.delegate("click", ".ah-steps-item-clickable", (_e, item) => {
        if (!el.classList.contains("ah-steps-disabled")) {
          this.choose(this.items().indexOf(item), true);
        }
      }, header);
      this.delegate("keydown", ".ah-steps-item-clickable", (e, item) => {
        const items = this.items();
        const vertical = el.classList.contains("ah-steps-vertical");
        const t = listKeys(e, items, items.indexOf(item), vertical, "ah-steps-item-disabled");
        if (t === null) { return; }
        e.preventDefault();
        this.choose(t, true);
        items[t].focus();
      }, header);
    }
    this.delegate<MouseEvent, HTMLButtonElement>("click", ".ah-steps-btn", (_e, btn) => {
      if (!btn.parentNode || btn.parentNode.parentNode !== el || btn.disabled) { return; }
      const dir = btn.getAttribute("data-action") === "prev" ? -1 : 1;
      this.choose(this.move(this.current(), dir), true);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  select(i: number | string): void { this.choose(parseInt(String(i), 10), false); }
  next(): void { this.choose(this.move(this.current(), 1), false); }
  prev(): void { this.choose(this.move(this.current(), -1), false); }
  first(): void { this.choose(0, false); }
  last(): void { this.choose(this.items().length - 1, false); }
  setStatus(i: number | string, status: string): void {
    const idx = parseInt(String(i), 10);
    const it = this.items()[idx];
    if (!it) { return; }
    it.classList.remove(...STEP_STATES, "ah-steps-item-disabled");
    stepIndicator(it, idx, status);
    connectorDone(it, status === "completed");
  }
  value(): number { return this.current(); }

  private items(): HTMLElement[] {
    return Array.from(this.element.querySelectorAll<HTMLElement>(":scope > .ah-steps-header > .ah-steps-item"));
  }

  private current(): number {
    return parseInt(this.element.getAttribute("data-ah-value") || "", 10) || 0;
  }

  private choose(idx: number, user: boolean): boolean {
    const el = this.element;
    const items = this.items();
    const cur = this.current();
    const n = items.length;
    if (!(idx >= 0 && idx < n) || idx === cur || items[idx].classList.contains("ah-steps-item-disabled")) {
      return false;
    }
    const clickable = el.getAttribute("data-clickable") !== "false";
    items.forEach((it) => {
      it.classList.remove("ah-steps-item-selected");
      it.removeAttribute("aria-current");
    });
    items[idx].classList.add("ah-steps-item-selected");
    items[idx].setAttribute("aria-current", "step");
    items.forEach((it, i) => {
      if (it.getAttribute("role") === "button") { it.setAttribute("tabindex", i === idx ? "0" : "-1"); }
      if (it.classList.contains("ah-steps-item-disabled") || it.classList.contains("ah-steps-item-error")) {
        return;
      }
      it.classList.remove(...STEP_STATES);
      stepIndicator(it, i, i < idx ? "completed" : (i === idx ? "active" : "pending"));
    });
    items.forEach((it) => {
      connectorDone(it, it.classList.contains("ah-steps-item-completed"));
    });
    el.querySelectorAll(":scope > .ah-steps-panels > .ah-steps-panel").forEach((p, i) => {
      p.classList.toggle("ah-steps-panel-active", i === idx);
    });
    const toggle = (action: string, on: boolean): void => {
      el.querySelectorAll<HTMLButtonElement>(":scope > .ah-steps-nav > [data-action=" + action + "]").forEach((b) => {
        b.disabled = !on;
        b.classList.toggle("ah-steps-btn-disabled", !on);
      });
    };
    toggle("prev", clickable && idx > 0);
    toggle("next", clickable && idx < n - 1);
    setValue(el, String(idx));
    if (user) {
      this.fire("change");
    }
    return true;
  }

  // Next non-disabled step from cur in direction dir, or cur.
  private move(cur: number, dir: number): number {
    const items = this.items();
    for (let i = cur + dir; i >= 0 && i < items.length; i += dir) {
      if (!items[i].classList.contains("ah-steps-item-disabled")) { return i; }
    }
    return cur;
  }
}

AH.register("steps", StepsController);
