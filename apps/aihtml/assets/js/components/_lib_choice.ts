/* Shared by the choice behaviours (checkbox.ts, radiobutton.ts,
 * switch_button.ts, checkbox_group.ts, radiobutton_group.ts,
 * radio_cards.ts): mirroring a native input onto sigil's classes, locked
 * controls and the group behaviours (ChoiceGroupController).
 *
 * The single controls keep a native <input> inside a <label>: the browser
 * does the toggling, the keyboard (Space) and the change event; the
 * behaviours only mirror the input's state onto sigil's classes and add
 * sigil's extras. The groups keep the root's data-ah-value in sync, stop
 * the inner inputs' change at the root and fire one "change" on the root
 * (a native event, no detail), so data-ah-on on the root sees the group
 * value. Arrow keys move a radio selection as in sigil. Methods never
 * fire change.
 */
import AH from "../core.ts";
import type { Controller } from "../core.ts";
import { join, split } from "./_lib_values.ts";

export const INPUT = "input.ah-choice-input";

/** Mirrors an input's state onto its control's classes. */
export type Sync = (input: HTMLInputElement) => void;

export function truthy(v: unknown): boolean {
  return v === true || v === "true" || v === "on" || v === 1 || v === "1";
}

function toggle(node: Element | null, cls: string, on: boolean): void {
  if (node) { node.classList.toggle(cls, !!on); }
}

export function syncCheckbox(input: HTMLInputElement): void {
  const mixed = input.indeterminate;
  const on = input.checked && !mixed;
  const box = input.closest(".ah-checkbox");
  if (!box) { return; }
  toggle(box, "ah-checkbox-checked", on);
  toggle(box, "ah-checkbox-indeterminate", mixed);
  toggle(box, "ah-checkbox-disabled", input.disabled);
  box.querySelectorAll(".ah-checkbox-check").forEach((c) => {
    toggle(c, "ah-checkbox-check-checked", on);
    toggle(c, "ah-checkbox-check-indeterminate", mixed);
  });
}

export function syncRadio(input: HTMLInputElement): void {
  const r = input.closest(".ah-radiobutton");
  if (!r) { return; }
  toggle(r, "ah-radiobutton-checked", input.checked);
  toggle(r, "ah-radiobutton-disabled", input.disabled);
  r.querySelectorAll(".ah-radiobutton-check").forEach((c) => {
    toggle(c, "ah-radiobutton-check-checked", input.checked);
  });
}

export function syncSwitch(input: HTMLInputElement): void {
  const s = input.closest(".ah-switch");
  toggle(s, "ah-switch-on", input.checked);
  toggle(s, "ah-switch-disabled", input.disabled);
}

/** The native input of a single control; the server always renders it. */
export function inputOf(el: Element): HTMLInputElement {
  return el.querySelector(INPUT) as HTMLInputElement;
}

/** locked: focusable but the user cannot change it. ctrl: the root's
 *  controller (in its setup). */
export function bindLocked(ctrl: Controller): void {
  ctrl.delegate("click", INPUT, (e) => {
    if (ctrl.element.hasAttribute("data-ah-locked")) {
      e.preventDefault();
    }
  });
}

export function setDisabled(input: HTMLInputElement, on: unknown, sync: Sync): void {
  input.disabled = !!on;
  sync(input);
}

// ------------------------------------------------------------------
// Groups
// ------------------------------------------------------------------

/** What differs between the groups: radio or not, how an input is
 *  mirrored, the item selector and how disabled shows. */
export interface GroupKind {
  radio: boolean;
  sync: Sync;
  item: string;
  /** class of an item whose input is disabled */
  itemDisabled?: string;
  /** class of the disabled root */
  disabled?: string;
  /** data-disabled="true|false" on the root */
  disabledAttr?: boolean;
}

const ARROW_STEP: Record<string, number> = { ArrowRight: 1, ArrowDown: 1, ArrowLeft: -1, ArrowUp: -1 };

/** The behaviour of checkbox-group, radiobutton-group and radio-cards: a
 *  subclass names its GroupKind. A radio group's value is the checked
 *  value itself; a checkbox group's is _lib_values text ("a,b", commas in
 *  values escaped). */
export abstract class ChoiceGroupController extends AH.Controller {
  protected abstract readonly kind: GroupKind;

  override setup(): void {
    this.sync();
    this.delegate("change", INPUT, (e) => {
      // one change per user action, fired by the root itself: the
      // input's own change goes no further (not even to the root's
      // later listeners, as with jQuery's delegated stop before)
      e.stopImmediatePropagation();
      this.changed();
    });
    if (this.kind.radio) {
      // Arrow keys: move to the next/previous enabled radio, select and
      // focus it.
      this.delegate<KeyboardEvent, HTMLInputElement>("keydown", INPUT, (e, input) => {
        const step = ARROW_STEP[e.key];
        if (!step || e.altKey || e.ctrlKey || e.metaKey) { return; }
        const enabled = this.inputs().filter((i) => !i.disabled);
        const n = enabled.length;
        if (!n) { return; }
        e.preventDefault();
        const i = enabled.indexOf(input);
        const next = enabled[((i < 0 ? 0 : i) + step + n) % n];
        next.checked = true;
        next.focus();
        this.changed();
      });
    }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  /** a value, an array or "a,b" (_lib_values); does not fire change */
  setValue(v: unknown): void {
    const kind = this.kind;
    let vals = Array.isArray(v) ? v.map(String)
      : (v === null || v === undefined || v === "") ? []
      : kind.radio ? [String(v)] : split(String(v));
    if (kind.radio) { vals = vals.slice(0, 1); }
    this.inputs().forEach((i) => {
      i.checked = vals.indexOf(i.value) >= 0;
    });
    this.sync();
  }

  getValue(): string | string[] {
    const v = this.value();
    return this.kind.radio ? v : split(v);
  }

  setDisabled(on: unknown): void {
    const el = this.element, kind = this.kind;
    this.inputs().forEach((i) => {
      const own = !!i.closest("[data-ah-item-disabled]");
      i.disabled = !!on || own;
      if (kind.itemDisabled) {
        toggle(i.closest(kind.item), kind.itemDisabled, i.disabled);
      }
    });
    if (kind.disabled) { el.classList.toggle(kind.disabled, !!on); }
    if (kind.disabledAttr) { el.setAttribute("data-disabled", on ? "true" : "false"); }
    if (on) { el.setAttribute("aria-disabled", "true"); } else { el.removeAttribute("aria-disabled"); }
    this.sync();
  }

  private inputs(): HTMLInputElement[] {
    return Array.from(this.element.querySelectorAll<HTMLInputElement>(INPUT));
  }

  private value(): string {
    const vals = this.inputs().filter((i) => i.checked).map((i) => i.value);
    return this.kind.radio ? (vals.length ? vals[0] : "") : join(vals);
  }

  private sync(): void {
    const el = this.element;
    const all = this.inputs();
    all.forEach((i) => { this.kind.sync(i); });
    el.setAttribute("data-ah-value", this.value());
    if (this.kind.radio) {
      // roving tab stop: the checked radio, else the first enabled one
      const enabled = all.filter((i) => !i.disabled);
      const stop = enabled.find((i) => i.checked) || enabled[0];
      all.forEach((i) => { i.setAttribute("tabindex", "-1"); });
      if (stop) { stop.setAttribute("tabindex", "0"); }
    }
  }

  private changed(): void {
    this.sync();
    this.fire("change");
  }
}
