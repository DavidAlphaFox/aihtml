/* Internal: what the text entry behaviours share (input, password-input,
 * number-input, input-otp, tag-input). Ported from sigil. The helpers
 * take elements; those that listen take the root's AH.Controller, so the
 * listeners go with it. */
import type { Controller } from "../core.ts";

/** A text field of the entry behaviours. */
export type Field = HTMLInputElement | HTMLTextAreaElement;

/** Floating label: up while focused or filled (sigil util/sync-float-label!). */
export function syncLabel(el: Element, prefix: string, filled: boolean, focused: boolean): void {
  el.querySelectorAll("." + prefix + "-label").forEach((l) => {
    l.classList.toggle(prefix + "-label-float", !!(filled || focused));
  });
}

/** Focus class on the shell plus the floating label (sigil shell); after
 *  runs on blur. */
export function focusShell(ctrl: Controller<Element>, input: Field | null, prefix: string,
                           after?: () => void): void {
  const el = ctrl.element;
  if (!input) { return; }
  ctrl.listen(input, "focus", () => {
    el.classList.add(prefix + "-focused");
    syncLabel(el, prefix, true, true);
  });
  ctrl.listen(input, "blur", () => {
    el.classList.remove(prefix + "-focused");
    syncLabel(el, prefix, input.value !== "", false);
    if (after) { after(); }
  });
}

/** Update data-ah-value and the hidden input; fire change on the root
 *  (a native event, no detail). False when the value is unchanged. */
export function commitValue(el: Element, value: string): boolean {
  if (el.getAttribute("data-ah-value") === value) { return false; }
  el.setAttribute("data-ah-value", value);
  el.querySelectorAll<HTMLInputElement>(":scope > input[type=hidden]").forEach((h) => { h.value = value; });
  el.dispatchEvent(new CustomEvent("change", { bubbles: true, cancelable: true }));
  return true;
}

/** Native events of the inner fields must not reach on(...) on the root:
 *  the root reports its own change. Registered at setup, before the
 *  page's own listeners on the root, which see only the root's events. */
export function isolate(ctrl: Controller<Element>, sel: string): void {
  const stop = (e: Event): void => { e.stopImmediatePropagation(); };
  ctrl.delegate("change", sel, stop);
  ctrl.delegate("input", sel, stop);
}

/** A native event on an inner field (what the browser would send), so
 *  on(input|change, ...) and the page's listeners hear it. */
export function emit(target: EventTarget, type: string): void {
  target.dispatchEvent(new Event(type, { bubbles: true, cancelable: type !== "input" && type !== "change" }));
}
