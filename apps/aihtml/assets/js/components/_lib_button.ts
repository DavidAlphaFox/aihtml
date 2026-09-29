/* Shared by the button components (toggle_button.ts, button_group.ts,
 * segmented_control.ts, dropdown_button.ts, split_button.ts): the value of
 * a value-bearing root, arrow-key stepping and the menus of
 * dropdown-button and split-button (ButtonMenu).
 *
 * Value-bearing roots keep data-ah-value and the hidden input
 * (input[data-ah-input]) in step and fire "change" on the root (a native
 * CustomEvent, detail ButtonChange: the new value).
 */
import AH from "../core.ts";
import type { Controller, FloatHandle, FloatOptions } from "../core.ts";

/** Detail of the button components' change: the new value. */
export type ButtonChange = string;

function emit<D>(el: Element, type: string, detail?: D): boolean {
  return el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail }));
}

/** Set the value of a value-bearing root; fire change when asked. */
export function setValue(el: Element, value: unknown, fire: boolean): void {
  const v = value == null ? "" : String(value);
  const old = el.getAttribute("data-ah-value");
  el.setAttribute("data-ah-value", v);
  const hidden = el.querySelector<HTMLInputElement>(":scope > input[data-ah-input]");
  if (hidden) { hidden.value = v; }
  if (fire && old !== v) {
    emit<ButtonChange>(el, "change", v);
  }
}

/** Move focus among enabled elements: key -> new index, or -1. */
export function step(key: string, idx: number, len: number): number {
  switch (key) {
    case "ArrowRight": case "ArrowDown": return (idx + 1) % len;
    case "ArrowLeft": case "ArrowUp": return (idx - 1 + len) % len;
    case "Home": return 0;
    case "End": return len - 1;
    default: return -1;
  }
}

// ------------------------------------------------------------------
// Menus shared by dropdown-button and split-button
// ------------------------------------------------------------------

/** How a component's menu looks: its selectors and state. show() opens
 *  or closes it (m: the ButtonMenu, for float / unfloat); select() marks
 *  the chosen item. */
export interface MenuConfig {
  trigger: string;
  menu: string;
  item: string;
  disabled(el: HTMLElement): boolean;
  isOpen(el: HTMLElement): boolean;
  show(el: HTMLElement, on: boolean, m: ButtonMenu): void;
  select(el: HTMLElement, item: HTMLElement): void;
}

/** Where open() puts the focus: nowhere, the first or the last item. */
export type MenuFocus = false | "first" | "last";

type ItemElement = HTMLElement & { disabled?: boolean };

function itemDisabled(it: ItemElement): boolean {
  return !!it.disabled || it.getAttribute("data-disabled") === "true";
}

/** The menu of a dropdown-button or split-button root: "ah:open" /
 *  "ah:close" on the root (no detail), "change" (detail: the item's
 *  data-value) on every choice. Made in the controller's setup (its
 *  listeners go with the controller); destroy() in its teardown. */
export class ButtonMenu {
  readonly el: HTMLElement;
  // Popups are pinned with AH.float (position: fixed), so an ancestor
  // with overflow: hidden cannot clip them.
  #handle: FloatHandle | null = null;
  #timer: ReturnType<typeof setTimeout> | undefined;

  constructor(ctrl: Controller, private readonly cfg: MenuConfig) {
    const el = this.el = ctrl.element;
    // On the trigger and the menu themselves, so a component can keep
    // these clicks from the root (split-button) and still handle them.
    el.querySelectorAll(cfg.trigger).forEach((t) => {
      ctrl.listen(t, "click", () => {
        if (cfg.isOpen(el)) { this.close(false); } else { this.open(false); }
      });
    });
    const menu = el.querySelector(cfg.menu);
    if (menu) {
      ctrl.delegate("click", cfg.item, (_e, item) => { this.choose(item); }, menu);
    }
    ctrl.listen(el, "keydown", (e) => {
      const t = e.target as Element;
      const inMenu = !!(t.closest && t.closest(cfg.menu));
      const onTrigger = !!(t.closest && t.closest(cfg.trigger));
      if (e.key === "Escape") {
        if (cfg.isOpen(el)) { e.preventDefault(); this.close(true); }
      } else if (e.key === "Tab") {
        this.close(false);
      } else if (onTrigger && (e.key === "ArrowDown" || e.key === "ArrowUp")) {
        e.preventDefault();
        if (e.altKey && e.key === "ArrowUp") { this.close(false); return; }
        this.open(e.altKey ? false : (e.key === "ArrowUp" ? "last" : "first"));
      } else if (inMenu) {
        const items = this.items();
        const i = step(e.key, items.indexOf(t as HTMLElement), items.length);
        if (i >= 0 && e.key !== "ArrowLeft" && e.key !== "ArrowRight") {
          e.preventDefault();
          items[i].focus();
        }
      }
    });
    ctrl.listen(document, "mousedown", (e) => {
      if (cfg.isOpen(el) && !el.contains(e.target as Node | null)) { this.close(false); }
    });
  }

  open(focus: MenuFocus): void {
    const { el, cfg } = this;
    if (cfg.disabled(el)) { return; }
    if (!cfg.isOpen(el)) {
      cfg.show(el, true, this);
      this.triggers().forEach((t) => { t.setAttribute("aria-expanded", "true"); });
      emit(el, "ah:open");
    }
    if (focus) {
      const items = this.items();
      const it = focus === "last" ? items[items.length - 1] : items[0];
      if (it) { it.focus(); }
    }
  }

  close(refocus: boolean): void {
    const { el, cfg } = this;
    if (!cfg.isOpen(el)) { return; }
    cfg.show(el, false, this);
    this.triggers().forEach((t) => { t.setAttribute("aria-expanded", "false"); });
    emit(el, "ah:close");
    if (refocus) {
      const t = el.querySelector<HTMLElement>(cfg.trigger);
      if (t) { t.focus(); }
    }
  }

  /** Pin the menu below the root (again: just reposition it). */
  float(menu: HTMLElement, opts: FloatOptions): void {
    clearTimeout(this.#timer);
    if (this.#handle) { this.#handle.update(); return; }
    this.#handle = AH.float(menu, this.el, opts);
  }

  /** delay: let a closing fade finish before the menu drops back in place */
  unfloat(delay: number): void {
    clearTimeout(this.#timer);
    const stop = (): void => {
      if (this.#handle) { this.#handle.stop(); }
      this.#handle = null;
    };
    if (delay) { this.#timer = setTimeout(stop, delay); } else { stop(); }
  }

  destroy(): void { this.unfloat(0); }

  private items(): HTMLElement[] {
    const menu = this.el.querySelector(this.cfg.menu);
    if (!menu) { return []; }
    return Array.from(menu.querySelectorAll<ItemElement>(this.cfg.item)).filter((it) => !itemDisabled(it));
  }

  private triggers(): NodeListOf<Element> { return this.el.querySelectorAll(this.cfg.trigger); }

  private choose(item: ItemElement): void {
    if (itemDisabled(item)) { return; }
    this.cfg.select(this.el, item);
    this.close(true);
    // A menu is a command: choosing the same item again fires again.
    const v = item.getAttribute("data-value");
    setValue(this.el, v, false);
    emit<ButtonChange>(this.el, "change", v == null ? "" : v);
  }
}
