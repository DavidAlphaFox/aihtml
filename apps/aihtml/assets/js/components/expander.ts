/* Behaviour of expander (value "true" / "false"; fires change).
 * Events on the root: ah:expanding / ah:collapsing when a change starts,
 * ah:expanded / ah:collapsed when its animation ends (no detail).
 * Follows data-ah-value: setting it opens or closes the expander, and a
 * morph keeps the behaviour set up. */
import AH from "../core.ts";
import { fade, hide, setValue, show, slide, stop } from "./_lib_layout.ts";

class ExpanderController extends AH.Controller {
  static override attrs = { value: Boolean };

  override setup(): void {
    const el = this.element;
    const mode = el.getAttribute("data-toggle-mode") || "click";
    if (mode === "none") {
      return;
    }
    const fromUser = (e: Event, h: HTMLElement): void => {
      if (h.parentNode !== el || el.classList.contains("ah-expander-disabled")) {
        return;
      }
      e.preventDefault();
      ExpanderController.set(el, !ExpanderController.isOpen(el), true);
    };
    this.delegate<Event, HTMLElement>(mode, ".ah-expander-header", fromUser);
    this.delegate("keydown", ".ah-expander-header", (e, h) => {
      if (e.target === h && (e.key === "Enter" || e.key === " ")) {
        fromUser(e, h);
      }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  open(): void { ExpanderController.set(this.element, true, false); }
  close(): void { ExpanderController.set(this.element, false, false); }
  toggle(): void { ExpanderController.set(this.element, !ExpanderController.isOpen(this.element), false); }
  isOpen(): boolean { return ExpanderController.isOpen(this.element); }

  // data-ah-value changed: open or close to match (no change event).
  valueValueChanged(open: boolean): void { ExpanderController.set(this.element, open, false); }

  private static header(el: Element): HTMLElement | null {
    return el.querySelector<HTMLElement>(":scope > .ah-expander-header");
  }

  private static isOpen(el: Element): boolean {
    const h = ExpanderController.header(el);
    return !!h && h.getAttribute("aria-expanded") === "true";
  }

  private static fire(el: Element, type: string): void {
    el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true }));
  }

  // Static: an accordion closes the other expanders of its group too.
  private static set(el: Element, open: boolean, user: boolean): void {
    if (ExpanderController.isOpen(el) === open) {
      return;
    }
    const h = ExpanderController.header(el);
    const b = el.querySelector<HTMLElement>(":scope > .ah-expander-body");
    const anim = el.getAttribute("data-animation") || "slide";
    const dur = parseInt(el.getAttribute("data-duration") || "250", 10);
    const fire = ExpanderController.fire;
    fire(el, open ? "ah:expanding" : "ah:collapsing");
    if (h) {
      h.classList.toggle("ah-expander-header-expanded", open);
      h.setAttribute("aria-expanded", String(open));
      h.querySelectorAll(":scope > .ah-expander-arrow").forEach((a) => {
        a.classList.toggle("ah-expander-arrow-expanded", open);
      });
    }
    setValue(el, String(open));
    const done = (): void => {
      fire(el, open ? "ah:expanded" : "ah:collapsed");
    };
    if (b) {
      stop(b);
      if (anim === "slide") {
        slide(b, open, dur, done);
      } else if (anim === "fade") {
        fade(b, open, dur, done);
      } else {
        if (open) { show(b); } else { hide(b); }
        done();
      }
    } else {
      done();
    }
    if (user) {
      fire(el, "change");
    }
    // Accordion: opening one closes the others sharing its name.
    const group = el.getAttribute("data-accordion");
    if (open && group) {
      document.querySelectorAll("[data-ah=expander]").forEach((other) => {
        if (other !== el && other.getAttribute("data-accordion") === group) {
          ExpanderController.set(other, false, user);
        }
      });
    }
  }
}

AH.register("expander", ExpanderController);
