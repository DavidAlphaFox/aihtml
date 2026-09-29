/* Behaviour of expander (value "true" / "false"; fires change).
 * Events on the root: ah:expanding / ah:collapsing when a change starts,
 * ah:expanded / ah:collapsed when its animation ends (no detail). */
import AH from "../core.js";
import "./_lib_layout.js";

const L = AH.lib.layout;

function fire(el, type) {
  el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true }));
}

function header(el) { return el.querySelector(":scope > .ah-expander-header"); }

function expIsOpen(el) {
  const h = header(el);
  return !!h && h.getAttribute("aria-expanded") === "true";
}

function expSet(el, open, user) {
  if (expIsOpen(el) === open) {
    return;
  }
  const h = header(el);
  const b = el.querySelector(":scope > .ah-expander-body");
  const anim = el.getAttribute("data-animation") || "slide";
  const dur = parseInt(el.getAttribute("data-duration") || "250", 10);
  fire(el, open ? "ah:expanding" : "ah:collapsing");
  h.classList.toggle("ah-expander-header-expanded", open);
  h.setAttribute("aria-expanded", String(open));
  h.querySelectorAll(":scope > .ah-expander-arrow").forEach(function (a) {
    a.classList.toggle("ah-expander-arrow-expanded", open);
  });
  L.setValue(el, String(open));
  const done = function () {
    fire(el, open ? "ah:expanded" : "ah:collapsed");
  };
  if (b) {
    L.stop(b);
    if (anim === "slide") {
      L.slide(b, open, dur, done);
    } else if (anim === "fade") {
      L.fade(b, open, dur, done);
    } else {
      if (open) { L.show(b); } else { L.hide(b); }
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
    document.querySelectorAll("[data-ah=expander]").forEach(function (other) {
      if (other !== el && other.getAttribute("data-accordion") === group) {
        expSet(other, false, user);
      }
    });
  }
}

AH.register("expander", class extends AH.Controller {
  setup() {
    const el = this.element;
    const mode = el.getAttribute("data-toggle-mode") || "click";
    if (mode === "none") {
      return;
    }
    const fromUser = function (e, h) {
      if (h.parentNode !== el || el.classList.contains("ah-expander-disabled")) {
        return;
      }
      e.preventDefault();
      expSet(el, !expIsOpen(el), true);
    };
    this.delegate(mode, ".ah-expander-header", fromUser);
    this.delegate("keydown", ".ah-expander-header", function (e, h) {
      if (e.target === h && (L.key(e) === "Enter" || L.key(e) === " ")) {
        fromUser(e, h);
      }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  open() { expSet(this.element, true, false); }
  close() { expSet(this.element, false, false); }
  toggle() { expSet(this.element, !expIsOpen(this.element), false); }
  isOpen() { return expIsOpen(this.element); }
});
