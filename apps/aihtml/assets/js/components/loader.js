/* Behaviour of loader: show / hide, optionally with a modal scrim. */
import AH from "../core.js";
import "./_lib_layout.js";

const L = AH.lib.layout;

const MODAL_ID = "ah-loader-modal";

// One Escape handler on document for the modal loader shown last.
let escHandler = null;

function setEsc(fn) {
  if (escHandler) { document.removeEventListener("keyup", escHandler); }
  escHandler = fn;
  if (fn) { document.addEventListener("keyup", fn); }
}

function loaderShow(el, left, top) {
  const modal = el.getAttribute("data-modal") === "true";
  if (modal) {
    let m = document.getElementById(MODAL_ID);
    if (!m) {
      m = document.createElement("div");
      m.id = MODAL_ID;
      m.className = "ah-loader-modal";
      document.body.appendChild(m);
    }
    m.classList.remove("ah-loader-hidden");
    setEsc(function (e) {
      if (L.key(e) === "Escape") { loaderHide(el); }
    });
  }
  el.classList.remove("ah-loader-hidden");
  el.setAttribute("aria-busy", "true");
  if (left !== undefined && left !== null && top !== undefined && top !== null) {
    el.classList.remove("ah-loader-center");
    el.style.left = left + "px";
    el.style.top = top + "px";
  } else if (modal) {
    el.classList.add("ah-loader-center");
  }
}

function loaderHide(el) {
  el.classList.add("ah-loader-hidden");
  el.setAttribute("aria-busy", "false");
  if (el.getAttribute("data-modal") === "true") {
    const m = document.getElementById(MODAL_ID);
    if (m) { m.classList.add("ah-loader-hidden"); }
    setEsc(null);
  }
}

AH.register("loader", class extends AH.Controller {
  setup() {
    const el = this.element;
    if (el.getAttribute("data-modal") === "true" && !el.classList.contains("ah-loader-hidden")) {
      loaderShow(el);
    }
  }

  teardown() {
    if (this.element.getAttribute("data-modal") === "true") {
      loaderHide(this.element);
    }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  show(left, top) { loaderShow(this.element, left, top); }
  hide() { loaderHide(this.element); }
  toggle() {
    if (this.element.classList.contains("ah-loader-hidden")) { loaderShow(this.element); }
    else { loaderHide(this.element); }
  }
  text(t) {
    this.element.querySelectorAll(":scope > .ah-loader-text").forEach(function (n) {
      n.textContent = t;
    });
    this.element.setAttribute("aria-label", t);
  }
  isOpen() { return !this.element.classList.contains("ah-loader-hidden"); }
});
