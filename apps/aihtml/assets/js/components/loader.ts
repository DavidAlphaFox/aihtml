/* Behaviour of loader: show / hide, optionally with a modal scrim. */
import AH from "../core.ts";

const MODAL_ID = "ah-loader-modal";

type KeyHandler = (e: KeyboardEvent) => void;

class LoaderController extends AH.Controller {
  // One Escape handler on document for the modal loader shown last.
  static #esc: KeyHandler | null = null;

  private static setEsc(fn: KeyHandler | null): void {
    if (LoaderController.#esc) { document.removeEventListener("keyup", LoaderController.#esc); }
    LoaderController.#esc = fn;
    if (fn) { document.addEventListener("keyup", fn); }
  }

  override setup(): void {
    const el = this.element;
    if (el.getAttribute("data-modal") === "true" && !el.classList.contains("ah-loader-hidden")) {
      this.show();
    }
  }

  override teardown(): void {
    if (this.element.getAttribute("data-modal") === "true") {
      this.hide();
    }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  show(left?: number | string | null, top?: number | string | null): void {
    const el = this.element;
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
      LoaderController.setEsc((e) => {
        if (e.key === "Escape") { this.hide(); }
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

  hide(): void {
    const el = this.element;
    el.classList.add("ah-loader-hidden");
    el.setAttribute("aria-busy", "false");
    if (el.getAttribute("data-modal") === "true") {
      const m = document.getElementById(MODAL_ID);
      if (m) { m.classList.add("ah-loader-hidden"); }
      LoaderController.setEsc(null);
    }
  }

  toggle(): void {
    if (this.element.classList.contains("ah-loader-hidden")) { this.show(); } else { this.hide(); }
  }

  text(t: string): void {
    this.element.querySelectorAll(":scope > .ah-loader-text").forEach((n) => {
      n.textContent = t;
    });
    this.element.setAttribute("aria-label", t);
  }

  isOpen(): boolean { return !this.element.classList.contains("ah-loader-hidden"); }
}

AH.register("loader", LoaderController);
