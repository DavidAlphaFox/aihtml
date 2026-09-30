/* Behaviour of the toolbar component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.ts.
   A tool with data-key sets data-ah-value (and the hidden input) and
   fires "change" on the root; ah:open / ah:close (no detail) follow the
   overflow popup. */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import { byKey, setValue, visible } from "./_lib_nav.ts";

// ------------------------------------------------------------------
// toolbar (sigil toolbar.cljs, toolbar/overflow.cljs)
// ------------------------------------------------------------------
//
// Tools that do not fit are moved (not copied) into the overflow popup,
// so their handlers and data-ah-on keep working, and moved back when
// there is room again.

function outerWidth(n: HTMLElement): number {
  const cs = getComputedStyle(n);
  return n.offsetWidth + (parseFloat(cs.marginLeft) || 0) + (parseFloat(cs.marginRight) || 0);
}

function innerWidth(n: HTMLElement): number {
  const cs = getComputedStyle(n);
  return n.clientWidth - (parseFloat(cs.paddingLeft) || 0) - (parseFloat(cs.paddingRight) || 0);
}

/** One tool of the bar and its place in the overflow popup. */
class Tool {
  readonly el: HTMLElement;
  /** the separator before it in the bar, and its copy in the popup */
  readonly sep: HTMLElement | null;
  readonly menuSep: HTMLElement | null = null;
  /** where its children go while it is minimised */
  readonly popupTool: HTMLElement;
  readonly minimizable: boolean;
  readonly button: boolean;
  /** minimised: its children are in the popup */
  min = false;

  constructor(el: HTMLElement, popup: HTMLElement) {
    const prev = el.previousElementSibling;
    this.el = el;
    this.sep = prev instanceof HTMLElement && prev.classList.contains("ah-toolbar-separator") ? prev : null;
    this.minimizable = el.getAttribute("data-ah-minimizable") !== "false";
    this.button = !!el.querySelector(":scope > button.ah-toolbar-tool-el");
    if (this.sep) {
      this.menuSep = document.createElement("div");
      this.menuSep.className = "ah-toolbar-popup-separator";
      this.menuSep.setAttribute("role", "separator");
      popup.appendChild(this.menuSep);
    }
    this.popupTool = document.createElement("div");
    this.popupTool.className = "ah-toolbar-popup-tool";
    popup.appendChild(this.popupTool);
  }

  minimize(): void {
    if (this.min) { return; }
    this.min = true;
    this.el.style.display = "none";
    if (this.sep) { this.sep.style.display = "none"; }
    if (this.menuSep) { this.menuSep.classList.add("ah-toolbar-popup-separator-visible"); }
    this.popupTool.append(...Array.from(this.el.children));
    this.popupTool.classList.add("ah-toolbar-popup-tool-visible");
  }

  restore(): void {
    if (!this.min) { return; }
    this.min = false;
    this.el.append(...Array.from(this.popupTool.children));
    this.el.style.display = "";
    if (this.sep) { this.sep.style.display = ""; }
    if (this.menuSep) { this.menuSep.classList.remove("ah-toolbar-popup-separator-visible"); }
    this.popupTool.classList.remove("ah-toolbar-popup-tool-visible");
  }

  /** Its bar width (and its separator's). */
  get width(): number { return outerWidth(this.el) + (this.sep ? outerWidth(this.sep) : 0); }
}

/** The overflow popup and the tools, from setup to teardown. */
interface Overflow {
  tools: Tool[];
  popup: HTMLElement;
  open: boolean;
  float: FloatHandle | null;
  ro: ResizeObserver | null;
}

// The button-group classes of the shown tools: runs of adjacent buttons
// without a separator between them.
function markGroups(tools: Tool[]): void {
  const shown = tools.filter((t) => !t.min);
  shown.forEach((t, i) => {
    const prev = i > 0 && shown[i - 1].button && !t.sep;
    const next = i + 1 < shown.length && shown[i + 1].button && !shown[i + 1].sep;
    t.el.classList.remove("ah-toolbar-tool-first", "ah-toolbar-tool-inner", "ah-toolbar-tool-last");
    if (!t.button) { return; }
    if (prev && next) { t.el.classList.add("ah-toolbar-tool-inner"); }
    else if (next) { t.el.classList.add("ah-toolbar-tool-first"); }
    else if (prev) { t.el.classList.add("ah-toolbar-tool-last"); }
  });
}

class ToolbarController extends AH.Controller {
  #st: Overflow | null = null;

  override setup(): void {
    const el = this.element;
    const popup = document.createElement("div");
    popup.className = "ah-toolbar-popup";
    popup.setAttribute("role", "menu");
    popup.setAttribute("aria-label", AH.t("toolbar", "overflow", "Overflow tools"));
    const tools = Array.from(el.querySelectorAll<HTMLElement>(":scope > .ah-toolbar-tool")).map((t) =>
      new Tool(t, popup));
    const st: Overflow = this.#st = { tools: tools, popup: popup, open: false, float: null, ro: null };
    document.body.appendChild(popup);

    const onTool = (_e: MouseEvent, btn: HTMLButtonElement): void => {
      if (btn.disabled) { return; }
      this.#activate(btn);
      if (popup.contains(btn) && !btn.hasAttribute("data-ah-toggle")) { this.close(); }
    };
    this.delegate<"click", HTMLButtonElement>("click", "button.ah-toolbar-tool-el", onTool);
    this.delegate<"click", HTMLButtonElement>("click", "button.ah-toolbar-tool-el", onTool, popup);
    this.listen(popup, "keydown", (e) => {
      if (e.key === "Escape") {
        this.close();
        const btn = this.#minBtn();
        if (btn) { btn.focus(); }
      }
    });
    this.delegate("click", ".ah-toolbar-minimize-btn", (e) => {
      e.stopPropagation();
      if (st.open) { this.close(); } else { this.open(); }
    });
    this.delegate("keydown", ".ah-toolbar-minimize-btn", (e) => {
      if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        if (st.open) { this.close(); }
        else {
          this.open();
          const f = Array.from(popup.querySelectorAll<HTMLElement>(
            "button:not([disabled]), select, input, [tabindex]")).filter(visible)[0];
          if (f) { f.focus(); }
        }
      }
    });
    // arrows move between tools, Home / End jump to the ends
    this.listen(el, "keydown", (e) => {
      if (!/^(ArrowLeft|ArrowRight|Home|End)$/.test(e.key)) { return; }
      const t = e.target;
      if (!(t instanceof Element) || /^(INPUT|SELECT|TEXTAREA)$/.test(t.tagName)) { return; }
      const items = Array.from(el.querySelectorAll<HTMLElement>(
        ".ah-toolbar-tool button:not([disabled]), .ah-toolbar-tool select, " +
        ".ah-toolbar-tool input, .ah-toolbar-minimize-btn")).filter(visible);
      const i = items.findIndex((x) => x === t);
      if (i < 0) { return; }
      const n = items.length;
      const to = e.key === "Home" ? 0 : e.key === "End" ? n - 1
        : (i + (e.key === "ArrowRight" ? 1 : -1) + n) % n;
      e.preventDefault();
      items[to].focus();
    });
    this.listen(document, "mousedown", (e) => {
      const t = e.target;
      if (st.open && !(t instanceof Node && popup.contains(t)) &&
          !(t instanceof Element && t.closest(".ah-toolbar-minimize-btn"))) {
        this.close();
      }
    });
    if (window.ResizeObserver) {
      st.ro = new ResizeObserver(() => { this.layout(); });
      st.ro.observe(el);
    } else {
      this.listen(window, "resize", () => { this.layout(); });
    }
    requestAnimationFrame(() => { this.layout(); });
  }

  override teardown(): void {
    const st = this.#st;
    if (!st) { return; }
    if (st.ro) { st.ro.disconnect(); }
    if (st.float) { st.float.stop(); }
    st.tools.forEach((t) => { t.restore(); });
    st.popup.remove();
    this.#st = null;
  }

  // methods (aihtml_action:call/4, AH.invoke)
  layout(): void {
    const el = this.element;
    const st = this.#st;
    if (!st || !visible(el)) { return; }
    const btn = this.#minBtn();
    const avail = (): number =>
      // the button's margin-left is auto, so count its box only
      innerWidth(el) - (btn && btn.classList.contains("ah-toolbar-minimize-visible") ? btn.offsetWidth : 0);
    const used = (): number => st.tools.reduce((acc, t) => (t.min ? acc : acc + t.width), 0);
    const showBtn = (on: boolean): void => { if (btn) { btn.classList.toggle("ah-toolbar-minimize-visible", on); } };
    let cands: Tool[];
    // minimise from the right while the tools overflow
    while (used() > avail() && (cands = st.tools.filter((t) => t.minimizable && !t.min)).length) {
      showBtn(true);
      cands[cands.length - 1].minimize();
    }
    // restore from the left while they fit
    let hidden: Tool[];
    while ((hidden = st.tools.filter((t) => t.minimizable && t.min)).length) {
      const t = hidden[0];
      t.restore();
      if (hidden.length === 1) { showBtn(false); }
      if (used() > avail()) {
        showBtn(true);
        t.minimize();
        break;
      }
    }
    const overflow = st.tools.some((t) => t.min);
    showBtn(overflow);
    if (!overflow) { this.close(); }
    markGroups(st.tools);
  }

  open(): void {
    const st = this.#st;
    if (!st || st.open) { return; }
    const w = parseInt(this.element.getAttribute("data-ah-popup-width") || "", 10) || 200;
    st.popup.style.width = w + "px";
    st.popup.classList.add("ah-toolbar-popup-open");
    st.float = AH.float(st.popup, this.element, { placement: "bottom", align: "end", offset: 0 });
    st.open = true;
    const btn = this.#minBtn();
    if (btn) { btn.setAttribute("aria-expanded", "true"); }
    this.fire("ah:open");
  }

  close(): void {
    const st = this.#st;
    if (!st || !st.open) { return; }
    st.popup.classList.remove("ah-toolbar-popup-open");
    if (st.float) { st.float.stop(); st.float = null; }
    st.open = false;
    const btn = this.#minBtn();
    if (btn) { btn.setAttribute("aria-expanded", "false"); }
    this.fire("ah:close");
  }

  disableTool(key: string | number, disabled?: boolean): void {
    this.#buttons(key).forEach((b) => { b.disabled = disabled !== false; });
  }

  setPressed(key: string | number, pressed: boolean): void {
    this.#buttons(key).forEach((b) => {
      b.setAttribute("aria-pressed", String(!!pressed));
      b.classList.toggle("ah-btn-toggled", !!pressed);
    });
  }

  #minBtn(): HTMLElement | null {
    return this.element.querySelector<HTMLElement>(":scope > .ah-toolbar-minimize-btn");
  }

  #activate(btn: HTMLButtonElement): void {
    const key = btn.getAttribute("data-key");
    if (btn.hasAttribute("data-ah-toggle")) {
      const on = btn.getAttribute("aria-pressed") !== "true";
      btn.setAttribute("aria-pressed", String(on));
      btn.classList.toggle("ah-btn-toggled", on);
    }
    if (key) { setValue(this.element, key, "change"); }
  }

  #buttons(key: string | number): HTMLButtonElement[] {
    const scopes: Element[] = [this.element];
    if (this.#st) { scopes.push(this.#st.popup); }
    return byKey<HTMLButtonElement>(scopes, "button.ah-toolbar-tool-el", "data-key", key);
  }
}

AH.register("toolbar", ToolbarController);
