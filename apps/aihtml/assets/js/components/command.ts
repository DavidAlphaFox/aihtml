/* Behaviour of the command palette (designs/04-components.md), ported from
 * sigil's overlay/command. It filters the commands the server rendered by
 * hiding the ones that do not match; with a search action (data-ah-remote)
 * the server renders the results instead. It builds no HTML.
 * Events on the root: ah:select (detail CommandSelect: the chosen value),
 * ah:query (detail CommandQuery: the query text), ah:open / ah:close (no
 * detail). */
import AH from "../core.ts";
import { hover } from "./_lib_nav.ts";

/** Detail of ah:select: the chosen command's data-value. */
export type CommandSelect = string | null;
/** Detail of ah:query: the text in the search field. */
export type CommandQuery = string;

// ------------------------------------------------------------------
// Command: search field + filtered list + keyboard navigation
// ------------------------------------------------------------------

function text(root: Element, sel: string): string {
  return Array.from(root.querySelectorAll(sel)).map((n) => n.textContent).join("");
}

class CommandController extends AH.Controller {
  #returnTo: Element | null = null;

  override setup(): void {
    const el = this.element;
    this.#returnTo = null;
    this.delegate<Event, HTMLInputElement>("input", ".ah-command__input", (_e, input) => {
      this.#filter();
      this.fire<CommandQuery>("ah:query", input.value);
    });
    this.delegate("keydown", ".ah-command__input", (e) => {
      const act = this.#active();
      switch (e.key) {
        case "ArrowDown": e.preventDefault(); this.#setActive(act + 1, true); break;
        case "ArrowUp": e.preventDefault(); this.#setActive(act - 1, true); break;
        case "Home": if (e.ctrlKey) { e.preventDefault(); this.#setActive(0, true); } break;
        case "End": if (e.ctrlKey) { e.preventDefault(); this.#setActive(-1, true); } break;
        case "Enter":
          e.preventDefault();
          this.#choose(this.#visible()[act]);
          break;
        case "Escape":
          e.preventDefault();
          if (this.#overlay) { this.close(); } else { this.fire("ah:close"); }
          break;
      }
    });
    hover(this, ".ah-command__item", (item) => {
      this.#setActive(this.#visible().indexOf(item), false);
    }, null);
    this.delegate("click", ".ah-command__item", (_e, item) => {
      this.#choose(item);
    });
    const ov = this.#overlay;
    if (ov) {
      this.listen(ov, "mousedown", (e) => {
        if (e.target === e.currentTarget) { this.close(); }
      });
    }
    const key = (el.getAttribute("data-hotkey") || "").toLowerCase();
    if (key) {
      this.listen(document, "keydown", (e) => {
        if ((e.ctrlKey || e.metaKey) && String(e.key).toLowerCase() === key) {
          e.preventDefault();
          if (ov && !ov.hidden) { this.close(); } else { this.open(); }
        }
      });
    }
    this.#filter();
    if (el.hasAttribute("data-auto-focus") && !ov) {
      setTimeout(() => { this.#focusInput(); }, 0);
    }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open(): void {
    const el = this.element;
    const ov = this.#overlay;
    if (!ov || !ov.hidden) { return; }
    this.#returnTo = document.activeElement;
    if (!el.hasAttribute("data-ah-remote")) {
      const input = this.#input;
      if (input) { input.value = el.getAttribute("data-ah-query") || ""; }
      this.#filter();
    }
    ov.hidden = false;
    this.#focusInput();
    this.fire("ah:open");
  }
  close(): void {
    const ov = this.#overlay;
    if (!ov || ov.hidden) { return; }
    ov.hidden = true;
    const back = this.#returnTo;
    this.#returnTo = null;
    if ((back instanceof HTMLElement || back instanceof SVGElement) && document.contains(back)) { back.focus(); }
    this.fire("ah:close");
  }
  toggle(): void {
    const ov = this.#overlay;
    if (ov && !ov.hidden) { this.close(); } else { this.open(); }
  }
  setQuery(q: unknown): void {
    const input = this.#input;
    if (!input) { return; }
    input.value = q === null || q === undefined ? "" : String(q);
    input.dispatchEvent(new Event("input", { bubbles: true }));
  }
  focus(): void { this.#focusInput(); }
  itemsLoaded(): void { this.#setActive(0, true); }

  get #input(): HTMLInputElement | null {
    return this.element.querySelector<HTMLInputElement>(".ah-command__input");
  }

  #visible(): HTMLElement[] {
    return Array.from(this.element.querySelectorAll<HTMLElement>(".ah-command__item:not([hidden])"));
  }

  /** The overlay around the palette (the modal form), or null. */
  get #overlay(): HTMLElement | null {
    const p = this.element.parentElement;
    return p && p.classList.contains("ah-command-overlay") ? p : null;
  }

  #active(): number {
    const cur = this.element.querySelector(".ah-command__item[data-active=true]");
    return this.#visible().findIndex((it) => it === cur);
  }

  #setActive(index: number, scroll: boolean): void {
    const v = this.#visible();
    const input = this.#input;
    this.element.querySelectorAll(".ah-command__item[data-active=true]").forEach((it) => {
      it.setAttribute("data-active", "false");
      it.setAttribute("aria-selected", "false");
    });
    if (!v.length) {
      if (input) { input.removeAttribute("aria-activedescendant"); }
      return;
    }
    const i = ((index % v.length) + v.length) % v.length;
    const it = v[i];
    it.setAttribute("data-active", "true");
    it.setAttribute("aria-selected", "true");
    if (it.id && input) { input.setAttribute("aria-activedescendant", it.id); }
    if (scroll && it.scrollIntoView) { it.scrollIntoView({ block: "nearest" }); }
  }

  // sigil's match?: a case-insensitive substring of value, label or
  // description.
  #filter(): void {
    const el = this.element;
    if (el.hasAttribute("data-ah-remote")) { return; }
    const input = this.#input;
    const q = String((input && input.value) || "").trim().toLowerCase();
    el.querySelectorAll<HTMLElement>(".ah-command__item").forEach((it) => {
      const hay = [it.getAttribute("data-value") || "",
                   text(it, ".ah-command__item-label"),
                   text(it, ".ah-command__item-desc")].join("\n").toLowerCase();
      it.hidden = q !== "" && hay.indexOf(q) < 0;
    });
    let found = false;
    el.querySelectorAll<HTMLElement>(".ah-command__group").forEach((g) => {
      const shown = !!g.querySelector(":scope > .ah-command__item:not([hidden])");
      g.hidden = !shown;
      found = found || shown;
    });
    el.querySelectorAll<HTMLElement>(".ah-command__empty").forEach((n) => { n.hidden = found; });
    this.#setActive(0, true);
  }

  #focusInput(): void {
    const inp = this.#input;
    if (inp) {
      inp.focus();
      const n = inp.value.length;
      try { inp.setSelectionRange(n, n); } catch { /* not a text field */ }
    }
  }

  #choose(it: HTMLElement | undefined): void {
    const el = this.element;
    if (!it || it.getAttribute("data-disabled") === "true") { return; }
    const v = it.getAttribute("data-value");
    el.setAttribute("data-ah-value", v === null ? "null" : v);
    this.fire<CommandSelect>("ah:select", v);
    if (this.#overlay && el.getAttribute("data-close-on-select") !== "false") {
      this.close();
    }
    const href = it.getAttribute("data-href");
    if (href) { window.location.href = href; }
  }
}

AH.register("command", CommandController);
