/* Behaviour of tabs (value: the active key; fires change). */
import AH from "../core.ts";
import { fade, hide, listKeys, setValue, stop } from "./_lib_layout.ts";
import { hover } from "./_lib_nav.ts";

class TabsController extends AH.Controller {
  override setup(): void {
    const el = this.element;
    const header = el.querySelector(":scope > .ah-tabs-header");
    if (!header) { return; }
    const choose = (item: HTMLElement): void => {
      if (!el.classList.contains("ah-tabs-disabled")) {
        this.choose(this.items().indexOf(item), true);
      }
    };
    if (el.getAttribute("data-selection-mode") === "hover") {
      hover<HTMLElement>(this, ".ah-tabs-item", choose, null, header);
    } else {
      this.delegate("click", ".ah-tabs-item", (_e, item) => { choose(item); }, header);
    }
    this.delegate("keydown", ".ah-tabs-item", (e, item) => {
      const items = this.items();
      const vertical = el.classList.contains("ah-tabs-left") || el.classList.contains("ah-tabs-right");
      const t = listKeys(e, items, items.indexOf(item), vertical, "ah-tabs-item-disabled");
      if (t === null) { return; }
      e.preventDefault();
      this.choose(t, true);
      items[t].focus();
    }, header);
    this.delegate("click", ".ah-tabs-scroll-btn", (_e, btn) => {
      const step = btn.classList.contains("ah-tabs-scroll-left") ? -80 : 80;
      header.scrollLeft = header.scrollLeft + step;
    }, header);
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  select(k: unknown): void { this.choose(this.indexOf(k), false); }
  disable(k: unknown): void {
    const it = this.items()[this.indexOf(k)];
    if (it) {
      it.classList.add("ah-tabs-item-disabled");
      it.setAttribute("aria-disabled", "true");
    }
  }
  enable(k: unknown): void {
    const it = this.items()[this.indexOf(k)];
    if (it) {
      it.classList.remove("ah-tabs-item-disabled");
      it.removeAttribute("aria-disabled");
    }
  }
  value(): string | null { return this.element.getAttribute("data-ah-value"); }

  private items(): HTMLElement[] {
    return Array.from(this.element.querySelectorAll<HTMLElement>(":scope > .ah-tabs-header > .ah-tabs-item"));
  }

  private panels(): HTMLElement[] {
    return Array.from(this.element.querySelectorAll<HTMLElement>(":scope > .ah-tabs-content > .ah-tabs-panel"));
  }

  private indexOf(k: unknown): number {
    return this.items().findIndex((it) => it.getAttribute("data-key") === String(k));
  }

  private choose(idx: number, user: boolean): boolean {
    const el = this.element;
    const items = this.items();
    const panels = this.panels();
    const cur = items.findIndex((it) => it.classList.contains("ah-tabs-item-selected"));
    if (idx < 0 || idx >= items.length || idx === cur ||
        items[idx].classList.contains("ah-tabs-item-disabled")) {
      return false;
    }
    items.forEach((it) => {
      it.classList.remove("ah-tabs-item-selected");
      it.setAttribute("aria-selected", "false");
      it.setAttribute("tabindex", "-1");
    });
    items[idx].classList.add("ah-tabs-item-selected");
    items[idx].setAttribute("aria-selected", "true");
    items[idx].setAttribute("tabindex", "0");
    panels.forEach((p) => { p.setAttribute("aria-hidden", "true"); });
    const next: HTMLElement | undefined = panels[idx];
    if (next) { next.setAttribute("aria-hidden", "false"); }
    const old = cur >= 0 ? (panels[cur] ? [panels[cur]] : [])
      : panels.filter((p) => p !== next);
    panels.forEach(stop);
    if ((el.getAttribute("data-animation") || "fade") === "fade" && old.length && next) {
      let left = old.length;
      old.forEach((o) => {
        fade(o, false, 100, () => {
          o.classList.remove("ah-tabs-panel-active");
          if (--left > 0) { return; }
          hide(next);
          fade(next, true, 100, () => { next.classList.add("ah-tabs-panel-active"); });
        });
      });
    } else {
      old.forEach((o) => {
        hide(o);
        o.classList.remove("ah-tabs-panel-active");
      });
      if (next) {
        next.style.display = "";
        next.classList.add("ah-tabs-panel-active");
      }
    }
    setValue(el, items[idx].getAttribute("data-key") || "");
    if (user) {
      this.fire("change");
    }
    return true;
  }
}

AH.register("tabs", TabsController);
