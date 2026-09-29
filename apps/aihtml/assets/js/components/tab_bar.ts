/* Behaviour of tab-bar (value: the active id; fires change, and ah:close
 * with detail = the closed tab's id, TabBarClose). */
import AH from "../core.ts";
import { listKeys, setValue } from "./_lib_layout.ts";

/** Detail of ah:close: the closed tab's data-id. */
export type TabBarClose = string | null;

class TabBarController extends AH.Controller {
  override setup(): void {
    this.delegate("click", ".ah-tab-bar__close", (e, btn) => {
      e.stopPropagation();
      this.closeTab(btn.closest<HTMLElement>(".ah-tab-bar__tab"), true);
    });
    this.delegate("click", ".ah-tab-bar__tab", (e, tab) => {
      // the close button's stopPropagation does not stop this listener
      // (same root), so skip its clicks here
      const t = e.target as Element;
      if (t.closest(".ah-tab-bar__close")) { return; }
      this.selectTab(tab, true);
    });
    this.delegate("keydown", ".ah-tab-bar__tab", (e, tab) => {
      const items = this.tabs();
      const cur = items.indexOf(tab);
      if (e.key === "Delete" && tab.querySelector(":scope > .ah-tab-bar__close")) {
        e.preventDefault();
        this.closeTab(tab, true);
        return;
      }
      const t = listKeys(e, items, cur, false, "ah-tab-bar__tab--none");
      if (t === null) { return; }
      e.preventDefault();
      this.selectTab(items[t], true);
      items[t].focus();
    });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  select(id: unknown): void { this.selectTab(this.find(id), false); }
  close(id: unknown): void { this.closeTab(this.find(id), false); }
  value(): string | null { return this.element.getAttribute("data-ah-value"); }

  private tabs(): HTMLElement[] {
    return Array.from(this.element.querySelectorAll<HTMLElement>(":scope > .ah-tab-bar__tab"));
  }

  private find(id: unknown): HTMLElement | null {
    return this.tabs().find((t) => t.getAttribute("data-id") === String(id)) || null;
  }

  private selectTab(tab: HTMLElement | null, user: boolean): void {
    if (!tab || tab.getAttribute("data-active") === "true") {
      return;
    }
    this.tabs().forEach((t) => {
      t.setAttribute("data-active", "false");
      t.setAttribute("aria-selected", "false");
      t.setAttribute("tabindex", "-1");
    });
    tab.setAttribute("data-active", "true");
    tab.setAttribute("aria-selected", "true");
    tab.setAttribute("tabindex", "0");
    setValue(this.element, tab.getAttribute("data-id") || "");
    if (user) {
      this.fire("change");
    }
  }

  private closeTab(tab: HTMLElement | null, user: boolean): void {
    if (!tab) {
      return;
    }
    const id = tab.getAttribute("data-id");
    const idx = this.tabs().indexOf(tab);
    const wasActive = tab.getAttribute("data-active") === "true";
    const hadFocus = tab.contains(document.activeElement);
    tab.remove();
    this.fire<TabBarClose>("ah:close", id);
    if (wasActive) {
      const left = this.tabs();
      if (left.length) {
        const next = left[Math.min(idx, left.length - 1)];
        this.selectTab(next, user);
        if (hadFocus) { next.focus(); }
      } else {
        setValue(this.element, "");
        if (user) { this.fire("change"); }
      }
    }
  }
}

AH.register("tab-bar", TabBarController);
