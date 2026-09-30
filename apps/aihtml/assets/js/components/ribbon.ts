/* The ribbon behaviour (designs/04-components.md), ported from sigil's
 * layout/ribbon.
 *
 * ribbon keeps the active tab's key in data-ah-value on the root, mirrors
 * it into a hidden input and fires "change" when the user switches tabs.
 * Every [data-command] inside the panels fires "ah:command" on the root
 * (detail RibbonCommand {command, pressed}) with the command copied into
 * data-command (and data-pressed for toggles), so Event.data.command
 * reaches an action. Collapsed and popup ribbons show a panel only while
 * it is open (class ah-ribbon-open); ah:collapse / ah:expand when the user
 * collapses or expands it. Methods called by the server (AH.invoke /
 * aihtml_action:call) fire no events. The ribbon builds no HTML.
 */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";

/** Detail of ah:command: pressed is the new state of a toggle, else null. */
export interface RibbonCommand { command: string | null; pressed: boolean | null; }

/** The open dropdown menu. */
interface OpenMenu { toggle: HTMLElement; menu: HTMLElement; float: FloatHandle; }

/** Elements with a disabled property (buttons, inputs, ...). */
type Disableable = HTMLElement & { disabled: boolean };

function isDisabled(n: Element): boolean {
  return "disabled" in n && (n as Disableable).disabled === true;
}

function tabDisabled(tab: Element): boolean {
  return tab.classList.contains("ah-ribbon-tab-disabled") || isDisabled(tab);
}

class RibbonController extends AH.Controller {
  #menu: OpenMenu | null = null;
  #ro: ResizeObserver | null = null;

  override setup(): void {
    const el = this.element;
    this.#menu = null;
    this.#ro = null;
    const own = (node: Element): boolean => node.closest(".ah-ribbon") === el;
    this.delegate("click", ".ah-ribbon-tab", (_e, tab) => {
      if (own(tab)) { this.choose(tab, true); }
    });
    // mouseenter of a tab
    this.delegate("mouseover", ".ah-ribbon-tab", (e, tab) => {
      if (e.relatedTarget instanceof Node && tab.contains(e.relatedTarget)) { return; }
      if (el.getAttribute("data-selection-mode") === "hover" && own(tab)) {
        this.choose(tab, false);
      }
    });
    this.delegate("dblclick", ".ah-ribbon-tab", (_e, tab) => {
      if (this.collapsible() && own(tab)) {
        this.setCollapsed(!el.classList.contains("ah-ribbon-mode-collapsed"), true);
      }
    });
    this.delegate<"keydown", HTMLButtonElement>("keydown", ".ah-ribbon-tab", (e, tab) => {
      if (own(tab)) { this.tabKey(tab, e); }
    });
    this.delegate("click", ".ah-ribbon-scroll-btn", (e, btn) => {
      e.preventDefault();
      const box = this.inner();
      if (!box) { return; }
      const dir = btn.getAttribute("data-scroll-direction");
      const amount = this.vertical() ? 50 : 100;
      const sign = (dir === "left" || dir === "up") ? -1 : 1;
      if (this.vertical()) { box.scrollTop += sign * amount; } else { box.scrollLeft += sign * amount; }
      this.updateScroll();
    });
    this.delegate("click", ".ah-ribbon-collapse-btn", () => {
      this.setCollapsed(!el.classList.contains("ah-ribbon-mode-collapsed"), true);
    });
    this.delegate("click", ".ah-ribbon-dropdown-toggle", (_e, toggle) => {
      if (this.#menu && this.#menu.toggle === toggle) { this.closeMenu(false); } else { this.openMenu(toggle, false); }
    });
    this.delegate("keydown", ".ah-ribbon-dropdown-toggle", (e, toggle) => {
      if (e.key === "ArrowDown" || e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        this.openMenu(toggle, true);
      }
    });
    this.delegate("keydown", ".ah-ribbon-menu .ah-dropdown-btn-item", (e, item) => {
      this.menuKey(item, e);
    });
    this.delegate("click", "[data-command]", (e, node) => {
      // the root carries the last command's data-command too (for the
      // action payload): it is not a command
      if (node === el || !own(node)) { return; }
      e.preventDefault();
      this.runCommand(node);
    });
    this.listen(el, "keydown", (e) => {
      if (e.cancelBubble) { return; }   // a menu handled its Escape
      if (e.key === "F1" && (e.ctrlKey || e.metaKey) && this.collapsible()) {
        e.preventDefault();
        this.setCollapsed(!el.classList.contains("ah-ribbon-mode-collapsed"), true);
      } else if (e.key === "Escape" && this.isOpen()) {
        this.close();
        const t = this.selectedTab();
        if (t) { t.focus(); }
      }
    });
    const box = this.inner();
    // scroll does not bubble
    if (box) { this.listen(box, "scroll", () => { this.updateScroll(); }); }
    this.listen(document, "pointerdown", (e) => {
      const m = this.#menu, t = e.target as Node | null;
      if (m && !m.menu.contains(t) && !m.toggle.contains(t)) {
        this.closeMenu(false);
      }
      if (this.isOpen() && !(t !== el && el.contains(t))) { this.close(); }
    });
    if (typeof ResizeObserver !== "undefined") {
      if (box) {
        this.#ro = new ResizeObserver(() => { this.updateScroll(); this.updateToken(); });
        this.#ro.observe(box);
      }
    } else {
      this.listen(window, "resize", () => { this.updateScroll(); this.updateToken(); });
    }
    this.updateScroll();
    this.updateToken();
  }

  override teardown(): void {
    this.closeMenu(false);
    if (this.#ro) { this.#ro.disconnect(); this.#ro = null; }
  }

  // ---- methods (aihtml_action:call/4, AH.invoke) ----

  /** Make the tab `key' active. Returns whether the value changed. */
  select(key: string): boolean {
    const el = this.element;
    const tab = this.tabByKey(key);
    if (!tab) { return false; }
    const k = tab.getAttribute("data-key") || "";
    const changed = el.getAttribute("data-ah-value") !== k;
    this.tabs().forEach((t) => {
      const on = t === tab;
      t.classList.toggle("ah-ribbon-tab-selected", on);
      t.setAttribute("aria-selected", String(on));
      t.setAttribute("tabindex", on ? "0" : "-1");
    });
    this.panels().forEach((p) => {
      p.classList.toggle("ah-ribbon-tab-content-active", p.getAttribute("data-key") === k);
    });
    el.setAttribute("data-ah-value", k);
    const hidden = el.querySelector<HTMLInputElement>(":scope > input[type=hidden]");
    if (hidden) { hidden.value = k; }
    this.updateToken();
    this.scrollTabIntoView(tab);
    return changed;
  }

  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }

  enableTab(key: string): void {
    const t = this.tabByKey(key);
    if (!t) { return; }
    t.classList.remove("ah-ribbon-tab-disabled");
    t.disabled = this.element.classList.contains("ah-ribbon-disabled");
    t.removeAttribute("aria-disabled");
  }

  disableTab(key: string): void {
    const t = this.tabByKey(key);
    if (!t) { return; }
    t.classList.add("ah-ribbon-tab-disabled");
    t.disabled = true;
    t.setAttribute("aria-disabled", "true");
  }

  enableCommand(cmd: string): void { this.commandNodes(cmd).forEach((n) => { n.disabled = false; }); }
  disableCommand(cmd: string): void { this.commandNodes(cmd).forEach((n) => { n.disabled = true; }); }

  setPressed(cmd: string | null, on: unknown): void {
    this.commandNodes(cmd).forEach((n) => {
      if (!n.hasAttribute("data-toggle")) { return; }
      n.setAttribute("aria-pressed", String(!!on));
      n.classList.toggle("ah-ribbon-button-pressed", !!on);
    });
  }

  collapse(): void { this.setCollapsed(true, false); }
  expand(): void { this.setCollapsed(false, false); }

  close(): void {
    this.closeMenu(false);
    this.element.classList.remove("ah-ribbon-open");
  }

  // ---- structure ----

  // the tabs are buttons (aihtml_ribbon)
  private tabs(): HTMLButtonElement[] {
    return Array.from(this.element.querySelectorAll<HTMLButtonElement>(":scope > .ah-ribbon-tabs .ah-ribbon-tab"));
  }

  private panels(): HTMLElement[] {
    return Array.from(this.element.querySelectorAll<HTMLElement>(
      ":scope > .ah-ribbon-tabs-content > .ah-ribbon-tab-content"));
  }

  private inner(): HTMLElement | null {
    return this.element.querySelector<HTMLElement>(":scope > .ah-ribbon-tabs > .ah-ribbon-tabs-inner");
  }

  private selectedTab(): HTMLElement | undefined {
    return this.tabs().filter((t) => t.classList.contains("ah-ribbon-tab-selected"))[0];
  }

  private vertical(): boolean { return /\bah-ribbon-position-(left|right)\b/.test(this.element.className); }
  private floating(): boolean { return /\bah-ribbon-mode-(collapsed|popup)\b/.test(this.element.className); }
  private collapsible(): boolean { return this.element.classList.contains("ah-ribbon-collapsible"); }
  private isOpen(): boolean { return this.element.classList.contains("ah-ribbon-open"); }

  private tabByKey(key: unknown): HTMLButtonElement | undefined {
    return this.tabs().filter((t) => t.getAttribute("data-key") === String(key))[0];
  }

  private enabledTabs(): HTMLButtonElement[] { return this.tabs().filter((t) => !tabDisabled(t)); }

  // ---- tab strip: scroll buttons and the selection token ----

  private updateScroll(): void {
    const box = this.inner();
    const bar = this.element.querySelector(":scope > .ah-ribbon-tabs");
    if (!box || !bar) { return; }
    const v = this.vertical();
    const pos = v ? box.scrollTop : box.scrollLeft;
    const max = v ? box.scrollHeight - box.clientHeight : box.scrollWidth - box.clientWidth;
    bar.querySelectorAll(":scope > .ah-ribbon-scroll-left, :scope > .ah-ribbon-scroll-up").forEach((b) => {
      b.classList.toggle("ah-ribbon-scroll-visible", pos > 0);
    });
    bar.querySelectorAll(":scope > .ah-ribbon-scroll-right, :scope > .ah-ribbon-scroll-down").forEach((b) => {
      b.classList.toggle("ah-ribbon-scroll-visible", pos < max - 1);
    });
  }

  private scrollTabIntoView(tab: HTMLElement | undefined): void {
    const box = this.inner();
    if (!box || !tab) { return; }
    if (this.vertical()) {
      if (tab.offsetTop < box.scrollTop) {
        box.scrollTop = tab.offsetTop;
      } else if (tab.offsetTop + tab.offsetHeight > box.scrollTop + box.clientHeight) {
        box.scrollTop = tab.offsetTop + tab.offsetHeight - box.clientHeight;
      }
    } else if (tab.offsetLeft < box.scrollLeft) {
      box.scrollLeft = tab.offsetLeft;
    } else if (tab.offsetLeft + tab.offsetWidth > box.scrollLeft + box.clientWidth) {
      box.scrollLeft = tab.offsetLeft + tab.offsetWidth - box.clientWidth;
    }
    this.updateScroll();
  }

  private updateToken(): void {
    const box = this.inner();
    const tab = this.selectedTab();
    const token = box && box.querySelector<HTMLElement>(":scope > .ah-ribbon-selection-token");
    if (!token) { return; }
    if (!tab) {
      token.style.width = token.style.height = "0px";
      return;
    }
    token.style.left = tab.offsetLeft + "px";
    token.style.top = tab.offsetTop + "px";
    token.style.width = tab.offsetWidth + "px";
    token.style.height = tab.offsetHeight + "px";
  }

  // ---- selecting, opening, collapsing ----

  private open(): void {
    if (!this.floating() || this.isOpen()) { return; }
    this.element.classList.add("ah-ribbon-open");
  }

  private setCollapsed(on: boolean, notify: boolean): void {
    const el = this.element;
    if (/\bah-ribbon-mode-popup\b/.test(el.className)) { return; }
    const was = el.classList.contains("ah-ribbon-mode-collapsed");
    if (was === on) { return; }
    this.close();
    el.classList.toggle("ah-ribbon-mode-collapsed", on);
    el.classList.toggle("ah-ribbon-mode-default", !on);
    el.querySelectorAll(":scope > .ah-ribbon-tabs > .ah-ribbon-collapse-btn").forEach((b) => {
      b.setAttribute("aria-expanded", String(!on));
      b.setAttribute("aria-label", on ? AH.t("ribbon", "expand", "Expand the ribbon")
                                     : AH.t("ribbon", "collapse", "Collapse the ribbon"));
    });
    if (notify) { this.fire(on ? "ah:collapse" : "ah:expand"); }
  }

  // A user choosing a tab: select it, open a floating panel, fire change.
  private choose(tab: HTMLElement | undefined, toggle: boolean): void {
    const el = this.element;
    if (!tab || tabDisabled(tab) || el.classList.contains("ah-ribbon-disabled")) { return; }
    const already = tab.classList.contains("ah-ribbon-tab-selected");
    const changed = this.select(tab.getAttribute("data-key") || "");
    if (this.floating()) {
      if (toggle && already && this.isOpen()) { this.close(); } else { this.open(); }
    }
    if (changed) { this.fire("change"); }
  }

  // ---- commands and dropdown menus ----

  private static menuItems(menu: Element): HTMLButtonElement[] {
    return Array.from(menu.querySelectorAll<HTMLButtonElement>(":scope > .ah-dropdown-btn-item"))
      .filter((b) => !b.disabled);
  }

  private openMenu(toggle: HTMLElement, focusFirst: boolean): void {
    this.closeMenu(false);
    const parent = toggle.parentElement;
    const menu = parent ? Array.from(parent.children).filter((c): c is HTMLElement =>
      c !== toggle && c.classList.contains("ah-ribbon-menu"))[0] : undefined;
    if (!menu) { return; }
    menu.hidden = false;
    toggle.setAttribute("aria-expanded", "true");
    this.#menu = { toggle: toggle, menu: menu, float: AH.float(menu, toggle, { placement: "bottom" }) };
    if (focusFirst) {
      const first = RibbonController.menuItems(menu)[0];
      if (first) { first.focus(); }
    }
  }

  private closeMenu(refocus: boolean): void {
    const m = this.#menu;
    if (!m) { return; }
    this.#menu = null;
    m.float.stop();
    m.menu.hidden = true;
    m.toggle.setAttribute("aria-expanded", "false");
    if (refocus) { m.toggle.focus(); }
  }

  private runCommand(node: HTMLElement): void {
    const el = this.element;
    if (isDisabled(node) || el.classList.contains("ah-ribbon-disabled")) { return; }
    const cmd = node.getAttribute("data-command");
    let pressed: boolean | null = null;
    if (node.hasAttribute("data-toggle")) {
      pressed = node.getAttribute("aria-pressed") !== "true";
      this.setPressed(cmd, pressed);
    }
    const inMenu = !!node.closest(".ah-ribbon-menu");
    this.closeMenu(inMenu);
    if (this.floating()) {
      this.close();
      const tab = this.selectedTab();
      if (tab && inMenu) { tab.focus(); }
    }
    el.setAttribute("data-command", cmd || "");
    if (pressed === null) {
      el.removeAttribute("data-pressed");
    } else {
      el.setAttribute("data-pressed", String(pressed));
    }
    this.fire<RibbonCommand>("ah:command", { command: cmd, pressed: pressed });
  }

  // The command elements (buttons, inputs: whatever carries data-command).
  private commandNodes(cmd: string | null): Disableable[] {
    const el = this.element;
    return Array.from(el.querySelectorAll<HTMLElement>("[data-command]")).filter((n) =>
      n.getAttribute("data-command") === String(cmd) && n.closest(".ah-ribbon") === el) as Disableable[];
  }

  // ---- keyboard ----

  private panelFocusables(): HTMLElement[] {
    const active = this.panels().filter((p) => p.classList.contains("ah-ribbon-tab-content-active"));
    const out: HTMLElement[] = [];
    active.forEach((p) => {
      p.querySelectorAll<HTMLElement>("button, [href], input, select, textarea, [tabindex]").forEach((n) => {
        if (!isDisabled(n) && n.getAttribute("tabindex") !== "-1" && !n.closest("[hidden]")) {
          out.push(n);
        }
      });
    });
    return out;
  }

  private tabKey(tab: HTMLButtonElement, e: KeyboardEvent): void {
    const el = this.element;
    const v = this.vertical();
    const en = this.enabledTabs();
    const i = en.indexOf(tab);
    let next = 0;
    const prevKey = v ? "ArrowUp" : "ArrowLeft";
    const nextKey = v ? "ArrowDown" : "ArrowRight";
    const into: Record<string, string> = { top: "ArrowDown", bottom: "ArrowUp", left: "ArrowRight", right: "ArrowLeft" };
    const intoKey = into[(/\bah-ribbon-position-(\w+)\b/.exec(el.className) || [0, "top"])[1]];
    switch (e.key) {
      case prevKey: next = (i - 1 + en.length) % en.length; break;
      case nextKey: next = (i + 1) % en.length; break;
      case "Home": next = 0; break;
      case "End": next = en.length - 1; break;
      case intoKey: {
        // into the panel: its first command
        e.preventDefault();
        this.choose(tab, false);
        this.open();
        const f = this.panelFocusables()[0];
        if (f) { f.focus(); }
        return;
      }
      case "Escape":
        if (this.isOpen()) { e.preventDefault(); this.close(); }
        return;
      default: return;
    }
    e.preventDefault();
    const t = en[next];
    if (t) {
      t.focus();
      this.choose(t, false);
    }
  }

  private menuKey(item: HTMLElement, e: KeyboardEvent): void {
    const menu = item.closest(".ah-ribbon-menu");
    const items = menu ? RibbonController.menuItems(menu) : [];
    const i = items.indexOf(item as HTMLButtonElement);
    let next = 0;
    switch (e.key) {
      case "ArrowDown": next = (i + 1) % items.length; break;
      case "ArrowUp": next = (i - 1 + items.length) % items.length; break;
      case "Home": next = 0; break;
      case "End": next = items.length - 1; break;
      case "Escape": e.preventDefault(); e.stopPropagation(); this.closeMenu(true); return;
      case "Tab": this.closeMenu(false); return;
      default: return;
    }
    e.preventDefault();
    if (items[next]) { items[next].focus(); }
  }
}

AH.register("ribbon", RibbonController);
