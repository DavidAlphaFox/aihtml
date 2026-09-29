/* Behaviour of the menu component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.ts.
   Value contract: data-ah-value (the chosen leaf's data-id), the hidden
   input and "change" on the root. */
import AH from "../core.ts";
import type { FloatHandle, FloatOptions } from "../core.ts";
import { byKey, hover, setValue, visible } from "./_lib_nav.ts";

type Timer = ReturnType<typeof setTimeout>;

function touchOnly(): boolean {
  return !!(window.matchMedia && window.matchMedia("(hover: none)").matches);
}

function child(node: Element | null, sel: string): HTMLElement | null {
  return node ? node.querySelector<HTMLElement>(":scope > " + sel) : null;
}

/** The event target as a node (for contains). */
function asNode(t: EventTarget | null): Node | null { return t instanceof Node ? t : null; }

function closestTo(t: EventTarget | null, sel: string): HTMLElement | null {
  return t instanceof Element ? t.closest<HTMLElement>(sel) : null;
}

// ------------------------------------------------------------------
// menu (sigil menu.cljs, menu/submenu, menu/keyboard, menu/responsive)
// ------------------------------------------------------------------

const M_OPEN = "ah-menu-submenu-open";
const M_FOCUS = "ah-menu-link-focus";
const TOP_LINKS = ".ah-menu-list > .ah-menu-item:not(.ah-menu-item-disabled) > .ah-menu-link";

function siblingLinks(item: Element): HTMLElement[] {
  const p = item.parentElement;
  return p ? Array.from(p.querySelectorAll<HTMLElement>(
    ":scope > .ah-menu-item:not(.ah-menu-item-disabled) > .ah-menu-link")) : [];
}

function firstIn(item: Element): HTMLElement | null {
  const sub = child(item, ".ah-menu-submenu");
  return sub ? sub.querySelector<HTMLElement>(".ah-menu-item:not(.ah-menu-item-disabled) > .ah-menu-link") : null;
}

/** How a submenu floats: AH.float's options and its anchor. */
interface SubFloat extends FloatOptions { anchor: Element; }

class MenuController extends AH.Controller {
  #openT: Timer | undefined;
  #closeT: Timer | undefined;
  #drawer: HTMLElement | null = null;
  #backdrop: HTMLElement | null = null;
  // Submenus float (AH.float) so an ancestor with overflow: hidden cannot
  // clip them: below a horizontal bar's top-level item, to the right of
  // any other item. AH.float flips and clamps them to the viewport.
  readonly #floats = new WeakMap<Element, FloatHandle>();

  override setup(): void {
    const el = this.element;
    this.#openT = undefined;
    this.#closeT = undefined;
    this.#drawer = null;
    this.#backdrop = null;
    const clickToOpen = el.hasAttribute("data-ah-click-to-open");

    // hover opens submenus, with a short intent delay when a sibling is open
    hover(this, ".ah-menu-has-submenu", (item) => {
      if (clickToOpen || el.classList.contains("ah-menu-is-minimized")) { return; }
      clearTimeout(this.#closeT);
      clearTimeout(this.#openT);
      const open = (): void => {
        this.#openT = undefined;
        this.#closeSiblings(item);
        this.#openSub(item);
      };
      if (item.parentElement && item.parentElement.querySelector(":scope > .ah-menu-item > ." + M_OPEN)) {
        this.#openT = setTimeout(open, 60);
      } else {
        open();
      }
    }, (item) => {
      if (clickToOpen) { return; }
      clearTimeout(this.#openT);
      this.#closeT = setTimeout(() => {
        this.#closeSiblings(item);
        this.#closeSub(item);
      }, 200);
    });
    // click toggles a submenu (always in click-to-open or touch mode;
    // otherwise it opens one that hover has not opened, e.g. from a
    // screen reader)
    this.delegate("click", ".ah-menu-has-submenu > .ah-menu-link", (e, link) => {
      e.preventDefault();
      const item = link.parentElement;
      if (!item) { return; }
      const sub = child(item, ".ah-menu-submenu");
      const open = !!sub && sub.classList.contains(M_OPEN);
      if (open && (clickToOpen || touchOnly())) {
        this.#closeSub(item);
      } else if (!open) {
        this.#closeSiblings(item);
        this.#openSub(item);
      }
    });
    this.delegate("click", ".ah-menu-item:not(.ah-menu-has-submenu):not(.ah-menu-item-disabled) > .ah-menu-link",
      (e, link) => { this.#select(link, e); });
    this.delegate("click", ".ah-menu-item-disabled > .ah-menu-link", (e) => { e.preventDefault(); });

    // outside click closes everything
    this.listen(document, "mousedown", (e) => {
      if (!el.contains(asNode(e.target)) && !closestTo(e.target, ".ah-menu-drawer")) {
        this.#closeAll();
        this.#clearFocus();
        this.#popupClose();
      }
    });

    // context menu
    if (el.classList.contains("ah-menu-popup")) {
      const target = el.getAttribute("data-ah-popup-target");
      const onContext = (e: MouseEvent): void => {
        e.preventDefault();
        this.#closeAll();
        this.#popupOpen(e.clientX, e.clientY);
      };
      if (target) {
        document.querySelectorAll(target).forEach((t) => { this.listen(t, "contextmenu", onContext); });
      } else {
        this.listen(document, "contextmenu", onContext);
      }
    }

    if (el.getAttribute("data-ah-keyboard") !== "false") {
      this.listen(el, "keydown", (e) => {
        const t = e.target;
        if (t === el || (t instanceof Element && t.classList.contains("ah-menu-link"))) { this.#keydown(e); }
      });
      this.listen(el, "focus", () => {
        if (!el.querySelector("." + M_FOCUS) && !el.classList.contains("ah-menu-is-minimized")) {
          const f = el.querySelector<HTMLElement>(TOP_LINKS);
          if (f) { this.#focus(f); }
        }
      });
    }

    // responsive collapse to a hamburger + drawer
    this.delegate("click", ".ah-menu-minimized-btn", () => {
      const drawer = this.#drawer;
      if (!drawer) { return; }
      if (this.#backdrop) { this.#backdrop.classList.add("ah-menu-drawer-backdrop-visible"); }
      requestAnimationFrame(() => {
        drawer.classList.add("ah-menu-drawer-open");
        const f = drawer.querySelector<HTMLElement>(".ah-menu-link");
        if (f) { f.setAttribute("tabindex", "0"); f.focus(); }
      });
    });
    const minW = parseInt(el.getAttribute("data-ah-minimize-width") || "", 10);
    if (minW) {
      const check = (): void => {
        if (window.innerWidth <= minW) { this.minimize(); } else { this.restore(); }
      };
      let t: Timer | undefined;
      this.listen(window, "resize", () => { clearTimeout(t); t = setTimeout(check, 150); });
      check();
    }
  }

  override teardown(): void {
    clearTimeout(this.#openT);
    clearTimeout(this.#closeT);
    this.#closeAll();
    this.restore();
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  open(x: number, y: number): void { this.#popupOpen(x, y); }
  close(): void { this.#closeAll(); this.element.classList.remove("ah-menu-open"); }
  closeAll(): void { this.#closeAll(); }
  openItem(key: string | number): void {
    const l = byKey(this.element, ".ah-menu-link", "data-id", key)[0];
    if (l && l.parentElement) { this.#openSub(l.parentElement); }
  }
  closeItem(key: string | number): void {
    const l = byKey(this.element, ".ah-menu-link", "data-id", key)[0];
    if (l && l.parentElement) { this.#closeSub(l.parentElement); }
  }
  disableItem(key: string | number): void {
    byKey(this.element, ".ah-menu-link", "data-id", key).forEach((l) => {
      l.setAttribute("aria-disabled", "true");
      if (l.parentElement) { l.parentElement.classList.add("ah-menu-item-disabled"); }
    });
  }
  enableItem(key: string | number): void {
    byKey(this.element, ".ah-menu-link", "data-id", key).forEach((l) => {
      l.removeAttribute("aria-disabled");
      if (l.parentElement) { l.parentElement.classList.remove("ah-menu-item-disabled"); }
    });
  }
  setValue(key: string | number | null): void {
    const el = this.element;
    this.#markActive(null);
    byKey(el, ".ah-menu-link", "data-id", key).forEach((l) => {
      l.classList.add("ah-menu-link-active");
      l.setAttribute("aria-current", "true");
    });
    setValue(el, key);
  }

  minimize(): void {
    const el = this.element;
    if (el.classList.contains("ah-menu-is-minimized")) { return; }
    el.classList.add("ah-menu-is-minimized");
    const title = el.getAttribute("data-title") || "Menu";
    const drawer = document.createElement("div");
    drawer.className = "ah-menu-drawer";
    drawer.setAttribute("role", "dialog");
    drawer.setAttribute("aria-modal", "true");
    drawer.setAttribute("aria-label", title);
    drawer.innerHTML = '<div class="ah-menu-drawer-title"><span></span>' +
      '<button type="button" class="ah-menu-drawer-close" aria-label="Close">×</button></div>' +
      '<div class="ah-menu-drawer-list"></div>';
    const span = drawer.querySelector(".ah-menu-drawer-title > span");
    if (span) { span.textContent = title; }
    const orig = child(el, ".ah-menu-list");
    const holder = drawer.querySelector(".ah-menu-drawer-list");
    if (orig && holder) {
      const list = orig.cloneNode(true) as HTMLElement;     // a clone of an element is one
      list.removeAttribute("id");
      list.querySelectorAll("[id]").forEach((n) => { n.removeAttribute("id"); });
      list.querySelectorAll("." + M_OPEN).forEach((n) => { n.classList.remove(M_OPEN); });
      holder.appendChild(list);
    }
    const backdrop = document.createElement("div");
    backdrop.className = "ah-menu-drawer-backdrop";
    document.body.append(backdrop, drawer);
    this.#drawer = drawer;
    this.#backdrop = backdrop;
    // the drawer and backdrop are removed on restore, their listeners too
    backdrop.addEventListener("click", () => { this.#drawerClose(); });
    drawer.addEventListener("keydown", (e) => {
      if (e.key === "Escape") {
        this.#drawerClose();
        const btn = child(el, ".ah-menu-minimized-btn");
        if (btn) { btn.focus(); }
      }
    });
    drawer.addEventListener("click", (e) => {
      const t = e.target;
      if (closestTo(t, ".ah-menu-drawer-close")) { this.#drawerClose(); return; }
      const parent = closestTo(t, ".ah-menu-has-submenu > .ah-menu-link");
      if (parent && drawer.contains(parent)) {
        e.preventDefault();
        const sub = child(parent.parentElement, ".ah-menu-submenu");
        if (sub) {
          const on = sub.classList.toggle(M_OPEN);
          parent.setAttribute("aria-expanded", String(on));
        }
        return;
      }
      const leaf = closestTo(t, ".ah-menu-item:not(.ah-menu-has-submenu):not(.ah-menu-item-disabled) > .ah-menu-link");
      if (leaf && drawer.contains(leaf)) {
        const same = byKey(el, ".ah-menu-link", "data-id", leaf.getAttribute("data-id"))[0];
        if (!leaf.getAttribute("href")) { e.preventDefault(); }
        if (same) { this.#select(same, null); }
        drawer.querySelectorAll(".ah-menu-link-active").forEach((n) => {
          n.classList.remove("ah-menu-link-active");
        });
        leaf.classList.add("ah-menu-link-active");
        this.#drawerClose();
      }
    });
  }

  restore(): void {
    const el = this.element;
    if (!el.classList.contains("ah-menu-is-minimized")) { return; }
    el.classList.remove("ah-menu-is-minimized");
    if (this.#drawer) { this.#drawer.remove(); }
    if (this.#backdrop) { this.#backdrop.remove(); }
    this.#drawer = this.#backdrop = null;
  }

  #drawerClose(): void {
    if (!this.#drawer) { return; }
    this.#drawer.classList.remove("ah-menu-drawer-open");
    if (this.#backdrop) { this.#backdrop.classList.remove("ah-menu-drawer-backdrop-visible"); }
  }

  #subFloat(sub: Element, opts: SubFloat | null): void {
    const h = this.#floats.get(sub);
    if (h) { h.stop(); this.#floats.delete(sub); }
    if (opts) { this.#floats.set(sub, AH.float(sub, opts.anchor, opts)); }
  }

  #subClose(subs: Iterable<HTMLElement>): void {
    for (const s of Array.from(subs)) {
      this.#subFloat(s, null);
      s.classList.remove(M_OPEN);
      s.style.left = s.style.right = s.style.top = s.style.bottom = "";
    }
  }

  #openSub(item: Element): void {
    const el = this.element;
    const sub = child(item, ".ah-menu-submenu");
    if (!sub || sub.classList.contains(M_OPEN)) { return; }
    sub.classList.add(M_OPEN);
    const link = child(item, ".ah-menu-link");
    if (!el.classList.contains("ah-menu-is-minimized") && !item.closest(".ah-menu-drawer")) {
      const bar = !!item.parentElement && item.parentElement.classList.contains("ah-menu-list") &&
        el.classList.contains("ah-menu-horizontal");
      const left = sub.classList.contains("ah-menu-open-left");
      this.#subFloat(sub, {
        anchor: bar && link ? link : item,
        placement: bar ? "bottom" : (left ? "left" : "right"),
        align: sub.classList.contains("ah-menu-open-up") ? "end" : (bar && left ? "end" : "start"),
        offset: 0
      });
    }
    if (link) { link.setAttribute("aria-expanded", "true"); }
  }

  #closeSub(item: Element): void {
    const sub = child(item, ".ah-menu-submenu");
    if (!sub) { return; }
    this.#subClose(sub.querySelectorAll<HTMLElement>("." + M_OPEN));
    sub.querySelectorAll("[aria-expanded=true]").forEach((n) => {
      n.setAttribute("aria-expanded", "false");
    });
    this.#subClose([sub]);
    const link = child(item, ".ah-menu-link");
    if (link) { link.setAttribute("aria-expanded", "false"); }
  }

  #closeSiblings(item: Element): void {
    if (!item.parentElement) { return; }
    Array.from(item.parentElement.children).forEach((s) => {
      if (s !== item && s.classList.contains("ah-menu-has-submenu")) { this.#closeSub(s); }
    });
  }

  #closeAll(): void {
    const el = this.element;
    this.#subClose(el.querySelectorAll<HTMLElement>("." + M_OPEN));
    el.querySelectorAll("[aria-expanded=true]:not(.ah-menu-minimized-btn)").forEach((n) => {
      n.setAttribute("aria-expanded", "false");
    });
  }

  #clearFocus(): void {
    this.element.querySelectorAll("." + M_FOCUS).forEach((n) => { n.classList.remove(M_FOCUS); });
  }

  #focus(link: HTMLElement): void {
    this.#clearFocus();
    link.classList.add(M_FOCUS);
    link.focus();
  }

  #popupClose(): void {
    if (this.element.classList.contains("ah-menu-popup")) {
      this.#closeAll();
      this.element.classList.remove("ah-menu-open");
    }
  }

  #popupOpen(x: number, y: number): void {
    const el = this.element;
    el.classList.add("ah-menu-open");
    el.style.left = x + "px";
    el.style.top = y + "px";
    const w = el.offsetWidth;
    const h = el.offsetHeight;
    const ax = x + w > window.innerWidth ? Math.max(0, window.innerWidth - w) : x;
    const ay = y + h > window.innerHeight ? Math.max(0, window.innerHeight - h) : y;
    el.style.left = ax + "px";
    el.style.top = ay + "px";
    el.focus();
  }

  #markActive(link: HTMLElement | null): void {
    this.element.querySelectorAll(".ah-menu-link-active").forEach((n) => {
      n.classList.remove("ah-menu-link-active");
      n.removeAttribute("aria-current");
    });
    if (link) {
      link.classList.add("ah-menu-link-active");
      link.setAttribute("aria-current", "true");
    }
  }

  // A leaf was chosen: follow its href, or make its key the value.
  #select(link: HTMLElement, e: Event | null): void {
    if (!link.getAttribute("href")) {
      if (e) { e.preventDefault(); }
      this.#markActive(link);
      setValue(this.element, link.getAttribute("data-id"), "change");
    }
    this.#closeAll();
    this.#clearFocus();
    this.#popupClose();
  }

  #keydown(e: KeyboardEvent): void {
    const el = this.element;
    let focused = el.querySelector<HTMLElement>("." + M_FOCUS);
    let item: HTMLElement | null = focused ? focused.parentElement : null;
    const horizontal = el.classList.contains("ah-menu-horizontal");
    const topLevel = !!item && !!item.parentElement && item.parentElement.classList.contains("ah-menu-list");
    const step = (d: number): void => {
      if (!item) { return; }
      const links = siblingLinks(item);
      const i = links.findIndex((l) => l === focused);
      if (links.length) { this.#focus(links[(i + d + links.length) % links.length]); }
    };
    const openFirst = (it: HTMLElement): void => {
      this.#closeSiblings(it);
      this.#openSub(it);
      const f = firstIn(it);
      if (f) { this.#focus(f); }
    };
    if (!item && /^Arrow|^Home$|^End$/.test(e.key)) {
      e.preventDefault();
      const first = el.querySelector<HTMLElement>(TOP_LINKS);
      if (first) { this.#focus(first); }
      return;
    }
    if (e.key === "Escape") {
      e.preventDefault();
      const up = item && item.parentElement ? item.parentElement.closest(".ah-menu-item") : null;
      const upSub = up && el.contains(up) ? child(up, ".ah-menu-submenu") : null;
      if (up && upSub && upSub.classList.contains(M_OPEN) && !el.classList.contains("ah-menu-popup")) {
        this.#closeSub(up);
        const l = child(up, ".ah-menu-link");
        if (l) { this.#focus(l); }
      } else {
        this.#closeAll();
        this.#clearFocus();
        this.#popupClose();
        if (visible(el)) { el.focus(); }
      }
      return;
    }
    if (e.key === "Tab") {
      this.#closeAll();
      this.#clearFocus();
      this.#popupClose();
      return;
    }
    if (!item || !focused) { return; }
    const cur: HTMLElement = item;
    switch (e.key) {
      case "ArrowDown":
        e.preventDefault();
        if (horizontal && topLevel && cur.classList.contains("ah-menu-has-submenu")) { openFirst(cur); }
        else { step(1); }
        break;
      case "ArrowUp":
        e.preventDefault();
        step(-1);
        break;
      case "ArrowRight":
        e.preventDefault();
        if (horizontal && topLevel) { this.#closeAll(); step(1); }
        else if (cur.classList.contains("ah-menu-has-submenu")) { openFirst(cur); }
        else if (horizontal) {
          // leave a submenu to the next top-level item, as menubars do
          let top: HTMLElement | null = null;
          for (let n = cur.parentElement; n && n !== el; n = n.parentElement) {
            if (n.matches(".ah-menu-list > .ah-menu-item")) { top = n; }
          }
          this.#closeAll();
          if (top) {
            focused = child(top, ".ah-menu-link");
            item = top;
            step(1);
          }
        }
        break;
      case "ArrowLeft":
        e.preventDefault();
        if (horizontal && topLevel) { this.#closeAll(); step(-1); }
        else {
          const parentItem = cur.parentElement ? cur.parentElement.closest(".ah-menu-item") : null;
          if (parentItem && el.contains(parentItem)) {
            this.#closeSub(parentItem);
            const l = child(parentItem, ".ah-menu-link");
            if (l) { this.#focus(l); }
          }
        }
        break;
      case "Home":
      case "End": {
        e.preventDefault();
        const ls = siblingLinks(cur);
        if (ls.length) { this.#focus(ls[e.key === "Home" ? 0 : ls.length - 1]); }
        break;
      }
      case "Enter":
      case " ":
        e.preventDefault();
        if (cur.classList.contains("ah-menu-has-submenu")) { openFirst(cur); }
        else { focused.click(); }
        break;
      default:
        break;
    }
  }
}

AH.register("menu", MenuController);
