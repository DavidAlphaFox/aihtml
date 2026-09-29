/* Behaviour of the menu component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.js.
   Value contract: data-ah-value (the chosen leaf's data-id), the hidden
   input and "change" on the root. */
import AH from "../core.js";
import "./_lib_nav.js";

const N = AH.lib.nav;
const visible = N.visible;
const setValue = N.setValue;
const byKey = N.byKey;

function touchOnly() {
  return window.matchMedia && window.matchMedia("(hover: none)").matches;
}

function child(node, sel) { return node ? node.querySelector(":scope > " + sel) : null; }

// ------------------------------------------------------------------
// menu (sigil menu.cljs, menu/submenu, menu/keyboard, menu/responsive)
// ------------------------------------------------------------------

const M_OPEN = "ah-menu-submenu-open";
const M_FOCUS = "ah-menu-link-focus";
const TOP_LINKS = ".ah-menu-list > .ah-menu-item:not(.ah-menu-item-disabled) > .ah-menu-link";

// Submenus float (AH.float) so an ancestor with overflow: hidden cannot
// clip them: below a horizontal bar's top-level item, to the right of
// any other item. AH.float flips and clamps them to the viewport.
const floats = new WeakMap();

function subFloat(sub, on, opts) {
  const h = floats.get(sub);
  if (h) { h.stop(); floats.delete(sub); }
  if (on) { floats.set(sub, AH.float(sub, opts.anchor, opts)); }
}

function subClose(subs) {
  Array.from(subs).forEach(function (s) {
    subFloat(s, false);
    s.classList.remove(M_OPEN);
    s.style.left = s.style.right = s.style.top = s.style.bottom = "";
  });
}

function menuOpenSub(item, el) {
  const sub = child(item, ".ah-menu-submenu");
  if (!sub || sub.classList.contains(M_OPEN)) { return; }
  sub.classList.add(M_OPEN);
  const link = child(item, ".ah-menu-link");
  if (!el.classList.contains("ah-menu-is-minimized") && !item.closest(".ah-menu-drawer")) {
    const bar = item.parentNode.classList.contains("ah-menu-list") &&
      el.classList.contains("ah-menu-horizontal");
    const left = sub.classList.contains("ah-menu-open-left");
    subFloat(sub, true, {
      anchor: bar ? link : item,
      placement: bar ? "bottom" : (left ? "left" : "right"),
      align: sub.classList.contains("ah-menu-open-up") ? "end" : (bar && left ? "end" : "start"),
      offset: 0
    });
  }
  if (link) { link.setAttribute("aria-expanded", "true"); }
}

function menuCloseSub(item) {
  const sub = child(item, ".ah-menu-submenu");
  if (!sub) { return; }
  subClose(sub.querySelectorAll("." + M_OPEN));
  sub.querySelectorAll("[aria-expanded=true]").forEach(function (n) {
    n.setAttribute("aria-expanded", "false");
  });
  subClose([sub]);
  const link = child(item, ".ah-menu-link");
  if (link) { link.setAttribute("aria-expanded", "false"); }
}

function menuCloseSiblings(item) {
  Array.from(item.parentNode.children).forEach(function (s) {
    if (s !== item && s.classList.contains("ah-menu-has-submenu")) { menuCloseSub(s); }
  });
}

function menuCloseAll(el) {
  subClose(el.querySelectorAll("." + M_OPEN));
  el.querySelectorAll("[aria-expanded=true]:not(.ah-menu-minimized-btn)").forEach(function (n) {
    n.setAttribute("aria-expanded", "false");
  });
}

function clearFocus(el) {
  el.querySelectorAll("." + M_FOCUS).forEach(function (n) { n.classList.remove(M_FOCUS); });
}

function menuFocus(el, link) {
  clearFocus(el);
  link.classList.add(M_FOCUS);
  link.focus();
}

function siblingLinks(item) {
  return Array.from(item.parentNode.querySelectorAll(
    ":scope > .ah-menu-item:not(.ah-menu-item-disabled) > .ah-menu-link"));
}

function firstIn(item) {
  const sub = child(item, ".ah-menu-submenu");
  return sub ? sub.querySelector(".ah-menu-item:not(.ah-menu-item-disabled) > .ah-menu-link") : null;
}

function menuPopupClose(el) {
  if (el.classList.contains("ah-menu-popup")) {
    menuCloseAll(el);
    el.classList.remove("ah-menu-open");
  }
}

function menuPopupOpen(el, x, y) {
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

function markActive(el, link) {
  el.querySelectorAll(".ah-menu-link-active").forEach(function (n) {
    n.classList.remove("ah-menu-link-active");
    n.removeAttribute("aria-current");
  });
  if (link) {
    link.classList.add("ah-menu-link-active");
    link.setAttribute("aria-current", "true");
  }
}

// A leaf was chosen: follow its href, or make its key the value.
function menuSelect(el, link, e) {
  if (!link.getAttribute("href")) {
    if (e) { e.preventDefault(); }
    markActive(el, link);
    setValue(el, link.getAttribute("data-id"), "change");
  }
  menuCloseAll(el);
  clearFocus(el);
  menuPopupClose(el);
}

function menuKeydown(el, e) {
  let focused = el.querySelector("." + M_FOCUS);
  let item = focused ? focused.parentNode : null;
  const horizontal = el.classList.contains("ah-menu-horizontal");
  const topLevel = item && item.parentNode.classList.contains("ah-menu-list");
  const step = function (d) {
    if (!item) { return; }
    const links = siblingLinks(item);
    const i = links.indexOf(focused);
    if (links.length) { menuFocus(el, links[(i + d + links.length) % links.length]); }
  };
  const openFirst = function () {
    menuCloseSiblings(item);
    menuOpenSub(item, el);
    const f = firstIn(item);
    if (f) { menuFocus(el, f); }
  };
  if (!item && /^Arrow|^Home$|^End$/.test(e.key)) {
    e.preventDefault();
    const first = el.querySelector(TOP_LINKS);
    if (first) { menuFocus(el, first); }
    return;
  }
  if (!item && e.key !== "Escape" && e.key !== "Tab") { return; }
  switch (e.key) {
    case "ArrowDown":
      e.preventDefault();
      if (horizontal && topLevel && item.classList.contains("ah-menu-has-submenu")) { openFirst(); }
      else { step(1); }
      break;
    case "ArrowUp":
      e.preventDefault();
      step(-1);
      break;
    case "ArrowRight":
      e.preventDefault();
      if (horizontal && topLevel) { menuCloseAll(el); step(1); }
      else if (item.classList.contains("ah-menu-has-submenu")) { openFirst(); }
      else if (horizontal) {
        // leave a submenu to the next top-level item, as menubars do
        let top = null;
        for (let n = item.parentNode; n && n !== el; n = n.parentNode) {
          if (n.matches(".ah-menu-list > .ah-menu-item")) { top = n; }
        }
        menuCloseAll(el);
        if (top) {
          focused = child(top, ".ah-menu-link");
          item = top;
          step(1);
        }
      }
      break;
    case "ArrowLeft":
      e.preventDefault();
      if (horizontal && topLevel) { menuCloseAll(el); step(-1); }
      else {
        const parentItem = item.parentNode.closest(".ah-menu-item");
        if (parentItem && el.contains(parentItem)) {
          menuCloseSub(parentItem);
          menuFocus(el, child(parentItem, ".ah-menu-link"));
        }
      }
      break;
    case "Home":
    case "End":
      e.preventDefault();
      if (item) {
        const ls = siblingLinks(item);
        if (ls.length) { menuFocus(el, ls[e.key === "Home" ? 0 : ls.length - 1]); }
      }
      break;
    case "Enter":
    case " ":
      e.preventDefault();
      if (item.classList.contains("ah-menu-has-submenu")) { openFirst(); }
      else { focused.click(); }
      break;
    case "Escape": {
      e.preventDefault();
      const up = item ? item.parentNode.closest(".ah-menu-item") : null;
      const upSub = up && el.contains(up) ? child(up, ".ah-menu-submenu") : null;
      if (upSub && upSub.classList.contains(M_OPEN) && !el.classList.contains("ah-menu-popup")) {
        menuCloseSub(up);
        menuFocus(el, child(up, ".ah-menu-link"));
      } else {
        menuCloseAll(el);
        clearFocus(el);
        menuPopupClose(el);
        if (visible(el)) { el.focus(); }
      }
      break;
    }
    case "Tab":
      menuCloseAll(el);
      clearFocus(el);
      menuPopupClose(el);
      break;
    default:
      break;
  }
}

AH.register("menu", class extends AH.Controller {
  setup() {
    const el = this.element;
    const self = this;
    this.openT = null;
    this.closeT = null;
    this.drawer = null;
    this.backdrop = null;
    const clickToOpen = el.hasAttribute("data-ah-click-to-open");

    // hover opens submenus, with a short intent delay when a sibling is open
    N.hover(this, ".ah-menu-has-submenu", function (item) {
      if (clickToOpen || el.classList.contains("ah-menu-is-minimized")) { return; }
      clearTimeout(self.closeT);
      clearTimeout(self.openT);
      const open = function () {
        self.openT = null;
        menuCloseSiblings(item);
        menuOpenSub(item, el);
      };
      if (item.parentNode.querySelector(":scope > .ah-menu-item > ." + M_OPEN)) {
        self.openT = setTimeout(open, 60);
      } else {
        open();
      }
    }, function (item) {
      if (clickToOpen) { return; }
      clearTimeout(self.openT);
      self.closeT = setTimeout(function () {
        menuCloseSiblings(item);
        menuCloseSub(item);
      }, 200);
    });
    // click toggles a submenu (always in click-to-open or touch mode;
    // otherwise it opens one that hover has not opened, e.g. from a
    // screen reader)
    this.delegate("click", ".ah-menu-has-submenu > .ah-menu-link", function (e, link) {
      e.preventDefault();
      const item = link.parentNode;
      const open = child(item, ".ah-menu-submenu").classList.contains(M_OPEN);
      if (open && (clickToOpen || touchOnly())) {
        menuCloseSub(item);
      } else if (!open) {
        menuCloseSiblings(item);
        menuOpenSub(item, el);
      }
    });
    this.delegate("click", ".ah-menu-item:not(.ah-menu-has-submenu):not(.ah-menu-item-disabled) > .ah-menu-link",
      function (e, link) { menuSelect(el, link, e); });
    this.delegate("click", ".ah-menu-item-disabled > .ah-menu-link", function (e) { e.preventDefault(); });

    // outside click closes everything
    this.listen(document, "mousedown", function (e) {
      const t = e.target;
      if (!el.contains(t) && !(t.closest && t.closest(".ah-menu-drawer"))) {
        menuCloseAll(el);
        clearFocus(el);
        menuPopupClose(el);
      }
    });

    // context menu
    if (el.classList.contains("ah-menu-popup")) {
      const target = el.getAttribute("data-ah-popup-target");
      const onContext = function (e) {
        e.preventDefault();
        menuCloseAll(el);
        menuPopupOpen(el, e.clientX, e.clientY);
      };
      if (target) {
        document.querySelectorAll(target).forEach(function (t) { self.listen(t, "contextmenu", onContext); });
      } else {
        this.listen(document, "contextmenu", onContext);
      }
    }

    if (el.getAttribute("data-ah-keyboard") !== "false") {
      this.listen(el, "keydown", function (e) {
        if (e.target === el || e.target.classList.contains("ah-menu-link")) { menuKeydown(el, e); }
      });
      this.listen(el, "focus", function () {
        if (!el.querySelector("." + M_FOCUS) && !el.classList.contains("ah-menu-is-minimized")) {
          const f = el.querySelector(TOP_LINKS);
          if (f) { menuFocus(el, f); }
        }
      });
    }

    // responsive collapse to a hamburger + drawer
    this.delegate("click", ".ah-menu-minimized-btn", function () {
      if (!self.drawer) { return; }
      self.backdrop.classList.add("ah-menu-drawer-backdrop-visible");
      const drawer = self.drawer;
      requestAnimationFrame(function () {
        drawer.classList.add("ah-menu-drawer-open");
        const f = drawer.querySelector(".ah-menu-link");
        if (f) { f.setAttribute("tabindex", "0"); f.focus(); }
      });
    });
    const minW = parseInt(el.getAttribute("data-ah-minimize-width"), 10);
    if (minW) {
      const check = function () {
        if (window.innerWidth <= minW) { self.minimize(); } else { self.restore(); }
      };
      let t = null;
      this.listen(window, "resize", function () { clearTimeout(t); t = setTimeout(check, 150); });
      check();
    }
  }

  teardown() {
    clearTimeout(this.openT);
    clearTimeout(this.closeT);
    menuCloseAll(this.element);
    this.restore();
  }

  drawerClose() {
    if (!this.drawer) { return; }
    this.drawer.classList.remove("ah-menu-drawer-open");
    this.backdrop.classList.remove("ah-menu-drawer-backdrop-visible");
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  open(x, y) { menuPopupOpen(this.element, x, y); }
  close() { menuCloseAll(this.element); this.element.classList.remove("ah-menu-open"); }
  closeAll() { menuCloseAll(this.element); }
  openItem(key) {
    const l = byKey(this.element, ".ah-menu-link", "data-id", key)[0];
    if (l) { menuOpenSub(l.parentNode, this.element); }
  }
  closeItem(key) {
    const l = byKey(this.element, ".ah-menu-link", "data-id", key)[0];
    if (l) { menuCloseSub(l.parentNode); }
  }
  disableItem(key) {
    byKey(this.element, ".ah-menu-link", "data-id", key).forEach(function (l) {
      l.setAttribute("aria-disabled", "true");
      l.parentNode.classList.add("ah-menu-item-disabled");
    });
  }
  enableItem(key) {
    byKey(this.element, ".ah-menu-link", "data-id", key).forEach(function (l) {
      l.removeAttribute("aria-disabled");
      l.parentNode.classList.remove("ah-menu-item-disabled");
    });
  }
  setValue(key) {
    const el = this.element;
    markActive(el, null);
    byKey(el, ".ah-menu-link", "data-id", key).forEach(function (l) {
      l.classList.add("ah-menu-link-active");
      l.setAttribute("aria-current", "true");
    });
    setValue(el, key);
  }

  minimize() {
    const el = this.element;
    const self = this;
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
    drawer.querySelector(".ah-menu-drawer-title > span").textContent = title;
    const orig = child(el, ".ah-menu-list");
    if (orig) {
      const list = orig.cloneNode(true);
      list.removeAttribute("id");
      list.querySelectorAll("[id]").forEach(function (n) { n.removeAttribute("id"); });
      list.querySelectorAll("." + M_OPEN).forEach(function (n) { n.classList.remove(M_OPEN); });
      drawer.querySelector(".ah-menu-drawer-list").appendChild(list);
    }
    const backdrop = document.createElement("div");
    backdrop.className = "ah-menu-drawer-backdrop";
    document.body.append(backdrop, drawer);
    this.drawer = drawer;
    this.backdrop = backdrop;
    // the drawer and backdrop are removed on restore, their listeners too
    backdrop.addEventListener("click", function () { self.drawerClose(); });
    drawer.addEventListener("keydown", function (e) {
      if (e.key === "Escape") {
        self.drawerClose();
        const btn = child(el, ".ah-menu-minimized-btn");
        if (btn) { btn.focus(); }
      }
    });
    drawer.addEventListener("click", function (e) {
      const t = e.target;
      if (t.closest(".ah-menu-drawer-close")) { self.drawerClose(); return; }
      const parent = t.closest(".ah-menu-has-submenu > .ah-menu-link");
      if (parent && drawer.contains(parent)) {
        e.preventDefault();
        const sub = child(parent.parentNode, ".ah-menu-submenu");
        const on = sub.classList.toggle(M_OPEN);
        parent.setAttribute("aria-expanded", String(on));
        return;
      }
      const leaf = t.closest(".ah-menu-item:not(.ah-menu-has-submenu):not(.ah-menu-item-disabled) > .ah-menu-link");
      if (leaf && drawer.contains(leaf)) {
        const same = byKey(el, ".ah-menu-link", "data-id", leaf.getAttribute("data-id"))[0];
        if (!leaf.getAttribute("href")) { e.preventDefault(); }
        if (same) { menuSelect(el, same, null); }
        drawer.querySelectorAll(".ah-menu-link-active").forEach(function (n) {
          n.classList.remove("ah-menu-link-active");
        });
        leaf.classList.add("ah-menu-link-active");
        self.drawerClose();
      }
    });
  }

  restore() {
    const el = this.element;
    if (!el.classList.contains("ah-menu-is-minimized")) { return; }
    el.classList.remove("ah-menu-is-minimized");
    if (this.drawer) { this.drawer.remove(); this.backdrop.remove(); }
    this.drawer = this.backdrop = null;
  }
});
