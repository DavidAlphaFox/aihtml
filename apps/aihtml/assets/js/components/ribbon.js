/* The ribbon behaviour (designs/04-components.md), ported from sigil's
 * layout/ribbon.
 *
 * ribbon keeps the active tab's key in data-ah-value on the root, mirrors
 * it into a hidden input and fires "change" when the user switches tabs.
 * Every [data-command] inside the panels fires "ah:command" on the root
 * (detail {command, pressed}) with the command copied into data-command
 * (and data-pressed for toggles), so Event.data.command reaches an
 * action. Collapsed and popup ribbons show a panel only while it is open
 * (class ah-ribbon-open); ah:collapse / ah:expand when the user collapses
 * or expands it. Methods called by the server (AH.invoke /
 * aihtml_action:call) fire no events. The ribbon builds no HTML.
 */
import AH from "../core.js";

function tabDisabled(tab) {
  return tab.classList.contains("ah-ribbon-tab-disabled") || tab.disabled;
}

AH.register("ribbon", class extends AH.Controller {
  setup() {
    var el = this.element;
    this.menu = null;       // the open dropdown menu: {toggle, menu, float}
    this.ro = null;
    var own = (node) => node.closest(".ah-ribbon") === el;
    this.delegate("click", ".ah-ribbon-tab", (e, tab) => {
      if (own(tab)) { this.choose(tab, true); }
    });
    // mouseenter of a tab
    this.delegate("mouseover", ".ah-ribbon-tab", (e, tab) => {
      if (e.relatedTarget && tab.contains(e.relatedTarget)) { return; }
      if (el.getAttribute("data-selection-mode") === "hover" && own(tab)) {
        this.choose(tab, false);
      }
    });
    this.delegate("dblclick", ".ah-ribbon-tab", (e, tab) => {
      if (this.collapsible() && own(tab)) {
        this.setCollapsed(!el.classList.contains("ah-ribbon-mode-collapsed"), true);
      }
    });
    this.delegate("keydown", ".ah-ribbon-tab", (e, tab) => {
      if (own(tab)) { this.tabKey(tab, e); }
    });
    this.delegate("click", ".ah-ribbon-scroll-btn", (e, btn) => {
      e.preventDefault();
      var box = this.inner();
      var dir = btn.getAttribute("data-scroll-direction");
      var amount = this.vertical() ? 50 : 100;
      var sign = (dir === "left" || dir === "up") ? -1 : 1;
      if (this.vertical()) { box.scrollTop += sign * amount; } else { box.scrollLeft += sign * amount; }
      this.updateScroll();
    });
    this.delegate("click", ".ah-ribbon-collapse-btn", () => {
      this.setCollapsed(!el.classList.contains("ah-ribbon-mode-collapsed"), true);
    });
    this.delegate("click", ".ah-ribbon-dropdown-toggle", (e, toggle) => {
      if (this.menu && this.menu.toggle === toggle) { this.closeMenu(false); } else { this.openMenu(toggle, false); }
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
      if (!own(node)) { return; }
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
        var t = this.selectedTab();
        if (t) { t.focus(); }
      }
    });
    var box = this.inner();
    // scroll does not bubble
    if (box) { this.listen(box, "scroll", () => { this.updateScroll(); }); }
    this.listen(document, "pointerdown", (e) => {
      var m = this.menu;
      if (m && !m.menu.contains(e.target) && !m.toggle.contains(e.target)) {
        this.closeMenu(false);
      }
      if (this.isOpen() && !(e.target !== el && el.contains(e.target))) { this.close(); }
    });
    if (typeof ResizeObserver !== "undefined") {
      if (box) {
        this.ro = new ResizeObserver(() => { this.updateScroll(); this.updateToken(); });
        this.ro.observe(box);
      }
    } else {
      this.listen(window, "resize", () => { this.updateScroll(); this.updateToken(); });
    }
    this.updateScroll();
    this.updateToken();
  }

  teardown() {
    this.closeMenu(false);
    if (this.ro) { this.ro.disconnect(); this.ro = null; }
  }

  // ---- methods (aihtml_action:call/4, AH.invoke) ----

  // Make the tab `key' active. Returns whether the value changed.
  select(key) {
    var el = this.element;
    var tab = this.tabByKey(key);
    if (!tab) { return false; }
    var k = tab.getAttribute("data-key");
    var changed = el.getAttribute("data-ah-value") !== k;
    this.tabs().forEach(function (t) {
      var on = t === tab;
      t.classList.toggle("ah-ribbon-tab-selected", on);
      t.setAttribute("aria-selected", String(on));
      t.setAttribute("tabindex", on ? "0" : "-1");
    });
    this.panels().forEach(function (p) {
      p.classList.toggle("ah-ribbon-tab-content-active", p.getAttribute("data-key") === k);
    });
    el.setAttribute("data-ah-value", k);
    var hidden = el.querySelector(":scope > input[type=hidden]");
    if (hidden) { hidden.value = k; }
    this.updateToken();
    this.scrollTabIntoView(tab);
    return changed;
  }

  getValue() { return this.element.getAttribute("data-ah-value"); }

  enableTab(key) {
    var t = this.tabByKey(key);
    if (!t) { return; }
    t.classList.remove("ah-ribbon-tab-disabled");
    t.disabled = this.element.classList.contains("ah-ribbon-disabled");
    t.removeAttribute("aria-disabled");
  }

  disableTab(key) {
    var t = this.tabByKey(key);
    if (!t) { return; }
    t.classList.add("ah-ribbon-tab-disabled");
    t.disabled = true;
    t.setAttribute("aria-disabled", "true");
  }

  enableCommand(cmd) { this.commandNodes(cmd).forEach(function (n) { n.disabled = false; }); }
  disableCommand(cmd) { this.commandNodes(cmd).forEach(function (n) { n.disabled = true; }); }

  setPressed(cmd, on) {
    this.commandNodes(cmd).forEach(function (n) {
      if (!n.hasAttribute("data-toggle")) { return; }
      n.setAttribute("aria-pressed", String(!!on));
      n.classList.toggle("ah-ribbon-button-pressed", !!on);
    });
  }

  collapse() { this.setCollapsed(true, false); }
  expand() { this.setCollapsed(false, false); }

  close() {
    this.closeMenu(false);
    this.element.classList.remove("ah-ribbon-open");
  }

  // ---- structure ----

  tabs() {
    return Array.from(this.element.querySelectorAll(":scope > .ah-ribbon-tabs .ah-ribbon-tab"));
  }

  panels() {
    return Array.from(this.element.querySelectorAll(
      ":scope > .ah-ribbon-tabs-content > .ah-ribbon-tab-content"));
  }

  inner() {
    return this.element.querySelector(":scope > .ah-ribbon-tabs > .ah-ribbon-tabs-inner");
  }

  selectedTab() {
    return this.tabs().filter(function (t) { return t.classList.contains("ah-ribbon-tab-selected"); })[0];
  }

  vertical() { return /\bah-ribbon-position-(left|right)\b/.test(this.element.className); }
  floating() { return /\bah-ribbon-mode-(collapsed|popup)\b/.test(this.element.className); }
  collapsible() { return this.element.classList.contains("ah-ribbon-collapsible"); }
  isOpen() { return this.element.classList.contains("ah-ribbon-open"); }

  tabByKey(key) {
    return this.tabs().filter(function (t) { return t.getAttribute("data-key") === String(key); })[0];
  }

  enabledTabs() { return this.tabs().filter(function (t) { return !tabDisabled(t); }); }

  // ---- tab strip: scroll buttons and the selection token ----

  updateScroll() {
    var box = this.inner();
    if (!box) { return; }
    var v = this.vertical();
    var pos = v ? box.scrollTop : box.scrollLeft;
    var max = v ? box.scrollHeight - box.clientHeight : box.scrollWidth - box.clientWidth;
    var bar = this.element.querySelector(":scope > .ah-ribbon-tabs");
    bar.querySelectorAll(":scope > .ah-ribbon-scroll-left, :scope > .ah-ribbon-scroll-up").forEach(function (b) {
      b.classList.toggle("ah-ribbon-scroll-visible", pos > 0);
    });
    bar.querySelectorAll(":scope > .ah-ribbon-scroll-right, :scope > .ah-ribbon-scroll-down").forEach(function (b) {
      b.classList.toggle("ah-ribbon-scroll-visible", pos < max - 1);
    });
  }

  scrollTabIntoView(tab) {
    var box = this.inner();
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

  updateToken() {
    var box = this.inner();
    var tab = this.selectedTab();
    var token = box && box.querySelector(":scope > .ah-ribbon-selection-token");
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

  open() {
    if (!this.floating() || this.isOpen()) { return; }
    this.element.classList.add("ah-ribbon-open");
  }

  setCollapsed(on, notify) {
    var el = this.element;
    if (/\bah-ribbon-mode-popup\b/.test(el.className)) { return; }
    var was = el.classList.contains("ah-ribbon-mode-collapsed");
    if (was === on) { return; }
    this.close();
    el.classList.toggle("ah-ribbon-mode-collapsed", on);
    el.classList.toggle("ah-ribbon-mode-default", !on);
    el.querySelectorAll(":scope > .ah-ribbon-tabs > .ah-ribbon-collapse-btn").forEach(function (b) {
      b.setAttribute("aria-expanded", String(!on));
      b.setAttribute("aria-label", on ? "Expand the ribbon" : "Collapse the ribbon");
    });
    if (notify) { this.fire(on ? "ah:collapse" : "ah:expand"); }
  }

  // A user choosing a tab: select it, open a floating panel, fire change.
  choose(tab, toggle) {
    var el = this.element;
    if (!tab || tabDisabled(tab) || el.classList.contains("ah-ribbon-disabled")) { return; }
    var already = tab.classList.contains("ah-ribbon-tab-selected");
    var changed = this.select(tab.getAttribute("data-key"));
    if (this.floating()) {
      if (toggle && already && this.isOpen()) { this.close(); } else { this.open(); }
    }
    if (changed) { this.fire("change"); }
  }

  // ---- commands and dropdown menus ----

  menuItems(menu) {
    return Array.from(menu.querySelectorAll(":scope > .ah-dropdown-btn-item"))
      .filter(function (b) { return !b.disabled; });
  }

  openMenu(toggle, focusFirst) {
    this.closeMenu(false);
    var menu = Array.from(toggle.parentNode.children).filter(function (c) {
      return c !== toggle && c.classList.contains("ah-ribbon-menu");
    })[0];
    if (!menu) { return; }
    menu.hidden = false;
    toggle.setAttribute("aria-expanded", "true");
    this.menu = { toggle: toggle, menu: menu, float: AH.float(menu, toggle, { placement: "bottom" }) };
    if (focusFirst) {
      var first = this.menuItems(menu)[0];
      if (first) { first.focus(); }
    }
  }

  closeMenu(refocus) {
    if (!this.menu) { return; }
    var m = this.menu;
    this.menu = null;
    m.float.stop();
    m.menu.hidden = true;
    m.toggle.setAttribute("aria-expanded", "false");
    if (refocus) { m.toggle.focus(); }
  }

  runCommand(node) {
    var el = this.element;
    if (node.disabled || el.classList.contains("ah-ribbon-disabled")) { return; }
    var cmd = node.getAttribute("data-command");
    var pressed = null;
    if (node.hasAttribute("data-toggle")) {
      pressed = node.getAttribute("aria-pressed") !== "true";
      this.setPressed(cmd, pressed);
    }
    var inMenu = !!node.closest(".ah-ribbon-menu");
    this.closeMenu(inMenu);
    if (this.floating()) {
      this.close();
      var tab = this.selectedTab();
      if (tab && inMenu) { tab.focus(); }
    }
    el.setAttribute("data-command", cmd);
    if (pressed === null) {
      el.removeAttribute("data-pressed");
    } else {
      el.setAttribute("data-pressed", String(pressed));
    }
    this.fire("ah:command", { command: cmd, pressed: pressed });
  }

  commandNodes(cmd) {
    var el = this.element;
    return Array.from(el.querySelectorAll("[data-command]")).filter(function (n) {
      return n.getAttribute("data-command") === String(cmd) && n.closest(".ah-ribbon") === el;
    });
  }

  // ---- keyboard ----

  panelFocusables() {
    var active = this.panels().filter(function (p) {
      return p.classList.contains("ah-ribbon-tab-content-active");
    });
    var out = [];
    active.forEach(function (p) {
      p.querySelectorAll("button, [href], input, select, textarea, [tabindex]").forEach(function (n) {
        if (!n.disabled && n.getAttribute("tabindex") !== "-1" && !n.closest("[hidden]")) {
          out.push(n);
        }
      });
    });
    return out;
  }

  tabKey(tab, e) {
    var el = this.element;
    var v = this.vertical();
    var en = this.enabledTabs();
    var i = en.indexOf(tab);
    var next = null;
    var prevKey = v ? "ArrowUp" : "ArrowLeft";
    var nextKey = v ? "ArrowDown" : "ArrowRight";
    var intoKey = { top: "ArrowDown", bottom: "ArrowUp", left: "ArrowRight", right: "ArrowLeft" }[
      (/\bah-ribbon-position-(\w+)\b/.exec(el.className) || [0, "top"])[1]];
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
        var f = this.panelFocusables()[0];
        if (f) { f.focus(); }
        return;
      }
      case "Escape":
        if (this.isOpen()) { e.preventDefault(); this.close(); }
        return;
      default: return;
    }
    e.preventDefault();
    var t = en[next];
    if (t) {
      t.focus();
      this.choose(t, false);
    }
  }

  menuKey(item, e) {
    var items = this.menuItems(item.closest(".ah-ribbon-menu"));
    var i = items.indexOf(item);
    var next = null;
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
});
