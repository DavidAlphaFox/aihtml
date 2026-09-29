/* Behaviour of the toolbar component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.js.
   A tool with data-key sets data-ah-value (and the hidden input) and
   fires "change" on the root; ah:open / ah:close (no detail) follow the
   overflow popup. */
import AH from "../core.js";
import "./_lib_nav.js";

const N = AH.lib.nav;
const visible = N.visible;
const setValue = N.setValue;
const byKey = N.byKey;

// ------------------------------------------------------------------
// toolbar (sigil toolbar.cljs, toolbar/overflow.cljs)
// ------------------------------------------------------------------
//
// Tools that do not fit are moved (not copied) into the overflow popup,
// so their handlers and data-ah-on keep working, and moved back when
// there is room again.

function outerWidth(n) {
  const cs = getComputedStyle(n);
  return n.offsetWidth + (parseFloat(cs.marginLeft) || 0) + (parseFloat(cs.marginRight) || 0);
}

function innerWidth(n) {
  const cs = getComputedStyle(n);
  return n.clientWidth - (parseFloat(cs.paddingLeft) || 0) - (parseFloat(cs.paddingRight) || 0);
}

function tbTools(el) {
  return Array.from(el.querySelectorAll(":scope > .ah-toolbar-tool")).map(function (tool) {
    const prev = tool.previousElementSibling;
    return {
      el: tool,
      sep: prev && prev.classList.contains("ah-toolbar-separator") ? prev : null,
      minimizable: tool.getAttribute("data-ah-minimizable") !== "false",
      button: !!tool.querySelector(":scope > button.ah-toolbar-tool-el")
    };
  });
}

function tbMinimize(t) {
  if (t.min) { return; }
  t.min = true;
  t.el.style.display = "none";
  if (t.sep) { t.sep.style.display = "none"; }
  if (t.menuSep) { t.menuSep.classList.add("ah-toolbar-popup-separator-visible"); }
  t.popupTool.append.apply(t.popupTool, Array.from(t.el.children));
  t.popupTool.classList.add("ah-toolbar-popup-tool-visible");
}

function tbRestore(t) {
  if (!t.min) { return; }
  t.min = false;
  t.el.append.apply(t.el, Array.from(t.popupTool.children));
  t.el.style.display = "";
  if (t.sep) { t.sep.style.display = ""; }
  if (t.menuSep) { t.menuSep.classList.remove("ah-toolbar-popup-separator-visible"); }
  t.popupTool.classList.remove("ah-toolbar-popup-tool-visible");
}

function tbGroups(st) {
  const shown = st.tools.filter(function (t) { return !t.min; });
  shown.forEach(function (t, i) {
    const prev = i > 0 && shown[i - 1].button && !t.sep;
    const next = i + 1 < shown.length && shown[i + 1].button && !shown[i + 1].sep;
    t.el.classList.remove("ah-toolbar-tool-first", "ah-toolbar-tool-inner", "ah-toolbar-tool-last");
    if (!t.button) { return; }
    if (prev && next) { t.el.classList.add("ah-toolbar-tool-inner"); }
    else if (next) { t.el.classList.add("ah-toolbar-tool-first"); }
    else if (prev) { t.el.classList.add("ah-toolbar-tool-last"); }
  });
}

AH.register("toolbar", class extends AH.Controller {
  setup() {
    const el = this.element;
    const self = this;
    const popup = document.createElement("div");
    popup.className = "ah-toolbar-popup";
    popup.setAttribute("role", "menu");
    popup.setAttribute("aria-label", "Overflow tools");
    const st = this.st = { tools: tbTools(el), popup: popup, open: false, float: null, ro: null };
    st.tools.forEach(function (t) {
      if (t.sep) {
        t.menuSep = document.createElement("div");
        t.menuSep.className = "ah-toolbar-popup-separator";
        t.menuSep.setAttribute("role", "separator");
        popup.appendChild(t.menuSep);
      }
      t.popupTool = document.createElement("div");
      t.popupTool.className = "ah-toolbar-popup-tool";
      popup.appendChild(t.popupTool);
      t.min = false;
    });
    document.body.appendChild(popup);

    const onTool = function (e, btn) {
      if (btn.disabled) { return; }
      self.activate(btn);
      if (popup.contains(btn) && !btn.hasAttribute("data-ah-toggle")) { self.close(); }
    };
    this.delegate("click", "button.ah-toolbar-tool-el", onTool);
    this.delegate("click", "button.ah-toolbar-tool-el", onTool, popup);
    this.listen(popup, "keydown", function (e) {
      if (e.key === "Escape") {
        self.close();
        const btn = self.minBtn();
        if (btn) { btn.focus(); }
      }
    });
    this.delegate("click", ".ah-toolbar-minimize-btn", function (e) {
      e.stopPropagation();
      if (st.open) { self.close(); } else { self.open(); }
    });
    this.delegate("keydown", ".ah-toolbar-minimize-btn", function (e) {
      if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        if (st.open) { self.close(); }
        else {
          self.open();
          const f = Array.from(popup.querySelectorAll("button:not([disabled]), select, input, [tabindex]"))
            .filter(visible)[0];
          if (f) { f.focus(); }
        }
      }
    });
    // arrows move between tools, Home / End jump to the ends
    this.listen(el, "keydown", function (e) {
      if (!/^(ArrowLeft|ArrowRight|Home|End)$/.test(e.key)) { return; }
      if (/^(INPUT|SELECT|TEXTAREA)$/.test(e.target.tagName)) { return; }
      const items = Array.from(el.querySelectorAll(
        ".ah-toolbar-tool button:not([disabled]), .ah-toolbar-tool select, " +
        ".ah-toolbar-tool input, .ah-toolbar-minimize-btn")).filter(visible);
      const i = items.indexOf(e.target);
      if (i < 0) { return; }
      const n = items.length;
      const to = e.key === "Home" ? 0 : e.key === "End" ? n - 1
        : (i + (e.key === "ArrowRight" ? 1 : -1) + n) % n;
      e.preventDefault();
      items[to].focus();
    });
    this.listen(document, "mousedown", function (e) {
      const t = e.target;
      if (st.open && !popup.contains(t) && !(t.closest && t.closest(".ah-toolbar-minimize-btn"))) {
        self.close();
      }
    });
    if (window.ResizeObserver) {
      st.ro = new ResizeObserver(function () { self.layout(); });
      st.ro.observe(el);
    } else {
      this.listen(window, "resize", function () { self.layout(); });
    }
    requestAnimationFrame(function () { self.layout(); });
  }

  teardown() {
    const st = this.st;
    if (!st) { return; }
    if (st.ro) { st.ro.disconnect(); }
    if (st.float) { st.float.stop(); }
    st.tools.forEach(tbRestore);
    st.popup.remove();
    this.st = null;
  }

  minBtn() { return this.element.querySelector(":scope > .ah-toolbar-minimize-btn"); }

  activate(btn) {
    const key = btn.getAttribute("data-key");
    if (btn.hasAttribute("data-ah-toggle")) {
      const on = btn.getAttribute("aria-pressed") !== "true";
      btn.setAttribute("aria-pressed", String(on));
      btn.classList.toggle("ah-btn-toggled", on);
    }
    if (key) { setValue(this.element, key, "change"); }
  }

  buttons(key) {
    const scopes = [this.element];
    if (this.st) { scopes.push(this.st.popup); }
    return byKey(scopes, "button.ah-toolbar-tool-el", "data-key", key);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  layout() {
    const el = this.element;
    const st = this.st;
    if (!st || !visible(el)) { return; }
    const btn = this.minBtn();
    const avail = function () {
      // the button's margin-left is auto, so count its box only
      return innerWidth(el) -
        (btn && btn.classList.contains("ah-toolbar-minimize-visible") ? btn.offsetWidth : 0);
    };
    const used = function () {
      return st.tools.reduce(function (acc, t) {
        if (t.min) { return acc; }
        return acc + outerWidth(t.el) + (t.sep ? outerWidth(t.sep) : 0);
      }, 0);
    };
    const showBtn = function (on) { if (btn) { btn.classList.toggle("ah-toolbar-minimize-visible", on); } };
    let cands;
    // minimise from the right while the tools overflow
    while (used() > avail() &&
           (cands = st.tools.filter(function (t) { return t.minimizable && !t.min; })).length) {
      showBtn(true);
      tbMinimize(cands[cands.length - 1]);
    }
    // restore from the left while they fit
    let hidden;
    while ((hidden = st.tools.filter(function (t) { return t.minimizable && t.min; })).length) {
      const t = hidden[0];
      tbRestore(t);
      if (hidden.length === 1) { showBtn(false); }
      if (used() > avail()) {
        showBtn(true);
        tbMinimize(t);
        break;
      }
    }
    const any = st.tools.some(function (t) { return t.min; });
    showBtn(any);
    if (!any) { this.close(); }
    tbGroups(st);
  }

  open() {
    const st = this.st;
    if (!st || st.open) { return; }
    const w = parseInt(this.element.getAttribute("data-ah-popup-width"), 10) || 200;
    st.popup.style.width = w + "px";
    st.popup.classList.add("ah-toolbar-popup-open");
    st.float = AH.float(st.popup, this.element, { placement: "bottom", align: "end", offset: 0 });
    st.open = true;
    const btn = this.minBtn();
    if (btn) { btn.setAttribute("aria-expanded", "true"); }
    this.fire("ah:open");
  }

  close() {
    const st = this.st;
    if (!st || !st.open) { return; }
    st.popup.classList.remove("ah-toolbar-popup-open");
    if (st.float) { st.float.stop(); st.float = null; }
    st.open = false;
    const btn = this.minBtn();
    if (btn) { btn.setAttribute("aria-expanded", "false"); }
    this.fire("ah:close");
  }

  disableTool(key, disabled) {
    this.buttons(key).forEach(function (b) { b.disabled = disabled !== false; });
  }

  setPressed(key, pressed) {
    this.buttons(key).forEach(function (b) {
      b.setAttribute("aria-pressed", String(!!pressed));
      b.classList.toggle("ah-btn-toggled", !!pressed);
    });
  }
});
