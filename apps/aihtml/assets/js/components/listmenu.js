/* Behaviour of the listmenu component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.js.
   Value contract: data-ah-value (the chosen item's data-key), the hidden
   input and "change" on the root. ah:navigate fires on the root after a
   page change, detail {id, label, page}. */
import AH from "../core.js";
import "./_lib_nav.js";

const N = AH.lib.nav;
const setValue = N.setValue;
const byKey = N.byKey;

function showEl(n) { n.style.display = ""; }
function hideEl(n) { n.style.display = "none"; }

function animateSwap(oldEl, newEl, kind, dir, done) {
  if (!oldEl || oldEl === newEl) {
    showEl(newEl);
    done();
    return;
  }
  if (kind === "none" || !newEl.animate) {
    hideEl(oldEl);
    showEl(newEl);
    done();
    return;
  }
  const dur = 250;
  if (kind === "fade") {
    oldEl.animate([{ opacity: 1 }, { opacity: 0 }], { duration: dur / 2 }).onfinish = function () {
      hideEl(oldEl);
      showEl(newEl);
      newEl.animate([{ opacity: 0 }, { opacity: 1 }], { duration: dur / 2 }).onfinish = done;
    };
    return;
  }
  // slide: the old page leaves to one side while the new one comes in
  const s = dir > 0 ? "-100%" : "100%";
  const e = dir > 0 ? "100%" : "-100%";
  showEl(newEl);
  Object.assign(oldEl.style, { position: "absolute", top: "0", left: "0", width: "100%" });
  oldEl.animate([{ transform: "translateX(0)" }, { transform: "translateX(" + s + ")" }],
                { duration: dur, easing: "ease" });
  newEl.animate([{ transform: "translateX(" + e + ")" }, { transform: "translateX(0)" }],
                { duration: dur, easing: "ease" }).onfinish = function () {
    hideEl(oldEl);
    Object.assign(oldEl.style, { position: "", top: "", left: "", width: "" });
    done();
  };
}

// ------------------------------------------------------------------
// listmenu (sigil listmenu.cljs, listmenu/nav.cljs)
// ------------------------------------------------------------------

const LM_FOCUS = "ah-listmenu-item-focus";

function lmPage(el, id) {
  return Array.from(el.querySelectorAll(".ah-listmenu-page")).find(function (p) {
    return p.getAttribute("data-page-id") === String(id);
  }) || null;
}

function lmItemLabel(el, itemId) {
  const it = Array.from(el.querySelectorAll(".ah-listmenu-item")).find(function (i) {
    return i.getAttribute("data-item-id") === String(itemId);
  });
  const label = it ? it.querySelector(":scope > .ah-listmenu-item-label") : null;
  return label ? label.textContent : "";
}

function lmItems(page) {
  return page ? Array.from(page.querySelectorAll(":scope > .ah-listmenu-item")).filter(function (i) {
    return !i.classList.contains("ah-listmenu-item-disabled") && i.style.display !== "none";
  }) : [];
}

function lmFocus(el, item) {
  el.querySelectorAll("." + LM_FOCUS).forEach(function (n) { n.classList.remove(LM_FOCUS); });
  if (item) {
    item.classList.add(LM_FOCUS);
    if (item.scrollIntoView) { item.scrollIntoView({ block: "nearest" }); }
  }
}

function lmMark(el, key) {
  el.querySelectorAll(".ah-listmenu-item-selected").forEach(function (n) {
    n.classList.remove("ah-listmenu-item-selected");
    n.setAttribute("aria-checked", "false");
  });
  byKey(el, ".ah-listmenu-item:not([aria-haspopup])", "data-key", key).forEach(function (n) {
    n.classList.add("ah-listmenu-item-selected");
    n.setAttribute("aria-checked", "true");
  });
}

AH.register("listmenu", class extends AH.Controller {
  setup() {
    const el = this.element;
    const self = this;
    const s = el.getAttribute("data-ah-stack");
    this.stack = s ? s.split(",") : [];
    this.busy = false;
    this.delegate("click", ".ah-listmenu-item", function (e, item) {
      lmFocus(el, null);
      self.activate(item, false);
    });
    this.delegate("click", ".ah-listmenu-back", function () { self.goBack(false); });
    this.delegate("input", ".ah-listmenu-filter-input", function (e, input) {
      e.stopPropagation();
      self.applyFilter(input.value);
    });
    // the filter's own change must not look like a new value
    this.delegate("change", ".ah-listmenu-filter-input", function (e) { e.stopPropagation(); });
    this.listen(el, "keydown", function (e) {
      const inFilter = e.target.classList.contains("ah-listmenu-filter-input");
      const items = lmItems(self.current());
      const cur = items.indexOf(el.querySelector("." + LM_FOCUS));
      switch (e.key) {
        case "ArrowDown":
          e.preventDefault();
          lmFocus(el, items[Math.min(items.length - 1, cur + 1)]);
          break;
        case "ArrowUp":
          e.preventDefault();
          lmFocus(el, items[Math.max(0, cur - 1)]);
          break;
        case "Home":
        case "End":
          if (inFilter) { return; }
          e.preventDefault();
          lmFocus(el, items[e.key === "Home" ? 0 : items.length - 1]);
          break;
        case "Enter":
        case " ":
        case "ArrowRight":
          if (inFilter && e.key !== "Enter") { return; }
          if (cur < 0) { return; }
          e.preventDefault();
          self.activate(items[cur], true);
          break;
        case "ArrowLeft":
        case "Backspace":
        case "Escape":
          if (inFilter && e.key !== "Escape") { return; }
          if (!self.stack.length) { return; }
          e.preventDefault();
          self.goBack(true);
          break;
        default:
          break;
      }
    });
    this.listen(el, "focus", function () {
      if (!el.querySelector("." + LM_FOCUS)) {
        const page = self.current();
        const sel = page ? page.querySelector(":scope > .ah-listmenu-item-selected") : null;
        lmFocus(el, sel || lmItems(page)[0]);
      }
    });
    this.listen(el, "blur", function () { lmFocus(el, null); });
  }

  current() {
    return lmPage(this.element, this.stack.length ? this.stack[this.stack.length - 1] : "root");
  }

  header() {
    const el = this.element;
    const root = !this.stack.length;
    el.querySelectorAll(".ah-listmenu-back").forEach(function (b) { b.style.display = root ? "none" : ""; });
    const title = root ? "" : lmItemLabel(el, this.stack[this.stack.length - 1]);
    el.querySelectorAll(".ah-listmenu-title").forEach(function (t) { t.textContent = title; });
  }

  applyFilter(text) {
    const t = (text || "").toLowerCase();
    const page = this.current();
    if (!page) { return; }
    page.querySelectorAll(":scope > .ah-listmenu-item").forEach(function (i) {
      const l = i.querySelector(":scope > .ah-listmenu-item-label");
      const label = (l ? l.textContent : "").toLowerCase();
      i.style.display = !t || label.indexOf(t) !== -1 ? "" : "none";
    });
  }

  go(pageId, dir, focus) {
    const el = this.element;
    const self = this;
    if (this.busy) { return; }
    const old = this.current();
    const next = lmPage(el, pageId === null ? "root" : pageId);
    if (!next) { return; }
    const top = this.stack[this.stack.length - 1];
    const label = dir > 0 ? lmItemLabel(el, pageId) : lmItemLabel(el, top);
    const id = dir > 0 ? pageId : top;
    if (dir > 0) { this.stack.push(String(pageId)); } else { this.stack.pop(); }
    this.header();
    const input = el.querySelector(".ah-listmenu-filter-input");
    if (input && input.value) {
      input.value = "";
      if (old) { Array.from(old.children).forEach(showEl); }
    }
    this.busy = true;
    animateSwap(old, next, el.getAttribute("data-ah-animation") || "slide", dir, function () {
      self.busy = false;
      lmFocus(el, focus ? lmItems(next)[0] : null);
      self.fire("ah:navigate", { id: id, label: label, page: next.getAttribute("data-page-id") });
    });
  }

  goBack(focus) {
    if (!this.stack.length) { return; }
    this.go(this.stack.length > 1 ? this.stack[this.stack.length - 2] : null, -1, focus);
  }

  activate(item, focus) {
    const el = this.element;
    if (item.classList.contains("ah-listmenu-item-disabled")) { return; }
    if (item.getAttribute("aria-haspopup")) {
      this.go(item.getAttribute("data-item-id"), 1, focus);
      return;
    }
    const href = item.getAttribute("data-href");
    if (href) {
      window.location.href = href;
      return;
    }
    const key = item.getAttribute("data-key");
    lmMark(el, key);
    if (el.getAttribute("data-ah-value") !== key) { setValue(el, key, "change"); }
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(key) { lmMark(this.element, key); setValue(this.element, key); }
  back() { this.goBack(false); }
  navigate(key) {
    const i = byKey(this.current(), ".ah-listmenu-item[aria-haspopup]", "data-key", key)[0];
    if (i) { this.go(i.getAttribute("data-item-id"), 1, false); }
  }
  filter(text) {
    const input = this.element.querySelector(".ah-listmenu-filter-input");
    if (input) { input.value = text || ""; }
    this.applyFilter(text);
  }
  currentPage() {
    const p = this.current();
    return p ? p.getAttribute("data-page-id") : undefined;
  }
});
