/* Behaviour of the command palette (designs/04-components.md), ported from
 * sigil's overlay/command. It filters the commands the server rendered by
 * hiding the ones that do not match; with a search action (data-ah-remote)
 * the server renders the results instead. It builds no HTML.
 * Events on the root: ah:select (detail: the chosen value), ah:query
 * (detail: the query text), ah:open / ah:close (no detail). */
import AH from "../core.js";
import "./_lib_nav.js";

// ------------------------------------------------------------------
// Command: search field + filtered list + keyboard navigation
// ------------------------------------------------------------------

function cmdInput(el) { return el.querySelector(".ah-command__input"); }
function cmdVisible(el) { return Array.from(el.querySelectorAll(".ah-command__item:not([hidden])")); }
function cmdOverlay(el) {
  const p = el.parentElement;
  return p && p.classList.contains("ah-command-overlay") ? p : null;
}

function cmdActive(el) {
  return cmdVisible(el).indexOf(el.querySelector(".ah-command__item[data-active=true]"));
}

function cmdSetActive(el, i, scroll) {
  const v = cmdVisible(el);
  const input = cmdInput(el);
  el.querySelectorAll(".ah-command__item[data-active=true]").forEach(function (it) {
    it.setAttribute("data-active", "false");
    it.setAttribute("aria-selected", "false");
  });
  if (!v.length) {
    if (input) { input.removeAttribute("aria-activedescendant"); }
    return;
  }
  i = ((i % v.length) + v.length) % v.length;
  const it = v[i];
  it.setAttribute("data-active", "true");
  it.setAttribute("aria-selected", "true");
  if (it.id && input) { input.setAttribute("aria-activedescendant", it.id); }
  if (scroll && it.scrollIntoView) { it.scrollIntoView({ block: "nearest" }); }
}

function text(root, sel) {
  return Array.from(root.querySelectorAll(sel)).map(function (n) { return n.textContent; }).join("");
}

// sigil's match?: a case-insensitive substring of value, label or
// description.
function cmdFilter(el) {
  if (el.hasAttribute("data-ah-remote")) { return; }
  const input = cmdInput(el);
  const q = String((input && input.value) || "").trim().toLowerCase();
  el.querySelectorAll(".ah-command__item").forEach(function (it) {
    const hay = [it.getAttribute("data-value") || "",
                 text(it, ".ah-command__item-label"),
                 text(it, ".ah-command__item-desc")].join("\n").toLowerCase();
    it.hidden = q !== "" && hay.indexOf(q) < 0;
  });
  let any = false;
  el.querySelectorAll(".ah-command__group").forEach(function (g) {
    const shown = !!g.querySelector(":scope > .ah-command__item:not([hidden])");
    g.hidden = !shown;
    any = any || shown;
  });
  el.querySelectorAll(".ah-command__empty").forEach(function (n) { n.hidden = any; });
  cmdSetActive(el, 0, true);
}

function cmdFocus(el) {
  const inp = cmdInput(el);
  if (inp) {
    inp.focus();
    const n = inp.value.length;
    try { inp.setSelectionRange(n, n); } catch (e) { /* not a text field */ }
  }
}

AH.register("command", class extends AH.Controller {
  setup() {
    const el = this.element;
    const self = this;
    this.returnTo = null;
    this.delegate("input", ".ah-command__input", function (e, input) {
      cmdFilter(el);
      self.fire("ah:query", input.value);
    });
    this.delegate("keydown", ".ah-command__input", function (e) {
      const act = cmdActive(el);
      switch (e.key) {
        case "ArrowDown": e.preventDefault(); cmdSetActive(el, act + 1, true); break;
        case "ArrowUp": e.preventDefault(); cmdSetActive(el, act - 1, true); break;
        case "Home": if (e.ctrlKey) { e.preventDefault(); cmdSetActive(el, 0, true); } break;
        case "End": if (e.ctrlKey) { e.preventDefault(); cmdSetActive(el, -1, true); } break;
        case "Enter":
          e.preventDefault();
          self.choose(cmdVisible(el)[act]);
          break;
        case "Escape":
          e.preventDefault();
          if (cmdOverlay(el)) { self.close(); } else { self.fire("ah:close"); }
          break;
      }
    });
    AH.lib.nav.hover(this, ".ah-command__item", function (item) {
      cmdSetActive(el, cmdVisible(el).indexOf(item), false);
    });
    this.delegate("click", ".ah-command__item", function (e, item) {
      self.choose(item);
    });
    const ov = cmdOverlay(el);
    if (ov) {
      this.listen(ov, "mousedown", function (e) {
        if (e.target === e.currentTarget) { self.close(); }
      });
    }
    const key = (el.getAttribute("data-hotkey") || "").toLowerCase();
    if (key) {
      this.listen(document, "keydown", function (e) {
        if ((e.ctrlKey || e.metaKey) && String(e.key).toLowerCase() === key) {
          e.preventDefault();
          if (ov && !ov.hidden) { self.close(); } else { self.open(); }
        }
      });
    }
    cmdFilter(el);
    if (el.hasAttribute("data-auto-focus") && !ov) {
      setTimeout(function () { cmdFocus(el); }, 0);
    }
  }

  choose(it) {
    const el = this.element;
    if (!it || it.getAttribute("data-disabled") === "true") { return; }
    const v = it.getAttribute("data-value");
    el.setAttribute("data-ah-value", v);
    this.fire("ah:select", v);
    if (cmdOverlay(el) && el.getAttribute("data-close-on-select") !== "false") {
      this.close();
    }
    const href = it.getAttribute("data-href");
    if (href) { window.location.href = href; }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open() {
    const el = this.element;
    const ov = cmdOverlay(el);
    if (!ov || !ov.hidden) { return; }
    this.returnTo = document.activeElement;
    if (!el.hasAttribute("data-ah-remote")) {
      const input = cmdInput(el);
      if (input) { input.value = el.getAttribute("data-ah-query") || ""; }
      cmdFilter(el);
    }
    ov.hidden = false;
    cmdFocus(el);
    this.fire("ah:open");
  }
  close() {
    const ov = cmdOverlay(this.element);
    if (!ov || ov.hidden) { return; }
    ov.hidden = true;
    const back = this.returnTo;
    this.returnTo = null;
    if (back && back.focus && document.contains(back)) { back.focus(); }
    this.fire("ah:close");
  }
  toggle() {
    const ov = cmdOverlay(this.element);
    if (ov && !ov.hidden) { this.close(); } else { this.open(); }
  }
  setQuery(q) {
    const input = cmdInput(this.element);
    if (!input) { return; }
    input.value = q == null ? "" : String(q);
    input.dispatchEvent(new Event("input", { bubbles: true }));
  }
  focus() { cmdFocus(this.element); }
  itemsLoaded() { cmdSetActive(this.element, 0, true); }
});
