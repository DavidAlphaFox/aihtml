/* Behaviour of the navigation bar (designs/04-components.md), ported from
 * sigil's layout/navigationbar. The value (the expanded indexes "0,2") is
 * kept in data-ah-value on the root, mirrored into a hidden input, and
 * "change" fires when the user changes it. Methods called by the server
 * (AH.invoke / aihtml_action:call) do not fire "change". ah:expand /
 * ah:collapse fire on the root when a section's animation ends, detail
 * {index}. */
import AH from "../core.js";
import "./_lib_layout.js";

const L = AH.lib.layout;

// ------------------------------------------------------------------
// NavigationBar: collapsible sections
// ------------------------------------------------------------------

function navItems(el) {
  return Array.from(el.querySelectorAll(":scope > .ah-navigationbar-item"));
}

function navHeaders(el) {
  return Array.from(el.querySelectorAll(":scope > .ah-navigationbar-item > .ah-navigationbar-header"));
}

function navHeader(el, i) {
  const it = navItems(el)[i];
  return it ? it.querySelector(":scope > .ah-navigationbar-header") : null;
}

function navExpanded(el) {
  return (el.getAttribute("data-ah-value") || "").split(",").filter(function (s) {
    return s !== "";
  }).map(Number);
}

function navStore(el, list) {
  const uniq = list.filter(function (v, i) { return list.indexOf(v) === i; });
  uniq.sort(function (a, b) { return a - b; });
  L.setValue(el, uniq.join(","));
}

function navMode(el) {
  return el.getAttribute("data-expand-mode") || "single_fit_height";
}

function px(cs, prop) { return parseFloat(cs[prop]) || 0; }

// single_fit_height with a fixed height: the open body fills what the
// headers leave (sigil's sync-content-height!).
function navFit(el) {
  if (!el.hasAttribute("data-fit")) { return; }
  let used = 0;
  navItems(el).forEach(function (it) {
    const h = it.querySelector(":scope > .ah-navigationbar-header");
    const cs = getComputedStyle(it);
    // header's outer height (border box) + the item's borders
    used += (h ? h.offsetHeight : 0) + px(cs, "borderTopWidth") + px(cs, "borderBottomWidth");
  });
  // the root's inner height (padding box)
  const h = Math.max(0, el.clientHeight - used);
  el.querySelectorAll(":scope > .ah-navigationbar-item > .ah-navigationbar-body").forEach(function (b) {
    if (b.style.display !== "none") { b.style.height = h + "px"; }
  });
}

function navAnimate(el, i, open) {
  const h = navHeader(el, i);
  const body = navItems(el)[i].querySelector(":scope > .ah-navigationbar-body");
  const anim = el.getAttribute("data-animation") || "slide";
  let ms = parseInt(el.getAttribute(open ? "data-expand-duration" : "data-collapse-duration"), 10);
  if (isNaN(ms)) { ms = 250; }
  if (h) {
    h.classList.toggle("ah-navigationbar-header-expanded", open);
    h.setAttribute("aria-expanded", String(open));
    h.querySelectorAll(":scope > .ah-navigationbar-arrow").forEach(function (a) {
      a.classList.toggle("ah-navigationbar-arrow-up", open);
    });
  }
  const done = function () {
    if (body) {
      if (open) {
        L.show(body);
        navFit(el);
      } else {
        L.hide(body);
      }
    }
    el.dispatchEvent(new CustomEvent(open ? "ah:expand" : "ah:collapse",
                                     { bubbles: true, cancelable: true, detail: { index: i } }));
  };
  if (!body) { done(); return; }
  L.stop(body);
  if (anim === "slide") {
    L.slide(body, open, ms, done);
  } else if (anim === "fade") {
    L.fade(body, open, ms, done);
  } else {
    done();
  }
}

function navValid(el, i) {
  return typeof i === "number" && i >= 0 && i < navItems(el).length;
}

function navDisabled(el, i) {
  const h = navHeader(el, i);
  return !!h && h.classList.contains("ah-navigationbar-disabled");
}

function navCollapse(el, i) {
  const cur = navExpanded(el);
  if (!navValid(el, i) || cur.indexOf(i) < 0) { return false; }
  navStore(el, cur.filter(function (x) { return x !== i; }));
  navAnimate(el, i, false);
  return true;
}

function navExpand(el, i) {
  const cur = navExpanded(el);
  if (!navValid(el, i) || navDisabled(el, i) || cur.indexOf(i) >= 0) { return false; }
  if (navMode(el) !== "multiple") {
    cur.forEach(function (x) { navCollapse(el, x); });
  }
  navStore(el, navExpanded(el).concat([i]));
  navAnimate(el, i, true);
  return true;
}

// A user toggle, as sigil's compute-proposed-indexes: single modes never
// close the open section, none never changes.
function navUserToggle(el, i) {
  const mode = navMode(el);
  if (el.classList.contains("ah-navigationbar-disabled") || navDisabled(el, i) ||
      mode === "none") {
    return;
  }
  let changed;
  if (navExpanded(el).indexOf(i) >= 0) {
    changed = (mode === "single" || mode === "single_fit_height") ? false
      : navCollapse(el, i);
  } else {
    changed = navExpand(el, i);
  }
  if (changed) { el.dispatchEvent(new CustomEvent("change", { bubbles: true, cancelable: true })); }
}

AH.register("navigationbar", class extends AH.Controller {
  setup() {
    const el = this.element;
    const mode = el.getAttribute("data-toggle-mode") || "click";
    const own = function (h) { return h.closest(".ah-navigationbar") === el; };
    if (mode !== "none") {
      this.delegate(mode, ".ah-navigationbar-header", function (e, h) {
        if (own(h)) { navUserToggle(el, navHeaders(el).indexOf(h)); }
      });
    }
    this.delegate("keydown", ".ah-navigationbar-header", function (e, h) {
      if (!own(h)) { return; }
      const hs = navHeaders(el).filter(function (x) { return x.getAttribute("tabindex") === "0"; });
      const i = hs.indexOf(h);
      let t;
      switch (e.key) {
        case "Enter": case " ":
          e.preventDefault();
          if (mode !== "none") { navUserToggle(el, navHeaders(el).indexOf(h)); }
          return;
        case "ArrowDown": t = hs[(i + 1) % hs.length]; break;
        case "ArrowUp": t = hs[(i - 1 + hs.length) % hs.length]; break;
        case "Home": t = hs[0]; break;
        case "End": t = hs[hs.length - 1]; break;
        default: return;
      }
      e.preventDefault();
      if (t) { t.focus(); }
    });
    navFit(el);
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  expand(i) { navExpand(this.element, Number(i)); }
  collapse(i) { navCollapse(this.element, Number(i)); }
  toggle(i) {
    i = Number(i);
    if (navExpanded(this.element).indexOf(i) >= 0) { navCollapse(this.element, i); }
    else { navExpand(this.element, i); }
  }
  setValue(v) {
    const el = this.element;
    const want = (Array.isArray(v) ? v : String(v == null ? "" : v).split(","))
      .filter(function (s) { return s !== ""; }).map(Number);
    navExpanded(el).forEach(function (i) {
      if (want.indexOf(i) < 0) { navCollapse(el, i); }
    });
    want.forEach(function (i) {
      if (navExpanded(el).indexOf(i) < 0 && navValid(el, i)) {
        // setValue may open several sections whatever the mode
        navStore(el, navExpanded(el).concat([i]));
        navAnimate(el, i, true);
      }
    });
  }
  getValue() { return navExpanded(this.element); }
  enable(i) {
    const h = navHeader(this.element, Number(i));
    if (h) {
      h.classList.remove("ah-navigationbar-disabled");
      h.removeAttribute("aria-disabled");
      h.setAttribute("tabindex", "0");
    }
  }
  disable(i) {
    const h = navHeader(this.element, Number(i));
    if (h) {
      h.classList.add("ah-navigationbar-disabled");
      h.setAttribute("aria-disabled", "true");
      h.setAttribute("tabindex", "-1");
    }
  }
});
