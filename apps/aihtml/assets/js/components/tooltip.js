/* The tooltip behaviour (designs/04-components.md), ported from sigil's
 * overlay/tooltip: [data-ah="tooltip"] wrappers and [data-ah-tooltip]
 * elements (tooltip_attrs/2), driven by delegated document listeners.
 * Escape closes open tooltips before any other overlay (an escHook of
 * _lib_overlay.js). Events on the host: ah:opening (cancelable), ah:open,
 * ah:close (detail {result: null}). */
// ah-load: [data-ah-tooltip]
import AH from "../core.js";
import "./_lib_overlay.js";

var L = AH.lib.overlay;
var uid = L.uid,
    floatWithArrow = L.floatWithArrow,
    matching = L.matching;

// ------------------------------------------------------------------
// Tooltip: [data-ah="tooltip"] wrappers and [data-ah-tooltip] elements
// ------------------------------------------------------------------

var TIP_HOSTS = '[data-ah="tooltip"],[data-ah-tooltip]';
var TIP_POSITIONS = "ah-tooltip-top ah-tooltip-bottom ah-tooltip-left ah-tooltip-right";
var openTips = [];
var touchOnly = !!(window.matchMedia && window.matchMedia("(hover: none)").matches);
// host -> {open, showTimer, hideTimer, tip, created, float}
var states = new WeakMap();

function tipOpt(host, name, dflt) {
  var v = host.getAttribute("data-ah-tip-" + name);
  return v === null || v === "" ? dflt : v;
}

function tipState(host) {
  var st = states.get(host);
  if (!st) {
    st = { open: false, showTimer: null, hideTimer: null, tip: null, created: false, float: null };
    states.set(host, st);
  }
  return st;
}

function tipElement(host, st) {
  if (st.tip) { return st.tip; }
  if (host.getAttribute("data-ah") === "tooltip") {
    st.tip = host.querySelector(":scope > .ah-tooltip");
  } else {
    var tip = document.createElement("span");
    tip.className = "ah-tooltip";
    tip.setAttribute("role", "tooltip");
    tip.innerHTML = '<span class="ah-tooltip-arrow" aria-hidden="true"></span>' +
      '<span class="ah-tooltip-content"></span>';
    tip.querySelector(".ah-tooltip-content").textContent = host.getAttribute("data-ah-tooltip");
    if (tipOpt(host, "arrow", "true") === "false") { tip.classList.add("ah-tooltip-no-arrow"); }
    document.body.appendChild(tip);
    st.tip = tip;
    st.created = true;
  }
  if (st.tip && !st.tip.id) { st.tip.id = uid("ah-tip-"); }
  return st.tip;
}

function positionTip(host, tip, e) {
  var pos = tipOpt(host, "position", "bottom");
  unfloatTip(tipState(host));
  tip.style.position = "fixed";
  if (pos === "mouse") {
    tip.classList.add("ah-tooltip-no-arrow");
    var x = e && e.clientX !== undefined ? e.clientX : host.getBoundingClientRect().left;
    var y = e && e.clientY !== undefined ? e.clientY : host.getBoundingClientRect().bottom;
    tip.style.left = (x + 10) + "px";
    tip.style.top = (y + 10) + "px";
    return;
  }
  // the tooltip's arrow is 6px (tooltip.css)
  var anchor = host;
  if (host.getAttribute("data-ah") === "tooltip") {
    anchor = Array.from(host.children).filter(function (c) {
      return !c.classList.contains("ah-tooltip");
    })[0] || host;
  }
  tipState(host).float = floatWithArrow(tip, anchor, pos, 6, "ah-tooltip-", TIP_POSITIONS);
}

function unfloatTip(st) {
  if (st.float) {
    st.float.stop();
    st.float = null;
  }
}

function fire(host, type, detail) { return L.fire(host, type, detail); }

function openTip(host, e) {
  var st = tipState(host);
  if (st.open || tipOpt(host, "disabled", "false") === "true") { return; }
  if (!fire(host, "ah:opening")) { return; }
  var tip = tipElement(host, st);
  if (!tip) { return; }
  L.stopFade(tip, false);
  Object.assign(tip.style, { display: "block", visibility: "hidden", opacity: "0" });
  positionTip(host, tip, e);
  tip.style.visibility = "visible";
  L.fade(tip, 0.9, 200);
  host.setAttribute("aria-describedby", tip.id);
  st.open = true;
  openTips.push(host);
  fire(host, "ah:open");
  if (tipOpt(host, "auto-hide", "true") !== "false") {
    clearTimeout(st.hideTimer);
    st.hideTimer = setTimeout(function () { closeTip(host); },
                              parseInt(tipOpt(host, "hide-delay", "3000"), 10));
  }
}

function closeTip(host, now) {
  var st = tipState(host);
  clearTimeout(st.showTimer);
  clearTimeout(st.hideTimer);
  if (!st.open) { return; }
  st.open = false;
  openTips = openTips.filter(function (h) { return h !== host; });
  host.removeAttribute("aria-describedby");
  var tip = st.tip;
  var done = function () {
    unfloatTip(st);
    tip.style.display = "none";
    tip.style.visibility = "hidden";
    if (st.created) {
      tip.remove();
      st.tip = null;
    }
  };
  if (now) {
    L.stopFade(tip, false);
    done();
  } else {
    L.fade(tip, 0, 200, done);
  }
  fire(host, "ah:close", { result: null });
}

function closeTips() {
  var had = openTips.length > 0;
  openTips.slice().forEach(function (h) { closeTip(h); });
  return had;
}

function tipTrigger(host) {
  var t = tipOpt(host, "trigger", "hover");
  return t === "hover" && touchOnly ? "click" : t;
}

function hosts(node) { return matching(node, TIP_HOSTS); }

// mouseenter / mouseleave of each host, from the bubbling mouseover /
// mouseout (the pointer came from, or went to, outside the host)
document.addEventListener("mouseover", function (e) {
  hosts(e.target).forEach(function (host) {
    if (e.relatedTarget && host.contains(e.relatedTarget)) { return; }
    if (tipTrigger(host) !== "hover") { return; }
    var st = tipState(host);
    clearTimeout(st.showTimer);
    var ev = { clientX: e.clientX, clientY: e.clientY };
    st.showTimer = setTimeout(function () {
      if (document.contains(host)) { openTip(host, ev); }
    }, parseInt(tipOpt(host, "delay", "100"), 10));
  });
});

document.addEventListener("mouseout", function (e) {
  hosts(e.target).forEach(function (host) {
    if (e.relatedTarget && host.contains(e.relatedTarget)) { return; }
    if (tipTrigger(host) === "hover" &&
        !(host !== document.activeElement && host.contains(document.activeElement))) {
      closeTip(host);
    }
  });
});

document.addEventListener("mousemove", function (e) {
  hosts(e.target).forEach(function (host) {
    var st = states.get(host);
    if (st && st.open && st.tip && tipOpt(host, "position", "") === "mouse") {
      st.tip.style.left = (e.clientX + 10) + "px";
      st.tip.style.top = (e.clientY + 10) + "px";
    }
  });
});

document.addEventListener("focusin", function (e) {
  hosts(e.target).forEach(function (host) {
    if (tipTrigger(host) === "hover") { openTip(host); }
  });
});

document.addEventListener("focusout", function (e) {
  hosts(e.target).forEach(function (host) {
    var to = e.relatedTarget;
    if (tipTrigger(host) === "hover" && !(to && to !== host && host.contains(to))) {
      closeTip(host);
    }
  });
});

document.addEventListener("click", function (e) {
  var inner = hosts(e.target)[0];
  if (inner && tipTrigger(inner) === "click") {
    if (tipState(inner).open) { closeTip(inner); } else { openTip(inner, e); }
  }
  // click-triggered tooltips close on a click elsewhere
  openTips.slice().forEach(function (host) {
    if (tipTrigger(host) === "click" && !host.contains(e.target)) {
      closeTip(host);
    }
  });
});

AH.register("tooltip", class extends AH.Controller {
  // the delegated document listeners above do the work
  teardown() {
    var el = this.element;
    closeTip(el, true);
    states.delete(el);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open() { openTip(this.element); }
  close() { closeTip(this.element); }
  toggle() {
    if (tipState(this.element).open) { closeTip(this.element); } else { openTip(this.element); }
  }
  setContent(text) {
    this.element.querySelectorAll(".ah-tooltip-content").forEach(function (c) {
      c.textContent = text;
    });
  }
});

L.escHooks.push(closeTips);
