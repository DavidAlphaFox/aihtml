/* Internal: what the overlay behaviours share (tooltip.js, popover.js,
 * drawer.js, sheet.js, window.js, toast.js, notification.js), ported
 * from sigil's internal/common and scroll_lock:
 *   - one z-index counter, so the overlay opened last is on top
 *   - one scroll-lock counter (body.ah-scroll-locked)
 *   - one stack of open overlays: Escape closes the top one (after the
 *     escHooks, e.g. open tooltips, had their turn), Tab is trapped in the
 *     top one when it is modal, focus returns to the element that had it
 *     when the overlay closes
 *   - the declarative triggers (aihtml_lib_overlay:opens/toggles/closes),
 *     one delegated click listener: data-ah-open / data-ah-toggle /
 *     data-ah-close hold a selector; an empty data-ah-close closes the
 *     enclosing overlay
 *   - bubble positioning with an arrow (tooltip, popover)
 *   - opacity fades (fade, fadeIn, fadeOut, stopFade)
 *   - the drawer / sheet controller (defineSlide)
 *   - notification cards in a screen corner (toast, notification)
 *
 * Events (native CustomEvents) on the component root: ah:opening and
 * ah:closing (cancelable; the window's ah:closing has detail {result}),
 * ah:open, ah:close (detail {result}), and for the window ah:collapse,
 * ah:expand, ah:moving / ah:moved (detail {x, y}), ah:resize (detail
 * {width, height}); notification cards fire ah:click. */
// ah-load: [data-ah-open], [data-ah-toggle], [data-ah-close]
import AH from "../core.js";

AH.lib = AH.lib || {};
// escHooks: functions run on Escape before the stack; one returning
// true handled it
var L = AH.lib.overlay = { escHooks: [] };

var FOCUSABLE = "a[href], area[href], button:not([disabled]), " +
  "input:not([disabled]):not([type='hidden']), select:not([disabled]), " +
  "textarea:not([disabled]), iframe, [tabindex]:not([tabindex='-1']), " +
  "[contenteditable='true']";
var OVERLAYS = '[data-ah="drawer"],[data-ah="sheet"],[data-ah="window"],' +
  '[data-ah="popover"],[data-ah="tooltip"],[data-ah="notification"]';
var seq = 0;

function uid(prefix) { return prefix + (++seq); }

// data-ah-<name>: "false" is false, absent is the default
function flag(el, name, dflt) {
  var v = el.getAttribute("data-ah-" + name);
  if (v === null || v === "") { return dflt; }
  return v !== "false";
}

function num(el, name, dflt) {
  var v = parseFloat(el.getAttribute("data-ah-" + name));
  return isNaN(v) ? dflt : v;
}

// A native bubbling, cancelable event; returns false when a listener
// prevented it.
function fire(target, type, detail) {
  return target.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
}

// The elements from node up to the document that match selector
// (innermost first): what a jQuery delegated handler ran for.
function matching(node, selector) {
  var out = [];
  for (var n = node && node.nodeType === 1 ? node : node && node.parentElement; n; n = n.parentElement) {
    if (n.matches(selector)) { out.push(n); }
  }
  return out;
}

function visible(el) {
  return !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length);
}

// ------------------------------------------------------------------
// Fades (jQuery's fadeIn / fadeOut / animate({opacity}) before): one
// running fade per element, a new one replaces it.
// ------------------------------------------------------------------

var fades = new WeakMap();

// Stop the element's fade: where it is (the opacity it reached stays
// inline), or at its end (jumpToEnd: its completion runs now).
function stopFade(el, jumpToEnd) {
  var f = fades.get(el);
  if (!f) { return; }
  fades.delete(el);
  f.anim.onfinish = null;
  if (!jumpToEnd) { el.style.opacity = window.getComputedStyle(el).opacity; }
  f.anim.cancel();
  if (jumpToEnd) { f.end(); }
}

// Fade el's opacity to `to' in ms, then done(). The final opacity stays
// inline (as jQuery's animate left it) unless end() changes it.
function fade(el, to, ms, done, end) {
  stopFade(el, false);
  var from = parseFloat(window.getComputedStyle(el).opacity);
  var finish = function () {
    el.style.opacity = String(to);
    if (end) { end(); }
    if (done) { done(); }
  };
  if (!ms || typeof el.animate !== "function") {
    finish();
    return;
  }
  el.style.opacity = String(to);
  var anim = el.animate([{ opacity: isNaN(from) ? 1 : from }, { opacity: to }],
                        { duration: ms, easing: "ease-in-out" });
  var f = { anim: anim, end: finish };
  fades.set(el, f);
  anim.onfinish = function () {
    if (fades.get(el) === f) { fades.delete(el); }
    finish();
  };
}

// Show a hidden element (its stylesheet display, block if that hides
// it) and fade it in.
function fadeIn(el, ms, done) {
  stopFade(el, false);
  if (window.getComputedStyle(el).display === "none") {
    el.style.display = "";
    if (window.getComputedStyle(el).display === "none") { el.style.display = "block"; }
    el.style.opacity = "0";
  }
  fade(el, 1, ms, done, function () { el.style.opacity = ""; });
}

// Fade out, then display: none (opacity restored for the next show).
function fadeOut(el, ms, done) {
  if (window.getComputedStyle(el).display === "none") {
    stopFade(el, false);
    el.style.display = "none";
    if (done) { done(); }
    return;
  }
  fade(el, 0, ms, done, function () {
    el.style.display = "none";
    el.style.opacity = "";
  });
}

// ------------------------------------------------------------------
// z-index, scroll lock, stack of open overlays
// ------------------------------------------------------------------

// Above sigil's fixed drawer (20600) and sheet (20500) layers, below the
// notification corner (99999).
var z = 21000;
function nextZ() { return ++z; }

var locks = 0;
function lock() {
  if (locks++ === 0) { document.body.classList.add("ah-scroll-locked"); }
}
function unlock() {
  locks = Math.max(0, locks - 1);
  if (locks === 0) { document.body.classList.remove("ah-scroll-locked"); }
}

// {el, trap: element to keep Tab in or null, esc: function -> handled?}
var stack = [];

function pushStack(entry) {
  pullStack(entry.el, false);
  entry.returnTo = document.activeElement;
  stack.push(entry);
}

function pullStack(el, restore) {
  var kept = [];
  var gone = null;
  for (var i = 0; i < stack.length; i++) {
    if (stack[i].el === el) { gone = stack[i]; } else { kept.push(stack[i]); }
  }
  stack = kept;
  if (restore && gone && gone.returnTo && document.contains(gone.returnTo)) {
    var active = document.activeElement;
    if (!active || active === document.body || el.contains(active)) {
      try { gone.returnTo.focus({ preventScroll: true }); } catch (e) { /* not focusable */ }
    }
  }
}

function topOfStack() { return stack[stack.length - 1]; }

// The visible focusable elements inside container (an array).
function focusables(container) {
  return Array.from(container.querySelectorAll(FOCUSABLE)).filter(visible);
}

function trapTab(e, container) {
  var f = focusables(container);
  var active = document.activeElement;
  if (!f.length) {
    e.preventDefault();
    container.focus();
    return;
  }
  var first = f[0];
  var last = f[f.length - 1];
  if (!container.contains(active)) {
    e.preventDefault();
    first.focus();
  } else if (e.shiftKey && (active === first || active === container)) {
    e.preventDefault();
    last.focus();
  } else if (!e.shiftKey && active === last) {
    e.preventDefault();
    first.focus();
  }
}

document.addEventListener("keydown", function (e) {
  if (e.key === "Escape") {
    for (var i = 0; i < L.escHooks.length; i++) {
      if (L.escHooks[i]()) { return; }
    }
    var t = topOfStack();
    if (t && t.esc(e)) { e.preventDefault(); }
  } else if (e.key === "Tab") {
    var top = topOfStack();
    if (top && top.trap) { trapTab(e, top.trap); }
  }
});

// ------------------------------------------------------------------
// Declarative triggers
// ------------------------------------------------------------------

function each(sel, f) {
  var els;
  try { els = document.querySelectorAll(sel); } catch (err) {
    console.error("aihtml: bad selector " + sel);
    return;
  }
  els.forEach(f);
}

document.addEventListener("click", function (e) {
  var triggers = matching(e.target, "[data-ah-open],[data-ah-toggle],[data-ah-close]");
  for (var i = 0; i < triggers.length && !e.cancelBubble; i++) {
    var trigger = triggers[i];
    if (trigger.tagName === "A") { e.preventDefault(); }
    var sel;
    if ((sel = trigger.getAttribute("data-ah-open"))) {
      each(sel, function (t) { AH.invoke(t, "open", { invoker: trigger }); });
    }
    if ((sel = trigger.getAttribute("data-ah-toggle"))) {
      each(sel, function (t) { AH.invoke(t, "toggle", { invoker: trigger }); });
    }
    if (trigger.hasAttribute("data-ah-close")) {
      sel = trigger.getAttribute("data-ah-close");
      var result = trigger.getAttribute("data-ah-result");
      if (sel) {
        each(sel, function (t) { AH.invoke(t, "close", result); });
      } else {
        var t = trigger.closest(OVERLAYS);
        if (t) { AH.invoke(t, "close", result); }
      }
    }
  }
});

// Enter / Space on non-button close controls (popover title bar).
document.addEventListener("keydown", function (e) {
  if (e.key !== "Enter" && e.key !== " ") { return; }
  var c = e.target && e.target.closest ? e.target.closest("[data-ah-close][role='button']") : null;
  if (c) {
    e.preventDefault();
    c.click();
  }
});

// ------------------------------------------------------------------
// Positioning: AH.float (core.js) places the bubble with position:
// fixed, flips it and follows scrolling; the side it used lands in
// data-ah-placement, which is mirrored into sigil's arrow class
// (<prefix><side>) whenever it changes. sideClasses: the classes to
// remove, space separated.
// ------------------------------------------------------------------

function floatWithArrow(el, anchor, side, offset, prefix, sideClasses) {
  var remove = sideClasses.split(/\s+/).filter(Boolean);
  function sync() {
    var used = el.getAttribute("data-ah-placement");
    if (used) {
      el.classList.remove.apply(el.classList, remove);
      el.classList.add(prefix + used);
    }
  }
  var h = AH.float(el, anchor, { placement: side, align: "center", offset: offset });
  sync();
  var mo = window.MutationObserver ? new MutationObserver(sync) : null;
  if (mo) { mo.observe(el, { attributes: true, attributeFilter: ["data-ah-placement"] }); }
  return {
    update: function () { h.update(); },
    stop: function () {
      if (mo) { mo.disconnect(); }
      h.stop();
    }
  };
}

// ------------------------------------------------------------------
// Drawer and sheet: the root is the scrim (…__overlay), CSS animates
// data-state; the drawer adds sigil's swipe-to-dismiss gesture.
// ------------------------------------------------------------------

var DRAG_AXIS = {
  right: { prop: "translateX", sign: 1, dim: "w" },
  left: { prop: "translateX", sign: -1, dim: "w" },
  bottom: { prop: "translateY", sign: 1, dim: "h" },
  top: { prop: "translateY", sign: -1, dim: "h" }
};

function defineSlide(name) {
  var P = "ah-" + name;

  AH.register(name, class extends AH.Controller {
    setup() {
      var el = this.element;
      this.listen(el, "mousedown", (e) => {
        if (e.target === el && flag(el, "scrim", true)) { this.close(); }
      });
      if (name === "drawer") { this.addDrag(); }
      if (el.getAttribute("data-ah-initial") === "open") { this.open(); }
    }

    teardown() {
      if (this.isOpen()) {
        unlock();
        pullStack(this.element, false);
      }
    }

    // methods (aihtml_action:call/4, AH.invoke)
    open() {
      var el = this.element;
      if (this.isOpen()) { return; }
      if (!this.fire("ah:opening")) { return; }
      el.style.zIndex = nextZ();
      void el.offsetHeight;             // first open: let the transition run
      this.setState("open");
      lock();
      var p = this.panel();
      pushStack({
        el: el, trap: p,
        esc: () => {
          if (!flag(el, "esc", true)) { return false; }
          this.close();
          return true;
        }
      });
      if (p) { p.focus({ preventScroll: true }); }
      this.fire("ah:open");
    }

    close(result) {
      if (!this.isOpen()) { return; }
      if (!this.fire("ah:closing")) { return; }
      var p = this.panel();
      if (p) {
        p.style.transform = "";
        p.setAttribute("data-dragging", "false");
      }
      this.setState("closed");
      unlock();
      pullStack(this.element, true);
      this.fire("ah:close", { result: result || null });
    }

    toggle() { if (this.isOpen()) { this.close(); } else { this.open(); } }

    isOpen() { return this.element.getAttribute("data-state") === "open"; }

    panel() { return this.element.querySelector("." + P + "__panel"); }

    setState(s) {
      this.element.setAttribute("data-state", s);
      var p = this.panel();
      if (p) { p.setAttribute("data-state", s); }
    }

    addDrag() {
      var el = this.element;
      var st = null;
      this.listen(el, "pointerdown", (e) => {
        var p = this.panel();
        if (!p || !flag(el, "dismissible", true) || !p.contains(e.target) ||
            e.target.closest("button, a, input, textarea, select, ." + P + "__body")) {
          return;
        }
        var side = p.getAttribute("data-side") || "bottom";
        var r = p.getBoundingClientRect();
        st = { side: side, size: DRAG_AXIS[side].dim === "w" ? r.width : r.height,
               x0: e.clientX, y0: e.clientY, t0: e.timeStamp, d: 0 };
        p.setAttribute("data-dragging", "true");
        try { p.setPointerCapture(e.pointerId); } catch (err) { /* no capture */ }
      });
      this.listen(el, "pointermove", (e) => {
        if (!st) { return; }
        var ax = DRAG_AXIS[st.side];
        var raw = ax.dim === "w" ? e.clientX - st.x0 : e.clientY - st.y0;
        st.d = Math.max(0, ax.sign * raw);   // only towards closing
        this.panel().style.transform = ax.prop + "(" + (ax.sign * st.d) + "px)";
      });
      var up = (e) => {
        if (!st) { return; }
        var s = st;
        st = null;
        var p = this.panel();
        var dt = Math.max(1, e.timeStamp - s.t0);
        p.setAttribute("data-dragging", "false");
        // past 30% of the panel, or a flick faster than 0.5px/ms that
        // also moved 50px (a short tap must not count as a flick)
        if (s.d > s.size * 0.3 || (s.d / dt > 0.5 && s.d > 50)) {
          this.close();
        } else {
          p.style.transform = "";
        }
      };
      this.listen(el, "pointerup", up);
      this.listen(el, "pointercancel", up);
    }
  });
}

// ------------------------------------------------------------------
// Notification cards, toast (sigil overlay/notification + toast)
// ------------------------------------------------------------------

// Card markup comes from templates/notification.mustache (toast content
// from templates/toast.mustache), the same templates aihtml_lib_overlay
// renders on the server. Only the corner container is built here.
var VARIANTS = { info: 1, success: 1, warning: 1, error: 1 };
var CORNERS = { "top-right": 1, "top-left": 1, "bottom-right": 1, "bottom-left": 1 };

// The corner container (an element), created on first use.
function corner(pos) {
  pos = CORNERS[pos] ? pos : "top-right";
  var c = document.querySelector("body > .ah-notify-container.ah-notify-" + pos);
  if (!c) {
    c = document.createElement("div");
    c.className = "ah-notify-container ah-notify-" + pos;
    document.body.appendChild(c);
  }
  return c;
}

// The view for templates/notification.mustache; aihtml_lib_overlay:card/2
// builds the same one. contentHtml must be trusted HTML.
function cardView(o, contentHtml) {
  var v = VARIANTS[o.variant] ? o.variant : "info";
  var w = o.width;
  return {
    variant: v, info: v === "info", success: v === "success",
    warning: v === "warning", error: v === "error",
    clickable: o.closeOnClick !== false && o.closeOnClick !== "false",
    closable: o.closable !== false && o.closable !== "false",
    width: w === undefined || w === null || w === "" ? null
      : (typeof w === "number" ? w + "px" : String(w)),
    content: contentHtml
  };
}

function duration(v, dflt) {
  return v === undefined || v === null || v === "" ? dflt : Number(v);
}

// HTML-escape text (for content put into a card).
function escapeHtml(s) {
  return String(s).replace(/[&<>"']/g, function (c) {
    return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c];
  });
}

// card element -> its close function
var closers = new WeakMap();

// Close a card shown by showCard (fades it out).
function closeCard(card) {
  var f = closers.get(card);
  if (f) { f(); }
}

// Put a card (trusted HTML string or element) in its corner and run it.
// o: {position, duration (ms, <= 0 stays), source}. Whether a click on
// the card closes it is read from the markup (.ah-notify-clickable).
// Events go to o.source (a notification template) or the card. Returns
// the card element.
function showCard(card, o) {
  if (typeof card === "string") {
    var t = document.createElement("template");
    t.innerHTML = card;
    card = Array.from(t.content.children).filter(function (n) {
      return n.classList.contains("ah-notify");
    })[0];
    if (!card) { return undefined; }
  }
  var pos = CORNERS[o.position] ? o.position : "top-right";
  var c = corner(pos);
  if (pos.indexOf("bottom") === 0) { c.insertBefore(card, c.firstChild); } else { c.appendChild(card); }
  var target = o.source || card;
  var timer = null;
  var closed = false;
  function close() {
    if (closed) { return; }
    closed = true;
    clearTimeout(timer);
    closers.delete(card);
    fadeOut(card, 300, function () {
      card.remove();
      if (!c.children.length) { c.remove(); }
      fire(target, "ah:close", { result: null });
    });
  }
  function arm() {
    if (o.duration > 0) { timer = setTimeout(close, o.duration); }
  }
  closers.set(card, close);
  card.addEventListener("click", function (e) {
    if (e.target.closest && e.target.closest(".ah-notify-close")) {
      e.stopPropagation();
      close();
      return;
    }
    if (card.classList.contains("ah-notify-clickable")) {
      fire(target, "ah:click");
      close();
    }
  });
  card.addEventListener("keydown", function (e) {
    if ((e.key === "Enter" || e.key === " ") &&
        e.target.closest && e.target.closest(".ah-notify-close")) {
      e.preventDefault();
      close();
    }
  });
  // hovering keeps the card (sigil lets it expire under the pointer)
  card.addEventListener("mouseenter", function () { clearTimeout(timer); });
  card.addEventListener("mouseleave", function () { if (!closed) { arm(); } });
  arm();
  card.style.display = "flex";
  card.style.opacity = "0";
  fade(card, 0.95, 300, function () {
    card.style.opacity = "";
    fire(target, "ah:open");
  });
  return card;
}

L.FOCUSABLE = FOCUSABLE;
L.OVERLAYS = OVERLAYS;
L.uid = uid;
L.flag = flag;
L.num = num;
L.fire = fire;
L.matching = matching;
L.fade = fade;
L.fadeIn = fadeIn;
L.fadeOut = fadeOut;
L.stopFade = stopFade;
L.nextZ = nextZ;
L.lock = lock;
L.unlock = unlock;
L.pushStack = pushStack;
L.pullStack = pullStack;
L.topOfStack = topOfStack;
L.focusables = focusables;
L.trapTab = trapTab;
L.floatWithArrow = floatWithArrow;
L.defineSlide = defineSlide;
L.corner = corner;
L.cardView = cardView;
L.duration = duration;
L.escapeHtml = escapeHtml;
L.showCard = showCard;
L.closeCard = closeCard;
