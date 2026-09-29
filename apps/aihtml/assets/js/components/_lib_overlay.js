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
 *   - the drawer / sheet behaviour (defineSlide)
 *   - notification cards in a screen corner (toast, notification)
 *
 * Events on the component root: ah:open, ah:close [{result}], and for
 * window ah:collapse, ah:expand, ah:moved, ah:resize; notification cards
 * fire ah:click. */
(function ($, AH) {
  "use strict";

  AH.lib = AH.lib || {};
  // escHooks: functions run on Escape before the stack; one returning
  // true handled it
  var L = AH.lib.overlay = { escHooks: [] };

  var NS = AH.NS;
  var GNS = ".ah-overlay";          // document-level listeners of the overlay behaviours
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

  // ------------------------------------------------------------------
  // z-index, scroll lock, stack of open overlays
  // ------------------------------------------------------------------

  // Above sigil's fixed drawer (20600) and sheet (20500) layers, below the
  // notification corner (99999).
  var z = 21000;
  function nextZ() { return ++z; }

  var locks = 0;
  function lock() {
    if (locks++ === 0) { $(document.body).addClass("ah-scroll-locked"); }
  }
  function unlock() {
    locks = Math.max(0, locks - 1);
    if (locks === 0) { $(document.body).removeClass("ah-scroll-locked"); }
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
      if (!active || active === document.body || $.contains(el, active) || active === el) {
        try { gone.returnTo.focus({ preventScroll: true }); } catch (e) { /* not focusable */ }
      }
    }
  }

  function topOfStack() { return stack[stack.length - 1]; }

  function focusables(container) {
    return $(container).find(FOCUSABLE).filter(function () {
      return this.offsetWidth || this.offsetHeight || this.getClientRects().length;
    });
  }

  function trapTab(e, container) {
    var $f = focusables(container);
    var active = document.activeElement;
    if (!$f.length) {
      e.preventDefault();
      container.focus();
      return;
    }
    var first = $f[0];
    var last = $f[$f.length - 1];
    if (!$.contains(container, active) && active !== container) {
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

  $(document).on("keydown" + GNS, function (e) {
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

  $(document).on("click" + GNS, "[data-ah-open],[data-ah-toggle],[data-ah-close]", function (e) {
    var trigger = this;
    if (trigger.tagName === "A") { e.preventDefault(); }
    var sel;
    if ((sel = trigger.getAttribute("data-ah-open"))) {
      AH.invoke($(sel), "open", { invoker: trigger });
    }
    if ((sel = trigger.getAttribute("data-ah-toggle"))) {
      AH.invoke($(sel), "toggle", { invoker: trigger });
    }
    if (trigger.hasAttribute("data-ah-close")) {
      sel = trigger.getAttribute("data-ah-close");
      var $t = sel ? $(sel) : $(trigger).closest(OVERLAYS);
      if ($t.length) {
        AH.invoke($t, "close", trigger.getAttribute("data-ah-result"));
      }
    }
  });

  // Enter / Space on non-button close controls (popover title bar).
  $(document).on("keydown" + GNS, "[data-ah-close][role='button']", function (e) {
    if (e.key === "Enter" || e.key === " ") {
      e.preventDefault();
      $(this).trigger("click");
    }
  });

  // ------------------------------------------------------------------
  // Positioning: AH.float (core.js) places the bubble with position:
  // fixed, flips it and follows scrolling; the side it used lands in
  // data-ah-placement, which is mirrored into sigil's arrow class
  // (<prefix><side>) whenever it changes.
  // ------------------------------------------------------------------

  function floatWithArrow(el, anchor, side, offset, prefix, sideClasses) {
    function sync() {
      var used = el.getAttribute("data-ah-placement");
      if (used) { $(el).removeClass(sideClasses).addClass(prefix + used); }
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

    function panel(el) { return el.querySelector("." + P + "__panel"); }
    function isOpen(el) { return el.getAttribute("data-state") === "open"; }
    function setState(el, s) {
      el.setAttribute("data-state", s);
      var p = panel(el);
      if (p) { p.setAttribute("data-state", s); }
    }

    function open(el) {
      if (isOpen(el)) { return; }
      var ev = $.Event("ah:opening");
      $(el).trigger(ev);
      if (ev.isDefaultPrevented()) { return; }
      el.style.zIndex = nextZ();
      void el.offsetHeight;             // first open: let the transition run
      setState(el, "open");
      lock();
      var p = panel(el);
      pushStack({
        el: el, trap: p,
        esc: function () {
          if (!flag(el, "esc", true)) { return false; }
          close(el);
          return true;
        }
      });
      if (p) { p.focus({ preventScroll: true }); }
      $(el).trigger("ah:open");
    }

    function close(el, result) {
      if (!isOpen(el)) { return; }
      var ev = $.Event("ah:closing");
      $(el).trigger(ev);
      if (ev.isDefaultPrevented()) { return; }
      var p = panel(el);
      if (p) {
        p.style.transform = "";
        p.setAttribute("data-dragging", "false");
      }
      setState(el, "closed");
      unlock();
      pullStack(el, true);
      $(el).trigger("ah:close", [{ result: result || null }]);
    }

    function addDrag(el, $el) {
      var st = null;
      $el.on("pointerdown" + NS, function (e) {
        var oe = e.originalEvent;
        var p = panel(el);
        if (!p || !flag(el, "dismissible", true) || !p.contains(oe.target) ||
            $(oe.target).closest("button, a, input, textarea, select, ." + P + "__body").length) {
          return;
        }
        var side = p.getAttribute("data-side") || "bottom";
        var r = p.getBoundingClientRect();
        st = { side: side, size: DRAG_AXIS[side].dim === "w" ? r.width : r.height,
               x0: oe.clientX, y0: oe.clientY, t0: oe.timeStamp, d: 0 };
        p.setAttribute("data-dragging", "true");
        try { p.setPointerCapture(oe.pointerId); } catch (err) { /* no capture */ }
      });
      $el.on("pointermove" + NS, function (e) {
        if (!st) { return; }
        var oe = e.originalEvent;
        var ax = DRAG_AXIS[st.side];
        var raw = ax.dim === "w" ? oe.clientX - st.x0 : oe.clientY - st.y0;
        st.d = Math.max(0, ax.sign * raw);   // only towards closing
        panel(el).style.transform = ax.prop + "(" + (ax.sign * st.d) + "px)";
      });
      $el.on("pointerup" + NS + " pointercancel" + NS, function (e) {
        if (!st) { return; }
        var s = st;
        st = null;
        var p = panel(el);
        var dt = Math.max(1, e.originalEvent.timeStamp - s.t0);
        p.setAttribute("data-dragging", "false");
        // past 30% of the panel, or a flick faster than 0.5px/ms that
        // also moved 50px (a short tap must not count as a flick)
        if (s.d > s.size * 0.3 || (s.d / dt > 0.5 && s.d > 50)) {
          close(el);
        } else {
          p.style.transform = "";
        }
      });
    }

    AH.define(name, {
      init: function (el, $el) {
        $el.on("mousedown" + NS, function (e) {
          if (e.target === el && flag(el, "scrim", true)) { close(el); }
        });
        if (name === "drawer") { addDrag(el, $el); }
        if (el.getAttribute("data-ah-initial") === "open") { open(el); }
      },
      destroy: function (el) {
        if (isOpen(el)) {
          unlock();
          pullStack(el, false);
        }
      },
      methods: {
        open: function (el) { open(el); },
        close: function (el, $el, result) { close(el, result); },
        toggle: function (el) { if (isOpen(el)) { close(el); } else { open(el); } },
        isOpen: function (el) { return isOpen(el); }
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

  function corner(pos) {
    pos = CORNERS[pos] ? pos : "top-right";
    var $c = $("body > .ah-notify-container.ah-notify-" + pos);
    if (!$c.length) {
      $c = $('<div class="ah-notify-container ah-notify-' + pos + '"></div>').appendTo(document.body);
    }
    return $c;
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

  // Put a card (HTML string or element) in its corner and run it.
  // o: {position, duration (ms, <= 0 stays), source}. Whether a click on
  // the card closes it is read from the markup (.ah-notify-clickable).
  // Events go to o.source (a notification template) or the card.
  function showCard(card, o) {
    var $card = typeof card === "string" ? $($.parseHTML(card)).filter(".ah-notify") : $(card);
    var pos = CORNERS[o.position] ? o.position : "top-right";
    var $c = corner(pos);
    if (pos.indexOf("bottom") === 0) { $card.prependTo($c); } else { $card.appendTo($c); }
    var target = o.source || $card[0];
    var timer = null;
    var closed = false;
    function close() {
      if (closed) { return; }
      closed = true;
      clearTimeout(timer);
      $card.stop(true).fadeOut(300, function () {
        $card.remove();
        if (!$c.children().length) { $c.remove(); }
        $(target).trigger("ah:close", [{ result: null }]);
      });
    }
    function arm() {
      if (o.duration > 0) { timer = setTimeout(close, o.duration); }
    }
    $card.data("ahClose", close);
    $card.on("click", ".ah-notify-close", function (e) {
      e.stopPropagation();
      close();
    });
    $card.on("keydown", ".ah-notify-close", function (e) {
      if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        close();
      }
    });
    if ($card.hasClass("ah-notify-clickable")) {
      $card.on("click", function () {
        $(target).trigger("ah:click");
        close();
      });
    }
    // hovering keeps the card (sigil lets it expire under the pointer)
    $card.on("mouseenter", function () { clearTimeout(timer); })
      .on("mouseleave", function () { if (!closed) { arm(); } });
    arm();
    $card.css({ display: "flex", opacity: 0 }).animate({ opacity: 0.95 }, 300, function () {
      $card.css("opacity", "");
      $(target).trigger("ah:open");
    });
    return $card[0];
  }

  L.GNS = GNS;
  L.FOCUSABLE = FOCUSABLE;
  L.OVERLAYS = OVERLAYS;
  L.uid = uid;
  L.flag = flag;
  L.num = num;
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
  L.showCard = showCard;
})(window.jQuery, window.AH);
