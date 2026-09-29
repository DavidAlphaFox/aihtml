/* Behaviours of the overlay components (designs/04-components.md):
 * tooltip, popover, drawer, sheet, window, notification, plus the page
 * functions AH.fn("toast") and AH.fn("notify").
 *
 * Declarative triggers (aihtml_overlay:opens/toggles/closes) are one
 * delegated click listener: data-ah-open / data-ah-toggle / data-ah-close
 * hold a selector; an empty data-ah-close closes the enclosing overlay.
 *
 * Shared machinery, ported from sigil's internal/common and scroll_lock:
 *   - one z-index counter, so the overlay opened last is on top
 *   - one scroll-lock counter (body.ah-scroll-locked)
 *   - one stack of open overlays: Escape closes the top one, Tab is
 *     trapped in the top one when it is modal, focus returns to the
 *     element that had it when the overlay closes
 *
 * Events on the component root: ah:open, ah:close [{result}], and for
 * window ah:collapse, ah:expand, ah:moved, ah:resize; notification cards
 * fire ah:click. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var GNS = ".ah-overlay";          // document-level listeners of this file
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
      if (closeTips()) { return; }
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
  // Tooltip: [data-ah="tooltip"] wrappers and [data-ah-tooltip] elements
  // ------------------------------------------------------------------

  var TIP_HOSTS = '[data-ah="tooltip"],[data-ah-tooltip]';
  var TIP_POSITIONS = "ah-tooltip-top ah-tooltip-bottom ah-tooltip-left ah-tooltip-right";
  var openTips = [];
  var touchOnly = !!(window.matchMedia && window.matchMedia("(hover: none)").matches);

  function tipOpt(host, name, dflt) {
    var v = host.getAttribute("data-ah-tip-" + name);
    return v === null || v === "" ? dflt : v;
  }

  function tipState(host) {
    var st = $.data(host, "ahTip");
    if (!st) {
      st = { open: false, showTimer: null, hideTimer: null, tip: null, created: false };
      $.data(host, "ahTip", st);
    }
    return st;
  }

  function tipElement(host, st) {
    if (st.tip) { return st.tip; }
    if (host.getAttribute("data-ah") === "tooltip") {
      st.tip = $(host).children(".ah-tooltip")[0];
    } else {
      var $tip = $('<span class="ah-tooltip" role="tooltip">' +
                   '<span class="ah-tooltip-arrow" aria-hidden="true"></span>' +
                   '<span class="ah-tooltip-content"></span></span>');
      $tip.find(".ah-tooltip-content").text(host.getAttribute("data-ah-tooltip"));
      if (tipOpt(host, "arrow", "true") === "false") { $tip.addClass("ah-tooltip-no-arrow"); }
      $tip.appendTo(document.body);
      st.tip = $tip[0];
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
      $(tip).addClass("ah-tooltip-no-arrow");
      var x = e && e.clientX !== undefined ? e.clientX : host.getBoundingClientRect().left;
      var y = e && e.clientY !== undefined ? e.clientY : host.getBoundingClientRect().bottom;
      tip.style.left = (x + 10) + "px";
      tip.style.top = (y + 10) + "px";
      return;
    }
    // the tooltip's arrow is 6px (tooltip.css)
    var anchor = host.getAttribute("data-ah") === "tooltip"
      ? ($(host).children().not(".ah-tooltip")[0] || host) : host;
    tipState(host).float = floatWithArrow(tip, anchor, pos, 6, "ah-tooltip-", TIP_POSITIONS);
  }

  function unfloatTip(st) {
    if (st.float) {
      st.float.stop();
      st.float = null;
    }
  }

  function openTip(host, e) {
    var st = tipState(host);
    if (st.open || tipOpt(host, "disabled", "false") === "true") { return; }
    var ev = $.Event("ah:opening");
    $(host).trigger(ev);
    if (ev.isDefaultPrevented()) { return; }
    var tip = tipElement(host, st);
    if (!tip) { return; }
    $(tip).stop(true).css({ display: "block", visibility: "hidden", opacity: 0 });
    positionTip(host, tip, e);
    $(tip).css({ visibility: "visible" }).animate({ opacity: 0.9 }, 200);
    host.setAttribute("aria-describedby", tip.id);
    st.open = true;
    openTips.push(host);
    $(host).trigger("ah:open");
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
        $(tip).remove();
        st.tip = null;
      }
    };
    if (now) {
      $(tip).stop(true);
      done();
    } else {
      $(tip).stop(true).animate({ opacity: 0 }, "fast", done);
    }
    $(host).trigger("ah:close", [{ result: null }]);
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

  $(document)
    .on("mouseenter" + GNS, TIP_HOSTS, function (e) {
      var host = this;
      if (tipTrigger(host) !== "hover") { return; }
      var st = tipState(host);
      clearTimeout(st.showTimer);
      var ev = { clientX: e.clientX, clientY: e.clientY };
      st.showTimer = setTimeout(function () {
        if (document.contains(host)) { openTip(host, ev); }
      }, parseInt(tipOpt(host, "delay", "100"), 10));
    })
    .on("mouseleave" + GNS, TIP_HOSTS, function () {
      if (tipTrigger(this) === "hover" && !$.contains(this, document.activeElement)) {
        closeTip(this);
      }
    })
    .on("mousemove" + GNS, TIP_HOSTS, function (e) {
      var st = $.data(this, "ahTip");
      if (st && st.open && st.tip && tipOpt(this, "position", "") === "mouse") {
        st.tip.style.left = (e.clientX + 10) + "px";
        st.tip.style.top = (e.clientY + 10) + "px";
      }
    })
    .on("focusin" + GNS, TIP_HOSTS, function () {
      if (tipTrigger(this) === "hover") { openTip(this); }
    })
    .on("focusout" + GNS, TIP_HOSTS, function (e) {
      if (tipTrigger(this) === "hover" && !$.contains(this, e.relatedTarget)) {
        closeTip(this);
      }
    })
    .on("click" + GNS, TIP_HOSTS, function (e) {
      if (tipTrigger(this) !== "click") { return; }
      if ($(e.target).closest(TIP_HOSTS)[0] !== this) { return; }
      if (tipState(this).open) { closeTip(this); } else { openTip(this, e); }
    })
    .on("click" + GNS, function (e) {
      // click-triggered tooltips close on a click elsewhere
      openTips.slice().forEach(function (host) {
        if (tipTrigger(host) === "click" && host !== e.target && !$.contains(host, e.target)) {
          closeTip(host);
        }
      });
    });

  AH.define("tooltip", {
    init: function () { /* delegated listeners above do the work */ },
    destroy: function (el) {
      closeTip(el, true);
      $.removeData(el, "ahTip");
    },
    methods: {
      open: function (el) { openTip(el); },
      close: function (el) { closeTip(el); },
      toggle: function (el) {
        if (tipState(el).open) { closeTip(el); } else { openTip(el); }
      },
      setContent: function (el, $el, text) {
        $el.find(".ah-tooltip-content").text(text);
      }
    }
  });

  // ------------------------------------------------------------------
  // Popover
  // ------------------------------------------------------------------

  var POP_POSITIONS = "ah-popover-top ah-popover-bottom ah-popover-left ah-popover-right";

  function popOpen(el) { return el.getAttribute("data-state") === "open"; }

  function popUnfloat(el) {
    var f = $.data(el, "ahFloat");
    if (f) {
      f.stop();
      $.removeData(el, "ahFloat");
    }
  }

  function popoverOpen(el, opts) {
    if (popOpen(el)) { return; }
    var anchor = (opts && opts.invoker) || $(el.getAttribute("data-ah-anchor") || null)[0];
    if (!anchor) {
      console.error("aihtml: popover has no anchor", el);
      return;
    }
    var ev = $.Event("ah:opening");
    $(el).trigger(ev);
    if (ev.isDefaultPrevented()) { return; }
    $.data(el, "ahAnchor", anchor);
    var zi = nextZ();
    var $el = $(el).stop(true, true);
    popUnfloat(el);
    $el.css({ zIndex: zi, display: "block", visibility: "hidden", opacity: 0 });
    $.data(el, "ahFloat", floatWithArrow(el, anchor, el.getAttribute("data-ah-position") || "bottom",
                                         8, "ah-popover-", POP_POSITIONS));
    if (flag(el, "modal", false)) {
      var $bd = $('<div class="ah-popover-modal-backdrop"></div>').css("z-index", zi - 1);
      $bd.insertBefore(el);
      $.data(el, "ahBackdrop", $bd[0]);
    }
    $el.css({ visibility: "visible" }).animate({ opacity: 1 }, "fast");
    el.setAttribute("data-state", "open");
    anchor.setAttribute("aria-expanded", "true");
    pushStack({ el: el, trap: null, esc: function () { popoverClose(el); return true; } });
    $el.trigger("ah:open");
  }

  function popoverClose(el, result) {
    if (!popOpen(el)) { return; }
    var anchor = $.data(el, "ahAnchor");
    el.setAttribute("data-state", "closed");
    if (anchor) { anchor.setAttribute("aria-expanded", "false"); }
    var bd = $.data(el, "ahBackdrop");
    if (bd) {
      $(bd).remove();
      $.removeData(el, "ahBackdrop");
    }
    var focusInside = $.contains(el, document.activeElement);
    pullStack(el, false);
    if (focusInside && anchor) { anchor.focus(); }
    $(el).stop(true).fadeOut("fast", function () { popUnfloat(el); });
    $(el).trigger("ah:close", [{ result: result || null }]);
  }

  AH.define("popover", {
    init: function (el) {
      var id = uid("pop");
      $.data(el, "ahNs", id);
      var anchorSel = el.getAttribute("data-ah-anchor");
      if (anchorSel) {
        // sigil's `selector' prop: the anchor toggles the popover
        $(document).on("click" + GNS + id, anchorSel, function (e) {
          if ($(this).is("[data-ah-open],[data-ah-toggle]")) { return; }
          e.preventDefault();
          AH.invoke(el, "toggle", { invoker: this });
        });
      }
      $(document).on("click" + GNS + id, function (e) {
        if (!popOpen(el) || !flag(el, "auto-close", true) || flag(el, "modal", false)) { return; }
        var anchor = $.data(el, "ahAnchor");
        var t = e.target;
        if (t === el || $.contains(el, t) || (anchor && (t === anchor || $.contains(anchor, t)))) {
          return;
        }
        popoverClose(el);
      });
    },
    destroy: function (el) {
      var id = $.data(el, "ahNs");
      $(document).off(GNS + id);
      popUnfloat(el);
      var bd = $.data(el, "ahBackdrop");
      if (bd) { $(bd).remove(); }
      pullStack(el, false);
    },
    methods: {
      open: function (el, $el, opts) { popoverOpen(el, opts); },
      close: function (el, $el, result) { popoverClose(el, result); },
      toggle: function (el, $el, opts) {
        if (popOpen(el)) { popoverClose(el); } else { popoverOpen(el, opts); }
      },
      isOpen: function (el) { return popOpen(el); }
    }
  });

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

  defineSlide("drawer");
  defineSlide("sheet");

  // ------------------------------------------------------------------
  // Window (sigil overlay/window/*)
  // ------------------------------------------------------------------

  function winOpen(el) { return el.getAttribute("data-state") === "open"; }

  function winFront(el) {
    var zi = nextZ();
    el.style.zIndex = zi;
    var bd = $.data(el, "ahBackdrop");
    if (bd) { bd.style.zIndex = zi - 1; }
  }

  function windowOpen(el) {
    if (winOpen(el)) { return; }
    var ev = $.Event("ah:opening");
    $(el).trigger(ev);
    if (ev.isDefaultPrevented()) { return; }
    var modal = flag(el, "modal", false);
    var $el = $(el).stop(true, true);
    if (!el.hasAttribute("data-ah-placed")) {
      // centre in the viewport on first open (sigil: position :center)
      el.style.visibility = "hidden";
      el.style.display = "flex";
      var w = el.offsetWidth;
      var h = el.offsetHeight;
      el.style.left = Math.max(0, (window.innerWidth - w) / 2) + "px";
      el.style.top = Math.max(0, (window.innerHeight - h) / 2) + "px";
      el.style.display = "none";
      el.style.visibility = "";
      el.setAttribute("data-ah-placed", "");
    }
    if (modal) {
      var $bd = $('<div class="ah-window-modal-backdrop"></div>');
      $bd.insertBefore(el).hide().fadeIn(250);
      $bd.on("mousedown" + NS, function () {
        if (flag(el, "scrim", false)) { windowClose(el); }
      });
      $.data(el, "ahBackdrop", $bd[0]);
      lock();
    }
    winFront(el);
    el.setAttribute("data-state", "open");
    pushStack({
      el: el, trap: modal ? el : null,
      esc: function () {
        if (!flag(el, "esc", true)) { return false; }
        var a = document.activeElement;
        if (!modal && a !== el && !$.contains(el, a)) { return false; }
        windowClose(el);
        return true;
      }
    });
    $el.fadeIn(250);
    el.focus({ preventScroll: true });
    $el.trigger("ah:open");
  }

  function windowClose(el, result) {
    if (!winOpen(el)) { return; }
    var ev = $.Event("ah:closing");
    $(el).trigger(ev, [{ result: result || null }]);
    if (ev.isDefaultPrevented()) { return; }
    el.setAttribute("data-state", "closed");
    var bd = $.data(el, "ahBackdrop");
    if (bd) {
      $.removeData(el, "ahBackdrop");
      $(bd).stop(true).fadeOut(250, function () { $(bd).remove(); });
      unlock();
    }
    pullStack(el, true);
    $(el).stop(true, true).fadeOut(250);
    $(el).trigger("ah:close", [{ result: result || null }]);
  }

  function windowCollapse(el, collapsed) {
    $(el).toggleClass("ah-window-collapsed", collapsed);
    $(el).find(".ah-window-collapse-btn").attr("aria-expanded", collapsed ? "false" : "true");
    $(el).trigger(collapsed ? "ah:collapse" : "ah:expand");
  }

  function windowMove(el, x, y) {
    el.style.left = x + "px";
    el.style.top = y + "px";
    el.setAttribute("data-ah-placed", "");
    $(el).trigger("ah:moved", [{ x: x, y: y }]);
  }

  function windowResize(el, w, h) {
    el.style.width = w + "px";
    el.style.height = h + "px";
    $(el).trigger("ah:resize", [{ width: w, height: h }]);
  }

  // One pointer drag: move(dx, dy) while the pointer moves, end() once.
  function drag(e, ns, move, end) {
    var x0 = e.clientX;
    var y0 = e.clientY;
    $(document)
      .on("pointermove" + ns, function (me) { move(me.clientX - x0, me.clientY - y0); })
      .on("pointerup" + ns + " pointercancel" + ns, function () {
        $(document).off(ns);
        end();
      });
  }

  var MIN_W = 100;
  var MIN_H = 60;

  AH.define("window", {
    init: function (el, $el) {
      var id = uid("win");
      $.data(el, "ahNs", GNS + id);
      $el.on("mousedown" + NS + " pointerdown" + NS, function () {
        if (winOpen(el)) { winFront(el); }
      });
      $el.on("click" + NS, ".ah-window-collapse-btn", function (e) {
        e.stopPropagation();
        windowCollapse(el, !$el.hasClass("ah-window-collapsed"));
      });
      // drag by the title bar
      $el.on("pointerdown" + NS, ".ah-window-header", function (e) {
        if (!flag(el, "draggable", true) || $(e.target).closest("button").length ||
            e.button !== 0) { return; }
        e.preventDefault();
        var l0 = el.offsetLeft;
        var t0 = el.offsetTop;
        drag(e, GNS + id + "-drag", function (dx, dy) {
          var x = Math.min(Math.max(0, l0 + dx), window.innerWidth - el.offsetWidth);
          var y = Math.min(Math.max(0, t0 + dy), window.innerHeight - el.offsetHeight);
          el.style.left = Math.max(0, x) + "px";
          el.style.top = Math.max(0, y) + "px";
          el.setAttribute("data-ah-placed", "");
          $el.trigger("ah:moving", [{ x: x, y: y }]);
        }, function () {
          $el.trigger("ah:moved", [{ x: el.offsetLeft, y: el.offsetTop }]);
        });
      });
      // eight resize handles
      $el.on("pointerdown" + NS, ".ah-window-resize-handle", function (e) {
        if (!$el.hasClass("ah-window-resizable") || e.button !== 0) { return; }
        e.preventDefault();
        e.stopPropagation();
        var dir = this.getAttribute("data-dir") || "";
        var w0 = el.offsetWidth;
        var h0 = el.offsetHeight;
        var l0 = el.offsetLeft;
        var t0 = el.offsetTop;
        drag(e, GNS + id + "-resize", function (dx, dy) {
          var w = w0;
          var h = h0;
          if (dir.indexOf("e") >= 0) { w = w0 + dx; }
          if (dir.indexOf("w") >= 0) { w = w0 - dx; }
          if (dir.indexOf("s") >= 0) { h = h0 + dy; }
          if (dir.indexOf("n") >= 0) { h = h0 - dy; }
          w = Math.max(MIN_W, w);
          h = Math.max(MIN_H, h);
          el.style.width = w + "px";
          el.style.height = h + "px";
          if (dir.indexOf("w") >= 0) { el.style.left = (l0 + w0 - w) + "px"; }
          if (dir.indexOf("n") >= 0) { el.style.top = (t0 + h0 - h) + "px"; }
          el.setAttribute("data-ah-placed", "");
        }, function () {
          $el.trigger("ah:resize", [{ width: el.offsetWidth, height: el.offsetHeight }]);
        });
      });
      // arrows move, Ctrl+arrows resize (only when the window itself or
      // its title bar has focus, so inputs keep their arrow keys)
      $el.on("keydown" + NS, function (e) {
        if (e.target !== el && !$(e.target).closest(".ah-window-header").length) { return; }
        var k = e.key;
        if (k !== "ArrowLeft" && k !== "ArrowRight" && k !== "ArrowUp" && k !== "ArrowDown") {
          return;
        }
        e.preventDefault();
        var dx = k === "ArrowLeft" ? -10 : k === "ArrowRight" ? 10 : 0;
        var dy = k === "ArrowUp" ? -10 : k === "ArrowDown" ? 10 : 0;
        if (e.ctrlKey) {
          windowResize(el, Math.max(MIN_W, el.offsetWidth + dx), Math.max(MIN_H, el.offsetHeight + dy));
        } else {
          windowMove(el, el.offsetLeft + dx, el.offsetTop + dy);
        }
      });
      if (el.getAttribute("data-ah-initial") === "open") { windowOpen(el); }
    },
    destroy: function (el) {
      var ns = $.data(el, "ahNs");
      $(document).off(ns + "-drag").off(ns + "-resize");
      var bd = $.data(el, "ahBackdrop");
      if (bd) {
        $(bd).remove();
        unlock();
      }
      pullStack(el, false);
    },
    methods: {
      open: function (el) { windowOpen(el); },
      close: function (el, $el, result) { windowClose(el, result); },
      toggle: function (el) { if (winOpen(el)) { windowClose(el); } else { windowOpen(el); } },
      collapse: function (el) { windowCollapse(el, true); },
      expand: function (el) { windowCollapse(el, false); },
      move: function (el, $el, x, y) { windowMove(el, x, y); },
      resize: function (el, $el, w, h) { windowResize(el, w, h); },
      bringToFront: function (el) { winFront(el); },
      isOpen: function (el) { return winOpen(el); }
    }
  });

  // ------------------------------------------------------------------
  // Notification cards, toast (sigil overlay/notification + toast)
  // ------------------------------------------------------------------

  // Card markup comes from templates/notification.mustache (toast content
  // from templates/toast.mustache), the same templates aihtml_overlay
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

  // The view for templates/notification.mustache; aihtml_overlay:card/2
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

  // AH.fn("toast", {title, description, variant, duration = 4000, position,
  // closable, closeOnClick, width}): sigil's toast/show!, for client-side
  // triggers (shows_toast/2). Text is escaped by the template.
  function toast(o) {
    o = typeof o === "string" ? { title: o } : (o || {});
    var title = o.title === undefined || o.title === null ? "" : String(o.title);
    var desc = o.description === undefined || o.description === null ? "" : String(o.description);
    var content = AH.tpl.toast({ has_title: title !== "", title: title,
                                 has_description: desc !== "", description: desc });
    return showCard(AH.tpl.notification(cardView(o, content)),
                    { position: o.position, duration: duration(o.duration, 4000) });
  }

  // AH.fn("notify", {card, position, duration = 3000}): card is the HTML
  // aihtml_overlay:toast/3 and notify/2 render on the server. Without it,
  // {text, variant, closable, closeOnClick, width} builds one here.
  function notify(o) {
    o = typeof o === "string" ? { text: o } : (o || {});
    var card = o.card || AH.tpl.notification(
      cardView(o, $("<div>").text(String(o.text || "")).html()));   // escaped text
    return showCard(card, { position: o.position, duration: duration(o.duration, 3000) });
  }

  AH.fn("toast", toast);
  AH.fn("notify", notify);
  AH.toast = toast;
  AH.notify = notify;

  $(document).on("click" + GNS, "[data-ah-toast]", function () {
    var $t = $(this);
    toast({
      title: $t.attr("data-ah-toast"),
      description: $t.attr("data-ah-toast-description"),
      variant: $t.attr("data-ah-toast-variant"),
      duration: $t.attr("data-ah-toast-duration"),
      position: $t.attr("data-ah-toast-position"),
      closable: $t.attr("data-ah-toast-closable")
    });
  });

  AH.define("notification", {
    init: function (el) { $.data(el, "ahCards", []); },
    destroy: function (el) {
      ($.data(el, "ahCards") || []).forEach(function (c) { $(c).remove(); });
      $(".ah-notify-container").each(function () {
        if (!$(this).children().length) { $(this).remove(); }
      });
    },
    methods: {
      open: function (el) {
        // the server rendered the card inside the template element
        var card = showCard($(el).children(".ah-notify").first().clone(), {
          position: el.getAttribute("data-ah-position"),
          duration: num(el, "duration", 3000),
          source: el
        });
        var cards = ($.data(el, "ahCards") || []).filter(function (c) {
          return document.contains(c);
        });
        cards.push(card);
        $.data(el, "ahCards", cards);
      },
      close: function (el) { AH.invoke(el, "closeAll"); },
      closeAll: function (el) {
        ($.data(el, "ahCards") || []).forEach(function (c) {
          var f = $(c).data("ahClose");
          if (f) { f(); }
        });
        $.data(el, "ahCards", []);
      },
      closeLast: function (el) {
        var cards = ($.data(el, "ahCards") || []).filter(function (c) {
          return document.contains(c);
        });
        var last = cards.pop();
        if (last && $(last).data("ahClose")) { $(last).data("ahClose")(); }
        $.data(el, "ahCards", cards);
      }
    }
  });
})(window.jQuery, window.AH);
