/* The tooltip behaviour (designs/04-components.md), ported from sigil's
 * overlay/tooltip: [data-ah="tooltip"] wrappers and [data-ah-tooltip]
 * elements (tooltip_attrs/2), driven by delegated document listeners.
 * Escape closes open tooltips before any other overlay (an escHook of
 * _lib_overlay.js). */
(function ($, AH) {
  "use strict";

  var L = AH.lib.overlay;
  var GNS = L.GNS,
      uid = L.uid,
      floatWithArrow = L.floatWithArrow;

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

  L.escHooks.push(closeTips);
})(window.jQuery, window.AH);
