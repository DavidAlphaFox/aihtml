/* The popover behaviour (designs/04-components.md), ported from sigil's
 * overlay/popover: a bubble anchored to the element that opened it or to
 * its data-ah-anchor selector. Shared machinery: _lib_overlay.js. */
(function ($, AH) {
  "use strict";

  var L = AH.lib.overlay;
  var GNS = L.GNS,
      uid = L.uid,
      flag = L.flag,
      nextZ = L.nextZ,
      pushStack = L.pushStack,
      pullStack = L.pullStack,
      floatWithArrow = L.floatWithArrow;

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
})(window.jQuery, window.AH);
