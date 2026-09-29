/* Behaviour of the responsive_panel component (designs/04-components.md),
 * ported from sigil's responsive-panel (cljs + jQuery). The only listener
 * on document is the click-outside one, which destroy removes.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var num = AH.lib.scroll.num;
  var seq = 0;

  // ------------------------------------------------------------------
  // Responsive panel (sigil layout/responsive_panel)
  // ------------------------------------------------------------------
  //
  // Folded when the parent is at most data-breakpoint px wide: the content
  // then floats below the toggle (AH.float) while open.

  var RP = "ah-responsive-panel";

  function rpState(el) { return $.data(el, "ahRpanel"); }
  function rpToggle($el) { return $el.children("." + RP + "-toggle"); }
  function rpContent($el) { return $el.children("." + RP + "-content"); }
  function rpDisabled($el) { return $el.hasClass(RP + "-disabled"); }

  function rpLoad($el) {
    var st = rpState($el[0]), $c = rpContent($el);
    if (st.loaded) { return; }
    st.loaded = true;
    if (/(^|\s)ah:load:/.test($c.attr("data-ah-on") || "")) { $c.trigger("ah:load"); }
  }

  function rpSpeed(el, name) { return num(el, name, 200); }

  function rpClearStyles($c) {
    $c.stop(true, true).css({ display: "", opacity: "", width: "" });
  }

  function rpOpen(el, $el) {
    var st = rpState(el);
    if (!st.collapsed || st.open || rpDisabled($el)) { return; }
    var $c = rpContent($el), $t = rpToggle($el);
    var anim = el.getAttribute("data-animation") || "fade", speed = rpSpeed(el, "data-show-duration");
    var cw = el.getAttribute("data-collapse-width");
    rpClearStyles($c);
    if (cw) { $c.css("width", /^\d+(\.\d+)?$/.test(cw) ? cw + "px" : cw); }
    st.open = true;
    $el.addClass(RP + "-open");
    $t.attr("aria-expanded", "true");
    st.float = AH.float($c[0], $t[0], { placement: "bottom", align: "start", offset: 4 });
    var shown = function () {
      if (st.float) { st.float.update(); }
      $el.trigger("ah:open");
    };
    if (anim === "fade") {
      $c.css("opacity", 0).animate({ opacity: 1 }, speed, shown);
    } else if (anim === "slide") {
      $c.hide().slideDown(speed, shown);
    } else {
      shown();
    }
    rpLoad($el);
  }

  function rpClose(el, $el, instant) {
    var st = rpState(el);
    if (!st.open) { return; }
    var $c = rpContent($el), $t = rpToggle($el);
    var anim = instant ? "none" : (el.getAttribute("data-animation") || "fade");
    var speed = rpSpeed(el, "data-hide-duration");
    st.open = false;
    $t.attr("aria-expanded", "false");
    if ($.contains($c[0], document.activeElement)) { $t[0].focus(); }
    var hidden = function () {
      $el.removeClass(RP + "-open");
      if (st.float) { st.float.stop(); st.float = null; }
      $c.css({ display: "", opacity: "" });
      if (!instant) { $el.trigger("ah:close"); }
    };
    $c.stop(true, true);
    if (anim === "fade") { $c.fadeOut(speed, hidden); }
    else if (anim === "slide") { $c.slideUp(speed, hidden); }
    else { hidden(); }
  }

  function rpCheck(el, $el) {
    var st = rpState(el);
    var bp = num(el, "data-breakpoint", 1000);
    var pw = $el.parent().width();
    if (!st.collapsed && pw <= bp) {
      if (st.open) { rpClose(el, $el, true); }
      st.collapsed = true;
      $el.addClass(RP + "-collapsed");
      $el.trigger("ah:collapse");
    } else if (st.collapsed && pw > bp) {
      rpClose(el, $el, true);
      st.collapsed = false;
      $el.removeClass(RP + "-collapsed " + RP + "-open");
      rpClearStyles(rpContent($el));
      $el.trigger("ah:expand");
      rpLoad($el);
    }
  }

  function rpFlip(el, $el) {
    if (rpState(el).open) { rpClose(el, $el); } else { rpOpen(el, $el); }
  }

  AH.define("responsive-panel", {
    init: function (el, $el) {
      var st = { collapsed: false, open: false, loaded: false, float: null,
                 ns: ".ahrp" + (++seq), ro: null, $ext: $() };
      $.data(el, "ahRpanel", st);
      var $t = rpToggle($el);
      $t.on("click" + NS, function () {
        if (!rpDisabled($el)) { rpFlip(el, $el); }
      });
      $t.on("keydown" + NS, function (e) {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          if (!rpDisabled($el)) { rpFlip(el, $el); }
        }
      });
      $el.on("keydown" + NS, function (e) {
        if (e.key === "Escape" && st.open) {
          e.stopPropagation();
          rpClose(el, $el);
          $t[0].focus();
        }
      });
      var sel = el.getAttribute("data-toggle-button");
      if (sel) {
        st.$ext = $(sel).on("click" + st.ns, function () {
          if (!rpDisabled($el)) { rpFlip(el, $el); }
        });
      }
      $(document).on("click" + st.ns, function (e) {
        if (st.open && el.getAttribute("data-auto-close") !== "false" &&
            !$.contains(el, e.target) && e.target !== el &&
            !st.$ext.filter(function () { return this === e.target || $.contains(this, e.target); }).length) {
          rpClose(el, $el);
        }
      });
      var check = function () { rpCheck(el, $el); };
      if (window.ResizeObserver && el.parentNode) {
        st.ro = new ResizeObserver(check);
        st.ro.observe(el.parentNode);
      }
      $(window).on("resize" + st.ns, check);
      check();
      if (!st.collapsed) { rpLoad($el); }
    },
    destroy: function (el, $el) {
      var st = rpState(el);
      if (!st) { return; }
      if (st.float) { st.float.stop(); }
      if (st.ro) { st.ro.disconnect(); }
      rpContent($el).stop(true, true);
      $(document).off(st.ns);
      $(window).off(st.ns);
      st.$ext.off(st.ns);
      $.removeData(el, "ahRpanel");
    },
    methods: {
      open: function (el, $el) { rpOpen(el, $el); },
      close: function (el, $el) { rpClose(el, $el); },
      toggle: function (el, $el) { if (rpState(el).collapsed) { rpFlip(el, $el); } },
      refresh: function (el, $el) { rpCheck(el, $el); },
      isCollapsed: function (el) { return !!rpState(el).collapsed; },
      isOpen: function (el) { return !!rpState(el).open; }
    }
  });
})(window.jQuery, window.AH);
