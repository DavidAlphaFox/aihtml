/* dropdown-button: a menu popup (outside click / Escape close, arrow
 * keys); choosing an item sets data-ah-value and fires change
 * (designs/04-components.md). The menu code is in _lib_button.js. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.button;

  var DROPDOWN = {
    trigger: ".ah-dropdown-btn-wrapper",
    menu: ".ah-dropdown-btn-popup",
    item: ".ah-dropdown-btn-item",
    disabled: function ($el) { return $el.hasClass("ah-dropdown-btn-disabled"); },
    isOpen: function ($el) { return $el.hasClass("ah-dropdown-btn-opened"); },
    show: function ($el, on) {
      var $popup = $el.children(".ah-dropdown-btn-popup");
      $el.toggleClass("ah-dropdown-btn-opened", on);
      if (on) {
        $popup.removeAttr("hidden");
        L.floatMenu($el[0], $popup[0], { placement: "bottom", align: "start", matchWidth: true });
      } else {
        $popup.attr("hidden", "");
        L.unfloat($el[0], 0);
      }
    },
    select: function ($el, $item) {
      $el.find(".ah-dropdown-btn-item").removeClass("selected");
      $item.addClass("selected");
    }
  };

  AH.define("dropdown-button", {
    init: function (el, $el) {
      L.menuInit(el, $el, DROPDOWN);
      var $trigger = $el.children(".ah-dropdown-btn-wrapper");
      $el.on("mouseenter" + NS, function () {
        if (DROPDOWN.disabled($el)) { return; }
        $el.addClass("ah-dropdown-btn-hover");
        if ($el.hasClass("ah-dropdown-btn-auto-open")) { L.menuOpen($el, DROPDOWN, false); }
      });
      $el.on("mouseleave" + NS, function () {
        $el.removeClass("ah-dropdown-btn-hover");
        if ($el.hasClass("ah-dropdown-btn-auto-open")) { L.menuClose($el, DROPDOWN, false); }
      });
      $trigger.on("focus" + NS, function () {
        // sigil shows the focus ring on focus; keep it to keyboard focus
        var visible = true;
        try { visible = $trigger[0].matches(":focus-visible"); } catch (err) { /* old browser */ }
        if (visible) { $el.addClass("ah-dropdown-btn-focused"); }
      });
      $trigger.on("blur" + NS, function () { $el.removeClass("ah-dropdown-btn-focused"); });
    },
    destroy: function (el, $el) {
      L.menuDestroy(el);
      $el.children(".ah-dropdown-btn-wrapper").off(NS);
    },
    methods: {
      open: function (el, $el) { L.menuOpen($el, DROPDOWN, false); },
      close: function (el, $el) { L.menuClose($el, DROPDOWN, false); },
      toggle: function (el, $el) {
        if (DROPDOWN.isOpen($el)) { L.menuClose($el, DROPDOWN, false); } else { L.menuOpen($el, DROPDOWN, false); }
      },
      setValue: function (el, $el, v) {
        var $item = $el.find(".ah-dropdown-btn-item").filter(function () {
          return this.getAttribute("data-value") === String(v);
        });
        $el.find(".ah-dropdown-btn-item").removeClass("selected");
        $item.addClass("selected");
        L.setValue($el, v, false);
      },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); }
    }
  });
})(window.jQuery, window.AH);
