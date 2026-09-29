/* Behaviour of tabs (value: the active key; fires change). */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.layout;

  function tabItems($el) {
    return $el.children(".ah-tabs-header").children(".ah-tabs-item");
  }

  function tabPanels($el) {
    return $el.children(".ah-tabs-content").children(".ah-tabs-panel");
  }

  function tabIndexOf($el, k) {
    var idx = -1;
    tabItems($el).each(function (i) {
      if (this.getAttribute("data-key") === String(k)) { idx = i; }
    });
    return idx;
  }

  function tabSelect(el, $el, idx, user) {
    var $items = tabItems($el);
    var $panels = tabPanels($el);
    var cur = $items.index($items.filter(".ah-tabs-item-selected"));
    if (idx < 0 || idx >= $items.length || idx === cur ||
        $items.eq(idx).hasClass("ah-tabs-item-disabled")) {
      return false;
    }
    $items.removeClass("ah-tabs-item-selected").attr({ "aria-selected": "false", tabindex: "-1" });
    $items.eq(idx).addClass("ah-tabs-item-selected").attr({ "aria-selected": "true", tabindex: "0" });
    $panels.attr("aria-hidden", "true");
    var $new = $panels.eq(idx).attr("aria-hidden", "false");
    var $old = cur >= 0 ? $panels.eq(cur) : $panels.not($new);
    $panels.stop(true, true);
    if ((el.getAttribute("data-animation") || "fade") === "fade" && $old.length) {
      $old.fadeOut(100, function () {
        $old.removeClass("ah-tabs-panel-active");
        $new.hide().fadeIn(100, function () { $new.addClass("ah-tabs-panel-active"); });
      });
    } else {
      $old.hide().removeClass("ah-tabs-panel-active");
      $new.css("display", "").addClass("ah-tabs-panel-active");
    }
    L.setValue(el, $el, $items.eq(idx).attr("data-key"));
    if (user) {
      $el.trigger("change");
    }
    return true;
  }

  AH.define("tabs", {
    init: function (el, $el) {
      var $header = $el.children(".ah-tabs-header");
      var ev = el.getAttribute("data-selection-mode") === "hover" ? "mouseenter" : "click";
      $header.on(ev + NS, ".ah-tabs-item", function () {
        if (!$el.hasClass("ah-tabs-disabled")) {
          tabSelect(el, $el, tabItems($el).index(this), true);
        }
      });
      $header.on("keydown" + NS, ".ah-tabs-item", function (e) {
        var $items = tabItems($el);
        var vertical = $el.hasClass("ah-tabs-left") || $el.hasClass("ah-tabs-right");
        var t = L.listKeys(e, $items, $items.index(this), vertical, "ah-tabs-item-disabled");
        if (t === null) { return; }
        e.preventDefault();
        tabSelect(el, $el, t, true);
        $items.eq(t).trigger("focus");
      });
      $header.on("click" + NS, ".ah-tabs-scroll-btn", function () {
        var step = $(this).hasClass("ah-tabs-scroll-left") ? -80 : 80;
        $header.scrollLeft($header.scrollLeft() + step);
      });
    },
    destroy: function (el, $el) {
      $el.children(".ah-tabs-header").off(NS);
    },
    methods: {
      select: function (el, $el, k) { tabSelect(el, $el, tabIndexOf($el, k), false); },
      disable: function (el, $el, k) {
        tabItems($el).eq(tabIndexOf($el, k)).addClass("ah-tabs-item-disabled").attr("aria-disabled", "true");
      },
      enable: function (el, $el, k) {
        tabItems($el).eq(tabIndexOf($el, k)).removeClass("ah-tabs-item-disabled").removeAttr("aria-disabled");
      },
      value: function (el) { return el.getAttribute("data-ah-value"); }
    }
  });
})(window.jQuery, window.AH);
