/* Behaviour of tab-bar (value: the active id; fires change and ah:close). */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.layout;

  function barTabs($el) {
    return $el.children(".ah-tab-bar__tab");
  }

  function barFind($el, id) {
    return barTabs($el).filter(function () { return this.getAttribute("data-id") === String(id); });
  }

  function barSelect(el, $el, $tab, user) {
    if (!$tab.length || $tab.attr("data-active") === "true") {
      return;
    }
    barTabs($el).attr({ "data-active": "false", "aria-selected": "false", tabindex: "-1" });
    $tab.attr({ "data-active": "true", "aria-selected": "true", tabindex: "0" });
    L.setValue(el, $el, $tab.attr("data-id"));
    if (user) {
      $el.trigger("change");
    }
  }

  function barClose(el, $el, $tab, user) {
    if (!$tab.length) {
      return;
    }
    var id = $tab.attr("data-id");
    var $all = barTabs($el);
    var idx = $all.index($tab);
    var wasActive = $tab.attr("data-active") === "true";
    var hadFocus = $.contains($tab[0], document.activeElement) || $tab[0] === document.activeElement;
    $tab.remove();
    $el.trigger("ah:close", [id]);
    if (wasActive) {
      var $left = barTabs($el);
      if ($left.length) {
        var $next = $left.eq(Math.min(idx, $left.length - 1));
        barSelect(el, $el, $next, user);
        if (hadFocus) { $next.trigger("focus"); }
      } else {
        L.setValue(el, $el, "");
        if (user) { $el.trigger("change"); }
      }
    }
  }

  AH.define("tab-bar", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-tab-bar__close", function (e) {
        e.stopPropagation();
        barClose(el, $el, $(this).closest(".ah-tab-bar__tab"), true);
      });
      $el.on("click" + NS, ".ah-tab-bar__tab", function () {
        barSelect(el, $el, $(this), true);
      });
      $el.on("keydown" + NS, ".ah-tab-bar__tab", function (e) {
        var $items = barTabs($el);
        var cur = $items.index(this);
        if (L.key(e) === "Delete" && $(this).children(".ah-tab-bar__close").length) {
          e.preventDefault();
          barClose(el, $el, $(this), true);
          return;
        }
        var t = L.listKeys(e, $items, cur, false, "ah-tab-bar__tab--none");
        if (t === null) { return; }
        e.preventDefault();
        barSelect(el, $el, $items.eq(t), true);
        $items.eq(t).trigger("focus");
      });
    },
    methods: {
      select: function (el, $el, id) { barSelect(el, $el, barFind($el, id), false); },
      close: function (el, $el, id) { barClose(el, $el, barFind($el, id), false); },
      value: function (el) { return el.getAttribute("data-ah-value"); }
    }
  });
})(window.jQuery, window.AH);
