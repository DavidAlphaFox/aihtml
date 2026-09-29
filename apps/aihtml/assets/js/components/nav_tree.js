/* Behaviour of the nav tree (designs/04-components.md).
 *
 * Ported from sigil (layout/nav_tree). The server renders links and
 * <details>; this keeps the active link, opens the nodes around it and
 * fires "change" when the user follows a link.
 *
 * Value-bearing roots keep their value in data-ah-value, mirror it into a
 * hidden input and fire "change" when the user changes it; methods called
 * by the server (AH.invoke / aihtml_action:call) do not fire it.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  function navActivate(el, route) {
    var $el = $(el);
    $el.find(".ah-nav-tree__item.ah-is-active").removeClass("ah-is-active").removeAttr("aria-current");
    var $a = $el.find("a.ah-nav-tree__item").filter(function () {
      return this.getAttribute("data-route") === route;
    }).first();
    $a.addClass("ah-is-active").attr("aria-current", "page");
    // as sigil re-renders: the nodes around the active link are open, others closed
    $el.find("details.ah-nav-tree__node").each(function () {
      var hit = $a.length > 0 && $.contains(this, $a[0]);
      this.open = hit;
      $(this).children("summary").toggleClass("ah-is-open", hit);
    });
    el.setAttribute("data-ah-value", route || "");
  }

  AH.define("nav-tree", {
    init: function (el, $el) {
      $el.on("click" + NS, "a.ah-nav-tree__item[data-route]", function () {
        var route = this.getAttribute("data-route");
        var prev = el.getAttribute("data-ah-value");
        navActivate(el, route);
        if (route !== prev) { $el.trigger("change"); }
      });
    },
    methods: {
      setValue: function (el, $el, route) { navActivate(el, route === null || route === undefined ? "" : String(route)); },
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; }
    }
  });
})(window.jQuery, window.AH);
