/* segmented-control: single selection, arrow keys move and select; the
 * root keeps data-ah-value and the hidden input in step and fires change
 * (designs/04-components.md). */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.button;

  function segItems($el) {
    return $el.children(".ah-segmented-control__item");
  }

  function segSet($el, v, fire) {
    v = String(v == null ? "" : v);
    var any = false;
    segItems($el).each(function () {
      var on = this.getAttribute("data-value") === v;
      any = any || on;
      $(this).attr({ "data-state": on ? "active" : "inactive", "aria-selected": String(on),
                     tabindex: on ? "0" : "-1" });
    });
    if (!any) {
      segItems($el).not(":disabled").first().attr("tabindex", "0");
    }
    L.setValue($el, v, fire);
  }

  AH.define("segmented-control", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-segmented-control__item", function () {
        if (this.disabled || $el.attr("data-disabled") === "true"
            || this.getAttribute("data-disabled") === "true") { return; }
        segSet($el, this.getAttribute("data-value"), true);
      });
      $el.on("keydown" + NS, ".ah-segmented-control__item", function (e) {
        var $items = segItems($el).not(":disabled");
        var i = L.step(e.key, $items.index(this), $items.length);
        if (i < 0) { return; }
        e.preventDefault();
        var $to = $items.eq(i);
        $to.trigger("focus");
        segSet($el, $to.attr("data-value"), true);
      });
    },
    methods: {
      setValue: function (el, $el, v) { segSet($el, v, false); },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); }
    }
  });
})(window.jQuery, window.AH);
