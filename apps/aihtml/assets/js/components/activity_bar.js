/* Behaviour of the activity bar (designs/04-components.md), ported from
 * sigil's layout/activity_bar. The value (the active item) is kept in
 * data-ah-value on the root, mirrored into a hidden input, and "change"
 * fires when the user changes it. Methods called by the server
 * (AH.invoke / aihtml_action:call) do not fire "change". */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  function setValue(el, v) {
    el.setAttribute("data-ah-value", v);
    $(el).children("input[type=hidden]").val(v);
  }

  // ------------------------------------------------------------------
  // ActivityBar: a vertical tablist of icon buttons
  // ------------------------------------------------------------------

  function barItems(el) {
    return $(el).children(".ah-activity-bar__item");
  }

  function barActivate(el, id) {
    barItems(el).each(function () {
      var on = this.getAttribute("data-id") === String(id);
      this.setAttribute("data-active", String(on));
      this.setAttribute("aria-selected", String(on));
      this.setAttribute("tabindex", on ? "0" : "-1");
    });
    // keep one item reachable with Tab when nothing is active
    var $items = barItems(el);
    if (!$items.filter("[tabindex=0]").length) {
      $items.not("[data-disabled=true]").first().attr("tabindex", "0");
    }
    setValue(el, id == null ? "" : String(id));
  }

  function barChoose(el, $el, item) {
    if (item.getAttribute("data-disabled") === "true") { return; }
    var id = item.getAttribute("data-id");
    var changed = el.getAttribute("data-ah-value") !== id;
    barActivate(el, id);
    $el.trigger("ah:select", [id]);
    if (changed) { $el.trigger("change"); }
  }

  AH.define("activity-bar", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-activity-bar__item", function () {
        barChoose(el, $el, this);
      });
      // WAI-ARIA tabs: arrows move and activate, Home / End jump
      $el.on("keydown" + NS, ".ah-activity-bar__item", function (e) {
        var $en = barItems(el).not("[data-disabled=true]");
        var i = $en.index(this);
        var next;
        switch (e.key) {
          case "ArrowDown": case "ArrowRight": next = (i + 1) % $en.length; break;
          case "ArrowUp": case "ArrowLeft": next = (i - 1 + $en.length) % $en.length; break;
          case "Home": next = 0; break;
          case "End": next = $en.length - 1; break;
          default: return;
        }
        e.preventDefault();
        var t = $en[next];
        if (t) {
          t.focus();
          barChoose(el, $el, t);
        }
      });
    },
    methods: {
      setValue: function (el, $el, v) { barActivate(el, v); },
      getValue: function (el) { return el.getAttribute("data-ah-value"); }
    }
  });

})(window.jQuery, window.AH);
