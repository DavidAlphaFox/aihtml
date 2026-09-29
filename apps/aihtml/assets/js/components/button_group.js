/* button-group: radio / checkbox selection (arrow keys in radio mode), a
 * short pressed flash in the default mode (designs/04-components.md).
 * In radio and checkbox mode the root keeps data-ah-value and the hidden
 * input in step and fires change. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.button;

  function groupMode($el) {
    return $el.hasClass("ah-btn-group-radio") ? "radio"
      : $el.hasClass("ah-btn-group-checkbox") ? "checkbox" : "default";
  }

  function groupButtons($el) {
    return $el.children(".ah-btn-group-btn");
  }

  function groupSelect($el, $btn, on) {
    $btn.toggleClass("ah-btn-group-btn-selected", on);
    if (groupMode($el) === "radio") {
      $btn.attr({ "aria-checked": String(on), tabindex: on ? "0" : "-1" });
    } else {
      $btn.attr("aria-pressed", String(on));
    }
  }

  function groupSync($el, fire) {
    var vals = groupButtons($el).filter(".ah-btn-group-btn-selected").map(function () {
      return this.getAttribute("data-value");
    }).get();
    L.setValue($el, vals.join(","), fire);
  }

  function groupSet($el, values) {
    var set = {};
    $.each(values, function (_, v) { set[String(v)] = true; });
    groupButtons($el).each(function () {
      groupSelect($el, $(this), !!set[this.getAttribute("data-value")]);
    });
    if (groupMode($el) === "radio" && !groupButtons($el).filter("[tabindex=0]").length) {
      groupButtons($el).not(":disabled").first().attr("tabindex", "0");
    }
    groupSync($el, false);
  }

  function groupClick($el, $btn) {
    switch (groupMode($el)) {
      case "radio":
        groupButtons($el).each(function () { groupSelect($el, $(this), this === $btn[0]); });
        groupSync($el, true);
        break;
      case "checkbox":
        groupSelect($el, $btn, !$btn.hasClass("ah-btn-group-btn-selected"));
        groupSync($el, true);
        break;
      default:
        $btn.addClass("ah-btn-group-btn-pressed");
        setTimeout(function () { $btn.removeClass("ah-btn-group-btn-pressed"); }, 150);
    }
  }

  AH.define("button-group", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-btn-group-btn", function () {
        if (this.disabled || $el.hasClass("ah-btn-group-disabled")) { return; }
        groupClick($el, $(this));
      });
      $el.on("mouseenter" + NS, ".ah-btn-group-btn", function () {
        if (!this.disabled) { $(this).addClass("ah-btn-group-btn-hover"); }
      });
      $el.on("mouseleave" + NS, ".ah-btn-group-btn", function () {
        $(this).removeClass("ah-btn-group-btn-hover");
      });
      // Radio mode is a radiogroup: arrows move focus and select.
      $el.on("keydown" + NS, ".ah-btn-group-btn", function (e) {
        if (groupMode($el) !== "radio") { return; }
        var $btns = groupButtons($el).not(":disabled");
        var i = L.step(e.key, $btns.index(this), $btns.length);
        if (i < 0) { return; }
        e.preventDefault();
        var $to = $btns.eq(i);
        $to.trigger("focus");
        groupClick($el, $to);
      });
    },
    methods: {
      setValue: function (el, $el, v) {
        groupSet($el, Array.isArray(v) ? v : String(v == null ? "" : v).split(",").filter(Boolean));
      },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); },
      clear: function (el, $el) { groupSet($el, []); }
    }
  });
})(window.jQuery, window.AH);
