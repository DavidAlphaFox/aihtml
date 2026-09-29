/* Behaviour of steps (value: the current index; fires change). */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_layout.js";
import "virtual:ah-tpl/steps_indicator";

var NS = AH.NS;
var L = AH.lib.layout;

var STEP_STATES = "ah-steps-item-pending ah-steps-item-active ah-steps-item-completed ah-steps-item-error";

function stepItems($el) {
  return $el.children(".ah-steps-header").children(".ah-steps-item");
}

// Status class and indicator, the indicator from the server's template
// (templates/steps_indicator.mustache).
function stepIndicator($it, i, status) {
  $it.addClass("ah-steps-item-" + status);
  $it.children(".ah-steps-indicator").html(AH.tpl.steps_indicator({
    check: status === "completed", error: status === "error",
    plain: status !== "completed" && status !== "error", number: i + 1
  }));
}

function stepCur(el) {
  return parseInt(el.getAttribute("data-ah-value"), 10) || 0;
}

function stepSelect(el, $el, idx, user) {
  var $items = stepItems($el);
  var cur = stepCur(el);
  var n = $items.length;
  if (idx < 0 || idx >= n || idx === cur || $items.eq(idx).hasClass("ah-steps-item-disabled")) {
    return false;
  }
  var clickable = el.getAttribute("data-clickable") !== "false";
  $items.removeClass("ah-steps-item-selected").removeAttr("aria-current");
  $items.eq(idx).addClass("ah-steps-item-selected").attr("aria-current", "step");
  $items.each(function (i) {
    var $it = $(this);
    if ($it.attr("role") === "button") { $it.attr("tabindex", i === idx ? "0" : "-1"); }
    if ($it.hasClass("ah-steps-item-disabled") || $it.hasClass("ah-steps-item-error")) {
      return;
    }
    $it.removeClass(STEP_STATES);
    stepIndicator($it, i, i < idx ? "completed" : (i === idx ? "active" : "pending"));
  });
  $items.each(function () {
    var $it = $(this);
    $it.children(".ah-steps-connector").toggleClass("ah-steps-connector-done",
                                                    $it.hasClass("ah-steps-item-completed"));
  });
  var $panels = $el.children(".ah-steps-panels").children(".ah-steps-panel");
  $panels.removeClass("ah-steps-panel-active").eq(idx).addClass("ah-steps-panel-active");
  var $nav = $el.children(".ah-steps-nav");
  var toggle = function (action, on) {
    $nav.children("[data-action=" + action + "]").prop("disabled", !on)
      .toggleClass("ah-steps-btn-disabled", !on);
  };
  toggle("prev", clickable && idx > 0);
  toggle("next", clickable && idx < n - 1);
  L.setValue(el, $el, String(idx));
  if (user) {
    $el.trigger("change");
  }
  return true;
}

// Next non-disabled step from cur in direction dir, or cur.
function stepMove($el, cur, dir) {
  var $items = stepItems($el);
  for (var i = cur + dir; i >= 0 && i < $items.length; i += dir) {
    if (!$items.eq(i).hasClass("ah-steps-item-disabled")) { return i; }
  }
  return cur;
}

AH.define("steps", {
  init: function (el, $el) {
    var $header = $el.children(".ah-steps-header");
    $header.on("click" + NS, ".ah-steps-item-clickable", function () {
      if (!$el.hasClass("ah-steps-disabled")) {
        stepSelect(el, $el, stepItems($el).index(this), true);
      }
    });
    $header.on("keydown" + NS, ".ah-steps-item-clickable", function (e) {
      var $items = stepItems($el);
      var vertical = $el.hasClass("ah-steps-vertical");
      var t = L.listKeys(e, $items, $items.index(this), vertical, "ah-steps-item-disabled");
      if (t === null) { return; }
      e.preventDefault();
      stepSelect(el, $el, t, true);
      $items.eq(t).trigger("focus");
    });
    $el.on("click" + NS, ".ah-steps-btn", function () {
      if (this.parentNode.parentNode !== el || this.disabled) { return; }
      var dir = this.getAttribute("data-action") === "prev" ? -1 : 1;
      stepSelect(el, $el, stepMove($el, stepCur(el), dir), true);
    });
  },
  destroy: function (el, $el) {
    $el.children(".ah-steps-header").off(NS);
  },
  methods: {
    select: function (el, $el, i) { stepSelect(el, $el, parseInt(i, 10), false); },
    next: function (el, $el) { stepSelect(el, $el, stepMove($el, stepCur(el), 1), false); },
    prev: function (el, $el) { stepSelect(el, $el, stepMove($el, stepCur(el), -1), false); },
    first: function (el, $el) { stepSelect(el, $el, 0, false); },
    last: function (el, $el) { stepSelect(el, $el, stepItems($el).length - 1, false); },
    setStatus: function (el, $el, i, status) {
      var $it = stepItems($el).eq(parseInt(i, 10));
      $it.removeClass(STEP_STATES + " ah-steps-item-disabled");
      stepIndicator($it, parseInt(i, 10), status);
      $it.children(".ah-steps-connector").toggleClass("ah-steps-connector-done", status === "completed");
    },
    value: function (el) { return stepCur(el); }
  }
});
