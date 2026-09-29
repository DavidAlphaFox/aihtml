/* rating behaviour (designs/04-components.md): a value-bearing custom
 * control: data-ah-value, the hidden input and a "change" on the root;
 * "ah:hover" [value|null] while the pointer previews a value. Methods
 * never fire change.
 */
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;

function ratingState(el) {
  return {
    value: parseFloat(el.getAttribute("data-ah-value")) || 0,
    max: parseInt(el.getAttribute("data-ah-max"), 10) || 5,
    step: el.getAttribute("data-precision") === "0.5" ? 0.5 : 1,
    live: el.getAttribute("data-readonly") !== "true" &&
          el.getAttribute("data-disabled") !== "true",
    clear: el.getAttribute("data-allow-clear") !== "false"
  };
}

function paintRating($el, v) {
  $el.find(".ah-rating__star").each(function (i) {
    var r = Math.max(0, Math.min(1, v - i));
    $(this).find(".ah-rating__filled").css("width", (r * 100) + "%");
  });
}

function setRating(el, $el, v, fire) {
  var s = ratingState(el);
  v = Math.max(0, Math.min(s.max, Math.round(Number(v) / s.step) * s.step || 0));
  var changed = v !== s.value;
  el.setAttribute("data-ah-value", String(v));
  $el.children("input[type=hidden]").val(String(v));
  $el.find(".ah-rating__star").each(function (i) {
    this.setAttribute("aria-checked", v >= i + 1 ? "true" : "false");
  });
  paintRating($el, v);
  if (fire && changed) {
    $el.trigger("change");
  }
}

// The value under the pointer; a keyboard click (detail 0) takes the
// whole star.
function ratingAt(el, star, e) {
  var s = ratingState(el);
  var idx = parseInt(star.getAttribute("data-index"), 10);
  if (s.step === 0.5 && e.detail !== 0 && e.clientX !== undefined) {
    var rect = star.getBoundingClientRect();
    return idx + ((e.clientX - rect.left) / rect.width <= 0.5 ? 0.5 : 1);
  }
  return idx + 1;
}

AH.define("rating", {
  init: function (el, $el) {
    $el.on("mousemove" + NS, ".ah-rating__star", function (e) {
      if (!ratingState(el).live) { return; }
      var v = ratingAt(el, this, e);
      paintRating($el, v);
      $el.trigger("ah:hover", [v]);
    });
    $el.on("mouseleave" + NS, function () {
      var s = ratingState(el);
      if (!s.live) { return; }
      paintRating($el, s.value);
      $el.trigger("ah:hover", [null]);
    });
    $el.on("click" + NS, ".ah-rating__star", function (e) {
      var s = ratingState(el);
      if (!s.live) { return; }
      var v = ratingAt(el, this, e);
      setRating(el, $el, s.clear && v === s.value ? 0 : v, true);
    });
    $el.on("keydown" + NS, ".ah-rating__star", function (e) {
      var s = ratingState(el);
      if (!s.live) { return; }
      var d = { ArrowRight: 1, ArrowUp: 1, ArrowLeft: -1, ArrowDown: -1 }[e.key];
      if (!d) { return; }
      e.preventDefault();
      setRating(el, $el, s.value + d * s.step, true);
    });
  },
  methods: {
    setValue: function (el, $el, v) { setRating(el, $el, v, false); },
    getValue: function (el) { return ratingState(el).value; }
  }
});
