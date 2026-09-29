/* Behaviour of range-selector (range_selector.js). The fixtures are server renders from
 * aihtml_range_selector, captured once; regenerate them if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var FX = {"rs":"<div class=\"ah-range-selector\" role=\"group\" data-ah=\"range-selector\" data-ah-value=\"20,60\" data-ah-min=\"0\" data-ah-max=\"100\" data-ah-step=\"5\" data-ah-page=\"25\" data-ah-min-span=\"10\" data-ah-format=\"{&quot;f&quot;:&quot;currency&quot;}\" id=\"t-r\"><div class=\"ah-range-selector-track\"><div class=\"ah-range-selector-ticks\" aria-hidden=\"true\"><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:0.0%\"></div><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:25.0%\"></div><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:50.0%\"></div><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:75.0%\"></div><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:100.0%\"></div><div class=\"ah-range-selector-label\" style=\"left:0.0%\">0</div><div class=\"ah-range-selector-label\" style=\"left:25.0%\">25</div><div class=\"ah-range-selector-label\" style=\"left:50.0%\">50</div><div class=\"ah-range-selector-label\" style=\"left:75.0%\">75</div><div class=\"ah-range-selector-label\" style=\"left:100.0%\">100</div></div><div class=\"ah-range-selector-shutter-left\" style=\"left:0;width:20.0%\"></div><div class=\"ah-range-selector-slider\" style=\"left:20.0%;width:40.0%\"><div class=\"ah-range-selector-slider-inner\"></div></div><div class=\"ah-range-selector-shutter-right\" style=\"left:60.0%;width:40.0%\"></div><div class=\"ah-range-selector-marker ah-range-selector-marker-left\" style=\"left:20.0%\" role=\"slider\" tabindex=\"0\" aria-label=\"Minimum\" aria-valuenow=\"20\" aria-valuetext=\"$20\" aria-valuemin=\"0\" aria-valuemax=\"100\"><span class=\"ah-range-selector-marker-value\">$20</span></div><div class=\"ah-range-selector-marker ah-range-selector-marker-right\" style=\"left:60.0%\" role=\"slider\" tabindex=\"0\" aria-label=\"Maximum\" aria-valuenow=\"60\" aria-valuetext=\"$60\" aria-valuemin=\"0\" aria-valuemax=\"100\"><span class=\"ah-range-selector-marker-value\">$60</span></div></div><input type=\"hidden\" name=\"r\" value=\"20,60\"></div>"};

  function mount(fx, html) { fx.innerHTML = html; AH.mount(fx); return fx.firstChild; }
  function key(el, k, extra) { var e = $.Event("keydown", $.extend({ key: k }, extra || {})); $(el).trigger(e); return e; }
  function events(el, type) {
    var seen = [];
    $(el).on(type, function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }

  T.test("range-selector: keyboard, min span, drag, setValue", function (fx) {
    fx.style.width = "400px";
    var el = mount(fx, FX.rs), changes = events(el, "change"), inputs = events(el, "input");
    var $l = $(el).find(".ah-range-selector-marker-left"), $r = $(el).find(".ah-range-selector-marker-right");
    key($l, "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "25,60");
    T.eq($l.attr("aria-valuetext"), "$25");
    T.eq($l.find(".ah-range-selector-marker-value").text(), "$25");
    T.eq($l[0].style.left, "25%");
    key($l, "End");
    T.eq(el.getAttribute("data-ah-value"), "50,60", "kept min_span away");
    key($r, "PageUp");
    T.eq(el.getAttribute("data-ah-value"), "50,85");
    T.eq($(el).find("input[type=hidden]").val(), "50,85");
    T.eq(changes, ["25,60", "50,60", "50,85"]);
    var track = $(el).find(".ah-range-selector-track")[0].getBoundingClientRect();
    var at = function (v) { return track.left + track.width * v / 100; };
    $r.trigger($.Event("mousedown", { button: 0, clientX: at(85) }));
    $(document).trigger($.Event("mousemove", { clientX: at(71) }));
    T.eq(el.getAttribute("data-ah-value"), "50,70", "snapped to the step");
    T.eq(changes.length, 3, "no change while dragging");
    $(document).trigger($.Event("mouseup"));
    T.eq(changes.slice(-1), ["50,70"]);
    var $bar = $(el).find(".ah-range-selector-slider");
    $bar.trigger($.Event("mousedown", { button: 0, clientX: at(60) }));
    $(document).trigger($.Event("mousemove", { clientX: at(10) }));
    T.eq(el.getAttribute("data-ah-value"), "0,20", "the bar moves both ends");
    $(document).trigger($.Event("mouseup"));
    T.eq(changes.slice(-1), ["0,20"]);
    T.ok(inputs.length >= 5);
    AH.invoke(el, "setValue", "40,30");
    T.eq(AH.invoke(el, "getValue"), [30, 40]);
    T.eq(changes.length, 5, "setValue fires nothing");
  });
})(window.AHTest, window.jQuery, window.AH);
