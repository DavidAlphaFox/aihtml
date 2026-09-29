/* Behaviour of range-selector (range_selector.js). The fixtures are server renders from
 * aihtml_range_selector, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"rs":"<div class=\"ah-range-selector\" role=\"group\" data-ah=\"range-selector\" data-ah-value=\"20,60\" data-ah-min=\"0\" data-ah-max=\"100\" data-ah-step=\"5\" data-ah-page=\"25\" data-ah-min-span=\"10\" data-ah-format=\"{&quot;f&quot;:&quot;currency&quot;}\" id=\"t-r\"><div class=\"ah-range-selector-track\"><div class=\"ah-range-selector-ticks\" aria-hidden=\"true\"><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:0.0%\"></div><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:25.0%\"></div><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:50.0%\"></div><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:75.0%\"></div><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:100.0%\"></div><div class=\"ah-range-selector-label\" style=\"left:0.0%\">0</div><div class=\"ah-range-selector-label\" style=\"left:25.0%\">25</div><div class=\"ah-range-selector-label\" style=\"left:50.0%\">50</div><div class=\"ah-range-selector-label\" style=\"left:75.0%\">75</div><div class=\"ah-range-selector-label\" style=\"left:100.0%\">100</div></div><div class=\"ah-range-selector-shutter-left\" style=\"left:0;width:20.0%\"></div><div class=\"ah-range-selector-slider\" style=\"left:20.0%;width:40.0%\"><div class=\"ah-range-selector-slider-inner\"></div></div><div class=\"ah-range-selector-shutter-right\" style=\"left:60.0%;width:40.0%\"></div><div class=\"ah-range-selector-marker ah-range-selector-marker-left\" style=\"left:20.0%\" role=\"slider\" tabindex=\"0\" aria-label=\"Minimum\" aria-valuenow=\"20\" aria-valuetext=\"$20\" aria-valuemin=\"0\" aria-valuemax=\"100\"><span class=\"ah-range-selector-marker-value\">$20</span></div><div class=\"ah-range-selector-marker ah-range-selector-marker-right\" style=\"left:60.0%\" role=\"slider\" tabindex=\"0\" aria-label=\"Maximum\" aria-valuenow=\"60\" aria-valuetext=\"$60\" aria-valuemin=\"0\" aria-valuemax=\"100\"><span class=\"ah-range-selector-marker-value\">$60</span></div></div><input type=\"hidden\" name=\"r\" value=\"20,60\"></div>"};

  async function mount(fx, html) { fx.innerHTML = html; await T.ready(fx); return fx.firstElementChild; }
  // Take the element out (its controller tears down) and put it back.
  async function reinsert(fx, el) {
    el.remove();
    await new Promise(function (r) { setTimeout(r, 0); });
    fx.appendChild(el);
    await T.ready(fx);
    return el;
  }
  function events(el, type) {
    var seen = [];
    el.addEventListener(type, function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function sleep(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function all(el, sel) { return Array.prototype.slice.call(el.querySelectorAll(sel)); }

  T.test("range-selector: keyboard, min span, drag, setValue", async function (fx) {
    fx.style.width = "400px";
    var el = await mount(fx, FX.rs), changes = events(el, "change"), inputs = events(el, "input");
    var l = el.querySelector(".ah-range-selector-marker-left"), r = el.querySelector(".ah-range-selector-marker-right");
    T.key(l, "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "25,60");
    T.eq(l.getAttribute("aria-valuetext"), "$25");
    T.eq(l.querySelector(".ah-range-selector-marker-value").textContent, "$25");
    T.eq(l.style.left, "25%");
    T.key(l, "End");
    T.eq(el.getAttribute("data-ah-value"), "50,60", "kept min_span away");
    T.key(r, "PageUp");
    T.eq(el.getAttribute("data-ah-value"), "50,85");
    T.eq(el.querySelector("input[type=hidden]").value, "50,85");
    T.eq(changes, ["25,60", "50,60", "50,85"]);
    var track = el.querySelector(".ah-range-selector-track").getBoundingClientRect();
    var at = function (v) { return track.left + track.width * v / 100; };
    T.fire(r, "mousedown", { button: 0, clientX: at(85) });
    T.fire(document, "mousemove", { clientX: at(71) });
    T.eq(el.getAttribute("data-ah-value"), "50,70", "snapped to the step");
    T.eq(changes.length, 3, "no change while dragging");
    T.fire(document, "mouseup");
    T.eq(changes.slice(-1), ["50,70"]);
    var bar = el.querySelector(".ah-range-selector-slider");
    T.fire(bar, "mousedown", { button: 0, clientX: at(60) });
    T.fire(document, "mousemove", { clientX: at(10) });
    T.eq(el.getAttribute("data-ah-value"), "0,20", "the bar moves both ends");
    T.fire(document, "mouseup");
    T.eq(changes.slice(-1), ["0,20"]);
    T.ok(inputs.length >= 5);
    AH.invoke(el, "setValue", "40,30");
    T.eq(AH.invoke(el, "getValue"), [30, 40]);
    T.eq(changes.length, 5, "setValue fires nothing");
  });

  T.test("range-selector: re-insertion keeps one drag per press", async function (fx) {
    fx.style.width = "400px";
    var el = await mount(fx, FX.rs), details = [];
    el.addEventListener("change", function (e) { if (e.target === el) { details.push(e.detail); } });
    await reinsert(fx, el);
    var r = el.querySelector(".ah-range-selector-marker-right");
    var track = el.querySelector(".ah-range-selector-track").getBoundingClientRect();
    T.fire(r, "mousedown", { button: 0, clientX: track.left + track.width * 0.6 });
    T.fire(document, "mousemove", { clientX: track.left + track.width * 0.8 });
    T.fire(document, "mouseup");
    T.fire(document, "mousemove", { clientX: track.left + track.width * 0.9 });
    T.eq(details, ["20,80"], "change detail is the value; the drag ended on release");
  });
})(window.AHTest, window.AH);
