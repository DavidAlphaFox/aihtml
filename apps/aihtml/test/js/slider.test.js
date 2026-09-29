/* Behaviour of slider (slider.js). The fixtures are server renders from
 * aihtml_example_demo_slider, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"one":"<div class=\"ah-slider ah-slider-horizontal ah-slider-buttons-hidden ah-slider-ticks-hidden\" role=\"slider\" tabindex=\"0\" aria-valuenow=\"40\" aria-valuetext=\"40\" aria-valuemin=\"0\" aria-valuemax=\"100\" aria-orientation=\"horizontal\" data-ah=\"slider\" data-ah-value=\"40\" data-ah-min=\"0\" data-ah-max=\"100\" data-ah-step=\"1\" aria-label=\"Volume\"><div class=\"ah-slider-content\"><div class=\"ah-slider-track\"><div class=\"ah-slider-range\" aria-hidden=\"true\" style=\"left:0;width:calc((100% - 18px) * 0.4 + 9px)\"></div><div class=\"ah-slider-thumb ah-slider-thumb-end\" style=\"left:calc((100% - 18px) * 0.4)\" aria-hidden=\"true\"></div></div></div><div class=\"ah-slider-tooltip\" aria-hidden=\"true\"></div><input type=\"hidden\" name=\"volume\" value=\"40\"></div>","range":"<div class=\"ah-slider ah-slider-horizontal ah-slider-success ah-slider-buttons-hidden ah-slider-range-slider ah-slider-ticks-bottom\" role=\"group\" aria-orientation=\"horizontal\" data-ah=\"slider\" data-ah-value=\"200,600\" data-ah-min=\"0\" data-ah-max=\"1000\" data-ah-step=\"50\" data-ah-min-range=\"100\"><div class=\"ah-slider-content\"><div class=\"ah-slider-track\"><div class=\"ah-slider-range\" aria-hidden=\"true\" style=\"left:calc((100% - 18px) * 0.2 + 9px);width:calc((100% - 18px) * 0.4)\"></div><div class=\"ah-slider-thumb ah-slider-thumb-start\" style=\"left:calc((100% - 18px) * 0.2)\" role=\"slider\" tabindex=\"0\" aria-label=\"Minimum\" aria-valuenow=\"200\" aria-valuetext=\"200\" aria-valuemin=\"0\" aria-valuemax=\"1000\" aria-orientation=\"horizontal\"></div><div class=\"ah-slider-thumb ah-slider-thumb-end\" style=\"left:calc((100% - 18px) * 0.6)\" role=\"slider\" tabindex=\"0\" aria-label=\"Maximum\" aria-valuenow=\"600\" aria-valuetext=\"600\" aria-valuemin=\"0\" aria-valuemax=\"1000\" aria-orientation=\"horizontal\"></div></div><div class=\"ah-slider-ticks ah-slider-ticks-bottom\" aria-hidden=\"true\"><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.0 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.25 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.5 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.75 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 1.0 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.05 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.1 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.15 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.2 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.3 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.35 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.4 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.45 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.55 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.6 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.65 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.7 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.8 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.85 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.9 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-minor ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.95 + 9px)\"></div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.0 + 9px)\">0</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.25 + 9px)\">250</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.5 + 9px)\">500</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.75 + 9px)\">750</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 1.0 + 9px)\">1000</div></div></div><div class=\"ah-slider-tooltip\" aria-hidden=\"true\"></div><input type=\"hidden\" name=\"price\" value=\"200,600\"></div>","btns":"<div class=\"ah-slider ah-slider-horizontal ah-slider-warning ah-slider-ticks-both\" role=\"slider\" tabindex=\"0\" aria-valuenow=\"3\" aria-valuetext=\"3\" aria-valuemin=\"0\" aria-valuemax=\"10\" aria-orientation=\"horizontal\" data-ah=\"slider\" data-ah-value=\"3\" data-ah-min=\"0\" data-ah-max=\"10\" data-ah-step=\"1\"><button class=\"ah-slider-button ah-slider-button-prev\" type=\"button\" tabindex=\"-1\" aria-label=\"Decrease\"><span class=\"ah-slider-button-icon\">◀</span></button><button class=\"ah-slider-button ah-slider-button-next\" type=\"button\" tabindex=\"-1\" aria-label=\"Increase\"><span class=\"ah-slider-button-icon\">▶</span></button><div class=\"ah-slider-content\"><div class=\"ah-slider-ticks ah-slider-ticks-top\" aria-hidden=\"true\"><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.0 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.1 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.2 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.3 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.4 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.5 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.6 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.7 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.8 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.9 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 1.0 + 9px)\"></div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.0 + 9px)\">0</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.1 + 9px)\">1</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.2 + 9px)\">2</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.3 + 9px)\">3</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.4 + 9px)\">4</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.5 + 9px)\">5</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.6 + 9px)\">6</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.7 + 9px)\">7</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.8 + 9px)\">8</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.9 + 9px)\">9</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 1.0 + 9px)\">10</div></div><div class=\"ah-slider-track\"><div class=\"ah-slider-range\" aria-hidden=\"true\" style=\"left:0;width:calc((100% - 18px) * 0.3 + 9px)\"></div><div class=\"ah-slider-thumb ah-slider-thumb-end\" style=\"left:calc((100% - 18px) * 0.3)\" aria-hidden=\"true\"></div></div><div class=\"ah-slider-ticks ah-slider-ticks-bottom\" aria-hidden=\"true\"><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.0 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.1 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.2 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.3 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.4 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.5 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.6 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.7 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.8 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 0.9 + 9px)\"></div><div class=\"ah-slider-tick ah-slider-tick-major ah-slider-tick-horizontal\" style=\"left:calc((100% - 18px) * 1.0 + 9px)\"></div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.0 + 9px)\">0</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.1 + 9px)\">1</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.2 + 9px)\">2</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.3 + 9px)\">3</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.4 + 9px)\">4</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.5 + 9px)\">5</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.6 + 9px)\">6</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.7 + 9px)\">7</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.8 + 9px)\">8</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 0.9 + 9px)\">9</div><div class=\"ah-slider-tick-label\" style=\"left:calc((100% - 18px) * 1.0 + 9px)\">10</div></div></div></div>"};

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

  T.test("slider: keys step, Home/End, input and change on the root", async function (fx) {
    fx.style.width = "300px";
    var el = await mount(fx, FX.one), inputs = events(el, "input"), changes = events(el, "change");
    el.focus();
    T.ok(!T.key(el, "ArrowRight"), "handled");
    T.eq(el.getAttribute("data-ah-value"), "41");
    T.eq(el.getAttribute("aria-valuenow"), "41");
    T.eq(el.querySelector("input[type=hidden]").value, "41");
    T.ok(el.querySelector(".ah-slider-thumb-end").style.left.indexOf("41%") >= 0, "the thumb moved");
    T.key(el, "PageDown");
    T.eq(el.getAttribute("data-ah-value"), "31");
    T.key(el, "End");
    T.eq(el.getAttribute("data-ah-value"), "100");
    T.key(el, "End");
    T.eq(changes, ["41", "31", "100"]);
    T.eq(inputs, ["41", "31", "100"]);
    T.ok(el.classList.contains("ah-slider-focused"));
    T.ok(el.querySelector(".ah-slider-tooltip").classList.contains("ah-slider-tooltip-visible"));
    T.eq(el.querySelector(".ah-slider-tooltip").textContent, "100");
  });

  T.test("slider: pointer drag on the track", async function (fx) {
    fx.style.width = "318px";
    var el = await mount(fx, FX.one), inputs = events(el, "input"), changes = events(el, "change");
    var r = el.querySelector(".ah-slider-track").getBoundingClientRect();
    var at = function (v) { return r.left + 9 + (r.width - 18) * v / 100; };
    var y = r.top + r.height / 2;
    T.fire(el.querySelector(".ah-slider-content"), "pointerdown", { button: 0, clientX: at(20), clientY: y, pointerId: 1 });
    T.eq(el.getAttribute("data-ah-value"), "20", "the press jumps there");
    T.fire(el, "pointermove", { clientX: at(75), clientY: y, pointerId: 1 });
    T.eq(el.getAttribute("data-ah-value"), "75");
    T.eq(changes, [], "no change while dragging");
    T.fire(el, "pointerup", { clientX: at(75), clientY: y, pointerId: 1 });
    T.eq(changes, ["75"]);
    T.eq(inputs, ["20", "75"]);
    T.fire(el, "pointermove", { clientX: at(10), clientY: y, pointerId: 1 });
    T.eq(el.getAttribute("data-ah-value"), "75", "released");
  });

  T.test("slider: range keeps min range; buttons; setValue", async function (fx) {
    fx.style.width = "300px";
    fx.innerHTML = FX.range + FX.btns;
    await T.ready(fx);
    var el = fx.children[0], btns = fx.children[1], changes = events(el, "change");
    var start = el.querySelector(".ah-slider-thumb-start"), end = el.querySelector(".ah-slider-thumb-end");
    start.focus();
    T.key(start, "End");
    T.eq(el.getAttribute("data-ah-value"), "500,600", "min range 100 away");
    T.eq(start.getAttribute("aria-valuenow"), "500");
    T.key(end, "Home");
    T.eq(el.getAttribute("data-ah-value"), "500,600");
    T.key(end, "PageUp");
    T.eq(el.getAttribute("data-ah-value"), "500,700");
    T.eq(changes, ["500,600", "500,700"]);
    AH.invoke(el, "setValue", [900, 100]);
    T.eq(AH.invoke(el, "getValue"), "100,900");
    AH.invoke(el, "setValue", "300,400", true);
    T.eq(changes, ["500,600", "500,700", "100,900"], "setValue fires change unless silent");
    var bc = events(btns, "change");
    btns.querySelector(".ah-slider-button-next").click();
    btns.querySelector(".ah-slider-button-next").click();
    btns.querySelector(".ah-slider-button-prev").click();
    T.eq(bc, ["4", "5", "4"]);
  });

  T.test("slider: re-insertion", async function (fx) {
    fx.style.width = "300px";
    var el = await mount(fx, FX.one), changes = events(el, "change");
    await reinsert(fx, el);
    el.focus();
    T.key(el, "ArrowLeft");
    T.eq(changes, ["39"], "one step per key after re-insertion");
    el.blur();
    T.ok(!el.classList.contains("ah-slider-focused"));
  });
})(window.AHTest, window.AH);
