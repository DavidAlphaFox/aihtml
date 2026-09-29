/* Behaviour of number-input (number_input.js). The fixtures are server renders from
 * aihtml_example_demo_number_input, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"num":"<div class=\"ah-numinput-group w-40\" data-ah=\"number-input\" data-min=\"0\" data-max=\"10\" data-step=\"1\" data-decimals=\"0\" data-allow-null=\"true\"><div class=\"ah-numinput-row\"><input class=\"ah-numinput-input\" type=\"text\" inputmode=\"decimal\" role=\"spinbutton\" autocomplete=\"off\" spellcheck=\"false\" value=\"5\" aria-valuemin=\"0\" aria-valuemax=\"10\" aria-valuenow=\"5\" name=\"qty\"><div class=\"ah-numinput-spin\"><span class=\"ah-numinput-spin-up\" aria-hidden=\"true\">▲</span><span class=\"ah-numinput-spin-down\" aria-hidden=\"true\">▼</span></div></div></div>","label":"<div class=\"ah-numinput-group w-40\" data-ah=\"number-input\" data-step=\"1\" data-decimals=\"0\" data-allow-null=\"true\"><div class=\"ah-numinput-row\"><input class=\"ah-numinput-input\" type=\"text\" inputmode=\"decimal\" role=\"spinbutton\" autocomplete=\"off\" spellcheck=\"false\" value=\"\" id=\"ah-in-18953\"><div class=\"ah-numinput-spin\"><span class=\"ah-numinput-spin-up\" aria-hidden=\"true\">▲</span><span class=\"ah-numinput-spin-down\" aria-hidden=\"true\">▼</span></div></div><label class=\"ah-numinput-label\" for=\"ah-in-18953\">Amount</label></div>"};

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

  T.test("number-input: arrows step and clamp, native input/change on the field", async function (fx) {
    var el = await mount(fx, FX.num), input = el.querySelector("input"), log = [];
    input.addEventListener("input", function () { log.push("input:" + input.value); });
    input.addEventListener("change", function () { log.push("change:" + input.value); });
    input.focus();
    T.ok(!T.key(input, "ArrowUp"), "handled");
    T.eq(input.value, "6");
    T.eq(input.getAttribute("aria-valuenow"), "6");
    T.key(input, "PageUp");
    T.eq(input.value, "10", "clamped to max");
    T.key(input, "PageUp");
    T.eq(log, ["input:6", "change:6", "input:10", "change:10"], "no event when nothing moved");
    T.ok(!T.key(input, "a"), "a letter is not typed");
    T.ok(!T.key(input, "."), "no decimals");
    input.value = "42";
    T.fire(input, "change");
    T.eq(input.value, "10", "typed text is clamped on change");
  });

  T.test("number-input: spin buttons repeat while held; wheel", async function (fx) {
    var el = await mount(fx, FX.num), input = el.querySelector("input");
    AH.invoke(el, "setValue", 0);
    T.fire(el.querySelector(".ah-numinput-spin-up"), "mousedown", { button: 0 });
    T.eq(input.value, "1", "at once");
    T.eq(document.activeElement, input);
    await sleep(520);
    T.fire(document, "mouseup");
    var v = parseInt(input.value, 10);
    T.ok(v >= 2 && v <= 4, "repeated: " + v);
    await sleep(120);
    T.eq(parseInt(input.value, 10), v, "stopped");
    input.dispatchEvent(new WheelEvent("wheel", { deltaY: -100, bubbles: true, cancelable: true }));
    T.eq(parseInt(input.value, 10), v + 1);
  });

  T.test("number-input: methods, floating label, re-insertion", async function (fx) {
    var el = await mount(fx, FX.label), input = el.querySelector("input"), label = el.querySelector("label"), n = 0;
    input.addEventListener("change", function () { n++; });
    AH.invoke(el, "setValue", "12");
    T.eq(AH.invoke(el, "getValue"), 12);
    T.ok(label.classList.contains("ah-numinput-label-float"));
    AH.invoke(el, "stepUp");
    AH.invoke(el, "stepDown");
    AH.invoke(el, "stepDown");
    T.eq(AH.invoke(el, "getValue"), 11);
    AH.invoke(el, "clear");
    T.eq(AH.invoke(el, "getValue"), null);
    T.ok(!label.classList.contains("ah-numinput-label-float"));
    T.eq(n, 5);
    await reinsert(fx, el);
    AH.invoke(el, "setValue", 3);
    input.focus();
    T.key(input, "ArrowDown");
    T.eq(input.value, "2", "one step per key after re-insertion");
  });
})(window.AHTest, window.AH);
