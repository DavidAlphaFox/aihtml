/* Behaviour of formatted-input (formatted_input.js). The fixtures are server renders from
 * aihtml_formatted_input, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"fmt":"<div class=\"ah-fmt-input-group\" id=\"t-f\" data-ah=\"formatted-input\" data-ah-value=\"250\" data-ah-radix=\"10\" data-ah-min=\"0\" data-ah-max=\"300\" data-ah-step=\"10\"><div class=\"ah-fmt-input-row\"><input class=\"ah-fmt-input\" type=\"text\" id=\"t-f-input\" autocomplete=\"off\" spellcheck=\"false\" placeholder=\"\" value=\"250\" role=\"spinbutton\" aria-valuenow=\"250\" aria-valuetext=\"250\" aria-valuemin=\"0\" aria-valuemax=\"300\"><div class=\"ah-fmt-spin-buttons\" aria-hidden=\"true\"><span class=\"ah-fmt-spin-up\">▲</span><span class=\"ah-fmt-spin-down\">▼</span></div><span class=\"ah-fmt-dropdown-btn\" role=\"button\" aria-label=\"Radix\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-f-radix\">▼</span></div><div class=\"ah-fmt-popup\" id=\"t-f-radix\" role=\"listbox\" aria-label=\"Radix\"><div class=\"ah-fmt-popup-item\" role=\"option\" id=\"t-f-radix-2\" aria-selected=\"false\" data-radix=\"2\"><span class=\"ah-fmt-popup-item-label\">BIN</span><span class=\"ah-fmt-popup-item-desc\">Binary</span></div><div class=\"ah-fmt-popup-item\" role=\"option\" id=\"t-f-radix-8\" aria-selected=\"false\" data-radix=\"8\"><span class=\"ah-fmt-popup-item-label\">OCT</span><span class=\"ah-fmt-popup-item-desc\">Octal</span></div><div class=\"ah-fmt-popup-item ah-fmt-popup-item-active\" role=\"option\" id=\"t-f-radix-10\" aria-selected=\"true\" data-radix=\"10\"><span class=\"ah-fmt-popup-item-label\">DEC</span><span class=\"ah-fmt-popup-item-desc\">Decimal</span></div><div class=\"ah-fmt-popup-item\" role=\"option\" id=\"t-f-radix-16\" aria-selected=\"false\" data-radix=\"16\"><span class=\"ah-fmt-popup-item-label\">HEX</span><span class=\"ah-fmt-popup-item-desc\">Hexadecimal</span></div></div><input type=\"hidden\" name=\"n\" value=\"250\"></div>"};

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

  T.test("formatted-input: step, clamp, radix menu, typing", async function (fx) {
    var el = await mount(fx, FX.fmt), inp = el.querySelector(".ah-fmt-input"), changes = events(el, "change");
    var popup = el.querySelector(".ah-fmt-popup"), radix = [];
    el.addEventListener("ah:radix-change", function (e) { radix.push(e.detail); });
    inp.focus();
    T.key(inp, "ArrowUp");
    T.eq(el.getAttribute("data-ah-value"), "260");
    T.key(inp, "ArrowUp", { }); T.key(inp, "ArrowUp"); T.key(inp, "ArrowUp"); T.key(inp, "ArrowUp");
    T.eq(el.getAttribute("data-ah-value"), "300", "clamped to max");
    T.eq(changes, ["260", "270", "280", "290", "300"]);
    T.ok(!T.key(inp, "g"), "not a decimal digit");
    T.key(inp, "ArrowDown", { altKey: true });
    T.ok(popup.classList.contains("ah-fmt-popup-open"), "alt+down opens");
    T.key(inp, "ArrowDown");                              // DEC -> HEX
    T.key(inp, "Enter");
    T.ok(!popup.classList.contains("ah-fmt-popup-open"));
    T.eq(AH.invoke(el, "getRadix"), 16);
    T.eq(radix, [{ radix: 16, old: 10 }]);
    T.eq(inp.value, "12c");
    T.ok(T.key(inp, "f"), "hex digit allowed");
    inp.value = "ff";
    T.fire(inp, "input");
    T.eq(el.getAttribute("data-ah-value"), "255");
    inp.blur();
    T.eq(changes.slice(-1), ["255"]);
    T.fire(el.querySelector(".ah-fmt-dropdown-btn"), "mousedown", { button: 0 });
    T.fire(el.querySelector('.ah-fmt-popup-item[data-radix="2"]'), "mousedown");
    T.eq(inp.value, "11111111");
    AH.invoke(el, "setValue", "-5");
    T.eq(el.getAttribute("data-ah-value"), "0", "clamped to min");
    AH.invoke(el, "setValue", 7);
    T.eq(inp.value, "111");
  });

  T.test("formatted-input: spin button repeats while held", async function (fx) {
    var el = await mount(fx, FX.fmt);
    AH.invoke(el, "setValue", 0);
    T.fire(el.querySelector(".ah-fmt-spin-up"), "mousedown", { button: 0 });
    await sleep(600);
    T.fire(document, "mouseup");
    var v = parseInt(el.getAttribute("data-ah-value"), 10);
    T.ok(v >= 30 && v <= 60, "repeated: " + v);
    await sleep(150);
    T.eq(parseInt(el.getAttribute("data-ah-value"), 10), v, "stopped");
  });

  T.test("formatted-input: an outside click closes the menu; re-insertion", async function (fx) {
    var el = await mount(fx, FX.fmt), popup = el.querySelector(".ah-fmt-popup"), log = [];
    el.addEventListener("ah:open", function () { log.push("open"); });
    el.addEventListener("ah:close", function () { log.push("close"); });
    AH.invoke(el, "open");
    T.ok(popup.classList.contains("ah-fmt-popup-open"));
    T.fire(document.body, "mousedown");
    T.ok(!popup.classList.contains("ah-fmt-popup-open"), "outside mousedown closes");
    T.eq(log, ["open", "close"]);
    await reinsert(fx, el);
    var inp = el.querySelector(".ah-fmt-input"), changes = events(el, "change");
    inp.focus();
    T.key(inp, "ArrowDown");
    T.eq(changes, ["240"], "one change per step after re-insertion");
  });
})(window.AHTest, window.AH);
