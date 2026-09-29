/* Behaviour of formatted-input (formatted_input.js). The fixtures are server renders from
 * aihtml_formatted_input, captured once; regenerate them if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var FX = {"fmt":"<div class=\"ah-fmt-input-group\" id=\"t-f\" data-ah=\"formatted-input\" data-ah-value=\"250\" data-ah-radix=\"10\" data-ah-min=\"0\" data-ah-max=\"300\" data-ah-step=\"10\"><div class=\"ah-fmt-input-row\"><input class=\"ah-fmt-input\" type=\"text\" id=\"t-f-input\" autocomplete=\"off\" spellcheck=\"false\" placeholder=\"\" value=\"250\" role=\"spinbutton\" aria-valuenow=\"250\" aria-valuetext=\"250\" aria-valuemin=\"0\" aria-valuemax=\"300\"><div class=\"ah-fmt-spin-buttons\" aria-hidden=\"true\"><span class=\"ah-fmt-spin-up\">▲</span><span class=\"ah-fmt-spin-down\">▼</span></div><span class=\"ah-fmt-dropdown-btn\" role=\"button\" aria-label=\"Radix\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-f-radix\">▼</span></div><div class=\"ah-fmt-popup\" id=\"t-f-radix\" role=\"listbox\" aria-label=\"Radix\"><div class=\"ah-fmt-popup-item\" role=\"option\" id=\"t-f-radix-2\" aria-selected=\"false\" data-radix=\"2\"><span class=\"ah-fmt-popup-item-label\">BIN</span><span class=\"ah-fmt-popup-item-desc\">Binary</span></div><div class=\"ah-fmt-popup-item\" role=\"option\" id=\"t-f-radix-8\" aria-selected=\"false\" data-radix=\"8\"><span class=\"ah-fmt-popup-item-label\">OCT</span><span class=\"ah-fmt-popup-item-desc\">Octal</span></div><div class=\"ah-fmt-popup-item ah-fmt-popup-item-active\" role=\"option\" id=\"t-f-radix-10\" aria-selected=\"true\" data-radix=\"10\"><span class=\"ah-fmt-popup-item-label\">DEC</span><span class=\"ah-fmt-popup-item-desc\">Decimal</span></div><div class=\"ah-fmt-popup-item\" role=\"option\" id=\"t-f-radix-16\" aria-selected=\"false\" data-radix=\"16\"><span class=\"ah-fmt-popup-item-label\">HEX</span><span class=\"ah-fmt-popup-item-desc\">Hexadecimal</span></div></div><input type=\"hidden\" name=\"n\" value=\"250\"></div>"};

  function mount(fx, html) { fx.innerHTML = html; AH.mount(fx); return fx.firstChild; }
  function key(el, k, extra) { var e = $.Event("keydown", $.extend({ key: k }, extra || {})); $(el).trigger(e); return e; }
  function events(el, type) {
    var seen = [];
    $(el).on(type, function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function sleep(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }

  T.test("formatted-input: step, clamp, radix menu, typing", function (fx) {
    var el = mount(fx, FX.fmt), $in = $(el).find(".ah-fmt-input"), changes = events(el, "change");
    $in[0].focus();
    key($in, "ArrowUp");
    T.eq(el.getAttribute("data-ah-value"), "260");
    key($in, "ArrowUp", { }); key($in, "ArrowUp"); key($in, "ArrowUp"); key($in, "ArrowUp");
    T.eq(el.getAttribute("data-ah-value"), "300", "clamped to max");
    T.eq(changes, ["260", "270", "280", "290", "300"]);
    T.ok(key($in, "g").isDefaultPrevented(), "not a decimal digit");
    key($in, "ArrowDown", { altKey: true });
    T.ok($(el).find(".ah-fmt-popup").hasClass("ah-fmt-popup-open"), "alt+down opens");
    key($in, "ArrowDown");                              // DEC -> HEX
    key($in, "Enter");
    T.ok(!$(el).find(".ah-fmt-popup").hasClass("ah-fmt-popup-open"));
    T.eq(AH.invoke(el, "getRadix"), 16);
    T.eq($in.val(), "12c");
    T.ok(!key($in, "f").isDefaultPrevented(), "hex digit allowed");
    $in.val("ff").trigger("input");
    T.eq(el.getAttribute("data-ah-value"), "255");
    $in[0].blur();
    T.eq(changes.slice(-1), ["255"]);
    $(el).find(".ah-fmt-dropdown-btn").trigger($.Event("mousedown", { button: 0 }));
    $(el).find('.ah-fmt-popup-item[data-radix="2"]').trigger("mousedown");
    T.eq($in.val(), "11111111");
    AH.invoke(el, "setValue", "-5");
    T.eq(el.getAttribute("data-ah-value"), "0", "clamped to min");
    AH.invoke(el, "setValue", 7);
    T.eq($in.val(), "111");
  });

  T.test("formatted-input: spin button repeats while held", async function (fx) {
    var el = mount(fx, FX.fmt);
    AH.invoke(el, "setValue", 0);
    $(el).find(".ah-fmt-spin-up").trigger($.Event("mousedown", { button: 0 }));
    await sleep(600);
    $(document).trigger("mouseup");
    var v = parseInt(el.getAttribute("data-ah-value"), 10);
    T.ok(v >= 30 && v <= 60, "repeated: " + v);
    await sleep(150);
    T.eq(parseInt(el.getAttribute("data-ah-value"), 10), v, "stopped");
  });
})(window.AHTest, window.jQuery, window.AH);
