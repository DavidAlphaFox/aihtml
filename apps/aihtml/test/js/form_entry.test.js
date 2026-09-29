/* Behaviours of the form_entry components (form_entry.js). The fixtures are
 * server renders from aihtml_form_entry, captured once; regenerate them if
 * the markup changes. */
(function (T, $, AH) {
  "use strict";

  var FX = {"mask":"<div class=\"ah-masked-input-group\" data-ah=\"masked-input\" data-ah-value=\"555\" data-ah-mask=\"(999) 999-9999\" data-ah-prompt=\"_\" id=\"t-m\"><input class=\"ah-masked-input\" type=\"text\" placeholder=\"\" autocomplete=\"off\" spellcheck=\"false\" autocorrect=\"off\" autocapitalize=\"off\" inputmode=\"numeric\" value=\"(555) ___-____\"><input type=\"hidden\" name=\"phone\" value=\"555\"></div>","maskf":"<div class=\"ah-masked-input-group\" data-ah=\"masked-input\" data-ah-value=\"\" data-ah-mask=\"99-LL\" data-ah-prompt=\"_\" id=\"t-mf\"><input class=\"ah-masked-input\" type=\"text\" placeholder=\"\" autocomplete=\"off\" spellcheck=\"false\" autocorrect=\"off\" autocapitalize=\"off\" value=\"\"><label class=\"ah-masked-input-label\">Code</label></div>","fmt":"<div class=\"ah-fmt-input-group\" id=\"t-f\" data-ah=\"formatted-input\" data-ah-value=\"250\" data-ah-radix=\"10\" data-ah-min=\"0\" data-ah-max=\"300\" data-ah-step=\"10\"><div class=\"ah-fmt-input-row\"><input class=\"ah-fmt-input\" type=\"text\" id=\"t-f-input\" autocomplete=\"off\" spellcheck=\"false\" placeholder=\"\" value=\"250\" role=\"spinbutton\" aria-valuenow=\"250\" aria-valuetext=\"250\" aria-valuemin=\"0\" aria-valuemax=\"300\"><div class=\"ah-fmt-spin-buttons\" aria-hidden=\"true\"><span class=\"ah-fmt-spin-up\">▲</span><span class=\"ah-fmt-spin-down\">▼</span></div><span class=\"ah-fmt-dropdown-btn\" role=\"button\" aria-label=\"Radix\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-f-radix\">▼</span></div><div class=\"ah-fmt-popup\" id=\"t-f-radix\" role=\"listbox\" aria-label=\"Radix\"><div class=\"ah-fmt-popup-item\" role=\"option\" id=\"t-f-radix-2\" aria-selected=\"false\" data-radix=\"2\"><span class=\"ah-fmt-popup-item-label\">BIN</span><span class=\"ah-fmt-popup-item-desc\">Binary</span></div><div class=\"ah-fmt-popup-item\" role=\"option\" id=\"t-f-radix-8\" aria-selected=\"false\" data-radix=\"8\"><span class=\"ah-fmt-popup-item-label\">OCT</span><span class=\"ah-fmt-popup-item-desc\">Octal</span></div><div class=\"ah-fmt-popup-item ah-fmt-popup-item-active\" role=\"option\" id=\"t-f-radix-10\" aria-selected=\"true\" data-radix=\"10\"><span class=\"ah-fmt-popup-item-label\">DEC</span><span class=\"ah-fmt-popup-item-desc\">Decimal</span></div><div class=\"ah-fmt-popup-item\" role=\"option\" id=\"t-f-radix-16\" aria-selected=\"false\" data-radix=\"16\"><span class=\"ah-fmt-popup-item-label\">HEX</span><span class=\"ah-fmt-popup-item-desc\">Hexadecimal</span></div></div><input type=\"hidden\" name=\"n\" value=\"250\"></div>","rs":"<div class=\"ah-range-selector\" role=\"group\" data-ah=\"range-selector\" data-ah-value=\"20,60\" data-ah-min=\"0\" data-ah-max=\"100\" data-ah-step=\"5\" data-ah-page=\"25\" data-ah-min-span=\"10\" data-ah-format=\"{&quot;f&quot;:&quot;currency&quot;}\" id=\"t-r\"><div class=\"ah-range-selector-track\"><div class=\"ah-range-selector-ticks\" aria-hidden=\"true\"><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:0.0%\"></div><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:25.0%\"></div><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:50.0%\"></div><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:75.0%\"></div><div class=\"ah-range-selector-tick ah-range-selector-tick-major\" style=\"left:100.0%\"></div><div class=\"ah-range-selector-label\" style=\"left:0.0%\">0</div><div class=\"ah-range-selector-label\" style=\"left:25.0%\">25</div><div class=\"ah-range-selector-label\" style=\"left:50.0%\">50</div><div class=\"ah-range-selector-label\" style=\"left:75.0%\">75</div><div class=\"ah-range-selector-label\" style=\"left:100.0%\">100</div></div><div class=\"ah-range-selector-shutter-left\" style=\"left:0;width:20.0%\"></div><div class=\"ah-range-selector-slider\" style=\"left:20.0%;width:40.0%\"><div class=\"ah-range-selector-slider-inner\"></div></div><div class=\"ah-range-selector-shutter-right\" style=\"left:60.0%;width:40.0%\"></div><div class=\"ah-range-selector-marker ah-range-selector-marker-left\" style=\"left:20.0%\" role=\"slider\" tabindex=\"0\" aria-label=\"Minimum\" aria-valuenow=\"20\" aria-valuetext=\"$20\" aria-valuemin=\"0\" aria-valuemax=\"100\"><span class=\"ah-range-selector-marker-value\">$20</span></div><div class=\"ah-range-selector-marker ah-range-selector-marker-right\" style=\"left:60.0%\" role=\"slider\" tabindex=\"0\" aria-label=\"Maximum\" aria-valuenow=\"60\" aria-valuetext=\"$60\" aria-valuemin=\"0\" aria-valuemax=\"100\"><span class=\"ah-range-selector-marker-value\">$60</span></div></div><input type=\"hidden\" name=\"r\" value=\"20,60\"></div>","rb":"<button class=\"ah-btn ah-btn-primary\" type=\"button\" value=\"1\" id=\"t-b\" data-ah=\"repeat-button\" data-ah-delay=\"60\" data-ah-interval=\"20\">+</button>"};

  function mount(fx, html) { fx.innerHTML = html; AH.mount(fx); return fx.firstChild; }
  function key(el, k, extra) { var e = $.Event("keydown", $.extend({ key: k }, extra || {})); $(el).trigger(e); return e; }
  function events(el, type) {
    var seen = [];
    $(el).on(type, function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function sleep(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }

  T.test("masked-input: typing, literals, delete, paste, change on blur", function (fx) {
    var el = mount(fx, FX.mask), $in = $(el).find(".ah-masked-input"), inp = $in[0];
    var inputs = events(el, "input"), changes = events(el, "change");
    inp.focus();
    inp.setSelectionRange(5, 5);                      // after "(555"
    T.ok(key($in, "1").isDefaultPrevented(), "keys are handled, not typed");
    T.eq(inp.value, "(555) 1__-____");
    T.eq(inp.selectionStart, 7, "cursor skips to the next editable position");
    key($in, "x");
    T.eq(inp.value, "(555) 1__-____", "a letter does not fit");
    key($in, "2"); key($in, "3");
    key($in, "-");                                    // the literal typed where it stands
    T.eq(inp.selectionStart, 10);
    key($in, "4");
    T.eq(el.getAttribute("data-ah-value"), "5551234");
    T.eq($(el).find("input[type=hidden]").val(), "5551234");
    key($in, "Backspace");
    T.eq(inp.value, "(555) 123-____");
    inp.setSelectionRange(10, 10);
    var paste = $.Event("paste", { originalEvent: { preventDefault: function () {},
      clipboardData: { getData: function () { return "98-76"; } } } });
    $in.trigger(paste);
    T.eq(inp.value, "(555) 123-9876", "paste skips what does not fit");
    T.ok(AH.invoke(el, "isComplete"));
    T.eq(AH.invoke(el, "getMaskedValue"), "(555) 123-9876");
    T.eq(inputs.length, 6);
    T.eq(changes, []);
    $in.trigger("blur");
    T.eq(changes, ["5551239876"]);
    AH.invoke(el, "setValue", "(111) 222-3333");
    T.eq(inp.value, "(111) 222-3333");
    AH.invoke(el, "setMask", "999-999");
    T.eq(inp.value, "111-222");
    AH.invoke(el, "clear");
    T.eq(changes, ["5551239876", ""]);
  });

  T.test("masked-input: floating label shows no mask while empty", function (fx) {
    var el = mount(fx, FX.maskf), $in = $(el).find(".ah-masked-input"), $l = $(el).find("label");
    T.eq($in.val(), "");
    $in[0].focus();
    T.eq($in.val(), "__-__");
    T.ok($l.hasClass("ah-masked-input-label-float"));
    $in[0].setSelectionRange(0, 0);
    key($in, "4"); key($in, "2"); key($in, "a"); key($in, "b");
    T.eq(el.getAttribute("data-ah-value"), "42ab");
    $in[0].blur();
    T.eq($in.val(), "42-ab");
    T.ok($l.hasClass("ah-masked-input-label-float"));
    AH.invoke(el, "clear");
    T.eq($in.val(), "");
    T.ok(!$l.hasClass("ah-masked-input-label-float"));
  });

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

  T.test("repeat-button: clicks while held, one per repetition", async function (fx) {
    var el = mount(fx, FX.rb), clicks = 0, bubbled = 0;
    $(el).on("click", function () { clicks++; });
    $(document).on("click.rbtest", function (e) { if (e.target === el) { bubbled++; } });
    $(el).trigger($.Event("mousedown", { button: 0 }));
    T.eq(clicks, 1, "at once");
    T.ok($(el).hasClass("ah-btn-pressed"));
    await sleep(150);
    $(el).trigger("mouseup");
    el.click();                                       // the browser's click on release
    var n = clicks;
    T.ok(n >= 3 && n <= 7, "repeated: " + n);
    T.eq(bubbled, n, "the release click does not reach the page");
    await sleep(60);
    T.eq(clicks, n, "stopped");
    key(el, "Enter");
    key(el, "Enter", { repeat: true });
    $(el).trigger($.Event("keyup", { key: "Enter" }));
    T.eq(clicks, n + 1, "keyboard press");
    el.click();
    await sleep(5);
    el.click();
    T.eq(bubbled, n + 2, "a plain click later is one click");
    $(document).off("click.rbtest");
  });
})(window.AHTest, window.jQuery, window.AH);
