/* Behaviour of masked-input (masked_input.js). The fixtures are server renders from
 * aihtml_masked_input, captured once; regenerate them if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var FX = {"mask":"<div class=\"ah-masked-input-group\" data-ah=\"masked-input\" data-ah-value=\"555\" data-ah-mask=\"(999) 999-9999\" data-ah-prompt=\"_\" id=\"t-m\"><input class=\"ah-masked-input\" type=\"text\" placeholder=\"\" autocomplete=\"off\" spellcheck=\"false\" autocorrect=\"off\" autocapitalize=\"off\" inputmode=\"numeric\" value=\"(555) ___-____\"><input type=\"hidden\" name=\"phone\" value=\"555\"></div>","maskf":"<div class=\"ah-masked-input-group\" data-ah=\"masked-input\" data-ah-value=\"\" data-ah-mask=\"99-LL\" data-ah-prompt=\"_\" id=\"t-mf\"><input class=\"ah-masked-input\" type=\"text\" placeholder=\"\" autocomplete=\"off\" spellcheck=\"false\" autocorrect=\"off\" autocapitalize=\"off\" value=\"\"><label class=\"ah-masked-input-label\">Code</label></div>"};

  function mount(fx, html) { fx.innerHTML = html; AH.mount(fx); return fx.firstChild; }
  function key(el, k, extra) { var e = $.Event("keydown", $.extend({ key: k }, extra || {})); $(el).trigger(e); return e; }
  function events(el, type) {
    var seen = [];
    $(el).on(type, function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }

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
})(window.AHTest, window.jQuery, window.AH);
