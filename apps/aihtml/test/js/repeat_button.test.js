/* Behaviour of repeat-button (repeat_button.js). The fixtures are server renders from
 * aihtml_repeat_button, captured once; regenerate them if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var FX = {"rb":"<button class=\"ah-btn ah-btn-primary\" type=\"button\" value=\"1\" id=\"t-b\" data-ah=\"repeat-button\" data-ah-delay=\"60\" data-ah-interval=\"20\">+</button>"};

  function mount(fx, html) { fx.innerHTML = html; AH.mount(fx); return fx.firstChild; }
  function key(el, k, extra) { var e = $.Event("keydown", $.extend({ key: k }, extra || {})); $(el).trigger(e); return e; }
  function sleep(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }

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
