/* activity_bar: the activity-bar behaviour on the markup the server renders.
 * SERVER holds renders of aihtml_activity_bar:activity_bar/4, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER =
{
    "a": "<div class=\"ah-activity-bar\" role=\"tablist\" aria-orientation=\"vertical\" data-placement=\"left\" data-ah=\"activity-bar\" data-ah-value=\"a\" id=\"ab\"><input type=\"hidden\" name=\"v\" value=\"a\"><button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" data-id=\"a\" data-active=\"true\" data-disabled=\"false\" aria-selected=\"true\" aria-label=\"Alpha\" title=\"Alpha\" tabindex=\"0\"><span class=\"ah-activity-bar__icon\">A</span></button><button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" data-id=\"b\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\" aria-label=\"Beta\" title=\"Beta\" tabindex=\"-1\"><span class=\"ah-activity-bar__icon\">B</span></button><div class=\"ah-activity-bar__divider\" role=\"presentation\" data-index=\"2\"></div><button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" data-id=\"c\" data-active=\"false\" data-disabled=\"true\" aria-selected=\"false\" aria-label=\"Gamma\" title=\"Gamma\" tabindex=\"-1\" disabled><span class=\"ah-activity-bar__icon\">C</span></button><button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" data-id=\"d\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\" aria-label=\"Delta\" title=\"Delta\" tabindex=\"-1\"><span class=\"ah-activity-bar__icon\">D</span></button></div>"
  }
;

  function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    AH.mount(fx);
    return fx.querySelector("[data-ah]");
  }

  function key(target, k, extra) {
    $(target).trigger($.Event("keydown", $.extend({ key: k }, extra || {})));
  }

  function events(el, type) {
    var got = [];
    $(el).on(type, function (e, d) { if (e.target === el) { got.push(d === undefined ? el.getAttribute("data-ah-value") : d); } });
    return got;
  }

  // ------------------------------------------------------------------ activity-bar

  T.test("activity-bar: click activates, fires change and ah:select", function (fx) {
    var el = mount(fx, "a");
    var changes = events(el, "change");
    var selects = events(el, "ah:select");
    $(el).find("[data-id=b]").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "b");
    T.eq($(el).children("input[type=hidden]").val(), "b");
    T.eq($(el).find("[data-id=b]").attr("aria-selected"), "true");
    T.eq($(el).find("[data-id=b]").attr("tabindex"), "0");
    T.eq($(el).find("[data-id=a]").attr("data-active"), "false");
    T.eq(changes, ["b"]);
    T.eq(selects, ["b"]);
    // the active item again: select, no change
    $(el).find("[data-id=b]").trigger("click");
    T.eq(changes.length, 1);
    T.eq(selects.length, 2);
  });

  T.test("activity-bar: arrows skip disabled items and wrap", function (fx) {
    var el = mount(fx, "a");
    var b = $(el).find("[data-id=b]")[0];
    key(b, "ArrowDown");
    T.eq(el.getAttribute("data-ah-value"), "d", "skips disabled c");
    key($(el).find("[data-id=d]")[0], "ArrowDown");
    T.eq(el.getAttribute("data-ah-value"), "a", "wraps");
    key($(el).find("[data-id=a]")[0], "End");
    T.eq(el.getAttribute("data-ah-value"), "d");
    $(el).find("[data-id=c]").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "d", "disabled does nothing");
  });

  T.test("activity-bar: setValue / getValue do not fire change", function (fx) {
    var el = mount(fx, "a");
    var changes = events(el, "change");
    AH.invoke(el, "setValue", "d");
    T.eq(AH.invoke(el, "getValue"), "d");
    T.eq($(el).find("[data-id=d]").attr("data-active"), "true");
    T.eq(changes.length, 0);
  });

})(window.AHTest, window.jQuery, window.AH);
