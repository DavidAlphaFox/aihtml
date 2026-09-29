/* navigationbar: the navigationbar behaviour on the markup the server renders.
 * SERVER holds renders of aihtml_navigationbar:navigationbar/4, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER =
{
    "n": "<div class=\"ah-navigationbar ah-navigationbar-vertical\" id=\"nb\" data-ah=\"navigationbar\" data-ah-value=\"0\" data-expand-mode=\"single_fit_height\" data-animation=\"none\" data-toggle-mode=\"click\" data-expand-duration=\"250\" data-collapse-duration=\"250\"><input type=\"hidden\" name=\"o\" value=\"0\"><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header ah-navigationbar-header-expanded\" id=\"nb-item-0-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"true\" aria-controls=\"nb-item-0-content\"><span class=\"ah-navigationbar-header-text\">One</span><span class=\"ah-navigationbar-arrow ah-navigationbar-arrow-up\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nb-item-0-content\" role=\"region\" aria-labelledby=\"nb-item-0-header\"><div class=\"ah-navigationbar-content\">1</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nb-item-1-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nb-item-1-content\"><span class=\"ah-navigationbar-header-text\">Two</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nb-item-1-content\" role=\"region\" aria-labelledby=\"nb-item-1-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">2</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header ah-navigationbar-disabled\" id=\"nb-item-2-header\" role=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-controls=\"nb-item-2-content\" aria-disabled=\"true\"><span class=\"ah-navigationbar-header-text\">Three</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nb-item-2-content\" role=\"region\" aria-labelledby=\"nb-item-2-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">3</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nb-item-3-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nb-item-3-content\"><span class=\"ah-navigationbar-header-text\">Four</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nb-item-3-content\" role=\"region\" aria-labelledby=\"nb-item-3-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">4</div></div></div></div>",
    "nm": "<div class=\"ah-navigationbar ah-navigationbar-vertical ah-navigationbar-expand-multiple\" id=\"nm\" data-ah=\"navigationbar\" data-ah-value=\"\" data-expand-mode=\"multiple\" data-animation=\"none\" data-toggle-mode=\"click\" data-expand-duration=\"250\" data-collapse-duration=\"250\"><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nm-item-0-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nm-item-0-content\"><span class=\"ah-navigationbar-header-text\">One</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nm-item-0-content\" role=\"region\" aria-labelledby=\"nm-item-0-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">1</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nm-item-1-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nm-item-1-content\"><span class=\"ah-navigationbar-header-text\">Two</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nm-item-1-content\" role=\"region\" aria-labelledby=\"nm-item-1-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">2</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nm-item-2-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nm-item-2-content\"><span class=\"ah-navigationbar-header-text\">Three</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nm-item-2-content\" role=\"region\" aria-labelledby=\"nm-item-2-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">3</div></div></div></div>"
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

  // ------------------------------------------------------------------ navigationbar

  function header(el, i) { return $(el).find(".ah-navigationbar-header").eq(i); }
  function shown(el, i) { return $(el).find(".ah-navigationbar-body").eq(i).css("display") !== "none"; }

  T.test("navigationbar: single_fit_height opens one and never closes it", function (fx) {
    var el = mount(fx, "n");
    var changes = events(el, "change");
    T.ok(shown(el, 0));
    header(el, 1).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "1");
    T.eq($(el).children("input[type=hidden]").val(), "1");
    T.ok(shown(el, 1) && !shown(el, 0));
    T.eq(header(el, 1).attr("aria-expanded"), "true");
    T.eq(header(el, 0).attr("aria-expanded"), "false");
    T.ok(header(el, 1).find(".ah-navigationbar-arrow").hasClass("ah-navigationbar-arrow-up"));
    header(el, 1).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "1", "the open one stays open");
    header(el, 2).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "1", "disabled");
    T.eq(changes, ["1"]);
  });

  T.test("navigationbar: multiple, keyboard and methods", function (fx) {
    var el = mount(fx, "nm");
    var changes = events(el, "change");
    key(header(el, 0)[0], "Enter");
    key(header(el, 2)[0], " ");
    T.eq(el.getAttribute("data-ah-value"), "0,2");
    key(header(el, 0)[0], "Enter");
    T.eq(el.getAttribute("data-ah-value"), "2");
    T.eq(changes.length, 3);
    header(el, 0)[0].focus();
    key(header(el, 0)[0], "ArrowDown");
    T.eq(document.activeElement, header(el, 1)[0]);
    key(header(el, 1)[0], "End");
    T.eq(document.activeElement, header(el, 2)[0]);
    AH.invoke(el, "setValue", "0,1");
    T.eq(AH.invoke(el, "getValue"), [0, 1]);
    T.ok(shown(el, 0) && shown(el, 1) && !shown(el, 2));
    AH.invoke(el, "disable", 1);
    AH.invoke(el, "collapse", 1);
    T.eq(el.getAttribute("data-ah-value"), "0");
    AH.invoke(el, "expand", 1);
    T.eq(el.getAttribute("data-ah-value"), "0", "disabled cannot expand");
    AH.invoke(el, "enable", 1);
    AH.invoke(el, "toggle", 1);
    T.eq(el.getAttribute("data-ah-value"), "0,1");
    T.eq(changes.length, 3, "methods fire no change");
  });

})(window.AHTest, window.jQuery, window.AH);
