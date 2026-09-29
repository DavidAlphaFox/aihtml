/* scrollview: the pager behaviour (scrollview.js). FX holds a server
 * render of aihtml_scrollview (id t-sv), generated from Erlang;
 * regenerate it if the markup changes. The harness loads no stylesheet,
 * so CSS holds the few rules the geometry depends on (from sigil and
 * extra/scrollview.css). */
(function (T, $, AH) {
  "use strict";

  var FX = {"sv":"<div class=\"ah-scrollview\" id=\"t-sv\" data-ah=\"scrollview\" data-ah-value=\"0\" role=\"region\" aria-roledescription=\"carousel\" aria-label=\"Carousel\" tabindex=\"0\" style=\"width:300px;height:100px;\"><div class=\"ah-scrollview-wrapper\" id=\"t-sv-pages\" aria-live=\"polite\"><div class=\"ah-scrollview-page\" role=\"group\" aria-roledescription=\"slide\" aria-label=\"1 / 3\">one</div><div class=\"ah-scrollview-page\" role=\"group\" aria-roledescription=\"slide\" aria-label=\"2 / 3\" aria-hidden=\"true\" inert>two</div><div class=\"ah-scrollview-page\" role=\"group\" aria-roledescription=\"slide\" aria-label=\"3 / 3\" aria-hidden=\"true\" inert>three</div></div><div class=\"ah-scrollview-buttons\" role=\"group\" aria-label=\"Pages\"><span class=\"ah-scrollview-button ah-scrollview-button-active\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 1\" aria-current=\"true\"></span><span class=\"ah-scrollview-button\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 2\"></span><span class=\"ah-scrollview-button\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 3\"></span></div><input type=\"hidden\" name=\"p\" value=\"0\"></div>"};

  var CSS = ".ah-scrollview{position:relative;overflow:hidden}.ah-scrollview-wrapper{display:flex;width:100%;height:100%}" +
    ".ah-scrollview-page{flex:none;width:100%}";

  function mount(fx, html) {
    if (!document.getElementById("t-scroll-css")) {
      $('<style id="t-scroll-css">').text(CSS).appendTo("head");
    }
    fx.innerHTML = html;
    AH.mount(fx);
    return fx.firstChild;
  }
  function key(el, k) { $(el).trigger($.Event("keydown", { key: k })); }
  function changes(el) {
    var seen = [];
    $(el).on("change", function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function ptr(el, type, x, y) {
    $(el).trigger($.Event(type, { button: 0, clientX: x, clientY: y || 0 }));
  }

  T.test("scrollview: keys, dots, value, hidden input, change", function (fx) {
    var el = mount(fx, FX.sv), seen = changes(el), pages = [];
    var $w = $(el).children(".ah-scrollview-wrapper");
    $(el).on("ah:page-changed", function (e, d) { pages.push([d.old, d.page]); });
    key(el, "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "1");
    T.eq($(el).find("input[type=hidden]").val(), "1");
    T.eq($w[0].style.marginLeft, "-100%");
    T.eq($(el).find(".ah-scrollview-button-active").index(), 1);
    T.eq($w.children().eq(0).attr("aria-hidden"), "true");
    T.ok(!$w.children()[1].hasAttribute("inert"), "current page is not inert");
    $(el).find(".ah-scrollview-button").eq(2).trigger("click");
    key(el, "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "2", "clamped at the last page");
    key(el, "Home");
    T.eq(seen, ["1", "2", "0"]);
    T.eq(pages, [[0, 1], [1, 2], [2, 0]]);
    AH.invoke(el, "setValue", 2);
    AH.invoke(el, "back");
    T.eq(AH.invoke(el, "getValue"), 1);
    T.eq(seen.length, 3, "methods do not fire change");
  });

  T.test("scrollview: drag past the threshold changes page, a short one snaps back", function (fx) {
    var el = mount(fx, FX.sv), seen = changes(el);
    var w = $(el).children(".ah-scrollview-wrapper")[0];
    ptr(w, "pointerdown", 250); ptr(w, "pointermove", 200); ptr(w, "pointermove", 180);
    T.eq(w.style.marginLeft, "-70px", "follows the pointer");
    ptr(w, "pointerup", 180);
    T.eq(el.getAttribute("data-ah-value"), "0");
    T.eq(w.style.marginLeft, "", "snapped back");
    ptr(w, "pointerdown", 250); ptr(w, "pointermove", 150); ptr(w, "pointermove", 60);
    ptr(w, "pointerup", 60);
    T.eq(el.getAttribute("data-ah-value"), "1");
    T.eq(seen, ["1"]);
    ptr(w, "pointerdown", 100); ptr(w, "pointermove", 105);
    ptr(w, "pointerup", 105);
    T.eq(el.getAttribute("data-ah-value"), "1", "inside the dead zone nothing moves");
  });
})(window.AHTest, window.jQuery, window.AH);
