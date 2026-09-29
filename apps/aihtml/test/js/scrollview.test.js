/* scrollview: the pager behaviour (scrollview.js). FX holds a server
 * render of aihtml_scrollview (id t-sv), generated from Erlang;
 * regenerate it if the markup changes. The harness loads no stylesheet,
 * so CSS holds the few rules the geometry depends on (from sigil and
 * extra/scrollview.css). */
(function (T, AH) {
  "use strict";

  var FX = {"sv":"<div class=\"ah-scrollview\" id=\"t-sv\" data-ah=\"scrollview\" data-ah-value=\"0\" role=\"region\" aria-roledescription=\"carousel\" aria-label=\"Carousel\" tabindex=\"0\" style=\"width:300px;height:100px;\"><div class=\"ah-scrollview-wrapper\" id=\"t-sv-pages\" aria-live=\"polite\"><div class=\"ah-scrollview-page\" role=\"group\" aria-roledescription=\"slide\" aria-label=\"1 / 3\">one</div><div class=\"ah-scrollview-page\" role=\"group\" aria-roledescription=\"slide\" aria-label=\"2 / 3\" aria-hidden=\"true\" inert>two</div><div class=\"ah-scrollview-page\" role=\"group\" aria-roledescription=\"slide\" aria-label=\"3 / 3\" aria-hidden=\"true\" inert>three</div></div><div class=\"ah-scrollview-buttons\" role=\"group\" aria-label=\"Pages\"><span class=\"ah-scrollview-button ah-scrollview-button-active\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 1\" aria-current=\"true\"></span><span class=\"ah-scrollview-button\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 2\"></span><span class=\"ah-scrollview-button\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 3\"></span></div><input type=\"hidden\" name=\"p\" value=\"0\"></div>"};

  var CSS = ".ah-scrollview{position:relative;overflow:hidden}.ah-scrollview-wrapper{display:flex;width:100%;height:100%}" +
    ".ah-scrollview-page{flex:none;width:100%}";

  var CSS_ID = "t-sv-css";

  async function mount(fx, html) {
    if (!document.getElementById(CSS_ID)) {
      var s = document.createElement("style");
      s.id = CSS_ID;
      s.textContent = CSS;
      document.head.appendChild(s);
    }
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function changes(el) {
    var seen = [];
    el.addEventListener("change", function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function ptr(el, type, x, y) {
    T.fire(el, type, { button: 0, clientX: x, clientY: y || 0, pointerId: 1 });
  }
  function kid(el, sel) { return el.querySelector(":scope > " + sel); }

  T.test("scrollview: keys, dots, value, hidden input, change", async function (fx) {
    var el = await mount(fx, FX.sv), seen = changes(el), pages = [];
    var w = kid(el, ".ah-scrollview-wrapper");
    el.addEventListener("ah:page-changed", function (e) { pages.push([e.detail.old, e.detail.page]); });
    T.key(el, "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "1");
    T.eq(el.querySelector("input[type=hidden]").value, "1");
    T.eq(w.style.marginLeft, "-100%");
    var dots = Array.prototype.slice.call(el.querySelectorAll(".ah-scrollview-button"));
    T.eq(dots.indexOf(el.querySelector(".ah-scrollview-button-active")), 1);
    T.eq(w.children[0].getAttribute("aria-hidden"), "true");
    T.ok(!w.children[1].hasAttribute("inert"), "current page is not inert");
    dots[2].click();
    T.key(el, "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "2", "clamped at the last page");
    T.key(el, "Home");
    T.eq(seen, ["1", "2", "0"]);
    T.eq(pages, [[0, 1], [1, 2], [2, 0]]);
    AH.invoke(el, "setValue", 2);
    AH.invoke(el, "back");
    T.eq(AH.invoke(el, "getValue"), 1);
    T.eq(seen.length, 3, "methods do not fire change");
  });

  T.test("scrollview: drag past the threshold changes page, a short one snaps back", async function (fx) {
    var el = await mount(fx, FX.sv), seen = changes(el);
    var w = kid(el, ".ah-scrollview-wrapper");
    ptr(w, "pointerdown", 250); ptr(w, "pointermove", 200); ptr(w, "pointermove", 180);
    T.eq(w.style.marginLeft, "-70px", "follows the pointer");
    ptr(w, "pointerup", 180);
    T.eq(el.getAttribute("data-ah-value"), "0");
    T.eq(w.style.marginLeft, "", "snapped back");
    ptr(w, "pointerdown", 250); ptr(w, "pointermove", 150); ptr(w, "pointermove", 60);
    ptr(w, "pointerup", 60);
    T.eq(el.getAttribute("data-ah-value"), "1");
    T.eq(seen, ["1"]);
    // the click that ends a drag is swallowed
    var clicks = 0;
    el.addEventListener("click", function () { clicks++; });
    T.fire(w.children[1], "click");
    T.eq(clicks, 0, "click after a drag swallowed");
    await wait(10);
    T.fire(w.children[1], "click");
    T.eq(clicks, 1, "later clicks pass");
    ptr(w, "pointerdown", 100); ptr(w, "pointermove", 105);
    ptr(w, "pointerup", 105);
    T.eq(el.getAttribute("data-ah-value"), "1", "inside the dead zone nothing moves");
  });

  T.test("scrollview: slide show advances; removal stops it", async function (fx) {
    var el = await mount(fx, FX.sv.replace('data-ah-value="0"', 'data-ah-value="0" data-slide-duration="30"'));
    AH.invoke(el, "startSlideShow");
    await wait(80);
    T.ok(AH.invoke(el, "getValue") > 0, "advanced");
    AH.invoke(el, "stopSlideShow");
    var v = el.getAttribute("data-ah-value");
    await wait(80);
    T.eq(el.getAttribute("data-ah-value"), v, "stopped");
    AH.invoke(el, "startSlideShow");
    fx.innerHTML = "";
    await wait(10);
    var after = el.getAttribute("data-ah-value");
    await wait(80);
    T.eq(el.getAttribute("data-ah-value"), after, "teardown stops the timer");
  });
})(window.AHTest, window.AH);
