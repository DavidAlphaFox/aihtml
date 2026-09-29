/* scrollbar: the standalone bar and the scroll area (scrollbar.js). FX
 * holds server renders of aihtml_scrollbar (ids t-sb, t-sa), generated
 * from Erlang; regenerate them if the markup changes. The harness loads
 * no stylesheet, so CSS holds the few rules the geometry depends on (from
 * sigil's scrollbar.css and extra/scrollbar.css). */
(function (T, AH) {
  "use strict";

  var FX = {"sb":"<div class=\"ah-scrollbar-host\" id=\"t-sb\" data-ah=\"scrollbar\" data-ah-value=\"100\" role=\"scrollbar\" aria-orientation=\"horizontal\" aria-valuemin=\"0\" aria-valuemax=\"1000\" aria-valuenow=\"100\" tabindex=\"0\" data-min=\"0\" data-max=\"1000\" data-step=\"10\" data-large-step=\"50\" data-thumb-min=\"10\" style=\"width:300px;\"><div class=\"ah-scrollbar ah-scrollbar-horizontal\" aria-hidden=\"true\"><div class=\"ah-scrollbar-btn-up\"></div><div class=\"ah-scrollbar-track-up\"></div><div class=\"ah-scrollbar-thumb\"></div><div class=\"ah-scrollbar-track-down\"></div><div class=\"ah-scrollbar-btn-down\"></div></div><input type=\"hidden\" name=\"o\" value=\"100\"></div>","sa":"<div class=\"ah-scrollbar-host ah-scrollbar-area\" id=\"t-sa\" data-ah=\"scrollbar\" data-area data-step=\"10\" data-thumb-min=\"10\" style=\"width:200px;height:100px;\"><div class=\"ah-scrollbar-viewport\" id=\"t-sa-viewport\" tabindex=\"0\"><div class=\"ah-scrollbar-content\"><p style=\"margin:0;height:20px\">1</p><p style=\"margin:0;height:20px\">2</p><p style=\"margin:0;height:20px\">3</p><p style=\"margin:0;height:20px\">4</p><p style=\"margin:0;height:20px\">5</p><p style=\"margin:0;height:20px\">6</p><p style=\"margin:0;height:20px\">7</p><p style=\"margin:0;height:20px\">8</p><p style=\"margin:0;height:20px\">9</p><p style=\"margin:0;height:20px\">10</p><p style=\"margin:0;height:20px\">11</p><p style=\"margin:0;height:20px\">12</p><p style=\"margin:0;height:20px\">13</p><p style=\"margin:0;height:20px\">14</p><p style=\"margin:0;height:20px\">15</p><p style=\"margin:0;height:20px\">16</p><p style=\"margin:0;height:20px\">17</p><p style=\"margin:0;height:20px\">18</p><p style=\"margin:0;height:20px\">19</p><p style=\"margin:0;height:20px\">20</p><p style=\"margin:0;height:20px\">21</p><p style=\"margin:0;height:20px\">22</p><p style=\"margin:0;height:20px\">23</p><p style=\"margin:0;height:20px\">24</p><p style=\"margin:0;height:20px\">25</p><p style=\"margin:0;height:20px\">26</p><p style=\"margin:0;height:20px\">27</p><p style=\"margin:0;height:20px\">28</p><p style=\"margin:0;height:20px\">29</p><p style=\"margin:0;height:20px\">30</p></div></div><div class=\"ah-scrollbar ah-scrollbar-vertical\" aria-hidden=\"true\"><div class=\"ah-scrollbar-btn-up\"></div><div class=\"ah-scrollbar-track-up\"></div><div class=\"ah-scrollbar-thumb\"></div><div class=\"ah-scrollbar-track-down\"></div><div class=\"ah-scrollbar-btn-down\"></div></div><div class=\"ah-scrollbar ah-scrollbar-horizontal\" aria-hidden=\"true\"><div class=\"ah-scrollbar-btn-up\"></div><div class=\"ah-scrollbar-track-up\"></div><div class=\"ah-scrollbar-thumb\"></div><div class=\"ah-scrollbar-track-down\"></div><div class=\"ah-scrollbar-btn-down\"></div></div><div class=\"ah-scrollbar-corner\"></div></div>"};

  var CSS = ".ah-scrollbar-host{position:relative;display:block;box-sizing:border-box}" +
    ".ah-scrollbar-host:not(.ah-scrollbar-area){height:14px}" +
    ".ah-scrollbar{position:absolute;display:flex;box-sizing:border-box}" +
    ".ah-scrollbar-vertical{width:14px;flex-direction:column;right:0;top:0;bottom:0}" +
    ".ah-scrollbar-horizontal{height:14px;left:0;right:0;bottom:0}" +
    ".ah-scrollbar-btn-up,.ah-scrollbar-btn-down{flex:0 0 14px}.ah-scrollbar-thumb,.ah-scrollbar-track-up,.ah-scrollbar-track-down{flex:0 0 auto}" +
    ".ah-scrollbar-area{overflow:hidden}.ah-scrollbar-viewport{height:100%;overflow:auto}" +
    ".ah-scrollbar-area>.ah-scrollbar{display:none}.ah-scrollbar-area-v{padding-right:14px}" +
    ".ah-scrollbar-area-v>.ah-scrollbar-vertical{display:flex}";

  var CSS_ID = "t-sb-css";

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

  T.test("scrollbar: geometry, keys, buttons, track, thumb drag", async function (fx) {
    var el = await mount(fx, FX.sb), seen = changes(el), inputs = 0;
    el.addEventListener("input", function () { inputs++; });
    var b = kid(el, ".ah-scrollbar");
    var thumb = kid(b, ".ah-scrollbar-thumb");
    // track 300 - 2 * 14 = 272; thumb 272^2 / (272 + 1000); at 100 of 1000
    T.eq(Math.round(parseFloat(thumb.style.width)), 58);
    T.eq(Math.round(parseFloat(kid(b, ".ah-scrollbar-track-up").style.width)), 21);
    T.key(el, "ArrowRight");
    T.key(el, "ArrowUp");
    T.key(el, "PageDown");
    T.eq(el.getAttribute("data-ah-value"), "160");
    T.eq(el.getAttribute("aria-valuenow"), "160");
    T.eq(el.querySelector("input[type=hidden]").value, "160");
    T.key(el, "End");
    T.key(el, "End");
    T.eq(seen, ["110", "160", "1000"]);
    AH.invoke(el, "setValue", 100);
    T.eq(AH.invoke(el, "getValue"), 100);
    var down = kid(b, ".ah-scrollbar-btn-down");
    ptr(down, "pointerdown", 0);
    ptr(down, "pointerup", 0);
    T.eq(el.getAttribute("data-ah-value"), "110");
    ptr(kid(b, ".ah-scrollbar-track-down"), "pointerdown", 0);
    T.eq(el.getAttribute("data-ah-value"), "160");
    seen.length = 0;
    var free = 272 - parseFloat(thumb.style.width);
    ptr(thumb, "pointerdown", 0);
    ptr(thumb, "pointermove", free / 2);
    ptr(thumb, "pointermove", free * 0.4);
    ptr(thumb, "pointerup", free * 0.4);
    T.eq(el.getAttribute("data-ah-value"), "560");
    T.eq(inputs, 2);
    T.eq(seen, ["560"], "one change when the drag ends");
    AH.invoke(el, "setMax", 200);
    T.eq(el.getAttribute("data-ah-value"), "200", "clamped to the new max");
    await wait(0);
  });

  T.test("scrollbar: scroll area bars follow the viewport", async function (fx) {
    var el = await mount(fx, FX.sa);
    var vp = kid(el, ".ah-scrollbar-viewport");
    var v = kid(el, ".ah-scrollbar-vertical");
    T.ok(el.classList.contains("ah-scrollbar-area-v"), "vertical bar shown");
    T.ok(!el.classList.contains("ah-scrollbar-area-h"), "no horizontal bar");
    var up = function () { return parseFloat(kid(v, ".ah-scrollbar-track-up").style.height); };
    T.eq(up(), 0);
    AH.invoke(el, "scrollTo", 0, 250);
    T.fire(vp, "scroll");
    T.ok(up() > 0, "thumb moved");
    var before = vp.scrollTop;
    ptr(kid(v, ".ah-scrollbar-track-down"), "pointerdown", 0);
    T.ok(vp.scrollTop > before, "a track click pages down");
    ptr(kid(v, ".ah-scrollbar-btn-up"), "pointerdown", 0);
    ptr(kid(v, ".ah-scrollbar-btn-up"), "pointerup", 0);
    T.ok(vp.scrollTop < before + 90, "an arrow click steps up");
    await wait(0);
  });

  T.test("scrollbar: removed and inserted again, one set of listeners", async function (fx) {
    var el = await mount(fx, FX.sb), seen = changes(el);
    fx.removeChild(el);
    await wait(0);
    fx.appendChild(el);
    await T.ready(fx);
    T.key(el, "ArrowRight");
    T.eq(seen, ["110"]);
    AH.invoke(el, "refresh");
    T.eq(Math.round(parseFloat(el.querySelector(".ah-scrollbar-thumb").style.width)), 58);
  });
})(window.AHTest, window.AH);
