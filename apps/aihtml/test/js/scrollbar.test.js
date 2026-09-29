/* scrollbar: the standalone bar and the scroll area (scrollbar.js). FX
 * holds server renders of aihtml_scrollbar (ids t-sb, t-sa), generated
 * from Erlang; regenerate them if the markup changes. The harness loads
 * no stylesheet, so CSS holds the few rules the geometry depends on (from
 * sigil's scrollbar.css and extra/scrollbar.css). */
(function (T, $, AH) {
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

  T.test("scrollbar: geometry, keys, buttons, track, thumb drag", async function (fx) {
    var el = mount(fx, FX.sb), seen = changes(el), inputs = 0;
    $(el).on("input", function () { inputs++; });
    var $b = $(el).children(".ah-scrollbar");
    var thumb = $b.children(".ah-scrollbar-thumb")[0];
    // track 300 - 2 * 14 = 272; thumb 272^2 / (272 + 1000); at 100 of 1000
    T.eq(Math.round(parseFloat(thumb.style.width)), 58);
    T.eq(Math.round(parseFloat($b.children(".ah-scrollbar-track-up")[0].style.width)), 21);
    key(el, "ArrowRight");
    key(el, "ArrowUp");
    key(el, "PageDown");
    T.eq(el.getAttribute("data-ah-value"), "160");
    T.eq(el.getAttribute("aria-valuenow"), "160");
    T.eq($(el).find("input[type=hidden]").val(), "160");
    key(el, "End");
    key(el, "End");
    T.eq(seen, ["110", "160", "1000"]);
    AH.invoke(el, "setValue", 100);
    T.eq(AH.invoke(el, "getValue"), 100);
    var down = $b.children(".ah-scrollbar-btn-down")[0];
    ptr(down, "pointerdown", 0);
    ptr(down, "pointerup", 0);
    T.eq(el.getAttribute("data-ah-value"), "110");
    ptr($b.children(".ah-scrollbar-track-down")[0], "pointerdown", 0);
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
    var el = mount(fx, FX.sa);
    var vp = $(el).children(".ah-scrollbar-viewport")[0];
    var $v = $(el).children(".ah-scrollbar-vertical");
    T.ok($(el).hasClass("ah-scrollbar-area-v"), "vertical bar shown");
    T.ok(!$(el).hasClass("ah-scrollbar-area-h"), "no horizontal bar");
    var up = function () { return parseFloat($v.children(".ah-scrollbar-track-up")[0].style.height); };
    T.eq(up(), 0);
    AH.invoke(el, "scrollTo", 0, 250);
    $(vp).trigger("scroll");
    T.ok(up() > 0, "thumb moved");
    var before = vp.scrollTop;
    ptr($v.children(".ah-scrollbar-track-down")[0], "pointerdown", 0);
    T.ok(vp.scrollTop > before, "a track click pages down");
    ptr($v.children(".ah-scrollbar-btn-up")[0], "pointerdown", 0);
    ptr($v.children(".ah-scrollbar-btn-up")[0], "pointerup", 0);
    T.ok(vp.scrollTop < before + 90, "an arrow click steps up");
    await wait(0);
  });
})(window.AHTest, window.jQuery, window.AH);
