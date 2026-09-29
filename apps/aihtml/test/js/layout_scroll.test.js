/* layout_scroll: scrollview, scrollbar and responsive-panel behaviours
 * (layout_scroll.js). FX holds server renders of aihtml_layout_scroll
 * (ids t-sv, t-sb, t-sa, t-rp), generated from Erlang; regenerate them if
 * the markup changes. The harness loads no stylesheet, so CSS holds the
 * few rules the geometry depends on (from sigil's scrollbar.css and
 * extra/layout_scroll.css). */
(function (T, $, AH) {
  "use strict";

  var FX = {"sv":"<div class=\"ah-scrollview\" id=\"t-sv\" data-ah=\"scrollview\" data-ah-value=\"0\" role=\"region\" aria-roledescription=\"carousel\" aria-label=\"Carousel\" tabindex=\"0\" style=\"width:300px;height:100px;\"><div class=\"ah-scrollview-wrapper\" id=\"t-sv-pages\" aria-live=\"polite\"><div class=\"ah-scrollview-page\" role=\"group\" aria-roledescription=\"slide\" aria-label=\"1 / 3\">one</div><div class=\"ah-scrollview-page\" role=\"group\" aria-roledescription=\"slide\" aria-label=\"2 / 3\" aria-hidden=\"true\" inert>two</div><div class=\"ah-scrollview-page\" role=\"group\" aria-roledescription=\"slide\" aria-label=\"3 / 3\" aria-hidden=\"true\" inert>three</div></div><div class=\"ah-scrollview-buttons\" role=\"group\" aria-label=\"Pages\"><span class=\"ah-scrollview-button ah-scrollview-button-active\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 1\" aria-current=\"true\"></span><span class=\"ah-scrollview-button\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 2\"></span><span class=\"ah-scrollview-button\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 3\"></span></div><input type=\"hidden\" name=\"p\" value=\"0\"></div>","sb":"<div class=\"ah-scrollbar-host\" id=\"t-sb\" data-ah=\"scrollbar\" data-ah-value=\"100\" role=\"scrollbar\" aria-orientation=\"horizontal\" aria-valuemin=\"0\" aria-valuemax=\"1000\" aria-valuenow=\"100\" tabindex=\"0\" data-min=\"0\" data-max=\"1000\" data-step=\"10\" data-large-step=\"50\" data-thumb-min=\"10\" style=\"width:300px;\"><div class=\"ah-scrollbar ah-scrollbar-horizontal\" aria-hidden=\"true\"><div class=\"ah-scrollbar-btn-up\"></div><div class=\"ah-scrollbar-track-up\"></div><div class=\"ah-scrollbar-thumb\"></div><div class=\"ah-scrollbar-track-down\"></div><div class=\"ah-scrollbar-btn-down\"></div></div><input type=\"hidden\" name=\"o\" value=\"100\"></div>","sa":"<div class=\"ah-scrollbar-host ah-scrollbar-area\" id=\"t-sa\" data-ah=\"scrollbar\" data-area data-step=\"10\" data-thumb-min=\"10\" style=\"width:200px;height:100px;\"><div class=\"ah-scrollbar-viewport\" id=\"t-sa-viewport\" tabindex=\"0\"><div class=\"ah-scrollbar-content\"><p style=\"margin:0;height:20px\">1</p><p style=\"margin:0;height:20px\">2</p><p style=\"margin:0;height:20px\">3</p><p style=\"margin:0;height:20px\">4</p><p style=\"margin:0;height:20px\">5</p><p style=\"margin:0;height:20px\">6</p><p style=\"margin:0;height:20px\">7</p><p style=\"margin:0;height:20px\">8</p><p style=\"margin:0;height:20px\">9</p><p style=\"margin:0;height:20px\">10</p><p style=\"margin:0;height:20px\">11</p><p style=\"margin:0;height:20px\">12</p><p style=\"margin:0;height:20px\">13</p><p style=\"margin:0;height:20px\">14</p><p style=\"margin:0;height:20px\">15</p><p style=\"margin:0;height:20px\">16</p><p style=\"margin:0;height:20px\">17</p><p style=\"margin:0;height:20px\">18</p><p style=\"margin:0;height:20px\">19</p><p style=\"margin:0;height:20px\">20</p><p style=\"margin:0;height:20px\">21</p><p style=\"margin:0;height:20px\">22</p><p style=\"margin:0;height:20px\">23</p><p style=\"margin:0;height:20px\">24</p><p style=\"margin:0;height:20px\">25</p><p style=\"margin:0;height:20px\">26</p><p style=\"margin:0;height:20px\">27</p><p style=\"margin:0;height:20px\">28</p><p style=\"margin:0;height:20px\">29</p><p style=\"margin:0;height:20px\">30</p></div></div><div class=\"ah-scrollbar ah-scrollbar-vertical\" aria-hidden=\"true\"><div class=\"ah-scrollbar-btn-up\"></div><div class=\"ah-scrollbar-track-up\"></div><div class=\"ah-scrollbar-thumb\"></div><div class=\"ah-scrollbar-track-down\"></div><div class=\"ah-scrollbar-btn-down\"></div></div><div class=\"ah-scrollbar ah-scrollbar-horizontal\" aria-hidden=\"true\"><div class=\"ah-scrollbar-btn-up\"></div><div class=\"ah-scrollbar-track-up\"></div><div class=\"ah-scrollbar-thumb\"></div><div class=\"ah-scrollbar-track-down\"></div><div class=\"ah-scrollbar-btn-down\"></div></div><div class=\"ah-scrollbar-corner\"></div></div>","rp":"<div class=\"ah-responsive-panel\" id=\"t-rp\" data-ah=\"responsive-panel\" data-breakpoint=\"400\" data-collapse-width=\"200px\" data-animation=\"none\" data-show-duration=\"200\" data-hide-duration=\"200\"><div class=\"ah-responsive-panel-toggle\" role=\"button\" tabindex=\"0\" title=\"Toggle panel\" aria-label=\"Toggle panel\" aria-expanded=\"false\" aria-controls=\"t-rp-content\">☰</div><div class=\"ah-responsive-panel-content\" id=\"t-rp-content\">nav</div></div>"};

  var CSS = ".ah-scrollview{position:relative;overflow:hidden}.ah-scrollview-wrapper{display:flex;width:100%;height:100%}" +
    ".ah-scrollview-page{flex:none;width:100%}" +
    ".ah-scrollbar-host{position:relative;display:block;box-sizing:border-box}" +
    ".ah-scrollbar-host:not(.ah-scrollbar-area){height:14px}" +
    ".ah-scrollbar{position:absolute;display:flex;box-sizing:border-box}" +
    ".ah-scrollbar-vertical{width:14px;flex-direction:column;right:0;top:0;bottom:0}" +
    ".ah-scrollbar-horizontal{height:14px;left:0;right:0;bottom:0}" +
    ".ah-scrollbar-btn-up,.ah-scrollbar-btn-down{flex:0 0 14px}.ah-scrollbar-thumb,.ah-scrollbar-track-up,.ah-scrollbar-track-down{flex:0 0 auto}" +
    ".ah-scrollbar-area{overflow:hidden}.ah-scrollbar-viewport{height:100%;overflow:auto}" +
    ".ah-scrollbar-area>.ah-scrollbar{display:none}.ah-scrollbar-area-v{padding-right:14px}" +
    ".ah-scrollbar-area-v>.ah-scrollbar-vertical{display:flex}" +
    ".ah-responsive-panel-collapsed .ah-responsive-panel-content{position:fixed;display:none}" +
    ".ah-responsive-panel-collapsed.ah-responsive-panel-open .ah-responsive-panel-content{display:block}";

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

  T.test("layout_scroll scrollview: keys, dots, value, hidden input, change", function (fx) {
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

  T.test("layout_scroll scrollview: drag past the threshold changes page, a short one snaps back", function (fx) {
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

  T.test("layout_scroll scrollbar: geometry, keys, buttons, track, thumb drag", async function (fx) {
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

  T.test("layout_scroll scrollbar: scroll area bars follow the viewport", async function (fx) {
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

  T.test("layout_scroll responsive panel: folds below the breakpoint, opens, closes", async function (fx) {
    var events = [];
    $(fx).on("ah:collapse ah:expand ah:open ah:close", function (e) { events.push(e.type); });
    mount(fx, '<div id="t-rp-box" style="width:300px">' + FX.rp + "</div>");
    var el = document.getElementById("t-rp"), $t = $(el).children(".ah-responsive-panel-toggle");
    T.ok($(el).hasClass("ah-responsive-panel-collapsed"), "collapsed");
    T.eq(AH.invoke(el, "isCollapsed"), true);
    $t.trigger("click");
    T.ok($(el).hasClass("ah-responsive-panel-open"), "open");
    T.eq($t.attr("aria-expanded"), "true");
    T.eq($(el).children(".ah-responsive-panel-content")[0].style.position, "fixed");
    $(el).children(".ah-responsive-panel-content").trigger("click");
    T.eq(AH.invoke(el, "isOpen"), true, "a click inside keeps it open");
    $(document.body).trigger("click");
    T.eq(AH.invoke(el, "isOpen"), false, "a click outside closes it");
    key($t, "Enter");
    T.eq(AH.invoke(el, "isOpen"), true);
    key($t, "Escape");
    T.eq(AH.invoke(el, "isOpen"), false);
    document.getElementById("t-rp-box").style.width = "600px";
    AH.invoke(el, "refresh");
    T.ok(!$(el).hasClass("ah-responsive-panel-collapsed"), "expanded");
    T.eq(events, ["ah:collapse", "ah:open", "ah:close", "ah:open", "ah:close", "ah:expand"]);
    AH.destroy(fx);
    await wait(0);
  });
})(window.AHTest, window.jQuery, window.AH);
