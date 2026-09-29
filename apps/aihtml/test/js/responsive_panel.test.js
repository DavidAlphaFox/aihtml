/* responsive_panel: folding, the overlay and its events
 * (responsive_panel.js). FX holds a server render of
 * aihtml_responsive_panel (id t-rp), generated from Erlang; regenerate
 * it if the markup changes. The harness loads no stylesheet, so CSS holds
 * the few rules the geometry depends on (from sigil). */
(function (T, $, AH) {
  "use strict";

  var FX = {"rp":"<div class=\"ah-responsive-panel\" id=\"t-rp\" data-ah=\"responsive-panel\" data-breakpoint=\"400\" data-collapse-width=\"200px\" data-animation=\"none\" data-show-duration=\"200\" data-hide-duration=\"200\"><div class=\"ah-responsive-panel-toggle\" role=\"button\" tabindex=\"0\" title=\"Toggle panel\" aria-label=\"Toggle panel\" aria-expanded=\"false\" aria-controls=\"t-rp-content\">☰</div><div class=\"ah-responsive-panel-content\" id=\"t-rp-content\">nav</div></div>"};

  var CSS = ".ah-responsive-panel-collapsed .ah-responsive-panel-content{position:fixed;display:none}" +
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

  T.test("responsive_panel: folds below the breakpoint, opens, closes", async function (fx) {
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
