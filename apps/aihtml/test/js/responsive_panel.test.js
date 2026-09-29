/* responsive_panel: folding, the overlay and its events
 * (responsive_panel.js). FX holds a server render of
 * aihtml_responsive_panel (id t-rp), generated from Erlang; regenerate
 * it if the markup changes. The harness loads no stylesheet, so CSS holds
 * the few rules the geometry depends on (from sigil). */
(function (T, AH) {
  "use strict";

  var FX = {"rp":"<div class=\"ah-responsive-panel\" id=\"t-rp\" data-ah=\"responsive-panel\" data-breakpoint=\"400\" data-collapse-width=\"200px\" data-animation=\"none\" data-show-duration=\"200\" data-hide-duration=\"200\"><div class=\"ah-responsive-panel-toggle\" role=\"button\" tabindex=\"0\" title=\"Toggle panel\" aria-label=\"Toggle panel\" aria-expanded=\"false\" aria-controls=\"t-rp-content\">☰</div><div class=\"ah-responsive-panel-content\" id=\"t-rp-content\">nav</div></div>"};

  var CSS = ".ah-responsive-panel-collapsed .ah-responsive-panel-content{position:fixed;display:none}" +
    ".ah-responsive-panel-collapsed.ah-responsive-panel-open .ah-responsive-panel-content{display:block}";

  async function mount(fx, html) {
    if (!document.getElementById("t-rp-css")) {
      var s = document.createElement("style");
      s.id = "t-rp-css";
      s.textContent = CSS;
      document.head.appendChild(s);
    }
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }

  T.test("responsive_panel: folds below the breakpoint, opens, closes", async function (fx) {
    var events = [];
    ["ah:collapse", "ah:expand", "ah:open", "ah:close"].forEach(function (t) {
      fx.addEventListener(t, function h(e) { events.push(e.type); });
    });
    await mount(fx, '<div id="t-rp-box" style="width:300px">' + FX.rp + "</div>");
    var el = document.getElementById("t-rp");
    var t = el.querySelector(":scope > .ah-responsive-panel-toggle");
    var content = el.querySelector(":scope > .ah-responsive-panel-content");
    T.ok(el.classList.contains("ah-responsive-panel-collapsed"), "collapsed");
    T.eq(AH.invoke(el, "isCollapsed"), true);
    t.click();
    T.ok(el.classList.contains("ah-responsive-panel-open"), "open");
    T.eq(t.getAttribute("aria-expanded"), "true");
    T.eq(content.style.position, "fixed");
    content.click();
    T.eq(AH.invoke(el, "isOpen"), true, "a click inside keeps it open");
    document.body.click();
    T.eq(AH.invoke(el, "isOpen"), false, "a click outside closes it");
    T.key(t, "Enter");
    T.eq(AH.invoke(el, "isOpen"), true);
    T.key(t, "Escape");
    T.eq(AH.invoke(el, "isOpen"), false);
    document.getElementById("t-rp-box").style.width = "600px";
    AH.invoke(el, "refresh");
    T.ok(!el.classList.contains("ah-responsive-panel-collapsed"), "expanded");
    T.eq(events, ["ah:collapse", "ah:open", "ah:close", "ah:open", "ah:close", "ah:expand"]);
    AH.destroy(fx);
    await wait(0);
  });

  T.test("responsive_panel: fade animation, an external toggle, cleanup", async function (fx) {
    var html = FX.rp.replace('data-animation="none"', 'data-animation="fade"')
      .replace(/data-(show|hide)-duration="200"/g, 'data-$1-duration="40"')
      .replace('data-breakpoint="400"', 'data-breakpoint="400" data-toggle-button="#t-rp-ext"');
    await mount(fx, '<button id="t-rp-ext" type="button">ext</button>' +
                '<div id="t-rp-box" style="width:300px">' + html + "</div>");
    var el = document.getElementById("t-rp"), ext = document.getElementById("t-rp-ext");
    var content = el.querySelector(":scope > .ah-responsive-panel-content");
    var opens = 0;
    el.addEventListener("ah:open", function () { opens++; });
    ext.click();
    T.eq(AH.invoke(el, "isOpen"), true, "the external button opens it");
    await wait(100);
    T.eq(opens, 1, "ah:open when the fade ends");
    T.eq(getComputedStyle(content).opacity, "1");
    ext.click();
    T.eq(AH.invoke(el, "isOpen"), false, "and closes it (not an outside click)");
    await wait(100);
    T.ok(!el.classList.contains("ah-responsive-panel-open"), "open class gone after the fade");
    T.eq(content.style.display, "", "inline display cleared");
    // taken out and put back: one set of listeners
    var box = el.parentNode;
    box.removeChild(el);
    await wait(0);
    box.appendChild(el);
    await T.ready(fx);
    ext.click();
    T.eq(AH.invoke(el, "isOpen"), true, "open again, one toggle listener");
    AH.invoke(el, "close");
    await wait(100);
  });
})(window.AHTest, window.AH);
