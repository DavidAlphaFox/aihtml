/* Popover and tooltip (components/popover.js, tooltip.js) positioned
 * with AH.float. Native events, T.ready after every fixture. */
(function (T, AH) {
  "use strict";

  async function html(fx, s) { fx.innerHTML = s; await T.ready(fx); }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }

  T.test("popover: popover and tooltip escape an overflow:hidden box via AH.float, arrow follows the flip", async function (fx) {
    await html(fx, '<div style="overflow:hidden;height:40px;width:200px;position:relative;margin-top:0">' +
         '<button id="pb" data-ah-toggle="#pp" style="margin-top:4px">p</button>' +
         '<div id="pp" class="ah-popover ah-popover-top" data-ah="popover" data-state="closed" data-ah-position="top">' +
         '<div class="ah-popover-arrow"></div><div class="ah-popover-content">content that is tall<br>2<br>3</div></div>' +
         '<span class="ah-tooltip-host" data-ah="tooltip" data-ah-tip-position="bottom" data-ah-tip-delay="0">' +
         '<button id="tb">t</button><span class="ah-tooltip ah-tooltip-bottom" role="tooltip">' +
         '<span class="ah-tooltip-arrow"></span><span class="ah-tooltip-content">tip</span></span></span></div>');
    window.scrollTo(0, 0);
    var pp = document.getElementById("pp"), pb = document.getElementById("pb");
    pb.click();
    T.eq(pp.style.position, "fixed");
    // no room above the button at the top of the page: flipped below
    T.eq(pp.getAttribute("data-ah-placement"), "bottom");
    T.ok(pp.classList.contains("ah-popover-bottom") && !pp.classList.contains("ah-popover-top"), "arrow class follows");
    var r = pp.getBoundingClientRect();
    T.ok(r.top >= pb.getBoundingClientRect().bottom && r.height > 40, "visible outside the clipped box");
    T.eq(document.elementFromPoint(r.left + 5, r.bottom - 5).closest("#pp"), pp);
    pb.click();
    await wait(400);
    T.eq(pp.style.position, "", "float stopped after close");
    var tb = document.getElementById("tb");
    tb.focus();
    var tip = document.querySelector(".ah-tooltip-host .ah-tooltip");
    T.eq(tip.style.position, "fixed");
    T.eq(tip.getAttribute("data-ah-placement"), "bottom");
    T.ok(tip.getBoundingClientRect().top >= tb.getBoundingClientRect().bottom, "tooltip below its trigger");
    tb.blur();
    await wait(400);
    T.eq(tip.style.display, "none");
    T.ok(!tip.hasAttribute("data-ah-placement"), "tooltip float stopped");
  });

  T.test("popover: anchor selector toggles, outside click and Escape close, modal backdrop, cleanup", async function (fx) {
    await html(fx, '<button id="an" type="button">a</button><button id="out" type="button">o</button>' +
         '<div id="pp" class="ah-popover" data-ah="popover" data-state="closed" data-ah-anchor="#an">' +
         '<div class="ah-popover-arrow"></div><div class="ah-popover-content"><button id="in" type="button">i</button></div></div>');
    var pp = document.getElementById("pp"), an = document.getElementById("an"), ev = [];
    ["ah:open", "ah:close"].forEach(function (t) {
      pp.addEventListener(t, function (e) { ev.push([e.type, e.detail]); });
    });
    an.click();
    T.eq(pp.getAttribute("data-state"), "open");
    T.eq(an.getAttribute("aria-expanded"), "true");
    document.getElementById("in").click();
    T.eq(pp.getAttribute("data-state"), "open", "a click inside keeps it");
    document.getElementById("out").click();
    T.eq(pp.getAttribute("data-state"), "closed", "a click outside closes it");
    T.eq(an.getAttribute("aria-expanded"), "false");
    an.click();
    document.getElementById("in").focus();
    T.key(document.activeElement, "Escape");
    T.eq(pp.getAttribute("data-state"), "closed");
    T.eq(document.activeElement, an, "focus back on the anchor");
    AH.invoke(pp, "close", "x");
    T.eq(ev, [["ah:open", null], ["ah:close", { result: null }], ["ah:open", null], ["ah:close", { result: null }]]);
    // modal: a backdrop, outside clicks do not close
    pp.setAttribute("data-ah-modal", "true");
    AH.invoke(pp, "open");
    T.ok(pp.previousElementSibling.classList.contains("ah-popover-modal-backdrop"));
    document.getElementById("out").click();
    T.eq(AH.invoke(pp, "isOpen"), true);
    AH.invoke(pp, "close", "done");
    T.eq(ev[ev.length - 1], ["ah:close", { result: "done" }]);
    T.eq(document.querySelectorAll(".ah-popover-modal-backdrop").length, 0);
    // removed and re-inserted: the anchor still toggles it
    pp.remove();
    await wait(0);
    pp.setAttribute("data-state", "closed");
    fx.appendChild(pp);
    await T.ready(fx);
    an.click();
    T.eq(pp.getAttribute("data-state"), "open");
    AH.invoke(pp, "close");
  });
})(window.AHTest, window.AH);
