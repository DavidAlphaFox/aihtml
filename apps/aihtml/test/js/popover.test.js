/* Popover and tooltip (components/popover.js, tooltip.js) positioned
 * with AH.float. */
(function (T, $, AH) {
  "use strict";

  function html(fx, s) { fx.innerHTML = s; AH.mount(fx); }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }


  T.test("popover and tooltip escape an overflow:hidden box via AH.float, arrow follows the flip", async function (fx) {
    html(fx, '<div style="overflow:hidden;height:40px;width:200px;position:relative;margin-top:0">' +
         '<button id="pb" data-ah-toggle="#pp" style="margin-top:4px">p</button>' +
         '<div id="pp" class="ah-popover ah-popover-top" data-ah="popover" data-state="closed" data-ah-position="top">' +
         '<div class="ah-popover-arrow"></div><div class="ah-popover-content">content that is tall<br>2<br>3</div></div>' +
         '<span class="ah-tooltip-host" data-ah="tooltip" data-ah-tip-position="bottom" data-ah-tip-delay="0">' +
         '<button id="tb">t</button><span class="ah-tooltip ah-tooltip-bottom" role="tooltip">' +
         '<span class="ah-tooltip-arrow"></span><span class="ah-tooltip-content">tip</span></span></span></div>');
    window.scrollTo(0, 0);
    var pp = document.getElementById("pp"), pb = document.getElementById("pb");
    $(pb).trigger("click");
    T.eq(pp.style.position, "fixed");
    // no room above the button at the top of the page: flipped below
    T.eq(pp.getAttribute("data-ah-placement"), "bottom");
    T.ok($(pp).hasClass("ah-popover-bottom") && !$(pp).hasClass("ah-popover-top"), "arrow class follows");
    var r = pp.getBoundingClientRect();
    T.ok(r.top >= pb.getBoundingClientRect().bottom && r.height > 40, "visible outside the clipped box");
    T.eq(document.elementFromPoint(r.left + 5, r.bottom - 5).closest("#pp"), pp);
    $(pb).trigger("click");
    await wait(400);
    T.eq(pp.style.position, "", "float stopped after close");
    var tb = document.getElementById("tb");
    tb.focus();
    var tip = $(".ah-tooltip-host .ah-tooltip")[0];
    T.eq(tip.style.position, "fixed");
    T.eq(tip.getAttribute("data-ah-placement"), "bottom");
    T.ok(tip.getBoundingClientRect().top >= tb.getBoundingClientRect().bottom, "tooltip below its trigger");
    tb.blur();
    await wait(400);
    T.eq(tip.style.display, "none");
    T.ok(!tip.hasAttribute("data-ah-placement"), "tooltip float stopped");
  });
})(window.AHTest, window.jQuery, window.AH);
