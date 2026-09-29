/* Tooltip (components/tooltip.js): [data-ah="tooltip"] wrappers and
 * [data-ah-tooltip] elements, driven by native mouse, focus and click
 * events. */
(function (T, AH) {
  "use strict";

  async function html(fx, s) { fx.innerHTML = s; await T.ready(fx); }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function over(el, from) { T.fire(el, "mouseover", { relatedTarget: from || null, clientX: 5, clientY: 5 }); }
  function out(el, to) { T.fire(el, "mouseout", { relatedTarget: to || null }); }

  T.test("tooltip: hovering a [data-ah-tooltip] element shows an escaped tip, leaving removes it", async function (fx) {
    await html(fx, '<button id="sv" data-ah-tooltip="<b>Save</b>" data-ah-tip-delay="0" data-ah-tip-position="top">S</button>' +
               '<p id="far">far</p>');
    var sv = document.getElementById("sv"), ev = [];
    ["ah:open", "ah:close"].forEach(function (t) { sv.addEventListener(t, function (e) { ev.push(e.type); }); });
    over(sv);
    await wait(20);
    var id = sv.getAttribute("aria-describedby");
    var tip = document.getElementById(id);
    T.ok(tip && tip.parentNode === document.body, "tip in <body>");
    T.eq(tip.querySelector(".ah-tooltip-content").textContent, "<b>Save</b>");
    T.eq(tip.querySelector("b"), null);
    T.eq(tip.style.position, "fixed");
    out(sv, document.getElementById("far"));
    T.ok(!sv.hasAttribute("aria-describedby"));
    await wait(400);
    T.ok(!document.body.contains(tip), "created tip removed");
    T.eq(ev, ["ah:open", "ah:close"]);
  });

  T.test("tooltip: moving inside the host keeps it; Escape closes; auto-hide", async function (fx) {
    await html(fx, '<span id="h" class="ah-tooltip-host" data-ah="tooltip" data-ah-tip-delay="0" data-ah-tip-hide-delay="150">' +
               '<button id="b">b</button><span class="ah-tooltip" role="tooltip"><span class="ah-tooltip-arrow"></span>' +
               '<span class="ah-tooltip-content">tip</span></span></span>');
    var h = document.getElementById("h"), b = document.getElementById("b");
    var tip = h.querySelector(".ah-tooltip");
    over(b);
    await wait(20);
    T.eq(tip.style.display, "block");
    out(b, h);                                // still inside the host
    over(h, b);
    T.eq(h.getAttribute("aria-describedby"), tip.id);
    T.key(document.body, "Escape");
    T.ok(!h.hasAttribute("aria-describedby"), "Escape closes it");
    await wait(300);
    T.eq(tip.style.display, "none");
    AH.invoke(h, "open");
    T.eq(tip.style.display, "block");
    await wait(400);
    T.eq(tip.style.display, "none", "auto-hidden after hide-delay");
  });

  T.test("tooltip: click trigger toggles, a click elsewhere closes; setContent; cleanup", async function (fx) {
    await html(fx, '<span id="h" class="ah-tooltip-host" data-ah="tooltip" data-ah-tip-trigger="click" data-ah-tip-auto-hide="false">' +
               '<button id="b">b</button><span class="ah-tooltip" role="tooltip"><span class="ah-tooltip-arrow"></span>' +
               '<span class="ah-tooltip-content">tip</span></span></span><button id="o">o</button>');
    var h = document.getElementById("h"), b = document.getElementById("b");
    b.click();
    T.ok(h.hasAttribute("aria-describedby"), "opened by a click");
    b.click();
    T.ok(!h.hasAttribute("aria-describedby"), "closed by a second click");
    b.click();
    document.getElementById("o").click();
    T.ok(!h.hasAttribute("aria-describedby"), "closed by a click elsewhere");
    AH.invoke(h, "setContent", "<i>new</i>");
    T.eq(h.querySelector(".ah-tooltip-content").textContent, "<i>new</i>");
    AH.invoke(h, "toggle");
    T.ok(h.hasAttribute("aria-describedby"));
    // removed while open: closed at once; re-inserted, it works again
    h.remove();
    await wait(0);
    T.eq(h.querySelector(".ah-tooltip").style.display, "none");
    fx.appendChild(h);
    await T.ready(fx);
    b.click();
    T.ok(h.hasAttribute("aria-describedby"));
    AH.invoke(h, "close");
  });
})(window.AHTest, window.AH);
