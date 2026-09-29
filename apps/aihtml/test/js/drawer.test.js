/* Drawer (components/drawer.js, the slide controller of _lib_overlay.js):
 * triggers, swipe to dismiss, Tab trap. Fixture: a reduced copy of
 * aihtml_drawer's render. */
(function (T, AH) {
  "use strict";

  var DRAWER = '<button id="ob" data-ah-open="#dr">o</button>' +
    '<div class="ah-drawer__overlay" data-ah="drawer" data-state="closed" id="dr">' +
    '<div class="ah-drawer__panel" role="dialog" aria-modal="true" data-side="bottom" data-state="closed"' +
    ' style="height:200px" tabindex="-1"><div class="ah-drawer__handle" id="hd" style="height:20px">' +
    '<span class="ah-drawer__handle-bar"></span></div><div class="ah-drawer__header">' +
    '<button id="x1" class="ah-drawer__close" type="button" data-ah-close="">x</button></div>' +
    '<div class="ah-drawer__body"><input id="in"></div>' +
    '<div class="ah-drawer__footer"><button id="ap" type="button" data-ah-close="" data-ah-result="apply">A</button></div>' +
    '</div></div>';

  async function mount(fx) { fx.innerHTML = DRAWER; await T.ready(fx); return document.getElementById("dr"); }
  function ptr(type, el, x, y, t) {
    el.dispatchEvent(new PointerEvent(type, { bubbles: true, cancelable: true, pointerId: 3,
      pointerType: "touch", clientX: x, clientY: y }));
  }

  T.test("drawer: a trigger opens it, Apply closes it with its result, focus returns", async function (fx) {
    var dr = await mount(fx), ob = document.getElementById("ob"), got = [];
    dr.addEventListener("ah:close", function (e) { got.push(e.detail); });
    ob.focus();
    ob.click();
    T.eq(dr.getAttribute("data-state"), "open");
    T.eq(document.activeElement, dr.querySelector(".ah-drawer__panel"), "panel focused");
    T.ok(parseInt(dr.style.zIndex, 10) > 21000, "on top");
    document.getElementById("ap").click();
    T.eq(dr.getAttribute("data-state"), "closed");
    T.eq(got, [{ result: "apply" }]);
    T.eq(document.activeElement, ob);
    T.ok(!document.body.classList.contains("ah-scroll-locked"));
  });

  T.test("drawer: Tab stays inside the open panel", async function (fx) {
    var dr = await mount(fx);
    AH.invoke(dr, "open");
    var ap = document.getElementById("ap"), x1 = document.getElementById("x1");
    ap.focus();
    T.key(ap, "Tab");
    T.eq(document.activeElement, x1, "wraps to the first");
    T.key(x1, "Tab", { shiftKey: true });
    T.eq(document.activeElement, ap, "wraps to the last");
    AH.invoke(dr, "close");
  });

  T.test("drawer: swiping the handle down dismisses it, a short drag snaps back", async function (fx) {
    var dr = await mount(fx), hd = document.getElementById("hd");
    AH.invoke(dr, "open");
    var r = hd.getBoundingClientRect(), x = r.left + 10, y = r.top + 5;
    var panel = dr.querySelector(".ah-drawer__panel");
    ptr("pointerdown", hd, x, y);
    T.eq(panel.getAttribute("data-dragging"), "true");
    ptr("pointermove", hd, x, y + 20);
    T.eq(panel.style.transform, "translateY(20px)");
    ptr("pointerup", hd, x, y + 20);
    T.eq(dr.getAttribute("data-state"), "open", "a short drag keeps it");
    T.eq(panel.style.transform, "");
    ptr("pointerdown", hd, x, y);
    ptr("pointermove", hd, x, y + 150);
    ptr("pointerup", hd, x, y + 150);
    T.eq(dr.getAttribute("data-state"), "closed", "past 30% closes");
    // drags from the body (an input) do not start a swipe
    AH.invoke(dr, "open");
    ptr("pointerdown", document.getElementById("in"), x, y);
    T.eq(panel.getAttribute("data-dragging"), "false");
    AH.invoke(dr, "close");
  });

  T.test("drawer: data-ah-esc=false keeps it on Escape; removing and re-inserting works", async function (fx) {
    var dr = await mount(fx);
    dr.setAttribute("data-ah-esc", "false");
    AH.invoke(dr, "open");
    T.key(document.activeElement, "Escape");
    T.eq(dr.getAttribute("data-state"), "open");
    dr.remove();
    await new Promise(function (r) { setTimeout(r, 0); });
    T.ok(!document.body.classList.contains("ah-scroll-locked"));
    dr.removeAttribute("data-ah-esc");
    dr.setAttribute("data-state", "closed");
    fx.appendChild(dr);
    await T.ready(fx);
    document.getElementById("ob").click();
    T.eq(dr.getAttribute("data-state"), "open");
    T.key(document.activeElement, "Escape");
    T.eq(dr.getAttribute("data-state"), "closed");
  });
})(window.AHTest, window.AH);
