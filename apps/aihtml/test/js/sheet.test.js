/* Sheet (components/sheet.js) driven by the declarative triggers of
 * _lib_overlay.js. Native events, T.ready after every fixture. */
(function (T, AH) {
  "use strict";

  async function html(fx, s) { fx.innerHTML = s; await T.ready(fx); }
  var SHEET = '<button id="ob" data-ah-open="#sh">o</button>' +
    '<div id="sh" class="ah-sheet__overlay" data-ah="sheet" data-state="closed">' +
    '<div class="ah-sheet__panel" data-side="right" data-state="closed" tabindex="-1">' +
    '<button id="cb" data-ah-close="">x</button><button id="rb" data-ah-close="" data-ah-result="ok">r</button></div></div>';

  T.test("sheet: opens / closes triggers drive a sheet, Escape and focus return", async function (fx) {
    await html(fx, SHEET);
    var ob = document.getElementById("ob"), sh = document.getElementById("sh");
    ob.focus();
    ob.click();
    T.eq(sh.getAttribute("data-state"), "open");
    T.ok(document.body.classList.contains("ah-scroll-locked"));
    T.key(document.activeElement, "Escape");
    T.eq(sh.getAttribute("data-state"), "closed");
    T.eq(document.activeElement, ob);
    ob.click();
    document.getElementById("cb").click();
    T.eq(sh.getAttribute("data-state"), "closed");
    T.ok(!document.body.classList.contains("ah-scroll-locked"));
  });

  T.test("sheet: events, result, scrim click, cancelable opening, methods, cleanup", async function (fx) {
    await html(fx, SHEET);
    var sh = document.getElementById("sh"), ev = [];
    ["ah:open", "ah:close"].forEach(function (t) {
      sh.addEventListener(t, function (e) { ev.push([e.type, e.detail]); });
    });
    AH.invoke(sh, "open");
    T.eq(AH.invoke(sh, "isOpen"), true);
    T.eq(sh.querySelector(".ah-sheet__panel").getAttribute("data-state"), "open");
    document.getElementById("rb").click();
    T.eq(ev, [["ah:open", null], ["ah:close", { result: "ok" }]]);
    AH.invoke(sh, "toggle");
    T.fire(sh, "mousedown");                  // the scrim
    T.eq(sh.getAttribute("data-state"), "closed");
    var veto = function (e) { e.preventDefault(); };
    sh.addEventListener("ah:opening", veto);
    AH.invoke(sh, "open");
    T.eq(sh.getAttribute("data-state"), "closed", "ah:opening prevented");
    sh.removeEventListener("ah:opening", veto);
    // removed while open: the scroll lock is released; re-inserted, it works
    AH.invoke(sh, "open");
    T.ok(document.body.classList.contains("ah-scroll-locked"));
    sh.remove();
    await new Promise(function (r) { setTimeout(r, 0); });
    T.ok(!document.body.classList.contains("ah-scroll-locked"), "unlocked on removal");
    sh.setAttribute("data-state", "closed");
    fx.appendChild(sh);
    await T.ready(fx);
    document.getElementById("ob").click();
    T.eq(sh.getAttribute("data-state"), "open");
    T.key(document.activeElement, "Escape");
    T.eq(sh.getAttribute("data-state"), "closed");
    T.ok(!document.body.classList.contains("ah-scroll-locked"));
  });
})(window.AHTest, window.AH);
