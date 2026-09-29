/* Window (components/window.js): open / close with triggers and results,
 * modal backdrop and Tab trap, drag by the title bar, resize handles,
 * keyboard move, collapse. Fixture: a reduced copy of aihtml_window's
 * render. */
(function (T, AH) {
  "use strict";

  function win(id, modal) {
    return '<button id="op-' + id + '" data-ah-open="#' + id + '">o</button>' +
      '<div class="ah-window ah-window-resizable" data-ah="window" data-state="closed" role="dialog" tabindex="-1"' +
      ' style="display:none;position:fixed;width:300px;height:200px;" data-ah-draggable="true"' +
      (modal ? ' data-ah-modal="true"' : '') + ' id="' + id + '">' +
      '<div class="ah-window-header ah-window-header-draggable" style="height:30px"><div class="ah-window-title">T</div>' +
      '<div class="ah-window-header-buttons"><button class="ah-window-collapse-btn" type="button" aria-expanded="true">c</button>' +
      '<button class="ah-window-close-btn" type="button" data-ah-close="">x</button></div></div>' +
      '<div class="ah-window-content"><input id="in-' + id + '"></div>' +
      '<div class="ah-window-footer"><button id="ok-' + id + '" type="button" data-ah-close="" data-ah-result="ok">ok</button></div>' +
      '<div class="ah-window-resize-handle ah-window-resize-se" data-dir="se" style="position:absolute;right:0;bottom:0;width:8px;height:8px"></div>' +
      '</div>';
  }
  async function mount(fx, id, modal) {
    fx.insertAdjacentHTML("beforeend", win(id, modal));
    await T.ready(fx);
    return document.getElementById(id);
  }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function ptr(type, el, x, y) {
    el.dispatchEvent(new PointerEvent(type, { bubbles: true, cancelable: true, pointerId: 5,
      pointerType: "mouse", button: 0, clientX: x, clientY: y }));
  }
  function log(el, types) {
    var got = [];
    types.forEach(function (t) { el.addEventListener(t, function (e) { got.push([e.type, e.detail]); }); });
    return got;
  }

  T.test("window: a trigger opens it centred, a result button closes it, focus returns", async function (fx) {
    var w = await mount(fx, "w1", true), ev = log(w, ["ah:open", "ah:close"]);
    var op = document.getElementById("op-w1");
    op.focus();
    op.click();
    T.eq(w.getAttribute("data-state"), "open");
    T.ok(w.style.display !== "none", "shown");
    T.eq(document.activeElement, w, "the window has the focus");
    T.eq(parseFloat(w.style.left), Math.max(0, (window.innerWidth - 300) / 2), "centred");
    var bd = w.previousElementSibling;
    T.ok(bd.classList.contains("ah-window-modal-backdrop"), "modal backdrop");
    T.eq(parseInt(bd.style.zIndex, 10), parseInt(w.style.zIndex, 10) - 1);
    T.ok(document.body.classList.contains("ah-scroll-locked"));
    // Tab is trapped in a modal window
    var ok = document.getElementById("ok-w1");
    ok.focus();
    T.key(ok, "Tab");
    T.ok(w.contains(document.activeElement), "focus stays inside");
    ok.click();
    T.eq(w.getAttribute("data-state"), "closed");
    T.eq(document.activeElement, op);
    T.ok(!document.body.classList.contains("ah-scroll-locked"));
    await wait(400);
    T.eq(w.style.display, "none");
    T.eq(document.querySelectorAll(".ah-window-modal-backdrop").length, 0, "backdrop removed");
    T.eq(ev, [["ah:open", null], ["ah:close", { result: "ok" }]]);
  });

  T.test("window: drag by the title bar, resize from a handle, keyboard move and resize", async function (fx) {
    var w = await mount(fx, "w2", false), ev = log(w, ["ah:moved", "ah:resize"]);
    AH.invoke(w, "open");
    AH.invoke(w, "move", 50, 60);
    T.eq([w.style.left, w.style.top], ["50px", "60px"]);
    var hd = w.querySelector(".ah-window-title");
    var r = hd.getBoundingClientRect();
    ptr("pointerdown", hd, r.left + 5, r.top + 5);
    ptr("pointermove", document, r.left + 45, r.top + 25);
    ptr("pointerup", document, r.left + 45, r.top + 25);
    T.eq([w.style.left, w.style.top], ["90px", "80px"]);
    var se = w.querySelector(".ah-window-resize-se").getBoundingClientRect();
    ptr("pointerdown", w.querySelector(".ah-window-resize-se"), se.left + 2, se.top + 2);
    ptr("pointermove", document, se.left + 52, se.top + 32);
    ptr("pointerup", document, se.left + 52, se.top + 32);
    T.eq([w.style.width, w.style.height], ["350px", "230px"]);
    // later moves of the pointer do nothing (the drag ended)
    ptr("pointermove", document, 0, 0);
    T.eq(w.style.left, "90px");
    w.focus();
    T.key(w, "ArrowRight");
    T.eq(w.style.left, "100px");
    T.key(w, "ArrowDown", { ctrlKey: true });
    T.eq(w.style.height, "240px");
    T.key(document.getElementById("in-w2"), "ArrowRight");
    T.eq(w.style.left, "100px", "inputs keep their arrows");
    T.eq(ev, [["ah:moved", { x: 50, y: 60 }], ["ah:moved", { x: 90, y: 80 }],
              ["ah:resize", { width: 350, height: 230 }], ["ah:moved", { x: 100, y: 80 }],
              ["ah:resize", { width: 350, height: 240 }]]);
    AH.invoke(w, "close");
  });

  T.test("window: collapse, bringToFront, Escape when focused, ah:closing veto, cleanup", async function (fx) {
    var w = await mount(fx, "w3", false), ev = log(w, ["ah:collapse", "ah:expand"]);
    var w4 = await mount(fx, "w4", true);
    AH.invoke(w, "open");
    w.querySelector(".ah-window-collapse-btn").click();
    T.ok(w.classList.contains("ah-window-collapsed"));
    T.eq(w.querySelector(".ah-window-collapse-btn").getAttribute("aria-expanded"), "false");
    AH.invoke(w, "expand");
    T.eq(ev.map(function (e) { return e[0]; }), ["ah:collapse", "ah:expand"]);
    AH.invoke(w4, "open");
    T.ok(parseInt(w4.style.zIndex, 10) > parseInt(w.style.zIndex, 10));
    T.fire(w, "mousedown");
    T.ok(parseInt(w.style.zIndex, 10) > parseInt(w4.style.zIndex, 10), "a click brings it to the front");
    AH.invoke(w4, "close");
    // a non-modal window closes on Escape only while it has the focus
    document.getElementById("op-w3").focus();
    T.key(document.activeElement, "Escape");
    T.eq(AH.invoke(w, "isOpen"), true);
    document.getElementById("in-w3").focus();
    var veto = function (e) { e.preventDefault(); };
    w.addEventListener("ah:closing", veto);
    T.key(document.activeElement, "Escape");
    T.eq(AH.invoke(w, "isOpen"), true, "ah:closing prevented");
    w.removeEventListener("ah:closing", veto);
    T.key(document.activeElement, "Escape");
    T.eq(AH.invoke(w, "isOpen"), false);
    // a modal window removed while open releases the scroll lock
    AH.invoke(w4, "open");
    w4.remove();
    await wait(300);                          // the first backdrop fades out
    T.ok(!document.body.classList.contains("ah-scroll-locked"));
    T.eq(document.querySelectorAll(".ah-window-modal-backdrop").length, 0);
    w4.setAttribute("data-state", "closed");
    fx.appendChild(w4);
    await T.ready(fx);
    document.getElementById("op-w4").click();
    T.eq(w4.getAttribute("data-state"), "open");
    AH.invoke(w4, "toggle");
    T.eq(w4.getAttribute("data-state"), "closed");
  });
})(window.AHTest, window.AH);
