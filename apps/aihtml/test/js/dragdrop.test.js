/* Dragdrop behaviour (dragdrop.js). The fixtures are server renders from
 * aihtml_dragdrop, captured once; regenerate them if the markup changes.
 * Drags are driven with synthetic pointer events; everything is native
 * (T.fire / T.key, T.ready after every fixture). */
(function (T, AH) {
  "use strict";

  var FX = {"dd":"<div class=\"ah-dragdrop\" data-ah=\"dragdrop\" data-ah-tolerance=\"intersect\" data-ah-move id=\"t-dd\"><div class=\"ah-draggable\" data-ah-drag=\"k1\" data-ah-drag-type=\"t\" tabindex=\"0\" aria-roledescription=\"draggable\">Item</div><div class=\"ah-drop-zone\" data-ah-drop=\"z1\" data-ah-drop-accept=\"t\">Z1</div><div class=\"ah-drop-zone\" data-ah-drop=\"z2\" data-ah-drop-accept=\"other\">Z2</div><div class=\"ah-drop-zone\" data-ah-drop=\"z3\">Z3</div></div>"};

  async function mount(fx, html) { var d = document.createElement("div"); d.innerHTML = html; fx.appendChild(d); await T.ready(d); return d.firstChild; }
  function key(el, k, extra) { T.key(el, k, extra); }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function ptr(type, el, x, y) {
    el.dispatchEvent(new PointerEvent(type, { bubbles: true, cancelable: true, pointerId: 7,
      pointerType: "mouse", button: 0, clientX: x, clientY: y }));
  }
  function mid(el) { var r = el.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2]; }
  async function dragTo(src, to, release) {
    var a = mid(src);
    ptr("pointerdown", src, a[0], a[1]);
    for (var i = 1; i <= 5; i++) {
      var b = to();
      ptr("pointermove", document, a[0] + (b[0] - a[0]) * i / 5, a[1] + (b[1] - a[1]) * i / 5);
      await wait(25);
    }
    if (release !== false) { var c = to(); ptr("pointerup", document, c[0], c[1]); }
  }

  T.test("dragdrop: pointer drop fires ah:drop with the key and the zone, move option", async function (fx) {
    var el = await mount(fx, FX.dd), got = [];
    var it = el.querySelector("[data-ah-drag]"), z1 = el.querySelector('[data-ah-drop="z1"]');
    el.addEventListener("ah:drop", function (e) { got.push([e.target.getAttribute("data-ah-drop"), e.detail]); });
    await dragTo(it, function () { return mid(z1); }, false);
    T.ok(z1.classList.contains("ah-drop-target-active"), "zone highlighted");
    T.ok(!el.querySelector('[data-ah-drop="z2"]').classList.contains("ah-drop-zone-accepting"), "z2 does not accept");
    var c = mid(z1); ptr("pointerup", document, c[0], c[1]);
    T.eq(got, [["z1", { drag: "k1", drop: "z1", from: "" }]]);
    T.eq([el.getAttribute("data-drag"), el.getAttribute("data-drop"), el.getAttribute("data-from")], ["k1", "z1", ""]);
    T.ok(it.parentNode === z1, "moved");
    T.eq(document.querySelectorAll(".ah-drag-feedback").length, 0);
    // a zone that does not accept the type is not a target
    var z2 = el.querySelector('[data-ah-drop="z2"]');
    await dragTo(it, function () { return mid(z2); });
    T.eq(got.length, 1);
    T.ok(it.parentNode === z1);
  });

  T.test("dragdrop: keyboard walks the accepting zones", async function (fx) {
    var el = await mount(fx, FX.dd), got = [];
    var it = el.querySelector("[data-ah-drag]");
    el.addEventListener("ah:drop", function (e) { got.push(e.detail); });
    it.focus();
    key(it, "Enter"); key(it, "ArrowDown"); key(it, "ArrowDown");
    T.eq(el.querySelector(".ah-drop-target-active").getAttribute("data-ah-drop"), "z3", "z2 skipped");
    key(it, "Escape");
    T.eq(el.querySelectorAll(".ah-drop-target-active").length, 0);
    key(it, " "); key(it, "ArrowUp"); key(it, " ");
    T.eq(got, [{ drag: "k1", drop: "z3", from: "" }]);
    T.ok(document.activeElement === it, "focus kept after the move");
  });

  T.test("dragdrop: drag events, disable / enable, cleanup on removal", async function (fx) {
    var el = await mount(fx, FX.dd), ev = [];
    var it = el.querySelector("[data-ah-drag]"), z3 = el.querySelector('[data-ah-drop="z3"]');
    ["ah:drag-start", "ah:drag-end", "ah:drag-cancel", "ah:drop-target-enter"].forEach(function (t) {
      el.addEventListener(t, function (e) { ev.push([t, e.detail]); });
    });
    AH.invoke(el, "disable");
    T.eq(el.getAttribute("aria-disabled"), "true");
    await dragTo(it, function () { return mid(z3); });
    T.eq(ev, [], "a disabled scope does not drag");
    AH.invoke(el, "enable");
    T.ok(!el.hasAttribute("aria-disabled"));
    await dragTo(it, function () { return mid(z3); }, false);
    T.ok(document.querySelector(".ah-drag-feedback"), "dragging");
    AH.invoke(el, "cancel");
    T.eq(document.querySelectorAll(".ah-drag-feedback").length, 0);
    var names = ev.map(function (e) { return e[0]; });
    T.eq(names[0], "ah:drag-start");
    T.ok(names.indexOf("ah:drop-target-enter") > 0, "a zone was entered");
    T.eq(names.slice(-2), ["ah:drag-end", "ah:drag-cancel"]);
    T.eq(ev[0][1], { key: "k1" });
    T.eq(it.parentNode.getAttribute("data-ah-drop"), null, "not moved");
    // removed during a drag: the drag is cancelled; re-inserted, it drags again
    await dragTo(it, function () { return mid(z3); }, false);
    var host = el.parentNode;
    el.remove();
    await wait(0);
    T.eq(document.querySelectorAll(".ah-drag-feedback").length, 0, "drag cancelled on removal");
    host.appendChild(el);
    await T.ready(host);
    var got = [];
    el.addEventListener("ah:drop", function (e) { got.push(e.detail); });
    await dragTo(it, function () { return mid(z3); });
    T.eq(got, [{ drag: "k1", drop: "z3", from: "" }]);
  });
})(window.AHTest, window.AH);
