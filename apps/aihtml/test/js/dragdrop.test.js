/* Dragdrop behaviour (dragdrop.js). The fixtures are server renders from
 * aihtml_dragdrop, captured once; regenerate them if the markup changes.
 * Drags are driven with synthetic pointer events. */
(function (T, $, AH) {
  "use strict";

  var FX = {"dd":"<div class=\"ah-dragdrop\" data-ah=\"dragdrop\" data-ah-tolerance=\"intersect\" data-ah-move id=\"t-dd\"><div class=\"ah-draggable\" data-ah-drag=\"k1\" data-ah-drag-type=\"t\" tabindex=\"0\" aria-roledescription=\"draggable\">Item</div><div class=\"ah-drop-zone\" data-ah-drop=\"z1\" data-ah-drop-accept=\"t\">Z1</div><div class=\"ah-drop-zone\" data-ah-drop=\"z2\" data-ah-drop-accept=\"other\">Z2</div><div class=\"ah-drop-zone\" data-ah-drop=\"z3\">Z3</div></div>"};

  function mount(fx, html) { var d = document.createElement("div"); d.innerHTML = html; fx.appendChild(d); AH.mount(d); return d.firstChild; }
  function key(el, k, extra) { $(el).trigger($.Event("keydown", $.extend({ key: k }, extra || {}))); }
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
    var el = mount(fx, FX.dd), got = [];
    var it = $(el).find("[data-ah-drag]")[0], z1 = $(el).find('[data-ah-drop="z1"]')[0];
    $(el).on("ah:drop", function (e, d) { got.push([e.target.getAttribute("data-ah-drop"), d]); });
    await dragTo(it, function () { return mid(z1); }, false);
    T.ok($(z1).hasClass("ah-drop-target-active"), "zone highlighted");
    T.ok(!$(el).find('[data-ah-drop="z2"]').hasClass("ah-drop-zone-accepting"), "z2 does not accept");
    var c = mid(z1); ptr("pointerup", document, c[0], c[1]);
    T.eq(got, [["z1", { drag: "k1", drop: "z1", from: "" }]]);
    T.eq([el.getAttribute("data-drag"), el.getAttribute("data-drop"), el.getAttribute("data-from")], ["k1", "z1", ""]);
    T.ok(it.parentNode === z1, "moved");
    T.eq(document.querySelectorAll(".ah-drag-feedback").length, 0);
    // a zone that does not accept the type is not a target
    var z2 = $(el).find('[data-ah-drop="z2"]')[0];
    await dragTo(it, function () { return mid(z2); });
    T.eq(got.length, 1);
    T.ok(it.parentNode === z1);
  });

  T.test("dragdrop: keyboard walks the accepting zones", function (fx) {
    var el = mount(fx, FX.dd), got = [];
    var it = $(el).find("[data-ah-drag]")[0];
    $(el).on("ah:drop", function (e, d) { got.push(d); });
    it.focus();
    key(it, "Enter"); key(it, "ArrowDown"); key(it, "ArrowDown");
    T.eq($(el).find(".ah-drop-target-active").attr("data-ah-drop"), "z3", "z2 skipped");
    key(it, "Escape");
    T.eq($(el).find(".ah-drop-target-active").length, 0);
    key(it, " "); key(it, "ArrowUp"); key(it, " ");
    T.eq(got, [{ drag: "k1", drop: "z3", from: "" }]);
    T.ok(document.activeElement === it, "focus kept after the move");
  });
})(window.AHTest, window.jQuery, window.AH);
