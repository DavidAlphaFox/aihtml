/* Sortable and dragdrop behaviours (layout_dnd.js). The fixtures are
 * server renders from aihtml_layout_dnd, captured once; regenerate them
 * if the markup changes. Drags are driven with synthetic pointer events. */
(function (T, $, AH) {
  "use strict";

  var FX = {"dd":"<div class=\"ah-dragdrop\" data-ah=\"dragdrop\" data-ah-tolerance=\"intersect\" data-ah-move id=\"t-dd\"><div class=\"ah-draggable\" data-ah-drag=\"k1\" data-ah-drag-type=\"t\" tabindex=\"0\" aria-roledescription=\"draggable\">Item</div><div class=\"ah-drop-zone\" data-ah-drop=\"z1\" data-ah-drop-accept=\"t\">Z1</div><div class=\"ah-drop-zone\" data-ah-drop=\"z2\" data-ah-drop-accept=\"other\">Z2</div><div class=\"ah-drop-zone\" data-ah-drop=\"z3\">Z3</div></div>","g1":"<div class=\"ah-sortable ah-sortable-vertical\" role=\"list\" data-ah=\"sortable\" data-ah-value=\"a,b\" data-ah-group=\"g\" id=\"t-g1\"><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"a\" tabindex=\"0\" aria-roledescription=\"sortable item\">A</div><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"b\" tabindex=\"-1\" aria-roledescription=\"sortable item\">B</div><span class=\"ah-sortable-live\" aria-live=\"assertive\" aria-atomic=\"true\"></span></div>","g2":"<div class=\"ah-sortable ah-sortable-vertical\" role=\"list\" data-ah=\"sortable\" data-ah-value=\"c\" data-ah-group=\"g\" id=\"t-g2\"><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"c\" tabindex=\"0\" aria-roledescription=\"sortable item\">C</div><span class=\"ah-sortable-live\" aria-live=\"assertive\" aria-atomic=\"true\"></span></div>","list":"<div class=\"ah-sortable ah-sortable-vertical\" role=\"list\" data-ah=\"sortable\" data-ah-value=\"a,b,c\" id=\"t-so\"><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"a\" tabindex=\"0\" aria-roledescription=\"sortable item\">A</div><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"b\" tabindex=\"-1\" aria-roledescription=\"sortable item\">B</div><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"c\" tabindex=\"-1\" aria-roledescription=\"sortable item\">C</div><input type=\"hidden\" name=\"o\" value=\"a,b,c\" data-ah-input><span class=\"ah-sortable-live\" aria-live=\"assertive\" aria-atomic=\"true\"></span></div>"};

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
  function changes(el) {
    var seen = [];
    $(el).on("change", function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function item(el, k) { return $(el).find('[data-value="' + k + '"]')[0]; }

  T.test("sortable: pointer drag reorders, updates value and hidden input, fires change", async function (fx) {
    var el = mount(fx, FX.list), seen = changes(el);
    await dragTo(item(el, "a"), function () { var r = item(el, "c").getBoundingClientRect(); return [r.left + 5, r.bottom - 2]; });
    T.eq(el.getAttribute("data-ah-value"), "b,c,a");
    T.eq($(el).find("input[type=hidden]").val(), "b,c,a");
    T.eq(seen, ["b,c,a"]);
    T.eq(document.querySelectorAll(".ah-sortable-placeholder, .ah-sortable-helper").length, 0, "cleaned up");
    T.eq(item(el, "a").getAttribute("tabindex"), "0", "dropped item is in the tab order");
  });

  T.test("sortable: Escape cancels a drag, disabled lists do not drag", async function (fx) {
    var el = mount(fx, FX.list), seen = changes(el);
    await dragTo(item(el, "a"), function () { var r = item(el, "c").getBoundingClientRect(); return [r.left + 5, r.bottom - 2]; }, false);
    T.ok(document.querySelector(".ah-sortable-helper"), "dragging");
    $(document).trigger($.Event("keydown", { key: "Escape" }));
    T.eq($(el).children(".ah-sortable-item").map(function () { return this.getAttribute("data-value"); }).get(), ["a", "b", "c"]);
    T.eq(document.querySelectorAll(".ah-sortable-placeholder, .ah-sortable-helper").length, 0);
    AH.invoke(el, "disable");
    await dragTo(item(el, "a"), function () { return mid(item(el, "c")); });
    T.eq(el.getAttribute("data-ah-value"), "a,b,c");
    T.eq(seen, []);
  });

  T.test("sortable: keyboard pick up, move, drop, cancel, Alt+arrow", function (fx) {
    var el = mount(fx, FX.list), seen = changes(el), a = item(el, "a");
    a.focus();
    key(a, "ArrowDown");
    T.ok(document.activeElement === item(el, "b"), "arrows move the focus");
    key(item(el, "b"), "ArrowUp");
    key(a, " "); key(a, "ArrowDown"); key(a, "ArrowDown");
    T.ok($(a).hasClass("ah-sortable-item-grabbed"));
    T.eq(el.getAttribute("data-ah-value"), "a,b,c", "value changes on drop only");
    T.ok(document.activeElement === a, "keeps focus");
    key(a, "Enter");
    T.eq(seen, ["b,c,a"]);
    key(a, " "); key(a, "Home"); key(a, "Escape");
    T.eq($(el).children(".ah-sortable-item").map(function () { return this.getAttribute("data-value"); }).get(), ["b", "c", "a"]);
    key(a, "ArrowUp", { altKey: true });
    T.eq(seen, ["b,c,a", "b,a,c"]);
  });

  T.test("sortable: setValue / getValue", function (fx) {
    var el = mount(fx, FX.list), seen = changes(el);
    AH.invoke(el, "setValue", ["c", "a"]);
    T.eq(AH.invoke(el, "getValue"), "c,a,b");
    AH.invoke(el, "setValue", "b,zz");
    T.eq(el.getAttribute("data-ah-value"), "b,c,a");
    T.eq($(el).find("input[type=hidden]").val(), "b,c,a");
    T.eq(seen, []);
  });

  T.test("sortable: connected lists exchange items", async function (fx) {
    var g1 = mount(fx, FX.g1), g2 = mount(fx, FX.g2), ev = [];
    $(g1).on("change ah:sort-remove", function (e) { ev.push("1:" + e.type); });
    $(g2).on("change ah:sort-receive", function (e) { ev.push("2:" + e.type); });
    await dragTo(item(g1, "a"), function () { var r = item(g2, "c").getBoundingClientRect(); return [r.left + 5, r.bottom - 2]; });
    T.eq([g1.getAttribute("data-ah-value"), g2.getAttribute("data-ah-value")], ["b", "c,a"]);
    T.eq(ev, ["1:ah:sort-remove", "2:ah:sort-receive", "1:change", "2:change"]);
  });

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
