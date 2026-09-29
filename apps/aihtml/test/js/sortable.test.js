/* Sortable behaviour (sortable.js). The fixtures are server renders from
 * aihtml_sortable, captured once; regenerate them if the markup changes.
 * Drags are driven with synthetic pointer events; everything is native
 * (T.fire / T.key, T.ready after every fixture). */
(function (T, AH) {
  "use strict";

  var FX = {"g1":"<div class=\"ah-sortable ah-sortable-vertical\" role=\"list\" data-ah=\"sortable\" data-ah-value=\"a,b\" data-ah-group=\"g\" id=\"t-g1\"><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"a\" tabindex=\"0\" aria-roledescription=\"sortable item\">A</div><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"b\" tabindex=\"-1\" aria-roledescription=\"sortable item\">B</div><span class=\"ah-sortable-live\" aria-live=\"assertive\" aria-atomic=\"true\"></span></div>","g2":"<div class=\"ah-sortable ah-sortable-vertical\" role=\"list\" data-ah=\"sortable\" data-ah-value=\"c\" data-ah-group=\"g\" id=\"t-g2\"><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"c\" tabindex=\"0\" aria-roledescription=\"sortable item\">C</div><span class=\"ah-sortable-live\" aria-live=\"assertive\" aria-atomic=\"true\"></span></div>","list":"<div class=\"ah-sortable ah-sortable-vertical\" role=\"list\" data-ah=\"sortable\" data-ah-value=\"a,b,c\" id=\"t-so\"><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"a\" tabindex=\"0\" aria-roledescription=\"sortable item\">A</div><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"b\" tabindex=\"-1\" aria-roledescription=\"sortable item\">B</div><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"c\" tabindex=\"-1\" aria-roledescription=\"sortable item\">C</div><input type=\"hidden\" name=\"o\" value=\"a,b,c\" data-ah-input><span class=\"ah-sortable-live\" aria-live=\"assertive\" aria-atomic=\"true\"></span></div>"};

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
  function changes(el) {
    var seen = [];
    el.addEventListener("change", function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function item(el, k) { return el.querySelector('[data-value="' + k + '"]'); }
  function keys(el) {
    return Array.from(el.querySelectorAll(":scope > .ah-sortable-item")).map(function (n) { return n.getAttribute("data-value"); });
  }

  T.test("sortable: pointer drag reorders, updates value and hidden input, fires change", async function (fx) {
    var el = await mount(fx, FX.list), seen = changes(el);
    await dragTo(item(el, "a"), function () { var r = item(el, "c").getBoundingClientRect(); return [r.left + 5, r.bottom - 2]; });
    T.eq(el.getAttribute("data-ah-value"), "b,c,a");
    T.eq(el.querySelector("input[type=hidden]").value, "b,c,a");
    T.eq(seen, ["b,c,a"]);
    T.eq(document.querySelectorAll(".ah-sortable-placeholder, .ah-sortable-helper").length, 0, "cleaned up");
    T.eq(item(el, "a").getAttribute("tabindex"), "0", "dropped item is in the tab order");
  });

  T.test("sortable: Escape cancels a drag, disabled lists do not drag", async function (fx) {
    var el = await mount(fx, FX.list), seen = changes(el);
    await dragTo(item(el, "a"), function () { var r = item(el, "c").getBoundingClientRect(); return [r.left + 5, r.bottom - 2]; }, false);
    T.ok(document.querySelector(".ah-sortable-helper"), "dragging");
    T.key(document, "Escape");
    T.eq(keys(el), ["a", "b", "c"]);
    T.eq(document.querySelectorAll(".ah-sortable-placeholder, .ah-sortable-helper").length, 0);
    AH.invoke(el, "disable");
    await dragTo(item(el, "a"), function () { return mid(item(el, "c")); });
    T.eq(el.getAttribute("data-ah-value"), "a,b,c");
    T.eq(seen, []);
  });

  T.test("sortable: keyboard pick up, move, drop, cancel, Alt+arrow", async function (fx) {
    var el = await mount(fx, FX.list), seen = changes(el), a = item(el, "a");
    a.focus();
    key(a, "ArrowDown");
    T.ok(document.activeElement === item(el, "b"), "arrows move the focus");
    key(item(el, "b"), "ArrowUp");
    key(a, " "); key(a, "ArrowDown"); key(a, "ArrowDown");
    T.ok(a.classList.contains("ah-sortable-item-grabbed"));
    T.eq(el.getAttribute("data-ah-value"), "a,b,c", "value changes on drop only");
    T.ok(document.activeElement === a, "keeps focus");
    key(a, "Enter");
    T.eq(seen, ["b,c,a"]);
    key(a, " "); key(a, "Home"); key(a, "Escape");
    T.eq(keys(el), ["b", "c", "a"]);
    key(a, "ArrowUp", { altKey: true });
    T.eq(seen, ["b,c,a", "b,a,c"]);
  });

  T.test("sortable: setValue / getValue", async function (fx) {
    var el = await mount(fx, FX.list), seen = changes(el);
    AH.invoke(el, "setValue", ["c", "a"]);
    T.eq(AH.invoke(el, "getValue"), "c,a,b");
    AH.invoke(el, "setValue", "b,zz");
    T.eq(el.getAttribute("data-ah-value"), "b,c,a");
    T.eq(el.querySelector("input[type=hidden]").value, "b,c,a");
    T.eq(seen, []);
  });

  T.test("sortable: connected lists exchange items", async function (fx) {
    var g1 = await mount(fx, FX.g1), g2 = await mount(fx, FX.g2), ev = [];
    ["change", "ah:sort-remove"].forEach(function (t) { g1.addEventListener(t, function (e) { ev.push("1:" + e.type); }); });
    ["change", "ah:sort-receive"].forEach(function (t) { g2.addEventListener(t, function (e) { ev.push("2:" + e.type); }); });
    await dragTo(item(g1, "a"), function () { var r = item(g2, "c").getBoundingClientRect(); return [r.left + 5, r.bottom - 2]; });
    T.eq([g1.getAttribute("data-ah-value"), g2.getAttribute("data-ah-value")], ["b", "c,a"]);
    T.eq(ev, ["1:ah:sort-remove", "2:ah:sort-receive", "1:change", "2:change"]);
  });

  T.test("sortable: sort events carry their detail; removal cancels a drag; re-inserted it works", async function (fx) {
    var el = await mount(fx, FX.list), ev = [];
    ["ah:sort-start", "ah:sort-stop"].forEach(function (t) {
      el.addEventListener(t, function (e) { ev.push([t, e.detail]); });
    });
    await dragTo(item(el, "c"), function () { var r = item(el, "a").getBoundingClientRect(); return [r.left + 5, r.top + 2]; });
    T.eq(keys(el), ["c", "a", "b"]);
    T.eq(ev, [["ah:sort-start", { key: "c", index: 2 }], ["ah:sort-stop", { key: "c", index: 0 }]]);
    await dragTo(item(el, "c"), function () { return mid(item(el, "b")); }, false);
    T.ok(document.querySelector(".ah-sortable-helper"), "dragging");
    var host = el.parentNode;
    el.remove();
    await wait(0);
    T.eq(document.querySelectorAll(".ah-sortable-placeholder, .ah-sortable-helper").length, 0, "drag cancelled");
    host.appendChild(el);
    await T.ready(host);
    var seen = changes(el);
    var c = item(el, "c");
    c.focus();
    key(c, "ArrowDown", { altKey: true });
    T.eq(seen, ["a,c,b"]);
  });

})(window.AHTest, window.AH);
