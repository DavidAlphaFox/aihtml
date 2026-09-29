/* Behaviour of segmented-control (segmented_control.js). The fixtures are server renders from
 * aihtml_example_demo_segmented_control, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"seg":"<div class=\"ah-segmented-control\" role=\"tablist\" data-size=\"md\" data-full-width=\"false\" data-disabled=\"false\" data-ah=\"segmented-control\" data-ah-value=\"grid\"><button class=\"ah-segmented-control__item\" type=\"button\" role=\"tab\" aria-selected=\"false\" data-value=\"list\" data-state=\"inactive\" data-disabled=\"false\" tabindex=\"-1\">List</button><button class=\"ah-segmented-control__item\" type=\"button\" role=\"tab\" aria-selected=\"true\" data-value=\"grid\" data-state=\"active\" data-disabled=\"false\" tabindex=\"0\">Grid</button><button class=\"ah-segmented-control__item\" type=\"button\" role=\"tab\" aria-selected=\"false\" data-value=\"board\" data-state=\"inactive\" data-disabled=\"false\" tabindex=\"-1\">Board</button><input type=\"hidden\" name=\"layout\" value=\"grid\" data-ah-input=\"\"></div>","dis":"<div class=\"ah-segmented-control\" role=\"tablist\" data-size=\"md\" data-full-width=\"true\" data-disabled=\"false\" data-ah=\"segmented-control\" data-ah-value=\"day\"><button class=\"ah-segmented-control__item\" type=\"button\" role=\"tab\" aria-selected=\"true\" data-value=\"day\" data-state=\"active\" data-disabled=\"false\" tabindex=\"0\">Day</button><button class=\"ah-segmented-control__item\" type=\"button\" role=\"tab\" aria-selected=\"false\" data-value=\"week\" data-state=\"inactive\" data-disabled=\"true\" tabindex=\"-1\" disabled=\"\">Week</button><button class=\"ah-segmented-control__item\" type=\"button\" role=\"tab\" aria-selected=\"false\" data-value=\"month\" data-state=\"inactive\" data-disabled=\"false\" tabindex=\"-1\">Month</button></div>"};

  async function mount(fx, html) { fx.innerHTML = html; await T.ready(fx); return fx.firstElementChild; }
  // Take the element out (its controller tears down) and put it back.
  async function reinsert(fx, el) {
    el.remove();
    await new Promise(function (r) { setTimeout(r, 0); });
    fx.appendChild(el);
    await T.ready(fx);
    return el;
  }
  function events(el, type) {
    var seen = [];
    el.addEventListener(type, function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function sleep(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function all(el, sel) { return Array.prototype.slice.call(el.querySelectorAll(sel)); }

  T.test("segmented-control: click and arrows select, change on the root", async function (fx) {
    var el = await mount(fx, FX.seg), changes = events(el, "change");
    var it = all(el, ".ah-segmented-control__item");
    it[2].click();
    T.eq(el.getAttribute("data-ah-value"), "board");
    T.eq(el.querySelector("input[type=hidden]").value, "board");
    T.eq(it[2].getAttribute("data-state"), "active");
    T.eq(it[2].getAttribute("aria-selected"), "true");
    T.eq(it[1].getAttribute("tabindex"), "-1");
    T.ok(!T.key(it[2], "ArrowRight"), "handled");
    T.eq(el.getAttribute("data-ah-value"), "list", "wraps around");
    T.eq(document.activeElement, it[0]);
    T.key(it[0], "ArrowLeft");
    T.eq(el.getAttribute("data-ah-value"), "board");
    T.eq(changes, ["board", "list", "board"]);
  });

  T.test("segmented-control: disabled items are skipped; methods; re-insertion", async function (fx) {
    var el = await mount(fx, FX.dis), changes = events(el, "change");
    var it = all(el, ".ah-segmented-control__item");
    T.fire(it[1], "click");
    T.eq(el.getAttribute("data-ah-value"), "day", "a disabled item does nothing");
    T.key(it[0], "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "month", "arrows skip it");
    AH.invoke(el, "setValue", "day");
    T.eq(AH.invoke(el, "getValue"), "day");
    T.eq(it[0].getAttribute("tabindex"), "0");
    AH.invoke(el, "setValue", "nope");
    T.eq(it[0].getAttribute("tabindex"), "0", "no match: the first enabled item is the tab stop");
    T.eq(changes, ["month"], "methods fire no change");
    await reinsert(fx, el);
    it[2].click();
    T.eq(changes, ["month", "month"], "one change after re-insertion");
  });
})(window.AHTest, window.AH);
