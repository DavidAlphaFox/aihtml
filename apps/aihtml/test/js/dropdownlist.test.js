/* Dropdownlist behaviour (dropdownlist.js). The fixtures are server
 * renders from aihtml_dropdownlist (the demo module
 * aihtml_example_demo_dropdownlist), captured once; regenerate them if
 * the markup changes. Events are native; every test awaits T.ready. */
(function (T, AH) {
  "use strict";

  var FX = {"dd": "<div class=\"ah-dropdownlist w-48\" role=\"combobox\" tabindex=\"0\" aria-haspopup=\"listbox\" aria-expanded=\"false\" data-ah=\"dropdownlist\" data-ah-value=\"banana\" data-ah-placeholder=\"Select\u2026\"><div class=\"ah-dropdownlist-input-area\"><span class=\"ah-dropdownlist-content\">Banana</span><span class=\"ah-dropdownlist-arrow\" aria-hidden=\"true\"><span class=\"ah-dropdownlist-arrow-icon\">\u25bc</span></span></div><div class=\"ah-dropdownlist-popup\"><div class=\"ah-listbox\"><div class=\"ah-listbox-content\" style=\"max-height:200px\"><ul class=\"ah-listbox-list\" role=\"listbox\"><li class=\"ah-listbox-item\" role=\"option\" data-idx=\"0\" data-value=\"apple\" aria-selected=\"false\"><span class=\"ah-listbox-label\">Apple</span></li><li class=\"ah-listbox-item ah-listbox-item-selected\" role=\"option\" data-idx=\"1\" data-value=\"banana\" aria-selected=\"true\"><span class=\"ah-listbox-label\">Banana</span></li><li class=\"ah-listbox-item\" role=\"option\" data-idx=\"2\" data-value=\"cherry\" aria-selected=\"false\"><span class=\"ah-listbox-label\">Cherry</span></li><li class=\"ah-listbox-item\" role=\"option\" data-idx=\"3\" data-value=\"grape\" aria-selected=\"false\"><span class=\"ah-listbox-label\">Grape</span></li><li class=\"ah-listbox-item\" role=\"option\" data-idx=\"4\" data-value=\"lemon\" aria-selected=\"false\"><span class=\"ah-listbox-label\">Lemon</span></li><li class=\"ah-listbox-item\" role=\"option\" data-idx=\"5\" data-value=\"mango\" aria-selected=\"false\"><span class=\"ah-listbox-label\">Mango</span></li><li class=\"ah-listbox-item\" role=\"option\" data-idx=\"6\" data-value=\"orange\" aria-selected=\"false\"><span class=\"ah-listbox-label\">Orange</span></li><li class=\"ah-listbox-item\" role=\"option\" data-idx=\"7\" data-value=\"peach\" aria-selected=\"false\"><span class=\"ah-listbox-label\">Peach</span></li></ul></div></div></div><input type=\"hidden\" name=\"fruit\" value=\"banana\"></div>", "ddf": "<div class=\"ah-dropdownlist w-56\" role=\"combobox\" tabindex=\"0\" aria-haspopup=\"listbox\" aria-expanded=\"false\" data-ah=\"dropdownlist\" data-ah-value=\"rasp\" data-ah-placeholder=\"Select\u2026\"><div class=\"ah-dropdownlist-input-area\"><span class=\"ah-dropdownlist-content\">Raspberry</span><span class=\"ah-dropdownlist-arrow\" aria-hidden=\"true\"><span class=\"ah-dropdownlist-arrow-icon\">\u25bc</span></span></div><div class=\"ah-dropdownlist-popup\"><div class=\"ah-listbox\"><div class=\"ah-listbox-filter\"><input class=\"ah-listbox-filter-input\" type=\"text\" autocomplete=\"off\" aria-label=\"Filter\" placeholder=\"Search\u2026\"></div><div class=\"ah-listbox-content\" style=\"max-height:200px\"><ul class=\"ah-listbox-list\" role=\"listbox\"><li class=\"ah-listbox-group\" role=\"presentation\">Citrus</li><li class=\"ah-listbox-item\" role=\"option\" data-idx=\"0\" data-value=\"lemon\" aria-selected=\"false\"><span class=\"ah-listbox-label\">Lemon</span></li><li class=\"ah-listbox-item\" role=\"option\" data-idx=\"1\" data-value=\"orange\" aria-selected=\"false\"><span class=\"ah-listbox-label\">Orange</span></li><li class=\"ah-listbox-group\" role=\"presentation\">Berries</li><li class=\"ah-listbox-item\" role=\"option\" data-idx=\"2\" data-value=\"straw\" aria-selected=\"false\"><span class=\"ah-listbox-label\">Strawberry</span></li><li class=\"ah-listbox-item ah-listbox-item-disabled\" role=\"option\" data-idx=\"3\" data-value=\"blue\" aria-selected=\"false\" aria-disabled=\"true\"><span class=\"ah-listbox-label\">Blueberry</span></li><li class=\"ah-listbox-item ah-listbox-item-selected\" role=\"option\" data-idx=\"4\" data-value=\"rasp\" aria-selected=\"true\"><span class=\"ah-listbox-label\">Raspberry</span></li></ul></div></div></div><input type=\"hidden\" name=\"berry\" value=\"rasp\"></div>"};

  async function mount(fx, html) { fx.innerHTML = html; await T.ready(fx); return fx.firstChild; }
  function changes(el) {
    var seen = [];
    el.addEventListener("change", function (e) {
      if (e.target === el) { seen.push([el.getAttribute("data-ah-value"), e.detail && e.detail.label]); }
    });
    return seen;
  }
  function q(el, s) { return el.querySelector(s); }
  function qa(el, s) { return Array.prototype.slice.call(el.querySelectorAll(s)); }
  function visible(n) { return n.getClientRects().length > 0; }
  function content(el) { return q(el, ".ah-dropdownlist-content").textContent; }
  function active(el) { var a = q(el, ".ah-listbox-item-focused"); return a ? a.getAttribute("data-value") : null; }

  T.test("dropdownlist: click opens, click an item selects, change carries {value, label}", async function (fx) {
    var el = await mount(fx, FX.dd), seen = changes(el), events = [];
    el.addEventListener("ah:open", function () { events.push("open"); });
    el.addEventListener("ah:close", function () { events.push("close"); });
    T.eq(el.getAttribute("aria-controls"), q(el, ".ah-listbox-list").id);
    q(el, ".ah-dropdownlist-input-area").click();
    T.ok(el.classList.contains("ah-dropdownlist-open"), "opens");
    T.eq(el.getAttribute("aria-expanded"), "true");
    T.eq(active(el), "banana", "the selected item is active");
    T.eq(el.getAttribute("aria-activedescendant"), q(el, ".ah-listbox-item-selected").id);
    q(el, '.ah-listbox-item[data-value="grape"]').click();
    T.ok(!el.classList.contains("ah-dropdownlist-open"), "closes");
    T.eq(el.getAttribute("data-ah-value"), "grape");
    T.eq(q(el, "input[type=hidden]").value, "grape");
    T.eq(content(el), "Grape");
    T.eq(q(el, '.ah-listbox-item[data-value="grape"]').getAttribute("aria-selected"), "true");
    T.eq(seen, [["grape", "Grape"]]);
    T.eq(events, ["open", "close"]);
  });

  T.test("dropdownlist: keyboard open, move, Enter; type-ahead when closed", async function (fx) {
    var el = await mount(fx, FX.dd), seen = changes(el);
    el.focus();
    T.key(el, "ArrowDown");
    T.ok(el.classList.contains("ah-dropdownlist-open"));
    T.key(el, "ArrowDown"); T.key(el, "ArrowDown");
    T.eq(active(el), "grape");
    T.key(el, "End");
    T.eq(active(el), "peach");
    T.key(el, "Home");
    T.eq(active(el), "apple");
    T.key(el, "Enter");
    T.ok(!el.classList.contains("ah-dropdownlist-open"));
    T.eq(el.getAttribute("data-ah-value"), "apple");
    T.key(el, "m");
    T.eq(el.getAttribute("data-ah-value"), "mango", "a letter selects the next item");
    T.key(el, "Escape");
    T.key(el, "F4");
    T.ok(el.classList.contains("ah-dropdownlist-open"), "F4 opens");
    T.key(el, "Escape");
    T.ok(!el.classList.contains("ah-dropdownlist-open"), "Escape closes");
    T.eq(seen, [["apple", "Apple"], ["mango", "Mango"]]);
  });

  T.test("dropdownlist: filter hides rows and empty groups, skips disabled", async function (fx) {
    var el = await mount(fx, FX.ddf), seen = changes(el);
    AH.invoke(el, "open");
    var f = q(el, ".ah-listbox-filter-input");
    T.eq(document.activeElement, f, "the filter takes the focus");
    var inputs = 0;
    fx.addEventListener("input", function () { inputs++; });
    f.value = "berry"; T.fire(f, "input");
    T.eq(inputs, 0, "the filter's input stays inside");
    T.eq(qa(el, ".ah-listbox-group").filter(visible).map(function (g) { return g.textContent; }), ["Berries"]);
    T.eq(qa(el, ".ah-listbox-item").filter(visible).length, 3);
    T.eq(active(el), "straw");
    T.key(f, "ArrowDown");                 // Blueberry is disabled
    T.eq(active(el), "rasp");
    T.key(f, "ArrowUp");
    T.key(f, "Enter");
    T.eq(el.getAttribute("data-ah-value"), "straw");
    T.eq(f.value, "", "closing clears the filter");
    T.eq(qa(el, ".ah-listbox-item").filter(function (i) { return i.style.display === "none"; }).length, 0);
    T.eq(seen, [["straw", "Strawberry"]]);
  });

  T.test("dropdownlist: methods (setValue, silent, disable/enable)", async function (fx) {
    var el = await mount(fx, FX.dd), seen = changes(el);
    AH.invoke(el, "setValue", "cherry", true);
    T.eq(AH.invoke(el, "getValue"), "cherry");
    T.eq(content(el), "Cherry");
    T.eq(seen, [], "silent");
    AH.invoke(el, "setValue", null);
    T.eq(content(el), "Select…");
    T.ok(q(el, ".ah-dropdownlist-content").classList.contains("ah-dropdownlist-content-placeholder"));
    T.eq(seen, [["", null]]);
    AH.invoke(el, "disable");
    T.eq(el.getAttribute("tabindex"), "-1");
    q(el, ".ah-dropdownlist-input-area").click();
    T.ok(!el.classList.contains("ah-dropdownlist-open"), "disabled does not open");
    AH.invoke(el, "enable");
    q(el, ".ah-dropdownlist-input-area").click();
    T.ok(el.classList.contains("ah-dropdownlist-open"));
    T.fire(document.body, "mousedown");
    T.ok(!el.classList.contains("ah-dropdownlist-open"), "outside mousedown closes");
  });

  T.test("dropdownlist: a change reaches on(change, ...) bindings; removed and re-inserted, it still works", async function (fx) {
    var el = await mount(fx, FX.dd);
    AH.invoke(el, "open");
    el.remove();
    await new Promise(function (r) { setTimeout(r, 0); });   // teardown ran
    T.ok(!el.classList.contains("ah-dropdownlist-open"), "teardown closes");
    fx.appendChild(el);
    await T.ready(fx);
    var got = null;
    document.addEventListener("change", function h(e) {
      if (e.target === el) { got = e.detail; document.removeEventListener("change", h); }
    });
    q(el, ".ah-dropdownlist-input-area").click();
    q(el, '.ah-listbox-item[data-value="lemon"]').click();
    T.eq(got, { value: "lemon", label: "Lemon" });
  });
})(window.AHTest, window.AH);
