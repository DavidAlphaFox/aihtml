/* listmenu: the listmenu behaviour on the markup the server renders.
 * SERVER holds renders of the aihtml_example_demo_listmenu demos, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "lmf": "<div class=\"w-72 border border-line rounded\"><div class=\"ah-listmenu\" data-ah=\"listmenu\" tabindex=\"0\" data-ah-stack=\"\" data-ah-animation=\"fade\"><div class=\"ah-listmenu-header\"><button class=\"ah-listmenu-back\" type=\"button\" tabindex=\"-1\" style=\"display:none\"><span class=\"ah-listmenu-back-arrow\" aria-hidden=\"true\">◀</span><span class=\"ah-listmenu-back-label\">Back</span></button><span class=\"ah-listmenu-title\"></span></div><div class=\"ah-listmenu-filter\"><input class=\"ah-listmenu-filter-input\" type=\"text\" tabindex=\"-1\" placeholder=\"Filter food\" aria-label=\"Filter food\"></div><div class=\"ah-listmenu-viewport\"><ul class=\"ah-listmenu-page\" data-page-id=\"root\" role=\"menu\"><li class=\"ah-listmenu-item\" data-item-id=\"0\" data-key=\"fruit\" role=\"menuitem\" aria-haspopup=\"true\"><span class=\"ah-listmenu-item-label\">Fruit</span><span class=\"ah-listmenu-arrow\" aria-hidden=\"true\">›</span></li><li class=\"ah-listmenu-item\" data-item-id=\"1\" data-key=\"veg\" role=\"menuitem\" aria-haspopup=\"true\"><span class=\"ah-listmenu-item-label\">Vegetables</span><span class=\"ah-listmenu-arrow\" aria-hidden=\"true\">›</span></li><li class=\"ah-listmenu-item\" data-item-id=\"2\" data-key=\"bread\" role=\"menuitemradio\" aria-checked=\"false\"><span class=\"ah-listmenu-item-label\">Bread</span></li><li class=\"ah-listmenu-item ah-listmenu-item-disabled\" data-item-id=\"3\" data-key=\"cake\" role=\"menuitemradio\" aria-checked=\"false\" aria-disabled=\"true\"><span class=\"ah-listmenu-item-label\">Cake</span></li></ul><ul class=\"ah-listmenu-page\" data-page-id=\"0\" role=\"menu\" style=\"display:none\"><li class=\"ah-listmenu-item\" data-item-id=\"4\" data-key=\"apple\" role=\"menuitemradio\" aria-checked=\"false\"><span class=\"ah-listmenu-item-label\">Apple</span></li><li class=\"ah-listmenu-item\" data-item-id=\"5\" data-key=\"banana\" role=\"menuitemradio\" aria-checked=\"false\"><span class=\"ah-listmenu-item-label\">Banana</span></li><li class=\"ah-listmenu-item\" data-item-id=\"6\" data-key=\"citrus\" role=\"menuitem\" aria-haspopup=\"true\"><span class=\"ah-listmenu-item-label\">Citrus</span><span class=\"ah-listmenu-arrow\" aria-hidden=\"true\">›</span></li></ul><ul class=\"ah-listmenu-page\" data-page-id=\"6\" role=\"menu\" style=\"display:none\"><li class=\"ah-listmenu-item\" data-item-id=\"7\" data-key=\"lemon\" role=\"menuitemradio\" aria-checked=\"false\"><span class=\"ah-listmenu-item-label\">Lemon</span></li><li class=\"ah-listmenu-item\" data-item-id=\"8\" data-key=\"orange\" role=\"menuitemradio\" aria-checked=\"false\"><span class=\"ah-listmenu-item-label\">Orange</span></li></ul><ul class=\"ah-listmenu-page\" data-page-id=\"1\" role=\"menu\" style=\"display:none\"><li class=\"ah-listmenu-item\" data-item-id=\"9\" data-key=\"carrot\" role=\"menuitemradio\" aria-checked=\"false\"><span class=\"ah-listmenu-item-label\">Carrot</span></li><li class=\"ah-listmenu-item\" data-item-id=\"10\" data-key=\"pea\" role=\"menuitemradio\" aria-checked=\"false\"><span class=\"ah-listmenu-item-label\">Pea</span></li></ul></div></div></div>"
  };

  async function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    await T.ready(fx);
    return fx.querySelector("[data-ah]");
  }

  // the value (or the event's detail) each time type fires on el itself
  function events(el, type) {
    var got = [];
    el.addEventListener(type, function (e) {
      if (e.target === el) { got.push(e.detail == null ? el.getAttribute("data-ah-value") : e.detail); }
    });
    return got;
  }

  function q(el, sel) { return el.querySelector(sel); }
  function qa(el, sel) { return Array.prototype.slice.call(el.querySelectorAll(sel)); }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms || 0); }); }

  // take el out of the page (its controller tears down) and put it back
  async function reinsert(fx, el) {
    fx.removeChild(el);
    await wait(0);
    fx.appendChild(el);
    await T.ready(fx);
  }

  function page(el) { return AH.invoke(el, "currentPage"); }
  function item(el, key) { return q(el, ".ah-listmenu-item[data-key=" + key + "]"); }
  function focused(el) {
    var f = q(el, ".ah-listmenu-item-focus");
    return f ? f.getAttribute("data-key") : null;
  }
  async function mountLm(fx, name) {
    fx.innerHTML = SERVER[name];
    await T.ready(fx);
    return fx.querySelector("[data-ah=listmenu]");
  }

  T.test("listmenu: drilling down and back, ah:navigate, a leaf sets the value", async function (fx) {
    var el = await mountLm(fx, "lmf");
    var changes = events(el, "change"), navs = [];
    el.addEventListener("ah:navigate", function (e) { navs.push(e.detail); });
    item(el, "fruit").click();
    await wait(350);
    T.eq(page(el), "0");
    T.eq(q(el, ".ah-listmenu-title").textContent, "Fruit");
    T.eq(q(el, ".ah-listmenu-back").style.display, "");
    T.eq(navs, [{ id: "0", label: "Fruit", page: "0" }]);
    q(item(el, "banana"), ".ah-listmenu-item-label").click();
    T.eq(el.getAttribute("data-ah-value"), "banana");
    T.eq(item(el, "banana").getAttribute("aria-checked"), "true");
    item(el, "banana").click();
    T.eq(changes, ["banana"], "the same item again: no change");
    q(el, ".ah-listmenu-back").click();
    await wait(350);
    T.eq(page(el), "root");
    T.eq(q(el, ".ah-listmenu-back").style.display, "none");
    item(el, "cake").click();
    T.eq(el.getAttribute("data-ah-value"), "banana", "disabled item");
  });

  T.test("listmenu: keyboard and the filter", async function (fx) {
    var el = await mountLm(fx, "lmf");
    var changes = events(el, "change");
    el.focus();
    T.eq(focused(el), "fruit");
    T.key(el, "ArrowDown");
    T.key(el, "ArrowDown");
    T.eq(focused(el), "bread");
    T.key(el, "Enter");
    T.eq(el.getAttribute("data-ah-value"), "bread");
    T.key(el, "Home");
    T.key(el, "ArrowRight");
    await wait(350);
    T.eq(page(el), "0");
    T.eq(focused(el), "apple", "focus goes into the new page");
    T.key(el, "Escape");
    await wait(350);
    T.eq(page(el), "root");
    var input = q(el, ".ah-listmenu-filter-input");
    var bubbled = 0;
    fx.addEventListener("input", function () { bubbled++; });
    input.value = "VEG";
    T.fire(input, "input");
    T.eq(qa(el, "[data-page-id=root] > .ah-listmenu-item").map(function (i) { return i.style.display; }),
         ["none", "", "none", "none"]);
    T.eq(bubbled, 0, "the filter's input stays inside");
    AH.invoke(el, "filter", "");
    T.eq(item(el, "fruit").style.display, "");
    T.eq(changes, ["bread"]);
  });

  T.test("listmenu: methods fire no change; re-insertion", async function (fx) {
    var el = await mountLm(fx, "lmf");
    var changes = events(el, "change");
    AH.invoke(el, "navigate", "fruit");
    await wait(350);
    T.eq(page(el), "0");
    AH.invoke(el, "setValue", "lemon");
    T.eq(el.getAttribute("data-ah-value"), "lemon");
    T.ok(item(el, "lemon").classList.contains("ah-listmenu-item-selected"));
    AH.invoke(el, "back");
    await wait(350);
    T.eq(page(el), "root");
    T.eq(changes, []);
    var box = el.parentNode;
    box.removeChild(el);
    await wait(0);
    box.appendChild(el);
    await T.ready(fx);
    item(el, "bread").click();
    T.eq(changes, ["bread"], "one listener");
  });
})(window.AHTest, window.AH);
