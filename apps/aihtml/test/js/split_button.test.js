/* Behaviour of split-button (split_button.js). The fixtures are server renders from
 * aihtml_example_demo_split_button, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"sb":"<div class=\"ah-split-button\" data-variant=\"primary\" data-size=\"md\" data-disabled=\"false\" data-menu-align=\"start\" data-open=\"false\" data-ah=\"split-button\" data-ah-value=\"\"><button class=\"ah-split-button__main ah-btn ah-btn-primary\" type=\"button\">Save</button><button class=\"ah-split-button__arrow ah-btn ah-btn-primary\" type=\"button\" aria-haspopup=\"menu\" aria-expanded=\"false\" aria-label=\"Open menu\"><span class=\"ah-split-button__caret\" aria-hidden=\"true\">▾</span></button><div class=\"ah-split-button__menu\" role=\"menu\"><button class=\"ah-split-button__item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-value=\"draft\" data-disabled=\"false\"><span>Save as draft</span></button><button class=\"ah-split-button__item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-value=\"copy\" data-disabled=\"false\"><span>Save a copy</span></button><div class=\"ah-split-button__divider\" role=\"separator\"></div><button class=\"ah-split-button__item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-value=\"template\" data-disabled=\"false\"><span>Save as template</span></button><button class=\"ah-split-button__item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-value=\"locked\" data-disabled=\"true\" disabled=\"\"><span>Publish</span></button></div></div>"};

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

  T.test("split-button: main click reaches the root, menu clicks do not", async function (fx) {
    var el = await mount(fx, FX.sb), clicks = 0, changes = events(el, "change"), details = [];
    el.addEventListener("click", function () { clicks++; });
    el.addEventListener("change", function (e) { details.push(e.detail); });
    el.querySelector(".ah-split-button__main").click();
    T.eq(clicks, 1, "the main action");
    var arrow = el.querySelector(".ah-split-button__arrow");
    arrow.click();
    T.eq(clicks, 1, "the arrow is not the main action");
    T.eq(el.getAttribute("data-open"), "true");
    T.eq(arrow.getAttribute("aria-expanded"), "true");
    var items = all(el, ".ah-split-button__item");
    items[3].click();
    T.eq(el.getAttribute("data-open"), "true", "a disabled item does nothing");
    items[1].click();
    T.eq(clicks, 1);
    T.eq(el.getAttribute("data-open"), "false");
    T.eq(el.getAttribute("data-ah-value"), "copy");
    T.eq(changes, ["copy"]);
    T.eq(details, ["copy"]);
    arrow.focus();
    T.key(arrow, "ArrowUp");
    T.eq(document.activeElement, items[2], "arrow up opens on the last enabled item");
    T.key(items[2], "Escape");
    T.eq(el.getAttribute("data-open"), "false");
    T.eq(document.activeElement, arrow);
  });

  T.test("split-button: methods; re-insertion", async function (fx) {
    var el = await mount(fx, FX.sb), changes = events(el, "change");
    AH.invoke(el, "open");
    T.eq(el.getAttribute("data-open"), "true");
    AH.invoke(el, "close");
    T.eq(el.getAttribute("data-open"), "false");
    AH.invoke(el, "setValue", "draft");
    T.eq(AH.invoke(el, "getValue"), "draft");
    T.eq(changes, []);
    await reinsert(fx, el);
    el.querySelector(".ah-split-button__arrow").click();
    T.eq(el.getAttribute("data-open"), "true");
    T.fire(document.body, "mousedown");
    T.eq(el.getAttribute("data-open"), "false", "outside mousedown closes");
    el.querySelector(".ah-split-button__arrow").click();
    el.querySelector('.ah-split-button__item[data-value="template"]').click();
    T.eq(changes, ["template"], "one change after re-insertion");
  });
})(window.AHTest, window.AH);
