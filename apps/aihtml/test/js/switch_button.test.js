/* Behaviour of switch-button (switch_button.js). The fixtures are server renders from
 * aihtml_example_demo_switch_button, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"off":"<label class=\"ah-switch\" data-ah=\"switch-button\"><input class=\"ah-choice-input\" type=\"checkbox\" name=\"bluetooth\" role=\"switch\"><span class=\"ah-switch-track\" aria-hidden=\"true\"><span class=\"ah-switch-thumb\"></span></span><span class=\"ah-switch-text\">Bluetooth</span></label>","locked":"<label class=\"ah-switch ah-switch-on\" data-ah=\"switch-button\" data-ah-locked=\"\"><input class=\"ah-choice-input\" type=\"checkbox\" checked=\"\" role=\"switch\"><span class=\"ah-switch-track\" aria-hidden=\"true\"><span class=\"ah-switch-thumb\"></span></span><span class=\"ah-switch-text\">Locked</span></label>"};

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

  T.test("switch-button: a click mirrors the input; locked", async function (fx) {
    fx.innerHTML = FX.off + FX.locked;
    await T.ready(fx);
    var sw = fx.children[0], locked = fx.children[1];
    sw.click();
    T.ok(sw.classList.contains("ah-switch-on"));
    T.eq(AH.invoke(sw, "getValue"), true);
    sw.click();
    T.ok(!sw.classList.contains("ah-switch-on"));
    locked.click();
    T.ok(locked.classList.contains("ah-switch-on"), "locked keeps its state");
    T.ok(locked.querySelector("input").checked);
  });

  T.test("switch-button: methods; re-insertion", async function (fx) {
    var el = await mount(fx, FX.off), seen = 0;
    el.addEventListener("change", function () { seen++; });
    AH.invoke(el, "setChecked", "on");
    T.ok(el.classList.contains("ah-switch-on"));
    AH.invoke(el, "setDisabled", true);
    T.ok(el.classList.contains("ah-switch-disabled"));
    AH.invoke(el, "setDisabled", false);
    T.eq(seen, 0, "methods fire no change");
    await reinsert(fx, el);
    el.click();
    T.ok(!el.classList.contains("ah-switch-on"), "works after re-insertion");
    T.eq(seen, 1);
  });
})(window.AHTest, window.AH);
