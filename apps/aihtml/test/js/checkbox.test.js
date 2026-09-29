/* Behaviour of checkbox (checkbox.js). The fixtures are server renders from
 * aihtml_example_demo_checkbox, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"cb":"<label class=\"ah-checkbox\" data-ah=\"checkbox\"><input class=\"ah-choice-input\" type=\"checkbox\" name=\"a\"><span class=\"ah-checkbox-box\" aria-hidden=\"true\"><span class=\"ah-checkbox-check\"></span></span><span class=\"ah-checkbox-label\">Unchecked</span></label>","three":"<label class=\"ah-checkbox ah-checkbox-checked\" data-ah=\"checkbox\" data-ah-three-states=\"\"><input class=\"ah-choice-input\" type=\"checkbox\" checked=\"\"><span class=\"ah-checkbox-box\" aria-hidden=\"true\"><span class=\"ah-checkbox-check ah-checkbox-check-checked\"></span></span><span class=\"ah-checkbox-label\">Click me: on, mixed, off</span></label>","locked":"<label class=\"ah-checkbox ah-checkbox-checked\" data-ah=\"checkbox\" data-ah-locked=\"\"><input class=\"ah-choice-input\" type=\"checkbox\" checked=\"\"><span class=\"ah-checkbox-box\" aria-hidden=\"true\"><span class=\"ah-checkbox-check ah-checkbox-check-checked\"></span></span><span class=\"ah-checkbox-label\">Locked</span></label>"};

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

  T.test("checkbox: a click mirrors the input onto the classes", async function (fx) {
    var el = await mount(fx, FX.cb), input = el.querySelector("input"), check = el.querySelector(".ah-checkbox-check");
    var seen = 0;
    el.addEventListener("change", function () { seen++; });
    el.click();                                        // the label toggles the input
    T.ok(input.checked);
    T.ok(el.classList.contains("ah-checkbox-checked"));
    T.ok(check.classList.contains("ah-checkbox-check-checked"));
    input.click();
    T.ok(!el.classList.contains("ah-checkbox-checked"));
    T.eq(seen, 2, "the input's native change bubbles");
    T.eq(AH.invoke(el, "getValue"), false);
  });

  T.test("checkbox: three states, locked, methods", async function (fx) {
    fx.innerHTML = FX.three + FX.locked;
    await T.ready(fx);
    var three = fx.children[0], locked = fx.children[1], input = three.querySelector("input");
    input.click();                                     // checked -> mixed
    T.eq(AH.invoke(three, "getValue"), "mixed");
    T.ok(three.classList.contains("ah-checkbox-indeterminate"));
    input.click();                                     // mixed -> unchecked
    T.eq(AH.invoke(three, "getValue"), false);
    input.click();                                     // unchecked -> checked
    T.eq(AH.invoke(three, "getValue"), true);
    T.fire(input, "change");                           // a script's change: no step
    T.eq(AH.invoke(three, "getValue"), true);
    locked.querySelector("input").click();
    T.ok(locked.querySelector("input").checked, "locked keeps its state");
    AH.invoke(three, "setChecked", "mixed");
    T.ok(input.indeterminate);
    T.ok(three.querySelector(".ah-checkbox-check").classList.contains("ah-checkbox-check-indeterminate"));
    AH.invoke(three, "setChecked", false);
    T.eq(AH.invoke(three, "getValue"), false);
    input.click();
    T.eq(AH.invoke(three, "getValue"), true, "the three states go on from setChecked");
    AH.invoke(three, "setDisabled", true);
    T.ok(input.disabled);
    T.ok(three.classList.contains("ah-checkbox-disabled"));
  });

  T.test("checkbox: re-insertion", async function (fx) {
    var el = await mount(fx, FX.three), input = el.querySelector("input");
    await reinsert(fx, el);
    input.click();
    T.eq(AH.invoke(el, "getValue"), "mixed", "one step per click after re-insertion");
  });
})(window.AHTest, window.AH);
