/* Behaviour of checkbox-group (checkbox_group.js). The fixtures are server renders from
 * aihtml_example_demo_checkbox_group, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"cg":"<div class=\"ah-checkbox-group ah-checkbox-group-vertical\" data-ah=\"checkbox-group\" role=\"group\" data-ah-value=\"apple,plum\" data-label-position=\"after\"><label class=\"ah-checkbox-group-item\" data-value=\"apple\" data-index=\"0\"><span class=\"ah-checkbox ah-checkbox-checked\"><input class=\"ah-choice-input\" type=\"checkbox\" value=\"apple\" name=\"fruit\" checked=\"\"><span class=\"ah-checkbox-box\" aria-hidden=\"true\"><span class=\"ah-checkbox-check ah-checkbox-check-checked\"></span></span></span><span class=\"ah-checkbox-group-label\">Apple</span></label><label class=\"ah-checkbox-group-item\" data-value=\"pear\" data-index=\"1\"><span class=\"ah-checkbox\"><input class=\"ah-choice-input\" type=\"checkbox\" value=\"pear\" name=\"fruit\"><span class=\"ah-checkbox-box\" aria-hidden=\"true\"><span class=\"ah-checkbox-check\"></span></span></span><span class=\"ah-checkbox-group-label\">Pear</span></label><label class=\"ah-checkbox-group-item\" data-value=\"plum\" data-index=\"2\"><span class=\"ah-checkbox ah-checkbox-checked\"><input class=\"ah-choice-input\" type=\"checkbox\" value=\"plum\" name=\"fruit\" checked=\"\"><span class=\"ah-checkbox-box\" aria-hidden=\"true\"><span class=\"ah-checkbox-check ah-checkbox-check-checked\"></span></span></span><span class=\"ah-checkbox-group-label\">Plum</span></label><label class=\"ah-checkbox-group-item ah-checkbox-group-item-disabled\" data-value=\"fig\" data-index=\"3\" data-ah-item-disabled=\"\"><span class=\"ah-checkbox ah-checkbox-disabled\"><input class=\"ah-choice-input\" type=\"checkbox\" value=\"fig\" name=\"fruit\" disabled=\"\"><span class=\"ah-checkbox-box\" aria-hidden=\"true\"><span class=\"ah-checkbox-check\"></span></span></span><span class=\"ah-checkbox-group-label\">Fig (sold out)</span></label></div>"};

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

  T.test("checkbox-group: one change on the root, the inputs' change stays inside", async function (fx) {
    var el = await mount(fx, FX.cg), changes = events(el, "change"), inner = 0;
    var onDoc = function (e) { if (e.target !== el && el.contains(e.target)) { inner++; } };
    document.addEventListener("change", onDoc);
    var inputs = all(el, "input.ah-choice-input");
    inputs[1].click();
    T.eq(el.getAttribute("data-ah-value"), "apple,pear,plum");
    T.ok(inputs[1].closest(".ah-checkbox").classList.contains("ah-checkbox-checked"));
    inputs[0].click();
    T.eq(el.getAttribute("data-ah-value"), "pear,plum");
    T.eq(changes, ["apple,pear,plum", "pear,plum"]);
    T.eq(inner, 0, "the inner inputs' change does not leave the root");
    document.removeEventListener("change", onDoc);
  });

  T.test("checkbox-group: methods; re-insertion", async function (fx) {
    var el = await mount(fx, FX.cg), changes = events(el, "change");
    AH.invoke(el, "setValue", ["pear"]);
    T.eq(AH.invoke(el, "getValue"), ["pear"]);
    AH.invoke(el, "setValue", "apple,plum");
    T.eq(el.getAttribute("data-ah-value"), "apple,plum");
    AH.invoke(el, "setDisabled", true);
    T.ok(el.classList.contains("ah-checkbox-group-disabled"));
    T.eq(el.getAttribute("aria-disabled"), "true");
    T.ok(all(el, "input.ah-choice-input").every(function (i) { return i.disabled; }));
    T.ok(el.querySelector(".ah-checkbox-group-item").classList.contains("ah-checkbox-group-item-disabled"));
    AH.invoke(el, "setDisabled", false);
    T.ok(!el.hasAttribute("aria-disabled"));
    T.eq(changes, [], "methods fire no change");
    await reinsert(fx, el);
    el.querySelector("input.ah-choice-input").click();
    T.eq(changes, ["plum"], "one change after re-insertion");
  });
})(window.AHTest, window.AH);
