/* Behaviour of radiobutton-group (radiobutton_group.js). The fixtures are server renders from
 * aihtml_example_demo_radiobutton_group, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"rg":"<div class=\"ah-radiobutton-group ah-radiobutton-group-horizontal\" data-ah=\"radiobutton-group\" role=\"radiogroup\" data-ah-value=\"m\" data-label-position=\"after\"><label class=\"ah-radiobutton-group-item\" data-value=\"s\" data-index=\"0\"><span class=\"ah-radiobutton\"><input class=\"ah-choice-input\" type=\"radio\" value=\"s\" name=\"size\"><span class=\"ah-radiobutton-box\" aria-hidden=\"true\"><span class=\"ah-radiobutton-check\"></span></span></span><span class=\"ah-radiobutton-group-label\">S</span></label><label class=\"ah-radiobutton-group-item\" data-value=\"m\" data-index=\"1\"><span class=\"ah-radiobutton ah-radiobutton-checked\"><input class=\"ah-choice-input\" type=\"radio\" value=\"m\" name=\"size\" checked=\"\"><span class=\"ah-radiobutton-box\" aria-hidden=\"true\"><span class=\"ah-radiobutton-check ah-radiobutton-check-checked\"></span></span></span><span class=\"ah-radiobutton-group-label\">M</span></label><label class=\"ah-radiobutton-group-item\" data-value=\"l\" data-index=\"2\"><span class=\"ah-radiobutton\"><input class=\"ah-choice-input\" type=\"radio\" value=\"l\" name=\"size\"><span class=\"ah-radiobutton-box\" aria-hidden=\"true\"><span class=\"ah-radiobutton-check\"></span></span></span><span class=\"ah-radiobutton-group-label\">L</span></label><label class=\"ah-radiobutton-group-item\" data-value=\"xl\" data-index=\"3\"><span class=\"ah-radiobutton\"><input class=\"ah-choice-input\" type=\"radio\" value=\"xl\" name=\"size\"><span class=\"ah-radiobutton-box\" aria-hidden=\"true\"><span class=\"ah-radiobutton-check\"></span></span></span><span class=\"ah-radiobutton-group-label\">XL</span></label></div>"};

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

  T.test("radiobutton-group: click and arrows select, one change on the root", async function (fx) {
    var el = await mount(fx, FX.rg), changes = events(el, "change");
    var inputs = all(el, "input.ah-choice-input");
    T.eq(inputs[1].getAttribute("tabindex"), "0", "the checked radio is the tab stop");
    inputs[0].click();
    T.eq(el.getAttribute("data-ah-value"), "s");
    T.ok(inputs[0].closest(".ah-radiobutton").classList.contains("ah-radiobutton-checked"));
    T.ok(!inputs[1].closest(".ah-radiobutton").classList.contains("ah-radiobutton-checked"));
    T.ok(!T.key(inputs[0], "ArrowLeft"), "handled");
    T.eq(el.getAttribute("data-ah-value"), inputs[inputs.length - 1].value, "wraps around");
    T.eq(document.activeElement, inputs[inputs.length - 1]);
    T.eq(changes.length, 2);
  });

  T.test("radiobutton-group: methods; re-insertion", async function (fx) {
    var el = await mount(fx, FX.rg), changes = events(el, "change");
    AH.invoke(el, "setValue", "s");
    T.eq(AH.invoke(el, "getValue"), "s");
    AH.invoke(el, "setDisabled", true);
    T.ok(el.classList.contains("ah-radiobutton-group-disabled"));
    AH.invoke(el, "setDisabled", false);
    T.eq(changes, []);
    await reinsert(fx, el);
    var inputs = all(el, "input.ah-choice-input");
    T.key(inputs[0], "ArrowRight");
    T.eq(changes, ["m"], "one change after re-insertion");
  });
})(window.AHTest, window.AH);
