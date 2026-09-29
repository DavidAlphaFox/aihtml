/* Behaviour of radiobutton (radiobutton.js). The fixtures are server renders from
 * aihtml_example_demo_radiobutton, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"email":"<label class=\"ah-radiobutton ah-radiobutton-checked\" data-ah=\"radiobutton\"><input class=\"ah-choice-input\" type=\"radio\" value=\"email\" name=\"contact\" checked=\"\"><span class=\"ah-radiobutton-box\" aria-hidden=\"true\"><span class=\"ah-radiobutton-check ah-radiobutton-check-checked\"></span></span><span class=\"ah-radiobutton-label\">Email</span></label>","phone":"<label class=\"ah-radiobutton\" data-ah=\"radiobutton\"><input class=\"ah-choice-input\" type=\"radio\" value=\"phone\" name=\"contact\"><span class=\"ah-radiobutton-box\" aria-hidden=\"true\"><span class=\"ah-radiobutton-check\"></span></span><span class=\"ah-radiobutton-label\">Phone</span></label>","post":"<label class=\"ah-radiobutton\" data-ah=\"radiobutton\"><input class=\"ah-choice-input\" type=\"radio\" value=\"post\" name=\"contact\"><span class=\"ah-radiobutton-box\" aria-hidden=\"true\"><span class=\"ah-radiobutton-check\"></span></span><span class=\"ah-radiobutton-label\">Post</span></label>"};

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

  T.test("radiobutton: a change restyles every radio of the group", async function (fx) {
    fx.innerHTML = FX.email + FX.phone + FX.post;
    await T.ready(fx);
    var r = Array.prototype.slice.call(fx.children);
    r[1].querySelector("input").click();
    T.ok(r[1].classList.contains("ah-radiobutton-checked"));
    T.ok(r[1].querySelector(".ah-radiobutton-check").classList.contains("ah-radiobutton-check-checked"));
    T.ok(!r[0].classList.contains("ah-radiobutton-checked"), "the old one is restyled");
    T.eq(AH.invoke(r[1], "getValue"), true);
    AH.invoke(r[2], "setChecked", true);
    T.ok(r[2].classList.contains("ah-radiobutton-checked"));
    T.ok(!r[1].classList.contains("ah-radiobutton-checked"));
    AH.invoke(r[0], "setDisabled", true);
    T.ok(r[0].classList.contains("ah-radiobutton-disabled"));
  });

  T.test("radiobutton: locked; re-insertion", async function (fx) {
    fx.innerHTML = FX.email + FX.phone;
    await T.ready(fx);
    var r = Array.prototype.slice.call(fx.children);
    r[1].setAttribute("data-ah-locked", "");
    r[1].querySelector("input").click();
    T.ok(r[0].querySelector("input").checked, "a locked radio cannot be chosen");
    r[1].removeAttribute("data-ah-locked");
    await reinsert(fx, r[1]);
    r[1].querySelector("input").click();
    T.ok(r[1].classList.contains("ah-radiobutton-checked"), "works after re-insertion");
    T.ok(!r[0].classList.contains("ah-radiobutton-checked"));
  });
})(window.AHTest, window.AH);
