/* Behaviour of input (input.js). The fixtures are server renders from
 * aihtml_example_demo_input, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"clear":"<div class=\"ah-input-group ah-input-clearable w-56 ah-input-has-value\" data-ah=\"input\"><div class=\"ah-input-row\"><input class=\"ah-input\" type=\"text\" value=\"Clear me\"><button class=\"ah-input-clear\" type=\"button\" tabindex=\"-1\" aria-label=\"Clear\">×</button></div></div>","label":"<div class=\"ah-input-group w-56\" data-ah=\"input\"><input class=\"ah-input\" type=\"text\" id=\"ah-in-18946\"><label class=\"ah-input-label\" for=\"ah-in-18946\">Full name</label></div>"};

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

  T.test("input: typing marks the value, the clear button empties it with native events", async function (fx) {
    var el = await mount(fx, FX.clear), input = el.querySelector("input.ah-input"), log = [];
    input.addEventListener("input", function () { log.push("input"); });
    input.addEventListener("change", function () { log.push("change"); });
    T.ok(el.classList.contains("ah-input-has-value"));
    T.fire(el.querySelector(".ah-input-clear"), "click");
    T.eq(input.value, "");
    T.ok(!el.classList.contains("ah-input-has-value"));
    T.eq(document.activeElement, input, "focus back in the field");
    T.eq(log, ["input", "change"]);
    input.value = "abc";
    T.fire(input, "input");
    T.ok(el.classList.contains("ah-input-has-value"));
    T.ok(!T.key(input, "Escape"), "Escape clears a clearable input");
    T.eq(input.value, "");
    T.eq(log, ["input", "change", "input", "input", "change"]);
  });

  T.test("input: floating label, methods", async function (fx) {
    var el = await mount(fx, FX.label), input = el.querySelector("input"), label = el.querySelector("label");
    T.ok(!label.classList.contains("ah-input-label-float"));
    input.focus();
    T.ok(label.classList.contains("ah-input-label-float"), "up while focused");
    T.ok(el.classList.contains("ah-input-focused"));
    input.blur();
    T.ok(!label.classList.contains("ah-input-label-float"), "down when empty");
    AH.invoke(el, "setValue", "Ada");
    T.eq(AH.invoke(el, "getValue"), "Ada");
    T.ok(label.classList.contains("ah-input-label-float"), "up when filled");
    AH.invoke(el, "selectAll");
    T.eq(document.activeElement, input);
    T.eq(input.selectionEnd - input.selectionStart, 3);
    AH.invoke(el, "clear");
    T.eq(input.value, "");
  });

  T.test("input: re-insertion", async function (fx) {
    var el = await mount(fx, FX.clear), input = el.querySelector("input.ah-input"), n = 0;
    input.addEventListener("input", function () { n++; });
    await reinsert(fx, el);
    el.querySelector(".ah-input-clear").click();
    T.eq(input.value, "");
    T.eq(n, 1, "one clear after re-insertion");
  });
})(window.AHTest, window.AH);
