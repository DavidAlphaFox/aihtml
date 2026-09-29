/* Behaviour of toggle-button (toggle_button.js). The fixtures are server renders from
 * aihtml_example_demo_toggle_button, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"tb":"<button class=\"ah-btn ah-btn-default\" type=\"button\" value=\"false\" aria-pressed=\"false\" data-ah=\"toggle-button\" data-ah-value=\"false\">Bold</button>"};

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

  T.test("toggle-button: click toggles and fires change", async function (fx) {
    var el = await mount(fx, FX.tb), changes = events(el, "change"), details = [];
    el.addEventListener("change", function (e) { details.push(e.detail); });
    el.click();
    T.eq(el.getAttribute("aria-pressed"), "true");
    T.eq(el.getAttribute("data-ah-value"), "true");
    T.eq(el.value, "true");
    T.ok(el.classList.contains("ah-btn-toggled"));
    el.click();
    T.eq(el.getAttribute("aria-pressed"), "false");
    T.ok(!el.classList.contains("ah-btn-toggled"));
    T.eq(changes, ["true", "false"]);
    T.eq(details, ["true", "false"]);
  });

  T.test("toggle-button: methods fire nothing; re-insertion", async function (fx) {
    var el = await mount(fx, FX.tb), changes = events(el, "change");
    AH.invoke(el, "toggle");
    T.eq(AH.invoke(el, "getValue"), true);
    AH.invoke(el, "setValue", "false");
    T.eq(AH.invoke(el, "getValue"), false);
    AH.invoke(el, "setValue", true);
    T.eq(el.getAttribute("data-ah-value"), "true");
    T.eq(changes, []);
    await reinsert(fx, el);
    el.click();
    T.eq(changes, ["false"], "one toggle per click after re-insertion");
  });
})(window.AHTest, window.AH);
