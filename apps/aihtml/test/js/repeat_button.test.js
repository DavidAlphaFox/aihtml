/* Behaviour of repeat-button (repeat_button.js). The fixtures are server renders from
 * aihtml_repeat_button, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"rb":"<button class=\"ah-btn ah-btn-primary\" type=\"button\" value=\"1\" id=\"t-b\" data-ah=\"repeat-button\" data-ah-delay=\"60\" data-ah-interval=\"20\">+</button>"};

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

  T.test("repeat-button: clicks while held, one per repetition", async function (fx) {
    var el = await mount(fx, FX.rb), clicks = 0, bubbled = 0;
    el.addEventListener("click", function () { clicks++; });
    var onDoc = function (e) { if (e.target === el) { bubbled++; } };
    document.addEventListener("click", onDoc);
    T.fire(el, "mousedown", { button: 0 });
    T.eq(clicks, 1, "at once");
    T.ok(el.classList.contains("ah-btn-pressed"));
    await sleep(150);
    T.fire(el, "mouseup");
    el.click();                                       // the browser's click on release
    var n = clicks;
    T.ok(n >= 3 && n <= 7, "repeated: " + n);
    T.eq(bubbled, n, "the release click does not reach the page");
    await sleep(60);
    T.eq(clicks, n, "stopped");
    T.key(el, "Enter");
    T.key(el, "Enter", { repeat: true });
    T.fire(el, "keyup", { key: "Enter" });
    T.eq(clicks, n + 1, "keyboard press");
    el.click();
    await sleep(5);
    el.click();
    T.eq(bubbled, n + 2, "a plain click later is one click");
    document.removeEventListener("click", onDoc);
  });

  T.test("repeat-button: stop() from the server; works again after re-insertion", async function (fx) {
    var el = await mount(fx, FX.rb), clicks = 0;
    el.addEventListener("click", function () { clicks++; });
    T.fire(el, "mousedown", { button: 0 });
    AH.invoke(el, "stop");
    T.ok(!el.classList.contains("ah-btn-pressed"), "released");
    await sleep(120);
    T.eq(clicks, 1, "no repetition after stop");
    await reinsert(fx, el);
    T.fire(el, "mousedown", { button: 0 });
    T.fire(el, "mouseup");
    T.eq(clicks, 2, "one click per press after re-insertion");
  });
})(window.AHTest, window.AH);
