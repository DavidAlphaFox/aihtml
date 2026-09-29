/* Behaviour of input-otp (input_otp.js). The fixtures are server renders from
 * aihtml_example_demo_input_otp, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"otp":"<div class=\"ah-input-otp\" data-ah=\"input-otp\" role=\"group\" data-ah-value=\"\" data-length=\"6\" data-pattern=\"digit\" data-disabled=\"false\" data-complete=\"false\"><input class=\"ah-input-otp__slot\" type=\"text\" inputmode=\"numeric\" autocomplete=\"one-time-code\" maxlength=\"1\" data-index=\"0\" data-filled=\"false\" value=\"\" aria-label=\"Character 1 of 6\"><input class=\"ah-input-otp__slot\" type=\"text\" inputmode=\"numeric\" maxlength=\"1\" data-index=\"1\" data-filled=\"false\" value=\"\" aria-label=\"Character 2 of 6\"><input class=\"ah-input-otp__slot\" type=\"text\" inputmode=\"numeric\" maxlength=\"1\" data-index=\"2\" data-filled=\"false\" value=\"\" aria-label=\"Character 3 of 6\"><input class=\"ah-input-otp__slot\" type=\"text\" inputmode=\"numeric\" maxlength=\"1\" data-index=\"3\" data-filled=\"false\" value=\"\" aria-label=\"Character 4 of 6\"><input class=\"ah-input-otp__slot\" type=\"text\" inputmode=\"numeric\" maxlength=\"1\" data-index=\"4\" data-filled=\"false\" value=\"\" aria-label=\"Character 5 of 6\"><input class=\"ah-input-otp__slot\" type=\"text\" inputmode=\"numeric\" maxlength=\"1\" data-index=\"5\" data-filled=\"false\" value=\"\" aria-label=\"Character 6 of 6\"><input type=\"hidden\" name=\"code\" value=\"\"></div>"};

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

  T.test("input-otp: typing moves on, backspace goes back, change and ah:complete", async function (fx) {
    var el = await mount(fx, FX.otp), s = all(el, ".ah-input-otp__slot"), changes = events(el, "change"), done = [];
    var inner = 0;
    var onDoc = function (e) { if (e.target !== el && el.contains(e.target)) { inner++; } };
    document.addEventListener("input", onDoc);
    el.addEventListener("ah:complete", function (e) { done.push(e.detail); });
    s[0].focus();
    s[0].value = "1";
    T.fire(s[0], "input");
    T.eq(document.activeElement, s[1], "moves to the next slot");
    s[1].value = "x";
    T.fire(s[1], "input");
    T.eq(s[1].value, "", "not a digit");
    T.eq(el.getAttribute("data-ah-value"), "1");
    T.eq(s[0].getAttribute("data-filled"), "true");
    s[1].value = "23456";                             // autofill spreads
    T.fire(s[1], "input");
    T.eq(el.getAttribute("data-ah-value"), "123456");
    T.eq(el.getAttribute("data-complete"), "true");
    T.eq(el.querySelector("input[type=hidden]").value, "123456");
    T.eq(done, ["123456"]);
    T.key(s[5], "Backspace");
    T.eq(el.getAttribute("data-ah-value"), "12345");
    T.key(s[5], "Backspace");
    T.eq(s[4].value, "", "backs up and clears");
    T.eq(document.activeElement, s[4]);
    T.eq(changes, ["1", "123456", "12345", "1234"]);
    T.eq(inner, 0, "the slots' input events stay inside");
    document.removeEventListener("input", onDoc);
  });

  T.test("input-otp: paste, methods, re-insertion", async function (fx) {
    var el = await mount(fx, FX.otp), s = all(el, ".ah-input-otp__slot"), changes = events(el, "change");
    var dt = new DataTransfer();
    dt.setData("text", "98 76");
    T.ok(!s[0].dispatchEvent(new ClipboardEvent("paste", { clipboardData: dt, bubbles: true, cancelable: true })));
    T.eq(el.getAttribute("data-ah-value"), "9876");
    T.eq(document.activeElement, s[4]);
    AH.invoke(el, "setValue", "12a3");
    T.eq(AH.invoke(el, "getValue"), "123");
    AH.invoke(el, "invalid");
    T.eq(el.getAttribute("data-invalid"), "true");
    AH.invoke(el, "invalid", false);
    T.ok(!el.hasAttribute("data-invalid"));
    AH.invoke(el, "clear");
    T.eq(AH.invoke(el, "getValue"), "");
    AH.invoke(el, "focus");
    T.eq(document.activeElement, s[0]);
    await reinsert(fx, el);
    s[0].value = "5";
    T.fire(s[0], "input");
    T.eq(changes, ["9876", "123", "", "5"], "one change per edit after re-insertion");
  });
})(window.AHTest, window.AH);
