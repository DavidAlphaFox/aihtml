/* Behaviour of password-input (password_input.js). The fixtures are server renders from
 * aihtml_example_demo_password_input, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"pwd":"<div class=\"ah-pwd-group w-72\" data-ah=\"password-input\"><div class=\"ah-pwd-wrapper\"><input class=\"ah-pwd\" type=\"password\" autocomplete=\"current-password\" spellcheck=\"false\" name=\"new_password\" placeholder=\"New password\"><button class=\"ah-pwd-toggle\" type=\"button\" tabindex=\"-1\" aria-label=\"Show password\" aria-pressed=\"false\"><svg class=\"ah-pwd-icon-show\" xmlns=\"http://www.w3.org/2000/svg\" width=\"18\" height=\"18\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z\"></path><circle cx=\"12\" cy=\"12\" r=\"3\"></circle></svg><svg class=\"ah-pwd-icon-hide\" xmlns=\"http://www.w3.org/2000/svg\" width=\"18\" height=\"18\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M17.94 17.94A10.07 10.07 0 0 1 12 20c-7 0-11-8-11-8a18.45 18.45 0 0 1 5.06-5.94M9.9 4.24A9.12 9.12 0 0 1 12 4c7 0 11 8 11 8a18.5 18.5 0 0 1-2.16 3.19m-6.72-1.07a3 3 0 1 1-4.24-4.24\"></path><line x1=\"1\" y1=\"1\" x2=\"23\" y2=\"23\"></line></svg></button></div><div class=\"ah-pwd-strength\"><div class=\"ah-pwd-strength-bar\"><div class=\"ah-pwd-strength-fill\"></div></div><span class=\"ah-pwd-strength-text\" aria-live=\"polite\"></span></div></div>"};

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

  T.test("password-input: strength meter follows typing; the toggle shows the text", async function (fx) {
    var el = await mount(fx, FX.pwd), input = el.querySelector("input.ah-pwd");
    var fill = el.querySelector(".ah-pwd-strength-fill"), text = el.querySelector(".ah-pwd-strength-text");
    input.value = "abc";
    T.fire(input, "input");
    T.eq(el.getAttribute("data-strength"), "too-short");
    T.eq(text.textContent, "Too short");
    T.eq(fill.style.width, "20%");
    input.value = "Tr0ub4dor&3xyz!";
    T.fire(input, "input");
    T.eq(el.getAttribute("data-strength"), "strong");
    T.eq(fill.style.width, "100%");
    var toggle = el.querySelector(".ah-pwd-toggle");
    T.ok(!T.fire(toggle, "mousedown"), "the mousedown keeps the focus");
    toggle.click();
    T.eq(input.type, "text");
    T.ok(el.classList.contains("ah-pwd-visible"));
    T.eq(toggle.getAttribute("aria-pressed"), "true");
    T.eq(toggle.getAttribute("aria-label"), "Hide password");
    toggle.click();
    T.eq(input.type, "password");
  });

  T.test("password-input: methods; re-insertion", async function (fx) {
    var el = await mount(fx, FX.pwd), input = el.querySelector("input.ah-pwd");
    AH.invoke(el, "setValue", "abcdefgh");
    T.eq(AH.invoke(el, "getValue"), "abcdefgh");
    T.eq(el.getAttribute("data-strength"), "weak");
    AH.invoke(el, "setValue", "");
    T.ok(!el.hasAttribute("data-strength"));
    AH.invoke(el, "toggle", true);
    T.eq(input.type, "text");
    AH.invoke(el, "toggle");
    T.eq(input.type, "password");
    AH.invoke(el, "focus");
    T.eq(document.activeElement, input);
    T.ok(el.classList.contains("ah-pwd-focused"));
    await reinsert(fx, el);
    el.querySelector(".ah-pwd-toggle").click();
    T.eq(input.type, "text", "one toggle per click after re-insertion");
  });
})(window.AHTest, window.AH);
