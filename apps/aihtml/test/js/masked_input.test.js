/* Behaviour of masked-input (masked_input.js). The fixtures are server renders from
 * aihtml_masked_input, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"mask":"<div class=\"ah-masked-input-group\" data-ah=\"masked-input\" data-ah-value=\"555\" data-ah-mask=\"(999) 999-9999\" data-ah-prompt=\"_\" id=\"t-m\"><input class=\"ah-masked-input\" type=\"text\" placeholder=\"\" autocomplete=\"off\" spellcheck=\"false\" autocorrect=\"off\" autocapitalize=\"off\" inputmode=\"numeric\" value=\"(555) ___-____\"><input type=\"hidden\" name=\"phone\" value=\"555\"></div>","maskf":"<div class=\"ah-masked-input-group\" data-ah=\"masked-input\" data-ah-value=\"\" data-ah-mask=\"99-LL\" data-ah-prompt=\"_\" id=\"t-mf\"><input class=\"ah-masked-input\" type=\"text\" placeholder=\"\" autocomplete=\"off\" spellcheck=\"false\" autocorrect=\"off\" autocapitalize=\"off\" value=\"\"><label class=\"ah-masked-input-label\">Code</label></div>"};

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

  function paste(el, text) {
    var dt = new DataTransfer();
    dt.setData("text/plain", text);
    return el.dispatchEvent(new ClipboardEvent("paste", { clipboardData: dt, bubbles: true, cancelable: true }));
  }

  T.test("masked-input: typing, literals, delete, paste, change on blur", async function (fx) {
    var el = await mount(fx, FX.mask), inp = el.querySelector(".ah-masked-input");
    var inputs = events(el, "input"), changes = events(el, "change");
    inp.focus();
    inp.setSelectionRange(5, 5);                      // after "(555"
    T.ok(!T.key(inp, "1"), "keys are handled, not typed");
    T.eq(inp.value, "(555) 1__-____");
    T.eq(inp.selectionStart, 7, "cursor skips to the next editable position");
    T.key(inp, "x");
    T.eq(inp.value, "(555) 1__-____", "a letter does not fit");
    T.key(inp, "2"); T.key(inp, "3");
    T.key(inp, "-");                                  // the literal typed where it stands
    T.eq(inp.selectionStart, 10);
    T.key(inp, "4");
    T.eq(el.getAttribute("data-ah-value"), "5551234");
    T.eq(el.querySelector("input[type=hidden]").value, "5551234");
    T.key(inp, "Backspace");
    T.eq(inp.value, "(555) 123-____");
    inp.setSelectionRange(10, 10);
    T.ok(!paste(inp, "98-76"), "the paste is handled");
    T.eq(inp.value, "(555) 123-9876", "paste skips what does not fit");
    T.ok(AH.invoke(el, "isComplete"));
    T.eq(AH.invoke(el, "getMaskedValue"), "(555) 123-9876");
    T.eq(inputs.length, 6);
    T.eq(changes, []);
    inp.blur();
    T.eq(changes, ["5551239876"]);
    AH.invoke(el, "setValue", "(111) 222-3333");
    T.eq(inp.value, "(111) 222-3333");
    AH.invoke(el, "setMask", "999-999");
    T.eq(inp.value, "111-222");
    AH.invoke(el, "clear");
    T.eq(changes, ["5551239876", ""]);
  });

  T.test("masked-input: floating label shows no mask while empty", async function (fx) {
    var el = await mount(fx, FX.maskf), inp = el.querySelector(".ah-masked-input"), l = el.querySelector("label");
    T.eq(inp.value, "");
    inp.focus();
    T.eq(inp.value, "__-__");
    T.ok(l.classList.contains("ah-masked-input-label-float"));
    inp.setSelectionRange(0, 0);
    T.key(inp, "4"); T.key(inp, "2"); T.key(inp, "a"); T.key(inp, "b");
    T.eq(el.getAttribute("data-ah-value"), "42ab");
    inp.blur();
    T.eq(inp.value, "42-ab");
    T.ok(l.classList.contains("ah-masked-input-label-float"));
    AH.invoke(el, "clear");
    T.eq(inp.value, "");
    T.ok(!l.classList.contains("ah-masked-input-label-float"));
  });

  T.test("masked-input: input events carry the value; re-insertion", async function (fx) {
    var el = await mount(fx, FX.mask), inp = el.querySelector(".ah-masked-input"), details = [];
    el.addEventListener("input", function (e) { if (e.target === el) { details.push(e.detail); } });
    await reinsert(fx, el);
    inp.focus();
    inp.setSelectionRange(5, 5);
    T.key(inp, "7");
    T.eq(details, ["5557"], "one input per key after re-insertion");
  });
})(window.AHTest, window.AH);
