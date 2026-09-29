/* Client-side validation (field.js: the document-level handlers of
 * [data-ah-validate] and the page functions validate/clearValidation).
 * The fixtures are reduced copies of aihtml_field:validate/1 renders
 * (the demo module aihtml_example_demo_field and a field/4 row). Events
 * are native; every test awaits T.ready. */
(function (T, AH) {
  "use strict";

  function attr(rules) { return JSON.stringify(rules).replace(/"/g, "&quot;"); }

  var FORM = '<form id="vf" action="#checked">' +
    '<input class="ah-input" type="text" name="zip" id="zip" data-ah-validate="' +
    attr([{ msg: "Enter a ZIP code", rule: "required" }, { rule: "zip_code" }]) +
    '" data-ah-validate-on="blur" data-ah-validate-hint="tooltip" aria-required="true">' +
    '<input class="ah-input" type="text" name="nick" id="nick" data-ah-validate="' +
    attr([{ args: [3], rule: "min_length" }]) + '" data-ah-validate-on="blur" data-ah-validate-hint="label">' +
    '<button class="ah-btn" type="submit" id="go">Check</button></form>';

  var ROW = '<form id="rf"><div class="ah-form-row"><label class="ah-form-label" for="em"><span>Email</span></label>' +
    '<div class="ah-form-body"><div class="ah-form-field"><div>' +
    '<input class="ah-input" type="text" id="em" aria-describedby="em-help" data-ah-validate="' +
    attr([{ rule: "email" }]) + '" data-ah-validate-on="input">' +
    '</div></div><div class="ah-form-help" id="em-help">Work address</div></div></div>' +
    '<div class="ah-form-row"><div class="ah-form-body"><select id="pick" multiple data-ah-validate="' +
    attr([{ rule: "required" }]) + '"><option value="a">a</option><option value="b">b</option></select></div></div>' +
    '<input type="password" id="pw" value="abc"><input type="password" id="pw2" value="abd" data-ah-validate="' +
    attr([{ rule: "same_as", args: ["#pw"] }]) + '" data-ah-validate-hint="label"></form>';

  async function mount(fx, html) { fx.innerHTML = html; await T.ready(fx); return fx.firstChild; }
  function tick() { return new Promise(function (r) { setTimeout(r, 0); }); }
  function type(el, v) { el.value = v; T.fire(el, "input"); }
  function bubbles() { return document.querySelectorAll(".ah-validator-hint").length; }

  T.test("field: blur checks, the tooltip bubble floats and clears once fixed", async function (fx) {
    await mount(fx, FORM);
    var zip = document.getElementById("zip");
    zip.focus();
    zip.blur();
    T.ok(zip.classList.contains("ah-validator-error-element"), "error class");
    T.eq(zip.getAttribute("aria-invalid"), "true");
    var hint = document.querySelector(".ah-validator-hint");
    T.ok(hint && hint.classList.contains("ah-validator-hint-visible"), "bubble");
    T.eq(hint.textContent, "Enter a ZIP code");
    T.eq(zip.getAttribute("aria-describedby"), hint.id);
    T.eq(hint.style.position, "fixed", "AH.float");
    type(zip, "123");                        // invalid: checked on every input
    T.eq(document.querySelector(".ah-validator-hint").textContent, "Please enter a valid ZIP code");
    type(zip, "12345");
    T.eq(bubbles(), 0, "fixed");
    T.ok(!zip.hasAttribute("aria-invalid"));
    T.ok(!zip.hasAttribute("aria-describedby"));
  });

  T.test("field: submit is stopped while invalid; label hints; events carry {invalid}", async function (fx) {
    var form = await mount(fx, FORM), got = [], submits = 0;
    form.addEventListener("ah:validation-error", function (e) { got.push(e.detail.invalid.map(function (n) { return n.id; })); });
    form.addEventListener("ah:validation-success", function (e) { got.push("ok"); });
    form.addEventListener("submit", function (e) { submits++; e.preventDefault(); });
    var nick = document.getElementById("nick");
    nick.value = "ab";
    document.getElementById("go").click();
    T.eq(submits, 0, "blocked");
    T.eq(got, [["zip", "nick"]]);
    T.eq(document.activeElement, document.getElementById("zip"), "first invalid focused");
    var label = nick.nextElementSibling;
    T.ok(label.matches("label.ah-validator-error-label"), "label after the control");
    T.eq(label.textContent, "Please enter at least 3 characters");
    T.eq(label.htmlFor, "nick");
    document.getElementById("zip").value = "12345";
    type(nick, "abc");
    T.ok(!nick.nextElementSibling || !nick.nextElementSibling.matches("label"), "label removed");
    document.getElementById("go").click();
    T.eq(submits, 1, "valid form submits");
    T.eq(got[1], "ok");
  });

  T.test("field: field/4 rows, multiple select, same_as; validate/clearValidation functions", async function (fx) {
    var form = await mount(fx, ROW);
    T.eq(runFn("validate", "#rf"), false);
    var em = document.getElementById("em");
    T.ok(em.closest(".ah-form-row").classList.contains("ah-form-row-invalid") === false, "empty email is fine");
    var pick = document.getElementById("pick");
    T.eq(pick.closest(".ah-form-body").querySelector(".ah-form-error").textContent, "This field is required");
    T.ok(pick.closest(".ah-form-row").classList.contains("ah-form-row-invalid"));
    T.eq(document.getElementById("pw2").nextElementSibling.textContent, "The values do not match");
    type(em, "x@");
    T.eq(em.closest(".ah-form-body").querySelector(".ah-form-error").textContent, "Please enter a valid email address");
    T.ok(/^em-help ah-vh\d+$/.test(em.getAttribute("aria-describedby")), "describedby kept and extended");
    pick.options[1].selected = true; T.fire(pick, "change");
    T.ok(!pick.closest(".ah-form-body").querySelector(".ah-form-error"), "fixed on change");
    runFn("clearValidation", form);
    T.eq(form.querySelectorAll(".ah-validator-error-element, .ah-form-error, label.ah-validator-error-label").length, 0);
    T.eq(em.getAttribute("aria-describedby"), "em-help");
    document.getElementById("pw2").value = "abc";
    T.eq(runFn("validate", form), false, "the email is still wrong");
    type(em, "x@y.z");
    T.eq(runFn("validate", form), true);
  });

  T.test("field: a bubble whose control left the page is swept; re-inserted controls still validate", async function (fx) {
    await mount(fx, FORM);
    var zip = document.getElementById("zip");
    zip.focus(); zip.blur();
    T.eq(bubbles(), 1);
    var form = fx.firstChild;
    form.remove();
    await tick();
    fx.innerHTML = FORM;
    await T.ready(fx);
    runFn("validate", document.getElementById("nick"));
    T.eq(bubbles(), 0, "old bubble swept");
    fx.appendChild(form);
    T.eq(runFn("validate", form), false);
    T.eq(bubbles(), 1);
    runFn("clearValidation");
    T.eq(bubbles(), 0);
  });

  // What call(Ctx, global, Name, [Target]) runs: the page function
  // through the call op. validate's result is read from the event it
  // fires on the checked scope.
  function runFn(name, target) {
    var result;
    function ok() { result = true; }
    function bad() { result = false; }
    document.addEventListener("ah:validation-success", ok);
    document.addEventListener("ah:validation-error", bad);
    AH.apply([{ op: "call", method: name, args: target === undefined ? [] : [target] }]);
    document.removeEventListener("ah:validation-success", ok);
    document.removeEventListener("ah:validation-error", bad);
    return result;
  }
})(window.AHTest, window.AH);
