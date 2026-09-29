/* Behaviour of dropdown-button (dropdown_button.js). The fixtures are server renders from
 * aihtml_example_demo_dropdown_button, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"dd":"<div class=\"ah-dropdown-btn\" data-ah=\"dropdown-button\" data-ah-value=\"\"><button class=\"ah-dropdown-btn-wrapper\" type=\"button\" aria-haspopup=\"menu\" aria-expanded=\"false\"><div class=\"ah-dropdown-btn-content\">Actions</div><div class=\"ah-dropdown-btn-arrow\" aria-hidden=\"true\"><span class=\"ah-dropdown-btn-arrow-icon\"></span></div></button><div class=\"ah-dropdown-btn-popup\" role=\"menu\" hidden=\"\"><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-value=\"draft\"><span>Save as draft</span></button><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-value=\"copy\"><span>Save a copy</span></button><div class=\"ah-dropdown-btn-divider\" role=\"separator\"></div><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-value=\"template\"><span>Save as template</span></button><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-value=\"locked\" disabled=\"\"><span>Publish</span></button></div><input type=\"hidden\" name=\"action\" value=\"\" data-ah-input=\"\"></div>"};

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

  T.test("dropdown-button: opens, arrows, choose fires change, Escape", async function (fx) {
    var el = await mount(fx, FX.dd), changes = events(el, "change"), log = [], details = [];
    el.addEventListener("ah:open", function () { log.push("open"); });
    el.addEventListener("ah:close", function () { log.push("close"); });
    el.addEventListener("change", function (e) { details.push(e.detail); });
    var trigger = el.querySelector(".ah-dropdown-btn-wrapper"), popup = el.querySelector(".ah-dropdown-btn-popup");
    var items = all(el, ".ah-dropdown-btn-item");
    trigger.click();
    T.ok(el.classList.contains("ah-dropdown-btn-opened"));
    T.ok(!popup.hidden, "shown");
    T.eq(trigger.getAttribute("aria-expanded"), "true");
    trigger.click();
    T.ok(popup.hidden, "a second click closes");
    trigger.focus();
    T.key(trigger, "ArrowDown");
    T.eq(document.activeElement, items[0], "arrow down opens on the first item");
    T.key(items[0], "ArrowUp");
    T.eq(document.activeElement, items[2], "wraps to the last enabled item");
    T.key(items[2], "ArrowDown");
    T.eq(document.activeElement, items[0]);
    items[1].click();
    T.ok(popup.hidden, "choosing closes");
    T.eq(document.activeElement, trigger, "focus back on the trigger");
    T.eq(el.getAttribute("data-ah-value"), "copy");
    T.eq(el.querySelector("input[type=hidden]").value, "copy");
    T.ok(items[1].classList.contains("selected"));
    trigger.click();
    items[1].click();
    T.eq(changes, ["copy", "copy"], "a command: the same item fires again");
    T.eq(details, ["copy", "copy"]);
    trigger.click();
    T.key(trigger, "Escape");
    T.ok(popup.hidden, "Escape closes");
    T.eq(log, ["open", "close", "open", "close", "open", "close", "open", "close"]);
  });

  T.test("dropdown-button: outside click, methods, re-insertion", async function (fx) {
    var el = await mount(fx, FX.dd), changes = events(el, "change");
    var popup = el.querySelector(".ah-dropdown-btn-popup");
    AH.invoke(el, "open");
    T.ok(!popup.hidden);
    T.fire(document.body, "mousedown");
    T.ok(popup.hidden, "outside mousedown closes");
    AH.invoke(el, "toggle");
    T.ok(!popup.hidden);
    AH.invoke(el, "close");
    T.ok(popup.hidden);
    AH.invoke(el, "setValue", "template");
    T.eq(AH.invoke(el, "getValue"), "template");
    T.ok(el.querySelector('[data-value="template"]').classList.contains("selected"));
    T.eq(changes, [], "methods fire no change");
    await reinsert(fx, el);
    el.querySelector(".ah-dropdown-btn-wrapper").click();
    T.ok(!popup.hidden, "opens after re-insertion");
    el.querySelector('[data-value="draft"]').click();
    T.eq(changes, ["draft"], "one change after re-insertion");
    AH.invoke(el, "open");
    T.fire(document.body, "mousedown");
    T.ok(popup.hidden, "the document listener is back");
  });
})(window.AHTest, window.AH);
