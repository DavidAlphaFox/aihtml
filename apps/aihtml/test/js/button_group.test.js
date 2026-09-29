/* Behaviour of button-group (button_group.js). The fixtures are server renders from
 * aihtml_example_demo_button_group, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"radio":"<div class=\"ah-btn-group ah-btn-group-radio ah-btn-group-horizontal ah-btn-group-rounded\" role=\"radiogroup\" data-ah=\"button-group\" data-ah-value=\"grid\"><button class=\"ah-btn-group-btn ah-btn-group-btn-first\" type=\"button\" value=\"list\" data-value=\"list\" tabindex=\"-1\" role=\"radio\" aria-checked=\"false\">List</button><button class=\"ah-btn-group-btn ah-btn-group-btn-selected\" type=\"button\" value=\"grid\" data-value=\"grid\" tabindex=\"0\" role=\"radio\" aria-checked=\"true\">Grid</button><button class=\"ah-btn-group-btn ah-btn-group-btn-last\" type=\"button\" value=\"board\" data-value=\"board\" tabindex=\"-1\" role=\"radio\" aria-checked=\"false\">Board</button><input type=\"hidden\" name=\"view\" value=\"grid\" data-ah-input=\"\"></div>","check":"<div class=\"ah-btn-group ah-btn-group-checkbox ah-btn-group-horizontal\" role=\"group\" data-ah=\"button-group\" data-ah-value=\"b,u\"><button class=\"ah-btn-group-btn ah-btn-group-btn-first ah-btn-group-btn-selected\" type=\"button\" value=\"b\" data-value=\"b\" aria-pressed=\"true\">B</button><button class=\"ah-btn-group-btn\" type=\"button\" value=\"i\" data-value=\"i\" aria-pressed=\"false\">I</button><button class=\"ah-btn-group-btn ah-btn-group-btn-last ah-btn-group-btn-selected\" type=\"button\" value=\"u\" data-value=\"u\" aria-pressed=\"true\">U</button></div>","plain":"<div class=\"ah-btn-group ah-btn-group-horizontal ah-btn-group-rounded\" role=\"group\" data-ah=\"button-group\"><button class=\"ah-btn-group-btn ah-btn-group-btn-first\" type=\"button\" value=\"Left\" data-value=\"Left\">Left</button><button class=\"ah-btn-group-btn\" type=\"button\" value=\"Middle\" data-value=\"Middle\">Middle</button><button class=\"ah-btn-group-btn ah-btn-group-btn-last\" type=\"button\" value=\"Right\" data-value=\"Right\">Right</button></div>"};

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

  T.test("button-group: radio mode selects, arrows move, change on the root", async function (fx) {
    var el = await mount(fx, FX.radio), changes = events(el, "change"), details = [];
    el.addEventListener("change", function (e) { details.push(e.detail); });
    var b = all(el, ".ah-btn-group-btn");
    b[0].click();
    T.eq(el.getAttribute("data-ah-value"), "list");
    T.eq(el.querySelector("input[type=hidden]").value, "list");
    T.eq(b[0].getAttribute("aria-checked"), "true");
    T.eq(b[0].getAttribute("tabindex"), "0");
    T.eq(b[1].getAttribute("aria-checked"), "false");
    T.eq(b[1].getAttribute("tabindex"), "-1");
    T.ok(!T.key(b[0], "ArrowRight"), "handled");
    T.eq(document.activeElement, b[1]);
    T.eq(el.getAttribute("data-ah-value"), "grid");
    T.key(b[1], "End");
    T.eq(el.getAttribute("data-ah-value"), "board");
    b[2].click();                                     // the same value: no change
    T.eq(changes, ["list", "grid", "board"]);
    T.eq(details, ["list", "grid", "board"], "detail is the value");
  });

  T.test("button-group: checkbox mode toggles; methods fire nothing", async function (fx) {
    var el = await mount(fx, FX.check), changes = events(el, "change");
    var b = all(el, ".ah-btn-group-btn");
    b[1].click();
    T.eq(el.getAttribute("data-ah-value"), "b,i,u");
    T.eq(b[1].getAttribute("aria-pressed"), "true");
    b[0].click();
    T.eq(el.getAttribute("data-ah-value"), "i,u");
    T.ok(!b[0].classList.contains("ah-btn-group-btn-selected"));
    T.eq(changes, ["b,i,u", "i,u"]);
    AH.invoke(el, "setValue", ["b"]);
    T.eq(AH.invoke(el, "getValue"), "b");
    AH.invoke(el, "setValue", "i,u");
    T.eq(el.getAttribute("data-ah-value"), "i,u");
    AH.invoke(el, "clear");
    T.eq(el.getAttribute("data-ah-value"), "");
    T.eq(changes.length, 2, "methods fire no change");
  });

  T.test("button-group: default mode flashes pressed, hover class; re-insertion", async function (fx) {
    var el = await mount(fx, FX.plain), changes = events(el, "change");
    var b = el.querySelector(".ah-btn-group-btn");
    b.click();
    T.ok(b.classList.contains("ah-btn-group-btn-pressed"));
    await sleep(200);
    T.ok(!b.classList.contains("ah-btn-group-btn-pressed"), "the flash ends");
    T.fire(b, "mouseover");
    T.ok(b.classList.contains("ah-btn-group-btn-hover"));
    T.fire(b, "mouseout");
    T.ok(!b.classList.contains("ah-btn-group-btn-hover"));
    T.eq(changes, [], "no value in the default mode");
    var r = await mount(fx, FX.radio), seen = events(r, "change");
    await reinsert(fx, r);
    r.querySelectorAll(".ah-btn-group-btn")[2].click();
    T.eq(seen, ["board"], "one change after re-insertion");
  });
})(window.AHTest, window.AH);
