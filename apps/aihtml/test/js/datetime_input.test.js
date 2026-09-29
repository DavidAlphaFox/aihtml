/* Datetime input behaviour (datetime_input.js). The fixtures are server
 * renders from aihtml_datetime_input, captured once; regenerate them if
 * the markup changes. Events are native (T.fire, T.key); every test
 * awaits T.ready after inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"dte":"<div class=\"ah-dti-group\" id=\"t-de\" data-ah=\"datetime-input\" data-ah-value=\"\" data-ah-format=\"dd/MM/yyyy\" data-ah-first-day=\"0\"><div class=\"ah-dti-row\"><input class=\"ah-dti-input\" type=\"text\" id=\"t-de-input\" readonly placeholder=\"\" autocomplete=\"off\" spellcheck=\"false\" value=\"\" aria-haspopup=\"dialog\" aria-expanded=\"false\" aria-controls=\"t-de-dropdown\" aria-description=\"Arrow keys change the selected part\"><div class=\"ah-dti-cal-btn\" data-action=\"toggle-dropdown\" aria-hidden=\"true\">📅</div></div><span class=\"ah-dti-live\" aria-live=\"polite\" aria-atomic=\"true\"></span><div class=\"ah-dti-dropdown\" id=\"t-de-dropdown\" role=\"dialog\" aria-label=\"Choose date\" hidden></div></div>","dti":"<div class=\"ah-dti-group\" id=\"t-d\" data-ah=\"datetime-input\" data-ah-value=\"2026-09-29\" data-ah-format=\"yyyy-MM-dd\" data-ah-min=\"2026-01-01\" data-ah-max=\"2026-12-31\" data-ah-first-day=\"0\"><div class=\"ah-dti-row\"><input class=\"ah-dti-input\" type=\"text\" id=\"t-d-input\" readonly placeholder=\"\" autocomplete=\"off\" spellcheck=\"false\" value=\"2026-09-29\" aria-haspopup=\"dialog\" aria-expanded=\"false\" aria-controls=\"t-d-dropdown\" aria-description=\"Arrow keys change the selected part\"><div class=\"ah-dti-cal-btn\" data-action=\"toggle-dropdown\" aria-hidden=\"true\">📅</div></div><span class=\"ah-dti-live\" aria-live=\"polite\" aria-atomic=\"true\"></span><input type=\"hidden\" name=\"due\" value=\"2026-09-29\"><div class=\"ah-dti-dropdown\" id=\"t-d-dropdown\" role=\"dialog\" aria-label=\"Choose date\" hidden></div></div>","dtt":"<div class=\"ah-dti-group\" id=\"t-dt\" data-ah=\"datetime-input\" data-ah-value=\"2026-09-29T14:05\" data-ah-format=\"yyyy-MM-dd hh:mm a\" data-ah-first-day=\"0\" data-ah-show-time><div class=\"ah-dti-row\"><input class=\"ah-dti-input\" type=\"text\" id=\"t-dt-input\" readonly placeholder=\"\" autocomplete=\"off\" spellcheck=\"false\" value=\"2026-09-29 02:05 PM\" aria-haspopup=\"dialog\" aria-expanded=\"false\" aria-controls=\"t-dt-dropdown\" aria-description=\"Arrow keys change the selected part\"><div class=\"ah-dti-cal-btn\" data-action=\"toggle-dropdown\" aria-hidden=\"true\">📅</div><div class=\"ah-dti-spinner\" aria-hidden=\"true\"><button class=\"ah-dti-spin ah-dti-spin-up\" type=\"button\" tabindex=\"-1\" aria-label=\"Increment\"><svg width=\"9\" height=\"9\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"3\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m6 15 6-6 6 6\"/></svg></button><button class=\"ah-dti-spin ah-dti-spin-down\" type=\"button\" tabindex=\"-1\" aria-label=\"Decrement\"><svg width=\"9\" height=\"9\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"3\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m6 9 6 6 6-6\"/></svg></button></div></div><span class=\"ah-dti-live\" aria-live=\"polite\" aria-atomic=\"true\"></span><div class=\"ah-dti-dropdown\" id=\"t-dt-dropdown\" role=\"dialog\" aria-label=\"Choose date\" hidden></div></div>"};

  async function mount(fx, html) { fx.innerHTML = html; await T.ready(fx); return fx.firstChild; }
  function events(el, name) {
    var seen = [];
    el.addEventListener(name, function (e) {
      if (e.target === el) { seen.push(name === "change" || name === "input" ? el.getAttribute("data-ah-value") : Object.assign({}, el.dataset)); }
    });
    return seen;
  }
  function q(el, sel) { return el.querySelector(sel); }
  function tick() { return new Promise(function (res) { setTimeout(res, 10); }); }

  T.test("datetime_input: segments, keys, change on leaving", async function (fx) {
    var el = await mount(fx, FX.dti), input = q(el, ".ah-dti-input"), ch = events(el, "change"), inp = events(el, "input");
    input.focus();
    T.ok(el.classList.contains("ah-dti-focused"));
    T.key(input, "ArrowUp");                               // year
    T.eq(el.getAttribute("data-ah-value"), "2027-09-29");
    T.key(input, "ArrowRight"); T.key(input, "ArrowDown");     // month
    T.eq(input.value, "2027-08-29");
    T.eq(q(el, ".ah-dti-live").textContent, "Month 08");
    T.key(input, "ArrowRight"); T.key(input, "3"); T.key(input, "1");
    T.eq(input.value, "2027-08-31");
    T.key(input, "Home"); T.key(input, "PageUp");
    T.eq(input.value, "2037-08-31");
    T.eq(ch.length, 0, "no change while editing");
    T.eq(inp, ["2027-09-29", "2027-08-29", "2027-08-31", "2037-08-31"]);
    input.blur();
    T.fire(el, "focusout", { relatedTarget: null });
    await tick();
    T.eq(ch, ["2026-12-31"], "clamped to max on leaving");
    T.eq(q(el, "input[type=hidden]").value, "2026-12-31");
  });

  T.test("datetime_input: calendar drop-down, time fields, AM/PM, spinner", async function (fx) {
    var el = await mount(fx, FX.dtt), input = q(el, ".ah-dti-input"), ch = events(el, "change");
    T.eq(input.value, "2026-09-29 02:05 PM");
    input.focus();
    T.key(input, "F4");
    T.ok(!q(el, ".ah-dti-dropdown").hidden, "opens");
    T.eq(q(el, ".ah-dti-cal-title").textContent, "September 2026");
    T.eq(q(el, ".ah-dti-cal-day-selected").getAttribute("data-date"), "2026-09-29");
    q(el, '[data-action="next-month"]').click();
    T.eq(q(el, ".ah-dti-cal-title").textContent, "October 2026");
    q(el, '.ah-dti-cal-day[data-date="2026-10-02"]').click();
    T.eq(ch, ["2026-10-02T14:05"], "picking keeps the time and fires change");
    T.ok(!q(el, ".ah-dti-dropdown").hidden, "stays open with show_time");
    var hours = q(el, '[data-field="hours"]');
    hours.value = "9";
    T.fire(hours, "change");
    T.eq(el.getAttribute("data-ah-value"), "2026-10-02T09:05");
    T.key(input, "Escape");
    T.ok(q(el, ".ah-dti-dropdown").hidden, "escape closes");
    T.key(input, "End");                                   // AM/PM
    T.key(input, "p");
    T.eq(input.value, "2026-10-02 09:05 PM");
    T.fire(q(el, ".ah-dti-spin-up"), "mousedown");
    T.fire(q(el, ".ah-dti-spin-up"), "mouseup");
    T.eq(input.value, "2026-10-02 09:05 AM");
    AH.invoke(el, "setValue", "2026-01-01T00:00");
    T.eq(input.value, "2026-01-01 12:00 AM");
    AH.invoke(el, "clear");
    T.eq(el.getAttribute("data-ah-value"), "");
    T.eq(ch[ch.length - 1], "");
  });

  T.test("datetime_input: typing into an empty field", async function (fx) {
    var el = await mount(fx, FX.dte), input = q(el, ".ah-dti-input");
    input.focus();
    T.key(input, "0"); T.key(input, "5");                      // day, then moves to month
    T.key(input, "1"); T.key(input, "1");
    T.key(input, "2"); T.key(input, "0"); T.key(input, "3");
    T.eq(input.value.slice(-4, -1), " 20".slice(0, 0) + input.value.slice(-4, -1));
    T.key(input, "0");
    T.eq(el.getAttribute("data-ah-value"), "2030-11-05");
    T.eq(input.value, "05/11/2030");
  });

  T.test("datetime_input: open/close methods and events, outside press, cleanup", async function (fx) {
    var el = await mount(fx, FX.dtt), dd = q(el, ".ah-dti-dropdown"), oc = [];
    el.addEventListener("ah:open", function () { oc.push("open"); });
    el.addEventListener("ah:close", function () { oc.push("close"); });
    AH.invoke(el, "open");
    T.eq([dd.hidden, q(el, ".ah-dti-input").getAttribute("aria-expanded")], [false, "true"]);
    T.eq(getComputedStyle(dd).position, "fixed");
    T.fire(document.body, "mousedown");
    T.eq([dd.hidden, dd.innerHTML], [true, ""], "a press outside closes and empties it");
    q(el, ".ah-dti-cal-btn").click();
    T.eq(dd.hidden, false, "the calendar button opens");
    AH.invoke(el, "close");
    T.eq(oc, ["open", "close", "open", "close"]);
    T.eq(AH.invoke(el, "getValue"), "2026-09-29T14:05");
    fx.removeChild(el);
    await new Promise(function (r) { setTimeout(r, 0); });
    fx.appendChild(el);
    await T.ready(fx);
    var inp = events(el, "input"), input = q(el, ".ah-dti-input");
    input.focus();
    T.key(input, "ArrowUp");
    T.eq(inp, ["2027-09-29T14:05"], "one listener after re-insertion");
  });
})(window.AHTest, window.AH);
