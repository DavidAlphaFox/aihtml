/* Datepicker behaviour (datepicker.js). The fixtures are server renders
 * from aihtml_datepicker, captured once; regenerate them if the markup
 * changes. Events are native (T.fire, T.key); every test awaits T.ready
 * after inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"dp":"<div class=\"ah-datepicker\" id=\"t-dp\" data-ah=\"datepicker\" data-ah-value=\"2026-09-29\" data-ah-format=\"yyyy-MM-dd\" data-ah-min=\"2026-09-10\" data-ah-first-day=\"0\" data-ah-labels=\"{&quot;clear&quot;:&quot;Clear&quot;,&quot;months&quot;:[&quot;January&quot;,&quot;February&quot;,&quot;March&quot;,&quot;April&quot;,&quot;May&quot;,&quot;June&quot;,&quot;July&quot;,&quot;August&quot;,&quot;September&quot;,&quot;October&quot;,&quot;November&quot;,&quot;December&quot;],&quot;months_short&quot;:[&quot;Jan&quot;,&quot;Feb&quot;,&quot;Mar&quot;,&quot;Apr&quot;,&quot;May&quot;,&quot;Jun&quot;,&quot;Jul&quot;,&quot;Aug&quot;,&quot;Sep&quot;,&quot;Oct&quot;,&quot;Nov&quot;,&quot;Dec&quot;],&quot;next_month&quot;:&quot;Next month&quot;,&quot;next_year&quot;:&quot;Next year&quot;,&quot;prev_month&quot;:&quot;Previous month&quot;,&quot;prev_year&quot;:&quot;Previous year&quot;,&quot;title&quot;:&quot;MMMM yyyy&quot;,&quot;today&quot;:&quot;Today&quot;,&quot;weekdays&quot;:[&quot;Su&quot;,&quot;Mo&quot;,&quot;Tu&quot;,&quot;We&quot;,&quot;Th&quot;,&quot;Fr&quot;,&quot;Sa&quot;]}\"><div class=\"ah-datepicker-input-area\"><input class=\"ah-datepicker-input\" type=\"text\" id=\"t-dp-input\" autocomplete=\"off\" spellcheck=\"false\" readonly placeholder=\"Select date...\" value=\"2026-09-29\" role=\"combobox\" aria-haspopup=\"dialog\" aria-expanded=\"false\"><span class=\"ah-datepicker-trigger\" aria-hidden=\"true\"><span class=\"ah-datepicker-icon\">📅</span></span></div><input type=\"hidden\" name=\"d\" value=\"2026-09-29\"><div class=\"ah-datepicker-popup\" role=\"dialog\" aria-label=\"Choose date\"></div></div>","dpr":"<div class=\"ah-datepicker ah-datepicker-clearable ah-datepicker-range\" id=\"t-dpr\" data-ah=\"datepicker\" data-ah-value=\"\" data-ah-range data-ah-format=\"yyyy-MM-dd\" data-ah-first-day=\"0\" data-ah-labels=\"{&quot;clear&quot;:&quot;Clear&quot;,&quot;months&quot;:[&quot;January&quot;,&quot;February&quot;,&quot;March&quot;,&quot;April&quot;,&quot;May&quot;,&quot;June&quot;,&quot;July&quot;,&quot;August&quot;,&quot;September&quot;,&quot;October&quot;,&quot;November&quot;,&quot;December&quot;],&quot;months_short&quot;:[&quot;Jan&quot;,&quot;Feb&quot;,&quot;Mar&quot;,&quot;Apr&quot;,&quot;May&quot;,&quot;Jun&quot;,&quot;Jul&quot;,&quot;Aug&quot;,&quot;Sep&quot;,&quot;Oct&quot;,&quot;Nov&quot;,&quot;Dec&quot;],&quot;next_month&quot;:&quot;Next month&quot;,&quot;next_year&quot;:&quot;Next year&quot;,&quot;prev_month&quot;:&quot;Previous month&quot;,&quot;prev_year&quot;:&quot;Previous year&quot;,&quot;title&quot;:&quot;MMMM yyyy&quot;,&quot;today&quot;:&quot;Today&quot;,&quot;weekdays&quot;:[&quot;Su&quot;,&quot;Mo&quot;,&quot;Tu&quot;,&quot;We&quot;,&quot;Th&quot;,&quot;Fr&quot;,&quot;Sa&quot;]}\"><div class=\"ah-datepicker-input-area\"><input class=\"ah-datepicker-input\" type=\"text\" id=\"t-dpr-input\" autocomplete=\"off\" spellcheck=\"false\" readonly placeholder=\"Select date...\" value=\"\" role=\"combobox\" aria-haspopup=\"dialog\" aria-expanded=\"false\"><button class=\"ah-datepicker-clear\" type=\"button\" tabindex=\"-1\" aria-label=\"Clear\">&times;</button><span class=\"ah-datepicker-trigger\" aria-hidden=\"true\"><span class=\"ah-datepicker-icon\">📅</span></span></div><div class=\"ah-datepicker-popup\" role=\"dialog\" aria-label=\"Choose date\"></div></div>"};

  async function mount(fx, html) { fx.innerHTML = html; await T.ready(fx); return fx.firstChild; }
  function changes(el) {
    var seen = [];
    el.addEventListener("change", function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function q(el, sel) { return el.querySelector(sel); }
  function open(el) { return el.classList.contains("ah-datepicker-open"); }

  T.test("datepicker: month grid from the shared template, keyboard, change", async function (fx) {
    var el = await mount(fx, FX.dp), input = q(el, ".ah-datepicker-input"), seen = changes(el);
    input.focus();
    T.key(input, "ArrowDown");
    T.ok(open(el), "opens");
    T.eq(q(el, ".ah-datepicker-title").textContent, "September 2026");
    T.eq(q(el, ".ah-datepicker-weekday").textContent, "Su");
    T.eq(q(el, ".ah-datepicker-day-focused").id, "t-dp-d2026-09-29");
    T.eq(q(el, '[data-date="2026-09-09"]').getAttribute("aria-disabled"), "true", "before min");
    T.key(input, "ArrowUp"); T.key(input, "ArrowLeft"); T.key(input, "Enter");
    T.eq(el.getAttribute("data-ah-value"), "2026-09-21");
    T.eq(q(el, "input[type=hidden]").value, "2026-09-21");
    T.eq(seen, ["2026-09-21"]);
    T.ok(!open(el), "closes");
    T.key(input, "Enter"); T.key(input, "PageDown"); T.key(input, "End");
    T.eq(q(el, ".ah-datepicker-day-focused").getAttribute("data-date"), "2026-10-31");
    T.key(input, "Escape");
    T.ok(!open(el), "escape");
  });

  T.test("datepicker: range, hover preview, clear", async function (fx) {
    var el = await mount(fx, FX.dpr), seen = changes(el);
    AH.invoke(el, "setValue", "2026-03-01");
    AH.invoke(el, "open");
    T.eq(el.getAttribute("data-ah-value"), "2026-03-01,", "setValue sets without an event");
    q(el, '[data-date="2026-03-10"]').click();
    T.fire(q(el, '[data-date="2026-03-04"]'), "mouseover");
    T.eq(el.querySelectorAll(".ah-datepicker-day-hover-range").length, 7);
    q(el, '[data-date="2026-03-04"]').click();
    T.eq(el.getAttribute("data-ah-value"), "2026-03-04,2026-03-10");
    T.eq(q(el, ".ah-datepicker-input").value, "2026-03-04 - 2026-03-10");
    q(el, ".ah-datepicker-clear").click();
    T.eq(seen, ["2026-03-04,2026-03-10", ""]);
  });

  T.test("datepicker: trigger, month nav, outside press, open/close events, methods", async function (fx) {
    var el = await mount(fx, FX.dp), seen = changes(el), popup = q(el, ".ah-datepicker-popup"), oc = [];
    el.addEventListener("ah:open", function () { oc.push("open"); });
    el.addEventListener("ah:close", function () { oc.push("close"); });
    T.eq(q(el, ".ah-datepicker-input").getAttribute("aria-controls"), popup.id);
    q(el, ".ah-datepicker-trigger").click();
    T.ok(open(el));
    T.eq(getComputedStyle(popup).position, "fixed");
    T.eq(q(el, ".ah-datepicker-input").getAttribute("aria-expanded"), "true");
    q(el, '[data-nav="1"]').click();
    T.eq(q(el, ".ah-datepicker-title").textContent, "October 2026");
    T.ok(open(el), "nav keeps it open");
    T.fire(document.body, "mousedown");
    T.ok(!open(el), "a press outside closes");
    T.eq(popup.style.display, "none");
    T.eq(oc, ["open", "close"]);
    AH.invoke(el, "setValue", "2026-09-15");
    T.eq(AH.invoke(el, "getValue"), "2026-09-15");
    T.eq(q(el, ".ah-datepicker-input").value, "2026-09-15");
    T.eq(seen, [], "setValue fires nothing");
    AH.invoke(el, "open");
    q(el, '[data-date="2026-09-16"]').click();
    T.eq(seen, ["2026-09-16"]);
    AH.invoke(el, "clear");
    T.eq(seen, ["2026-09-16", ""]);
  });

  T.test("datepicker: removed and re-inserted, it still works; no listener left behind", async function (fx) {
    var el = await mount(fx, FX.dp);
    AH.invoke(el, "open");
    fx.removeChild(el);
    await new Promise(function (r) { setTimeout(r, 0); });
    fx.appendChild(el);
    await T.ready(fx);
    var seen = changes(el), input = q(el, ".ah-datepicker-input");
    AH.invoke(el, "close");
    input.click();
    T.ok(open(el), "one click handler: opens");
    T.key(input, "ArrowRight");
    T.key(input, "Enter");
    T.eq(seen, ["2026-09-30"]);
    fx.innerHTML = "";
    await new Promise(function (r) { setTimeout(r, 0); });
    T.fire(document.body, "mousedown");
  });
})(window.AHTest, window.AH);
