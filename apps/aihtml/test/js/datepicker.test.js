/* Datepicker behaviour (datepicker.js). The fixtures are server renders
 * from aihtml_datepicker, captured once; regenerate them if the markup
 * changes. */
(function (T, $, AH) {
  "use strict";

  var FX = {"dp":"<div class=\"ah-datepicker\" id=\"t-dp\" data-ah=\"datepicker\" data-ah-value=\"2026-09-29\" data-ah-format=\"yyyy-MM-dd\" data-ah-min=\"2026-09-10\" data-ah-first-day=\"0\" data-ah-labels=\"{&quot;clear&quot;:&quot;Clear&quot;,&quot;months&quot;:[&quot;January&quot;,&quot;February&quot;,&quot;March&quot;,&quot;April&quot;,&quot;May&quot;,&quot;June&quot;,&quot;July&quot;,&quot;August&quot;,&quot;September&quot;,&quot;October&quot;,&quot;November&quot;,&quot;December&quot;],&quot;months_short&quot;:[&quot;Jan&quot;,&quot;Feb&quot;,&quot;Mar&quot;,&quot;Apr&quot;,&quot;May&quot;,&quot;Jun&quot;,&quot;Jul&quot;,&quot;Aug&quot;,&quot;Sep&quot;,&quot;Oct&quot;,&quot;Nov&quot;,&quot;Dec&quot;],&quot;next_month&quot;:&quot;Next month&quot;,&quot;next_year&quot;:&quot;Next year&quot;,&quot;prev_month&quot;:&quot;Previous month&quot;,&quot;prev_year&quot;:&quot;Previous year&quot;,&quot;title&quot;:&quot;MMMM yyyy&quot;,&quot;today&quot;:&quot;Today&quot;,&quot;weekdays&quot;:[&quot;Su&quot;,&quot;Mo&quot;,&quot;Tu&quot;,&quot;We&quot;,&quot;Th&quot;,&quot;Fr&quot;,&quot;Sa&quot;]}\"><div class=\"ah-datepicker-input-area\"><input class=\"ah-datepicker-input\" type=\"text\" id=\"t-dp-input\" autocomplete=\"off\" spellcheck=\"false\" readonly placeholder=\"Select date...\" value=\"2026-09-29\" role=\"combobox\" aria-haspopup=\"dialog\" aria-expanded=\"false\"><span class=\"ah-datepicker-trigger\" aria-hidden=\"true\"><span class=\"ah-datepicker-icon\">📅</span></span></div><input type=\"hidden\" name=\"d\" value=\"2026-09-29\"><div class=\"ah-datepicker-popup\" role=\"dialog\" aria-label=\"Choose date\"></div></div>","dpr":"<div class=\"ah-datepicker ah-datepicker-clearable ah-datepicker-range\" id=\"t-dpr\" data-ah=\"datepicker\" data-ah-value=\"\" data-ah-range data-ah-format=\"yyyy-MM-dd\" data-ah-first-day=\"0\" data-ah-labels=\"{&quot;clear&quot;:&quot;Clear&quot;,&quot;months&quot;:[&quot;January&quot;,&quot;February&quot;,&quot;March&quot;,&quot;April&quot;,&quot;May&quot;,&quot;June&quot;,&quot;July&quot;,&quot;August&quot;,&quot;September&quot;,&quot;October&quot;,&quot;November&quot;,&quot;December&quot;],&quot;months_short&quot;:[&quot;Jan&quot;,&quot;Feb&quot;,&quot;Mar&quot;,&quot;Apr&quot;,&quot;May&quot;,&quot;Jun&quot;,&quot;Jul&quot;,&quot;Aug&quot;,&quot;Sep&quot;,&quot;Oct&quot;,&quot;Nov&quot;,&quot;Dec&quot;],&quot;next_month&quot;:&quot;Next month&quot;,&quot;next_year&quot;:&quot;Next year&quot;,&quot;prev_month&quot;:&quot;Previous month&quot;,&quot;prev_year&quot;:&quot;Previous year&quot;,&quot;title&quot;:&quot;MMMM yyyy&quot;,&quot;today&quot;:&quot;Today&quot;,&quot;weekdays&quot;:[&quot;Su&quot;,&quot;Mo&quot;,&quot;Tu&quot;,&quot;We&quot;,&quot;Th&quot;,&quot;Fr&quot;,&quot;Sa&quot;]}\"><div class=\"ah-datepicker-input-area\"><input class=\"ah-datepicker-input\" type=\"text\" id=\"t-dpr-input\" autocomplete=\"off\" spellcheck=\"false\" readonly placeholder=\"Select date...\" value=\"\" role=\"combobox\" aria-haspopup=\"dialog\" aria-expanded=\"false\"><button class=\"ah-datepicker-clear\" type=\"button\" tabindex=\"-1\" aria-label=\"Clear\">&times;</button><span class=\"ah-datepicker-trigger\" aria-hidden=\"true\"><span class=\"ah-datepicker-icon\">📅</span></span></div><div class=\"ah-datepicker-popup\" role=\"dialog\" aria-label=\"Choose date\"></div></div>"};

  function mount(fx, html) { fx.innerHTML = html; AH.mount(fx); return fx.firstChild; }
  function key(el, k, extra) { $(el).trigger($.Event("keydown", $.extend({ key: k }, extra || {}))); }
  function changes(el) {
    var seen = [];
    $(el).on("change", function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }

  T.test("datepicker: month grid from the shared template, keyboard, change", function (fx) {
    var el = mount(fx, FX.dp), $in = $(el).find(".ah-datepicker-input"), seen = changes(el);
    $in[0].focus();
    key($in, "ArrowDown");
    T.ok($(el).hasClass("ah-datepicker-open"), "opens");
    T.eq($(el).find(".ah-datepicker-title").text(), "September 2026");
    T.eq($(el).find(".ah-datepicker-weekday").first().text(), "Su");
    T.eq($(el).find(".ah-datepicker-day-focused").attr("id"), "t-dp-d2026-09-29");
    T.eq($(el).find('[data-date="2026-09-09"]').attr("aria-disabled"), "true", "before min");
    key($in, "ArrowUp"); key($in, "ArrowLeft"); key($in, "Enter");
    T.eq(el.getAttribute("data-ah-value"), "2026-09-21");
    T.eq($(el).find("input[type=hidden]").val(), "2026-09-21");
    T.eq(seen, ["2026-09-21"]);
    T.ok(!$(el).hasClass("ah-datepicker-open"), "closes");
    key($in, "Enter"); key($in, "PageDown"); key($in, "End");
    T.eq($(el).find(".ah-datepicker-day-focused").attr("data-date"), "2026-10-31");
    key($in, "Escape");
    T.ok(!$(el).hasClass("ah-datepicker-open"), "escape");
  });

  T.test("datepicker: range, hover preview, clear", function (fx) {
    var el = mount(fx, FX.dpr), seen = changes(el);
    AH.invoke(el, "setValue", "2026-03-01");
    AH.invoke(el, "open");
    T.eq(el.getAttribute("data-ah-value"), "2026-03-01,", "setValue sets without an event");
    $(el).find('[data-date="2026-03-10"]').trigger("click");
    $(el).find('[data-date="2026-03-04"]').trigger("mouseenter");
    T.eq($(el).find(".ah-datepicker-day-hover-range").length, 7);
    $(el).find('[data-date="2026-03-04"]').trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "2026-03-04,2026-03-10");
    T.eq($(el).find(".ah-datepicker-input").val(), "2026-03-04 - 2026-03-10");
    $(el).find(".ah-datepicker-clear").trigger("click");
    T.eq(seen, ["2026-03-04,2026-03-10", ""]);
  });
})(window.AHTest, window.jQuery, window.AH);
