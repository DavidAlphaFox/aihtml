/* Datetime input behaviour (datetime_input.js). The fixtures are server
 * renders from aihtml_datetime_input, captured once; regenerate them if
 * the markup changes. */
(function (T, $, AH) {
  "use strict";

  var FX = {"dte":"<div class=\"ah-dti-group\" id=\"t-de\" data-ah=\"datetime-input\" data-ah-value=\"\" data-ah-format=\"dd/MM/yyyy\" data-ah-first-day=\"0\"><div class=\"ah-dti-row\"><input class=\"ah-dti-input\" type=\"text\" id=\"t-de-input\" readonly placeholder=\"\" autocomplete=\"off\" spellcheck=\"false\" value=\"\" aria-haspopup=\"dialog\" aria-expanded=\"false\" aria-controls=\"t-de-dropdown\" aria-description=\"Arrow keys change the selected part\"><div class=\"ah-dti-cal-btn\" data-action=\"toggle-dropdown\" aria-hidden=\"true\">📅</div></div><span class=\"ah-dti-live\" aria-live=\"polite\" aria-atomic=\"true\"></span><div class=\"ah-dti-dropdown\" id=\"t-de-dropdown\" role=\"dialog\" aria-label=\"Choose date\" hidden></div></div>","dti":"<div class=\"ah-dti-group\" id=\"t-d\" data-ah=\"datetime-input\" data-ah-value=\"2026-09-29\" data-ah-format=\"yyyy-MM-dd\" data-ah-min=\"2026-01-01\" data-ah-max=\"2026-12-31\" data-ah-first-day=\"0\"><div class=\"ah-dti-row\"><input class=\"ah-dti-input\" type=\"text\" id=\"t-d-input\" readonly placeholder=\"\" autocomplete=\"off\" spellcheck=\"false\" value=\"2026-09-29\" aria-haspopup=\"dialog\" aria-expanded=\"false\" aria-controls=\"t-d-dropdown\" aria-description=\"Arrow keys change the selected part\"><div class=\"ah-dti-cal-btn\" data-action=\"toggle-dropdown\" aria-hidden=\"true\">📅</div></div><span class=\"ah-dti-live\" aria-live=\"polite\" aria-atomic=\"true\"></span><input type=\"hidden\" name=\"due\" value=\"2026-09-29\"><div class=\"ah-dti-dropdown\" id=\"t-d-dropdown\" role=\"dialog\" aria-label=\"Choose date\" hidden></div></div>","dtt":"<div class=\"ah-dti-group\" id=\"t-dt\" data-ah=\"datetime-input\" data-ah-value=\"2026-09-29T14:05\" data-ah-format=\"yyyy-MM-dd hh:mm a\" data-ah-first-day=\"0\" data-ah-show-time><div class=\"ah-dti-row\"><input class=\"ah-dti-input\" type=\"text\" id=\"t-dt-input\" readonly placeholder=\"\" autocomplete=\"off\" spellcheck=\"false\" value=\"2026-09-29 02:05 PM\" aria-haspopup=\"dialog\" aria-expanded=\"false\" aria-controls=\"t-dt-dropdown\" aria-description=\"Arrow keys change the selected part\"><div class=\"ah-dti-cal-btn\" data-action=\"toggle-dropdown\" aria-hidden=\"true\">📅</div><div class=\"ah-dti-spinner\" aria-hidden=\"true\"><button class=\"ah-dti-spin ah-dti-spin-up\" type=\"button\" tabindex=\"-1\" aria-label=\"Increment\"><svg width=\"9\" height=\"9\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"3\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m6 15 6-6 6 6\"/></svg></button><button class=\"ah-dti-spin ah-dti-spin-down\" type=\"button\" tabindex=\"-1\" aria-label=\"Decrement\"><svg width=\"9\" height=\"9\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"3\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m6 9 6 6 6-6\"/></svg></button></div></div><span class=\"ah-dti-live\" aria-live=\"polite\" aria-atomic=\"true\"></span><div class=\"ah-dti-dropdown\" id=\"t-dt-dropdown\" role=\"dialog\" aria-label=\"Choose date\" hidden></div></div>"};

  function mount(fx, html) { fx.innerHTML = html; AH.mount(fx); return fx.firstChild; }
  function events(el, name) {
    var seen = [];
    $(el).on(name, function (e) {
      if (e.target === el) { seen.push(name === "change" || name === "input" ? el.getAttribute("data-ah-value") : $.extend({}, el.dataset)); }
    });
    return seen;
  }
  function key(el, k, extra) { $(el).trigger($.Event("keydown", $.extend({ key: k }, extra || {}))); }

  T.test("datetime_input: segments, keys, change on leaving", function (fx) {
    var el = mount(fx, FX.dti), $in = $(el).find(".ah-dti-input"), ch = events(el, "change"), inp = events(el, "input");
    $in[0].focus();
    T.ok($(el).hasClass("ah-dti-focused"));
    key($in, "ArrowUp");                               // year
    T.eq(el.getAttribute("data-ah-value"), "2027-09-29");
    key($in, "ArrowRight"); key($in, "ArrowDown");     // month
    T.eq($in.val(), "2027-08-29");
    T.eq($(el).find(".ah-dti-live").text(), "Month 08");
    key($in, "ArrowRight"); key($in, "3"); key($in, "1");
    T.eq($in.val(), "2027-08-31");
    key($in, "Home"); key($in, "PageUp");
    T.eq($in.val(), "2037-08-31");
    T.eq(ch.length, 0, "no change while editing");
    T.eq(inp, ["2027-09-29", "2027-08-29", "2027-08-31", "2037-08-31"]);
    $in[0].blur();
    $(el).trigger($.Event("focusout", { relatedTarget: null }));
    return new Promise(function (res) { setTimeout(res, 10); }).then(function () {
      T.eq(ch, ["2026-12-31"], "clamped to max on leaving");
      T.eq($(el).find("input[type=hidden]").val(), "2026-12-31");
    });
  });

  T.test("datetime_input: calendar drop-down, time fields, AM/PM, spinner", function (fx) {
    var el = mount(fx, FX.dtt), $in = $(el).find(".ah-dti-input"), ch = events(el, "change");
    T.eq($in.val(), "2026-09-29 02:05 PM");
    $in[0].focus();
    key($in, "F4");
    T.ok(!$(el).find(".ah-dti-dropdown").prop("hidden"), "opens");
    T.eq($(el).find(".ah-dti-cal-title").text(), "September 2026");
    T.eq($(el).find(".ah-dti-cal-day-selected").attr("data-date"), "2026-09-29");
    $(el).find('[data-action="next-month"]').trigger("click");
    T.eq($(el).find(".ah-dti-cal-title").text(), "October 2026");
    $(el).find('.ah-dti-cal-day[data-date="2026-10-02"]').trigger("click");
    T.eq(ch, ["2026-10-02T14:05"], "picking keeps the time and fires change");
    T.ok(!$(el).find(".ah-dti-dropdown").prop("hidden"), "stays open with show_time");
    $(el).find('[data-field="hours"]').val("9").trigger("change");
    T.eq(el.getAttribute("data-ah-value"), "2026-10-02T09:05");
    key($in, "Escape");
    T.ok($(el).find(".ah-dti-dropdown").prop("hidden"), "escape closes");
    key($in, "End");                                   // AM/PM
    key($in, "p");
    T.eq($in.val(), "2026-10-02 09:05 PM");
    $(el).find(".ah-dti-spin-up").trigger("mousedown").trigger("mouseup");
    T.eq($in.val(), "2026-10-02 09:05 AM");
    AH.invoke(el, "setValue", "2026-01-01T00:00");
    T.eq($in.val(), "2026-01-01 12:00 AM");
    AH.invoke(el, "clear");
    T.eq(el.getAttribute("data-ah-value"), "");
    T.eq(ch[ch.length - 1], "");
  });

  T.test("datetime_input: typing into an empty field", function (fx) {
    var el = mount(fx, FX.dte), $in = $(el).find(".ah-dti-input");
    $in[0].focus();
    key($in, "0"); key($in, "5");                      // day, then moves to month
    key($in, "1"); key($in, "1");
    key($in, "2"); key($in, "0"); key($in, "3");
    T.eq($in.val().slice(-4, -1), " 20".slice(0, 0) + $in.val().slice(-4, -1));
    key($in, "0");
    T.eq(el.getAttribute("data-ah-value"), "2030-11-05");
    T.eq($in.val(), "05/11/2030");
  });
})(window.AHTest, window.jQuery, window.AH);
