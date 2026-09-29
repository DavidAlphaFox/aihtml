/* heatmap_calendar: the heatmap-calendar behaviour on markup the server
 * renders. SERVER holds a render of aihtml_heatmap_calendar:heatmap_calendar/3
 * (id "hm"); regenerate it from Erlang if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "heat": "<div class=\"ah-heatmap-calendar\" data-ah=\"heatmap-calendar\" data-tip=\"{date}: {value}\" id=\"hm\"><div class=\"ah-heatmap-calendar__months\" aria-hidden=\"true\"><span class=\"ah-heatmap-calendar__month\" style=\"width:15px;\">Aug</span><span class=\"ah-heatmap-calendar__month\" style=\"width:75px;\">Sep</span></div><div class=\"ah-heatmap-calendar__body\"><div class=\"ah-heatmap-calendar__weekdays\" aria-hidden=\"true\"><span class=\"ah-heatmap-calendar__weekday\"></span><span class=\"ah-heatmap-calendar__weekday\">Mon</span><span class=\"ah-heatmap-calendar__weekday\"></span><span class=\"ah-heatmap-calendar__weekday\">Wed</span><span class=\"ah-heatmap-calendar__weekday\"></span><span class=\"ah-heatmap-calendar__weekday\">Fri</span><span class=\"ah-heatmap-calendar__weekday\"></span></div><div class=\"ah-heatmap-calendar__grid\"><div class=\"ah-heatmap-calendar__week\"><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-23\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-24\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-25\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-26\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-27\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-28\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-29\" data-value=\"0\"></div></div><div class=\"ah-heatmap-calendar__week\"><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-30\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-31\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"2\" data-date=\"2026-09-01\" data-value=\"3\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-02\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-03\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-04\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-05\" data-value=\"0\"></div></div><div class=\"ah-heatmap-calendar__week\"><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-06\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-07\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-08\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-09\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-10\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-11\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-12\" data-value=\"0\"></div></div><div class=\"ah-heatmap-calendar__week\"><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-13\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-14\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-15\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-16\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-17\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-18\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-19\" data-value=\"0\"></div></div><div class=\"ah-heatmap-calendar__week\"><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-20\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-21\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-22\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-23\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-24\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-25\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-26\" data-value=\"0\"></div></div><div class=\"ah-heatmap-calendar__week\"><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-27\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-28\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-29\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-30\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-10-01\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-10-02\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-10-03\" data-value=\"0\"></div></div></div></div><div class=\"ah-heatmap-calendar__legend\" aria-hidden=\"true\"><span>Less</span><div class=\"ah-heatmap-calendar__legend-cell\" data-level=\"0\"></div><div class=\"ah-heatmap-calendar__legend-cell\" data-level=\"1\"></div><div class=\"ah-heatmap-calendar__legend-cell\" data-level=\"2\"></div><div class=\"ah-heatmap-calendar__legend-cell\" data-level=\"3\"></div><div class=\"ah-heatmap-calendar__legend-cell\" data-level=\"4\"></div><span>More</span></div><div class=\"ah-heatmap-calendar__tooltip\" data-visible=\"false\" role=\"tooltip\"></div></div>"
  };

  async function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    await T.ready(fx);
    return fx.firstChild;
  }

  T.test("heatmap-calendar: tooltip and select", async function (fx) {
    var el = await mount(fx, "heat");
    var cell = el.querySelector("[data-date='2026-09-01']");
    var tip = el.querySelector(".ah-heatmap-calendar__tooltip");
    T.fire(cell, "mouseover", { relatedTarget: el });
    T.eq(tip.getAttribute("data-visible"), "true");
    T.eq(tip.textContent, "2026-09-01: 3");
    T.eq(tip.style.position, "fixed");
    T.fire(cell, "mouseout", { relatedTarget: el });
    T.eq(tip.getAttribute("data-visible"), "false");
    var got = null;
    el.addEventListener("ah:select", function (e) { got = e.detail; });
    cell.click();
    T.eq(got, { date: "2026-09-01", value: 3 });
    T.eq(el.getAttribute("data-ah-value"), "2026-09-01");
  });

  T.test("heatmap-calendar: moving between cells; removal stops the tooltip", async function (fx) {
    var el = await mount(fx, "heat");
    var a = el.querySelector("[data-date='2026-09-01']"), b = el.querySelector("[data-date='2026-09-02']");
    var tip = el.querySelector(".ah-heatmap-calendar__tooltip");
    T.fire(a, "mouseover", { relatedTarget: el });
    T.fire(a, "mouseout", { relatedTarget: b });
    T.fire(b, "mouseover", { relatedTarget: a });
    T.eq(tip.textContent, "2026-09-02: 0");
    T.eq(tip.getAttribute("data-visible"), "true");
    el.remove();
    await new Promise(function (res) { setTimeout(res, 0); });
    T.eq(tip.style.position, "", "float stopped on teardown");
    el = await mount(fx, "heat");
    var got = null;
    el.addEventListener("ah:select", function (e) { got = e.detail.date; });
    el.querySelector("[data-date='2026-09-02']").click();
    T.eq(got, "2026-09-02", "works after re-insertion");
  });
})(window.AHTest, window.AH);
