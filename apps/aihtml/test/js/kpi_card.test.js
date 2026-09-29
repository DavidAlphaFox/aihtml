/* kpi-card behaviour (kpi_card.js). Fixtures are reduced copies of
 * aihtml_kpi_card's render (a card with a trend). */
(function (T, AH) {
  "use strict";

  var CARD = '<div class="ah-kpi-card ah-kpi-card-trend-up" data-ah="kpi-card"><div class="ah-kpi-card-content">' +
    '<span class="ah-kpi-card-trend"><span class="ah-kpi-card-trend-up"><span class="ah-kpi-card-trend-icon" aria-hidden="true">' +
    '<svg viewBox="0 0 24 24"><polyline points="23 6 13.5 15.5 8.5 10.5 1 18"></polyline><polyline points="17 6 23 6 23 12"></polyline></svg>' +
    '</span><span class="ah-kpi-card-trend-value">+5.2%</span></span><span class="ah-kpi-card-trend-label">vs last month</span></span>' +
    '<div class="ah-kpi-card-body"><div class="ah-kpi-card-value-section"><div class="ah-kpi-card-title">Active users</div>' +
    '<div class="ah-kpi-card-value">12,480</div></div></div></div></div>';

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function q(el, s) { return el.querySelector(s); }
  function points(el) {
    return Array.prototype.map.call(el.querySelectorAll("polyline"), function (p) { return p.getAttribute("points"); });
  }
  function tick() { return new Promise(function (res) { setTimeout(res, 0); }); }

  T.test("kpi-card: setValue and setTrend (down, then up)", async function (fx) {
    var el = await mount(fx, CARD);
    AH.invoke(el, "setValue", "13,002");
    T.eq(q(el, ".ah-kpi-card-value").textContent, "13,002");
    AH.invoke(el, "setTrend", -3.14);
    T.ok(el.classList.contains("ah-kpi-card-trend-down") && !el.classList.contains("ah-kpi-card-trend-up"));
    T.ok(q(el, ".ah-kpi-card-trend").firstElementChild.classList.contains("ah-kpi-card-trend-down"));
    T.eq(q(el, ".ah-kpi-card-trend-value").textContent, "-3.1%");
    T.eq(points(el), ["23 18 13.5 8.5 8.5 13.5 1 6", "17 18 23 18 23 12"]);
    AH.invoke(el, "setTrend", "2");
    T.eq(q(el, ".ah-kpi-card-trend-value").textContent, "+2.0%");
    T.eq(points(el), ["23 6 13.5 15.5 8.5 10.5 1 18", "17 6 23 6 23 12"]);
    AH.invoke(el, "setTrend", "n/a");
    T.eq(q(el, ".ah-kpi-card-trend-value").textContent, "+2.0%", "not a number: unchanged");
  });

  T.test("kpi-card: removed and inserted again, it works", async function (fx) {
    var el = await mount(fx, CARD);
    el.remove();
    await tick();
    fx.appendChild(el);
    await T.ready(fx);
    AH.invoke(el, "setValue", 1);
    T.eq(q(el, ".ah-kpi-card-value").textContent, "1");
  });
})(window.AHTest, window.AH);
