/* progress-circle behaviour (progress_circle.js). Fixtures are reduced
 * copies of aihtml_progress_circle's render. */
(function (T, AH) {
  "use strict";

  var CIRCLE = '<div class="ah-progress-circle" role="progressbar" aria-valuemin="0" aria-valuemax="100" aria-valuenow="25"' +
    ' aria-valuetext="25%" data-ah="progress-circle" data-ah-value="25"><div class="ah-progress-circle-ring">' +
    '<svg viewBox="0 0 100 100" aria-hidden="true"><circle class="ah-progress-circle-track" cx="50" cy="50" r="45"></circle>' +
    '<circle class="ah-progress-circle-fill" cx="50" cy="50" r="45" stroke-dasharray="282.7433" stroke-dashoffset="212.0575"></circle>' +
    '</svg><span class="ah-progress-circle-value">25%</span></div></div>';

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function tick() { return new Promise(function (res) { setTimeout(res, 0); }); }

  T.test("progress-circle: setValue, getValue, change and ah:complete", async function (fx) {
    var el = await mount(fx, CIRCLE), seen = [];
    ["change", "ah:complete"].forEach(function (t) {
      el.addEventListener(t, function (e) { seen.push([t, e.detail.previous, e.detail.value]); });
    });
    AH.invoke(el, "setValue", 50.7);
    T.eq(AH.invoke(el, "getValue"), 50);
    T.eq(el.querySelector(".ah-progress-circle-value").textContent, "50%");
    T.eq(Math.round(parseFloat(el.querySelector(".ah-progress-circle-fill").getAttribute("stroke-dashoffset"))), 141);
    T.eq(el.getAttribute("aria-valuetext"), "50%");
    AH.invoke(el, "setValue", 120);
    T.eq(seen, [["change", 25, 50], ["change", 50, 100], ["ah:complete", 50, 100]]);
  });

  T.test("progress-circle: removed and inserted again, it works", async function (fx) {
    var el = await mount(fx, CIRCLE);
    el.remove();
    await tick();
    fx.appendChild(el);
    await T.ready(fx);
    AH.invoke(el, "setValue", 5);
    T.eq(el.getAttribute("aria-valuenow"), "5");
  });
})(window.AHTest, window.AH);
