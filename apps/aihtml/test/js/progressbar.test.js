/* progressbar behaviour (progressbar.js). Fixtures are reduced copies of
 * aihtml_progressbar's render (plain, with ranges, custom text). */
(function (T, AH) {
  "use strict";

  function bar(v, extra, inner) {
    return '<div class="ah-progressbar ah-progressbar-horizontal" role="progressbar" aria-valuemin="0" aria-valuemax="100"' +
      ' aria-valuenow="' + v + '" data-ah="progressbar" data-ah-value="' + v + '" data-ah-min="0" data-ah-max="100"' +
      (extra || "") + ">" + (inner || '<div class="ah-progressbar-value" style="width: ' + v + '%;"></div>') +
      '<div class="ah-progressbar-text-host"><span class="ah-progressbar-text">' + v + "%</span></div></div>";
  }
  var RANGES = '<div class="ah-progressbar-range" data-ah-stop="30" style="width: 30%;"></div>' +
    '<div class="ah-progressbar-range" data-ah-stop="60" style="width: 40%;"></div>' +
    '<div class="ah-progressbar-range" data-ah-stop="100" style="width: 40%;"></div>';

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function log(el) {
    var seen = [];
    ["change", "ah:complete"].forEach(function (t) {
      el.addEventListener(t, function (e) { seen.push([t, e.detail.previous, e.detail.value]); });
    });
    return seen;
  }
  function tick() { return new Promise(function (res) { setTimeout(res, 0); }); }

  T.test("progressbar: setValue writes width, text, aria; change and ah:complete", async function (fx) {
    var el = await mount(fx, bar(35)), seen = log(el);
    AH.invoke(el, "setValue", 60);
    T.eq(el.querySelector(".ah-progressbar-value").style.width, "60%");
    T.eq(el.querySelector(".ah-progressbar-text").textContent, "60%");
    T.eq(el.getAttribute("aria-valuenow"), "60");
    T.eq(el.getAttribute("aria-valuetext"), "60%");
    T.eq(AH.invoke(el, "getValue"), 60);
    AH.invoke(el, "setValue", 60);
    AH.invoke(el, "setValue", 150);
    T.eq(AH.invoke(el, "getValue"), 100, "clamped");
    T.eq(seen, [["change", 35, 60], ["change", 60, 100], ["ah:complete", 60, 100]]);
    AH.invoke(el, "setValue", 10, "Step 1");
    T.eq(el.querySelector(".ah-progressbar-text").textContent, "Step 1");
    T.eq(el.getAttribute("aria-valuetext"), "Step 1");
  });

  T.test("progressbar: ranges fill up to their stops; indeterminate ends", async function (fx) {
    var el = await mount(fx, bar(0, ' aria-busy="true"', RANGES));
    el.classList.add("ah-progressbar-indeterminate");
    AH.invoke(el, "setValue", 50);
    var w = Array.prototype.map.call(el.querySelectorAll(".ah-progressbar-range"), function (r) { return r.style.width; });
    T.eq(w, ["30%", "50%", "50%"]);
    T.ok(!el.classList.contains("ah-progressbar-indeterminate") && !el.hasAttribute("aria-busy"));
  });

  T.test("progressbar: custom text is kept; removed and inserted again, it works", async function (fx) {
    var el = await mount(fx, bar(20, ' data-ah-text="custom"'));
    AH.invoke(el, "setValue", 40);
    T.eq(el.querySelector(".ah-progressbar-text").textContent, "20%", "custom text untouched");
    T.eq(el.getAttribute("aria-valuetext"), "40%");
    el.remove();
    await tick();
    fx.appendChild(el);
    await T.ready(fx);
    AH.invoke(el, "setValue", 45);
    T.eq(el.getAttribute("data-ah-value"), "45");
  });
})(window.AHTest, window.AH);
