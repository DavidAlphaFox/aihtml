/* alert behaviour (alert.js). Fixtures are reduced copies of
 * aihtml_alert's render (a dismissible alert). */
(function (T, AH) {
  "use strict";

  var ALERT = '<div class="ah-alert ah-alert-success ah-alert-dismissible" role="alert" data-ah="alert">' +
    '<div class="ah-alert-content"><div class="ah-alert-title">Saved</div>' +
    '<div class="ah-alert-body">Your changes were saved.</div></div>' +
    '<button class="ah-alert-close" type="button" aria-label="Close">×</button></div>';

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function tick() { return new Promise(function (res) { setTimeout(res, 0); }); }

  T.test("alert: the close button fires ah:dismiss and removes the alert", async function (fx) {
    var el = await mount(fx, ALERT), seen = 0;
    el.addEventListener("ah:dismiss", function () { seen++; });
    el.querySelector(".ah-alert-close").click();
    T.eq(seen, 1);
    T.ok(!el.isConnected, "removed");
  });

  T.test("alert: a cancelled ah:dismiss keeps it; dismiss() from the server", async function (fx) {
    var el = await mount(fx, ALERT);
    var keep = function (e) { e.preventDefault(); };
    el.addEventListener("ah:dismiss", keep);
    el.querySelector(".ah-alert-close").click();
    T.ok(el.isConnected, "kept");
    el.removeEventListener("ah:dismiss", keep);
    // what on(ah:dismiss, ...) sees: a native event from the root
    var got = null;
    document.addEventListener("ah:dismiss", function h(e) { got = e.target; document.removeEventListener("ah:dismiss", h); });
    AH.invoke(el, "dismiss");
    T.eq(got, el);
    T.ok(!el.isConnected, "removed by the method");
  });

  T.test("alert: removed and inserted again, it works", async function (fx) {
    var el = await mount(fx, ALERT);
    el.remove();
    await tick();
    fx.appendChild(el);
    await T.ready(fx);
    el.querySelector(".ah-alert-close").click();
    T.ok(!el.isConnected, "dismissed after re-insertion");
  });
})(window.AHTest, window.AH);
