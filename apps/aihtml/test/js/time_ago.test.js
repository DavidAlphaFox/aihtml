/* time-ago behaviour (time_ago.js). Fixtures are reduced copies of
 * aihtml_time_ago's render. */
(function (T, AH) {
  "use strict";

  function ago(iso, extra) {
    return '<time class="ah-time-ago" datetime="' + iso + '" data-ah="time-ago" data-ah-title="true"' +
      (extra || "") + ">?</time>";
  }
  function iso(msAgo) { return new Date(Date.now() - msAgo).toISOString().replace(/\.\d{3}Z$/, "Z"); }
  var MIN = 60000, HOUR = 60 * MIN, DAY = 24 * HOUR;

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function tick() { return new Promise(function (res) { setTimeout(res, 0); }); }

  T.test("time-ago: renders sigil's format and the title", async function (fx) {
    var el = await mount(fx, ago(iso(5 * MIN)));
    T.eq(el.textContent, "5m ago");
    T.ok(el.getAttribute("title").length > 0, "title");
    el = await mount(fx, ago(iso(3 * HOUR), ' data-ah-label-hours="vor {n} Std."'));
    T.eq(el.textContent, "vor 3 Std.");
    el = await mount(fx, ago(iso(40 * DAY)));
    T.eq(el.textContent, "1mo ago");
  });

  T.test("time-ago: setDate and refresh from the server", async function (fx) {
    var el = await mount(fx, ago(iso(2 * DAY), ' data-ah-live="false"'));
    T.eq(el.textContent, "2d ago");
    AH.invoke(el, "setDate", Date.now() - 10 * 1000);
    T.eq(el.textContent, "just now");
    AH.invoke(el, "setDate", new Date(Date.now() - 2 * HOUR));
    T.eq(el.textContent, "2h ago");
    T.ok(/Z$/.test(el.getAttribute("datetime")) && !/\.\d{3}Z$/.test(el.getAttribute("datetime")), "iso without ms");
    AH.invoke(el, "setDate", "not a date");
    T.eq(el.textContent, "2h ago");
    el.setAttribute("datetime", iso(7 * MIN));
    AH.invoke(el, "refresh");
    T.eq(el.textContent, "7m ago");
  });

  T.test("time-ago: the timer stops on removal; re-inserted it renders again", async function (fx) {
    var el = await mount(fx, ago(iso(5 * MIN)));
    el.remove();
    await tick();
    el.textContent = "?";
    el.setAttribute("datetime", iso(9 * MIN));
    fx.appendChild(el);
    await T.ready(fx);
    T.eq(el.textContent, "9m ago", "setup ran again");
  });
})(window.AHTest, window.AH);
