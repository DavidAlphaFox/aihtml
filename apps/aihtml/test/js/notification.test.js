/* Notifications (components/notification.js, cards from
 * _lib_overlay.js): AH.notify, notification templates. Native events,
 * T.ready after every fixture. */
(function (T, AH) {
  "use strict";

  async function html(fx, s) { fx.innerHTML = s; await T.ready(fx); }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function cleanup() {
    document.querySelectorAll(".ah-notify-container").forEach(function (c) { c.remove(); });
    document.body.classList.remove("ah-scroll-locked");
  }
  function $$(sel) { return Array.from(document.querySelectorAll(sel)); }
  var TPL = '<button id="nb" data-ah-open="#nt">n</button>' +
    '<div id="nt" class="ah-notify-tpl" data-ah="notification" hidden data-ah-position="top-right" data-ah-duration="0">';

  T.test("notification: notify places a server-rendered card as is", async function () {
    cleanup();
    AH.apply([{ op: "call", method: "notify",
                args: [{ card: '<div class="ah-notify ah-notify-error" role="alert"><div class="ah-notify-content"><em>srv</em></div></div>',
                         position: "top-left", duration: 0 }] }]);
    await wait(0);
    var c = document.querySelector(".ah-notify-top-left .ah-notify-error");
    T.eq(c.querySelector("em").textContent, "srv");
    T.ok(!c.classList.contains("ah-notify-clickable"));
    T.fire(c, "click");                      // not clickable: stays
    await wait(400);
    T.ok(document.body.contains(c));
    cleanup();
  });

  T.test("notification: notify builds an escaped card from text", async function () {
    cleanup();
    var card = AH.notify({ text: "<b>x</b>", variant: "success", duration: 0 });
    T.ok(card.classList.contains("ah-notify-success"));
    T.eq(card.querySelector("b"), null);
    T.ok(card.textContent.indexOf("<b>x</b>") >= 0);
    cleanup();
  });

  T.test("notification: template clones its server-rendered card", async function (fx) {
    cleanup();
    await html(fx, TPL + AH.tpl.notification({ variant: "warning", warning: true, clickable: true, closable: true,
                                                content: "<strong>hi</strong>" }) + '</div>');
    var nt = document.getElementById("nt"), nb = document.getElementById("nb");
    var events = [];
    ["ah:open", "ah:close", "ah:click"].forEach(function (t) {
      nt.addEventListener(t, function (e) { events.push(e.type); });
    });
    nb.click();
    nb.click();
    await wait(400);
    T.eq($$(".ah-notify-container .ah-notify-warning strong").length, 2);
    T.eq(nt.querySelectorAll(".ah-notify").length, 1, "template keeps its card");
    document.querySelector(".ah-notify-container .ah-notify-close").click();
    await wait(400);
    T.eq($$(".ah-notify-container .ah-notify").length, 1);
    AH.invoke(nt, "closeAll");
    await wait(400);
    T.eq($$(".ah-notify-container").length, 0);
    T.eq(events.filter(function (e) { return e === "ah:open"; }).length, 2);
    T.eq(events.filter(function (e) { return e === "ah:close"; }).length, 2);
  });

  T.test("notification: a click on a clickable card fires ah:click; closeLast; cleanup", async function (fx) {
    cleanup();
    var card = AH.tpl.notification({ variant: "info", info: true, clickable: true, closable: true, content: "c" });
    await html(fx, TPL + card + '</div>');
    var nt = document.getElementById("nt"), clicks = 0;
    nt.addEventListener("ah:click", function () { clicks++; });
    AH.invoke(nt, "open");
    AH.invoke(nt, "open");
    await wait(400);
    document.querySelector(".ah-notify-container .ah-notify").click();
    T.eq(clicks, 1);
    await wait(400);
    T.eq($$(".ah-notify-container .ah-notify").length, 1);
    AH.invoke(nt, "open");
    AH.invoke(nt, "closeLast");
    await wait(400);
    T.eq($$(".ah-notify-container .ah-notify").length, 1);
    // removing the template removes its cards; re-inserting works
    nt.remove();
    await wait(0);
    T.eq($$(".ah-notify-container").length, 0, "cards removed with the template");
    fx.appendChild(nt);
    await T.ready(fx);
    AH.invoke(nt, "open");
    T.eq($$(".ah-notify-container .ah-notify").length, 1);
    AH.invoke(nt, "close");
    await wait(400);
    T.eq($$(".ah-notify-container").length, 0);
  });
})(window.AHTest, window.AH);
