/* Notifications (components/notification.js, cards from
 * _lib_overlay.js): AH.notify, notification templates. */
(function (T, $, AH) {
  "use strict";

  function html(fx, s) { fx.innerHTML = s; AH.mount(fx); }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function cleanup() { $(".ah-notify-container").remove(); $("body").removeClass("ah-scroll-locked"); }


  T.test("notify places a server-rendered card as is", async function () {
    cleanup();
    AH.apply([{ op: "call", method: "notify",
                args: [{ card: '<div class="ah-notify ah-notify-error" role="alert"><div class="ah-notify-content"><em>srv</em></div></div>',
                         position: "top-left", duration: 0 }] }]);
    var $c = $(".ah-notify-top-left .ah-notify-error");
    T.eq($c.find("em").text(), "srv");
    T.ok(!$c.hasClass("ah-notify-clickable"));
    $c.trigger("click");                      // not clickable: stays
    await wait(400);
    T.ok(document.body.contains($c[0]));
    cleanup();
  });

  T.test("notification template clones its server-rendered card", async function (fx) {
    cleanup();
    html(fx, '<button id="nb" data-ah-open="#nt">n</button>' +
         '<div id="nt" class="ah-notify-tpl" data-ah="notification" hidden data-ah-position="top-right" data-ah-duration="0">' +
         AH.tpl.notification({ variant: "warning", warning: true, clickable: true, closable: true,
                               content: "<strong>hi</strong>" }) + '</div>');
    var events = [];
    $("#nt").on("ah:open ah:close ah:click", function (e) { events.push(e.type); });
    $("#nb").trigger("click");
    $("#nb").trigger("click");
    await wait(400);
    T.eq($(".ah-notify-container .ah-notify-warning strong").length, 2);
    T.eq($("#nt .ah-notify").length, 1, "template keeps its card");
    $(".ah-notify-container .ah-notify-close").first().trigger("click");
    await wait(400);
    T.eq($(".ah-notify-container .ah-notify").length, 1);
    AH.invoke($("#nt"), "closeAll");
    await wait(400);
    T.eq($(".ah-notify-container").length, 0);
    T.eq(events.filter(function (e) { return e === "ah:open"; }).length, 2);
    T.eq(events.filter(function (e) { return e === "ah:close"; }).length, 2);
  });
})(window.AHTest, window.jQuery, window.AH);
