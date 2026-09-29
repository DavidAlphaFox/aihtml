/* Toasts (components/toast.js, cards from _lib_overlay.js): the shared
 * card templates, AH.toast. */
(function (T, $, AH) {
  "use strict";

  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function cleanup() { $(".ah-notify-container").remove(); $("body").removeClass("ah-scroll-locked"); }


  T.test("toast renders the shared templates and escapes text", async function () {
    cleanup();
    var card = AH.toast({ title: "<b>T</b>", description: "d", variant: "success", duration: 0 });
    T.ok($(card).is(".ah-notify.ah-notify-success.ah-notify-clickable"), "card classes");
    T.eq($(card).find("b").length, 0);
    T.eq($(card).find(".ah-toast__title").text(), "<b>T</b>");
    T.eq($(card).find(".ah-toast__description").text(), "d");
    T.eq(card.outerHTML.replace(/ style="[^"]*"/, ""),
         $(AH.tpl.notification({ variant: "success", info: false, success: true, warning: false,
                               error: false, clickable: true, closable: true, width: null,
                               content: AH.tpl.toast({ has_title: true, title: "<b>T</b>",
                                                       has_description: true, description: "d" }) }))[0].outerHTML);
    T.ok($(card).parent().is(".ah-notify-container.ah-notify-top-right"), "in its corner");
    cleanup();
  });

  T.test("toast auto-dismisses and removes an empty corner", async function () {
    cleanup();
    var card = AH.toast({ title: "x", duration: 50, position: "bottom-left" });
    T.ok($(card).parent().is(".ah-notify-bottom-left"));
    await wait(500);
    T.ok(!document.body.contains(card), "card gone");
    T.eq($(".ah-notify-bottom-left").length, 0);
  });
})(window.AHTest, window.jQuery, window.AH);
