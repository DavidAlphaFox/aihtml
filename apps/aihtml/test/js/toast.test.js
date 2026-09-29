/* Toasts (components/toast.js, cards from _lib_overlay.js): the shared
 * card templates, AH.toast, [data-ah-toast] triggers. */
(function (T, AH) {
  "use strict";

  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function cleanup() {
    document.querySelectorAll(".ah-notify-container").forEach(function (c) { c.remove(); });
    document.body.classList.remove("ah-scroll-locked");
  }
  function parse(html) {
    var t = document.createElement("template");
    t.innerHTML = html;
    return t.content.firstElementChild;
  }

  T.test("toast: renders the shared templates and escapes text", async function () {
    cleanup();
    var card = AH.toast({ title: "<b>T</b>", description: "d", variant: "success", duration: 0 });
    T.ok(card.matches(".ah-notify.ah-notify-success.ah-notify-clickable"), "card classes");
    T.eq(card.querySelectorAll("b").length, 0);
    T.eq(card.querySelector(".ah-toast__title").textContent, "<b>T</b>");
    T.eq(card.querySelector(".ah-toast__description").textContent, "d");
    T.eq(card.outerHTML.replace(/ style="[^"]*"/, ""),
         parse(AH.tpl.notification({ variant: "success", info: false, success: true, warning: false,
                                     error: false, clickable: true, closable: true, width: null,
                                     content: AH.tpl.toast({ has_title: true, title: "<b>T</b>",
                                                             has_description: true, description: "d" }) })).outerHTML);
    T.ok(card.parentNode.matches(".ah-notify-container.ah-notify-top-right"), "in its corner");
    cleanup();
  });

  T.test("toast: auto-dismisses and removes an empty corner", async function () {
    cleanup();
    var card = AH.toast({ title: "x", duration: 50, position: "bottom-left" });
    T.ok(card.parentNode.matches(".ah-notify-bottom-left"));
    await wait(500);
    T.ok(!document.body.contains(card), "card gone");
    T.eq(document.querySelectorAll(".ah-notify-bottom-left").length, 0);
  });

  T.test("toast: a [data-ah-toast] click pops a card; its close button closes it", async function (fx) {
    cleanup();
    fx.innerHTML = '<button id="tt" data-ah-toast="Saved" data-ah-toast-description="ok"' +
      ' data-ah-toast-variant="warning" data-ah-toast-duration="0" data-ah-toast-position="bottom-right">t</button>';
    await T.ready(fx);
    document.getElementById("tt").click();
    var card = document.querySelector(".ah-notify-bottom-right .ah-notify-warning");
    T.ok(card, "card shown");
    T.eq(card.querySelector(".ah-toast__title").textContent, "Saved");
    T.eq(card.querySelector(".ah-toast__description").textContent, "ok");
    card.querySelector(".ah-notify-close").click();
    await wait(400);
    T.ok(!document.body.contains(card));
    T.eq(document.querySelectorAll(".ah-notify-container").length, 0);
  });
})(window.AHTest, window.AH);
