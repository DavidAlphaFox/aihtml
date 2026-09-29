/* Overlay components (components/overlay.js): shared card templates,
 * toast/notify, notification templates, declarative triggers. */
(function (T, $, AH) {
  "use strict";

  function html(fx, s) { fx.innerHTML = s; AH.mount(fx); }
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

  T.test("opens / closes triggers drive a sheet, Escape and focus return", function (fx) {
    html(fx, '<button id="ob" data-ah-open="#sh">o</button>' +
         '<div id="sh" class="ah-sheet__overlay" data-ah="sheet" data-state="closed">' +
         '<div class="ah-sheet__panel" data-side="right" data-state="closed" tabindex="-1">' +
         '<button id="cb" data-ah-close="">x</button></div></div>');
    var ob = document.getElementById("ob");
    ob.focus();
    $(ob).trigger("click");
    T.eq($("#sh").attr("data-state"), "open");
    T.ok($("body").hasClass("ah-scroll-locked"));
    $(document.activeElement).trigger($.Event("keydown", { key: "Escape" }));
    T.eq($("#sh").attr("data-state"), "closed");
    T.eq(document.activeElement, ob);
    $(ob).trigger("click");
    $("#cb").trigger("click");
    T.eq($("#sh").attr("data-state"), "closed");
    T.ok(!$("body").hasClass("ah-scroll-locked"));
  });

  T.test("popover and tooltip escape an overflow:hidden box via AH.float, arrow follows the flip", async function (fx) {
    html(fx, '<div style="overflow:hidden;height:40px;width:200px;position:relative;margin-top:0">' +
         '<button id="pb" data-ah-toggle="#pp" style="margin-top:4px">p</button>' +
         '<div id="pp" class="ah-popover ah-popover-top" data-ah="popover" data-state="closed" data-ah-position="top">' +
         '<div class="ah-popover-arrow"></div><div class="ah-popover-content">content that is tall<br>2<br>3</div></div>' +
         '<span class="ah-tooltip-host" data-ah="tooltip" data-ah-tip-position="bottom" data-ah-tip-delay="0">' +
         '<button id="tb">t</button><span class="ah-tooltip ah-tooltip-bottom" role="tooltip">' +
         '<span class="ah-tooltip-arrow"></span><span class="ah-tooltip-content">tip</span></span></span></div>');
    window.scrollTo(0, 0);
    var pp = document.getElementById("pp"), pb = document.getElementById("pb");
    $(pb).trigger("click");
    T.eq(pp.style.position, "fixed");
    // no room above the button at the top of the page: flipped below
    T.eq(pp.getAttribute("data-ah-placement"), "bottom");
    T.ok($(pp).hasClass("ah-popover-bottom") && !$(pp).hasClass("ah-popover-top"), "arrow class follows");
    var r = pp.getBoundingClientRect();
    T.ok(r.top >= pb.getBoundingClientRect().bottom && r.height > 40, "visible outside the clipped box");
    T.eq(document.elementFromPoint(r.left + 5, r.bottom - 5).closest("#pp"), pp);
    $(pb).trigger("click");
    await wait(400);
    T.eq(pp.style.position, "", "float stopped after close");
    var tb = document.getElementById("tb");
    tb.focus();
    var tip = $(".ah-tooltip-host .ah-tooltip")[0];
    T.eq(tip.style.position, "fixed");
    T.eq(tip.getAttribute("data-ah-placement"), "bottom");
    T.ok(tip.getBoundingClientRect().top >= tb.getBoundingClientRect().bottom, "tooltip below its trigger");
    tb.blur();
    await wait(400);
    T.eq(tip.style.display, "none");
    T.ok(!tip.hasAttribute("data-ah-placement"), "tooltip float stopped");
  });
})(window.AHTest, window.jQuery, window.AH);
