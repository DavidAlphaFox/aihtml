/* Sheet (components/sheet.js) driven by the declarative triggers of
 * _lib_overlay.js. */
(function (T, $, AH) {
  "use strict";

  function html(fx, s) { fx.innerHTML = s; AH.mount(fx); }


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
})(window.AHTest, window.jQuery, window.AH);
