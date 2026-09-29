/* timepicker behaviour (timepicker.js). The markup is a reduced copy of
 * aihtml_timepicker's render; the header and clock numbers are drawn by
 * the shared templates. */
(function (T, $, AH) {
  "use strict";

  function clock(value, format, extra) {
    return '<div id="tp" class="ah-timepicker-field ah-timepicker-field-inline" data-ah="timepicker"' +
      ' data-ah-value="' + value + '" data-format="' + format + '" data-step="5"' + (extra || "") + '>' +
      '<div class="ah-timepicker"><div class="ah-timepicker-header"></div>' +
      '<div class="ah-timepicker-body"><div class="ah-timepicker-clock-wrap" style="width:260px;height:260px">' +
      '<svg class="ah-timepicker-svg" viewBox="0 0 260 260" tabindex="0" role="slider">' +
      '<circle class="ah-timepicker-face" cx="130" cy="130" r="120"></circle>' +
      '<line class="ah-timepicker-hand" x1="130" y1="130" x2="130" y2="25"></line>' +
      '<circle class="ah-timepicker-selection" cx="130" cy="25" r="18"></circle>' +
      '<g class="ah-timepicker-numbers"></g></svg></div></div></div>' +
      '<input type="hidden" name="t" value="' + value + '"></div>';
  }

  function mount(fx, html) {
    fx.innerHTML = html;
    AH.mount(fx);
    return fx.firstChild;
  }

  function key(node, k) {
    node.dispatchEvent(new KeyboardEvent("keydown", { key: k, bubbles: true, cancelable: true }));
  }

  T.test("timepicker draws header and numbers from the shared templates", function (fx) {
    var el = mount(fx, clock("21:05", "24h"));
    T.eq($(el).find(".ah-timepicker-header").html(),
         AH.tpl.timepicker_header({ hours: "21", minutes: "05", hours_active: true,
                                    minutes_active: false, twelve: false, am: false, pm: true,
                                    disabled: false, tabindex: 0 }));
    var texts = el.querySelectorAll(".ah-timepicker-numbers text");
    T.eq(texts.length, 24);
    T.eq(texts[0].namespaceURI, "http://www.w3.org/2000/svg");
    T.eq($(el).find(".ah-timepicker-number-selected").text(), "21");
    T.eq($(el).find(".ah-timepicker-hand").attr("x2"), "60");
  });

  T.test("timepicker minutes mode and keyboard commit a change", function (fx) {
    var el = mount(fx, clock("14:30", "12h"));
    var got = [];
    $(el).on("change", function (e) { if (e.target === el) { got.push(el.getAttribute("data-ah-value")); } });
    $(el).find("[data-action=select-minutes]").trigger("click");
    T.eq(el.querySelectorAll(".ah-timepicker-numbers text").length, 12);
    T.eq($(el).find(".ah-timepicker-number-selected").text(), "30");
    key(el.querySelector(".ah-timepicker-svg"), "ArrowUp");
    T.eq(got, ["14:35"]);
    T.eq($(el).find("input[type=hidden]").val(), "14:35");
    key(el.querySelector("[data-action=set-am]"), "Enter");
    T.eq(got, ["14:35", "02:35"]);
    T.eq($(el).find(".ah-timepicker-header-am-active").length, 1);
  });

  T.test("timepicker marks hours outside min/max disabled", function (fx) {
    var el = mount(fx, clock("10:00", "24h", ' data-min="09:00" data-max="17:30"'));
    var off = $(el).find(".ah-timepicker-number-disabled").map(function () { return this.textContent; }).get();
    T.eq(off, ["1", "2", "3", "4", "5", "6", "7", "8", "00", "18", "19", "20", "21", "22", "23"]);
  });
})(window.AHTest, window.jQuery, window.AH);
