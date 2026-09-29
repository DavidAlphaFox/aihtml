/* timepicker / colorpicker behaviours (form_time_color.js). The markup is a
 * reduced copy of aihtml_form_time_color's render; the header and clock
 * numbers are drawn by the shared templates. */
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

  function picker(value) {
    return '<div id="cp" class="ah-colorpicker-field ah-colorpicker-field-inline" data-ah="colorpicker"' +
      ' data-ah-value="' + value + '"><div class="ah-colorpicker"><div class="ah-colorpicker-body">' +
      '<div class="ah-colorpicker-map" tabindex="0" style="width:200px;height:160px">' +
      '<div class="ah-colorpicker-map-pointer"></div></div>' +
      '<div class="ah-colorpicker-bar" tabindex="0"><div class="ah-colorpicker-bar-pointer"></div></div></div>' +
      '<input class="ah-colorpicker-field ah-colorpicker-hex-input" type="text">' +
      '<button type="button" class="ah-colorpicker-swatch" data-color="#ef4444"></button>' +
      '</div><input type="hidden" name="c" value="' + value + '"></div>';
  }

  T.test("colorpicker hex typing fires input then change; swatches commit", function (fx) {
    var el = mount(fx, picker("#3b82f6"));
    var got = [];
    $(el).on("input change", function (e) { if (e.target === el) { got.push(e.type + " " + el.getAttribute("data-ah-value")); } });
    var hex = el.querySelector(".ah-colorpicker-hex-input");
    T.eq(hex.value, "3b82f6");
    hex.value = "10b981";
    hex.dispatchEvent(new Event("input", { bubbles: true }));
    hex.dispatchEvent(new Event("change", { bubbles: true }));
    el.querySelector(".ah-colorpicker-swatch").click();
    T.eq(got, ["input #10b981", "change #10b981", "input #ef4444", "change #ef4444"]);
    T.eq($(el).find("input[type=hidden]").val(), "#ef4444");
    T.eq(el.querySelector(".ah-colorpicker-swatch").getAttribute("aria-pressed"), "true");
  });

  T.test("colorpicker map drag and hue keys", function (fx) {
    var el = mount(fx, picker("#ff0000"));
    var map = el.querySelector(".ah-colorpicker-map");
    var r = map.getBoundingClientRect();
    var types = [];
    $(el).on("input change", function (e) { if (e.target === el) { types.push(e.type); } });
    ["pointerdown", "pointermove", "pointerup"].forEach(function (t, i) {
      map.dispatchEvent(new PointerEvent(t, { bubbles: true, cancelable: true, button: 0, pointerId: 1,
                                              clientX: r.left + (i ? r.width / 2 : 1), clientY: r.top + 1 }));
    });
    T.eq(types, ["input", "input", "change"]);
    T.eq(el.getAttribute("data-ah-value"), "#fc7e7e");
    key(el.querySelector(".ah-colorpicker-bar"), "End");
    T.eq(el.querySelector(".ah-colorpicker-bar").getAttribute("aria-valuenow"), "359");
  });
  T.test("popups float out of an overflow:hidden card and stop on close", function (fx) {
    fx.innerHTML = '<div style="overflow:hidden;height:50px;width:400px">' +
      '<div id="cpf" class="ah-colorpicker-field" data-ah="colorpicker" data-ah-value="#3b82f6">' +
      '<button type="button" class="ah-colorpicker-trigger" aria-expanded="false">x</button>' +
      '<div class="ah-colorpicker-popup" hidden><div class="ah-colorpicker" style="width:240px">' +
      '<div class="ah-colorpicker-body"><div class="ah-colorpicker-map" tabindex="0" style="width:200px;height:160px">' +
      '<div class="ah-colorpicker-map-pointer"></div></div>' +
      '<div class="ah-colorpicker-bar" tabindex="0" style="height:160px"><div class="ah-colorpicker-bar-pointer"></div></div>' +
      '</div></div></div><input type="hidden" value="#3b82f6"></div></div>';
    AH.mount(fx);
    var el = document.getElementById("cpf");
    var pop = el.querySelector(".ah-colorpicker-popup");
    el.querySelector(".ah-colorpicker-trigger").click();
    T.eq(pop.hidden, false);
    T.eq(getComputedStyle(pop).position, "fixed");
    var map = el.querySelector(".ah-colorpicker-map");
    var r = map.getBoundingClientRect();
    T.ok(map.contains(document.elementFromPoint(r.left + 5, r.bottom - 5)), "map not clipped");
    map.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true, cancelable: true, button: 0,
                                                        pointerId: 1, clientX: r.left + 1, clientY: r.bottom - 1 }));
    map.dispatchEvent(new PointerEvent("pointerup", { bubbles: true, pointerId: 1, clientX: r.left + 1, clientY: r.bottom - 1 }));
    T.eq(el.getAttribute("data-ah-value"), "#030303");
    key(el, "Escape");
    T.eq(pop.hidden, true);
    T.eq(pop.style.position, "");
  });
})(window.AHTest, window.jQuery, window.AH);
