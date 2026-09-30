/* timepicker behaviour (timepicker.js). The markup is a reduced copy of
 * aihtml_timepicker's render; the header and clock numbers are drawn by
 * the shared templates. Events are native (T.fire, T.key); every test
 * awaits T.ready after inserting its fixture. */
(function (T, AH) {
  "use strict";

  function clockBody() {
    return '<div class="ah-timepicker"><div class="ah-timepicker-header"></div>' +
      '<div class="ah-timepicker-body"><div class="ah-timepicker-clock-wrap" style="width:260px;height:260px">' +
      '<svg class="ah-timepicker-svg" viewBox="0 0 260 260" tabindex="0" role="slider">' +
      '<circle class="ah-timepicker-face" cx="130" cy="130" r="120"></circle>' +
      '<line class="ah-timepicker-hand" x1="130" y1="130" x2="130" y2="25"></line>' +
      '<circle class="ah-timepicker-selection" cx="130" cy="25" r="18"></circle>' +
      '<g class="ah-timepicker-numbers"></g></svg></div></div></div>';
  }

  function clock(value, format, extra) {
    return '<div id="tp" class="ah-timepicker-field ah-timepicker-field-inline" data-ah="timepicker"' +
      ' data-ah-value="' + value + '" data-format="' + format + '" data-step="5"' + (extra || "") + '>' +
      clockBody() + '<input type="hidden" name="t" value="' + value + '"></div>';
  }

  function field(value) {
    return '<div id="tpf" class="ah-timepicker-field" data-ah="timepicker" data-ah-value="' + value + '"' +
      ' data-format="24h" data-step="5"><div class="ah-timepicker-input-area">' +
      '<input class="ah-timepicker-input" type="text" value="" aria-expanded="false">' +
      '<button type="button" class="ah-timepicker-clear">x</button></div>' +
      '<div class="ah-timepicker-popup" hidden>' + clockBody() + '</div>' +
      '<input type="hidden" name="t" value="' + value + '"></div>';
  }

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }

  function changes(el) {
    var got = [];
    el.addEventListener("change", function (e) { if (e.target === el) { got.push(el.getAttribute("data-ah-value")); } });
    return got;
  }

  // in a Chinese page the field shows 上午 / 下午 first and reads them back
  T.test("timepicker reads and writes the page language's AM / PM", async function (fx) {
    var cat = document.createElement("script");
    cat.type = "application/json"; cat.id = "ah-labels";
    cat.textContent = JSON.stringify({ format: { am: "上午", pm: "下午", time_12h: "{ampm}{time}" } });
    document.head.appendChild(cat);
    AH.reloadTexts();
    try {
      var el = await mount(fx, field("09:30").replace('data-format="24h"', 'data-format="12h"'));
      var input = el.querySelector(".ah-timepicker-input");
      input.value = "下午7:40";
      T.fire(input, "change");
      T.eq(el.getAttribute("data-ah-value"), "19:40");
      T.eq(input.value, "下午7:40");
      input.value = "8:05 上午";
      T.fire(input, "change");
      T.eq(el.getAttribute("data-ah-value"), "08:05");
      T.eq(input.value, "上午8:05");
    } finally {
      cat.remove();
      AH.reloadTexts();
    }
  });

  T.test("timepicker draws header and numbers from the shared templates", async function (fx) {
    var el = await mount(fx, clock("21:05", "24h"));
    T.eq(el.querySelector(".ah-timepicker-header").innerHTML,
         AH.tpl.timepicker_header({ hours: "21", minutes: "05", hours_active: true,
                                    minutes_active: false, twelve: false, am: false, pm: true,
                                    disabled: false, tabindex: 0,
                                    txt_hours: "Hours", txt_minutes: "Minutes", txt_am: "AM", txt_pm: "PM",
                                    period_first: false }));
    var texts = el.querySelectorAll(".ah-timepicker-numbers text");
    T.eq(texts.length, 24);
    T.eq(texts[0].namespaceURI, "http://www.w3.org/2000/svg");
    T.eq(el.querySelector(".ah-timepicker-number-selected").textContent, "21");
    T.eq(el.querySelector(".ah-timepicker-hand").getAttribute("x2"), "60");
  });

  T.test("timepicker minutes mode and keyboard commit a change", async function (fx) {
    var el = await mount(fx, clock("14:30", "12h"));
    var got = changes(el), details = [];
    el.addEventListener("change", function (e) { if (e.target === el) { details.push(e.detail); } });
    el.querySelector("[data-action=select-minutes]").click();
    T.eq(el.querySelectorAll(".ah-timepicker-numbers text").length, 12);
    T.eq(el.querySelector(".ah-timepicker-number-selected").textContent, "30");
    T.key(el.querySelector(".ah-timepicker-svg"), "ArrowUp");
    T.eq(got, ["14:35"]);
    T.eq(el.querySelector("input[type=hidden]").value, "14:35");
    T.key(el.querySelector("[data-action=set-am]"), "Enter");
    T.eq(got, ["14:35", "02:35"]);
    T.eq(el.querySelectorAll(".ah-timepicker-header-am-active").length, 1);
    T.eq(details, [{ value: "14:35" }, { value: "02:35" }], "change detail");
  });

  T.test("timepicker marks hours outside min/max disabled", async function (fx) {
    var el = await mount(fx, clock("10:00", "24h", ' data-min="09:00" data-max="17:30"'));
    var off = Array.prototype.map.call(el.querySelectorAll(".ah-timepicker-number-disabled"),
                                       function (n) { return n.textContent; });
    T.eq(off, ["1", "2", "3", "4", "5", "6", "7", "8", "00", "18", "19", "20", "21", "22", "23"]);
  });

  T.test("timepicker: field popup, typing, clear, methods, outside press", async function (fx) {
    var el = await mount(fx, field("09:15")), got = changes(el);
    var input = el.querySelector(".ah-timepicker-input"), pop = el.querySelector(".ah-timepicker-popup");
    var bubbled = 0;
    document.addEventListener("change", function h(e) {
      if (e.target === input) { bubbled++; }
      if (e.target === el) { document.removeEventListener("change", h); }
    });
    T.eq(input.getAttribute("aria-controls"), pop.id);
    T.fire(el.querySelector(".ah-timepicker-input-area"), "mousedown");
    T.eq(pop.hidden, false, "opens");
    T.ok(el.classList.contains("ah-timepicker-open"));
    T.eq(input.getAttribute("aria-expanded"), "true");
    T.eq(el.querySelector(".ah-timepicker-number-selected").textContent, "9");
    T.fire(document.body, "mousedown");
    T.eq(pop.hidden, true, "a press outside closes");
    input.value = "7:40 pm";
    T.fire(input, "change");
    T.eq(el.getAttribute("data-ah-value"), "19:40");
    T.eq(input.value, "19:40");
    T.eq(bubbled, 0, "the inner field's change stays inside");
    T.key(input, "ArrowDown");
    T.eq(pop.hidden, false);
    T.key(input, "Escape");
    T.eq(pop.hidden, true, "escape closes");
    el.querySelector(".ah-timepicker-clear").click();
    T.eq([el.getAttribute("data-ah-value"), input.value], ["", ""]);
    T.eq(el.querySelector(".ah-timepicker-clear").hidden, true);
    AH.invoke(el, "setValue", "08:05");
    T.eq(AH.invoke(el, "getValue"), "08:05");
    T.eq(got, ["19:40", ""], "setValue fires nothing");
    AH.invoke(el, "open");
    T.eq(pop.hidden, false);
    AH.invoke(el, "setMode", "minutes");
    T.eq(el.querySelector(".ah-timepicker-number-selected").textContent, "05");
    AH.invoke(el, "close");
    T.eq(pop.hidden, true);
  });

  T.test("timepicker: removed and re-inserted, it still works; teardown stops the popup", async function (fx) {
    var el = await mount(fx, field("10:00"));
    AH.invoke(el, "open");
    var pop = el.querySelector(".ah-timepicker-popup");
    T.eq(getComputedStyle(pop).position, "fixed");
    fx.removeChild(el);
    await new Promise(function (r) { setTimeout(r, 0); });
    fx.appendChild(el);
    await T.ready(fx);
    var got = changes(el);
    var input = el.querySelector(".ah-timepicker-input");
    input.value = "11:30";
    T.fire(input, "change");
    T.eq(got, ["11:30"]);
    fx.innerHTML = "";
    await new Promise(function (r) { setTimeout(r, 0); });
    T.fire(document.body, "mousedown");      // no listener left behind throws
    T.ok(true);
  });
})(window.AHTest, window.AH);
