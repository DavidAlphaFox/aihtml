/* colorpicker behaviour (colorpicker.js). The markup is a reduced copy
 * of aihtml_colorpicker's render. Events are native (T.fire, T.key);
 * every test awaits T.ready after inserting its fixture. */
(function (T, AH) {
  "use strict";

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }

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

  function popupPicker() {
    return '<div style="overflow:hidden;height:50px;width:400px">' +
      '<div id="cpf" class="ah-colorpicker-field" data-ah="colorpicker" data-ah-value="#3b82f6">' +
      '<button type="button" class="ah-colorpicker-trigger" aria-expanded="false" data-placeholder="None">' +
      '<span class="ah-colorpicker-trigger-swatch"></span><span class="ah-colorpicker-trigger-text">#3b82f6</span></button>' +
      '<div class="ah-colorpicker-popup" hidden><div class="ah-colorpicker" style="width:240px">' +
      '<div class="ah-colorpicker-body"><div class="ah-colorpicker-map" tabindex="0" style="width:200px;height:160px">' +
      '<div class="ah-colorpicker-map-pointer"></div></div>' +
      '<div class="ah-colorpicker-bar" tabindex="0" style="height:160px"><div class="ah-colorpicker-bar-pointer"></div></div>' +
      '</div><div class="ah-colorpicker-transparent"><a href="#">none</a></div></div></div>' +
      '<input type="hidden" value="#3b82f6"></div></div>';
  }

  function record(el, types) {
    var got = [];
    types.forEach(function (t) {
      el.addEventListener(t, function (e) { if (e.target === el) { got.push(e.type + " " + el.getAttribute("data-ah-value")); } });
    });
    return got;
  }

  T.test("colorpicker hex typing fires input then change; swatches commit", async function (fx) {
    var el = await mount(fx, picker("#3b82f6"));
    var got = record(el, ["input", "change"]), details = [];
    el.addEventListener("change", function (e) { if (e.target === el) { details.push(e.detail); } });
    var hex = el.querySelector(".ah-colorpicker-hex-input");
    T.eq(hex.value, "3b82f6");
    hex.value = "10b981";
    T.fire(hex, "input");
    T.fire(hex, "change");
    el.querySelector(".ah-colorpicker-swatch").click();
    T.eq(got, ["input #10b981", "change #10b981", "input #ef4444", "change #ef4444"]);
    T.eq(details, [{ value: "#10b981" }, { value: "#ef4444" }], "change detail");
    T.eq(el.querySelector("input[type=hidden]").value, "#ef4444");
    T.eq(el.querySelector(".ah-colorpicker-swatch").getAttribute("aria-pressed"), "true");
  });

  T.test("colorpicker map drag and hue keys", async function (fx) {
    var el = await mount(fx, picker("#ff0000"));
    var map = el.querySelector(".ah-colorpicker-map");
    var r = map.getBoundingClientRect();
    var types = [];
    ["input", "change"].forEach(function (t) {
      el.addEventListener(t, function (e) { if (e.target === el) { types.push(e.type); } });
    });
    ["pointerdown", "pointermove", "pointerup"].forEach(function (t, i) {
      T.fire(map, t, { button: 0, pointerId: 1, clientX: r.left + (i ? r.width / 2 : 1), clientY: r.top + 1 });
    });
    T.eq(types, ["input", "input", "change"]);
    T.eq(el.getAttribute("data-ah-value"), "#fc7e7e");
    T.key(el.querySelector(".ah-colorpicker-bar"), "End");
    T.eq(el.querySelector(".ah-colorpicker-bar").getAttribute("aria-valuenow"), "359");
  });

  T.test("popups float out of an overflow:hidden card and stop on close", async function (fx) {
    fx.innerHTML = popupPicker();
    await T.ready(fx);
    var el = document.getElementById("cpf");
    var pop = el.querySelector(".ah-colorpicker-popup");
    el.querySelector(".ah-colorpicker-trigger").click();
    T.eq(pop.hidden, false);
    T.eq(getComputedStyle(pop).position, "fixed");
    var map = el.querySelector(".ah-colorpicker-map");
    var r = map.getBoundingClientRect();
    T.ok(map.contains(document.elementFromPoint(r.left + 5, r.bottom - 5)), "map not clipped");
    T.fire(map, "pointerdown", { button: 0, pointerId: 1, clientX: r.left + 1, clientY: r.bottom - 1 });
    T.fire(map, "pointerup", { pointerId: 1, clientX: r.left + 1, clientY: r.bottom - 1 });
    T.eq(el.getAttribute("data-ah-value"), "#030303");
    T.key(el, "Escape");
    T.eq(pop.hidden, true);
    T.eq(pop.style.position, "");
  });

  T.test("colorpicker: trigger text, clear link, methods, outside press", async function (fx) {
    fx.innerHTML = popupPicker();
    await T.ready(fx);
    var el = document.getElementById("cpf"), got = record(el, ["change"]);
    var trigger = el.querySelector(".ah-colorpicker-trigger"), pop = el.querySelector(".ah-colorpicker-popup");
    T.eq(trigger.getAttribute("aria-controls"), pop.id);
    T.key(trigger, "ArrowDown");
    T.eq([pop.hidden, trigger.getAttribute("aria-expanded")], [false, "true"]);
    T.eq(document.activeElement, el.querySelector(".ah-colorpicker-map"), "focus on the area");
    T.fire(document.body, "mousedown");
    T.eq(pop.hidden, true, "a press outside closes");
    AH.invoke(el, "setValue", "#00ff00");
    T.eq(AH.invoke(el, "getValue"), "#00ff00");
    T.eq(el.querySelector(".ah-colorpicker-trigger-text").textContent, "#00ff00");
    T.eq(el.querySelector(".ah-colorpicker-trigger-swatch").style.getPropertyValue("--ah-cp-swatch"), "rgba(0,255,0,1)");
    AH.invoke(el, "open");
    el.querySelector(".ah-colorpicker-transparent a").click();
    T.eq([el.getAttribute("data-ah-value"), pop.hidden], ["", true]);
    T.eq(el.querySelector(".ah-colorpicker-trigger-text").textContent, "None");
    T.ok(el.querySelector(".ah-colorpicker-trigger-swatch").classList.contains("ah-colorpicker-trigger-empty"));
    T.eq(got, ["change "], "setValue fires nothing, clearing commits");
  });

  T.test("colorpicker: removed and re-inserted, it still works", async function (fx) {
    var el = await mount(fx, picker("#3b82f6"));
    fx.removeChild(el);
    await new Promise(function (r) { setTimeout(r, 0); });
    fx.appendChild(el);
    await T.ready(fx);
    var got = record(el, ["change"]);
    el.querySelector(".ah-colorpicker-swatch").click();
    T.eq(got, ["change #ef4444"], "one change: no listener bound twice");
  });
})(window.AHTest, window.AH);
