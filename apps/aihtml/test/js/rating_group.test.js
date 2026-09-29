/* rating behaviour (rating_group.js), the pilot of the native Stimulus
 * controllers (designs/06-bundling.md, phase 2): fixtures are reduced
 * copies of aihtml_rating_group's render; events are native (T.fire,
 * T.key), and every test awaits T.ready after inserting its fixture. */
(function (T) {
  "use strict";

  function rating(value, opts) {
    opts = opts || {};
    var stars = "";
    for (var i = 0; i < 5; i++) {
      stars += '<button class="ah-rating__star" type="button" data-index="' + i + '" role="radio"' +
        ' aria-checked="false" style="width:20px;display:inline-block">' +
        '<span class="ah-rating__empty"></span><span class="ah-rating__filled" style="width:0%;"></span></button>';
    }
    return '<div class="ah-rating" data-ah="rating" id="r" data-ah-value="' + value + '" data-ah-max="5"' +
      ' data-precision="' + (opts.half ? "0.5" : "1") + '" data-readonly="' + !!opts.readonly + '"' +
      ' data-disabled="false" data-allow-clear="' + (opts.clear === false ? "false" : "true") + '">' +
      stars + '<input type="hidden" name="r" value="' + value + '"></div>';
  }

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }

  function changes(el) {
    var seen = [];
    el.addEventListener("change", function (e) {
      if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); }
    });
    return seen;
  }

  function star(el, i) { return el.querySelectorAll(".ah-rating__star")[i]; }
  function widths(el) {
    return Array.prototype.map.call(el.querySelectorAll(".ah-rating__filled"),
                                    function (f) { return f.style.width; });
  }

  T.test("rating: click sets the value, fires change and fills the stars", async function (fx) {
    var el = await mount(fx, rating(2)), seen = changes(el);
    T.fire(star(el, 3), "click", { detail: 1 });
    T.eq(el.getAttribute("data-ah-value"), "4");
    T.eq(el.querySelector("input[type=hidden]").value, "4");
    T.eq(widths(el), ["100%", "100%", "100%", "100%", "0%"]);
    T.eq(star(el, 3).getAttribute("aria-checked"), "true");
    T.eq(star(el, 4).getAttribute("aria-checked"), "false");
    // clicking the current value clears it (allow_clear)
    T.fire(star(el, 3), "click", { detail: 1 });
    T.eq(el.getAttribute("data-ah-value"), "0");
    T.eq(seen, ["4", "0"]);
  });

  T.test("rating: arrow keys step the value; without clear a click keeps it", async function (fx) {
    var el = await mount(fx, rating(3, { clear: false })), seen = changes(el);
    T.key(star(el, 0), "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "4");
    T.key(star(el, 0), "ArrowDown");
    T.key(star(el, 0), "ArrowLeft");
    T.eq(el.getAttribute("data-ah-value"), "2");
    T.fire(star(el, 1), "click", { detail: 1 });
    T.eq(el.getAttribute("data-ah-value"), "2");
    T.eq(seen, ["4", "3", "2"]);
  });

  T.test("rating: half stars follow the pointer; hover previews and resets", async function (fx) {
    var el = await mount(fx, rating(1, { half: true }));
    var hovers = [];
    el.addEventListener("ah:hover", function (e) { hovers.push(e.detail); });
    var r = star(el, 2).getBoundingClientRect();
    T.fire(star(el, 2), "mousemove", { clientX: r.left + 1, clientY: r.top + 1, detail: 1 });
    T.eq(widths(el).slice(0, 3), ["100%", "100%", "50%"]);
    T.fire(el, "mouseleave", { bubbles: false });
    T.eq(widths(el).slice(0, 3), ["100%", "0%", "0%"]);
    T.eq(hovers, [2.5, null]);
    T.fire(star(el, 2), "click", { clientX: r.right - 1, clientY: r.top + 1, detail: 1 });
    T.eq(el.getAttribute("data-ah-value"), "3");
  });

  T.test("rating: methods from the server; readonly ignores input", async function (fx) {
    var el = await mount(fx, rating(0, { readonly: true })), seen = changes(el);
    AH.invoke(el, "setValue", 4.4);
    T.eq(AH.invoke(el, "getValue"), 4);
    T.eq(el.querySelector("input[type=hidden]").value, "4");
    T.fire(star(el, 0), "click", { detail: 1 });
    T.key(star(el, 0), "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "4");
    T.eq(seen, [], "methods and a readonly rating fire no change");
  });

  T.test("rating: a change reaches on(change, ...) bindings (native event)", async function (fx) {
    var el = await mount(fx, rating(1));
    var got = null;
    // what the runtime's delegated data-ah-on listener sees
    document.addEventListener("change", function h(e) {
      if (e.target === el) { got = el.getAttribute("data-ah-value"); document.removeEventListener("change", h); }
    });
    T.fire(star(el, 4), "click", { detail: 1 });
    T.eq(got, "5");
  });
})(window.AHTest);
