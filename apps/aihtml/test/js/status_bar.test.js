/* status_bar: the status-bar behaviour on the markup the server renders.
 * SERVER holds renders of the aihtml_example_demo_status_bar demos, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "st": "<div class=\"ah-status-bar\" role=\"status\" data-ah=\"status-bar\" data-dirty=\"false\"><div class=\"ah-status-bar__side\"><div class=\"ah-status-bar__count\" tabindex=\"0\"><span class=\"ah-status-bar__count-num\">16</span><span> words</span><div class=\"ah-status-bar__popover\" role=\"tooltip\"><div class=\"ah-status-bar__row\"><span class=\"ah-status-bar__row-label\">CJK</span><span class=\"ah-status-bar__row-val\">15</span></div><div class=\"ah-status-bar__row\"><span class=\"ah-status-bar__row-label\">EN words</span><span class=\"ah-status-bar__row-val\">1</span></div><div class=\"ah-status-bar__row\"><span class=\"ah-status-bar__row-label\">Chars</span><span class=\"ah-status-bar__row-val\">26</span></div><div class=\"ah-status-bar__row\"><span class=\"ah-status-bar__row-label\">No spaces</span><span class=\"ah-status-bar__row-val\">23</span></div><div class=\"ah-status-bar__row\"><span class=\"ah-status-bar__row-label\">Lines</span><span class=\"ah-status-bar__row-val\">3</span></div><div class=\"ah-status-bar__row\"><span class=\"ah-status-bar__row-label\">Paragraphs</span><span class=\"ah-status-bar__row-val\">2</span></div></div></div><div class=\"ah-status-bar__extra\">Ln 12, Col 4</div></div><div class=\"ah-status-bar__side ah-status-bar__side--right\"><div class=\"ah-status-bar__extra\">UTF-8</div><div class=\"ah-status-bar__extra\">Markdown</div><div class=\"ah-status-bar__save\"><span class=\"ah-status-bar__dot\" aria-hidden=\"true\"></span><span>Saved</span></div></div></div>"
  };

  async function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    await T.ready(fx);
    return fx.querySelector("[data-ah]");
  }

  // the value (or the event's detail) each time type fires on el itself
  function events(el, type) {
    var got = [];
    el.addEventListener(type, function (e) {
      if (e.target === el) { got.push(e.detail == null ? el.getAttribute("data-ah-value") : e.detail); }
    });
    return got;
  }

  function q(el, sel) { return el.querySelector(sel); }
  function qa(el, sel) { return Array.prototype.slice.call(el.querySelectorAll(sel)); }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms || 0); }); }

  // take el out of the page (its controller tears down) and put it back
  async function reinsert(fx, el) {
    fx.removeChild(el);
    await wait(0);
    fx.appendChild(el);
    await T.ready(fx);
  }

  T.test("status-bar: the count popover floats while hovered or focused", async function (fx) {
    var el = await mount(fx, "st");
    var count = q(el, ".ah-status-bar__count"), pop = q(count, ".ah-status-bar__popover");
    T.fire(q(count, ".ah-status-bar__count-num"), "mouseover", { relatedTarget: el });
    T.eq(pop.style.position, "fixed", "floated");
    T.eq(pop.getAttribute("data-ah-placement") !== null, true);
    T.fire(count, "mouseout", { relatedTarget: el });
    await wait(260);
    T.eq(pop.style.position, "", "released after the fade");
    count.focus();
    T.eq(pop.style.position, "fixed", "focus shows it");
    count.blur();
    await wait(260);
    T.eq(pop.style.position, "");
  });

  T.test("status-bar: removal stops the float; re-insertion works", async function (fx) {
    var el = await mount(fx, "st");
    var count = q(el, ".ah-status-bar__count"), pop = q(count, ".ah-status-bar__popover");
    T.fire(count, "mouseover", { relatedTarget: el });
    await reinsert(fx, el);
    T.eq(pop.style.position, "", "teardown released it");
    T.fire(count, "mouseover", { relatedTarget: el });
    T.eq(pop.style.position, "fixed");
    AH.destroy(fx);
    T.eq(pop.style.position, "");
  });
})(window.AHTest, window.AH);
