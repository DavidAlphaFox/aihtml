/* tabs: the tabs behaviour on the markup the server renders.
 * SERVER holds renders of the aihtml_example_demo_tabs demos, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "tabs": "<div class=\"ah-tabs ah-tabs-top\" id=\"ah-tabs-22018\" data-ah=\"tabs\" data-ah-value=\"specs\"><ul class=\"ah-tabs-header\" role=\"tablist\" aria-orientation=\"horizontal\"><li class=\"ah-tabs-item\" id=\"ah-tabs-22018-tab-0\" role=\"tab\" data-key=\"overview\" tabindex=\"-1\" aria-selected=\"false\" aria-controls=\"ah-tabs-22018-panel-0\">Overview</li><li class=\"ah-tabs-item ah-tabs-item-selected\" id=\"ah-tabs-22018-tab-1\" role=\"tab\" data-key=\"specs\" tabindex=\"0\" aria-selected=\"true\" aria-controls=\"ah-tabs-22018-panel-1\">Specs</li><li class=\"ah-tabs-item ah-tabs-item-disabled\" id=\"ah-tabs-22018-tab-2\" role=\"tab\" data-key=\"reviews\" tabindex=\"-1\" aria-selected=\"false\" aria-controls=\"ah-tabs-22018-panel-2\" aria-disabled=\"true\">Reviews</li><li class=\"ah-tabs-item\" id=\"ah-tabs-22018-tab-3\" role=\"tab\" data-key=\"faq\" tabindex=\"-1\" aria-selected=\"false\" aria-controls=\"ah-tabs-22018-panel-3\">FAQ</li></ul><div class=\"ah-tabs-content\"><div class=\"ah-tabs-panel\" id=\"ah-tabs-22018-panel-0\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"ah-tabs-22018-tab-0\" aria-hidden=\"true\" style=\"display:none\"><p>Product overview.</p></div><div class=\"ah-tabs-panel ah-tabs-panel-active\" id=\"ah-tabs-22018-panel-1\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"ah-tabs-22018-tab-1\" aria-hidden=\"false\"><p>Weight 1.2 kg, 13 inch display.</p></div><div class=\"ah-tabs-panel\" id=\"ah-tabs-22018-panel-2\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"ah-tabs-22018-tab-2\" aria-hidden=\"true\" style=\"display:none\"><p>No reviews yet.</p></div><div class=\"ah-tabs-panel\" id=\"ah-tabs-22018-panel-3\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"ah-tabs-22018-tab-3\" aria-hidden=\"true\" style=\"display:none\"><p>Questions and answers.</p></div></div><input type=\"hidden\" name=\"section\" value=\"specs\"></div>",
    "tabsh": "<div class=\"ah-tabs ah-tabs-top\" id=\"ah-tabs-22530\" data-ah=\"tabs\" data-ah-value=\"week\" data-animation=\"none\" data-selection-mode=\"hover\"><ul class=\"ah-tabs-header\" role=\"tablist\" aria-orientation=\"horizontal\"><li class=\"ah-tabs-item\" id=\"ah-tabs-22530-tab-0\" role=\"tab\" data-key=\"day\" tabindex=\"-1\" aria-selected=\"false\" aria-controls=\"ah-tabs-22530-panel-0\">Day</li><li class=\"ah-tabs-item ah-tabs-item-selected\" id=\"ah-tabs-22530-tab-1\" role=\"tab\" data-key=\"week\" tabindex=\"0\" aria-selected=\"true\" aria-controls=\"ah-tabs-22530-panel-1\">Week</li><li class=\"ah-tabs-item\" id=\"ah-tabs-22530-tab-2\" role=\"tab\" data-key=\"month\" tabindex=\"-1\" aria-selected=\"false\" aria-controls=\"ah-tabs-22530-panel-2\">Month</li></ul><div class=\"ah-tabs-content\"><div class=\"ah-tabs-panel\" id=\"ah-tabs-22530-panel-0\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"ah-tabs-22530-tab-0\" aria-hidden=\"true\" style=\"display:none\"><p>Hourly view.</p></div><div class=\"ah-tabs-panel ah-tabs-panel-active\" id=\"ah-tabs-22530-panel-1\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"ah-tabs-22530-tab-1\" aria-hidden=\"false\"><p>Seven days.</p></div><div class=\"ah-tabs-panel\" id=\"ah-tabs-22530-panel-2\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"ah-tabs-22530-tab-2\" aria-hidden=\"true\" style=\"display:none\"><p>Whole month.</p></div></div></div>"
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

  function items(el) { return qa(el, ".ah-tabs-item"); }
  function panels(el) { return qa(el, ".ah-tabs-panel"); }
  function shownPanels(el) {
    return panels(el).map(function (p) { return getComputedStyle(p).display !== "none"; });
  }

  T.test("tabs: a click selects with a fade, fires change, updates the hidden input", async function (fx) {
    var el = await mount(fx, "tabs");
    var changes = events(el, "change");
    items(el)[3].click();
    T.eq(el.getAttribute("data-ah-value"), "faq");
    T.eq(q(el, "input[type=hidden]").value, "faq");
    T.eq(items(el)[3].getAttribute("aria-selected"), "true");
    T.eq(items(el)[3].getAttribute("tabindex"), "0");
    T.eq(items(el)[1].getAttribute("aria-selected"), "false");
    T.eq(panels(el)[3].getAttribute("aria-hidden"), "false");
    T.eq(changes, ["faq"]);
    await wait(260);
    T.eq(shownPanels(el), [false, false, false, true]);
    T.ok(panels(el)[3].classList.contains("ah-tabs-panel-active"));
    T.ok(!panels(el)[1].classList.contains("ah-tabs-panel-active"));
    items(el)[2].click();
    T.eq(el.getAttribute("data-ah-value"), "faq", "disabled tab");
  });

  T.test("tabs: arrows skip disabled tabs and wrap; methods fire no change", async function (fx) {
    var el = await mount(fx, "tabs");
    var changes = events(el, "change");
    T.key(items(el)[1], "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "faq", "skips reviews");
    T.eq(document.activeElement, items(el)[3]);
    T.key(items(el)[3], "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "overview", "wraps");
    T.key(items(el)[0], "End");
    T.eq(el.getAttribute("data-ah-value"), "faq");
    T.eq(changes, ["faq", "overview", "faq"]);
    AH.invoke(el, "enable", "reviews");
    AH.invoke(el, "select", "reviews");
    T.eq(AH.invoke(el, "value"), "reviews");
    AH.invoke(el, "disable", "specs");
    T.ok(items(el)[1].classList.contains("ah-tabs-item-disabled"));
    T.eq(items(el)[1].getAttribute("aria-disabled"), "true");
    AH.invoke(el, "select", "specs");
    T.eq(AH.invoke(el, "value"), "reviews", "a disabled tab cannot be selected");
    T.eq(changes.length, 3);
    await wait(260);
  });

  T.test("tabs: hover selection without animation", async function (fx) {
    var el = await mount(fx, "tabsh");
    var changes = events(el, "change");
    T.fire(items(el)[2], "mouseover", { relatedTarget: q(el, ".ah-tabs-content") });
    T.eq(el.getAttribute("data-ah-value"), "month");
    T.eq(shownPanels(el), [false, false, true]);
    items(el)[0].click();
    T.eq(el.getAttribute("data-ah-value"), "month", "a click does not select in hover mode");
    T.eq(changes, ["month"]);
  });

  T.test("tabs: removed and inserted again, one listener", async function (fx) {
    var el = await mount(fx, "tabsh");
    await reinsert(fx, el);
    var changes = events(el, "change");
    T.fire(items(el)[0], "mouseover");
    T.eq(changes, ["day"]);
  });
  T.test("tabs: setting data-ah-value selects that tab, without change", async function (fx) {
    var el = await mount(fx, "tabs");
    var changes = events(el, "change");
    el.setAttribute("data-ah-value", "overview");
    await wait(260);
    T.eq(items(el)[0].getAttribute("aria-selected"), "true");
    T.eq(shownPanels(el), [true, false, false, false]);
    T.eq(q(el, "input[type=hidden]").value, "overview");
    T.eq(changes, []);
  });

  T.test("tabs: a key that cannot be selected gives way to the shown one", async function (fx) {
    var el = await mount(fx, "tabs");
    el.setAttribute("data-ah-value", "reviews");          // disabled
    await wait(0);
    T.eq(el.getAttribute("data-ah-value"), "specs");
    T.eq(items(el)[1].getAttribute("aria-selected"), "true");
    el.setAttribute("data-ah-value", "nope");
    await wait(0);
    T.eq(el.getAttribute("data-ah-value"), "specs");
  });

  T.test("tabs: a morph to another active tab keeps the controller", async function (fx) {
    var el = await mount(fx, "tabs");
    var before = AH.stimulus().getControllerForElementAndIdentifier(el, "tabs");
    var html = SERVER.tabs.replace('data-ah-value="specs"', 'data-ah-value="faq"')
      .replace('class="ah-tabs-item ah-tabs-item-selected" id="ah-tabs-22018-tab-1" role="tab" data-key="specs" tabindex="0" aria-selected="true"',
               'class="ah-tabs-item" id="ah-tabs-22018-tab-1" role="tab" data-key="specs" tabindex="-1" aria-selected="false"')
      .replace('class="ah-tabs-item" id="ah-tabs-22018-tab-3" role="tab" data-key="faq" tabindex="-1" aria-selected="false"',
               'class="ah-tabs-item ah-tabs-item-selected" id="ah-tabs-22018-tab-3" role="tab" data-key="faq" tabindex="0" aria-selected="true"');
    AH.morph(el, html);
    await wait(0);
    T.ok(AH.stimulus().getControllerForElementAndIdentifier(el, "tabs") === before, "same controller");
    T.eq(el.getAttribute("data-ah-value"), "faq");
    T.eq(items(el)[3].getAttribute("aria-selected"), "true");
    items(el)[0].click();
    T.eq(el.getAttribute("data-ah-value"), "overview", "still answers clicks");
  });
})(window.AHTest, window.AH);
