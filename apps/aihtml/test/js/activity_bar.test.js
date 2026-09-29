/* activity_bar: the activity-bar behaviour on the markup the server renders.
 * SERVER holds renders of aihtml_activity_bar:activity_bar/4, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER =
{
    "a": "<div class=\"ah-activity-bar\" role=\"tablist\" aria-orientation=\"vertical\" data-placement=\"left\" data-ah=\"activity-bar\" data-ah-value=\"a\" id=\"ab\"><input type=\"hidden\" name=\"v\" value=\"a\"><button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" data-id=\"a\" data-active=\"true\" data-disabled=\"false\" aria-selected=\"true\" aria-label=\"Alpha\" title=\"Alpha\" tabindex=\"0\"><span class=\"ah-activity-bar__icon\">A</span></button><button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" data-id=\"b\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\" aria-label=\"Beta\" title=\"Beta\" tabindex=\"-1\"><span class=\"ah-activity-bar__icon\">B</span></button><div class=\"ah-activity-bar__divider\" role=\"presentation\" data-index=\"2\"></div><button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" data-id=\"c\" data-active=\"false\" data-disabled=\"true\" aria-selected=\"false\" aria-label=\"Gamma\" title=\"Gamma\" tabindex=\"-1\" disabled><span class=\"ah-activity-bar__icon\">C</span></button><button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" data-id=\"d\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\" aria-label=\"Delta\" title=\"Delta\" tabindex=\"-1\"><span class=\"ah-activity-bar__icon\">D</span></button></div>"
  }
;

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

  // ------------------------------------------------------------------ activity-bar

  T.test("activity-bar: click activates, fires change and ah:select", async function (fx) {
    var el = await mount(fx, "a");
    var changes = events(el, "change");
    var selects = events(el, "ah:select");
    q(el, "[data-id=b]").click();
    T.eq(el.getAttribute("data-ah-value"), "b");
    T.eq(q(el, ":scope > input[type=hidden]").value, "b");
    T.eq(q(el, "[data-id=b]").getAttribute("aria-selected"), "true");
    T.eq(q(el, "[data-id=b]").getAttribute("tabindex"), "0");
    T.eq(q(el, "[data-id=a]").getAttribute("data-active"), "false");
    T.eq(changes, ["b"]);
    T.eq(selects, ["b"]);
    // the active item again: select, no change
    q(el, "[data-id=b]").click();
    T.eq(changes.length, 1);
    T.eq(selects.length, 2);
  });

  T.test("activity-bar: arrows skip disabled items and wrap", async function (fx) {
    var el = await mount(fx, "a");
    T.key(q(el, "[data-id=b]"), "ArrowDown");
    T.eq(el.getAttribute("data-ah-value"), "d", "skips disabled c");
    T.key(q(el, "[data-id=d]"), "ArrowDown");
    T.eq(el.getAttribute("data-ah-value"), "a", "wraps");
    T.key(q(el, "[data-id=a]"), "End");
    T.eq(el.getAttribute("data-ah-value"), "d");
    T.fire(q(el, "[data-id=c]"), "click");
    T.eq(el.getAttribute("data-ah-value"), "d", "disabled does nothing");
  });

  T.test("activity-bar: setValue / getValue do not fire change", async function (fx) {
    var el = await mount(fx, "a");
    var changes = events(el, "change");
    AH.invoke(el, "setValue", "d");
    T.eq(AH.invoke(el, "getValue"), "d");
    T.eq(q(el, "[data-id=d]").getAttribute("data-active"), "true");
    T.eq(changes.length, 0);
  });

  T.test("activity-bar: removed and inserted again, it still works", async function (fx) {
    var el = await mount(fx, "a");
    await reinsert(fx, el);
    var changes = events(el, "change");
    q(el, "[data-id=d]").click();
    T.eq(changes, ["d"]);
  });

})(window.AHTest, window.AH);
