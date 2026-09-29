/* navigationbar: the navigationbar behaviour on the markup the server renders.
 * SERVER holds renders of aihtml_navigationbar:navigationbar/4, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER =
{
    "n": "<div class=\"ah-navigationbar ah-navigationbar-vertical\" id=\"nb\" data-ah=\"navigationbar\" data-ah-value=\"0\" data-expand-mode=\"single_fit_height\" data-animation=\"none\" data-toggle-mode=\"click\" data-expand-duration=\"250\" data-collapse-duration=\"250\"><input type=\"hidden\" name=\"o\" value=\"0\"><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header ah-navigationbar-header-expanded\" id=\"nb-item-0-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"true\" aria-controls=\"nb-item-0-content\"><span class=\"ah-navigationbar-header-text\">One</span><span class=\"ah-navigationbar-arrow ah-navigationbar-arrow-up\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nb-item-0-content\" role=\"region\" aria-labelledby=\"nb-item-0-header\"><div class=\"ah-navigationbar-content\">1</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nb-item-1-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nb-item-1-content\"><span class=\"ah-navigationbar-header-text\">Two</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nb-item-1-content\" role=\"region\" aria-labelledby=\"nb-item-1-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">2</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header ah-navigationbar-disabled\" id=\"nb-item-2-header\" role=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-controls=\"nb-item-2-content\" aria-disabled=\"true\"><span class=\"ah-navigationbar-header-text\">Three</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nb-item-2-content\" role=\"region\" aria-labelledby=\"nb-item-2-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">3</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nb-item-3-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nb-item-3-content\"><span class=\"ah-navigationbar-header-text\">Four</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nb-item-3-content\" role=\"region\" aria-labelledby=\"nb-item-3-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">4</div></div></div></div>",
    "nm": "<div class=\"ah-navigationbar ah-navigationbar-vertical ah-navigationbar-expand-multiple\" id=\"nm\" data-ah=\"navigationbar\" data-ah-value=\"\" data-expand-mode=\"multiple\" data-animation=\"none\" data-toggle-mode=\"click\" data-expand-duration=\"250\" data-collapse-duration=\"250\"><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nm-item-0-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nm-item-0-content\"><span class=\"ah-navigationbar-header-text\">One</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nm-item-0-content\" role=\"region\" aria-labelledby=\"nm-item-0-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">1</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nm-item-1-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nm-item-1-content\"><span class=\"ah-navigationbar-header-text\">Two</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nm-item-1-content\" role=\"region\" aria-labelledby=\"nm-item-1-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">2</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nm-item-2-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nm-item-2-content\"><span class=\"ah-navigationbar-header-text\">Three</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nm-item-2-content\" role=\"region\" aria-labelledby=\"nm-item-2-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">3</div></div></div></div>"
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

  // ------------------------------------------------------------------ navigationbar

  function header(el, i) { return qa(el, ".ah-navigationbar-header")[i]; }
  function shown(el, i) { return getComputedStyle(qa(el, ".ah-navigationbar-body")[i]).display !== "none"; }

  T.test("navigationbar: single_fit_height opens one and never closes it", async function (fx) {
    var el = await mount(fx, "n");
    var changes = events(el, "change");
    T.ok(shown(el, 0));
    header(el, 1).click();
    T.eq(el.getAttribute("data-ah-value"), "1");
    T.eq(q(el, ":scope > input[type=hidden]").value, "1");
    T.ok(shown(el, 1) && !shown(el, 0));
    T.eq(header(el, 1).getAttribute("aria-expanded"), "true");
    T.eq(header(el, 0).getAttribute("aria-expanded"), "false");
    T.ok(q(header(el, 1), ".ah-navigationbar-arrow").classList.contains("ah-navigationbar-arrow-up"));
    header(el, 1).click();
    T.eq(el.getAttribute("data-ah-value"), "1", "the open one stays open");
    header(el, 2).click();
    T.eq(el.getAttribute("data-ah-value"), "1", "disabled");
    T.eq(changes, ["1"]);
  });

  T.test("navigationbar: multiple, keyboard and methods", async function (fx) {
    var el = await mount(fx, "nm");
    var changes = events(el, "change");
    T.key(header(el, 0), "Enter");
    T.key(header(el, 2), " ");
    T.eq(el.getAttribute("data-ah-value"), "0,2");
    T.key(header(el, 0), "Enter");
    T.eq(el.getAttribute("data-ah-value"), "2");
    T.eq(changes.length, 3);
    header(el, 0).focus();
    T.key(header(el, 0), "ArrowDown");
    T.eq(document.activeElement, header(el, 1));
    T.key(header(el, 1), "End");
    T.eq(document.activeElement, header(el, 2));
    AH.invoke(el, "setValue", "0,1");
    T.eq(AH.invoke(el, "getValue"), [0, 1]);
    T.ok(shown(el, 0) && shown(el, 1) && !shown(el, 2));
    AH.invoke(el, "disable", 1);
    AH.invoke(el, "collapse", 1);
    T.eq(el.getAttribute("data-ah-value"), "0");
    AH.invoke(el, "expand", 1);
    T.eq(el.getAttribute("data-ah-value"), "0", "disabled cannot expand");
    AH.invoke(el, "enable", 1);
    AH.invoke(el, "toggle", 1);
    T.eq(el.getAttribute("data-ah-value"), "0,1");
    T.eq(changes.length, 3, "methods fire no change");
  });

  T.test("navigationbar: slide animation, ah:expand / ah:collapse with {index}", async function (fx) {
    fx.innerHTML = SERVER.nm.replace('data-animation="none"', 'data-animation="slide"')
      .replace(/data-(expand|collapse)-duration="250"/g, 'data-$1-duration="40"');
    await T.ready(fx);
    var el = fx.querySelector("[data-ah]");
    var got = [];
    el.addEventListener("ah:expand", function (e) { got.push(["expand", e.detail.index]); });
    el.addEventListener("ah:collapse", function (e) { got.push(["collapse", e.detail.index]); });
    header(el, 0).click();
    T.ok(shown(el, 0), "shown while it slides down");
    await wait(120);
    T.ok(shown(el, 0));
    T.eq(qa(el, ".ah-navigationbar-body")[0].style.overflow, "", "inline styles restored");
    header(el, 0).click();
    T.ok(shown(el, 0), "still shown while it slides up");
    await wait(120);
    T.ok(!shown(el, 0), "hidden at the end");
    T.eq(got, [["expand", 0], ["collapse", 0]]);
  });

  T.test("navigationbar: removed and inserted again, it still works", async function (fx) {
    var el = await mount(fx, "nm");
    await reinsert(fx, el);
    var changes = events(el, "change");
    header(el, 1).click();
    T.eq(changes, ["1"], "one listener, one change");
  });

})(window.AHTest, window.AH);
