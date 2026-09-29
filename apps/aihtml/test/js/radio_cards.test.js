/* Behaviour of radio-cards (radio_cards.js). The fixtures are server renders from
 * aihtml_example_demo_radio_cards, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"rc":"<div class=\"ah-radio-cards\" data-ah=\"radio-cards\" role=\"radiogroup\" data-ah-value=\"pro\" data-columns=\"2\" data-align=\"start\" data-disabled=\"false\"><label class=\"ah-radio-cards__card\" data-value=\"free\" data-index=\"0\" data-selected=\"false\" data-disabled=\"false\"><input class=\"ah-choice-input\" type=\"radio\" value=\"free\" name=\"plan\"><span class=\"ah-radio-cards__text\"><span class=\"ah-radio-cards__label\">Free</span><span class=\"ah-radio-cards__description\">Personal trial, limited features</span></span></label><label class=\"ah-radio-cards__card\" data-value=\"pro\" data-index=\"1\" data-selected=\"true\" data-disabled=\"false\"><input class=\"ah-choice-input\" type=\"radio\" value=\"pro\" name=\"plan\" checked=\"\"><span class=\"ah-radio-cards__text\"><span class=\"ah-radio-cards__label\">Pro</span><span class=\"ah-radio-cards__description\">All features and priority support</span></span></label><label class=\"ah-radio-cards__card\" data-value=\"team\" data-index=\"2\" data-selected=\"false\" data-disabled=\"false\"><input class=\"ah-choice-input\" type=\"radio\" value=\"team\" name=\"plan\"><span class=\"ah-radio-cards__text\"><span class=\"ah-radio-cards__label\">Team</span><span class=\"ah-radio-cards__description\">Collaboration and permissions</span></span></label><label class=\"ah-radio-cards__card\" data-value=\"enterprise\" data-index=\"3\" data-selected=\"false\" data-disabled=\"true\" data-ah-item-disabled=\"\"><input class=\"ah-choice-input\" type=\"radio\" value=\"enterprise\" name=\"plan\" disabled=\"\"><span class=\"ah-radio-cards__text\"><span class=\"ah-radio-cards__label\">Enterprise</span><span class=\"ah-radio-cards__description\">Contact sales</span></span></label></div>"};

  async function mount(fx, html) { fx.innerHTML = html; await T.ready(fx); return fx.firstElementChild; }
  // Take the element out (its controller tears down) and put it back.
  async function reinsert(fx, el) {
    el.remove();
    await new Promise(function (r) { setTimeout(r, 0); });
    fx.appendChild(el);
    await T.ready(fx);
    return el;
  }
  function events(el, type) {
    var seen = [];
    el.addEventListener(type, function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function sleep(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function all(el, sel) { return Array.prototype.slice.call(el.querySelectorAll(sel)); }

  T.test("radio-cards: click and arrows select the card, change on the root", async function (fx) {
    var el = await mount(fx, FX.rc), changes = events(el, "change");
    var cards = all(el, ".ah-radio-cards__card");
    cards[0].click();
    T.eq(el.getAttribute("data-ah-value"), "free");
    T.eq(cards[0].getAttribute("data-selected"), "true");
    T.eq(cards[1].getAttribute("data-selected"), "false");
    T.key(cards[0].querySelector("input"), "ArrowDown");
    T.eq(el.getAttribute("data-ah-value"), "pro");
    T.eq(changes, ["free", "pro"]);
  });

  T.test("radio-cards: setDisabled, setValue; re-insertion", async function (fx) {
    var el = await mount(fx, FX.rc), changes = events(el, "change");
    AH.invoke(el, "setDisabled", true);
    T.eq(el.getAttribute("data-disabled"), "true");
    T.ok(all(el, ".ah-radio-cards__card").every(function (c) { return c.getAttribute("data-disabled") === "true"; }));
    AH.invoke(el, "setDisabled", false);
    AH.invoke(el, "setValue", "free");
    T.eq(AH.invoke(el, "getValue"), "free");
    T.eq(changes, []);
    await reinsert(fx, el);
    el.querySelectorAll(".ah-radio-cards__card")[1].click();
    T.eq(changes, ["pro"], "one change after re-insertion");
  });
})(window.AHTest, window.AH);
