/* expandable-text behaviour (expandable_text.js). Fixtures are reduced
 * copies of aihtml_expandable_text's render. */
(function (T, AH) {
  "use strict";

  var TEXT = '<div class="ah-expandable-text" data-expanded="false" data-truncated="true" data-ah="expandable-text">' +
    '<span class="ah-expandable-text__body"><span data-ah-part="short">Short…</span>' +
    '<span data-ah-part="full" hidden>The full text.</span></span>' +
    '<button class="ah-expandable-text__toggle" type="button" aria-expanded="false" data-ah-expand-label="More"' +
    ' data-ah-collapse-label="Less">More</button></div>';

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function part(el, p) { return el.querySelector("[data-ah-part=" + p + "]"); }
  function btn(el) { return el.querySelector(".ah-expandable-text__toggle"); }
  function log(el) {
    var seen = [];
    el.addEventListener("ah:toggle", function (e) { seen.push(e.detail); });
    return seen;
  }
  function tick() { return new Promise(function (res) { setTimeout(res, 0); }); }

  T.test("expandable-text: the toggle swaps the parts and the label", async function (fx) {
    var el = await mount(fx, TEXT), seen = log(el);
    btn(el).click();
    T.eq(el.getAttribute("data-expanded"), "true");
    T.ok(part(el, "short").hidden && !part(el, "full").hidden);
    T.eq(btn(el).textContent, "Less");
    T.eq(btn(el).getAttribute("aria-expanded"), "true");
    btn(el).click();
    T.ok(!part(el, "short").hidden && part(el, "full").hidden);
    T.eq(btn(el).textContent, "More");
    T.eq(seen, [true, false]);
  });

  T.test("expandable-text: expand / collapse / toggle from the server", async function (fx) {
    var el = await mount(fx, TEXT), seen = log(el);
    AH.invoke(el, "expand");
    AH.invoke(el, "expand");
    AH.invoke(el, "collapse");
    AH.invoke(el, "toggle");
    T.eq(seen, [true, false, true], "no event without a change");
  });

  T.test("expandable-text: removed and inserted again, it works", async function (fx) {
    var el = await mount(fx, TEXT);
    el.remove();
    await tick();
    fx.appendChild(el);
    await T.ready(fx);
    var seen = log(el);
    btn(el).click();
    T.eq(seen, [true], "one listener");
  });
})(window.AHTest, window.AH);
