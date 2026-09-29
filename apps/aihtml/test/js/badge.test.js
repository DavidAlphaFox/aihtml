/* badge behaviour (badge.js). Fixtures are reduced copies of
 * aihtml_badge's render. */
(function (T, AH) {
  "use strict";

  function badge(opts) {
    opts = opts || {};
    return '<span class="ah-badge-root" data-overlap="rect" data-ah="badge" data-ah-max="99"' +
      (opts.zero ? ' data-ah-show-zero="true"' : "") + '><span class="ah-avatar">A</span>' +
      '<span class="ah-badge-indicator" data-variant="' + (opts.dot ? "dot" : "standard") + '" data-dot="' +
      !!opts.dot + '" data-invisible="false" aria-hidden="true">' + (opts.dot ? "" : "5") + "</span></span>";
  }

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function ind(el) { return el.querySelector(".ah-badge-indicator"); }
  function tick() { return new Promise(function (res) { setTimeout(res, 0); }); }

  T.test("badge: setCount follows the max and zero rules", async function (fx) {
    var el = await mount(fx, badge());
    AH.invoke(el, "setCount", 12);
    T.eq(ind(el).textContent, "12");
    AH.invoke(el, "setCount", 150);
    T.eq(ind(el).textContent, "99+");
    AH.invoke(el, "setCount", "7");
    T.eq(ind(el).textContent, "7");
    AH.invoke(el, "setCount", 0);
    T.eq(ind(el).getAttribute("data-invisible"), "true", "zero hides");
    AH.invoke(el, "setCount", "new");
    T.eq(ind(el).textContent, "new");
    T.eq(ind(el).getAttribute("data-invisible"), "false");
  });

  T.test("badge: show_zero keeps 0; a dot ignores counts", async function (fx) {
    var el = await mount(fx, badge({ zero: true }));
    AH.invoke(el, "setCount", 0);
    T.eq(ind(el).textContent, "0");
    T.eq(ind(el).getAttribute("data-invisible"), "false");
    el = await mount(fx, badge({ dot: true }));
    AH.invoke(el, "setCount", 3);
    T.eq(ind(el).textContent, "");
  });

  T.test("badge: removed and inserted again, it works", async function (fx) {
    var el = await mount(fx, badge());
    el.remove();
    await tick();
    fx.appendChild(el);
    await T.ready(fx);
    AH.invoke(el, "setCount", 4);
    T.eq(ind(el).textContent, "4");
  });
})(window.AHTest, window.AH);
