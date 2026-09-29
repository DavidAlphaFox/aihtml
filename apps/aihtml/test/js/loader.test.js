/* loader: the loader behaviour on the markup the server renders.
 * SERVER holds renders of the aihtml_example_demo_loader demos, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "ld": "<div class=\"relative w-56 h-32 p-3 border border-line rounded\"><p>Shown by AH.invoke(el, &#39;show&#39;) or a server call.</p><div class=\"ah-loader ah-loader-text-bottom ah-loader-hidden\" role=\"status\" aria-live=\"polite\" aria-busy=\"false\" aria-label=\"Loading...\" data-ah=\"loader\" id=\"report-loader\"><div class=\"ah-loader-icon\" aria-hidden=\"true\"></div><div class=\"ah-loader-text\">Loading...</div></div></div>"
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

  function modal() { return document.getElementById("ah-loader-modal"); }

  T.test("loader: show / hide / toggle / text / isOpen", async function (fx) {
    var el = await mount(fx, "ld");
    T.eq(AH.invoke(el, "isOpen"), false);
    AH.invoke(el, "show", 10, 20);
    T.ok(!el.classList.contains("ah-loader-hidden"));
    T.eq(el.getAttribute("aria-busy"), "true");
    T.eq(el.style.left, "10px");
    T.eq(el.style.top, "20px");
    AH.invoke(el, "text", "Saving <b>");
    T.eq(q(el, ".ah-loader-text").textContent, "Saving <b>");
    T.eq(el.getAttribute("aria-label"), "Saving <b>");
    AH.invoke(el, "toggle");
    T.eq(AH.invoke(el, "isOpen"), false);
    T.eq(el.getAttribute("aria-busy"), "false");
  });

  T.test("loader: a modal loader adds the scrim, Escape hides it, removal cleans up", async function (fx) {
    fx.innerHTML = SERVER.ld.replace('data-ah="loader"', 'data-ah="loader" data-modal="true"')
      .replace("ah-loader ah-loader-text-bottom ah-loader-hidden", "ah-loader ah-loader-text-bottom");
    await T.ready(fx);
    var el = fx.querySelector("[data-ah=loader]");
    T.ok(modal() && !modal().classList.contains("ah-loader-hidden"), "scrim shown at setup");
    T.ok(el.classList.contains("ah-loader-center"));
    T.fire(document, "keyup", { key: "Escape" });
    T.eq(AH.invoke(el, "isOpen"), false);
    T.ok(modal().classList.contains("ah-loader-hidden"));
    AH.invoke(el, "show");
    T.ok(!modal().classList.contains("ah-loader-hidden"));
    fx.innerHTML = "";
    await wait(0);
    T.ok(modal().classList.contains("ah-loader-hidden"), "teardown hides the scrim");
    modal().remove();
  });
})(window.AHTest, window.AH);
