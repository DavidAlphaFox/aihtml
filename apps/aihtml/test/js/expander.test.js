/* expander: the expander behaviour on the markup the server renders.
 * SERVER holds renders of the aihtml_example_demo_expander demos, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "exp": "<div class=\"flex flex-col gap-2\"><div class=\"ah-expander ah-expander-top\" id=\"ah-expander-18946\" data-ah=\"expander\" data-ah-value=\"true\" data-accordion=\"faq\"><div class=\"ah-expander-header ah-expander-header-expanded\" id=\"ah-expander-18946-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"true\" aria-controls=\"ah-expander-18946-content\"><span class=\"ah-expander-header-text\">How do I start?</span><span class=\"ah-expander-arrow ah-expander-arrow-expanded\" aria-hidden=\"true\">▾</span></div><div class=\"ah-expander-body\" id=\"ah-expander-18946-content\" role=\"region\" aria-labelledby=\"ah-expander-18946-header\"><div class=\"ah-expander-content\"><p>Create an account first.</p></div></div></div><div class=\"ah-expander ah-expander-top\" id=\"ah-expander-19458\" data-ah=\"expander\" data-ah-value=\"false\" data-accordion=\"faq\"><div class=\"ah-expander-header\" id=\"ah-expander-19458-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"ah-expander-19458-content\"><span class=\"ah-expander-header-text\">Can I cancel?</span><span class=\"ah-expander-arrow\" aria-hidden=\"true\">▾</span></div><div class=\"ah-expander-body\" id=\"ah-expander-19458-content\" role=\"region\" aria-labelledby=\"ah-expander-19458-header\" style=\"display:none\"><div class=\"ah-expander-content\"><p>Yes, any time from settings.</p></div></div></div><div class=\"ah-expander ah-expander-top\" id=\"ah-expander-19970\" data-ah=\"expander\" data-ah-value=\"false\" data-accordion=\"faq\"><div class=\"ah-expander-header\" id=\"ah-expander-19970-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"ah-expander-19970-content\"><span class=\"ah-expander-header-text\">Where is support?</span><span class=\"ah-expander-arrow\" aria-hidden=\"true\">▾</span></div><div class=\"ah-expander-body\" id=\"ah-expander-19970-content\" role=\"region\" aria-labelledby=\"ah-expander-19970-header\" style=\"display:none\"><div class=\"ah-expander-content\"><p>Email support@example.com.</p></div></div></div></div>",
    "exps": "<div class=\"flex flex-col gap-2\"><div class=\"ah-expander ah-expander-bottom\" id=\"ah-expander-20482\" data-ah=\"expander\" data-ah-value=\"false\"><div class=\"ah-expander-header\" id=\"ah-expander-20482-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"ah-expander-20482-content\"><span class=\"ah-expander-header-text\">Header at the bottom</span><span class=\"ah-expander-arrow\" aria-hidden=\"true\">▾</span></div><div class=\"ah-expander-body\" id=\"ah-expander-20482-content\" role=\"region\" aria-labelledby=\"ah-expander-20482-header\" style=\"display:none\"><div class=\"ah-expander-content\"><p>The header sits below.</p></div></div></div><div class=\"ah-expander ah-expander-top ah-expander-no-gutters\" id=\"ah-expander-20994\" data-ah=\"expander\" data-ah-value=\"false\"><div class=\"ah-expander-header\" id=\"ah-expander-20994-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"ah-expander-20994-content\"><span class=\"ah-expander-header-text\">No gutters</span><span class=\"ah-expander-arrow\" aria-hidden=\"true\">▾</span></div><div class=\"ah-expander-body\" id=\"ah-expander-20994-content\" role=\"region\" aria-labelledby=\"ah-expander-20994-header\" style=\"display:none\"><div class=\"ah-expander-content\"><p>No frame around it.</p></div></div></div><div class=\"ah-expander ah-expander-top ah-expander-disabled\" id=\"ah-expander-21506\" data-ah=\"expander\" data-ah-value=\"false\"><div class=\"ah-expander-header\" id=\"ah-expander-21506-header\" role=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-controls=\"ah-expander-21506-content\" aria-disabled=\"true\"><span class=\"ah-expander-header-text\">Disabled</span><span class=\"ah-expander-arrow\" aria-hidden=\"true\">▾</span></div><div class=\"ah-expander-body\" id=\"ah-expander-21506-content\" role=\"region\" aria-labelledby=\"ah-expander-21506-header\" style=\"display:none\"><div class=\"ah-expander-content\"><p>Hidden</p></div></div></div></div>"
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

  function exp(fx, i) { return qa(fx, "[data-ah=expander]")[i]; }
  function head(el) { return q(el, ":scope > .ah-expander-header"); }
  function body(el) { return q(el, ":scope > .ah-expander-body"); }
  function shown(el) { return getComputedStyle(body(el)).display !== "none"; }

  T.test("expander: a click toggles, fires change and the animation events", async function (fx) {
    fx.innerHTML = SERVER.exps;
    await T.ready(fx);
    var el = exp(fx, 0);
    var changes = events(el, "change"), seen = [];
    ["ah:expanding", "ah:expanded", "ah:collapsing", "ah:collapsed"].forEach(function (t) {
      el.addEventListener(t, function () { seen.push(t); });
    });
    T.ok(!shown(el));
    q(head(el), ".ah-expander-header-text").click();
    T.eq(el.getAttribute("data-ah-value"), "true");
    T.eq(head(el).getAttribute("aria-expanded"), "true");
    T.ok(head(el).classList.contains("ah-expander-header-expanded"));
    T.ok(q(el, ".ah-expander-arrow").classList.contains("ah-expander-arrow-expanded"));
    T.ok(shown(el), "shown while it slides down");
    T.eq(changes, ["true"]);
    await wait(320);
    T.eq(seen, ["ah:expanding", "ah:expanded"]);
    T.eq(body(el).style.height, "", "no inline height left");
    T.key(head(el), "Enter");
    T.eq(el.getAttribute("data-ah-value"), "false");
    T.ok(shown(el), "still shown while it slides up");
    await wait(320);
    T.ok(!shown(el), "hidden at the end");
    T.eq(seen, ["ah:expanding", "ah:expanded", "ah:collapsing", "ah:collapsed"]);
    T.eq(changes, ["true", "false"]);
  });

  T.test("expander: disabled ignores the user; methods fire no change", async function (fx) {
    fx.innerHTML = SERVER.exps;
    await T.ready(fx);
    var off = exp(fx, 2), el = exp(fx, 1);
    head(off).click();
    T.key(head(off), " ");
    T.eq(off.getAttribute("data-ah-value"), "false", "disabled");
    var changes = events(el, "change");
    AH.invoke(el, "open");
    T.eq(AH.invoke(el, "isOpen"), true);
    AH.invoke(el, "toggle");
    T.eq(AH.invoke(el, "isOpen"), false);
    AH.invoke(el, "close");
    T.eq(el.getAttribute("data-ah-value"), "false");
    T.eq(changes, []);
  });

  T.test("expander: an accordion keeps one open", async function (fx) {
    fx.innerHTML = SERVER.exp;
    await T.ready(fx);
    var a = exp(fx, 0), b = exp(fx, 1), c = exp(fx, 2);
    var ca = events(a, "change"), cb = events(b, "change");
    head(b).click();
    T.eq([a, b, c].map(function (x) { return x.getAttribute("data-ah-value"); }), ["false", "true", "false"]);
    T.eq(ca, ["false"], "the one it closed reports its change too");
    T.eq(cb, ["true"]);
    AH.invoke(c, "open");
    T.eq(b.getAttribute("data-ah-value"), "false");
    T.eq(cb, ["true"], "a method fires no change");
  });

  T.test("expander: removed and inserted again, one listener", async function (fx) {
    fx.innerHTML = SERVER.exps;
    await T.ready(fx);
    var el = exp(fx, 1), box = el.parentNode;
    box.removeChild(el);
    await wait(0);
    box.appendChild(el);
    await T.ready(fx);
    var changes = events(el, "change");
    head(el).click();
    T.eq(changes, ["true"]);
  });
  T.test("expander: setting data-ah-value opens or closes it, without change", async function (fx) {
    var el = await mount(fx, "exps");
    var changes = events(el, "change");
    var header = q(el, ".ah-expander-header");
    el.setAttribute("data-ah-value", "true");
    await wait(0);
    T.eq(header.getAttribute("aria-expanded"), "true");
    el.setAttribute("data-ah-value", "false");
    await wait(0);
    T.eq(header.getAttribute("aria-expanded"), "false");
    T.eq(changes, []);
  });
})(window.AHTest, window.AH);
