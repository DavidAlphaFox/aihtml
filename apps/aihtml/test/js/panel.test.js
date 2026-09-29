/* panel: the panel behaviour on the markup the server renders.
 * SERVER holds renders of the aihtml_example_demo_panel demos, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "ph": "<div class=\"w-80\"><div class=\"ah-panel ah-panel-bordered ah-panel-has-header\" id=\"ah-panel-23554\" data-ah=\"panel\" aria-labelledby=\"ah-panel-23554-title\" role=\"region\"><div class=\"ah-panel-header\"><div class=\"ah-panel-title\" id=\"ah-panel-23554-title\">Activity</div><div class=\"ah-panel-actions\"><button class=\"ah-btn ah-btn-sm ah-btn-outlined\" type=\"button\" value=\"refresh\">Refresh</button></div><button class=\"ah-panel-toggle\" type=\"button\" aria-expanded=\"true\" aria-controls=\"ah-panel-23554-body\" aria-label=\"Toggle\"><svg viewBox=\"0 0 24 24\" width=\"16\" height=\"16\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><polyline points=\"6 9 12 15 18 9\"/></svg></button></div><div class=\"ah-panel-wrapper\" id=\"ah-panel-23554-body\" style=\"max-height:180px;\"><div class=\"ah-panel-content\"><p>Deployed 1 minutes ago</p><p>Deployed 2 minutes ago</p><p>Deployed 3 minutes ago</p><p>Deployed 4 minutes ago</p><p>Deployed 5 minutes ago</p><p>Deployed 6 minutes ago</p><p>Deployed 7 minutes ago</p><p>Deployed 8 minutes ago</p><p>Deployed 9 minutes ago</p><p>Deployed 10 minutes ago</p><p>Deployed 11 minutes ago</p><p>Deployed 12 minutes ago</p></div></div></div></div>",
    "pc": "<div class=\"w-80\"><div class=\"ah-panel ah-panel-bordered ah-panel-has-header ah-panel-collapsed\" id=\"ah-panel-23042\" data-ah=\"panel\" aria-labelledby=\"ah-panel-23042-title\" role=\"region\"><div class=\"ah-panel-header\"><div class=\"ah-panel-title\" id=\"ah-panel-23042-title\">Advanced</div><button class=\"ah-panel-toggle\" type=\"button\" aria-expanded=\"false\" aria-controls=\"ah-panel-23042-body\" aria-label=\"Toggle\"><svg viewBox=\"0 0 24 24\" width=\"16\" height=\"16\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><polyline points=\"6 9 12 15 18 9\"/></svg></button></div><div class=\"ah-panel-wrapper\" id=\"ah-panel-23042-body\" style=\"display:none;\"><div class=\"ah-panel-content\"><p>Advanced settings go here.</p></div></div></div></div>"
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

  function toggle(el) { return q(el, ".ah-panel-toggle"); }
  function wrapper(el) { return q(el, ".ah-panel-wrapper"); }
  function shown(el) { return getComputedStyle(wrapper(el)).display !== "none"; }

  T.test("panel: the toggle slides the body and fires ah:collapse / ah:expand", async function (fx) {
    var el = await mount(fx, "ph");
    var seen = [];
    el.addEventListener("ah:collapse", function () { seen.push("collapse"); });
    el.addEventListener("ah:expand", function () { seen.push("expand"); });
    q(toggle(el), "svg").dispatchEvent(new MouseEvent("click", { bubbles: true }));
    T.eq(toggle(el).getAttribute("aria-expanded"), "false");
    T.ok(el.classList.contains("ah-panel-collapsed"));
    T.ok(shown(el), "shown while it slides up");
    await wait(260);
    T.ok(!shown(el), "hidden after 200ms");
    T.eq(wrapper(el).style.maxHeight, "180px", "its own inline style is kept");
    toggle(el).click();
    await wait(260);
    T.ok(shown(el));
    T.eq(seen, ["collapse", "expand"]);
  });

  T.test("panel: methods collapse / expand / toggle quietly; scrollTo scrolls", async function (fx) {
    var el = await mount(fx, "pc");
    var seen = 0;
    el.addEventListener("ah:expand", function () { seen++; });
    el.addEventListener("ah:collapse", function () { seen++; });
    AH.invoke(el, "expand");
    T.ok(!el.classList.contains("ah-panel-collapsed"));
    T.eq(toggle(el).getAttribute("aria-expanded"), "true");
    AH.invoke(el, "toggle");
    T.ok(el.classList.contains("ah-panel-collapsed"));
    AH.invoke(el, "toggle");
    await wait(260);
    T.ok(shown(el));
    T.eq(seen, 0, "methods fire no event");
    fx.innerHTML = SERVER.ph;
    await T.ready(fx);
    var p = fx.querySelector("[data-ah=panel]");
    wrapper(p).style.overflow = "auto";
    AH.invoke(p, "scrollTo", 0, 40);
    T.eq(wrapper(p).scrollTop, 40);
  });

  T.test("panel: removed and inserted again, one listener", async function (fx) {
    var el = await mount(fx, "ph");
    var box = el.parentNode;
    box.removeChild(el);
    await wait(0);
    box.appendChild(el);
    await T.ready(fx);
    toggle(el).click();
    T.eq(toggle(el).getAttribute("aria-expanded"), "false", "toggled once");
    await wait(260);
  });
})(window.AHTest, window.AH);
