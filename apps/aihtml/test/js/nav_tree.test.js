/* nav_tree: the nav-tree behaviour on markup the server renders. SERVER
 * holds a render of aihtml_nav_tree:nav_tree/4 (id "nt"); regenerate it
 * from Erlang if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "nav": "<nav class=\"ah-nav-tree\" data-ah=\"nav-tree\" data-ah-value=\"home\" id=\"nt\"><div class=\"ah-nav-tree__group\"><div class=\"ah-nav-tree__group-label\">G</div><a class=\"ah-nav-tree__item ah-is-active\" href=\"#/home\" data-route=\"home\" aria-current=\"page\"><span class=\"ah-nav-tree__label\">Home</span></a><details class=\"ah-nav-tree__node\"><summary class=\"ah-nav-tree__item ah-nav-tree__item--parent\"><span class=\"ah-nav-tree__label\">U</span><span class=\"ah-nav-tree__caret\"><svg class=\"ah-nav-tree__caret-svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m6 9 6 6 6-6\"/></svg></span></summary><div class=\"ah-nav-tree__children\"><div class=\"ah-nav-tree__children-inner\"><a class=\"ah-nav-tree__item\" href=\"#/u/p\" data-route=\"u/p\"><span class=\"ah-nav-tree__label\">P</span></a><a class=\"ah-nav-tree__item\" href=\"#/u/q\" data-route=\"u/q\"><span class=\"ah-nav-tree__label\">Q</span></a></div></div></details></div></nav>"
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

  function noFollow(el) {
    el.addEventListener("click", function (e) { if (e.target.closest("a")) { e.preventDefault(); } });
  }

  T.test("nav-tree: following a link marks it and fires change", async function (fx) {
    var el = await mount(fx, "nav");
    var changes = 0;
    el.addEventListener("change", function () { changes++; });
    noFollow(el);
    var link = q(el, "a[data-route='u/q']");
    link.click();
    T.eq(el.getAttribute("data-ah-value"), "u/q");
    T.ok(link.classList.contains("ah-is-active"));
    T.eq(link.getAttribute("aria-current"), "page");
    T.eq(qa(el, ".ah-is-active").length, 1);
    T.eq(qa(el, "details")[0].open, true);
    T.ok(qa(el, "summary").some(function (s) { return s.classList.contains("ah-is-open"); }));
    T.eq(changes, 1);
    link.click();
    T.eq(changes, 1);
    AH.invoke(el, "setValue", "home");
    T.eq(qa(el, "details")[0].open, false);
    T.eq(AH.invoke(el, "getValue"), "home");
    T.eq(changes, 1);
  });

  T.test("nav-tree: removed and inserted again, it still works", async function (fx) {
    var el = await mount(fx, "nav");
    noFollow(el);
    await reinsert(fx, el);
    var changes = events(el, "change");
    q(el, "a[data-route='u/q']").click();
    T.eq(changes, ["u/q"]);
  });
})(window.AHTest, window.AH);
