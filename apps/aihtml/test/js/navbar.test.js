/* navbar: the navbar behaviour on the markup the server renders.
 * SERVER holds renders of the aihtml_example_demo_navbar demos, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "nb": "<div class=\"ah-navbar\" data-ah=\"navbar\" role=\"tablist\" aria-orientation=\"horizontal\" data-ah-value=\"products\"><div class=\"ah-navbar-header\" style=\"height:36px;\" role=\"button\" tabindex=\"0\" aria-haspopup=\"true\" aria-expanded=\"false\" aria-label=\"Navigation\"><div class=\"ah-navbar-toggle\" aria-hidden=\"true\"><span class=\"ah-navbar-toggle-bar\"></span><span class=\"ah-navbar-toggle-bar\"></span><span class=\"ah-navbar-toggle-bar\"></span></div><span class=\"ah-navbar-title\"></span></div><div class=\"ah-navbar-brand\"><strong>Acme</strong></div><div class=\"ah-navbar-item\" role=\"tab\" data-key=\"home\" tabindex=\"-1\" aria-selected=\"false\">Home</div><div class=\"ah-navbar-item ah-navbar-item-selected\" role=\"tab\" data-key=\"products\" tabindex=\"0\" aria-selected=\"true\">Products</div><div class=\"ah-navbar-item\" role=\"tab\" data-key=\"pricing\" tabindex=\"-1\" aria-selected=\"false\">Pricing</div><div class=\"ah-navbar-item ah-navbar-item-disabled\" role=\"tab\" data-key=\"docs\" tabindex=\"-1\" aria-selected=\"false\" aria-disabled=\"true\">Docs</div><div class=\"ah-navbar-extra\"><button class=\"ah-btn ah-btn-sm ah-btn-primary\" type=\"button\">Sign in</button></div><input type=\"hidden\" name=\"section\" value=\"products\"></div>",
    "nbm": "<div class=\"w-72\"><div class=\"ah-navbar ah-navbar-minimized\" data-ah=\"navbar\" role=\"tablist\" aria-orientation=\"horizontal\" data-ah-value=\"pricing\" data-ah-minimized=\"static\"><div class=\"ah-navbar-header\" style=\"height:36px;\" role=\"button\" tabindex=\"0\" aria-haspopup=\"true\" aria-expanded=\"false\" aria-label=\"Pricing\"><div class=\"ah-navbar-toggle\" aria-hidden=\"true\"><span class=\"ah-navbar-toggle-bar\"></span><span class=\"ah-navbar-toggle-bar\"></span><span class=\"ah-navbar-toggle-bar\"></span></div><span class=\"ah-navbar-title\">Pricing</span></div><div class=\"ah-navbar-item\" role=\"tab\" data-key=\"home\" tabindex=\"-1\" aria-selected=\"false\">Home</div><div class=\"ah-navbar-item\" role=\"tab\" data-key=\"products\" tabindex=\"-1\" aria-selected=\"false\">Products</div><div class=\"ah-navbar-item ah-navbar-item-selected\" role=\"tab\" data-key=\"pricing\" tabindex=\"0\" aria-selected=\"true\">Pricing</div></div></div>"
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

  function item(el, key) { return q(el, ":scope > .ah-navbar-item[data-key=" + key + "]"); }
  function popup() { return document.querySelector(".ah-navbar-popup"); }

  T.test("navbar: a click selects, fires change; arrows move the focus", async function (fx) {
    var el = await mount(fx, "nb");
    var changes = events(el, "change");
    item(el, "pricing").click();
    T.eq(el.getAttribute("data-ah-value"), "pricing");
    T.eq(q(el, "input[type=hidden]").value, "pricing");
    T.ok(item(el, "pricing").classList.contains("ah-navbar-item-selected"));
    T.eq(item(el, "pricing").getAttribute("aria-selected"), "true");
    T.eq(item(el, "products").getAttribute("tabindex"), "-1");
    item(el, "docs").click();
    T.eq(el.getAttribute("data-ah-value"), "pricing", "disabled");
    T.key(item(el, "pricing"), "ArrowRight");
    T.eq(document.activeElement, item(el, "home"), "wraps, skipping docs");
    T.key(item(el, "home"), "Enter");
    T.eq(el.getAttribute("data-ah-value"), "home");
    T.eq(changes, ["pricing", "home"]);
    T.fire(item(el, "home"), "mouseover", { relatedTarget: el });
    T.ok(item(el, "home").classList.contains("ah-navbar-item-hover"));
    T.fire(item(el, "home"), "mouseout", { relatedTarget: el });
    T.ok(!item(el, "home").classList.contains("ah-navbar-item-hover"));
  });

  T.test("navbar: minimized, the header opens a popup list", async function (fx) {
    var el = await mount(fx, "nbm");
    var changes = events(el, "change");
    var head = q(el, ".ah-navbar-header");
    head.click();
    var p = popup();
    T.ok(!!p, "popup open");
    T.eq(head.getAttribute("aria-expanded"), "true");
    T.eq(p.querySelectorAll(".ah-navbar-item").length, 3);
    q(p, "[data-key=products]").click();
    T.eq(el.getAttribute("data-ah-value"), "products");
    T.ok(!popup(), "closed after a choice");
    T.key(head, "Enter");
    T.ok(!!popup());
    T.eq(document.activeElement, q(popup(), ".ah-navbar-item-selected"));
    T.key(document.activeElement, "ArrowDown");
    T.eq(document.activeElement.getAttribute("data-key"), "pricing");
    T.key(document.activeElement, "Escape");
    T.ok(!popup());
    T.eq(document.activeElement, head);
    head.click();
    T.fire(document.body, "mousedown");
    T.ok(!popup(), "outside mousedown closes");
    T.eq(changes, ["products"]);
  });

  T.test("navbar: methods fire no change; removal closes the popup", async function (fx) {
    var el = await mount(fx, "nb");
    var changes = events(el, "change");
    AH.invoke(el, "setValue", "home");
    T.eq(el.getAttribute("data-ah-value"), "home");
    T.ok(item(el, "home").classList.contains("ah-navbar-item-selected"));
    AH.invoke(el, "select", "pricing");
    T.eq(el.getAttribute("data-ah-value"), "pricing");
    T.eq(changes, ["pricing"], "select acts as a user choice (as before)");
    AH.invoke(el, "minimize");
    T.ok(el.classList.contains("ah-navbar-minimized"));
    q(el, ".ah-navbar-header").click();
    T.ok(!!popup());
    await reinsert(fx, el);
    T.ok(!popup(), "teardown closes the popup");
    item(el, "home").click();
    T.eq(changes, ["pricing", "home"], "one listener after re-insertion");
    AH.invoke(el, "restore");
    T.ok(!el.classList.contains("ah-navbar-minimized"));
  });
})(window.AHTest, window.AH);
