/* sidenav: the sidenav behaviour on the markup the server renders.
 * SERVER holds renders of the aihtml_example_demo_sidenav demos, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "sn": "<div class=\"flex h-[420px] border border-line rounded overflow-hidden\"><aside class=\"ah-sidenav\" data-ah=\"sidenav\" data-ah-value=\"roles\" style=\"--ah-ssn-height:100%\"><div class=\"ah-sidenav__head\"><div class=\"ah-sidenav__brand\"><span class=\"ah-sidenav__brand-mark\"><strong>S</strong></span><span class=\"ah-sidenav__brand-name\">Sigil</span></div><button class=\"ah-sidenav__toggle\" type=\"button\" aria-label=\"Toggle navigation\" aria-expanded=\"true\"><svg width=\"18\" height=\"18\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m15 18-6-6 6-6\"/></svg></button></div><div class=\"ah-sidenav__nav\"><nav class=\"ah-nav-tree\"><div class=\"ah-nav-tree__group\"><div class=\"ah-nav-tree__group-label\">Overview</div><a class=\"ah-nav-tree__item\" href=\"#\" data-route=\"dashboard\" title=\"Dashboard\"><span class=\"ah-nav-tree__label\">Dashboard</span></a><a class=\"ah-nav-tree__item\" href=\"#\" data-route=\"analytics\" title=\"Analytics\"><span class=\"ah-nav-tree__label\">Analytics</span></a></div><div class=\"ah-nav-tree__group\"><div class=\"ah-nav-tree__group-label\">Management</div><details class=\"ah-nav-tree__node\" open><summary class=\"ah-nav-tree__item ah-nav-tree__item--parent ah-is-open\" title=\"Users\" data-route=\"users\"><span class=\"ah-nav-tree__label\">Users</span><span class=\"ah-nav-tree__caret\"><svg class=\"ah-nav-tree__caret-svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m6 9 6 6 6-6\"/></svg></span></summary><div class=\"ah-nav-tree__children\"><div class=\"ah-nav-tree__children-inner\"><a class=\"ah-nav-tree__item\" href=\"#\" data-route=\"user_list\" title=\"List\"><span class=\"ah-nav-tree__label\">List</span></a><a class=\"ah-nav-tree__item ah-is-active\" href=\"#\" data-route=\"roles\" title=\"Roles\" aria-current=\"page\"><span class=\"ah-nav-tree__label\">Roles</span></a></div></div></details><a class=\"ah-nav-tree__item\" href=\"#\" data-route=\"settings\" title=\"Settings\"><span class=\"ah-nav-tree__label\">Settings</span></a></div></nav></div><div class=\"ah-sidenav__footer\"><small class=\"text-muted\">v1.0</small></div></aside><div class=\"p-4 text-sm text-muted\">Content</div></div>"
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

  function item(el, route) { return q(el, ".ah-nav-tree__item[data-route=" + route + "]"); }

  T.test("sidenav: a link click marks it and fires change", async function (fx) {
    fx.innerHTML = SERVER.sn;
    await T.ready(fx);
    var el = fx.querySelector("[data-ah=sidenav]");
    var changes = events(el, "change");
    q(item(el, "settings"), ".ah-nav-tree__label").click();
    T.eq(el.getAttribute("data-ah-value"), "settings");
    T.ok(item(el, "settings").classList.contains("ah-is-active"));
    T.eq(item(el, "settings").getAttribute("aria-current"), "page");
    T.eq(item(el, "roles").getAttribute("aria-current"), null);
    item(el, "settings").click();
    T.eq(changes, ["settings"], "the same link again: no change");
  });

  T.test("sidenav: collapse / expand, a group click expands; details toggle", async function (fx) {
    fx.innerHTML = SERVER.sn;
    await T.ready(fx);
    var el = fx.querySelector("[data-ah=sidenav]");
    var got = [];
    el.addEventListener("ah:collapse", function (e) { got.push(e.detail.collapsed); });
    var toggle = q(el, ".ah-sidenav__toggle");
    toggle.click();
    T.ok(el.classList.contains("ah-sidenav-collapsed"));
    T.eq(toggle.getAttribute("aria-expanded"), "false");
    var details = q(el, "details"), summary = q(details, "summary");
    details.open = false;
    summary.click();
    T.ok(!el.classList.contains("ah-sidenav-collapsed"), "a group click expands");
    T.ok(details.open);
    AH.invoke(el, "collapse");
    AH.invoke(el, "toggle");
    T.eq(got, [true, false, true, false]);
    details.open = false;
    await wait(20);
    T.ok(!summary.classList.contains("ah-is-open"), "toggle event syncs the summary");
    AH.invoke(el, "setValue", "user_list");
    T.ok(details.open, "setValue opens the group around it");
    T.ok(summary.classList.contains("ah-is-open"));
    T.eq(q(el, ".ah-is-active[data-route]").getAttribute("data-route"), "user_list");
  });

  T.test("sidenav: arrow keys move between visible entries; re-insertion", async function (fx) {
    fx.innerHTML = SERVER.sn;
    await T.ready(fx);
    var el = fx.querySelector("[data-ah=sidenav]");
    item(el, "dashboard").focus();
    T.key(item(el, "dashboard"), "ArrowDown");
    T.eq(document.activeElement, item(el, "analytics"));
    T.key(item(el, "analytics"), "End");
    T.eq(document.activeElement, item(el, "settings"));
    var box = el.parentNode;
    box.removeChild(el);
    await wait(0);
    box.insertBefore(el, box.firstChild);
    await T.ready(fx);
    var changes = events(el, "change");
    item(el, "analytics").click();
    T.eq(changes, ["analytics"]);
  });
})(window.AHTest, window.AH);
