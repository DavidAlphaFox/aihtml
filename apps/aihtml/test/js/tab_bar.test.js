/* tab_bar: the tab-bar behaviour on the markup the server renders.
 * SERVER holds renders of the aihtml_example_demo_tab_bar demos, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "tb": "<div class=\"ah-tab-bar\" role=\"tablist\" data-ah=\"tab-bar\" data-ah-value=\"core\"><div class=\"ah-tab-bar__tab\" role=\"tab\" data-id=\"index\" data-active=\"false\" data-dirty=\"false\" aria-selected=\"false\" tabindex=\"-1\" title=\"index.erl\"><span class=\"ah-tab-bar__label\">index.erl</span><button class=\"ah-tab-bar__close\" type=\"button\" tabindex=\"-1\" aria-label=\"close index.erl\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" aria-hidden=\"true\"><line x1=\"18\" y1=\"6\" x2=\"6\" y2=\"18\"/><line x1=\"6\" y1=\"6\" x2=\"18\" y2=\"18\"/></svg></button></div><div class=\"ah-tab-bar__tab\" role=\"tab\" data-id=\"core\" data-active=\"true\" data-dirty=\"true\" aria-selected=\"true\" tabindex=\"0\" title=\"core.erl\"><span class=\"ah-tab-bar__label\">core.erl</span><span class=\"ah-tab-bar__dot\" aria-hidden=\"true\"></span><button class=\"ah-tab-bar__close\" type=\"button\" tabindex=\"-1\" aria-label=\"close core.erl\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" aria-hidden=\"true\"><line x1=\"18\" y1=\"6\" x2=\"6\" y2=\"18\"/><line x1=\"6\" y1=\"6\" x2=\"18\" y2=\"18\"/></svg></button></div><div class=\"ah-tab-bar__tab\" role=\"tab\" data-id=\"readme\" data-active=\"false\" data-dirty=\"false\" aria-selected=\"false\" tabindex=\"-1\" title=\"README.md\"><span class=\"ah-tab-bar__icon\">📄</span><span class=\"ah-tab-bar__label\">README.md</span><button class=\"ah-tab-bar__close\" type=\"button\" tabindex=\"-1\" aria-label=\"close README.md\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" aria-hidden=\"true\"><line x1=\"18\" y1=\"6\" x2=\"6\" y2=\"18\"/><line x1=\"6\" y1=\"6\" x2=\"18\" y2=\"18\"/></svg></button></div><div class=\"ah-tab-bar__tab\" role=\"tab\" data-id=\"config\" data-active=\"false\" data-dirty=\"false\" aria-selected=\"false\" tabindex=\"-1\" title=\"rebar.config\"><span class=\"ah-tab-bar__label\">rebar.config</span><button class=\"ah-tab-bar__close\" type=\"button\" tabindex=\"-1\" aria-label=\"close rebar.config\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" aria-hidden=\"true\"><line x1=\"18\" y1=\"6\" x2=\"6\" y2=\"18\"/><line x1=\"6\" y1=\"6\" x2=\"18\" y2=\"18\"/></svg></button></div></div>"
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

  function tabs(el) { return qa(el, ".ah-tab-bar__tab"); }
  function ids(el) { return tabs(el).map(function (t) { return t.getAttribute("data-id"); }); }
  function tab(el, id) { return q(el, "[data-id=" + id + "]"); }

  T.test("tab-bar: a click selects and fires change", async function (fx) {
    var el = await mount(fx, "tb");
    var changes = events(el, "change");
    q(tab(el, "readme"), ".ah-tab-bar__label").click();
    T.eq(el.getAttribute("data-ah-value"), "readme");
    T.eq(tab(el, "readme").getAttribute("data-active"), "true");
    T.eq(tab(el, "readme").getAttribute("aria-selected"), "true");
    T.eq(tab(el, "readme").getAttribute("tabindex"), "0");
    T.eq(tab(el, "core").getAttribute("data-active"), "false");
    tab(el, "readme").click();
    T.eq(changes, ["readme"], "the active tab again: no change");
  });

  T.test("tab-bar: closing the active tab selects its neighbour; ah:close carries the id", async function (fx) {
    var el = await mount(fx, "tb");
    var changes = events(el, "change"), closed = [];
    el.addEventListener("ah:close", function (e) { closed.push(e.detail); });
    q(tab(el, "core"), ".ah-tab-bar__close").click();
    T.eq(ids(el), ["index", "readme", "config"]);
    T.eq(el.getAttribute("data-ah-value"), "readme");
    T.eq(closed, ["core"]);
    T.eq(changes, ["readme"]);
    tab(el, "readme").focus();
    T.key(tab(el, "readme"), "Delete");
    T.eq(closed, ["core", "readme"]);
    T.eq(document.activeElement, tab(el, "config"), "focus follows");
    T.key(tab(el, "config"), "ArrowLeft");
    T.eq(el.getAttribute("data-ah-value"), "index");
    T.key(tab(el, "index"), "End");
    T.eq(el.getAttribute("data-ah-value"), "config");
  });

  T.test("tab-bar: methods fire no change; the last tab leaves an empty value", async function (fx) {
    var el = await mount(fx, "tb");
    var changes = events(el, "change");
    AH.invoke(el, "select", "config");
    T.eq(AH.invoke(el, "value"), "config");
    ["index", "core", "readme", "config"].forEach(function (id) { AH.invoke(el, "close", id); });
    T.eq(ids(el), []);
    T.eq(AH.invoke(el, "value"), "");
    T.eq(changes, []);
  });

  T.test("tab-bar: removed and inserted again, one listener", async function (fx) {
    var el = await mount(fx, "tb");
    await reinsert(fx, el);
    var changes = events(el, "change");
    tab(el, "index").click();
    T.eq(changes, ["index"]);
  });
})(window.AHTest, window.AH);
