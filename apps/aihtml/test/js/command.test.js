/* command: the command behaviour on the markup the server renders.
 * SERVER holds renders of aihtml_command:command/3, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER =
{
    "c": "<div class=\"ah-command\" id=\"cmd\" data-ah=\"command\" data-ah-query=\"\" data-close-on-select=\"true\"><div class=\"ah-command__input-wrap\"><input class=\"ah-command__input\" type=\"text\" id=\"cmd-input\" placeholder=\"Type a command or search…\" value=\"\" autocomplete=\"off\" spellcheck=\"false\" aria-label=\"command input\" role=\"combobox\" aria-expanded=\"true\" aria-autocomplete=\"list\" aria-controls=\"cmd-list\" data-command=\"cmd\" data-empty=\"No results found.\"></div><div class=\"ah-command__list\" id=\"cmd-list\" role=\"listbox\"><div class=\"ah-command__group\" role=\"group\"><div class=\"ah-command__group-heading\" role=\"presentation\">Files</div><div class=\"ah-command__item\" id=\"cmd-item-0\" role=\"option\" data-value=\"new\" data-index=\"0\" data-active=\"true\" data-disabled=\"false\" aria-selected=\"true\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">New file</span></div></div><div class=\"ah-command__item\" id=\"cmd-item-1\" role=\"option\" data-value=\"open\" data-index=\"1\" data-active=\"false\" data-disabled=\"true\" aria-selected=\"false\" aria-disabled=\"true\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">Open</span></div></div></div><div class=\"ah-command__group\" role=\"group\"><div class=\"ah-command__group-heading\" role=\"presentation\">View</div><div class=\"ah-command__item\" id=\"cmd-item-2\" role=\"option\" data-value=\"theme\" data-index=\"2\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">Theme</span><span class=\"ah-command__item-desc\">dark or light</span></div></div><div class=\"ah-command__item\" id=\"cmd-item-3\" role=\"option\" data-value=\"side\" data-index=\"3\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">Sidebar</span></div></div></div><div class=\"ah-command__empty\" hidden>No results found.</div></div></div>",
    "p": "<div class=\"ah-command-overlay\" hidden><div class=\"ah-command ah-command-panel\" id=\"pal\" role=\"dialog\" aria-modal=\"true\" aria-label=\"Command palette\" data-ah=\"command\" data-ah-query=\"\" data-hotkey=\"k\" data-close-on-select=\"true\"><div class=\"ah-command__input-wrap\"><input class=\"ah-command__input\" type=\"text\" id=\"pal-input\" placeholder=\"Type a command or search…\" value=\"\" autocomplete=\"off\" spellcheck=\"false\" aria-label=\"command input\" role=\"combobox\" aria-expanded=\"true\" aria-autocomplete=\"list\" aria-controls=\"pal-list\" data-command=\"pal\" data-empty=\"No results found.\"></div><div class=\"ah-command__list\" id=\"pal-list\" role=\"listbox\"><div class=\"ah-command__group\" role=\"group\"><div class=\"ah-command__item\" id=\"pal-item-0\" role=\"option\" data-value=\"Alpha\" data-index=\"0\" data-active=\"true\" data-disabled=\"false\" aria-selected=\"true\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">Alpha</span></div></div><div class=\"ah-command__item\" id=\"pal-item-1\" role=\"option\" data-value=\"Beta\" data-index=\"1\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">Beta</span></div></div></div><div class=\"ah-command__empty\" hidden>No results found.</div></div></div></div>"
  }
;

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

  // ------------------------------------------------------------------ command

  function visible(el) {
    return qa(el, ".ah-command__item:not([hidden])").map(function (n) {
      return n.getAttribute("data-value");
    });
  }
  function active(el) {
    var a = q(el, ".ah-command__item[data-active=true]");
    return a ? a.getAttribute("data-value") : undefined;
  }
  function type(el, text) {
    var input = q(el, ".ah-command__input");
    input.value = text;
    T.fire(input, "input");
  }

  T.test("command: filters by value, label and description", async function (fx) {
    var el = await mount(fx, "c");
    var queries = events(el, "ah:query");
    T.eq(visible(el), ["new", "open", "theme", "side"]);
    type(el, "DARK");
    T.eq(visible(el), ["theme"]);
    T.ok(qa(el, ".ah-command__group")[0].hidden, "empty group hidden");
    T.ok(q(el, ".ah-command__empty").hidden);
    T.eq(active(el), "theme");
    type(el, "zzz");
    T.eq(visible(el), []);
    T.ok(!q(el, ".ah-command__empty").hidden, "empty text shown");
    T.eq(q(el, ".ah-command__input").getAttribute("aria-activedescendant"), null);
    T.eq(queries, ["DARK", "zzz"]);
  });

  T.test("command: arrows, Enter selects, disabled is skipped by Enter", async function (fx) {
    var el = await mount(fx, "c");
    var inp = q(el, ".ah-command__input");
    var selects = events(el, "ah:select");
    T.eq(active(el), "new");
    T.key(inp, "ArrowDown");
    T.eq(active(el), "open");
    T.eq(inp.getAttribute("aria-activedescendant"), "cmd-item-1");
    T.key(inp, "Enter");
    T.eq(selects.length, 0, "disabled");
    T.key(inp, "ArrowUp");
    T.key(inp, "ArrowUp");
    T.eq(active(el), "side", "wraps");
    T.key(inp, "Enter");
    T.eq(selects, ["side"]);
    T.eq(el.getAttribute("data-ah-value"), "side");
    q(el, "[data-value=theme]").click();
    T.eq(selects, ["side", "theme"]);
  });

  T.test("command: the pointer over an item makes it active; setQuery filters", async function (fx) {
    var el = await mount(fx, "c");
    var queries = events(el, "ah:query");
    T.fire(q(el, "[data-value=side] .ah-command__item-label"), "mouseover",
           { relatedTarget: q(el, ".ah-command__input") });
    T.eq(active(el), "side");
    AH.invoke(el, "setQuery", "the");
    T.eq(visible(el), ["theme"]);
    T.eq(queries, ["the"]);
  });

  T.test("command: palette opens, focuses, closes on Escape / select / backdrop", async function (fx) {
    var el = await mount(fx, "p");
    var ov = el.parentNode;
    T.ok(ov.hidden);
    AH.invoke(el, "open");
    T.ok(!ov.hidden);
    T.eq(document.activeElement, q(el, ".ah-command__input"));
    type(el, "be");
    T.eq(visible(el), ["Beta"]);
    T.key(document.activeElement, "Escape");
    T.ok(ov.hidden);
    AH.invoke(el, "open");
    T.eq(visible(el), ["Alpha", "Beta"], "query reset on open");
    T.key(q(el, ".ah-command__input"), "Enter");
    T.ok(ov.hidden, "closed after select");
    T.key(document, "k", { ctrlKey: true });
    T.ok(!ov.hidden, "hotkey opens");
    T.fire(ov, "mousedown");
    T.ok(ov.hidden, "backdrop closes");
    AH.destroy(fx);
    T.key(document, "k", { ctrlKey: true });
    T.ok(ov.hidden, "hotkey unbound on destroy");
  });

  T.test("command: removed and inserted again, the hotkey works once", async function (fx) {
    var el = await mount(fx, "p");
    var ov = el.parentNode, opens = 0;
    el.addEventListener("ah:open", function () { opens++; });
    await reinsert(fx, ov);
    T.key(document, "k", { ctrlKey: true });
    T.ok(!ov.hidden, "hotkey opens");
    T.eq(opens, 1, "one hotkey listener");
    AH.invoke(el, "close");
    fx.innerHTML = "";
    await wait(0);
    T.key(document, "k", { ctrlKey: true });
    T.eq(opens, 1, "hotkey unbound once removed");
  });
})(window.AHTest, window.AH);
