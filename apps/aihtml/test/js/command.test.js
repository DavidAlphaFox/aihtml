/* command: the command behaviour on the markup the server renders.
 * SERVER holds renders of aihtml_command:command/3, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER =
{
    "c": "<div class=\"ah-command\" id=\"cmd\" data-ah=\"command\" data-ah-query=\"\" data-close-on-select=\"true\"><div class=\"ah-command__input-wrap\"><input class=\"ah-command__input\" type=\"text\" id=\"cmd-input\" placeholder=\"Type a command or search…\" value=\"\" autocomplete=\"off\" spellcheck=\"false\" aria-label=\"command input\" role=\"combobox\" aria-expanded=\"true\" aria-autocomplete=\"list\" aria-controls=\"cmd-list\" data-command=\"cmd\" data-empty=\"No results found.\"></div><div class=\"ah-command__list\" id=\"cmd-list\" role=\"listbox\"><div class=\"ah-command__group\" role=\"group\"><div class=\"ah-command__group-heading\" role=\"presentation\">Files</div><div class=\"ah-command__item\" id=\"cmd-item-0\" role=\"option\" data-value=\"new\" data-index=\"0\" data-active=\"true\" data-disabled=\"false\" aria-selected=\"true\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">New file</span></div></div><div class=\"ah-command__item\" id=\"cmd-item-1\" role=\"option\" data-value=\"open\" data-index=\"1\" data-active=\"false\" data-disabled=\"true\" aria-selected=\"false\" aria-disabled=\"true\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">Open</span></div></div></div><div class=\"ah-command__group\" role=\"group\"><div class=\"ah-command__group-heading\" role=\"presentation\">View</div><div class=\"ah-command__item\" id=\"cmd-item-2\" role=\"option\" data-value=\"theme\" data-index=\"2\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">Theme</span><span class=\"ah-command__item-desc\">dark or light</span></div></div><div class=\"ah-command__item\" id=\"cmd-item-3\" role=\"option\" data-value=\"side\" data-index=\"3\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">Sidebar</span></div></div></div><div class=\"ah-command__empty\" hidden>No results found.</div></div></div>",
    "p": "<div class=\"ah-command-overlay\" hidden><div class=\"ah-command ah-command-panel\" id=\"pal\" role=\"dialog\" aria-modal=\"true\" aria-label=\"Command palette\" data-ah=\"command\" data-ah-query=\"\" data-hotkey=\"k\" data-close-on-select=\"true\"><div class=\"ah-command__input-wrap\"><input class=\"ah-command__input\" type=\"text\" id=\"pal-input\" placeholder=\"Type a command or search…\" value=\"\" autocomplete=\"off\" spellcheck=\"false\" aria-label=\"command input\" role=\"combobox\" aria-expanded=\"true\" aria-autocomplete=\"list\" aria-controls=\"pal-list\" data-command=\"pal\" data-empty=\"No results found.\"></div><div class=\"ah-command__list\" id=\"pal-list\" role=\"listbox\"><div class=\"ah-command__group\" role=\"group\"><div class=\"ah-command__item\" id=\"pal-item-0\" role=\"option\" data-value=\"Alpha\" data-index=\"0\" data-active=\"true\" data-disabled=\"false\" aria-selected=\"true\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">Alpha</span></div></div><div class=\"ah-command__item\" id=\"pal-item-1\" role=\"option\" data-value=\"Beta\" data-index=\"1\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">Beta</span></div></div></div><div class=\"ah-command__empty\" hidden>No results found.</div></div></div></div>"
  }
;

  function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    AH.mount(fx);
    return fx.querySelector("[data-ah]");
  }

  function key(target, k, extra) {
    $(target).trigger($.Event("keydown", $.extend({ key: k }, extra || {})));
  }

  function events(el, type) {
    var got = [];
    $(el).on(type, function (e, d) { if (e.target === el) { got.push(d === undefined ? el.getAttribute("data-ah-value") : d); } });
    return got;
  }

  // ------------------------------------------------------------------ command

  function visible(el) {
    return $(el).find(".ah-command__item").not("[hidden]").map(function () {
      return this.getAttribute("data-value");
    }).get();
  }
  function active(el) { return $(el).find(".ah-command__item[data-active=true]").attr("data-value"); }
  function type(el, q) { $(el).find(".ah-command__input").val(q).trigger("input"); }

  T.test("command: filters by value, label and description", function (fx) {
    var el = mount(fx, "c");
    var queries = events(el, "ah:query");
    T.eq(visible(el), ["new", "open", "theme", "side"]);
    type(el, "DARK");
    T.eq(visible(el), ["theme"]);
    T.ok($(el).find(".ah-command__group").eq(0).prop("hidden"), "empty group hidden");
    T.ok($(el).find(".ah-command__empty").prop("hidden"));
    T.eq(active(el), "theme");
    type(el, "zzz");
    T.eq(visible(el), []);
    T.ok(!$(el).find(".ah-command__empty").prop("hidden"), "empty text shown");
    T.eq($(el).find(".ah-command__input").attr("aria-activedescendant"), undefined);
    T.eq(queries, ["DARK", "zzz"]);
  });

  T.test("command: arrows, Enter selects, disabled is skipped by Enter", function (fx) {
    var el = mount(fx, "c");
    var inp = $(el).find(".ah-command__input")[0];
    var selects = events(el, "ah:select");
    T.eq(active(el), "new");
    key(inp, "ArrowDown");
    T.eq(active(el), "open");
    T.eq(inp.getAttribute("aria-activedescendant"), "cmd-item-1");
    key(inp, "Enter");
    T.eq(selects.length, 0, "disabled");
    key(inp, "ArrowUp");
    key(inp, "ArrowUp");
    T.eq(active(el), "side", "wraps");
    key(inp, "Enter");
    T.eq(selects, ["side"]);
    T.eq(el.getAttribute("data-ah-value"), "side");
    $(el).find("[data-value=theme]").trigger("click");
    T.eq(selects, ["side", "theme"]);
  });

  T.test("command: palette opens, focuses, closes on Escape / select / backdrop", function (fx) {
    var el = mount(fx, "p");
    var ov = el.parentNode;
    T.ok(ov.hidden);
    AH.invoke(el, "open");
    T.ok(!ov.hidden);
    T.eq(document.activeElement, $(el).find(".ah-command__input")[0]);
    type(el, "be");
    T.eq(visible(el), ["Beta"]);
    key(document.activeElement, "Escape");
    T.ok(ov.hidden);
    AH.invoke(el, "open");
    T.eq(visible(el), ["Alpha", "Beta"], "query reset on open");
    key($(el).find(".ah-command__input")[0], "Enter");
    T.ok(ov.hidden, "closed after select");
    $(document).trigger($.Event("keydown", { key: "k", ctrlKey: true }));
    T.ok(!ov.hidden, "hotkey opens");
    $(ov).trigger($.Event("mousedown", { target: ov }));
    T.ok(ov.hidden, "backdrop closes");
    AH.destroy(fx);
    $(document).trigger($.Event("keydown", { key: "k", ctrlKey: true }));
    T.ok(ov.hidden, "hotkey unbound on destroy");
  });
})(window.AHTest, window.jQuery, window.AH);
