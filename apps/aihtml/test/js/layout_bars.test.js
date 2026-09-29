/* layout_bars: activity-bar, navigationbar and command behaviours on the
 * markup the server renders. SERVER holds renders of
 * aihtml_layout_bars:activity_bar/4, navigationbar/4 and command/3,
 * generated from Erlang; regenerate them if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER =
{
    "a": "<div class=\"ah-activity-bar\" role=\"tablist\" aria-orientation=\"vertical\" data-placement=\"left\" data-ah=\"activity-bar\" data-ah-value=\"a\" id=\"ab\"><input type=\"hidden\" name=\"v\" value=\"a\"><button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" data-id=\"a\" data-active=\"true\" data-disabled=\"false\" aria-selected=\"true\" aria-label=\"Alpha\" title=\"Alpha\" tabindex=\"0\"><span class=\"ah-activity-bar__icon\">A</span></button><button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" data-id=\"b\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\" aria-label=\"Beta\" title=\"Beta\" tabindex=\"-1\"><span class=\"ah-activity-bar__icon\">B</span></button><div class=\"ah-activity-bar__divider\" role=\"presentation\" data-index=\"2\"></div><button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" data-id=\"c\" data-active=\"false\" data-disabled=\"true\" aria-selected=\"false\" aria-label=\"Gamma\" title=\"Gamma\" tabindex=\"-1\" disabled><span class=\"ah-activity-bar__icon\">C</span></button><button class=\"ah-activity-bar__item\" type=\"button\" role=\"tab\" data-id=\"d\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\" aria-label=\"Delta\" title=\"Delta\" tabindex=\"-1\"><span class=\"ah-activity-bar__icon\">D</span></button></div>",
    "c": "<div class=\"ah-command\" id=\"cmd\" data-ah=\"command\" data-ah-query=\"\" data-close-on-select=\"true\"><div class=\"ah-command__input-wrap\"><input class=\"ah-command__input\" type=\"text\" id=\"cmd-input\" placeholder=\"Type a command or search…\" value=\"\" autocomplete=\"off\" spellcheck=\"false\" aria-label=\"command input\" role=\"combobox\" aria-expanded=\"true\" aria-autocomplete=\"list\" aria-controls=\"cmd-list\" data-command=\"cmd\" data-empty=\"No results found.\"></div><div class=\"ah-command__list\" id=\"cmd-list\" role=\"listbox\"><div class=\"ah-command__group\" role=\"group\"><div class=\"ah-command__group-heading\" role=\"presentation\">Files</div><div class=\"ah-command__item\" id=\"cmd-item-0\" role=\"option\" data-value=\"new\" data-index=\"0\" data-active=\"true\" data-disabled=\"false\" aria-selected=\"true\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">New file</span></div></div><div class=\"ah-command__item\" id=\"cmd-item-1\" role=\"option\" data-value=\"open\" data-index=\"1\" data-active=\"false\" data-disabled=\"true\" aria-selected=\"false\" aria-disabled=\"true\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">Open</span></div></div></div><div class=\"ah-command__group\" role=\"group\"><div class=\"ah-command__group-heading\" role=\"presentation\">View</div><div class=\"ah-command__item\" id=\"cmd-item-2\" role=\"option\" data-value=\"theme\" data-index=\"2\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">Theme</span><span class=\"ah-command__item-desc\">dark or light</span></div></div><div class=\"ah-command__item\" id=\"cmd-item-3\" role=\"option\" data-value=\"side\" data-index=\"3\" data-active=\"false\" data-disabled=\"false\" aria-selected=\"false\"><div class=\"ah-command__item-text\"><span class=\"ah-command__item-label\">Sidebar</span></div></div></div><div class=\"ah-command__empty\" hidden>No results found.</div></div></div>",
    "n": "<div class=\"ah-navigationbar ah-navigationbar-vertical\" id=\"nb\" data-ah=\"navigationbar\" data-ah-value=\"0\" data-expand-mode=\"single_fit_height\" data-animation=\"none\" data-toggle-mode=\"click\" data-expand-duration=\"250\" data-collapse-duration=\"250\"><input type=\"hidden\" name=\"o\" value=\"0\"><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header ah-navigationbar-header-expanded\" id=\"nb-item-0-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"true\" aria-controls=\"nb-item-0-content\"><span class=\"ah-navigationbar-header-text\">One</span><span class=\"ah-navigationbar-arrow ah-navigationbar-arrow-up\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nb-item-0-content\" role=\"region\" aria-labelledby=\"nb-item-0-header\"><div class=\"ah-navigationbar-content\">1</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nb-item-1-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nb-item-1-content\"><span class=\"ah-navigationbar-header-text\">Two</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nb-item-1-content\" role=\"region\" aria-labelledby=\"nb-item-1-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">2</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header ah-navigationbar-disabled\" id=\"nb-item-2-header\" role=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-controls=\"nb-item-2-content\" aria-disabled=\"true\"><span class=\"ah-navigationbar-header-text\">Three</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nb-item-2-content\" role=\"region\" aria-labelledby=\"nb-item-2-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">3</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nb-item-3-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nb-item-3-content\"><span class=\"ah-navigationbar-header-text\">Four</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nb-item-3-content\" role=\"region\" aria-labelledby=\"nb-item-3-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">4</div></div></div></div>",
    "nm": "<div class=\"ah-navigationbar ah-navigationbar-vertical ah-navigationbar-expand-multiple\" id=\"nm\" data-ah=\"navigationbar\" data-ah-value=\"\" data-expand-mode=\"multiple\" data-animation=\"none\" data-toggle-mode=\"click\" data-expand-duration=\"250\" data-collapse-duration=\"250\"><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nm-item-0-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nm-item-0-content\"><span class=\"ah-navigationbar-header-text\">One</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nm-item-0-content\" role=\"region\" aria-labelledby=\"nm-item-0-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">1</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nm-item-1-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nm-item-1-content\"><span class=\"ah-navigationbar-header-text\">Two</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nm-item-1-content\" role=\"region\" aria-labelledby=\"nm-item-1-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">2</div></div></div><div class=\"ah-navigationbar-item\"><div class=\"ah-navigationbar-header\" id=\"nm-item-2-header\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" aria-controls=\"nm-item-2-content\"><span class=\"ah-navigationbar-header-text\">Three</span><span class=\"ah-navigationbar-arrow\" aria-hidden=\"true\">▼</span></div><div class=\"ah-navigationbar-body\" id=\"nm-item-2-content\" role=\"region\" aria-labelledby=\"nm-item-2-header\" style=\"display:none;\"><div class=\"ah-navigationbar-content\">3</div></div></div></div>",
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

  // ------------------------------------------------------------------ activity-bar

  T.test("activity-bar: click activates, fires change and ah:select", function (fx) {
    var el = mount(fx, "a");
    var changes = events(el, "change");
    var selects = events(el, "ah:select");
    $(el).find("[data-id=b]").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "b");
    T.eq($(el).children("input[type=hidden]").val(), "b");
    T.eq($(el).find("[data-id=b]").attr("aria-selected"), "true");
    T.eq($(el).find("[data-id=b]").attr("tabindex"), "0");
    T.eq($(el).find("[data-id=a]").attr("data-active"), "false");
    T.eq(changes, ["b"]);
    T.eq(selects, ["b"]);
    // the active item again: select, no change
    $(el).find("[data-id=b]").trigger("click");
    T.eq(changes.length, 1);
    T.eq(selects.length, 2);
  });

  T.test("activity-bar: arrows skip disabled items and wrap", function (fx) {
    var el = mount(fx, "a");
    var b = $(el).find("[data-id=b]")[0];
    key(b, "ArrowDown");
    T.eq(el.getAttribute("data-ah-value"), "d", "skips disabled c");
    key($(el).find("[data-id=d]")[0], "ArrowDown");
    T.eq(el.getAttribute("data-ah-value"), "a", "wraps");
    key($(el).find("[data-id=a]")[0], "End");
    T.eq(el.getAttribute("data-ah-value"), "d");
    $(el).find("[data-id=c]").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "d", "disabled does nothing");
  });

  T.test("activity-bar: setValue / getValue do not fire change", function (fx) {
    var el = mount(fx, "a");
    var changes = events(el, "change");
    AH.invoke(el, "setValue", "d");
    T.eq(AH.invoke(el, "getValue"), "d");
    T.eq($(el).find("[data-id=d]").attr("data-active"), "true");
    T.eq(changes.length, 0);
  });

  // ------------------------------------------------------------------ navigationbar

  function header(el, i) { return $(el).find(".ah-navigationbar-header").eq(i); }
  function shown(el, i) { return $(el).find(".ah-navigationbar-body").eq(i).css("display") !== "none"; }

  T.test("navigationbar: single_fit_height opens one and never closes it", function (fx) {
    var el = mount(fx, "n");
    var changes = events(el, "change");
    T.ok(shown(el, 0));
    header(el, 1).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "1");
    T.eq($(el).children("input[type=hidden]").val(), "1");
    T.ok(shown(el, 1) && !shown(el, 0));
    T.eq(header(el, 1).attr("aria-expanded"), "true");
    T.eq(header(el, 0).attr("aria-expanded"), "false");
    T.ok(header(el, 1).find(".ah-navigationbar-arrow").hasClass("ah-navigationbar-arrow-up"));
    header(el, 1).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "1", "the open one stays open");
    header(el, 2).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "1", "disabled");
    T.eq(changes, ["1"]);
  });

  T.test("navigationbar: multiple, keyboard and methods", function (fx) {
    var el = mount(fx, "nm");
    var changes = events(el, "change");
    key(header(el, 0)[0], "Enter");
    key(header(el, 2)[0], " ");
    T.eq(el.getAttribute("data-ah-value"), "0,2");
    key(header(el, 0)[0], "Enter");
    T.eq(el.getAttribute("data-ah-value"), "2");
    T.eq(changes.length, 3);
    header(el, 0)[0].focus();
    key(header(el, 0)[0], "ArrowDown");
    T.eq(document.activeElement, header(el, 1)[0]);
    key(header(el, 1)[0], "End");
    T.eq(document.activeElement, header(el, 2)[0]);
    AH.invoke(el, "setValue", "0,1");
    T.eq(AH.invoke(el, "getValue"), [0, 1]);
    T.ok(shown(el, 0) && shown(el, 1) && !shown(el, 2));
    AH.invoke(el, "disable", 1);
    AH.invoke(el, "collapse", 1);
    T.eq(el.getAttribute("data-ah-value"), "0");
    AH.invoke(el, "expand", 1);
    T.eq(el.getAttribute("data-ah-value"), "0", "disabled cannot expand");
    AH.invoke(el, "enable", 1);
    AH.invoke(el, "toggle", 1);
    T.eq(el.getAttribute("data-ah-value"), "0,1");
    T.eq(changes.length, 3, "methods fire no change");
  });

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
