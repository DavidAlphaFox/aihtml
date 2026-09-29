/* ribbon behaviour on the markup the server renders. SERVER holds
 * renders of aihtml_ribbon:ribbon/4, generated from Erlang; regenerate
 * them if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER =
{
  "r": "<div class=\"ah-ribbon ah-ribbon-mode-default ah-ribbon-position-top\" id=\"rb\" data-ah=\"ribbon\" data-ah-value=\"home\" data-selection-mode=\"click\"><input type=\"hidden\" name=\"tab\" value=\"home\"><div class=\"ah-ribbon-tabs\"><button class=\"ah-ribbon-scroll-btn ah-ribbon-scroll-left\" type=\"button\" data-scroll-direction=\"left\" aria-label=\"Scroll left\" tabindex=\"-1\">◀</button><div class=\"ah-ribbon-tabs-inner\" role=\"tablist\" aria-label=\"Ribbon tabs\" aria-orientation=\"horizontal\"><button class=\"ah-ribbon-tab ah-ribbon-tab-selected\" type=\"button\" role=\"tab\" id=\"rb-tab-0\" data-index=\"0\" data-key=\"home\" aria-selected=\"true\" aria-controls=\"rb-panel-0\" tabindex=\"0\"><span class=\"ah-ribbon-tab-text\">Home</span></button><button class=\"ah-ribbon-tab ah-ribbon-tab-disabled\" type=\"button\" role=\"tab\" id=\"rb-tab-1\" data-index=\"1\" data-key=\"edit\" aria-selected=\"false\" aria-controls=\"rb-panel-1\" aria-disabled=\"true\" tabindex=\"-1\" disabled><span class=\"ah-ribbon-tab-text\">Edit</span></button><button class=\"ah-ribbon-tab\" type=\"button\" role=\"tab\" id=\"rb-tab-2\" data-index=\"2\" data-key=\"view\" aria-selected=\"false\" aria-controls=\"rb-panel-2\" tabindex=\"-1\"><span class=\"ah-ribbon-tab-text\">View</span></button><button class=\"ah-ribbon-tab\" type=\"button\" role=\"tab\" id=\"rb-tab-3\" data-index=\"3\" data-key=\"data\" aria-selected=\"false\" aria-controls=\"rb-panel-3\" tabindex=\"-1\"><span class=\"ah-ribbon-tab-text\">Data</span></button><div class=\"ah-ribbon-selection-token\" aria-hidden=\"true\"></div></div><button class=\"ah-ribbon-scroll-btn ah-ribbon-scroll-right\" type=\"button\" data-scroll-direction=\"right\" aria-label=\"Scroll right\" tabindex=\"-1\">▶</button></div><div class=\"ah-ribbon-tabs-content\"><div class=\"ah-ribbon-tab-content ah-ribbon-tab-content-active\" id=\"rb-panel-0\" role=\"tabpanel\" aria-labelledby=\"rb-tab-0\" data-index=\"0\" data-key=\"home\"><div class=\"ah-ribbon-group\" role=\"group\" aria-labelledby=\"rb-panel-0-g0\"><div class=\"ah-ribbon-group-content\"><button class=\"ah-ribbon-button-large\" type=\"button\" data-command=\"paste\"><span class=\"ah-ribbon-button-large-text\">Paste</span></button><button class=\"ah-ribbon-button\" type=\"button\" data-command=\"bold\" data-toggle aria-pressed=\"false\"><span class=\"ah-ribbon-button-text\">Bold</span></button><div class=\"ah-ribbon-dropdown\"><button class=\"ah-ribbon-button ah-ribbon-dropdown-toggle\" type=\"button\" data-menu=\"more\" aria-haspopup=\"menu\" aria-expanded=\"false\"><span class=\"ah-ribbon-button-text\">More</span><span class=\"ah-ribbon-caret\" aria-hidden=\"true\">▾</span></button><div class=\"ah-dropdown-btn-popup ah-ribbon-menu\" role=\"menu\" hidden><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-command=\"a\"><span>A</span></button><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-command=\"b\" disabled><span>B</span></button><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-command=\"c\"><span>C</span></button></div></div></div><div class=\"ah-ribbon-group-label\" id=\"rb-panel-0-g0\">G</div></div></div><div class=\"ah-ribbon-tab-content\" id=\"rb-panel-1\" role=\"tabpanel\" aria-labelledby=\"rb-tab-1\" data-index=\"1\" data-key=\"edit\">e</div><div class=\"ah-ribbon-tab-content\" id=\"rb-panel-2\" role=\"tabpanel\" aria-labelledby=\"rb-tab-2\" data-index=\"2\" data-key=\"view\">v</div><div class=\"ah-ribbon-tab-content\" id=\"rb-panel-3\" role=\"tabpanel\" aria-labelledby=\"rb-tab-3\" data-index=\"3\" data-key=\"data\">d</div></div></div>",
  "rc": "<div class=\"ah-ribbon ah-ribbon-mode-collapsed ah-ribbon-position-top ah-ribbon-collapsible\" id=\"rc\" data-ah=\"ribbon\" data-ah-value=\"home\" data-selection-mode=\"click\"><div class=\"ah-ribbon-tabs\"><button class=\"ah-ribbon-scroll-btn ah-ribbon-scroll-left\" type=\"button\" data-scroll-direction=\"left\" aria-label=\"Scroll left\" tabindex=\"-1\">◀</button><div class=\"ah-ribbon-tabs-inner\" role=\"tablist\" aria-label=\"Ribbon tabs\" aria-orientation=\"horizontal\"><button class=\"ah-ribbon-tab ah-ribbon-tab-selected\" type=\"button\" role=\"tab\" id=\"rc-tab-0\" data-index=\"0\" data-key=\"home\" aria-selected=\"true\" aria-controls=\"rc-panel-0\" tabindex=\"0\"><span class=\"ah-ribbon-tab-text\">Home</span></button><button class=\"ah-ribbon-tab ah-ribbon-tab-disabled\" type=\"button\" role=\"tab\" id=\"rc-tab-1\" data-index=\"1\" data-key=\"edit\" aria-selected=\"false\" aria-controls=\"rc-panel-1\" aria-disabled=\"true\" tabindex=\"-1\" disabled><span class=\"ah-ribbon-tab-text\">Edit</span></button><button class=\"ah-ribbon-tab\" type=\"button\" role=\"tab\" id=\"rc-tab-2\" data-index=\"2\" data-key=\"view\" aria-selected=\"false\" aria-controls=\"rc-panel-2\" tabindex=\"-1\"><span class=\"ah-ribbon-tab-text\">View</span></button><button class=\"ah-ribbon-tab\" type=\"button\" role=\"tab\" id=\"rc-tab-3\" data-index=\"3\" data-key=\"data\" aria-selected=\"false\" aria-controls=\"rc-panel-3\" tabindex=\"-1\"><span class=\"ah-ribbon-tab-text\">Data</span></button><div class=\"ah-ribbon-selection-token\" aria-hidden=\"true\"></div></div><button class=\"ah-ribbon-scroll-btn ah-ribbon-scroll-right\" type=\"button\" data-scroll-direction=\"right\" aria-label=\"Scroll right\" tabindex=\"-1\">▶</button><button class=\"ah-ribbon-collapse-btn\" type=\"button\" aria-label=\"Collapse the ribbon\" aria-expanded=\"false\" title=\"Collapse the ribbon (Ctrl+F1)\"><svg viewBox=\"0 0 16 16\" width=\"14\" height=\"14\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M4 10l4-4 4 4\"/></svg></button></div><div class=\"ah-ribbon-tabs-content\"><div class=\"ah-ribbon-tab-content ah-ribbon-tab-content-active\" id=\"rc-panel-0\" role=\"tabpanel\" aria-labelledby=\"rc-tab-0\" data-index=\"0\" data-key=\"home\"><div class=\"ah-ribbon-group\" role=\"group\" aria-labelledby=\"rc-panel-0-g0\"><div class=\"ah-ribbon-group-content\"><button class=\"ah-ribbon-button-large\" type=\"button\" data-command=\"paste\"><span class=\"ah-ribbon-button-large-text\">Paste</span></button><button class=\"ah-ribbon-button\" type=\"button\" data-command=\"bold\" data-toggle aria-pressed=\"false\"><span class=\"ah-ribbon-button-text\">Bold</span></button><div class=\"ah-ribbon-dropdown\"><button class=\"ah-ribbon-button ah-ribbon-dropdown-toggle\" type=\"button\" data-menu=\"more\" aria-haspopup=\"menu\" aria-expanded=\"false\"><span class=\"ah-ribbon-button-text\">More</span><span class=\"ah-ribbon-caret\" aria-hidden=\"true\">▾</span></button><div class=\"ah-dropdown-btn-popup ah-ribbon-menu\" role=\"menu\" hidden><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-command=\"a\"><span>A</span></button><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-command=\"b\" disabled><span>B</span></button><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-command=\"c\"><span>C</span></button></div></div></div><div class=\"ah-ribbon-group-label\" id=\"rc-panel-0-g0\">G</div></div></div><div class=\"ah-ribbon-tab-content\" id=\"rc-panel-1\" role=\"tabpanel\" aria-labelledby=\"rc-tab-1\" data-index=\"1\" data-key=\"edit\">e</div><div class=\"ah-ribbon-tab-content\" id=\"rc-panel-2\" role=\"tabpanel\" aria-labelledby=\"rc-tab-2\" data-index=\"2\" data-key=\"view\">v</div><div class=\"ah-ribbon-tab-content\" id=\"rc-panel-3\" role=\"tabpanel\" aria-labelledby=\"rc-tab-3\" data-index=\"3\" data-key=\"data\">d</div></div></div>"
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
    $(el).on(type, function (e, d) {
      if (e.target === el) { got.push(d === undefined ? el.getAttribute("data-ah-value") : d); }
    });
    return got;
  }

  // ------------------------------------------------------------------ ribbon

  T.test("ribbon: click switches tabs and fires change", function (fx) {
    var el = mount(fx, "r");
    var changes = events(el, "change");
    $(el).find(".ah-ribbon-tab[data-key=view]").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "view");
    T.eq($(el).children("input[type=hidden]").val(), "view");
    T.eq($(el).find(".ah-ribbon-tab[data-key=view]").attr("aria-selected"), "true");
    T.eq($(el).find(".ah-ribbon-tab[data-key=view]").attr("tabindex"), "0");
    T.eq($(el).find(".ah-ribbon-tab[data-key=home]").attr("tabindex"), "-1");
    T.ok($(el).find(".ah-ribbon-tab-content[data-key=view]").hasClass("ah-ribbon-tab-content-active"));
    T.ok(!$(el).find(".ah-ribbon-tab-content[data-key=home]").hasClass("ah-ribbon-tab-content-active"));
    $(el).find(".ah-ribbon-tab[data-key=edit]").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "view", "disabled tab");
    T.eq(changes, ["view"]);
  });

  T.test("ribbon: arrows skip disabled tabs and wrap", function (fx) {
    var el = mount(fx, "r");
    key($(el).find(".ah-ribbon-tab[data-key=home]")[0], "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "view");
    key($(el).find(".ah-ribbon-tab[data-key=view]")[0], "End");
    T.eq(el.getAttribute("data-ah-value"), "data");
    key($(el).find(".ah-ribbon-tab[data-key=data]")[0], "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "home");
    key($(el).find(".ah-ribbon-tab[data-key=home]")[0], "ArrowLeft");
    T.eq(el.getAttribute("data-ah-value"), "data");
  });

  T.test("ribbon: commands and toggles fire ah:command with data-command", function (fx) {
    var el = mount(fx, "r");
    var cmds = events(el, "ah:command");
    $(el).find("[data-command=paste]").trigger("click");
    T.eq(el.getAttribute("data-command"), "paste");
    T.ok(!el.hasAttribute("data-pressed"));
    $(el).find("[data-command=bold]").trigger("click");
    T.eq(el.getAttribute("data-pressed"), "true");
    T.eq($(el).find("[data-command=bold]").attr("aria-pressed"), "true");
    T.ok($(el).find("[data-command=bold]").hasClass("ah-ribbon-button-pressed"));
    T.eq(cmds, [{ command: "paste", pressed: null }, { command: "bold", pressed: true }]);
    AH.invoke(el, "setPressed", "bold", false);
    T.eq($(el).find("[data-command=bold]").attr("aria-pressed"), "false");
    AH.invoke(el, "disableCommand", "paste");
    $(el).find("[data-command=paste]").trigger("click");
    T.eq(cmds.length, 2, "disabled command");
  });

  T.test("ribbon: dropdown menu opens, navigates and runs an item", function (fx) {
    var el = mount(fx, "r");
    var cmds = events(el, "ah:command");
    var tog = $(el).find("[data-menu=more]")[0];
    var menu = $(tog).siblings(".ah-ribbon-menu")[0];
    key(tog, "ArrowDown");
    T.ok(!menu.hidden);
    T.eq(tog.getAttribute("aria-expanded"), "true");
    T.eq(document.activeElement.getAttribute("data-command"), "a");
    key(document.activeElement, "ArrowDown");
    T.eq(document.activeElement.getAttribute("data-command"), "c", "skips disabled b");
    key(document.activeElement, "Escape");
    T.ok(menu.hidden);
    T.eq(document.activeElement, tog);
    tog.click();
    $(menu).find("[data-command=c]")[0].click();
    T.ok(menu.hidden);
    T.eq(cmds, [{ command: "c", pressed: null }]);
  });

  T.test("ribbon: collapsed panels open on a tab click and close on a command", function (fx) {
    var el = mount(fx, "rc");
    var ev = [];
    $(el).on("ah:collapse ah:expand", function (e) { ev.push(e.type); });
    T.ok(el.classList.contains("ah-ribbon-mode-collapsed"));
    $(el).find(".ah-ribbon-tab[data-key=home]").trigger("click");
    T.ok(el.classList.contains("ah-ribbon-open"));
    $(el).find(".ah-ribbon-tab[data-key=home]").trigger("click");
    T.ok(!el.classList.contains("ah-ribbon-open"), "same tab toggles");
    $(el).find(".ah-ribbon-tab[data-key=home]").trigger("click");
    $(el).find("[data-command=paste]").trigger("click");
    T.ok(!el.classList.contains("ah-ribbon-open"));
    $(el).find(".ah-ribbon-collapse-btn")[0].click();
    T.ok(el.classList.contains("ah-ribbon-mode-default"));
    T.eq($(el).find(".ah-ribbon-collapse-btn").attr("aria-expanded"), "true");
    key(el, "F1", { ctrlKey: true });
    T.ok(el.classList.contains("ah-ribbon-mode-collapsed"));
    T.eq(ev, ["ah:expand", "ah:collapse"]);
    AH.invoke(el, "expand");
    T.ok(el.classList.contains("ah-ribbon-mode-default"));
    T.eq(ev.length, 2, "methods fire nothing");
  });

  T.test("ribbon: select method does not fire change", function (fx) {
    var el = mount(fx, "r");
    var changes = events(el, "change");
    AH.invoke(el, "select", "data");
    T.eq(AH.invoke(el, "getValue"), "data");
    AH.invoke(el, "disableTab", "view");
    T.ok($(el).find(".ah-ribbon-tab[data-key=view]")[0].disabled);
    AH.invoke(el, "enableTab", "edit");
    T.ok(!$(el).find(".ah-ribbon-tab[data-key=edit]")[0].disabled);
    T.eq(changes.length, 0);
  });
})(window.AHTest, window.jQuery, window.AH);
