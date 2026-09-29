/* layout_tiles: ribbon and tile-layout behaviours on the markup the
 * server renders. SERVER holds renders of aihtml_layout_tiles:ribbon/4 and
 * tile_layout/4, generated from Erlang; regenerate them if the markup
 * changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER =
{
  "r": "<div class=\"ah-ribbon ah-ribbon-mode-default ah-ribbon-position-top\" id=\"rb\" data-ah=\"ribbon\" data-ah-value=\"home\" data-selection-mode=\"click\"><input type=\"hidden\" name=\"tab\" value=\"home\"><div class=\"ah-ribbon-tabs\"><button class=\"ah-ribbon-scroll-btn ah-ribbon-scroll-left\" type=\"button\" data-scroll-direction=\"left\" aria-label=\"Scroll left\" tabindex=\"-1\">◀</button><div class=\"ah-ribbon-tabs-inner\" role=\"tablist\" aria-label=\"Ribbon tabs\" aria-orientation=\"horizontal\"><button class=\"ah-ribbon-tab ah-ribbon-tab-selected\" type=\"button\" role=\"tab\" id=\"rb-tab-0\" data-index=\"0\" data-key=\"home\" aria-selected=\"true\" aria-controls=\"rb-panel-0\" tabindex=\"0\"><span class=\"ah-ribbon-tab-text\">Home</span></button><button class=\"ah-ribbon-tab ah-ribbon-tab-disabled\" type=\"button\" role=\"tab\" id=\"rb-tab-1\" data-index=\"1\" data-key=\"edit\" aria-selected=\"false\" aria-controls=\"rb-panel-1\" aria-disabled=\"true\" tabindex=\"-1\" disabled><span class=\"ah-ribbon-tab-text\">Edit</span></button><button class=\"ah-ribbon-tab\" type=\"button\" role=\"tab\" id=\"rb-tab-2\" data-index=\"2\" data-key=\"view\" aria-selected=\"false\" aria-controls=\"rb-panel-2\" tabindex=\"-1\"><span class=\"ah-ribbon-tab-text\">View</span></button><button class=\"ah-ribbon-tab\" type=\"button\" role=\"tab\" id=\"rb-tab-3\" data-index=\"3\" data-key=\"data\" aria-selected=\"false\" aria-controls=\"rb-panel-3\" tabindex=\"-1\"><span class=\"ah-ribbon-tab-text\">Data</span></button><div class=\"ah-ribbon-selection-token\" aria-hidden=\"true\"></div></div><button class=\"ah-ribbon-scroll-btn ah-ribbon-scroll-right\" type=\"button\" data-scroll-direction=\"right\" aria-label=\"Scroll right\" tabindex=\"-1\">▶</button></div><div class=\"ah-ribbon-tabs-content\"><div class=\"ah-ribbon-tab-content ah-ribbon-tab-content-active\" id=\"rb-panel-0\" role=\"tabpanel\" aria-labelledby=\"rb-tab-0\" data-index=\"0\" data-key=\"home\"><div class=\"ah-ribbon-group\" role=\"group\" aria-labelledby=\"rb-panel-0-g0\"><div class=\"ah-ribbon-group-content\"><button class=\"ah-ribbon-button-large\" type=\"button\" data-command=\"paste\"><span class=\"ah-ribbon-button-large-text\">Paste</span></button><button class=\"ah-ribbon-button\" type=\"button\" data-command=\"bold\" data-toggle aria-pressed=\"false\"><span class=\"ah-ribbon-button-text\">Bold</span></button><div class=\"ah-ribbon-dropdown\"><button class=\"ah-ribbon-button ah-ribbon-dropdown-toggle\" type=\"button\" data-menu=\"more\" aria-haspopup=\"menu\" aria-expanded=\"false\"><span class=\"ah-ribbon-button-text\">More</span><span class=\"ah-ribbon-caret\" aria-hidden=\"true\">▾</span></button><div class=\"ah-dropdown-btn-popup ah-ribbon-menu\" role=\"menu\" hidden><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-command=\"a\"><span>A</span></button><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-command=\"b\" disabled><span>B</span></button><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-command=\"c\"><span>C</span></button></div></div></div><div class=\"ah-ribbon-group-label\" id=\"rb-panel-0-g0\">G</div></div></div><div class=\"ah-ribbon-tab-content\" id=\"rb-panel-1\" role=\"tabpanel\" aria-labelledby=\"rb-tab-1\" data-index=\"1\" data-key=\"edit\">e</div><div class=\"ah-ribbon-tab-content\" id=\"rb-panel-2\" role=\"tabpanel\" aria-labelledby=\"rb-tab-2\" data-index=\"2\" data-key=\"view\">v</div><div class=\"ah-ribbon-tab-content\" id=\"rb-panel-3\" role=\"tabpanel\" aria-labelledby=\"rb-tab-3\" data-index=\"3\" data-key=\"data\">d</div></div></div>",
  "rc": "<div class=\"ah-ribbon ah-ribbon-mode-collapsed ah-ribbon-position-top ah-ribbon-collapsible\" id=\"rc\" data-ah=\"ribbon\" data-ah-value=\"home\" data-selection-mode=\"click\"><div class=\"ah-ribbon-tabs\"><button class=\"ah-ribbon-scroll-btn ah-ribbon-scroll-left\" type=\"button\" data-scroll-direction=\"left\" aria-label=\"Scroll left\" tabindex=\"-1\">◀</button><div class=\"ah-ribbon-tabs-inner\" role=\"tablist\" aria-label=\"Ribbon tabs\" aria-orientation=\"horizontal\"><button class=\"ah-ribbon-tab ah-ribbon-tab-selected\" type=\"button\" role=\"tab\" id=\"rc-tab-0\" data-index=\"0\" data-key=\"home\" aria-selected=\"true\" aria-controls=\"rc-panel-0\" tabindex=\"0\"><span class=\"ah-ribbon-tab-text\">Home</span></button><button class=\"ah-ribbon-tab ah-ribbon-tab-disabled\" type=\"button\" role=\"tab\" id=\"rc-tab-1\" data-index=\"1\" data-key=\"edit\" aria-selected=\"false\" aria-controls=\"rc-panel-1\" aria-disabled=\"true\" tabindex=\"-1\" disabled><span class=\"ah-ribbon-tab-text\">Edit</span></button><button class=\"ah-ribbon-tab\" type=\"button\" role=\"tab\" id=\"rc-tab-2\" data-index=\"2\" data-key=\"view\" aria-selected=\"false\" aria-controls=\"rc-panel-2\" tabindex=\"-1\"><span class=\"ah-ribbon-tab-text\">View</span></button><button class=\"ah-ribbon-tab\" type=\"button\" role=\"tab\" id=\"rc-tab-3\" data-index=\"3\" data-key=\"data\" aria-selected=\"false\" aria-controls=\"rc-panel-3\" tabindex=\"-1\"><span class=\"ah-ribbon-tab-text\">Data</span></button><div class=\"ah-ribbon-selection-token\" aria-hidden=\"true\"></div></div><button class=\"ah-ribbon-scroll-btn ah-ribbon-scroll-right\" type=\"button\" data-scroll-direction=\"right\" aria-label=\"Scroll right\" tabindex=\"-1\">▶</button><button class=\"ah-ribbon-collapse-btn\" type=\"button\" aria-label=\"Collapse the ribbon\" aria-expanded=\"false\" title=\"Collapse the ribbon (Ctrl+F1)\"><svg viewBox=\"0 0 16 16\" width=\"14\" height=\"14\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M4 10l4-4 4 4\"/></svg></button></div><div class=\"ah-ribbon-tabs-content\"><div class=\"ah-ribbon-tab-content ah-ribbon-tab-content-active\" id=\"rc-panel-0\" role=\"tabpanel\" aria-labelledby=\"rc-tab-0\" data-index=\"0\" data-key=\"home\"><div class=\"ah-ribbon-group\" role=\"group\" aria-labelledby=\"rc-panel-0-g0\"><div class=\"ah-ribbon-group-content\"><button class=\"ah-ribbon-button-large\" type=\"button\" data-command=\"paste\"><span class=\"ah-ribbon-button-large-text\">Paste</span></button><button class=\"ah-ribbon-button\" type=\"button\" data-command=\"bold\" data-toggle aria-pressed=\"false\"><span class=\"ah-ribbon-button-text\">Bold</span></button><div class=\"ah-ribbon-dropdown\"><button class=\"ah-ribbon-button ah-ribbon-dropdown-toggle\" type=\"button\" data-menu=\"more\" aria-haspopup=\"menu\" aria-expanded=\"false\"><span class=\"ah-ribbon-button-text\">More</span><span class=\"ah-ribbon-caret\" aria-hidden=\"true\">▾</span></button><div class=\"ah-dropdown-btn-popup ah-ribbon-menu\" role=\"menu\" hidden><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-command=\"a\"><span>A</span></button><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-command=\"b\" disabled><span>B</span></button><button class=\"ah-dropdown-btn-item\" type=\"button\" role=\"menuitem\" tabindex=\"-1\" data-command=\"c\"><span>C</span></button></div></div></div><div class=\"ah-ribbon-group-label\" id=\"rc-panel-0-g0\">G</div></div></div><div class=\"ah-ribbon-tab-content\" id=\"rc-panel-1\" role=\"tabpanel\" aria-labelledby=\"rc-tab-1\" data-index=\"1\" data-key=\"edit\">e</div><div class=\"ah-ribbon-tab-content\" id=\"rc-panel-2\" role=\"tabpanel\" aria-labelledby=\"rc-tab-2\" data-index=\"2\" data-key=\"view\">v</div><div class=\"ah-ribbon-tab-content\" id=\"rc-panel-3\" role=\"tabpanel\" aria-labelledby=\"rc-tab-3\" data-index=\"3\" data-key=\"data\">d</div></div></div>",
  "t": "<div class=\"ah-tl\" id=\"tl\" style=\"height:300px;\" data-ah=\"tile-layout\" data-ah-value=\"{&quot;closed&quot;:[],&quot;root&quot;:{&quot;id&quot;:&quot;n&quot;,&quot;items&quot;:[{&quot;active&quot;:&quot;a&quot;,&quot;id&quot;:&quot;left&quot;,&quot;tabs&quot;:[&quot;a&quot;,&quot;b&quot;],&quot;type&quot;:&quot;tabs&quot;},{&quot;id&quot;:&quot;n1&quot;,&quot;items&quot;:[{&quot;id&quot;:&quot;ed&quot;,&quot;type&quot;:&quot;item&quot;},{&quot;active&quot;:&quot;t&quot;,&quot;id&quot;:&quot;bot&quot;,&quot;tabs&quot;:[&quot;t&quot;],&quot;type&quot;:&quot;tabs&quot;}],&quot;type&quot;:&quot;rows&quot;}],&quot;type&quot;:&quot;columns&quot;}}\" data-splitbar-size=\"4\"><input type=\"hidden\" name=\"arr\" value=\"{&quot;closed&quot;:[],&quot;root&quot;:{&quot;id&quot;:&quot;n&quot;,&quot;items&quot;:[{&quot;active&quot;:&quot;a&quot;,&quot;id&quot;:&quot;left&quot;,&quot;tabs&quot;:[&quot;a&quot;,&quot;b&quot;],&quot;type&quot;:&quot;tabs&quot;},{&quot;id&quot;:&quot;n1&quot;,&quot;items&quot;:[{&quot;id&quot;:&quot;ed&quot;,&quot;type&quot;:&quot;item&quot;},{&quot;active&quot;:&quot;t&quot;,&quot;id&quot;:&quot;bot&quot;,&quot;tabs&quot;:[&quot;t&quot;],&quot;type&quot;:&quot;tabs&quot;}],&quot;type&quot;:&quot;rows&quot;}],&quot;type&quot;:&quot;columns&quot;}}\"><div class=\"ah-tl-group ah-tl-vertical\" data-id=\"n\" data-type=\"layout-group\" data-orientation=\"vertical\" style=\"grid-template-columns:1fr 4px 1fr\"><div class=\"ah-tl-tab-group\" data-id=\"left\" data-type=\"tab-group\"><div class=\"ah-tl-tab-strip\" role=\"tablist\" aria-orientation=\"horizontal\"><div class=\"ah-tl-tab ah-tl-tab-selected\" role=\"tab\" id=\"tl-tab-a\" data-tab-id=\"a\" data-modifiers=\"drag,close\" aria-controls=\"tl-panel-a\" aria-selected=\"true\" tabindex=\"0\"><span class=\"ah-tl-tab-label\">A</span><span class=\"ah-tl-tab-close\" aria-hidden=\"true\">&times;</span></div><div class=\"ah-tl-tab\" role=\"tab\" id=\"tl-tab-b\" data-tab-id=\"b\" data-modifiers=\"drag,close\" aria-controls=\"tl-panel-b\" aria-selected=\"false\" tabindex=\"-1\"><span class=\"ah-tl-tab-label\">B</span><span class=\"ah-tl-tab-close\" aria-hidden=\"true\">&times;</span></div></div><div class=\"ah-tl-tab-content ah-tl-tab-content-active\" id=\"tl-panel-a\" data-id=\"a\" role=\"tabpanel\" aria-labelledby=\"tl-tab-a\">aa</div><div class=\"ah-tl-tab-content\" id=\"tl-panel-b\" data-id=\"b\" role=\"tabpanel\" aria-labelledby=\"tl-tab-b\">bb</div></div><div class=\"ah-tl-splitbar ah-tl-splitbar-v\" role=\"separator\" aria-orientation=\"vertical\" aria-label=\"Resize\" tabindex=\"0\"></div><div class=\"ah-tl-group ah-tl-horizontal\" data-id=\"n1\" data-type=\"layout-group\" data-orientation=\"horizontal\" style=\"grid-template-rows:1fr 4px 1fr\"><div class=\"ah-tl-item\" data-id=\"ed\" data-type=\"layout-item\" data-label=\"Editor\">editor</div><div class=\"ah-tl-splitbar ah-tl-splitbar-h\" role=\"separator\" aria-orientation=\"horizontal\" aria-label=\"Resize\" tabindex=\"0\"></div><div class=\"ah-tl-tab-group\" data-id=\"bot\" data-type=\"tab-group\"><div class=\"ah-tl-tab-strip\" role=\"tablist\" aria-orientation=\"horizontal\"><div class=\"ah-tl-tab ah-tl-tab-selected\" role=\"tab\" id=\"tl-tab-t\" data-tab-id=\"t\" data-modifiers=\"drag\" aria-controls=\"tl-panel-t\" aria-selected=\"true\" tabindex=\"0\"><span class=\"ah-tl-tab-label\">T</span></div></div><div class=\"ah-tl-tab-content ah-tl-tab-content-active\" id=\"tl-panel-t\" data-id=\"t\" role=\"tabpanel\" aria-labelledby=\"tl-tab-t\">tt</div></div></div></div></div>"
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

  function pe(type, target, x, y) {
    target.dispatchEvent(new PointerEvent(type, { bubbles: true, clientX: x, clientY: y,
                                                  button: 0, pointerId: 1 }));
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

  // ------------------------------------------------------------------ tile-layout

  // the test page loads no stylesheet: the grid rules the layout needs
  $("<style>").text(".ah-tl{position:relative;overflow:hidden}" +
    ".ah-tl-group{display:grid;width:100%;height:100%;min-width:0;min-height:0}" +
    ".ah-tl-vertical{grid-auto-flow:column}.ah-tl-horizontal{grid-auto-flow:row}" +
    ".ah-tl-tab-group{display:flex;flex-direction:column;min-width:0;min-height:0}" +
    ".ah-tl-tab-content{display:none}.ah-tl-tab-content-active{display:block}" +
    ".ah-tl-drop-area{position:absolute}").appendTo("head");

  function tl(fx) {
    var el = mount(fx, "t");
    el.style.width = "600px";
    return el;
  }

  function val(el) { return JSON.parse(el.getAttribute("data-ah-value")); }

  T.test("tile-layout: the browser writes the value the server wrote", function (fx) {
    var el = tl(fx);
    var server = el.getAttribute("data-ah-value");
    AH.invoke(el, "select", "a");
    T.eq(el.getAttribute("data-ah-value"), server);
    T.eq(JSON.stringify(AH.invoke(el, "getValue")), server);
  });

  T.test("tile-layout: tab click and keys select, fire change", function (fx) {
    var el = tl(fx);
    var changes = events(el, "change");
    $(el).find("[data-tab-id=b]").trigger("click");
    T.eq(val(el).root.items[0].active, "b");
    T.ok($(el).find("#tl-panel-b").hasClass("ah-tl-tab-content-active"));
    T.eq($(el).find("[data-tab-id=b]").attr("aria-selected"), "true");
    key($(el).find("[data-tab-id=b]")[0], "ArrowRight");
    T.eq(val(el).root.items[0].active, "a");
    T.eq($(el).children("input[type=hidden]").val(), el.getAttribute("data-ah-value"));
    T.eq(changes.length, 2);
  });

  T.test("tile-layout: closing tabs, then the empty group", function (fx) {
    var el = tl(fx);
    var changes = events(el, "change");
    $(el).find("[data-tab-id=a] .ah-tl-tab-close")[0].click();
    T.eq(val(el).closed, ["a"]);
    T.eq(val(el).root.items[0].active, "b");
    key($(el).find("[data-tab-id=b]")[0], "Delete");
    var v = val(el);
    T.eq(v.closed, ["a", "b"]);
    // the left group went, and the columns group with a single child gave way to it
    T.eq(v.root.type, "rows");
    T.eq(v.root.items.map(function (n) { return n.id; }), ["ed", "bot"]);
    T.eq($(el).find(".ah-tl-splitbar").length, 1);
    key($(el).find("[data-tab-id=t]")[0], "Delete");
    T.eq(val(el).closed, ["a", "b"], "t cannot close");
    T.eq(changes.length, 2);
  });

  T.test("tile-layout: splitbar keys and drag resize as fr tracks", function (fx) {
    var el = tl(fx);
    var changes = events(el, "change");
    var bar = $(el).find(".ah-tl-splitbar-v")[0];
    key(bar, "ArrowRight", { shiftKey: true });
    var sizes = val(el).root.items.map(function (n) { return n.size; });
    T.ok(/fr$/.test(sizes[0]) && /fr$/.test(sizes[1]), sizes.join());
    T.ok(parseFloat(sizes[0]) > parseFloat(sizes[1]), "left grew");
    var r = bar.getBoundingClientRect();
    pe("pointerdown", bar, r.left + 1, r.top + 10);
    pe("pointermove", document, r.left - 99, r.top + 10);
    pe("pointerup", document, r.left - 99, r.top + 10);
    var lr = $(el).find("[data-id=left]")[0].getBoundingClientRect();
    T.ok(Math.abs(lr.right - (r.left - 100)) < 3, "left ends at " + lr.right);
    T.eq(changes.length, 2);
  });

  T.test("tile-layout: dragging a tab onto a tile makes a tab group", function (fx) {
    var el = tl(fx);
    var tab = $(el).find("[data-tab-id=b]")[0];
    var tr = tab.getBoundingClientRect();
    var ed = $(el).find("[data-id=ed]")[0].getBoundingClientRect();
    var cx = ed.left + ed.width / 2, cy = ed.top + ed.height / 2;
    pe("pointerdown", tab, tr.left + 3, tr.top + 3);
    pe("pointermove", document, tr.left + 20, tr.top + 20);
    pe("pointermove", document, cx, cy);
    T.eq($(el).children(".ah-tl-drop-area").length, 1);
    pe("pointerup", document, cx, cy);
    T.eq($(el).children(".ah-tl-drop-area").length, 0);
    T.eq($(".ah-tl-feedback, .ah-tl-overlay").length, 0);
    var g = val(el).root.items[1].items[0];
    T.eq(g.type, "tabs");
    T.eq(g.tabs, ["ed", "b"]);
    T.eq(g.active, "b");
    // the tile's content moved into its tab panel, the tab from the template
    T.eq($(el).find("#tl-panel-ed").text(), "editor");
    T.eq($(el).find("#tl-tab-ed .ah-tl-tab-label").text(), "Editor");
  });

  T.test("tile-layout: dragging a tab to an edge splits the pane", function (fx) {
    var el = tl(fx);
    var tab = $(el).find("[data-tab-id=t]")[0];
    var tr = tab.getBoundingClientRect();
    var left = $(el).find("[data-id=left]")[0].getBoundingClientRect();
    var x = left.left + left.width / 2, y = left.bottom - 5;
    pe("pointerdown", tab, tr.left + 3, tr.top + 3);
    pe("pointermove", document, tr.left + 20, tr.top + 20);
    pe("pointermove", document, x, y);
    pe("pointerup", document, x, y);
    var v = val(el).root;
    // bottom edge of a column: a rows group [left, new group with t]
    T.eq(v.items[0].type, "rows");
    T.eq(v.items[0].items[0].id, "left");
    T.eq(v.items[0].items[1].tabs, ["t"]);
    // the old bottom group was emptied and removed; ed stands alone
    T.eq(v.items[1].type, "item");
    T.eq(v.items[1].id, "ed");
    T.eq($(el).find("#tl-panel-t").text(), "tt");
  });

  T.test("tile-layout: Escape cancels a drag", function (fx) {
    var el = tl(fx);
    var before = el.getAttribute("data-ah-value");
    var tab = $(el).find("[data-tab-id=a]")[0];
    var tr = tab.getBoundingClientRect();
    pe("pointerdown", tab, tr.left + 3, tr.top + 3);
    pe("pointermove", document, tr.left + 200, tr.top + 100);
    key(document, "Escape");
    pe("pointerup", document, tr.left + 200, tr.top + 100);
    T.eq(el.getAttribute("data-ah-value"), before);
    T.eq($(".ah-tl-feedback, .ah-tl-overlay").length, 0);
  });
})(window.AHTest, window.jQuery, window.AH);
