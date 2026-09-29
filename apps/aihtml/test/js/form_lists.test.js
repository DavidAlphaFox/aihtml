/* Cascader, listbox and transfer behaviours (form_lists.js). The fixtures
 * are server renders from aihtml_form_lists (and the operations its
 * cascader_children/3 and listbox_items/3 send), captured once;
 * regenerate them if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var FX = {"cs":"<div class=\"ah-cascader\" id=\"t-cs\" data-ah=\"cascader\" data-ah-value=\"zj,hz,bj\" data-ah-separator=\" / \" data-ah-empty=\"No results found\"><div class=\"ah-cascader-input-area\"><input class=\"ah-cascader-input\" type=\"text\" id=\"t-cs-input\" autocomplete=\"off\" spellcheck=\"false\" readonly placeholder=\"Please select\" value=\"Zhejiang / Hangzhou / Binjiang\" role=\"combobox\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-cs-menus\"><span class=\"ah-cascader-clear\" role=\"button\" aria-label=\"Clear\">Ã</span><span class=\"ah-cascader-arrow\" aria-hidden=\"true\"><span class=\"ah-cascader-arrow-icon\">â¼</span></span></div><input type=\"hidden\" name=\"r\" value=\"zj,hz,bj\"><div class=\"ah-cascader-popup\" id=\"t-cs-popup\"><div class=\"ah-cascader-menus\" id=\"t-cs-menus\" role=\"group\" style=\"max-height:240px\"><div class=\"ah-cascader-menu-column\" data-level=\"0\" data-parent=\"\"><ul class=\"ah-cascader-menu\" role=\"listbox\"><li class=\"has-children active\" role=\"option\" aria-selected=\"true\" aria-haspopup=\"true\" data-value=\"zj\" data-level=\"0\"><span class=\"ah-cascader-menu-item-label\">Zhejiang</span><span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">â¶</span></li><li class=\"has-children\" role=\"option\" aria-selected=\"false\" aria-haspopup=\"true\" data-value=\"js\" data-level=\"0\" data-lazy><span class=\"ah-cascader-menu-item-label\">Jiangsu</span><span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">â¶</span></li><li role=\"option\" aria-selected=\"false\" data-value=\"hk\" data-level=\"0\"><span class=\"ah-cascader-menu-item-label\">Hong Kong</span></li></ul></div><div class=\"ah-cascader-menu-column\" data-level=\"1\" data-parent=\"zj\" hidden><ul class=\"ah-cascader-menu\" role=\"listbox\"><li class=\"has-children active\" role=\"option\" aria-selected=\"true\" aria-haspopup=\"true\" data-value=\"hz\" data-level=\"1\"><span class=\"ah-cascader-menu-item-label\">Hangzhou</span><span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">â¶</span></li><li role=\"option\" aria-selected=\"false\" aria-disabled=\"true\" data-value=\"nb\" data-level=\"1\"><span class=\"ah-cascader-menu-item-label\">Ningbo</span></li></ul></div><div class=\"ah-cascader-menu-column\" data-level=\"2\" data-parent=\"zj,hz\" hidden><ul class=\"ah-cascader-menu\" role=\"listbox\"><li role=\"option\" aria-selected=\"false\" data-value=\"xihu\" data-level=\"2\"><span class=\"ah-cascader-menu-item-label\">xihu</span></li><li class=\"active\" role=\"option\" aria-selected=\"true\" data-value=\"bj\" data-level=\"2\"><span class=\"ah-cascader-menu-item-label\">Binjiang</span></li></ul></div></div></div><span class=\"ah-cascader-loader\" hidden data-cascader=\"t-cs\" data-ah-on=\"ah:load:g2gDdxdhaWh0bWxfZm9ybV9saXN0c190ZXN0c3cEbG9hZHQAAAAA.u7DNHHV6BpxDaSBfjI7rWv0Bu4uVctZWqlw7DuDO7yo\" data-ah-sync=\"queue\"></span></div>","csf":"<div class=\"ah-cascader ah-cascader-change-on-select ah-cascader-filterable\" id=\"t-csf\" data-ah=\"cascader\" data-ah-value=\"\" data-ah-separator=\" / \" data-ah-empty=\"No results found\" data-ah-change-on-select><div class=\"ah-cascader-input-area\"><input class=\"ah-cascader-input\" type=\"text\" id=\"t-csf-input\" autocomplete=\"off\" spellcheck=\"false\" placeholder=\"Please select\" value=\"\" role=\"combobox\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-csf-menus\" aria-autocomplete=\"list\"><span class=\"ah-cascader-clear\" role=\"button\" aria-label=\"Clear\" hidden>Ã</span><span class=\"ah-cascader-arrow\" aria-hidden=\"true\"><span class=\"ah-cascader-arrow-icon\">â¼</span></span></div><div class=\"ah-cascader-popup\" id=\"t-csf-popup\"><div class=\"ah-cascader-menus\" id=\"t-csf-menus\" role=\"group\" style=\"max-height:240px\"><div class=\"ah-cascader-menu-column\" data-level=\"0\" data-parent=\"\"><ul class=\"ah-cascader-menu\" role=\"listbox\"><li class=\"has-children\" role=\"option\" aria-selected=\"false\" aria-haspopup=\"true\" data-value=\"zj\" data-level=\"0\"><span class=\"ah-cascader-menu-item-label\">Zhejiang</span><span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">â¶</span></li><li class=\"has-children\" role=\"option\" aria-selected=\"false\" aria-haspopup=\"true\" data-value=\"js\" data-level=\"0\" data-lazy><span class=\"ah-cascader-menu-item-label\">Jiangsu</span><span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">â¶</span></li><li role=\"option\" aria-selected=\"false\" data-value=\"hk\" data-level=\"0\"><span class=\"ah-cascader-menu-item-label\">Hong Kong</span></li></ul></div><div class=\"ah-cascader-menu-column\" data-level=\"1\" data-parent=\"zj\" hidden><ul class=\"ah-cascader-menu\" role=\"listbox\"><li class=\"has-children\" role=\"option\" aria-selected=\"false\" aria-haspopup=\"true\" data-value=\"hz\" data-level=\"1\"><span class=\"ah-cascader-menu-item-label\">Hangzhou</span><span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">â¶</span></li><li role=\"option\" aria-selected=\"false\" aria-disabled=\"true\" data-value=\"nb\" data-level=\"1\"><span class=\"ah-cascader-menu-item-label\">Ningbo</span></li></ul></div><div class=\"ah-cascader-menu-column\" data-level=\"2\" data-parent=\"zj,hz\" hidden><ul class=\"ah-cascader-menu\" role=\"listbox\"><li role=\"option\" aria-selected=\"false\" data-value=\"xihu\" data-level=\"2\"><span class=\"ah-cascader-menu-item-label\">xihu</span></li><li role=\"option\" aria-selected=\"false\" data-value=\"bj\" data-level=\"2\"><span class=\"ah-cascader-menu-item-label\">Binjiang</span></li></ul></div></div><ul class=\"ah-cascader-search-panel\" id=\"t-csf-search\" role=\"listbox\" hidden><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" data-path=\"zj\" data-label=\"Zhejiang\"><span class=\"ah-cascader-search-item-label\">Zhejiang</span></li><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" data-path=\"zj,hz\" data-label=\"Zhejiang / Hangzhou\"><span class=\"ah-cascader-search-item-label\">Zhejiang / Hangzhou</span></li><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" data-path=\"zj,hz,xihu\" data-label=\"Zhejiang / Hangzhou / xihu\"><span class=\"ah-cascader-search-item-label\">Zhejiang / Hangzhou / xihu</span></li><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" data-path=\"zj,hz,bj\" data-label=\"Zhejiang / Hangzhou / Binjiang\"><span class=\"ah-cascader-search-item-label\">Zhejiang / Hangzhou / Binjiang</span></li><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" aria-disabled=\"true\" data-path=\"zj,nb\" data-label=\"Zhejiang / Ningbo\"><span class=\"ah-cascader-search-item-label\">Zhejiang / Ningbo</span></li><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" data-path=\"js\" data-label=\"Jiangsu\"><span class=\"ah-cascader-search-item-label\">Jiangsu</span></li><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" data-path=\"hk\" data-label=\"Hong Kong\"><span class=\"ah-cascader-search-item-label\">Hong Kong</span></li></ul></div></div>","csops":[{"id":"t-cs-menus","op":"html","html":"<div class=\"ah-cascader-menu-column\" data-level=\"1\" data-parent=\"js\" hidden><ul class=\"ah-cascader-menu\" role=\"listbox\"><li role=\"option\" aria-selected=\"false\" data-value=\"nj\" data-level=\"1\"><span class=\"ah-cascader-menu-item-label\">Nanjing</span></li><li role=\"option\" aria-selected=\"false\" data-value=\"sz\" data-level=\"1\"><span class=\"ah-cascader-menu-item-label\">Suzhou</span></li></ul></div>","swap":"append"},{"args":["js"],"id":"t-cs","op":"call","method":"childrenLoaded"}],"lb":"<div class=\"ah-listbox ah-listbox-filterable\" id=\"t-lb\" data-ah=\"listbox\" data-ah-value=\"Banana\" tabindex=\"0\" role=\"listbox\" aria-multiselectable=\"false\"><div class=\"ah-listbox-filter\"><input class=\"ah-listbox-filter-input\" type=\"text\" id=\"t-lb-filter\" autocomplete=\"off\" placeholder=\"Search\" aria-label=\"Search\" aria-controls=\"t-lb\" data-listbox=\"t-lb\"></div><div class=\"ah-listbox-content\"><ul class=\"ah-listbox-list\" id=\"t-lb-list\" role=\"none\"><li class=\"ah-listbox-item\" id=\"t-lb-o-0\" role=\"option\" aria-selected=\"false\" data-idx=\"0\" data-value=\"Apple\"><span class=\"ah-listbox-label\">Apple</span></li><li class=\"ah-listbox-item ah-listbox-item-selected\" id=\"t-lb-o-1\" role=\"option\" aria-selected=\"true\" data-idx=\"1\" data-value=\"Banana\"><span class=\"ah-listbox-label\">Banana</span></li><li class=\"ah-listbox-item\" id=\"t-lb-o-2\" role=\"option\" aria-selected=\"false\" data-idx=\"2\" data-value=\"Cherry\"><span class=\"ah-listbox-label\">Cherry</span></li><li class=\"ah-listbox-item ah-listbox-item-disabled\" id=\"t-lb-o-3\" role=\"option\" aria-selected=\"false\" aria-disabled=\"true\" data-idx=\"3\" data-value=\"D\"><span class=\"ah-listbox-label\">Date</span></li><li class=\"ah-listbox-item\" id=\"t-lb-o-4\" role=\"option\" aria-selected=\"false\" data-idx=\"4\" data-value=\"Elder\"><span class=\"ah-listbox-label\">Elder</span></li></ul><div class=\"ah-listbox-empty\" hidden>No data</div></div><input type=\"hidden\" name=\"f\" value=\"Banana\"></div>","lbc":"<div class=\"ah-listbox ah-listbox-checkboxes ah-listbox-filterable\" id=\"t-lbc\" data-ah=\"listbox\" data-ah-value=\"\" tabindex=\"0\" role=\"listbox\" aria-multiselectable=\"true\"><div class=\"ah-listbox-filter\"><input class=\"ah-listbox-filter-input\" type=\"text\" id=\"t-lbc-filter\" autocomplete=\"off\" placeholder=\"Search\" aria-label=\"Search\" aria-controls=\"t-lbc\" data-listbox=\"t-lbc\" data-checkboxes=\"true\"></div><div class=\"ah-listbox-check-all\" role=\"button\" aria-pressed=\"false\"><span class=\"ah-listbox-checkbox\"></span><span class=\"ah-listbox-label\">Select all</span></div><div class=\"ah-listbox-content\"><ul class=\"ah-listbox-list\" id=\"t-lbc-list\" role=\"none\"><li class=\"ah-listbox-group\" role=\"presentation\">g1</li><li class=\"ah-listbox-item\" id=\"t-lbc-o-0\" role=\"option\" aria-selected=\"false\" data-idx=\"0\" data-value=\"a\"><span class=\"ah-listbox-checkbox\"></span><span class=\"ah-listbox-label\">a</span></li><li class=\"ah-listbox-item\" id=\"t-lbc-o-1\" role=\"option\" aria-selected=\"false\" data-idx=\"1\" data-value=\"b\"><span class=\"ah-listbox-checkbox\"></span><span class=\"ah-listbox-label\">b</span></li><li class=\"ah-listbox-group\" role=\"presentation\">g2</li><li class=\"ah-listbox-item\" id=\"t-lbc-o-2\" role=\"option\" aria-selected=\"false\" data-idx=\"2\" data-value=\"c\"><span class=\"ah-listbox-checkbox\"></span><span class=\"ah-listbox-label\">c</span></li></ul><div class=\"ah-listbox-empty\" hidden>No data</div></div></div>","lbm":"<div class=\"ah-listbox ah-listbox-multiple\" id=\"t-lbm\" data-ah=\"listbox\" data-ah-value=\"b\" tabindex=\"0\" role=\"listbox\" aria-multiselectable=\"true\"><div class=\"ah-listbox-content\"><ul class=\"ah-listbox-list\" id=\"t-lbm-list\" role=\"none\"><li class=\"ah-listbox-item\" id=\"t-lbm-o-0\" role=\"option\" aria-selected=\"false\" data-idx=\"0\" data-value=\"a\"><span class=\"ah-listbox-label\">a</span></li><li class=\"ah-listbox-item ah-listbox-item-selected\" id=\"t-lbm-o-1\" role=\"option\" aria-selected=\"true\" data-idx=\"1\" data-value=\"b\"><span class=\"ah-listbox-label\">b</span></li><li class=\"ah-listbox-item\" id=\"t-lbm-o-2\" role=\"option\" aria-selected=\"false\" data-idx=\"2\" data-value=\"c\"><span class=\"ah-listbox-label\">c</span></li><li class=\"ah-listbox-item\" id=\"t-lbm-o-3\" role=\"option\" aria-selected=\"false\" data-idx=\"3\" data-value=\"d\"><span class=\"ah-listbox-label\">d</span></li></ul><div class=\"ah-listbox-empty\" hidden>No data</div></div></div>","lr":"<div class=\"ah-listbox ah-listbox-remote\" id=\"t-lr\" data-ah=\"listbox\" data-ah-value=\"\" tabindex=\"0\" role=\"listbox\" aria-multiselectable=\"false\"><div class=\"ah-listbox-filter\"><input class=\"ah-listbox-filter-input\" type=\"text\" id=\"t-lr-filter\" autocomplete=\"off\" placeholder=\"Search\" aria-label=\"Search\" aria-controls=\"t-lr\" data-listbox=\"t-lr\" data-ah-on=\"input:g2gDdxdhaWh0bWxfZm9ybV9saXN0c190ZXN0c3cGc2VhcmNodAAAAAA.s2cFGMKiw8kOQuCcn80ERuS6M2JfyXxd78-EArdXsqw:250\"></div><div class=\"ah-listbox-content\"><ul class=\"ah-listbox-list\" id=\"t-lr-list\" role=\"none\"></ul><div class=\"ah-listbox-empty\">No data</div></div></div>","lrops":[{"id":"t-lr-list","op":"html","html":"<li class=\"ah-listbox-item\" id=\"t-lr-o-0\" role=\"option\" aria-selected=\"false\" data-idx=\"0\" data-value=\"Pear\"><span class=\"ah-listbox-label\">Pear</span></li><li class=\"ah-listbox-item\" id=\"t-lr-o-1\" role=\"option\" aria-selected=\"false\" data-idx=\"1\" data-value=\"Plum\"><span class=\"ah-listbox-label\">Plum</span></li>","swap":"morph_inner"},{"args":[],"id":"t-lr","op":"call","method":"itemsLoaded"}],"tr":"<div class=\"ah-transfer\" id=\"t-tr\" data-ah=\"transfer\" data-ah-value=\"c\"><div class=\"ah-transfer-panels\"><div class=\"ah-transfer-panel ah-transfer-panel-source\"><div class=\"ah-transfer-panel-header\"><span class=\"ah-transfer-panel-title\" id=\"t-tr-source-title\">Source</span><span class=\"ah-transfer-panel-count\">4</span></div><div class=\"ah-transfer-filter\"><input class=\"ah-transfer-filter-input\" type=\"text\" autocomplete=\"off\" placeholder=\"Search\" aria-label=\"Search\" data-panel=\"source\"></div><div class=\"ah-transfer-panel-content\"><ul class=\"ah-transfer-list\" id=\"t-tr-source\" data-panel=\"source\" role=\"listbox\" tabindex=\"0\" aria-multiselectable=\"true\" aria-labelledby=\"t-tr-source-title\" data-empty-text=\"No data\"><li class=\"ah-transfer-item\" id=\"t-tr-i-0\" role=\"option\" aria-selected=\"false\" data-value=\"a\" data-idx=\"0\" data-source=\"source\"><span class=\"ah-transfer-item-label\">a</span></li><li class=\"ah-transfer-item\" id=\"t-tr-i-1\" role=\"option\" aria-selected=\"false\" data-value=\"b\" data-idx=\"1\" data-source=\"source\"><span class=\"ah-transfer-item-label\">b</span></li><li class=\"ah-transfer-item ah-transfer-item-disabled\" id=\"t-tr-i-3\" role=\"option\" aria-selected=\"false\" aria-disabled=\"true\" data-value=\"d\" data-idx=\"3\" data-source=\"source\"><span class=\"ah-transfer-item-label\">d</span></li><li class=\"ah-transfer-item\" id=\"t-tr-i-4\" role=\"option\" aria-selected=\"false\" data-value=\"e\" data-idx=\"4\" data-source=\"source\"><span class=\"ah-transfer-item-label\">e</span></li></ul></div></div><div class=\"ah-transfer-buttons\"><button class=\"ah-transfer-btn ah-transfer-btn-to-target ah-transfer-btn-disabled\" type=\"button\" data-direction=\"to-target\" title=\"Move to target\" aria-label=\"Move to target\" disabled>âº</button><button class=\"ah-transfer-btn ah-transfer-btn-to-source ah-transfer-btn-disabled\" type=\"button\" data-direction=\"to-source\" title=\"Move to source\" aria-label=\"Move to source\" disabled>âº</button></div><div class=\"ah-transfer-panel ah-transfer-panel-target\"><div class=\"ah-transfer-panel-header\"><span class=\"ah-transfer-panel-title\" id=\"t-tr-target-title\">Target</span><span class=\"ah-transfer-panel-count\">1</span></div><div class=\"ah-transfer-filter\"><input class=\"ah-transfer-filter-input\" type=\"text\" autocomplete=\"off\" placeholder=\"Search\" aria-label=\"Search\" data-panel=\"target\"></div><div class=\"ah-transfer-panel-content\"><ul class=\"ah-transfer-list\" id=\"t-tr-target\" data-panel=\"target\" role=\"listbox\" tabindex=\"0\" aria-multiselectable=\"true\" aria-labelledby=\"t-tr-target-title\" data-empty-text=\"No data\"><li class=\"ah-transfer-item\" id=\"t-tr-i-2\" role=\"option\" aria-selected=\"false\" data-value=\"c\" data-idx=\"2\" data-source=\"target\"><span class=\"ah-transfer-item-label\">c</span></li></ul></div></div></div><input type=\"hidden\" name=\"k\" value=\"c\"></div>"};

  function mount(fx, html) { fx.innerHTML = html; AH.mount(fx); return fx.firstChild; }
  function key(el, k, extra) { $(el).trigger($.Event("keydown", $.extend({ key: k }, extra || {}))); }
  function changes(el) {
    var seen = [];
    $(el).on("change", function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function visibleCols(el) {
    return $(el).find(".ah-cascader-menu-column:not([hidden])").map(function () {
      return this.getAttribute("data-parent"); }).get();
  }
  function cursor(el) { return $(el).find(".ah-cascader-menu-item-focused").attr("data-value"); }

  T.test("cascader: open on the value path, keyboard, pick a leaf, clear", function (fx) {
    var el = mount(fx, FX.cs), $in = $(el).find(".ah-cascader-input"), seen = changes(el);
    key($in, "ArrowDown");
    T.ok($(el).hasClass("ah-cascader-open"), "opens");
    T.eq(visibleCols(el), ["", "zj", "zj,hz"]);
    T.eq(cursor(el), "bj");
    T.eq($in.attr("aria-activedescendant"), $(el).find(".ah-cascader-menu-item-focused").attr("id"));
    key($in, "ArrowLeft");
    T.eq(cursor(el), "hz");
    key($in, "ArrowDown");                   // skips the disabled Ningbo, wraps
    T.eq(cursor(el), "hz");
    T.eq(visibleCols(el), ["", "zj"], "moving closes the columns to the right");
    key($in, "ArrowRight");
    T.eq(visibleCols(el), ["", "zj", "zj,hz"]);
    T.eq(cursor(el), "xihu");
    key($in, "Enter");
    T.eq(el.getAttribute("data-ah-value"), "zj,hz,xihu");
    T.eq($(el).find("input[type=hidden]").val(), "zj,hz,xihu");
    T.eq($in.val(), "Zhejiang / Hangzhou / xihu");
    T.ok(!$(el).hasClass("ah-cascader-open"), "closes");
    $(el).find(".ah-cascader-clear").trigger("click");
    T.eq($in.val(), "");
    T.ok($(el).find(".ah-cascader-clear").prop("hidden"), "clear button hidden");
    T.eq(seen, ["zj,hz,xihu", ""]);
  });

  T.test("cascader: click a branch then a leaf, setValue, labels", function (fx) {
    var el = mount(fx, FX.cs), seen = changes(el);
    AH.invoke(el, "open");
    $(el).find('li[data-value="hk"]').trigger("click");
    T.eq(seen, ["hk"]);
    T.eq(AH.invoke(el, "getLabels"), ["Hong Kong"]);
    AH.invoke(el, "setValue", ["zj", "hz", "bj"]);
    T.eq($(el).find(".ah-cascader-input").val(), "Zhejiang / Hangzhou / Binjiang");
    T.eq(seen, ["hk"], "setValue fires no change");
  });

  T.test("cascader: lazy level loaded from the server's column", function (fx) {
    var el = mount(fx, FX.cs), seen = changes(el), loads = [];
    $(el).find(".ah-cascader-loader").on("ah:load", function (e) {
      loads.push(this.getAttribute("data-ah-value"));
      e.stopPropagation();                  // no server here
    });
    AH.invoke(el, "open");
    $(el).find('li[data-value="js"]').trigger("click");
    T.eq(loads, ["js"]);
    T.eq($(el).find(".ah-cascader-loading").length, 1, "loading message");
    AH.apply(FX.csops);                      // what cascader_children/3 sends
    T.eq($(el).find(".ah-cascader-loading").length, 0);
    T.eq(visibleCols(el), ["", "js"]);
    T.ok(!$(el).find('li[data-value="js"]')[0].hasAttribute("data-lazy"), "loaded");
    $(el).find('li[data-value="sz"]').trigger("click");
    T.eq(seen, ["js,sz"]);
    T.eq($(el).find(".ah-cascader-input").val(), "Jiangsu / Suzhou");
    // a second visit uses the loaded column
    AH.invoke(el, "open");
    $(el).find('li[data-value="js"]').trigger("click");
    T.eq(loads, ["js"]);
  });

  T.test("cascader: empty lazy answer makes a leaf", function (fx) {
    var el = mount(fx, FX.cs), seen = changes(el);
    $(el).find(".ah-cascader-loader").on("ah:load", function (e) { e.stopPropagation(); });
    AH.invoke(el, "open");
    $(el).find('li[data-value="js"]').trigger("click");
    AH.invoke(el, "childrenLoaded", "js");
    T.ok(!$(el).find('li[data-value="js"]').hasClass("has-children"), "leaf");
    T.eq(seen, ["js"]);
  });

  T.test("cascader: search paths, change_on_select", function (fx) {
    var el = mount(fx, FX.csf), $in = $(el).find(".ah-cascader-input"), seen = changes(el);
    $in[0].focus();
    $in.val("bin").trigger("input");
    T.eq($(el).find(".ah-cascader-search-item:visible .ah-cascader-search-item-label").map(function () {
      return this.innerHTML; }).get(), ["Zhejiang / Hangzhou / <b>Bin</b>jiang"]);
    key($in, "ArrowDown"); key($in, "Enter");
    T.eq(seen, ["zj,hz,bj"]);
    T.eq($in.val(), "Zhejiang / Hangzhou / Binjiang");
    $in.val("zzz").trigger("input");
    T.eq($(el).find(".ah-cascader-empty").text(), "No results found");
    key($in, "Escape");
    AH.invoke(el, "close");
    AH.invoke(el, "open");
    $(el).find('li[data-value="zj"]').trigger("click");
    T.eq(seen, ["zj,hz,bj", "zj"], "a branch is a value with change_on_select");
    T.ok($(el).hasClass("ah-cascader-open"), "stays open");
  });

  T.test("listbox: single, keyboard, type-ahead, filter", function (fx) {
    var el = mount(fx, FX.lb), seen = changes(el);
    $(el).trigger("focus");
    el.focus();
    key(el, "ArrowDown");
    T.eq(el.getAttribute("data-ah-value"), "Cherry");
    key(el, "ArrowDown");                    // Date is disabled
    T.eq(el.getAttribute("data-ah-value"), "Elder");
    key(el, "Home");
    T.eq(el.getAttribute("aria-activedescendant"), "t-lb-o-0");
    key(el, "c");
    T.eq(el.getAttribute("data-ah-value"), "Cherry");
    $(el).find('.ah-listbox-item[data-value="Banana"]').trigger("click");
    T.eq($(el).find("input[type=hidden]").val(), "Banana");
    T.eq(seen, ["Cherry", "Elder", "Apple", "Cherry", "Banana"]);
    $(el).find(".ah-listbox-filter-input").val("an").trigger("input");
    T.eq($(el).find(".ah-listbox-item:visible").length, 1);
    $(el).find(".ah-listbox-filter-input").val("zz").trigger("input");
    T.ok(!$(el).find(".ah-listbox-empty").prop("hidden"), "empty message");
  });

  T.test("listbox: multiple with Ctrl, Shift, Space and Ctrl+A", function (fx) {
    var el = mount(fx, FX.lbm);
    var li = function (v) { return $(el).find('.ah-listbox-item[data-value="' + v + '"]'); };
    li("a").trigger($.Event("click", { ctrlKey: true }));
    T.eq(el.getAttribute("data-ah-value"), "b,a");
    li("d").trigger($.Event("click", { shiftKey: true }));
    T.eq(el.getAttribute("data-ah-value"), "b,a,c,d");
    li("c").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "c");
    key(el, "ArrowDown", { shiftKey: true });
    T.eq(el.getAttribute("data-ah-value"), "c,d");
    key(el, "ArrowUp", { ctrlKey: true }); key(el, "ArrowUp", { ctrlKey: true });
    key(el, " ");
    T.eq(el.getAttribute("data-ah-value"), "c,d,b");
    key(el, "a", { ctrlKey: true });
    T.eq(el.getAttribute("data-ah-value"), "a,b,c,d");
    T.eq($(el).find(".ah-listbox-item-selected").length, 4);
  });

  T.test("listbox: check boxes, check-all over visible rows, groups", function (fx) {
    var el = mount(fx, FX.lbc);
    $(el).find('.ah-listbox-item[data-value="b"]').trigger("click");
    T.ok($(el).find(".ah-listbox-check-all .ah-listbox-checkbox").hasClass("ah-listbox-checkbox-indeterminate"));
    $(el).find(".ah-listbox-filter-input").val("c").trigger("input");
    T.eq($(el).find(".ah-listbox-group:visible").text(), "g2", "empty group hidden");
    $(el).find(".ah-listbox-check-all").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "b,c");
    AH.invoke(el, "filter", "");
    $(el).find(".ah-listbox-check-all").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "a,b,c");
    T.ok($(el).find(".ah-listbox-check-all .ah-listbox-checkbox").hasClass("ah-listbox-checkbox-checked"));
    $(el).find(".ah-listbox-check-all").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "");
  });

  T.test("listbox: server search rows morphed in", function (fx) {
    var el = mount(fx, FX.lr);
    T.ok(!$(el).find(".ah-listbox-empty").prop("hidden"), "empty at first");
    AH.invoke(el, "setValue", "Plum");
    AH.apply(FX.lrops);                      // what listbox_items/3 sends
    T.ok($(el).find(".ah-listbox-empty").prop("hidden"));
    T.eq($(el).find(".ah-listbox-item-selected").attr("data-value"), "Plum");
  });

  T.test("transfer: select, move with buttons, Enter, double click, setValue", function (fx) {
    var el = mount(fx, FX.tr), seen = changes(el);
    var li = function (v) { return $(el).find('.ah-transfer-item[data-value="' + v + '"]'); };
    var side = function (s) { return $(el).find('.ah-transfer-list[data-panel="' + s + '"] .ah-transfer-item').map(function () {
      return this.getAttribute("data-value"); }).get(); };
    T.ok($(el).find(".ah-transfer-btn-to-target").prop("disabled"), "nothing selected");
    li("e").trigger("click"); li("a").trigger("click");
    T.ok(!$(el).find(".ah-transfer-btn-to-target").prop("disabled"));
    $(el).find(".ah-transfer-btn-to-target").trigger("click");
    T.eq(side("target"), ["c", "a", "e"]);
    T.eq(seen, ["c,a,e"]);
    T.eq($(el).find(".ah-transfer-panel-source .ah-transfer-panel-count").text(), "2");
    li("c").trigger("dblclick");
    T.eq(side("source"), ["b", "c", "d"], "back in item order");
    var $src = $(el).find('.ah-transfer-list[data-panel="source"]');
    $src.trigger("focus");
    key($src, "ArrowDown"); key($src, "Enter");
    T.eq(side("target"), ["a", "e", "c"]);
    $(el).find(".ah-transfer-panel-target .ah-transfer-filter-input").val("e").trigger("input");
    T.eq($(el).find(".ah-transfer-panel-target .ah-transfer-item:visible").length, 1);
    AH.invoke(el, "setValue", ["b"]);
    T.eq(side("target"), ["b"]);
    T.eq(side("source"), ["a", "c", "d", "e"]);
    T.eq(seen, ["c,a,e", "a,e", "a,e,c"]);
    T.eq(el.getAttribute("data-ah-value"), "b");
  });
})(window.AHTest, window.jQuery, window.AH);
