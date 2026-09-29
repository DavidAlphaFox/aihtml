/* Listbox behaviour (listbox.js). The fixtures are server renders
 * from aihtml_listbox (and the operations its listbox_items/3 sends), captured once;
 * regenerate them if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var FX = {"lb":"<div class=\"ah-listbox ah-listbox-filterable\" id=\"t-lb\" data-ah=\"listbox\" data-ah-value=\"Banana\" tabindex=\"0\" role=\"listbox\" aria-multiselectable=\"false\"><div class=\"ah-listbox-filter\"><input class=\"ah-listbox-filter-input\" type=\"text\" id=\"t-lb-filter\" autocomplete=\"off\" placeholder=\"Search\" aria-label=\"Search\" aria-controls=\"t-lb\" data-listbox=\"t-lb\"></div><div class=\"ah-listbox-content\"><ul class=\"ah-listbox-list\" id=\"t-lb-list\" role=\"none\"><li class=\"ah-listbox-item\" id=\"t-lb-o-0\" role=\"option\" aria-selected=\"false\" data-idx=\"0\" data-value=\"Apple\"><span class=\"ah-listbox-label\">Apple</span></li><li class=\"ah-listbox-item ah-listbox-item-selected\" id=\"t-lb-o-1\" role=\"option\" aria-selected=\"true\" data-idx=\"1\" data-value=\"Banana\"><span class=\"ah-listbox-label\">Banana</span></li><li class=\"ah-listbox-item\" id=\"t-lb-o-2\" role=\"option\" aria-selected=\"false\" data-idx=\"2\" data-value=\"Cherry\"><span class=\"ah-listbox-label\">Cherry</span></li><li class=\"ah-listbox-item ah-listbox-item-disabled\" id=\"t-lb-o-3\" role=\"option\" aria-selected=\"false\" aria-disabled=\"true\" data-idx=\"3\" data-value=\"D\"><span class=\"ah-listbox-label\">Date</span></li><li class=\"ah-listbox-item\" id=\"t-lb-o-4\" role=\"option\" aria-selected=\"false\" data-idx=\"4\" data-value=\"Elder\"><span class=\"ah-listbox-label\">Elder</span></li></ul><div class=\"ah-listbox-empty\" hidden>No data</div></div><input type=\"hidden\" name=\"f\" value=\"Banana\"></div>","lbc":"<div class=\"ah-listbox ah-listbox-checkboxes ah-listbox-filterable\" id=\"t-lbc\" data-ah=\"listbox\" data-ah-value=\"\" tabindex=\"0\" role=\"listbox\" aria-multiselectable=\"true\"><div class=\"ah-listbox-filter\"><input class=\"ah-listbox-filter-input\" type=\"text\" id=\"t-lbc-filter\" autocomplete=\"off\" placeholder=\"Search\" aria-label=\"Search\" aria-controls=\"t-lbc\" data-listbox=\"t-lbc\" data-checkboxes=\"true\"></div><div class=\"ah-listbox-check-all\" role=\"button\" aria-pressed=\"false\"><span class=\"ah-listbox-checkbox\"></span><span class=\"ah-listbox-label\">Select all</span></div><div class=\"ah-listbox-content\"><ul class=\"ah-listbox-list\" id=\"t-lbc-list\" role=\"none\"><li class=\"ah-listbox-group\" role=\"presentation\">g1</li><li class=\"ah-listbox-item\" id=\"t-lbc-o-0\" role=\"option\" aria-selected=\"false\" data-idx=\"0\" data-value=\"a\"><span class=\"ah-listbox-checkbox\"></span><span class=\"ah-listbox-label\">a</span></li><li class=\"ah-listbox-item\" id=\"t-lbc-o-1\" role=\"option\" aria-selected=\"false\" data-idx=\"1\" data-value=\"b\"><span class=\"ah-listbox-checkbox\"></span><span class=\"ah-listbox-label\">b</span></li><li class=\"ah-listbox-group\" role=\"presentation\">g2</li><li class=\"ah-listbox-item\" id=\"t-lbc-o-2\" role=\"option\" aria-selected=\"false\" data-idx=\"2\" data-value=\"c\"><span class=\"ah-listbox-checkbox\"></span><span class=\"ah-listbox-label\">c</span></li></ul><div class=\"ah-listbox-empty\" hidden>No data</div></div></div>","lbm":"<div class=\"ah-listbox ah-listbox-multiple\" id=\"t-lbm\" data-ah=\"listbox\" data-ah-value=\"b\" tabindex=\"0\" role=\"listbox\" aria-multiselectable=\"true\"><div class=\"ah-listbox-content\"><ul class=\"ah-listbox-list\" id=\"t-lbm-list\" role=\"none\"><li class=\"ah-listbox-item\" id=\"t-lbm-o-0\" role=\"option\" aria-selected=\"false\" data-idx=\"0\" data-value=\"a\"><span class=\"ah-listbox-label\">a</span></li><li class=\"ah-listbox-item ah-listbox-item-selected\" id=\"t-lbm-o-1\" role=\"option\" aria-selected=\"true\" data-idx=\"1\" data-value=\"b\"><span class=\"ah-listbox-label\">b</span></li><li class=\"ah-listbox-item\" id=\"t-lbm-o-2\" role=\"option\" aria-selected=\"false\" data-idx=\"2\" data-value=\"c\"><span class=\"ah-listbox-label\">c</span></li><li class=\"ah-listbox-item\" id=\"t-lbm-o-3\" role=\"option\" aria-selected=\"false\" data-idx=\"3\" data-value=\"d\"><span class=\"ah-listbox-label\">d</span></li></ul><div class=\"ah-listbox-empty\" hidden>No data</div></div></div>","lr":"<div class=\"ah-listbox ah-listbox-remote\" id=\"t-lr\" data-ah=\"listbox\" data-ah-value=\"\" tabindex=\"0\" role=\"listbox\" aria-multiselectable=\"false\"><div class=\"ah-listbox-filter\"><input class=\"ah-listbox-filter-input\" type=\"text\" id=\"t-lr-filter\" autocomplete=\"off\" placeholder=\"Search\" aria-label=\"Search\" aria-controls=\"t-lr\" data-listbox=\"t-lr\" data-ah-on=\"input:g2gDdxdhaWh0bWxfZm9ybV9saXN0c190ZXN0c3cGc2VhcmNodAAAAAA.s2cFGMKiw8kOQuCcn80ERuS6M2JfyXxd78-EArdXsqw:250\"></div><div class=\"ah-listbox-content\"><ul class=\"ah-listbox-list\" id=\"t-lr-list\" role=\"none\"></ul><div class=\"ah-listbox-empty\">No data</div></div></div>","lrops":[{"id":"t-lr-list","op":"html","html":"<li class=\"ah-listbox-item\" id=\"t-lr-o-0\" role=\"option\" aria-selected=\"false\" data-idx=\"0\" data-value=\"Pear\"><span class=\"ah-listbox-label\">Pear</span></li><li class=\"ah-listbox-item\" id=\"t-lr-o-1\" role=\"option\" aria-selected=\"false\" data-idx=\"1\" data-value=\"Plum\"><span class=\"ah-listbox-label\">Plum</span></li>","swap":"morph_inner"},{"args":[],"id":"t-lr","op":"call","method":"itemsLoaded"}]};

  function mount(fx, html) { fx.innerHTML = html; AH.mount(fx); return fx.firstChild; }
  function key(el, k, extra) { $(el).trigger($.Event("keydown", $.extend({ key: k }, extra || {}))); }
  function changes(el) {
    var seen = [];
    $(el).on("change", function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }

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
})(window.AHTest, window.jQuery, window.AH);
