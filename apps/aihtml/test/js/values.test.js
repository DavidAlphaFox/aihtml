/* AH.lib.values (_lib_values.js): the same cases as aihtml_value_tests. */
(function (T, $, AH) {
  "use strict";

  var V = function () { return AH.lib.values; };

  T.test("values: plain values join and split as before", function () {
    T.eq(V().join(["a", "b", "c"]), "a,b,c");
    T.eq(V().join([1, 2.5, "x"]), "1,2.5,x");
    T.eq(V().join([]), "");
    T.eq(V().split("a,b,c"), ["a", "b", "c"]);
    T.eq(V().split(""), []);
    T.eq(V().split(null), []);
    T.eq(V().split(undefined), []);
    T.eq(V().split("a,,b"), ["a", "", "b"]);
    T.eq(V().split(","), ["", ""]);
    T.eq(V().split(["a", 1]), ["a", "1"], "arrays pass through as strings");
  });

  T.test("values: commas and backslashes are escaped", function () {
    T.eq(V().join(["a,b", "c"]), "a\\,b,c");
    T.eq(V().join(["x\\y"]), "x\\\\y");
    T.eq(V().join(["\\,"]), "\\\\\\,");
    T.eq(V().split("a\\,b,c"), ["a,b", "c"]);
    T.eq(V().split("x\\\\y"), ["x\\y"]);
    T.eq(V().split("a\\b"), ["ab"]);
    T.eq(V().split("a\\"), ["a\\"]);
    T.eq(V().join(["北京,上海", "東京"]), "北京\\,上海,東京");
  });

  T.test("values: round trips", function () {
    [["a"], ["a,b", "c"], ["1,000", "2,000,000"], ["\\", ",", "\\,", ",\\"],
     ["a", "", "b"], ["", "x"], ["x", ""], ["", ""],
     ["北京,上海", "東京", "é,ü\\ñ"], ["emoji 😀,🎉"], [" spaced , value ", "trailing\\"]
    ].forEach(function (c) { T.eq(V().split(V().join(c)), c); });
  });

  // Components with a value containing a comma. The fixtures are server
  // renders (aihtml_listbox, aihtml_transfer, ...), captured once;
  // regenerate them if the markup changes.
  var FX = {"lb": "<div class=\"ah-listbox ah-listbox-multiple\" id=\"t-lbc\" data-ah=\"listbox\" data-ah-value=\"a\\,b\" tabindex=\"0\" role=\"listbox\" aria-multiselectable=\"true\"><div class=\"ah-listbox-content\"><ul class=\"ah-listbox-list\" id=\"t-lbc-list\" role=\"none\"><li class=\"ah-listbox-item ah-listbox-item-selected\" id=\"t-lbc-o-0\" role=\"option\" aria-selected=\"true\" data-idx=\"0\" data-value=\"a,b\"><span class=\"ah-listbox-label\">a,b</span></li><li class=\"ah-listbox-item\" id=\"t-lbc-o-1\" role=\"option\" aria-selected=\"false\" data-idx=\"1\" data-value=\"c\"><span class=\"ah-listbox-label\">c</span></li><li class=\"ah-listbox-item\" id=\"t-lbc-o-2\" role=\"option\" aria-selected=\"false\" data-idx=\"2\" data-value=\"d\\e\"><span class=\"ah-listbox-label\">d\\e</span></li></ul><div class=\"ah-listbox-empty\" hidden>No data</div></div><input type=\"hidden\" name=\"k\" value=\"a\\,b\"></div>", "tr": "<div class=\"ah-transfer\" id=\"t-trc\" data-ah=\"transfer\" data-ah-value=\"2\\,000\"><div class=\"ah-transfer-panels\"><div class=\"ah-transfer-panel ah-transfer-panel-source\"><div class=\"ah-transfer-panel-header\"><span class=\"ah-transfer-panel-title\" id=\"t-trc-source-title\">Source</span><span class=\"ah-transfer-panel-count\">2</span></div><div class=\"ah-transfer-filter\"><input class=\"ah-transfer-filter-input\" type=\"text\" autocomplete=\"off\" placeholder=\"Search\" aria-label=\"Search\" data-panel=\"source\"></div><div class=\"ah-transfer-panel-content\"><ul class=\"ah-transfer-list\" id=\"t-trc-source\" data-panel=\"source\" role=\"listbox\" tabindex=\"0\" aria-multiselectable=\"true\" aria-labelledby=\"t-trc-source-title\" data-empty-text=\"No data\"><li class=\"ah-transfer-item\" id=\"t-trc-i-0\" role=\"option\" aria-selected=\"false\" data-value=\"1,000\" data-idx=\"0\" data-source=\"source\"><span class=\"ah-transfer-item-label\">1,000</span></li><li class=\"ah-transfer-item\" id=\"t-trc-i-2\" role=\"option\" aria-selected=\"false\" data-value=\"x\" data-idx=\"2\" data-source=\"source\"><span class=\"ah-transfer-item-label\">x</span></li></ul></div></div><div class=\"ah-transfer-buttons\"><button class=\"ah-transfer-btn ah-transfer-btn-to-target ah-transfer-btn-disabled\" type=\"button\" data-direction=\"to-target\" title=\"Move to target\" aria-label=\"Move to target\" disabled>âº</button><button class=\"ah-transfer-btn ah-transfer-btn-to-source ah-transfer-btn-disabled\" type=\"button\" data-direction=\"to-source\" title=\"Move to source\" aria-label=\"Move to source\" disabled>âº</button></div><div class=\"ah-transfer-panel ah-transfer-panel-target\"><div class=\"ah-transfer-panel-header\"><span class=\"ah-transfer-panel-title\" id=\"t-trc-target-title\">Target</span><span class=\"ah-transfer-panel-count\">1</span></div><div class=\"ah-transfer-filter\"><input class=\"ah-transfer-filter-input\" type=\"text\" autocomplete=\"off\" placeholder=\"Search\" aria-label=\"Search\" data-panel=\"target\"></div><div class=\"ah-transfer-panel-content\"><ul class=\"ah-transfer-list\" id=\"t-trc-target\" data-panel=\"target\" role=\"listbox\" tabindex=\"0\" aria-multiselectable=\"true\" aria-labelledby=\"t-trc-target-title\" data-empty-text=\"No data\"><li class=\"ah-transfer-item\" id=\"t-trc-i-1\" role=\"option\" aria-selected=\"false\" data-value=\"2,000\" data-idx=\"1\" data-source=\"target\"><span class=\"ah-transfer-item-label\">2,000</span></li></ul></div></div></div><input type=\"hidden\" name=\"k\" value=\"2\\,000\"></div>", "cb": "<div class=\"ah-combobox ah-combobox-multiple\" id=\"t-cbc\" data-ah=\"combobox\" data-ah-value=\"a\\,b\" data-ah-search-mode=\"contains_ignore_case\" data-ah-placeholder=\"\"><div class=\"ah-combobox-input-area\"><div class=\"ah-combobox-tags\"><span class=\"ah-combobox-tag\"><span class=\"ah-combobox-tag-text\">a,b</span><span class=\"ah-combobox-tag-close\" data-value=\"a,b\" role=\"button\" aria-label=\"Remove a,b\">&times;</span></span><input class=\"ah-combobox-input\" type=\"text\" id=\"t-cbc-input\" autocomplete=\"off\" spellcheck=\"false\" value=\"\" role=\"combobox\" aria-autocomplete=\"list\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-cbc-list\" data-combobox=\"t-cbc\"></div><span class=\"ah-combobox-arrow\" aria-hidden=\"true\"><span class=\"ah-combobox-arrow-icon\">â¼</span></span></div><input type=\"hidden\" name=\"k\" value=\"a\\,b\"><div class=\"ah-combobox-popup\" style=\"\"><ul class=\"ah-combobox-list\" id=\"t-cbc-list\" role=\"listbox\" aria-multiselectable=\"true\"><li class=\"ah-combobox-item ah-combobox-item-selected\" id=\"t-cbc-opt-0\" role=\"option\" aria-selected=\"true\" data-index=\"0\" data-value=\"a,b\" data-label=\"a,b\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">a,b</div></div></li><li class=\"ah-combobox-item\" id=\"t-cbc-opt-1\" role=\"option\" aria-selected=\"false\" data-index=\"1\" data-value=\"c\" data-label=\"c\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">c</div></div></li></ul></div></div>", "cbs": "<div class=\"ah-combobox\" id=\"t-cbs\" data-ah=\"combobox\" data-ah-value=\"a,b\" data-ah-search-mode=\"contains_ignore_case\"><div class=\"ah-combobox-input-area\"><input class=\"ah-combobox-input\" type=\"text\" id=\"t-cbs-input\" autocomplete=\"off\" spellcheck=\"false\" placeholder=\"\" value=\"a,b\" role=\"combobox\" aria-autocomplete=\"list\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-cbs-list\" data-combobox=\"t-cbs\"><span class=\"ah-combobox-arrow\" aria-hidden=\"true\"><span class=\"ah-combobox-arrow-icon\">â¼</span></span></div><div class=\"ah-combobox-popup\" style=\"\"><ul class=\"ah-combobox-list\" id=\"t-cbs-list\" role=\"listbox\"><li class=\"ah-combobox-item ah-combobox-item-selected\" id=\"t-cbs-opt-0\" role=\"option\" aria-selected=\"true\" data-index=\"0\" data-value=\"a,b\" data-label=\"a,b\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">a,b</div></div></li><li class=\"ah-combobox-item\" id=\"t-cbs-opt-1\" role=\"option\" aria-selected=\"false\" data-index=\"1\" data-value=\"c\" data-label=\"c\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">c</div></div></li></ul></div></div>", "so": "<div class=\"ah-sortable ah-sortable-vertical\" role=\"list\" data-ah=\"sortable\" data-ah-value=\"a\\,b,c,d\\\\e\" id=\"t-soc\"><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"a,b\" tabindex=\"0\" aria-roledescription=\"sortable item\">AB</div><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"c\" tabindex=\"-1\" aria-roledescription=\"sortable item\">C</div><div class=\"ah-sortable-item\" role=\"listitem\" data-value=\"d\\e\" tabindex=\"-1\" aria-roledescription=\"sortable item\">DE</div><span class=\"ah-sortable-live\" aria-live=\"assertive\" aria-atomic=\"true\"></span></div>", "cg": "<div class=\"ah-checkbox-group ah-checkbox-group-vertical\" data-ah=\"checkbox-group\" role=\"group\" data-ah-value=\"a\\,b\" data-label-position=\"after\" id=\"t-cgc\"><label class=\"ah-checkbox-group-item\" data-value=\"a,b\" data-index=\"0\"><span class=\"ah-checkbox ah-checkbox-checked\"><input class=\"ah-choice-input\" type=\"checkbox\" value=\"a,b\" checked><span class=\"ah-checkbox-box\" aria-hidden=\"true\"><span class=\"ah-checkbox-check ah-checkbox-check-checked\"></span></span></span><span class=\"ah-checkbox-group-label\">AB</span></label><label class=\"ah-checkbox-group-item\" data-value=\"c\" data-index=\"1\"><span class=\"ah-checkbox\"><input class=\"ah-choice-input\" type=\"checkbox\" value=\"c\"><span class=\"ah-checkbox-box\" aria-hidden=\"true\"><span class=\"ah-checkbox-check\"></span></span></span><span class=\"ah-checkbox-group-label\">C</span></label></div>", "ti": "<div class=\"ah-tag-input\" data-ah=\"tag-input\" role=\"group\" data-ah-value=\"1\\,000\" data-disabled=\"false\" data-chip-color=\"primary\" data-chip-variant=\"soft\" id=\"t-tic\"><span class=\"ah-chip ah-tag-input__chip\" data-variant=\"soft\" data-color=\"primary\" data-size=\"small\" data-index=\"0\"><span class=\"ah-chip__label\">1,000</span><button type=\"button\" class=\"ah-chip__delete\" tabindex=\"-1\" aria-label=\"Remove 1,000\">&times;</button></span><input class=\"ah-tag-input__field\" type=\"text\" placeholder=\"Add tagâ¦\" aria-label=\"Add tag\"></div>"};

  function mount(fx, html) { fx.innerHTML = html; AH.mount(fx); return fx.firstChild; }
  function hidden(el) { return $(el).children("input[type=hidden]").val(); }
  function changes(el) {
    var seen = [];
    $(el).on("change", function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }

  T.test("values: listbox multiple with a comma in a value", function (fx) {
    var el = mount(fx, FX.lb), seen = changes(el);
    T.eq(AH.lib.values.split(AH.invoke(el, "getValue")), ["a,b"]);
    $(el).find('.ah-listbox-item[data-value="c"]').trigger($.Event("click", { ctrlKey: true }));
    T.eq(seen, ["a\\,b,c"]);
    T.eq($(el).find(".ah-listbox-item-selected").length, 2);
    AH.invoke(el, "setValue", "d\\\\e,a\\,b");
    T.eq($(el).find(".ah-listbox-item-selected").map(function () { return this.getAttribute("data-value"); }).get(),
         ["a,b", "d\\e"]);
    T.eq(el.getAttribute("data-ah-value"), "d\\\\e,a\\,b", "in the order given");
  });

  T.test("values: transfer keeps values with commas whole", function (fx) {
    var el = mount(fx, FX.tr), seen = changes(el);
    T.eq($(el).find('.ah-transfer-list[data-panel="target"] .ah-transfer-item').length, 1);
    $(el).find('.ah-transfer-item[data-value="1,000"]').trigger("dblclick");
    T.eq(seen, ["2\\,000,1\\,000"]);
    T.eq(hidden(el), "2\\,000,1\\,000");
    AH.invoke(el, "setValue", "x,1\\,000");
    T.eq($(el).find('.ah-transfer-list[data-panel="target"] .ah-transfer-item').map(function () {
      return this.getAttribute("data-value"); }).get(), ["x", "1,000"]);
  });

  T.test("values: combobox multiple escapes, single keeps the value as it is", function (fx) {
    var el = mount(fx, FX.cb);
    T.eq($(el).find(".ah-combobox-item-selected").attr("data-value"), "a,b");
    AH.invoke(el, "setValue", ["c", "a,b"]);
    T.eq(el.getAttribute("data-ah-value"), "c,a\\,b");
    T.eq(hidden(el), "c,a\\,b");
    fx.innerHTML = "";
    var s = mount(fx, FX.cbs);
    T.eq($(s).find(".ah-combobox-item-selected").attr("data-value"), "a,b");
    AH.invoke(s, "setValue", "a,b");
    T.eq(s.getAttribute("data-ah-value"), "a,b");
  });

  T.test("values: sortable order, checkbox group and tag input", function (fx) {
    var so = mount(fx, FX.so);
    T.eq(AH.invoke(so, "getValue"), "a\\,b,c,d\\\\e");
    AH.invoke(so, "setValue", "d\\\\e,a\\,b");
    T.eq($(so).children(".ah-sortable-item").map(function () { return this.getAttribute("data-value"); }).get(),
         ["d\\e", "a,b", "c"]);
    T.eq(so.getAttribute("data-ah-value"), "d\\\\e,a\\,b,c");
    fx.innerHTML = "";
    var cg = mount(fx, FX.cg);
    T.eq(AH.invoke(cg, "getValue"), ["a,b"]);
    AH.invoke(cg, "setValue", "c,a\\,b");
    T.eq(cg.getAttribute("data-ah-value"), "a\\,b,c");
    fx.innerHTML = "";
    var ti = mount(fx, FX.ti);
    T.eq(AH.invoke(ti, "getTags"), ["1,000"]);
    AH.invoke(ti, "setTags", ["1,000", "x\\y"]);
    T.eq(ti.getAttribute("data-ah-value"), "1\\,000,x\\\\y");
  });
})(window.AHTest, window.jQuery, window.AH);
