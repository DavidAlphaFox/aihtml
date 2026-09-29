/* Transfer behaviour (transfer.js). The fixtures are server renders
 * from aihtml_transfer, captured once;
 * regenerate them if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var FX = {"tr":"<div class=\"ah-transfer\" id=\"t-tr\" data-ah=\"transfer\" data-ah-value=\"c\"><div class=\"ah-transfer-panels\"><div class=\"ah-transfer-panel ah-transfer-panel-source\"><div class=\"ah-transfer-panel-header\"><span class=\"ah-transfer-panel-title\" id=\"t-tr-source-title\">Source</span><span class=\"ah-transfer-panel-count\">4</span></div><div class=\"ah-transfer-filter\"><input class=\"ah-transfer-filter-input\" type=\"text\" autocomplete=\"off\" placeholder=\"Search\" aria-label=\"Search\" data-panel=\"source\"></div><div class=\"ah-transfer-panel-content\"><ul class=\"ah-transfer-list\" id=\"t-tr-source\" data-panel=\"source\" role=\"listbox\" tabindex=\"0\" aria-multiselectable=\"true\" aria-labelledby=\"t-tr-source-title\" data-empty-text=\"No data\"><li class=\"ah-transfer-item\" id=\"t-tr-i-0\" role=\"option\" aria-selected=\"false\" data-value=\"a\" data-idx=\"0\" data-source=\"source\"><span class=\"ah-transfer-item-label\">a</span></li><li class=\"ah-transfer-item\" id=\"t-tr-i-1\" role=\"option\" aria-selected=\"false\" data-value=\"b\" data-idx=\"1\" data-source=\"source\"><span class=\"ah-transfer-item-label\">b</span></li><li class=\"ah-transfer-item ah-transfer-item-disabled\" id=\"t-tr-i-3\" role=\"option\" aria-selected=\"false\" aria-disabled=\"true\" data-value=\"d\" data-idx=\"3\" data-source=\"source\"><span class=\"ah-transfer-item-label\">d</span></li><li class=\"ah-transfer-item\" id=\"t-tr-i-4\" role=\"option\" aria-selected=\"false\" data-value=\"e\" data-idx=\"4\" data-source=\"source\"><span class=\"ah-transfer-item-label\">e</span></li></ul></div></div><div class=\"ah-transfer-buttons\"><button class=\"ah-transfer-btn ah-transfer-btn-to-target ah-transfer-btn-disabled\" type=\"button\" data-direction=\"to-target\" title=\"Move to target\" aria-label=\"Move to target\" disabled>âº</button><button class=\"ah-transfer-btn ah-transfer-btn-to-source ah-transfer-btn-disabled\" type=\"button\" data-direction=\"to-source\" title=\"Move to source\" aria-label=\"Move to source\" disabled>âº</button></div><div class=\"ah-transfer-panel ah-transfer-panel-target\"><div class=\"ah-transfer-panel-header\"><span class=\"ah-transfer-panel-title\" id=\"t-tr-target-title\">Target</span><span class=\"ah-transfer-panel-count\">1</span></div><div class=\"ah-transfer-filter\"><input class=\"ah-transfer-filter-input\" type=\"text\" autocomplete=\"off\" placeholder=\"Search\" aria-label=\"Search\" data-panel=\"target\"></div><div class=\"ah-transfer-panel-content\"><ul class=\"ah-transfer-list\" id=\"t-tr-target\" data-panel=\"target\" role=\"listbox\" tabindex=\"0\" aria-multiselectable=\"true\" aria-labelledby=\"t-tr-target-title\" data-empty-text=\"No data\"><li class=\"ah-transfer-item\" id=\"t-tr-i-2\" role=\"option\" aria-selected=\"false\" data-value=\"c\" data-idx=\"2\" data-source=\"target\"><span class=\"ah-transfer-item-label\">c</span></li></ul></div></div></div><input type=\"hidden\" name=\"k\" value=\"c\"></div>"};

  function mount(fx, html) { fx.innerHTML = html; AH.mount(fx); return fx.firstChild; }
  function key(el, k, extra) { $(el).trigger($.Event("keydown", $.extend({ key: k }, extra || {}))); }
  function changes(el) {
    var seen = [];
    $(el).on("change", function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }

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
