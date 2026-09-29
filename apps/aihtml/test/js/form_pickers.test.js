/* Datepicker and combobox behaviours (form_pickers.js). The fixtures are
 * server renders from aihtml_form_pickers (and the operations its
 * set_items/3 sends), captured once; regenerate them if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var FX = {"remote":"<div class=\"ah-combobox\" id=\"t-rem\" data-ah=\"combobox\" data-ah-value=\"\" data-ah-search-mode=\"contains_ignore_case\" data-ah-remote><div class=\"ah-combobox-input-area\"><input class=\"ah-combobox-input\" type=\"text\" id=\"t-rem-input\" autocomplete=\"off\" spellcheck=\"false\" placeholder=\"\" value=\"\" role=\"combobox\" aria-autocomplete=\"list\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-rem-list\" data-combobox=\"t-rem\" data-ah-on=\"input:g2gDdxlhaWh0bWxfZm9ybV9waWNrZXJzX3Rlc3RzdwZzZWFyY2h0AAAAAA.m9FfOeHQMuFkj7YcmxekgBC4k3Kyj82xmu3gL5VesTY:250\"><span class=\"ah-combobox-arrow\" aria-hidden=\"true\"><span class=\"ah-combobox-arrow-icon\">▼</span></span></div><div class=\"ah-combobox-popup\" style=\"\"><ul class=\"ah-combobox-list\" id=\"t-rem-list\" role=\"listbox\"></ul></div></div>","ops":[{"id":"t-rem-list","op":"html","html":"<li class=\"ah-combobox-item\" id=\"t-rem-opt-0\" role=\"option\" aria-selected=\"false\" data-index=\"0\" data-value=\"Plum\" data-label=\"Plum\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">Plum</div></div></li><li class=\"ah-combobox-item\" id=\"t-rem-opt-1\" role=\"option\" aria-selected=\"false\" data-index=\"1\" data-value=\"Pear\" data-label=\"Pear\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">Pear</div></div></li>","swap":"morph_inner"},{"args":[],"id":"t-rem","op":"call","method":"itemsLoaded"}],"dp":"<div class=\"ah-datepicker\" id=\"t-dp\" data-ah=\"datepicker\" data-ah-value=\"2026-09-29\" data-ah-format=\"yyyy-MM-dd\" data-ah-min=\"2026-09-10\" data-ah-first-day=\"0\" data-ah-labels=\"{&quot;clear&quot;:&quot;Clear&quot;,&quot;months&quot;:[&quot;January&quot;,&quot;February&quot;,&quot;March&quot;,&quot;April&quot;,&quot;May&quot;,&quot;June&quot;,&quot;July&quot;,&quot;August&quot;,&quot;September&quot;,&quot;October&quot;,&quot;November&quot;,&quot;December&quot;],&quot;months_short&quot;:[&quot;Jan&quot;,&quot;Feb&quot;,&quot;Mar&quot;,&quot;Apr&quot;,&quot;May&quot;,&quot;Jun&quot;,&quot;Jul&quot;,&quot;Aug&quot;,&quot;Sep&quot;,&quot;Oct&quot;,&quot;Nov&quot;,&quot;Dec&quot;],&quot;next_month&quot;:&quot;Next month&quot;,&quot;next_year&quot;:&quot;Next year&quot;,&quot;prev_month&quot;:&quot;Previous month&quot;,&quot;prev_year&quot;:&quot;Previous year&quot;,&quot;title&quot;:&quot;MMMM yyyy&quot;,&quot;today&quot;:&quot;Today&quot;,&quot;weekdays&quot;:[&quot;Su&quot;,&quot;Mo&quot;,&quot;Tu&quot;,&quot;We&quot;,&quot;Th&quot;,&quot;Fr&quot;,&quot;Sa&quot;]}\"><div class=\"ah-datepicker-input-area\"><input class=\"ah-datepicker-input\" type=\"text\" id=\"t-dp-input\" autocomplete=\"off\" spellcheck=\"false\" readonly placeholder=\"Select date...\" value=\"2026-09-29\" role=\"combobox\" aria-haspopup=\"dialog\" aria-expanded=\"false\"><span class=\"ah-datepicker-trigger\" aria-hidden=\"true\"><span class=\"ah-datepicker-icon\">📅</span></span></div><input type=\"hidden\" name=\"d\" value=\"2026-09-29\"><div class=\"ah-datepicker-popup\" role=\"dialog\" aria-label=\"Choose date\"></div></div>","dpr":"<div class=\"ah-datepicker ah-datepicker-clearable ah-datepicker-range\" id=\"t-dpr\" data-ah=\"datepicker\" data-ah-value=\"\" data-ah-range data-ah-format=\"yyyy-MM-dd\" data-ah-first-day=\"0\" data-ah-labels=\"{&quot;clear&quot;:&quot;Clear&quot;,&quot;months&quot;:[&quot;January&quot;,&quot;February&quot;,&quot;March&quot;,&quot;April&quot;,&quot;May&quot;,&quot;June&quot;,&quot;July&quot;,&quot;August&quot;,&quot;September&quot;,&quot;October&quot;,&quot;November&quot;,&quot;December&quot;],&quot;months_short&quot;:[&quot;Jan&quot;,&quot;Feb&quot;,&quot;Mar&quot;,&quot;Apr&quot;,&quot;May&quot;,&quot;Jun&quot;,&quot;Jul&quot;,&quot;Aug&quot;,&quot;Sep&quot;,&quot;Oct&quot;,&quot;Nov&quot;,&quot;Dec&quot;],&quot;next_month&quot;:&quot;Next month&quot;,&quot;next_year&quot;:&quot;Next year&quot;,&quot;prev_month&quot;:&quot;Previous month&quot;,&quot;prev_year&quot;:&quot;Previous year&quot;,&quot;title&quot;:&quot;MMMM yyyy&quot;,&quot;today&quot;:&quot;Today&quot;,&quot;weekdays&quot;:[&quot;Su&quot;,&quot;Mo&quot;,&quot;Tu&quot;,&quot;We&quot;,&quot;Th&quot;,&quot;Fr&quot;,&quot;Sa&quot;]}\"><div class=\"ah-datepicker-input-area\"><input class=\"ah-datepicker-input\" type=\"text\" id=\"t-dpr-input\" autocomplete=\"off\" spellcheck=\"false\" readonly placeholder=\"Select date...\" value=\"\" role=\"combobox\" aria-haspopup=\"dialog\" aria-expanded=\"false\"><button class=\"ah-datepicker-clear\" type=\"button\" tabindex=\"-1\" aria-label=\"Clear\">&times;</button><span class=\"ah-datepicker-trigger\" aria-hidden=\"true\"><span class=\"ah-datepicker-icon\">📅</span></span></div><div class=\"ah-datepicker-popup\" role=\"dialog\" aria-label=\"Choose date\"></div></div>","cb":"<div class=\"ah-combobox\" id=\"t-cb\" data-ah=\"combobox\" data-ah-value=\"Banana\" data-ah-search-mode=\"contains_ignore_case\"><div class=\"ah-combobox-input-area\"><input class=\"ah-combobox-input\" type=\"text\" id=\"t-cb-input\" autocomplete=\"off\" spellcheck=\"false\" placeholder=\"\" value=\"Banana\" role=\"combobox\" aria-autocomplete=\"list\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-cb-list\" data-combobox=\"t-cb\"><span class=\"ah-combobox-arrow\" aria-hidden=\"true\"><span class=\"ah-combobox-arrow-icon\">▼</span></span></div><input type=\"hidden\" name=\"f\" value=\"Banana\"><div class=\"ah-combobox-popup\" style=\"\"><ul class=\"ah-combobox-list\" id=\"t-cb-list\" role=\"listbox\"><li class=\"ah-combobox-item\" id=\"t-cb-opt-0\" role=\"option\" aria-selected=\"false\" data-index=\"0\" data-value=\"Apple\" data-label=\"Apple\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">Apple</div></div></li><li class=\"ah-combobox-item\" id=\"t-cb-opt-1\" role=\"option\" aria-selected=\"false\" data-index=\"1\" data-value=\"Apricot\" data-label=\"Apricot\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">Apricot</div></div></li><li class=\"ah-combobox-item ah-combobox-item-selected\" id=\"t-cb-opt-2\" role=\"option\" aria-selected=\"true\" data-index=\"2\" data-value=\"Banana\" data-label=\"Banana\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">Banana</div></div></li><li class=\"ah-combobox-group-header\" role=\"presentation\">G</li><li class=\"ah-combobox-item\" id=\"t-cb-opt-3\" role=\"option\" aria-selected=\"false\" data-index=\"3\" data-value=\"g\" data-label=\"Grape\" data-group=\"G\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">Grape</div></div></li></ul></div></div>","cbm":"<div class=\"ah-combobox ah-combobox-checkboxes\" id=\"t-cbm\" data-ah=\"combobox\" data-ah-value=\"a\" data-ah-search-mode=\"contains_ignore_case\" data-ah-placeholder=\"\"><div class=\"ah-combobox-input-area\"><div class=\"ah-combobox-tags\"><span class=\"ah-combobox-tag\"><span class=\"ah-combobox-tag-text\">a</span><span class=\"ah-combobox-tag-close\" data-value=\"a\" role=\"button\" aria-label=\"Remove a\">&times;</span></span><input class=\"ah-combobox-input\" type=\"text\" id=\"t-cbm-input\" autocomplete=\"off\" spellcheck=\"false\" value=\"\" role=\"combobox\" aria-autocomplete=\"list\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-cbm-list\" data-combobox=\"t-cbm\" data-checkboxes=\"true\"></div><span class=\"ah-combobox-arrow\" aria-hidden=\"true\"><span class=\"ah-combobox-arrow-icon\">▼</span></span></div><div class=\"ah-combobox-popup\" style=\"\"><ul class=\"ah-combobox-list\" id=\"t-cbm-list\" role=\"listbox\" aria-multiselectable=\"true\"><li class=\"ah-combobox-item ah-combobox-item-selected\" id=\"t-cbm-opt-0\" role=\"option\" aria-selected=\"true\" data-index=\"0\" data-value=\"a\" data-label=\"a\"><span class=\"ah-combobox-checkbox ah-combobox-checkbox-checked\"><span class=\"ah-combobox-checkbox-icon\">✓</span></span><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">a</div></div></li><li class=\"ah-combobox-item\" id=\"t-cbm-opt-1\" role=\"option\" aria-selected=\"false\" data-index=\"1\" data-value=\"b\" data-label=\"b\"><span class=\"ah-combobox-checkbox\"></span><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">b</div></div></li><li class=\"ah-combobox-item\" id=\"t-cbm-opt-2\" role=\"option\" aria-selected=\"false\" data-index=\"2\" data-value=\"c\" data-label=\"c\"><span class=\"ah-combobox-checkbox\"></span><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">c</div></div></li></ul></div></div>"};

  function mount(fx, html) { fx.innerHTML = html; AH.mount(fx); return fx.firstChild; }
  function key(el, k, extra) { $(el).trigger($.Event("keydown", $.extend({ key: k }, extra || {}))); }
  function changes(el) {
    var seen = [];
    $(el).on("change", function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }

  T.test("datepicker: month grid from the shared template, keyboard, change", function (fx) {
    var el = mount(fx, FX.dp), $in = $(el).find(".ah-datepicker-input"), seen = changes(el);
    $in[0].focus();
    key($in, "ArrowDown");
    T.ok($(el).hasClass("ah-datepicker-open"), "opens");
    T.eq($(el).find(".ah-datepicker-title").text(), "September 2026");
    T.eq($(el).find(".ah-datepicker-weekday").first().text(), "Su");
    T.eq($(el).find(".ah-datepicker-day-focused").attr("id"), "t-dp-d2026-09-29");
    T.eq($(el).find('[data-date="2026-09-09"]').attr("aria-disabled"), "true", "before min");
    key($in, "ArrowUp"); key($in, "ArrowLeft"); key($in, "Enter");
    T.eq(el.getAttribute("data-ah-value"), "2026-09-21");
    T.eq($(el).find("input[type=hidden]").val(), "2026-09-21");
    T.eq(seen, ["2026-09-21"]);
    T.ok(!$(el).hasClass("ah-datepicker-open"), "closes");
    key($in, "Enter"); key($in, "PageDown"); key($in, "End");
    T.eq($(el).find(".ah-datepicker-day-focused").attr("data-date"), "2026-10-31");
    key($in, "Escape");
    T.ok(!$(el).hasClass("ah-datepicker-open"), "escape");
  });

  T.test("datepicker: range, hover preview, clear", function (fx) {
    var el = mount(fx, FX.dpr), seen = changes(el);
    AH.invoke(el, "setValue", "2026-03-01");
    AH.invoke(el, "open");
    T.eq(el.getAttribute("data-ah-value"), "2026-03-01,", "setValue sets without an event");
    $(el).find('[data-date="2026-03-10"]').trigger("click");
    $(el).find('[data-date="2026-03-04"]').trigger("mouseenter");
    T.eq($(el).find(".ah-datepicker-day-hover-range").length, 7);
    $(el).find('[data-date="2026-03-04"]').trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "2026-03-04,2026-03-10");
    T.eq($(el).find(".ah-datepicker-input").val(), "2026-03-04 - 2026-03-10");
    $(el).find(".ah-datepicker-clear").trigger("click");
    T.eq(seen, ["2026-03-04,2026-03-10", ""]);
  });

  T.test("combobox: filter by hiding rows, highlight with <b>, keyboard pick", function (fx) {
    var el = mount(fx, FX.cb), $in = $(el).find(".ah-combobox-input"), seen = changes(el);
    var apple = document.getElementById("t-cb-opt-0");
    $in[0].focus();
    $in.val("ap").trigger("input");
    T.eq($(el).find(".ah-combobox-item:visible .ah-combobox-item-label").map(function () {
      return this.innerHTML; }).get(), ["<b>Ap</b>ple", "<b>Ap</b>ricot", "Gr<b>ap</b>e"]);
    T.ok(document.getElementById("t-cb-opt-0") === apple, "rows are kept, not rebuilt");
    $in.val("an").trigger("input");
    T.eq($(el).find(".ah-combobox-group-header:visible").length, 0, "empty group hidden");
    $in.val("ap").trigger("input");
    key($in, "ArrowDown"); key($in, "ArrowDown");
    T.eq($in.attr("aria-activedescendant"), "t-cb-opt-1");
    key($in, "Enter");
    T.eq(el.getAttribute("data-ah-value"), "Apricot");
    T.eq($in.val(), "Apricot");
    T.eq(seen, ["Apricot"]);
    $in.val("zz").trigger("input");
    T.eq($(el).find(".ah-combobox-empty").text(), "No results found");
  });

  T.test("combobox: check boxes and tags from the shared template", function (fx) {
    var el = mount(fx, FX.cbm), $in = $(el).find(".ah-combobox-input");
    $in[0].focus();
    key($in, "ArrowDown");
    $(el).find('.ah-combobox-item[data-value="c"]').trigger("mousedown");
    T.eq(el.getAttribute("data-ah-value"), "a,c");
    T.eq($(el).find(".ah-combobox-checkbox-checked").length, 2);
    T.eq($(el).find(".ah-combobox-tag-text").map(function () { return $(this).text(); }).get(), ["a", "c"]);
    T.eq($(el).find('.ah-combobox-tag-close[data-value="c"]').attr("aria-label"), "Remove c");
    $(el).find('.ah-combobox-tag-close[data-value="a"]').trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "c");
    T.eq($(el).find(".ah-combobox-checkbox-checked").length, 1);
  });

  T.test("combobox: server search results are morphed in, focus and caret kept", function (fx) {
    var el = mount(fx, FX.remote), $in = $(el).find(".ah-combobox-input");
    T.ok(/^input:[^ ]+:250$/.test($in.attr("data-ah-on")), "debounced search action");
    T.eq($in.attr("data-combobox"), "t-rem");
    $in[0].focus();
    $in.val("p").trigger("input");
    T.eq($(el).find(".ah-combobox-loading").length, 1, "loading");
    $in[0].setSelectionRange(1, 1);
    AH.apply(FX.ops);                     // what set_items/3 sends
    T.eq(document.activeElement, $in[0]);
    T.eq([$in[0].selectionStart, $in.val()], [1, "p"]);
    T.eq($(el).find(".ah-combobox-loading").length, 0);
    T.eq($(el).find(".ah-combobox-item-label").map(function () { return this.innerHTML; }).get(),
         ["<b>P</b>lum", "<b>P</b>ear"]);
    key($in, "ArrowDown"); key($in, "Enter");
    T.eq(el.getAttribute("data-ah-value"), "Plum");
  });
  T.test("popups escape an overflow:hidden box (AH.float) and stop on close", function (fx) {
    fx.innerHTML = '<div id="clip" style="overflow:hidden;height:44px;width:320px;margin-top:20px"></div>';
    var box = document.getElementById("clip");
    box.innerHTML = FX.cb;
    AH.mount(box);
    var el = box.firstChild, $in = $(el).find(".ah-combobox-input"), pop = $(el).find(".ah-combobox-popup")[0];
    $in[0].focus();
    key($in, "ArrowDown");
    var p = pop.getBoundingClientRect(), a = el.getBoundingClientRect();
    T.eq(getComputedStyle(pop).position, "fixed");
    T.ok(p.bottom > box.getBoundingClientRect().bottom + 50, "below the clip box");
    T.ok(pop.contains(document.elementFromPoint(p.left + 10, p.bottom - 8)), "not clipped");
    T.ok(p.width >= a.width - 0.5, "matchWidth");
    $in.val("gr").trigger("input");
    T.ok(pop.contains(document.elementFromPoint(p.left + 10, pop.getBoundingClientRect().bottom - 8)), "re-placed after filtering");
    $(document.body).trigger("mousedown");
    T.ok(!$(el).hasClass("ah-combobox-open"), "outside click closes");
    T.eq(pop.style.position, "", "float stopped");
    box.innerHTML = FX.dp;
    AH.mount(box);
    var dp = box.firstChild, dpop = $(dp).find(".ah-datepicker-popup")[0];
    AH.invoke(dp, "open");
    // (no stylesheet on this test page, so the grid is taller than the
    // viewport: check the placement, the preview script checks visibility)
    T.eq(dpop.style.position, "fixed", "datepicker floated");
    T.ok(!!dpop.getAttribute("data-ah-placement"), "datepicker placed");
    AH.invoke(dp, "close");
    T.eq(dpop.style.position, "", "datepicker float stopped");
  });
})(window.AHTest, window.jQuery, window.AH);
