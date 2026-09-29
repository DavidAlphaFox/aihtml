/* Combobox behaviour (combobox.js). The fixtures are server renders from
 * aihtml_combobox (and the operations its set_items/3 sends), and one
 * datepicker for the shared popup test, captured once; regenerate them
 * if the markup changes. */
(function (T, AH) {
  "use strict";

  var FX = {"remote":"<div class=\"ah-combobox\" id=\"t-rem\" data-ah=\"combobox\" data-ah-value=\"\" data-ah-search-mode=\"contains_ignore_case\" data-ah-remote><div class=\"ah-combobox-input-area\"><input class=\"ah-combobox-input\" type=\"text\" id=\"t-rem-input\" autocomplete=\"off\" spellcheck=\"false\" placeholder=\"\" value=\"\" role=\"combobox\" aria-autocomplete=\"list\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-rem-list\" data-combobox=\"t-rem\" data-ah-on=\"input:g2gDdxlhaWh0bWxfZm9ybV9waWNrZXJzX3Rlc3RzdwZzZWFyY2h0AAAAAA.m9FfOeHQMuFkj7YcmxekgBC4k3Kyj82xmu3gL5VesTY:250\"><span class=\"ah-combobox-arrow\" aria-hidden=\"true\"><span class=\"ah-combobox-arrow-icon\">▼</span></span></div><div class=\"ah-combobox-popup\" style=\"\"><ul class=\"ah-combobox-list\" id=\"t-rem-list\" role=\"listbox\"></ul></div></div>","ops":[{"id":"t-rem-list","op":"html","html":"<li class=\"ah-combobox-item\" id=\"t-rem-opt-0\" role=\"option\" aria-selected=\"false\" data-index=\"0\" data-value=\"Plum\" data-label=\"Plum\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">Plum</div></div></li><li class=\"ah-combobox-item\" id=\"t-rem-opt-1\" role=\"option\" aria-selected=\"false\" data-index=\"1\" data-value=\"Pear\" data-label=\"Pear\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">Pear</div></div></li>","swap":"morph_inner"},{"args":[],"id":"t-rem","op":"call","method":"itemsLoaded"}],"cb":"<div class=\"ah-combobox\" id=\"t-cb\" data-ah=\"combobox\" data-ah-value=\"Banana\" data-ah-search-mode=\"contains_ignore_case\"><div class=\"ah-combobox-input-area\"><input class=\"ah-combobox-input\" type=\"text\" id=\"t-cb-input\" autocomplete=\"off\" spellcheck=\"false\" placeholder=\"\" value=\"Banana\" role=\"combobox\" aria-autocomplete=\"list\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-cb-list\" data-combobox=\"t-cb\"><span class=\"ah-combobox-arrow\" aria-hidden=\"true\"><span class=\"ah-combobox-arrow-icon\">▼</span></span></div><input type=\"hidden\" name=\"f\" value=\"Banana\"><div class=\"ah-combobox-popup\" style=\"\"><ul class=\"ah-combobox-list\" id=\"t-cb-list\" role=\"listbox\"><li class=\"ah-combobox-item\" id=\"t-cb-opt-0\" role=\"option\" aria-selected=\"false\" data-index=\"0\" data-value=\"Apple\" data-label=\"Apple\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">Apple</div></div></li><li class=\"ah-combobox-item\" id=\"t-cb-opt-1\" role=\"option\" aria-selected=\"false\" data-index=\"1\" data-value=\"Apricot\" data-label=\"Apricot\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">Apricot</div></div></li><li class=\"ah-combobox-item ah-combobox-item-selected\" id=\"t-cb-opt-2\" role=\"option\" aria-selected=\"true\" data-index=\"2\" data-value=\"Banana\" data-label=\"Banana\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">Banana</div></div></li><li class=\"ah-combobox-group-header\" role=\"presentation\">G</li><li class=\"ah-combobox-item\" id=\"t-cb-opt-3\" role=\"option\" aria-selected=\"false\" data-index=\"3\" data-value=\"g\" data-label=\"Grape\" data-group=\"G\"><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">Grape</div></div></li></ul></div></div>","cbm":"<div class=\"ah-combobox ah-combobox-checkboxes\" id=\"t-cbm\" data-ah=\"combobox\" data-ah-value=\"a\" data-ah-search-mode=\"contains_ignore_case\" data-ah-placeholder=\"\"><div class=\"ah-combobox-input-area\"><div class=\"ah-combobox-tags\"><span class=\"ah-combobox-tag\"><span class=\"ah-combobox-tag-text\">a</span><span class=\"ah-combobox-tag-close\" data-value=\"a\" role=\"button\" aria-label=\"Remove a\">&times;</span></span><input class=\"ah-combobox-input\" type=\"text\" id=\"t-cbm-input\" autocomplete=\"off\" spellcheck=\"false\" value=\"\" role=\"combobox\" aria-autocomplete=\"list\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-cbm-list\" data-combobox=\"t-cbm\" data-checkboxes=\"true\"></div><span class=\"ah-combobox-arrow\" aria-hidden=\"true\"><span class=\"ah-combobox-arrow-icon\">▼</span></span></div><div class=\"ah-combobox-popup\" style=\"\"><ul class=\"ah-combobox-list\" id=\"t-cbm-list\" role=\"listbox\" aria-multiselectable=\"true\"><li class=\"ah-combobox-item ah-combobox-item-selected\" id=\"t-cbm-opt-0\" role=\"option\" aria-selected=\"true\" data-index=\"0\" data-value=\"a\" data-label=\"a\"><span class=\"ah-combobox-checkbox ah-combobox-checkbox-checked\"><span class=\"ah-combobox-checkbox-icon\">✓</span></span><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">a</div></div></li><li class=\"ah-combobox-item\" id=\"t-cbm-opt-1\" role=\"option\" aria-selected=\"false\" data-index=\"1\" data-value=\"b\" data-label=\"b\"><span class=\"ah-combobox-checkbox\"></span><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">b</div></div></li><li class=\"ah-combobox-item\" id=\"t-cbm-opt-2\" role=\"option\" aria-selected=\"false\" data-index=\"2\" data-value=\"c\" data-label=\"c\"><span class=\"ah-combobox-checkbox\"></span><div class=\"ah-combobox-item-content\"><div class=\"ah-combobox-item-label\">c</div></div></li></ul></div></div>","dp":"<div class=\"ah-datepicker\" id=\"t-dp\" data-ah=\"datepicker\" data-ah-value=\"2026-09-29\" data-ah-format=\"yyyy-MM-dd\" data-ah-min=\"2026-09-10\" data-ah-first-day=\"0\" data-ah-labels=\"{&quot;clear&quot;:&quot;Clear&quot;,&quot;months&quot;:[&quot;January&quot;,&quot;February&quot;,&quot;March&quot;,&quot;April&quot;,&quot;May&quot;,&quot;June&quot;,&quot;July&quot;,&quot;August&quot;,&quot;September&quot;,&quot;October&quot;,&quot;November&quot;,&quot;December&quot;],&quot;months_short&quot;:[&quot;Jan&quot;,&quot;Feb&quot;,&quot;Mar&quot;,&quot;Apr&quot;,&quot;May&quot;,&quot;Jun&quot;,&quot;Jul&quot;,&quot;Aug&quot;,&quot;Sep&quot;,&quot;Oct&quot;,&quot;Nov&quot;,&quot;Dec&quot;],&quot;next_month&quot;:&quot;Next month&quot;,&quot;next_year&quot;:&quot;Next year&quot;,&quot;prev_month&quot;:&quot;Previous month&quot;,&quot;prev_year&quot;:&quot;Previous year&quot;,&quot;title&quot;:&quot;MMMM yyyy&quot;,&quot;today&quot;:&quot;Today&quot;,&quot;weekdays&quot;:[&quot;Su&quot;,&quot;Mo&quot;,&quot;Tu&quot;,&quot;We&quot;,&quot;Th&quot;,&quot;Fr&quot;,&quot;Sa&quot;]}\"><div class=\"ah-datepicker-input-area\"><input class=\"ah-datepicker-input\" type=\"text\" id=\"t-dp-input\" autocomplete=\"off\" spellcheck=\"false\" readonly placeholder=\"Select date...\" value=\"2026-09-29\" role=\"combobox\" aria-haspopup=\"dialog\" aria-expanded=\"false\"><span class=\"ah-datepicker-trigger\" aria-hidden=\"true\"><span class=\"ah-datepicker-icon\">📅</span></span></div><input type=\"hidden\" name=\"d\" value=\"2026-09-29\"><div class=\"ah-datepicker-popup\" role=\"dialog\" aria-label=\"Choose date\"></div></div>"};

  async function mount(fx, html) { fx.innerHTML = html; await T.ready(fx); return fx.firstChild; }
  function key(el, k, extra) { T.key(el, k, extra); }
  function changes(el) {
    var seen = [];
    el.addEventListener("change", function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function q(el, s) { return el.querySelector(s); }
  function qa(el, s) { return Array.prototype.slice.call(el.querySelectorAll(s)); }
  function visible(n) { return n.getClientRects().length > 0; }
  function type(input, v) { input.value = v; T.fire(input, "input"); }
  function html(n) { return n.innerHTML; }

  T.test("combobox: filter by hiding rows, highlight with <b>, keyboard pick", async function (fx) {
    var el = await mount(fx, FX.cb), inp = q(el, ".ah-combobox-input"), seen = changes(el);
    var apple = document.getElementById("t-cb-opt-0");
    inp.focus();
    type(inp, "ap");
    T.eq(qa(el, ".ah-combobox-item").filter(visible).map(function (li) {
      return html(li.querySelector(".ah-combobox-item-label")); }), ["<b>Ap</b>ple", "<b>Ap</b>ricot", "Gr<b>ap</b>e"]);
    T.ok(document.getElementById("t-cb-opt-0") === apple, "rows are kept, not rebuilt");
    type(inp, "an");
    T.eq(qa(el, ".ah-combobox-group-header").filter(visible).length, 0, "empty group hidden");
    type(inp, "ap");
    key(inp, "ArrowDown"); key(inp, "ArrowDown");
    T.eq(inp.getAttribute("aria-activedescendant"), "t-cb-opt-1");
    key(inp, "Enter");
    T.eq(el.getAttribute("data-ah-value"), "Apricot");
    T.eq(inp.value, "Apricot");
    T.eq(seen, ["Apricot"]);
    type(inp, "zz");
    T.eq(q(el, ".ah-combobox-empty").textContent, "No results found");
  });

  T.test("combobox: check boxes and tags from the shared template", async function (fx) {
    var el = await mount(fx, FX.cbm), inp = q(el, ".ah-combobox-input");
    inp.focus();
    key(inp, "ArrowDown");
    T.fire(q(el, '.ah-combobox-item[data-value="c"]'), "mousedown");
    T.eq(el.getAttribute("data-ah-value"), "a,c");
    T.eq(qa(el, ".ah-combobox-checkbox-checked").length, 2);
    T.eq(qa(el, ".ah-combobox-tag-text").map(function (t) { return t.textContent; }), ["a", "c"]);
    T.eq(q(el, '.ah-combobox-tag-close[data-value="c"]').getAttribute("aria-label"), "Remove c");
    q(el, '.ah-combobox-tag-close[data-value="a"]').click();
    T.eq(el.getAttribute("data-ah-value"), "c");
    T.eq(qa(el, ".ah-combobox-checkbox-checked").length, 1);
  });

  T.test("combobox: server search results are morphed in, focus and caret kept", async function (fx) {
    var el = await mount(fx, FX.remote), inp = q(el, ".ah-combobox-input");
    T.ok(/^input:[^ ]+:250$/.test(inp.getAttribute("data-ah-on")), "debounced search action");
    T.eq(inp.getAttribute("data-combobox"), "t-rem");
    inp.focus();
    // a native input event also reaches the data-ah-on binding: keep the
    // request from leaving (no server here)
    inp.addEventListener("input", function (e) { e.stopPropagation(); });
    type(inp, "p");
    T.eq(qa(el, ".ah-combobox-loading").length, 1, "loading");
    inp.setSelectionRange(1, 1);
    AH.apply(FX.ops);                     // what set_items/3 sends
    T.eq(document.activeElement, inp);
    T.eq([inp.selectionStart, inp.value], [1, "p"]);
    T.eq(qa(el, ".ah-combobox-loading").length, 0);
    T.eq(qa(el, ".ah-combobox-item-label").map(html), ["<b>P</b>lum", "<b>P</b>ear"]);
    key(inp, "ArrowDown"); key(inp, "Enter");
    T.eq(el.getAttribute("data-ah-value"), "Plum");
  });

  T.test("combobox: methods; removed and re-inserted, it still works", async function (fx) {
    var el = await mount(fx, FX.cb), seen = changes(el);
    AH.invoke(el, "setValue", "Apple");
    T.eq(q(el, ".ah-combobox-input").value, "Apple");
    T.eq(q(el, "input[type=hidden]").value, "Apple");
    T.eq(seen, [], "setValue fires no change");
    el.remove();
    await new Promise(function (r) { setTimeout(r, 0); });   // teardown ran
    fx.appendChild(el);
    await T.ready(fx);
    AH.invoke(el, "open");
    T.ok(el.classList.contains("ah-combobox-open"));
    T.fire(q(el, '.ah-combobox-item[data-value="g"]'), "mousedown");
    T.eq(AH.invoke(el, "getValue"), "g");
    T.eq(q(el, ".ah-combobox-input").value, "Grape");
    T.ok(!el.classList.contains("ah-combobox-open"), "a pick closes");
    AH.invoke(el, "clear");
    T.eq(seen, ["g", ""]);
  });

  T.test("popups escape an overflow:hidden box (AH.float) and stop on close", async function (fx) {
    fx.innerHTML = '<div id="clip" style="overflow:hidden;height:44px;width:320px;margin-top:20px"></div>';
    var box = document.getElementById("clip");
    box.innerHTML = FX.cb;
    await T.ready(box);
    var el = box.firstChild, inp = q(el, ".ah-combobox-input"), pop = q(el, ".ah-combobox-popup");
    inp.focus();
    key(inp, "ArrowDown");
    var p = pop.getBoundingClientRect(), a = el.getBoundingClientRect();
    T.eq(getComputedStyle(pop).position, "fixed");
    T.ok(p.bottom > box.getBoundingClientRect().bottom + 50, "below the clip box");
    T.ok(pop.contains(document.elementFromPoint(p.left + 10, p.bottom - 8)), "not clipped");
    T.ok(p.width >= a.width - 0.5, "matchWidth");
    type(inp, "gr");
    T.ok(pop.contains(document.elementFromPoint(p.left + 10, pop.getBoundingClientRect().bottom - 8)), "re-placed after filtering");
    T.fire(document.body, "mousedown");
    T.ok(!el.classList.contains("ah-combobox-open"), "outside click closes");
    T.eq(pop.style.position, "", "float stopped");
    box.innerHTML = FX.dp;
    await T.ready(box);
    var dp = box.firstChild, dpop = q(dp, ".ah-datepicker-popup");
    AH.invoke(dp, "open");
    // (no stylesheet on this test page, so the grid is taller than the
    // viewport: check the placement, the preview script checks visibility)
    T.eq(dpop.style.position, "fixed", "datepicker floated");
    T.ok(!!dpop.getAttribute("data-ah-placement"), "datepicker placed");
    AH.invoke(dp, "close");
    T.eq(dpop.style.position, "", "datepicker float stopped");
  });
})(window.AHTest, window.AH);
