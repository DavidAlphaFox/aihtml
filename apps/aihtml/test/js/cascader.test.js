/* Cascader behaviour (cascader.js). The fixtures are server renders
 * from aihtml_cascader (and the operations its cascader_children/3 sends), captured once;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var FX = {"cs":"<div class=\"ah-cascader\" id=\"t-cs\" data-ah=\"cascader\" data-ah-value=\"zj,hz,bj\" data-ah-separator=\" / \" data-ah-empty=\"No results found\"><div class=\"ah-cascader-input-area\"><input class=\"ah-cascader-input\" type=\"text\" id=\"t-cs-input\" autocomplete=\"off\" spellcheck=\"false\" readonly placeholder=\"Please select\" value=\"Zhejiang / Hangzhou / Binjiang\" role=\"combobox\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-cs-menus\"><span class=\"ah-cascader-clear\" role=\"button\" aria-label=\"Clear\">Ã</span><span class=\"ah-cascader-arrow\" aria-hidden=\"true\"><span class=\"ah-cascader-arrow-icon\">â¼</span></span></div><input type=\"hidden\" name=\"r\" value=\"zj,hz,bj\"><div class=\"ah-cascader-popup\" id=\"t-cs-popup\"><div class=\"ah-cascader-menus\" id=\"t-cs-menus\" role=\"group\" style=\"max-height:240px\"><div class=\"ah-cascader-menu-column\" data-level=\"0\" data-parent=\"\"><ul class=\"ah-cascader-menu\" role=\"listbox\"><li class=\"has-children active\" role=\"option\" aria-selected=\"true\" aria-haspopup=\"true\" data-value=\"zj\" data-level=\"0\"><span class=\"ah-cascader-menu-item-label\">Zhejiang</span><span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">â¶</span></li><li class=\"has-children\" role=\"option\" aria-selected=\"false\" aria-haspopup=\"true\" data-value=\"js\" data-level=\"0\" data-lazy><span class=\"ah-cascader-menu-item-label\">Jiangsu</span><span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">â¶</span></li><li role=\"option\" aria-selected=\"false\" data-value=\"hk\" data-level=\"0\"><span class=\"ah-cascader-menu-item-label\">Hong Kong</span></li></ul></div><div class=\"ah-cascader-menu-column\" data-level=\"1\" data-parent=\"zj\" hidden><ul class=\"ah-cascader-menu\" role=\"listbox\"><li class=\"has-children active\" role=\"option\" aria-selected=\"true\" aria-haspopup=\"true\" data-value=\"hz\" data-level=\"1\"><span class=\"ah-cascader-menu-item-label\">Hangzhou</span><span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">â¶</span></li><li role=\"option\" aria-selected=\"false\" aria-disabled=\"true\" data-value=\"nb\" data-level=\"1\"><span class=\"ah-cascader-menu-item-label\">Ningbo</span></li></ul></div><div class=\"ah-cascader-menu-column\" data-level=\"2\" data-parent=\"zj,hz\" hidden><ul class=\"ah-cascader-menu\" role=\"listbox\"><li role=\"option\" aria-selected=\"false\" data-value=\"xihu\" data-level=\"2\"><span class=\"ah-cascader-menu-item-label\">xihu</span></li><li class=\"active\" role=\"option\" aria-selected=\"true\" data-value=\"bj\" data-level=\"2\"><span class=\"ah-cascader-menu-item-label\">Binjiang</span></li></ul></div></div></div><span class=\"ah-cascader-loader\" hidden data-cascader=\"t-cs\" data-ah-on=\"ah:load:g2gDdxdhaWh0bWxfZm9ybV9saXN0c190ZXN0c3cEbG9hZHQAAAAA.u7DNHHV6BpxDaSBfjI7rWv0Bu4uVctZWqlw7DuDO7yo\" data-ah-sync=\"queue\"></span></div>","csf":"<div class=\"ah-cascader ah-cascader-change-on-select ah-cascader-filterable\" id=\"t-csf\" data-ah=\"cascader\" data-ah-value=\"\" data-ah-separator=\" / \" data-ah-empty=\"No results found\" data-ah-change-on-select><div class=\"ah-cascader-input-area\"><input class=\"ah-cascader-input\" type=\"text\" id=\"t-csf-input\" autocomplete=\"off\" spellcheck=\"false\" placeholder=\"Please select\" value=\"\" role=\"combobox\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-controls=\"t-csf-menus\" aria-autocomplete=\"list\"><span class=\"ah-cascader-clear\" role=\"button\" aria-label=\"Clear\" hidden>Ã</span><span class=\"ah-cascader-arrow\" aria-hidden=\"true\"><span class=\"ah-cascader-arrow-icon\">â¼</span></span></div><div class=\"ah-cascader-popup\" id=\"t-csf-popup\"><div class=\"ah-cascader-menus\" id=\"t-csf-menus\" role=\"group\" style=\"max-height:240px\"><div class=\"ah-cascader-menu-column\" data-level=\"0\" data-parent=\"\"><ul class=\"ah-cascader-menu\" role=\"listbox\"><li class=\"has-children\" role=\"option\" aria-selected=\"false\" aria-haspopup=\"true\" data-value=\"zj\" data-level=\"0\"><span class=\"ah-cascader-menu-item-label\">Zhejiang</span><span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">â¶</span></li><li class=\"has-children\" role=\"option\" aria-selected=\"false\" aria-haspopup=\"true\" data-value=\"js\" data-level=\"0\" data-lazy><span class=\"ah-cascader-menu-item-label\">Jiangsu</span><span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">â¶</span></li><li role=\"option\" aria-selected=\"false\" data-value=\"hk\" data-level=\"0\"><span class=\"ah-cascader-menu-item-label\">Hong Kong</span></li></ul></div><div class=\"ah-cascader-menu-column\" data-level=\"1\" data-parent=\"zj\" hidden><ul class=\"ah-cascader-menu\" role=\"listbox\"><li class=\"has-children\" role=\"option\" aria-selected=\"false\" aria-haspopup=\"true\" data-value=\"hz\" data-level=\"1\"><span class=\"ah-cascader-menu-item-label\">Hangzhou</span><span class=\"ah-cascader-menu-item-arrow\" aria-hidden=\"true\">â¶</span></li><li role=\"option\" aria-selected=\"false\" aria-disabled=\"true\" data-value=\"nb\" data-level=\"1\"><span class=\"ah-cascader-menu-item-label\">Ningbo</span></li></ul></div><div class=\"ah-cascader-menu-column\" data-level=\"2\" data-parent=\"zj,hz\" hidden><ul class=\"ah-cascader-menu\" role=\"listbox\"><li role=\"option\" aria-selected=\"false\" data-value=\"xihu\" data-level=\"2\"><span class=\"ah-cascader-menu-item-label\">xihu</span></li><li role=\"option\" aria-selected=\"false\" data-value=\"bj\" data-level=\"2\"><span class=\"ah-cascader-menu-item-label\">Binjiang</span></li></ul></div></div><ul class=\"ah-cascader-search-panel\" id=\"t-csf-search\" role=\"listbox\" hidden><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" data-path=\"zj\" data-label=\"Zhejiang\"><span class=\"ah-cascader-search-item-label\">Zhejiang</span></li><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" data-path=\"zj,hz\" data-label=\"Zhejiang / Hangzhou\"><span class=\"ah-cascader-search-item-label\">Zhejiang / Hangzhou</span></li><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" data-path=\"zj,hz,xihu\" data-label=\"Zhejiang / Hangzhou / xihu\"><span class=\"ah-cascader-search-item-label\">Zhejiang / Hangzhou / xihu</span></li><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" data-path=\"zj,hz,bj\" data-label=\"Zhejiang / Hangzhou / Binjiang\"><span class=\"ah-cascader-search-item-label\">Zhejiang / Hangzhou / Binjiang</span></li><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" aria-disabled=\"true\" data-path=\"zj,nb\" data-label=\"Zhejiang / Ningbo\"><span class=\"ah-cascader-search-item-label\">Zhejiang / Ningbo</span></li><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" data-path=\"js\" data-label=\"Jiangsu\"><span class=\"ah-cascader-search-item-label\">Jiangsu</span></li><li class=\"ah-cascader-search-item\" role=\"option\" aria-selected=\"false\" data-path=\"hk\" data-label=\"Hong Kong\"><span class=\"ah-cascader-search-item-label\">Hong Kong</span></li></ul></div></div>","csops":[{"id":"t-cs-menus","op":"html","html":"<div class=\"ah-cascader-menu-column\" data-level=\"1\" data-parent=\"js\" hidden><ul class=\"ah-cascader-menu\" role=\"listbox\"><li role=\"option\" aria-selected=\"false\" data-value=\"nj\" data-level=\"1\"><span class=\"ah-cascader-menu-item-label\">Nanjing</span></li><li role=\"option\" aria-selected=\"false\" data-value=\"sz\" data-level=\"1\"><span class=\"ah-cascader-menu-item-label\">Suzhou</span></li></ul></div>","swap":"append"},{"args":["js"],"id":"t-cs","op":"call","method":"childrenLoaded"}]};

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
  function visibleCols(el) {
    return qa(el, ".ah-cascader-menu-column:not([hidden])").map(function (c) {
      return c.getAttribute("data-parent"); });
  }
  function cursor(el) {
    var c = q(el, ".ah-cascader-menu-item-focused");
    return c ? c.getAttribute("data-value") : undefined;
  }

  T.test("cascader: open on the value path, keyboard, pick a leaf, clear", async function (fx) {
    var el = await mount(fx, FX.cs), inp = q(el, ".ah-cascader-input"), seen = changes(el);
    key(inp, "ArrowDown");
    T.ok(el.classList.contains("ah-cascader-open"), "opens");
    T.eq(visibleCols(el), ["", "zj", "zj,hz"]);
    T.eq(cursor(el), "bj");
    T.eq(inp.getAttribute("aria-activedescendant"), q(el, ".ah-cascader-menu-item-focused").id);
    key(inp, "ArrowLeft");
    T.eq(cursor(el), "hz");
    key(inp, "ArrowDown");                   // skips the disabled Ningbo, wraps
    T.eq(cursor(el), "hz");
    T.eq(visibleCols(el), ["", "zj"], "moving closes the columns to the right");
    key(inp, "ArrowRight");
    T.eq(visibleCols(el), ["", "zj", "zj,hz"]);
    T.eq(cursor(el), "xihu");
    key(inp, "Enter");
    T.eq(el.getAttribute("data-ah-value"), "zj,hz,xihu");
    T.eq(q(el, "input[type=hidden]").value, "zj,hz,xihu");
    T.eq(inp.value, "Zhejiang / Hangzhou / xihu");
    T.ok(!el.classList.contains("ah-cascader-open"), "closes");
    q(el, ".ah-cascader-clear").click();
    T.eq(inp.value, "");
    T.ok(q(el, ".ah-cascader-clear").hidden, "clear button hidden");
    T.eq(seen, ["zj,hz,xihu", ""]);
  });

  T.test("cascader: click a branch then a leaf, setValue, labels", async function (fx) {
    var el = await mount(fx, FX.cs), seen = changes(el);
    AH.invoke(el, "open");
    q(el, 'li[data-value="hk"]').click();
    T.eq(seen, ["hk"]);
    T.eq(AH.invoke(el, "getLabels"), ["Hong Kong"]);
    AH.invoke(el, "setValue", ["zj", "hz", "bj"]);
    T.eq(q(el, ".ah-cascader-input").value, "Zhejiang / Hangzhou / Binjiang");
    T.eq(seen, ["hk"], "setValue fires no change");
  });

  T.test("cascader: lazy level loaded from the server's column", async function (fx) {
    var el = await mount(fx, FX.cs), seen = changes(el), loads = [];
    q(el, ".ah-cascader-loader").addEventListener("ah:load", function (e) {
      loads.push(this.getAttribute("data-ah-value"));
      e.stopPropagation();                  // no server here
    });
    AH.invoke(el, "open");
    q(el, 'li[data-value="js"]').click();
    T.eq(loads, ["js"]);
    T.eq(qa(el, ".ah-cascader-loading").length, 1, "loading message");
    AH.apply(FX.csops);                      // what cascader_children/3 sends
    T.eq(qa(el, ".ah-cascader-loading").length, 0);
    T.eq(visibleCols(el), ["", "js"]);
    T.ok(!q(el, 'li[data-value="js"]').hasAttribute("data-lazy"), "loaded");
    q(el, 'li[data-value="sz"]').click();
    T.eq(seen, ["js,sz"]);
    T.eq(q(el, ".ah-cascader-input").value, "Jiangsu / Suzhou");
    // a second visit uses the loaded column
    AH.invoke(el, "open");
    q(el, 'li[data-value="js"]').click();
    T.eq(loads, ["js"]);
  });

  T.test("cascader: empty lazy answer makes a leaf", async function (fx) {
    var el = await mount(fx, FX.cs), seen = changes(el);
    q(el, ".ah-cascader-loader").addEventListener("ah:load", function (e) { e.stopPropagation(); });
    AH.invoke(el, "open");
    q(el, 'li[data-value="js"]').click();
    AH.invoke(el, "childrenLoaded", "js");
    T.ok(!q(el, 'li[data-value="js"]').classList.contains("has-children"), "leaf");
    T.eq(seen, ["js"]);
  });

  T.test("cascader: search paths, change_on_select", async function (fx) {
    var el = await mount(fx, FX.csf), inp = q(el, ".ah-cascader-input"), seen = changes(el);
    inp.focus();
    type(inp, "bin");
    T.eq(qa(el, ".ah-cascader-search-item").filter(visible).map(function (li) {
      return li.querySelector(".ah-cascader-search-item-label").innerHTML; }), ["Zhejiang / Hangzhou / <b>Bin</b>jiang"]);
    key(inp, "ArrowDown"); key(inp, "Enter");
    T.eq(seen, ["zj,hz,bj"]);
    T.eq(inp.value, "Zhejiang / Hangzhou / Binjiang");
    type(inp, "zzz");
    T.eq(q(el, ".ah-cascader-empty").textContent, "No results found");
    key(inp, "Escape");
    AH.invoke(el, "close");
    AH.invoke(el, "open");
    q(el, 'li[data-value="zj"]').click();
    T.eq(seen, ["zj,hz,bj", "zj"], "a branch is a value with change_on_select");
    T.ok(el.classList.contains("ah-cascader-open"), "stays open");
  });

  T.test("cascader: outside mousedown closes; removed and re-inserted, it still works", async function (fx) {
    var el = await mount(fx, FX.cs), events = [];
    el.addEventListener("ah:open", function () { events.push("open"); });
    el.addEventListener("ah:close", function () { events.push("close"); });
    q(el, ".ah-cascader-input").click();
    T.fire(document.body, "mousedown");
    T.eq(events, ["open", "close"]);
    el.remove();
    await new Promise(function (r) { setTimeout(r, 0); });   // teardown ran
    fx.appendChild(el);
    await T.ready(fx);
    var seen = changes(el);
    AH.invoke(el, "open");
    q(el, 'li[data-value="hk"]').click();
    T.eq(seen, ["hk"]);
    T.ok(!el.classList.contains("ah-cascader-open"));
  });
})(window.AHTest, window.AH);
