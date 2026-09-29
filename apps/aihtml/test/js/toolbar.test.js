/* toolbar: the toolbar behaviour on the markup the server renders.
 * SERVER holds renders of the aihtml_example_demo_toolbar demos, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "tlo": "<div class=\"w-64\"><div class=\"ah-toolbar\" data-ah=\"toolbar\" role=\"toolbar\" aria-orientation=\"horizontal\" data-ah-popup-width=\"160\"><div class=\"ah-toolbar-tool ah-toolbar-tool-first\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"new\"><span>New</span></button></div><div class=\"ah-toolbar-tool ah-toolbar-tool-inner\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"open\"><span>Open</span></button></div><div class=\"ah-toolbar-tool ah-toolbar-tool-last ah-toolbar-tool-separator-after\" data-ah-minimizable=\"false\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"save\"><span>Save</span></button></div><div class=\"ah-toolbar-separator\" role=\"separator\" aria-orientation=\"vertical\"></div><div class=\"ah-toolbar-tool ah-toolbar-tool-first\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"cut\"><span>Cut</span></button></div><div class=\"ah-toolbar-tool ah-toolbar-tool-inner\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"copy\"><span>Copy</span></button></div><div class=\"ah-toolbar-tool ah-toolbar-tool-last ah-toolbar-tool-separator-after\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"paste\"><span>Paste</span></button></div><div class=\"ah-toolbar-separator\" role=\"separator\" aria-orientation=\"vertical\"></div><div class=\"ah-toolbar-tool\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"print\"><span>Print</span></button></div><div class=\"ah-toolbar-minimize-btn\" role=\"button\" tabindex=\"0\" aria-label=\"More tools\" aria-haspopup=\"menu\" aria-expanded=\"false\">☰</div></div></div>",
    "tl": "<div class=\"ah-toolbar\" data-ah=\"toolbar\" role=\"toolbar\" aria-orientation=\"horizontal\" aria-label=\"Formatting\"><div class=\"ah-toolbar-tool ah-toolbar-tool-first\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el ah-btn-toggled\" type=\"button\" data-key=\"bold\" title=\"Bold\" aria-pressed=\"true\" data-ah-toggle><span>B</span></button></div><div class=\"ah-toolbar-tool ah-toolbar-tool-inner\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"italic\" title=\"Italic\" aria-pressed=\"false\" data-ah-toggle><span>I</span></button></div><div class=\"ah-toolbar-tool ah-toolbar-tool-last ah-toolbar-tool-separator-after\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"underline\" title=\"Underline\" aria-pressed=\"false\" data-ah-toggle><span>U</span></button></div><div class=\"ah-toolbar-separator\" role=\"separator\" aria-orientation=\"vertical\"></div><div class=\"ah-toolbar-tool ah-toolbar-tool-first\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"left\"><span>Left</span></button></div><div class=\"ah-toolbar-tool ah-toolbar-tool-inner\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"center\"><span>Center</span></button></div><div class=\"ah-toolbar-tool ah-toolbar-tool-last ah-toolbar-tool-separator-after\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"right\"><span>Right</span></button></div><div class=\"ah-toolbar-separator\" role=\"separator\" aria-orientation=\"vertical\"></div><div class=\"ah-toolbar-tool ah-toolbar-tool-first\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"undo\"><span>Undo</span></button></div><div class=\"ah-toolbar-tool ah-toolbar-tool-last ah-toolbar-tool-separator-after\"><button class=\"ah-btn ah-btn-sm ah-toolbar-tool-el\" type=\"button\" data-key=\"redo\" disabled><span>Redo</span></button></div><div class=\"ah-toolbar-separator\" role=\"separator\" aria-orientation=\"vertical\"></div><div class=\"ah-toolbar-tool\"><div class=\"ah-toolbar-tool-el\"><span class=\"ah-select ah-select-sm\"><select class=\"ah-select-control\"><option value=\"p\" selected>Paragraph</option><option value=\"h1\">Heading</option></select><span class=\"ah-select-arrow\" aria-hidden=\"true\"><span class=\"ah-dropdownlist-arrow-icon\">▼</span></span></span></div></div><div class=\"ah-toolbar-minimize-btn\" role=\"button\" tabindex=\"0\" aria-label=\"More tools\" aria-haspopup=\"menu\" aria-expanded=\"false\">☰</div></div>"
  };

  async function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    await T.ready(fx);
    return fx.querySelector("[data-ah]");
  }

  // the value (or the event's detail) each time type fires on el itself
  function events(el, type) {
    var got = [];
    el.addEventListener(type, function (e) {
      if (e.target === el) { got.push(e.detail == null ? el.getAttribute("data-ah-value") : e.detail); }
    });
    return got;
  }

  function q(el, sel) { return el.querySelector(sel); }
  function qa(el, sel) { return Array.prototype.slice.call(el.querySelectorAll(sel)); }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms || 0); }); }

  // take el out of the page (its controller tears down) and put it back
  async function reinsert(fx, el) {
    fx.removeChild(el);
    await wait(0);
    fx.appendChild(el);
    await T.ready(fx);
  }

  var CSS = ".t-tb .ah-toolbar{display:flex;align-items:center}" +
    ".t-tb .ah-toolbar-tool{flex:none}.t-tb .ah-toolbar-tool button{width:40px;margin:0;padding:0}" +
    ".t-tb .ah-toolbar-minimize-btn{display:none;width:20px;margin-left:auto}" +
    ".t-tb .ah-toolbar-minimize-btn.ah-toolbar-minimize-visible{display:block}" +
    ".ah-toolbar-popup{display:none}.ah-toolbar-popup-open{display:block}";

  async function mountTb(fx, name, width) {
    if (!document.getElementById("t-tb-css")) {
      var s = document.createElement("style");
      s.id = "t-tb-css";
      s.textContent = CSS;
      document.head.appendChild(s);
    }
    fx.innerHTML = '<div class="t-tb" style="width:' + width + 'px">' + SERVER[name] + "</div>";
    await T.ready(fx);
    await wait(50);
    return fx.querySelector("[data-ah=toolbar]");
  }
  function popup() { return document.querySelector(".ah-toolbar-popup"); }
  function inBar(el) {
    return qa(el, ".ah-toolbar-tool-el").map(function (b) { return b.getAttribute("data-key"); });
  }
  function btn(key) { return document.querySelector("button[data-key=" + key + "]"); }

  T.test("toolbar: overflowing tools move to the popup and work there", async function (fx) {
    var el = await mountTb(fx, "tlo", 200);
    var changes = events(el, "change"), seen = [];
    el.addEventListener("ah:open", function () { seen.push("open"); });
    el.addEventListener("ah:close", function () { seen.push("close"); });
    T.eq(inBar(el), ["new", "open", "save", "cut"]);
    var more = q(el, ".ah-toolbar-minimize-btn");
    T.ok(more.classList.contains("ah-toolbar-minimize-visible"));
    T.ok(popup().contains(btn("print")), "moved, not copied");
    more.click();
    T.ok(popup().classList.contains("ah-toolbar-popup-open"));
    T.eq(more.getAttribute("aria-expanded"), "true");
    q(btn("paste"), "span").click();
    T.eq(el.getAttribute("data-ah-value"), "paste");
    T.ok(!popup().classList.contains("ah-toolbar-popup-open"), "a tool closes the popup");
    btn("new").click();
    T.eq(changes, ["paste", "new"]);
    T.eq(seen, ["open", "close"]);
    el.parentNode.style.width = "600px";
    await wait(80);
    T.eq(inBar(el), ["new", "open", "save", "cut", "copy", "paste", "print"], "room again");
    T.ok(!more.classList.contains("ah-toolbar-minimize-visible"));
  });

  T.test("toolbar: toggles, keys and methods", async function (fx) {
    var el = await mountTb(fx, "tl", 900);
    var changes = events(el, "change");
    btn("italic").click();
    T.eq(btn("italic").getAttribute("aria-pressed"), "true");
    T.ok(btn("italic").classList.contains("ah-btn-toggled"));
    T.eq(changes, ["italic"]);
    btn("bold").focus();
    T.key(btn("bold"), "ArrowRight");
    T.eq(document.activeElement, btn("italic"));
    T.key(btn("italic"), "ArrowLeft");
    T.key(btn("bold"), "ArrowLeft");
    T.eq(document.activeElement, q(el, "select"), "wraps to the last visible tool");
    AH.invoke(el, "setPressed", "bold", false);
    T.eq(btn("bold").getAttribute("aria-pressed"), "false");
    AH.invoke(el, "disableTool", "left");
    T.ok(btn("left").disabled);
    AH.invoke(el, "disableTool", "left", false);
    T.ok(!btn("left").disabled);
    T.eq(changes, ["italic"], "methods fire no change");
  });

  T.test("toolbar: removal puts the tools back and drops the popup", async function (fx) {
    var el = await mountTb(fx, "tlo", 200);
    var box = el.parentNode;
    box.removeChild(el);
    await wait(0);
    T.ok(!popup(), "popup removed");
    T.eq(inBar(el).length, 7, "tools back in the bar");
    box.style.width = "200px";
    box.appendChild(el);
    await T.ready(fx);
    await wait(50);
    T.eq(document.querySelectorAll(".ah-toolbar-popup").length, 1, "one popup again");
    T.eq(inBar(el), ["new", "open", "save", "cut"]);
  });
})(window.AHTest, window.AH);
