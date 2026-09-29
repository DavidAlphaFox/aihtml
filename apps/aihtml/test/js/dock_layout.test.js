/* Dock layout behaviour (dock_layout.js). The fixtures are server
 * renders from aihtml_dock_layout (and the operation its
 * dock_layout_open/4 sends), captured once; regenerate them if the markup
 * changes. */
(function (T, AH) {
  "use strict";

  var FX = {"open":[{"args":["<div class=\"ah-dl-tabbed\" data-group-id=\"t-dl-o18434\" data-allow-pin=\"true\" data-allow-close=\"true\" data-pinned=\"true\"><div class=\"ah-tabs ah-tabs-top ah-dl-tabs\"><ul class=\"ah-tabs-header\" role=\"tablist\" aria-orientation=\"horizontal\"><li class=\"ah-tabs-item ah-tabs-item-selected\" id=\"t-dl-t-p\" role=\"tab\" tabindex=\"0\" aria-selected=\"true\" aria-controls=\"t-dl-p-p\" data-panel-id=\"p\">Props</li><li class=\"ah-dl-tabbed-actions\" role=\"presentation\"><button type=\"button\" class=\"ah-dl-btn ah-dl-btn-pin\" aria-label=\"Auto Hide\" title=\"Auto Hide\" aria-pressed=\"false\"></button><button type=\"button\" class=\"ah-dl-btn ah-dl-btn-close\" aria-label=\"Close\" title=\"Close\"></button></li></ul><div class=\"ah-tabs-content\"><div class=\"ah-tabs-panel ah-tabs-panel-active\" id=\"t-dl-p-p\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"t-dl-t-p\"><div class=\"ah-dl-panel-content\" data-panel-id=\"p\">&lt;b&gt;props&lt;/b&gt;</div></div></div></div></div>",{"edge":"left"}],"id":"t-dl","op":"call","method":"openPanel"}],"dl":"<div class=\"ah-dl\" id=\"t-dl\" data-ah=\"dock-layout\" data-ah-value=\"[{&quot;size&quot;:100,&quot;type&quot;:&quot;split&quot;,&quot;items&quot;:[{&quot;active&quot;:&quot;e&quot;,&quot;close&quot;:true,&quot;id&quot;:&quot;left&quot;,&quot;size&quot;:25,&quot;type&quot;:&quot;tabs&quot;,&quot;items&quot;:[&quot;e&quot;,&quot;s&quot;],&quot;pin&quot;:true},{&quot;size&quot;:75,&quot;type&quot;:&quot;split&quot;,&quot;items&quot;:[{&quot;active&quot;:&quot;m&quot;,&quot;close&quot;:true,&quot;id&quot;:&quot;docs&quot;,&quot;size&quot;:65,&quot;type&quot;:&quot;documents&quot;,&quot;items&quot;:[&quot;m&quot;,&quot;n&quot;]},{&quot;active&quot;:&quot;c&quot;,&quot;close&quot;:true,&quot;id&quot;:&quot;bottom&quot;,&quot;size&quot;:35,&quot;type&quot;:&quot;tabs&quot;,&quot;items&quot;:[&quot;c&quot;],&quot;pin&quot;:true}],&quot;orientation&quot;:&quot;vertical&quot;}],&quot;orientation&quot;:&quot;horizontal&quot;},{&quot;active&quot;:&quot;i&quot;,&quot;id&quot;:&quot;fl&quot;,&quot;type&quot;:&quot;float&quot;,&quot;x&quot;:40,&quot;y&quot;:30,&quot;items&quot;:[&quot;i&quot;],&quot;width&quot;:260,&quot;height&quot;:180},{&quot;active&quot;:&quot;o&quot;,&quot;close&quot;:true,&quot;id&quot;:&quot;ah&quot;,&quot;size&quot;:150,&quot;type&quot;:&quot;autohide&quot;,&quot;items&quot;:[&quot;o&quot;],&quot;edge&quot;:&quot;right&quot;,&quot;pin&quot;:true}]\" data-ah-min-size=\"100\" data-ah-labels=\"{&quot;close&quot;:&quot;Close&quot;,&quot;float&quot;:&quot;Float&quot;,&quot;auto_hide&quot;:&quot;Auto Hide&quot;,&quot;dock&quot;:&quot;Dock&quot;}\"><div class=\"ah-dl-autohide-strip ah-dl-autohide-strip-top\"></div><div class=\"ah-dl-autohide-preview-slot ah-dl-autohide-preview-slot-top\"></div><div class=\"ah-dl-middle\"><div class=\"ah-dl-autohide-strip ah-dl-autohide-strip-left\"></div><div class=\"ah-dl-autohide-preview-slot ah-dl-autohide-preview-slot-left\"></div><div class=\"ah-dl-inner\"><div class=\"ah-dl-group ah-dl-horizontal\" style=\"flex:100 1 0px\"><div class=\"ah-dl-tabbed\" data-group-id=\"left\" data-allow-pin=\"true\" data-allow-close=\"true\" data-pinned=\"true\" style=\"flex:25 1 0px\"><div class=\"ah-tabs ah-tabs-top ah-dl-tabs\"><ul class=\"ah-tabs-header\" role=\"tablist\" aria-orientation=\"horizontal\"><li class=\"ah-tabs-item ah-tabs-item-selected\" id=\"t-dl-t-e\" role=\"tab\" tabindex=\"0\" aria-selected=\"true\" aria-controls=\"t-dl-p-e\" data-panel-id=\"e\">Explorer</li><li class=\"ah-tabs-item\" id=\"t-dl-t-s\" role=\"tab\" tabindex=\"-1\" aria-selected=\"false\" aria-controls=\"t-dl-p-s\" data-panel-id=\"s\">Search</li><li class=\"ah-dl-tabbed-actions\" role=\"presentation\"><button type=\"button\" class=\"ah-dl-btn ah-dl-btn-pin\" aria-label=\"Auto Hide\" title=\"Auto Hide\" aria-pressed=\"false\"></button><button type=\"button\" class=\"ah-dl-btn ah-dl-btn-close\" aria-label=\"Close\" title=\"Close\"></button></li></ul><div class=\"ah-tabs-content\"><div class=\"ah-tabs-panel ah-tabs-panel-active\" id=\"t-dl-p-e\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"t-dl-t-e\"><div class=\"ah-dl-panel-content\" data-panel-id=\"e\">tree</div></div><div class=\"ah-tabs-panel\" id=\"t-dl-p-s\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"t-dl-t-s\" hidden><div class=\"ah-dl-panel-content\" data-panel-id=\"s\">find</div></div></div></div></div><div class=\"ah-dl-splitbar\" role=\"separator\" tabindex=\"0\" aria-orientation=\"vertical\"></div><div class=\"ah-dl-group ah-dl-vertical\" style=\"flex:75 1 0px\"><div class=\"ah-dl-tabbed ah-dl-document-group\" data-group-id=\"docs\" data-allow-pin=\"false\" data-allow-close=\"true\" data-pinned=\"true\" data-document=\"true\" style=\"flex:65 1 0px\"><div class=\"ah-tabs ah-tabs-top ah-dl-tabs\"><ul class=\"ah-tabs-header\" role=\"tablist\" aria-orientation=\"horizontal\"><li class=\"ah-tabs-item ah-tabs-item-selected\" id=\"t-dl-t-m\" role=\"tab\" tabindex=\"0\" aria-selected=\"true\" aria-controls=\"t-dl-p-m\" data-panel-id=\"m\">main.erl</li><li class=\"ah-tabs-item\" id=\"t-dl-t-n\" role=\"tab\" tabindex=\"-1\" aria-selected=\"false\" aria-controls=\"t-dl-p-n\" data-panel-id=\"n\">notes.md</li><li class=\"ah-dl-tabbed-actions\" role=\"presentation\"><button type=\"button\" class=\"ah-dl-btn ah-dl-btn-pin\" aria-label=\"Auto Hide\" title=\"Auto Hide\" aria-pressed=\"false\"></button><button type=\"button\" class=\"ah-dl-btn ah-dl-btn-close\" aria-label=\"Close\" title=\"Close\"></button></li></ul><div class=\"ah-tabs-content\"><div class=\"ah-tabs-panel ah-tabs-panel-active\" id=\"t-dl-p-m\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"t-dl-t-m\"><div class=\"ah-dl-panel-content\" data-panel-id=\"m\">code</div></div><div class=\"ah-tabs-panel\" id=\"t-dl-p-n\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"t-dl-t-n\" hidden><div class=\"ah-dl-panel-content\" data-panel-id=\"n\">text</div></div></div></div></div><div class=\"ah-dl-splitbar\" role=\"separator\" tabindex=\"0\" aria-orientation=\"horizontal\"></div><div class=\"ah-dl-tabbed\" data-group-id=\"bottom\" data-allow-pin=\"true\" data-allow-close=\"true\" data-pinned=\"true\" style=\"flex:35 1 0px\"><div class=\"ah-tabs ah-tabs-top ah-dl-tabs\"><ul class=\"ah-tabs-header\" role=\"tablist\" aria-orientation=\"horizontal\"><li class=\"ah-tabs-item ah-tabs-item-selected\" id=\"t-dl-t-c\" role=\"tab\" tabindex=\"0\" aria-selected=\"true\" aria-controls=\"t-dl-p-c\" data-panel-id=\"c\">Console</li><li class=\"ah-dl-tabbed-actions\" role=\"presentation\"><button type=\"button\" class=\"ah-dl-btn ah-dl-btn-pin\" aria-label=\"Auto Hide\" title=\"Auto Hide\" aria-pressed=\"false\"></button><button type=\"button\" class=\"ah-dl-btn ah-dl-btn-close\" aria-label=\"Close\" title=\"Close\"></button></li></ul><div class=\"ah-tabs-content\"><div class=\"ah-tabs-panel ah-tabs-panel-active\" id=\"t-dl-p-c\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"t-dl-t-c\"><div class=\"ah-dl-panel-content\" data-panel-id=\"c\">$</div></div></div></div></div></div></div></div><div class=\"ah-dl-autohide-preview-slot ah-dl-autohide-preview-slot-right\"><div class=\"ah-dl-tabbed\" data-group-id=\"ah\" data-allow-pin=\"true\" data-allow-close=\"true\" data-pinned=\"false\" data-edge=\"right\" data-size=\"150\"><div class=\"ah-tabs ah-tabs-top ah-dl-tabs\"><ul class=\"ah-tabs-header\" role=\"tablist\" aria-orientation=\"horizontal\"><li class=\"ah-tabs-item ah-tabs-item-selected\" id=\"t-dl-t-o\" role=\"tab\" tabindex=\"0\" aria-selected=\"true\" aria-controls=\"t-dl-p-o\" data-panel-id=\"o\">Output</li><li class=\"ah-dl-tabbed-actions\" role=\"presentation\"><button type=\"button\" class=\"ah-dl-btn ah-dl-btn-pin ah-dl-unpinned\" aria-label=\"Auto Hide\" title=\"Auto Hide\" aria-pressed=\"true\"></button><button type=\"button\" class=\"ah-dl-btn ah-dl-btn-close\" aria-label=\"Close\" title=\"Close\"></button></li></ul><div class=\"ah-tabs-content\"><div class=\"ah-tabs-panel ah-tabs-panel-active\" id=\"t-dl-p-o\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"t-dl-t-o\"><div class=\"ah-dl-panel-content\" data-panel-id=\"o\">log</div></div></div></div></div></div><div class=\"ah-dl-autohide-strip ah-dl-autohide-strip-right\"><div class=\"ah-dl-autohide-tab\" data-group-id=\"ah\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\">Output</div></div></div><div class=\"ah-dl-autohide-preview-slot ah-dl-autohide-preview-slot-bottom\"></div><div class=\"ah-dl-autohide-strip ah-dl-autohide-strip-bottom\"></div><div class=\"ah-dl-float-container\"><div class=\"ah-dl-float-window\" data-group-id=\"fl\" role=\"dialog\" aria-label=\"Inspector\" style=\"left:40px;top:30px;width:260px;height:180px;\"><div class=\"ah-dl-float-titlebar\" tabindex=\"0\"><span class=\"ah-dl-float-title\">Inspector</span><div class=\"ah-dl-float-actions\"><button type=\"button\" class=\"ah-dl-btn ah-dl-btn-close\" aria-label=\"Close\" title=\"Close\"></button></div></div><div class=\"ah-dl-float-body\"><div class=\"ah-dl-tabbed\" data-group-id=\"fl-g\" data-allow-pin=\"true\" data-allow-close=\"true\" data-pinned=\"true\"><div class=\"ah-tabs ah-tabs-top ah-dl-tabs\"><ul class=\"ah-tabs-header\" role=\"tablist\" aria-orientation=\"horizontal\"><li class=\"ah-tabs-item ah-tabs-item-selected\" id=\"t-dl-t-i\" role=\"tab\" tabindex=\"0\" aria-selected=\"true\" aria-controls=\"t-dl-p-i\" data-panel-id=\"i\">Inspector</li><li class=\"ah-dl-tabbed-actions\" role=\"presentation\"><button type=\"button\" class=\"ah-dl-btn ah-dl-btn-pin\" aria-label=\"Auto Hide\" title=\"Auto Hide\" aria-pressed=\"false\"></button><button type=\"button\" class=\"ah-dl-btn ah-dl-btn-close\" aria-label=\"Close\" title=\"Close\"></button></li></ul><div class=\"ah-tabs-content\"><div class=\"ah-tabs-panel ah-tabs-panel-active\" id=\"t-dl-p-i\" role=\"tabpanel\" tabindex=\"0\" aria-labelledby=\"t-dl-t-i\"><div class=\"ah-dl-panel-content\" data-panel-id=\"i\">dom</div></div></div></div></div></div><div class=\"ah-dl-float-resize-se\" aria-hidden=\"true\"></div></div></div><div class=\"ah-dl-dock-overlay\" aria-hidden=\"true\"><div class=\"ah-dl-dock-cross\"><div class=\"ah-dl-dock-zone\" data-zone=\"top\"></div><div class=\"ah-dl-dock-zone\" data-zone=\"left\"></div><div class=\"ah-dl-dock-zone\" data-zone=\"center\"></div><div class=\"ah-dl-dock-zone\" data-zone=\"right\"></div><div class=\"ah-dl-dock-zone\" data-zone=\"bottom\"></div></div><div class=\"ah-dl-dock-edge\" data-zone=\"edge-top\"></div><div class=\"ah-dl-dock-edge\" data-zone=\"edge-left\"></div><div class=\"ah-dl-dock-edge\" data-zone=\"edge-right\"></div><div class=\"ah-dl-dock-edge\" data-zone=\"edge-bottom\"></div><div class=\"ah-dl-dock-preview\"></div></div></div>"};

  // The test page has no stylesheets: the geometry the behaviours rely on
  // (from dock_layout.css, tabs.css and extra/dock_layout.css).
  var CSS = [
    ".ah-dl{display:flex;flex-direction:column;width:100%;height:100%;position:relative;overflow:hidden}",
    ".ah-dl-middle,.ah-dl-inner{display:flex;flex:1;min-height:0}",
    ".ah-dl-group{display:flex;position:relative}.ah-dl-horizontal{flex-direction:row}.ah-dl-vertical{flex-direction:column}",
    ".ah-dl-inner>*,.ah-dl-group>*{min-width:0;min-height:0}",
    ".ah-dl-splitbar{flex:0 0 5px}",
    ".ah-dl-tabbed{display:flex;flex-direction:column;overflow:hidden}",
    ".ah-tabs-header{display:flex;list-style:none;margin:0;padding:0}.ah-tabs-item{padding:4px 8px}",
    ".ah-dl-autohide-strip{display:none;width:24px}.ah-dl-autohide-strip:not(:empty){display:flex}",
    ".ah-dl-autohide-strip-top,.ah-dl-autohide-strip-bottom{height:24px;width:auto}",
    ".ah-dl-autohide-preview-slot{display:none}",
    ".ah-dl-autohide-preview-slot>.ah-dl-tabbed:not(.ah-dl-autohide-current){display:none!important}",
    ".ah-dl-float-container{position:absolute;inset:0;pointer-events:none;z-index:200}",
    ".ah-dl-float-window{position:absolute;pointer-events:all;display:flex;flex-direction:column}",
    ".ah-dl-float-titlebar{height:24px}",
    ".ah-dl-dock-overlay{position:absolute;inset:0;z-index:250;pointer-events:none;display:none}",
    ".ah-dl-dock-overlay-visible{display:block}",
    ".ah-dl-dock-cross{position:absolute;display:grid;grid-template:40px 40px 40px/40px 40px 40px;gap:4px}",
    ".ah-dl-dock-zone[data-zone=top]{grid-area:1/2/2/3}.ah-dl-dock-zone[data-zone=left]{grid-area:2/1/3/2}",
    ".ah-dl-dock-zone[data-zone=center]{grid-area:2/2/3/3}.ah-dl-dock-zone[data-zone=right]{grid-area:2/3/3/4}",
    ".ah-dl-dock-zone[data-zone=bottom]{grid-area:3/2/4/3}",
    ".ah-dl-dock-edge{position:absolute}",
    ".ah-dl-dock-edge[data-zone=edge-top]{top:0;left:0;right:0;height:40px}",
    ".ah-dl-dock-edge[data-zone=edge-left]{top:40px;bottom:40px;left:0;width:40px}",
    ".ah-dl-dock-edge[data-zone=edge-right]{top:40px;bottom:40px;right:0;width:40px}",
    ".ah-dl-dock-edge[data-zone=edge-bottom]{bottom:0;left:0;right:0;height:40px}",
    ".ah-dl-context-menu{position:absolute}"
  ].join("\n");
  var style = document.createElement("style");
  style.textContent = CSS;
  document.head.appendChild(style);

  async function mount(fx, html) {
    fx.innerHTML = '<div style="width:900px;height:400px;position:relative">' + html + "</div>";
    await T.ready(fx);
    return fx.firstChild.firstChild;
  }
  function q(el, sel) { return el.querySelector(sel); }
  function qa(el, sel) { return Array.prototype.slice.call(el.querySelectorAll(sel)); }
  function tab(el, id) { return q(el, ".ah-tabs-item[data-panel-id=" + id + "]"); }
  function value(el) { return JSON.parse(el.getAttribute("data-ah-value")); }
  function events(el, types) {
    var seen = [];
    types.split(" ").forEach(function (type) {
      el.addEventListener(type, function (e) {
        if (e.target === el) {
          seen.push(e.type + (e.type === "change" ? "" : ":" + (el.getAttribute("data-window") || el.getAttribute("data-panels"))));
        }
      });
    });
    return seen;
  }
  function center(node) {
    var r = node.getBoundingClientRect();
    return { x: r.left + r.width / 2, y: r.top + r.height / 2 };
  }
  function pointer(type, target, x, y) {
    target.dispatchEvent(new PointerEvent(type, { bubbles: true, cancelable: true, clientX: x, clientY: y,
                                                  button: 0, pointerId: 1, isPrimary: true }));
  }
  function drag(from, x0, y0, x1, y1) {
    pointer("pointerdown", from, x0, y0);
    pointer("pointermove", document, x0 + 8, y0 + 8);
    pointer("pointermove", document, x1, y1);
    pointer("pointerup", document, x1, y1);
  }
  function summary(v) {
    var s = function (n) {
      return n.type === "split" ? n.orientation[0] + "(" + n.items.map(s).join(" ") + ")"
        : n.type === "float" ? "F[" + n.items + "]" : n.type === "autohide" ? "A" + n.edge + "[" + n.items + "]"
        : (n.type === "documents" ? "D" : "T") + "[" + n.items + "]";
    };
    return v.map(s).join(" ");
  }

  T.test("dock_layout: tabs, keyboard, value", async function (fx) {
    var el = await mount(fx, FX.dl), seen = events(el, "change");
    T.eq(summary(value(el)), "h(T[e,s] v(D[m,n] T[c])) F[i] Aright[o]");
    var s = tab(el, "s");
    s.click();
    T.eq(s.getAttribute("aria-selected"), "true");
    T.ok(!document.getElementById("t-dl-p-s").hidden && document.getElementById("t-dl-p-e").hidden, "panels switch");
    T.eq(value(el)[0].items[0].active, "s");
    T.key(s, "ArrowRight");
    T.eq(value(el)[0].items[0].active, "e");
    T.ok(document.activeElement === tab(el, "e"), "focus follows");
    T.key(tab(el, "m"), "Delete");
    T.eq(summary(value(el)), "T[e,s] v(D[n] T[c]) F[i] Aright[o]");
    T.eq(seen.length, 3);
  });

  T.test("dock_layout: drag a tab to a dock zone and out to float", async function (fx) {
    var el = await mount(fx, FX.dl);
    var c = tab(el, "c"), p = center(c);
    var left = q(el, "[data-group-id=left]"), r = left.getBoundingClientRect();
    // the bottom zone of the cross over the left group
    drag(c, p.x, p.y, r.left + r.width / 2, r.top + r.height / 2 + 44);
    T.eq(summary(value(el)), "v(T[e,s] T[c]) D[m,n] F[i] Aright[o]");
    T.ok(!q(el, ":scope > .ah-dl-dock-overlay").classList.contains("ah-dl-dock-overlay-visible"), "overlay hidden");
    var s = tab(el, "s"), sp = center(s);
    var doc = q(el, ".ah-dl-document-group").getBoundingClientRect();
    drag(s, sp.x, sp.y, doc.left + 30, doc.bottom - 60);
    T.eq(summary(value(el)), "v(T[e] T[c]) D[m,n] F[i] F[s] Aright[o]");
    var wins = qa(el, ".ah-dl-float-window"), win = wins[wins.length - 1];
    T.eq(q(win, ".ah-dl-float-title").textContent, "Search");
    // its title bar dragged onto the centre of the documents joins them
    var tb = q(win, ".ah-dl-float-titlebar"), tp = center(tb);
    doc = q(el, ".ah-dl-document-group").getBoundingClientRect();
    drag(tb, tp.x - 40, tp.y, doc.left + doc.width / 2, doc.top + doc.height / 2);
    T.eq(summary(value(el)), "v(T[e] T[c]) D[m,n,s] F[i] Aright[o]");
  });

  T.test("dock_layout: auto hide, preview, pin back, context menu", async function (fx) {
    var el = await mount(fx, FX.dl);
    var left = q(el, "[data-group-id=left]");
    q(left, ".ah-dl-btn-pin").click();
    T.eq(summary(value(el)), "v(D[m,n] T[c]) F[i] Aleft[e,s] Aright[o]");
    var strip = q(el, ".ah-dl-autohide-strip-left .ah-dl-autohide-tab");
    T.eq(strip.textContent, "Explorer");
    strip.click();
    T.ok(left.classList.contains("ah-dl-autohide-current") && left.offsetWidth > 0, "preview shown");
    T.eq(strip.getAttribute("aria-expanded"), "true");
    T.key(q(left, ".ah-tabs-item"), "Escape");
    T.ok(left.offsetWidth === 0, "Escape hides it");
    // Dock from the menu of the right group's preview
    T.fire(tab(el, "o"), "contextmenu", { clientX: 100, clientY: 100 });
    T.eq(qa(el, ".ah-dl-menu-item").map(function (n) { return n.textContent; }), ["Dock", "Float", "Close"]);
    T.ok(document.activeElement === q(el, ".ah-dl-menu-item"), "menu focused");
    T.key(document.activeElement, "Enter");
    T.eq(summary(value(el)), "v(D[m,n] T[c]) T[o] F[i] Aleft[e,s]");
    q(left, ".ah-dl-btn-pin").click();
    T.eq(summary(value(el)), "T[e,s] v(D[m,n] T[c]) T[o] F[i]");
    // Shift+F10 on a tab: Auto Hide is off for a group in the middle
    var c = tab(el, "c");
    c.focus();
    T.key(c, "F10", { shiftKey: true });
    var off = q(el, ".ah-dl-menu-item-disabled");
    T.eq(off ? off.getAttribute("data-action") : undefined, undefined, "c can pin (last in its split)");
    T.key(document.activeElement, "Escape");
    T.eq(qa(el, ".ah-dl-context-menu").length, 0);
    T.ok(document.activeElement === c, "focus back");
  });

  T.test("dock_layout: auto hide tab hover shows and hides the preview", async function (fx) {
    var el = await mount(fx, FX.dl);
    var strip = q(el, ".ah-dl-autohide-strip-right .ah-dl-autohide-tab");
    var g = q(el, "[data-group-id=ah]");
    T.fire(strip, "mouseover", { relatedTarget: el });
    await new Promise(function (res) { setTimeout(res, 260); });
    T.ok(g.classList.contains("ah-dl-autohide-current"), "shown after the delay");
    T.fire(strip, "mouseout", { relatedTarget: el });
    await new Promise(function (res) { setTimeout(res, 460); });
    T.ok(!g.classList.contains("ah-dl-autohide-current"), "hidden after leaving");
  });

  T.test("dock_layout: splitbar keys, methods, openPanel", async function (fx) {
    var el = await mount(fx, FX.dl), seen = events(el, "change ah:panel-close");
    var bar = q(el, ".ah-dl-splitbar");
    T.key(bar, "ArrowRight", { shiftKey: true });
    T.ok(value(el)[0].items[0].size > 25, "grew: " + value(el)[0].items[0].size);
    AH.invoke(el, "float", "c");
    T.eq(summary(value(el)), "T[e,s] D[m,n] F[i] F[c] Aright[o]");
    AH.invoke(el, "dock", "c");
    T.eq(summary(value(el)), "T[e,s] D[m,n,c] F[i] Aright[o]");
    AH.invoke(el, "close", "i");
    T.eq(summary(value(el)), "T[e,s] D[m,n,c] Aright[o]");
    AH.apply(FX.open);
    T.eq(summary(value(el)), "T[p] T[e,s] D[m,n,c] Aright[o]");
    T.eq(q(el, ".ah-dl-panel-content[data-panel-id=p]").textContent, "<b>props</b>", "text, escaped");
    AH.invoke(el, "activate", "o");
    T.ok(q(el, "[data-group-id=ah]").classList.contains("ah-dl-autohide-current"), "activate opens the preview");
    T.eq(seen.filter(function (t) { return t !== "change"; }), [], "methods fire no close events");
  });

  T.test("dock_layout: close button fires ah:panel-close; works again after re-insertion", async function (fx) {
    var el = await mount(fx, FX.dl), seen = events(el, "ah:panel-close");
    var got = null;
    el.addEventListener("ah:panel-close", function (e) { got = e.detail; });
    q(q(el, "[data-group-id=left]"), ".ah-dl-tabbed-actions > .ah-dl-btn-close").click();
    T.eq(seen, ["ah:panel-close:e,s"]);
    T.eq(got, "e,s");
    var host = el.parentNode;
    el.remove();
    await new Promise(function (res) { setTimeout(res, 0); });
    host.innerHTML = FX.dl;
    await T.ready(fx);
    el = host.firstChild;
    tab(el, "s").click();
    T.eq(value(el)[0].items[0].active, "s");
  });
})(window.AHTest, window.AH);
