/* Docking behaviour (docking.js). The fixtures are server renders from
 * aihtml_docking, captured once; regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var FX = {"dk":"<div class=\"ah-docking ah-docking-horizontal h-64\" id=\"t-dk\" data-ah=\"docking\" data-ah-value=\"{&quot;closed&quot;:[],&quot;panels&quot;:[{&quot;id&quot;:&quot;a&quot;,&quot;windows&quot;:[&quot;a1&quot;,&quot;a2&quot;]},{&quot;id&quot;:&quot;b&quot;,&quot;windows&quot;:[&quot;b1&quot;]}],&quot;floating&quot;:[],&quot;collapsed&quot;:[]}\" data-ah-drag-opacity=\"0.3\" style=\"height:240px;width:600px\"><div class=\"ah-docking-panel\" data-panel-id=\"a\"><div class=\"ah-docking-window ah-docking-window-docked\" data-window-id=\"a1\" role=\"region\" aria-labelledby=\"t-dk-w-a1-title\"><div class=\"ah-docking-window-header\" tabindex=\"0\"><div class=\"ah-docking-window-title\" id=\"t-dk-w-a1-title\">A1</div><div class=\"ah-docking-window-buttons\"><button class=\"ah-docking-window-collapse-btn\" type=\"button\" aria-label=\"Collapse\" title=\"Collapse\" aria-controls=\"t-dk-w-a1-content\" aria-expanded=\"true\"></button><button class=\"ah-docking-window-close-btn\" type=\"button\" aria-label=\"Close\" title=\"Close\"></button></div></div><div class=\"ah-docking-window-content\" id=\"t-dk-w-a1-content\">x</div></div><div class=\"ah-docking-window ah-docking-window-docked\" data-window-id=\"a2\" role=\"region\" aria-labelledby=\"t-dk-w-a2-title\"><div class=\"ah-docking-window-header\" tabindex=\"0\"><div class=\"ah-docking-window-title\" id=\"t-dk-w-a2-title\">A2</div><div class=\"ah-docking-window-buttons\"><button class=\"ah-docking-window-collapse-btn\" type=\"button\" aria-label=\"Collapse\" title=\"Collapse\" aria-controls=\"t-dk-w-a2-content\" aria-expanded=\"true\"></button><button class=\"ah-docking-window-close-btn\" type=\"button\" aria-label=\"Close\" title=\"Close\"></button></div></div><div class=\"ah-docking-window-content\" id=\"t-dk-w-a2-content\">y</div></div></div><div class=\"ah-docking-panel\" data-panel-id=\"b\"><div class=\"ah-docking-window ah-docking-window-docked\" data-window-id=\"b1\" role=\"region\" aria-labelledby=\"t-dk-w-b1-title\"><div class=\"ah-docking-window-header\" tabindex=\"0\"><div class=\"ah-docking-window-title\" id=\"t-dk-w-b1-title\">B1</div><div class=\"ah-docking-window-buttons\"><button class=\"ah-docking-window-collapse-btn\" type=\"button\" aria-label=\"Collapse\" title=\"Collapse\" aria-controls=\"t-dk-w-b1-content\" aria-expanded=\"true\"></button><button class=\"ah-docking-window-close-btn\" type=\"button\" aria-label=\"Close\" title=\"Close\"></button></div></div><div class=\"ah-docking-window-content\" id=\"t-dk-w-b1-content\">z</div></div></div><div class=\"ah-docking-live\" aria-live=\"polite\"></div><input type=\"hidden\" name=\"lay\" value=\"{&quot;closed&quot;:[],&quot;panels&quot;:[{&quot;id&quot;:&quot;a&quot;,&quot;windows&quot;:[&quot;a1&quot;,&quot;a2&quot;]},{&quot;id&quot;:&quot;b&quot;,&quot;windows&quot;:[&quot;b1&quot;]}],&quot;floating&quot;:[],&quot;collapsed&quot;:[]}\"></div>"};

  // The test page has no stylesheets: the geometry the behaviours rely on
  // (from docking.css).
  var CSS = [
    ".ah-docking{display:flex;position:relative;overflow:hidden;height:240px}",
    ".ah-docking-panel{display:flex;flex-direction:column;flex:1}",
    ".ah-docking-window{display:flex;flex-direction:column}",
    ".ah-docking-window-floating{position:absolute;min-width:200px}"
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
  function header(win) { return win.querySelector(":scope > .ah-docking-window-header"); }
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

  T.test("docking: drag a window to another panel, value and change", async function (fx) {
    var el = await mount(fx, FX.dk), seen = events(el, "change");
    var a1 = q(el, "[data-window-id=a1]"), b1 = q(el, "[data-window-id=b1]");
    var h = center(header(a1)), t = b1.getBoundingClientRect();
    drag(header(a1), h.x, h.y, t.left + 20, t.bottom + 20);
    T.eq(value(el).panels, [{ id: "a", windows: ["a2"] }, { id: "b", windows: ["b1", "a1"] }]);
    T.eq(q(el, "input[type=hidden]").value, el.getAttribute("data-ah-value"));
    T.eq(seen.length, 1);
    T.ok(a1.classList.contains("ah-docking-window-docked"), "docked again");
  });

  T.test("docking: drag out floats; Escape cancels a drag", async function (fx) {
    var el = await mount(fx, FX.dk);
    var a1 = q(el, "[data-window-id=a1]");
    var h = center(header(a1));
    pointer("pointerdown", header(a1), h.x, h.y);
    pointer("pointermove", document, h.x + 30, h.y + 30);
    T.ok(a1.classList.contains("ah-docking-window-dragging"), "dragging");
    T.ok(document.documentElement.classList.contains("ah-dock-dragging"));
    T.key(document, "Escape");
    T.ok(!a1.classList.contains("ah-docking-window-dragging"), "cancelled");
    T.ok(!document.documentElement.classList.contains("ah-dock-dragging"));
    T.eq(value(el).panels[0].windows, ["a1", "a2"], "back in place");
    // released outside every panel: it floats, kept inside the container
    var r = el.getBoundingClientRect();
    drag(header(a1), h.x, h.y, r.left + 300, r.bottom + 60);
    T.ok(a1.classList.contains("ah-docking-window-floating"), "floating");
    T.eq(value(el).floating.map(function (f) { return f.id; }), ["a1"]);
    T.ok(value(el).floating[0].y <= el.clientHeight - 40, "inside the container");
  });

  T.test("docking: collapse, close, keyboard move, methods", async function (fx) {
    var el = await mount(fx, FX.dk), seen = events(el, "change ah:window-collapse ah:window-close");
    var a2 = q(el, "[data-window-id=a2]");
    q(a2, ".ah-docking-window-collapse-btn").click();
    T.ok(a2.classList.contains("ah-docking-window-collapsed"));
    T.eq(q(a2, ".ah-docking-window-collapse-btn").getAttribute("aria-expanded"), "false");
    T.eq(value(el).collapsed, ["a2"]);
    var hd = header(a2);
    hd.focus();
    T.key(hd, "ArrowRight", { altKey: true });
    T.eq(value(el).panels[1].windows, ["b1", "a2"]);
    T.ok(document.activeElement === hd, "focus stays");
    T.key(hd, "ArrowUp", { altKey: true });
    T.eq(value(el).panels[1].windows, ["a2", "b1"]);
    q(el, "[data-window-id=b1] .ah-docking-window-close-btn").click();
    T.eq(value(el).closed, ["b1"]);
    T.eq(seen, ["ah:window-collapse:a2", "change", "change", "change", "ah:window-close:b1", "change"]);
    AH.invoke(el, "move", "a2", "a", 0);
    AH.invoke(el, "expand", "a2");
    T.eq(value(el).panels[0].windows, ["a2", "a1"]);
    T.eq(value(el).collapsed, []);
    AH.invoke(el, "setLayout", { panels: [{ id: "b", windows: ["a1"] }], floating: [{ id: "a2", x: 10, y: 12, width: 200 }] });
    T.ok(q(el, "[data-window-id=a2]").classList.contains("ah-docking-window-floating"));
    T.eq(value(el).floating, [{ id: "a2", x: 10, y: 12, width: 200 }]);
    AH.invoke(el, "addWindow", "a", '<div class="ah-docking-window ah-docking-window-docked" data-window-id="b1"><div class="ah-docking-window-header" tabindex="0"><div class="ah-docking-window-title">B1</div></div><div class="ah-docking-window-content">new</div></div>');
    T.eq(value(el).panels[0].windows, ["b1"]);
    T.eq(value(el).closed, [], "reopened");
  });

  T.test("docking: removed and inserted again, it works; a drag stops on removal", async function (fx) {
    var el = await mount(fx, FX.dk);
    var a1 = q(el, "[data-window-id=a1]"), h = center(header(a1));
    pointer("pointerdown", header(a1), h.x, h.y);
    pointer("pointermove", document, h.x + 30, h.y + 30);
    var host = el.parentNode;
    el.remove();
    await new Promise(function (res) { setTimeout(res, 0); });
    T.ok(!document.documentElement.classList.contains("ah-dock-dragging"), "drag stopped");
    fx.innerHTML = "";
    fx.appendChild(host);
    host.innerHTML = FX.dk;
    await T.ready(fx);
    el = host.firstChild;
    var seen = events(el, "change");
    q(el, "[data-window-id=a2] .ah-docking-window-collapse-btn").click();
    T.eq(value(el).collapsed, ["a2"]);
    T.eq(seen.length, 1);
  });
})(window.AHTest, window.AH);
