/* Swimlane behaviour (swimlane.js). The fixtures are server renders from
 * aihtml_swimlane, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"swim":"<div class=\"ah-swimlane\" id=\"t-w\" data-ah=\"swimlane\" data-axis=\"discrete\" data-lane-height=\"110\" data-phase-width=\"190\" data-node-width=\"132\" data-node-height=\"52\" data-editable><div class=\"ah-swimlane-content\"><div class=\"ah-swimlane-lanes\" style=\"width:150px\"><div class=\"ah-swimlane-lanes__corner\" style=\"height:46px\">Lane / Phase</div><div class=\"ah-swimlane-lanes__body\"><div class=\"ah-swimlane-lanes__row\" data-lane-id=\"l1\" style=\"height:110px\"><span class=\"ah-swimlane-lanes__accent\" style=\"background-color:#6B7280\"></span><span class=\"ah-swimlane-lanes__name\">L1</span></div><div class=\"ah-swimlane-lanes__row\" data-lane-id=\"l2\" style=\"height:110px\"><span class=\"ah-swimlane-lanes__accent\" style=\"background-color:#6B7280\"></span><span class=\"ah-swimlane-lanes__name\">L2</span></div></div></div><div class=\"ah-swimlane-grid\"><div class=\"ah-swimlane-grid__header\" style=\"height:46px\"><div class=\"ah-swimlane-grid__header-track\" style=\"width:570px\"><div class=\"ah-swimlane-grid__phase\" data-phase-id=\"p1\" style=\"width:190px\">P1</div><div class=\"ah-swimlane-grid__phase\" data-phase-id=\"p2\" style=\"width:190px\">P2</div><div class=\"ah-swimlane-grid__phase\" data-phase-id=\"p3\" style=\"width:190px\">P3</div></div></div><div class=\"ah-swimlane-grid__body\"><div class=\"ah-swimlane-grid__canvas\" style=\"width:570px;height:220px\"><svg class=\"ah-swimlane-flows\" width=\"570\" height=\"220\" viewBox=\"0 0 570 220\" aria-hidden=\"true\"><defs><marker id=\"t-w-arrow\" markerWidth=\"9\" markerHeight=\"9\" refX=\"7\" refY=\"3\" orient=\"auto\" markerUnits=\"userSpaceOnUse\"><path class=\"ah-swimlane-flows__head\" d=\"M0,0 L7,3 L0,6 Z\"></path></marker><marker id=\"t-w-arrow-active\" markerWidth=\"9\" markerHeight=\"9\" refX=\"7\" refY=\"3\" orient=\"auto\" markerUnits=\"userSpaceOnUse\"><path class=\"ah-swimlane-flows__head ah-swimlane-flows__head--active\" d=\"M0,0 L7,3 L0,6 Z\"></path></marker></defs><g data-from=\"a\" data-to=\"b\"><path class=\"ah-swimlane-flows__line\" d=\"M161,55 L180,55 Q190,55 190,65 L190,155 Q190,165 200,165 L219,165\" fill=\"none\" marker-end=\"url(#t-w-arrow)\"></path><rect class=\"ah-swimlane-flows__label-bg\" x=\"175\" y=\"101\" width=\"30\" height=\"18\" rx=\"5\"></rect><text class=\"ah-swimlane-flows__label\" x=\"190\" y=\"110\" text-anchor=\"middle\" dominant-baseline=\"central\">go</text></g><g data-from=\"b\" data-to=\"c\"><path class=\"ah-swimlane-flows__line\" d=\"M285,139 L285,120 Q285,110 285,110 L285,110 Q285,110 285,100 L285,81\" fill=\"none\" marker-end=\"url(#t-w-arrow)\"></path></g></svg><div class=\"ah-swimlane-grid__lane-band\" style=\"top:0px;height:110px;width:570px\"></div><div class=\"ah-swimlane-grid__lane-band\" data-odd=\"true\" style=\"top:110px;height:110px;width:570px\"></div><div class=\"ah-swimlane-grid__phase-sep\" style=\"left:190px;height:220px\"></div><div class=\"ah-swimlane-grid__phase-sep\" style=\"left:380px;height:220px\"></div><div class=\"ah-swimlane-node\" data-id=\"a\" data-lane=\"l1\" data-phase=\"p1\" data-type=\"start\" data-variant=\"solid\" tabindex=\"0\" role=\"button\" aria-pressed=\"false\" aria-label=\"A\" style=\"left:29px;top:29px;width:132px;height:52px;background-color:#6B7280\"><span class=\"ah-swimlane-node__label\">A</span></div><div class=\"ah-swimlane-node\" data-id=\"b\" data-lane=\"l2\" data-phase=\"p2\" data-type=\"task\" data-variant=\"solid\" tabindex=\"0\" role=\"button\" aria-pressed=\"false\" aria-label=\"B\" style=\"left:219px;top:139px;width:132px;height:52px;background-color:#6B7280\"><span class=\"ah-swimlane-node__label\">B</span></div><div class=\"ah-swimlane-node\" data-id=\"c\" data-lane=\"l1\" data-phase=\"p2\" data-type=\"task\" data-variant=\"solid\" tabindex=\"0\" role=\"button\" aria-pressed=\"false\" aria-label=\"C\" style=\"left:219px;top:29px;width:132px;height:52px;background-color:#6B7280\"><span class=\"ah-swimlane-node__label\">C</span></div></div></div></div></div></div>"};

  async function mount(fx, html) { fx.innerHTML = html; await T.ready(fx); return fx.firstChild; }
  function events(el, name) {
    var seen = [];
    el.addEventListener(name, function (e) {
      if (e.target === el) { seen.push(e.detail === null || e.detail === undefined ? el.getAttribute("data-ah-value") : e.detail); }
    });
    return seen;
  }
  function q(el, sel) { return el.querySelector(sel); }
  function kid(g, sel) { return Array.prototype.find.call(g.children, function (k) { return k.matches(sel); }); }

  T.test("swimlane: select highlights flows; Shift+arrow moves a node", async function (fx) {
    var el = await mount(fx, FX.swim), sel = events(el, "ah:select"), ch = events(el, "ah:node-change");
    var node = function (id) { return q(el, '.ah-swimlane-node[data-id="' + id + '"]'); };
    var g = function (f) { return q(el, '.ah-swimlane-flows g[data-from="' + f + '"]'); };
    node("a").click();
    T.eq(node("a").getAttribute("data-state"), "selected");
    T.eq([g("a").getAttribute("data-active"), g("b").getAttribute("data-dim")], ["true", "true"]);
    T.ok(/-arrow-active\)$/.test(kid(g("a"), "path").getAttribute("marker-end")));
    T.eq(sel, [{ node: "a" }]);
    var c = node("c");
    c.focus();
    T.key(c, "ArrowRight", { shiftKey: true });
    T.eq([c.getAttribute("data-phase"), c.style.left, c.style.top], ["p3", "409px", "29px"]);
    T.eq(ch, [{ node: "c", lane: "l1", phase: "p3", oldLane: "l1", oldPhase: "p2" }]);
    T.eq(kid(g("b"), "path").getAttribute("d").indexOf("M351,165 L"), 0, "flow recomputed");
    AH.invoke(el, "moveNode", "b", "l1", "p3");
    T.eq([node("b").style.top, node("c").style.top], ["-2px", "60px"], "stacked in one cell, in order");
    T.eq(ch.length, 1, "methods fire nothing");
    T.key(node("a"), "ArrowRight");
    T.eq(document.activeElement, node("b"), "arrows move the focus");
  });

  T.test("swimlane: pointer drag moves a node; a press without moving selects; Escape clears", async function (fx) {
    var el = await mount(fx, FX.swim), sel = events(el, "ah:select"), ch = events(el, "ah:node-change");
    var node = function (id) { return q(el, '.ah-swimlane-node[data-id="' + id + '"]'); };
    var a = node("a"), r = a.getBoundingClientRect(), pw = parseFloat(el.getAttribute("data-phase-width")) || 190;
    T.fire(a, "pointerdown", { button: 0, clientX: r.left + 5, clientY: r.top + 5 });
    T.fire(document, "pointerup", { button: 0, clientX: r.left + 5, clientY: r.top + 5 });
    T.eq(sel, [{ node: "a" }], "a press selects");
    T.eq(document.activeElement, a);
    var phase = a.getAttribute("data-phase");
    T.fire(a, "pointerdown", { button: 0, clientX: r.left + 5, clientY: r.top + 5 });
    T.fire(document, "pointermove", { clientX: r.left + 5 + pw, clientY: r.top + 5 });
    T.eq(a.getAttribute("data-state"), "dragging");
    T.fire(document, "pointerup", { clientX: r.left + 5 + pw, clientY: r.top + 5 });
    T.eq(ch.length, 1);
    T.eq([ch[0].node, ch[0].oldPhase], ["a", phase]);
    T.ok(ch[0].phase !== phase, "moved to the next phase");
    T.eq(a.getAttribute("data-state"), "selected", "stays selected");
    T.key(a, "Escape");
    T.eq(sel[sel.length - 1], { node: null });
    T.eq(el.hasAttribute("data-selected"), false);
    AH.invoke(el, "select", "b");
    T.eq(node("b").getAttribute("aria-pressed"), "true");
    T.eq(sel.length, 2, "a drag and select() fire no ah:select");
  });

  T.test("swimlane: lane click; removed and re-inserted, it still works", async function (fx) {
    var el = await mount(fx, FX.swim), lanes = events(el, "ah:lane-click");
    var row = q(el, ".ah-swimlane-lanes__row");
    row.click();
    T.eq(lanes, [{ lane: row.getAttribute("data-lane-id") }]);
    T.eq(el.getAttribute("data-lane"), row.getAttribute("data-lane-id"));
    fx.removeChild(el);
    await new Promise(function (res) { setTimeout(res, 0); });
    fx.appendChild(el);
    await T.ready(fx);
    row.click();
    T.eq(lanes.length, 2, "one listener after re-insertion");
  });
})(window.AHTest, window.AH);
