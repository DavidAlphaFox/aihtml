/* The relation-graph behaviour (aihtml_relation_graph), ported from
 * sigil (data/relation_graph): the panel around a nested chart (the
 * `chart' behaviour of _lib_chart.js draws the graph): node selection
 * (ah:select, data-ah-value), detail card, toolbar (fit, refresh ->
 * ah:refresh), keyboard (arrows, Home/End, Escape) and a live region
 * naming the node. The visually hidden table of nodes and links beside
 * the canvas (aihtml_lib_chart:data_text/2) follows the graph's data:
 * the server's new table when setOption brings one (chart_update/3),
 * else rebuilt from the canvas's option (ah:chart-data).
 *
 * Events (native CustomEvents on the root, bubbling):
 *   ah:select      detail: {id} (null when cleared), user selections only
 *   ah:node-click  detail: {id}
 *   ah:refresh     no detail
 */
import AH from "../core.js";
import { updateText } from "./_lib_chart.js";

function canvas(el) { return el.querySelector(":scope > .ah-relation-graph__canvas"); }

// The nodes in data order: [{id, name, index}]; tree nodes depth first.
function nodes(el) {
  var o = AH.invoke(canvas(el), "getOption");
  var sr = o && o.series && o.series[0];
  if (!sr) { return []; }
  var out = [];
  if (sr.type === "tree") {
    (function walk(list) {
      (list || []).forEach(function (n) {
        if (n.id !== "__root__") { out.push({ id: String(n.id), name: n.name }); }
        walk(n.children);
      });
    })(sr.data);
  } else {
    (sr.data || []).forEach(function (n, i) {
      out.push({ id: String(n.id !== undefined ? n.id : n.name), name: n.name, index: i });
    });
  }
  return out;
}

function findNode(el, id) {
  var list = nodes(el);
  for (var i = 0; i < list.length; i++) {
    if (list[i].id === id) { return list[i]; }
  }
  return null;
}

function highlight(el) {
  var c = canvas(el);
  var id = el.getAttribute("data-ah-value");
  AH.invoke(c, "dispatchAction", { type: "downplay", seriesIndex: 0 });
  var n = id ? findNode(el, id) : null;
  if (n) {
    AH.invoke(c, "dispatchAction", n.index !== undefined
      ? { type: "highlight", seriesIndex: 0, dataIndex: n.index }
      : { type: "highlight", seriesIndex: 0, name: n.name });
  }
}

function showDetail(el, id) {
  var card = el.querySelector(":scope > .ah-relation-graph__detail");
  if (!card) { return; }
  var hits = 0;
  card.querySelectorAll(".ah-relation-graph__detail-item").forEach(function (item) {
    var hit = item.getAttribute("data-node") === id;
    if (hit) { hits++; item.removeAttribute("hidden"); }
    else { item.setAttribute("hidden", "hidden"); }
  });
  card.setAttribute("data-visible", id && hits ? "true" : "false");
}

function select(el, id, user) {
  id = id === null || id === undefined ? "" : String(id);
  var changed = el.getAttribute("data-ah-value") !== id;
  el.setAttribute("data-ah-value", id);
  var n = id ? findNode(el, id) : null;
  el.querySelectorAll(":scope > .ah-relation-graph__live").forEach(function (live) {
    live.textContent = n ? n.name : "";
  });
  showDetail(el, id);
  highlight(el);
  if (user && changed) {
    el.dispatchEvent(new CustomEvent("ah:select", { bubbles: true, cancelable: true,
                                                    detail: { id: id || null } }));
  }
}

// Pan so that the node is in the middle (graph layouts only).
function focusNode(el, id) {
  var chart = AH.invoke(canvas(el), "instance");
  var n = id ? findNode(el, String(id)) : null;
  if (!chart || !n || n.index === undefined) { return; }
  var sm = chart.getModel().getSeriesByIndex(0);
  var layout = sm && sm.getData().getItemLayout(n.index);
  if (!layout) { return; }
  var p = chart.convertToPixel({ seriesIndex: 0 }, layout);
  if (!p) { return; }
  chart.dispatchAction({ type: "graphRoam", seriesIndex: 0,
                         dx: chart.getWidth() / 2 - p[0], dy: chart.getHeight() / 2 - p[1] });
}

AH.register("relation-graph", class extends AH.Controller {
  setup() {
    var el = this.element, self = this;
    var c = canvas(el);
    var focused = false;
    var label = el.getAttribute("aria-label");
    this.listen(c, "ah:chart-click", function (e) {
      var info = e.detail;
      if (!info || info.componentType !== "series" || info.dataType === "edge") { return; }
      var d = info.data || {};
      var id = d.id !== undefined ? String(d.id) : info.name;
      if (!id || id === "__root__") { return; }
      self.fire("ah:node-click", { id: id });
      select(el, id, true);
    });
    this.listen(c, "ah:chart-ready", function () {
      highlight(el);
      var f = el.getAttribute("data-ah-focus");
      if (f && !focused) {
        focused = true;
        // a force layout settles for a while first
        setTimeout(function () { if (self.signal && !self.signal.aborted) { focusNode(el, f); } },
                   el.getAttribute("data-layout") === "force" ? 800 : 50);
      }
    });
    this.listen(c, "ah:chart-data", function (e) {
      var html = e.detail && e.detail.text;
      if (typeof html === "string") { updateText(el, c, null, html, label); }
      else { updateText(el, c, AH.invoke(c, "getOption"), undefined, label); }
    });
    this.delegate("click", ".ah-relation-graph__tool", function (e, tool) {
      var act = tool.getAttribute("data-act");
      AH.invoke(c, "resetView");
      if (act === "refresh") { self.fire("ah:refresh"); }
    });
    this.delegate("click", ".ah-relation-graph__detail-close", function () {
      select(el, null, true);
      el.focus();
    });
    this.listen(el, "keydown", function (e) {
      if (e.target !== el) { return; }
      var list = nodes(el);
      if (!list.length) { return; }
      var cur = el.getAttribute("data-ah-value");
      var i = -1;
      list.forEach(function (n, j) { if (n.id === cur) { i = j; } });
      var next = null;
      switch (e.key) {
        case "ArrowRight": case "ArrowDown": next = list[(i + 1) % list.length]; break;
        case "ArrowLeft": case "ArrowUp": next = list[i <= 0 ? list.length - 1 : i - 1]; break;
        case "Home": next = list[0]; break;
        case "End": next = list[list.length - 1]; break;
        case "Escape":
          if (!cur) { return; }
          e.preventDefault();
          select(el, null, true);
          return;
        default: return;
      }
      e.preventDefault();
      select(el, next.id, true);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  select(id) { select(this.element, id, false); }
  getSelected() { return this.element.getAttribute("data-ah-value") || null; }
  focus(id) { focusNode(this.element, id); }
  fit() { AH.invoke(canvas(this.element), "resetView"); }
  setOption(option, notMerge, html) {
    AH.invoke(canvas(this.element), "setOption", option, notMerge, html);
    highlight(this.element);
  }
});
