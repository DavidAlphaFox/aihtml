/* The relation-graph behaviour (aihtml_relation_graph), ported from
 * sigil (data/relation_graph): the panel around a nested chart (the
 * `chart' behaviour of _lib_chart.js draws the graph): node selection
 * (ah:select, data-ah-value), detail card, toolbar (fit, refresh ->
 * ah:refresh), keyboard (arrows, Home/End, Escape) and a live region
 * naming the node.
 */
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;

function canvas(el) { return $(el).children(".ah-relation-graph__canvas")[0]; }

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
  var $card = $(el).children(".ah-relation-graph__detail");
  var $items = $card.find(".ah-relation-graph__detail-item");
  $items.attr("hidden", "hidden");
  var $hit = $items.filter(function () { return this.getAttribute("data-node") === id; });
  $hit.removeAttr("hidden");
  $card.attr("data-visible", id && $hit.length ? "true" : "false");
}

function select(el, id, user) {
  id = id === null || id === undefined ? "" : String(id);
  var changed = el.getAttribute("data-ah-value") !== id;
  el.setAttribute("data-ah-value", id);
  var n = id ? findNode(el, id) : null;
  $(el).children(".ah-relation-graph__live").text(n ? n.name : "");
  showDetail(el, id);
  highlight(el);
  if (user && changed) { $(el).trigger("ah:select", [{ id: id || null }]); }
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

AH.define("relation-graph", {
  init: function (el, $el) {
    var c = canvas(el);
    var focused = false;
    $(c).on("ah:chart-click" + NS, function (e, info) {
      if (!info || info.componentType !== "series" || info.dataType === "edge") { return; }
      var d = info.data || {};
      var id = d.id !== undefined ? String(d.id) : info.name;
      if (!id || id === "__root__") { return; }
      $el.trigger("ah:node-click", [{ id: id }]);
      select(el, id, true);
    });
    $(c).on("ah:chart-ready" + NS, function () {
      highlight(el);
      var f = el.getAttribute("data-ah-focus");
      if (f && !focused) {
        focused = true;
        // a force layout settles for a while first
        setTimeout(function () { focusNode(el, f); },
                   el.getAttribute("data-layout") === "force" ? 800 : 50);
      }
    });
    $el.on("click" + NS, ".ah-relation-graph__tool", function () {
      var act = this.getAttribute("data-act");
      AH.invoke(c, "resetView");
      if (act === "refresh") { $el.trigger("ah:refresh"); }
    });
    $el.on("click" + NS, ".ah-relation-graph__detail-close", function () {
      select(el, null, true);
      el.focus();
    });
    $el.on("keydown" + NS, function (e) {
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
  },
  methods: {
    select: function (el, $el, id) { select(el, id, false); },
    getSelected: function (el) { return el.getAttribute("data-ah-value") || null; },
    focus: function (el, $el, id) { focusNode(el, id); },
    fit: function (el) { AH.invoke(canvas(el), "resetView"); },
    setOption: function (el, $el, option, notMerge) {
      AH.invoke(canvas(el), "setOption", option, notMerge);
      highlight(el);
    }
  }
});
