/* Behaviour of the swimlane (designs/04-components.md). Ported from
 * sigil: data/swimlane (+ layout).
 *
 * The grid, the nodes and the flow lines (SVG paths and labels) are
 * rendered by aihtml_swimlane. The behaviour selects nodes
 * (highlighting their flows), syncs the scroll areas and, with `editable',
 * drags nodes to another cell: it restacks the cells and recomputes the
 * flow paths of the existing SVG elements (twins of flow_points/2 and
 * points_path/2 in the Erlang module), then fires ah:node-change. */
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;
var GAP = 10;

function conf(el) {
  var a = function (k, d) { return parseFloat(el.getAttribute("data-" + k)) || d; };
  return {
    lh: a("lane-height", 110), pw: a("phase-width", 190),
    nw: a("node-width", 132), nh: a("node-height", 52),
    continuous: el.getAttribute("data-axis") === "continuous",
    editable: el.hasAttribute("data-editable")
  };
}

function ids($els, attr) { return $els.map(function () { return this.getAttribute(attr); }).get(); }
function lanes(el) { return ids($(el).find(".ah-swimlane-lanes__row"), "data-lane-id"); }
function phases(el) { return ids($(el).find(".ah-swimlane-grid__phase"), "data-phase-id"); }
function nodes(el) { return $(el).find(".ah-swimlane-node"); }
function nodeById(el, id) {
  return nodes(el).filter(function () { return this.getAttribute("data-id") === String(id); })[0];
}
function box(n) {
  return { x: parseFloat(n.style.left), y: parseFloat(n.style.top),
           w: parseFloat(n.style.width), h: parseFloat(n.style.height) };
}
function fmt(n) { return Math.round(n * 100) / 100; }

// Stack the nodes of each cell, centred in the lane (sigil's assign-boxes).
function layout(el) {
  var c = conf(el), li = {}, pi = {}, count = {}, seen = {};
  lanes(el).forEach(function (id, i) { li[id] = i; });
  phases(el).forEach(function (id, i) { pi[id] = i; });
  var cell = function (n) { return n.getAttribute("data-lane") + "\u0000" + n.getAttribute("data-phase"); };
  nodes(el).each(function () { count[cell(this)] = (count[cell(this)] || 0) + 1; });
  nodes(el).each(function () {
    var l = li[this.getAttribute("data-lane")];
    if (l === undefined) { return; }
    if (c.continuous) {
      this.style.top = fmt(l * c.lh + (c.lh - c.nh) / 2) + "px";
      return;
    }
    var p = pi[this.getAttribute("data-phase")], k = cell(this), i = seen[k] || 0, n = count[k];
    seen[k] = i + 1;
    if (p === undefined) { return; }
    this.style.left = fmt(p * c.pw + (c.pw - c.nw) / 2) + "px";
    this.style.top = fmt(l * c.lh + (c.lh - (n * c.nh + (n - 1) * GAP)) / 2 + i * (c.nh + GAP)) + "px";
  });
  drawFlows(el);
}

// sigil's flow-points (twin of flow_points/2)
function flowPoints(s, t) {
  var sx = s.x + s.w, sy = s.y + s.h / 2, tx = t.x, ty = t.y + t.h / 2;
  var scx = s.x + s.w / 2, sty = s.y, sby = s.y + s.h;
  var tcx = t.x + t.w / 2, tty = t.y, tby = t.y + t.h;
  var overlap = Math.abs(scx - tcx) < Math.max(s.w, t.w);
  var below = tty >= sby, above = tby <= sty;
  var gap = below ? tty - sby : above ? sty - tby : 0;
  var tight = overlap && (below || above) && gap < 24, mx, my;
  if (tx >= sx + 8) { mx = (sx + tx) / 2; return [[sx, sy], [mx, sy], [mx, ty], [tx, ty]]; }
  if (tight) { var lx = Math.min(s.x, t.x) - 24; return [[s.x, sy], [lx, sy], [lx, ty], [t.x, ty]]; }
  if (overlap && below) { my = (sby + tty) / 2; return [[scx, sby], [scx, my], [tcx, my], [tcx, tty]]; }
  if (overlap && above) { my = (sty + tby) / 2; return [[scx, sty], [scx, my], [tcx, my], [tcx, tby]]; }
  var txr = t.x + t.w;
  mx = (s.x + txr) / 2;
  return [[s.x, sy], [mx, sy], [mx, ty], [txr, ty]];
}

function len(a, b) { return Math.hypot(b[0] - a[0], b[1] - a[1]); }
function lerp(a, b, t) { return [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t]; }

// sigil's points->path: rounded corners (twin of points_path/2)
function pointsPath(pts, radius) {
  var out = ["M" + fmt(pts[0][0]) + "," + fmt(pts[0][1])];
  for (var i = 1; i < pts.length; i++) {
    var p = pts[i];
    if (i === pts.length - 1) { out.push("L" + fmt(p[0]) + "," + fmt(p[1])); continue; }
    var prev = pts[i - 1], next = pts[i + 1], l1 = len(prev, p), l2 = len(p, next);
    var r1 = Math.min(radius, l1 / 2), r2 = Math.min(radius, l2 / 2);
    var a = l1 > 0 ? lerp(prev, p, (l1 - r1) / l1) : p, b = l2 > 0 ? lerp(p, next, r2 / l2) : p;
    out.push("L" + fmt(a[0]) + "," + fmt(a[1]) + " Q" + fmt(p[0]) + "," + fmt(p[1]) +
             " " + fmt(b[0]) + "," + fmt(b[1]));
  }
  return out.join(" ");
}

function drawFlows(el) {
  $(el).find(".ah-swimlane-flows g").each(function () {
    var a = nodeById(el, this.getAttribute("data-from")), b = nodeById(el, this.getAttribute("data-to"));
    if (!a || !b) { return; }
    var pts = flowPoints(box(a), box(b)), n = pts.length, i = Math.floor((n - 1) / 2);
    var m = [(pts[i][0] + pts[Math.min(i + 1, n - 1)][0]) / 2, (pts[i][1] + pts[Math.min(i + 1, n - 1)][1]) / 2];
    $(this).children(".ah-swimlane-flows__line").attr("d", pointsPath(pts, 10));
    var $bg = $(this).children(".ah-swimlane-flows__label-bg");
    if ($bg.length) {
      var w = parseFloat($bg.attr("width"));
      $bg.attr({ x: fmt(m[0] - w / 2), y: fmt(m[1] - 9) });
      $(this).children(".ah-swimlane-flows__label").attr({ x: fmt(m[0]), y: fmt(m[1]) });
    }
  });
}

function select(el, id) {
  var sid = id === null || id === undefined ? null : String(id);
  if (sid) { el.setAttribute("data-selected", sid); } else { el.removeAttribute("data-selected"); }
  nodes(el).each(function () {
    var on = this.getAttribute("data-id") === sid;
    if (on) { this.setAttribute("data-state", "selected"); } else if (this.getAttribute("data-state") === "selected") { this.removeAttribute("data-state"); }
    this.setAttribute("aria-pressed", on ? "true" : "false");
  });
  $(el).find(".ah-swimlane-flows g").each(function () {
    var active = !!sid && (this.getAttribute("data-from") === sid || this.getAttribute("data-to") === sid);
    if (active) { this.setAttribute("data-active", "true"); } else { this.removeAttribute("data-active"); }
    if (sid && !active) { this.setAttribute("data-dim", "true"); } else { this.removeAttribute("data-dim"); }
    var path = $(this).children(".ah-swimlane-flows__line")[0], m = path && path.getAttribute("marker-end");
    if (m) { path.setAttribute("marker-end", m.replace(/-arrow(-active)?\)$/, active ? "-arrow-active)" : "-arrow)")); }
  });
}

function pick(el, node) {
  var id = node.getAttribute("data-id");
  select(el, id);
  el.setAttribute("data-node", id);
  $(el).trigger("ah:select", [{ node: id }]);
  $(el).trigger("ah:node-click", [{ node: id }]);
}

// Move a node to another cell; fires ah:node-change when asked to.
function move(el, node, lane, phase, notify) {
  var old = { lane: node.getAttribute("data-lane"), phase: node.getAttribute("data-phase") };
  node.setAttribute("data-lane", lane);
  if (phase !== undefined && phase !== null) { node.setAttribute("data-phase", phase); }
  layout(el);
  if (notify && (old.lane !== lane || old.phase !== node.getAttribute("data-phase"))) {
    var d = { node: node.getAttribute("data-id"), lane: lane, phase: node.getAttribute("data-phase") || "",
              oldLane: old.lane, oldPhase: old.phase || "" };
    el.setAttribute("data-node", d.node);
    el.setAttribute("data-lane", d.lane);
    el.setAttribute("data-phase", d.phase);
    el.setAttribute("data-old-lane", d.oldLane);
    el.setAttribute("data-old-phase", d.oldPhase);
    $(el).trigger("ah:node-change", [d]);
  }
}

function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }

function dragStart(el, node, e) {
  var c = conf(el), st = $.data(el, "ah-swimlane");
  if (!c.editable || e.button !== 0) { return; }
  e.preventDefault();
  var d = { node: node, x: e.clientX, y: e.clientY, x0: parseFloat(node.style.left),
            y0: parseFloat(node.style.top), moved: false };
  st.drag = d;
  $(document).on("pointermove" + st.ns, function (me) {
    var dx = me.clientX - d.x, dy = me.clientY - d.y;
    if (!d.moved && Math.abs(dx) + Math.abs(dy) < 4) { return; }
    d.moved = true;
    node.setAttribute("data-state", "dragging");
    node.style.left = (d.x0 + dx) + "px";
    node.style.top = (d.y0 + dy) + "px";
  }).on("pointerup" + st.ns + " pointercancel" + st.ns, function (ue) {
    $(document).off(st.ns);
    st.drag = null;
    if (!d.moved) { pick(el, node); node.focus(); return; }
    node.removeAttribute("data-state");
    if (node.getAttribute("aria-pressed") === "true") { node.setAttribute("data-state", "selected"); }
    var ls = lanes(el), ps = phases(el);
    var li = clamp(ls.indexOf(node.getAttribute("data-lane")) + Math.round((ue.clientY - d.y) / c.lh), 0, ls.length - 1);
    var phase = null;
    if (!c.continuous) {
      phase = ps[clamp(ps.indexOf(node.getAttribute("data-phase")) + Math.round((ue.clientX - d.x) / c.pw),
                       0, ps.length - 1)];
    }
    move(el, node, ls[li], phase, true);
  });
}

function nodeKey(el, node, e) {
  var c = conf(el), k = e.key;
  if (k === "Enter" || k === " ") { e.preventDefault(); pick(el, node); return; }
  if (k === "Escape") { select(el, null); el.removeAttribute("data-node"); $(el).trigger("ah:select", [{ node: null }]); return; }
  var dir = { ArrowLeft: [-1, 0], ArrowRight: [1, 0], ArrowUp: [0, -1], ArrowDown: [0, 1] }[k];
  if (!dir) { return; }
  e.preventDefault();
  if (e.shiftKey && c.editable) {
    var ls = lanes(el), ps = phases(el);
    var li = clamp(ls.indexOf(node.getAttribute("data-lane")) + dir[1], 0, ls.length - 1);
    var phase = c.continuous ? null
      : ps[clamp(ps.indexOf(node.getAttribute("data-phase")) + dir[0], 0, ps.length - 1)];
    move(el, node, ls[li], phase, true);
    node.focus();
    return;
  }
  // focus the nearest node in that direction
  var b = box(node), best = null, bestD = Infinity;
  nodes(el).each(function () {
    if (this === node) { return; }
    var o = box(this), dx = (o.x + o.w / 2) - (b.x + b.w / 2), dy = (o.y + o.h / 2) - (b.y + b.h / 2);
    var along = dir[0] ? dx * dir[0] : dy * dir[1], across = dir[0] ? Math.abs(dy) : Math.abs(dx);
    if (along <= 0) { return; }
    var dist = along + across * 2;
    if (dist < bestD) { bestD = dist; best = this; }
  });
  if (best) { best.focus(); }
}

AH.define("swimlane", {
  init: function (el, $el) {
    var st = { ns: NS + "-swim" + Math.random().toString(36).slice(2) };
    $.data(el, "ah-swimlane", st);
    var body = $el.find(".ah-swimlane-grid__body")[0];
    if (body) {
      $(body).on("scroll" + NS, function () {
        $el.find(".ah-swimlane-grid__header")[0].scrollLeft = this.scrollLeft;
        $el.find(".ah-swimlane-lanes__body")[0].scrollTop = this.scrollTop;
      });
    }
    $el.on("pointerdown" + NS, ".ah-swimlane-node", function (e) { dragStart(el, this, e); });
    $el.on("click" + NS, ".ah-swimlane-node", function (e) {
      // pointer clicks are handled on pointerup when editable
      if (!conf(el).editable || !e.originalEvent || e.detail === 0) { pick(el, this); }
    });
    $el.on("keydown" + NS, ".ah-swimlane-node", function (e) { nodeKey(el, this, e); });
    $el.on("click" + NS, ".ah-swimlane-lanes__row", function () {
      el.setAttribute("data-lane", this.getAttribute("data-lane-id"));
      $(el).trigger("ah:lane-click", [{ lane: this.getAttribute("data-lane-id") }]);
    });
  },
  destroy: function (el, $el) {
    var st = $.data(el, "ah-swimlane");
    if (st) { $(document).off(st.ns); }
    $el.find(".ah-swimlane-grid__body").off(NS);
    $.removeData(el, "ah-swimlane");
  },
  methods: {
    select: function (el, $el, id) { select(el, id); },
    moveNode: function (el, $el, id, lane, phase) {
      var n = nodeById(el, id);
      if (n) { move(el, n, String(lane), phase === undefined || phase === null ? null : String(phase), false); }
    }
  }
});
