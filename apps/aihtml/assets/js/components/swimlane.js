/* Behaviour of the swimlane (designs/04-components.md). Ported from
 * sigil: data/swimlane (+ layout).
 *
 * The grid, the nodes and the flow lines (SVG paths and labels) are
 * rendered by aihtml_swimlane. The behaviour selects nodes
 * (highlighting their flows), syncs the scroll areas and, with `editable',
 * drags nodes to another cell: it restacks the cells and recomputes the
 * flow paths of the existing SVG elements (twins of flow_points/2 and
 * points_path/2 in the Erlang module), then fires ah:node-change.
 *
 * Events (detail, also written to the root as data-*): ah:select
 * ({node}, node null when cleared), ah:node-click ({node}),
 * ah:lane-click ({lane}), ah:node-change ({node, lane, phase, oldLane,
 * oldPhase}). */
import AH from "../core.js";

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

function ids(els, attr) { return Array.prototype.map.call(els, function (n) { return n.getAttribute(attr); }); }
function lanes(el) { return ids(el.querySelectorAll(".ah-swimlane-lanes__row"), "data-lane-id"); }
function phases(el) { return ids(el.querySelectorAll(".ah-swimlane-grid__phase"), "data-phase-id"); }
function nodes(el) { return Array.from(el.querySelectorAll(".ah-swimlane-node")); }
function nodeById(el, id) {
  return nodes(el).find(function (n) { return n.getAttribute("data-id") === String(id); });
}
function kids(node, sel) {
  return Array.prototype.filter.call(node.children, function (k) { return k.matches(sel); });
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
  nodes(el).forEach(function (node) { count[cell(node)] = (count[cell(node)] || 0) + 1; });
  nodes(el).forEach(function (node) {
    var l = li[node.getAttribute("data-lane")];
    if (l === undefined) { return; }
    if (c.continuous) {
      node.style.top = fmt(l * c.lh + (c.lh - c.nh) / 2) + "px";
      return;
    }
    var p = pi[node.getAttribute("data-phase")], k = cell(node), i = seen[k] || 0, n = count[k];
    seen[k] = i + 1;
    if (p === undefined) { return; }
    node.style.left = fmt(p * c.pw + (c.pw - c.nw) / 2) + "px";
    node.style.top = fmt(l * c.lh + (c.lh - (n * c.nh + (n - 1) * GAP)) / 2 + i * (c.nh + GAP)) + "px";
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
  el.querySelectorAll(".ah-swimlane-flows g").forEach(function (g) {
    var a = nodeById(el, g.getAttribute("data-from")), b = nodeById(el, g.getAttribute("data-to"));
    if (!a || !b) { return; }
    var pts = flowPoints(box(a), box(b)), n = pts.length, i = Math.floor((n - 1) / 2);
    var m = [(pts[i][0] + pts[Math.min(i + 1, n - 1)][0]) / 2, (pts[i][1] + pts[Math.min(i + 1, n - 1)][1]) / 2];
    kids(g, ".ah-swimlane-flows__line").forEach(function (l) { l.setAttribute("d", pointsPath(pts, 10)); });
    var bg = kids(g, ".ah-swimlane-flows__label-bg");
    if (bg.length) {
      var w = parseFloat(bg[0].getAttribute("width"));
      bg.forEach(function (x) {
        x.setAttribute("x", fmt(m[0] - w / 2));
        x.setAttribute("y", fmt(m[1] - 9));
      });
      kids(g, ".ah-swimlane-flows__label").forEach(function (x) {
        x.setAttribute("x", fmt(m[0]));
        x.setAttribute("y", fmt(m[1]));
      });
    }
  });
}

function select(el, id) {
  var sid = id === null || id === undefined ? null : String(id);
  if (sid) { el.setAttribute("data-selected", sid); } else { el.removeAttribute("data-selected"); }
  nodes(el).forEach(function (node) {
    var on = node.getAttribute("data-id") === sid;
    if (on) { node.setAttribute("data-state", "selected"); } else if (node.getAttribute("data-state") === "selected") { node.removeAttribute("data-state"); }
    node.setAttribute("aria-pressed", on ? "true" : "false");
  });
  el.querySelectorAll(".ah-swimlane-flows g").forEach(function (g) {
    var active = !!sid && (g.getAttribute("data-from") === sid || g.getAttribute("data-to") === sid);
    if (active) { g.setAttribute("data-active", "true"); } else { g.removeAttribute("data-active"); }
    if (sid && !active) { g.setAttribute("data-dim", "true"); } else { g.removeAttribute("data-dim"); }
    var path = kids(g, ".ah-swimlane-flows__line")[0], m = path && path.getAttribute("marker-end");
    if (m) { path.setAttribute("marker-end", m.replace(/-arrow(-active)?\)$/, active ? "-arrow-active)" : "-arrow)")); }
  });
}

function pick(ctl, node) {
  var el = ctl.element, id = node.getAttribute("data-id");
  select(el, id);
  el.setAttribute("data-node", id);
  ctl.fire("ah:select", { node: id });
  ctl.fire("ah:node-click", { node: id });
}

// Move a node to another cell; fires ah:node-change when asked to.
function move(ctl, node, lane, phase, notify) {
  var el = ctl.element;
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
    ctl.fire("ah:node-change", d);
  }
}

function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }

function dragStart(ctl, node, e) {
  var el = ctl.element, c = conf(el), st = ctl.st;
  if (!c.editable || e.button !== 0) { return; }
  e.preventDefault();
  var d = { node: node, x: e.clientX, y: e.clientY, x0: parseFloat(node.style.left),
            y0: parseFloat(node.style.top), moved: false };
  st.drag = d;
  if (st.dragAc) { st.dragAc.abort(); }
  var ac = st.dragAc = new AbortController(), o = { signal: ac.signal };
  document.addEventListener("pointermove", function (me) {
    var dx = me.clientX - d.x, dy = me.clientY - d.y;
    if (!d.moved && Math.abs(dx) + Math.abs(dy) < 4) { return; }
    d.moved = true;
    node.setAttribute("data-state", "dragging");
    node.style.left = (d.x0 + dx) + "px";
    node.style.top = (d.y0 + dy) + "px";
  }, o);
  var up = function (ue) {
    ac.abort();
    st.dragAc = null;
    st.drag = null;
    if (!d.moved) { pick(ctl, node); node.focus(); return; }
    node.removeAttribute("data-state");
    if (node.getAttribute("aria-pressed") === "true") { node.setAttribute("data-state", "selected"); }
    var ls = lanes(el), ps = phases(el);
    var li = clamp(ls.indexOf(node.getAttribute("data-lane")) + Math.round((ue.clientY - d.y) / c.lh), 0, ls.length - 1);
    var phase = null;
    if (!c.continuous) {
      phase = ps[clamp(ps.indexOf(node.getAttribute("data-phase")) + Math.round((ue.clientX - d.x) / c.pw),
                       0, ps.length - 1)];
    }
    move(ctl, node, ls[li], phase, true);
  };
  document.addEventListener("pointerup", up, o);
  document.addEventListener("pointercancel", up, o);
}

function nodeKey(ctl, node, e) {
  var el = ctl.element, c = conf(el), k = e.key;
  if (k === "Enter" || k === " ") { e.preventDefault(); pick(ctl, node); return; }
  if (k === "Escape") { select(el, null); el.removeAttribute("data-node"); ctl.fire("ah:select", { node: null }); return; }
  var dir = { ArrowLeft: [-1, 0], ArrowRight: [1, 0], ArrowUp: [0, -1], ArrowDown: [0, 1] }[k];
  if (!dir) { return; }
  e.preventDefault();
  if (e.shiftKey && c.editable) {
    var ls = lanes(el), ps = phases(el);
    var li = clamp(ls.indexOf(node.getAttribute("data-lane")) + dir[1], 0, ls.length - 1);
    var phase = c.continuous ? null
      : ps[clamp(ps.indexOf(node.getAttribute("data-phase")) + dir[0], 0, ps.length - 1)];
    move(ctl, node, ls[li], phase, true);
    node.focus();
    return;
  }
  // focus the nearest node in that direction
  var b = box(node), best = null, bestD = Infinity;
  nodes(el).forEach(function (other) {
    if (other === node) { return; }
    var o = box(other), dx = (o.x + o.w / 2) - (b.x + b.w / 2), dy = (o.y + o.h / 2) - (b.y + b.h / 2);
    var along = dir[0] ? dx * dir[0] : dy * dir[1], across = dir[0] ? Math.abs(dy) : Math.abs(dx);
    if (along <= 0) { return; }
    var dist = along + across * 2;
    if (dist < bestD) { bestD = dist; best = other; }
  });
  if (best) { best.focus(); }
}

AH.register("swimlane", class extends AH.Controller {
  setup() {
    var ctl = this, el = this.element;
    this.st = { drag: null, dragAc: null };
    var body = el.querySelector(".ah-swimlane-grid__body");
    if (body) {
      this.listen(body, "scroll", function () {
        var head = el.querySelector(".ah-swimlane-grid__header"), lanesBody = el.querySelector(".ah-swimlane-lanes__body");
        if (head) { head.scrollLeft = body.scrollLeft; }
        if (lanesBody) { lanesBody.scrollTop = body.scrollTop; }
      });
    }
    this.delegate("pointerdown", ".ah-swimlane-node", function (e, node) { dragStart(ctl, node, e); });
    this.delegate("click", ".ah-swimlane-node", function (e, node) {
      // pointer clicks are handled on pointerup when editable; keyboard
      // and script clicks (detail 0) select here
      if (!conf(el).editable || e.detail === 0) { pick(ctl, node); }
    });
    this.delegate("keydown", ".ah-swimlane-node", function (e, node) { nodeKey(ctl, node, e); });
    this.delegate("click", ".ah-swimlane-lanes__row", function (e, row) {
      el.setAttribute("data-lane", row.getAttribute("data-lane-id"));
      ctl.fire("ah:lane-click", { lane: row.getAttribute("data-lane-id") });
    });
  }

  teardown() {
    if (this.st.dragAc) { this.st.dragAc.abort(); this.st.dragAc = null; }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  select(id) { select(this.element, id); }
  moveNode(id, lane, phase) {
    var n = nodeById(this.element, id);
    if (n) { move(this, n, String(lane), phase === undefined || phase === null ? null : String(phase), false); }
  }
});
