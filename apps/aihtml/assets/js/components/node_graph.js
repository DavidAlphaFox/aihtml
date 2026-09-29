/* node_graph: the node-graph behaviour (sigil data/node_graph), on markup
 * rendered by aihtml_node_graph:node_graph/3.
 *
 * The root's data-ah-value holds the graph as JSON ({nodes, links,
 * groups}); the server rendered the cards, links and groups with the
 * shared templates node_graph_{node,link,group}. This file keeps the graph
 * in memory, redraws with the same templates (AH.tpl.node_graph_*) and
 * after every edit writes data-ah-value, sets data-op / data-changed /
 * data-removed on the root and fires change. Selection changes fire
 * ah:selection-change (data-selection), a link dropped on empty canvas
 * without a node library fires ah:link-drop (data-origin, data-point).
 *
 * Coordinates: graph units are the nodes' world coordinates; the camera
 * {x, y, z} maps them to the viewport: screen = (graph + [x, y]) * z. A
 * card's slots sit 30 (header) + 4 + 20 * i + 10 below its top, as in
 * node_graph.css and the Erlang module.
 */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_values.js";
import "virtual:ah-tpl/node_graph_group";
import "virtual:ah-tpl/node_graph_link";
import "virtual:ah-tpl/node_graph_menu";
import "virtual:ah-tpl/node_graph_minimap";
import "virtual:ah-tpl/node_graph_node";
import "virtual:ah-tpl/node_graph_search";

var TITLE_H = 30, SLOT_H = 20, PAD_TOP = 4, PAD_BOTTOM = 8;
var DEFAULT_W = 240, MIN_W = 225, MIN_H = 60;
var GRID_DISPLAY = 50, MIN_Z = 0.1, MAX_Z = 4;
var SNAP_RADIUS = 22, MINI_W = 176, MINI_H = 120, MINI_PAD = 8;
var KEY = "ahNodeGraph";
var FALLBACK = "var(--ah-datatype-default, #aaa)";
var TWO_SLICES = ["M0 50 A 50 50 0 0 1 100 50", "M100 50 A 50 50 0 0 1 0 50"];
var THREE_SLICES = ["M0 50A50 50 0 0 0 75 93L50 50", "M75 93A50 50 0 0 0 75 7L50 50",
                    "M75 7A50 50 0 0 0 0 50L50 50"];
var SVG_NS = "http://www.w3.org/2000/svg";
var seq = 0;

// ------------------------------------------------------------------
// Model (sigil node_graph/model): pure functions on {nodes, links,
// groups}; nodes are in z order, the last on top.
// ------------------------------------------------------------------

function normType(t) {
  var s = t === undefined || t === null ? "" : String(t).trim();
  return s === "" ? "*" : s.toUpperCase();
}

function typeSet(t) {
  var s = normType(t);
  if (s === "*") { return ["*"]; }
  return s.split(",").map(function (x) { return x.trim(); }).filter(Boolean);
}

function typeCompatible(a, b) {
  var sa = typeSet(a), sb = typeSet(b);
  if (sa.indexOf("*") >= 0 || sb.indexOf("*") >= 0) { return true; }
  return sa.some(function (x) { return sb.indexOf(x) >= 0; });
}

function findNode(g, id) {
  for (var i = 0; i < g.nodes.length; i++) {
    if (g.nodes[i].id === id) { return g.nodes[i]; }
  }
  return null;
}

function findLink(g, id) {
  for (var i = 0; i < g.links.length; i++) {
    if (g.links[i].id === id) { return g.links[i]; }
  }
  return null;
}

function findGroup(g, id) {
  for (var i = 0; i < g.groups.length; i++) {
    if (g.groups[i].id === id) { return g.groups[i]; }
  }
  return null;
}

function slotOf(g, id, kind, i) {
  var n = findNode(g, id);
  if (!n) { return null; }
  return (kind === "input" ? n.inputs : n.outputs)[i] || null;
}

function inputLink(g, id, i) {
  for (var k = 0; k < g.links.length; k++) {
    var l = g.links[k];
    if (l.target[0] === id && l.target[1] === i) { return l; }
  }
  return null;
}

function slotKey(id, kind, i) { return id + ":" + (kind === "input" ? "i" : "o") + i; }

// Can we walk the links from `from' to `to' (cycle check)?
function downstream(g, from, to) {
  var todo = [from], seen = {};
  while (todo.length) {
    var cur = todo.shift();
    if (cur === to) { return true; }
    if (seen[cur]) { continue; }
    seen[cur] = true;
    g.links.forEach(function (l) { if (l.source[0] === cur) { todo.push(l.target[0]); } });
  }
  return false;
}

// null when output `from' [id, i] may connect to input `to', else why not
function connectProblem(g, from, to, allowCycles) {
  var out = slotOf(g, from[0], "output", from[1]);
  var inp = slotOf(g, to[0], "input", to[1]);
  if (!out) { return "no-such-output"; }
  if (!inp) { return "no-such-input"; }
  if (from[0] === to[0]) { return "self-connection"; }
  if (!typeCompatible(out.type, inp.type)) { return "type-mismatch"; }
  if (!allowCycles && downstream(g, to[0], from[0])) { return "cycle"; }
  return null;
}

function freshId(used, prefix) {
  for (var i = 1; ; i++) {
    if (!used[prefix + i]) { return prefix + i; }
  }
}

function ids(list) {
  var o = {};
  list.forEach(function (x) { o[x.id] = true; });
  return o;
}

// Connect (mutating g); an input's previous link is replaced (ComfyUI).
function connect(g, from, to, allowCycles) {
  if (connectProblem(g, from, to, allowCycles)) { return null; }
  var id = freshId(ids(g.links), "l");
  var old = inputLink(g, to[0], to[1]);
  if (old) { g.links = g.links.filter(function (l) { return l !== old; }); }
  g.links.push({ id: id, source: [from[0], from[1]], target: [to[0], to[1]] });
  return id;
}

function removeNodes(g, list) {
  var dead = {};
  list.forEach(function (id) { dead[id] = true; });
  g.nodes = g.nodes.filter(function (n) { return !dead[n.id]; });
  g.links = g.links.filter(function (l) { return !dead[l.source[0]] && !dead[l.target[0]]; });
}

function extract(g, list) {
  var s = {};
  list.forEach(function (id) { s[id] = true; });
  return {
    nodes: g.nodes.filter(function (n) { return s[n.id]; }),
    links: g.links.filter(function (l) { return s[l.source[0]] && s[l.target[0]]; })
  };
}

// Insert a sub-graph with fresh ids, offset by [dx, dy]; returns the id map.
function insert(g, sub, dx, dy) {
  var map = {};
  sub.nodes.forEach(function (n) {
    var c = clone(n);
    c.id = freshId(ids(g.nodes), "n");
    c.pos = [Math.round(n.pos[0] + dx), Math.round(n.pos[1] + dy)];
    map[n.id] = c.id;
    g.nodes.push(c);
  });
  sub.links.forEach(function (l) {
    if (map[l.source[0]] && map[l.target[0]]) {
      connect(g, [map[l.source[0]], l.source[1]], [map[l.target[0]], l.target[1]], true);
    }
  });
  return map;
}

function clone(x) { return JSON.parse(JSON.stringify(x)); }

function normalize(g) {
  g = g && typeof g === "object" ? g : {};
  return {
    nodes: (g.nodes || []).map(function (n, i) {
      var c = $.extend({}, n);
      c.id = String(c.id === undefined ? "n" + i : c.id);
      c.pos = c.pos || [0, 0];
      c.inputs = c.inputs || [];
      c.outputs = c.outputs || [];
      return c;
    }),
    links: (g.links || []).map(function (l, i) {
      var c = $.extend({}, l);
      c.id = String(c.id === undefined ? "l" + i : c.id);
      c.source = [String(l.source[0]), l.source[1]];
      c.target = [String(l.target[0]), l.target[1]];
      return c;
    }),
    groups: (g.groups || []).map(function (gr, i) {
      var c = $.extend({}, gr);
      c.id = String(c.id === undefined ? "g" + (i + 1) : c.id);
      return c;
    })
  };
}

// ------------------------------------------------------------------
// Colours (sigil node_graph/colors): data type colours are user data
// and deliberately do not follow the palette.
// ------------------------------------------------------------------

function typeColor(t) {
  var n = normType(t);
  if (n === "*") { return FALLBACK; }
  return "var(--ah-datatype-" + n.replace(/[^A-Z0-9_]/g, "_") + ", " + FALLBACK + ")";
}

function slotColors(t) {
  var n = normType(t);
  if (n === "*") { return [FALLBACK]; }
  var parts = n.split(",").map(function (x) { return x.trim(); }).filter(Boolean).slice(0, 3);
  return parts.length ? parts.map(typeColor) : [FALLBACK];
}

function linkColor(t) { return slotColors(t)[0]; }

function safeColor(c) {
  return c && /^[#a-zA-Z0-9(),.% -]+$/.test(String(c)) ? String(c) : null;
}

// ------------------------------------------------------------------
// Geometry (sigil node_graph/geometry)
// ------------------------------------------------------------------

function r2(n) { return Math.round(n * 100) / 100; }
function num(n) { return String(r2(n)); }
function pt(p) { return num(p[0]) + "," + num(p[1]); }
function clamp(v, lo, hi) { return Math.min(hi, Math.max(lo, v)); }

function nodeWidth(n) { return n.width ? Math.max(MIN_W, n.width) : DEFAULT_W; }

function nodeHeight(n, measured) {
  if (n.collapsed) { return TITLE_H; }
  if (n.height) { return Math.max(TITLE_H, n.height); }
  if (measured) { return measured; }
  return TITLE_H + PAD_TOP + SLOT_H * Math.max(n.inputs.length, n.outputs.length) + PAD_BOTTOM;
}

// The node as drawn this frame: drag offset and live resize applied.
function shown(st, n, live) {
  var d = live ? st.nodeDrag : null;
  var x = n.pos[0], y = n.pos[1], w = nodeWidth(n);
  var h = nodeHeight(n, st.sizes[n.id]);
  if (d) {
    if (d.ids && d.ids[n.id]) { x += d.dx; y += d.dy; }
    if (d.resize && d.resize.id === n.id) { w = d.resize.w; h = d.resize.h; }
  }
  return { id: n.id, x: x, y: y, w: w, h: h, collapsed: !!n.collapsed };
}

function shownMap(st, live) {
  var m = {};
  st.g.nodes.forEach(function (n) { m[n.id] = shown(st, n, live); });
  return m;
}

function slotPos(s, kind, i) {
  var x = kind === "input" ? s.x : s.x + s.w;
  return s.collapsed ? [x, s.y + TITLE_H / 2]
    : [x, s.y + TITLE_H + PAD_TOP + SLOT_H * i + SLOT_H / 2];
}

function screenToGraph(cam, sx, sy) { return [sx / cam.z - cam.x, sy / cam.z - cam.y]; }

function pan(cam, dx, dy) { return { x: cam.x + dx / cam.z, y: cam.y + dy / cam.z, z: cam.z }; }

function zoomAt(cam, f, sx, sy) {
  var z = clamp(cam.z * f, MIN_Z, MAX_Z);
  var gp = screenToGraph(cam, sx, sy);
  return { z: z, x: sx / z - gp[0], y: sy / z - gp[1] };
}

function contentBounds(boxes) {
  if (!boxes.length) { return null; }
  var x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
  boxes.forEach(function (b) {
    x0 = Math.min(x0, b.x); y0 = Math.min(y0, b.y);
    x1 = Math.max(x1, b.x + b.w); y1 = Math.max(y1, b.y + b.h);
  });
  return { x: x0, y: y0, w: x1 - x0, h: y1 - y0 };
}

function fitView(boxes, vw, vh, padding) {
  var b = contentBounds(boxes);
  if (!b) { return { x: 0, y: 0, z: 1 }; }
  var z = clamp(Math.min(Math.max(1, vw - 2 * padding) / Math.max(1, b.w),
                         Math.max(1, vh - 2 * padding) / Math.max(1, b.h)), MIN_Z, 1);
  return { z: z, x: (vw - b.w * z) / 2 / z - b.x, y: (vh - b.h * z) / 2 / z - b.y };
}

function normRect(a, b) {
  return { x: Math.min(a[0], b[0]), y: Math.min(a[1], b[1]),
           w: Math.abs(b[0] - a[0]), h: Math.abs(b[1] - a[1]) };
}

function intersects(a, b) {
  return a.x < b.x + b.w && a.x + a.w > b.x && a.y < b.y + b.h && a.y + a.h > b.y;
}

function contains(a, b) {
  return a.x <= b.x && a.y <= b.y && a.x + a.w >= b.x + b.w && a.y + a.h >= b.y + b.h;
}

function snap(v, size) { return size > 0 ? size * Math.round(v / size) : v; }

// One segment after the moveto; each segment leaves its left end to the
// right and enters its right end from the left.
function pathBody(mode, a, b) {
  if (mode === "linear") {
    return "L" + pt([a[0] + 15, a[1]]) + " L" + pt([b[0] - 15, b[1]]) + " L" + pt(b);
  }
  if (mode === "straight") {
    var ia = [a[0] + 10, a[1]], ib = [b[0] - 10, b[1]];
    var mid = num(0.5 * (ia[0] + ib[0]));
    return "L" + pt(ia) + " L" + mid + "," + num(ia[1]) + " L" + mid + "," + num(ib[1]) +
      " L" + pt(ib) + " L" + pt(b);
  }
  var d = Math.max(30, Math.hypot(b[0] - a[0], b[1] - a[1]) * 0.25);
  return "C" + pt([a[0] + d, a[1]]) + " " + pt([b[0] - d, b[1]]) + " " + pt(b);
}

function chainPath(mode, pts) {
  var parts = [];
  for (var i = 0; i + 1 < pts.length; i++) { parts.push(pathBody(mode, pts[i], pts[i + 1])); }
  return "M" + pt(pts[0]) + " " + parts.join(" ");
}

function nearestSegment(pts, p) {
  var best = -1, dist = Infinity;
  for (var i = 0; i + 1 < pts.length; i++) {
    var cx = (pts[i][0] + pts[i + 1][0]) / 2, cy = (pts[i][1] + pts[i + 1][1]) / 2;
    var d = Math.hypot(cx - p[0], cy - p[1]);
    if (d < dist) { dist = d; best = i; }
  }
  return best;
}

function linkPoints(st, l, sm, live) {
  var a = sm[l.source[0]], b = sm[l.target[0]];
  if (!a || !b) { return null; }
  var pts = (l.points || []).map(function (p) { return p.slice(); });
  var w = live && st.wpDrag;
  if (w && w.link === l.id && pts[w.index]) { pts[w.index] = w.point; }
  return [slotPos(a, "output", l.source[1])].concat(pts, [slotPos(b, "input", l.target[1])]);
}

// ------------------------------------------------------------------
// Template views: the same fields aihtml_node_graph.erl builds.
// ------------------------------------------------------------------

function nodeStyle(n) {
  var c = safeColor(n.color);
  return "transform:translate3d(" + num(n.pos[0]) + "px," + num(n.pos[1]) + "px,0);" +
    "--ah-ng-node-width:" + num(nodeWidth(n)) + "px;" +
    (n.height ? "--ah-ng-node-height:" + num(n.height) + "px;" : "") +
    (c ? "--ah-ng-node-accent:" + c + ";" : "");
}

function slotView(id, kind, i, s, connected) {
  var label = s.label !== undefined && s.label !== null ? String(s.label) : String(s.name);
  var colors = slotColors(s.type);
  var multi = colors.length > 1;
  var paths = colors.length === 2 ? TWO_SLICES : THREE_SLICES;
  return {
    kind: kind, key: slotKey(id, kind, i), index: String(i),
    connected: !!connected, optional: !!s.optional, tip: normType(s.type),
    shape: s.shape || "circle", multi: multi, color: colors[0],
    slices: multi ? colors.map(function (c, k) { return { d: paths[k], fill: c }; }) : [],
    has_label: label !== "", label: label
  };
}

function nodeView(n, conn, readOnly, hw) {
  var c = conn[n.id] || { "in": {}, out: {} };
  var collapsed = !!n.collapsed;
  var type = n.type !== undefined && n.type !== null ? String(n.type) : "";
  return {
    id: n.id,
    title: String(n.title || n.type || n.id),
    has_type: type !== "", type: type,
    style: nodeStyle(n),
    collapsed: collapsed,
    sized: !!n.height && !collapsed,
    expanded: collapsed ? "false" : "true",
    toggle_label: collapsed ? "Expand node" : "Collapse node",
    stub_in: collapsed && n.inputs.length ? [{ color: linkColor(n.inputs[0].type) }] : [],
    stub_out: collapsed && n.outputs.length ? [{ color: linkColor(n.outputs[0].type) }] : [],
    inputs: n.inputs.map(function (s, i) { return slotView(n.id, "input", i, s, c["in"][i]); }),
    outputs: n.outputs.map(function (s, i) { return slotView(n.id, "output", i, s, c.out[i]); }),
    has_widgets: hw.widgets.length > 0,
    widgets: hw.widgets.map(function (h) { return { html: h }; }),
    has_body: hw.body !== null && hw.body !== undefined,
    body: hw.body || "",
    resizable: !readOnly
  };
}

function groupView(gr, readOnly, bounds, moved) {
  var b = bounds || gr.bounds, c = safeColor(gr.color);
  var x = b[0] + (moved ? moved[0] : 0), y = b[1] + (moved ? moved[1] : 0);
  return {
    id: gr.id,
    title: gr.title ? String(gr.title) : "Group",
    style: "transform:translate3d(" + num(x) + "px," + num(y) + "px,0);" +
      "width:" + num(b[2]) + "px;height:" + num(b[3]) + "px;" +
      (c ? "--ah-ng-group-color:" + c + ";" : ""),
    editable: !readOnly
  };
}

function connIndex(g) {
  var c = {};
  g.links.forEach(function (l) {
    var s = c[l.source[0]] = c[l.source[0]] || { "in": {}, out: {} };
    s.out[l.source[1]] = true;
    var t = c[l.target[0]] = c[l.target[0]] || { "in": {}, out: {} };
    t["in"][l.target[1]] = true;
  });
  return c;
}

function outType(g, l) {
  var s = slotOf(g, l.source[0], "output", l.source[1]);
  return s ? s.type : null;
}

// ------------------------------------------------------------------
// State
// ------------------------------------------------------------------

function state(el) { return $.data(el, KEY); }

function opts(st) { return st.opts; }

function graphJson(g) { return JSON.stringify(g); }

// Widget HTML of a live card (to copy it); mounted markers dropped so
// the copy mounts again.
function cardHtml(card) {
  var out = { widgets: [], body: null };
  if (!card) { return out; }
  var body = $(card).children(".ah-node-graph-node-body");
  body.children(".ah-node-graph-widgets").children(".ah-node-graph-widget").each(function () {
    out.widgets.push(cleanHtml(this));
  });
  var custom = body.children(".ah-node-graph-custom")[0];
  if (custom) { out.body = cleanHtml(custom); }
  return out;
}

function cleanHtml(el) {
  var c = el.cloneNode(true);
  $(c).find("[data-ah-mounted]").removeAttr("data-ah-mounted");
  return c.innerHTML;
}

function takeHtml(st, n) {
  if (n.html) {
    st.pending[n.id] = { widgets: n.html.widgets || [], body: n.html.body === undefined ? null : n.html.body };
    delete n.html;
  }
}

function cardOf(st, id) {
  return $(st.nodesEl).children(".ah-node-graph-node").filter(function () {
    return this.getAttribute("data-node-id") === id;
  })[0] || null;
}

// ------------------------------------------------------------------
// Drawing
// ------------------------------------------------------------------

function cardSig(st, n, conn) {
  var c = conn[n.id] || { "in": {}, out: {} };
  return JSON.stringify([n.title, n.type, n.width, n.height, !!n.collapsed, n.color,
                         n.inputs, n.outputs, Object.keys(c["in"]).sort(),
                         Object.keys(c.out).sort(), opts(st).readOnly]);
}

function renderNodes(st) {
  var box = st.nodesEl, conn = connIndex(st.g), cards = {};
  $(box).children(".ah-node-graph-node").each(function () {
    cards[this.getAttribute("data-node-id")] = this;
  });
  var prev = null;
  st.g.nodes.forEach(function (n) {
    var card = cards[n.id];
    delete cards[n.id];
    var sig = cardSig(st, n, conn);
    if (!card || card._ahSig !== sig || st.pending[n.id]) {
      card = renderCard(st, n, conn, card);
      card._ahSig = sig;
    } else {
      card.style.transform = "translate3d(" + num(n.pos[0]) + "px," + num(n.pos[1]) + "px,0)";
      card.removeAttribute("data-dragging");
    }
    var want = prev ? prev.nextSibling : box.firstChild;
    if (want !== card) { box.insertBefore(card, want); }
    prev = card;
  });
  $.each(cards, function (_, card) { AH.destroy(card); $(card).remove(); });
  measure(st);
}

function renderCard(st, n, conn, old) {
  var pending = st.pending[n.id];
  delete st.pending[n.id];
  var body = old && $(old).children(".ah-node-graph-node-body");
  var oldW = !pending && body && body.children(".ah-node-graph-widgets")[0];
  var oldB = !pending && body && body.children(".ah-node-graph-custom")[0];
  var hw = pending || {
    widgets: oldW ? $(oldW).children().map(function () { return ""; }).get() : [],
    body: oldB ? "" : null
  };
  var card = $($.parseHTML(AH.tpl.node_graph_node(nodeView(n, conn, opts(st).readOnly, hw))))
    .filter(".ah-node-graph-node")[0];
  var nb = $(card).children(".ah-node-graph-node-body");
  if (oldW) { nb.children(".ah-node-graph-widgets").replaceWith(oldW); }
  if (oldB) { nb.children(".ah-node-graph-custom").replaceWith(oldB); }
  var refocus = old && old.contains(document.activeElement) &&
    $(document.activeElement).hasClass("ah-node-graph-collapse");
  if (old) {
    if (pending) { AH.destroy(old); }
    old.parentNode.replaceChild(card, old);
  } else {
    st.nodesEl.appendChild(card);
  }
  if (pending) { AH.mount(card); }
  if (refocus) { $(card).find(".ah-node-graph-collapse").first().trigger("focus"); }
  return card;
}

// Card heights only the browser knows (widget rows): for marquee hits,
// fit and the minimap. Nodes with their own height keep it.
function measure(st) {
  $(st.nodesEl).children(".ah-node-graph-node").each(function () {
    var id = this.getAttribute("data-node-id");
    var h = this.offsetHeight;
    if (h > 0) { st.sizes[id] = h; }
  });
}

function renderLinks(st) {
  var sm = shownMap(st, true), sel = st.lsel, g = st.g;
  var detached = st.linkDrag && st.linkDrag.origin.detach;
  st.linksEl.innerHTML = g.links.map(function (l) {
    var pts = l.id === detached ? null : linkPoints(st, l, sm, true);
    if (!pts) { return ""; }
    var color = linkColor(outType(g, l));
    return AH.tpl.node_graph_link({
      id: l.id, selected: sel.indexOf(l.id) >= 0,
      d: chainPath(opts(st).mode, pts), color: color,
      points: (l.points || []).map(function (p, i) {
        var q = pts[i + 1];
        return { index: String(i), x: num(q[0]), y: num(q[1]), color: color };
      })
    });
  }).join("");
  st.dragLinkEl = null;
  applyLinkDrag(st);
}

// Per frame while dragging: only the path data and reroute points move.
function updateLinkPaths(st) {
  var sm = shownMap(st, true), mode = opts(st).mode;
  var detached = st.linkDrag && st.linkDrag.origin.detach;
  $(st.linksEl).children(".ah-node-graph-link").each(function () {
    var l = findLink(st.g, this.getAttribute("data-link-id"));
    var pts = l && l.id !== detached ? linkPoints(st, l, sm, true) : null;
    if (!pts) { this.style.display = "none"; return; }
    this.style.display = "";
    var d = chainPath(mode, pts);
    $(this).children("path").attr("d", d);
    $(this).children(".ah-node-graph-waypoint").each(function () {
      var q = pts[parseInt(this.getAttribute("data-index"), 10) + 1];
      if (q) { this.setAttribute("cx", num(q[0])); this.setAttribute("cy", num(q[1])); }
    });
  });
}

function renderGroups(st) {
  var ro = opts(st).readOnly;
  st.groupsEl.innerHTML = st.g.groups.map(function (gr) {
    return AH.tpl.node_graph_group(groupView(gr, ro));
  }).join("");
}

function renderAll(st) {
  renderGroups(st);
  renderNodes(st);
  renderLinks(st);
  applySelection(st);
  applyToolbar(st);
  renderMinimap(st);
}

// Drag frame: cards, group frames and link paths follow st.nodeDrag.
function applyNodeDrag(st) {
  var d = st.nodeDrag;
  $(st.nodesEl).children(".ah-node-graph-node").each(function () {
    var n = findNode(st.g, this.getAttribute("data-node-id"));
    if (!n) { return; }
    var s = shown(st, n, true);
    var moved = d && d.ids && d.ids[n.id];
    this.style.transform = "translate3d(" + num(s.x) + "px," + num(s.y) + "px,0)";
    if (moved) { this.setAttribute("data-dragging", "true"); } else { this.removeAttribute("data-dragging"); }
    if (d && d.resize && d.resize.id === n.id) {
      this.style.setProperty("--ah-ng-node-width", num(d.resize.w) + "px");
      this.style.setProperty("--ah-ng-node-height", num(d.resize.h) + "px");
      this.setAttribute("data-sized", "true");
    }
  });
  $(st.groupsEl).children(".ah-node-graph-group").each(function () {
    var gr = findGroup(st.g, this.getAttribute("data-group-id"));
    if (!gr) { return; }
    var b = d && d.groupResize && d.groupResize.id === gr.id ? d.groupResize.bounds : gr.bounds;
    var mv = d && d.group === gr.id ? [d.dx, d.dy] : [0, 0];
    this.style.transform = "translate3d(" + num(b[0] + mv[0]) + "px," + num(b[1] + mv[1]) + "px,0)";
    this.style.width = num(b[2]) + "px";
    this.style.height = num(b[3]) + "px";
  });
  updateLinkPaths(st);
  renderMinimap(st);
}

// Link drag frame: slot states and the link following the pointer.
function applyLinkDrag(st) {
  var drag = st.linkDrag;
  $(st.nodesEl).find(".ah-node-graph-slot").each(function () {
    if (!drag) { this.removeAttribute("data-drag-state"); return; }
    var k = this.getAttribute("data-slot-key");
    var c = drag.candidate;
    this.setAttribute("data-drag-state",
                      c && k === slotKey(c[0], c[1], c[2]) ? "candidate"
                      : drag.compatible[k] ? "compatible" : "dimmed");
  });
  if (st.dragLinkEl) { $(st.dragLinkEl).remove(); st.dragLinkEl = null; }
  $(st.linksEl).children(".ah-node-graph-link").each(function () {
    if (drag && drag.origin.detach === this.getAttribute("data-link-id")) {
      this.style.display = "none";
    }
  });
  if (!drag) { return; }
  var sm = shownMap(st, true), o = drag.origin;
  var anchor = sm[o.node] && slotPos(sm[o.node], o.kind, o.index);
  if (!anchor) { return; }
  var c = drag.candidate;
  var target = c && sm[c[0]] ? slotPos(sm[c[0]], c[1], c[2]) : drag.point;
  var a = o.kind === "output" ? anchor : target, b = o.kind === "output" ? target : anchor;
  var gEl = document.createElementNS(SVG_NS, "g");
  gEl.setAttribute("class", "ah-node-graph-drag-link");
  var p = document.createElementNS(SVG_NS, "path");
  p.setAttribute("class", "ah-node-graph-link-line");
  p.setAttribute("d", "M" + pt(a) + " " + pathBody(opts(st).mode, a, b));
  p.setAttribute("style", "stroke:" + linkColor(drag.type));
  gEl.appendChild(p);
  st.linksEl.appendChild(gEl);
  st.dragLinkEl = gEl;
}

function applyMarquee(st) {
  var el = st.marqueeEl, r = st.marquee;
  if (!el) { return; }
  if (!r) { el.hidden = true; return; }
  var b = normRect(r[0], r[1]);
  el.hidden = false;
  el.style.transform = "translate3d(" + num(b.x) + "px," + num(b.y) + "px,0)";
  el.style.width = num(b.w) + "px";
  el.style.height = num(b.h) + "px";
}

function applySelection(st) {
  $(st.nodesEl).children(".ah-node-graph-node").each(function () {
    if (st.sel.indexOf(this.getAttribute("data-node-id")) >= 0) {
      this.setAttribute("data-selected", "true");
    } else { this.removeAttribute("data-selected"); }
  });
  $(st.linksEl).children(".ah-node-graph-link").each(function () {
    if (st.lsel.indexOf(this.getAttribute("data-link-id")) >= 0) {
      this.setAttribute("data-selected", "true");
    } else { this.removeAttribute("data-selected"); }
  });
  applyToolbar(st);
}

function applyCamera(st) {
  var c = st.cam;
  st.canvasEl.style.transform = "scale3d(" + c.z + "," + c.z + ",1) translate3d(" +
    c.x + "px," + c.y + "px,0)";
  var s = st.vp.style;
  s.setProperty("--ah-ng-grid-size", GRID_DISPLAY * c.z + "px");
  s.setProperty("--ah-ng-grid-x", c.x * c.z + "px");
  s.setProperty("--ah-ng-grid-y", c.y * c.z + "px");
  applyToolbar(st);
  renderMinimap(st);
}

function applyToolbar(st) {
  var bar = $(st.el).children(".ah-node-graph-toolbar");
  if (!bar.length) { return; }
  bar.find(".ah-node-graph-zoom").text(Math.round(st.cam.z * 100) + "%");
  bar.find('[data-action="undo"]').prop("disabled", !st.past.length);
  bar.find('[data-action="redo"]').prop("disabled", !st.future.length);
  bar.find('[data-action="delete"]').prop("disabled", !st.sel.length && !st.lsel.length);
}

function miniTransform(content, view) {
  var boxes = [content, view].filter(Boolean);
  var b = contentBounds(boxes);
  var s = Math.min((MINI_W - 2 * MINI_PAD) / Math.max(1, b.w), (MINI_H - 2 * MINI_PAD) / Math.max(1, b.h));
  return { scale: s, ox: b.x - MINI_PAD / s, oy: b.y - MINI_PAD / s };
}

function viewBox(st) {
  var r = st.vp.getBoundingClientRect(), c = st.cam;
  return { x: -c.x, y: -c.y, w: Math.max(1, r.width) / c.z, h: Math.max(1, r.height) / c.z };
}

function renderMinimap(st) {
  var mini = $(st.el).children(".ah-node-graph-minimap")[0];
  if (!mini) { return; }
  var boxes = st.g.nodes.map(function (n) { return shown(st, n, true); });
  var view = viewBox(st);
  var t = miniTransform(contentBounds(boxes), view);
  var style = function (b) {
    return "left:" + num((b.x - t.ox) * t.scale) + "px;top:" + num((b.y - t.oy) * t.scale) +
      "px;width:" + num(Math.max(1, b.w * t.scale)) + "px;height:" +
      num(Math.max(1, b.h * t.scale)) + "px;";
  };
  mini.innerHTML = AH.tpl.node_graph_minimap({
    nodes: boxes.map(function (b) { return { selected: st.sel.indexOf(b.id) >= 0, style: style(b) }; }),
    view: style(view)
  });
}

// ------------------------------------------------------------------
// Edits, history, events
// ------------------------------------------------------------------

function writeValue(st) {
  var json = graphJson(st.g);
  st.el.setAttribute("data-ah-value", json);
  $(st.el).children("input[type=hidden]").val(json);
}

function diffList(a, b) {
  var before = {}, after = {};
  a.forEach(function (x) { before[x.id] = JSON.stringify(x); });
  b.forEach(function (x) { after[x.id] = true; });
  return {
    changed: b.filter(function (x) { return before[x.id] !== JSON.stringify(x); }),
    removed: a.filter(function (x) { return !after[x.id]; }).map(function (x) { return x.id; })
  };
}

function emit(st, op, old) {
  writeValue(st);
  var dn = diffList(old.nodes, st.g.nodes), dl = diffList(old.links, st.g.links);
  var dg = diffList(old.groups, st.g.groups);
  var changed = { nodes: dn.changed, links: dl.changed, groups: dg.changed };
  var removed = { nodes: dn.removed, links: dl.removed, groups: dg.removed };
  st.el.setAttribute("data-op", op);
  st.el.setAttribute("data-changed", JSON.stringify(changed));
  st.el.setAttribute("data-removed", JSON.stringify(removed));
  $(st.el).trigger("change", [{ op: op, changed: changed, removed: removed, graph: clone(st.g) }]);
}

// The one way to change the graph: fn edits a copy; a real change goes
// into the history, is drawn and fires change.
function mutate(st, op, fn) {
  if (opts(st).readOnly) { return false; }
  var before = graphJson(st.g);
  var g = JSON.parse(before);
  if (fn(g) === false) { return false; }
  if (graphJson(g) === before) { return false; }
  st.past.push(before);
  if (st.past.length > 200) { st.past.shift(); }
  st.future = [];
  var old = st.g;
  st.g = g;
  pruneSelection(st);
  renderAll(st);
  emit(st, op, old);
  return true;
}

function travel(st, from, to, op) {
  if (!from.length) { return; }
  to.push(graphJson(st.g));
  var old = st.g;
  st.g = JSON.parse(from.pop());
  pruneSelection(st);
  renderAll(st);
  emit(st, op, old);
}

function undo(st) { travel(st, st.past, st.future, "undo"); }
function redo(st) { travel(st, st.future, st.past, "redo"); }

function pruneSelection(st) {
  var n = ids(st.g.nodes), l = ids(st.g.links);
  var sel = st.sel.filter(function (id) { return n[id]; });
  st.lsel = st.lsel.filter(function (id) { return l[id]; });
  if (sel.length !== st.sel.length) { setSelection(st, sel, st.lsel); }
}

function setSelection(st, nodes, links) {
  var before = AH.lib.values.join(st.sel);
  st.sel = nodes;
  st.lsel = links;
  applySelection(st);
  renderMinimap(st);
  if (AH.lib.values.join(st.sel) !== before) {
    st.el.setAttribute("data-selection", AH.lib.values.join(st.sel));
    $(st.el).trigger("ah:selection-change", [st.sel.slice()]);
  }
}

function select(st, list, mode) {
  var cur = st.sel.slice();
  list.forEach(function (id) {
    var i = cur.indexOf(id);
    if (mode === "toggle" && i >= 0) { cur.splice(i, 1); } else if (i < 0) { cur.push(id); }
  });
  setSelection(st, mode === "replace" ? list.slice() : cur, []);
}

// Raise nodes in the z order: no history, no change event (a click on a
// card is not an edit); the value follows.
function bringToFront(st, list) {
  var s = {};
  list.forEach(function (id) { s[id] = true; });
  var order = st.g.nodes.filter(function (n) { return !s[n.id]; })
    .concat(st.g.nodes.filter(function (n) { return s[n.id]; }));
  var same = order.every(function (n, i) { return n === st.g.nodes[i]; });
  if (same) { return; }
  st.g.nodes = order;
  order.forEach(function (n) {
    var card = cardOf(st, n.id);
    if (card) { st.nodesEl.appendChild(card); }
  });
  writeValue(st);
}

function deleteSelection(st) {
  var nodes = st.sel.slice(), links = st.lsel.slice();
  if (!nodes.length && !links.length) { return; }
  mutate(st, links.length && !nodes.length ? "disconnect" : "remove", function (g) {
    g.links = g.links.filter(function (l) { return links.indexOf(l.id) < 0; });
    removeNodes(g, nodes);
  });
}

function copy(st, list) {
  if (!list.length) { return; }
  var sub = extract(st.g, list);
  st.clip = { graph: clone(sub), html: {} };
  sub.nodes.forEach(function (n) { st.clip.html[n.id] = cardHtml(cardOf(st, n.id)); });
}

function paste(st, clip, op) {
  if (!clip || !clip.graph.nodes.length) { return; }
  var map;
  var ok = mutate(st, op, function (g) {
    map = insert(g, clip.graph, 20, 20);
    $.each(map, function (from, to) { st.pending[to] = clip.html[from]; });
  });
  if (ok) {
    select(st, clip.graph.nodes.map(function (n) { return map[n.id]; }), "replace");
  } else if (map) {
    $.each(map, function (_, to) { delete st.pending[to]; });
  }
}

function duplicate(st, list) {
  var saved = st.clip;
  copy(st, list);
  var clip = st.clip;
  st.clip = saved;
  paste(st, clip, "duplicate");
}

function fit(st) {
  var r = st.vp.getBoundingClientRect();
  if (r.width <= 0 || r.height <= 0) { return; }
  st.cam = fitView(st.g.nodes.map(function (n) { return shown(st, n, false); }), r.width, r.height, 40);
  applyCamera(st);
}

function setCamera(st, cam) {
  st.cam = cam;
  applyCamera(st);
}

function zoomBy(st, f, sx, sy) {
  if (sx === undefined) {
    var r = st.vp.getBoundingClientRect();
    sx = r.width / 2; sy = r.height / 2;
  }
  setCamera(st, zoomAt(st.cam, f, sx, sy));
}

// ------------------------------------------------------------------
// Pointer drags (sigil begin-drag!): capture on the viewport, end on
// pointerup, a move without buttons or a window blur; Escape cancels.
// ------------------------------------------------------------------

function beginDrag(st, e, spec) {
  var oe = e.originalEvent || e;
  var x0 = oe.clientX, y0 = oe.clientY, pid = oe.pointerId;
  var mouse = oe.pointerType !== "touch";
  var threshold = spec.threshold || 0;
  var started = threshold === 0, done = false;
  var last = { x: x0, y: y0, dx: 0, dy: 0 };
  var cap = st.vp;
  function at(ev) { return { x: ev.clientX, y: ev.clientY, dx: ev.clientX - x0, dy: ev.clientY - y0 }; }
  function mine(ev) { return pid === undefined || ev.pointerId === undefined || ev.pointerId === pid; }
  function cleanup() {
    document.removeEventListener("pointermove", move);
    document.removeEventListener("pointerup", up);
    document.removeEventListener("pointercancel", up);
    document.removeEventListener("keydown", key, true);
    window.removeEventListener("blur", blur);
    try { if (pid !== undefined && cap.hasPointerCapture(pid)) { cap.releasePointerCapture(pid); } } catch (x) { /* gone */ }
    st.drags = st.drags.filter(function (f) { return f !== cancel; });
  }
  function finish(f, arg) {
    if (done) { return; }
    done = true;
    cleanup();
    if (f) { f(arg); }
  }
  function lost() { if (started) { finish(spec.end, last); } else { finish(null); } }
  function move(ev) {
    if (!mine(ev)) { return; }
    if (mouse && ev.buttons === 0) { lost(); return; }
    var s = at(ev);
    last = s;
    if (!started && Math.hypot(s.dx, s.dy) > threshold) {
      started = true;
      if (spec.start) { spec.start(s); }
    }
    if (started && spec.move) { spec.move(s); }
  }
  function up(ev) {
    if (!mine(ev)) { return; }
    if (started) { finish(spec.end, at(ev)); } else { finish(spec.click || null, at(ev)); }
  }
  function key(ev) {
    if (ev.key === "Escape") {
      ev.stopPropagation();
      finish(function () { if (spec.cancel) { spec.cancel(); } });
    }
  }
  function blur() { lost(); }
  function cancel() { finish(function () { if (spec.cancel) { spec.cancel(); } }); }
  document.addEventListener("pointermove", move);
  document.addEventListener("pointerup", up);
  document.addEventListener("pointercancel", up);
  document.addEventListener("keydown", key, true);
  window.addEventListener("blur", blur);
  try { if (pid !== undefined) { cap.setPointerCapture(pid); } } catch (x) { /* synthetic */ }
  st.drags.push(cancel);
  if (started && spec.start) { spec.start(last); }
  return cancel;
}

function clientToScreen(st, cx, cy) {
  var r = st.vp.getBoundingClientRect();
  return [cx - r.left, cy - r.top];
}

function clientToGraph(st, cx, cy) {
  var s = clientToScreen(st, cx, cy);
  return screenToGraph(st.cam, s[0], s[1]);
}

function clientToHost(st, cx, cy) {
  var r = st.el.getBoundingClientRect();
  return [cx - r.left, cy - r.top];
}

function point(e) {
  var oe = e.originalEvent || e;
  return [oe.clientX, oe.clientY];
}

// ------------------------------------------------------------------
// Overlays: context menu, node search menu
// ------------------------------------------------------------------

function closeOverlay(st, refocus) {
  var o = st.overlay;
  if (!o) { return; }
  st.overlay = null;
  o.cleanup();
  $(o.el).remove();
  if (refocus) { st.vp.focus(); }
}

function openOverlay(st, el, at) {
  closeOverlay(st);
  st.el.appendChild(el);
  // keep it inside the component
  var host = st.el.getBoundingClientRect();
  var x = Math.max(4, Math.min(at[0], host.width - el.offsetWidth - 4));
  var y = Math.max(4, Math.min(at[1], host.height - el.offsetHeight - 4));
  el.style.left = x + "px";
  el.style.top = y + "px";
  function outside(e) { if (!el.contains(e.target)) { closeOverlay(st); } }
  function key(e) { if (e.key === "Escape") { e.preventDefault(); closeOverlay(st, true); } }
  var t = setTimeout(function () { document.addEventListener("pointerdown", outside, true); }, 0);
  $(el).on("keydown", key);
  st.overlay = {
    el: el,
    cleanup: function () {
      clearTimeout(t);
      document.removeEventListener("pointerdown", outside, true);
      $(el).off("keydown", key);
    }
  };
  $(el).on("contextmenu", function (e) { e.preventDefault(); });
}

// items: [{label, hint, danger, run}]
function contextMenu(st, at, items) {
  items = items.filter(Boolean);
  var el = $('<div class="ah-node-graph-ctxmenu" role="menu" aria-label="Actions"></div>')[0];
  el.innerHTML = AH.tpl.node_graph_menu({
    items: items.map(function (it, i) {
      return { index: String(i), label: it.label, danger: !!it.danger,
               has_hint: !!it.hint, hint: it.hint || "" };
    })
  });
  openOverlay(st, el, at);
  var buttons = $(el).children(".ah-node-graph-ctxmenu-item");
  $(el).on("click", ".ah-node-graph-ctxmenu-item", function () {
    var it = items[parseInt(this.getAttribute("data-index"), 10)];
    closeOverlay(st, true);
    if (it && it.run) { it.run(); }
  });
  $(el).on("keydown", function (e) {
    var i = buttons.index(document.activeElement), n = buttons.length;
    var next = { ArrowDown: (i + 1) % n, ArrowUp: (i - 1 + n) % n, Home: 0, End: n - 1 }[e.key];
    if (next !== undefined) { e.preventDefault(); buttons.eq(next).trigger("focus"); }
    if (e.key === "Tab") { e.preventDefault(); closeOverlay(st, true); }
  });
  buttons.first().trigger("focus");
}

function filterItems(items, q) {
  q = String(q || "").trim().toLowerCase();
  if (!q) { return items.slice(); }
  return items.filter(function (it) {
    return [it.label, it.type, it.category].some(function (v) {
      return v !== undefined && v !== null && String(v).toLowerCase().indexOf(q) >= 0;
    });
  });
}

// The node search menu: right click on the canvas, or a link dropped on
// empty canvas (origin set: the new node gets connected).
function searchMenu(st, at, graphPt, origin) {
  var canvas = origin ? [] : [{ kind: "group", label: "New group frame", category: "Canvas" }];
  var items = st.library;
  var listId = st.el.id + "-search-" + (++seq);
  var s = { q: "", active: 0 };
  var el = $('<div class="ah-node-graph-search" role="dialog" aria-label="Add node"></div>')[0];
  var input = $('<input class="ah-node-graph-search-input" type="text" role="combobox" ' +
                'aria-autocomplete="list" aria-expanded="true" placeholder="Search nodes…">')[0];
  input.setAttribute("aria-controls", listId);
  var body = $('<div class="ah-node-graph-search-body"></div>')[0];
  el.appendChild(input);
  el.appendChild(body);
  function list() { return filterItems(canvas, s.q).concat(filterItems(items, s.q)); }
  function paint() {
    var l = list(), n = l.length, idx = n ? ((s.active % n) + n) % n : 0;
    body.innerHTML = AH.tpl.node_graph_search({
      list_id: listId, empty: n === 0, empty_text: "No matching node",
      items: l.map(function (it, i) {
        return { index: String(i), label: String(it.label || it.type), active: i === idx,
                 has_category: !!it.category, category: it.category ? String(it.category) : "" };
      })
    });
    if (n) { input.setAttribute("aria-activedescendant", listId + "-" + idx); }
    else { input.removeAttribute("aria-activedescendant"); }
    var act = $(body).find('[data-active="true"]')[0];
    if (act && act.scrollIntoView) { act.scrollIntoView({ block: "nearest" }); }
  }
  function pick(i) {
    var it = list()[i];
    if (!it) { return; }
    closeOverlay(st, true);
    var x = Math.round(graphPt[0]), y = Math.round(graphPt[1]);
    if (it.kind === "group") {
      mutate(st, "group-add", function (g) {
        g.groups.push({ id: freshId(ids(g.groups), "g"), title: "Group", bounds: [x, y, 340, 260] });
      });
      return;
    }
    addNode(st, it, [x, y], origin);
  }
  openOverlay(st, el, at);
  paint();
  $(input).on("input", function () { s.q = input.value; s.active = 0; paint(); });
  $(input).on("keydown", function (e) {
    var n = list().length;
    if (e.key === "ArrowDown") { e.preventDefault(); s.active++; paint(); }
    else if (e.key === "ArrowUp") { e.preventDefault(); s.active--; paint(); }
    else if (e.key === "Enter") { e.preventDefault(); pick(n ? ((s.active % n) + n) % n : 0); }
  });
  // pointerdown, not click: the input's blur comes before click
  $(body).on("pointerdown", ".ah-node-graph-search-item", function (e) {
    e.preventDefault();
    pick(parseInt(this.getAttribute("data-index"), 10));
  });
  input.focus();
}

// A library entry dropped at `pos'; connected to `origin' when it came
// from a link drag. One history entry for both.
function addNode(st, item, pos, origin) {
  var id;
  var src = clone(item.node || {});
  var html = src.html;
  delete src.html;
  mutate(st, "add", function (g) {
    id = freshId(ids(g.nodes), "n");
    var n = $.extend(src, { id: id, pos: pos, title: item.label || item.type, type: item.type });
    n.inputs = n.inputs || [];
    n.outputs = n.outputs || [];
    g.nodes.push(n);
    if (html) { st.pending[id] = { widgets: html.widgets || [], body: html.body === undefined ? null : html.body }; }
    if (origin) {
      var want = origin.kind === "output" ? "input" : "output";
      var slots = want === "input" ? n.inputs : n.outputs;
      for (var i = 0; i < slots.length; i++) {
        var pair = pairFor(origin, [id, want, i]);
        if (!connectProblem(g, pair[0], pair[1], opts(st).allowCycles)) {
          if (origin.detach) { g.links = g.links.filter(function (l) { return l.id !== origin.detach; }); }
          connect(g, pair[0], pair[1], opts(st).allowCycles);
          break;
        }
      }
    }
  });
  if (id && findNode(st.g, id)) { select(st, [id], "replace"); }
}

// ------------------------------------------------------------------
// Link drag helpers (sigil node_graph/link_drag)
// ------------------------------------------------------------------

// Dragging from a connected input picks its link up: the real start is
// the output at its other end.
function originOf(g, id, kind, index) {
  if (kind === "input") {
    var l = inputLink(g, id, index);
    if (l) { return { node: l.source[0], kind: "output", index: l.source[1], detach: l.id }; }
  }
  return { node: id, kind: kind, index: index, detach: null };
}

function pairFor(origin, cand) {
  return origin.kind === "output"
    ? [[origin.node, origin.index], [cand[0], cand[2]]]
    : [[cand[0], cand[2]], [origin.node, origin.index]];
}

function compatibleMap(g, origin, allowCycles) {
  var want = origin.kind === "output" ? "input" : "output", out = {};
  g.nodes.forEach(function (n) {
    (want === "input" ? n.inputs : n.outputs).forEach(function (_, i) {
      var p = pairFor(origin, [n.id, want, i]);
      if (!connectProblem(g, p[0], p[1], allowCycles)) { out[slotKey(n.id, want, i)] = true; }
    });
  });
  return out;
}

function nearestCandidate(st, compat, origin, p, radius) {
  var want = origin.kind === "output" ? "input" : "output", best = null, dist = Infinity;
  var sm = shownMap(st, false);
  st.g.nodes.forEach(function (n) {
    (want === "input" ? n.inputs : n.outputs).forEach(function (_, i) {
      if (!compat[slotKey(n.id, want, i)]) { return; }
      var q = slotPos(sm[n.id], want, i), d = Math.hypot(q[0] - p[0], q[1] - p[1]);
      if (d <= radius && d < dist) { dist = d; best = [n.id, want, i]; }
    });
  });
  return best;
}

// ------------------------------------------------------------------
// Title editing
// ------------------------------------------------------------------

function editTitle(st, span, commit) {
  var old = span.textContent;
  var input = $('<input type="text">').addClass(span.className + "-input")[0];
  input.value = old;
  input.setAttribute("aria-label", "Title");
  span.parentNode.replaceChild(input, span);
  input.focus();
  input.select();
  var done = false;
  function restore(v) {
    if (done) { return; }
    done = true;
    span.textContent = v;
    if (input.parentNode) { input.parentNode.replaceChild(span, input); }
    st.vp.focus();
  }
  $(input).on("pointerdown", function (e) { e.stopPropagation(); });
  $(input).on("blur", function () {
    var v = input.value.trim();
    restore(v || old);
    if (v && v !== old) { commit(v); }
  });
  $(input).on("keydown", function (e) {
    e.stopPropagation();
    if (e.key === "Enter") { e.preventDefault(); input.blur(); }
    if (e.key === "Escape") { e.preventDefault(); restore(old); }
  });
}

// ------------------------------------------------------------------
// Wiring
// ------------------------------------------------------------------

function nodeIdOf(el) {
  var card = $(el).closest(".ah-node-graph-node")[0];
  return card ? card.getAttribute("data-node-id") : null;
}

function linkIdOf(el) {
  var g = $(el).closest(".ah-node-graph-link")[0];
  return g ? g.getAttribute("data-link-id") : null;
}

function groupIdOf(el) {
  var g = $(el).closest(".ah-node-graph-group")[0];
  return g ? g.getAttribute("data-group-id") : null;
}

function isEditable(t) {
  return /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName) || t.isContentEditable ||
    $(t).closest(".ah-node-graph-widget, .ah-node-graph-custom, .ah-node-graph-search, .ah-node-graph-ctxmenu").length > 0;
}

function stop(e) { e.stopPropagation(); }

function wireNodes(st) {
  var $el = $(st.el), NS = AH.NS;
  $el.on("pointerdown" + NS, ".ah-node-graph-collapse, .ah-node-graph-widget, .ah-node-graph-custom, " +
         ".ah-node-graph-tool, .ah-node-graph-group-delete", stop);
  $el.on("click" + NS, ".ah-node-graph-collapse", function (e) {
    e.stopPropagation();
    var id = nodeIdOf(this);
    mutate(st, "collapse", function (g) {
      var n = findNode(g, id);
      if (!n) { return false; }
      n.collapsed = !n.collapsed;
      if (!n.collapsed) { delete n.collapsed; }
    });
  });
  // Tabbing onto a card's collapse button selects the card, so the
  // keyboard reaches nodes (Delete, arrows, Ctrl+D, ...).
  $el.on("focusin" + NS, ".ah-node-graph-collapse", function () {
    var id = nodeIdOf(this);
    if (st.sel.length !== 1 || st.sel[0] !== id) { select(st, [id], "replace"); }
  });
  $el.on("dblclick" + NS, ".ah-node-graph-node-title", function (e) {
    if (opts(st).readOnly) { return; }
    e.stopPropagation();
    var id = nodeIdOf(this);
    editTitle(st, this, function (v) {
      mutate(st, "rename", function (g) { findNode(g, id).title = v; });
    });
  });

  // resize handle
  $el.on("pointerdown" + NS, ".ah-node-graph-node-resize", function (e) {
    e.stopPropagation();
    if (e.button !== 0 || opts(st).readOnly) { return; }
    var card = $(this).closest(".ah-node-graph-node")[0];
    var id = card.getAttribute("data-node-id"), n = findNode(st.g, id);
    var w0 = nodeWidth(n), h0 = n.height || card.offsetHeight || nodeHeight(n);
    var hMin = Math.max(MIN_H, TITLE_H + PAD_TOP + SLOT_H * Math.max(n.inputs.length, n.outputs.length) + PAD_BOTTOM);
    function size(s) {
      return { id: id, w: Math.max(MIN_W, Math.round(w0 + s.dx / st.cam.z)),
               h: Math.max(hMin, Math.round(h0 + s.dy / st.cam.z)) };
    }
    beginDrag(st, e, {
      move: function (s) { st.nodeDrag = { resize: size(s) }; applyNodeDrag(st); },
      end: function (s) {
        var z = size(s);
        st.nodeDrag = null;
        card._ahSig = null;
        if (!mutate(st, "resize", function (g) {
          var m = findNode(g, id);
          m.width = z.w;
          m.height = z.h;
        })) { renderAll(st); }
      },
      cancel: function () { st.nodeDrag = null; card._ahSig = null; renderAll(st); }
    });
  });

  // slots: drag a link out
  $el.on("pointerdown" + NS, ".ah-node-graph-slot-hit", function (e) {
    e.stopPropagation();
    if (e.button !== 0 || opts(st).readOnly) { return; }
    var slotEl = $(this).closest(".ah-node-graph-slot")[0];
    var origin = originOf(st.g, nodeIdOf(this), slotEl.getAttribute("data-kind"),
                          parseInt(slotEl.getAttribute("data-slot-index"), 10));
    var compat = compatibleMap(st.g, origin, opts(st).allowCycles);
    var slot = slotOf(st.g, origin.node, origin.kind, origin.index);
    var p0 = point(e);
    st.vp.focus();
    st.linkDrag = { origin: origin, type: slot && slot.type, compatible: compat,
                    point: clientToGraph(st, p0[0], p0[1]), candidate: null };
    applyLinkDrag(st);
    beginDrag(st, e, {
      move: function (s) {
        var p = clientToGraph(st, s.x, s.y);
        st.linkDrag.point = p;
        st.linkDrag.candidate = nearestCandidate(st, compat, origin, p, SNAP_RADIUS / st.cam.z);
        applyLinkDrag(st);
      },
      end: function (s) {
        var cand = st.linkDrag && st.linkDrag.candidate;
        st.linkDrag = null;
        if (!cand && Math.hypot(s.dx, s.dy) < 3) {
          renderLinks(st);                  // a click on a slot is not a drop
        } else if (cand) {
          dropLink(st, origin, pairFor(origin, cand));
        } else {
          var gp = clientToGraph(st, s.x, s.y);
          if (st.library.length) {
            renderLinks(st);
            applyLinkDrag(st);
            searchMenu(st, clientToHost(st, s.x, s.y), gp, origin);
          } else {
            st.el.setAttribute("data-origin", origin.node + ":" + origin.kind + ":" + origin.index);
            st.el.setAttribute("data-point", Math.round(gp[0]) + "," + Math.round(gp[1]));
            if (!origin.detach || !dropLink(st, origin, null)) { renderLinks(st); }
            $(st.el).trigger("ah:link-drop", [{ origin: origin, point: gp }]);
          }
        }
      },
      cancel: function () { st.linkDrag = null; renderLinks(st); },
      click: function () { st.linkDrag = null; renderLinks(st); }
    });
  });

  // the card itself: select and move
  $el.on("pointerdown" + NS, ".ah-node-graph-node", function (e) {
    if (e.button !== 0 || st.hand) { return; }
    e.stopPropagation();
    var id = this.getAttribute("data-node-id");
    var additive = e.shiftKey || e.ctrlKey || e.metaKey;
    var list = additive ? st.sel.concat(st.sel.indexOf(id) < 0 ? [id] : [])
      : (st.sel.indexOf(id) >= 0 ? st.sel.slice() : [id]);
    st.vp.focus({ preventScroll: true });
    if (additive) { select(st, [id], "add"); }
    else if (st.sel.indexOf(id) < 0) { select(st, [id], "replace"); }
    bringToFront(st, list);
    if (opts(st).readOnly) { return; }
    var moving = {};
    list.forEach(function (x) { moving[x] = true; });
    function delta(s) {
      var sn = opts(st).snap;
      return [snap(s.dx / st.cam.z, sn), snap(s.dy / st.cam.z, sn)];
    }
    beginDrag(st, e, {
      threshold: 3,
      move: function (s) {
        var d = delta(s);
        st.nodeDrag = { ids: moving, dx: d[0], dy: d[1] };
        applyNodeDrag(st);
      },
      end: function (s) {
        var d = delta(s);
        st.nodeDrag = null;
        if (!(d[0] || d[1]) || !moveNodes(st, list, d[0], d[1])) { applyNodeDrag(st); }
      },
      cancel: function () { st.nodeDrag = null; applyNodeDrag(st); },
      click: function () {
        // a plain click on a selected card among several selects it alone
        if (!additive && st.sel.length > 1) { select(st, [id], "replace"); }
      }
    });
  });

  $el.on("contextmenu" + NS, ".ah-node-graph-node", function (e) {
    e.preventDefault();
    e.stopPropagation();
    var p = point(e);
    nodeMenu(st, this.getAttribute("data-node-id"), clientToHost(st, p[0], p[1]));
  });
}

function moveNodes(st, list, dx, dy) {
  return mutate(st, "move", function (g) {
    list.forEach(function (id) {
      var n = findNode(g, id);
      if (n) { n.pos = [Math.round(n.pos[0] + dx), Math.round(n.pos[1] + dy)]; }
    });
  });
}

function dropLink(st, origin, pair) {
  // the same link again (or the one picked up put back) changes nothing
  if (pair && st.g.links.some(function (l) {
    return l.source[0] === pair[0][0] && l.source[1] === pair[0][1] &&
      l.target[0] === pair[1][0] && l.target[1] === pair[1][1];
  })) {
    renderLinks(st);
    return false;
  }
  var ok = mutate(st, pair ? "connect" : "disconnect", function (g) {
    if (origin.detach) { g.links = g.links.filter(function (x) { return x.id !== origin.detach; }); }
    if (pair) { connect(g, pair[0], pair[1], opts(st).allowCycles); }
  });
  if (!ok) { renderLinks(st); }
  return ok;
}

function nodeMenu(st, id, at) {
  var n = findNode(st.g, id);
  if (!n) { return; }
  var list = st.sel.indexOf(id) >= 0 ? st.sel.slice() : [id];
  if (st.sel.indexOf(id) < 0) { select(st, [id], "replace"); }
  var many = list.length > 1, ro = opts(st).readOnly;
  contextMenu(st, at, [
    ro ? null : { label: n.collapsed ? "Expand" : "Collapse",
                  run: function () { $(cardOf(st, id)).find(".ah-node-graph-collapse").first().trigger("click"); } },
    ro ? null : { label: many ? "Duplicate " + list.length + " nodes" : "Duplicate node", hint: "Ctrl+D",
                  run: function () { duplicate(st, list); } },
    { label: many ? "Copy " + list.length + " nodes" : "Copy node", hint: "Ctrl+C",
      run: function () { copy(st, list); } },
    ro ? null : { label: many ? "Delete " + list.length + " nodes" : "Delete node", hint: "Del", danger: true,
                  run: function () { mutate(st, "remove", function (g) { removeNodes(g, list); }); } }
  ]);
}

function wireLinks(st) {
  var $el = $(st.el), NS = AH.NS;
  $el.on("pointerdown" + NS, ".ah-node-graph-link-hit", function (e) {
    e.stopPropagation();
    var id = linkIdOf(this);
    st.vp.focus({ preventScroll: true });
    setSelection(st, [], e.shiftKey ? st.lsel.concat(st.lsel.indexOf(id) < 0 ? [id] : []) : [id]);
  });
  // double click adds a reroute point
  $el.on("dblclick" + NS, ".ah-node-graph-link-hit", function (e) {
    e.stopPropagation();
    if (opts(st).readOnly) { return; }
    var p = point(e);
    addWaypoint(st, linkIdOf(this), clientToGraph(st, p[0], p[1]));
  });
  $el.on("contextmenu" + NS, ".ah-node-graph-link-hit", function (e) {
    e.preventDefault();
    e.stopPropagation();
    var id = linkIdOf(this), p = point(e), gp = clientToGraph(st, p[0], p[1]);
    setSelection(st, [], [id]);
    if (opts(st).readOnly) { return; }
    contextMenu(st, clientToHost(st, p[0], p[1]), [
      { label: "Add reroute point here", run: function () { addWaypoint(st, id, gp); } },
      { label: "Delete link", hint: "Del", danger: true,
        run: function () { mutate(st, "disconnect", function (g) {
          g.links = g.links.filter(function (l) { return l.id !== id; });
        }); } }
    ]);
  });
  // reroute points: drag to move, double click to remove
  $el.on("pointerdown" + NS, ".ah-node-graph-waypoint", function (e) {
    e.stopPropagation();
    if (e.button !== 0 || opts(st).readOnly) { return; }
    var id = linkIdOf(this), i = parseInt(this.getAttribute("data-index"), 10);
    beginDrag(st, e, {
      threshold: 2,
      move: function (s) {
        var p = clientToGraph(st, s.x, s.y);
        st.wpDrag = { link: id, index: i, point: [Math.round(p[0]), Math.round(p[1])] };
        updateLinkPaths(st);
      },
      end: function () {
        var w = st.wpDrag;
        st.wpDrag = null;
        if (!w || !mutate(st, "reroute", function (g) {
          var l = findLink(g, id);
          if (!l || !l.points || !l.points[i]) { return false; }
          l.points[i] = w.point;
        })) { updateLinkPaths(st); }
      },
      cancel: function () { st.wpDrag = null; updateLinkPaths(st); }
    });
  });
  $el.on("dblclick" + NS, ".ah-node-graph-waypoint", function (e) {
    e.stopPropagation();
    var id = linkIdOf(this), i = parseInt(this.getAttribute("data-index"), 10);
    mutate(st, "reroute", function (g) {
      var l = findLink(g, id);
      if (!l || !l.points) { return false; }
      l.points.splice(i, 1);
      if (!l.points.length) { delete l.points; }
    });
  });
}

function addWaypoint(st, id, p) {
  var l = findLink(st.g, id);
  if (!l) { return; }
  var pts = linkPoints(st, l, shownMap(st, false), false);
  var q = [Math.round(p[0]), Math.round(p[1])];
  var i = pts ? nearestSegment(pts, q) : -1;
  if (i < 0) { return; }
  mutate(st, "reroute", function (g) {
    var m = findLink(g, id);
    m.points = (m.points || []).slice();
    m.points.splice(i, 0, q);
  });
}

function wireGroups(st) {
  var $el = $(st.el), NS = AH.NS;
  $el.on("pointerdown" + NS, ".ah-node-graph-group-header", function (e) {
    if (e.button !== 0 || opts(st).readOnly || st.hand) { return; }
    e.stopPropagation();
    var id = groupIdOf(this), gr = findGroup(st.g, id);
    var b = { x: gr.bounds[0], y: gr.bounds[1], w: gr.bounds[2], h: gr.bounds[3] };
    // the nodes it holds are fixed when the drag starts
    var inside = {}, list = [];
    st.g.nodes.forEach(function (n) {
      var s = shown(st, n, false);
      if (contains(b, s)) { inside[n.id] = true; list.push(n.id); }
    });
    st.vp.focus({ preventScroll: true });
    function delta(s) {
      var sn = opts(st).snap;
      return [snap(s.dx / st.cam.z, sn), snap(s.dy / st.cam.z, sn)];
    }
    beginDrag(st, e, {
      threshold: 3,
      move: function (s) {
        var d = delta(s);
        st.nodeDrag = { ids: inside, group: id, dx: d[0], dy: d[1] };
        applyNodeDrag(st);
      },
      end: function (s) {
        var d = delta(s);
        st.nodeDrag = null;
        if (!(d[0] || d[1]) || !mutate(st, "group-move", function (g) {
          var m = findGroup(g, id);
          m.bounds = [Math.round(m.bounds[0] + d[0]), Math.round(m.bounds[1] + d[1]), m.bounds[2], m.bounds[3]];
          list.forEach(function (nid) {
            var n = findNode(g, nid);
            n.pos = [Math.round(n.pos[0] + d[0]), Math.round(n.pos[1] + d[1])];
          });
        })) { applyNodeDrag(st); }
      },
      cancel: function () { st.nodeDrag = null; applyNodeDrag(st); }
    });
  });
  $el.on("dblclick" + NS, ".ah-node-graph-group-title", function (e) {
    if (opts(st).readOnly) { return; }
    e.stopPropagation();
    var id = groupIdOf(this);
    editTitle(st, this, function (v) {
      mutate(st, "group-rename", function (g) { findGroup(g, id).title = v; });
    });
  });
  $el.on("click" + NS, ".ah-node-graph-group-delete", function (e) {
    e.stopPropagation();
    removeGroup(st, groupIdOf(this));
  });
  $el.on("pointerdown" + NS, ".ah-node-graph-group-resize", function (e) {
    e.stopPropagation();
    if (e.button !== 0 || opts(st).readOnly) { return; }
    var id = groupIdOf(this), b0 = findGroup(st.g, id).bounds;
    function bounds(s) {
      return [b0[0], b0[1], Math.max(120, Math.round(b0[2] + s.dx / st.cam.z)),
              Math.max(80, Math.round(b0[3] + s.dy / st.cam.z))];
    }
    beginDrag(st, e, {
      move: function (s) { st.nodeDrag = { groupResize: { id: id, bounds: bounds(s) } }; applyNodeDrag(st); },
      end: function (s) {
        var b = bounds(s);
        st.nodeDrag = null;
        if (!mutate(st, "group-resize", function (g) { findGroup(g, id).bounds = b; })) { applyNodeDrag(st); }
      },
      cancel: function () { st.nodeDrag = null; applyNodeDrag(st); }
    });
  });
  $el.on("contextmenu" + NS, ".ah-node-graph-group", function (e) {
    e.preventDefault();
    e.stopPropagation();
    if (opts(st).readOnly) { return; }
    var id = groupIdOf(this), p = point(e);
    contextMenu(st, clientToHost(st, p[0], p[1]), [
      { label: "Delete group frame", hint: "frame only", danger: true,
        run: function () { removeGroup(st, id); } }
    ]);
  });
}

function removeGroup(st, id) {
  mutate(st, "group-remove", function (g) {
    g.groups = g.groups.filter(function (x) { return x.id !== id; });
  });
}

function wheelFactor(e) {
  var unit = e.deltaMode === 1 ? 16 : e.deltaMode === 2 ? 400 : 1;
  return Math.pow(1.0015, -e.deltaY * unit);
}

function wireViewport(st) {
  var $el = $(st.el), NS = AH.NS, vp = st.vp;
  var pointers = {}, pinch = null, cancel = null;

  // native, non-passive: jQuery's wheel may be passive and then the page scrolls
  function onWheel(e) {
    e.preventDefault();
    var s = clientToScreen(st, e.clientX, e.clientY);
    zoomBy(st, wheelFactor(e), s[0], s[1]);
  }
  vp.addEventListener("wheel", onWheel, { passive: false });
  st.cleanups.push(function () { vp.removeEventListener("wheel", onWheel); });

  function touches() { return Object.keys(pointers).map(function (k) { return pointers[k]; }); }

  $el.on("pointerdown" + NS, ".ah-node-graph-viewport", function (e) {
    var oe = e.originalEvent || e;
    if (oe.pointerType === "touch") { pointers[oe.pointerId] = [oe.clientX, oe.clientY]; }
    var tp = touches();
    if (tp.length === 2) {
      if (cancel) { cancel(); cancel = null; }
      pinch = { dist: Math.hypot(tp[1][0] - tp[0][0], tp[1][1] - tp[0][1]),
                mid: [(tp[0][0] + tp[1][0]) / 2, (tp[0][1] + tp[1][1]) / 2], cam: st.cam };
      return;
    }
    if ((e.button !== 0 && e.button !== 1) || pinch) { return; }
    if (e.button === 1) { e.preventDefault(); }
    vp.focus({ preventScroll: true });
    var panning = e.button === 1 || st.space || st.hand || oe.pointerType === "touch";
    if (panning) {
      var start = st.cam;
      vp.setAttribute("data-panning", "true");
      cancel = beginDrag(st, e, {
        move: function (s) { setCamera(st, pan(start, s.dx, s.dy)); },
        end: function () { vp.removeAttribute("data-panning"); cancel = null; },
        cancel: function () { vp.removeAttribute("data-panning"); cancel = null; },
        click: function () { vp.removeAttribute("data-panning"); cancel = null; }
      });
      return;
    }
    var additive = e.shiftKey || e.ctrlKey || e.metaKey;
    var origin = clientToGraph(st, oe.clientX, oe.clientY);
    if (!additive) { setSelection(st, [], []); }
    cancel = beginDrag(st, e, {
      threshold: 3,
      move: function (s) { st.marquee = [origin, clientToGraph(st, s.x, s.y)]; applyMarquee(st); },
      end: function () {
        if (st.marquee) {
          var r = normRect(st.marquee[0], st.marquee[1]);
          var hit = st.g.nodes.filter(function (n) { return intersects(r, shown(st, n, false)); })
            .map(function (n) { return n.id; });
          select(st, hit, additive ? "add" : "replace");
        }
        st.marquee = null;
        applyMarquee(st);
        cancel = null;
      },
      cancel: function () { st.marquee = null; applyMarquee(st); cancel = null; }
    });
  });

  $el.on("pointermove" + NS, ".ah-node-graph-viewport", function (e) {
    var oe = e.originalEvent || e;
    if (!pointers[oe.pointerId]) { return; }
    pointers[oe.pointerId] = [oe.clientX, oe.clientY];
    var tp = touches();
    if (!pinch || tp.length !== 2) { return; }
    var d = Math.hypot(tp[1][0] - tp[0][0], tp[1][1] - tp[0][1]);
    var m = [(tp[0][0] + tp[1][0]) / 2, (tp[0][1] + tp[1][1]) / 2];
    var a = clientToScreen(st, pinch.mid[0], pinch.mid[1]);
    var z = zoomAt(pinch.cam, d / Math.max(1, pinch.dist), a[0], a[1]);
    setCamera(st, pan(z, m[0] - pinch.mid[0], m[1] - pinch.mid[1]));
  });

  // fingers lifted outside the component still count
  function lift(e) {
    delete pointers[e.pointerId];
    if (touches().length < 2) { pinch = null; }
  }
  document.addEventListener("pointerup", lift);
  document.addEventListener("pointercancel", lift);
  st.cleanups.push(function () {
    document.removeEventListener("pointerup", lift);
    document.removeEventListener("pointercancel", lift);
  });

  $el.on("contextmenu" + NS, ".ah-node-graph-viewport", function (e) {
    e.preventDefault();
    if (opts(st).readOnly) { return; }
    var p = point(e);
    searchMenu(st, clientToHost(st, p[0], p[1]), clientToGraph(st, p[0], p[1]), null);
  });

  $el.on("keydown" + NS, ".ah-node-graph-viewport", function (e) {
    if (isEditable(e.target)) { return; }
    var k = e.key, mod = e.ctrlKey || e.metaKey, lower = k.length === 1 ? k.toLowerCase() : k;
    var ro = opts(st).readOnly, onButton = e.target.tagName === "BUTTON";
    if (k === " " && !onButton) {
      e.preventDefault();
      st.space = true;
      vp.setAttribute("data-hand", "true");
    } else if (mod && lower === "z") {
      e.preventDefault();
      if (e.shiftKey) { redo(st); } else { undo(st); }
    } else if (mod && lower === "y") {
      e.preventDefault(); redo(st);
    } else if (mod && lower === "a") {
      e.preventDefault(); select(st, st.g.nodes.map(function (n) { return n.id; }), "replace");
    } else if (mod && lower === "c") {
      e.preventDefault(); copy(st, st.sel);
    } else if (mod && lower === "v") {
      e.preventDefault(); if (!ro) { paste(st, st.clip, "paste"); }
    } else if (mod && lower === "x") {
      e.preventDefault();
      var cut = st.sel.slice();
      copy(st, cut);
      if (!ro && cut.length) { mutate(st, "remove", function (g) { removeNodes(g, cut); }); }
    } else if (mod && lower === "d") {
      e.preventDefault(); if (!ro) { duplicate(st, st.sel); }
    } else if (k === "Delete" || k === "Backspace") {
      if (!ro) { e.preventDefault(); deleteSelection(st); }
    } else if (k === "Escape") {
      setSelection(st, [], []);
    } else if (/^Arrow/.test(k)) {
      e.preventDefault();
      var step = e.shiftKey ? 50 : (opts(st).snap || 10);
      var dx = k === "ArrowLeft" ? -step : k === "ArrowRight" ? step : 0;
      var dy = k === "ArrowUp" ? -step : k === "ArrowDown" ? step : 0;
      if (st.sel.length && !ro) { moveNodes(st, st.sel.slice(), dx, dy); }
      else { setCamera(st, pan(st.cam, -Math.sign(dx) * (e.shiftKey ? 160 : 40), -Math.sign(dy) * (e.shiftKey ? 160 : 40))); }
    } else if (!mod && (k === "+" || k === "=")) {
      zoomBy(st, 1.25);
    } else if (!mod && k === "-") {
      zoomBy(st, 0.8);
    } else if (k === "ContextMenu" || (e.shiftKey && k === "F10")) {
      e.preventDefault();
      var r = st.vp.getBoundingClientRect();
      var at = [r.width / 2, r.height / 2];
      if (st.sel.length) {
        var n = findNode(st.g, st.sel[st.sel.length - 1]);
        var sc = [(n.pos[0] + st.cam.x) * st.cam.z, (n.pos[1] + st.cam.y) * st.cam.z];
        nodeMenu(st, n.id, [sc[0] + 20, sc[1] + 20]);
      } else if (!ro) {
        searchMenu(st, at, screenToGraph(st.cam, at[0], at[1]), null);
      }
    }
  });
  $el.on("keyup" + NS, ".ah-node-graph-viewport", function (e) {
    if (e.key === " ") {
      st.space = false;
      if (!st.hand) { vp.removeAttribute("data-hand"); }
    }
  });
}

function wireToolbar(st) {
  var $el = $(st.el), NS = AH.NS;
  $el.on("click" + NS, ".ah-node-graph-tool", function (e) {
    e.stopPropagation();
    switch (this.getAttribute("data-action")) {
      case "zoom-in": zoomBy(st, 1.25); break;
      case "zoom-out": zoomBy(st, 0.8); break;
      case "hand":
        st.hand = !st.hand;
        if (st.hand) { st.vp.setAttribute("data-hand", "true"); this.setAttribute("data-active", "true"); }
        else { st.vp.removeAttribute("data-hand"); this.removeAttribute("data-active"); }
        this.setAttribute("aria-pressed", String(st.hand));
        break;
      case "fit": fit(st); break;
      case "undo": undo(st); break;
      case "redo": redo(st); break;
      case "delete": deleteSelection(st); break;
    }
  });
  // minimap: click to centre the view there
  $el.on("pointerdown" + NS, ".ah-node-graph-minimap", function (e) {
    e.stopPropagation();
    var view = viewBox(st);
    var t = miniTransform(contentBounds(st.g.nodes.map(function (n) { return shown(st, n, false); })), view);
    var r = this.getBoundingClientRect(), p = point(e);
    var gx = t.ox + (p[0] - r.left) / t.scale, gy = t.oy + (p[1] - r.top) / t.scale;
    var z = st.cam.z;
    setCamera(st, { z: z, x: view.w / 2 - gx, y: view.h / 2 - gy });
  });
}

// ------------------------------------------------------------------
// Behaviour
// ------------------------------------------------------------------

function readLibrary(el) {
  var s = $(el).children("script.ah-node-graph-data")[0];
  if (!s) { return []; }
  try { return JSON.parse(s.textContent).library || []; } catch (x) { return []; }
}

function setGraph(st, graph) {
  var g = normalize(typeof graph === "string" ? JSON.parse(graph) : graph);
  g.nodes.forEach(function (n) { takeHtml(st, n); });
  st.g = g;
  st.past = [];
  st.future = [];
  pruneSelection(st);
  st.lsel = [];
  renderAll(st);
  writeValue(st);
}

AH.define("node-graph", {
  init: function (el, $el) {
    if (!el.id) { el.id = "ah-g" + (++seq) + Date.now().toString(36); }
    var vp = $el.children(".ah-node-graph-viewport")[0];
    var canvas = $(vp).children(".ah-node-graph-canvas")[0];
    var st = {
      el: el, vp: vp, canvasEl: canvas,
      groupsEl: $(canvas).children(".ah-node-graph-groups")[0],
      linksEl: $(canvas).children(".ah-node-graph-links")[0],
      nodesEl: $(canvas).children(".ah-node-graph-nodes")[0],
      marqueeEl: $(canvas).children(".ah-node-graph-marquee")[0],
      opts: {
        mode: el.getAttribute("data-ah-link-mode") || "spline",
        snap: parseInt(el.getAttribute("data-ah-snap") || "0", 10) || 0,
        readOnly: el.getAttribute("data-read-only") === "true",
        allowCycles: el.getAttribute("data-ah-allow-cycles") === "true"
      },
      library: readLibrary(el),
      g: { nodes: [], links: [], groups: [] },
      cam: { x: 0, y: 0, z: 1 },
      sel: [], lsel: [], sizes: {}, past: [], future: [], clip: null, pending: {},
      nodeDrag: null, linkDrag: null, wpDrag: null, marquee: null, dragLinkEl: null,
      hand: false, space: false, drags: [], cleanups: [], overlay: null
    };
    try { st.g = normalize(JSON.parse(el.getAttribute("data-ah-value") || "{}")); } catch (x) { /* empty */ }
    $.data(el, KEY, st);
    // adopt the server's cards: same template, same data
    var conn = connIndex(st.g);
    st.g.nodes.forEach(function (n) {
      var card = cardOf(st, n.id);
      if (card) { card._ahSig = cardSig(st, n, conn); }
    });
    measure(st);
    wireNodes(st);
    wireLinks(st);
    wireGroups(st);
    wireViewport(st);
    wireToolbar(st);
    applyCamera(st);
    if (el.getAttribute("data-ah-auto-fit") === "true") { fit(st); }
  },
  destroy: function (el) {
    var st = state(el);
    if (!st) { return; }
    st.drags.slice().forEach(function (f) { f(); });
    st.cleanups.forEach(function (f) { f(); });
    closeOverlay(st);
    $.removeData(el, KEY);
  },
  methods: {
    getGraph: function (el) { return clone(state(el).g); },
    getValue: function (el) { return el.getAttribute("data-ah-value"); },
    setGraph: function (el, $el, graph) { setGraph(state(el), graph); },
    fitView: function (el) { fit(state(el)); },
    undo: function (el) { undo(state(el)); },
    redo: function (el) { redo(state(el)); },
    getSelection: function (el) { return state(el).sel.slice(); },
    selectNodes: function (el, $el, list) { select(state(el), (list || []).map(String), "replace"); },
    deleteSelection: function (el) { deleteSelection(state(el)); }
  }
});
