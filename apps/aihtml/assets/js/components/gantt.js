/* Behaviour of the gantt (designs/04-components.md). Ported from sigil:
 * data/gantt (+ timeline, rendering, interactions).
 *
 * aihtml_gantt renders every row, bar, summary bar and dependency
 * path on the server. The behaviour syncs the scroll areas, collapses rows
 * (hiding rows and bars, moving bars up, showing summary bars), drags bars
 * and recomputes the positions and dependency paths of the existing
 * elements; it builds no HTML.
 *
 * Times are minutes since 1970-01-01 in UTC arithmetic: task times are
 * local wall times without a zone, so there is no DST shifting.
 *
 * Events (detail, also written to the root as data-*): ah:row-expand
 * ({row, expanded}), ah:row-click ({row}), ah:task-click ({task}),
 * ah:task-change ({task, from, to, row, kind: "move" | "resize", days}). */
import AH from "../core.js";
import "./_lib_values.js";

var DAY = 1440;

function pad(n) { return (n < 10 ? "0" : "") + n; }
// ISO date or date-time -> { t: minutes, dateOnly }
function parseT(s) {
  var m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2}))?/.exec(String(s || ""));
  if (!m) { return null; }
  var t = Math.round(Date.UTC(+m[1], +m[2] - 1, +m[3]) / 60000);
  return m[4] === undefined ? { t: t, dateOnly: true } : { t: t + (+m[4]) * 60 + (+m[5]), dateOnly: false };
}
function fmtT(t, dateOnly) {
  var d = new Date(t * 60000);
  var s = d.getUTCFullYear() + "-" + pad(d.getUTCMonth() + 1) + "-" + pad(d.getUTCDate());
  return dateOnly && t % DAY === 0 ? s : s + "T" + pad(d.getUTCHours()) + ":" + pad(d.getUTCMinutes());
}
function num(x) { return Math.round(x * 1000) / 1000; }

function conf(el) {
  return {
    cw: parseFloat(el.getAttribute("data-column-width")) || 60,
    rh: parseFloat(el.getAttribute("data-row-height")) || 40,
    origin: (parseT(el.getAttribute("data-origin")) || { t: 0 }).t,
    editable: el.hasAttribute("data-editable")
  };
}

function sidebarRows(el) { return Array.from(el.querySelectorAll(".ah-gantt-sidebar-row")); }
function barOf(el, id) {
  return Array.prototype.find.call(el.querySelectorAll(".ah-gantt-task-bar"), function (b) {
    return b.getAttribute("data-taskid") === String(id);
  });
}
function times(bar) {
  var s = parseT(bar.getAttribute("data-start")), e = parseT(bar.getAttribute("data-end"));
  return { s: s.t, e: e.t, sd: s.dateOnly, ed: e.dateOnly };
}
function pos(c, s, e) {
  return { left: (s - c.origin) / DAY * c.cw, width: Math.max((e - s) / DAY * c.cw, c.cw) };
}

// Row visibility from the expanded state of the parents (tree order):
// hides rows, grid rows and bars, moves bars, shows summary bars of
// collapsed rows, then redraws the dependency lines.
function layout(el) {
  var c = conf(el), open = {}, index = {}, parent = {}, n = 0, collapsed = [];
  sidebarRows(el).forEach(function (row) {
    var id = row.getAttribute("data-rowid"), p = row.getAttribute("data-parent");
    var vis = !p || open[p] === true;
    parent[id] = p;
    row.hidden = !vis;
    var expanded = row.getAttribute("aria-expanded");
    if (expanded === "false") { collapsed.push(id); }
    open[id] = vis && expanded !== "false";
    if (vis) { index[id] = n++; }
  });
  el.setAttribute("data-collapsed", AH.lib.values.join(collapsed));
  el.querySelectorAll(".ah-gantt-grid-row").forEach(function (g) {
    g.hidden = !(g.getAttribute("data-rowid") in index);
  });
  var bars = Array.from(el.querySelectorAll(".ah-gantt-task-bar"));
  bars.forEach(function (bar) {
    var r = bar.getAttribute("data-rowid"), t = times(bar), p = pos(c, t.s, t.e);
    bar.hidden = !(r in index);
    bar.style.left = num(p.left) + "px";
    bar.style.width = num(p.width) + "px";
    if (r in index) { bar.style.top = (index[r] * c.rh + 4) + "px"; }
  });
  function under(r, anc) {
    for (var p = parent[r]; p; p = parent[p]) { if (p === anc) { return true; } }
    return false;
  }
  el.querySelectorAll(".ah-gantt-summary-bar").forEach(function (sum) {
    var r = sum.getAttribute("data-rowid"), s = Infinity, e = -Infinity, k = 0;
    bars.forEach(function (bar) {
      if (under(bar.getAttribute("data-rowid"), r)) {
        var t = times(bar);
        s = Math.min(s, t.s); e = Math.max(e, t.e); k++;
      }
    });
    var show = k > 0 && (r in index) && collapsed.indexOf(r) >= 0;
    sum.hidden = !show;
    if (show) {
      var p = pos(c, s, e);
      sum.style.left = num(p.left) + "px";
      sum.style.width = num(p.width) + "px";
      sum.style.top = (index[r] * c.rh + 4) + "px";
    }
  });
  var h = n * c.rh;
  var layers = el.querySelectorAll(".ah-gantt-tasks-layer");
  layers.forEach(function (l) { l.style.height = h + "px"; });
  var svg = el.querySelector(".ah-gantt-deps-layer");
  if (svg) {
    var w = parseFloat(layers[0].style.width) || 0;
    svg.style.height = h + "px";
    svg.setAttribute("viewBox", "0 0 " + w + " " + h);
    drawDeps(el, c, index);
  }
}

// sigil's render-dependency-line (twin of dep_d/5 in the Erlang module)
function depD(x1, y1, x2, y2, same) {
  if (same) { return "M" + num(x1) + "," + num(y1) + " L" + num(x2) + "," + num(y2); }
  if (x1 >= x2) {
    var ext = 30 + (x1 - x2) / 3;
    return "M" + num(x1) + "," + num(y1) + " C" + num(x1 + ext) + "," + num(y1) + " " +
      num(x2 - ext) + "," + num(y2) + " " + num(x2) + "," + num(y2);
  }
  var mx = (x1 + x2) / 2;
  return "M" + num(x1) + "," + num(y1) + " C" + num(mx) + "," + num(y1) + " " +
    num(mx) + "," + num(y2) + " " + num(x2) + "," + num(y2);
}

function drawDeps(el, c, index) {
  el.querySelectorAll(".ah-gantt-dep-line").forEach(function (line) {
    var a = barOf(el, line.getAttribute("data-from")), b = barOf(el, line.getAttribute("data-to"));
    var ra = a && a.getAttribute("data-rowid"), rb = b && b.getAttribute("data-rowid");
    if (!a || !b || !(ra in index) || !(rb in index)) { line.setAttribute("d", ""); return; }
    var ta = times(a), tb = times(b), pa = pos(c, ta.s, ta.e), pb = pos(c, tb.s, tb.e);
    line.setAttribute("d", depD(pa.left + pa.width, index[ra] * c.rh + c.rh / 2,
                                pb.left, index[rb] * c.rh + c.rh / 2, ra === rb));
  });
}

// ---- rows ---------------------------------------------------------

function visibleRows(el) { return sidebarRows(el).filter(function (r) { return !r.hidden; }); }

function setExpanded(ctl, row, open, notify) {
  var el = ctl.element;
  if (!row || row.getAttribute("aria-expanded") === null) { return; }
  if ((row.getAttribute("aria-expanded") === "true") === open) { return; }
  row.setAttribute("aria-expanded", open ? "true" : "false");
  row.querySelectorAll(".ah-gantt-expand-icon").forEach(function (i) {
    i.classList.toggle("ah-gantt-expand-icon-expanded", open);
  });
  layout(el);
  if (notify) {
    el.setAttribute("data-row", row.getAttribute("data-rowid"));
    el.setAttribute("data-expanded", open ? "true" : "false");
    ctl.fire("ah:row-expand", { row: row.getAttribute("data-rowid"), expanded: open });
  }
}

function rowById(el, id) {
  return sidebarRows(el).find(function (r) { return r.getAttribute("data-rowid") === String(id); });
}

function focusRow(el, row) {
  if (!row) { return; }
  sidebarRows(el).forEach(function (r) { r.setAttribute("tabindex", "-1"); });
  row.setAttribute("tabindex", "0");
  row.focus();
}

function rowKey(ctl, row, e) {
  var el = ctl.element, vis = visibleRows(el), i = vis.indexOf(row), open = row.getAttribute("aria-expanded");
  switch (e.key) {
    case "ArrowDown": focusRow(el, vis[i + 1]); break;
    case "ArrowUp": focusRow(el, vis[i - 1]); break;
    case "Home": focusRow(el, vis[0]); break;
    case "End": focusRow(el, vis[vis.length - 1]); break;
    case "ArrowRight":
      if (open === "false") { setExpanded(ctl, row, true, true); }
      else if (open === "true") { focusRow(el, vis[i + 1]); }
      break;
    case "ArrowLeft":
      if (open === "true") { setExpanded(ctl, row, false, true); }
      else { focusRow(el, rowById(el, row.getAttribute("data-parent"))); }
      break;
    case "Enter": case " ": rowClick(ctl, row); break;
    default: return;
  }
  e.preventDefault();
}

function rowClick(ctl, row) {
  ctl.element.setAttribute("data-row", row.getAttribute("data-rowid"));
  ctl.fire("ah:row-click", { row: row.getAttribute("data-rowid") });
}

// ---- tasks --------------------------------------------------------

function taskClick(ctl, bar) {
  ctl.element.setAttribute("data-task", bar.getAttribute("data-taskid"));
  ctl.fire("ah:task-click", { task: bar.getAttribute("data-taskid") });
}

// Apply a change to a bar and fire ah:task-change.
function change(ctl, bar, s, e, row, kind, days, notify) {
  var el = ctl.element, t = times(bar);
  bar.setAttribute("data-start", fmtT(s, t.sd));
  bar.setAttribute("data-end", fmtT(e, t.ed));
  if (row) { bar.setAttribute("data-rowid", row); }
  layout(el);
  if (!notify) { return; }
  var d = {
    task: bar.getAttribute("data-taskid"), from: bar.getAttribute("data-start"),
    to: bar.getAttribute("data-end"), row: bar.getAttribute("data-rowid"), kind: kind, days: days
  };
  Object.keys(d).forEach(function (k) { el.setAttribute("data-" + k, String(d[k])); });
  ctl.fire("ah:task-change", d);
}

function taskKey(ctl, bar, e) {
  var el = ctl.element, c = conf(el);
  if (e.key === "Enter" || e.key === " ") { e.preventDefault(); taskClick(ctl, bar); return; }
  if (!c.editable) { return; }
  var t = times(bar), step = 0;
  if (e.key === "ArrowLeft") { step = -1; } else if (e.key === "ArrowRight") { step = 1; }
  if (step && e.shiftKey) {
    e.preventDefault();
    if (t.e + step * DAY > t.s) { change(ctl, bar, t.s, t.e + step * DAY, null, "resize", step, true); }
  } else if (step) {
    e.preventDefault();
    change(ctl, bar, t.s + step * DAY, t.e + step * DAY, null, "move", step, true);
  } else if (e.altKey && (e.key === "ArrowUp" || e.key === "ArrowDown")) {
    e.preventDefault();
    var vis = visibleRows(el), i = vis.indexOf(rowById(el, bar.getAttribute("data-rowid")));
    var target = i < 0 ? null : vis[i + (e.key === "ArrowUp" ? -1 : 1)];
    if (target) { change(ctl, bar, t.s, t.e, target.getAttribute("data-rowid"), "move", 0, true); }
  }
}

// One drag at a time: mousedown on a bar decides move or resize; the
// document's mousemove/mouseup finish it (sigil's setup-task-drag!).
function dragStart(ctl, e) {
  var el = ctl.element, c = conf(el), t = e.target, bar = t.closest(".ah-gantt-task-bar");
  if (!bar || e.button !== 0 || !c.editable) { return; }
  e.preventDefault();
  var st = ctl.st, rect = bar.getBoundingClientRect();
  var d = { bar: bar, x: e.pageX, y: e.pageY, moved: false,
            left: parseFloat(bar.style.left), width: rect.width };
  if (t.classList.contains("ah-gantt-resize-handle")) {
    d.mode = "resize";
    d.side = t.classList.contains("ah-gantt-resize-left") ? "left" : "right";
  } else {
    d.mode = "move";
    d.ox = e.clientX - rect.left;
    d.oy = e.clientY - rect.top;
  }
  st.drag = d;
  var ac = st.dragAc = new AbortController();
  document.addEventListener("mousemove", function (me) { dragMove(ctl, c, me); }, { signal: ac.signal });
  document.addEventListener("mouseup", function (ue) { dragEnd(ctl, c, ue); }, { signal: ac.signal });
}

function dragMove(ctl, c, e) {
  var d = ctl.st.drag;
  if (!d) { return; }
  var dx = e.pageX - d.x, dy = e.pageY - d.y;
  if (!d.moved && Math.abs(dx) + Math.abs(dy) < 4) { return; }
  d.moved = true;
  if (d.mode === "move") {
    if (!d.ghost) {
      var g = d.ghost = d.bar.cloneNode(true);
      g.removeAttribute("tabindex");
      g.removeAttribute("id");
      g.classList.add("ah-gantt-task-ghost");
      Object.assign(g.style, { position: "fixed", zIndex: 9999, opacity: 0.6, pointerEvents: "none", margin: 0,
                               width: d.width + "px", height: d.bar.offsetHeight + "px" });
      document.body.appendChild(g);
    }
    d.ghost.style.left = (e.clientX - d.ox) + "px";
    d.ghost.style.top = (e.clientY - d.oy) + "px";
  } else if (d.side === "right") {
    d.bar.style.width = Math.max(c.cw, d.width + dx) + "px";
    d.dx = dx;
  } else {
    var dxl = Math.min(dx, d.width - c.cw);
    d.bar.style.left = (d.left + dxl) + "px";
    d.bar.style.width = (d.width - dxl) + "px";
    d.dx = dxl;
  }
}

function dragStop(st) {
  if (st.dragAc) { st.dragAc.abort(); st.dragAc = null; }
  var d = st.drag;
  st.drag = null;
  if (d && d.ghost) { d.ghost.remove(); }
  return d;
}

function dragEnd(ctl, c, e) {
  var el = ctl.element, st = ctl.st, d = dragStop(st);
  if (!d || !d.moved) { return; }
  st.noClick = true;
  setTimeout(function () { st.noClick = false; }, 0);
  var t = times(d.bar);
  if (d.mode === "move") {
    var days = Math.round((e.pageX - d.x) / c.cw);
    var vis = visibleRows(el), i = vis.indexOf(rowById(el, d.bar.getAttribute("data-rowid")));
    var j = Math.max(0, Math.min(vis.length - 1, i + Math.round((e.pageY - d.y) / c.rh)));
    var row = vis[j] && vis[j].getAttribute("data-rowid");
    var rowChanged = row && row !== d.bar.getAttribute("data-rowid");
    if (days || rowChanged) {
      change(ctl, d.bar, t.s + days * DAY, t.e + days * DAY, rowChanged ? row : null, "move", days, true);
    }
  } else {
    var n = Math.round((d.dx || 0) / c.cw);
    if (d.side === "right" && t.e + n * DAY <= t.s) { n = Math.ceil((t.s - t.e) / DAY) + 1; }
    if (d.side === "left" && t.s + n * DAY >= t.e) { n = Math.floor((t.e - t.s) / DAY) - 1; }
    if (n) {
      change(ctl, d.bar, d.side === "left" ? t.s + n * DAY : t.s,
             d.side === "right" ? t.e + n * DAY : t.e, null, "resize", n, true);
    } else {
      layout(el);
    }
  }
}

function scrollToTime(el, t) {
  var c = conf(el), body = el.querySelector(".ah-gantt-timeline-body");
  if (!body) { return; }
  body.scrollLeft = Math.max(0, (t - c.origin) / DAY * c.cw - 100);
  var head = el.querySelector(".ah-gantt-timeline-header");
  if (head) { head.scrollLeft = body.scrollLeft; }
}

// The server morphs updates into the kept root (gantt_update): the
// controller stays, its listeners are delegated from the root (scroll in
// the capture phase), and the scroll areas keep their position.
AH.register("gantt", class extends AH.Controller {
  setup() {
    var ctl = this, el = this.element;
    var st = this.st = { drag: null, dragAc: null, noClick: false };
    // scroll does not bubble: listen in the capture phase
    this.listen(el, "scroll", function (e) {
      var sc = e.target;
      if (!sc.classList) { return; }
      if (sc.classList.contains("ah-gantt-timeline-body")) {
        var side = el.querySelector(".ah-gantt-sidebar-body"), head = el.querySelector(".ah-gantt-timeline-header");
        if (side) { side.scrollTop = sc.scrollTop; }
        if (head) { head.scrollLeft = sc.scrollLeft; }
      } else if (sc.classList.contains("ah-gantt-sidebar-body")) {
        var body = el.querySelector(".ah-gantt-timeline-body");
        if (body) { body.scrollTop = sc.scrollTop; }
      }
    }, { capture: true });
    this.delegate("click", ".ah-gantt-expand-icon", function (e, icon) {
      e.stopPropagation();
      var row = icon.closest(".ah-gantt-sidebar-row");
      setExpanded(ctl, row, row.getAttribute("aria-expanded") === "false", true);
    });
    this.delegate("click", ".ah-gantt-sidebar-row", function (e, row) {
      focusRow(el, row);
      rowClick(ctl, row);
    });
    this.delegate("keydown", ".ah-gantt-sidebar-row", function (e, row) { rowKey(ctl, row, e); });
    this.delegate("click", ".ah-gantt-task-bar", function (e, bar) {
      e.stopPropagation();
      if (!st.noClick) { taskClick(ctl, bar); }
    });
    this.delegate("keydown", ".ah-gantt-task-bar", function (e, bar) { taskKey(ctl, bar, e); });
    this.delegate("mousedown", ".ah-gantt-timeline-body", function (e) { dragStart(ctl, e); });
    // sigil scrolls to the first task
    var first = Infinity;
    el.querySelectorAll(".ah-gantt-task-bar").forEach(function (bar) { first = Math.min(first, times(bar).s); });
    if (first < Infinity) { scrollToTime(el, first); }
  }

  teardown() { dragStop(this.st); }

  // methods (aihtml_action:call/4, AH.invoke)
  expandRow(id) { setExpanded(this, rowById(this.element, id), true, false); }
  collapseRow(id) { setExpanded(this, rowById(this.element, id), false, false); }
  scrollToDate(iso) { var p = parseT(iso); if (p) { scrollToTime(this.element, p.t); } }
  setTask(id, from, to, row) {
    var el = this.element, bar = barOf(el, id), a = parseT(from), b = parseT(to);
    if (bar && a && b) {
      bar.setAttribute("data-start", fmtT(a.t, a.dateOnly));
      bar.setAttribute("data-end", fmtT(b.t, b.dateOnly));
      if (row) { bar.setAttribute("data-rowid", row); }
      layout(el);
    }
  }
});
