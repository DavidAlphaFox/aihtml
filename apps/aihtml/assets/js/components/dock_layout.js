/* The dock_layout behaviour (designs/04-components.md), ported from
 * sigil's layout/dock_layout (+ common, dock, pin, menu, serialize):
 * splits, tab groups and documents: tabs dragged onto the dock targets (a
 * side of a group, its centre, an edge of the layout) or out to float;
 * auto hide at an edge; splitbars; a context menu (right click, the
 * context menu key or Shift+F10 on a tab or a float's title bar).
 *
 * The arrangement is the value: JSON in data-ah-value (and the hidden
 * input), rewritten after every change, and `change' on the root when it
 * changed (user actions and the rearranging methods). Sizes are flex
 * weights (style "flex: W 1 0px"), written as percentages of the parent.
 * Elements are only ever moved, never rebuilt, so panel contents keep
 * their state; new tab groups, float windows and the menu come from the
 * shared templates (AH.tpl.dock_layout_*). The drag tracking is shared
 * with docking.js (_lib_dock.js).
 *
 * Events: "ah:panel-close" on the root with the closed panel ids (a
 * multi-value) in data-panels, and as the detail.
 */
import AH from "../core.js";
import "./_lib_dock.js";
import "./_lib_values.js";
import "virtual:ah-tpl/dock_layout_float";
import "virtual:ah-tpl/dock_layout_group";
import "virtual:ah-tpl/dock_layout_menu";

const L = AH.lib.dock;
const CROSS = 128;           // the dock cross: 3 * 40 + 2 * 4
const SHOW_DELAY = 200, HIDE_DELAY = 400;   // auto hide preview on hover

const { inside, parse, commit, fire, track, stopDrag, kids, parseHTML } = L;
// per layout root: {memo, timer, preview, labels, menu, menuAc}
const states = new WeakMap();

function round2(x) { return Math.round(x * 100) / 100; }

function div(cls, attrs) {
  const d = document.createElement("div");
  d.className = cls;
  Object.keys(attrs || {}).forEach((k) => { d.setAttribute(k, attrs[k]); });
  return d;
}

function px(v) { return typeof v === "number" ? v + "px" : v; }
function css(node, props) {
  Object.keys(props).forEach((k) => { node.style[k] = px(props[k]); });
}

// ------------------------------------------------------------------
// dock_layout: structure
// ------------------------------------------------------------------

function dlState(el) {
  let st = states.get(el);
  if (!st) {
    st = { memo: {}, timer: null, preview: null, menu: null, menuAc: null,
           labels: Object.assign({ auto_hide: "Auto Hide", float: "Float", dock: "Dock", close: "Close" },
                                 parse(el.getAttribute("data-ah-labels")) || {}) };
    states.set(el, st);
  }
  return st;
}

function inner(el) { return el.querySelector(":scope > .ah-dl-middle > .ah-dl-inner"); }
function floats(el) { return kids(el, ".ah-dl-float-container"); }
function floatWins(el) {
  const out = [];
  floats(el).forEach((f) => { out.push(...kids(f, ".ah-dl-float-window")); });
  return out;
}
function slots(el) {
  return Array.from(el.querySelectorAll(":scope > .ah-dl-autohide-preview-slot, " +
                                        ":scope > .ah-dl-middle > .ah-dl-autohide-preview-slot"));
}
function strips(el) {
  return Array.from(el.querySelectorAll(":scope > .ah-dl-autohide-strip, " +
                                        ":scope > .ah-dl-middle > .ah-dl-autohide-strip"));
}
function slotGroups(el) {
  const out = [];
  slots(el).forEach((s) => { out.push(...kids(s, ".ah-dl-tabbed")); });
  return out;
}
function slotOf(el, edge) { return slots(el).find((s) => s.matches(".ah-dl-autohide-preview-slot-" + edge)); }
function stripOf(el, edge) { return strips(el).find((s) => s.matches(".ah-dl-autohide-strip-" + edge)); }
function layoutGroups(el) { return Array.from(inner(el).querySelectorAll(".ah-dl-tabbed")); }

function isBar(n) { return n.classList.contains("ah-dl-splitbar"); }
function real(c) { return kids(c).filter((n) => !isBar(n)); }
function isContainer(c) { return !!c && !!c.matches && c.matches(".ah-dl-group, .ah-dl-inner"); }
function isHoriz(c) {
  return c.classList.contains("ah-dl-inner") ? !c.classList.contains("ah-dl-vertical")
                                             : c.classList.contains("ah-dl-horizontal");
}
function weight(n) {
  const g = parseFloat(n.style.flexGrow);
  return isNaN(g) || g <= 0 ? 1 : g;
}
function setWeight(n, w) { n.style.flex = round2(w) + " 1 0px"; }

function header(g) { return g.querySelector(":scope > .ah-dl-tabs > .ah-tabs-header"); }
function tabsOf(g) { return kids(header(g), ".ah-tabs-item"); }
function content(g) { return g.querySelector(":scope > .ah-dl-tabs > .ah-tabs-content"); }
function panelsOf(g) { return kids(content(g), ".ah-tabs-panel"); }
function pid(li) { return li.getAttribute("data-panel-id"); }
function panelFor(g, li) { return panelsOf(g)[tabsOf(g).indexOf(li)]; }
function activeTab(g) {
  const t = tabsOf(g);
  return t.find((li) => li.classList.contains("ah-tabs-item-selected")) || t[0];
}
function groupOf(node) { return node.closest(".ah-dl-tabbed"); }
function winOf(node) { return node.closest(".ah-dl-float-window"); }
function winGroup(win) { return win.querySelector(".ah-dl-tabbed"); }
function inSlot(node) { return !!node.closest(".ah-dl-autohide-preview-slot"); }
function inLayout(el, node) { const i = inner(el); return i !== node && i.contains(node); }

function flag(g, name) { return g.getAttribute("data-allow-" + name) === "true"; }
function opt(el, name) { return el.getAttribute("data-ah-" + name) !== "false"; }

function select(g, li) {
  const t = tabsOf(g), i = t.indexOf(li);
  if (i < 0) { return; }
  t.forEach((n) => {
    n.classList.remove("ah-tabs-item-selected");
    n.setAttribute("aria-selected", "false");
    n.setAttribute("tabindex", "-1");
  });
  li.classList.add("ah-tabs-item-selected");
  li.setAttribute("aria-selected", "true");
  li.setAttribute("tabindex", "0");
  panelsOf(g).forEach((p, j) => {
    p.classList.toggle("ah-tabs-panel-active", j === i);
    p.hidden = j !== i;
  });
  const win = winOf(g);
  if (win) {
    const text = li.textContent;
    win.setAttribute("aria-label", text);
    const title = win.querySelector(".ah-dl-float-title");
    if (title) { title.textContent = text; }
  }
}

function newGroup(el, pin, close) {
  const st = dlState(el);
  return parseHTML(AH.tpl.dock_layout_group({
    gid: el.id + "-n" + L.next(), document: false, pin: pin ? "true" : "false",
    close: close ? "true" : "false", pinned: "true", unpinned: false, edge: false, size: 0,
    style: false, pin_label: st.labels.auto_hide, close_label: st.labels.close, tabs: [] }))[0];
}

function newContainer(horiz) {
  return div("ah-dl-group " + (horiz ? "ah-dl-horizontal" : "ah-dl-vertical"));
}

// Take a tab and its panel out of their group (selecting a neighbour).
function extract(li) {
  const g = groupOf(li), panel = panelFor(g, li);
  const i = tabsOf(g).indexOf(li), was = li.classList.contains("ah-tabs-item-selected");
  li.remove();
  if (panel) { panel.remove(); }
  const t = tabsOf(g);
  if (was && t.length) { select(g, t[Math.min(i, t.length - 1)]); }
  return { li: li, panel: panel, from: g };
}

function insertTab(g, it, before) {
  const h = header(g), ref = before || kids(h, ".ah-dl-tabbed-actions")[0] || null;
  h.insertBefore(it.li, ref);
  const refPanel = before ? panelFor(g, before) : null;
  content(g).insertBefore(it.panel, refPanel || null);
  select(g, it.li);
}

// A group left without tabs goes away with its float window or its auto
// hide tab; one in the layout is removed by tidy().
function dropEmpty(el, g) {
  if (!g || tabsOf(g).length) { return; }
  const win = winOf(g);
  if (win) { win.remove(); return; }
  if (inSlot(g)) { discardHidden(el, g); }
  g.remove();
}

// Keep the tree regular: no empty groups, a splitbar between each pair
// of siblings, groups of one child replaced by it, a group inside a
// container of the same orientation merged into it.
function tidy(el) {
  const st = dlState(el);
  (function walk(c) {
    kids(c, ".ah-dl-group").forEach(walk);
    kids(c, ".ah-dl-tabbed").forEach((g) => { if (!tabsOf(g).length) { g.remove(); } });
    kids(c, ".ah-dl-group").forEach((g) => {
      const ks = real(g);
      if (!ks.length) { g.remove(); return; }
      if (ks.length === 1) {
        ks[0].style.flex = g.style.flex || "1 1 0px";
        c.insertBefore(ks[0], g);
        c.removeChild(g);
      } else if (isHoriz(g) === isHoriz(c)) {
        const gw = weight(g);
        let sum = 0;
        ks.forEach((k) => { sum += weight(k); });
        ks.forEach((k) => { setWeight(k, weight(k) * gw / sum); c.insertBefore(k, g); });
        c.removeChild(g);
      }
    });
    const nodes = kids(c);
    let prevReal = false;
    nodes.forEach((n, i) => {
      if (isBar(n)) {
        const nextReal = i + 1 < nodes.length && !isBar(nodes[i + 1]);
        if (!prevReal || !nextReal) { n.remove(); } else { prevReal = false; }
      } else {
        if (prevReal) { c.insertBefore(splitbar(el), n); }
        prevReal = true;
      }
    });
  })(inner(el));
  // memos of auto hidden groups whose old parent is gone
  Object.keys(st.memo).forEach((gid) => {
    if (!st.memo[gid].parent.isConnected) { delete st.memo[gid]; }
  });
  refresh(el);
}

function splitbar(el) {
  const bar = div("ah-dl-splitbar", { role: "separator" });
  if (opt(el, "resizable")) { bar.setAttribute("tabindex", "0"); }
  return bar;
}

function canPin(g) {
  const p = g.parentNode;
  if (!isContainer(p) || g.getAttribute("data-document") === "true") { return false; }
  const ks = real(p), i = ks.indexOf(g);
  return ks.length >= 2 && (i === 0 || i === ks.length - 1);
}

function refresh(el) {
  const inn = inner(el);
  inn.querySelectorAll(".ah-dl-splitbar").forEach((bar) => {
    bar.setAttribute("aria-orientation", isHoriz(bar.parentNode) ? "vertical" : "horizontal");
  });
  inn.querySelectorAll(".ah-dl-tabbed").forEach((g) => {
    const off = !canPin(g), h = header(g);
    if (!h) { return; }
    h.querySelectorAll(".ah-dl-btn-pin").forEach((b) => {
      b.classList.toggle("ah-dl-btn-disabled", off);
      if (off) { b.setAttribute("aria-disabled", "true"); } else { b.removeAttribute("aria-disabled"); }
    });
  });
}

// ------------------------------------------------------------------
// dock_layout: value
// ------------------------------------------------------------------

function groupValue(g, size) {
  const doc = g.getAttribute("data-document") === "true";
  const v = { type: doc ? "documents" : "tabs", id: g.getAttribute("data-group-id"), size: size,
              items: tabsOf(g).map(pid), active: pid(activeTab(g)) };
  if (!doc) { v.pin = flag(g, "pin"); }
  v.close = flag(g, "close");
  return v;
}

function nodeValue(n, size) {
  if (n.classList.contains("ah-dl-group")) {
    return { type: "split", orientation: isHoriz(n) ? "horizontal" : "vertical", size: size,
             items: kidsValue(n) };
  }
  if (n.classList.contains("ah-dl-tabbed")) { return groupValue(n, size); }
  if (n.classList.contains("ah-dl-panel")) { return { type: "panel", size: size, item: pid(n) }; }
  return null;
}

function kidsValue(c) {
  const ks = real(c);
  let total = 0;
  ks.forEach((k) => { total += weight(k); });
  return ks.map((k) => nodeValue(k, round2(weight(k) * 100 / total))).filter((v) => v);
}

function dlSerialize(el) {
  const inn = inner(el);
  let out;
  if (!isHoriz(inn) && real(inn).length > 1) {
    out = [{ type: "split", orientation: "vertical", size: 100, items: kidsValue(inn) }];
  } else {
    out = kidsValue(inn);
  }
  floatWins(el).forEach((w) => {
    const g = winGroup(w);
    if (!g) { return; }
    const v = groupValue(g);
    out.push({ type: "float", id: w.getAttribute("data-group-id"), items: v.items, active: v.active,
               x: Math.round(parseFloat(w.style.left) || 0), y: Math.round(parseFloat(w.style.top) || 0),
               width: Math.round(parseFloat(w.style.width) || w.offsetWidth),
               height: Math.round(parseFloat(w.style.height) || w.offsetHeight) });
  });
  slotGroups(el).forEach((g) => {
    const v = groupValue(g);
    out.push({ type: "autohide", id: v.id, edge: g.getAttribute("data-edge"),
               size: Math.round(parseFloat(g.getAttribute("data-size")) || 250),
               items: v.items, active: v.active, pin: v.pin, close: v.close });
  });
  return JSON.stringify(out);
}

function done(el, fire_) {
  tidy(el);
  return commit(el, dlSerialize(el), fire_);
}

// ------------------------------------------------------------------
// dock_layout: docking
// ------------------------------------------------------------------

// Put a new group beside a target (splitting the target's space), or in
// a new container around the target when it runs the other way.
function insertBeside(target, g, zone) {
  const horiz = zone === "left" || zone === "right", before = zone === "left" || zone === "top";
  const p = target.parentNode;
  if (isContainer(p) && isHoriz(p) === horiz) {
    const w = weight(target);
    setWeight(target, w / 2);
    setWeight(g, w / 2);
    p.insertBefore(g, before ? target : target.nextSibling);
  } else {
    const wrap = newContainer(horiz);
    wrap.style.flex = target.style.flex || "1 1 0px";
    p.insertBefore(wrap, target);
    setWeight(target, 50);
    setWeight(g, 50);
    if (before) { wrap.appendChild(g); wrap.appendChild(target); }
    else { wrap.appendChild(target); wrap.appendChild(g); }
  }
}

// Put a group at an edge of the whole layout, a quarter of its size.
function insertEdge(el, g, zone, size) {
  const inn = inner(el), horiz = zone === "edge-left" || zone === "edge-right";
  const first = zone === "edge-left" || zone === "edge-top";
  let ks = real(inn);
  if (ks.length > 1 && isHoriz(inn) !== horiz) {
    const wrap = newContainer(isHoriz(inn));
    while (inn.firstChild) { wrap.appendChild(inn.firstChild); }
    setWeight(wrap, 100);
    inn.appendChild(wrap);
    ks = [wrap];
  }
  inn.classList.toggle("ah-dl-vertical", !horiz);
  let total = 0;
  ks.forEach((k) => { total += weight(k); });
  let share = 0.25;
  if (size && ks.length) {
    const full = horiz ? inn.clientWidth : inn.clientHeight;
    if (full > size) { share = Math.min(0.75, size / full); }
  }
  setWeight(g, ks.length ? total * share / (1 - share) : 100);
  inn.insertBefore(g, first ? inn.firstChild : null);
}

// Dock tab items (from a drag, a float or a server-rendered group) at a
// zone: "center" of a target group, a side of it, or an edge.
function dockItems(el, items, zone, target, flags) {
  if (zone === "center") {
    items.forEach((it) => { insertTab(target, it); });
    return;
  }
  const g = newGroup(el, flags.pin, flags.close);
  items.forEach((it) => { insertTab(g, it); });
  if (flags.active) { select(g, flags.active); }
  if (/^edge-/.test(zone)) { insertEdge(el, g, zone); }
  else { insertBeside(target, g, zone); }
}

function defaultTarget(el) {
  const gs = layoutGroups(el);
  return gs.find((g) => g.getAttribute("data-document") === "true") || gs[0];
}

function floatItems(el, items, x, y, flags) {
  const st = dlState(el), rr = el.getBoundingClientRect();
  const w = 300, h = 200;
  x = Math.max(0, Math.min(Math.round(x), Math.round(rr.width) - 120));
  y = Math.max(0, Math.min(Math.round(y), Math.round(rr.height) - 60));
  const win = parseHTML(AH.tpl.dock_layout_float({
    fid: el.id + "-f" + L.next(), title: "", x: x, y: y, width: w, height: h,
    close_label: st.labels.close, body: "" }))[0];
  const g = newGroup(el, flags.pin, flags.close);
  win.querySelector(":scope > .ah-dl-float-body").appendChild(g);
  floats(el)[0].appendChild(win);
  items.forEach((it) => { insertTab(g, it); });
  if (flags.active) { select(g, flags.active); }
  raise(win);
  return win;
}

function raise(win) {
  let max = 0;
  kids(win.parentNode, ".ah-dl-float-window").forEach((w) => {
    if (w !== win) { max = Math.max(max, parseInt(w.style.zIndex, 10) || 0); }
  });
  win.style.zIndex = max + 1;
}

function sourceFlags(g) {
  return { pin: flag(g, "pin") || g.getAttribute("data-document") === "true",
           close: flag(g, "close") || g.getAttribute("data-document") === "true" };
}

function floatTab(el, li, x, y) {
  const g = groupOf(li), flags = sourceFlags(g), it = extract(li);
  dropEmpty(el, g);
  return floatItems(el, [it], x, y, flags);
}

// Dock a float window's tabs back (the whole group for a side or edge).
function dockFloat(el, win, zone, target) {
  const g = winGroup(win);
  if (!zone) {
    target = defaultTarget(el);
    zone = target ? "center" : "edge-right";
  }
  if (zone === "center") {
    const act = activeTab(g);
    tabsOf(g).forEach((li) => { insertTab(target, extract(li)); });
    select(target, act);
  } else {
    g.removeAttribute("style");
    if (/^edge-/.test(zone)) { insertEdge(el, g, zone); } else { insertBeside(target, g, zone); }
  }
  win.remove();
}

// ------------------------------------------------------------------
// dock_layout: auto hide
// ------------------------------------------------------------------

function detectEdge(g) {
  for (let node = g; ; node = node.parentNode) {
    const p = node.parentNode;
    if (!isContainer(p)) { return "left"; }
    const ks = real(p), i = ks.indexOf(node), h = isHoriz(p);
    if (ks.length > 1 && i === 0) { return h ? "left" : "top"; }
    if (ks.length > 1 && i === ks.length - 1) { return h ? "right" : "bottom"; }
    if (p.classList.contains("ah-dl-inner")) { return "left"; }
  }
}

function stripTab(el, gid) {
  for (const s of strips(el)) {
    const t = kids(s, ".ah-dl-autohide-tab").find((n) => n.getAttribute("data-group-id") === gid);
    if (t) { return t; }
  }
  return null;
}

function setPinned(g, pinned) {
  g.setAttribute("data-pinned", pinned ? "true" : "false");
  header(g).querySelectorAll(".ah-dl-btn-pin").forEach((b) => {
    b.classList.toggle("ah-dl-unpinned", !pinned);
    b.setAttribute("aria-pressed", pinned ? "false" : "true");
  });
}

function autoHide(el, g) {
  if (!canPin(g)) { return false; }
  const st = dlState(el), gid = g.getAttribute("data-group-id"), edge = detectEdge(g);
  const p = g.parentNode, side = edge === "left" || edge === "right";
  const size = Math.round(side ? g.offsetWidth : g.offsetHeight) || 250;
  st.memo[gid] = { parent: p, index: real(p).indexOf(g), weight: weight(g) };
  setPinned(g, false);
  g.setAttribute("data-edge", edge);
  g.setAttribute("data-size", size);
  g.removeAttribute("style");
  const focus = g !== document.activeElement && g.contains(document.activeElement);
  slotOf(el, edge).appendChild(g);
  const tab = div("ah-dl-autohide-tab", { role: "button", tabindex: "0", "aria-expanded": "false",
                                          "data-group-id": gid });
  tab.textContent = tabsOf(g)[0] ? tabsOf(g)[0].textContent : "";
  stripOf(el, edge).appendChild(tab);
  if (focus) { tab.focus(); }
  return true;
}

function showPreview(el, gid) {
  const st = dlState(el);
  clearTimeout(st.timer);
  const g = slotGroups(el).find((n) => n.getAttribute("data-group-id") === gid);
  if (!g) { return; }
  if (st.preview && st.preview !== gid) { hidePreview(el); }
  const edge = g.getAttribute("data-edge"), slot = g.parentNode;
  kids(slot, ".ah-dl-tabbed").forEach((n) => { n.classList.remove("ah-dl-autohide-current"); });
  g.classList.add("ah-dl-autohide-current");
  slot.style.display = "flex";
  slot.style.flex = "0 0 " + (parseFloat(g.getAttribute("data-size")) ||
                              (edge === "top" || edge === "bottom" ? 200 : 280)) + "px";
  const tab = stripTab(el, gid);
  if (tab) {
    tab.classList.add("ah-dl-autohide-tab-active");
    tab.setAttribute("aria-expanded", "true");
  }
  st.preview = gid;
}

function hidePreview(el) {
  const st = dlState(el);
  clearTimeout(st.timer);
  slots(el).forEach((s) => {
    s.style.display = "";
    s.style.flex = "";
    kids(s, ".ah-dl-tabbed").forEach((n) => { n.classList.remove("ah-dl-autohide-current"); });
  });
  strips(el).forEach((s) => {
    kids(s, ".ah-dl-autohide-tab").forEach((t) => {
      t.classList.remove("ah-dl-autohide-tab-active");
      t.setAttribute("aria-expanded", "false");
    });
  });
  st.preview = null;
}

function discardHidden(el, g) {
  const st = dlState(el), gid = g.getAttribute("data-group-id");
  if (st.preview === gid) { hidePreview(el); }
  const tab = stripTab(el, gid);
  if (tab) { tab.remove(); }
  delete st.memo[gid];
}

// Back from the edge: into its old place if that still exists, else at
// its edge of the layout.
function repin(el, g) {
  const st = dlState(el), gid = g.getAttribute("data-group-id");
  const edge = g.getAttribute("data-edge") || "left", size = parseFloat(g.getAttribute("data-size"));
  const m = st.memo[gid];
  discardHidden(el, g);
  g.classList.remove("ah-dl-autohide-current");
  g.removeAttribute("data-edge");
  g.removeAttribute("data-size");
  setPinned(g, true);
  if (m && m.parent.isConnected && inLayout(el, m.parent) || m && m.parent === inner(el)) {
    const ks = real(m.parent);
    m.parent.insertBefore(g, m.index < ks.length ? ks[m.index] : null);
    setWeight(g, m.weight);
  } else {
    insertEdge(el, g, "edge-" + edge, size);
  }
}

function scheduleHide(el) {
  const st = dlState(el);
  clearTimeout(st.timer);
  st.timer = setTimeout(() => {
    const a = document.activeElement;
    if (!kids(el, ".ah-dl-context-menu").length &&
        !slots(el).some((s) => s !== a && s.contains(a))) { hidePreview(el); }
  }, HIDE_DELAY);
}

// ------------------------------------------------------------------
// dock_layout: closing
// ------------------------------------------------------------------

function closed(el, ids, user) {
  if (user && ids.length) { fire(el, "ah:panel-close", "panels", AH.lib.values.join(ids)); }
}

function closeGroup(el, g, user) {
  const ids = tabsOf(g).map(pid);
  const win = winOf(g);
  if (inSlot(g)) { discardHidden(el, g); }
  L.drop(win || g);
  closed(el, ids, user);
}

function closeTab(el, li, user) {
  const g = groupOf(li), focus = g.contains(document.activeElement) && g !== document.activeElement;
  const it = extract(li);
  L.drop(it.panel);
  li.remove();
  if (focus && tabsOf(g).length) { activeTab(g).focus(); }
  dropEmpty(el, g);
  closed(el, [pid(li)], user);
}

// ------------------------------------------------------------------
// dock_layout: dragging
// ------------------------------------------------------------------

function overlay(el) { return kids(el, ".ah-dl-dock-overlay")[0] || null; }

function clearZones(ov) {
  if (!ov) { return; }
  ov.querySelectorAll(".ah-dl-dock-zone-active").forEach((n) => { n.classList.remove("ah-dl-dock-zone-active"); });
  ov.querySelectorAll(".ah-dl-dock-edge-active").forEach((n) => { n.classList.remove("ah-dl-dock-edge-active"); });
  kids(ov, ".ah-dl-dock-preview").forEach((n) => { n.classList.remove("ah-dl-dock-preview-visible"); });
}

// The smallest tab group of the layout under the pointer.
function groupAt(el, x, y, skip) {
  let best = null, area = Infinity;
  layoutGroups(el).forEach((g) => {
    if (g === skip) { return; }
    const r = g.getBoundingClientRect();
    if (inside(x, y, r) && r.width * r.height < area) { area = r.width * r.height; best = g; }
  });
  return best;
}

// Place the cross over the group under the pointer and find the zone
// under it: {zone, target} or null; shows the preview rectangle.
function trackZones(el, x, y, skip) {
  const ov = overlay(el);
  if (!ov) { return null; }
  const rr = el.getBoundingClientRect(), cross = kids(ov, ".ah-dl-dock-cross")[0];
  const target = groupAt(el, x, y, skip);
  if (cross) {
    if (target) {
      const tr = target.getBoundingClientRect();
      css(cross, { display: "grid",
                   left: Math.round(tr.left - rr.left + tr.width / 2 - CROSS / 2),
                   top: Math.round(tr.top - rr.top + tr.height / 2 - CROSS / 2) });
    } else {
      cross.style.display = "none";
    }
  }
  clearZones(ov);
  // the edges of the layout first: the cross of a group at the rim may
  // reach under them
  let hit = null;
  kids(ov, ".ah-dl-dock-edge").forEach((n) => {
    if (!hit && inside(x, y, n.getBoundingClientRect())) {
      hit = { zone: n.getAttribute("data-zone"), target: null, el: n };
    }
  });
  if (!hit && target && cross) {
    kids(cross, ".ah-dl-dock-zone").forEach((n) => {
      if (!hit && inside(x, y, n.getBoundingClientRect())) {
        hit = { zone: n.getAttribute("data-zone"), target: target, el: n };
      }
    });
  }
  if (!hit) { return null; }
  hit.el.classList.add(hit.target ? "ah-dl-dock-zone-active" : "ah-dl-dock-edge-active");
  const box = (hit.target || inner(el)).getBoundingClientRect();
  const l = box.left - rr.left, t = box.top - rr.top, w = box.width, h = box.height;
  const p = { top: [l, t, w, h / 2], bottom: [l, t + h / 2, w, h / 2], left: [l, t, w / 2, h],
              right: [l + w / 2, t, w / 2, h], center: [l, t, w, h],
              "edge-top": [l, t, w, h / 4], "edge-bottom": [l, t + h * 0.75, w, h / 4],
              "edge-left": [l, t, w / 4, h], "edge-right": [l + w * 0.75, t, w / 4, h] }[hit.zone];
  kids(ov, ".ah-dl-dock-preview").forEach((n) => {
    n.classList.add("ah-dl-dock-preview-visible");
    css(n, { left: p[0], top: p[1], width: p[2], height: p[3] });
  });
  return hit;
}

function showOverlay(el) {
  const ov = overlay(el);
  if (ov) { ov.classList.add("ah-dl-dock-overlay-visible"); }
}
function hideOverlay(el) {
  const ov = overlay(el);
  if (!ov) { return; }
  clearZones(ov);
  ov.classList.remove("ah-dl-dock-overlay-visible");
  kids(ov, ".ah-dl-dock-cross").forEach((n) => { n.style.display = "none"; });
}

function dragTab(el, li, e) {
  const g = groupOf(li);
  const allowDock = opt(el, "allow-dock"), allowFloat = opt(el, "allow-float");
  let ghost = null, zone = null;
  // a group of one tab is not a target for its own tab
  const skip = () => (tabsOf(g).length === 1 ? g : null);
  track(el, e, {
    start: (ev) => {
      ghost = div("ah-dl-drag-ghost", { "aria-hidden": "true" });
      ghost.textContent = li.textContent;
      css(ghost, { left: ev.clientX, top: ev.clientY });
      el.appendChild(ghost);
      li.classList.add("ah-dl-tab-dragging");
      if (allowDock) { showOverlay(el); }
    },
    move: (ev) => {
      css(ghost, { left: ev.clientX, top: ev.clientY });
      zone = allowDock ? trackZones(el, ev.clientX, ev.clientY, skip()) : null;
    },
    end: (ev, cancelled) => {
      if (ghost) { ghost.remove(); }
      li.classList.remove("ah-dl-tab-dragging");
      hideOverlay(el);
      if (cancelled || !ev) { return; }
      const h = header(g);
      if (zone && !(zone.zone === "center" && zone.target === g)) {
        const flags = sourceFlags(g), it = extract(li);
        dropEmpty(el, g);
        dockItems(el, [it], zone.zone, zone.target, flags);
      } else if (inside(ev.clientX, ev.clientY, h.getBoundingClientRect())) {
        // reorder within the header
        let over = null;
        tabsOf(g).forEach((t) => {
          const r = t.getBoundingClientRect();
          if (!over && t !== li && ev.clientX < r.left + r.width / 2) { over = t; }
        });
        if (over !== li.nextSibling) { insertTab(g, extract(li), over); }
      } else if (allowFloat && !(zone && zone.target === g)) {
        const rr = el.getBoundingClientRect();
        floatTab(el, li, ev.clientX - rr.left - 20, ev.clientY - rr.top - 12);
      } else {
        return;
      }
      if (done(el, true) && el.contains(li)) { li.focus(); }
    }
  });
}

function dragFloat(el, win, e) {
  const allowDock = opt(el, "allow-dock");
  let x0, y0, zone = null;
  track(el, e, {
    start: () => {
      x0 = parseFloat(win.style.left) || 0;
      y0 = parseFloat(win.style.top) || 0;
      if (allowDock) { showOverlay(el); }
    },
    move: (ev) => {
      win.style.left = Math.round(x0 + ev.clientX - e.clientX) + "px";
      win.style.top = Math.round(y0 + ev.clientY - e.clientY) + "px";
      zone = allowDock ? trackZones(el, ev.clientX, ev.clientY, null) : null;
    },
    end: (ev, cancelled) => {
      hideOverlay(el);
      if (cancelled) {
        win.style.left = x0 + "px";
        win.style.top = y0 + "px";
        return;
      }
      if (zone) { dockFloat(el, win, zone.zone, zone.target); }
      done(el, true);
    }
  }, 3);
}

function resizeFloat(el, win, e) {
  const w0 = win.offsetWidth, h0 = win.offsetHeight;
  track(el, e, {
    start: () => {},
    move: (ev) => {
      win.style.width = Math.max(150, Math.round(w0 + ev.clientX - e.clientX)) + "px";
      win.style.height = Math.max(100, Math.round(h0 + ev.clientY - e.clientY)) + "px";
    },
    end: (ev, cancelled) => {
      if (cancelled) { win.style.width = w0 + "px"; win.style.height = h0 + "px"; }
      done(el, !cancelled);
    }
  }, 0);
}

// Resize the two neighbours of a splitbar by `delta' px from sizes
// `sp', `sn' (px) and weights `wp', `wn'.
function resizePair(el, bar, delta, base) {
  const prev = bar.previousElementSibling, next = bar.nextElementSibling;
  const min = Math.min(parseFloat(el.getAttribute("data-ah-min-size")) || 0, (base.sp + base.sn) / 2);
  const np = Math.max(min, Math.min(base.sp + delta, base.sp + base.sn - min));
  const total = base.wp + base.wn;
  setWeight(prev, total * np / (base.sp + base.sn));
  setWeight(next, total - total * np / (base.sp + base.sn));
  return np - base.sp;
}

function pairBase(bar) {
  const prev = bar.previousElementSibling, next = bar.nextElementSibling;
  const horiz = isHoriz(bar.parentNode);
  const a = prev.getBoundingClientRect(), b = next.getBoundingClientRect();
  return { horiz: horiz, sp: horiz ? a.width : a.height, sn: horiz ? b.width : b.height,
           wp: weight(prev), wn: weight(next) };
}

function dragSplitbar(el, bar, e) {
  const feedback = el.getAttribute("data-ah-resize-mode") === "feedback";
  let base, line = null, delta = 0;
  track(el, e, {
    start: () => {
      base = pairBase(bar);
      if (feedback) {
        const rr = el.getBoundingClientRect(), br = bar.getBoundingClientRect();
        line = div("ah-dl-resize-feedback " +
                   (base.horiz ? "ah-dl-resize-feedback-h" : "ah-dl-resize-feedback-v"));
        css(line, base.horiz ? { left: br.left - rr.left, top: br.top - rr.top, bottom: "auto", height: br.height }
                             : { top: br.top - rr.top, left: br.left - rr.left, right: "auto", width: br.width });
        el.appendChild(line);
        base.line = base.horiz ? br.left - rr.left : br.top - rr.top;
      }
    },
    move: (ev) => {
      const d = base.horiz ? ev.clientX - e.clientX : ev.clientY - e.clientY;
      if (feedback) {
        const min = Math.min(parseFloat(el.getAttribute("data-ah-min-size")) || 0, (base.sp + base.sn) / 2);
        delta = Math.max(min - base.sp, Math.min(d, base.sn - min));
        line.style[base.horiz ? "left" : "top"] = (base.line + delta) + "px";
      } else {
        resizePair(el, bar, d, base);
      }
    },
    end: (ev, cancelled) => {
      if (line) { line.remove(); }
      if (cancelled) {
        setWeight(bar.previousElementSibling, base.wp);
        setWeight(bar.nextElementSibling, base.wn);
        return;
      }
      if (feedback) { resizePair(el, bar, delta, base); }
      done(el, true);
    }
  }, 0);
}

// ------------------------------------------------------------------
// dock_layout: context menu
// ------------------------------------------------------------------

function menuItems(el, g, win) {
  const lb = dlState(el).labels, items = [];
  const item = (action, label, disabled) => {
    items.push({ action: action, label: label, disabled: !!disabled, divider: false });
  };
  const allowFloat = opt(el, "allow-float");
  if (win) {
    if (opt(el, "allow-dock")) { item("dock", lb.dock); }
    item("close-float", lb.close);
  } else if (g.getAttribute("data-document") === "true") {
    if (allowFloat) { item("float", lb.float); }
    if (flag(g, "close")) { items.push({ divider: true }); item("close", lb.close); }
  } else if (inSlot(g)) {
    item("unpin", lb.dock);
    if (allowFloat) { item("float", lb.float); }
    if (flag(g, "close")) { items.push({ divider: true }); item("close", lb.close); }
  } else {
    if (flag(g, "pin")) { item("pin", lb.auto_hide, !canPin(g)); }
    if (allowFloat) { item("float", lb.float); }
    if (flag(g, "close")) { items.push({ divider: true }); item("close", lb.close); }
  }
  if (items.length && items[0].divider) { items.shift(); }
  return items;
}

function menuEl(el) { return kids(el, ".ah-dl-context-menu")[0] || null; }
function menuEntries(el) {
  const m = menuEl(el);
  return m ? kids(m, ".ah-dl-menu-item").filter((n) => !n.classList.contains("ah-dl-menu-item-disabled")) : [];
}

function closeMenu(el, refocus) {
  const st = dlState(el), m = menuEl(el);
  if (!m) { return; }
  kids(el, ".ah-dl-context-menu").forEach((n) => { n.remove(); });
  if (st.menuAc) { st.menuAc.abort(); st.menuAc = null; }
  if (refocus && st.menu && st.menu.focus && el.contains(st.menu.focus) && st.menu.focus !== el) {
    st.menu.focus.focus();
  }
  st.menu = null;
}

function openMenu(el, g, win, li, x, y) {
  closeMenu(el, false);
  const items = menuItems(el, g, win);
  if (!items.length) { return; }
  const st = dlState(el), rr = el.getBoundingClientRect();
  const m = parseHTML(AH.tpl.dock_layout_menu({
    x: Math.round(x - rr.left), y: Math.round(y - rr.top), items: items }))[0];
  el.appendChild(m);
  if (m.offsetLeft + m.offsetWidth > el.clientWidth) { m.style.left = Math.max(0, el.clientWidth - m.offsetWidth) + "px"; }
  if (m.offsetTop + m.offsetHeight > el.clientHeight) { m.style.top = Math.max(0, el.clientHeight - m.offsetHeight) + "px"; }
  st.menu = { g: g, win: win, li: li, x: x, y: y, focus: document.activeElement };
  st.menuAc = new AbortController();
  document.addEventListener("pointerdown", (e) => {
    if (!m.contains(e.target)) { closeMenu(el, false); }
  }, { signal: st.menuAc.signal });
  const first = menuEntries(el)[0];
  if (first) { first.focus(); }
}

function runMenu(el, action) {
  const st = dlState(el), m = st.menu;
  if (!m) { return; }
  closeMenu(el, false);
  const rr = el.getBoundingClientRect(), g = m.g;
  let focus = null;
  switch (action) {
    case "pin":
      if (autoHide(el, g)) { focus = stripTab(el, g.getAttribute("data-group-id")); }
      break;
    case "unpin":
      repin(el, g);
      focus = activeTab(g);
      break;
    case "float": {
      const li = m.li && groupOf(m.li) === g ? m.li : activeTab(g);
      floatTab(el, li, m.x - rr.left, m.y - rr.top);
      focus = li;
      break;
    }
    case "dock":
      focus = activeTab(winGroup(m.win));
      dockFloat(el, m.win);
      break;
    case "close-float":
      closeGroup(el, winGroup(m.win), true);
      break;
    case "close":
      closeGroup(el, g, true);
      break;
    default:
      return;
  }
  done(el, true);
  if (focus && el.contains(focus)) { focus.focus(); }
  else if (m.focus && el.contains(m.focus)) { m.focus.focus(); }
}

function menuKey(el, e) {
  const items = menuEntries(el);
  const i = items.indexOf(document.activeElement);
  const focusAt = (n) => { if (items[n]) { items[n].focus(); } };
  switch (e.key) {
    case "ArrowDown": focusAt((i + 1) % items.length); break;
    case "ArrowUp": focusAt((i - 1 + items.length) % items.length); break;
    case "Home": focusAt(0); break;
    case "End": focusAt(items.length - 1); break;
    case "Enter": case " ":
      if (i >= 0) { runMenu(el, items[i].getAttribute("data-action")); }
      break;
    case "Escape": case "Tab": closeMenu(el, true); break;
    default: return;
  }
  e.preventDefault();
}

function menuAt(node) {
  const r = node.getBoundingClientRect();
  return { x: r.left, y: r.bottom };
}

// ------------------------------------------------------------------
// dock_layout: behaviour
// ------------------------------------------------------------------

const TAB = ".ah-dl-tabs > .ah-tabs-header > .ah-tabs-item";

function findTab(el, id) {
  return Array.from(el.querySelectorAll(TAB)).find((li) =>
    pid(li) === String(id) && li.closest(".ah-dl") === el);
}

function dlDisabled(el) { return el.classList.contains("ah-dl-disabled"); }

// Show a panel: select its tab, open its auto hide group, raise its float.
function activate(el, id) {
  const li = findTab(el, id);
  if (!li) { return; }
  const g = groupOf(li), win = winOf(g);
  if (inSlot(g)) { showPreview(el, g.getAttribute("data-group-id")); }
  if (win) { raise(win); }
  if (!li.classList.contains("ah-tabs-item-selected")) {
    select(g, li);
    commit(el, dlSerialize(el), true);
  }
}

AH.register("dock-layout", class extends AH.Controller {
  setup() {
    const el = this.element;
    const st = dlState(el);
    const mine = (node) => node.closest(".ah-dl") === el && !dlDisabled(el);
    // mouseenter / mouseleave of the matches of a selector, from the
    // delegated over / out
    const hover = (selector, enter, leave) => {
      this.delegate("mouseover", selector, (e, n) => {
        if (!(e.relatedTarget && n.contains(e.relatedTarget))) { enter(n); }
      });
      this.delegate("mouseout", selector, (e, n) => {
        if (!(e.relatedTarget && n.contains(e.relatedTarget))) { leave(n); }
      });
    };

    // tabs: select, keyboard, drag, context menu
    this.delegate("click", TAB, (e, li) => {
      if (!mine(li)) { return; }
      const g = groupOf(li);
      if (!li.classList.contains("ah-tabs-item-selected")) {
        select(g, li);
        commit(el, dlSerialize(el), true);
      }
    });
    this.delegate("keydown", TAB, (e, li) => {
      if (!mine(li)) { return; }
      const g = groupOf(li), t = tabsOf(g), i = t.indexOf(li);
      let n = null;
      switch (e.key) {
        case "ArrowRight": n = (i + 1) % t.length; break;
        case "ArrowLeft": n = (i - 1 + t.length) % t.length; break;
        case "Home": n = 0; break;
        case "End": n = t.length - 1; break;
        case "Delete":
          if (flag(g, "close") && !winOf(g)) {
            e.preventDefault();
            closeTab(el, li, true);
            done(el, true);
          }
          return;
        case "ContextMenu": case "F10": {
          if (e.key === "F10" && !e.shiftKey) { return; }
          e.preventDefault();
          const p = menuAt(li);
          openMenu(el, g, winOf(g), li, p.x, p.y);
          return;
        }
        default: return;
      }
      e.preventDefault();
      select(g, t[n]);
      t[n].focus();
      commit(el, dlSerialize(el), true);
    });
    this.delegate("pointerdown", TAB, (e, li) => {
      if (e.button !== 0 || !mine(li)) { return; }
      const g = groupOf(li);
      if (winOf(g) || inSlot(g) || (!opt(el, "allow-float") && !opt(el, "allow-dock"))) { return; }
      dragTab(el, li, e);
    });
    this.delegate("contextmenu", TAB + ", .ah-dl-tabbed-actions, .ah-dl-float-titlebar", (e, n) => {
      if (!mine(n)) { return; }
      e.preventDefault();
      const win = winOf(n);
      const g = win ? winGroup(win) : groupOf(n);
      openMenu(el, g, win, n.classList.contains("ah-tabs-item") ? n : null, e.clientX, e.clientY);
    });
    this.delegate("click", ".ah-dl-context-menu > .ah-dl-menu-item", (e, n) => {
      e.stopPropagation();
      if (!n.classList.contains("ah-dl-menu-item-disabled")) { runMenu(el, n.getAttribute("data-action")); }
    });
    this.delegate("keydown", ".ah-dl-context-menu", (e) => { menuKey(el, e); });

    // group buttons
    this.delegate("click", ".ah-dl-tabbed-actions > .ah-dl-btn-pin", (e, b) => {
      if (!mine(b) || b.classList.contains("ah-dl-btn-disabled")) { return; }
      e.stopPropagation();
      const g = groupOf(b);
      if (inSlot(g)) {
        hidePreview(el);
        repin(el, g);
        done(el, true);
        activeTab(g).focus();
      } else if (autoHide(el, g)) {
        done(el, true);
      }
    });
    this.delegate("click", ".ah-dl-tabbed-actions > .ah-dl-btn-close", (e, b) => {
      if (!mine(b)) { return; }
      e.stopPropagation();
      closeGroup(el, groupOf(b), true);
      done(el, true);
    });

    // float windows
    this.delegate("pointerdown", ".ah-dl-float-window", (e, w) => {
      if (mine(w)) { raise(w); }
    });
    this.delegate("pointerdown", ".ah-dl-float-titlebar", (e, bar) => {
      if (e.button !== 0 || !mine(bar) || e.target.closest(".ah-dl-btn")) { return; }
      dragFloat(el, winOf(bar), e);
    });
    this.delegate("pointerdown", ".ah-dl-float-resize-se", (e, n) => {
      if (e.button !== 0 || !mine(n)) { return; }
      e.preventDefault();
      e.stopPropagation();
      resizeFloat(el, winOf(n), e);
    });
    this.delegate("click", ".ah-dl-float-actions > .ah-dl-btn-close", (e, b) => {
      if (!mine(b)) { return; }
      e.stopPropagation();
      closeGroup(el, winGroup(winOf(b)), true);
      done(el, true);
    });
    this.delegate("keydown", ".ah-dl-float-titlebar", (e, bar) => {
      if (e.target !== bar || !mine(bar)) { return; }
      const win = winOf(bar), step = 10;
      const dx = e.key === "ArrowLeft" ? -step : e.key === "ArrowRight" ? step : 0;
      const dy = e.key === "ArrowUp" ? -step : e.key === "ArrowDown" ? step : 0;
      if (e.key === "ContextMenu" || (e.key === "F10" && e.shiftKey)) {
        e.preventDefault();
        const p = menuAt(bar);
        openMenu(el, winGroup(win), win, null, p.x, p.y);
        return;
      }
      if (!dx && !dy) { return; }
      e.preventDefault();
      if (e.shiftKey) {
        win.style.width = Math.max(150, win.offsetWidth + dx) + "px";
        win.style.height = Math.max(100, win.offsetHeight + dy) + "px";
      } else {
        win.style.left = ((parseFloat(win.style.left) || 0) + dx) + "px";
        win.style.top = ((parseFloat(win.style.top) || 0) + dy) + "px";
      }
      commit(el, dlSerialize(el), true);
    });

    // splitbars
    this.delegate("pointerdown", ".ah-dl-splitbar", (e, bar) => {
      if (e.button !== 0 || !mine(bar) || !opt(el, "resizable")) { return; }
      e.preventDefault();
      dragSplitbar(el, bar, e);
    });
    this.delegate("keydown", ".ah-dl-splitbar", (e, bar) => {
      if (!mine(bar) || !opt(el, "resizable")) { return; }
      const horiz = isHoriz(bar.parentNode), step = e.shiftKey ? 50 : 10;
      const d = { ArrowLeft: horiz ? -step : 0, ArrowRight: horiz ? step : 0,
                  ArrowUp: horiz ? 0 : -step, ArrowDown: horiz ? 0 : step }[e.key];
      if (!d) { return; }
      e.preventDefault();
      resizePair(el, bar, d, pairBase(bar));
      commit(el, dlSerialize(el), true);
    });

    // auto hide strips and previews
    hover(".ah-dl-autohide-tab", (tab) => {
      if (!mine(tab)) { return; }
      const gid = tab.getAttribute("data-group-id");
      clearTimeout(st.timer);
      st.timer = setTimeout(() => { showPreview(el, gid); }, SHOW_DELAY);
    }, (tab) => {
      if (mine(tab)) { scheduleHide(el); }
    });
    hover(".ah-dl-autohide-preview-slot", (slot) => {
      if (mine(slot)) { clearTimeout(st.timer); }
    }, (slot) => {
      if (mine(slot)) { scheduleHide(el); }
    });
    this.delegate("click", ".ah-dl-autohide-tab", (e, tab) => {
      if (!mine(tab)) { return; }
      const gid = tab.getAttribute("data-group-id");
      if (st.preview === gid) { hidePreview(el); } else { showPreview(el, gid); }
    });
    this.delegate("keydown", ".ah-dl-autohide-tab", (e, tab) => {
      if (!mine(tab) || (e.key !== "Enter" && e.key !== " ")) { return; }
      e.preventDefault();
      const gid = tab.getAttribute("data-group-id");
      if (st.preview === gid) { hidePreview(el); return; }
      showPreview(el, gid);
      let g = null;
      slots(el).forEach((s) => { g = g || kids(s, ".ah-dl-autohide-current")[0] || null; });
      if (g) { activeTab(g).focus(); }
    });
    this.delegate("keydown", ".ah-dl-autohide-preview-slot", (e, slot) => {
      if (e.key !== "Escape" || !mine(slot) || !st.preview) { return; }
      e.preventDefault();
      const gid = st.preview;
      hidePreview(el);
      const tab = stripTab(el, gid);
      if (tab) { tab.focus(); }
    });

    refresh(el);
    commit(el, dlSerialize(el), false);
  }

  teardown() {
    const el = this.element, st = states.get(el);
    stopDrag(el);
    if (st) {
      clearTimeout(st.timer);
      closeMenu(el, false);
    }
    states.delete(el);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  activate(id) { activate(this.element, id); }
  float(id) {
    const el = this.element, li = findTab(el, id);
    if (!li || winOf(li)) { return; }
    const n = floatWins(el).length;
    floatTab(el, li, 40 + 24 * n, 40 + 24 * n);
    done(el, true);
  }
  dock(id) {
    const el = this.element, li = findTab(el, id);
    if (!li) { return; }
    const g = groupOf(li), win = winOf(g);
    if (win) { dockFloat(el, win); }
    else if (inSlot(g)) { hidePreview(el); repin(el, g); }
    else { return; }
    done(el, true);
  }
  close(id) {
    const el = this.element, li = findTab(el, id);
    if (!li) { return; }
    closeTab(el, li, false);
    done(el, true);
  }
  openPanel(html, where) {
    const el = this.element;
    where = where || {};
    const g = parseHTML(html).find((n) => n.matches(".ah-dl-tabbed"));
    let li = g && tabsOf(g)[0];
    if (!li) { return; }
    const id = pid(li);
    if (findTab(el, id)) { activate(el, id); return; }
    const byId = (n) => n.getAttribute("data-group-id") === String(where["in"]);
    let target = where["in"] ? (layoutGroups(el).find(byId) || slotGroups(el).find(byId)) : null;
    if (where.float) {
      const n = floatWins(el).length;
      floatItems(el, [extract(li)], where.x !== undefined ? where.x : 40 + 24 * n,
                 where.y !== undefined ? where.y : 40 + 24 * n, sourceFlags(g));
    } else if (where.edge) {
      insertEdge(el, g, "edge-" + where.edge);
    } else if (target || (target = defaultTarget(el))) {
      insertTab(target, extract(li));
    } else {
      insertEdge(el, g, "edge-right");
    }
    li = findTab(el, id);
    AH.mount(panelFor(groupOf(li), li));
    done(el, true);
  }
  getValue() { return this.element.getAttribute("data-ah-value"); }
});
