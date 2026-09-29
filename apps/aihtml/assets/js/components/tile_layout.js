/* The tile layout behaviour (designs/04-components.md), ported from sigil's
 * layout/tile_layout: splitbars resize panes, tabs are dragged between
 * tab groups or to an edge of a pane (which splits it), tabs close.
 *
 * The browser moves the live nodes the server rendered, so tile contents
 * keep their state and behaviours. New shells (a tab group, a group, a
 * splitbar) are single elements; a tab for a tile that becomes a tab
 * group comes from the shared template AH.tpl.tile_layout_tab.
 *
 * The arrangement is kept in data-ah-value as JSON (the same shape and
 * key order the server writes, see aihtml_tile_layout) and mirrored
 * into the hidden input; every resize, move, close and tab switch fires
 * "change" on the root. "ah:tab-select" and "ah:tab-close" carry the
 * tab id as their detail.
 */
import AH from "../core.js";
import "virtual:ah-tpl/tile_layout_tab";

let seq = 0;
const MIN = 20;
// per layout root: {closed, drag, resize}
const states = new WeakMap();

function state(el) {
  let s = states.get(el);
  if (!s) {
    let closed = [];
    try { closed = JSON.parse(el.getAttribute("data-ah-value") || "{}").closed || []; }
    catch (e) { closed = []; }
    s = { closed: closed, drag: null, resize: null };
    states.set(el, s);
  }
  return s;
}

function mine(el, node) { return node.closest(".ah-tl") === el; }
function own(el, selector) {
  return Array.from(el.querySelectorAll(selector)).filter((n) => mine(el, n));
}

function disabled(el) {
  return el.classList.contains("ah-tl-disabled");
}

function newId() {
  return "tl-" + Date.now().toString(36) + "-" + (++seq);
}

function barSize(el) {
  const n = parseInt(el.getAttribute("data-splitbar-size"), 10);
  return isNaN(n) ? 4 : n;
}

function div(cls, attrs) {
  const d = document.createElement("div");
  d.className = cls;
  Object.keys(attrs || {}).forEach((k) => { d.setAttribute(k, attrs[k]); });
  return d;
}

function emit(el, type, detail) {
  el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
}

// ------------------------------------------------------------------
// Groups: children, grid tracks, sizes
// ------------------------------------------------------------------

function isGroup(n) { return n && n.getAttribute && n.getAttribute("data-type") === "layout-group"; }
function isTabGroup(n) { return n && n.getAttribute && n.getAttribute("data-type") === "tab-group"; }

function kids(group) {
  return Array.from(group.children).filter((n) => !n.classList.contains("ah-tl-splitbar"));
}

function vert(group) {
  return group.getAttribute("data-orientation") === "vertical";
}

function applyTemplate(el, group) {
  const sizes = kids(group).map((n) => n.getAttribute("data-size") || "1fr");
  const tpl = sizes.join(" " + barSize(el) + "px ");
  group.style.gridTemplateColumns = vert(group) ? tpl : "";
  group.style.gridTemplateRows = vert(group) ? "" : tpl;
}

function measure(node, v) {
  const r = node.getBoundingClientRect();
  return v ? r.width : r.height;
}

function fr(px, total) {
  return total > 0 ? (Math.round(px / total * 10000) / 100) + "fr" : "1fr";
}

// Write sizes (px, one per child) as proportional fr tracks.
function setSizes(el, group, px) {
  const total = px.reduce((a, b) => a + b, 0);
  kids(group).forEach((n, i) => { n.setAttribute("data-size", fr(px[i], total)); });
  applyTemplate(el, group);
}

function sizesOf(group) {
  const v = vert(group);
  return kids(group).map((n) => measure(n, v));
}

function splitbar(v) {
  return div("ah-tl-splitbar " + (v ? "ah-tl-splitbar-v" : "ah-tl-splitbar-h"),
             { role: "separator", "aria-orientation": v ? "vertical" : "horizontal",
               "aria-label": "Resize", tabindex: "0" });
}

function newGroup(v) {
  return div("ah-tl-group " + (v ? "ah-tl-vertical" : "ah-tl-horizontal"),
             { "data-id": newId(), "data-type": "layout-group",
               "data-orientation": v ? "vertical" : "horizontal" });
}

function newTabGroup() {
  const tg = div("ah-tl-tab-group", { "data-id": newId(), "data-type": "tab-group" });
  tg.appendChild(div("ah-tl-tab-strip", { role: "tablist", "aria-orientation": "horizontal" }));
  return tg;
}

// ------------------------------------------------------------------
// Tabs
// ------------------------------------------------------------------

function strip(tg) { return tg.querySelector(":scope > .ah-tl-tab-strip"); }
function tabsOf(tg) {
  const s = strip(tg);
  return s ? Array.from(s.children).filter((n) => n.classList.contains("ah-tl-tab")) : [];
}
function tabId(tab) { return tab.getAttribute("data-tab-id"); }
function tabGroupOf(tab) { return tab.closest(".ah-tl-tab-group"); }
function contents(tg) {
  return Array.from(tg.children).filter((n) => n.classList.contains("ah-tl-tab-content"));
}

function panelOf(tg, id) {
  return contents(tg).find((n) => n.getAttribute("data-id") === id);
}

function allows(node, what) {
  const m = node.getAttribute("data-modifiers");
  return m === null || m.split(",").indexOf(what) >= 0;
}

function selectTab(tg, tab) {
  tabsOf(tg).forEach((t) => {
    const on = t === tab;
    t.classList.toggle("ah-tl-tab-selected", on);
    t.setAttribute("aria-selected", String(on));
    t.setAttribute("tabindex", on ? "0" : "-1");
  });
  const id = tab && tabId(tab);
  contents(tg).forEach((c) => {
    c.classList.toggle("ah-tl-tab-content-active", c.getAttribute("data-id") === id);
  });
}

function findTab(el, id) {
  return own(el, ".ah-tl-tab").find((t) => tabId(t) === String(id));
}

// ------------------------------------------------------------------
// The arrangement (keys in the order the server's JSON encoder uses)
// ------------------------------------------------------------------

function nodeValue(n) {
  const t = n.getAttribute("data-type");
  const size = n.getAttribute("data-size");
  const o = {};
  if (t === "layout-group") {
    o.id = n.getAttribute("data-id");
    o.items = kids(n).map(nodeValue);
    if (size) { o.size = size; }
    o.type = vert(n) ? "columns" : "rows";
  } else if (t === "tab-group") {
    const sel = tabsOf(n).find((x) => x.classList.contains("ah-tl-tab-selected"));
    o.active = sel ? tabId(sel) : "";
    o.id = n.getAttribute("data-id");
    if (size) { o.size = size; }
    o.tabs = tabsOf(n).map(tabId);
    o.type = "tabs";
  } else {
    o.id = n.getAttribute("data-id");
    if (size) { o.size = size; }
    o.type = "item";
  }
  return o;
}

function value(el) {
  const root = el.querySelector(":scope > [data-type]");
  const closed = state(el).closed.slice().sort();
  return { closed: closed, root: root ? nodeValue(root) : null };
}

function sync(el) {
  const v = JSON.stringify(value(el));
  el.setAttribute("data-ah-value", v);
  el.querySelectorAll(":scope > input[type=hidden]").forEach((i) => { i.value = v; });
}

function commit(el) {
  sync(el);
  emit(el, "change");
}

// ------------------------------------------------------------------
// Removing nodes and tidying up
// ------------------------------------------------------------------

function removeNode(el, n) {
  const parent = n.parentNode;
  if (isGroup(parent)) {
    setSizes(el, parent, sizesOf(parent));
    const prev = n.previousElementSibling, next = n.nextElementSibling;
    if (prev && prev.classList.contains("ah-tl-splitbar")) {
      prev.remove();
    } else if (next && next.classList.contains("ah-tl-splitbar")) {
      next.remove();
    }
    n.remove();
    applyTemplate(el, parent);
  } else {
    n.remove();
  }
}

// Empty tab groups and groups go; a group with one child gives way to it.
function cleanup(el) {
  let again = true;
  while (again) {
    again = false;
    own(el, ".ah-tl-tab-group").forEach((tg) => {
      if (!tabsOf(tg).length) { removeNode(el, tg); again = true; }
    });
    own(el, ".ah-tl-group").forEach((g) => {
      if (!g.isConnected) { return; }
      const k = kids(g);
      if (k.length === 0) {
        removeNode(el, g);
        again = true;
      } else if (k.length === 1) {
        const child = k[0];
        const size = g.getAttribute("data-size");
        if (size) { child.setAttribute("data-size", size); } else { child.removeAttribute("data-size"); }
        const parent = g.parentNode;
        g.replaceWith(child);
        if (isGroup(parent)) { applyTemplate(el, parent); }
        again = true;
      }
    });
  }
}

function close(el, tab) {
  const tg = tabGroupOf(tab);
  const id = tabId(tab);
  const wasSel = tab.classList.contains("ah-tl-tab-selected");
  const i = tabsOf(tg).indexOf(tab);
  const panel = panelOf(tg, id);
  if (panel) { panel.remove(); }
  tab.remove();
  const s = state(el);
  if (s.closed.indexOf(id) < 0) { s.closed.push(id); }
  const rest = tabsOf(tg);
  if (wasSel && rest.length) {
    const next = rest[Math.min(i, rest.length - 1)];
    selectTab(tg, next);
    const a = document.activeElement;
    if ((a !== tg && tg.contains(a)) || a === document.body) {
      next.focus();
    }
  }
  cleanup(el);
  commit(el);
  emit(el, "ah:tab-close", id);
}

// ------------------------------------------------------------------
// Dropping a tab
// ------------------------------------------------------------------

// sigil's five zones: the outer quarter of each side, else the centre.
function zoneAt(x, y, r) {
  const rx = x - r.left, ry = y - r.top;
  if (ry < r.height * 0.25) { return "top"; }
  if (ry > r.height * 0.75) { return "bottom"; }
  if (rx < r.width * 0.25) { return "left"; }
  if (rx > r.width * 0.75) { return "right"; }
  return "center";
}

function targetAt(el, x, y) {
  let found = null;
  own(el, ".ah-tl-item, .ah-tl-tab-group").forEach((n) => {
    const r = n.getBoundingClientRect();
    if (x >= r.left && x <= r.right && y >= r.top && y <= r.bottom) { found = n; }
  });
  return found;
}

function showIndicator(el, zone, target) {
  hideIndicator(el);
  const r = target.getBoundingClientRect(), o = el.getBoundingClientRect();
  let left = r.left - o.left, top = r.top - o.top, w = r.width, h = r.height;
  if (zone === "top") { h = h / 2; }
  if (zone === "bottom") { top += h / 2; h = h / 2; }
  if (zone === "left") { w = w / 2; }
  if (zone === "right") { left += w / 2; w = w / 2; }
  const area = div("ah-tl-drop-area");
  Object.assign(area.style, { left: left + "px", top: top + "px", width: w + "px", height: h + "px" });
  el.appendChild(area);
}

function hideIndicator(el) {
  el.querySelectorAll(":scope > .ah-tl-drop-area").forEach((n) => { n.remove(); });
}

// A plain tile becomes a tab group holding it as its first tab.
function wrapItem(el, item) {
  const tg = newTabGroup();
  const id = item.getAttribute("data-id");
  ["data-size", "data-min", "data-resize"].forEach((a) => {
    if (item.hasAttribute(a)) { tg.setAttribute(a, item.getAttribute(a)); }
  });
  strip(tg).insertAdjacentHTML("beforeend", AH.tpl.tile_layout_tab({
    selected: false, dom_id: el.id + "-tab-" + id, id: id, modifiers: "drag,close",
    panel_id: el.id + "-panel-" + id, aria_selected: "false", tabindex: "-1",
    label: item.getAttribute("data-label") || id, close: true
  }));
  const panel = div("ah-tl-tab-content",
                    { id: el.id + "-panel-" + id, "data-id": id, role: "tabpanel",
                      "aria-labelledby": el.id + "-tab-" + id });
  while (item.firstChild) { panel.appendChild(item.firstChild); }
  tg.appendChild(panel);
  item.replaceWith(tg);
  return tg;
}

function drop(el, tab, zone, target) {
  const src = tabGroupOf(tab);
  if (target === src && (zone === "center" || tabsOf(src).length === 1)) { return false; }
  const id = tabId(tab);
  const panel = panelOf(src, id);
  const wasSel = tab.classList.contains("ah-tl-tab-selected");
  tab.remove();
  if (panel) { panel.remove(); }
  if (wasSel && tabsOf(src).length) { selectTab(src, tabsOf(src)[0]); }
  let into;
  if (zone === "center") {
    into = isTabGroup(target) ? target : wrapItem(el, target);
  } else {
    into = newTabGroup();
    const needVert = zone === "left" || zone === "right";
    const before = zone === "left" || zone === "top";
    const parent = target.parentNode;
    if (isGroup(parent) && vert(parent) === needVert) {
      // same axis: the new group takes half of the target's room
      const px = sizesOf(parent);
      const at = kids(parent).indexOf(target);
      const half = px[at] / 2;
      px.splice(at, 1, half, half);
      if (before) {
        target.before(into, splitbar(needVert));
      } else {
        target.after(splitbar(needVert), into);
        px.splice(at, 2, half, half);
      }
      setSizes(el, parent, px);
    } else {
      // across: target and new group share a new group in its place
      const g = newGroup(needVert);
      if (target.hasAttribute("data-size")) {
        g.setAttribute("data-size", target.getAttribute("data-size"));
        target.removeAttribute("data-size");
      }
      target.replaceWith(g);
      if (before) { g.append(into, splitbar(needVert), target); }
      else { g.append(target, splitbar(needVert), into); }
      applyTemplate(el, g);
      if (isGroup(parent)) { applyTemplate(el, parent); }
    }
  }
  strip(into).appendChild(tab);
  if (panel) { into.appendChild(panel); }
  selectTab(into, tab);
  cleanup(el);
  commit(el);
  return true;
}

// ------------------------------------------------------------------
// Pointer interactions
// ------------------------------------------------------------------

function tabDown(el, tab, e) {
  if (disabled(el) || e.button !== 0 || e.target.closest(".ah-tl-tab-close")) { return; }
  const tg = tabGroupOf(tab);
  if (!allows(tab, "drag") || !allows(tg, "drag")) { return; }
  state(el).drag = { tab: tab, x: e.clientX, y: e.clientY, started: false };
}

function dragMove(el, e) {
  const d = state(el).drag;
  if (!d) { return; }
  if (!d.started) {
    if (Math.sqrt(Math.pow(e.clientX - d.x, 2) + Math.pow(e.clientY - d.y, 2)) <= 5) { return; }
    d.started = true;
    d.feedback = div("ah-tl-feedback");
    const lbl = d.tab.querySelector(".ah-tl-tab-label");
    d.feedback.textContent = lbl ? lbl.textContent : "";
    document.body.appendChild(d.feedback);
    d.overlay = div("ah-tl-overlay");
    document.body.appendChild(d.overlay);
  }
  d.feedback.style.left = (e.clientX + 10) + "px";
  d.feedback.style.top = (e.clientY + 10) + "px";
  const t = targetAt(el, e.clientX, e.clientY);
  if (t) {
    d.target = t;
    d.zone = zoneAt(e.clientX, e.clientY, t.getBoundingClientRect());
    showIndicator(el, d.zone, t);
  } else {
    d.target = d.zone = null;
    hideIndicator(el);
  }
}

function dragEnd(el, cancel) {
  const s = state(el), d = s.drag;
  s.drag = null;
  if (!d) { return; }
  if (d.feedback) { d.feedback.remove(); }
  if (d.overlay) { d.overlay.remove(); }
  hideIndicator(el);
  if (!cancel && d.started && d.target) { drop(el, d.tab, d.zone, d.target); }
}

// Resizing: the two panes around a splitbar trade room, within their
// minimum sizes; the group's tracks become proportional fr values.
function resizeStart(el, bar, coord) {
  const group = bar.parentNode;
  const prev = bar.previousElementSibling, next = bar.nextElementSibling;
  if (disabled(el) || !isGroup(group) || !prev || !next ||
      prev.getAttribute("data-resize") === "false" || next.getAttribute("data-resize") === "false") {
    return null;
  }
  const k = kids(group);
  return {
    bar: bar, group: group, start: coord, px: sizesOf(group),
    i: k.indexOf(prev), moved: false,
    saved: k.map((n) => n.getAttribute("data-size")),
    minPrev: parseInt(prev.getAttribute("data-min"), 10) || MIN,
    minNext: parseInt(next.getAttribute("data-min"), 10) || MIN
  };
}

function resizeTo(el, r, delta) {
  const a = r.px[r.i], b = r.px[r.i + 1];
  delta = Math.max(-(a - r.minPrev), Math.min(delta, b - r.minNext));
  const px = r.px.slice();
  px[r.i] = a + delta;
  px[r.i + 1] = b - delta;
  setSizes(el, r.group, px);
  r.moved = r.moved || delta !== 0;
}

function resizeCancel(el, r) {
  kids(r.group).forEach((n, i) => {
    if (r.saved[i] === null) { n.removeAttribute("data-size"); }
    else { n.setAttribute("data-size", r.saved[i]); }
  });
  applyTemplate(el, r.group);
  r.bar.classList.remove("ah-tl-splitbar-active");
}

// ------------------------------------------------------------------
// Behaviour
// ------------------------------------------------------------------

AH.register("tile-layout", class extends AH.Controller {
  setup() {
    const el = this.element;
    state(el);
    this.delegate("click", ".ah-tl-tab-close", (e, btn) => {
      e.stopPropagation();
      if (disabled(el)) { return; }
      const tab = btn.closest(".ah-tl-tab");
      if (tab && mine(el, tab) && allows(tab, "close")) { close(el, tab); }
    });
    this.delegate("click", ".ah-tl-tab", (e, tab) => {
      if (!mine(el, tab) || disabled(el)) { return; }
      const tg = tabGroupOf(tab);
      const changed = !tab.classList.contains("ah-tl-tab-selected");
      selectTab(tg, tab);
      if (changed) {
        commit(el);
        this.fire("ah:tab-select", tabId(tab));
      }
    });
    this.delegate("keydown", ".ah-tl-tab", (e, tab) => {
      if (!mine(el, tab) || disabled(el)) { return; }
      const tg = tabGroupOf(tab);
      const side = /\bah-tl-tab-group-(left|right)\b/.test(tg.className);
      const ts = tabsOf(tg), i = ts.indexOf(tab);
      let next = null;
      switch (e.key) {
        case side ? "ArrowUp" : "ArrowLeft": next = (i - 1 + ts.length) % ts.length; break;
        case side ? "ArrowDown" : "ArrowRight": next = (i + 1) % ts.length; break;
        case "Home": next = 0; break;
        case "End": next = ts.length - 1; break;
        case "Delete":
          if (allows(tab, "close")) { e.preventDefault(); close(el, tab); }
          return;
        case "Enter": case " ": next = i; break;
        default: return;
      }
      e.preventDefault();
      const t = ts[next];
      t.focus();
      if (!t.classList.contains("ah-tl-tab-selected")) {
        selectTab(tg, t);
        commit(el);
        this.fire("ah:tab-select", tabId(t));
      }
    });
    // the pane last pressed is outlined (sigil's selection)
    this.listen(el, "pointerdown", (e) => {
      const pane = e.target.closest(".ah-tl-item, .ah-tl-tab-group");
      if (!pane || !mine(el, pane)) { return; }
      own(el, "[data-tl-selected]").forEach((n) => { n.removeAttribute("data-tl-selected"); });
      pane.setAttribute("data-tl-selected", "true");
    });
    this.delegate("pointerdown", ".ah-tl-tab", (e, tab) => {
      if (mine(el, tab)) { tabDown(el, tab, e); }
    });
    this.delegate("pointerdown", ".ah-tl-splitbar", (e, bar) => {
      if (!mine(el, bar) || e.button !== 0) { return; }
      const group = bar.parentNode;
      const r = resizeStart(el, bar, vert(group) ? e.clientX : e.clientY);
      if (!r) { return; }
      e.preventDefault();
      bar.classList.add("ah-tl-splitbar-active");
      state(el).resize = r;
    });
    this.delegate("keydown", ".ah-tl-splitbar", (e, bar) => {
      if (!mine(el, bar)) { return; }
      const v = vert(bar.parentNode);
      const step = e.shiftKey ? 50 : 10;
      const d = { ArrowLeft: v ? -step : 0, ArrowRight: v ? step : 0,
                  ArrowUp: v ? 0 : -step, ArrowDown: v ? 0 : step }[e.key];
      if (!d) { return; }
      e.preventDefault();
      const r = resizeStart(el, bar, 0);
      if (!r) { return; }
      resizeTo(el, r, d);
      if (r.moved) { commit(el); }
    });
    this.listen(document, "pointermove", (e) => {
      const r = state(el).resize;
      if (r) {
        resizeTo(el, r, (vert(r.group) ? e.clientX : e.clientY) - r.start);
      } else {
        dragMove(el, e);
      }
    });
    const up = (e) => {
      const st = state(el), r = st.resize;
      if (r) {
        st.resize = null;
        r.bar.classList.remove("ah-tl-splitbar-active");
        if (r.moved) { commit(el); }
      } else {
        dragEnd(el, e.type === "pointercancel");
      }
    };
    this.listen(document, "pointerup", up);
    this.listen(document, "pointercancel", up);
    this.listen(document, "keydown", (e) => {
      if (e.key !== "Escape") { return; }
      const st = state(el);
      if (st.resize) { resizeCancel(el, st.resize); st.resize = null; }
      if (st.drag) { dragEnd(el, true); }
    });
  }

  teardown() {
    const el = this.element;
    if (state(el).drag) { dragEnd(el, true); }
    states.delete(el);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue() { return value(this.element); }
  select(id) {
    const tab = findTab(this.element, id);
    if (tab) {
      selectTab(tabGroupOf(tab), tab);
      sync(this.element);
    }
  }
  close(id) {
    const tab = findTab(this.element, id);
    if (tab) { close(this.element, tab); }
  }
});
