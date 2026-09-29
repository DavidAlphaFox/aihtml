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
 * "change" on the root.
 */
import $ from "jquery";
import AH from "../core.js";
import "virtual:ah-tpl/tile_layout_tab";

var NS = AH.NS;
var seq = 0;
var MIN = 20;

function state(el) {
  var s = $.data(el, "ah-tl");
  if (!s) {
    var closed = [];
    try { closed = JSON.parse(el.getAttribute("data-ah-value") || "{}").closed || []; }
    catch (e) { closed = []; }
    s = { ns: ".ahtl" + (++seq), closed: closed, drag: null, resize: null };
    $.data(el, "ah-tl", s);
  }
  return s;
}

function own(el, $set) {
  return $set.filter(function () { return $(this).closest(".ah-tl")[0] === el; });
}

function disabled(el) {
  return el.classList.contains("ah-tl-disabled");
}

function newId() {
  return "tl-" + Date.now().toString(36) + "-" + (++seq);
}

function barSize(el) {
  var n = parseInt(el.getAttribute("data-splitbar-size"), 10);
  return isNaN(n) ? 4 : n;
}

// ------------------------------------------------------------------
// Groups: children, grid tracks, sizes
// ------------------------------------------------------------------

function isGroup(n) { return n && n.getAttribute && n.getAttribute("data-type") === "layout-group"; }
function isTabGroup(n) { return n && n.getAttribute && n.getAttribute("data-type") === "tab-group"; }

function kids(group) {
  return $(group).children().not(".ah-tl-splitbar");
}

function vert(group) {
  return group.getAttribute("data-orientation") === "vertical";
}

function applyTemplate(el, group) {
  var sizes = kids(group).map(function () { return this.getAttribute("data-size") || "1fr"; }).get();
  var tpl = sizes.join(" " + barSize(el) + "px ");
  group.style.gridTemplateColumns = vert(group) ? tpl : "";
  group.style.gridTemplateRows = vert(group) ? "" : tpl;
}

function measure(node, v) {
  var r = node.getBoundingClientRect();
  return v ? r.width : r.height;
}

function fr(px, total) {
  return total > 0 ? (Math.round(px / total * 10000) / 100) + "fr" : "1fr";
}

// Write sizes (px, one per child) as proportional fr tracks.
function setSizes(el, group, px) {
  var total = px.reduce(function (a, b) { return a + b; }, 0);
  kids(group).each(function (i) { this.setAttribute("data-size", fr(px[i], total)); });
  applyTemplate(el, group);
}

function sizesOf(group) {
  var v = vert(group);
  return kids(group).map(function () { return measure(this, v); }).get();
}

function splitbar(v) {
  return $("<div>").addClass("ah-tl-splitbar " + (v ? "ah-tl-splitbar-v" : "ah-tl-splitbar-h"))
    .attr({ role: "separator", "aria-orientation": v ? "vertical" : "horizontal",
            "aria-label": "Resize", tabindex: "0" })[0];
}

function newGroup(v) {
  return $("<div>").addClass("ah-tl-group " + (v ? "ah-tl-vertical" : "ah-tl-horizontal"))
    .attr({ "data-id": newId(), "data-type": "layout-group",
            "data-orientation": v ? "vertical" : "horizontal" })[0];
}

function newTabGroup() {
  var tg = $("<div>").addClass("ah-tl-tab-group")
    .attr({ "data-id": newId(), "data-type": "tab-group" })[0];
  $("<div>").addClass("ah-tl-tab-strip")
    .attr({ role: "tablist", "aria-orientation": "horizontal" }).appendTo(tg);
  return tg;
}

// ------------------------------------------------------------------
// Tabs
// ------------------------------------------------------------------

function strip(tg) { return $(tg).children(".ah-tl-tab-strip")[0]; }
function tabsOf(tg) { return $(strip(tg)).children(".ah-tl-tab"); }
function tabId(tab) { return tab.getAttribute("data-tab-id"); }

function panelOf(tg, id) {
  return $(tg).children(".ah-tl-tab-content").filter(function () {
    return this.getAttribute("data-id") === id;
  })[0];
}

function allows(node, what) {
  var m = node.getAttribute("data-modifiers");
  return m === null || m.split(",").indexOf(what) >= 0;
}

function selectTab(tg, tab) {
  tabsOf(tg).each(function () {
    var on = this === tab;
    this.classList.toggle("ah-tl-tab-selected", on);
    this.setAttribute("aria-selected", String(on));
    this.setAttribute("tabindex", on ? "0" : "-1");
  });
  var id = tab && tabId(tab);
  $(tg).children(".ah-tl-tab-content").each(function () {
    this.classList.toggle("ah-tl-tab-content-active", this.getAttribute("data-id") === id);
  });
}

function findTab(el, id) {
  return own(el, $(el).find(".ah-tl-tab")).filter(function () {
    return tabId(this) === String(id);
  })[0];
}

// ------------------------------------------------------------------
// The arrangement (keys in the order the server's JSON encoder uses)
// ------------------------------------------------------------------

function nodeValue(n) {
  var t = n.getAttribute("data-type");
  var size = n.getAttribute("data-size");
  var o = {};
  if (t === "layout-group") {
    o.id = n.getAttribute("data-id");
    o.items = kids(n).map(function () { return nodeValue(this); }).get();
    if (size) { o.size = size; }
    o.type = vert(n) ? "columns" : "rows";
  } else if (t === "tab-group") {
    var sel = tabsOf(n).filter(".ah-tl-tab-selected")[0];
    o.active = sel ? tabId(sel) : "";
    o.id = n.getAttribute("data-id");
    if (size) { o.size = size; }
    o.tabs = tabsOf(n).map(function () { return tabId(this); }).get();
    o.type = "tabs";
  } else {
    o.id = n.getAttribute("data-id");
    if (size) { o.size = size; }
    o.type = "item";
  }
  return o;
}

function value(el) {
  var root = $(el).children("[data-type]")[0];
  var closed = state(el).closed.slice().sort();
  return { closed: closed, root: root ? nodeValue(root) : null };
}

function sync(el) {
  var v = JSON.stringify(value(el));
  el.setAttribute("data-ah-value", v);
  $(el).children("input[type=hidden]").val(v);
}

function commit(el) {
  sync(el);
  $(el).trigger("change");
}

// ------------------------------------------------------------------
// Removing nodes and tidying up
// ------------------------------------------------------------------

function removeNode(el, n) {
  var parent = n.parentNode;
  if (isGroup(parent)) {
    setSizes(el, parent, sizesOf(parent));
    var prev = n.previousElementSibling, next = n.nextElementSibling;
    if (prev && prev.classList.contains("ah-tl-splitbar")) {
      $(prev).remove();
    } else if (next && next.classList.contains("ah-tl-splitbar")) {
      $(next).remove();
    }
    $(n).remove();
    applyTemplate(el, parent);
  } else {
    $(n).remove();
  }
}

// Empty tab groups and groups go; a group with one child gives way to it.
function cleanup(el) {
  var again = true;
  while (again) {
    again = false;
    own(el, $(el).find(".ah-tl-tab-group")).each(function () {
      if (!tabsOf(this).length) { removeNode(el, this); again = true; }
    });
    own(el, $(el).find(".ah-tl-group")).each(function () {
      var $k = kids(this);
      if ($k.length === 0) {
        removeNode(el, this);
        again = true;
      } else if ($k.length === 1) {
        var child = $k[0];
        var size = this.getAttribute("data-size");
        if (size) { child.setAttribute("data-size", size); } else { child.removeAttribute("data-size"); }
        var parent = this.parentNode;
        $(this).replaceWith(child);
        if (isGroup(parent)) { applyTemplate(el, parent); }
        again = true;
      }
    });
  }
}

function close(el, tab) {
  var tg = $(tab).closest(".ah-tl-tab-group")[0];
  var id = tabId(tab);
  var wasSel = tab.classList.contains("ah-tl-tab-selected");
  var i = tabsOf(tg).index(tab);
  $(panelOf(tg, id)).remove();
  $(tab).remove();
  var s = state(el);
  if (s.closed.indexOf(id) < 0) { s.closed.push(id); }
  var $rest = tabsOf(tg);
  if (wasSel && $rest.length) {
    var next = $rest[Math.min(i, $rest.length - 1)];
    selectTab(tg, next);
    if ($.contains(tg, document.activeElement) || document.activeElement === document.body) {
      next.focus();
    }
  }
  cleanup(el);
  commit(el);
  $(el).trigger("ah:tab-close", [id]);
}

// ------------------------------------------------------------------
// Dropping a tab
// ------------------------------------------------------------------

// sigil's five zones: the outer quarter of each side, else the centre.
function zoneAt(x, y, r) {
  var rx = x - r.left, ry = y - r.top;
  if (ry < r.height * 0.25) { return "top"; }
  if (ry > r.height * 0.75) { return "bottom"; }
  if (rx < r.width * 0.25) { return "left"; }
  if (rx > r.width * 0.75) { return "right"; }
  return "center";
}

function targetAt(el, x, y) {
  var found = null;
  own(el, $(el).find(".ah-tl-item, .ah-tl-tab-group")).each(function () {
    var r = this.getBoundingClientRect();
    if (x >= r.left && x <= r.right && y >= r.top && y <= r.bottom) { found = this; }
  });
  return found;
}

function showIndicator(el, zone, target) {
  hideIndicator(el);
  var r = target.getBoundingClientRect(), o = el.getBoundingClientRect();
  var left = r.left - o.left, top = r.top - o.top, w = r.width, h = r.height;
  if (zone === "top") { h = h / 2; }
  if (zone === "bottom") { top += h / 2; h = h / 2; }
  if (zone === "left") { w = w / 2; }
  if (zone === "right") { left += w / 2; w = w / 2; }
  $("<div>").addClass("ah-tl-drop-area")
    .css({ left: left + "px", top: top + "px", width: w + "px", height: h + "px" })
    .appendTo(el);
}

function hideIndicator(el) {
  $(el).children(".ah-tl-drop-area").remove();
}

// A plain tile becomes a tab group holding it as its first tab.
function wrapItem(el, item) {
  var tg = newTabGroup();
  var id = item.getAttribute("data-id");
  ["data-size", "data-min", "data-resize"].forEach(function (a) {
    if (item.hasAttribute(a)) { tg.setAttribute(a, item.getAttribute(a)); }
  });
  $(strip(tg)).append(AH.tpl.tile_layout_tab({
    selected: false, dom_id: el.id + "-tab-" + id, id: id, modifiers: "drag,close",
    panel_id: el.id + "-panel-" + id, aria_selected: "false", tabindex: "-1",
    label: item.getAttribute("data-label") || id, close: true
  }));
  var panel = $("<div>").addClass("ah-tl-tab-content")
    .attr({ id: el.id + "-panel-" + id, "data-id": id, role: "tabpanel",
            "aria-labelledby": el.id + "-tab-" + id })[0];
  while (item.firstChild) { panel.appendChild(item.firstChild); }
  tg.appendChild(panel);
  $(item).replaceWith(tg);
  return tg;
}

function drop(el, tab, zone, target) {
  var src = $(tab).closest(".ah-tl-tab-group")[0];
  if (target === src && (zone === "center" || tabsOf(src).length === 1)) { return false; }
  var id = tabId(tab);
  var panel = panelOf(src, id);
  var wasSel = tab.classList.contains("ah-tl-tab-selected");
  $(tab).detach();
  $(panel).detach();
  if (wasSel && tabsOf(src).length) { selectTab(src, tabsOf(src)[0]); }
  var into;
  if (zone === "center") {
    into = isTabGroup(target) ? target : wrapItem(el, target);
  } else {
    into = newTabGroup();
    var needVert = zone === "left" || zone === "right";
    var before = zone === "left" || zone === "top";
    var parent = target.parentNode;
    if (isGroup(parent) && vert(parent) === needVert) {
      // same axis: the new group takes half of the target's room
      var px = sizesOf(parent);
      var at = kids(parent).index(target);
      var half = px[at] / 2;
      px.splice(at, 1, half, half);
      if (before) {
        $(target).before(into, splitbar(needVert));
      } else {
        $(target).after(splitbar(needVert), into);
        px.splice(at, 2, half, half);
      }
      setSizes(el, parent, px);
    } else {
      // across: target and new group share a new group in its place
      var g = newGroup(needVert);
      if (target.hasAttribute("data-size")) {
        g.setAttribute("data-size", target.getAttribute("data-size"));
        target.removeAttribute("data-size");
      }
      $(target).replaceWith(g);
      if (before) { $(g).append(into, splitbar(needVert), target); }
      else { $(g).append(target, splitbar(needVert), into); }
      applyTemplate(el, g);
      if (isGroup(parent)) { applyTemplate(el, parent); }
    }
  }
  $(strip(into)).append(tab);
  $(into).append(panel);
  selectTab(into, tab);
  cleanup(el);
  commit(el);
  return true;
}

// ------------------------------------------------------------------
// Pointer interactions
// ------------------------------------------------------------------

function tabDown(el, tab, e) {
  if (disabled(el) || e.button !== 0 || $(e.target).closest(".ah-tl-tab-close").length) { return; }
  var tg = $(tab).closest(".ah-tl-tab-group")[0];
  if (!allows(tab, "drag") || !allows(tg, "drag")) { return; }
  state(el).drag = { tab: tab, x: e.clientX, y: e.clientY, started: false };
}

function dragMove(el, e) {
  var d = state(el).drag;
  if (!d) { return; }
  if (!d.started) {
    if (Math.sqrt(Math.pow(e.clientX - d.x, 2) + Math.pow(e.clientY - d.y, 2)) <= 5) { return; }
    d.started = true;
    d.feedback = $("<div>").addClass("ah-tl-feedback")
      .text($(d.tab).find(".ah-tl-tab-label").text()).appendTo(document.body)[0];
    d.overlay = $("<div>").addClass("ah-tl-overlay").appendTo(document.body)[0];
  }
  $(d.feedback).css({ left: (e.clientX + 10) + "px", top: (e.clientY + 10) + "px" });
  var t = targetAt(el, e.clientX, e.clientY);
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
  var s = state(el), d = s.drag;
  s.drag = null;
  if (!d) { return; }
  $(d.feedback).remove();
  $(d.overlay).remove();
  hideIndicator(el);
  if (!cancel && d.started && d.target) { drop(el, d.tab, d.zone, d.target); }
}

// Resizing: the two panes around a splitbar trade room, within their
// minimum sizes; the group's tracks become proportional fr values.
function resizeStart(el, bar, coord) {
  var group = bar.parentNode;
  var prev = bar.previousElementSibling, next = bar.nextElementSibling;
  if (disabled(el) || !isGroup(group) || !prev || !next ||
      prev.getAttribute("data-resize") === "false" || next.getAttribute("data-resize") === "false") {
    return null;
  }
  var $k = kids(group);
  return {
    bar: bar, group: group, start: coord, px: sizesOf(group),
    i: $k.index(prev), moved: false,
    saved: $k.map(function () { return this.getAttribute("data-size"); }).get(),
    minPrev: parseInt(prev.getAttribute("data-min"), 10) || MIN,
    minNext: parseInt(next.getAttribute("data-min"), 10) || MIN
  };
}

function resizeTo(el, r, delta) {
  var a = r.px[r.i], b = r.px[r.i + 1];
  delta = Math.max(-(a - r.minPrev), Math.min(delta, b - r.minNext));
  var px = r.px.slice();
  px[r.i] = a + delta;
  px[r.i + 1] = b - delta;
  setSizes(el, r.group, px);
  r.moved = r.moved || delta !== 0;
}

function resizeCancel(el, r) {
  kids(r.group).each(function (i) {
    if (r.saved[i] === null) { this.removeAttribute("data-size"); }
    else { this.setAttribute("data-size", r.saved[i]); }
  });
  applyTemplate(el, r.group);
  r.bar.classList.remove("ah-tl-splitbar-active");
}

// ------------------------------------------------------------------
// Behaviour
// ------------------------------------------------------------------

AH.define("tile-layout", {
  init: function (el, $el) {
    var s = state(el);
    $el.on("click" + NS, ".ah-tl-tab-close", function (e) {
      e.stopPropagation();
      if (disabled(el)) { return; }
      var tab = $(this).closest(".ah-tl-tab")[0];
      if ($(tab).closest(".ah-tl")[0] === el && allows(tab, "close")) { close(el, tab); }
    });
    $el.on("click" + NS, ".ah-tl-tab", function () {
      if ($(this).closest(".ah-tl")[0] !== el || disabled(el)) { return; }
      var tg = $(this).closest(".ah-tl-tab-group")[0];
      var changed = !this.classList.contains("ah-tl-tab-selected");
      selectTab(tg, this);
      if (changed) {
        commit(el);
        $el.trigger("ah:tab-select", [tabId(this)]);
      }
    });
    $el.on("keydown" + NS, ".ah-tl-tab", function (e) {
      if ($(this).closest(".ah-tl")[0] !== el || disabled(el)) { return; }
      var tg = $(this).closest(".ah-tl-tab-group")[0];
      var side = /\bah-tl-tab-group-(left|right)\b/.test(tg.className);
      var $t = tabsOf(tg), i = $t.index(this), next = null;
      switch (e.key) {
        case side ? "ArrowUp" : "ArrowLeft": next = (i - 1 + $t.length) % $t.length; break;
        case side ? "ArrowDown" : "ArrowRight": next = (i + 1) % $t.length; break;
        case "Home": next = 0; break;
        case "End": next = $t.length - 1; break;
        case "Delete":
          if (allows(this, "close")) { e.preventDefault(); close(el, this); }
          return;
        case "Enter": case " ": next = i; break;
        default: return;
      }
      e.preventDefault();
      var t = $t[next];
      t.focus();
      if (!t.classList.contains("ah-tl-tab-selected")) {
        selectTab(tg, t);
        commit(el);
        $el.trigger("ah:tab-select", [tabId(t)]);
      }
    });
    // the pane last pressed is outlined (sigil's selection)
    $el.on("pointerdown" + NS, function (e) {
      var pane = $(e.target).closest(".ah-tl-item, .ah-tl-tab-group")[0];
      if (!pane || $(pane).closest(".ah-tl")[0] !== el) { return; }
      own(el, $(el).find("[data-tl-selected]")).removeAttr("data-tl-selected");
      pane.setAttribute("data-tl-selected", "true");
    });
    $el.on("pointerdown" + NS, ".ah-tl-tab", function (e) {
      if ($(this).closest(".ah-tl")[0] === el) { tabDown(el, this, e); }
    });
    $el.on("pointerdown" + NS, ".ah-tl-splitbar", function (e) {
      if ($(this).closest(".ah-tl")[0] !== el || e.button !== 0) { return; }
      var group = this.parentNode;
      var r = resizeStart(el, this, vert(group) ? e.clientX : e.clientY);
      if (!r) { return; }
      e.preventDefault();
      this.classList.add("ah-tl-splitbar-active");
      state(el).resize = r;
    });
    $el.on("keydown" + NS, ".ah-tl-splitbar", function (e) {
      if ($(this).closest(".ah-tl")[0] !== el) { return; }
      var v = vert(this.parentNode);
      var step = e.shiftKey ? 50 : 10;
      var d = { ArrowLeft: v ? -step : 0, ArrowRight: v ? step : 0,
                ArrowUp: v ? 0 : -step, ArrowDown: v ? 0 : step }[e.key];
      if (!d) { return; }
      e.preventDefault();
      var r = resizeStart(el, this, 0);
      if (!r) { return; }
      resizeTo(el, r, d);
      if (r.moved) { commit(el); }
    });
    $(document).on("pointermove" + s.ns, function (e) {
      var r = state(el).resize;
      if (r) {
        resizeTo(el, r, (vert(r.group) ? e.clientX : e.clientY) - r.start);
      } else {
        dragMove(el, e);
      }
    });
    $(document).on("pointerup" + s.ns + " pointercancel" + s.ns, function (e) {
      var st = state(el), r = st.resize;
      if (r) {
        st.resize = null;
        r.bar.classList.remove("ah-tl-splitbar-active");
        if (r.moved) { commit(el); }
      } else {
        dragEnd(el, e.type === "pointercancel");
      }
    });
    $(document).on("keydown" + s.ns, function (e) {
      if (e.key !== "Escape") { return; }
      var st = state(el);
      if (st.resize) { resizeCancel(el, st.resize); st.resize = null; }
      if (st.drag) { dragEnd(el, true); }
    });
  },
  destroy: function (el) {
    var s = state(el);
    if (s.drag) { dragEnd(el, true); }
    $(document).off(s.ns);
    $.removeData(el, "ah-tl");
  },
  methods: {
    getValue: function (el) { return value(el); },
    select: function (el, $el, id) {
      var tab = findTab(el, id);
      if (tab) {
        selectTab($(tab).closest(".ah-tl-tab-group")[0], tab);
        sync(el);
      }
    },
    close: function (el, $el, id) {
      var tab = findTab(el, id);
      if (tab) { close(el, tab); }
    }
  }
});
