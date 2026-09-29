/* Behaviours of the layout_dock components (designs/04-components.md).
 * Ported from sigil: layout/docking (+ docking/drag) and layout/dock_layout
 * (+ common, dock, pin, menu, serialize). Pointer events (mouse, pen,
 * touch) instead of sigil's mouse sequences; one drag at a time, tracked
 * on the document with its own namespace and unbound when it ends or the
 * component is destroyed.
 *
 *   docking      windows dragged by their header between panels, left
 *                floating, collapsed, closed; Alt+arrows move the focused
 *                window from the keyboard
 *   dock_layout  splits, tab groups and documents: tabs dragged onto the
 *                dock targets (a side of a group, its centre, an edge of
 *                the layout) or out to float; auto hide at an edge;
 *                splitbars; a context menu (right click, the context menu
 *                key or Shift+F10 on a tab or a float's title bar)
 *
 * The arrangement is the value: JSON in data-ah-value (and the hidden
 * input), rewritten after every change, and `change' on the root when it
 * changed (user actions and the rearranging methods; setLayout excepted).
 * Sizes are flex weights (style "flex: W 1 0px"), written as percentages
 * of the parent. Elements are only ever moved, never rebuilt, so panel
 * contents keep their state; new tab groups, float windows and the menu
 * come from the shared templates (AH.tpl.dock_layout_*).
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var DISTANCE = 5;          // px of movement before a press becomes a drag
  var CROSS = 128;           // the dock cross: 3 * 40 + 2 * 4
  var SHOW_DELAY = 200, HIDE_DELAY = 400;   // auto hide preview on hover
  var seq = 0;

  // ------------------------------------------------------------------
  // Shared
  // ------------------------------------------------------------------

  function inside(x, y, r) {
    return x >= r.left && x <= r.right && y >= r.top && y <= r.bottom;
  }

  function parse(json) {
    try { return JSON.parse(json || "null"); } catch (e) { return null; }
  }

  function round2(x) { return Math.round(x * 100) / 100; }

  function commit(el, json, fire) {
    if (json === el.getAttribute("data-ah-value")) { return false; }
    el.setAttribute("data-ah-value", json);
    $(el).children("input[type=hidden]").val(json);
    if (fire) { $(el).trigger("change"); }
    return true;
  }

  // A component event with its payload in data-* attributes of the root,
  // which the action receives as Event.data.
  function fire(el, type, key, value) {
    el.setAttribute("data-" + key, value);
    $(el).trigger(type);
  }

  // Follow one pointer press on the document: `start' after DISTANCE px
  // (or at once with threshold 0; returning false gives up), then `move',
  // then `end(ev, cancelled)' on release, pointercancel or Escape.
  function track(owner, e, h, threshold) {
    var ns = ".ahdock" + (++seq);
    var sx = e.clientX, sy = e.clientY, started = false;
    var min = threshold === undefined ? DISTANCE : threshold;
    var $doc = $(document);
    function stop() {
      $doc.off(ns);
      $(document.documentElement).removeClass("ah-dock-dragging");
      $.removeData(owner, "ah-dock-stop");
    }
    function begin(ev) {
      started = true;
      if (h.start(ev) === false) { stop(); return false; }
      $(document.documentElement).addClass("ah-dock-dragging");
      return true;
    }
    var prev = $.data(owner, "ah-dock-stop");
    if (prev) { prev(); }
    $.data(owner, "ah-dock-stop", function () {
      stop();
      if (started) { h.end(null, true); }
    });
    if (min === 0 && !begin(e)) { return; }
    $doc.on("pointermove" + ns, function (ev) {
      if (!started) {
        if (Math.abs(ev.clientX - sx) < min && Math.abs(ev.clientY - sy) < min) { return; }
        if (!begin(ev)) { return; }
      }
      ev.preventDefault();
      h.move(ev);
    });
    $doc.on("pointerup" + ns + " pointercancel" + ns, function (ev) {
      stop();
      if (started) { h.end(ev, ev.type === "pointercancel"); }
    });
    $doc.on("keydown" + ns, function (ev) {
      if (ev.key === "Escape" && started) {
        ev.preventDefault();
        stop();
        h.end(ev, true);
      }
    });
  }

  function stopDrag(el) {
    var stop = $.data(el, "ah-dock-stop");
    if (stop) { stop(); }
  }

  // ------------------------------------------------------------------
  // docking
  // ------------------------------------------------------------------

  function dkState(el) {
    var st = $.data(el, "ah-docking");
    if (!st) {
      var v = parse(el.getAttribute("data-ah-value"));
      st = { closed: (v && Array.isArray(v.closed)) ? v.closed.slice() : [] };
      $.data(el, "ah-docking", st);
    }
    return st;
  }

  function dkPanels(el) { return $(el).children(".ah-docking-panel"); }
  function dkWid(win) { return win.getAttribute("data-window-id"); }

  function dkWindow(el, id) {
    return $(el).find(".ah-docking-window").filter(function () {
      return dkWid(this) === String(id) && $(this).closest(".ah-docking")[0] === el;
    }).first();
  }

  function dkSerialize(el) {
    var panels = [], floating = [], collapsed = [];
    dkPanels(el).each(function () {
      panels.push({ id: this.getAttribute("data-panel-id"),
                    windows: $(this).children(".ah-docking-window").map(function () {
                      return dkWid(this); }).get() });
    });
    $(el).children(".ah-docking-window-floating").each(function () {
      floating.push({ id: dkWid(this), x: Math.round(parseFloat(this.style.left) || 0),
                      y: Math.round(parseFloat(this.style.top) || 0),
                      width: Math.round(parseFloat(this.style.width) || this.offsetWidth) });
    });
    dkPanels(el).children(".ah-docking-window-collapsed").add(
      $(el).children(".ah-docking-window-floating.ah-docking-window-collapsed")).each(function () {
      collapsed.push(dkWid(this));
    });
    return JSON.stringify({ panels: panels, floating: floating, collapsed: collapsed,
                            closed: dkState(el).closed });
  }

  function dkCommit(el, fire_) { return commit(el, dkSerialize(el), fire_); }

  function dkDock(win) {
    $(win).removeClass("ah-docking-window-floating").addClass("ah-docking-window-docked")
      .css({ left: "", top: "", width: "", zIndex: "", opacity: "" });
  }

  function dkFloat(el, win, x, y, w) {
    $(win).removeClass("ah-docking-window-docked").addClass("ah-docking-window-floating")
      .css({ left: Math.round(x) + "px", top: Math.round(y) + "px" });
    if (w) { win.style.width = Math.round(w) + "px"; }
    if (win.parentNode !== el) { el.appendChild(win); }
  }

  // Insert a window into a panel at an index (-1 or past the end: last).
  function dkInsert(panel, win, index) {
    var $ws = $(panel).children(".ah-docking-window").not(win);
    dkDock(win);
    if (index < 0 || index >= $ws.length) { panel.appendChild(win); }
    else { panel.insertBefore(win, $ws[index]); }
  }

  function dkIndexAt(panel, y, skip) {
    var ws = $(panel).children(".ah-docking-window").not(skip).get();
    for (var i = 0; i < ws.length; i++) {
      var r = ws[i].getBoundingClientRect();
      if (y < r.top + r.height / 2) { return i; }
    }
    return ws.length;
  }

  function dkAnnounce(el, text) {
    var $live = $(el).children(".ah-docking-live");
    $live.text("");
    setTimeout(function () { $live.text(text); }, 20);
  }

  function dkCollapse(el, win, on, user) {
    var $w = $(win);
    if ($w.hasClass("ah-docking-window-collapsed") === on) { return; }
    $w.toggleClass("ah-docking-window-collapsed", on);
    $w.find(".ah-docking-window-collapse-btn").first().attr("aria-expanded", on ? "false" : "true");
    if (user) { fire(el, on ? "ah:window-collapse" : "ah:window-expand", "window", dkWid(win)); }
    dkCommit(el, true);
  }

  function dkClose(el, win, user) {
    var id = dkWid(win);
    var focus = $.contains(win, document.activeElement);
    var $next = $(win).nextAll(".ah-docking-window").first();
    if (!$next.length) { $next = $(win).prevAll(".ah-docking-window").first(); }
    if (user) { fire(el, "ah:window-close", "window", id); }
    AH.destroy(win);
    $(win).remove();
    var st = dkState(el);
    if (st.closed.indexOf(id) < 0) { st.closed.push(id); }
    if (focus && $next.length) { $next.children(".ah-docking-window-header").trigger("focus"); }
    dkCommit(el, true);
  }

  function dkDisabled(el) { return $(el).hasClass("ah-docking-disabled"); }

  function dkDrag(el, win, e) {
    var $win = $(win), origin, offX, offY, target = null, index = 0;
    var $ind = $('<div class="ah-docking-drop-indicator" aria-hidden="true"></div>');
    var opacity = parseFloat(el.getAttribute("data-ah-drag-opacity"));
    track(el, e, {
      start: function () {
        var r = win.getBoundingClientRect(), rr = el.getBoundingClientRect();
        origin = { parent: win.parentNode, next: win.nextSibling,
                   floating: $win.hasClass("ah-docking-window-floating"),
                   style: win.getAttribute("style") };
        offX = e.clientX - r.left;
        offY = e.clientY - r.top;
        dkFloat(el, win, r.left - rr.left - el.clientLeft, r.top - rr.top - el.clientTop, r.width);
        $win.addClass("ah-docking-window-dragging")
          .css({ zIndex: 9999, opacity: isNaN(opacity) ? "" : opacity });
      },
      move: function (ev) {
        var rr = el.getBoundingClientRect();
        win.style.left = Math.round(ev.clientX - offX - rr.left - el.clientLeft) + "px";
        win.style.top = Math.round(ev.clientY - offY - rr.top - el.clientTop) + "px";
        target = null;
        dkPanels(el).each(function () {
          if (inside(ev.clientX, ev.clientY, this.getBoundingClientRect())) { target = this; }
        });
        $ind.detach();
        if (target) {
          index = dkIndexAt(target, ev.clientY, win);
          var ws = $(target).children(".ah-docking-window").not(win).get();
          if (index < ws.length) { target.insertBefore($ind[0], ws[index]); }
          else { target.appendChild($ind[0]); }
        }
      },
      end: function (ev, cancelled) {
        $ind.remove();
        $win.removeClass("ah-docking-window-dragging").css({ zIndex: "", opacity: "" });
        if (!cancelled && target) {
          dkInsert(target, win, index);
        } else if (!cancelled && el.getAttribute("data-ah-allow-float") !== "false") {
          // keep the floating window (its header at least) inside the container
          win.style.left = Math.max(0, Math.min(parseFloat(win.style.left) || 0,
                                                el.clientWidth - 60)) + "px";
          win.style.top = Math.max(0, Math.min(parseFloat(win.style.top) || 0,
                                               el.clientHeight - 40)) + "px";
        } else if (cancelled || el.getAttribute("data-ah-allow-float") === "false") {
          if (origin.floating) {
            win.setAttribute("style", origin.style || "");
          } else {
            dkDock(win);
            origin.parent.insertBefore(win, origin.next && origin.next.parentNode === origin.parent
                                       ? origin.next : null);
          }
        }
        dkCommit(el, true);
      }
    });
  }

  // Alt+arrows: move the focused window within its panel or to the next
  // panel; plain arrows move a floating window.
  function dkKey(el, win, e) {
    var $win = $(win), header = $win.children(".ah-docking-window-header")[0];
    var k = e.key, moved = false;
    if (k === "Delete" && $win.find(".ah-docking-window-close-btn").length) {
      e.preventDefault();
      dkClose(el, win, true);
      return;
    }
    if (!/^Arrow/.test(k) || $win.hasClass("ah-docking-window-pinned")) { return; }
    var $panels = dkPanels(el);
    if ($win.hasClass("ah-docking-window-floating")) {
      if (e.altKey) {
        dkInsert($panels[0], win, -1);
      } else {
        var dx = k === "ArrowLeft" ? -10 : k === "ArrowRight" ? 10 : 0;
        var dy = k === "ArrowUp" ? -10 : k === "ArrowDown" ? 10 : 0;
        win.style.left = ((parseFloat(win.style.left) || 0) + dx) + "px";
        win.style.top = ((parseFloat(win.style.top) || 0) + dy) + "px";
      }
      moved = true;
    } else if (e.altKey) {
      var panel = win.parentNode, pi = $panels.index(panel);
      var i = $(panel).children(".ah-docking-window").index(win);
      var vertical = $(el).hasClass("ah-docking-vertical");
      var along = vertical ? { prev: "ArrowLeft", next: "ArrowRight" } : { prev: "ArrowUp", next: "ArrowDown" };
      var across = vertical ? { prev: "ArrowUp", next: "ArrowDown" } : { prev: "ArrowLeft", next: "ArrowRight" };
      if (k === along.prev && i > 0) { dkInsert(panel, win, i - 1); moved = true; }
      else if (k === along.next) { dkInsert(panel, win, i + 1); moved = true; }
      else if (k === across.prev && pi > 0) { dkInsert($panels[pi - 1], win, i); moved = true; }
      else if (k === across.next && pi < $panels.length - 1) { dkInsert($panels[pi + 1], win, i); moved = true; }
    }
    if (!moved) { return; }
    e.preventDefault();
    header.focus();
    if (!$win.hasClass("ah-docking-window-floating")) {
      var p = win.parentNode;
      dkAnnounce(el, $win.find(".ah-docking-window-title").first().text() + ": " +
                 ($panels.index(p) + 1) + " / " + ($(p).children(".ah-docking-window").index(win) + 1));
    }
    dkCommit(el, true);
  }

  function dkApply(el, v) {
    if (!v) { return; }
    var st = dkState(el);
    $.each(v.closed || [], function (_, id) {
      var $w = dkWindow(el, id);
      if ($w.length) { AH.destroy($w[0]); $w.remove(); }
      if (st.closed.indexOf(String(id)) < 0) { st.closed.push(String(id)); }
    });
    $.each(v.panels || [], function (_, p) {
      var panel = dkPanels(el).filter(function () {
        return this.getAttribute("data-panel-id") === String(p.id); })[0];
      if (!panel) { return; }
      $.each(p.windows || [], function (_, id) {
        var $w = dkWindow(el, id);
        if ($w.length) { dkInsert(panel, $w[0], -1); }
      });
    });
    $.each(v.floating || [], function (_, f) {
      var $w = dkWindow(el, f.id);
      if ($w.length) { dkFloat(el, $w[0], f.x || 0, f.y || 0, f.width); }
    });
    var collapsed = (v.collapsed || []).map(String);
    $(el).find(".ah-docking-window").each(function () {
      if ($(this).closest(".ah-docking")[0] !== el) { return; }
      var on = collapsed.indexOf(dkWid(this)) >= 0;
      $(this).toggleClass("ah-docking-window-collapsed", on)
        .find(".ah-docking-window-collapse-btn").first().attr("aria-expanded", on ? "false" : "true");
    });
  }

  function own(el, node, rootSel) { return $(node).closest(rootSel)[0] === el; }

  AH.define("docking", {
    init: function (el, $el) {
      dkState(el);
      $el.on("pointerdown" + NS, ".ah-docking-window-header", function (e) {
        var win = this.parentNode;
        if (e.button !== 0 || !own(el, this, ".ah-docking") || dkDisabled(el) ||
            $(win).hasClass("ah-docking-window-pinned") ||
            $(e.target).closest("button, a, input, select, textarea").length) { return; }
        dkDrag(el, win, e);
      });
      $el.on("click" + NS, ".ah-docking-window-collapse-btn", function (e) {
        if (!own(el, this, ".ah-docking") || dkDisabled(el)) { return; }
        e.stopPropagation();
        var win = $(this).closest(".ah-docking-window")[0];
        dkCollapse(el, win, !$(win).hasClass("ah-docking-window-collapsed"), true);
      });
      $el.on("click" + NS, ".ah-docking-window-close-btn", function (e) {
        if (!own(el, this, ".ah-docking") || dkDisabled(el)) { return; }
        e.stopPropagation();
        dkClose(el, $(this).closest(".ah-docking-window")[0], true);
      });
      $el.on("keydown" + NS, ".ah-docking-window-header", function (e) {
        if (e.target !== this || !own(el, this, ".ah-docking") || dkDisabled(el)) { return; }
        dkKey(el, this.parentNode, e);
      });
      dkCommit(el, false);
    },
    destroy: function (el) {
      stopDrag(el);
      $.removeData(el, "ah-docking");
    },
    methods: {
      collapse: function (el, $el, id) { var $w = dkWindow(el, id); if ($w.length) { dkCollapse(el, $w[0], true, false); } },
      expand: function (el, $el, id) { var $w = dkWindow(el, id); if ($w.length) { dkCollapse(el, $w[0], false, false); } },
      close: function (el, $el, id) { var $w = dkWindow(el, id); if ($w.length) { dkClose(el, $w[0], false); } },
      move: function (el, $el, id, panel, index) {
        var $w = dkWindow(el, id);
        var $p = typeof panel === "number" ? dkPanels(el).eq(panel)
          : dkPanels(el).filter(function () { return this.getAttribute("data-panel-id") === String(panel); });
        if (!$w.length || !$p.length) { return; }
        dkInsert($p[0], $w[0], index === undefined ? -1 : index);
        dkCommit(el, true);
      },
      pin: function (el, $el, id) { dkWindow(el, id).addClass("ah-docking-window-pinned"); },
      unpin: function (el, $el, id) { dkWindow(el, id).removeClass("ah-docking-window-pinned"); },
      addWindow: function (el, $el, panel, html) {
        var $p = dkPanels(el).filter(function () { return this.getAttribute("data-panel-id") === String(panel); });
        var $w = $($.parseHTML(String(html))).filter(".ah-docking-window");
        if (!$p.length || !$w.length) { return; }
        dkWindow(el, dkWid($w[0])).each(function () { AH.destroy(this); $(this).remove(); });
        $p[0].appendChild($w[0]);
        AH.mount($w[0]);
        var st = dkState(el), i = st.closed.indexOf(dkWid($w[0]));
        if (i >= 0) { st.closed.splice(i, 1); }
        dkCommit(el, true);
      },
      setLayout: function (el, $el, json) {
        dkApply(el, typeof json === "string" ? parse(json) : json);
        dkCommit(el, false);
      },
      disable: function (el, $el) { $el.addClass("ah-docking-disabled").attr("aria-disabled", "true"); },
      enable: function (el, $el) { $el.removeClass("ah-docking-disabled").removeAttr("aria-disabled"); },
      getValue: function (el) { return el.getAttribute("data-ah-value"); }
    }
  });

  // ------------------------------------------------------------------
  // dock_layout: structure
  // ------------------------------------------------------------------

  function dlState(el) {
    var st = $.data(el, "ah-dl");
    if (!st) {
      st = { memo: {}, timer: null, preview: null,
             labels: $.extend({ auto_hide: "Auto Hide", float: "Float", dock: "Dock", close: "Close" },
                              parse(el.getAttribute("data-ah-labels")) || {}) };
      $.data(el, "ah-dl", st);
    }
    return st;
  }

  function inner(el) { return $(el).children(".ah-dl-middle").children(".ah-dl-inner")[0]; }
  function floats(el) { return $(el).children(".ah-dl-float-container"); }
  function slots(el) {
    return $(el).children(".ah-dl-autohide-preview-slot")
      .add($(el).children(".ah-dl-middle").children(".ah-dl-autohide-preview-slot"));
  }
  function strips(el) {
    return $(el).children(".ah-dl-autohide-strip")
      .add($(el).children(".ah-dl-middle").children(".ah-dl-autohide-strip"));
  }
  function slotOf(el, edge) { return slots(el).filter(".ah-dl-autohide-preview-slot-" + edge)[0]; }
  function stripOf(el, edge) { return strips(el).filter(".ah-dl-autohide-strip-" + edge)[0]; }

  function isBar(n) { return n.classList.contains("ah-dl-splitbar"); }
  function real(c) { return $(c).children().not(".ah-dl-splitbar").get(); }
  function isContainer(c) { return !!c && $(c).is(".ah-dl-group, .ah-dl-inner"); }
  function isHoriz(c) {
    var $c = $(c);
    return $c.hasClass("ah-dl-inner") ? !$c.hasClass("ah-dl-vertical") : $c.hasClass("ah-dl-horizontal");
  }
  function weight(n) {
    var g = parseFloat(n.style.flexGrow);
    return isNaN(g) || g <= 0 ? 1 : g;
  }
  function setWeight(n, w) { n.style.flex = round2(w) + " 1 0px"; }

  function header(g) { return $(g).children(".ah-dl-tabs").children(".ah-tabs-header"); }
  function tabsOf(g) { return header(g).children(".ah-tabs-item"); }
  function panelsOf(g) { return $(g).children(".ah-dl-tabs").children(".ah-tabs-content").children(".ah-tabs-panel"); }
  function pid(li) { return li.getAttribute("data-panel-id"); }
  function panelFor(g, li) { return panelsOf(g).eq(tabsOf(g).index(li))[0]; }
  function activeTab(g) {
    var $t = tabsOf(g), $s = $t.filter(".ah-tabs-item-selected");
    return ($s.length ? $s : $t).first()[0];
  }
  function groupOf(node) { return $(node).closest(".ah-dl-tabbed")[0]; }
  function winOf(node) { return $(node).closest(".ah-dl-float-window")[0]; }
  function inSlot(node) { return $(node).closest(".ah-dl-autohide-preview-slot").length > 0; }
  function inLayout(el, node) { return $.contains(inner(el), node); }

  function flag(g, name) { return g.getAttribute("data-allow-" + name) === "true"; }
  function opt(el, name) { return el.getAttribute("data-ah-" + name) !== "false"; }

  function select(g, li) {
    var $t = tabsOf(g), i = $t.index(li);
    if (i < 0) { return; }
    $t.removeClass("ah-tabs-item-selected").attr({ "aria-selected": "false", tabindex: "-1" });
    $(li).addClass("ah-tabs-item-selected").attr({ "aria-selected": "true", tabindex: "0" });
    panelsOf(g).each(function (j) {
      $(this).toggleClass("ah-tabs-panel-active", j === i).prop("hidden", j !== i);
    });
    var win = winOf(g);
    if (win) {
      var t = $(li).text();
      $(win).attr("aria-label", t).find(".ah-dl-float-title").first().text(t);
    }
  }

  function newGroup(el, pin, close) {
    var st = dlState(el);
    return $($.parseHTML(AH.tpl.dock_layout_group({
      gid: el.id + "-n" + (++seq), document: false, pin: pin ? "true" : "false",
      close: close ? "true" : "false", pinned: "true", unpinned: false, edge: false, size: 0,
      style: false, pin_label: st.labels.auto_hide, close_label: st.labels.close, tabs: [] })))[0];
  }

  function newContainer(horiz) {
    return $('<div class="ah-dl-group"></div>').addClass(horiz ? "ah-dl-horizontal" : "ah-dl-vertical")[0];
  }

  // Take a tab and its panel out of their group (selecting a neighbour).
  function extract(li) {
    var g = groupOf(li), panel = panelFor(g, li);
    var $t = tabsOf(g), i = $t.index(li), was = $(li).hasClass("ah-tabs-item-selected");
    $(li).detach();
    $(panel).detach();
    $t = tabsOf(g);
    if (was && $t.length) { select(g, $t[Math.min(i, $t.length - 1)]); }
    return { li: li, panel: panel, from: g };
  }

  function insertTab(g, it, before) {
    var $h = header(g), ref = before || $h.children(".ah-dl-tabbed-actions")[0] || null;
    $h[0].insertBefore(it.li, ref);
    var content = $(g).children(".ah-dl-tabs").children(".ah-tabs-content")[0];
    var refPanel = before ? panelFor(g, before) : null;
    content.insertBefore(it.panel, refPanel || null);
    select(g, it.li);
  }

  // A group left without tabs goes away with its float window or its auto
  // hide tab; one in the layout is removed by tidy().
  function dropEmpty(el, g) {
    if (!g || tabsOf(g).length) { return; }
    var win = winOf(g);
    if (win) { $(win).remove(); return; }
    if (inSlot(g)) { discardHidden(el, g); }
    $(g).remove();
  }

  // Keep the tree regular: no empty groups, a splitbar between each pair
  // of siblings, groups of one child replaced by it, a group inside a
  // container of the same orientation merged into it.
  function tidy(el) {
    var st = dlState(el);
    (function walk(c) {
      $(c).children(".ah-dl-group").each(function () { walk(this); });
      $(c).children(".ah-dl-tabbed").each(function () { if (!tabsOf(this).length) { $(this).remove(); } });
      $(c).children(".ah-dl-group").each(function () {
        var g = this, kids = real(g);
        if (!kids.length) { $(g).remove(); return; }
        if (kids.length === 1) {
          kids[0].style.flex = g.style.flex || "1 1 0px";
          c.insertBefore(kids[0], g);
          c.removeChild(g);
        } else if (isHoriz(g) === isHoriz(c)) {
          var gw = weight(g), sum = 0;
          kids.forEach(function (k) { sum += weight(k); });
          kids.forEach(function (k) { setWeight(k, weight(k) * gw / sum); c.insertBefore(k, g); });
          c.removeChild(g);
        }
      });
      var nodes = $(c).children().get(), prevReal = false;
      nodes.forEach(function (n, i) {
        if (isBar(n)) {
          var nextReal = i + 1 < nodes.length && !isBar(nodes[i + 1]);
          if (!prevReal || !nextReal) { $(n).remove(); } else { prevReal = false; }
        } else {
          if (prevReal) { c.insertBefore(splitbar(el), n); }
          prevReal = true;
        }
      });
    })(inner(el));
    // memos of auto hidden groups whose old parent is gone
    $.each(st.memo, function (gid, m) {
      if (!m.parent.isConnected) { delete st.memo[gid]; }
    });
    refresh(el);
  }

  function splitbar(el) {
    var bar = $('<div class="ah-dl-splitbar" role="separator"></div>')[0];
    if (opt(el, "resizable")) { bar.setAttribute("tabindex", "0"); }
    return bar;
  }

  function canPin(g) {
    var p = g.parentNode;
    if (!isContainer(p) || g.getAttribute("data-document") === "true") { return false; }
    var kids = real(p), i = kids.indexOf(g);
    return kids.length >= 2 && (i === 0 || i === kids.length - 1);
  }

  function refresh(el) {
    $(inner(el)).find(".ah-dl-splitbar").each(function () {
      this.setAttribute("aria-orientation", isHoriz(this.parentNode) ? "vertical" : "horizontal");
    });
    $(inner(el)).find(".ah-dl-tabbed").each(function () {
      var off = !canPin(this);
      header(this).find(".ah-dl-btn-pin").toggleClass("ah-dl-btn-disabled", off)
        .attr("aria-disabled", off ? "true" : null);
    });
  }

  // ------------------------------------------------------------------
  // dock_layout: value
  // ------------------------------------------------------------------

  function groupValue(g, size) {
    var doc = g.getAttribute("data-document") === "true";
    var v = { type: doc ? "documents" : "tabs", id: g.getAttribute("data-group-id"), size: size,
              items: tabsOf(g).map(function () { return pid(this); }).get(),
              active: pid(activeTab(g)) };
    if (!doc) { v.pin = flag(g, "pin"); }
    v.close = flag(g, "close");
    return v;
  }

  function nodeValue(n, size) {
    var $n = $(n);
    if ($n.hasClass("ah-dl-group")) {
      return { type: "split", orientation: isHoriz(n) ? "horizontal" : "vertical", size: size,
               items: kidsValue(n) };
    }
    if ($n.hasClass("ah-dl-tabbed")) { return groupValue(n, size); }
    if ($n.hasClass("ah-dl-panel")) { return { type: "panel", size: size, item: pid(n) }; }
    return null;
  }

  function kidsValue(c) {
    var kids = real(c), total = 0;
    kids.forEach(function (k) { total += weight(k); });
    return kids.map(function (k) { return nodeValue(k, round2(weight(k) * 100 / total)); })
      .filter(function (v) { return v; });
  }

  function dlSerialize(el) {
    var inn = inner(el), out;
    if (!isHoriz(inn) && real(inn).length > 1) {
      out = [{ type: "split", orientation: "vertical", size: 100, items: kidsValue(inn) }];
    } else {
      out = kidsValue(inn);
    }
    floats(el).children(".ah-dl-float-window").each(function () {
      var g = $(this).find(".ah-dl-tabbed")[0];
      if (!g) { return; }
      var v = groupValue(g);
      out.push({ type: "float", id: this.getAttribute("data-group-id"), items: v.items, active: v.active,
                 x: Math.round(parseFloat(this.style.left) || 0), y: Math.round(parseFloat(this.style.top) || 0),
                 width: Math.round(parseFloat(this.style.width) || this.offsetWidth),
                 height: Math.round(parseFloat(this.style.height) || this.offsetHeight) });
    });
    slots(el).children(".ah-dl-tabbed").each(function () {
      var v = groupValue(this);
      out.push({ type: "autohide", id: v.id, edge: this.getAttribute("data-edge"),
                 size: Math.round(parseFloat(this.getAttribute("data-size")) || 250),
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
    var horiz = zone === "left" || zone === "right", before = zone === "left" || zone === "top";
    var p = target.parentNode;
    if (isContainer(p) && isHoriz(p) === horiz) {
      var w = weight(target);
      setWeight(target, w / 2);
      setWeight(g, w / 2);
      p.insertBefore(g, before ? target : target.nextSibling);
    } else {
      var wrap = newContainer(horiz);
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
    var inn = inner(el), horiz = zone === "edge-left" || zone === "edge-right";
    var first = zone === "edge-left" || zone === "edge-top";
    var kids = real(inn);
    if (kids.length > 1 && isHoriz(inn) !== horiz) {
      var wrap = newContainer(isHoriz(inn));
      while (inn.firstChild) { wrap.appendChild(inn.firstChild); }
      setWeight(wrap, 100);
      inn.appendChild(wrap);
      kids = [wrap];
    }
    $(inn).toggleClass("ah-dl-vertical", !horiz);
    var total = 0;
    kids.forEach(function (k) { total += weight(k); });
    var share = 0.25;
    if (size && kids.length) {
      var full = horiz ? inn.clientWidth : inn.clientHeight;
      if (full > size) { share = Math.min(0.75, size / full); }
    }
    setWeight(g, kids.length ? total * share / (1 - share) : 100);
    inn.insertBefore(g, first ? inn.firstChild : null);
  }

  // Dock tab items (from a drag, a float or a server-rendered group) at a
  // zone: "center" of a target group, a side of it, or an edge.
  function dockItems(el, items, zone, target, flags) {
    if (zone === "center") {
      items.forEach(function (it) { insertTab(target, it); });
      return;
    }
    var g = newGroup(el, flags.pin, flags.close);
    items.forEach(function (it) { insertTab(g, it); });
    if (flags.active) { select(g, flags.active); }
    if (/^edge-/.test(zone)) { insertEdge(el, g, zone); }
    else { insertBeside(target, g, zone); }
  }

  function defaultTarget(el) {
    var $gs = $(inner(el)).find(".ah-dl-tabbed");
    var $doc = $gs.filter("[data-document=true]");
    return ($doc.length ? $doc : $gs).first()[0];
  }

  function floatItems(el, items, x, y, flags) {
    var st = dlState(el), rr = el.getBoundingClientRect();
    var w = 300, h = 200;
    x = Math.max(0, Math.min(Math.round(x), Math.round(rr.width) - 120));
    y = Math.max(0, Math.min(Math.round(y), Math.round(rr.height) - 60));
    var win = $($.parseHTML(AH.tpl.dock_layout_float({
      fid: el.id + "-f" + (++seq), title: "", x: x, y: y, width: w, height: h,
      close_label: st.labels.close, body: "" })))[0];
    var g = newGroup(el, flags.pin, flags.close);
    $(win).children(".ah-dl-float-body")[0].appendChild(g);
    floats(el)[0].appendChild(win);
    items.forEach(function (it) { insertTab(g, it); });
    if (flags.active) { select(g, flags.active); }
    raise(win);
    return win;
  }

  function raise(win) {
    var max = 0;
    $(win).siblings(".ah-dl-float-window").each(function () {
      max = Math.max(max, parseInt(this.style.zIndex, 10) || 0);
    });
    win.style.zIndex = max + 1;
  }

  function sourceFlags(g) {
    return { pin: flag(g, "pin") || g.getAttribute("data-document") === "true",
             close: flag(g, "close") || g.getAttribute("data-document") === "true" };
  }

  function floatTab(el, li, x, y) {
    var g = groupOf(li), flags = sourceFlags(g), it = extract(li);
    dropEmpty(el, g);
    return floatItems(el, [it], x, y, flags);
  }

  // Dock a float window's tabs back (the whole group for a side or edge).
  function dockFloat(el, win, zone, target) {
    var g = $(win).find(".ah-dl-tabbed")[0];
    if (!zone) {
      target = defaultTarget(el);
      zone = target ? "center" : "edge-right";
    }
    if (zone === "center") {
      var act = activeTab(g);
      tabsOf(g).get().forEach(function (li) { insertTab(target, extract(li)); });
      select(target, act);
    } else {
      g.removeAttribute("style");
      if (/^edge-/.test(zone)) { insertEdge(el, g, zone); } else { insertBeside(target, g, zone); }
    }
    $(win).remove();
  }

  // ------------------------------------------------------------------
  // dock_layout: auto hide
  // ------------------------------------------------------------------

  function detectEdge(g) {
    for (var node = g; ; node = node.parentNode) {
      var p = node.parentNode;
      if (!isContainer(p)) { return "left"; }
      var kids = real(p), i = kids.indexOf(node), h = isHoriz(p);
      if (kids.length > 1 && i === 0) { return h ? "left" : "top"; }
      if (kids.length > 1 && i === kids.length - 1) { return h ? "right" : "bottom"; }
      if ($(p).hasClass("ah-dl-inner")) { return "left"; }
    }
  }

  function stripTab(el, gid) {
    return strips(el).children(".ah-dl-autohide-tab").filter(function () {
      return this.getAttribute("data-group-id") === gid; });
  }

  function setPinned(g, pinned) {
    g.setAttribute("data-pinned", pinned ? "true" : "false");
    header(g).find(".ah-dl-btn-pin").toggleClass("ah-dl-unpinned", !pinned)
      .attr("aria-pressed", pinned ? "false" : "true");
  }

  function autoHide(el, g) {
    if (!canPin(g)) { return false; }
    var st = dlState(el), gid = g.getAttribute("data-group-id"), edge = detectEdge(g);
    var p = g.parentNode, side = edge === "left" || edge === "right";
    var size = Math.round(side ? g.offsetWidth : g.offsetHeight) || 250;
    st.memo[gid] = { parent: p, index: real(p).indexOf(g), weight: weight(g) };
    setPinned(g, false);
    g.setAttribute("data-edge", edge);
    g.setAttribute("data-size", size);
    g.removeAttribute("style");
    var focus = $.contains(g, document.activeElement);
    slotOf(el, edge).appendChild(g);
    var $tab = $('<div class="ah-dl-autohide-tab" role="button" tabindex="0" aria-expanded="false"></div>')
      .attr("data-group-id", gid).text($(tabsOf(g)[0]).text());
    stripOf(el, edge).appendChild($tab[0]);
    if (focus) { $tab.trigger("focus"); }
    return true;
  }

  function showPreview(el, gid) {
    var st = dlState(el);
    clearTimeout(st.timer);
    var g = slots(el).children(".ah-dl-tabbed").filter(function () {
      return this.getAttribute("data-group-id") === gid; })[0];
    if (!g) { return; }
    if (st.preview && st.preview !== gid) { hidePreview(el); }
    var edge = g.getAttribute("data-edge"), slot = g.parentNode;
    $(slot).children(".ah-dl-tabbed").removeClass("ah-dl-autohide-current");
    $(g).addClass("ah-dl-autohide-current");
    slot.style.display = "flex";
    slot.style.flex = "0 0 " + (parseFloat(g.getAttribute("data-size")) ||
                                 (edge === "top" || edge === "bottom" ? 200 : 280)) + "px";
    stripTab(el, gid).addClass("ah-dl-autohide-tab-active").attr("aria-expanded", "true");
    st.preview = gid;
  }

  function hidePreview(el) {
    var st = dlState(el);
    clearTimeout(st.timer);
    slots(el).each(function () {
      this.style.display = "";
      this.style.flex = "";
      $(this).children(".ah-dl-tabbed").removeClass("ah-dl-autohide-current");
    });
    strips(el).children(".ah-dl-autohide-tab").removeClass("ah-dl-autohide-tab-active")
      .attr("aria-expanded", "false");
    st.preview = null;
  }

  function discardHidden(el, g) {
    var st = dlState(el), gid = g.getAttribute("data-group-id");
    if (st.preview === gid) { hidePreview(el); }
    stripTab(el, gid).remove();
    delete st.memo[gid];
  }

  // Back from the edge: into its old place if that still exists, else at
  // its edge of the layout.
  function repin(el, g) {
    var st = dlState(el), gid = g.getAttribute("data-group-id");
    var edge = g.getAttribute("data-edge") || "left", size = parseFloat(g.getAttribute("data-size"));
    var m = st.memo[gid];
    discardHidden(el, g);
    $(g).removeClass("ah-dl-autohide-current");
    g.removeAttribute("data-edge");
    g.removeAttribute("data-size");
    setPinned(g, true);
    if (m && m.parent.isConnected && inLayout(el, m.parent) || m && m.parent === inner(el)) {
      var kids = real(m.parent);
      m.parent.insertBefore(g, m.index < kids.length ? kids[m.index] : null);
      setWeight(g, m.weight);
    } else {
      insertEdge(el, g, "edge-" + edge, size);
    }
  }

  function scheduleHide(el) {
    var st = dlState(el);
    clearTimeout(st.timer);
    st.timer = setTimeout(function () {
      if (!$(el).children(".ah-dl-context-menu").length &&
          !$(slots(el)).find(document.activeElement).length) { hidePreview(el); }
    }, HIDE_DELAY);
  }

  // ------------------------------------------------------------------
  // dock_layout: closing
  // ------------------------------------------------------------------

  function closed(el, ids, user) {
    if (user && ids.length) { fire(el, "ah:panel-close", "panels", ids.join(",")); }
  }

  function closeGroup(el, g, user) {
    var ids = tabsOf(g).map(function () { return pid(this); }).get();
    var win = winOf(g);
    if (inSlot(g)) { discardHidden(el, g); }
    AH.destroy(win || g);
    $(win || g).remove();
    closed(el, ids, user);
  }

  function closeTab(el, li, user) {
    var g = groupOf(li), focus = $.contains(g, document.activeElement), it = extract(li);
    AH.destroy(it.panel);
    $(it.panel).remove();
    $(li).remove();
    if (focus && tabsOf(g).length) { activeTab(g).focus(); }
    dropEmpty(el, g);
    closed(el, [pid(li)], user);
  }

  // ------------------------------------------------------------------
  // dock_layout: dragging
  // ------------------------------------------------------------------

  function overlay(el) { return $(el).children(".ah-dl-dock-overlay"); }

  function clearZones($ov) {
    $ov.find(".ah-dl-dock-zone-active").removeClass("ah-dl-dock-zone-active");
    $ov.find(".ah-dl-dock-edge-active").removeClass("ah-dl-dock-edge-active");
    $ov.children(".ah-dl-dock-preview").removeClass("ah-dl-dock-preview-visible");
  }

  // The smallest tab group of the layout under the pointer.
  function groupAt(el, x, y, skip) {
    var best = null, area = Infinity;
    $(inner(el)).find(".ah-dl-tabbed").each(function () {
      if (this === skip) { return; }
      var r = this.getBoundingClientRect();
      if (inside(x, y, r) && r.width * r.height < area) { area = r.width * r.height; best = this; }
    });
    return best;
  }

  // Place the cross over the group under the pointer and find the zone
  // under it: {zone, target} or null; shows the preview rectangle.
  function trackZones(el, x, y, skip) {
    var $ov = overlay(el), rr = el.getBoundingClientRect(), $cross = $ov.children(".ah-dl-dock-cross");
    var target = groupAt(el, x, y, skip);
    if (target) {
      var tr = target.getBoundingClientRect();
      $cross.css({ display: "grid",
                   left: Math.round(tr.left - rr.left + tr.width / 2 - CROSS / 2) + "px",
                   top: Math.round(tr.top - rr.top + tr.height / 2 - CROSS / 2) + "px" });
    } else {
      $cross.css("display", "none");
    }
    clearZones($ov);
    // the edges of the layout first: the cross of a group at the rim may
    // reach under them
    var hit = null;
    $ov.children(".ah-dl-dock-edge").each(function () {
      if (!hit && inside(x, y, this.getBoundingClientRect())) {
        hit = { zone: this.getAttribute("data-zone"), target: null, el: this };
      }
    });
    if (!hit && target) {
      $cross.children(".ah-dl-dock-zone").each(function () {
        if (!hit && inside(x, y, this.getBoundingClientRect())) {
          hit = { zone: this.getAttribute("data-zone"), target: target, el: this };
        }
      });
    }
    if (!hit) { return null; }
    $(hit.el).addClass(hit.target ? "ah-dl-dock-zone-active" : "ah-dl-dock-edge-active");
    var box = (hit.target || inner(el)).getBoundingClientRect();
    var l = box.left - rr.left, t = box.top - rr.top, w = box.width, h = box.height;
    var p = { top: [l, t, w, h / 2], bottom: [l, t + h / 2, w, h / 2], left: [l, t, w / 2, h],
              right: [l + w / 2, t, w / 2, h], center: [l, t, w, h],
              "edge-top": [l, t, w, h / 4], "edge-bottom": [l, t + h * 0.75, w, h / 4],
              "edge-left": [l, t, w / 4, h], "edge-right": [l + w * 0.75, t, w / 4, h] }[hit.zone];
    $ov.children(".ah-dl-dock-preview").addClass("ah-dl-dock-preview-visible")
      .css({ left: p[0] + "px", top: p[1] + "px", width: p[2] + "px", height: p[3] + "px" });
    return hit;
  }

  function showOverlay(el) { overlay(el).addClass("ah-dl-dock-overlay-visible"); }
  function hideOverlay(el) {
    var $ov = overlay(el);
    clearZones($ov);
    $ov.removeClass("ah-dl-dock-overlay-visible").children(".ah-dl-dock-cross").css("display", "none");
  }

  function dragTab(el, li, e) {
    var g = groupOf(li), $ghost, zone = null;
    var allowDock = opt(el, "allow-dock"), allowFloat = opt(el, "allow-float");
    // a group of one tab is not a target for its own tab
    var skip = function () { return tabsOf(g).length === 1 ? g : null; };
    track(el, e, {
      start: function (ev) {
        $ghost = $('<div class="ah-dl-drag-ghost" aria-hidden="true"></div>').text($(li).text())
          .css({ left: ev.clientX + "px", top: ev.clientY + "px" }).appendTo(el);
        $(li).addClass("ah-dl-tab-dragging");
        if (allowDock) { showOverlay(el); }
      },
      move: function (ev) {
        $ghost.css({ left: ev.clientX + "px", top: ev.clientY + "px" });
        zone = allowDock ? trackZones(el, ev.clientX, ev.clientY, skip()) : null;
      },
      end: function (ev, cancelled) {
        $ghost.remove();
        $(li).removeClass("ah-dl-tab-dragging");
        hideOverlay(el);
        if (cancelled || !ev) { return; }
        var h = header(g)[0];
        if (zone && !(zone.zone === "center" && zone.target === g)) {
          var flags = sourceFlags(g), it = extract(li);
          dropEmpty(el, g);
          dockItems(el, [it], zone.zone, zone.target, flags);
        } else if (inside(ev.clientX, ev.clientY, h.getBoundingClientRect())) {
          // reorder within the header
          var over = null;
          tabsOf(g).each(function () {
            var r = this.getBoundingClientRect();
            if (!over && this !== li && ev.clientX < r.left + r.width / 2) { over = this; }
          });
          if (over !== li.nextSibling) { insertTab(g, extract(li), over); }
        } else if (allowFloat && !(zone && zone.target === g)) {
          var rr = el.getBoundingClientRect();
          floatTab(el, li, ev.clientX - rr.left - 20, ev.clientY - rr.top - 12);
        } else {
          return;
        }
        if (done(el, true) && $.contains(el, li)) { li.focus(); }
      }
    });
  }

  function dragFloat(el, win, e) {
    var allowDock = opt(el, "allow-dock"), x0, y0, zone = null;
    track(el, e, {
      start: function () {
        x0 = parseFloat(win.style.left) || 0;
        y0 = parseFloat(win.style.top) || 0;
        if (allowDock) { showOverlay(el); }
      },
      move: function (ev) {
        win.style.left = Math.round(x0 + ev.clientX - e.clientX) + "px";
        win.style.top = Math.round(y0 + ev.clientY - e.clientY) + "px";
        zone = allowDock ? trackZones(el, ev.clientX, ev.clientY, null) : null;
      },
      end: function (ev, cancelled) {
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
    var w0 = win.offsetWidth, h0 = win.offsetHeight;
    track(el, e, {
      start: function () {},
      move: function (ev) {
        win.style.width = Math.max(150, Math.round(w0 + ev.clientX - e.clientX)) + "px";
        win.style.height = Math.max(100, Math.round(h0 + ev.clientY - e.clientY)) + "px";
      },
      end: function (ev, cancelled) {
        if (cancelled) { win.style.width = w0 + "px"; win.style.height = h0 + "px"; }
        done(el, !cancelled);
      }
    }, 0);
  }

  // Resize the two neighbours of a splitbar by `delta' px from sizes
  // `sp', `sn' (px) and weights `wp', `wn'.
  function resizePair(el, bar, delta, base) {
    var prev = bar.previousElementSibling, next = bar.nextElementSibling;
    var min = Math.min(parseFloat(el.getAttribute("data-ah-min-size")) || 0, (base.sp + base.sn) / 2);
    var np = Math.max(min, Math.min(base.sp + delta, base.sp + base.sn - min));
    var total = base.wp + base.wn;
    setWeight(prev, total * np / (base.sp + base.sn));
    setWeight(next, total - total * np / (base.sp + base.sn));
    return np - base.sp;
  }

  function pairBase(bar) {
    var prev = bar.previousElementSibling, next = bar.nextElementSibling;
    var horiz = isHoriz(bar.parentNode);
    var a = prev.getBoundingClientRect(), b = next.getBoundingClientRect();
    return { horiz: horiz, sp: horiz ? a.width : a.height, sn: horiz ? b.width : b.height,
             wp: weight(prev), wn: weight(next) };
  }

  function dragSplitbar(el, bar, e) {
    var base, $line = null, feedback = el.getAttribute("data-ah-resize-mode") === "feedback", delta = 0;
    track(el, e, {
      start: function () {
        base = pairBase(bar);
        if (feedback) {
          var rr = el.getBoundingClientRect(), br = bar.getBoundingClientRect();
          $line = $('<div class="ah-dl-resize-feedback"></div>')
            .addClass(base.horiz ? "ah-dl-resize-feedback-h" : "ah-dl-resize-feedback-v")
            .css(base.horiz ? { left: br.left - rr.left, top: br.top - rr.top, bottom: "auto", height: br.height }
                            : { top: br.top - rr.top, left: br.left - rr.left, right: "auto", width: br.width })
            .appendTo(el);
          base.line = base.horiz ? br.left - rr.left : br.top - rr.top;
        }
      },
      move: function (ev) {
        var d = base.horiz ? ev.clientX - e.clientX : ev.clientY - e.clientY;
        if (feedback) {
          var min = Math.min(parseFloat(el.getAttribute("data-ah-min-size")) || 0, (base.sp + base.sn) / 2);
          delta = Math.max(min - base.sp, Math.min(d, base.sn - min));
          $line.css(base.horiz ? "left" : "top", base.line + delta);
        } else {
          resizePair(el, bar, d, base);
        }
      },
      end: function (ev, cancelled) {
        if ($line) { $line.remove(); }
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
    var L = dlState(el).labels, items = [];
    var item = function (action, label, disabled) {
      items.push({ action: action, label: label, disabled: !!disabled, divider: false });
    };
    var allowFloat = opt(el, "allow-float");
    if (win) {
      if (opt(el, "allow-dock")) { item("dock", L.dock); }
      item("close-float", L.close);
    } else if (g.getAttribute("data-document") === "true") {
      if (allowFloat) { item("float", L.float); }
      if (flag(g, "close")) { items.push({ divider: true }); item("close", L.close); }
    } else if (inSlot(g)) {
      item("unpin", L.dock);
      if (allowFloat) { item("float", L.float); }
      if (flag(g, "close")) { items.push({ divider: true }); item("close", L.close); }
    } else {
      if (flag(g, "pin")) { item("pin", L.auto_hide, !canPin(g)); }
      if (allowFloat) { item("float", L.float); }
      if (flag(g, "close")) { items.push({ divider: true }); item("close", L.close); }
    }
    if (items.length && items[0].divider) { items.shift(); }
    return items;
  }

  function closeMenu(el, refocus) {
    var st = dlState(el), $m = $(el).children(".ah-dl-context-menu");
    if (!$m.length) { return; }
    $m.remove();
    $(document).off(".ahdlmenu" + st.menuNs);
    if (refocus && st.menu && st.menu.focus && $.contains(el, st.menu.focus)) { st.menu.focus.focus(); }
    st.menu = null;
  }

  function openMenu(el, g, win, li, x, y) {
    closeMenu(el, false);
    var items = menuItems(el, g, win);
    if (!items.length) { return; }
    var st = dlState(el), rr = el.getBoundingClientRect();
    var $m = $($.parseHTML(AH.tpl.dock_layout_menu({
      x: Math.round(x - rr.left), y: Math.round(y - rr.top), items: items })));
    $m.appendTo(el);
    var m = $m[0];
    if (m.offsetLeft + m.offsetWidth > el.clientWidth) { m.style.left = Math.max(0, el.clientWidth - m.offsetWidth) + "px"; }
    if (m.offsetTop + m.offsetHeight > el.clientHeight) { m.style.top = Math.max(0, el.clientHeight - m.offsetHeight) + "px"; }
    st.menu = { g: g, win: win, li: li, x: x, y: y, focus: document.activeElement };
    st.menuNs = ++seq;
    $(document).on("pointerdown.ahdlmenu" + st.menuNs, function (e) {
      if (!$(e.target).closest(m).length) { closeMenu(el, false); }
    });
    $m.children(".ah-dl-menu-item").not(".ah-dl-menu-item-disabled").first().trigger("focus");
  }

  function runMenu(el, action) {
    var st = dlState(el), m = st.menu;
    if (!m) { return; }
    closeMenu(el, false);
    var rr = el.getBoundingClientRect(), g = m.g, focus = null;
    switch (action) {
      case "pin":
        if (autoHide(el, g)) { focus = stripTab(el, g.getAttribute("data-group-id"))[0]; }
        break;
      case "unpin":
        repin(el, g);
        focus = activeTab(g);
        break;
      case "float":
        var li = m.li && groupOf(m.li) === g ? m.li : activeTab(g);
        floatTab(el, li, m.x - rr.left, m.y - rr.top);
        focus = li;
        break;
      case "dock":
        focus = activeTab($(m.win).find(".ah-dl-tabbed")[0]);
        dockFloat(el, m.win);
        break;
      case "close-float":
        closeGroup(el, $(m.win).find(".ah-dl-tabbed")[0], true);
        break;
      case "close":
        closeGroup(el, g, true);
        break;
      default:
        return;
    }
    done(el, true);
    if (focus && $.contains(el, focus)) { focus.focus(); }
    else if (m.focus && $.contains(el, m.focus)) { m.focus.focus(); }
  }

  function menuKey(el, e) {
    var $items = $(el).children(".ah-dl-context-menu").children(".ah-dl-menu-item").not(".ah-dl-menu-item-disabled");
    var i = $items.index(document.activeElement);
    switch (e.key) {
      case "ArrowDown": $items.eq((i + 1) % $items.length).trigger("focus"); break;
      case "ArrowUp": $items.eq((i - 1 + $items.length) % $items.length).trigger("focus"); break;
      case "Home": $items.first().trigger("focus"); break;
      case "End": $items.last().trigger("focus"); break;
      case "Enter": case " ":
        if (i >= 0) { runMenu(el, $items[i].getAttribute("data-action")); }
        break;
      case "Escape": case "Tab": closeMenu(el, true); break;
      default: return;
    }
    e.preventDefault();
  }

  function menuAt(node) {
    var r = node.getBoundingClientRect();
    return { x: r.left, y: r.bottom };
  }

  // ------------------------------------------------------------------
  // dock_layout: behaviour
  // ------------------------------------------------------------------

  function findTab(el, id) {
    return $(el).find(".ah-dl-tabs > .ah-tabs-header > .ah-tabs-item").filter(function () {
      return pid(this) === String(id) && $(this).closest(".ah-dl")[0] === el;
    })[0];
  }

  function dlDisabled(el) { return $(el).hasClass("ah-dl-disabled"); }

  // Show a panel: select its tab, open its auto hide group, raise its float.
  function activate(el, id) {
    var li = findTab(el, id);
    if (!li) { return; }
    var g = groupOf(li), win = winOf(g);
    if (inSlot(g)) { showPreview(el, g.getAttribute("data-group-id")); }
    if (win) { raise(win); }
    if (!$(li).hasClass("ah-tabs-item-selected")) {
      select(g, li);
      commit(el, dlSerialize(el), true);
    }
  }

  var TAB = ".ah-dl-tabs > .ah-tabs-header > .ah-tabs-item";

  AH.define("dock_layout", {
    init: function (el, $el) {
      var st = dlState(el);
      var mine = function (node) { return $(node).closest(".ah-dl")[0] === el && !dlDisabled(el); };

      // tabs: select, keyboard, drag, context menu
      $el.on("click" + NS, TAB, function () {
        if (!mine(this)) { return; }
        var g = groupOf(this);
        if (!$(this).hasClass("ah-tabs-item-selected")) {
          select(g, this);
          commit(el, dlSerialize(el), true);
        }
      });
      $el.on("keydown" + NS, TAB, function (e) {
        if (!mine(this)) { return; }
        var g = groupOf(this), $t = tabsOf(g), i = $t.index(this), t = null;
        switch (e.key) {
          case "ArrowRight": t = (i + 1) % $t.length; break;
          case "ArrowLeft": t = (i - 1 + $t.length) % $t.length; break;
          case "Home": t = 0; break;
          case "End": t = $t.length - 1; break;
          case "Delete":
            if (flag(g, "close") && !winOf(g)) {
              e.preventDefault();
              closeTab(el, this, true);
              done(el, true);
            }
            return;
          case "ContextMenu": case "F10":
            if (e.key === "F10" && !e.shiftKey) { return; }
            e.preventDefault();
            var p = menuAt(this);
            openMenu(el, g, winOf(g), this, p.x, p.y);
            return;
          default: return;
        }
        e.preventDefault();
        select(g, $t[t]);
        $t[t].focus();
        commit(el, dlSerialize(el), true);
      });
      $el.on("pointerdown" + NS, TAB, function (e) {
        if (e.button !== 0 || !mine(this)) { return; }
        var g = groupOf(this);
        if (winOf(g) || inSlot(g) || (!opt(el, "allow-float") && !opt(el, "allow-dock"))) { return; }
        dragTab(el, this, e);
      });
      $el.on("contextmenu" + NS, TAB + ", .ah-dl-tabbed-actions, .ah-dl-float-titlebar", function (e) {
        if (!mine(this)) { return; }
        e.preventDefault();
        var win = winOf(this);
        var g = win ? $(win).find(".ah-dl-tabbed")[0] : groupOf(this);
        openMenu(el, g, win, $(this).hasClass("ah-tabs-item") ? this : null, e.clientX, e.clientY);
      });
      $el.on("click" + NS, ".ah-dl-context-menu > .ah-dl-menu-item", function (e) {
        e.stopPropagation();
        if (!$(this).hasClass("ah-dl-menu-item-disabled")) { runMenu(el, this.getAttribute("data-action")); }
      });
      $el.on("keydown" + NS, ".ah-dl-context-menu", function (e) { menuKey(el, e); });

      // group buttons
      $el.on("click" + NS, ".ah-dl-tabbed-actions > .ah-dl-btn-pin", function (e) {
        if (!mine(this) || $(this).hasClass("ah-dl-btn-disabled")) { return; }
        e.stopPropagation();
        var g = groupOf(this);
        if (inSlot(g)) {
          hidePreview(el);
          repin(el, g);
          done(el, true);
          activeTab(g).focus();
        } else if (autoHide(el, g)) {
          done(el, true);
        }
      });
      $el.on("click" + NS, ".ah-dl-tabbed-actions > .ah-dl-btn-close", function (e) {
        if (!mine(this)) { return; }
        e.stopPropagation();
        closeGroup(el, groupOf(this), true);
        done(el, true);
      });

      // float windows
      $el.on("pointerdown" + NS, ".ah-dl-float-window", function () {
        if (mine(this)) { raise(this); }
      });
      $el.on("pointerdown" + NS, ".ah-dl-float-titlebar", function (e) {
        if (e.button !== 0 || !mine(this) || $(e.target).closest(".ah-dl-btn").length) { return; }
        dragFloat(el, winOf(this), e);
      });
      $el.on("pointerdown" + NS, ".ah-dl-float-resize-se", function (e) {
        if (e.button !== 0 || !mine(this)) { return; }
        e.preventDefault();
        e.stopPropagation();
        resizeFloat(el, winOf(this), e);
      });
      $el.on("click" + NS, ".ah-dl-float-actions > .ah-dl-btn-close", function (e) {
        if (!mine(this)) { return; }
        e.stopPropagation();
        closeGroup(el, $(winOf(this)).find(".ah-dl-tabbed")[0], true);
        done(el, true);
      });
      $el.on("keydown" + NS, ".ah-dl-float-titlebar", function (e) {
        if (e.target !== this || !mine(this)) { return; }
        var win = winOf(this), step = 10;
        var dx = e.key === "ArrowLeft" ? -step : e.key === "ArrowRight" ? step : 0;
        var dy = e.key === "ArrowUp" ? -step : e.key === "ArrowDown" ? step : 0;
        if (e.key === "ContextMenu" || (e.key === "F10" && e.shiftKey)) {
          e.preventDefault();
          var p = menuAt(this);
          openMenu(el, $(win).find(".ah-dl-tabbed")[0], win, null, p.x, p.y);
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
      $el.on("pointerdown" + NS, ".ah-dl-splitbar", function (e) {
        if (e.button !== 0 || !mine(this) || !opt(el, "resizable")) { return; }
        e.preventDefault();
        dragSplitbar(el, this, e);
      });
      $el.on("keydown" + NS, ".ah-dl-splitbar", function (e) {
        if (!mine(this) || !opt(el, "resizable")) { return; }
        var horiz = isHoriz(this.parentNode), step = e.shiftKey ? 50 : 10;
        var d = { ArrowLeft: horiz ? -step : 0, ArrowRight: horiz ? step : 0,
                  ArrowUp: horiz ? 0 : -step, ArrowDown: horiz ? 0 : step }[e.key];
        if (!d) { return; }
        e.preventDefault();
        resizePair(el, this, d, pairBase(this));
        commit(el, dlSerialize(el), true);
      });

      // auto hide strips and previews
      $el.on("mouseenter" + NS, ".ah-dl-autohide-tab", function () {
        if (!mine(this)) { return; }
        var gid = this.getAttribute("data-group-id");
        clearTimeout(st.timer);
        st.timer = setTimeout(function () { showPreview(el, gid); }, SHOW_DELAY);
      });
      $el.on("mouseleave" + NS, ".ah-dl-autohide-tab", function () {
        if (mine(this)) { scheduleHide(el); }
      });
      $el.on("mouseenter" + NS, ".ah-dl-autohide-preview-slot", function () {
        if (mine(this)) { clearTimeout(st.timer); }
      });
      $el.on("mouseleave" + NS, ".ah-dl-autohide-preview-slot", function () {
        if (mine(this)) { scheduleHide(el); }
      });
      $el.on("click" + NS, ".ah-dl-autohide-tab", function () {
        if (!mine(this)) { return; }
        var gid = this.getAttribute("data-group-id");
        if (st.preview === gid) { hidePreview(el); } else { showPreview(el, gid); }
      });
      $el.on("keydown" + NS, ".ah-dl-autohide-tab", function (e) {
        if (!mine(this) || (e.key !== "Enter" && e.key !== " ")) { return; }
        e.preventDefault();
        var gid = this.getAttribute("data-group-id");
        if (st.preview === gid) { hidePreview(el); return; }
        showPreview(el, gid);
        var g = slots(el).children(".ah-dl-autohide-current")[0];
        if (g) { activeTab(g).focus(); }
      });
      $el.on("keydown" + NS, ".ah-dl-autohide-preview-slot", function (e) {
        if (e.key !== "Escape" || !mine(this) || !st.preview) { return; }
        e.preventDefault();
        var gid = st.preview;
        hidePreview(el);
        stripTab(el, gid).trigger("focus");
      });

      refresh(el);
      commit(el, dlSerialize(el), false);
    },
    destroy: function (el) {
      var st = $.data(el, "ah-dl");
      stopDrag(el);
      if (st) {
        clearTimeout(st.timer);
        closeMenu(el, false);
      }
      $.removeData(el, "ah-dl");
    },
    methods: {
      activate: function (el, $el, id) { activate(el, id); },
      float: function (el, $el, id) {
        var li = findTab(el, id);
        if (!li || winOf(li)) { return; }
        var n = floats(el).children().length;
        floatTab(el, li, 40 + 24 * n, 40 + 24 * n);
        done(el, true);
      },
      dock: function (el, $el, id) {
        var li = findTab(el, id);
        if (!li) { return; }
        var g = groupOf(li), win = winOf(g);
        if (win) { dockFloat(el, win); }
        else if (inSlot(g)) { hidePreview(el); repin(el, g); }
        else { return; }
        done(el, true);
      },
      close: function (el, $el, id) {
        var li = findTab(el, id);
        if (!li) { return; }
        closeTab(el, li, false);
        done(el, true);
      },
      openPanel: function (el, $el, html, where) {
        where = where || {};
        var g = $($.parseHTML(String(html))).filter(".ah-dl-tabbed")[0];
        var li = g && tabsOf(g)[0];
        if (!li) { return; }
        var id = pid(li);
        if (findTab(el, id)) { activate(el, id); return; }
        var byId = function () { return this.getAttribute("data-group-id") === String(where["in"]); };
        var target = where["in"] ? ($(inner(el)).find(".ah-dl-tabbed").filter(byId)[0] ||
                                    slots(el).children(".ah-dl-tabbed").filter(byId)[0]) : null;
        if (where.float) {
          var n = floats(el).children().length;
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
      },
      getValue: function (el) { return el.getAttribute("data-ah-value"); }
    }
  });
})(window.jQuery, window.AH);
