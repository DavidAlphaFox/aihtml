/* The docking behaviour (designs/04-components.md), ported from sigil's
 * layout/docking (+ docking/drag): windows dragged by their header between
 * panels, left floating, collapsed, closed; Alt+arrows move the focused
 * window from the keyboard.
 *
 * The arrangement is the value: JSON in data-ah-value (and the hidden
 * input), rewritten after every change, and `change' on the root when it
 * changed (user actions and the rearranging methods; setLayout excepted).
 * Windows are only ever moved, never rebuilt, so their contents keep
 * their state. The drag tracking is shared with dock_layout.js
 * (_lib_dock.js).
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.dock;
  var inside = L.inside,
      parse = L.parse,
      commit = L.commit,
      fire = L.fire,
      track = L.track,
      stopDrag = L.stopDrag,
      own = L.own;

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
})(window.jQuery, window.AH);
