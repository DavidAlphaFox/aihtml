/* The window behaviour (designs/04-components.md), ported from sigil's
 * overlay/window/*: a draggable, resizable window, optionally modal.
 * Shared machinery: _lib_overlay.js. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.overlay;
  var GNS = L.GNS,
      uid = L.uid,
      flag = L.flag,
      nextZ = L.nextZ,
      lock = L.lock,
      unlock = L.unlock,
      pushStack = L.pushStack,
      pullStack = L.pullStack;

  // ------------------------------------------------------------------
  // Window (sigil overlay/window/*)
  // ------------------------------------------------------------------

  function winOpen(el) { return el.getAttribute("data-state") === "open"; }

  function winFront(el) {
    var zi = nextZ();
    el.style.zIndex = zi;
    var bd = $.data(el, "ahBackdrop");
    if (bd) { bd.style.zIndex = zi - 1; }
  }

  function windowOpen(el) {
    if (winOpen(el)) { return; }
    var ev = $.Event("ah:opening");
    $(el).trigger(ev);
    if (ev.isDefaultPrevented()) { return; }
    var modal = flag(el, "modal", false);
    var $el = $(el).stop(true, true);
    if (!el.hasAttribute("data-ah-placed")) {
      // centre in the viewport on first open (sigil: position :center)
      el.style.visibility = "hidden";
      el.style.display = "flex";
      var w = el.offsetWidth;
      var h = el.offsetHeight;
      el.style.left = Math.max(0, (window.innerWidth - w) / 2) + "px";
      el.style.top = Math.max(0, (window.innerHeight - h) / 2) + "px";
      el.style.display = "none";
      el.style.visibility = "";
      el.setAttribute("data-ah-placed", "");
    }
    if (modal) {
      var $bd = $('<div class="ah-window-modal-backdrop"></div>');
      $bd.insertBefore(el).hide().fadeIn(250);
      $bd.on("mousedown" + NS, function () {
        if (flag(el, "scrim", false)) { windowClose(el); }
      });
      $.data(el, "ahBackdrop", $bd[0]);
      lock();
    }
    winFront(el);
    el.setAttribute("data-state", "open");
    pushStack({
      el: el, trap: modal ? el : null,
      esc: function () {
        if (!flag(el, "esc", true)) { return false; }
        var a = document.activeElement;
        if (!modal && a !== el && !$.contains(el, a)) { return false; }
        windowClose(el);
        return true;
      }
    });
    $el.fadeIn(250);
    el.focus({ preventScroll: true });
    $el.trigger("ah:open");
  }

  function windowClose(el, result) {
    if (!winOpen(el)) { return; }
    var ev = $.Event("ah:closing");
    $(el).trigger(ev, [{ result: result || null }]);
    if (ev.isDefaultPrevented()) { return; }
    el.setAttribute("data-state", "closed");
    var bd = $.data(el, "ahBackdrop");
    if (bd) {
      $.removeData(el, "ahBackdrop");
      $(bd).stop(true).fadeOut(250, function () { $(bd).remove(); });
      unlock();
    }
    pullStack(el, true);
    $(el).stop(true, true).fadeOut(250);
    $(el).trigger("ah:close", [{ result: result || null }]);
  }

  function windowCollapse(el, collapsed) {
    $(el).toggleClass("ah-window-collapsed", collapsed);
    $(el).find(".ah-window-collapse-btn").attr("aria-expanded", collapsed ? "false" : "true");
    $(el).trigger(collapsed ? "ah:collapse" : "ah:expand");
  }

  function windowMove(el, x, y) {
    el.style.left = x + "px";
    el.style.top = y + "px";
    el.setAttribute("data-ah-placed", "");
    $(el).trigger("ah:moved", [{ x: x, y: y }]);
  }

  function windowResize(el, w, h) {
    el.style.width = w + "px";
    el.style.height = h + "px";
    $(el).trigger("ah:resize", [{ width: w, height: h }]);
  }

  // One pointer drag: move(dx, dy) while the pointer moves, end() once.
  function drag(e, ns, move, end) {
    var x0 = e.clientX;
    var y0 = e.clientY;
    $(document)
      .on("pointermove" + ns, function (me) { move(me.clientX - x0, me.clientY - y0); })
      .on("pointerup" + ns + " pointercancel" + ns, function () {
        $(document).off(ns);
        end();
      });
  }

  var MIN_W = 100;
  var MIN_H = 60;

  AH.define("window", {
    init: function (el, $el) {
      var id = uid("win");
      $.data(el, "ahNs", GNS + id);
      $el.on("mousedown" + NS + " pointerdown" + NS, function () {
        if (winOpen(el)) { winFront(el); }
      });
      $el.on("click" + NS, ".ah-window-collapse-btn", function (e) {
        e.stopPropagation();
        windowCollapse(el, !$el.hasClass("ah-window-collapsed"));
      });
      // drag by the title bar
      $el.on("pointerdown" + NS, ".ah-window-header", function (e) {
        if (!flag(el, "draggable", true) || $(e.target).closest("button").length ||
            e.button !== 0) { return; }
        e.preventDefault();
        var l0 = el.offsetLeft;
        var t0 = el.offsetTop;
        drag(e, GNS + id + "-drag", function (dx, dy) {
          var x = Math.min(Math.max(0, l0 + dx), window.innerWidth - el.offsetWidth);
          var y = Math.min(Math.max(0, t0 + dy), window.innerHeight - el.offsetHeight);
          el.style.left = Math.max(0, x) + "px";
          el.style.top = Math.max(0, y) + "px";
          el.setAttribute("data-ah-placed", "");
          $el.trigger("ah:moving", [{ x: x, y: y }]);
        }, function () {
          $el.trigger("ah:moved", [{ x: el.offsetLeft, y: el.offsetTop }]);
        });
      });
      // eight resize handles
      $el.on("pointerdown" + NS, ".ah-window-resize-handle", function (e) {
        if (!$el.hasClass("ah-window-resizable") || e.button !== 0) { return; }
        e.preventDefault();
        e.stopPropagation();
        var dir = this.getAttribute("data-dir") || "";
        var w0 = el.offsetWidth;
        var h0 = el.offsetHeight;
        var l0 = el.offsetLeft;
        var t0 = el.offsetTop;
        drag(e, GNS + id + "-resize", function (dx, dy) {
          var w = w0;
          var h = h0;
          if (dir.indexOf("e") >= 0) { w = w0 + dx; }
          if (dir.indexOf("w") >= 0) { w = w0 - dx; }
          if (dir.indexOf("s") >= 0) { h = h0 + dy; }
          if (dir.indexOf("n") >= 0) { h = h0 - dy; }
          w = Math.max(MIN_W, w);
          h = Math.max(MIN_H, h);
          el.style.width = w + "px";
          el.style.height = h + "px";
          if (dir.indexOf("w") >= 0) { el.style.left = (l0 + w0 - w) + "px"; }
          if (dir.indexOf("n") >= 0) { el.style.top = (t0 + h0 - h) + "px"; }
          el.setAttribute("data-ah-placed", "");
        }, function () {
          $el.trigger("ah:resize", [{ width: el.offsetWidth, height: el.offsetHeight }]);
        });
      });
      // arrows move, Ctrl+arrows resize (only when the window itself or
      // its title bar has focus, so inputs keep their arrow keys)
      $el.on("keydown" + NS, function (e) {
        if (e.target !== el && !$(e.target).closest(".ah-window-header").length) { return; }
        var k = e.key;
        if (k !== "ArrowLeft" && k !== "ArrowRight" && k !== "ArrowUp" && k !== "ArrowDown") {
          return;
        }
        e.preventDefault();
        var dx = k === "ArrowLeft" ? -10 : k === "ArrowRight" ? 10 : 0;
        var dy = k === "ArrowUp" ? -10 : k === "ArrowDown" ? 10 : 0;
        if (e.ctrlKey) {
          windowResize(el, Math.max(MIN_W, el.offsetWidth + dx), Math.max(MIN_H, el.offsetHeight + dy));
        } else {
          windowMove(el, el.offsetLeft + dx, el.offsetTop + dy);
        }
      });
      if (el.getAttribute("data-ah-initial") === "open") { windowOpen(el); }
    },
    destroy: function (el) {
      var ns = $.data(el, "ahNs");
      $(document).off(ns + "-drag").off(ns + "-resize");
      var bd = $.data(el, "ahBackdrop");
      if (bd) {
        $(bd).remove();
        unlock();
      }
      pullStack(el, false);
    },
    methods: {
      open: function (el) { windowOpen(el); },
      close: function (el, $el, result) { windowClose(el, result); },
      toggle: function (el) { if (winOpen(el)) { windowClose(el); } else { windowOpen(el); } },
      collapse: function (el) { windowCollapse(el, true); },
      expand: function (el) { windowCollapse(el, false); },
      move: function (el, $el, x, y) { windowMove(el, x, y); },
      resize: function (el, $el, w, h) { windowResize(el, w, h); },
      bringToFront: function (el) { winFront(el); },
      isOpen: function (el) { return winOpen(el); }
    }
  });
})(window.jQuery, window.AH);
