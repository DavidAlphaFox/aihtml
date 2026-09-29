/* Behaviours of the layout_scroll components (designs/04-components.md).
 *
 * Ported from sigil's scrollview, scrollbar and responsive-panel (cljs +
 * jQuery). The scrollview and the standalone scrollbar keep their value in
 * data-ah-value on the root, mirror it into a hidden input when there is
 * one, and fire "change" on the root when the user changes it. Methods
 * called by the server (AH.invoke / aihtml_action:call) update the value
 * without firing "change".
 *
 * Drags use pointer capture, so nothing is bound on document except the
 * responsive panel's click-outside listener, which destroy removes.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  function setValue(el, $el, v) {
    el.setAttribute("data-ah-value", v);
    $el.children("input[type=hidden]").val(v);
  }

  function num(el, name, dflt) {
    var v = parseFloat(el.getAttribute(name));
    return isNaN(v) ? dflt : v;
  }

  function capture(node, e) {
    var id = e.originalEvent && e.originalEvent.pointerId;
    if (id !== undefined && node.setPointerCapture) {
      try { node.setPointerCapture(id); } catch (err) { /* synthetic event */ }
    }
  }

  // ------------------------------------------------------------------
  // ScrollView: a horizontal pager (sigil layout/scrollview)
  // ------------------------------------------------------------------
  //
  // Pages are 100% wide and the wrapper moves by margin-left in percent,
  // so the layout follows the container without measuring; drags move it
  // in pixels and the page change animates back to a percentage.

  var SV = "ah-scrollview";
  var SV_DEAD_ZONE = 15;

  function svState(el) {
    var st = $.data(el, "ahScrollview");
    if (!st) {
      st = { timer: null, anim: null, paused: false };
      $.data(el, "ahScrollview", st);
    }
    return st;
  }

  function svWrapper($el) { return $el.children("." + SV + "-wrapper"); }
  function svPages($el) { return svWrapper($el).children("." + SV + "-page"); }
  function svIndex(el) { return parseInt(el.getAttribute("data-ah-value"), 10) || 0; }
  function svDisabled($el) { return $el.hasClass(SV + "-disabled"); }

  function svDuration(el) { return num(el, "data-duration", 300); }

  // Turn the transition on for one move and off again when it ends.
  function svAnimate(el, $el) {
    var st = svState(el);
    $el.addClass(SV + "-animating");
    clearTimeout(st.anim);
    st.anim = setTimeout(function () {
      st.anim = null;
      $el.removeClass(SV + "-animating");
    }, svDuration(el) + 50);
  }

  function svPlace($el, idx) {
    svWrapper($el)[0].style.marginLeft = idx ? (-idx * 100) + "%" : "";
  }

  function svMark($el, idx) {
    svPages($el).each(function (i) {
      var on = i === idx;
      if (on) {
        this.removeAttribute("aria-hidden");
        this.removeAttribute("inert");
      } else {
        this.setAttribute("aria-hidden", "true");
        this.setAttribute("inert", "");
      }
    });
    $el.children("." + SV + "-buttons").children("." + SV + "-button").each(function (i) {
      var on = i === idx;
      $(this).toggleClass(SV + "-button-active", on);
      if (on) { this.setAttribute("aria-current", "true"); } else { this.removeAttribute("aria-current"); }
    });
  }

  // how: "user" fires change, "api" and "auto" only ah:page-changed.
  function svGo(el, $el, idx, how) {
    var cnt = svPages($el).length;
    if (!cnt) { return; }
    idx = Math.max(0, Math.min(cnt - 1, idx));
    var old = svIndex(el);
    svAnimate(el, $el);
    svPlace($el, idx);
    if (idx === old) { return; }
    svMark($el, idx);
    setValue(el, $el, String(idx));
    $el.trigger("ah:page-changed", [{ page: idx, old: old }]);
    if (how === "user") { $el.trigger("change"); }
  }

  function svStart(el, $el) {
    var st = svState(el);
    if (st.timer) { return; }
    st.timer = setInterval(function () {
      if (st.paused || svDisabled($el)) { return; }
      var cnt = svPages($el).length, cur = svIndex(el);
      svGo(el, $el, cur + 1 >= cnt ? 0 : cur + 1, "auto");
    }, num(el, "data-slide-duration", 3000));
  }

  function svStop(el) {
    var st = svState(el);
    clearInterval(st.timer);
    st.timer = null;
  }

  AH.define("scrollview", {
    init: function (el, $el) {
      var st = svState(el);
      var $w = svWrapper($el);
      var drag = null;
      svMark($el, svIndex(el));

      $w.on("pointerdown" + NS, function (e) {
        if (svDisabled($el) || (e.button !== undefined && e.button !== 0)) { return; }
        $el.removeClass(SV + "-animating");
        drag = { x: e.clientX, ml: parseFloat(getComputedStyle(this).marginLeft) || 0,
                 moving: false, node: this, e: e };
      });
      $w.on("pointermove" + NS, function (e) {
        if (!drag) { return; }
        var dx = e.clientX - drag.x;
        if (!drag.moving) {
          if (Math.abs(dx) <= SV_DEAD_ZONE) { return; }
          drag.moving = true;
          capture(drag.node, drag.e);
          $el.addClass(SV + "-dragging");
        }
        e.preventDefault();
        var cw = $el.width(), cnt = svPages($el).length;
        var ml = drag.ml + dx;
        if (el.getAttribute("data-bounce") === "false") {
          ml = Math.max(-(cnt - 1) * cw, Math.min(0, ml));
        }
        this.style.marginLeft = ml + "px";
      });
      $w.on("pointerup" + NS + " pointercancel" + NS, function () {
        if (!drag) { return; }
        var d = drag;
        drag = null;
        if (!d.moving) { return; }
        $el.removeClass(SV + "-dragging");
        // swallow the click that ends a drag, so links in a page stay put
        $w.one("click" + NS, function (c) { c.preventDefault(); c.stopPropagation(); });
        setTimeout(function () { $w.off("click" + NS); }, 0);
        var dx = (parseFloat(this.style.marginLeft) || 0) - d.ml;
        var cw = $el.width(), threshold = num(el, "data-threshold", 0.5) * cw;
        var cur = svIndex(el), target = cur;
        if (dx < -threshold) { target = cur + 1; } else if (dx > threshold) { target = cur - 1; }
        svGo(el, $el, target, "user");
      });
      $w.on("dragstart" + NS, function (e) { e.preventDefault(); });

      $el.on("click" + NS, "." + SV + "-button", function () {
        if ($(this).closest("." + SV)[0] !== el || svDisabled($el)) { return; }
        svGo(el, $el, $(this).index(), "user");
      });

      $el.on("keydown" + NS, function (e) {
        if (e.target !== el || svDisabled($el)) { return; }
        var cur = svIndex(el), cnt = svPages($el).length, to = null;
        switch (e.key) {
          case "ArrowLeft": case "ArrowUp": case "PageUp": to = cur - 1; break;
          case "ArrowRight": case "ArrowDown": case "PageDown": to = cur + 1; break;
          case "Home": to = 0; break;
          case "End": to = cnt - 1; break;
          default: return;
        }
        e.preventDefault();
        svGo(el, $el, to, "user");
      });

      // the slide show waits while the pointer or the focus is inside
      $el.on("mouseenter" + NS + " focusin" + NS, function () { st.paused = true; });
      $el.on("mouseleave" + NS + " focusout" + NS, function (e) {
        if (e.type === "focusout" && e.relatedTarget && $.contains(el, e.relatedTarget)) { return; }
        st.paused = false;
      });
      if (el.hasAttribute("data-slide-show")) { svStart(el, $el); }
    },
    destroy: function (el) {
      var st = svState(el);
      svStop(el);
      clearTimeout(st.anim);
      $.removeData(el, "ahScrollview");
    },
    methods: {
      setValue: function (el, $el, v) { svGo(el, $el, parseInt(v, 10) || 0, "api"); },
      getValue: function (el) { return svIndex(el); },
      forward: function (el, $el) {
        var cur = svIndex(el);
        if (cur + 1 < svPages($el).length) { svGo(el, $el, cur + 1, "api"); }
      },
      back: function (el, $el) {
        var cur = svIndex(el);
        if (cur > 0) { svGo(el, $el, cur - 1, "api"); }
      },
      startSlideShow: function (el, $el) { svStart(el, $el); },
      stopSlideShow: function (el) { svStop(el); },
      refresh: function (el, $el) {
        var cnt = svPages($el).length, cur = Math.min(svIndex(el), Math.max(0, cnt - 1));
        setValue(el, $el, String(cur));
        svPlace($el, cur);
        svMark($el, cur);
      }
    }
  });

  // ------------------------------------------------------------------
  // Scrollbar (sigil layout/scrollbar, and the bars of sigil's panel)
  // ------------------------------------------------------------------
  //
  // One bar is .ah-scrollbar with five parts: up button, track before the
  // thumb, thumb, track after it, down button. sbLayout sizes them from a
  // value in [min, max] exactly as sigil's arrange! does.

  var SB = "ah-scrollbar";
  var SB_BTN = 14;

  function sbParts(bar) {
    var $b = $(bar);
    return {
      up: $b.children("." + SB + "-btn-up")[0],
      tu: $b.children("." + SB + "-track-up")[0],
      thumb: $b.children("." + SB + "-thumb")[0],
      td: $b.children("." + SB + "-track-down")[0],
      down: $b.children("." + SB + "-btn-down")[0]
    };
  }

  function sbVertical(bar) { return $(bar).hasClass(SB + "-vertical"); }

  // Geometry of a bar for a value: track length, thumb size and position.
  function sbGeom(bar, o) {
    var vert = sbVertical(bar);
    var total = vert ? bar.clientHeight : bar.clientWidth;
    var track = Math.max(0, total - (o.buttons ? 2 * SB_BTN : 0));
    var range = o.max - o.min;
    var ts = range <= 0 ? track : Math.max(o.thumbMin, track * track / (track + range));
    ts = Math.min(ts, track);
    var tp = range <= 0 ? 0 : (o.value - o.min) / range * (track - ts);
    return { vert: vert, track: track, ts: ts, tp: tp };
  }

  function sbLayout(bar, o) {
    var g = sbGeom(bar, o), p = sbParts(bar), dim = g.vert ? "height" : "width";
    p.up.style.display = p.down.style.display = o.buttons ? "" : "none";
    p.tu.style[dim] = g.tp + "px";
    p.thumb.style[dim] = g.ts + "px";
    p.td.style[dim] = Math.max(0, g.track - g.ts - g.tp) + "px";
    return g;
  }

  function sbPosToValue(bar, o, pos) {
    var g = sbGeom(bar, o), free = g.track - g.ts;
    return free <= 0 ? o.min : o.min + Math.max(0, Math.min(free, pos)) / free * (o.max - o.min);
  }

  // Repeat fn while a button is held: once now, again after 300ms, then
  // every 50ms (sigil util/start-repeat-timer!).
  function sbRepeat(node, e, fn) {
    fn();
    var iv = null;
    var t = setTimeout(function () { iv = setInterval(fn, 50); }, 300);
    capture(node, e);
    $(node).on("pointerup.ahrep pointercancel.ahrep lostpointercapture.ahrep", function () {
      clearTimeout(t);
      clearInterval(iv);
      $(node).off(".ahrep");
    });
  }

  // Wire one bar. api: {opts(), set(value, phase), start()} where phase is
  // "input" while dragging, "change" otherwise and "end" when a drag ends.
  function sbBind(bar, api) {
    var p = sbParts(bar);
    $(p.up).add(p.down).on("pointerdown" + NS, function (e) {
      if (e.button !== undefined && e.button !== 0) { return; }
      e.preventDefault();
      var dir = this === p.up ? -1 : 1;
      sbRepeat(this, e, function () { var o = api.opts(); api.set(o.value + dir * o.step, "change"); });
    });
    $(p.tu).add(p.td).on("pointerdown" + NS, function (e) {
      if (e.button !== undefined && e.button !== 0) { return; }
      e.preventDefault();
      var o = api.opts();
      api.set(o.value + (this === p.tu ? -1 : 1) * o.large, "change");
    });
    var drag = null;
    $(p.thumb).on("pointerdown" + NS, function (e) {
      if (e.button !== undefined && e.button !== 0) { return; }
      e.preventDefault();
      var o = api.opts(), g = sbGeom(bar, o);
      drag = { start: g.vert ? e.clientY : e.clientX, pos: g.tp, vert: g.vert, value: o.value };
      capture(this, e);
      $(this).addClass(SB + "-thumb-pressed");
      if (api.start) { api.start(); }
    });
    $(p.thumb).on("pointermove" + NS, function (e) {
      if (!drag) { return; }
      var cur = drag.vert ? e.clientY : e.clientX;
      api.set(sbPosToValue(bar, api.opts(), drag.pos + cur - drag.start), "input");
    });
    $(p.thumb).on("pointerup" + NS + " pointercancel" + NS, function () {
      if (!drag) { return; }
      var before = drag.value;
      drag = null;
      $(this).removeClass(SB + "-thumb-pressed");
      api.set(api.opts().value, "end", before);
    });
  }

  function sbObserve(el, targets, fn) {
    var st = $.data(el, "ahScrollbar");
    if (window.ResizeObserver) {
      st.ro = new ResizeObserver(function () { fn(); });
      targets.forEach(function (t) { if (t) { st.ro.observe(t); } });
    } else {
      st.ns = ".ahsb" + (++seq);
      $(window).on("resize" + st.ns, fn);
    }
  }

  // --- standalone bar ---------------------------------------------------

  function sbOpts(el) {
    return {
      min: num(el, "data-min", 0), max: num(el, "data-max", 1000),
      value: num(el, "data-ah-value", 0),
      step: num(el, "data-step", 10), large: num(el, "data-large-step", 50),
      thumbMin: num(el, "data-thumb-min", 10),
      buttons: el.getAttribute("data-buttons") !== "false"
    };
  }

  function sbIntegral(o) {
    return o.min % 1 === 0 && o.max % 1 === 0 && o.step % 1 === 0 && o.large % 1 === 0;
  }

  function sbBar($el) { return $el.children("." + SB)[0]; }

  function sbSet(el, $el, v, event) {
    var o = sbOpts(el);
    v = Math.max(o.min, Math.min(o.max, +v || 0));
    if (sbIntegral(o)) { v = Math.round(v); }
    var changed = v !== o.value;
    if (changed) {
      setValue(el, $el, String(v));
      el.setAttribute("aria-valuenow", String(v));
    }
    o.value = v;
    sbLayout(sbBar($el), o);
    if (changed && event) { $el.trigger(event); }
    return changed;
  }

  function sbStandalone(el, $el) {
    var bar = sbBar($el);
    var disabled = function () { return $el.hasClass(SB + "-disabled"); };
    sbBind(bar, {
      opts: function () { return sbOpts(el); },
      set: function (v, phase, before) {
        if (disabled()) { return; }
        if (phase === "end") {
          if (String(before) !== el.getAttribute("data-ah-value")) { $el.trigger("change"); }
          return;
        }
        sbSet(el, $el, v, phase);
      },
      start: function () { el.focus({ preventScroll: true }); }
    });
    $el.on("keydown" + NS, function (e) {
      if (e.target !== el || disabled()) { return; }
      var o = sbOpts(el), vert = sbVertical(bar), to;
      switch (e.key) {
        case "ArrowLeft": if (vert) { return; } to = o.value - o.step; break;
        case "ArrowRight": if (vert) { return; } to = o.value + o.step; break;
        case "ArrowUp": if (!vert) { return; } to = o.value - o.step; break;
        case "ArrowDown": if (!vert) { return; } to = o.value + o.step; break;
        case "PageUp": to = o.value - o.large; break;
        case "PageDown": to = o.value + o.large; break;
        case "Home": to = o.min; break;
        case "End": to = o.max; break;
        default: return;
      }
      e.preventDefault();
      sbSet(el, $el, to, "change");
    });
    var relayout = function () { sbLayout(bar, sbOpts(el)); };
    sbObserve(el, [el], relayout);
    relayout();
  }

  // --- scroll area --------------------------------------------------------

  function saParts($el) {
    return {
      vp: $el.children("." + SB + "-viewport")[0],
      v: $el.children("." + SB + "-vertical")[0],
      h: $el.children("." + SB + "-horizontal")[0]
    };
  }

  function saOpts(el, vp, vert) {
    var max = vert ? vp.scrollHeight - vp.clientHeight : vp.scrollWidth - vp.clientWidth;
    var page = vert ? vp.clientHeight : vp.clientWidth;
    return {
      min: 0, max: Math.max(0, max),
      value: vert ? vp.scrollTop : vp.scrollLeft,
      step: num(el, "data-step", 10) * 3, large: Math.max(10, page * 0.9),
      thumbMin: num(el, "data-thumb-min", 10),
      buttons: el.getAttribute("data-buttons") !== "false"
    };
  }

  // Which bars the content needs: showing one narrows the viewport, which
  // may make the other one necessary, so settle it in two passes.
  function saLayout(el, $el) {
    var p = saParts($el), vp = p.vp;
    var needV = false, needH = false;
    for (var i = 0; i < 2; i++) {
      $el.toggleClass(SB + "-area-v", needV).toggleClass(SB + "-area-h", needH);
      needV = vp.scrollHeight > vp.clientHeight + 1;
      needH = vp.scrollWidth > vp.clientWidth + 1;
    }
    $el.toggleClass(SB + "-area-v", needV).toggleClass(SB + "-area-h", needH);
    if (needV) { sbLayout(p.v, saOpts(el, vp, true)); }
    if (needH) { sbLayout(p.h, saOpts(el, vp, false)); }
  }

  function saSync(el, $el) {
    var p = saParts($el);
    if ($el.hasClass(SB + "-area-v")) { sbLayout(p.v, saOpts(el, p.vp, true)); }
    if ($el.hasClass(SB + "-area-h")) { sbLayout(p.h, saOpts(el, p.vp, false)); }
  }

  function saArea(el, $el) {
    var p = saParts($el);
    [[p.v, true], [p.h, false]].forEach(function (b) {
      var vert = b[1];
      sbBind(b[0], {
        opts: function () { return saOpts(el, p.vp, vert); },
        set: function (v, phase) {
          if (phase === "end" || $el.hasClass(SB + "-disabled")) { return; }
          if (vert) { p.vp.scrollTop = v; } else { p.vp.scrollLeft = v; }
          saSync(el, $el);
        }
      });
    });
    $(p.vp).on("scroll" + NS, function () { saSync(el, $el); });
    var content = $(p.vp).children("." + SB + "-content")[0];
    sbObserve(el, [p.vp, content], function () { saLayout(el, $el); });
    saLayout(el, $el);
  }

  AH.define("scrollbar", {
    init: function (el, $el) {
      $.data(el, "ahScrollbar", {});
      if (el.hasAttribute("data-area")) { saArea(el, $el); } else { sbStandalone(el, $el); }
    },
    destroy: function (el) {
      var st = $.data(el, "ahScrollbar") || {};
      if (st.ro) { st.ro.disconnect(); }
      if (st.ns) { $(window).off(st.ns); }
      $.removeData(el, "ahScrollbar");
    },
    methods: {
      setValue: function (el, $el, v) {
        if (!el.hasAttribute("data-area")) { sbSet(el, $el, v, null); }
      },
      getValue: function (el) { return num(el, "data-ah-value", 0); },
      setMax: function (el, $el, max) {
        el.setAttribute("data-max", String(max));
        el.setAttribute("aria-valuemax", String(max));
        sbSet(el, $el, num(el, "data-ah-value", 0), null);
        sbLayout(sbBar($el), sbOpts(el));
      },
      scrollTo: function (el, $el, x, y) {
        var vp = saParts($el).vp;
        if (vp) {
          vp.scrollLeft = x || 0;
          vp.scrollTop = y || 0;
        }
      },
      refresh: function (el, $el) {
        if (el.hasAttribute("data-area")) { saLayout(el, $el); } else { sbLayout(sbBar($el), sbOpts(el)); }
      }
    }
  });

  // ------------------------------------------------------------------
  // Responsive panel (sigil layout/responsive_panel)
  // ------------------------------------------------------------------
  //
  // Folded when the parent is at most data-breakpoint px wide: the content
  // then floats below the toggle (AH.float) while open.

  var RP = "ah-responsive-panel";

  function rpState(el) { return $.data(el, "ahRpanel"); }
  function rpToggle($el) { return $el.children("." + RP + "-toggle"); }
  function rpContent($el) { return $el.children("." + RP + "-content"); }
  function rpDisabled($el) { return $el.hasClass(RP + "-disabled"); }

  function rpLoad($el) {
    var st = rpState($el[0]), $c = rpContent($el);
    if (st.loaded) { return; }
    st.loaded = true;
    if (/(^|\s)ah:load:/.test($c.attr("data-ah-on") || "")) { $c.trigger("ah:load"); }
  }

  function rpSpeed(el, name) { return num(el, name, 200); }

  function rpClearStyles($c) {
    $c.stop(true, true).css({ display: "", opacity: "", width: "" });
  }

  function rpOpen(el, $el) {
    var st = rpState(el);
    if (!st.collapsed || st.open || rpDisabled($el)) { return; }
    var $c = rpContent($el), $t = rpToggle($el);
    var anim = el.getAttribute("data-animation") || "fade", speed = rpSpeed(el, "data-show-duration");
    var cw = el.getAttribute("data-collapse-width");
    rpClearStyles($c);
    if (cw) { $c.css("width", /^\d+(\.\d+)?$/.test(cw) ? cw + "px" : cw); }
    st.open = true;
    $el.addClass(RP + "-open");
    $t.attr("aria-expanded", "true");
    st.float = AH.float($c[0], $t[0], { placement: "bottom", align: "start", offset: 4 });
    var shown = function () {
      if (st.float) { st.float.update(); }
      $el.trigger("ah:open");
    };
    if (anim === "fade") {
      $c.css("opacity", 0).animate({ opacity: 1 }, speed, shown);
    } else if (anim === "slide") {
      $c.hide().slideDown(speed, shown);
    } else {
      shown();
    }
    rpLoad($el);
  }

  function rpClose(el, $el, instant) {
    var st = rpState(el);
    if (!st.open) { return; }
    var $c = rpContent($el), $t = rpToggle($el);
    var anim = instant ? "none" : (el.getAttribute("data-animation") || "fade");
    var speed = rpSpeed(el, "data-hide-duration");
    st.open = false;
    $t.attr("aria-expanded", "false");
    if ($.contains($c[0], document.activeElement)) { $t[0].focus(); }
    var hidden = function () {
      $el.removeClass(RP + "-open");
      if (st.float) { st.float.stop(); st.float = null; }
      $c.css({ display: "", opacity: "" });
      if (!instant) { $el.trigger("ah:close"); }
    };
    $c.stop(true, true);
    if (anim === "fade") { $c.fadeOut(speed, hidden); }
    else if (anim === "slide") { $c.slideUp(speed, hidden); }
    else { hidden(); }
  }

  function rpCheck(el, $el) {
    var st = rpState(el);
    var bp = num(el, "data-breakpoint", 1000);
    var pw = $el.parent().width();
    if (!st.collapsed && pw <= bp) {
      if (st.open) { rpClose(el, $el, true); }
      st.collapsed = true;
      $el.addClass(RP + "-collapsed");
      $el.trigger("ah:collapse");
    } else if (st.collapsed && pw > bp) {
      rpClose(el, $el, true);
      st.collapsed = false;
      $el.removeClass(RP + "-collapsed " + RP + "-open");
      rpClearStyles(rpContent($el));
      $el.trigger("ah:expand");
      rpLoad($el);
    }
  }

  function rpFlip(el, $el) {
    if (rpState(el).open) { rpClose(el, $el); } else { rpOpen(el, $el); }
  }

  AH.define("responsive-panel", {
    init: function (el, $el) {
      var st = { collapsed: false, open: false, loaded: false, float: null,
                 ns: ".ahrp" + (++seq), ro: null, $ext: $() };
      $.data(el, "ahRpanel", st);
      var $t = rpToggle($el);
      $t.on("click" + NS, function () {
        if (!rpDisabled($el)) { rpFlip(el, $el); }
      });
      $t.on("keydown" + NS, function (e) {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          if (!rpDisabled($el)) { rpFlip(el, $el); }
        }
      });
      $el.on("keydown" + NS, function (e) {
        if (e.key === "Escape" && st.open) {
          e.stopPropagation();
          rpClose(el, $el);
          $t[0].focus();
        }
      });
      var sel = el.getAttribute("data-toggle-button");
      if (sel) {
        st.$ext = $(sel).on("click" + st.ns, function () {
          if (!rpDisabled($el)) { rpFlip(el, $el); }
        });
      }
      $(document).on("click" + st.ns, function (e) {
        if (st.open && el.getAttribute("data-auto-close") !== "false" &&
            !$.contains(el, e.target) && e.target !== el &&
            !st.$ext.filter(function () { return this === e.target || $.contains(this, e.target); }).length) {
          rpClose(el, $el);
        }
      });
      var check = function () { rpCheck(el, $el); };
      if (window.ResizeObserver && el.parentNode) {
        st.ro = new ResizeObserver(check);
        st.ro.observe(el.parentNode);
      }
      $(window).on("resize" + st.ns, check);
      check();
      if (!st.collapsed) { rpLoad($el); }
    },
    destroy: function (el, $el) {
      var st = rpState(el);
      if (!st) { return; }
      if (st.float) { st.float.stop(); }
      if (st.ro) { st.ro.disconnect(); }
      rpContent($el).stop(true, true);
      $(document).off(st.ns);
      $(window).off(st.ns);
      st.$ext.off(st.ns);
      $.removeData(el, "ahRpanel");
    },
    methods: {
      open: function (el, $el) { rpOpen(el, $el); },
      close: function (el, $el) { rpClose(el, $el); },
      toggle: function (el, $el) { if (rpState(el).collapsed) { rpFlip(el, $el); } },
      refresh: function (el, $el) { rpCheck(el, $el); },
      isCollapsed: function (el) { return !!rpState(el).collapsed; },
      isOpen: function (el) { return !!rpState(el).open; }
    }
  });
})(window.jQuery, window.AH);
