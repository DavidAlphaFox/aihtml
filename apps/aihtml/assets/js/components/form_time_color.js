/* Behaviours of the form_time_color components (designs/04-components.md).
 * Ported from sigil: form/timepicker (+ timepicker/math, timepicker/svg)
 * and form/colorpicker (+ colorpicker/color, events, render).
 *
 * Both are value-bearing: data-ah-value and the hidden input follow the
 * value, the root fires `change` on commit (the colour picker also fires
 * `input` while dragging). Native input/change events of the inner text
 * fields are stopped at the root so they are not taken for the
 * component's own events. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  function uid(prefix) {
    seq += 1;
    return prefix + seq + "-" + Math.random().toString(36).slice(2, 7);
  }

  function clamp(v, lo, hi) {
    return Math.max(lo, Math.min(hi, v));
  }

  function disabled(el) {
    return el.getAttribute("aria-disabled") === "true";
  }

  // Keep the inner fields' native events inside the component.
  function fenceNativeEvents($el) {
    $el.on("input" + NS + " change" + NS, "input", function (e) {
      e.stopPropagation();
    });
  }

  // ------------------------------------------------------------------
  // Popup shared by both pickers: a field toggles a panel placed by
  // AH.float (below, above when there is no room), closed by Escape or a
  // press outside.
  // ------------------------------------------------------------------

  function popupOf($el) {
    return $el.children(".ah-timepicker-popup, .ah-colorpicker-popup").first();
  }

  function isOpen($el) {
    var $p = popupOf($el);
    return $p.length > 0 && !$p.prop("hidden");
  }

  function openPopup(el, $el, cls, $opener, onOpen) {
    var $p = popupOf($el);
    if (!$p.length || !$p.prop("hidden") || disabled(el)) { return; }
    if (onOpen) { onOpen(); }
    $p.prop("hidden", false);
    $el.addClass(cls + "-open");
    $opener.attr("aria-expanded", "true");
    // Fixed positioning at the field (the input area or the trigger), so
    // an overflow:hidden ancestor such as a card does not clip it; flips
    // above when there is no room below and follows scrolling.
    $.data(el, "ah-float", AH.float($p[0], $el.children().first()[0],
                                    { placement: "bottom", align: "start", offset: 4 }));
    var ns = ".ahpop" + el.getAttribute("data-ah-uid");
    $(document).off(ns).on("mousedown" + ns + " touchstart" + ns + " focusin" + ns, function (e) {
      if (!$.contains(el, e.target) && e.target !== el) {
        closePopup(el, $el, cls, $opener, false);
      }
    });
  }

  function closePopup(el, $el, cls, $opener, refocus) {
    var $p = popupOf($el);
    stopFloat(el);
    if (!$p.length || $p.prop("hidden")) { return; }
    $p.prop("hidden", true);
    $el.removeClass(cls + "-open");
    $opener.attr("aria-expanded", "false");
    if (refocus) { $opener.trigger("focus"); }
  }

  function stopFloat(el) {
    $(document).off(".ahpop" + el.getAttribute("data-ah-uid"));
    var h = $.data(el, "ah-float");
    if (h) { h.stop(); $.removeData(el, "ah-float"); }
  }

  function pointerXY(e) {
    var oe = e.originalEvent || e;
    var t = oe.touches && oe.touches[0] ? oe.touches[0] : oe;
    return { x: t.clientX, y: t.clientY };
  }

  // Pointer drag on one element: start(e), move(e), end(e). Uses pointer
  // capture, so nothing is bound on document.
  function drag($target, el, start, move, end) {
    $target.on("pointerdown" + NS, function (e) {
      if (disabled(el) || (e.button !== undefined && e.button !== 0)) { return; }
      if (start(e) === false) { return; }
      e.preventDefault();
      var node = this;
      var id = e.originalEvent && e.originalEvent.pointerId;
      if (id !== undefined && node.setPointerCapture) {
        try { node.setPointerCapture(id); } catch (err) { /* synthetic event */ }
      }
      node.focus && node.focus({ preventScroll: true });
      var $n = $(node);
      $n.on("pointermove" + NS + "drag", function (m) { move(m); });
      $n.on("pointerup" + NS + "drag pointercancel" + NS + "drag", function (u) {
        $n.off(NS + "drag");
        end(u);
      });
    });
  }

  // ------------------------------------------------------------------
  // timepicker (sigil timepicker, timepicker/math, timepicker/svg)
  // ------------------------------------------------------------------

  var TWO_PI = 2 * Math.PI;
  var CX = 130, CY = 130, OUTER_R = 105, INNER_R = 70;
  var TP = "ah-timepicker";

  function pad2(n) { return n < 10 ? "0" + n : String(n); }

  function to12(h) {
    if (h === 0) { return { h12: 12, period: "am" }; }
    if (h < 12) { return { h12: h, period: "am" }; }
    if (h === 12) { return { h12: 12, period: "pm" }; }
    return { h12: h - 12, period: "pm" };
  }

  function to24(h12, period) {
    if (period === "am") { return h12 === 12 ? 0 : h12; }
    return h12 === 12 ? 12 : h12 + 12;
  }

  function angleXY(angle, r) {
    return { x: CX + r * Math.sin(angle), y: CY - r * Math.cos(angle) };
  }

  function round2(n) { return Math.round(n * 100) / 100; }

  // "14:30", "14:30:00", "2:30 pm", "2 pm", "1430" -> minutes of the day
  function parseTime(s) {
    var m = /^\s*(\d{1,2})(?::?(\d{2}))?(?::\d{2})?\s*([ap])?\.?m?\.?\s*$/i.exec(s || "");
    if (!m) { return null; }
    var h = parseInt(m[1], 10);
    var min = m[2] ? parseInt(m[2], 10) : 0;
    if (min > 59) { return null; }
    if (m[3]) {
      if (h < 1 || h > 12) { return null; }
      h = to24(h, m[3].toLowerCase() === "a" ? "am" : "pm");
    } else if (h > 23) {
      return null;
    }
    return h * 60 + min;
  }

  function tpState(el) { return $.data(el, "ah-tp"); }

  function tpAllowed(st, t) { return t >= st.lo && t <= st.hi; }

  function tpHourAllowed(st, h) {
    for (var m = 0; m < 60; m += st.step) {
      if (tpAllowed(st, h * 60 + m)) { return true; }
    }
    return false;
  }

  function tpClampState(st) {
    var t = clamp(st.h * 60 + st.m, st.lo, st.hi);
    st.h = Math.floor(t / 60);
    st.m = t % 60;
  }

  function tpDisplay(st, value) {
    if (value === "") { return ""; }
    var t = parseTime(value);
    var h = Math.floor(t / 60), m = t % 60;
    if (st.format === "24h") { return pad2(h) + ":" + pad2(m); }
    var p = to12(h);
    return p.h12 + ":" + pad2(m) + " " + p.period.toUpperCase();
  }

  // View data for templates/timepicker_header.mustache, as the server
  // builds it in aihtml_form_time_color:time_header/5.
  function tpHeaderView(st, isDisabled) {
    var p = to12(st.h);
    return {
      hours: st.format === "24h" ? pad2(st.h) : String(p.h12),
      minutes: pad2(st.m),
      hours_active: st.mode === "hours",
      minutes_active: st.mode === "minutes",
      twelve: st.format === "12h",
      am: p.period === "am",
      pm: p.period === "pm",
      disabled: isDisabled,
      tabindex: isDisabled ? -1 : 0
    };
  }

  // Redraw header and clock from the state (sigil sync-header!, sync-clock!).
  function tpRender(el) {
    var st = tpState(el);
    var $el = $(el);
    var $header = $el.find("." + TP + "-header");
    var focusedAction = $header.find(":focus").attr("data-action");
    // Same markup as the server's first render: templates/timepicker_header.mustache
    $header.html(AH.tpl.timepicker_header(tpHeaderView(st, disabled(el))));
    if (focusedAction) { $header.find("[data-action='" + focusedAction + "']").trigger("focus"); }

    var svg = $el.find("." + TP + "-svg")[0];
    if (!svg) { return; }
    var g = svg.querySelector("." + TP + "-numbers");
    var p = to12(st.h);
    var angle, r, selected, items = [];
    if (st.mode === "hours") {
      var i;
      for (i = 1; i <= 12; i++) {
        items.push({ label: String(i), val: i, r: OUTER_R, inner: false,
                     ok: tpHourAllowed(st, st.format === "24h" ? i : to24(i, p.period)) });
      }
      if (st.format === "24h") {
        [0, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23].forEach(function (v) {
          items.push({ label: pad2(v), val: v, r: INNER_R, inner: true, ok: tpHourAllowed(st, v) });
        });
      }
      var disp = st.format === "24h" ? st.h % 12 : p.h12 % 12;
      angle = disp / 12 * TWO_PI;
      r = (st.format === "24h" && (st.h === 0 || st.h >= 13)) ? INNER_R : OUTER_R;
      selected = st.format === "24h" ? st.h : p.h12;
      items.forEach(function (it) { it.angle = (it.val % 12) / 12 * TWO_PI; });
    } else {
      for (var m = 0; m < 60; m += Math.max(st.step, 5)) {
        items.push({ label: pad2(m), val: m, r: OUTER_R, inner: false, angle: m / 60 * TWO_PI,
                     ok: tpAllowed(st, st.h * 60 + m) });
      }
      angle = st.m / 60 * TWO_PI;
      r = OUTER_R;
      selected = st.m;
    }
    // Same markup as the server's first render: templates/timepicker_numbers.mustache
    // (innerHTML on an SVG element parses its children as SVG).
    g.innerHTML = AH.tpl.timepicker_numbers({ numbers: items.map(function (it) {
      var xy = angleXY(it.angle, it.r);
      return { label: it.label, val: it.val, x: String(round2(xy.x)), y: String(round2(xy.y)),
               inner: it.inner, selected: it.val === selected, disabled: !it.ok };
    }) });
    var end = angleXY(angle, r);
    var hand = svg.querySelector("." + TP + "-hand");
    hand.setAttribute("x2", round2(end.x));
    hand.setAttribute("y2", round2(end.y));
    var sel = svg.querySelector("." + TP + "-selection");
    sel.setAttribute("cx", round2(end.x));
    sel.setAttribute("cy", round2(end.y));
    var hoursMode = st.mode === "hours";
    svg.setAttribute("aria-label", hoursMode ? "Hours" : "Minutes");
    svg.setAttribute("aria-valuemax", hoursMode ? "23" : "59");
    svg.setAttribute("aria-valuenow", hoursMode ? st.h : st.m);
    svg.setAttribute("aria-valuetext", hoursMode ? String(selected) : pad2(st.m));
  }

  // Commit a value ("HH:MM" or ""): data-ah-value, hidden input, field
  // text, clear button; `change` when it differs from the current one.
  function tpCommit(el, value, silent) {
    var st = tpState(el);
    var $el = $(el);
    var old = el.getAttribute("data-ah-value") || "";
    el.setAttribute("data-ah-value", value);
    $el.children("input[type=hidden]").val(value);
    $el.find("." + TP + "-input").val(tpDisplay(st, value));
    $el.find("." + TP + "-clear").prop("hidden", value === "");
    if (!silent && value !== old) {
      $el.trigger("change", [{ value: value }]);
    }
  }

  function tpCurrent(st) { return pad2(st.h) + ":" + pad2(st.m); }

  // Load the committed value into the state, or now (rounded to the step)
  // when empty, like sigil's default.
  function tpLoad(el) {
    var st = tpState(el);
    var t = parseTime(el.getAttribute("data-ah-value"));
    if (t === null) {
      var now = new Date();
      t = now.getHours() * 60 + Math.round(now.getMinutes() / st.step) * st.step;
      t = t % (24 * 60);
    }
    st.h = Math.floor(t / 60);
    st.m = t % 60;
    tpClampState(st);
  }

  function tpFromPolar(el, e) {
    var st = tpState(el);
    var svg = $(el).find("." + TP + "-svg")[0];
    var rect = svg.getBoundingClientRect();
    var pt = pointerXY(e);
    var dx = (pt.x - rect.left) / rect.width * 260 - CX;
    var dy = (pt.y - rect.top) / rect.height * 260 - CY;
    var angle = Math.atan2(dx, -dy);
    if (angle < 0) { angle += TWO_PI; }
    return { angle: angle, radius: Math.sqrt(dx * dx + dy * dy) };
  }

  // sigil compute-from-polar with snap-hour / snap-minute.
  function tpApplyPolar(el, polar) {
    var st = tpState(el);
    if (st.mode === "hours") {
      var idx = Math.round(polar.angle / (TWO_PI / 12)) % 12;
      var h;
      if (st.format === "24h") {
        h = polar.radius < 87.5 ? (idx === 0 ? 0 : idx + 12) : (idx === 0 ? 12 : idx);
      } else {
        h = to24(idx === 0 ? 12 : idx, to12(st.h).period);
      }
      if (!tpHourAllowed(st, h)) { return false; }
      st.h = h;
      tpClampState(st);
    } else {
      var raw = Math.round(polar.angle / (TWO_PI / 60)) % 60;
      var m = (Math.round(raw / st.step) * st.step) % 60;
      if (!tpAllowed(st, st.h * 60 + m)) { return false; }
      st.m = m;
    }
    return true;
  }

  // Arrow keys: next allowed hour / minute step in direction dir.
  function tpStep(st, dir) {
    var i, t;
    if (st.mode === "hours") {
      var h = st.h;
      for (i = 0; i < 24; i++) {
        h = (h + dir + 24) % 24;
        if (tpHourAllowed(st, h)) { st.h = h; tpClampState(st); return true; }
      }
    } else {
      var m = st.m - (st.m % st.step);
      if (dir < 0 && m !== st.m) { m += st.step; }
      for (i = 0; i < 60; i++) {
        m = (m + dir * st.step + 60) % 60;
        t = st.h * 60 + m;
        if (tpAllowed(st, t)) { st.m = m; return true; }
      }
    }
    return false;
  }

  function tpSetMode(el, mode) {
    tpState(el).mode = mode;
    tpRender(el);
  }

  function tpSetPeriod(el, period) {
    var st = tpState(el);
    var h = to24(to12(st.h).h12, period);
    var t = clamp(h * 60 + st.m, st.lo, st.hi);
    st.h = Math.floor(t / 60);
    st.m = t % 60;
    tpRender(el);
    tpCommit(el, tpCurrent(st));
  }

  AH.define("timepicker", {
    init: function (el, $el) {
      el.setAttribute("data-ah-uid", ++seq);
      var lo = parseTime(el.getAttribute("data-min"));
      var hi = parseTime(el.getAttribute("data-max"));
      var st = {
        mode: "hours",
        format: el.getAttribute("data-format") === "24h" ? "24h" : "12h",
        step: parseInt(el.getAttribute("data-step"), 10) || 5,
        auto: el.getAttribute("data-auto-switch") !== "false",
        lo: lo === null ? 0 : lo,
        hi: hi === null ? 24 * 60 - 1 : hi,
        h: 12, m: 0
      };
      $.data(el, "ah-tp", st);
      tpLoad(el);
      var $input = $el.find("." + TP + "-input");
      var $popup = popupOf($el);
      var $svg = $el.find("." + TP + "-svg");
      var popup = $popup.length > 0;
      if (popup) {
        $popup.attr("id", $popup.attr("id") || uid("ah-tp-popup-"));
        $input.attr("aria-controls", $popup.attr("id"));
      }
      fenceNativeEvents($el);

      function open(focusClock) {
        openPopup(el, $el, TP, $input, function () {
          st.mode = "hours";
          tpLoad(el);
          tpRender(el);
        });
        if (focusClock) { $svg.trigger("focus"); }
      }
      function close(refocus) { closePopup(el, $el, TP, $input, refocus); }
      $.data(el, "ah-tp-open", open);
      $.data(el, "ah-tp-close", close);

      // Field: click toggles; typing a time commits on change.
      $el.on("mousedown" + NS, "." + TP + "-input-area", function (e) {
        if ($(e.target).closest("." + TP + "-clear").length) { return; }
        if (isOpen($el)) {
          if (!$(e.target).is($input)) { e.preventDefault(); close(true); }
        } else {
          open(false);
        }
      });
      $input.on("keydown" + NS, function (e) {
        if (e.key === "ArrowDown" || (e.key === " " && !$input.val())) {
          e.preventDefault();
          open(true);
        } else if (e.key === "Enter" && !isOpen($el)) {
          e.preventDefault();
          $input.trigger("change");
        }
      });
      $input.on("change" + NS, function () {
        var text = $input.val();
        if (String(text).trim() === "") { tpCommit(el, ""); return; }
        var t = parseTime(text);
        if (t === null) {
          $input.val(tpDisplay(st, el.getAttribute("data-ah-value") || ""));
          return;
        }
        t = clamp(t, st.lo, st.hi);
        st.h = Math.floor(t / 60);
        st.m = t % 60;
        tpRender(el);
        tpCommit(el, tpCurrent(st));
      });
      $el.on("click" + NS, "." + TP + "-clear", function (e) {
        e.preventDefault();
        tpCommit(el, "");
        close(false);
        $input.trigger("focus");
      });
      $el.on("keydown" + NS, function (e) {
        if (e.key === "Escape" && isOpen($el)) {
          e.preventDefault();
          e.stopPropagation();
          close(true);
        }
      });

      // Header: hours / minutes / AM / PM (sigil setup-header-clicks!).
      function headerAction(action) {
        if (disabled(el)) { return; }
        if (action === "select-hours") { tpSetMode(el, "hours"); }
        else if (action === "select-minutes") { tpSetMode(el, "minutes"); }
        else if (action === "set-am") { tpSetPeriod(el, "am"); }
        else if (action === "set-pm") { tpSetPeriod(el, "pm"); }
      }
      $el.on("click" + NS, "." + TP + "-header [data-action]", function () {
        headerAction(this.getAttribute("data-action"));
      });
      $el.on("keydown" + NS, "." + TP + "-header [data-action]", function (e) {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          headerAction(this.getAttribute("data-action"));
        }
      });

      // Clock: drag (sigil setup-clock-drag!) and keyboard.
      var dragMode = null;
      drag($svg, el, function (e) {
        var polar = tpFromPolar(el, e);
        if (polar.radius <= 20) { return false; }
        dragMode = st.mode;
        if (tpApplyPolar(el, polar)) { tpRender(el); }
      }, function (e) {
        if (tpApplyPolar(el, tpFromPolar(el, e))) { tpRender(el); }
      }, function () {
        tpCommit(el, tpCurrent(st));
        if (dragMode === "hours" && st.auto) {
          tpSetMode(el, "minutes");
        } else if (dragMode === "minutes" && popup) {
          close(true);
        }
        dragMode = null;
      });
      $svg.on("keydown" + NS, function (e) {
        if (disabled(el)) { return; }
        var dir = { ArrowUp: 1, ArrowRight: 1, ArrowDown: -1, ArrowLeft: -1 }[e.key];
        if (dir) {
          e.preventDefault();
          if (tpStep(st, dir)) {
            tpRender(el);
            tpCommit(el, tpCurrent(st));
          }
        } else if (e.key === "Home" || e.key === "End") {
          e.preventDefault();
          var t = e.key === "Home" ? st.lo : st.hi;
          if (st.mode === "hours") {
            st.h = Math.floor(t / 60);
            tpClampState(st);
          } else {
            var base = st.h * 60;
            var m = e.key === "Home" ? 0 : 60 - st.step;
            while (!tpAllowed(st, base + m) && m >= 0 && m < 60) { m += e.key === "Home" ? st.step : -st.step; }
            if (m >= 0 && m < 60) { st.m = m; }
          }
          tpRender(el);
          tpCommit(el, tpCurrent(st));
        } else if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          tpCommit(el, tpCurrent(st));
          if (st.mode === "hours") {
            tpSetMode(el, "minutes");
          } else if (popup) {
            close(true);
          }
        }
      });
      if (!popup) { tpRender(el); }
    },
    destroy: function (el) { stopFloat(el); },
    methods: {
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      // Set the value without firing change (server driven).
      setValue: function (el, $el, v) {
        var t = parseTime(v == null ? "" : String(v));
        var st = tpState(el);
        if (t === null) { tpCommit(el, "", true); return; }
        st.h = Math.floor(t / 60);
        st.m = t % 60;
        tpRender(el);
        tpCommit(el, tpCurrent(st), true);
      },
      clear: function (el) { tpCommit(el, ""); },
      open: function (el) { $.data(el, "ah-tp-open")(true); },
      close: function (el) { $.data(el, "ah-tp-close")(false); },
      setMode: function (el, $el, mode) { tpSetMode(el, mode === "minutes" ? "minutes" : "hours"); }
    }
  });

  // ------------------------------------------------------------------
  // colorpicker (sigil colorpicker, colorpicker/color, events, render)
  // ------------------------------------------------------------------

  var CP = "ah-colorpicker";

  function hsvToRgb(h, s, v) {
    s /= 100; v /= 100;
    var c = v * s;
    var x = c * (1 - Math.abs(((h / 60) % 2) - 1));
    var m = v - c;
    var rgb = h < 60 ? [c, x, 0] : h < 120 ? [x, c, 0] : h < 180 ? [0, c, x]
      : h < 240 ? [0, x, c] : h < 300 ? [x, 0, c] : [c, 0, x];
    return { r: Math.round((rgb[0] + m) * 255), g: Math.round((rgb[1] + m) * 255),
             b: Math.round((rgb[2] + m) * 255) };
  }

  function rgbToHsv(r, g, b) {
    r /= 255; g /= 255; b /= 255;
    var max = Math.max(r, g, b), min = Math.min(r, g, b), d = max - min;
    var h = d === 0 ? 0 : max === r ? 60 * ((((g - b) / d) % 6 + 6) % 6)
      : max === g ? 60 * ((b - r) / d + 2) : 60 * ((r - g) / d + 4);
    return { h: Math.round(h) % 360, s: Math.round(max === 0 ? 0 : d / max * 100),
             v: Math.round(max * 100) };
  }

  function hex2(n) { return (n < 16 ? "0" : "") + n.toString(16); }

  // "#rgb", "rgb", "#rrggbb", with alpha also 4 and 8 digits -> {r,g,b,a}
  function parseHex(s, alpha) {
    var h = String(s || "").trim().replace(/^#/, "");
    if (!/^[0-9a-f]+$/i.test(h)) { return null; }
    if (h.length === 3 || (alpha && h.length === 4)) {
      h = h.replace(/./g, "$&$&");
    }
    if (h.length !== 6 && !(alpha && h.length === 8)) { return null; }
    return { r: parseInt(h.slice(0, 2), 16), g: parseInt(h.slice(2, 4), 16),
             b: parseInt(h.slice(4, 6), 16), a: h.length === 8 ? parseInt(h.slice(6, 8), 16) : 255 };
  }

  function cpState(el) { return $.data(el, "ah-cp"); }

  // The exact RGB a colour was loaded with (hex, RGB inputs, swatch) is
  // kept until the HSV controls change it, so rounding through HSV does
  // not alter a typed colour.
  function cpRgb(st) {
    var e = st.exact;
    if (e && e.h === st.h && e.s === st.s && e.v === st.v) { return e.rgb; }
    return hsvToRgb(st.h, st.s, st.v);
  }

  function cpHex(st) {
    var c = cpRgb(st);
    var hex = "#" + hex2(c.r) + hex2(c.g) + hex2(c.b);
    return st.alpha && st.a < 255 ? hex + hex2(st.a) : hex;
  }

  function cpRgba(st) {
    var c = cpRgb(st);
    return "rgba(" + c.r + "," + c.g + "," + c.b + "," + round2(st.a / 255) + ")";
  }

  function cpLoad(st, c) {
    var hsv = rgbToHsv(c.r, c.g, c.b);
    // Keep the hue when the colour is grey or black, so the area does
    // not jump back to red.
    if (hsv.s === 0 || hsv.v === 0) { hsv.h = st.h; }
    if (hsv.v === 0) { hsv.s = st.s; }
    st.h = hsv.h; st.s = hsv.s; st.v = hsv.v; st.a = c.a;
    st.exact = { h: st.h, s: st.s, v: st.v, rgb: { r: c.r, g: c.g, b: c.b } };
  }

  // sigil 同步全部UI!: area colour, pointers, preview, inputs; plus the
  // alpha bar, swatches, ARIA and the popup trigger.
  function cpSync(el, skip) {
    var st = cpState(el);
    var $el = $(el);
    var c = cpRgb(st);
    var bright = 0.299 * c.r + 0.587 * c.g + 0.114 * c.b > 150;
    var hex6 = "#" + hex2(c.r) + hex2(c.g) + hex2(c.b);
    var $map = $el.find("." + CP + "-map");
    $map.css("background-color", "hsl(" + st.h + ", 100%, 50%)")
      .attr({ "aria-valuenow": st.s,
              "aria-valuetext": "Saturation " + st.s + "%, brightness " + st.v + "%" });
    $map.find("." + CP + "-map-pointer").css({ left: st.s + "%", top: (100 - st.v) + "%" })
      .toggleClass(CP + "-map-pointer-dark", bright)
      .toggleClass(CP + "-map-pointer-light", !bright);
    var $hue = $el.find("." + CP + "-bar").not("." + CP + "-alpha");
    $hue.attr("aria-valuenow", st.h).find("." + CP + "-bar-pointer").css("top", (st.h / 360 * 100) + "%");
    var pct = Math.round(st.a / 255 * 100);
    var $alpha = $el.find("." + CP + "-alpha");
    $alpha.css("--ah-cp-rgb", hex6).attr({ "aria-valuenow": pct, "aria-valuetext": pct + "%" })
      .find("." + CP + "-bar-pointer").css("top", (100 - pct) + "%");
    $el.find("." + CP + "-preview").css("background-color", cpRgba(st));
    if (skip !== "hex") {
      $el.find("." + CP + "-hex-input").val(cpHex(st).slice(1));
    }
    if (skip !== "rgb") {
      $el.find("." + CP + "-r-input").val(c.r);
      $el.find("." + CP + "-g-input").val(c.g);
      $el.find("." + CP + "-b-input").val(c.b);
      $el.find("." + CP + "-a-input").val(pct);
    }
  }

  // The value-bearing side: data-ah-value, hidden input, trigger, swatches.
  function cpSetValue(el, value) {
    var $el = $(el);
    var st = cpState(el);
    el.setAttribute("data-ah-value", value);
    $el.children("input[type=hidden]").val(value);
    var $trigger = $el.children("." + CP + "-trigger");
    $trigger.find("." + CP + "-trigger-swatch")
      .toggleClass(CP + "-trigger-empty", value === "")
      .css("--ah-cp-swatch", value === "" ? "" : cpRgba(st));
    $trigger.find("." + CP + "-trigger-text")
      .text(value === "" ? ($trigger.attr("data-placeholder") || "") : value);
    $el.find("." + CP + "-swatch").each(function () {
      this.setAttribute("aria-pressed", String(this.getAttribute("data-color") === value));
    });
  }

  // type: "input" (live) or "change" (commit). change fires only when the
  // value differs from the last committed one.
  function cpEmit(el, type, skip) {
    var st = cpState(el);
    cpSync(el, skip);
    var value = cpHex(st);
    var before = el.getAttribute("data-ah-value");
    cpSetValue(el, value);
    if (type === "input") {
      if (value !== before) { $(el).trigger("input", [{ value: value }]); }
    } else {
      cpCommit(el, value);
    }
  }

  function cpCommit(el, value) {
    var st = cpState(el);
    if (value !== st.committed) {
      st.committed = value;
      $(el).trigger("change", [{ value: value }]);
    }
  }

  function cpClear(el) {
    cpSetValue(el, "");
    cpCommit(el, "");
  }

  AH.define("colorpicker", {
    init: function (el, $el) {
      el.setAttribute("data-ah-uid", ++seq);
      var value = el.getAttribute("data-ah-value") || "";
      var alpha = el.getAttribute("data-alpha") === "true";
      var st = { h: 0, s: 100, v: 100, a: 255, alpha: alpha, committed: value };
      $.data(el, "ah-cp", st);
      var start = parseHex(value, alpha);
      if (start) { cpLoad(st, start); }
      var $trigger = $el.children("." + CP + "-trigger");
      var $popup = popupOf($el);
      var popup = $popup.length > 0;
      if (popup) {
        $popup.attr("id", $popup.attr("id") || uid("ah-cp-popup-"));
        $trigger.attr("aria-controls", $popup.attr("id"));
      }
      fenceNativeEvents($el);

      function open() {
        openPopup(el, $el, CP, $trigger, function () { cpSync(el); });
        $el.find("." + CP + "-map").trigger("focus");
      }
      function close(refocus) { closePopup(el, $el, CP, $trigger, refocus); }
      $.data(el, "ah-cp-open", open);
      $.data(el, "ah-cp-close", close);

      $trigger.on("click" + NS, function (e) {
        e.preventDefault();
        if (isOpen($el)) { close(true); } else { open(); }
      });
      $trigger.on("keydown" + NS, function (e) {
        if (e.key === "ArrowDown") { e.preventDefault(); open(); }
      });
      $el.on("keydown" + NS, function (e) {
        if (e.key === "Escape" && isOpen($el)) {
          e.preventDefault();
          e.stopPropagation();
          close(true);
        }
      });

      // Saturation/value area (sigil 处理面板拖拽).
      var $map = $el.find("." + CP + "-map");
      function fromMap(e) {
        var r = $map[0].getBoundingClientRect();
        var p = pointerXY(e);
        st.s = Math.round(clamp((p.x - r.left) / r.width, 0, 1) * 100);
        st.v = Math.round((1 - clamp((p.y - r.top) / r.height, 0, 1)) * 100);
        cpEmit(el, "input");
      }
      function endDrag() { cpEmit(el, "change"); }
      drag($map, el, fromMap, fromMap, endDrag);

      // Hue bar (sigil 处理色相拖拽) and alpha bar.
      var $hue = $el.find("." + CP + "-bar").not("." + CP + "-alpha");
      function fromHue(e) {
        var r = $hue[0].getBoundingClientRect();
        st.h = Math.round(clamp((pointerXY(e).y - r.top) / r.height, 0, 1) * 360) % 360;
        cpEmit(el, "input");
      }
      drag($hue, el, fromHue, fromHue, endDrag);
      var $alpha = $el.find("." + CP + "-alpha");
      function fromAlpha(e) {
        var r = $alpha[0].getBoundingClientRect();
        st.a = Math.round((1 - clamp((pointerXY(e).y - r.top) / r.height, 0, 1)) * 255);
        cpEmit(el, "input");
      }
      if ($alpha.length) { drag($alpha, el, fromAlpha, fromAlpha, endDrag); }

      // Keyboard: arrows move by 1, Shift by 10; Home / End.
      function keys($t, apply) {
        $t.on("keydown" + NS, function (e) {
          if (disabled(el)) { return; }
          var n = e.shiftKey ? 10 : 1;
          if (apply(e.key, n) !== false) {
            e.preventDefault();
            cpEmit(el, "input");
            cpEmit(el, "change");
          }
        });
      }
      keys($map, function (key, n) {
        switch (key) {
          case "ArrowLeft": st.s = clamp(st.s - n, 0, 100); break;
          case "ArrowRight": st.s = clamp(st.s + n, 0, 100); break;
          case "ArrowUp": st.v = clamp(st.v + n, 0, 100); break;
          case "ArrowDown": st.v = clamp(st.v - n, 0, 100); break;
          case "Home": st.s = 0; break;
          case "End": st.s = 100; break;
          default: return false;
        }
      });
      // The hue grows downwards on the bar, so Down increases it.
      keys($hue, function (key, n) {
        switch (key) {
          case "ArrowDown": case "ArrowRight": st.h = (st.h + n) % 360; break;
          case "ArrowUp": case "ArrowLeft": st.h = (st.h - n + 360) % 360; break;
          case "Home": st.h = 0; break;
          case "End": st.h = 359; break;
          default: return false;
        }
      });
      keys($alpha, function (key, n) {
        var step = Math.round(n * 2.55);
        switch (key) {
          case "ArrowUp": case "ArrowRight": st.a = clamp(st.a + step, 0, 255); break;
          case "ArrowDown": case "ArrowLeft": st.a = clamp(st.a - step, 0, 255); break;
          case "Home": st.a = 0; break;
          case "End": st.a = 255; break;
          default: return false;
        }
      });

      // Hex input (sigil 处理hex输入): live while valid, commit on change.
      var $hexIn = $el.find("." + CP + "-hex-input");
      $hexIn.on("input" + NS, function () {
        var c = parseHex($hexIn.val(), alpha);
        var len = String($hexIn.val()).trim().replace(/^#/, "").length;
        if (c && len >= 6) {
          cpLoad(st, c);
          cpEmit(el, "input", "hex");
        }
      });
      $hexIn.on("change" + NS, function () {
        var c = parseHex($hexIn.val(), alpha);
        if (c) { cpLoad(st, c); }
        cpEmit(el, "change");
      });
      $hexIn.on("keydown" + NS, function (e) {
        if (e.key === "Enter") { e.preventDefault(); $hexIn.trigger("change"); }
      });

      // RGB(A) inputs (sigil 处理rgb输入).
      var $rgbIn = $el.find("." + CP + "-r-input, ." + CP + "-g-input, ." + CP + "-b-input, ." + CP + "-a-input");
      function fromRgb() {
        var n = function (cls, max) {
          var v = parseInt($el.find("." + CP + "-" + cls + "-input").val(), 10);
          return isNaN(v) ? null : clamp(v, 0, max);
        };
        var r = n("r", 255), g = n("g", 255), b = n("b", 255);
        if (r === null || g === null || b === null) { return false; }
        var a = alpha ? n("a", 100) : 100;
        cpLoad(st, { r: r, g: g, b: b, a: a === null ? st.a : Math.round(a * 2.55) });
        return true;
      }
      $rgbIn.on("input" + NS, function () {
        if (fromRgb()) { cpEmit(el, "input", "rgb"); }
      });
      $rgbIn.on("change" + NS, function () {
        fromRgb();
        cpEmit(el, "change");
      });

      // Swatches and the clear link (sigil's transparent link).
      $el.on("click" + NS, "." + CP + "-swatch", function (e) {
        e.preventDefault();
        var c = parseHex(this.getAttribute("data-color"), alpha);
        if (!c || disabled(el)) { return; }
        cpLoad(st, c);
        cpEmit(el, "input");
        cpEmit(el, "change");
      });
      $el.on("click" + NS, "." + CP + "-transparent a", function (e) {
        e.preventDefault();
        if (disabled(el)) { return; }
        cpClear(el);
        close(true);
      });
      cpSync(el);
    },
    destroy: function (el) { stopFloat(el); },
    methods: {
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      // Set the value without firing events (server driven); "" clears.
      setValue: function (el, $el, v) {
        var st = cpState(el);
        var c = parseHex(v == null ? "" : String(v), st.alpha);
        if (!c) {
          st.committed = "";
          cpSetValue(el, "");
          return;
        }
        cpLoad(st, c);
        cpSync(el);
        st.committed = cpHex(st);
        cpSetValue(el, st.committed);
      },
      clear: function (el) { cpClear(el); },
      open: function (el) { $.data(el, "ah-cp-open")(); },
      close: function (el) { $.data(el, "ah-cp-close")(false); }
    }
  });
})(window.jQuery, window.AH);
