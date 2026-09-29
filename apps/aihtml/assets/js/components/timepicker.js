/* Behaviour of the timepicker (designs/04-components.md). Ported from
 * sigil: form/timepicker (+ timepicker/math, timepicker/svg).
 *
 * Value-bearing: data-ah-value and the hidden input follow the value, the
 * root fires `change` on commit (detail: {value}). Native input/change
 * events of the inner text field are stopped at the root so they are not
 * taken for the component's own events. */
import AH from "../core.js";
import "./_lib_picker.js";
import "virtual:ah-tpl/timepicker_header";
import "virtual:ah-tpl/timepicker_numbers";

var P = AH.lib.picker;
var uid = P.uid, clamp = P.clamp, round2 = P.round2, disabled = P.disabled,
  pointerXY = P.pointerXY;

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
// builds it in aihtml_timepicker:time_header/5.
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

function tpCurrent(st) { return pad2(st.h) + ":" + pad2(st.m); }


// Redraw header and clock from the state (sigil sync-header!, sync-clock!).
function tpRender(c) {
  var st = c.st, el = c.element;
  var header = el.querySelector("." + TP + "-header");
  if (header) {
    var focused = header.querySelector(":focus");
    var focusedAction = focused && focused.getAttribute("data-action");
    // Same markup as the server's first render: templates/timepicker_header.mustache
    header.innerHTML = AH.tpl.timepicker_header(tpHeaderView(st, disabled(el)));
    if (focusedAction) {
      var again = header.querySelector("[data-action='" + focusedAction + "']");
      if (again) { again.focus(); }
    }
  }

  var svg = el.querySelector("." + TP + "-svg");
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
function tpCommit(c, value, silent) {
  var st = c.st, el = c.element;
  var old = el.getAttribute("data-ah-value") || "";
  el.setAttribute("data-ah-value", value);
  var hidden = el.querySelector(":scope > input[type=hidden]");
  if (hidden) { hidden.value = value; }
  el.querySelectorAll("." + TP + "-input").forEach(function (i) { i.value = tpDisplay(st, value); });
  el.querySelectorAll("." + TP + "-clear").forEach(function (b) { b.hidden = value === ""; });
  if (!silent && value !== old) {
    c.fire("change", { value: value });
  }
}

// Load the committed value into the state, or now (rounded to the step)
// when empty, like sigil's default.
function tpLoad(c) {
  var st = c.st;
  var t = parseTime(c.element.getAttribute("data-ah-value"));
  if (t === null) {
    var now = new Date();
    t = now.getHours() * 60 + Math.round(now.getMinutes() / st.step) * st.step;
    t = t % (24 * 60);
  }
  st.h = Math.floor(t / 60);
  st.m = t % 60;
  tpClampState(st);
}

function tpFromPolar(c, e) {
  var svg = c.element.querySelector("." + TP + "-svg");
  var rect = svg.getBoundingClientRect();
  var pt = pointerXY(e);
  var dx = (pt.x - rect.left) / rect.width * 260 - CX;
  var dy = (pt.y - rect.top) / rect.height * 260 - CY;
  var angle = Math.atan2(dx, -dy);
  if (angle < 0) { angle += TWO_PI; }
  return { angle: angle, radius: Math.sqrt(dx * dx + dy * dy) };
}

// sigil compute-from-polar with snap-hour / snap-minute.
function tpApplyPolar(st, polar) {
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

function tpSetMode(c, mode) {
  c.st.mode = mode;
  tpRender(c);
}

function tpSetPeriod(c, period) {
  var st = c.st;
  var h = to24(to12(st.h).h12, period);
  var t = clamp(h * 60 + st.m, st.lo, st.hi);
  st.h = Math.floor(t / 60);
  st.m = t % 60;
  tpRender(c);
  tpCommit(c, tpCurrent(st));
}

AH.register("timepicker", class extends AH.Controller {
  setup() {
    var c = this, el = this.element;
    var lo = parseTime(el.getAttribute("data-min"));
    var hi = parseTime(el.getAttribute("data-max"));
    var st = this.st = {
      mode: "hours",
      format: el.getAttribute("data-format") === "24h" ? "24h" : "12h",
      step: parseInt(el.getAttribute("data-step"), 10) || 5,
      auto: el.getAttribute("data-auto-switch") !== "false",
      lo: lo === null ? 0 : lo,
      hi: hi === null ? 24 * 60 - 1 : hi,
      h: 12, m: 0
    };
    tpLoad(this);
    var input = el.querySelector("." + TP + "-input");
    var popupEl = P.popupOf(el, TP);
    var svg = el.querySelector("." + TP + "-svg");
    var popup = !!popupEl;
    if (popup) {
      popupEl.id = popupEl.id || uid("ah-tp-popup-");
      if (input) { input.setAttribute("aria-controls", popupEl.id); }
    }
    P.fenceNativeEvents(this);
    var isOpen = function () { return P.isOpen(el, TP); };

    var open = this._open = function (focusClock) {
      P.openPopup(c, TP, input, function () {
        st.mode = "hours";
        tpLoad(c);
        tpRender(c);
      });
      if (focusClock && svg) { svg.focus(); }
    };
    var close = this._close = function (refocus) { P.closePopup(c, TP, input, refocus); };

    // Field: click toggles; typing a time commits on change.
    this.delegate("mousedown", "." + TP + "-input-area", function (e) {
      if (e.target.closest("." + TP + "-clear")) { return; }
      if (isOpen()) {
        if (e.target !== input) { e.preventDefault(); close(true); }
      } else {
        open(false);
      }
    });
    if (input) {
      this.listen(input, "keydown", function (e) {
        if (e.key === "ArrowDown" || (e.key === " " && !input.value)) {
          e.preventDefault();
          open(true);
        } else if (e.key === "Enter" && !isOpen()) {
          e.preventDefault();
          input.dispatchEvent(new Event("change", { bubbles: true }));
        }
      });
      this.listen(input, "change", function () {
        var text = input.value;
        if (String(text).trim() === "") { tpCommit(c, ""); return; }
        var t = parseTime(text);
        if (t === null) {
          input.value = tpDisplay(st, el.getAttribute("data-ah-value") || "");
          return;
        }
        t = clamp(t, st.lo, st.hi);
        st.h = Math.floor(t / 60);
        st.m = t % 60;
        tpRender(c);
        tpCommit(c, tpCurrent(st));
      });
    }
    this.delegate("click", "." + TP + "-clear", function (e) {
      e.preventDefault();
      tpCommit(c, "");
      close(false);
      if (input) { input.focus(); }
    });
    this.listen(el, "keydown", function (e) {
      if (e.key === "Escape" && isOpen()) {
        e.preventDefault();
        e.stopPropagation();
        close(true);
      }
    });

    // Header: hours / minutes / AM / PM (sigil setup-header-clicks!).
    function headerAction(action) {
      if (disabled(el)) { return; }
      if (action === "select-hours") { tpSetMode(c, "hours"); }
      else if (action === "select-minutes") { tpSetMode(c, "minutes"); }
      else if (action === "set-am") { tpSetPeriod(c, "am"); }
      else if (action === "set-pm") { tpSetPeriod(c, "pm"); }
    }
    this.delegate("click", "." + TP + "-header [data-action]", function (e, t) {
      headerAction(t.getAttribute("data-action"));
    });
    this.delegate("keydown", "." + TP + "-header [data-action]", function (e, t) {
      if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        headerAction(t.getAttribute("data-action"));
      }
    });

    // Clock: drag (sigil setup-clock-drag!) and keyboard.
    var dragMode = null;
    P.drag(this, svg, function (e) {
      var polar = tpFromPolar(c, e);
      if (polar.radius <= 20) { return false; }
      dragMode = st.mode;
      if (tpApplyPolar(st, polar)) { tpRender(c); }
    }, function (e) {
      if (tpApplyPolar(st, tpFromPolar(c, e))) { tpRender(c); }
    }, function () {
      tpCommit(c, tpCurrent(st));
      if (dragMode === "hours" && st.auto) {
        tpSetMode(c, "minutes");
      } else if (dragMode === "minutes" && popup) {
        close(true);
      }
      dragMode = null;
    });
    if (svg) {
      this.listen(svg, "keydown", function (e) {
        if (disabled(el)) { return; }
        var dir = { ArrowUp: 1, ArrowRight: 1, ArrowDown: -1, ArrowLeft: -1 }[e.key];
        if (dir) {
          e.preventDefault();
          if (tpStep(st, dir)) {
            tpRender(c);
            tpCommit(c, tpCurrent(st));
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
          tpRender(c);
          tpCommit(c, tpCurrent(st));
        } else if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          tpCommit(c, tpCurrent(st));
          if (st.mode === "hours") {
            tpSetMode(c, "minutes");
          } else if (popup) {
            close(true);
          }
        }
      });
    }
    if (!popup) { tpRender(this); }
  }

  teardown() { P.stopFloat(this); }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  // Set the value without firing change (server driven).
  setValue(v) {
    var t = parseTime(v == null ? "" : String(v));
    var st = this.st;
    if (t === null) { tpCommit(this, "", true); return; }
    st.h = Math.floor(t / 60);
    st.m = t % 60;
    tpRender(this);
    tpCommit(this, tpCurrent(st), true);
  }
  clear() { tpCommit(this, ""); }
  open() { this._open(true); }
  close() { this._close(false); }
  setMode(mode) { tpSetMode(this, mode === "minutes" ? "minutes" : "hours"); }
});
