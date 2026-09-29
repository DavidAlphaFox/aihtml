/* Behaviour of the datetime_input (designs/04-components.md). Ported from
 * sigil: form/datetime_input (+ format, editor, dropdown). Dates are day
 * numbers (AH.lib.date). */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var D = AH.lib.date;
  var dnum = D.dnum, ymd = D.ymd, sow = D.sow, lastDay = D.lastDay, addMonths = D.addMonths,
      pad = D.pad, pad4 = D.pad4, isoDate = D.isoDate, todayNum = D.todayNum,
      validYmd = D.validYmd, parseDate = D.parseDate;
  var seq = 0;

  function ensureId(el, prefix) {
    if (!el.id) { el.id = prefix + (++seq); }
    return el.id;
  }

  function outside(el, e) {
    return e.target.isConnected !== false && !$.contains(el, e.target) && e.target !== el;
  }


  function readJson(el, attr, dflt) {
    try { return JSON.parse(el.getAttribute(attr) || "null") || dflt; } catch (err) { return dflt; }
  }


  function cls(base, opts) {
    return base + opts.filter(function (o) { return o[0]; }).map(function (o) { return " " + o[1]; }).join("");
  }

  function h12(h) { return h === 0 ? 12 : (h > 12 ? h - 12 : h); }

  var DTI_LABELS = {
    months: ["January", "February", "March", "April", "May", "June", "July",
             "August", "September", "October", "November", "December"],
    weekdays: ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"],
    title: "MMMM yyyy", time: "Time",
    prev_month: "Previous month", next_month: "Next month"
  };
  // [pattern, type, min, max]; single letters are two digits wide, as in
  // segments/1 of the Erlang side.
  var TOKENS = [["yyyy", "year", 1900, 2100], ["yy", "year2", 0, 99], ["MM", "month", 1, 12],
                ["M", "month", 1, 12], ["dd", "day", 1, 31], ["d", "day", 1, 31],
                ["HH", "hour", 0, 23], ["H", "hour", 0, 23], ["hh", "hour12", 1, 12],
                ["h", "hour12", 1, 12], ["mm", "minute", 0, 59], ["m", "minute", 0, 59],
                ["ss", "second", 0, 59], ["s", "second", 0, 59], ["aa", "ampm", 0, 1],
                ["a", "ampm", 0, 1]];
  var SEG_NAMES = { year: "Year", year2: "Year", month: "Month", day: "Day", hour: "Hour",
                    hour12: "Hour", minute: "Minute", second: "Second", ampm: "AM/PM" };

  // format.cljs parse-format: [{type, start, end, len, min, max, editable}]
  function dtiSegments(f) {
    var segs = [], pos = 0, i = 0;
    while (i < f.length) {
      var tok = null;
      for (var k = 0; k < TOKENS.length; k++) {
        if (f.substr(i, TOKENS[k][0].length) === TOKENS[k][0]) { tok = TOKENS[k]; break; }
      }
      if (tok) {
        var len = tok[1] === "year" ? 4 : 2;
        segs.push({ type: tok[1], start: pos, end: pos + len, len: len, min: tok[2], max: tok[3],
                    editable: true });
        pos += len;
        i += tok[0].length;
      } else {
        var last = segs[segs.length - 1];
        if (last && !last.editable) { last.text += f[i]; last.end++; last.len++; } else {
          segs.push({ type: "literal", text: f[i], start: pos, end: pos + 1, len: 1, editable: false });
        }
        pos++;
        i++;
      }
    }
    return segs;
  }

  function dtiKind(segs) {
    var date = false, time = false, sec = false;
    segs.forEach(function (s) {
      if (/^(year|year2|month|day)$/.test(s.type)) { date = true; }
      if (/^(hour|hour12|minute|second|ampm)$/.test(s.type)) { time = true; }
      if (s.type === "second") { sec = true; }
    });
    return { date: date || !time, time: time, sec: sec };
  }

  // values are {y, mo, d, h, mi, s}
  function dtiParse(s, kind) {
    s = String(s || "");
    var m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2})(?::(\d{2}))?)?$/.exec(s);
    if (m && validYmd(+m[1], +m[2], +m[3])) {
      return { y: +m[1], mo: +m[2], d: +m[3], h: +(m[4] || 0), mi: +(m[5] || 0), s: +(m[6] || 0) };
    }
    m = /^(\d{2}):(\d{2})(?::(\d{2}))?$/.exec(s);
    if (m && kind.time) {
      var t = ymd(todayNum());
      return { y: t[0], mo: t[1], d: t[2], h: +m[1], mi: +m[2], s: +(m[3] || 0) };
    }
    return null;
  }

  function dtiIso(v, kind) {
    if (!v) { return ""; }
    var date = pad4(v.y) + "-" + pad(v.mo) + "-" + pad(v.d);
    var time = pad(v.h) + ":" + pad(v.mi) + (kind.sec ? ":" + pad(v.s) : "");
    return !kind.time ? date : (kind.date ? date + "T" + time : time);
  }

  // A number that orders values of this kind.
  function dtiOrd(v, kind) {
    var t = v.h * 3600 + v.mi * 60 + v.s, d = dnum(v.y, v.mo, v.d);
    return !kind.time ? d : (kind.date ? d * 86400 + t : t);
  }

  function segValue(v, seg) {
    switch (seg.type) {
      case "year": return v.y;
      case "year2": return v.y % 100;
      case "month": return v.mo;
      case "day": return v.d;
      case "hour": return v.h;
      case "hour12": return h12(v.h);
      case "minute": return v.mi;
      case "second": return v.s;
      case "ampm": return v.h < 12 ? 0 : 1;
      default: return null;
    }
  }

  // The day is clamped to the month's length (sigil's js/Date rolls over).
  function setSeg(v0, seg, val) {
    var v = $.extend({}, v0);
    switch (seg.type) {
      case "year": v.y = val; break;
      case "year2": v.y = Math.floor(v.y / 100) * 100 + val; break;
      case "month": v.mo = val; break;
      case "day": v.d = val; break;
      case "hour": v.h = val; break;
      case "hour12": {
        var pm = v.h >= 12;
        v.h = pm ? (val === 12 ? 12 : val + 12) : (val === 12 ? 0 : val);
        break;
      }
      case "minute": v.mi = val; break;
      case "second": v.s = val; break;
      case "ampm": v.h = val === 0 ? (v.h >= 12 ? v.h - 12 : v.h) : (v.h < 12 ? v.h + 12 : v.h); break;
      default: break;
    }
    v.d = Math.min(v.d, lastDay(v.y, v.mo));
    return v;
  }

  function segText(v, seg) {
    if (!seg.editable) { return seg.text; }
    if (seg.type === "ampm") { return v.h < 12 ? "AM" : "PM"; }
    var n = segValue(v, seg);
    return seg.len === 4 ? pad4(n) : pad(n);
  }

  function segMax(v, seg) { return seg.type === "day" ? lastDay(v.y, v.mo) : seg.max; }

  function dtiState(el) { return $.data(el, "ah-dti"); }

  function dtiEditable(st) { return st.segs.map(function (s, i) { return s.editable ? i : -1; })
    .filter(function (i) { return i >= 0; }); }

  function dtiDisplay(st) {
    return st.value ? st.segs.map(function (s) { return segText(st.value, s); }).join("") : "";
  }

  function dtiSelect(st) {
    var seg = st.segs[st.active];
    if (!seg || !st.value || document.activeElement !== st.input) { return; }
    try { st.input.setSelectionRange(seg.start, seg.end); } catch (err) { /* not focused */ }
  }

  // Show the value, publish it (data-ah-value, hidden input) and fire
  // `input' when it changed; `change' fires on leaving (dtiCommit).
  function dtiShow(el, $el) {
    var st = dtiState(el);
    st.input.value = dtiDisplay(st);
    dtiSelect(st);
    var iso = dtiIso(st.value, st.kind), old = el.getAttribute("data-ah-value") || "";
    el.setAttribute("data-ah-value", iso);
    $el.children("input[type=hidden]").val(iso);
    $el.find(".ah-dti-label").toggleClass("ah-dti-label-float", !!st.value || $el.hasClass("ah-dti-focused"));
    if (iso !== old) { $el.trigger("input"); }
    if (st.open) { dtiRenderCal(el); }
  }

  function dtiClamp(st, v) {
    if (!v) { return v; }
    var k = dtiOrd(v, st.kind);
    if (st.min && k < dtiOrd(st.min, st.kind)) { return $.extend({}, st.min); }
    if (st.max && k > dtiOrd(st.max, st.kind)) { return $.extend({}, st.max); }
    return v;
  }

  function dtiCommit(el, $el) {
    var st = dtiState(el);
    dtiFlush(st);
    st.value = dtiClamp(st, st.value);
    dtiShow(el, $el);
    var iso = dtiIso(st.value, st.kind);
    if (iso !== st.committed) {
      st.committed = iso;
      $el.trigger("change");
    }
  }

  function dtiEnsure(st) {
    if (!st.value) {
      var n = new Date();
      st.value = { y: n.getFullYear(), mo: n.getMonth() + 1, d: n.getDate(),
                   h: n.getHours(), mi: n.getMinutes(), s: 0 };
    }
  }

  function dtiFlush(st) {
    if (st.active === null || !st.buf) { return; }
    var seg = st.segs[st.active], n = parseInt(st.buf, 10);
    st.buf = "";
    if (st.value && !isNaN(n)) { st.value = setSeg(st.value, seg, Math.max(seg.min, Math.min(seg.max, n))); }
  }

  function dtiFocusSeg(el, $el, i) {
    var st = dtiState(el);
    dtiFlush(st);
    st.active = i;
    dtiShow(el, $el);
  }

  function dtiAnnounce(el, $el) {
    var st = dtiState(el), seg = st.segs[st.active];
    if (seg && st.value) { $el.find(".ah-dti-live").text(SEG_NAMES[seg.type] + " " + segText(st.value, seg)); }
  }

  // editor.cljs handle-digit!: buffer until the part is full, then commit
  // and move on
  function dtiDigit(el, $el, ch) {
    var st = dtiState(el), seg = st.segs[st.active];
    if (!seg || !seg.editable || seg.type === "ampm") { return; }
    dtiEnsure(st);
    st.buf += ch;
    if (st.buf.length >= seg.len) {
      var n = parseInt(st.buf, 10);
      st.buf = "";
      st.value = setSeg(st.value, seg, Math.max(seg.min, Math.min(segMax(st.value, seg), n)));
      var next = dtiEditable(st).filter(function (i) { return i > st.active; })[0];
      if (next !== undefined) { st.active = next; }
      dtiShow(el, $el);
    } else {
      // preview: the typed digits right-aligned in the part
      var shown = dtiDisplay(st), p = (new Array(seg.len - st.buf.length + 1)).join(" ") + st.buf;
      st.input.value = shown.slice(0, seg.start) + p + shown.slice(seg.end);
      dtiSelect(st);
    }
  }

  function dtiStep(el, $el, delta, big) {
    var st = dtiState(el), seg = st.segs[st.active];
    if (!seg || !seg.editable) { return; }
    dtiEnsure(st);
    dtiFlush(st);
    var v = st.value;
    if (seg.type === "ampm") {
      st.value = setSeg(v, seg, segValue(v, seg) ? 0 : 1);
    } else {
      var mn = seg.min, mx = segMax(v, seg), cur = segValue(v, seg), n;
      if (big) {
        n = mn + (((cur - mn + delta) % (mx - mn + 1)) + (mx - mn + 1)) % (mx - mn + 1);
      } else {
        n = cur + delta;
        n = n < mn ? mx : (n > mx ? mn : n);
      }
      st.value = setSeg(v, seg, n);
    }
    dtiShow(el, $el);
    dtiAnnounce(el, $el);
  }

  function dtiMoveSeg(el, $el, dir) {
    var st = dtiState(el), eds = dtiEditable(st);
    var next = dir > 0 ? eds.filter(function (i) { return i > st.active; })[0]
      : eds.filter(function (i) { return i < st.active; }).pop();
    if (next === undefined) { dtiFlush(st); return false; }
    dtiFocusSeg(el, $el, next);
    return true;
  }

  function dtiBlocked($el) { return $el.hasClass("ah-dti-disabled") || $el.hasClass("ah-dti-readonly"); }

  // editor.cljs on-keydown
  function dtiKey(el, $el, e) {
    var st = dtiState(el), k = e.key;
    if (k === "Tab") {
      if (!dtiBlocked($el) && dtiMoveSeg(el, $el, e.shiftKey ? -1 : 1)) { e.preventDefault(); } else { dtiClose(el, $el); }
      return;
    }
    if (e.ctrlKey || e.metaKey) { return; }
    if (k === "Escape") {
      if (st.open) { e.preventDefault(); dtiClose(el, $el); }
      return;
    }
    e.preventDefault();
    if (dtiBlocked($el)) { return; }
    if ((k === "ArrowDown" && e.altKey) || k === "F4") {
      if (st.open) { dtiClose(el, $el); } else { dtiOpen(el, $el); }
      return;
    }
    if (/^[0-9]$/.test(k)) { dtiDigit(el, $el, k); return; }
    var eds = dtiEditable(st), seg = st.segs[st.active];
    switch (k) {
      case "ArrowUp": dtiStep(el, $el, 1, false); break;
      case "ArrowDown": dtiStep(el, $el, -1, false); break;
      case "PageUp": dtiStep(el, $el, 10, true); break;
      case "PageDown": dtiStep(el, $el, -10, true); break;
      case "ArrowLeft": dtiMoveSeg(el, $el, -1); break;
      case "ArrowRight": dtiMoveSeg(el, $el, 1); break;
      case "Home": dtiFocusSeg(el, $el, eds[0]); break;
      case "End": dtiFocusSeg(el, $el, eds[eds.length - 1]); break;
      case "Backspace":
      case "Delete":
        if (seg && seg.editable && st.value) {
          st.buf = "";
          st.value = setSeg(st.value, seg, seg.min);
          dtiShow(el, $el);
        }
        break;
      case "a": case "A": case "p": case "P":
        if (seg && seg.type === "ampm" && st.value) {
          st.value = setSeg(st.value, seg, /a/i.test(k) ? 0 : 1);
          dtiShow(el, $el);
        }
        break;
      default: break;
    }
  }

  // format.cljs segment-at-cursor: the part under the caret, or the nearest
  function dtiSegAt(st, pos) {
    var eds = dtiEditable(st), best = eds[0], dist = Infinity;
    for (var i = 0; i < eds.length; i++) {
      var s = st.segs[eds[i]];
      if (pos >= s.start && pos < s.end) { return eds[i]; }
      var dd = Math.min(Math.abs(pos - s.start), Math.abs(pos - s.end));
      if (dd < dist) { dist = dd; best = eds[i]; }
    }
    return best;
  }

  // ---- drop-down calendar ----

  function dtiCalView(st) {
    var L = st.L, y = st.navY, m = st.navM, today = todayNum();
    var start = sow(dnum(y, m, 1), st.first);
    var sel = st.value ? dnum(st.value.y, st.value.mo, st.value.d) : null;
    var min = st.min && st.kind.date ? dnum(st.min.y, st.min.mo, st.min.d) : null;
    var max = st.max && st.kind.date ? dnum(st.max.y, st.max.mo, st.max.d) : null;
    var days = [];
    for (var i = 0; i < 42; i++) {
      var d = start + i, p = ymd(d);
      var dis = (min !== null && d < min) || (max !== null && d > max);
      days.push({
        cls: cls("ah-dti-cal-day", [[d === today, "ah-dti-cal-day-today"], [d === sel, "ah-dti-cal-day-selected"],
                                    [p[1] !== m, "ah-dti-cal-day-other"], [dis, "ah-dti-cal-day-disabled"]]),
        date: isoDate(d), day: String(p[2]), disabled: dis, selected: d === sel
      });
    }
    var weekdays = [];
    for (var k = 0; k < 7; k++) { weekdays.push({ label: L.weekdays[(st.first + k) % 7] }); }
    var title = L.title.replace(/yyyy|MMMM|MM|M/g, function (t) {
      return t === "yyyy" ? String(y) : t === "MMMM" ? L.months[m - 1] : t === "MM" ? pad(m) : String(m);
    });
    return { title: title, prev_month: L.prev_month, next_month: L.next_month, weekdays: weekdays,
             days: days, show_time: st.showTime && !!st.value, time_label: L.time,
             hours: st.value ? pad(st.value.h) : "", minutes: st.value ? pad(st.value.mi) : "" };
  }

  function dtiRenderCal(el) {
    var st = dtiState(el);
    var focused = document.activeElement;
    var field = focused && $.contains(st.$dd[0], focused) ? focused.getAttribute("data-field") : null;
    st.$dd.html(AH.tpl.datetime_input_calendar(dtiCalView(st)));
    if (field) { st.$dd.find('[data-field="' + field + '"]').trigger("focus"); }
    if (st.float) { st.float.update(); }
  }

  function dtiOpen(el, $el) {
    var st = dtiState(el);
    if (st.open || !st.$dd.length || dtiBlocked($el)) { return; }
    var v = st.value;
    var t = ymd(todayNum());
    st.navY = v ? v.y : t[0];
    st.navM = v ? v.mo : t[1];
    st.open = true;
    st.$dd.prop("hidden", false);
    dtiRenderCal(el);
    st.float = AH.float(st.$dd[0], $el.children(".ah-dti-row")[0], { offset: 2 });
    $(st.input).attr("aria-expanded", "true");
    $(document).on("mousedown" + st.ns, function (e) { if (outside(el, e)) { dtiClose(el, $el); } });
    $el.trigger("ah:open");
  }

  function dtiClose(el, $el) {
    var st = dtiState(el);
    if (!st.open) { return; }
    st.open = false;
    st.$dd.prop("hidden", true).empty();
    if (st.float) { st.float.stop(); st.float = null; }
    $(st.input).attr("aria-expanded", "false");
    $(document).off(st.ns);
    $el.trigger("ah:close");
  }

  // dropdown.cljs on-day-click: keep the time, clamp, fire change
  function dtiPickDay(el, $el, iso) {
    var st = dtiState(el), d = parseDate(iso);
    if (d === null) { return; }
    var p = ymd(d), v = st.value || { h: 0, mi: 0, s: 0 };
    st.value = dtiClamp(st, { y: p[0], mo: p[1], d: p[2], h: v.h, mi: v.mi, s: v.s });
    st.navY = st.value.y;
    st.navM = st.value.mo;
    dtiShow(el, $el);
    dtiCommit(el, $el);
    if (!st.showTime) { dtiClose(el, $el); st.input.focus(); }
  }

  function dtiSpinStop(st) { clearTimeout(st.spin); st.spin = null; }

  AH.define("datetime_input", {
    init: function (el, $el) {
      ensureId(el, "ah-dti");
      var segs = dtiSegments(el.getAttribute("data-ah-format") || "yyyy-MM-dd");
      var kind = dtiKind(segs);
      var first = parseInt(el.getAttribute("data-ah-first-day") || "0", 10);
      var st = {
        ns: ".ahdti" + (++seq),
        segs: segs, kind: kind,
        input: $el.find("input.ah-dti-input")[0],
        $dd: $el.children(".ah-dti-dropdown"),
        value: dtiParse(el.getAttribute("data-ah-value"), kind),
        min: dtiParse(el.getAttribute("data-ah-min"), kind),
        max: dtiParse(el.getAttribute("data-ah-max"), kind),
        first: first >= 0 && first <= 6 ? first : 0,
        showTime: el.hasAttribute("data-ah-show-time"),
        L: $.extend({}, DTI_LABELS, readJson(el, "data-ah-labels", {})),
        active: null, buf: "", open: false, float: null, spin: null
      };
      st.committed = dtiIso(st.value, kind);
      $.data(el, "ah-dti", st);
      var $in = $(st.input);
      $in.on("focus" + NS, function () {
        $el.addClass("ah-dti-focused");
        if (st.active === null) { st.active = dtiEditable(st)[0]; }
        $el.find(".ah-dti-label").addClass("ah-dti-label-float");
        setTimeout(function () { dtiSelect(st); }, 0);
      }).on("mouseup" + NS, function () {
        if (!st.value) { return; }
        dtiFocusSeg(el, $el, dtiSegAt(st, st.input.selectionStart || 0));
      }).on("keydown" + NS, function (e) { dtiKey(el, $el, e); })
        // the text field is internal: only the root reports changes
        .on("change" + NS + " input" + NS, function (e) { e.stopPropagation(); });
      // leaving the component (the time fields of the drop-down are inside)
      $el.on("focusout" + NS, function (e) {
        if (e.relatedTarget && $.contains(el, e.relatedTarget)) { return; }
        setTimeout(function () {
          if ($.contains(el, document.activeElement)) { return; }
          $el.removeClass("ah-dti-focused");
          dtiCommit(el, $el);
          dtiClose(el, $el);
        }, 0);
      });
      $el.on("click" + NS, ".ah-dti-cal-btn", function () {
        if (dtiBlocked($el)) { return; }
        st.input.focus();
        if (st.open) { dtiClose(el, $el); } else { dtiOpen(el, $el); }
      });
      $el.on("mousedown" + NS, ".ah-dti-cal-btn", function (e) { e.preventDefault(); });
      // spinner: step, then repeat after 400ms every 120ms while held
      $el.on("mousedown" + NS, ".ah-dti-spin", function (e) {
        e.preventDefault();
        if (dtiBlocked($el)) { return; }
        var delta = $(this).hasClass("ah-dti-spin-up") ? 1 : -1;
        if (st.active === null) { st.active = dtiEditable(st)[0]; }
        st.input.focus();
        dtiSpinStop(st);
        dtiStep(el, $el, delta, false);
        var rep = function () { dtiStep(el, $el, delta, false); st.spin = setTimeout(rep, 120); };
        st.spin = setTimeout(rep, 400);
      }).on("mouseup" + NS + " mouseleave" + NS, ".ah-dti-spin", function () { dtiSpinStop(st); });
      // drop-down: keep the focus in the field, except for the time inputs
      st.$dd.on("mousedown" + NS, function (e) {
        if (!$(e.target).is("input")) { e.preventDefault(); }
      }).on("click" + NS, ".ah-dti-cal-day", function () {
        if (!$(this).hasClass("ah-dti-cal-day-disabled")) { dtiPickDay(el, $el, this.getAttribute("data-date")); }
      }).on("click" + NS, "[data-action]", function () {
        var n = dnum(st.navY, st.navM, 1);
        var t = ymd(addMonths(n, this.getAttribute("data-action") === "prev-month" ? -1 : 1));
        st.navY = t[0];
        st.navM = t[1];
        dtiRenderCal(el);
      }).on("change" + NS, ".ah-dti-time-input", function (e) {
        e.stopPropagation();
        var n = parseInt(this.value, 10);
        if (isNaN(n)) { return; }
        dtiEnsure(st);
        st.value = $.extend({}, st.value);
        if (this.getAttribute("data-field") === "hours") { st.value.h = Math.max(0, Math.min(23, n)); }
        else { st.value.mi = Math.max(0, Math.min(59, n)); }
        dtiShow(el, $el);
        dtiCommit(el, $el);
      }).on("input" + NS, ".ah-dti-time-input", function (e) { e.stopPropagation(); })
        .on("keydown" + NS, ".ah-dti-time-input", function (e) {
          if (e.key === "Escape") { e.preventDefault(); dtiClose(el, $el); st.input.focus(); }
          if (e.key === "Enter") { e.preventDefault(); $(this).trigger("change"); }
        });
    },
    destroy: function (el, $el) {
      var st = dtiState(el);
      if (!st) { return; }
      dtiSpinStop(st);
      dtiClose(el, $el);
    },
    methods: {
      setValue: function (el, $el, v) {
        var st = dtiState(el);
        st.value = v ? dtiParse(v, st.kind) : null;
        st.buf = "";
        st.committed = dtiIso(st.value, st.kind);
        dtiShow(el, $el);
      },
      getValue: function (el) { return el.getAttribute("data-ah-value"); },
      clear: function (el, $el) {
        var st = dtiState(el);
        st.value = null;
        st.buf = "";
        dtiShow(el, $el);
        dtiCommit(el, $el);
      },
      open: function (el, $el) { dtiOpen(el, $el); },
      close: function (el, $el) { dtiClose(el, $el); }
    }
  });
})(window.jQuery, window.AH);
