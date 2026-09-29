/* Behaviours of the form_pickers components (designs/04-components.md).
 * Ported from sigil: form/datepicker (+ calendar, popup, util) and
 * form/combobox (+ popup, search). Date math is plain JS on local dates. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  function ensureId(el, prefix) {
    if (!el.id) { el.id = prefix + (++seq); }
    return el.id;
  }

  // A mousedown outside the component. A target that is gone was inside
  // a list re-rendered by this very mousedown (picking in multiple mode).
  function outside(el, e) {
    return e.target.isConnected !== false && !$.contains(el, e.target) && e.target !== el;
  }

  // Value-bearing contract: data-ah-value + hidden input, then `change`.
  function publish(el, $el, value, fire) {
    var old = el.getAttribute("data-ah-value") || "";
    el.setAttribute("data-ah-value", value);
    $el.children("input[type=hidden]").val(value);
    if (fire && old !== value) { $el.trigger("change"); }
  }

  // ==================================================================
  // datepicker
  // ==================================================================

  var DAY = 86400000;

  function pad(n) { return (n < 10 ? "0" : "") + n; }
  function mk(y, m, d) { return new Date(y, m, d); }          // m: 0..11, overflow ok
  function iso(d) { return d ? d.getFullYear() + "-" + pad(d.getMonth() + 1) + "-" + pad(d.getDate()) : ""; }
  function parse(s) {
    var m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(s || "");
    return m ? mk(+m[1], +m[2] - 1, +m[3]) : null;
  }
  function today() { var n = new Date(); return mk(n.getFullYear(), n.getMonth(), n.getDate()); }
  function same(a, b) { return !!(a && b) && a.getTime() === b.getTime(); }
  function addDays(d, n) { return mk(d.getFullYear(), d.getMonth(), d.getDate() + n); }
  // Month arithmetic keeps the day, clamped to the target month (date-fns addMonths).
  function addMonths(d, n) {
    var t = mk(d.getFullYear(), d.getMonth() + n, 1);
    var last = mk(t.getFullYear(), t.getMonth() + 1, 0).getDate();
    return mk(t.getFullYear(), t.getMonth(), Math.min(d.getDate(), last));
  }
  function startOfWeek(d, first) { return addDays(d, -((d.getDay() - first + 7) % 7)); }
  // date-fns getWeek with weekStartsOn = first, firstWeekContainsDate = 1.
  function weekNumber(d, first) {
    var y = d.getFullYear();
    var wy = d >= startOfWeek(mk(y + 1, 0, 1), first) ? y + 1
      : (d >= startOfWeek(mk(y, 0, 1), first) ? y : y - 1);
    var diff = startOfWeek(d, first) - startOfWeek(mk(wy, 0, 1), first);
    return Math.round(diff / (7 * DAY)) + 1;
  }

  // The display formats the Erlang side knows: yyyy yy MMMM MMM MM M dd d.
  function format(d, fmt, L) {
    if (!d) { return ""; }
    return fmt.replace(/yyyy|yy|MMMM|MMM|MM|M|dd|d/g, function (t) {
      switch (t) {
        case "yyyy": return String(d.getFullYear());
        case "yy": return String(d.getFullYear()).slice(2);
        case "MMMM": return L.months[d.getMonth()];
        case "MMM": return L.months_short[d.getMonth()];
        case "MM": return pad(d.getMonth() + 1);
        case "M": return String(d.getMonth() + 1);
        case "dd": return pad(d.getDate());
        default: return String(d.getDate());
      }
    });
  }

  var DEFAULT_LABELS = {
    months: ["January", "February", "March", "April", "May", "June", "July",
             "August", "September", "October", "November", "December"],
    months_short: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    weekdays: ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"],
    title: "MMMM yyyy", today: "Today", clear: "Clear",
    prev_month: "Previous month", next_month: "Next month",
    prev_year: "Previous year", next_year: "Next year"
  };

  function dpState(el) { return $.data(el, "ah-dp"); }

  function dpDisabled(st, d) {
    return (st.min && d < st.min) || (st.max && d > st.max) || !!st.off[iso(d)];
  }

  function dpDisplay(st) {
    if (st.range) {
      if (!st.from) { return ""; }
      return format(st.from, st.fmt, st.L) + (st.to ? " - " + format(st.to, st.fmt, st.L) : "");
    }
    return format(st.value, st.fmt, st.L);
  }

  function dpIsoValue(st) {
    return st.range ? (st.from ? iso(st.from) + "," + iso(st.to) : "") : iso(st.value);
  }

  // The view of templates/datepicker_month.mustache: every class, label
  // and id computed here (the Erlang month_view/1 builds the same view for
  // the server's `inline' render).
  function dpView(el) {
    var st = dpState(el);
    var L = st.L;
    var y = st.viewY, m = st.viewM;
    var first = st.firstDay;
    var monthStart = mk(y, m, 1);
    var gridStart = startOfWeek(monthStart, first);
    var gridEnd = addDays(startOfWeek(mk(y, m + 1, 0), first), 7);
    var now = today();
    var from = st.range ? (st.pending || st.from) : null;
    var to = st.range ? (st.pending ? null : st.to) : null;
    var weeks = [];
    for (var w = gridStart; w < gridEnd; w = addDays(w, 7)) {
      var days = [];
      for (var k = 0; k < 7; k++) {
        var d = addDays(w, k);
        var other = d.getMonth() !== m;
        if (other && !st.otherMonth) { days.push({ empty: true }); continue; }
        var dis = !!dpDisabled(st, d);
        var isStart = same(d, from), isEnd = same(d, to);
        var sel = st.range ? (isStart || isEnd) : same(d, st.value);
        var inRange = !!(st.range && from && to && d >= from && d <= to);
        var dow = d.getDay();
        days.push({
          empty: false,
          cls: "ah-datepicker-day" +
            (other ? " ah-datepicker-day-other-month" : "") +
            (same(d, now) ? " ah-datepicker-day-today" : "") +
            (st.weekends && (dow === 0 || dow === 6) ? " ah-datepicker-day-weekend" : "") +
            (dis ? " ah-datepicker-day-disabled" : "") +
            (sel ? " ah-datepicker-day-selected" : "") +
            (inRange ? " ah-datepicker-day-in-range" : "") +
            (isStart ? " ah-datepicker-day-range-start" : "") +
            (isEnd ? " ah-datepicker-day-range-end" : "") +
            (same(d, st.focus) ? " ah-datepicker-day-focused" : ""),
          id: el.id + "-d" + iso(d),
          date: iso(d),
          selected: String(sel),
          disabled: String(dis),
          label: format(d, "d MMMM yyyy", L),
          day: String(d.getDate())
        });
      }
      weeks.push({ num: String(weekNumber(w, first)), days: days });
    }
    var weekdays = [];
    for (var i = 0; i < 7; i++) { weekdays.push({ label: L.weekdays[(first + i) % 7] }); }
    return {
      title_id: el.id + "-title",
      title: format(monthStart, L.title, L),
      prev_year: L.prev_year, prev_month: L.prev_month,
      next_month: L.next_month, next_year: L.next_year,
      today: L.today,
      week_numbers: st.weekNumbers,
      weekdays: weekdays,
      weeks: weeks
    };
  }

  function dpRender(el) {
    var st = dpState(el);
    st.$popup.html(AH.tpl.datepicker_month(dpView(el)));
    st.$input.attr("aria-activedescendant", st.focus ? el.id + "-d" + iso(st.focus) : null);
  }

  // popup.cljs position!: at least 280px wide (the viewport minus a
  // margin on small screens); AH.float keeps it fixed under the field (or
  // above, when there is no room), inside the viewport and following
  // scrolls, so clipping ancestors do not cut it.
  function dpPosition(el) {
    var st = dpState(el);
    var w = Math.min(Math.max(el.getBoundingClientRect().width, 280), window.innerWidth - 16);
    st.$popup.css({ width: w + "px" });
    if (st.float) { st.float.update(); } else { st.float = AH.float(st.$popup[0], el); }
  }

  function dpBlocked(el, $el) {
    return $el.hasClass("ah-datepicker-disabled") || $el.hasClass("ah-datepicker-readonly");
  }

  function dpOpen(el, $el) {
    var st = dpState(el);
    if (st.open || (dpBlocked(el, $el) && !st.inline)) { return; }
    st.pending = null;
    st.focus = (st.range ? st.from : st.value) || today();
    st.viewY = st.focus.getFullYear();
    st.viewM = st.focus.getMonth();
    st.open = true;
    dpRender(el);
    if (st.inline) { return; }        // always shown, in the flow
    st.$popup.show();
    dpPosition(el);
    $el.addClass("ah-datepicker-open");
    st.$input.attr("aria-expanded", "true");
    $(document).on("mousedown" + st.ns, function (e) {
      if (outside(el, e)) { dpClose(el, $el); }
    });
    $el.trigger("ah:open");
  }

  function dpClose(el, $el) {
    var st = dpState(el);
    if (!st.open || st.inline) { return; }
    st.open = false;
    st.pending = null;
    st.$popup.hide();
    $el.removeClass("ah-datepicker-open");
    st.$input.attr("aria-expanded", "false").removeAttr("aria-activedescendant");
    if (st.float) { st.float.stop(); st.float = null; }
    $(document).off(st.ns);
    $el.trigger("ah:close");
  }

  function dpCommit(el, $el, fire) {
    var st = dpState(el);
    st.$input.val(dpDisplay(st));
    publish(el, $el, dpIsoValue(st), fire);
  }

  function dpSetFocus(el, d) {
    var st = dpState(el);
    st.focus = d;
    st.viewY = d.getFullYear();
    st.viewM = d.getMonth();
    dpRender(el);
    if (st.open && !st.inline) { dpPosition(el); }
  }

  function dpNavMonths(el, n) {
    var st = dpState(el);
    var f = st.focus && st.focus.getMonth() === st.viewM ? st.focus : mk(st.viewY, st.viewM, 1);
    dpSetFocus(el, addMonths(f, n));
  }

  // calendar.cljs handle-day-click: single selects and closes; range takes
  // a start, then an end (sorted), then closes.
  function dpPick(el, $el, d) {
    var st = dpState(el);
    if (!d || dpDisabled(st, d) || dpBlocked(el, $el)) { return; }
    if (!st.range) {
      st.value = d;
      st.focus = d;
      dpCommit(el, $el, true);
      dpClose(el, $el);
      if (st.inline) { dpRender(el); }
    } else if (!st.pending) {
      st.pending = d;
      st.focus = d;
      dpRender(el);
    } else {
      var a = st.pending;
      st.from = d < a ? d : a;
      st.to = d < a ? a : d;
      st.pending = null;
      dpCommit(el, $el, true);
      dpClose(el, $el);
      if (st.inline) { dpRender(el); }
    }
  }

  function dpHover(el, d) {
    var st = dpState(el);
    if (!st.range || !st.pending) { return; }
    var a = st.pending < d ? st.pending : d;
    var b = st.pending < d ? d : st.pending;
    st.$popup.find(".ah-datepicker-day[data-date]").each(function () {
      var c = parse(this.getAttribute("data-date"));
      $(this).toggleClass("ah-datepicker-day-hover-range", c >= a && c <= b);
    });
  }

  // popup.cljs handle-keydown. Up/Down move a week (sigil moves a day),
  // Shift+PageUp/PageDown a year.
  function dpKey(el, $el, e) {
    var st = dpState(el);
    if (dpBlocked(el, $el)) { return; }
    var open = st.open;
    var f = st.focus || today();
    switch (e.key) {
      case "ArrowDown":
        e.preventDefault();
        if (e.altKey || !open) { dpOpen(el, $el); } else { dpSetFocus(el, addDays(f, 7)); }
        break;
      case "ArrowUp":
        if (!open) { return; }
        e.preventDefault();
        if (e.altKey) { dpClose(el, $el); } else { dpSetFocus(el, addDays(f, -7)); }
        break;
      case "ArrowLeft":
        if (open) { e.preventDefault(); dpSetFocus(el, addDays(f, -1)); }
        break;
      case "ArrowRight":
        if (open) { e.preventDefault(); dpSetFocus(el, addDays(f, 1)); }
        break;
      case "Enter":
      case " ":
        e.preventDefault();
        if (open) { dpPick(el, $el, f); } else { dpOpen(el, $el); }
        break;
      case "Escape":
        if (open) { e.preventDefault(); dpClose(el, $el); }
        break;
      case "Tab":
        dpClose(el, $el);
        break;
      case "PageUp":
        if (open) { e.preventDefault(); dpNavMonths(el, e.shiftKey ? -12 : -1); }
        break;
      case "PageDown":
        if (open) { e.preventDefault(); dpNavMonths(el, e.shiftKey ? 12 : 1); }
        break;
      case "Home":
        if (open) { e.preventDefault(); dpSetFocus(el, mk(st.viewY, st.viewM, 1)); }
        break;
      case "End":
        if (open) { e.preventDefault(); dpSetFocus(el, mk(st.viewY, st.viewM + 1, 0)); }
        break;
      case "Backspace":
      case "Delete":
        if ($el.hasClass("ah-datepicker-clearable")) { e.preventDefault(); dpClear(el, $el, true); }
        break;
      default:
        break;
    }
  }

  function dpClear(el, $el, fire) {
    var st = dpState(el);
    st.value = st.from = st.to = st.pending = null;
    dpCommit(el, $el, fire);
    if (st.open) { dpRender(el); }
  }

  function dpSet(el, $el, v) {
    var st = dpState(el);
    if (Array.isArray(v)) { v = v.join(","); }
    v = v || "";
    if (st.range) {
      var p = v.split(",");
      var a = parse(p[0]), b = parse(p[1]);
      st.from = a && b && b < a ? b : a;
      st.to = a && b && b < a ? a : b;
    } else {
      st.value = parse(v);
    }
    st.pending = null;
    dpCommit(el, $el, false);
    if (st.open) { dpRender(el); }
  }

  AH.define("datepicker", {
    init: function (el, $el) {
      ensureId(el, "ah-dp");
      var labels = {};
      try { labels = JSON.parse(el.getAttribute("data-ah-labels") || "{}"); } catch (err) { labels = {}; }
      var off = {};
      (el.getAttribute("data-ah-disabled-dates") || "").split(",").forEach(function (d) {
        if (d) { off[d] = true; }
      });
      var st = {
        ns: ".ahdp" + (++seq),
        $input: $el.find("input.ah-datepicker-input"),
        $popup: $el.children(".ah-datepicker-popup"),
        range: el.hasAttribute("data-ah-range"),
        fmt: el.getAttribute("data-ah-format") || "yyyy-MM-dd",
        min: parse(el.getAttribute("data-ah-min")),
        max: parse(el.getAttribute("data-ah-max")),
        off: off,
        firstDay: parseInt(el.getAttribute("data-ah-first-day") || "0", 10) || 0,
        weekNumbers: el.hasAttribute("data-ah-week-numbers"),
        weekends: el.hasAttribute("data-ah-weekends"),
        otherMonth: el.getAttribute("data-ah-other-month-days") !== "false",
        L: $.extend({}, DEFAULT_LABELS, labels),
        inline: $el.hasClass("ah-datepicker-inline"),
        open: false, value: null, from: null, to: null, pending: null, focus: null
      };
      $.data(el, "ah-dp", st);
      st.$popup.attr("id", el.id + "-popup");
      st.$input.attr("aria-controls", el.id + "-popup");
      var v = el.getAttribute("data-ah-value") || "";
      if (st.range) {
        var p = v.split(",");
        st.from = parse(p[0]);
        st.to = parse(p[1]);
      } else {
        st.value = parse(v);
      }

      st.$input
        .on("focus" + NS, function () { $el.addClass("ah-datepicker-focused"); })
        .on("blur" + NS, function () { $el.removeClass("ah-datepicker-focused"); })
        .on("click" + NS, function (e) {
          e.preventDefault();
          if (st.open) { dpClose(el, $el); } else { dpOpen(el, $el); }
        })
        .on("keydown" + NS, function (e) { dpKey(el, $el, e); })
        // the text field is internal: only the root reports changes
        .on("change" + NS + " input" + NS, function (e) { e.stopPropagation(); });
      $el.on("click" + NS, ".ah-datepicker-trigger", function (e) {
        e.preventDefault();
        e.stopPropagation();
        st.$input.trigger("focus");
        if (st.open) { dpClose(el, $el); } else { dpOpen(el, $el); }
      });
      $el.on("mousedown" + NS, ".ah-datepicker-clear", function (e) { e.preventDefault(); });
      $el.on("click" + NS, ".ah-datepicker-clear", function (e) {
        e.preventDefault();
        e.stopPropagation();
        dpClear(el, $el, true);
      });
      // Keep the focus in the text field while using the calendar.
      st.$popup.on("mousedown" + NS, function (e) {
        e.preventDefault();
        if (st.inline && !dpBlocked(el, $el)) { st.$input[0].focus(); }
      })
        .on("click" + NS, function (e) { e.stopPropagation(); })
        .on("click" + NS, ".ah-datepicker-day[data-date]", function () {
          dpPick(el, $el, parse(this.getAttribute("data-date")));
        })
        .on("mouseenter" + NS, ".ah-datepicker-day[data-date]", function () {
          dpHover(el, parse(this.getAttribute("data-date")));
        })
        .on("click" + NS, "[data-nav]", function () {
          dpNavMonths(el, parseInt(this.getAttribute("data-nav"), 10));
        })
        .on("click" + NS, ".ah-datepicker-today-btn", function () { dpSetFocus(el, today()); });
      if (st.inline) { dpOpen(el, $el); }
    },
    destroy: function (el) {
      var st = dpState(el);
      if (st) {
        if (st.float) { st.float.stop(); st.float = null; }
        $(document).off(st.ns);
        st.$popup.off(NS);
      }
    },
    methods: {
      // setValue("2026-09-29"), setValue("from,to") or setValue([from, to]);
      // no change event (the server set it).
      setValue: function (el, $el, v) { dpSet(el, $el, v); },
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      clear: function (el, $el) { dpClear(el, $el, true); },
      open: function (el, $el) { dpOpen(el, $el); },
      close: function (el, $el) { dpClose(el, $el); }
    }
  });

  // ==================================================================
  // combobox
  // ==================================================================

  function cbState(el) { return $.data(el, "ah-cb"); }

  // search.cljs match-fn
  var MATCH = {
    contains_ignore_case: function (t, q) { return t.toLowerCase().indexOf(q.toLowerCase()) >= 0; },
    contains: function (t, q) { return t.indexOf(q) >= 0; },
    starts_with_ignore_case: function (t, q) { return t.toLowerCase().indexOf(q.toLowerCase()) === 0; },
    starts_with: function (t, q) { return t.indexOf(q) === 0; },
    equals_ignore_case: function (t, q) { return t.toLowerCase() === q.toLowerCase(); },
    equals: function (t, q) { return t === q; },
    none: function () { return true; }
  };

  // search.cljs highlight-match, as DOM nodes: the first case-insensitive
  // occurrence of the query in <b>.
  function highlight(node, text, q) {
    var i = q ? text.toLowerCase().indexOf(q.toLowerCase()) : -1;
    node.textContent = i < 0 ? text : text.slice(0, i);
    if (i < 0) { return; }
    var b = document.createElement("b");
    b.textContent = text.slice(i, i + q.length);
    node.appendChild(b);
    node.appendChild(document.createTextNode(text.slice(i + q.length)));
  }

  // The items are the server-rendered <li>s (first render, or morphed in
  // by aihtml_form_pickers:set_items); this reads them back.
  function cbRead(el) {
    var st = cbState(el);
    st.items = st.$list.children(".ah-combobox-item").map(function () {
      var it = {
        el: this,
        value: this.getAttribute("data-value"),
        label: this.getAttribute("data-label"),
        disabled: this.getAttribute("aria-disabled") === "true"
      };
      st.labels[it.value] = it.label;
      return it;
    }).get();
  }

  function cbLabel(st, v) {
    return Object.prototype.hasOwnProperty.call(st.labels, v) ? st.labels[v] : v;
  }

  // Selected state of the rows (after picks in multiple mode, or new rows).
  function cbMark(el) {
    var st = cbState(el);
    st.items.forEach(function (it) {
      var sel = st.selected.indexOf(it.value) >= 0;
      var $li = $(it.el).toggleClass("ah-combobox-item-selected", sel)
        .attr("aria-selected", String(sel));
      var $box = $li.children(".ah-combobox-checkbox").toggleClass("ah-combobox-checkbox-checked", sel);
      if (!$box.length) { return; }
      var $icon = $box.children(".ah-combobox-checkbox-icon");
      if (sel && !$icon.length) {
        $box.append($('<span class="ah-combobox-checkbox-icon"></span>').text("✓"));
      } else if (!sel) {
        $icon.remove();
      }
    });
  }

  // popup.cljs update-query!: show the matching rows (all of them for
  // server results), highlight the query, hide empty groups, and show the
  // empty or loading message.
  function cbFilter(el) {
    var st = cbState(el);
    var match = (st.remote || !st.query || st.mode === "none") ? null
      : (MATCH[st.mode] || MATCH.contains_ignore_case);
    st.visible = [];
    st.items.forEach(function (it) {
      var show = !match || match(it.label, st.query);
      it.el.style.display = show ? "" : "none";
      if (show) { st.visible.push(it); }
      highlight(it.el.querySelector(".ah-combobox-item-label"), it.label, st.query);
    });
    st.$list.children(".ah-combobox-group-header").each(function () {
      var $rows = $(this).nextUntil(".ah-combobox-group-header");
      this.style.display = $rows.filter(function () { return this.style.display !== "none"; }).length
        ? "" : "none";
    });
    cbMark(el);
    st.$popup.children(".ah-combobox-empty, .ah-combobox-loading").remove();
    if (st.loading) {
      st.$popup.append($('<div class="ah-combobox-loading"></div>').text("Loading…"));
    } else if (!st.visible.length) {
      st.$popup.append($('<div class="ah-combobox-empty"></div>').text(st.empty));
    }
    st.$list.toggle(!st.loading && st.visible.length > 0);
    cbActive(el, -1);
  }

  function cbActive(el, idx) {
    var st = cbState(el);
    st.active = idx;
    $(st.items.map(function (it) { return it.el; })).removeClass("ah-combobox-item-active");
    var item = idx >= 0 && st.visible[idx] ? st.visible[idx].el : null;
    if (!item) {
      st.active = -1;
      st.$input.removeAttr("aria-activedescendant");
      return;
    }
    $(item).addClass("ah-combobox-item-active");
    st.$input.attr("aria-activedescendant", item.id || null);
    var p = st.$popup[0];               // popup.cljs scroll-item-into-view!
    if (item.offsetTop < p.scrollTop) { p.scrollTop = item.offsetTop; }
    if (item.offsetTop + item.offsetHeight > p.scrollTop + p.clientHeight) {
      p.scrollTop = item.offsetTop + item.offsetHeight - p.clientHeight;
    }
  }

  function cbMove(el, dir) {
    var st = cbState(el);
    var n = st.visible.length;
    if (!n) { return; }
    var i = st.active;
    for (var k = 0; k < n; k++) {
      i = dir > 0 ? (i < n - 1 ? i + 1 : 0) : (i > 0 ? i - 1 : n - 1);
      if (!st.visible[i].disabled) { cbActive(el, i); return; }
    }
  }

  // AH.float: fixed at the field, at least as wide, flipped above when
  // there is no room; update() after the list or the tags change size.
  function cbPosition(el) {
    var st = cbState(el);
    if (!st.open) { return; }
    if (st.float) {
      st.float.update();
    } else {
      st.float = AH.float(st.$popup[0], el, { matchWidth: true });
    }
  }

  function cbBlocked($el) { return $el.hasClass("ah-combobox-disabled"); }

  function cbOpen(el, $el) {
    var st = cbState(el);
    if (cbBlocked($el)) { return; }
    cbFilter(el);
    if (st.open) { cbPosition(el); return; }
    st.open = true;
    st.$popup.addClass("ah-combobox-popup-open");
    cbPosition(el);
    $el.addClass("ah-combobox-open");
    st.$input.attr("aria-expanded", "true");
    $(document).on("mousedown" + st.ns, function (e) {
      if (outside(el, e)) { cbClose(el, $el); }
    });
    $el.trigger("ah:open");
  }

  function cbClose(el, $el) {
    var st = cbState(el);
    if (!st.open) { return; }
    st.open = false;
    st.$popup.removeClass("ah-combobox-popup-open");
    if (st.float) { st.float.stop(); st.float = null; }
    $el.removeClass("ah-combobox-open");
    st.$input.attr("aria-expanded", "false").removeAttr("aria-activedescendant");
    st.active = -1;
    $(document).off(st.ns);
    $el.trigger("ah:close");
  }

  // Tags added in the browser use the server's markup:
  // templates/combobox_tag.mustache
  function cbTags(el) {
    var st = cbState(el);
    st.$input.siblings(".ah-combobox-tag").remove();
    st.selected.forEach(function (v) {
      $(AH.tpl.combobox_tag({ value: v, label: cbLabel(st, v) })).insertBefore(st.$input);
    });
    st.$input.attr("placeholder", st.selected.length ? "" : st.placeholder);
  }

  function cbSync(el, $el, fire) {
    var st = cbState(el);
    if (st.multi) {
      cbTags(el);
    } else {
      st.$input.val(st.selected.length ? cbLabel(st, st.selected[0]) : "");
    }
    publish(el, $el, st.selected.join(","), fire);
  }

  // popup.cljs select-single-item! / toggle-multi-item!
  function cbPick(el, $el, it) {
    var st = cbState(el);
    if (!it || it.disabled) { return; }
    if (st.multi) {
      var i = st.selected.indexOf(it.value);
      if (i >= 0) { st.selected.splice(i, 1); } else { st.selected.push(it.value); }
      cbSync(el, $el, true);
      cbMark(el);
      cbPosition(el);
    } else {
      st.selected = [it.value];
      st.query = "";
      cbSync(el, $el, true);
      cbClose(el, $el);
    }
  }

  // Leaving the field: free text becomes the value, otherwise the text
  // goes back to the selected item's label (an emptied field clears it).
  function cbSettle(el, $el) {
    var st = cbState(el);
    if (st.multi) {
      if (!st.free) { st.$input.val(""); st.query = ""; return; }
      var t = String(st.$input.val()).trim();
      if (t && st.selected.indexOf(t) < 0) { st.selected.push(t); st.labels[t] = t; }
      st.$input.val("");
      st.query = "";
      cbSync(el, $el, true);
      return;
    }
    var text = String(st.$input.val());
    var current = st.selected.length ? cbLabel(st, st.selected[0]) : "";
    st.query = "";
    if (text === current) { return; }
    if (text === "") {
      st.selected = [];
    } else if (st.free) {
      var exact = st.items.filter(function (it) { return it.label === text; })[0];
      st.selected = [exact ? exact.value : text];
      if (!exact) { st.labels[text] = text; }
    }
    cbSync(el, $el, true);
  }

  function cbKey(el, $el, e) {
    var st = cbState(el);
    if (cbBlocked($el)) { return; }
    switch (e.key) {
      case "ArrowDown":
        e.preventDefault();
        if (!st.open || e.altKey) { cbOpen(el, $el); } else { cbMove(el, 1); }
        break;
      case "ArrowUp":
        e.preventDefault();
        if (e.altKey) { cbClose(el, $el); } else if (st.open) { cbMove(el, -1); }
        break;
      case "Enter":
        e.preventDefault();
        if (!st.open) { cbOpen(el, $el); return; }
        if (st.active >= 0) {
          cbPick(el, $el, st.visible[st.active]);
        } else if (st.free) {
          cbSettle(el, $el);
          cbClose(el, $el);
        } else {
          var enabled = st.visible.filter(function (it) { return !it.disabled; });
          if (enabled.length === 1) { cbPick(el, $el, enabled[0]); }
        }
        break;
      case "Escape":
        if (st.open) {
          e.preventDefault();
          cbClose(el, $el);
        } else if (!st.multi) {
          st.$input.val(st.selected.length ? cbLabel(st, st.selected[0]) : "");
          st.query = "";
        }
        break;
      case "Tab":
        if (st.open && st.active >= 0 && !st.multi) {
          cbPick(el, $el, st.visible[st.active]);
        } else {
          cbClose(el, $el);
        }
        break;
      case "Backspace":
        if (st.multi && st.$input.val() === "" && st.selected.length) {
          st.selected.pop();
          cbSync(el, $el, true);
          if (st.open) { cbMark(el); cbPosition(el); }
        }
        break;
      default:
        break;
    }
  }

  function cbSetValue(el, $el, v, fire) {
    var st = cbState(el);
    if (v == null || v === "") { v = []; }
    if (!Array.isArray(v)) { v = st.multi ? String(v).split(",") : [String(v)]; }
    st.selected = v.map(String).slice(0, st.multi ? v.length : 1);
    st.query = "";
    cbSync(el, $el, fire);
    if (st.open) { cbFilter(el); } else { cbMark(el); }
  }

  AH.define("combobox", {
    init: function (el, $el) {
      ensureId(el, "ah-cb");
      var $popup = $el.children(".ah-combobox-popup");
      var st = {
        ns: ".ahcb" + (++seq),
        $input: $el.find("input.ah-combobox-input"),
        $popup: $popup,
        $list: $popup.children(".ah-combobox-list"),
        multi: $el.hasClass("ah-combobox-multiple") || $el.hasClass("ah-combobox-checkboxes"),
        free: $el.hasClass("ah-combobox-free-text"),
        remote: el.hasAttribute("data-ah-remote"),
        mode: el.getAttribute("data-ah-search-mode") || "contains_ignore_case",
        minLength: parseInt(el.getAttribute("data-ah-min-length") || "0", 10) || 0,
        empty: el.getAttribute("data-ah-empty") || "No results found",
        placeholder: el.getAttribute("data-ah-placeholder") || "",
        items: [], labels: {}, selected: [], visible: [],
        query: "", open: false, active: -1, loading: false
      };
      $.data(el, "ah-cb", st);
      if (!st.multi) { st.placeholder = st.$input.attr("placeholder") || ""; }
      cbRead(el);
      $el.find(".ah-combobox-tag-close").each(function () {
        var v = this.getAttribute("data-value");
        if (!(v in st.labels)) { st.labels[v] = $(this).siblings(".ah-combobox-tag-text").text(); }
      });
      var v = el.getAttribute("data-ah-value") || "";
      st.selected = v === "" ? [] : (st.multi ? v.split(",") : [v]);
      if (!st.multi && st.selected.length && !(st.selected[0] in st.labels)) {
        st.labels[st.selected[0]] = st.$input.val();
      }
      st.$input.attr("data-combobox", el.id).attr("aria-controls", st.$list.attr("id") || null);

      st.$input
        .on("focus" + NS, function () { $el.addClass("ah-combobox-focused"); })
        .on("blur" + NS, function () {
          $el.removeClass("ah-combobox-focused");
          // popup.cljs: close a moment later, after a click on an item
          setTimeout(function () {
            if (document.activeElement !== st.$input[0]) {
              cbClose(el, $el);
              cbSettle(el, $el);
            }
          }, 150);
        })
        .on("input" + NS, function () {
          st.query = String(st.$input.val());
          if (st.query.length < st.minLength) { cbClose(el, $el); return; }
          st.loading = st.remote;     // until set_items answers (itemsLoaded)
          cbOpen(el, $el);
        })
        .on("click" + NS, function (e) {
          e.preventDefault();
          if (st.open) { cbClose(el, $el); } else { cbOpen(el, $el); }
        })
        .on("keydown" + NS, function (e) { cbKey(el, $el, e); })
        // the text field is internal: only the root reports changes
        .on("change" + NS, function (e) { e.stopPropagation(); })
        .on("ah:error" + NS, function () {
          if (st.loading) { st.loading = false; if (st.open) { cbFilter(el); cbPosition(el); } }
        });
      $el.on("mousedown" + NS, ".ah-combobox-arrow, .ah-combobox-tag-close", function (e) {
        e.preventDefault();
      });
      $el.on("click" + NS, ".ah-combobox-arrow", function (e) {
        e.preventDefault();
        st.$input.trigger("focus");
        if (st.open) { cbClose(el, $el); } else { cbOpen(el, $el); }
      });
      $el.on("click" + NS, ".ah-combobox-tag-close", function (e) {
        e.preventDefault();
        e.stopPropagation();
        if (cbBlocked($el)) { return; }
        var i = st.selected.indexOf(this.getAttribute("data-value"));
        if (i >= 0) { st.selected.splice(i, 1); }
        cbSync(el, $el, true);
        if (st.open) { cbMark(el); cbPosition(el); }
      });
      // popup.cljs selects on mousedown, keeping the focus in the field.
      $popup.on("mousedown" + NS, function (e) { e.preventDefault(); })
        .on("mousedown" + NS, ".ah-combobox-item", function () {
          var li = this;
          cbPick(el, $el, st.visible.filter(function (it) { return it.el === li; })[0]);
        })
        .on("mouseenter" + NS, ".ah-combobox-item", function () {
          var li = this;
          cbActive(el, st.visible.map(function (it) { return it.el; }).indexOf(li));
        });
    },
    destroy: function (el) {
      var st = cbState(el);
      if (st) {
        if (st.float) { st.float.stop(); st.float = null; }
        $(document).off(st.ns);
        st.$popup.off(NS);
      }
    },
    methods: {
      // Called by aihtml_form_pickers:set_items/3,4 after it morphed the
      // server-rendered items into the list.
      itemsLoaded: function (el, $el) {
        var st = cbState(el);
        st.loading = false;
        cbRead(el);
        if (st.open || document.activeElement === st.$input[0]) {
          cbOpen(el, $el);
        } else {
          cbFilter(el);
        }
      },
      // A value or a list (multiple); no change event (the server set it).
      setValue: function (el, $el, v) { cbSetValue(el, $el, v, false); },
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      clear: function (el, $el) { cbSetValue(el, $el, [], true); },
      open: function (el, $el) { cbOpen(el, $el); },
      close: function (el, $el) { cbClose(el, $el); }
    }
  });
})(window.jQuery, window.AH);
