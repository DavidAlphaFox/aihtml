/* Behaviours of the display components (designs/04-components.md):
   avatar, badge, chip, time-ago, expandable-text, progressbar,
   progress-circle, kpi-card, timeline, ranking-list, tag-cloud, alert. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  function num(v, dflt) {
    var n = parseFloat(v);
    return isNaN(n) ? dflt : n;
  }

  function clamp(v, lo, hi) {
    return Math.max(lo, Math.min(hi, v));
  }

  // Remove an element the way the runtime does (behaviours destroyed first).
  function drop(el) {
    AH.destroy(el);
    $(el).remove();
  }

  // Enter / Space act as a click on focusable non-button elements.
  function keyClick(e) {
    if (e.key === "Enter" || e.key === " ") {
      e.preventDefault();
      $(e.currentTarget).trigger("click");
    }
  }

  // ------------------------------------------------------------------
  // avatar: a failed image shows the fallback underneath
  // ------------------------------------------------------------------

  AH.define("avatar", {
    init: function (el, $el) {
      $el.find(".ah-avatar__image").each(function () {
        var img = this;
        var broken = function () { $(img).addClass("ah-avatar__image--broken"); };
        // error does not bubble, and may have happened before init
        $(img).on("error" + NS, broken);
        if (img.complete && img.naturalWidth === 0) { broken(); }
      });
    },
    destroy: function (el, $el) {
      $el.find(".ah-avatar__image").off(NS);
    }
  });

  // ------------------------------------------------------------------
  // badge: setCount(n) with the same max / zero rules as the server
  // ------------------------------------------------------------------

  AH.define("badge", {
    methods: {
      setCount: function (el, $el, n) {
        var $ind = $el.children(".ah-badge-indicator");
        if ($ind.attr("data-dot") === "true") { return; }
        var max = num($el.attr("data-ah-max"), 99);
        var isNum = typeof n === "number" || (n !== "" && n !== null && !isNaN(n));
        var v = isNum ? Number(n) : n;
        $ind.text(v === null || v === undefined ? "" : (isNum && v > max ? max + "+" : String(v)));
        var hide = isNum && v === 0 && $el.attr("data-ah-show-zero") !== "true";
        $ind.attr("data-invisible", hide ? "true" : "false");
      }
    }
  });

  // ------------------------------------------------------------------
  // chip: remove button, keyboard
  // ------------------------------------------------------------------

  function removeChip(el, $el) {
    var ev = $.Event("ah:remove");
    $el.trigger(ev, [{ value: $el.attr("data-ah-value") }]);
    if (ev.isDefaultPrevented()) { return false; }
    // value contract: the root's change carries data-ah-value
    $el.trigger("change");
    drop(el);
    return true;
  }

  AH.define("chip", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-chip__delete", function (e) {
        e.stopPropagation();
        if ($el.attr("data-disabled") !== "true") { removeChip(el, $el); }
      });
      $el.on("keydown" + NS, function (e) {
        if (e.target !== el || $el.attr("data-disabled") === "true") { return; }
        if ((e.key === "Backspace" || e.key === "Delete") && $el.find(".ah-chip__delete").length) {
          e.preventDefault();
          var $next = $el.next("[tabindex]").length ? $el.next("[tabindex]") : $el.prev("[tabindex]");
          if (removeChip(el, $el)) { $next.trigger("focus"); }
        } else if ($el.attr("data-clickable") === "true") {
          keyClick(e);
        }
      });
    },
    methods: {
      remove: function (el, $el) { removeChip(el, $el); }
    }
  });

  // ------------------------------------------------------------------
  // time-ago: sigil's format, refreshed every 60 s
  // ------------------------------------------------------------------

  var AGO = { "just-now": "just now", minutes: "{n}m ago", hours: "{n}h ago",
              days: "{n}d ago", months: "{n}mo ago" };

  function formatAgo(el, t) {
    var label = function (k, n) {
      var s = el.getAttribute("data-ah-label-" + k) || AGO[k];
      return n === undefined ? s : s.split("{n}").join(String(n));
    };
    var secs = Math.floor((Date.now() - t) / 1000);
    var mins = Math.floor(secs / 60), hours = Math.floor(mins / 60);
    var days = Math.floor(hours / 24), months = Math.floor(days / 30);
    if (secs < 60) { return label("just-now"); }
    if (mins < 60) { return label("minutes", mins); }
    if (hours < 24) { return label("hours", hours); }
    if (days < 30) { return label("days", days); }
    return label("months", months);
  }

  function renderAgo(el) {
    var t = Date.parse(el.getAttribute("datetime"));
    if (isNaN(t)) { return; }
    el.textContent = formatAgo(el, t);
    if (el.getAttribute("data-ah-title") === "true") {
      el.setAttribute("title", new Date(t).toLocaleString());
    }
  }

  function stopAgo(el) {
    clearInterval($.data(el, "ah-timer"));
    $.removeData(el, "ah-timer");
  }

  AH.define("time-ago", {
    init: function (el) {
      renderAgo(el);
      if (el.getAttribute("data-ah-live") === "false") { return; }
      $.data(el, "ah-timer", setInterval(function () {
        // removed without AH.destroy: stop instead of leaking
        if (!document.documentElement.contains(el)) { stopAgo(el); return; }
        renderAgo(el);
      }, 60000));
    },
    destroy: stopAgo,
    methods: {
      // iso string, Date or epoch milliseconds
      setDate: function (el, $el, d) {
        var t = typeof d === "number" ? d : Date.parse(d instanceof Date ? d.toISOString() : d);
        if (isNaN(t)) { return; }
        el.setAttribute("datetime", new Date(t).toISOString().replace(/\.\d{3}Z$/, "Z"));
        renderAgo(el);
      },
      refresh: function (el) { renderAgo(el); }
    }
  });

  // ------------------------------------------------------------------
  // expandable-text
  // ------------------------------------------------------------------

  function setExpanded(el, $el, on) {
    var $btn = $el.children(".ah-expandable-text__toggle");
    if (!$btn.length || ($el.attr("data-expanded") === "true") === on) { return; }
    $el.attr("data-expanded", on ? "true" : "false");
    $el.find("[data-ah-part=short]").prop("hidden", on);
    $el.find("[data-ah-part=full]").prop("hidden", !on);
    $btn.attr("aria-expanded", on ? "true" : "false")
      .text($btn.attr(on ? "data-ah-collapse-label" : "data-ah-expand-label"));
    $el.trigger("ah:toggle", [on]);
  }

  AH.define("expandable-text", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-expandable-text__toggle", function () {
        setExpanded(el, $el, $el.attr("data-expanded") !== "true");
      });
    },
    methods: {
      toggle: function (el, $el) { setExpanded(el, $el, $el.attr("data-expanded") !== "true"); },
      expand: function (el, $el) { setExpanded(el, $el, true); },
      collapse: function (el, $el) { setExpanded(el, $el, false); }
    }
  });

  // ------------------------------------------------------------------
  // progressbar / progress-circle: setValue
  // ------------------------------------------------------------------

  function pct(v, lo, hi) {
    return hi > lo ? 100 * (v - lo) / (hi - lo) : 0;
  }

  function fireProgress($el, old, v, max) {
    if (old === v) { return; }
    $el.trigger("change", [{ previous: old, value: v }]);
    if (v === max) { $el.trigger("ah:complete", [{ previous: old, value: v }]); }
  }

  AH.define("progressbar", {
    methods: {
      setValue: function (el, $el, value, text) {
        var lo = num($el.attr("data-ah-min"), 0), hi = num($el.attr("data-ah-max"), 100);
        var old = num($el.attr("data-ah-value"), lo);
        var v = clamp(num(value, lo), lo, hi);
        var p = pct(v, lo, hi);
        var dim = $el.hasClass("ah-progressbar-vertical") ? "height" : "width";
        $el.removeClass("ah-progressbar-indeterminate").removeAttr("aria-busy");
        $el.children(".ah-progressbar-value, .ah-progressbar-value-vertical").css(dim, p + "%");
        $el.children(".ah-progressbar-range").each(function () {
          var stop = num(this.getAttribute("data-ah-stop"), hi);
          $(this).css(dim, pct(Math.min(stop, hi, v), lo, hi) + "%");
        });
        var label = text !== undefined && text !== null ? String(text)
          : ($el.attr("data-ah-text") === "custom" ? null : Math.round(p) + "%");
        if (label !== null) { $el.find(".ah-progressbar-text").text(label); }
        $el.attr({ "data-ah-value": v, "aria-valuenow": v,
                   "aria-valuetext": label !== null ? label : Math.round(p) + "%" });
        fireProgress($el, old, v, hi);
      },
      getValue: function (el, $el) { return num($el.attr("data-ah-value"), 0); }
    }
  });

  var CIRC = 2 * Math.PI * 45;

  AH.define("progress-circle", {
    methods: {
      setValue: function (el, $el, value) {
        var old = num($el.attr("data-ah-value"), 0);
        var v = Math.trunc(clamp(num(value, 0), 0, 100));
        $el.removeClass("ah-progress-circle--indeterminate").removeAttr("aria-busy");
        $el.find(".ah-progress-circle-fill").attr("stroke-dashoffset", CIRC * (1 - v / 100));
        $el.find(".ah-progress-circle-value").text(v + "%");
        $el.attr({ "data-ah-value": v, "aria-valuenow": v, "aria-valuetext": v + "%" });
        fireProgress($el, old, v, 100);
      },
      getValue: function (el, $el) { return num($el.attr("data-ah-value"), 0); }
    }
  });

  // ------------------------------------------------------------------
  // kpi-card: setValue / setTrend
  // ------------------------------------------------------------------

  AH.define("kpi-card", {
    methods: {
      setValue: function (el, $el, value) {
        $el.find(".ah-kpi-card-value").text(String(value));
      },
      setTrend: function (el, $el, trend) {
        var t = num(trend, NaN);
        var $t = $el.find(".ah-kpi-card-trend");
        if (isNaN(t) || !$t.length) { return; }
        var up = t > 0, cls = up ? "ah-kpi-card-trend-up" : "ah-kpi-card-trend-down";
        $el.removeClass("ah-kpi-card-trend-up ah-kpi-card-trend-down").addClass(cls);
        $t.children().first().removeClass("ah-kpi-card-trend-up ah-kpi-card-trend-down")
          .addClass(cls);
        $t.find(".ah-kpi-card-trend-value").text((up ? "+" : "") + t.toFixed(1) + "%");
        // swap the arrow: mirror the polylines vertically
        $t.find(".ah-kpi-card-trend-icon polyline").each(function (i) {
          var pts = [["23 6 13.5 15.5 8.5 10.5 1 18", "17 6 23 6 23 12"],
                     ["23 18 13.5 8.5 8.5 13.5 1 6", "17 18 23 18 23 12"]][up ? 0 : 1];
          this.setAttribute("points", pts[i] || pts[0]);
        });
      }
    }
  });

  // ------------------------------------------------------------------
  // timeline: cards with a description expand on click / Enter
  // ------------------------------------------------------------------

  AH.define("timeline", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-timeline-item[ah-collapsible]", function () {
        var $item = $(this).toggleClass("ah-timeline-item-expanded");
        var on = $item.hasClass("ah-timeline-item-expanded");
        $item.attr("aria-expanded", on ? "true" : "false");
        // three grid cells per item
        var cell = $item.closest(".ah-timeline-near-cell, .ah-timeline-far-cell").index();
        $el.trigger("ah:toggle", [on, Math.floor(cell / 3)]);
      });
      $el.on("keydown" + NS, ".ah-timeline-item[ah-collapsible]", keyClick);
    }
  });

  // ------------------------------------------------------------------
  // ranking-list: clickable rows
  // ------------------------------------------------------------------

  AH.define("ranking-list", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-ranking-list__item--clickable", function () {
        $el.trigger("ah:item-click", [{ index: num(this.getAttribute("data-idx"), 0) }]);
      });
      $el.on("keydown" + NS, ".ah-ranking-list__item--clickable", keyClick);
    }
  });

  // ------------------------------------------------------------------
  // tag-cloud: ah:tag-click; links without a url behave as buttons
  // ------------------------------------------------------------------

  function tagItem($el, index) {
    return $el.find(".ah-tagcloud-item[data-index='" + parseInt(index, 10) + "']");
  }

  AH.define("tag-cloud", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-tagcloud-link", function (e) {
        var a = this;
        var ev = $.Event("ah:tag-click");
        $el.trigger(ev, [{
          label: a.getAttribute("data-ah-label"),
          value: num(a.getAttribute("data-ah-weight"), 0),
          url: a.getAttribute("href"),
          index: num($(a).closest(".ah-tagcloud-item").attr("data-index"), 0)
        }]);
        if (ev.isDefaultPrevented() || !a.hasAttribute("href")) { e.preventDefault(); }
      });
      $el.on("keydown" + NS, ".ah-tagcloud-link:not([href])", keyClick);
    },
    methods: {
      hideItem: function (el, $el, i) { tagItem($el, i).hide(); },
      showItem: function (el, $el, i) { tagItem($el, i).show(); }
    }
  });

  // ------------------------------------------------------------------
  // alert: dismiss
  // ------------------------------------------------------------------

  function dismiss(el, $el) {
    var ev = $.Event("ah:dismiss");
    $el.trigger(ev);
    if (!ev.isDefaultPrevented()) { drop(el); }
  }

  AH.define("alert", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-alert-close", function () { dismiss(el, $el); });
    },
    methods: {
      dismiss: dismiss
    }
  });
})(window.jQuery, window.AH);
