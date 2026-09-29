/* time-ago behaviour (designs/04-components.md): sigil's format,
   refreshed every 60 s. */
(function ($, AH) {
  "use strict";

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
})(window.jQuery, window.AH);
