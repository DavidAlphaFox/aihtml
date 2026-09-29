/* Recurrence rules (sigil's calendar/recurrence.cljs): AH.lib.rrule, the
 * browser twin of aihtml_lib_rrule. An iCalendar RRULE subset: FREQ,
 * INTERVAL, COUNT, UNTIL, BYDAY, BYMONTHDAY, BYMONTH. Times are minutes
 * since 1970-01-01 (AH.lib.date). */
import AH from "../core.js";
import "./_lib_date.js";

var D = AH.lib.date;
var DAY = D.DAY;
var MAX_ITERS = 5000;
var RDAYS = ["SU", "MO", "TU", "WE", "TH", "FR", "SA"];

function parse(str) {
  var r = {};
  String(str).replace(/^RRULE:/, "").split(";").forEach(function (part) {
    if (!part) { return; }
    var i = part.indexOf("=");
    var k = part.slice(0, i).toUpperCase(), v = part.slice(i + 1);
    switch (k) {
      case "FREQ": r.freq = v.toLowerCase(); break;
      case "INTERVAL": r.interval = parseInt(v, 10); break;
      case "COUNT": r.count = parseInt(v, 10); break;
      case "UNTIL": {
        var m = /^(\d{4})(\d{2})(\d{2})(?:T(\d{2})(\d{2}))?/.exec(v);
        if (m) { r.until = D.dnum(+m[1], +m[2], +m[3]) * DAY + (m[4] ? +m[4] * 60 + (+m[5]) : 0); }
        break;
      }
      case "BYDAY": r.byday = v.split(",").map(function (d) { return RDAYS.indexOf(d.toUpperCase()); }); break;
      case "BYMONTHDAY": r.bymonthday = v.split(",").map(function (d) { return parseInt(d, 10); }); break;
      case "BYMONTH": r.bymonth = v.split(",").map(function (d) { return parseInt(d, 10); }); break;
      default: break;
    }
  });
  return r;
}

function advance(c, freq, i) {
  var d = Math.floor(c / DAY), r = c - d * DAY;
  switch (freq) {
    case "weekly": return c + 7 * i * DAY;
    case "monthly": return D.addMonths(d, i) * DAY + r;
    case "yearly": return D.addMonths(d, 12 * i) * DAY + r;
    default: return c + i * DAY;
  }
}

function matches(c, rule) {
  var d = Math.floor(c / DAY), p = D.ymd(d);
  return (!rule.byday || rule.byday.indexOf(D.dow(d)) >= 0) &&
    (!rule.bymonthday || rule.bymonthday.indexOf(p[2]) >= 0) &&
    (!rule.bymonth || rule.bymonth.indexOf(p[1]) >= 0);
}

// The occurrences [start, end] of a series overlapping [rs, re), minutes.
function expand(s, e, rule, rs, re, ex) {
  if (!rule.freq) { return []; }
  var dur = e - s, I = rule.interval || 1, out = [], iter = 0, count = 0;
  var countOk = function () { return rule.count === undefined || count < rule.count; };
  var untilOk = function (t) { return rule.until === undefined || t <= rule.until; };
  var add = function (c) {
    if (ex.indexOf(D.isoDate(Math.floor(c / DAY))) < 0 && c < re && c + dur > rs) { out.push([c, c + dur]); }
  };
  if (rule.freq === "weekly" && rule.byday && rule.byday.length) {
    var byday = rule.byday.slice().sort(function (a, b) { return a - b; });
    var offset = s % DAY;
    for (var w = D.sow(Math.floor(s / DAY), 1);
         iter < MAX_ITERS && w * DAY < re && untilOk(w * DAY) && countOk(); w += 7 * I) {
      var wd = D.dow(w);
      var cands = byday.map(function (d) { return (w + (d - wd + 7) % 7) * DAY + offset; })
        .sort(function (a, b) { return a - b; });
      for (var k = 0; k < cands.length; k++) {
        var c = cands[k];
        if (countOk() && iter < MAX_ITERS && c >= s && untilOk(c) && c < re) {
          iter++; count++; add(c);
        }
      }
    }
  } else {
    for (var t = s; iter < MAX_ITERS && t < re && untilOk(t) && countOk(); t = advance(t, rule.freq, I)) {
      iter++;
      if (matches(t, rule)) { count++; add(t); }
    }
  }
  return out;
}

// The occurrence suffix of a start time, yyyyMMdd'T'HHmmss: an
// occurrence's id is <series id>_<stamp>.
function stamp(t) {
  var d = Math.floor(t / DAY), p = D.ymd(d), r = t - d * DAY;
  return D.pad4(p[0]) + D.pad(p[1]) + D.pad(p[2]) + "T" + D.pad(Math.floor(r / 60)) + D.pad(r % 60) + "00";
}

AH.lib = AH.lib || {};
AH.lib.rrule = { parse: parse, expand: expand, stamp: stamp };
