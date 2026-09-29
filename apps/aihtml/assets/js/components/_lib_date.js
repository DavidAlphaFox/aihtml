/* Date helpers shared by the date components (calendar, datetime_input,
 * scheduler): AH.lib.date, the browser twin of aihtml_lib_date.
 *
 * Dates are day numbers (days since 1970-01-01) and times minutes since
 * day 0, computed with UTC arithmetic: event times are local wall times
 * without a zone, so there is no DST or zone shifting. */
import AH from "../core.js";

var DAY = 1440;

function dnum(y, m, d) { return Math.round(Date.UTC(y, m - 1, d) / 864e5); }   // m 1..12
function ymd(n) {
  var d = new Date(n * 864e5);
  return [d.getUTCFullYear(), d.getUTCMonth() + 1, d.getUTCDate()];
}
function dow(n) { return ((n + 4) % 7 + 7) % 7; }                               // 0 = Sunday
function sow(n, first) { return n - (dow(n) - first + 7) % 7; }
function lastDay(y, m) { return new Date(Date.UTC(y, m, 0)).getUTCDate(); }
function firstOfMonth(n) { var p = ymd(n); return dnum(p[0], p[1], 1); }
function lastOfMonth(n) { var p = ymd(n); return dnum(p[0], p[1], lastDay(p[0], p[1])); }
// date-fns addMonths: the day clamped to the target month's length
function addMonths(n, k) {
  var p = ymd(n), t = p[0] * 12 + (p[1] - 1) + k;
  var y = Math.floor(t / 12), m = t - y * 12 + 1;
  return dnum(y, m, Math.min(p[2], lastDay(y, m)));
}
function pad(n) { return (n < 10 ? "0" : "") + n; }
function pad4(n) { return ("000" + n).slice(-4); }
function isoDate(n) { var p = ymd(n); return pad4(p[0]) + "-" + pad(p[1]) + "-" + pad(p[2]); }
function todayNum() { var t = new Date(); return dnum(t.getFullYear(), t.getMonth() + 1, t.getDate()); }
function validYmd(y, m, d) { return m >= 1 && m <= 12 && d >= 1 && d <= lastDay(y, m); }
function parseDate(s) {
  var m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(s || "");
  return m && validYmd(+m[1], +m[2], +m[3]) ? dnum(+m[1], +m[2], +m[3]) : null;
}
// ISO date or date-time -> { t: minutes, dateOnly }
function parseTime(s) {
  var m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2}))?/.exec(String(s || ""));
  if (!m || !validYmd(+m[1], +m[2], +m[3])) { return null; }
  var t = dnum(+m[1], +m[2], +m[3]) * DAY;
  if (m[4] === undefined) { return { t: t, dateOnly: true }; }
  if (+m[4] > 23 || +m[5] > 59) { return null; }
  return { t: t + (+m[4]) * 60 + (+m[5]), dateOnly: false };
}
function isoTime(t, allDay) {
  var d = Math.floor(t / DAY), r = t - d * DAY;
  if (allDay && r === 0) { return isoDate(d); }
  return isoDate(d) + "T" + pad(Math.floor(r / 60)) + ":" + pad(r % 60);
}

AH.lib = AH.lib || {};
AH.lib.date = {
  DAY: DAY, dnum: dnum, ymd: ymd, dow: dow, sow: sow, lastDay: lastDay,
  firstOfMonth: firstOfMonth, lastOfMonth: lastOfMonth, addMonths: addMonths,
  pad: pad, pad4: pad4, isoDate: isoDate, todayNum: todayNum, validYmd: validYmd,
  parseDate: parseDate, parseTime: parseTime, isoTime: isoTime
};
