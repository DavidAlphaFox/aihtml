/* Recurrence rules (sigil's calendar/recurrence.cljs), the browser twin
 * of aihtml_lib_rrule. An iCalendar RRULE subset: FREQ, INTERVAL, COUNT,
 * UNTIL, BYDAY, BYMONTHDAY, BYMONTH. Times are minutes since 1970-01-01
 * (_lib_date). */
import { DAY, addMonths, dnum, dow, isoDate, pad, pad4, sow, ymd } from "./_lib_date.ts";
import type { Minutes } from "./_lib_date.ts";

const MAX_ITERS = 5000;
const RDAYS = ["SU", "MO", "TU", "WE", "TH", "FR", "SA"];

/** A parsed RRULE; freq is lowercased ("daily", "weekly", ...), byday
 *  holds weekdays (0 = Sunday). */
export interface Rule {
  freq?: string;
  interval?: number;
  count?: number;
  until?: Minutes;
  byday?: number[];
  bymonthday?: number[];
  bymonth?: number[];
}

/** An occurrence: [start, end] in minutes. */
export type Occurrence = [Minutes, Minutes];

/** "RRULE:FREQ=WEEKLY;BYDAY=MO,WE" (the prefix is optional) -> Rule */
export function parse(str: string): Rule {
  const r: Rule = {};
  String(str).replace(/^RRULE:/, "").split(";").forEach((part) => {
    if (!part) { return; }
    const i = part.indexOf("=");
    const k = part.slice(0, i).toUpperCase(), v = part.slice(i + 1);
    switch (k) {
      case "FREQ": r.freq = v.toLowerCase(); break;
      case "INTERVAL": r.interval = parseInt(v, 10); break;
      case "COUNT": r.count = parseInt(v, 10); break;
      case "UNTIL": {
        const m = /^(\d{4})(\d{2})(\d{2})(?:T(\d{2})(\d{2}))?/.exec(v);
        if (m) { r.until = dnum(+m[1], +m[2], +m[3]) * DAY + (m[4] ? +m[4] * 60 + (+m[5]) : 0); }
        break;
      }
      case "BYDAY": r.byday = v.split(",").map((d) => RDAYS.indexOf(d.toUpperCase())); break;
      case "BYMONTHDAY": r.bymonthday = v.split(",").map((d) => parseInt(d, 10)); break;
      case "BYMONTH": r.bymonth = v.split(",").map((d) => parseInt(d, 10)); break;
      default: break;
    }
  });
  return r;
}

function advance(c: Minutes, freq: string, i: number): Minutes {
  const d = Math.floor(c / DAY), r = c - d * DAY;
  switch (freq) {
    case "weekly": return c + 7 * i * DAY;
    case "monthly": return addMonths(d, i) * DAY + r;
    case "yearly": return addMonths(d, 12 * i) * DAY + r;
    default: return c + i * DAY;
  }
}

function matches(c: Minutes, rule: Rule): boolean {
  const d = Math.floor(c / DAY), p = ymd(d);
  return (!rule.byday || rule.byday.indexOf(dow(d)) >= 0) &&
    (!rule.bymonthday || rule.bymonthday.indexOf(p[2]) >= 0) &&
    (!rule.bymonth || rule.bymonth.indexOf(p[1]) >= 0);
}

/** The occurrences [start, end] of a series (first occurrence s..e)
 *  overlapping [rs, re), minutes; ex lists excluded ISO dates. */
export function expand(s: Minutes, e: Minutes, rule: Rule, rs: Minutes, re: Minutes,
                       ex: readonly string[]): Occurrence[] {
  const freq = rule.freq;
  if (!freq) { return []; }
  const dur = e - s, I = rule.interval || 1, out: Occurrence[] = [];
  let iter = 0, count = 0;
  const countOk = (): boolean => rule.count === undefined || count < rule.count;
  const untilOk = (t: Minutes): boolean => rule.until === undefined || t <= rule.until;
  const add = (c: Minutes): void => {
    if (ex.indexOf(isoDate(Math.floor(c / DAY))) < 0 && c < re && c + dur > rs) { out.push([c, c + dur]); }
  };
  if (freq === "weekly" && rule.byday && rule.byday.length) {
    const byday = rule.byday.slice().sort((a, b) => a - b);
    const offset = s % DAY;
    for (let w = sow(Math.floor(s / DAY), 1);
         iter < MAX_ITERS && w * DAY < re && untilOk(w * DAY) && countOk(); w += 7 * I) {
      const wd = dow(w);
      const cands = byday.map((d) => (w + (d - wd + 7) % 7) * DAY + offset).sort((a, b) => a - b);
      for (const c of cands) {
        if (countOk() && iter < MAX_ITERS && c >= s && untilOk(c) && c < re) {
          iter++; count++; add(c);
        }
      }
    }
  } else {
    for (let t = s; iter < MAX_ITERS && t < re && untilOk(t) && countOk(); t = advance(t, freq, I)) {
      iter++;
      if (matches(t, rule)) { count++; add(t); }
    }
  }
  return out;
}

/** The occurrence suffix of a start time, yyyyMMdd'T'HHmmss: an
 *  occurrence's id is <series id>_<stamp>. */
export function stamp(t: Minutes): string {
  const d = Math.floor(t / DAY), p = ymd(d), r = t - d * DAY;
  return pad4(p[0]) + pad(p[1]) + pad(p[2]) + "T" + pad(Math.floor(r / 60)) + pad(r % 60) + "00";
}
