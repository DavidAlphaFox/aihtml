/* Date helpers shared by the date components (calendar, datetime_input,
 * scheduler), the browser twin of aihtml_lib_date.
 *
 * Dates are day numbers (days since 1970-01-01) and times minutes since
 * day 0, computed with UTC arithmetic: event times are local wall times
 * without a zone, so there is no DST or zone shifting. */

import AH from "../core.ts";

/** A day number: days since 1970-01-01. */
export type DayNum = number;
/** Minutes since day 0. */
export type Minutes = number;

/** Minutes in a day. */
export const DAY = 1440;

/** m is 1..12 */
export function dnum(y: number, m: number, d: number): DayNum { return Math.round(Date.UTC(y, m - 1, d) / 864e5); }
export function ymd(n: DayNum): [number, number, number] {
  const d = new Date(n * 864e5);
  return [d.getUTCFullYear(), d.getUTCMonth() + 1, d.getUTCDate()];
}
/** 0 = Sunday */
export function dow(n: DayNum): number { return ((n + 4) % 7 + 7) % 7; }
/** The first day of n's week, weeks starting on `first' (0 = Sunday). */
export function sow(n: DayNum, first: number): DayNum { return n - (dow(n) - first + 7) % 7; }
export function lastDay(y: number, m: number): number { return new Date(Date.UTC(y, m, 0)).getUTCDate(); }
export function firstOfMonth(n: DayNum): DayNum { const p = ymd(n); return dnum(p[0], p[1], 1); }
export function lastOfMonth(n: DayNum): DayNum { const p = ymd(n); return dnum(p[0], p[1], lastDay(p[0], p[1])); }
/** date-fns addMonths: the day clamped to the target month's length */
export function addMonths(n: DayNum, k: number): DayNum {
  const p = ymd(n), t = p[0] * 12 + (p[1] - 1) + k;
  const y = Math.floor(t / 12), m = t - y * 12 + 1;
  return dnum(y, m, Math.min(p[2], lastDay(y, m)));
}
export function pad(n: number): string { return (n < 10 ? "0" : "") + n; }
/** A 12-hour time with its AM / PM text, in the page language's order
 *  ("10:00 AM", "上午10:00"; time_12h/2 in aihtml_lib_date). */
export function time12(time: string, ampm: string): string {
  return AH.format("time_12h", "{time} {ampm}").split("{time}").join(time).split("{ampm}").join(ampm);
}
export function pad4(n: number): string { return ("000" + n).slice(-4); }
export function isoDate(n: DayNum): string { const p = ymd(n); return pad4(p[0]) + "-" + pad(p[1]) + "-" + pad(p[2]); }
export function todayNum(): DayNum { const t = new Date(); return dnum(t.getFullYear(), t.getMonth() + 1, t.getDate()); }
export function validYmd(y: number, m: number, d: number): boolean { return m >= 1 && m <= 12 && d >= 1 && d <= lastDay(y, m); }
export function parseDate(s: string | null | undefined): DayNum | null {
  const m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(s || "");
  return m && validYmd(+m[1], +m[2], +m[3]) ? dnum(+m[1], +m[2], +m[3]) : null;
}
/** A parsed ISO date or date-time. */
export interface ParsedTime { t: Minutes; dateOnly: boolean; }
/** ISO date or date-time -> { t: minutes, dateOnly } */
export function parseTime(s: unknown): ParsedTime | null {
  const m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2}))?/.exec(String(s || ""));
  if (!m || !validYmd(+m[1], +m[2], +m[3])) { return null; }
  const t = dnum(+m[1], +m[2], +m[3]) * DAY;
  if (m[4] === undefined) { return { t, dateOnly: true }; }
  if (+m[4] > 23 || +m[5] > 59) { return null; }
  return { t: t + (+m[4]) * 60 + (+m[5]), dateOnly: false };
}
export function isoTime(t: Minutes, allDay?: boolean): string {
  const d = Math.floor(t / DAY), r = t - d * DAY;
  if (allDay && r === 0) { return isoDate(d); }
  return isoDate(d) + "T" + pad(Math.floor(r / 60)) + ":" + pad(r % 60);
}
