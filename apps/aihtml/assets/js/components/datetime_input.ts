/* Behaviour of the datetime_input (designs/04-components.md). Ported from
 * sigil: form/datetime_input (+ format, editor, dropdown). Dates are day
 * numbers (_lib_date).
 *
 * Value-bearing: data-ah-value and the hidden input; `input` on the root
 * while editing, `change` on leaving (or picking in the drop-down);
 * `ah:open` / `ah:close` around the drop-down calendar. */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import {
  addMonths, dnum, isoDate, lastDay, pad, pad4, parseDate, sow, todayNum, validYmd, ymd
} from "./_lib_date.ts";
import "virtual:ah-tpl/datetime_input_calendar";

type LabelValue = string | string[];

const DTI_LABELS = {
  months: ["January", "February", "March", "April", "May", "June", "July",
           "August", "September", "October", "November", "December"],
  weekdays: ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"],
  title: "MMMM yyyy", time: "Time",
  prev_month: "Previous month", next_month: "Next month"
};
type Labels = typeof DTI_LABELS;

// The defaults, overridden by the labels of data-ah-labels of the same
// kind (a text or a list of texts).
function readLabels<T extends Record<string, LabelValue>>(el: Element, dflt: T): T {
  const out: Record<string, LabelValue> = { ...dflt };
  let raw: unknown = null;
  try { raw = JSON.parse(el.getAttribute("data-ah-labels") || "null"); } catch (err) { raw = null; }
  if (typeof raw === "object" && raw !== null) {
    const r = raw as Record<string, unknown>;
    Object.keys(r).forEach((k) => {
      const v = r[k], d = out[k];
      if (typeof v === "string" && (d === undefined || typeof d === "string")) { out[k] = v; }
      if (Array.isArray(v) && (d === undefined || Array.isArray(d))) { out[k] = v.map(String); }
    });
  }
  return out as T;
}

type SegType = "year" | "year2" | "month" | "day" | "hour" | "hour12" | "minute" | "second" | "ampm";

/** A part of the format: an editable field or literal text. */
interface Seg {
  type: SegType | "literal";
  start: number;
  end: number;
  len: number;
  min: number;
  max: number;
  editable: boolean;
  /** the text of a literal */
  text: string;
}

/** What the format shows. */
interface Kind { date: boolean; time: boolean; sec: boolean; }

/** A value: year, month (1..12), day, hours, minutes, seconds. */
interface Val { y: number; mo: number; d: number; h: number; mi: number; s: number; }

// [pattern, type, min, max]; single letters are two digits wide, as in
// segments/1 of the Erlang side.
const TOKENS: [string, SegType, number, number][] = [
  ["yyyy", "year", 1900, 2100], ["yy", "year2", 0, 99], ["MM", "month", 1, 12],
  ["M", "month", 1, 12], ["dd", "day", 1, 31], ["d", "day", 1, 31],
  ["HH", "hour", 0, 23], ["H", "hour", 0, 23], ["hh", "hour12", 1, 12],
  ["h", "hour12", 1, 12], ["mm", "minute", 0, 59], ["m", "minute", 0, 59],
  ["ss", "second", 0, 59], ["s", "second", 0, 59], ["aa", "ampm", 0, 1],
  ["a", "ampm", 0, 1]];
const SEG_NAMES: Record<SegType, string> = {
  year: "Year", year2: "Year", month: "Month", day: "Day", hour: "Hour",
  hour12: "Hour", minute: "Minute", second: "Second", ampm: "AM/PM"
};

function cls(base: string, opts: [boolean, string][]): string {
  return base + opts.filter((o) => o[0]).map((o) => " " + o[1]).join("");
}

function h12(h: number): number { return h === 0 ? 12 : (h > 12 ? h - 12 : h); }

// format.cljs parse-format: [{type, start, end, len, min, max, editable}]
function dtiSegments(f: string): Seg[] {
  const segs: Seg[] = [];
  let pos = 0, i = 0;
  while (i < f.length) {
    const tok = TOKENS.find((t) => f.substr(i, t[0].length) === t[0]);
    if (tok) {
      const len = tok[1] === "year" ? 4 : 2;
      segs.push({ type: tok[1], start: pos, end: pos + len, len, min: tok[2], max: tok[3],
                  editable: true, text: "" });
      pos += len;
      i += tok[0].length;
    } else {
      const last = segs[segs.length - 1];
      if (last && !last.editable) { last.text += f[i]; last.end++; last.len++; } else {
        segs.push({ type: "literal", text: f[i], start: pos, end: pos + 1, len: 1, min: 0, max: 0,
                    editable: false });
      }
      pos++;
      i++;
    }
  }
  return segs;
}

function dtiKind(segs: readonly Seg[]): Kind {
  let date = false, time = false, sec = false;
  segs.forEach((s) => {
    if (/^(year|year2|month|day)$/.test(s.type)) { date = true; }
    if (/^(hour|hour12|minute|second|ampm)$/.test(s.type)) { time = true; }
    if (s.type === "second") { sec = true; }
  });
  return { date: date || !time, time, sec };
}

function dtiParse(raw: unknown, kind: Kind): Val | null {
  const s = String(raw || "");
  let m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2})(?::(\d{2}))?)?$/.exec(s);
  if (m && validYmd(+m[1], +m[2], +m[3])) {
    return { y: +m[1], mo: +m[2], d: +m[3], h: +(m[4] || 0), mi: +(m[5] || 0), s: +(m[6] || 0) };
  }
  m = /^(\d{2}):(\d{2})(?::(\d{2}))?$/.exec(s);
  if (m && kind.time) {
    const t = ymd(todayNum());
    return { y: t[0], mo: t[1], d: t[2], h: +m[1], mi: +m[2], s: +(m[3] || 0) };
  }
  return null;
}

function dtiIso(v: Val | null, kind: Kind): string {
  if (!v) { return ""; }
  const date = pad4(v.y) + "-" + pad(v.mo) + "-" + pad(v.d);
  const time = pad(v.h) + ":" + pad(v.mi) + (kind.sec ? ":" + pad(v.s) : "");
  return !kind.time ? date : (kind.date ? date + "T" + time : time);
}

// A number that orders values of this kind.
function dtiOrd(v: Val, kind: Kind): number {
  const t = v.h * 3600 + v.mi * 60 + v.s, d = dnum(v.y, v.mo, v.d);
  return !kind.time ? d : (kind.date ? d * 86400 + t : t);
}

function segValue(v: Val, seg: Seg): number {
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
    default: return 0;
  }
}

// The day is clamped to the month's length (sigil's js/Date rolls over).
function setSeg(v0: Val, seg: Seg, val: number): Val {
  const v = { ...v0 };
  switch (seg.type) {
    case "year": v.y = val; break;
    case "year2": v.y = Math.floor(v.y / 100) * 100 + val; break;
    case "month": v.mo = val; break;
    case "day": v.d = val; break;
    case "hour": v.h = val; break;
    case "hour12": {
      const pm = v.h >= 12;
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

function segText(v: Val, seg: Seg): string {
  if (!seg.editable) { return seg.text; }
  if (seg.type === "ampm") { return v.h < 12 ? "AM" : "PM"; }
  const n = segValue(v, seg);
  return seg.len === 4 ? pad4(n) : pad(n);
}

function segMax(v: Val, seg: Seg): number { return seg.type === "day" ? lastDay(v.y, v.mo) : seg.max; }

class DatetimeInputController extends AH.Controller {
  static #seq = 0;

  // server-rendered parts and settings (setup reads them)
  #segs: Seg[] = [];
  #kind: Kind = { date: true, time: false, sec: false };
  #input!: HTMLInputElement;
  #dd: HTMLElement | null = null;
  #min: Val | null = null;
  #max: Val | null = null;
  #first = 0;
  #showTime = false;
  #L: Labels = DTI_LABELS;
  // state
  #value: Val | null = null;
  #committed = "";
  #active: number | null = null;
  #buf = "";
  #open = false;
  #navY = 0;
  #navM = 1;
  #float: FloatHandle | null = null;
  #spin: number | undefined = undefined;
  #outside: AbortController | null = null;

  override setup(): void {
    const el = this.element;
    if (!el.id) { el.id = "ah-dti" + (++DatetimeInputController.#seq); }
    const segs = this.#segs = dtiSegments(el.getAttribute("data-ah-format") || "yyyy-MM-dd");
    const kind = this.#kind = dtiKind(segs);
    const first = parseInt(el.getAttribute("data-ah-first-day") || "0", 10);
    // the server always renders the text field
    const input = this.#input = el.querySelector<HTMLInputElement>("input.ah-dti-input")!;
    const dd = this.#dd = el.querySelector<HTMLElement>(":scope > .ah-dti-dropdown");
    this.#value = dtiParse(el.getAttribute("data-ah-value"), kind);
    this.#min = dtiParse(el.getAttribute("data-ah-min"), kind);
    this.#max = dtiParse(el.getAttribute("data-ah-max"), kind);
    this.#first = first >= 0 && first <= 6 ? first : 0;
    this.#showTime = el.hasAttribute("data-ah-show-time");
    this.#L = readLabels(el, DTI_LABELS);
    this.#active = null;
    this.#buf = "";
    this.#open = false;
    this.#committed = dtiIso(this.#value, kind);
    this.listen(input, "focus", () => {
      el.classList.add("ah-dti-focused");
      if (this.#active === null) { this.#active = this.editable()[0] ?? null; }
      el.querySelectorAll(".ah-dti-label").forEach((l) => { l.classList.add("ah-dti-label-float"); });
      setTimeout(() => { this.selectSeg(); }, 0);
    });
    this.listen(input, "mouseup", () => {
      if (!this.#value) { return; }
      this.focusSeg(this.segAt(input.selectionStart || 0));
    });
    this.listen(input, "keydown", (e) => { this.key(e); });
    // the text field is internal: only the root reports changes
    const stop = (e: Event): void => { e.stopPropagation(); };
    this.listen(input, "change", stop);
    this.listen(input, "input", stop);
    // leaving the component (the time fields of the drop-down are inside)
    this.listen(el, "focusout", (e) => {
      if (e.relatedTarget instanceof Node && el.contains(e.relatedTarget)) { return; }
      setTimeout(() => {
        if (el.contains(document.activeElement)) { return; }
        el.classList.remove("ah-dti-focused");
        this.commit();
        this.close();
      }, 0);
    });
    this.delegate<MouseEvent, HTMLElement>("click", ".ah-dti-cal-btn", () => {
      if (this.blocked()) { return; }
      input.focus();
      if (this.#open) { this.close(); } else { this.open(); }
    });
    this.delegate<MouseEvent, HTMLElement>("mousedown", ".ah-dti-cal-btn", (e) => { e.preventDefault(); });
    // spinner: step, then repeat after 400ms every 120ms while held
    this.delegate<MouseEvent, HTMLElement>("mousedown", ".ah-dti-spin", (e, spin) => {
      e.preventDefault();
      if (this.blocked()) { return; }
      const delta = spin.classList.contains("ah-dti-spin-up") ? 1 : -1;
      if (this.#active === null) { this.#active = this.editable()[0] ?? null; }
      input.focus();
      this.spinStop();
      this.step(delta, false);
      const rep = (): void => { this.step(delta, false); this.#spin = window.setTimeout(rep, 120); };
      this.#spin = window.setTimeout(rep, 400);
    });
    this.delegate<MouseEvent, HTMLElement>("mouseup", ".ah-dti-spin", () => { this.spinStop(); });
    el.querySelectorAll(".ah-dti-spin").forEach((spin) => {
      this.listen(spin, "mouseleave", () => { this.spinStop(); });
    });
    // drop-down: keep the focus in the field, except for the time inputs
    if (dd) {
      this.listen(dd, "mousedown", (e) => {
        if (!(e.target instanceof Element && e.target.matches("input"))) { e.preventDefault(); }
      });
      this.delegate<MouseEvent, HTMLElement>("click", ".ah-dti-cal-day", (_e, day) => {
        if (!day.classList.contains("ah-dti-cal-day-disabled")) { this.pickDay(day.getAttribute("data-date")); }
      }, dd);
      this.delegate<MouseEvent, HTMLElement>("click", "[data-action]", (_e, b) => {
        const n = dnum(this.#navY, this.#navM, 1);
        const t = ymd(addMonths(n, b.getAttribute("data-action") === "prev-month" ? -1 : 1));
        this.#navY = t[0];
        this.#navM = t[1];
        this.renderCal();
      }, dd);
      this.delegate<Event, HTMLInputElement>("change", ".ah-dti-time-input", (e, f) => {
        e.stopPropagation();
        const n = parseInt(f.value, 10);
        if (isNaN(n)) { return; }
        const v = { ...this.ensure() };
        if (f.getAttribute("data-field") === "hours") { v.h = Math.max(0, Math.min(23, n)); }
        else { v.mi = Math.max(0, Math.min(59, n)); }
        this.#value = v;
        this.show();
        this.commit();
      }, dd);
      this.delegate<Event, HTMLElement>("input", ".ah-dti-time-input", (e) => { e.stopPropagation(); }, dd);
      this.delegate<KeyboardEvent, HTMLElement>("keydown", ".ah-dti-time-input", (e, f) => {
        if (e.key === "Escape") { e.preventDefault(); this.close(); input.focus(); }
        if (e.key === "Enter") { e.preventDefault(); f.dispatchEvent(new Event("change", { bubbles: true })); }
      }, dd);
    }
  }

  override teardown(): void {
    this.spinStop();
    this.close();
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v: unknown): void {
    this.#value = v ? dtiParse(v, this.#kind) : null;
    this.#buf = "";
    this.#committed = dtiIso(this.#value, this.#kind);
    this.show();
  }
  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }
  clear(): void {
    this.#value = null;
    this.#buf = "";
    this.show();
    this.commit();
  }

  open(): void {
    const el = this.element, dd = this.#dd;
    if (this.#open || !dd || this.blocked()) { return; }
    const v = this.#value;
    const t = ymd(todayNum());
    this.#navY = v ? v.y : t[0];
    this.#navM = v ? v.mo : t[1];
    this.#open = true;
    dd.hidden = false;
    this.renderCal();
    this.#float = AH.float(dd, el.querySelector(":scope > .ah-dti-row"), { offset: 2 });
    this.#input.setAttribute("aria-expanded", "true");
    this.#outside = new AbortController();
    document.addEventListener("mousedown", (e) => {
      const t = e.target;
      // a target that is gone was inside a part this very press re-rendered
      if (t instanceof Node && t.isConnected !== false && !el.contains(t)) { this.close(); }
    }, { signal: this.#outside.signal });
    this.fire("ah:open");
  }

  close(): void {
    const dd = this.#dd;
    if (!this.#open || !dd) { return; }
    this.#open = false;
    dd.hidden = true;
    dd.innerHTML = "";
    if (this.#float) { this.#float.stop(); this.#float = null; }
    this.#input.setAttribute("aria-expanded", "false");
    if (this.#outside) { this.#outside.abort(); this.#outside = null; }
    this.fire("ah:close");
  }

  // ---- editing ----

  private blocked(): boolean {
    const cl = this.element.classList;
    return cl.contains("ah-dti-disabled") || cl.contains("ah-dti-readonly");
  }

  private editable(): number[] {
    return this.#segs.map((s, i) => s.editable ? i : -1).filter((i) => i >= 0);
  }

  private activeSeg(): Seg | undefined {
    return this.#active === null ? undefined : this.#segs[this.#active];
  }

  private displayText(): string {
    const v = this.#value;
    return v ? this.#segs.map((s) => segText(v, s)).join("") : "";
  }

  private selectSeg(): void {
    const seg = this.activeSeg();
    if (!seg || !this.#value || document.activeElement !== this.#input) { return; }
    try { this.#input.setSelectionRange(seg.start, seg.end); } catch (err) { /* not focused */ }
  }

  // Show the value, publish it (data-ah-value, hidden input) and fire
  // `input' when it changed; `change' fires on leaving (commit).
  private show(): void {
    const el = this.element;
    this.#input.value = this.displayText();
    this.selectSeg();
    const iso = dtiIso(this.#value, this.#kind), old = el.getAttribute("data-ah-value") || "";
    el.setAttribute("data-ah-value", iso);
    const hidden = el.querySelector<HTMLInputElement>(":scope > input[type=hidden]");
    if (hidden) { hidden.value = iso; }
    const float = !!this.#value || el.classList.contains("ah-dti-focused");
    el.querySelectorAll(".ah-dti-label").forEach((l) => { l.classList.toggle("ah-dti-label-float", float); });
    if (iso !== old) { this.fire("input"); }
    if (this.#open) { this.renderCal(); }
  }

  private clampVal(v: Val | null): Val | null {
    if (!v) { return v; }
    const k = dtiOrd(v, this.#kind);
    if (this.#min && k < dtiOrd(this.#min, this.#kind)) { return { ...this.#min }; }
    if (this.#max && k > dtiOrd(this.#max, this.#kind)) { return { ...this.#max }; }
    return v;
  }

  private commit(): void {
    this.flush();
    this.#value = this.clampVal(this.#value);
    this.show();
    const iso = dtiIso(this.#value, this.#kind);
    if (iso !== this.#committed) {
      this.#committed = iso;
      this.fire("change");
    }
  }

  // The value, now when there is none.
  private ensure(): Val {
    if (!this.#value) {
      const n = new Date();
      this.#value = { y: n.getFullYear(), mo: n.getMonth() + 1, d: n.getDate(),
                      h: n.getHours(), mi: n.getMinutes(), s: 0 };
    }
    return this.#value;
  }

  private flush(): void {
    const seg = this.activeSeg();
    if (!seg || !this.#buf) { return; }
    const n = parseInt(this.#buf, 10);
    this.#buf = "";
    if (this.#value && !isNaN(n)) { this.#value = setSeg(this.#value, seg, Math.max(seg.min, Math.min(seg.max, n))); }
  }

  private focusSeg(i: number | null): void {
    this.flush();
    this.#active = i;
    this.show();
  }

  private announce(): void {
    const seg = this.activeSeg(), v = this.#value;
    if (seg && seg.type !== "literal" && v) {
      const text = SEG_NAMES[seg.type] + " " + segText(v, seg);
      this.element.querySelectorAll(".ah-dti-live").forEach((l) => { l.textContent = text; });
    }
  }

  // editor.cljs handle-digit!: buffer until the part is full, then commit
  // and move on
  private digit(ch: string): void {
    const seg = this.activeSeg(), active = this.#active;
    if (!seg || active === null || !seg.editable || seg.type === "ampm") { return; }
    const v = this.ensure();
    this.#buf += ch;
    if (this.#buf.length >= seg.len) {
      const n = parseInt(this.#buf, 10);
      this.#buf = "";
      this.#value = setSeg(v, seg, Math.max(seg.min, Math.min(segMax(v, seg), n)));
      const next = this.editable().filter((i) => i > active)[0];
      if (next !== undefined) { this.#active = next; }
      this.show();
    } else {
      // preview: the typed digits right-aligned in the part
      const shown = this.displayText(), p = " ".repeat(seg.len - this.#buf.length) + this.#buf;
      this.#input.value = shown.slice(0, seg.start) + p + shown.slice(seg.end);
      this.selectSeg();
    }
  }

  private step(delta: number, big: boolean): void {
    const seg = this.activeSeg();
    if (!seg || !seg.editable) { return; }
    this.ensure();
    this.flush();
    const v = this.ensure();
    if (seg.type === "ampm") {
      this.#value = setSeg(v, seg, segValue(v, seg) ? 0 : 1);
    } else {
      const mn = seg.min, mx = segMax(v, seg), cur = segValue(v, seg);
      let n: number;
      if (big) {
        n = mn + (((cur - mn + delta) % (mx - mn + 1)) + (mx - mn + 1)) % (mx - mn + 1);
      } else {
        n = cur + delta;
        n = n < mn ? mx : (n > mx ? mn : n);
      }
      this.#value = setSeg(v, seg, n);
    }
    this.show();
    this.announce();
  }

  private moveSeg(dir: number): boolean {
    const eds = this.editable(), active = this.#active ?? -1;
    const next = dir > 0 ? eds.filter((i) => i > active)[0] : eds.filter((i) => i < active).pop();
    if (next === undefined) { this.flush(); return false; }
    this.focusSeg(next);
    return true;
  }

  // editor.cljs on-keydown
  private key(e: KeyboardEvent): void {
    const k = e.key;
    if (k === "Tab") {
      if (!this.blocked() && this.moveSeg(e.shiftKey ? -1 : 1)) { e.preventDefault(); } else { this.close(); }
      return;
    }
    if (e.ctrlKey || e.metaKey) { return; }
    if (k === "Escape") {
      if (this.#open) { e.preventDefault(); this.close(); }
      return;
    }
    e.preventDefault();
    if (this.blocked()) { return; }
    if ((k === "ArrowDown" && e.altKey) || k === "F4") {
      if (this.#open) { this.close(); } else { this.open(); }
      return;
    }
    if (/^[0-9]$/.test(k)) { this.digit(k); return; }
    const eds = this.editable(), seg = this.activeSeg(), v = this.#value;
    switch (k) {
      case "ArrowUp": this.step(1, false); break;
      case "ArrowDown": this.step(-1, false); break;
      case "PageUp": this.step(10, true); break;
      case "PageDown": this.step(-10, true); break;
      case "ArrowLeft": this.moveSeg(-1); break;
      case "ArrowRight": this.moveSeg(1); break;
      case "Home": this.focusSeg(eds[0] ?? null); break;
      case "End": this.focusSeg(eds[eds.length - 1] ?? null); break;
      case "Backspace":
      case "Delete":
        if (seg && seg.editable && v) {
          this.#buf = "";
          this.#value = setSeg(v, seg, seg.min);
          this.show();
        }
        break;
      case "a": case "A": case "p": case "P":
        if (seg && seg.type === "ampm" && v) {
          this.#value = setSeg(v, seg, /a/i.test(k) ? 0 : 1);
          this.show();
        }
        break;
      default: break;
    }
  }

  // format.cljs segment-at-cursor: the part under the caret, or the nearest
  private segAt(pos: number): number | null {
    const eds = this.editable();
    let best: number | null = eds[0] ?? null, dist = Infinity;
    for (const i of eds) {
      const s = this.#segs[i];
      if (pos >= s.start && pos < s.end) { return i; }
      const dd = Math.min(Math.abs(pos - s.start), Math.abs(pos - s.end));
      if (dd < dist) { dist = dd; best = i; }
    }
    return best;
  }

  private spinStop(): void { clearTimeout(this.#spin); this.#spin = undefined; }

  // ---- drop-down calendar ----

  private calView(): unknown {
    const L = this.#L, y = this.#navY, m = this.#navM, today = todayNum(), v = this.#value;
    const start = sow(dnum(y, m, 1), this.#first);
    const sel = v ? dnum(v.y, v.mo, v.d) : null;
    const mn = this.#min, mx = this.#max;
    const min = mn && this.#kind.date ? dnum(mn.y, mn.mo, mn.d) : null;
    const max = mx && this.#kind.date ? dnum(mx.y, mx.mo, mx.d) : null;
    const days: unknown[] = [];
    for (let i = 0; i < 42; i++) {
      const d = start + i, p = ymd(d);
      const dis = (min !== null && d < min) || (max !== null && d > max);
      days.push({
        cls: cls("ah-dti-cal-day", [[d === today, "ah-dti-cal-day-today"], [d === sel, "ah-dti-cal-day-selected"],
                                    [p[1] !== m, "ah-dti-cal-day-other"], [dis, "ah-dti-cal-day-disabled"]]),
        date: isoDate(d), day: String(p[2]), disabled: dis, selected: d === sel
      });
    }
    const weekdays: { label: string }[] = [];
    for (let k = 0; k < 7; k++) { weekdays.push({ label: L.weekdays[(this.#first + k) % 7] }); }
    const title = L.title.replace(/yyyy|MMMM|MM|M/g, (t) =>
      t === "yyyy" ? String(y) : t === "MMMM" ? L.months[m - 1] : t === "MM" ? pad(m) : String(m));
    return { title, prev_month: L.prev_month, next_month: L.next_month, weekdays,
             days, show_time: this.#showTime && !!v, time_label: L.time,
             hours: v ? pad(v.h) : "", minutes: v ? pad(v.mi) : "" };
  }

  private renderCal(): void {
    const dd = this.#dd;
    if (!dd) { return; }
    const focused = document.activeElement;
    const field = focused && dd.contains(focused) ? focused.getAttribute("data-field") : null;
    dd.innerHTML = AH.tpl.datetime_input_calendar(this.calView());
    if (field) {
      const again = dd.querySelector<HTMLElement>('[data-field="' + field + '"]');
      if (again) { again.focus(); }
    }
    if (this.#float) { this.#float.update(); }
  }

  // dropdown.cljs on-day-click: keep the time, clamp, fire change
  private pickDay(iso: string | null): void {
    const d = parseDate(iso);
    if (d === null) { return; }
    const p = ymd(d), v = this.#value || { h: 0, mi: 0, s: 0 };
    const picked = this.clampVal({ y: p[0], mo: p[1], d: p[2], h: v.h, mi: v.mi, s: v.s });
    this.#value = picked;
    if (picked) {
      this.#navY = picked.y;
      this.#navM = picked.mo;
    }
    this.show();
    this.commit();
    if (!this.#showTime) { this.close(); this.#input.focus(); }
  }
}

AH.register("datetime-input", DatetimeInputController);
