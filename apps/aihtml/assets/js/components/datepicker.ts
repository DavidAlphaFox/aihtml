/* Behaviour of the datepicker (designs/04-components.md). Ported from
 * sigil: form/datepicker (+ calendar, popup, util). Date math is plain JS
 * on local dates.
 *
 * Value-bearing: data-ah-value and the hidden input, then `change` on the
 * root; `ah:open` / `ah:close` when the popup shows / hides. */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import "virtual:ah-tpl/datepicker_month";

const DAY = 86400000;

type LabelValue = string | string[];

const DEFAULT_LABELS = {
  months: ["January", "February", "March", "April", "May", "June", "July",
           "August", "September", "October", "November", "December"],
  months_short: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
  weekdays: ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"],
  title: "MMMM yyyy", today: "Today", clear: "Clear",
  prev_month: "Previous month", next_month: "Next month",
  prev_year: "Previous year", next_year: "Next year"
};
type Labels = typeof DEFAULT_LABELS;

// The defaults, overridden by the labels of data-ah-labels of the same
// kind (a text or a list of texts).
function readLabels<T extends Record<string, LabelValue>>(el: Element, dflt: T): T {
  const out: Record<string, LabelValue> = { ...dflt };
  let raw: unknown = null;
  try { raw = JSON.parse(el.getAttribute("data-ah-labels") || "{}"); } catch (err) { raw = null; }
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

// A mousedown outside the component. A target that is gone was inside
// a list re-rendered by this very mousedown (picking in multiple mode).
function outside(el: Element, e: Event): boolean {
  const t = e.target;
  return t instanceof Node && t.isConnected !== false && !el.contains(t);
}

function pad(n: number): string { return (n < 10 ? "0" : "") + n; }
/** m: 0..11, overflow ok */
function mk(y: number, m: number, d: number): Date { return new Date(y, m, d); }
function iso(d: Date | null): string {
  return d ? d.getFullYear() + "-" + pad(d.getMonth() + 1) + "-" + pad(d.getDate()) : "";
}
function parse(s: string | null | undefined): Date | null {
  const m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(s || "");
  return m ? mk(+m[1], +m[2] - 1, +m[3]) : null;
}
function today(): Date { const n = new Date(); return mk(n.getFullYear(), n.getMonth(), n.getDate()); }
function same(a: Date | null, b: Date | null): boolean { return !!(a && b) && a.getTime() === b.getTime(); }
function addDays(d: Date, n: number): Date { return mk(d.getFullYear(), d.getMonth(), d.getDate() + n); }
// Month arithmetic keeps the day, clamped to the target month (date-fns addMonths).
function addMonths(d: Date, n: number): Date {
  const t = mk(d.getFullYear(), d.getMonth() + n, 1);
  const last = mk(t.getFullYear(), t.getMonth() + 1, 0).getDate();
  return mk(t.getFullYear(), t.getMonth(), Math.min(d.getDate(), last));
}
function startOfWeek(d: Date, first: number): Date { return addDays(d, -((d.getDay() - first + 7) % 7)); }
// date-fns getWeek with weekStartsOn = first, firstWeekContainsDate = 1.
function weekNumber(d: Date, first: number): number {
  const y = d.getFullYear();
  const wy = d >= startOfWeek(mk(y + 1, 0, 1), first) ? y + 1
    : (d >= startOfWeek(mk(y, 0, 1), first) ? y : y - 1);
  const diff = startOfWeek(d, first).getTime() - startOfWeek(mk(wy, 0, 1), first).getTime();
  return Math.round(diff / (7 * DAY)) + 1;
}

// The display formats the Erlang side knows: yyyy yy MMMM MMM MM M dd d.
function format(d: Date | null, fmt: string, L: Labels): string {
  if (!d) { return ""; }
  return fmt.replace(/yyyy|yy|MMMM|MMM|MM|M|dd|d/g, (t) => {
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

class DatepickerController extends AH.Controller {
  static #seq = 0;

  // server-rendered parts and settings (setup reads them)
  #input!: HTMLInputElement;
  #popup!: HTMLElement;
  #range = false;
  #fmt = "yyyy-MM-dd";
  #min: Date | null = null;
  #max: Date | null = null;
  #off = new Set<string>();
  #firstDay = 0;
  #weekNumbers = false;
  #weekends = false;
  #otherMonth = true;
  #L: Labels = DEFAULT_LABELS;
  #inline = false;
  // state
  #open = false;
  #value: Date | null = null;
  #from: Date | null = null;
  #to: Date | null = null;
  #pending: Date | null = null;
  #focus: Date | null = null;
  #viewY = 0;
  #viewM = 0;
  #float: FloatHandle | null = null;
  #outside: AbortController | null = null;

  override setup(): void {
    const el = this.element;
    if (!el.id) { el.id = "ah-dp" + (++DatepickerController.#seq); }
    this.#off = new Set();
    (el.getAttribute("data-ah-disabled-dates") || "").split(",").forEach((d) => {
      if (d) { this.#off.add(d); }
    });
    // the server always renders the text field and the popup
    const input = this.#input = el.querySelector<HTMLInputElement>("input.ah-datepicker-input")!;
    const popup = this.#popup = el.querySelector<HTMLElement>(":scope > .ah-datepicker-popup")!;
    this.#range = el.hasAttribute("data-ah-range");
    this.#fmt = el.getAttribute("data-ah-format") || "yyyy-MM-dd";
    this.#min = parse(el.getAttribute("data-ah-min"));
    this.#max = parse(el.getAttribute("data-ah-max"));
    this.#firstDay = parseInt(el.getAttribute("data-ah-first-day") || "0", 10) || 0;
    this.#weekNumbers = el.hasAttribute("data-ah-week-numbers");
    this.#weekends = el.hasAttribute("data-ah-weekends");
    this.#otherMonth = el.getAttribute("data-ah-other-month-days") !== "false";
    this.#L = readLabels(el, DEFAULT_LABELS);
    this.#inline = el.classList.contains("ah-datepicker-inline");
    this.#open = false;
    this.#value = this.#from = this.#to = this.#pending = this.#focus = null;
    popup.id = el.id + "-popup";
    input.setAttribute("aria-controls", el.id + "-popup");
    const v = el.getAttribute("data-ah-value") || "";
    if (this.#range) {
      const p = v.split(",");
      this.#from = parse(p[0]);
      this.#to = parse(p[1]);
    } else {
      this.#value = parse(v);
    }

    this.listen(input, "focus", () => { el.classList.add("ah-datepicker-focused"); });
    this.listen(input, "blur", () => { el.classList.remove("ah-datepicker-focused"); });
    this.listen(input, "click", (e) => {
      e.preventDefault();
      if (this.#open) { this.close(); } else { this.open(); }
    });
    this.listen(input, "keydown", (e) => { this.key(e); });
    // the text field is internal: only the root reports changes
    const stop = (e: Event): void => { e.stopPropagation(); };
    this.listen(input, "change", stop);
    this.listen(input, "input", stop);
    this.delegate<MouseEvent, HTMLElement>("click", ".ah-datepicker-trigger", (e) => {
      e.preventDefault();
      e.stopPropagation();
      input.focus();
      if (this.#open) { this.close(); } else { this.open(); }
    });
    this.delegate<MouseEvent, HTMLElement>("mousedown", ".ah-datepicker-clear", (e) => { e.preventDefault(); });
    this.delegate<MouseEvent, HTMLElement>("click", ".ah-datepicker-clear", (e) => {
      e.preventDefault();
      e.stopPropagation();
      this.clearValue(true);
    });
    // Keep the focus in the text field while using the calendar.
    this.listen(popup, "mousedown", (e) => {
      e.preventDefault();
      if (this.#inline && !this.blocked()) { input.focus(); }
    });
    // The popup's own delegated clicks run first, then it stops the click.
    this.delegate<MouseEvent, HTMLElement>("click", ".ah-datepicker-day[data-date]", (_e, day) => {
      this.pick(parse(day.getAttribute("data-date")));
    }, popup);
    this.delegate<MouseEvent, HTMLElement>("click", "[data-nav]", (_e, b) => {
      this.navMonths(parseInt(b.getAttribute("data-nav") || "", 10));
    }, popup);
    this.delegate<MouseEvent, HTMLElement>("click", ".ah-datepicker-today-btn", () => {
      this.setFocus(today());
    }, popup);
    this.listen(popup, "click", (e) => { e.stopPropagation(); });
    // mouseenter on a day: mouseover from outside that day
    this.delegate<MouseEvent, HTMLElement>("mouseover", ".ah-datepicker-day[data-date]", (e, day) => {
      if (e.relatedTarget instanceof Node && day.contains(e.relatedTarget)) { return; }
      const d = parse(day.getAttribute("data-date"));
      if (d) { this.hover(d); }
    }, popup);
    if (this.#inline) { this.open(); }
  }

  override teardown(): void { this.stop(); }

  // methods (aihtml_action:call/4, AH.invoke)
  // setValue("2026-09-29"), setValue("from,to") or setValue([from, to]);
  // no change event (the server set it).
  setValue(v: unknown): void {
    const text = Array.isArray(v) ? v.join(",") : String(v || "");
    if (this.#range) {
      const p = text.split(",");
      const a = parse(p[0]), b = parse(p[1]);
      this.#from = a && b && b < a ? b : a;
      this.#to = a && b && b < a ? a : b;
    } else {
      this.#value = parse(text);
    }
    this.#pending = null;
    this.commit(false);
    if (this.#open) { this.render(); }
  }
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }
  clear(): void { this.clearValue(true); }

  open(): void {
    const el = this.element;
    if (this.#open || (this.blocked() && !this.#inline)) { return; }
    this.#pending = null;
    const f = (this.#range ? this.#from : this.#value) || today();
    this.#focus = f;
    this.#viewY = f.getFullYear();
    this.#viewM = f.getMonth();
    this.#open = true;
    this.render();
    if (this.#inline) { return; }        // always shown, in the flow
    this.#popup.style.display = "block";
    this.position();
    el.classList.add("ah-datepicker-open");
    this.#input.setAttribute("aria-expanded", "true");
    this.#outside = new AbortController();
    document.addEventListener("mousedown", (e) => {
      if (outside(el, e)) { this.close(); }
    }, { signal: this.#outside.signal });
    this.fire("ah:open");
  }

  close(): void {
    if (!this.#open || this.#inline) { return; }
    this.#open = false;
    this.#pending = null;
    this.#popup.style.display = "none";
    this.element.classList.remove("ah-datepicker-open");
    this.#input.setAttribute("aria-expanded", "false");
    this.#input.removeAttribute("aria-activedescendant");
    this.stop();
    this.fire("ah:close");
  }

  // ---- internals ----

  private blocked(): boolean {
    const cl = this.element.classList;
    return cl.contains("ah-datepicker-disabled") || cl.contains("ah-datepicker-readonly");
  }

  private isDisabled(d: Date): boolean {
    return !!(this.#min && d < this.#min) || !!(this.#max && d > this.#max) || this.#off.has(iso(d));
  }

  private display(): string {
    if (this.#range) {
      if (!this.#from) { return ""; }
      return format(this.#from, this.#fmt, this.#L) + (this.#to ? " - " + format(this.#to, this.#fmt, this.#L) : "");
    }
    return format(this.#value, this.#fmt, this.#L);
  }

  private isoValue(): string {
    return this.#range ? (this.#from ? iso(this.#from) + "," + iso(this.#to) : "") : iso(this.#value);
  }

  // The view of templates/datepicker_month.mustache: every class, label
  // and id computed here (the Erlang month_view/1 builds the same view for
  // the server's `inline' render).
  private view(): unknown {
    const el = this.element, L = this.#L;
    const y = this.#viewY, m = this.#viewM;
    const first = this.#firstDay;
    const monthStart = mk(y, m, 1);
    const gridStart = startOfWeek(monthStart, first);
    const gridEnd = addDays(startOfWeek(mk(y, m + 1, 0), first), 7);
    const now = today();
    const from = this.#range ? (this.#pending || this.#from) : null;
    const to = this.#range ? (this.#pending ? null : this.#to) : null;
    const weeks: unknown[] = [];
    for (let w = gridStart; w < gridEnd; w = addDays(w, 7)) {
      const days: unknown[] = [];
      for (let k = 0; k < 7; k++) {
        const d = addDays(w, k);
        const other = d.getMonth() !== m;
        if (other && !this.#otherMonth) { days.push({ empty: true }); continue; }
        const dis = this.isDisabled(d);
        const isStart = same(d, from), isEnd = same(d, to);
        const sel = this.#range ? (isStart || isEnd) : same(d, this.#value);
        const inRange = !!(this.#range && from && to && d >= from && d <= to);
        const dow = d.getDay();
        days.push({
          empty: false,
          cls: "ah-datepicker-day" +
            (other ? " ah-datepicker-day-other-month" : "") +
            (same(d, now) ? " ah-datepicker-day-today" : "") +
            (this.#weekends && (dow === 0 || dow === 6) ? " ah-datepicker-day-weekend" : "") +
            (dis ? " ah-datepicker-day-disabled" : "") +
            (sel ? " ah-datepicker-day-selected" : "") +
            (inRange ? " ah-datepicker-day-in-range" : "") +
            (isStart ? " ah-datepicker-day-range-start" : "") +
            (isEnd ? " ah-datepicker-day-range-end" : "") +
            (same(d, this.#focus) ? " ah-datepicker-day-focused" : ""),
          id: el.id + "-d" + iso(d),
          date: iso(d),
          selected: String(sel),
          disabled: String(dis),
          label: format(d, "d MMMM yyyy", L),
          day: String(d.getDate())
        });
      }
      weeks.push({ num: String(weekNumber(w, first)), days });
    }
    const weekdays: { label: string }[] = [];
    for (let i = 0; i < 7; i++) { weekdays.push({ label: L.weekdays[(first + i) % 7] }); }
    return {
      title_id: el.id + "-title",
      title: format(monthStart, L.title, L),
      prev_year: L.prev_year, prev_month: L.prev_month,
      next_month: L.next_month, next_year: L.next_year,
      today: L.today,
      week_numbers: this.#weekNumbers,
      weekdays,
      weeks
    };
  }

  private render(): void {
    this.#popup.innerHTML = AH.tpl.datepicker_month(this.view());
    if (this.#focus) {
      this.#input.setAttribute("aria-activedescendant", this.element.id + "-d" + iso(this.#focus));
    } else {
      this.#input.removeAttribute("aria-activedescendant");
    }
  }

  // popup.cljs position!: at least 280px wide (the viewport minus a
  // margin on small screens); AH.float keeps it fixed under the field (or
  // above, when there is no room), inside the viewport and following
  // scrolls, so clipping ancestors do not cut it.
  private position(): void {
    const w = Math.min(Math.max(this.element.getBoundingClientRect().width, 280), window.innerWidth - 16);
    this.#popup.style.width = w + "px";
    if (this.#float) { this.#float.update(); } else { this.#float = AH.float(this.#popup, this.element); }
  }

  private stop(): void {
    if (this.#float) { this.#float.stop(); this.#float = null; }
    if (this.#outside) { this.#outside.abort(); this.#outside = null; }
  }

  private commit(fire: boolean): void {
    const el = this.element, value = this.isoValue();
    this.#input.value = this.display();
    // value-bearing contract: data-ah-value + hidden input, then `change`
    const old = el.getAttribute("data-ah-value") || "";
    el.setAttribute("data-ah-value", value);
    const hidden = el.querySelector<HTMLInputElement>(":scope > input[type=hidden]");
    if (hidden) { hidden.value = value; }
    if (fire && old !== value) { this.fire("change"); }
  }

  private setFocus(d: Date): void {
    this.#focus = d;
    this.#viewY = d.getFullYear();
    this.#viewM = d.getMonth();
    this.render();
    if (this.#open && !this.#inline) { this.position(); }
  }

  private navMonths(n: number): void {
    const f = this.#focus && this.#focus.getMonth() === this.#viewM ? this.#focus : mk(this.#viewY, this.#viewM, 1);
    this.setFocus(addMonths(f, n));
  }

  // calendar.cljs handle-day-click: single selects and closes; range takes
  // a start, then an end (sorted), then closes.
  private pick(d: Date | null): void {
    if (!d || this.isDisabled(d) || this.blocked()) { return; }
    const a = this.#pending;
    if (!this.#range) {
      this.#value = d;
      this.#focus = d;
      this.commit(true);
      this.close();
      if (this.#inline) { this.render(); }
    } else if (!a) {
      this.#pending = d;
      this.#focus = d;
      this.render();
    } else {
      this.#from = d < a ? d : a;
      this.#to = d < a ? a : d;
      this.#pending = null;
      this.commit(true);
      this.close();
      if (this.#inline) { this.render(); }
    }
  }

  private hover(d: Date): void {
    const p = this.#pending;
    if (!this.#range || !p) { return; }
    const a = p < d ? p : d;
    const b = p < d ? d : p;
    this.#popup.querySelectorAll(".ah-datepicker-day[data-date]").forEach((cell) => {
      const x = parse(cell.getAttribute("data-date"));
      cell.classList.toggle("ah-datepicker-day-hover-range", !!x && x >= a && x <= b);
    });
  }

  // popup.cljs handle-keydown. Up/Down move a week (sigil moves a day),
  // Shift+PageUp/PageDown a year.
  private key(e: KeyboardEvent): void {
    if (this.blocked()) { return; }
    const open = this.#open;
    const f = this.#focus || today();
    switch (e.key) {
      case "ArrowDown":
        e.preventDefault();
        if (e.altKey || !open) { this.open(); } else { this.setFocus(addDays(f, 7)); }
        break;
      case "ArrowUp":
        if (!open) { return; }
        e.preventDefault();
        if (e.altKey) { this.close(); } else { this.setFocus(addDays(f, -7)); }
        break;
      case "ArrowLeft":
        if (open) { e.preventDefault(); this.setFocus(addDays(f, -1)); }
        break;
      case "ArrowRight":
        if (open) { e.preventDefault(); this.setFocus(addDays(f, 1)); }
        break;
      case "Enter":
      case " ":
        e.preventDefault();
        if (open) { this.pick(f); } else { this.open(); }
        break;
      case "Escape":
        if (open) { e.preventDefault(); this.close(); }
        break;
      case "Tab":
        this.close();
        break;
      case "PageUp":
        if (open) { e.preventDefault(); this.navMonths(e.shiftKey ? -12 : -1); }
        break;
      case "PageDown":
        if (open) { e.preventDefault(); this.navMonths(e.shiftKey ? 12 : 1); }
        break;
      case "Home":
        if (open) { e.preventDefault(); this.setFocus(mk(this.#viewY, this.#viewM, 1)); }
        break;
      case "End":
        if (open) { e.preventDefault(); this.setFocus(mk(this.#viewY, this.#viewM + 1, 0)); }
        break;
      case "Backspace":
      case "Delete":
        if (this.element.classList.contains("ah-datepicker-clearable")) { e.preventDefault(); this.clearValue(true); }
        break;
      default:
        break;
    }
  }

  private clearValue(fire: boolean): void {
    this.#value = this.#from = this.#to = this.#pending = null;
    this.commit(fire);
    if (this.#open) { this.render(); }
  }
}

AH.register("datepicker", DatepickerController);
