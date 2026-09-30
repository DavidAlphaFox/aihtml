/* Behaviour of the timepicker (designs/04-components.md). Ported from
 * sigil: form/timepicker (+ timepicker/math, timepicker/svg).
 *
 * Value-bearing: data-ah-value and the hidden input follow the value, the
 * root fires `change` on commit (detail TimepickerChange: {value}). Native
 * input/change events of the inner text field are stopped at the root so
 * they are not taken for the component's own events. */
import AH from "../core.ts";
import { time12 } from "./_lib_date.ts";
import { PickerPopup, clamp, disabled, drag, fenceNativeEvents, pointerXY, round2, uid } from "./_lib_picker.ts";
import "virtual:ah-tpl/timepicker_header";
import "virtual:ah-tpl/timepicker_numbers";

/** Detail of `change`: the committed value, "HH:MM" or "". */
export interface TimepickerChange { value: string; }

type Mode = "hours" | "minutes";
type Period = "am" | "pm";
type Format = "12h" | "24h";

interface Polar { angle: number; radius: number; }

const TWO_PI = 2 * Math.PI;
const CX = 130, CY = 130, OUTER_R = 105, INNER_R = 70;
const TP = "ah-timepicker";
const KEY_DIR: Record<string, number> = { ArrowUp: 1, ArrowRight: 1, ArrowDown: -1, ArrowLeft: -1 };

function pad2(n: number): string { return n < 10 ? "0" + n : String(n); }

function to12(h: number): { h12: number; period: Period } {
  if (h === 0) { return { h12: 12, period: "am" }; }
  if (h < 12) { return { h12: h, period: "am" }; }
  if (h === 12) { return { h12: 12, period: "pm" }; }
  return { h12: h - 12, period: "pm" };
}

function to24(h12: number, period: Period): number {
  if (period === "am") { return h12 === 12 ? 0 : h12; }
  return h12 === 12 ? 12 : h12 + 12;
}

function angleXY(angle: number, r: number): { x: number; y: number } {
  return { x: CX + r * Math.sin(angle), y: CY - r * Math.cos(angle) };
}

/** "14:30", "14:30:00", "2:30 pm", "2 pm", "1430" -> minutes of the day */
// The page language's AM / PM texts (上午, 下午), before or after the time,
// read as the "am" / "pm" parseTime knows.
function unlocalize(s: string): string {
  const am = AH.format("am", "AM"), pm = AH.format("pm", "PM");
  if (am && s.indexOf(am) >= 0) { return s.split(am).join("") + " am"; }
  if (pm && s.indexOf(pm) >= 0) { return s.split(pm).join("") + " pm"; }
  return s;
}

function parseTime(s: string | null | undefined): number | null {
  const m = /^\s*(\d{1,2})(?::?(\d{2}))?(?::\d{2})?\s*([ap])?\.?m?\.?\s*$/i.exec(unlocalize(s || ""));
  if (!m) { return null; }
  let h = parseInt(m[1], 10);
  const min = m[2] ? parseInt(m[2], 10) : 0;
  if (min > 59) { return null; }
  if (m[3]) {
    if (h < 1 || h > 12) { return null; }
    h = to24(h, m[3].toLowerCase() === "a" ? "am" : "pm");
  } else if (h > 23) {
    return null;
  }
  return h * 60 + min;
}

/** One number on the clock face. */
interface Face { label: string; val: number; r: number; inner: boolean; ok: boolean; angle: number; }

class TimepickerController extends AH.Controller {
  #mode: Mode = "hours";
  #format: Format = "12h";
  #step = 5;
  #auto = true;
  #lo = 0;
  #hi = 24 * 60 - 1;
  #h = 12;
  #m = 0;
  #input: HTMLInputElement | null = null;
  #svg: SVGSVGElement | null = null;
  #hasPopup = false;
  #popup = new PickerPopup(this, TP);
  #dragMode: Mode | null = null;

  override setup(): void {
    const el = this.element;
    const lo = parseTime(el.getAttribute("data-min"));
    const hi = parseTime(el.getAttribute("data-max"));
    this.#mode = "hours";
    this.#format = el.getAttribute("data-format") === "24h" ? "24h" : "12h";
    this.#step = parseInt(el.getAttribute("data-step") || "", 10) || 5;
    this.#auto = el.getAttribute("data-auto-switch") !== "false";
    this.#lo = lo === null ? 0 : lo;
    this.#hi = hi === null ? 24 * 60 - 1 : hi;
    this.#h = 12;
    this.#m = 0;
    this.load();
    const input = this.#input = el.querySelector<HTMLInputElement>("." + TP + "-input");
    const popupEl = this.#popup.popup;
    const svg = this.#svg = el.querySelector<SVGSVGElement>("." + TP + "-svg");
    this.#hasPopup = !!popupEl;
    if (popupEl) {
      popupEl.id = popupEl.id || uid("ah-tp-popup-");
      if (input) { input.setAttribute("aria-controls", popupEl.id); }
    }
    fenceNativeEvents(this);

    // Field: click toggles; typing a time commits on change.
    this.delegate<MouseEvent, HTMLElement>("mousedown", "." + TP + "-input-area", (e) => {
      const t = e.target instanceof Element ? e.target : null;
      if (t && t.closest("." + TP + "-clear")) { return; }
      if (this.#popup.isOpen) {
        if (t !== input) { e.preventDefault(); this.closePopup(true); }
      } else {
        this.openPopup(false);
      }
    });
    if (input) {
      this.listen(input, "keydown", (e) => {
        if (e.key === "ArrowDown" || (e.key === " " && !input.value)) {
          e.preventDefault();
          this.openPopup(true);
        } else if (e.key === "Enter" && !this.#popup.isOpen) {
          e.preventDefault();
          input.dispatchEvent(new Event("change", { bubbles: true }));
        }
      });
      this.listen(input, "change", () => {
        const text = input.value;
        if (String(text).trim() === "") { this.commit(""); return; }
        let t = parseTime(text);
        if (t === null) {
          input.value = this.display(el.getAttribute("data-ah-value") || "");
          return;
        }
        t = clamp(t, this.#lo, this.#hi);
        this.#h = Math.floor(t / 60);
        this.#m = t % 60;
        this.render();
        this.commit(this.current());
      });
    }
    this.delegate<MouseEvent, HTMLElement>("click", "." + TP + "-clear", (e) => {
      e.preventDefault();
      this.commit("");
      this.closePopup(false);
      if (input) { input.focus(); }
    });
    this.listen(el, "keydown", (e) => {
      if (e.key === "Escape" && this.#popup.isOpen) {
        e.preventDefault();
        e.stopPropagation();
        this.closePopup(true);
      }
    });

    // Header: hours / minutes / AM / PM (sigil setup-header-clicks!).
    this.delegate<MouseEvent, HTMLElement>("click", "." + TP + "-header [data-action]", (_e, t) => {
      this.headerAction(t.getAttribute("data-action"));
    });
    this.delegate<KeyboardEvent, HTMLElement>("keydown", "." + TP + "-header [data-action]", (e, t) => {
      if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        this.headerAction(t.getAttribute("data-action"));
      }
    });

    // Clock: drag (sigil setup-clock-drag!) and keyboard.
    drag(this, svg, (e) => {
      const polar = this.fromPolar(e);
      if (polar.radius <= 20) { return false; }
      this.#dragMode = this.#mode;
      if (this.applyPolar(polar)) { this.render(); }
      return true;
    }, (e) => {
      if (this.applyPolar(this.fromPolar(e))) { this.render(); }
    }, () => {
      this.commit(this.current());
      if (this.#dragMode === "hours" && this.#auto) {
        this.switchMode("minutes");
      } else if (this.#dragMode === "minutes" && this.#hasPopup) {
        this.closePopup(true);
      }
      this.#dragMode = null;
    });
    if (svg) {
      this.listen<KeyboardEvent>(svg, "keydown", (e) => { this.clockKey(e); });
    }
    if (!this.#hasPopup) { this.render(); }
  }

  override teardown(): void { this.#popup.stop(); }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }
  // Set the value without firing change (server driven).
  setValue(v: unknown): void {
    const t = parseTime(v == null ? "" : String(v));
    if (t === null) { this.commit("", true); return; }
    this.#h = Math.floor(t / 60);
    this.#m = t % 60;
    this.render();
    this.commit(this.current(), true);
  }
  clear(): void { this.commit(""); }
  open(): void { this.openPopup(true); }
  close(): void { this.closePopup(false); }
  setMode(mode: unknown): void { this.switchMode(mode === "minutes" ? "minutes" : "hours"); }

  // ---- popup ----

  private openPopup(focusClock: boolean): void {
    this.#popup.open(this.#input, () => {
      this.#mode = "hours";
      this.load();
      this.render();
    });
    if (focusClock && this.#svg) { this.#svg.focus(); }
  }

  private closePopup(refocus: boolean): void { this.#popup.close(this.#input, refocus); }

  // ---- state ----

  private allowed(t: number): boolean { return t >= this.#lo && t <= this.#hi; }

  private hourAllowed(h: number): boolean {
    for (let m = 0; m < 60; m += this.#step) {
      if (this.allowed(h * 60 + m)) { return true; }
    }
    return false;
  }

  private clampState(): void {
    const t = clamp(this.#h * 60 + this.#m, this.#lo, this.#hi);
    this.#h = Math.floor(t / 60);
    this.#m = t % 60;
  }

  private display(value: string): string {
    if (value === "") { return ""; }
    const t = parseTime(value) ?? 0;
    const h = Math.floor(t / 60), m = t % 60;
    if (this.#format === "24h") { return pad2(h) + ":" + pad2(m); }
    const p = to12(h);
    return time12(p.h12 + ":" + pad2(m), p.period === "am" ? AH.format("am", "AM") : AH.format("pm", "PM"));
  }

  private current(): string { return pad2(this.#h) + ":" + pad2(this.#m); }

  // View data for templates/timepicker_header.mustache, as the server
  // builds it in aihtml_timepicker:time_header/5.
  private headerView(isDisabled: boolean): unknown {
    const p = to12(this.#h);
    return {
      hours: this.#format === "24h" ? pad2(this.#h) : String(p.h12),
      minutes: pad2(this.#m),
      hours_active: this.#mode === "hours",
      minutes_active: this.#mode === "minutes",
      twelve: this.#format === "12h",
      am: p.period === "am",
      pm: p.period === "pm",
      disabled: isDisabled,
      tabindex: isDisabled ? -1 : 0,
      txt_hours: AH.t("common", "hours", "Hours"),
      txt_minutes: AH.t("common", "minutes", "Minutes"),
      txt_am: AH.format("am", "AM"),
      txt_pm: AH.format("pm", "PM"),
      // the AM / PM part first when the language writes it first (上午9:30)
      period_first: AH.format("time_12h", "{time} {ampm}").indexOf("{ampm}") <
        AH.format("time_12h", "{time} {ampm}").indexOf("{time}")
    };
  }

  // Redraw header and clock from the state (sigil sync-header!, sync-clock!).
  private render(): void {
    const el = this.element;
    const header = el.querySelector("." + TP + "-header");
    if (header) {
      const focused = header.querySelector(":focus");
      const focusedAction = focused && focused.getAttribute("data-action");
      // Same markup as the server's first render: templates/timepicker_header.mustache
      header.innerHTML = AH.tpl.timepicker_header(this.headerView(disabled(el)));
      if (focusedAction) {
        const again = header.querySelector<HTMLElement>("[data-action='" + focusedAction + "']");
        if (again) { again.focus(); }
      }
    }

    const svg = el.querySelector("." + TP + "-svg");
    if (!svg) { return; }
    const g = svg.querySelector("." + TP + "-numbers");
    const p = to12(this.#h);
    const items: Face[] = [];
    let angle: number, r: number, selected: number;
    if (this.#mode === "hours") {
      for (let i = 1; i <= 12; i++) {
        items.push({ label: String(i), val: i, r: OUTER_R, inner: false, angle: 0,
                     ok: this.hourAllowed(this.#format === "24h" ? i : to24(i, p.period)) });
      }
      if (this.#format === "24h") {
        [0, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23].forEach((v) => {
          items.push({ label: pad2(v), val: v, r: INNER_R, inner: true, angle: 0, ok: this.hourAllowed(v) });
        });
      }
      const disp = this.#format === "24h" ? this.#h % 12 : p.h12 % 12;
      angle = disp / 12 * TWO_PI;
      r = (this.#format === "24h" && (this.#h === 0 || this.#h >= 13)) ? INNER_R : OUTER_R;
      selected = this.#format === "24h" ? this.#h : p.h12;
      items.forEach((it) => { it.angle = (it.val % 12) / 12 * TWO_PI; });
    } else {
      for (let m = 0; m < 60; m += Math.max(this.#step, 5)) {
        items.push({ label: pad2(m), val: m, r: OUTER_R, inner: false, angle: m / 60 * TWO_PI,
                     ok: this.allowed(this.#h * 60 + m) });
      }
      angle = this.#m / 60 * TWO_PI;
      r = OUTER_R;
      selected = this.#m;
    }
    // Same markup as the server's first render: templates/timepicker_numbers.mustache
    // (innerHTML on an SVG element parses its children as SVG).
    if (g) {
      g.innerHTML = AH.tpl.timepicker_numbers({ numbers: items.map((it) => {
        const xy = angleXY(it.angle, it.r);
        return { label: it.label, val: it.val, x: String(round2(xy.x)), y: String(round2(xy.y)),
                 inner: it.inner, selected: it.val === selected, disabled: !it.ok };
      }) });
    }
    const end = angleXY(angle, r);
    const hand = svg.querySelector("." + TP + "-hand");
    if (hand) {
      hand.setAttribute("x2", String(round2(end.x)));
      hand.setAttribute("y2", String(round2(end.y)));
    }
    const sel = svg.querySelector("." + TP + "-selection");
    if (sel) {
      sel.setAttribute("cx", String(round2(end.x)));
      sel.setAttribute("cy", String(round2(end.y)));
    }
    const hoursMode = this.#mode === "hours";
    svg.setAttribute("aria-label", hoursMode ? AH.t("common", "hours", "Hours") : AH.t("common", "minutes", "Minutes"));
    svg.setAttribute("aria-valuemax", hoursMode ? "23" : "59");
    svg.setAttribute("aria-valuenow", String(hoursMode ? this.#h : this.#m));
    svg.setAttribute("aria-valuetext", hoursMode ? String(selected) : pad2(this.#m));
  }

  // Commit a value ("HH:MM" or ""): data-ah-value, hidden input, field
  // text, clear button; `change` when it differs from the current one.
  private commit(value: string, silent?: boolean): void {
    const el = this.element;
    const old = el.getAttribute("data-ah-value") || "";
    el.setAttribute("data-ah-value", value);
    const hidden = el.querySelector<HTMLInputElement>(":scope > input[type=hidden]");
    if (hidden) { hidden.value = value; }
    el.querySelectorAll<HTMLInputElement>("." + TP + "-input").forEach((i) => { i.value = this.display(value); });
    el.querySelectorAll<HTMLElement>("." + TP + "-clear").forEach((b) => { b.hidden = value === ""; });
    if (!silent && value !== old) {
      this.fire<TimepickerChange>("change", { value });
    }
  }

  // Load the committed value into the state, or now (rounded to the step)
  // when empty, like sigil's default.
  private load(): void {
    let t = parseTime(this.element.getAttribute("data-ah-value"));
    if (t === null) {
      const now = new Date();
      t = now.getHours() * 60 + Math.round(now.getMinutes() / this.#step) * this.#step;
      t = t % (24 * 60);
    }
    this.#h = Math.floor(t / 60);
    this.#m = t % 60;
    this.clampState();
  }

  private fromPolar(e: PointerEvent): Polar {
    const rect = this.#svg ? this.#svg.getBoundingClientRect() : new DOMRect();
    const pt = pointerXY(e);
    const dx = (pt.x - rect.left) / rect.width * 260 - CX;
    const dy = (pt.y - rect.top) / rect.height * 260 - CY;
    let angle = Math.atan2(dx, -dy);
    if (angle < 0) { angle += TWO_PI; }
    return { angle, radius: Math.sqrt(dx * dx + dy * dy) };
  }

  // sigil compute-from-polar with snap-hour / snap-minute.
  private applyPolar(polar: Polar): boolean {
    if (this.#mode === "hours") {
      const idx = Math.round(polar.angle / (TWO_PI / 12)) % 12;
      let h: number;
      if (this.#format === "24h") {
        h = polar.radius < 87.5 ? (idx === 0 ? 0 : idx + 12) : (idx === 0 ? 12 : idx);
      } else {
        h = to24(idx === 0 ? 12 : idx, to12(this.#h).period);
      }
      if (!this.hourAllowed(h)) { return false; }
      this.#h = h;
      this.clampState();
    } else {
      const raw = Math.round(polar.angle / (TWO_PI / 60)) % 60;
      const m = (Math.round(raw / this.#step) * this.#step) % 60;
      if (!this.allowed(this.#h * 60 + m)) { return false; }
      this.#m = m;
    }
    return true;
  }

  // Arrow keys: next allowed hour / minute step in direction dir.
  private stepBy(dir: number): boolean {
    if (this.#mode === "hours") {
      let h = this.#h;
      for (let i = 0; i < 24; i++) {
        h = (h + dir + 24) % 24;
        if (this.hourAllowed(h)) { this.#h = h; this.clampState(); return true; }
      }
    } else {
      const step = this.#step;
      let m = this.#m - (this.#m % step);
      if (dir < 0 && m !== this.#m) { m += step; }
      for (let i = 0; i < 60; i++) {
        m = (m + dir * step + 60) % 60;
        if (this.allowed(this.#h * 60 + m)) { this.#m = m; return true; }
      }
    }
    return false;
  }

  private switchMode(mode: Mode): void {
    this.#mode = mode;
    this.render();
  }

  private setPeriod(period: Period): void {
    const h = to24(to12(this.#h).h12, period);
    const t = clamp(h * 60 + this.#m, this.#lo, this.#hi);
    this.#h = Math.floor(t / 60);
    this.#m = t % 60;
    this.render();
    this.commit(this.current());
  }

  private headerAction(action: string | null): void {
    if (disabled(this.element)) { return; }
    if (action === "select-hours") { this.switchMode("hours"); }
    else if (action === "select-minutes") { this.switchMode("minutes"); }
    else if (action === "set-am") { this.setPeriod("am"); }
    else if (action === "set-pm") { this.setPeriod("pm"); }
  }

  private clockKey(e: KeyboardEvent): void {
    if (disabled(this.element)) { return; }
    const dir = KEY_DIR[e.key];
    if (dir) {
      e.preventDefault();
      if (this.stepBy(dir)) {
        this.render();
        this.commit(this.current());
      }
    } else if (e.key === "Home" || e.key === "End") {
      e.preventDefault();
      const t = e.key === "Home" ? this.#lo : this.#hi;
      if (this.#mode === "hours") {
        this.#h = Math.floor(t / 60);
        this.clampState();
      } else {
        const base = this.#h * 60;
        let m = e.key === "Home" ? 0 : 60 - this.#step;
        while (!this.allowed(base + m) && m >= 0 && m < 60) { m += e.key === "Home" ? this.#step : -this.#step; }
        if (m >= 0 && m < 60) { this.#m = m; }
      }
      this.render();
      this.commit(this.current());
    } else if (e.key === "Enter" || e.key === " ") {
      e.preventDefault();
      this.commit(this.current());
      if (this.#mode === "hours") {
        this.switchMode("minutes");
      } else if (this.#hasPopup) {
        this.closePopup(true);
      }
    }
  }
}

AH.register("timepicker", TimepickerController);
