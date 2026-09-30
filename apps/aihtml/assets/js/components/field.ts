/* The validator of aihtml_field:validate/1 (designs/04-components.md):
   client-side checks whose messages show in ah_field/4 and ah_form_layout/4
   rows, or as sigil's tooltip bubble. Ported from sigil
   (sigil.components.form.validator). */
// ah-load: [data-ah-validate]
import AH from "../core.ts";
import type { FloatHandle, Placement } from "../core.ts";

// ------------------------------------------------------------------
// validator (aihtml_field:validate/1)
// ------------------------------------------------------------------
//
// Controls carry data-ah-validate='[{"rule":"required"}, ...]'. They are
// checked on their trigger events (default blur) and, once invalid, on
// every input/change until fixed. A form holding such controls is
// checked on submit, and on a click of its submit button: when a check
// fails the event is stopped in the capture phase, so neither the
// browser, data-ah-fetch nor an aihtml action (on/2) sees it.
//
// Errors show as in sigil: the error class on the control plus either a
// tooltip bubble (.ah-validator-hint) or an error label. Inside an
// ah_field/4 row ("auto", the default) the label goes under the control.
//
//   ah:validation-error (detail ValidationError {invalid: [el]}) /
//   ah:validation-success                 native events on the form
//   AH.fn("validate", target) -> bool     check a form or control
//   AH.fn("clearValidation", target)      remove every message

/** Detail of ah:validation-error: the controls that failed, in order. */
export interface ValidationError { invalid: HTMLElement[]; }

/** One entry of data-ah-validate. */
interface Rule { rule: string; args: readonly unknown[]; msg: string | null; }

type Check = (v: string, el: HTMLElement, a: readonly unknown[]) => boolean;

type Native = HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement;

// the messages of the rules, in the page's language
const MESSAGES: Record<string, () => string> = {
  required: () => AH.t("field", "required", "This field is required"),
  email: () => AH.t("field", "email", "Please enter a valid email address"),
  number: () => AH.t("field", "number", "Please enter a number"),
  integer: () => AH.t("field", "integer", "Please enter a whole number"),
  phone: () => AH.t("field", "phone", "Please enter a phone number like (555)555-5555"),
  zip_code: () => AH.t("field", "zip_code", "Please enter a valid ZIP code"),
  ssn: () => AH.t("field", "ssn", "Please enter a valid SSN"),
  not_number: () => AH.t("field", "not_number", "Digits are not allowed"),
  starts_with_letter: () => AH.t("field", "starts_with_letter", "Must start with a letter"),
  min_length: () => AH.t("field", "min_length", "Please enter at least {0} characters"),
  max_length: () => AH.t("field", "max_length", "Please enter at most {0} characters"),
  length: () => AH.t("field", "length", "Please enter {0} to {1} characters"),
  min: () => AH.t("field", "min", "Must be at least {0}"),
  max: () => AH.t("field", "max", "Must be at most {0}"),
  range: () => AH.t("field", "range", "Must be between {0} and {1}"),
  pattern: () => AH.t("field", "pattern", "Please match the requested format"),
  same_as: () => AH.t("field", "same_as", "The values do not match")
};

function trim(s: unknown): string { return String(s === null || s === undefined ? "" : s).trim(); }

function fmt(s: string, args: readonly unknown[]): string {
  return s.replace(/\{(\d)\}/g, (_, i: string) => String(args[+i]));
}

function isNative(el: unknown): el is Native {
  return el instanceof HTMLInputElement || el instanceof HTMLSelectElement || el instanceof HTMLTextAreaElement;
}

function valueOf(el: HTMLElement): string {
  if (isNative(el)) {
    if (el instanceof HTMLSelectElement && el.multiple) {
      return Array.from(el.selectedOptions).map((o) => o.value).join(",");
    }
    const v = el.value;
    return String(v === null || v === undefined ? "" : v);
  }
  return el.getAttribute("data-ah-value") || "";
}

function blank(s: string): boolean { return trim(s) === ""; }

// An element, or the first match of a selector ("#id").
function find(target: unknown): HTMLElement | null {
  if (!target) { return null; }
  if (typeof target === "string") {
    try { return document.querySelector<HTMLElement>(target); } catch (_err) { return null; }
  }
  if (target instanceof HTMLElement) { return target; }
  if (typeof target === "object" && 0 in target) {        // an array-like of elements
    const first: unknown = (target as { 0: unknown })[0];
    return first instanceof HTMLElement ? first : null;
  }
  return null;
}

const RULES: Record<string, Check> = {
  required: (v, el) => {
    if (el instanceof HTMLInputElement && el.type === "checkbox") { return el.checked; }
    if (el instanceof HTMLInputElement && el.type === "radio") {
      return !!(el.form || document).querySelector(
        "input[type=radio][name=\"" + CSS.escape(el.name) + "\"]:checked");
    }
    return !blank(v);
  },
  email: (v) => blank(v) || /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(trim(v)),
  number: (v) => blank(v) || isFinite(Number(trim(v))),
  integer: (v) => blank(v) || /^[-+]?\d+$/.test(trim(v)),
  phone: (v) => blank(v) || /^\(\d{3}\)\d{3}-\d{4}$/.test(trim(v)),
  zip_code: (v) => blank(v) || /^(\d{5})(-\d{4})?$/.test(trim(v)),
  ssn: (v) => blank(v) || /^\d{3}-\d{2}-\d{4}$/.test(trim(v)),
  not_number: (v) => blank(v) || !/\d/.test(v),
  starts_with_letter: (v) => blank(v) || /^[a-zA-Z]/.test(trim(v)),
  min_length: (v, _el, a) => blank(v) || v.length >= Number(a[0]),
  max_length: (v, _el, a) => v.length <= Number(a[0]),
  length: (v, _el, a) => blank(v) || (v.length >= Number(a[0]) && v.length <= Number(a[1])),
  min: (v, _el, a) => blank(v) || Number(v) >= Number(a[0]),
  max: (v, _el, a) => blank(v) || Number(v) <= Number(a[0]),
  range: (v, _el, a) => blank(v) || (Number(v) >= Number(a[0]) && Number(v) <= Number(a[1])),
  pattern: (v, _el, a) => blank(v) || new RegExp("^(?:" + String(a[0]) + ")$").test(v),
  same_as: (v, _el, a) => {
    const other = find(a[0]);
    return !other || valueOf(other) === v;
  }
};

/** data-ah-validate: a JSON array of {rule, args?, msg?}; bad entries are dropped. */
function parseRules(json: string): Rule[] {
  let data: unknown;
  try { data = JSON.parse(json); } catch (_err) { return []; }
  if (!Array.isArray(data)) { return []; }
  const out: Rule[] = [];
  data.forEach((r: unknown) => {
    if (!r || typeof r !== "object") { return; }
    const o = r as Record<string, unknown>;
    if (typeof o["rule"] !== "string") { return; }
    out.push({ rule: o["rule"],
               args: Array.isArray(o["args"]) ? o["args"] : [],
               msg: typeof o["msg"] === "string" && o["msg"] ? o["msg"] : null });
  });
  return out;
}

function hintMode(el: HTMLElement): string {
  const mode = el.getAttribute("data-ah-validate-hint") || "auto";
  if (mode === "auto") {
    return el.closest(".ah-form-body") ? "field" : "tooltip";
  }
  return mode;
}

function placement(s: string | null): Placement {
  return s === "top" || s === "bottom" || s === "left" ? s : "right";
}

function skip(el: HTMLElement): boolean {
  const disabled = (isNative(el) || el instanceof HTMLButtonElement || el instanceof HTMLFieldSetElement) && el.disabled;
  return disabled || (el instanceof HTMLInputElement && el.type === "hidden") || el.getClientRects().length === 0;
}

// scope and its descendants carrying data-ah-validate.
function controls(scope: Element): HTMLElement[] {
  const out: HTMLElement[] = scope instanceof HTMLElement && scope.matches("[data-ah-validate]") ? [scope] : [];
  return out.concat(Array.from(scope.querySelectorAll<HTMLElement>("[data-ah-validate]")));
}

function fire<D>(target: EventTarget, type: string, detail?: D): void {
  target.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
}

function guarded(form: HTMLFormElement | null): boolean {
  return !!form && !form.hasAttribute("data-ah-novalidate") &&
    !!form.querySelector("[data-ah-validate]");
}

function block(e: Event): void {
  e.preventDefault();
  e.stopImmediatePropagation();
}

function triggers(el: Element): string[] {
  return (el.getAttribute("data-ah-validate-on") || "blur").split(/\s+/);
}

// A delegated handler: fn(match) for every element matching the
// selector from the target up to the document.
function eachMatch(e: Event, selector: string, fn: (match: HTMLElement) => void): void {
  let n = e.target instanceof Element ? e.target.closest<HTMLElement>(selector) : null;
  while (n) {
    fn(n);
    n = n.parentElement ? n.parentElement.closest<HTMLElement>(selector) : null;
  }
}

/** The page's validator: the messages it shows and the document
 *  listeners that check controls and guard forms. One per page. */
class Validator {
  #seq = 0;
  #rules = new WeakMap<HTMLElement, Rule[]>();              // control -> parsed rules
  #hints = new WeakMap<HTMLElement, HTMLElement>();         // control -> its message element
  #owners = new WeakMap<HTMLElement, HTMLElement>();        // tooltip bubble -> control
  #floats = new WeakMap<HTMLElement, FloatHandle>();        // tooltip bubble -> AH.float handle

  /** Capture phase on document: runs before the delegated handlers of
   *  core.ts (actions, fetch), which listen in the bubble phase. */
  install(): void {
    document.addEventListener("submit", (e) => {
      const form = e.target instanceof HTMLFormElement ? e.target : null;
      const submitter = e.submitter;
      if (!form || !guarded(form) ||
          ((submitter instanceof HTMLButtonElement || submitter instanceof HTMLInputElement) && submitter.formNoValidate)) {
        return;
      }
      if (!this.checkAll(form)) { block(e); }
    }, true);

    document.addEventListener("click", (e) => {
      const t = e.target instanceof Element ? e.target : null;
      const btn = t && t.closest("button, input[type=submit], input[type=image]");
      if (!(btn instanceof HTMLButtonElement || btn instanceof HTMLInputElement) ||
          btn.type !== "submit" && btn.type !== "image") { return; }
      if (btn.formNoValidate || !guarded(btn.form)) { return; }
      if (btn.form && !this.checkAll(btn.form)) { block(e); }
    }, true);

    document.addEventListener("focusout", (e) => {
      eachMatch(e, "[data-ah-validate]", (el) => {
        if (e.relatedTarget instanceof Node && el.contains(e.relatedTarget)) { return; }
        if (triggers(el).indexOf("blur") >= 0) { this.checkOne(el); }
      });
    });

    const onEdit = (e: Event): void => {
      eachMatch(e, "[data-ah-validate]", (el) => {
        if (e.target !== el && !isNative(e.target) && e.type === "input") { return; }
        if (triggers(el).indexOf(e.type) >= 0 || el.classList.contains("ah-validator-error-element")) {
          this.checkOne(el);
        }
      });
    };
    document.addEventListener("input", onEdit);
    document.addEventListener("change", onEdit);
  }

  /** Check every control in scope; fires ah:validation-error/-success on it. */
  checkAll(scope: Element): boolean {
    const invalid: HTMLElement[] = [];
    controls(scope).forEach((el) => {
      if (!this.checkOne(el)) { invalid.push(el); }
    });
    const first = invalid[0];
    if (first) {
      if (first.scrollIntoView) { first.scrollIntoView({ block: "nearest", behavior: "smooth" }); }
      first.focus({ preventScroll: true });
      fire<ValidationError>(scope, "ah:validation-error", { invalid: invalid });
    } else {
      fire(scope, "ah:validation-success");
    }
    return invalid.length === 0;
  }

  /** Remove every message of the controls in scope (default the page). */
  clear(scope: Element | null): void {
    if (scope) { controls(scope).forEach((el) => { this.hideHint(el); }); }
    this.sweep();
  }

  private rulesOf(el: HTMLElement): Rule[] {
    let r = this.#rules.get(el);
    if (!r) {
      r = parseRules(el.getAttribute("data-ah-validate") || "[]");
      this.#rules.set(el, r);
    }
    return r;
  }

  // The first failing rule's message, or null.
  private failure(el: HTMLElement): string | null {
    const v = valueOf(el);
    for (const r of this.rulesOf(el)) {
      const f = RULES[r.rule];
      if (f && !f(v, el, r.args)) {
        const message = MESSAGES[r.rule];
        return r.msg || fmt(message ? message() : AH.t("field", "invalid", "Invalid value"), r.args);
      }
    }
    return null;
  }

  private checkOne(el: HTMLElement): boolean {
    this.sweep();
    if (skip(el)) { this.hideHint(el); return true; }
    const msg = this.failure(el);
    if (msg) { this.showHint(el, msg); } else { this.hideHint(el); }
    return !msg;
  }

  private hideHint(el: HTMLElement): void {
    el.classList.remove("ah-validator-error-element");
    el.removeAttribute("aria-invalid");
    const hint = this.#hints.get(el);
    if (hint) {
      this.removeHint(hint);
      this.#hints.delete(el);
    }
    const body = el.closest(".ah-form-body");
    if (body && !body.querySelector(".ah-validator-error-element")) {
      body.querySelectorAll(":scope > .ah-form-error").forEach((x) => { x.remove(); });
      const row = body.closest(".ah-form-row-invalid");
      if (row) { row.classList.remove("ah-form-row-invalid"); }
    }
    let desc = el.getAttribute("aria-describedby");
    if (desc && /ah-vh\d+/.test(desc)) {
      desc = trim(desc.replace(/\bah-vh\d+\b/g, ""));
      if (desc) { el.setAttribute("aria-describedby", desc); } else { el.removeAttribute("aria-describedby"); }
    }
  }

  // The bubble is anchored with AH.float (flips when out of room, follows
  // scrolling); extra/field.css points its arrow back at the control
  // from the side in data-ah-placement.
  private floatTooltip(el: HTMLElement, hint: HTMLElement): void {
    const pos = placement(el.getAttribute("data-ah-validate-position"));
    this.#floats.set(hint, AH.float(hint, el, { placement: pos, align: "center", offset: 8 }));
  }

  private removeHint(hint: HTMLElement): void {
    const f = this.#floats.get(hint);
    if (f) { f.stop(); this.#floats.delete(hint); }
    hint.remove();
  }

  private showHint(el: HTMLElement, message: string): void {
    this.hideHint(el);
    const id = "ah-vh" + (++this.#seq);
    el.classList.add("ah-validator-error-element");
    el.setAttribute("aria-invalid", "true");
    el.setAttribute("aria-describedby", trim((el.getAttribute("aria-describedby") || "") + " " + id));
    const mode = hintMode(el);
    const body = el.closest(".ah-form-body");
    if (mode === "field" && body) {
      body.querySelectorAll(":scope > .ah-form-error").forEach((x) => { x.remove(); });
      const hint = document.createElement("div");
      hint.className = "ah-form-error ah-validator-error-label";
      hint.setAttribute("role", "alert");
      hint.id = id;
      hint.textContent = message;
      body.appendChild(hint);
      const row = body.closest(".ah-form-row, .ah-form-col");
      if (row) { row.classList.add("ah-form-row-invalid"); }
      this.#hints.set(el, hint);
      return;
    }
    if (mode === "label") {
      const hint = document.createElement("label");
      hint.className = "ah-validator-error-label";
      hint.setAttribute("role", "alert");
      hint.id = id;
      if (el.id) { hint.htmlFor = el.id; }
      hint.textContent = message;
      if (el.getAttribute("data-ah-validate-position") === "top") { el.before(hint); }
      else { el.after(hint); }
      this.#hints.set(el, hint);
      return;
    }
    const hint = document.createElement("div");
    hint.className = "ah-validator-hint";
    hint.setAttribute("role", "alert");
    hint.id = id;
    hint.innerHTML = "<div class=\"ah-validator-arrow\"></div>";
    hint.appendChild(document.createTextNode(message));
    document.body.appendChild(hint);
    this.#owners.set(hint, el);
    this.floatTooltip(el, hint);
    hint.classList.add("ah-validator-hint-visible");
    hint.addEventListener("click", () => { this.hideHint(el); });     // sigil: click closes
    this.#hints.set(el, hint);
  }

  // Bubbles whose control has left the page (replaced by an action).
  private sweep(): void {
    document.querySelectorAll<HTMLElement>(".ah-validator-hint").forEach((hint) => {
      const owner = this.#owners.get(hint);
      if (!owner || !document.body.contains(owner)) { this.removeHint(hint); }
    });
  }
}

const validator = new Validator();
validator.install();

// validate(target): target is a form or control (an element or a
// selector); the default is the whole page.
AH.fn("validate", (target?: unknown) => validator.checkAll(find(target) || document.body));
AH.fn("clearValidation", (target?: unknown) => {
  validator.clear(target ? find(target) : document.body);
});
