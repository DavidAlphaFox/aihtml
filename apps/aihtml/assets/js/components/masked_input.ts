/* Behaviour of masked_input (designs/04-components.md), after sigil's
 * form/masked_input.
 *
 *   masked-input     only characters that fit the mask are typed, deleted
 *                    or pasted; input on each edit, change on blur
 *
 * The root keeps data-ah-value and its hidden input in step and fires
 * "input" / "change" (native events, detail: the value, MaskedValue).
 *
 * The server renders the whole first state, so setup only binds events.
 */
import AH from "../core.ts";

/** Detail of input / change: the value. */
export type MaskedValue = string;

// ------------------------------------------------------------------
// masked-input
// ------------------------------------------------------------------
//
// The mask is a list of positions {re, ch} (editable, ch null when
// empty) or {lit} (a literal), parsed as aihtml_masked_input:parse_mask.
// The controller keeps it with the input, the label, the prompt and the
// value at focus.

interface Slot { re: RegExp; ch: string | null; }
interface Literal { lit: string; }
type MaskItem = Slot | Literal;

const MASK_RE: Record<string, string> = { "9": "\\d", "0": "\\d", "#": "[\\d|+|-]", "A": "\\w", "a": "\\w",
                                          "L": "[a-zA-Z]", "l": "[a-zA-Z]", "c": ".", "C": "." };

function isLit(it: MaskItem): it is Literal { return "lit" in it; }

function maskParse(mask: string | null): MaskItem[] {
  const items: MaskItem[] = [], chars = Array.from(mask || "");
  for (let i = 0; i < chars.length; i++) {
    const c = chars[i] ?? "";
    const re = MASK_RE[c];
    if (c === "[") {
      let j = chars.indexOf("]", i);
      if (j < 0) { j = chars.length - 1; }
      items.push({ re: new RegExp("^(?:(" + chars.slice(i, j + 1).join("") + "))$", "i"), ch: null });
      i = j;
    } else if (re) {
      items.push({ re: new RegExp("^(?:" + re + ")$", "i"), ch: null });
    } else {
      items.push({ lit: c });
    }
  }
  return items;
}

// Fill the editable positions from text, as the server does.
function maskFill(items: MaskItem[], text: unknown): MaskItem[] {
  const cs = Array.from(text === null || text === undefined ? "" : String(text));
  let k = 0;
  items.forEach((it) => {
    if (isLit(it)) {
      if (cs[k] === it.lit) { k++; }
      return;
    }
    it.ch = null;
    while (k < cs.length && !it.re.test(cs[k] ?? "")) { k++; }
    if (k < cs.length) { it.ch = cs[k++] ?? null; }
  });
  return items;
}

function setValue(el: Element, v: string): void {
  el.setAttribute("data-ah-value", v);
  el.querySelectorAll<HTMLInputElement>(":scope > input[type=hidden]").forEach((h) => { h.value = v; });
}

class MaskedInputController extends AH.Controller {
  #input: HTMLInputElement | null = null;
  #label: HTMLElement | null = null;
  #prompt = "_";
  #literals = false;
  #items: MaskItem[] = [];
  #focusValue: string | null = null;

  override setup(): void {
    const el = this.element;
    const inp = el.querySelector<HTMLInputElement>(":scope > input.ah-masked-input");
    this.#input = inp;
    this.#label = el.querySelector<HTMLElement>(":scope > .ah-masked-input-label");
    this.#prompt = el.getAttribute("data-ah-prompt") || "_";
    this.#literals = el.hasAttribute("data-ah-literals");
    this.#items = maskParse(el.getAttribute("data-ah-mask"));
    this.#focusValue = null;
    if (!inp) { return; }
    // the server filled the mask: read the positions back
    Array.from(inp.value).forEach((c, i) => {
      const it = this.#items[i];
      if (it && !isLit(it) && c !== this.#prompt) { it.ch = c; }
    });

    this.listen(inp, "keydown", (e) => {
      const k = e.key || "";
      if (e.keyCode === 229) { return; }                       // IME: beforeinput
      if (e.ctrlKey || e.metaKey || e.altKey) {
        if ((k === "x" || k === "X") && !this.blocked()) {
          const sel = this.selection();
          // after the browser copied the selection
          setTimeout(() => {
            if (inp.value === this.display() && sel.end > sel.start) {
              this.clearRange(sel.start, sel.end);
              this.render(sel.start, true);
            }
          }, 10);
        }
        return;
      }
      if (k === "Backspace" || k === "Delete") {
        e.preventDefault();
        if (!this.blocked()) { this.remove(k === "Backspace"); }
      } else if (k.length > 0 && Array.from(k).length === 1) {
        e.preventDefault();
        if (!this.blocked()) { this.insert(k); }
      }
    });
    this.listen(inp, "beforeinput", (e) => {
      switch (e.inputType) {
        case "insertText": case "insertReplacementText": case "insertCompositionText":
          e.preventDefault();
          if (e.data && !this.blocked()) { this.insert(e.data); }
          break;
        case "insertFromPaste": case "insertFromDrop":
          e.preventDefault();                              // the paste handler
          break;
        case "deleteContentBackward": case "deleteByCut":
          e.preventDefault();
          if (!this.blocked()) { this.remove(true); }
          break;
        case "deleteContentForward":
          e.preventDefault();
          if (!this.blocked()) { this.remove(false); }
          break;
      }
    });
    this.listen(inp, "paste", (e) => {
      const text = e.clipboardData && e.clipboardData.getData("text/plain");
      e.preventDefault();
      if (text && !this.blocked()) { this.insert(text); }
    });
    // safety net: whatever got through, show the items again
    this.listen(inp, "input", () => {
      const s = this.selection();
      if (inp.value !== this.text()) {
        inp.value = this.text();
        this.setCursor(s.start);
      }
    });
    // sigil's snap-to-editable: a click lands on an editable position
    this.listen(inp, "mouseup", () => {
      if (this.blocked()) { return; }
      const s = this.selection(), e = this.firstEmpty();
      let n = s.start;
      if (s.start !== s.end) { return; }
      // not past the first empty position
      if (e >= 0 && n > e) { this.setCursor(e); return; }
      if (this.editable(n)) { return; }
      n = n >= this.#items.length ? this.prevEditable(this.#items.length) : this.nextEditable(n);
      if (n < 0) { n = this.prevEditable(s.start); }
      this.setCursor(n < 0 ? 0 : n);
    });
    this.listen(inp, "focus", () => {
      el.classList.add("ah-masked-input-focused");
      if (this.#label) { this.#label.classList.add("ah-masked-input-label-float"); }
      inp.value = this.display();
      this.#focusValue = el.getAttribute("data-ah-value");
      setTimeout(() => {
        if (document.activeElement !== inp) { return; }
        const e = this.firstEmpty();
        if (e >= 0) { this.setCursor(e); }
      }, 0);
    });
    this.listen(inp, "blur", () => {
      el.classList.remove("ah-masked-input-focused");
      if (this.#label) { this.#label.classList.toggle("ah-masked-input-label-float", this.raw() !== ""); }
      inp.value = this.text();
      const v = el.getAttribute("data-ah-value");
      if (this.#focusValue !== null && v !== this.#focusValue) { this.fire("change", v); }
      this.#focusValue = null;
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v: unknown): void {
    maskFill(this.#items, v);
    this.render(null, false);
    if (this.#focusValue !== null) { this.#focusValue = this.element.getAttribute("data-ah-value"); }
  }
  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }
  getMaskedValue(): string { return this.display(); }
  isComplete(): boolean {
    return this.#items.every((it) => isLit(it) || it.ch !== null);
  }
  clear(): void {
    const old = this.element.getAttribute("data-ah-value");
    maskFill(this.#items, "");
    this.render(null, false);
    if (old !== "") { this.fire<MaskedValue>("change", ""); }
  }
  setMask(mask: string): void {
    const raw = this.raw();
    this.#items = maskFill(maskParse(mask), raw);
    this.element.setAttribute("data-ah-mask", mask);
    this.render(null, false);
  }
  focus(): void { this.#input?.focus(); }

  // ------------------------------------------------------------------
  // the mask
  // ------------------------------------------------------------------

  private shown(it: MaskItem): string {
    return isLit(it) ? it.lit : (it.ch === null ? this.#prompt : it.ch);
  }

  private display(): string {
    return this.#items.map((it) => this.shown(it)).join("");
  }

  private raw(): string {
    return this.#items.map((it) => !isLit(it) && it.ch !== null ? it.ch : "").join("");
  }

  private value(): string {
    const raw = this.raw();
    return this.#literals ? (raw ? this.display() : "") : raw;
  }

  /** The editable position i, or null. */
  private slot(i: number): Slot | null {
    const it = i >= 0 ? this.#items[i] : undefined;
    return it && !isLit(it) ? it : null;
  }
  private editable(i: number): boolean { return this.slot(i) !== null; }
  private nextEditable(from: number): number {
    for (let i = from; i < this.#items.length; i++) { if (this.editable(i)) { return i; } }
    return -1;
  }
  private prevEditable(from: number): number {
    for (let i = from - 1; i >= 0; i--) { if (this.editable(i)) { return i; } }
    return -1;
  }
  private firstEmpty(): number {
    for (let i = 0; i < this.#items.length; i++) { if (this.slot(i)?.ch === null) { return i; } }
    return -1;
  }

  // Positions are characters; the input's selection counts UTF-16 units.
  private unitsTo(pos: number): number {
    let n = 0;
    for (let i = 0; i < pos && i < this.#items.length; i++) {
      const it = this.#items[i];
      if (it) { n += this.shown(it).length; }
    }
    return n;
  }
  private posOf(units: number): number {
    let n = 0;
    for (let i = 0; i < this.#items.length; i++) {
      if (n >= units) { return i; }
      const it = this.#items[i];
      if (it) { n += this.shown(it).length; }
    }
    return this.#items.length;
  }
  private selection(): { start: number; end: number } {
    const inp = this.#input;
    return { start: this.posOf(inp?.selectionStart ?? 0), end: this.posOf(inp?.selectionEnd ?? 0) };
  }
  private setCursor(pos: number): void {
    const u = this.unitsTo(Math.max(0, Math.min(pos, this.#items.length)));
    try { this.#input?.setSelectionRange(u, u); } catch (_e) { /* not focused */ }
  }

  private clearRange(a: number, b: number): void {
    for (let i = a; i < b; i++) {
      const s = this.slot(i);
      if (s) { s.ch = null; }
    }
  }

  // What the field shows: the mask, or nothing for an empty field with a
  // floating label (the label stands there) while it has no focus.
  private text(): string {
    return this.#label && document.activeElement !== this.#input && this.raw() === ""
      ? "" : this.display();
  }

  // Show the items; fire input when the value changed.
  private render(cursor: number | null, fire: boolean): void {
    const el = this.element, old = el.getAttribute("data-ah-value"), v = this.value();
    if (this.#input) { this.#input.value = this.text(); }
    setValue(el, v);
    if (cursor !== null) { this.setCursor(cursor); }
    if (this.#label && document.activeElement !== this.#input) {
      this.#label.classList.toggle("ah-masked-input-label-float", this.raw() !== "");
    }
    if (fire && v !== old) { this.fire<MaskedValue>("input", v); }
  }

  // Type one character (sigil's do-insert-char!: into the next editable
  // position if it fits, else jump past the literal it names) or paste a
  // text (sigil's do-paste!: characters that do not fit are skipped).
  private insert(text: string): void {
    const sel = this.selection(), chars = Array.from(text);
    let pos = sel.start;
    const cleared = sel.end > sel.start;
    if (cleared) { this.clearRange(sel.start, sel.end); }
    const one = chars.length === 1 ? chars[0] : undefined;
    if (one !== undefined) {
      const i = this.nextEditable(pos);
      const s = this.slot(i);
      if (s && s.re.test(one)) {
        s.ch = one;
        const j = this.nextEditable(i + 1);
        this.render(j < 0 ? this.#items.length : j, true);
        return;
      }
      for (let j = pos; j < this.#items.length; j++) {
        const it = this.#items[j];
        if (it && isLit(it) && it.lit === one) {
          if (cleared) { this.render(j + 1, true); } else { this.setCursor(j + 1); }
          return;
        }
      }
      if (cleared) { this.render(sel.start, true); }
      return;
    }
    let k = 0;
    while (k < chars.length && pos < this.#items.length) {
      const s = this.slot(pos);
      if (!s) { pos++; continue; }
      const c = chars[k] ?? "";
      if (s.re.test(c)) { s.ch = c; pos++; }
      k++;
    }
    const i = this.firstEmpty();
    this.render(i < 0 ? this.#items.length : i, true);
  }

  private remove(back: boolean): void {
    const sel = this.selection();
    if (sel.end > sel.start) {
      this.clearRange(sel.start, sel.end);
      this.render(sel.start, true);
    } else if (back) {
      const i = this.prevEditable(sel.start);
      const s = this.slot(i);
      if (s) { s.ch = null; this.render(i, true); }
    } else {
      const s = this.slot(this.nextEditable(sel.start));
      if (s) { s.ch = null; this.render(sel.start, true); }
    }
  }

  private blocked(): boolean { return !this.#input || this.#input.disabled || this.#input.readOnly; }
}

AH.register("masked-input", MaskedInputController);
