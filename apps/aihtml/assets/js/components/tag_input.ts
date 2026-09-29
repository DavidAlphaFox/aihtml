/* Behaviour of the tag_input component (designs/04-components.md).
 * Ported from sigil: form/tag_input. The root fires "change" (no
 * detail) when the tags change. */
import AH from "../core.ts";
import { commitValue, isolate } from "./_lib_input.ts";
import { join } from "./_lib_values.ts";
import "virtual:ah-tpl/tag_input_chip";

const FIELD = ".ah-tag-input__field";

class TagInputController extends AH.Controller {
  override setup(): void {
    const el = this.element;
    isolate(this, FIELD);
    this.delegate("click", ".ah-chip__delete", (e, del) => {
      e.stopPropagation();
      if (this.disabled()) { return; }
      const chip = del.closest(".ah-tag-input__chip");
      const i = parseInt(chip ? chip.getAttribute("data-index") || "" : "", 10);
      const tags = this.read();
      if (i >= 0 && i < tags.length) {
        tags.splice(i, 1);
        this.render(tags);
        this.focus();
      }
    });
    this.delegate<KeyboardEvent, HTMLInputElement>("keydown", FIELD, (e, field) => {
      if (e.key === "Enter" || e.key === ",") {
        e.preventDefault();
        this.addFromField();
      } else if (e.key === "Backspace" && field.value.trim() === "") {
        const tags = this.read();
        if (tags.length) {
          e.preventDefault();
          tags.pop();
          this.render(tags);
        }
      }
    });
    // A pasted list becomes several tags (sigil takes it as typed text).
    this.delegate<ClipboardEvent, HTMLInputElement>("paste", FIELD, (e, field) => {
      const cd = e.clipboardData;
      const text = cd ? cd.getData("text") : "";
      if (!/[,\n\r\t]/.test(text)) { return; }
      e.preventDefault();
      const parts = (field.value + text).split(/[,\n\r\t]+/);
      field.value = "";
      this.addTags(parts);
    });
    this.delegate("focusout", FIELD, () => { this.addFromField(); });
    // A click on the empty area focuses the field.
    this.listen(el, "click", (e) => {
      if (e.target === el) { this.focus(); }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getTags(): string[] { return this.read(); }
  setTags(tags: unknown): void {
    const list: readonly unknown[] = tags == null ? [] : Array.isArray(tags) ? tags : [tags];
    this.render(list.map(String));
  }
  add(tag: unknown): void { this.addTags([tag]); }
  remove(tag: unknown): void {
    const tags = this.read(), i = tags.indexOf(String(tag));
    if (i >= 0) { tags.splice(i, 1); this.render(tags); }
  }
  clear(): void { this.render([]); }
  focus(): void {
    const f = this.field();
    if (f) { f.focus(); }
  }

  private field(): HTMLInputElement | null { return this.element.querySelector<HTMLInputElement>(FIELD); }
  private disabled(): boolean { return this.element.getAttribute("data-disabled") === "true"; }

  private read(): string[] {
    return Array.from(this.element.querySelectorAll(".ah-tag-input__chip .ah-chip__label"))
      .map((l) => l.textContent || "");
  }

  // Same markup as the server's first render: templates/tag_input_chip.mustache
  private chip(tag: string, i: number): string {
    const el = this.element;
    return AH.tpl.tag_input_chip({
      variant: el.getAttribute("data-chip-variant") || "soft",
      color: el.getAttribute("data-chip-color") || "primary",
      index: i,
      label: tag,
      disabled: el.getAttribute("data-disabled") === "true"
    });
  }

  private render(tags: readonly string[]): boolean {
    const el = this.element;
    el.querySelectorAll(".ah-tag-input__chip").forEach((c) => { c.remove(); });
    const field = this.field();
    if (field) {
      field.insertAdjacentHTML("beforebegin", tags.map((t, i) => this.chip(t, i)).join(""));
    }
    return commitValue(el, join(tags));
  }

  // sigil tag-input/add-tag: trim, skip blanks, max-tags and duplicates.
  private addTags(list: readonly unknown[]): boolean {
    const tags = this.read();
    const max = parseInt(this.element.getAttribute("data-max-tags") || "", 10);
    const dup = this.element.hasAttribute("data-allow-duplicates");
    let changed = false;
    list.forEach((raw) => {
      const t = String(raw).trim().replace(/,/g, "");
      if (!t || (!isNaN(max) && tags.length >= max) || (!dup && tags.indexOf(t) >= 0)) { return; }
      tags.push(t);
      changed = true;
    });
    return changed ? this.render(tags) : false;
  }

  private addFromField(): boolean {
    const f = this.field();
    if (!f) { return false; }
    const raw = f.value;
    f.value = "";
    return this.addTags([raw]);
  }
}

AH.register("tag-input", TagInputController);
