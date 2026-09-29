/* rating behaviour (designs/04-components.md): a value-bearing custom
 * control: data-ah-value, the hidden input and a "change" on the root;
 * "ah:hover" (detail: the value, or null) while the pointer previews a
 * value. Methods never fire change.
 */
import AH from "../core.ts";

/** Detail of ah:hover: the previewed value, null when the preview ends. */
export type RatingHover = number | null;

/** The settings the server renders on the root. */
interface Settings {
  value: number;
  max: number;
  step: 0.5 | 1;
  live: boolean;
  clear: boolean;
}

const KEY_STEP: Record<string, number> = { ArrowRight: 1, ArrowUp: 1, ArrowLeft: -1, ArrowDown: -1 };

class RatingController extends AH.Controller {
  override setup(): void {
    this.delegate("mousemove", ".ah-rating__star", (e, star) => {
      if (!this.settings.live) { return; }
      const v = this.valueAt(star, e);
      this.paint(v);
      this.fire<RatingHover>("ah:hover", v);
    });
    this.listen(this.element, "mouseleave", () => {
      const s = this.settings;
      if (!s.live) { return; }
      this.paint(s.value);
      this.fire<RatingHover>("ah:hover", null);
    });
    this.delegate("click", ".ah-rating__star", (e, star) => {
      const s = this.settings;
      if (!s.live) { return; }
      const v = this.valueAt(star, e);
      this.set(s.clear && v === s.value ? 0 : v, true);
    });
    this.delegate("keydown", ".ah-rating__star", (e) => {
      const s = this.settings;
      const d = KEY_STEP[e.key];
      if (!s.live || !d) { return; }
      e.preventDefault();
      this.set(s.value + d * s.step, true);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v: number | string): void { this.set(v, false); }
  getValue(): number { return this.settings.value; }

  /** Read from the root on every use: the server may re-render it. */
  private get settings(): Settings {
    const el = this.element;
    return {
      value: parseFloat(el.getAttribute("data-ah-value") || "") || 0,
      max: parseInt(el.getAttribute("data-ah-max") || "", 10) || 5,
      step: el.getAttribute("data-precision") === "0.5" ? 0.5 : 1,
      live: el.getAttribute("data-readonly") !== "true" && el.getAttribute("data-disabled") !== "true",
      clear: el.getAttribute("data-allow-clear") !== "false"
    };
  }

  private get stars(): NodeListOf<HTMLElement> {
    return this.element.querySelectorAll<HTMLElement>(".ah-rating__star");
  }

  private paint(v: number): void {
    this.stars.forEach((star, i) => {
      const r = Math.max(0, Math.min(1, v - i));
      star.querySelectorAll<HTMLElement>(".ah-rating__filled").forEach((f) => { f.style.width = (r * 100) + "%"; });
    });
  }

  private set(raw: number | string, fire: boolean): void {
    const s = this.settings;
    const v = Math.max(0, Math.min(s.max, Math.round(Number(raw) / s.step) * s.step || 0));
    const changed = v !== s.value;
    this.element.setAttribute("data-ah-value", String(v));
    const hidden = this.element.querySelector<HTMLInputElement>(":scope > input[type=hidden]");
    if (hidden) { hidden.value = String(v); }
    this.stars.forEach((star, i) => {
      star.setAttribute("aria-checked", v >= i + 1 ? "true" : "false");
    });
    this.paint(v);
    if (fire && changed) { this.fire("change"); }
  }

  // The value under the pointer; a keyboard click (detail 0) takes the
  // whole star.
  private valueAt(star: HTMLElement, e: MouseEvent): number {
    const idx = parseInt(star.getAttribute("data-index") || "0", 10);
    if (this.settings.step === 0.5 && e.detail !== 0 && e.clientX !== undefined) {
      const rect = star.getBoundingClientRect();
      return idx + ((e.clientX - rect.left) / rect.width <= 0.5 ? 0.5 : 1);
    }
    return idx + 1;
  }
}

AH.register("rating", RatingController);
