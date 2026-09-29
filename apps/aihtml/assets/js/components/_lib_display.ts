/* Helpers shared by the display component behaviours (chip, badge,
   progressbar, progress-circle, kpi-card, timeline, ranking-list,
   tag-cloud, alert). */
import AH from "../core.ts";

/** parseFloat with a default for anything that is not a number */
export function num(v: unknown, dflt: number): number {
  const n = parseFloat(String(v));
  return isNaN(n) ? dflt : n;
}

export function clamp(v: number, lo: number, hi: number): number {
  return Math.max(lo, Math.min(hi, v));
}

/** Remove an element the way the runtime does (behaviours destroyed first). */
export function drop(el: Element): void {
  AH.destroy(el);
  el.remove();
}

/** Enter / Space act as a click on focusable non-button elements. target:
 *  the element that acts (default e.currentTarget), so it can be used
 *  directly as a delegate() handler (handler(e, match)). */
export function keyClick(e: KeyboardEvent, target?: HTMLElement): void {
  if (e.key === "Enter" || e.key === " ") {
    e.preventDefault();
    (target || e.currentTarget as HTMLElement).click();
  }
}
