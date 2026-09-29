/* Helpers shared by the progressbar and progress-circle behaviours. */

/** Detail of change and ah:complete. */
export interface ProgressChange { previous: number; value: number; }

export function pct(v: number, lo: number, hi: number): number {
  return hi > lo ? 100 * (v - lo) / (hi - lo) : 0;
}

/** change on every new value, ah:complete when it reaches max; both with
 *  detail {previous, value} */
export function fireProgress(el: Element, old: number, v: number, max: number): void {
  if (old === v) { return; }
  const detail: ProgressChange = { previous: old, value: v };
  el.dispatchEvent(new CustomEvent("change", { bubbles: true, cancelable: true, detail }));
  if (v === max) {
    el.dispatchEvent(new CustomEvent("ah:complete", { bubbles: true, cancelable: true, detail }));
  }
}
