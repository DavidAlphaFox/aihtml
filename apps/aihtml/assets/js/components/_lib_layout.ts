/* Helpers shared by the layout components (panel, expander, tabs,
 * tab-bar, pagination, steps, loader, navigationbar, responsive-panel).
 *
 * Value-bearing components keep their value in data-ah-value on the root,
 * mirror it into a hidden input when there is one, and fire "change" on
 * the root when the user changes it. Methods called by the server
 * (AH.invoke / aihtml_action:call) update the value without firing
 * "change".
 *
 * API (named exports):
 *   setValue(el, v)                     data-ah-value and the hidden input
 *   nextEnabled(items, start, dir, cls) next index without class cls, wrapping
 *   listKeys(e, items, cur, vertical, cls)
 *                                       the index an arrow/Home/End/Enter/
 *                                       Space key moves to, or null
 *   show(el) / hide(el)                 set or clear an inline display
 *   slide(el, open, ms, done)           slide down (open) or up, by height
 *   fade(el, open, ms, done)            fade in (open) or out, by opacity
 *   animate(el, keyframes, ms, done)    a Web Animation that stop(el) can cut
 *                                       short
 *   stop(el)                            the running slide/fade/animate jumps
 *                                       to its end and its callback runs now
 */

type Done = () => void;

/** data-ah-value and the hidden input */
export function setValue(el: Element, v: string): void {
  el.setAttribute("data-ah-value", v);
  const hidden = el.querySelector<HTMLInputElement>(":scope > input[type=hidden]");
  if (hidden) { hidden.value = v; }
}

/** Next enabled index from start in direction dir, wrapping. */
export function nextEnabled(items: readonly Element[], start: number, dir: number, disabledCls: string): number {
  const n = items.length;
  for (let s = 1, i = (start + dir + n) % n; s <= n; s++, i = (i + dir + n) % n) {
    if (!items[i].classList.contains(disabledCls)) { return i; }
  }
  return start;
}

/** Arrow keys (by orientation), Home, End; Enter/Space activate. */
export function listKeys(e: KeyboardEvent, items: readonly Element[], cur: number, vertical: boolean,
                         disabledCls: string): number | null {
  const k = e.key;
  const prev = vertical ? "ArrowUp" : "ArrowLeft";
  const next = vertical ? "ArrowDown" : "ArrowRight";
  if (k === "Home") { return nextEnabled(items, -1, 1, disabledCls); }
  if (k === "End") { return nextEnabled(items, items.length, -1, disabledCls); }
  if (k === prev) { return nextEnabled(items, cur, -1, disabledCls); }
  if (k === next) { return nextEnabled(items, cur, 1, disabledCls); }
  if (k === "Enter" || k === " ") { return cur; }
  return null;
}

// ---- show / hide / slide / fade -------------------------------------

function hidden(el: Element): boolean {
  return getComputedStyle(el).display === "none";
}

export function show(el: HTMLElement): void {
  el.style.display = "";
  if (hidden(el)) { el.style.display = "block"; }
}

export function hide(el: HTMLElement): void {
  el.style.display = "none";
}

// The finish function of the animation running on each element (the
// registry stop() needs; an entry goes when its animation ends).
const running = new WeakMap<Element, Done>();

export function stop(el: Element): void {
  const r = running.get(el);
  if (r) { r(); }
}

/** Run keyframes on el for ms; end() runs once, when the animation ends
 *  or when stop(el) cuts it short (synchronously then). */
export function animate(el: HTMLElement, frames: Keyframe[], ms: number, end: Done): void {
  stop(el);
  let over = false;
  const anim = ms > 0 && typeof el.animate === "function"
    ? el.animate(frames, { duration: ms, easing: "ease-in-out", fill: "forwards" }) : null;
  const finish = (): void => {
    if (over) { return; }
    over = true;
    running.delete(el);
    end();
    if (anim) { anim.cancel(); }
  };
  running.set(el, finish);
  if (anim) { anim.onfinish = finish; } else { finish(); }
}

const SLIDE_PROPS = ["height", "paddingTop", "paddingBottom", "marginTop", "marginBottom"] as const;

export function slide(el: HTMLElement, open: boolean, ms: number, done?: Done): void {
  stop(el);
  const cb = done || ((): void => {});
  if (open === !hidden(el)) { cb(); return; }
  if (open) { show(el); }
  const cs = getComputedStyle(el);
  const full: Keyframe = {};
  const zero: Keyframe = {};
  SLIDE_PROPS.forEach((p) => {
    full[p] = p === "height" ? el.getBoundingClientRect().height + "px" : cs[p];
    zero[p] = "0px";
  });
  const overflow = el.style.overflow;
  el.style.overflow = "hidden";
  animate(el, open ? [zero, full] : [full, zero], ms, () => {
    el.style.overflow = overflow;
    if (!open) { hide(el); }
    cb();
  });
}

export function fade(el: HTMLElement, open: boolean, ms: number, done?: Done): void {
  stop(el);
  const cb = done || ((): void => {});
  if (open === !hidden(el)) { cb(); return; }
  if (open) { show(el); }
  const from = getComputedStyle(el).opacity;
  animate(el, open ? [{ opacity: 0 }, { opacity: from }] : [{ opacity: from }, { opacity: 0 }], ms, () => {
    if (!open) { hide(el); }
    cb();
  });
}
