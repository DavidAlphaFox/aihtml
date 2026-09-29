/* Helpers the scrolling components share (scrollview, scrollbar,
 * responsive_panel): the value on the root and its hidden input, numeric
 * data attributes and pointer capture. */

/** data-ah-value and the hidden input */
export function setValue(el: Element, v: string | number): void {
  el.setAttribute("data-ah-value", String(v));
  const hidden = el.querySelector<HTMLInputElement>(":scope > input[type=hidden]");
  if (hidden) { hidden.value = String(v); }
}

/** a numeric attribute, dflt when missing */
export function num(el: Element, name: string, dflt: number): number {
  const v = parseFloat(el.getAttribute(name) || "");
  return isNaN(v) ? dflt : v;
}

/** pointer capture for a native pointer event */
export function capture(node: Element, e: { pointerId?: number } | null | undefined): void {
  const id = e ? e.pointerId : undefined;
  if (id !== undefined) {
    try { node.setPointerCapture(id); } catch { /* synthetic event */ }
  }
}
