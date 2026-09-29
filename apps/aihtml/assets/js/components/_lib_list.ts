/* Shared by the list selection behaviours (cascader, combobox, listbox,
 * transfer, dropdownlist, ported from sigil): ids, the value-bearing
 * contract and row helpers. All rows, columns and lists are rendered on
 * the server; the behaviours show, hide, mark and move them. */
import { split as splitValues, join as joinValues } from "./_lib_values.ts";

// The id registry of the list behaviours: generated ids stay unique.
let seq = 0;

/** el's id, given one (prefix + a number) when it has none. */
export function ensureId(el: Element, prefix: string): string {
  if (!el.id) { el.id = prefix + (++seq); }
  return el.id;
}

/** Value-bearing contract: data-ah-value + hidden input, then (with
 *  fire, when the value changed) a native bubbling "change" on el. */
export function publish(el: Element, value: string, fire: boolean): void {
  const old = el.getAttribute("data-ah-value") || "";
  el.setAttribute("data-ah-value", value);
  el.querySelectorAll<HTMLInputElement>(":scope > input[type=hidden]").forEach((h) => { h.value = value; });
  if (fire && old !== value) {
    el.dispatchEvent(new Event("change", { bubbles: true, cancelable: true }));
  }
}

/** Direct children of el matching selector. */
export function kids<E extends Element = HTMLElement>(el: Element | null | undefined, selector: string): E[] {
  if (!el) { return []; }
  const out: E[] = [];
  Array.from(el.children).forEach((c) => { if (c.matches(selector)) { out.push(c as E); } });
  return out;
}

/** Text content of the first direct child matching selector ("" if none). */
export function childText(el: Element | null | undefined, selector: string): string {
  const c = kids(el, selector)[0];
  return c ? c.textContent || "" : "";
}

/** A value text (_lib_values) or an array -> values. With `single`
 *  true a text is one value, commas and all. */
export function split(v: unknown, single?: boolean): string[] {
  if (v == null || v === "") { return []; }
  if (Array.isArray(v)) { return v.map((x: unknown) => String(x)); }
  return single ? [String(v)] : splitValues(String(v));
}

/** The value text of values: joined, or with `single` the one value. */
export function join(vals: readonly string[], single?: boolean): string {
  return single ? (vals.length ? String(vals[0]) : "") : joinValues(vals);
}

/** Not hidden by a filter. */
export function shown(li: HTMLElement): boolean { return li.style.display !== "none"; }
/** Not aria-disabled. */
export function enabled(li: Element): boolean { return li.getAttribute("aria-disabled") !== "true"; }

/** Keep a row visible in its scrolling container. */
export function scrollInto(box: HTMLElement | null | undefined, item: HTMLElement | null | undefined): void {
  if (!box || !item) { return; }
  const top = item.getBoundingClientRect().top - box.getBoundingClientRect().top - box.clientTop +
    box.scrollTop;
  const bottom = top + item.offsetHeight;
  if (top < box.scrollTop) { box.scrollTop = top; }
  if (bottom > box.scrollTop + box.clientHeight) { box.scrollTop = bottom - box.clientHeight; }
}

/** sigil search.cljs highlight-match, as DOM nodes: text in node with
 *  the first case-insensitive occurrence of the query in <b>. */
export function highlight(node: Node | null | undefined, text: string, q: string): void {
  if (!node) { return; }
  const i = q ? text.toLowerCase().indexOf(q.toLowerCase()) : -1;
  node.textContent = i < 0 ? text : text.slice(0, i);
  if (i < 0) { return; }
  const b = document.createElement("b");
  b.textContent = text.slice(i, i + q.length);
  node.appendChild(b);
  node.appendChild(document.createTextNode(text.slice(i + q.length)));
}
