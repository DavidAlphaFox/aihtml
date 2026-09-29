/* Shared by the navigation behaviours (menu, navbar, sidenav, toolbar,
   splitter, listmenu, tabs). Ported from sigil's components/layout/*.cljs. */
import type { Controller } from "../core.ts";

/** laid out (jQuery's :visible) */
export function visible(el: HTMLElement): boolean {
  return !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length);
}

/** data-ah-value, the hidden input, then a native bubbling event on el */
export function setValue(el: Element, v: unknown, eventName?: string): void {
  const s = v === null || v === undefined ? "" : String(v);
  el.setAttribute("data-ah-value", s);
  const hidden = el.querySelector<HTMLInputElement>(":scope > input[type=hidden]");
  if (hidden) { hidden.value = s; }
  if (eventName) {
    el.dispatchEvent(new CustomEvent(eventName, { bubbles: true, cancelable: true }));
  }
}

/** The elements under scope (an element or an array of them) matching
 *  sel whose attr equals key. */
export function byKey<E extends Element = HTMLElement>(scope: Element | readonly Element[], sel: string,
                                                       attr: string, key: unknown): E[] {
  const roots: readonly Element[] = Array.isArray(scope) ? scope : [scope as Element];
  const out: E[] = [];
  roots.forEach((root) => {
    root.querySelectorAll<E>(sel).forEach((n) => {
      if (n.getAttribute(attr) === String(key)) { out.push(n); }
    });
  });
  return out;
}

/** Delegated mouseenter / mouseleave on root (default ctrl.element), as
 *  jQuery's .on("mouseenter", sel, fn): enter(match, e) / leave(match, e).
 *  mouseover / mouseout that cross the boundary of a match are its
 *  mouseenter / mouseleave; nested matches each get theirs, innermost
 *  first. */
export function hover<M extends Element = HTMLElement>(
  ctrl: Controller<Element>, sel: string,
  enter: ((match: M, e: MouseEvent) => void) | null,
  leave: ((match: M, e: MouseEvent) => void) | null,
  scope?: Element): void {
  const root = scope || ctrl.element;
  const edges = (e: MouseEvent): M[] => {
    const out: M[] = [];
    const rel = e.relatedTarget as Node | null;
    for (let n = e.target as Node | null; n && n !== root && n.nodeType === 1; n = n.parentNode) {
      const el = n as M;
      if (el.matches(sel) && !(rel && (rel === n || n.contains(rel)))) { out.push(el); }
    }
    return root.contains(e.target as Node) ? out : [];
  };
  if (enter) {
    ctrl.listen(root, "mouseover", (e) => { edges(e).forEach((hit) => { enter(hit, e); }); });
  }
  if (leave) {
    ctrl.listen(root, "mouseout", (e) => { edges(e).forEach((hit) => { leave(hit, e); }); });
  }
}
