// Element helpers of the runtime.

/** What runtime functions accept for elements: an element (or the
 *  document), a selector, or an array, NodeList or other array-like. */
export type Targets = Element | Document | string | ArrayLike<Node | null | undefined> | null | undefined;

/** A subtree the runtime works on: an element or the whole document. */
export type Root = Element | Document;

function isRoot(n: unknown): n is Root {
  return !!n && typeof n === "object" && ((n as Node).nodeType === 1 || (n as Node).nodeType === 9);
}

/** The elements and documents x stands for. */
export function roots(x: Targets): Root[] {
  if (!x) { return []; }
  if (typeof x === "string") { return Array.from(document.querySelectorAll(x)); }
  if (isRoot(x)) { return [x]; }
  if (typeof (x as ArrayLike<unknown>).length === "number") {
    return Array.prototype.filter.call(x, isRoot) as Root[];
  }
  return [];
}

/** The elements x stands for (a document stands for none). */
export function elements(x: Targets): Element[] {
  return roots(x).filter((n): n is Element => n.nodeType === 1);
}

/** The first element or document x stands for. */
export function one(x: Targets): Root | undefined {
  return isRoot(x) ? x : roots(x)[0];
}

/** node itself when it matches, then its descendants that do. */
export function withSelf(node: Node | null | undefined, sel: string): Element[] {
  if (!isRoot(node)) { return []; }
  const out: Element[] = node.nodeType === 1 && (node as Element).matches(sel) ? [node as Element] : [];
  node.querySelectorAll(sel).forEach((n) => { out.push(n); });
  return out;
}

/** A native, bubbling, cancelable CustomEvent; false when it was cancelled. */
export function fire<D>(target: EventTarget, type: string, detail?: D): boolean {
  return target.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail }));
}

/** One listener on document for events on elements matching selector:
 *  handler(e, match) runs for the target and every matching ancestor,
 *  innermost first, until one stops propagation. A click with a button
 *  other than the primary one, and a click on a disabled element, is not
 *  delegated. */
export function delegateDocument(type: string, selector: string,
                                 handler: (e: Event, match: Element) => void): void {
  document.addEventListener(type, (e) => {
    if (type === "click" && (e as MouseEvent).button >= 1) { return; }
    const t = e.target as Node | null;
    const start = t && t.nodeType === 1 ? t as Element : t && t.parentElement;
    for (let n = start && start.closest(selector); n; n = n.parentElement && n.parentElement.closest(selector)) {
      if (type === "click" && (n as HTMLButtonElement).disabled === true) { continue; }
      handler(e, n);
      if (e.cancelBubble) { return; }
    }
  });
}
