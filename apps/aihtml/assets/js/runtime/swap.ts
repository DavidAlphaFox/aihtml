// Swapping server HTML into the page.
//
// Modes: inner (default) | outer | append | prepend | none, and the two
// morph modes, which patch the existing DOM towards the new HTML instead of
// replacing it: morph (the target element itself) and morph_inner (its
// children). Morphing keeps every node that survives, so focus, caret,
// scroll position, open popups and component state stay put.
//
// Every mode restores focus afterwards: the focused element is found again
// by id and its selection is put back. Scripts in the HTML are dropped.
import type { Behaviours } from "./behaviours.ts";
import { elements, withSelf } from "./dom.ts";
import type { Targets } from "./dom.ts";

export type SwapMode = "inner" | "outer" | "append" | "prepend" | "none" | "morph" | "morph_inner";

/** After a swap the new top-level elements carry the class ah-added and
 *  the target ah-settling, both removed this many ms later. */
export const SETTLE_MS = 20;
const SETTLE_ATTRS = ["class", "style", "width", "height"] as const;
type SettleAttr = typeof SETTLE_ATTRS[number];

const EXECUTABLE = /^$|^module$|\/(?:java|ecma)script/i;

interface Preserved { placeholder: Element; el: Element; }
interface Settling { el: Element; saved: Record<SettleAttr, string | null>; }
interface Focus { id: string; start: number | null; end: number | null; dir: string | null; }

/** What a morph collects: nodes it added, components whose DOM changed. */
interface MorphContext { added: Node[]; changed: Element[]; }

type Movable = Element & { moveBefore?: (node: Node, child: Node | null) => void };

export class Swapper {
  readonly #behaviours: Behaviours;

  constructor(behaviours: Behaviours) {
    this.#behaviours = behaviours;
  }

  /** Put html into target by mode; returns the inserted top-level nodes
   *  (empty for morph, which mounts what it adds itself, and none). */
  swap(target: Targets, html: string, mode?: SwapMode | string): Node[] {
    const focus = Swapper.#captureFocus();
    const added = this.#doSwap(elements(target), html, mode || "inner");
    Swapper.#restoreFocus(focus);
    return added;
  }

  #doSwap(targets: Element[], html: string, mode: string): Node[] {
    if (mode === "morph" || mode === "morph_inner") {
      targets.forEach((t) => { this.morph(t, html, mode === "morph"); });
      return [];
    }
    if (mode === "none" || !targets.length) {
      return [];
    }
    const nodes = Swapper.#parseHTML(html);
    const pantry = Swapper.#stashPreserved(nodes);
    const settle = Swapper.#prepareSettle(nodes);
    const added = this.#insert(targets, nodes, mode);
    Swapper.#restorePreserved(pantry);
    Swapper.#finishSettle(targets, added.filter((n): n is Element => n.nodeType === 1), settle);
    return added;
  }

  static #parseHTML(html: string): Node[] {
    const tpl = document.createElement("template");
    tpl.innerHTML = html;
    tpl.content.querySelectorAll("script").forEach((s) => {
      if (EXECUTABLE.test(s.type)) { s.remove(); }
    });
    return Array.from(tpl.content.childNodes);
  }

  // With several targets, all but the last get a copy of the nodes and
  // the last gets the nodes themselves. Returns every inserted node.
  #insert(targets: Element[], nodes: Node[], mode: string): Node[] {
    const added: Node[] = [];
    targets.forEach((t, i) => {
      const these = i === targets.length - 1 ? nodes : nodes.map((n) => n.cloneNode(true));
      added.push(...these);
      switch (mode) {
        case "outer":
          this.#behaviours.destroy(t);
          t.replaceWith(...these);
          break;
        case "append":
          t.append(...these);
          break;
        case "prepend":
          t.prepend(...these);
          break;
        default:
          this.#behaviours.destroy(Array.from(t.children));
          t.replaceChildren(...these);
          break;
      }
    });
    return added;
  }

  // ---- preserve ----------------------------------------------------
  //
  // An element with data-ah-preserve and an id is never replaced: when new
  // content brings an element with the same id, the existing one (a
  // playing video, an editor with unsaved text, a mounted component) is
  // moved into its place and the new copy is dropped. Moves use moveBefore
  // where the browser has it, which keeps iframes and media running.

  static #moveTo(parent: Node, node: Element, before?: Node | null): void {
    const p = parent as Movable;
    if (p.moveBefore && node.isConnected && parent.isConnected) {
      p.moveBefore(node, before || null);
    } else {
      parent.insertBefore(node, before || null);
    }
  }

  static #stashPreserved(nodes: Node[]): Preserved[] {
    const found: Preserved[] = [];
    nodes.forEach((n) => {
      withSelf(n, "[data-ah-preserve][id]").forEach((ph) => {
        const old = document.getElementById(ph.id);
        if (old && old !== ph && old.hasAttribute("data-ah-preserve")) {
          found.push({ placeholder: ph, el: old });
        }
      });
    });
    if (!found.length) {
      return found;
    }
    let pantry = document.getElementById("ah-preserve-pantry");
    if (!pantry) {
      pantry = document.createElement("div");
      pantry.id = "ah-preserve-pantry";
      pantry.hidden = true;
      document.body.appendChild(pantry);
    }
    const into = pantry;
    found.forEach((f) => { Swapper.#moveTo(into, f.el); });
    return found;
  }

  static #restorePreserved(found: Preserved[]): void {
    found.forEach((f) => {
      const ph = f.placeholder, parent = ph.parentNode;
      if (parent) {
        Swapper.#moveTo(parent, f.el, ph);
        parent.removeChild(ph);
      }
    });
  }

  // ---- settle ------------------------------------------------------
  //
  // An element whose id already existed first takes the old element's
  // class, style, width and height, and gets its new ones after the delay:
  // a class or style change between the two renders becomes a CSS
  // transition. Component roots (data-ah) are left out; their behaviours
  // read the real attributes on setup.

  static #prepareSettle(nodes: Node[]): Settling[] {
    const list: Settling[] = [];
    nodes.forEach((n) => {
      withSelf(n, "[id]").forEach((el) => {
        const old = document.getElementById(el.id);
        if (!old || old === el || el.hasAttribute("data-ah") || el.hasAttribute("data-ah-preserve")) {
          return;
        }
        const saved = {} as Record<SettleAttr, string | null>;
        SETTLE_ATTRS.forEach((a) => {
          saved[a] = el.getAttribute(a);
          const v = old.getAttribute(a);
          if (v === null) { el.removeAttribute(a); } else { el.setAttribute(a, v); }
        });
        list.push({ el, saved });
      });
    });
    return list;
  }

  static #finishSettle(targets: Element[], added: Element[], list: Settling[]): void {
    added.forEach((el) => { el.classList.add("ah-added"); });
    targets.forEach((el) => { el.classList.add("ah-settling"); });
    setTimeout(() => {
      list.forEach((x) => {
        SETTLE_ATTRS.forEach((a) => {
          const v = x.saved[a];
          if (v === null) { x.el.removeAttribute(a); } else { x.el.setAttribute(a, v); }
        });
      });
      Swapper.#dropClass(added, "ah-added");
      Swapper.#dropClass(targets, "ah-settling");
    }, SETTLE_MS);
  }

  // Remove a transient class without leaving class="" behind.
  static #dropClass(els: Element[], cls: string): void {
    els.forEach((el) => {
      el.classList.remove(cls);
      if (el.getAttribute("class") === "") { el.removeAttribute("class"); }
    });
  }

  static #captureFocus(): Focus | null {
    const a = document.activeElement;
    if (!a || a === document.body || !a.id) {
      return null;
    }
    const f: Focus = { id: a.id, start: null, end: null, dir: null };
    if (a instanceof HTMLInputElement || a instanceof HTMLTextAreaElement) {
      try {                          // throws for inputs without a caret
        f.start = a.selectionStart;
        f.end = a.selectionEnd;
        f.dir = a.selectionDirection;
      } catch { /* no selection to keep */ }
    }
    return f;
  }

  static #restoreFocus(f: Focus | null): void {
    if (!f) {
      return;
    }
    const el = document.getElementById(f.id);
    if (!el) {
      return;
    }
    if (el !== document.activeElement) {
      el.focus({ preventScroll: true });
    }
    if (f.start !== null && (el instanceof HTMLInputElement || el instanceof HTMLTextAreaElement)) {
      try {
        el.setSelectionRange(f.start, f.end, (f.dir || "none") as "forward" | "backward" | "none");
      } catch { /* ignore */ }
    }
  }

  // ---- morph -------------------------------------------------------
  //
  // Children are matched by id first, otherwise by position when the node
  // type and tag agree. Attributes are synced, except data-ah-mounted. Form
  // controls take the server's value unless they have the focus, where the
  // user is typing. A mounted component whose subtree changed is
  // re-initialised on its existing nodes (destroy, then mount: teardown
  // and setup); added nodes are mounted; removed ones are destroyed first.

  /** Patch target (outer: the element itself, else its children) towards html. */
  morph(target: Element, html: string, outer: boolean): void {
    const tpl = document.createElement("template");
    tpl.innerHTML = html;
    const ctx: MorphContext = { added: [], changed: [] };
    if (outer) {
      const rs = Swapper.#significantChildren(tpl.content);
      if (rs.length !== 1) {
        throw new Error("aihtml: morph needs exactly one root element");
      }
      this.#morphNode(target, rs[0], ctx);
    } else {
      this.#morphChildren(target, tpl.content, ctx);
    }
    const b = this.#behaviours;
    // Re-initialise only the outermost changed components: destroy and
    // mount already cover the components nested inside them.
    ctx.changed.filter((el) => !ctx.changed.some((other) => other !== el && other.contains(el)))
      .forEach((el) => {
        if (document.contains(el) && b.isMounted(el)) {
          b.destroy(el);
          b.mount(el);
        }
      });
    const added = ctx.added.filter((n): n is Element => n.nodeType === 1 && document.contains(n));
    added.forEach((el) => { b.mount(el); });
    Swapper.#finishSettle([target], added, []);
  }

  static #significantChildren(node: Node): Node[] {
    return Array.prototype.filter.call(node.childNodes, (n: Node) =>
      n.nodeType === 1 || (n.nodeType === 3 && (n.nodeValue || "").trim() !== "")) as Node[];
  }

  static #sameKind(a: Node, b: Node): boolean {
    return a.nodeType === b.nodeType && (a.nodeType !== 1 || (a as Element).tagName === (b as Element).tagName);
  }

  // Returns true when anything under `old` changed.
  #morphNode(old: Node, neu: Node, ctx: MorphContext): boolean {
    if (old instanceof Element && old.id && old.hasAttribute("data-ah-preserve")) {
      return false;                 // preserved: never patched
    }
    if (!Swapper.#sameKind(old, neu)) {
      const fresh = document.importNode(neu, true);
      if (old instanceof Element) { this.#behaviours.destroy(old); }
      old.parentNode?.replaceChild(fresh, old);
      ctx.added.push(fresh);
      return true;
    }
    if (!(old instanceof Element)) {
      if (old.nodeValue !== neu.nodeValue) {
        old.nodeValue = neu.nodeValue;
        return true;
      }
      return false;
    }
    const n = neu as Element;
    let changed = Swapper.#syncAttributes(old, n);
    if (old instanceof HTMLTextAreaElement) {
      changed = Swapper.#syncValue(old, n.textContent || "") || changed;
    } else {
      changed = this.#morphChildren(old, n, ctx) || changed;
    }
    if (old instanceof HTMLInputElement) {
      changed = Swapper.#syncInput(old, n) || changed;
    } else if (old instanceof HTMLSelectElement && old !== document.activeElement) {
      Array.from(old.options).forEach((o) => { o.selected = o.hasAttribute("selected"); });
    }
    if (changed && this.#behaviours.isMounted(old) && ctx.changed.indexOf(old) < 0) {
      ctx.changed.push(old);
    }
    return changed;
  }

  #morphChildren(oldParent: Node, newParent: Node, ctx: MorphContext): boolean {
    let changed = false;
    const newKids = Array.from(newParent.childNodes);
    let pos = 0;
    newKids.forEach((nk) => {
      const cur = oldParent.childNodes[pos] || null;
      let match: Node | null = null;
      if (nk instanceof Element && nk.id) {
        let byId: Node | null = null;
        for (let i = pos; i < oldParent.childNodes.length; i++) {
          const c = oldParent.childNodes[i];
          if (c instanceof Element && c.id === nk.id) { byId = c; break; }
        }
        match = byId && Swapper.#sameKind(byId, nk) ? byId : null;
      } else if (cur && Swapper.#sameKind(cur, nk) && !(cur instanceof Element && cur.id)) {
        match = cur;
      }
      if (match) {
        if (match !== cur) {
          oldParent.insertBefore(match, cur);
          changed = true;
        }
        changed = this.#morphNode(match, nk, ctx) || changed;
      } else {
        const fresh = document.importNode(nk, true);
        oldParent.insertBefore(fresh, cur);
        ctx.added.push(fresh);
        changed = true;
      }
      pos++;
    });
    while (oldParent.childNodes.length > pos) {
      const gone = oldParent.childNodes[pos];
      if (gone instanceof Element) { this.#behaviours.destroy(gone); }
      oldParent.removeChild(gone);
      changed = true;
    }
    return changed;
  }

  static #syncAttributes(old: Element, neu: Element): boolean {
    let changed = false;
    Array.from(neu.attributes).forEach((a) => {
      if (old.getAttribute(a.name) !== a.value) {
        old.setAttribute(a.name, a.value);
        changed = true;
      }
    });
    Array.from(old.attributes).forEach((a) => {
      if (a.name !== "data-ah-mounted" && !neu.hasAttribute(a.name)) {
        old.removeAttribute(a.name);
        changed = true;
      }
    });
    return changed;
  }

  // The focused control keeps what the user is typing.
  static #syncValue(el: HTMLInputElement | HTMLTextAreaElement, value: string): boolean {
    if (el === document.activeElement || el.value === value) {
      return false;
    }
    el.value = value;
    return true;
  }

  static #syncInput(old: HTMLInputElement, neu: Element): boolean {
    if (old.type === "checkbox" || old.type === "radio") {
      const on = neu.hasAttribute("checked");
      if (old !== document.activeElement && old.checked !== on) {
        old.checked = on;
        return true;
      }
      return false;
    }
    return Swapper.#syncValue(old, neu.getAttribute("value") || "");
  }
}
