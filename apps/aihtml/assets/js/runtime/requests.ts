// Request state (designs/02-actions.md): while a request runs, aria-busy
// on the element, the class ah-request on it and on its indicators
// (data-ah-indicator), and the elements named by data-ah-disable are
// disabled. Selectors may be "this" or "closest <selector>". Overlapping
// requests are counted, so the first one to finish does not clear the
// others' state.

interface Counts { [key: string]: number | boolean | undefined; wasDisabled?: boolean; }

const counts = new WeakMap<Element, Counts>();

/** The elements a request-state selector names, relative to el. */
export function resolveTargets(el: Element, sel: string | null): Element[] {
  if (!sel) { return []; }
  if (sel === "this") { return [el]; }
  const m = /^closest\s+(.+)$/.exec(sel);
  if (m) {
    const c = el.closest(m[1]);
    return c ? [c] : [];
  }
  return Array.from(document.querySelectorAll(sel));
}

function bump(els: Element[], cls: string, by: number): void {
  els.forEach((el) => {
    const st = counts.get(el) || {};
    counts.set(el, st);
    const n = ((st[cls] as number | undefined) || 0) + by;
    st[cls] = n;
    if (cls === "disabled") {
      const d = el as HTMLButtonElement;
      if (n > 0 && by > 0 && n === 1) {
        st.wasDisabled = d.disabled;
        d.disabled = true;
      } else if (n === 0) {
        d.disabled = !!st.wasDisabled;
      }
    } else {
      el.classList.toggle(cls, n > 0);
    }
  });
}

/** Mark a request on el as running; returns the function that ends it
 *  (calling it again does nothing). */
export function requestStart(el: Element): () => void {
  const ind = Array.from(new Set([el, ...resolveTargets(el, el.getAttribute("data-ah-indicator"))]));
  const dis = resolveTargets(el, el.getAttribute("data-ah-disable"));
  el.setAttribute("aria-busy", "true");
  bump(ind, "ah-request", 1);
  bump(dis, "disabled", 1);
  let ended = false;
  return () => {
    if (ended) { return; }
    ended = true;
    bump(ind, "ah-request", -1);
    bump(dis, "disabled", -1);
    if (!el.classList.contains("ah-request")) { el.removeAttribute("aria-busy"); }
  };
}
