/* Behaviour of the tree (designs/04-components.md).
 *
 * Ported from sigil (data/tree). The server renders every node; this file
 * only moves state around in that DOM: expand / collapse (slide), single
 * selection, keyboard (roving tabindex, arrows, Home/End, Enter/Space, *,
 * type-ahead), lazy nodes loaded through the tree's load action
 * (data-load: a signed token), which answers with set_children/3 ->
 * childrenLoaded.
 *
 * Value-bearing roots keep their value in data-ah-value, mirror it into a
 * hidden input and fire "change" when the user changes it; methods called
 * by the server (AH.invoke / aihtml_action:call) do not fire it.
 *
 * Events: "ah:expand", "ah:collapse", "ah:item-click" on the root and
 * "ah:load" on the lazy node, all with detail {value, label, id} of the
 * node.
 */
import AH from "../core.js";

const SLIDE_MS = 200;
const slides = new WeakMap();     // ul -> end() of its running slide

function items(el) { return Array.from(el.querySelectorAll("li[role=treeitem]")); }
function own(el, node) { return !!node && node.closest(".ah-tree") === el; }
function expandable(li) { return li.hasAttribute("aria-expanded"); }
function isOpen(li) { return li.getAttribute("aria-expanded") === "true"; }
function isDisabled(li) { return li.getAttribute("aria-disabled") === "true"; }
function group(li) { return li.querySelector(":scope > ul.ah-tree-list"); }
function row(li) { return li.querySelector(":scope > .ah-tree-row"); }
function parentItem(el, li) {
  const p = li.parentElement && li.parentElement.closest("li[role=treeitem]");
  return p && el.contains(p) && p !== el ? p : null;
}
function label(li) {
  const r = row(li), l = r && r.querySelector(".ah-tree-label");
  return l ? l.textContent : "";
}
function info(li) {
  return { value: li.getAttribute("data-value"), label: label(li), id: li.id };
}

function byValue(el, v) {
  if (v === null || v === undefined) { return null; }
  v = String(v);
  return items(el).find((li) => li.getAttribute("data-value") === v) || null;
}

// Items whose ancestors are all open, in document order.
function visible(el) {
  return items(el).filter((li) => {
    for (let p = parentItem(el, li); p; p = parentItem(el, p)) {
      if (!isOpen(p)) { return false; }
    }
    return true;
  });
}

function treeOff(el) {
  return el.getAttribute("aria-disabled") === "true" || el.classList.contains("ah-tree-disabled");
}

// Roving tabindex: exactly one item is in the tab order.
function focusItem(el, li, move) {
  if (!li) { return; }
  items(el).forEach((n) => { n.setAttribute("tabindex", "-1"); });
  li.setAttribute("tabindex", "0");
  if (move) { li.focus(); }
}

// slideDown / slideUp of a child list; a slide still running jumps to
// its end first (its done callback runs).
function finishSlide(ul) {
  const running = slides.get(ul);
  if (running) { running(); }
}

function slide(ul, open, done) {
  finishSlide(ul);
  if (open) { ul.style.display = ""; }
  const h = ul.scrollHeight;
  const overflow = ul.style.overflow;
  ul.style.overflow = "hidden";
  const anim = ul.animate([{ height: (open ? 0 : h) + "px" }, { height: (open ? h : 0) + "px" }],
                          { duration: SLIDE_MS, easing: "ease-in-out" });
  const end = () => {
    if (slides.get(ul) !== end) { return; }
    slides.delete(ul);
    anim.cancel();
    ul.style.overflow = overflow;
    if (!open) { ul.style.display = "none"; }
    done();
  };
  slides.set(ul, end);
  anim.onfinish = end;
}

function mark(el, li) {
  items(el).forEach((n) => {
    if (!n.hasAttribute("aria-selected")) { return; }
    n.removeAttribute("aria-selected");
    const r = row(n);
    if (r) { r.classList.remove("ah-tree-row-selected"); }
  });
  if (li) {
    li.setAttribute("aria-selected", "true");
    const r = row(li);
    if (r) { r.classList.add("ah-tree-row-selected"); }
  }
}

function writeValue(el, v) {
  el.setAttribute("data-ah-value", v);
  const hidden = el.querySelector(":scope > input[type=hidden]");
  if (hidden) { hidden.value = v; }
}

function ensureVisible(ctl, li) {
  for (let p = parentItem(ctl.element, li); p; p = parentItem(ctl.element, p)) {
    ctl.setOpen(p, true, false);
  }
}

function typeahead(el, current, ch) {
  const vis = visible(el);
  const start = vis.indexOf(current);
  for (let i = 1; i <= vis.length; i++) {
    const li = vis[(start + i) % vis.length];
    if (label(li).trim().toLowerCase().indexOf(ch) === 0) { return li; }
  }
  return null;
}

AH.register("tree", class extends AH.Controller {
  setup() {
    const el = this.element;
    this.delegate("click", ".ah-tree-row", (e) => { this.rowClick(e, false); });
    this.delegate("dblclick", ".ah-tree-row", (e) => { this.rowClick(e, true); });
    this.listen(el, "keydown", (e) => { this.keydown(e); });
    if (!el.querySelector("li[role=treeitem][tabindex='0']")) { focusItem(el, visible(el)[0], false); }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v) {
    const el = this.element, li = byValue(el, v);
    if (!li) {
      mark(el, null);
      writeValue(el, "");
      return;
    }
    ensureVisible(this, li);
    this.select(li, false);
  }
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  expand(v) { this.setOpen(byValue(this.element, v), true); }
  collapse(v) { this.setOpen(byValue(this.element, v), false); }
  expandAll() {
    items(this.element).filter((li) => expandable(li) && !li.hasAttribute("data-lazy"))
      .forEach((li) => { this.setOpen(li, true); });
  }
  collapseAll() {
    items(this.element).filter(expandable).forEach((li) => { this.setOpen(li, false); });
  }
  ensureVisible(v) {
    const li = byValue(this.element, v);
    if (li) { ensureVisible(this, li); }
  }
  childrenLoaded(id) {
    const el = this.element, li = document.getElementById(id);
    if (!li) { return; }
    li.classList.remove("ah-tree-item-loading");
    ["aria-busy", "data-lazy", "data-ah-on"].forEach((a) => { li.removeAttribute(a); });
    const ul = group(li);
    if (!ul || !ul.querySelector(":scope > li")) {
      li.removeAttribute("aria-expanded");
      li.classList.add("ah-tree-item-leaf");
      const t = row(li) && row(li).querySelector(":scope > .ah-tree-toggle");
      if (t) { t.classList.add("ah-tree-toggle-leaf"); }
      return;
    }
    const sel = byValue(el, el.getAttribute("data-ah-value"));
    if (sel && li.contains(sel)) { mark(el, sel); }
    this.setOpen(li, true);
  }

  setOpen(li, open, animate) {
    const el = this.element;
    if (!li || !expandable(li) || isOpen(li) === open) { return; }
    if (open && li.getAttribute("data-lazy") === "true") {
      this.load(li);
      return;
    }
    li.setAttribute("aria-expanded", String(open));
    const t = row(li) && row(li).querySelector(":scope > .ah-tree-toggle");
    if (t) { t.classList.toggle("ah-tree-toggle-open", open); }
    // collapsing hides the focused item: the node itself takes the tab stop
    if (!open && li.querySelector("li[tabindex='0']")) {
      focusItem(el, li, li.contains(document.activeElement));
    }
    const ul = group(li);
    const done = () => { this.fire(open ? "ah:expand" : "ah:collapse", info(li)); };
    if (ul && animate !== false && el.getAttribute("data-animation") !== "none" && ul.animate) {
      slide(ul, open, done);
    } else {
      if (ul) {
        finishSlide(ul);
        ul.style.display = open ? "" : "none";
      }
      done();
    }
  }

  // A lazy node asks the server for its children: the tree's load token is
  // bound to the node (ah:load), so each node has its own request.
  load(li) {
    if (li.classList.contains("ah-tree-item-loading")) { return; }
    li.classList.add("ah-tree-item-loading");
    li.setAttribute("aria-busy", "true");
    const token = this.element.getAttribute("data-load");
    if (token && !li.hasAttribute("data-ah-on")) {
      li.setAttribute("data-ah-on", "ah:load:" + token);
      AH.mount(li);                 // registers the ah:load listener
    }
    this.fire("ah:load", info(li), li);
  }

  select(li, user) {
    const el = this.element;
    if (!li || isDisabled(li)) { return; }
    const prev = el.getAttribute("data-ah-value");
    const v = li.getAttribute("data-value");
    mark(el, li);
    writeValue(el, v);
    focusItem(el, li, false);
    if (user && prev !== v) { this.fire("change"); }
  }

  keydown(e) {
    const el = this.element;
    const li = e.target.closest && e.target.closest("li[role=treeitem]");
    if (!li || !own(el, li) || treeOff(el) || e.altKey || e.ctrlKey || e.metaKey) { return; }
    const vis = visible(el);
    const i = vis.indexOf(li);
    let to = null;
    switch (e.key) {
      case "ArrowDown": to = vis[Math.min(i + 1, vis.length - 1)]; break;
      case "ArrowUp": to = vis[Math.max(i - 1, 0)]; break;
      case "Home": to = vis[0]; break;
      case "End": to = vis[vis.length - 1]; break;
      case "ArrowRight":
        if (expandable(li) && !isOpen(li)) { this.setOpen(li, true); }
        else if (isOpen(li)) {
          const ul = group(li);
          to = (ul && ul.querySelector(":scope > li")) || null;
        }
        break;
      case "ArrowLeft":
        if (isOpen(li)) { this.setOpen(li, false); } else { to = parentItem(el, li); }
        break;
      case "Enter":
      case " ":
        this.select(li, true);
        break;
      case "*":
        Array.from(li.parentNode.children)
          .filter((n) => n === li || (n.matches("li[aria-expanded]")))
          .forEach((n) => { this.setOpen(n, true); });
        break;
      default:
        if (e.key && e.key.length === 1 && /\S/.test(e.key)) {
          to = typeahead(el, li, e.key.toLowerCase());
          if (!to) { return; }
        } else {
          return;
        }
    }
    e.preventDefault();
    if (to) { focusItem(el, to, true); }
  }

  rowClick(e, dbl) {
    const el = this.element;
    const li = e.target.closest("li[role=treeitem]");
    if (!li || !own(el, li) || isDisabled(li) || treeOff(el)) { return; }
    const onToggle = !!e.target.closest(".ah-tree-toggle");
    const mode = el.getAttribute("data-toggle-mode") || "click";
    if (dbl) {
      if (mode === "dblclick" && !onToggle) { this.setOpen(li, !isOpen(li)); }
      return;
    }
    this.fire("ah:item-click", info(li));
    this.select(li, true);
    focusItem(el, li, true);
    if (expandable(li) && (onToggle || mode === "click")) { this.setOpen(li, !isOpen(li)); }
  }
});
