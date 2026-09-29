/* The docking behaviour (designs/04-components.md), ported from sigil's
 * layout/docking (+ docking/drag): windows dragged by their header between
 * panels, left floating, collapsed, closed; Alt+arrows move the focused
 * window from the keyboard.
 *
 * The arrangement is the value: JSON in data-ah-value (and the hidden
 * input), rewritten after every change, and `change' on the root when it
 * changed (user actions and the rearranging methods; setLayout excepted).
 * Windows are only ever moved, never rebuilt, so their contents keep
 * their state. The drag tracking is shared with dock_layout.js
 * (_lib_dock.js).
 *
 * Events: "ah:window-close", "ah:window-collapse", "ah:window-expand" on
 * the root with the window id in data-window (and as the detail).
 */
import AH from "../core.js";
import "./_lib_dock.js";

const L = AH.lib.dock;
const { inside, parse, commit, fire, track, stopDrag, own, kids } = L;

const WIN = ".ah-docking-window";

function dkPanels(el) { return kids(el, ".ah-docking-panel"); }
function dkWid(win) { return win.getAttribute("data-window-id"); }
function panelWins(panel, skip) { return kids(panel, WIN).filter((w) => w !== skip); }

function dkWindow(el, id) {
  return Array.from(el.querySelectorAll(WIN)).find((w) =>
    dkWid(w) === String(id) && w.closest(".ah-docking") === el) || null;
}

function dkPanel(el, id) {
  return dkPanels(el).find((p) => p.getAttribute("data-panel-id") === String(id)) || null;
}

function dkSerialize(ctl) {
  const el = ctl.element;
  const panels = [], floating = [], collapsed = [];
  dkPanels(el).forEach((p) => {
    panels.push({ id: p.getAttribute("data-panel-id"), windows: kids(p, WIN).map(dkWid) });
  });
  kids(el, ".ah-docking-window-floating").forEach((w) => {
    floating.push({ id: dkWid(w), x: Math.round(parseFloat(w.style.left) || 0),
                    y: Math.round(parseFloat(w.style.top) || 0),
                    width: Math.round(parseFloat(w.style.width) || w.offsetWidth) });
  });
  // in document order
  const coll = [];
  dkPanels(el).forEach((p) => { coll.push(...kids(p, ".ah-docking-window-collapsed")); });
  coll.push(...kids(el, ".ah-docking-window-floating.ah-docking-window-collapsed"));
  coll.sort((a, b) => (a.compareDocumentPosition(b) & Node.DOCUMENT_POSITION_FOLLOWING) ? -1 : 1);
  coll.forEach((w) => { collapsed.push(dkWid(w)); });
  return JSON.stringify({ panels: panels, floating: floating, collapsed: collapsed,
                          closed: ctl.closed });
}

function dkDock(win) {
  win.classList.remove("ah-docking-window-floating");
  win.classList.add("ah-docking-window-docked");
  Object.assign(win.style, { left: "", top: "", width: "", zIndex: "", opacity: "" });
}

function dkFloat(el, win, x, y, w) {
  win.classList.remove("ah-docking-window-docked");
  win.classList.add("ah-docking-window-floating");
  win.style.left = Math.round(x) + "px";
  win.style.top = Math.round(y) + "px";
  if (w) { win.style.width = Math.round(w) + "px"; }
  if (win.parentNode !== el) { el.appendChild(win); }
}

// Insert a window into a panel at an index (-1 or past the end: last).
function dkInsert(panel, win, index) {
  const ws = panelWins(panel, win);
  dkDock(win);
  if (index < 0 || index >= ws.length) { panel.appendChild(win); }
  else { panel.insertBefore(win, ws[index]); }
}

function dkIndexAt(panel, y, skip) {
  const ws = panelWins(panel, skip);
  for (let i = 0; i < ws.length; i++) {
    const r = ws[i].getBoundingClientRect();
    if (y < r.top + r.height / 2) { return i; }
  }
  return ws.length;
}

function setCollapsed(win, on) {
  win.classList.toggle("ah-docking-window-collapsed", on);
  const btn = win.querySelector(".ah-docking-window-collapse-btn");
  if (btn) { btn.setAttribute("aria-expanded", on ? "false" : "true"); }
}

AH.register("docking", class extends AH.Controller {
  setup() {
    const el = this.element;
    const v = parse(el.getAttribute("data-ah-value"));
    this.closed = (v && Array.isArray(v.closed)) ? v.closed.slice() : [];
    const live = (node) => own(el, node, ".ah-docking") && !this.disabled();
    this.delegate("pointerdown", ".ah-docking-window-header", (e, hd) => {
      const win = hd.parentNode;
      if (e.button !== 0 || !live(hd) || win.classList.contains("ah-docking-window-pinned") ||
          e.target.closest("button, a, input, select, textarea")) { return; }
      this.drag(win, e);
    });
    this.delegate("click", ".ah-docking-window-collapse-btn", (e, btn) => {
      if (!live(btn)) { return; }
      e.stopPropagation();
      const win = btn.closest(WIN);
      this.setCollapse(win, !win.classList.contains("ah-docking-window-collapsed"), true);
    });
    this.delegate("click", ".ah-docking-window-close-btn", (e, btn) => {
      if (!live(btn)) { return; }
      e.stopPropagation();
      this.closeWin(btn.closest(WIN), true);
    });
    this.delegate("keydown", ".ah-docking-window-header", (e, hd) => {
      if (e.target !== hd || !live(hd)) { return; }
      this.key(hd.parentNode, e);
    });
    this.commit(false);
  }

  teardown() { stopDrag(this.element); }

  // methods (aihtml_action:call/4, AH.invoke)
  collapse(id) { const w = dkWindow(this.element, id); if (w) { this.setCollapse(w, true, false); } }
  expand(id) { const w = dkWindow(this.element, id); if (w) { this.setCollapse(w, false, false); } }
  close(id) { const w = dkWindow(this.element, id); if (w) { this.closeWin(w, false); } }
  move(id, panel, index) {
    const el = this.element, w = dkWindow(el, id);
    const p = typeof panel === "number" ? dkPanels(el)[panel] : dkPanel(el, panel);
    if (!w || !p) { return; }
    dkInsert(p, w, index === undefined ? -1 : index);
    this.commit(true);
  }
  pin(id) { const w = dkWindow(this.element, id); if (w) { w.classList.add("ah-docking-window-pinned"); } }
  unpin(id) { const w = dkWindow(this.element, id); if (w) { w.classList.remove("ah-docking-window-pinned"); } }
  addWindow(panel, html) {
    const el = this.element, p = dkPanel(el, panel);
    const w = L.parseHTML(html).find((n) => n.matches(WIN));
    if (!p || !w) { return; }
    const old = dkWindow(el, dkWid(w));
    if (old) { L.drop(old); }
    p.appendChild(w);
    AH.mount(w);
    const i = this.closed.indexOf(dkWid(w));
    if (i >= 0) { this.closed.splice(i, 1); }
    this.commit(true);
  }
  setLayout(json) {
    this.apply(typeof json === "string" ? parse(json) : json);
    this.commit(false);
  }
  disable() {
    this.element.classList.add("ah-docking-disabled");
    this.element.setAttribute("aria-disabled", "true");
  }
  enable() {
    this.element.classList.remove("ah-docking-disabled");
    this.element.removeAttribute("aria-disabled");
  }
  getValue() { return this.element.getAttribute("data-ah-value"); }

  // internals
  disabled() { return this.element.classList.contains("ah-docking-disabled"); }
  commit(fire_) { return commit(this.element, dkSerialize(this), fire_); }

  announce(text) {
    const live = kids(this.element, ".ah-docking-live");
    live.forEach((n) => { n.textContent = ""; });
    setTimeout(() => { live.forEach((n) => { n.textContent = text; }); }, 20);
  }

  setCollapse(win, on, user) {
    if (win.classList.contains("ah-docking-window-collapsed") === on) { return; }
    setCollapsed(win, on);
    if (user) { fire(this.element, on ? "ah:window-collapse" : "ah:window-expand", "window", dkWid(win)); }
    this.commit(true);
  }

  closeWin(win, user) {
    const el = this.element, id = dkWid(win);
    const focus = win.contains(document.activeElement);
    let next = win.nextElementSibling;
    while (next && !next.matches(WIN)) { next = next.nextElementSibling; }
    if (!next) {
      next = win.previousElementSibling;
      while (next && !next.matches(WIN)) { next = next.previousElementSibling; }
    }
    if (user) { fire(el, "ah:window-close", "window", id); }
    L.drop(win);
    if (this.closed.indexOf(id) < 0) { this.closed.push(id); }
    if (focus && next) {
      const hd = next.querySelector(":scope > .ah-docking-window-header");
      if (hd) { hd.focus(); }
    }
    this.commit(true);
  }

  drag(win, e) {
    const root = this.element;
    let origin, offX, offY, target = null, index = 0;
    const ind = document.createElement("div");
    ind.className = "ah-docking-drop-indicator";
    ind.setAttribute("aria-hidden", "true");
    const opacity = parseFloat(root.getAttribute("data-ah-drag-opacity"));
    track(root, e, {
      start: () => {
        const r = win.getBoundingClientRect(), rr = root.getBoundingClientRect();
        origin = { parent: win.parentNode, next: win.nextSibling,
                   floating: win.classList.contains("ah-docking-window-floating"),
                   style: win.getAttribute("style") };
        offX = e.clientX - r.left;
        offY = e.clientY - r.top;
        dkFloat(root, win, r.left - rr.left - root.clientLeft, r.top - rr.top - root.clientTop, r.width);
        win.classList.add("ah-docking-window-dragging");
        win.style.zIndex = 9999;
        win.style.opacity = isNaN(opacity) ? "" : opacity;
      },
      move: (ev) => {
        const rr = root.getBoundingClientRect();
        win.style.left = Math.round(ev.clientX - offX - rr.left - root.clientLeft) + "px";
        win.style.top = Math.round(ev.clientY - offY - rr.top - root.clientTop) + "px";
        target = null;
        dkPanels(root).forEach((p) => {
          if (inside(ev.clientX, ev.clientY, p.getBoundingClientRect())) { target = p; }
        });
        ind.remove();
        if (target) {
          index = dkIndexAt(target, ev.clientY, win);
          const ws = panelWins(target, win);
          if (index < ws.length) { target.insertBefore(ind, ws[index]); }
          else { target.appendChild(ind); }
        }
      },
      end: (ev, cancelled) => {
        ind.remove();
        win.classList.remove("ah-docking-window-dragging");
        win.style.zIndex = "";
        win.style.opacity = "";
        const allowFloat = root.getAttribute("data-ah-allow-float") !== "false";
        if (!cancelled && target) {
          dkInsert(target, win, index);
        } else if (!cancelled && allowFloat) {
          // keep the floating window (its header at least) inside the container
          win.style.left = Math.max(0, Math.min(parseFloat(win.style.left) || 0,
                                                root.clientWidth - 60)) + "px";
          win.style.top = Math.max(0, Math.min(parseFloat(win.style.top) || 0,
                                               root.clientHeight - 40)) + "px";
        } else if (origin.floating) {
          win.setAttribute("style", origin.style || "");
        } else {
          dkDock(win);
          origin.parent.insertBefore(win, origin.next && origin.next.parentNode === origin.parent
                                     ? origin.next : null);
        }
        this.commit(true);
      }
    });
  }

  // Alt+arrows: move the focused window within its panel or to the next
  // panel; plain arrows move a floating window.
  key(win, e) {
    const el = this.element;
    const header = win.querySelector(":scope > .ah-docking-window-header");
    const k = e.key;
    let moved = false;
    if (k === "Delete" && win.querySelector(".ah-docking-window-close-btn")) {
      e.preventDefault();
      this.closeWin(win, true);
      return;
    }
    if (!/^Arrow/.test(k) || win.classList.contains("ah-docking-window-pinned")) { return; }
    const panels = dkPanels(el);
    if (win.classList.contains("ah-docking-window-floating")) {
      if (e.altKey) {
        dkInsert(panels[0], win, -1);
      } else {
        const dx = k === "ArrowLeft" ? -10 : k === "ArrowRight" ? 10 : 0;
        const dy = k === "ArrowUp" ? -10 : k === "ArrowDown" ? 10 : 0;
        win.style.left = ((parseFloat(win.style.left) || 0) + dx) + "px";
        win.style.top = ((parseFloat(win.style.top) || 0) + dy) + "px";
      }
      moved = true;
    } else if (e.altKey) {
      const panel = win.parentNode, pi = panels.indexOf(panel);
      const i = kids(panel, WIN).indexOf(win);
      const vertical = el.classList.contains("ah-docking-vertical");
      const along = vertical ? { prev: "ArrowLeft", next: "ArrowRight" } : { prev: "ArrowUp", next: "ArrowDown" };
      const across = vertical ? { prev: "ArrowUp", next: "ArrowDown" } : { prev: "ArrowLeft", next: "ArrowRight" };
      if (k === along.prev && i > 0) { dkInsert(panel, win, i - 1); moved = true; }
      else if (k === along.next) { dkInsert(panel, win, i + 1); moved = true; }
      else if (k === across.prev && pi > 0) { dkInsert(panels[pi - 1], win, i); moved = true; }
      else if (k === across.next && pi < panels.length - 1) { dkInsert(panels[pi + 1], win, i); moved = true; }
    }
    if (!moved) { return; }
    e.preventDefault();
    header.focus();
    if (!win.classList.contains("ah-docking-window-floating")) {
      const p = win.parentNode, title = win.querySelector(".ah-docking-window-title");
      this.announce((title ? title.textContent : "") + ": " +
                    (panels.indexOf(p) + 1) + " / " + (kids(p, WIN).indexOf(win) + 1));
    }
    this.commit(true);
  }

  apply(v) {
    const el = this.element;
    if (!v) { return; }
    (v.closed || []).forEach((id) => {
      const w = dkWindow(el, id);
      if (w) { L.drop(w); }
      if (this.closed.indexOf(String(id)) < 0) { this.closed.push(String(id)); }
    });
    (v.panels || []).forEach((p) => {
      const panel = dkPanel(el, p.id);
      if (!panel) { return; }
      (p.windows || []).forEach((id) => {
        const w = dkWindow(el, id);
        if (w) { dkInsert(panel, w, -1); }
      });
    });
    (v.floating || []).forEach((f) => {
      const w = dkWindow(el, f.id);
      if (w) { dkFloat(el, w, f.x || 0, f.y || 0, f.width); }
    });
    const collapsed = (v.collapsed || []).map(String);
    el.querySelectorAll(WIN).forEach((w) => {
      if (w.closest(".ah-docking") !== el) { return; }
      setCollapsed(w, collapsed.indexOf(dkWid(w)) >= 0);
    });
  }
});
