/* Behaviour of transfer (designs/04-components.md), ported from sigil's
 * form/transfer. Both lists are rendered on the server (aihtml_transfer);
 * moving an item moves its node. Value contract: data-ah-value (the keys
 * of the target list), the hidden input and a native "change" on the
 * root. Shared helpers: _lib_list.ts. */
import AH from "../core.ts";
import { ensureId, publish, split, join, shown, enabled, scrollInto, kids, childText } from "./_lib_list.ts";

/** One of the two lists. */
type Side = "source" | "target";

function items(list: Element): HTMLElement[] { return kids(list, ".ah-transfer-item"); }
function rows(list: Element): HTMLElement[] {
  return items(list).filter((li) => shown(li) && enabled(li));
}
function isSel(li: Element): boolean { return li.classList.contains("ah-transfer-item-selected"); }
function selected(list: Element): HTMLElement[] { return rows(list).filter(isSel); }

function select(li: Element, on: boolean): void {
  li.classList.toggle("ah-transfer-item-selected", on);
  li.setAttribute("aria-selected", String(on));
}

function idx(li: Element): number { return parseInt(li.getAttribute("data-idx") || "", 10) || 0; }

// Back to the source: in the order of the items.
function insertOrdered(list: Element, li: Element): void {
  const after = items(list).filter((x) => idx(x) < idx(li)).pop();
  if (after) { after.after(li); } else { list.prepend(li); }
}

class TransferController extends AH.Controller {
  // the two lists are always rendered (aihtml_transfer)
  private source!: HTMLElement;
  private target!: HTMLElement;
  private disabled = false;
  private cursor: HTMLElement | null = null;

  override setup(): void {
    const el = this.element;
    ensureId(el, "ah-tr");
    this.source = el.querySelector<HTMLElement>(".ah-transfer-list[data-panel=source]") as HTMLElement;
    this.target = el.querySelector<HTMLElement>(".ah-transfer-list[data-panel=target]") as HTMLElement;
    this.disabled = el.classList.contains("ah-transfer-disabled");
    this.cursor = null;
    this.delegate("mousedown", ".ah-transfer-item", (e) => {
      if (e.shiftKey || e.detail > 1) { e.preventDefault(); }  // no text selection
    });
    this.delegate("click", ".ah-transfer-item", (_e, li) => {
      if (this.disabled || !enabled(li)) { return; }
      select(li, !isSel(li));
      this.moveCursor(li);
      this.sync();
    });
    this.delegate("dblclick", ".ah-transfer-item", (_e, li) => {
      if (this.disabled || !enabled(li)) { return; }
      select(li, true);
      const panel = li.parentElement ? li.parentElement.getAttribute("data-panel") : null;
      this.move(panel === "source" ? "source" : "target");
    });
    this.delegate("click", ".ah-transfer-btn", (e, btn) => {
      e.preventDefault();
      this.move(btn.getAttribute("data-direction") === "to-target" ? "source" : "target");
    });
    this.delegate("keydown", ".ah-transfer-list", (e, list) => { this.key(list, e); });
    this.delegate("focusin", ".ah-transfer-list", (_e, list) => {
      if (!this.cursor || this.cursor.parentNode !== list) { this.moveCursor(rows(list)[0]); }
    });
    this.delegate("input", ".ah-transfer-filter-input", () => { this.sync(); });
    this.delegate("change", ".ah-transfer-filter-input", (e) => { e.stopPropagation(); });
    this.sync();
  }

  // methods (aihtml_action:call/4, AH.invoke)
  /** The keys of the right list, in order; no change event. */
  setValue(v: string | readonly string[] | null): void {
    const keys = split(v);
    const all = items(this.source).concat(items(this.target));
    all.sort((a, b) => idx(a) - idx(b));
    all.forEach((li) => { select(li, false); });
    keys.forEach((k) => {
      const li = all.filter((x) => x.getAttribute("data-value") === k)[0];
      if (li) { li.setAttribute("data-source", "target"); this.target.append(li); }
    });
    all.forEach((li) => {
      if (keys.indexOf(li.getAttribute("data-value") || "") < 0) {
        li.setAttribute("data-source", "source");
        this.source.append(li);
      }
    });
    this.moveCursor(null);
    this.sync();
    this.publish(false);
  }
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }
  moveToTarget(): void { this.move("source"); }
  moveToSource(): void { this.move("target"); }
  selectAll(side?: Side): void {
    rows(this.list(side || "source")).forEach((li) => { select(li, true); });
    this.sync();
  }
  clearSelection(side?: Side): void {
    items(this.list(side || "source")).forEach((li) => { select(li, false); });
    this.sync();
  }

  private list(side: string): HTMLElement { return side === "source" ? this.source : this.target; }

  // Counts, move buttons, filters.
  private sync(): void {
    (["source", "target"] as const).forEach((side) => {
      const list = this.list(side);
      const panel = list.closest(".ah-transfer-panel");
      if (!panel) { return; }
      const count = panel.querySelector(".ah-transfer-panel-count");
      if (count) { count.textContent = String(items(list).length); }
      const input = panel.querySelector<HTMLInputElement>(".ah-transfer-filter-input");
      const q = String(input ? input.value : "").trim().toLowerCase();
      items(list).forEach((li) => {
        const hit = !q || childText(li, ".ah-transfer-item-label").toLowerCase().indexOf(q) >= 0;
        li.style.display = hit ? "" : "none";
        if (!hit) { select(li, false); }
      });
      const off = this.disabled || !selected(list).length;
      this.element.querySelectorAll<HTMLButtonElement>(side === "source" ? ".ah-transfer-btn-to-target"
                                                                         : ".ah-transfer-btn-to-source")
        .forEach((btn) => {
          btn.classList.toggle("ah-transfer-btn-disabled", off);
          btn.disabled = off;
        });
    });
  }

  private publish(fire: boolean): void {
    publish(this.element, join(items(this.target).map((li) => li.getAttribute("data-value") || "")), fire);
  }

  private move(from: Side): void {
    if (this.disabled) { return; }
    const moving = selected(this.list(from));
    if (!moving.length) { return; }
    const to: Side = from === "source" ? "target" : "source";
    const dest = this.list(to);
    moving.forEach((li) => {
      select(li, false);
      li.classList.remove("ah-transfer-item-focused");
      li.setAttribute("data-source", to);
      if (to === "target") { dest.append(li); } else { insertOrdered(dest, li); }
    });
    if (this.cursor && moving.indexOf(this.cursor) >= 0) { this.moveCursor(null); }
    this.sync();
    this.publish(true);
  }

  private moveCursor(li: HTMLElement | null | undefined): void {
    this.element.querySelectorAll(".ah-transfer-item-focused").forEach((x) => {
      x.classList.remove("ah-transfer-item-focused");
    });
    this.source.removeAttribute("aria-activedescendant");
    this.target.removeAttribute("aria-activedescendant");
    this.cursor = li || null;
    if (!li) { return; }
    li.classList.add("ah-transfer-item-focused");
    const list = li.parentElement;
    if (!list) { return; }
    list.setAttribute("aria-activedescendant", li.id);
    scrollInto(list.parentElement, li);
  }

  private key(list: HTMLElement, e: KeyboardEvent): void {
    if (this.disabled) { return; }
    const rs = rows(list), side: Side = list.getAttribute("data-panel") === "source" ? "source" : "target";
    const i = this.cursor ? rs.indexOf(this.cursor) : -1;
    switch (e.key) {
      case "ArrowDown": e.preventDefault(); this.moveCursor(rs[Math.min(i + 1, rs.length - 1)]); break;
      case "ArrowUp": e.preventDefault(); this.moveCursor(rs[Math.max(i - 1, 0)]); break;
      case "Home": e.preventDefault(); this.moveCursor(rs[0]); break;
      case "End": e.preventDefault(); this.moveCursor(rs[rs.length - 1]); break;
      case " ":
        e.preventDefault();
        if (this.cursor && rs.indexOf(this.cursor) >= 0) {
          select(this.cursor, !isSel(this.cursor));
          this.sync();
        }
        break;
      case "Enter": {
        e.preventDefault();
        if (!selected(list).length && this.cursor && rs.indexOf(this.cursor) >= 0) { select(this.cursor, true); }
        const next = rs.filter((li) => !isSel(li))[0];
        this.move(side);
        if (next) { this.moveCursor(next); }
        break;
      }
      default:
        if ((e.key === "a" || e.key === "A") && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          rs.forEach((li) => { select(li, true); });
          this.sync();
        }
    }
  }
}

AH.register("transfer", TransferController);
