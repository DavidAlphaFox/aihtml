/* Behaviour of the nav tree (designs/04-components.md).
 *
 * Ported from sigil (layout/nav_tree). The server renders links and
 * <details>; this keeps the active link, opens the nodes around it and
 * fires "change" when the user follows a link.
 *
 * Value-bearing roots keep their value in data-ah-value, mirror it into a
 * hidden input and fire "change" when the user changes it; methods called
 * by the server (AH.invoke / aihtml_action:call) do not fire it.
 */
import AH from "../core.ts";

class NavTreeController extends AH.Controller {
  override setup(): void {
    const el = this.element;
    this.delegate("click", "a.ah-nav-tree__item[data-route]", (_e, a) => {
      const route = a.getAttribute("data-route") || "";
      const prev = el.getAttribute("data-ah-value");
      this.activate(route);
      if (route !== prev) { this.fire("change"); }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(route: unknown): void { this.activate(route === null || route === undefined ? "" : String(route)); }
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }

  private activate(route: string): void {
    const el = this.element;
    el.querySelectorAll(".ah-nav-tree__item.ah-is-active").forEach((n) => {
      n.classList.remove("ah-is-active");
      n.removeAttribute("aria-current");
    });
    const a = Array.from(el.querySelectorAll("a.ah-nav-tree__item")).find((n) => n.getAttribute("data-route") === route);
    if (a) {
      a.classList.add("ah-is-active");
      a.setAttribute("aria-current", "page");
    }
    // as sigil re-renders: the nodes around the active link are open, others closed
    el.querySelectorAll<HTMLDetailsElement>("details.ah-nav-tree__node").forEach((d) => {
      const hit = !!a && d.contains(a);
      d.open = hit;
      d.querySelectorAll(":scope > summary").forEach((s) => { s.classList.toggle("ah-is-open", hit); });
    });
    el.setAttribute("data-ah-value", route || "");
  }
}

AH.register("nav-tree", NavTreeController);
