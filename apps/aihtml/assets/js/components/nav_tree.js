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
import AH from "../core.js";

function navActivate(el, route) {
  el.querySelectorAll(".ah-nav-tree__item.ah-is-active").forEach(function (n) {
    n.classList.remove("ah-is-active");
    n.removeAttribute("aria-current");
  });
  const a = Array.from(el.querySelectorAll("a.ah-nav-tree__item")).find(function (n) {
    return n.getAttribute("data-route") === route;
  });
  if (a) {
    a.classList.add("ah-is-active");
    a.setAttribute("aria-current", "page");
  }
  // as sigil re-renders: the nodes around the active link are open, others closed
  el.querySelectorAll("details.ah-nav-tree__node").forEach(function (d) {
    const hit = !!a && d.contains(a);
    d.open = hit;
    d.querySelectorAll(":scope > summary").forEach(function (s) { s.classList.toggle("ah-is-open", hit); });
  });
  el.setAttribute("data-ah-value", route || "");
}

AH.register("nav-tree", class extends AH.Controller {
  setup() {
    const el = this.element;
    const self = this;
    this.delegate("click", "a.ah-nav-tree__item[data-route]", function (e, a) {
      const route = a.getAttribute("data-route");
      const prev = el.getAttribute("data-ah-value");
      navActivate(el, route);
      if (route !== prev) { self.fire("change"); }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(route) { navActivate(this.element, route === null || route === undefined ? "" : String(route)); }
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
});
