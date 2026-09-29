/* tag-cloud behaviour (designs/04-components.md): "ah:tag-click"
   (cancelable, detail {label, value, url, index}); links without a url
   behave as buttons. */
import AH from "../core.js";
import "./_lib_display.js";

const L = AH.lib.display;

AH.register("tag-cloud", class extends AH.Controller {
  setup() {
    this.delegate("click", ".ah-tagcloud-link", (e, a) => {
      const item = a.closest(".ah-tagcloud-item");
      const ok = this.fire("ah:tag-click", {
        label: a.getAttribute("data-ah-label"),
        value: L.num(a.getAttribute("data-ah-weight"), 0),
        url: a.getAttribute("href"),
        index: L.num(item && item.getAttribute("data-index"), 0)
      });
      if (!ok || !a.hasAttribute("href")) { e.preventDefault(); }
    });
    this.delegate("keydown", ".ah-tagcloud-link:not([href])", L.keyClick);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  hideItem(i) { this.items(i).forEach((n) => { n.style.display = "none"; }); }
  showItem(i) { this.items(i).forEach((n) => { n.style.display = ""; }); }

  items(index) {
    return this.element.querySelectorAll(".ah-tagcloud-item[data-index='" + parseInt(index, 10) + "']");
  }
});
