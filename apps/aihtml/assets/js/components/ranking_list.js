/* ranking-list behaviour (designs/04-components.md): clickable rows fire
   "ah:item-click" with detail {index}. */
import AH from "../core.js";
import "./_lib_display.js";

const L = AH.lib.display;
const ITEM = ".ah-ranking-list__item--clickable";

AH.register("ranking-list", class extends AH.Controller {
  setup() {
    this.delegate("click", ITEM, (e, item) => {
      this.fire("ah:item-click", { index: L.num(item.getAttribute("data-idx"), 0) });
    });
    this.delegate("keydown", ITEM, L.keyClick);
  }
});
