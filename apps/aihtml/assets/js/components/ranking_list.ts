/* ranking-list behaviour (designs/04-components.md): clickable rows fire
   "ah:item-click" with detail RankingItemClick {index}. */
import AH from "../core.ts";
import { keyClick, num } from "./_lib_display.ts";

/** Detail of ah:item-click: the row's index. */
export interface RankingItemClick { index: number; }

const ITEM = ".ah-ranking-list__item--clickable";

class RankingListController extends AH.Controller {
  override setup(): void {
    this.delegate("click", ITEM, (_e, item) => {
      this.fire<RankingItemClick>("ah:item-click", { index: num(item.getAttribute("data-idx"), 0) });
    });
    this.delegate("keydown", ITEM, (e, item) => { keyClick(e, item); });
  }
}

AH.register("ranking-list", RankingListController);
