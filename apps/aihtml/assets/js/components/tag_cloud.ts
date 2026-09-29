/* tag-cloud behaviour (designs/04-components.md): "ah:tag-click"
   (cancelable, detail TagClick {label, value, url, index}); links without
   a url behave as buttons. */
import AH from "../core.ts";
import { keyClick, num } from "./_lib_display.ts";

/** Detail of ah:tag-click. */
export interface TagClick { label: string | null; value: number; url: string | null; index: number; }

class TagCloudController extends AH.Controller {
  override setup(): void {
    this.delegate("click", ".ah-tagcloud-link", (e, a) => {
      const item = a.closest(".ah-tagcloud-item");
      const ok = this.fire<TagClick>("ah:tag-click", {
        label: a.getAttribute("data-ah-label"),
        value: num(a.getAttribute("data-ah-weight"), 0),
        url: a.getAttribute("href"),
        index: num(item && item.getAttribute("data-index"), 0)
      });
      if (!ok || !a.hasAttribute("href")) { e.preventDefault(); }
    });
    this.delegate("keydown", ".ah-tagcloud-link:not([href])", (e, item) => { keyClick(e, item); });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  hideItem(i: number | string): void { this.items(i).forEach((n) => { n.style.display = "none"; }); }
  showItem(i: number | string): void { this.items(i).forEach((n) => { n.style.display = ""; }); }

  private items(index: number | string): NodeListOf<HTMLElement> {
    return this.element.querySelectorAll<HTMLElement>(
      ".ah-tagcloud-item[data-index='" + parseInt(String(index), 10) + "']");
  }
}

AH.register("tag-cloud", TagCloudController);
