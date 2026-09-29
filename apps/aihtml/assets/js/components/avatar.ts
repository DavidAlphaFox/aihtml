/* avatar behaviour (designs/04-components.md): a failed image shows the
   fallback underneath. */
import AH from "../core.ts";

class AvatarController extends AH.Controller {
  override setup(): void {
    this.element.querySelectorAll<HTMLImageElement>(".ah-avatar__image").forEach((img) => {
      const broken = (): void => { img.classList.add("ah-avatar__image--broken"); };
      // error does not bubble, and may have happened before setup
      this.listen(img, "error", broken);
      if (img.complete && img.naturalWidth === 0) { broken(); }
    });
  }
}

AH.register("avatar", AvatarController);
