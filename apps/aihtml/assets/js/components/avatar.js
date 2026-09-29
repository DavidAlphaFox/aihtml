/* avatar behaviour (designs/04-components.md): a failed image shows the
   fallback underneath. */
import AH from "../core.js";

AH.register("avatar", class extends AH.Controller {
  setup() {
    this.element.querySelectorAll(".ah-avatar__image").forEach((img) => {
      const broken = () => { img.classList.add("ah-avatar__image--broken"); };
      // error does not bubble, and may have happened before setup
      this.listen(img, "error", broken);
      if (img.complete && img.naturalWidth === 0) { broken(); }
    });
  }
});
