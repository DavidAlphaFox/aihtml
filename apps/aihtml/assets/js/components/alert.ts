/* alert behaviour (designs/04-components.md): dismiss. "ah:dismiss"
   (cancelable, no detail) before the alert is removed. */
import AH from "../core.ts";
import { drop } from "./_lib_display.ts";

class AlertController extends AH.Controller {
  override setup(): void {
    this.delegate("click", ".ah-alert-close", () => { this.dismiss(); });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  dismiss(): void {
    if (this.fire("ah:dismiss")) { drop(this.element); }
  }
}

AH.register("alert", AlertController);
