/* alert behaviour (designs/04-components.md): dismiss. "ah:dismiss"
   (cancelable, no detail) before the alert is removed. */
import AH from "../core.js";
import "./_lib_display.js";

AH.register("alert", class extends AH.Controller {
  setup() {
    this.delegate("click", ".ah-alert-close", () => { this.dismiss(); });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  dismiss() {
    if (this.fire("ah:dismiss")) { AH.lib.display.drop(this.element); }
  }
});
