/* radiobutton-group behaviour (designs/04-components.md), see
 * ChoiceGroupController in _lib_choice.ts; arrow keys move the selection.
 */
// ah-define: radiobutton-group
import AH from "../core.ts";
import { ChoiceGroupController, syncRadio } from "./_lib_choice.ts";
import type { GroupKind } from "./_lib_choice.ts";

class RadiobuttonGroupController extends ChoiceGroupController {
  protected readonly kind: GroupKind = {
    radio: true, sync: syncRadio, item: ".ah-radiobutton-group-item",
    itemDisabled: "ah-radiobutton-group-item-disabled", disabled: "ah-radiobutton-group-disabled"
  };
}

AH.register("radiobutton-group", RadiobuttonGroupController);
