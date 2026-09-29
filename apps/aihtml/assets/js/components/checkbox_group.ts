/* checkbox-group behaviour (designs/04-components.md), see
 * ChoiceGroupController in _lib_choice.ts.
 */
// ah-define: checkbox-group
import AH from "../core.ts";
import { ChoiceGroupController, syncCheckbox } from "./_lib_choice.ts";
import type { GroupKind } from "./_lib_choice.ts";

class CheckboxGroupController extends ChoiceGroupController {
  protected readonly kind: GroupKind = {
    radio: false, sync: syncCheckbox, item: ".ah-checkbox-group-item",
    itemDisabled: "ah-checkbox-group-item-disabled", disabled: "ah-checkbox-group-disabled"
  };
}

AH.register("checkbox-group", CheckboxGroupController);
