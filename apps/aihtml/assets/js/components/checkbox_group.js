/* checkbox-group behaviour (designs/04-components.md), see
 * AH.lib.choice.defineGroup.
 */
// ah-define: checkbox-group
import AH from "../core.js";
import "./_lib_choice.js";

var L = AH.lib.choice;

L.defineGroup("checkbox-group", {
  radio: false, sync: L.syncCheckbox, item: ".ah-checkbox-group-item",
  itemDisabled: "ah-checkbox-group-item-disabled", disabled: "ah-checkbox-group-disabled"
});
