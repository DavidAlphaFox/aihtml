/* radiobutton-group behaviour (designs/04-components.md), see
 * AH.lib.choice.defineGroup; arrow keys move the selection.
 */
// ah-define: radiobutton-group
import AH from "../core.js";
import "./_lib_choice.js";

var L = AH.lib.choice;

L.defineGroup("radiobutton-group", {
  radio: true, sync: L.syncRadio, item: ".ah-radiobutton-group-item",
  itemDisabled: "ah-radiobutton-group-item-disabled", disabled: "ah-radiobutton-group-disabled"
});
