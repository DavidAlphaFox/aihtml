/* radio-cards behaviour (designs/04-components.md), see
 * AH.lib.choice.defineGroup; arrow keys move the selection.
 */
// ah-define: radio-cards
import $ from "jquery";
import AH from "../core.js";
import "./_lib_choice.js";

var L = AH.lib.choice;

function syncCard(input) {
  $(input).closest(".ah-radio-cards__card")
    .attr("data-selected", input.checked ? "true" : "false")
    .attr("data-disabled", input.disabled ? "true" : "false");
}

L.defineGroup("radio-cards", {
  radio: true, sync: syncCard, item: ".ah-radio-cards__card", disabledAttr: true
});
