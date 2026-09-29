/* radio-cards behaviour (designs/04-components.md), see
 * AH.lib.choice.defineGroup; arrow keys move the selection.
 */
// ah-define: radio-cards
import AH from "../core.js";
import "./_lib_choice.js";

var L = AH.lib.choice;

function syncCard(input) {
  var card = input.closest(".ah-radio-cards__card");
  if (!card) { return; }
  card.setAttribute("data-selected", input.checked ? "true" : "false");
  card.setAttribute("data-disabled", input.disabled ? "true" : "false");
}

L.defineGroup("radio-cards", {
  radio: true, sync: syncCard, item: ".ah-radio-cards__card", disabledAttr: true
});
