/* radio-cards behaviour (designs/04-components.md), see
 * ChoiceGroupController in _lib_choice.ts; arrow keys move the selection.
 */
// ah-define: radio-cards
import AH from "../core.ts";
import { ChoiceGroupController } from "./_lib_choice.ts";
import type { GroupKind } from "./_lib_choice.ts";

function syncCard(input: HTMLInputElement): void {
  const card = input.closest(".ah-radio-cards__card");
  if (!card) { return; }
  card.setAttribute("data-selected", input.checked ? "true" : "false");
  card.setAttribute("data-disabled", input.disabled ? "true" : "false");
}

class RadioCardsController extends ChoiceGroupController {
  protected readonly kind: GroupKind = {
    radio: true, sync: syncCard, item: ".ah-radio-cards__card", disabledAttr: true
  };
}

AH.register("radio-cards", RadioCardsController);
