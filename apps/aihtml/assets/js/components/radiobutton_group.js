/* radiobutton-group behaviour (designs/04-components.md), see
 * AH.lib.choice.defineGroup; arrow keys move the selection.
 */
(function ($, AH) {
  "use strict";

  var L = AH.lib.choice;

  L.defineGroup("radiobutton-group", {
    radio: true, sync: L.syncRadio, item: ".ah-radiobutton-group-item",
    itemDisabled: "ah-radiobutton-group-item-disabled", disabled: "ah-radiobutton-group-disabled"
  });
})(window.jQuery, window.AH);
