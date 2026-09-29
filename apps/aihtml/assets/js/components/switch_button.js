/* switch-button behaviour (designs/04-components.md): mirrors the native
 * checkbox onto sigil's classes; locked. Methods never fire change.
 */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_choice.js";

var NS = AH.NS;
var L = AH.lib.choice;

AH.define("switch-button", {
  init: function (el, $el) {
    L.bindLocked(el, $el);
    $el.on("change" + NS, L.INPUT, function () { L.syncSwitch(this); });
  },
  methods: {
    setChecked: function (el, $el, v) {
      var input = L.inputOf($el);
      input.checked = L.truthy(v);
      L.syncSwitch(input);
    },
    getValue: function (el, $el) { return L.inputOf($el).checked; },
    setDisabled: function (el, $el, on) { L.setDisabled(el, L.inputOf($el), on, L.syncSwitch); }
  }
});
