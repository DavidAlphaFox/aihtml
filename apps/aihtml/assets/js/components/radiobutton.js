/* radiobutton behaviour (designs/04-components.md): restyles every radio
 * of the native group when one changes; locked. Methods never fire change.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.choice;

  // The radios a browser treats as one group with this one.
  function sameGroup(input) {
    if (!input.name) {
      return $(input);
    }
    return $(input.form || document).find("input[type=radio]").filter(function () {
      return this.name === input.name && this.form === input.form;
    });
  }

  function syncRadioGroup(input) {
    sameGroup(input).each(function () { L.syncRadio(this); });
  }

  AH.define("radiobutton", {
    init: function (el, $el) {
      L.bindLocked(el, $el);
      $el.on("change" + NS, L.INPUT, function () { syncRadioGroup(this); });
    },
    methods: {
      setChecked: function (el, $el, v) {
        var input = L.inputOf($el);
        input.checked = L.truthy(v);
        syncRadioGroup(input);
      },
      getValue: function (el, $el) { return L.inputOf($el).checked; },
      setDisabled: function (el, $el, on) { L.setDisabled(el, L.inputOf($el), on, L.syncRadio); }
    }
  });
})(window.jQuery, window.AH);
