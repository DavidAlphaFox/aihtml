/* checkbox behaviour (designs/04-components.md): mirrors the native
 * input onto sigil's classes; sigil's three states (checked -> mixed ->
 * unchecked) and locked. Methods never fire change.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.choice;

  function checkState(input) {
    return input.indeterminate ? "mixed" : input.checked;
  }

  function setCheck(input, v) {
    var mixed = v === "mixed" || v === "indeterminate" || v === null;
    input.indeterminate = mixed;
    input.checked = !mixed && L.truthy(v);
    L.syncCheckbox(input);
  }

  AH.define("checkbox", {
    init: function (el, $el) {
      var input = L.inputOf($el);
      if (!input) { return; }
      if ($el.hasClass("ah-checkbox-indeterminate")) {
        input.indeterminate = true;
      }
      $.data(el, "ah-state", checkState(input));
      L.bindLocked(el, $el);
      $el.on("change" + NS, L.INPUT, function (e) {
        // sigil's three states: checked -> mixed -> unchecked -> checked
        if (!e.isTrigger && el.hasAttribute("data-ah-three-states")) {
          var prev = $.data(el, "ah-state");
          setCheck(input, prev === true ? "mixed" : prev === "mixed" ? false : true);
        }
        $.data(el, "ah-state", checkState(input));
        L.syncCheckbox(input);
      });
    },
    methods: {
      // true | false | "mixed"
      setChecked: function (el, $el, v) {
        var input = L.inputOf($el);
        setCheck(input, v);
        $.data(el, "ah-state", checkState(input));
      },
      getValue: function (el, $el) { return checkState(L.inputOf($el)); },
      setDisabled: function (el, $el, on) { L.setDisabled(el, L.inputOf($el), on, L.syncCheckbox); }
    }
  });
})(window.jQuery, window.AH);
