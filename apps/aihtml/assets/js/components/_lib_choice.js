/* Shared by the choice behaviours (checkbox.js, radiobutton.js,
 * switch_button.js, checkbox_group.js, radiobutton_group.js,
 * radio_cards.js): mirroring a native input onto sigil's classes, locked
 * controls and the group behaviours.
 *
 * The single controls keep a native <input> inside a <label>: the browser
 * does the toggling, the keyboard (Space) and the change event; the
 * behaviours only mirror the input's state onto sigil's classes and add
 * sigil's extras. The groups keep the root's data-ah-value in sync, stop
 * the inner inputs' change at the root and fire one "change" on the root,
 * so data-ah-on on the root sees the group value. Arrow keys move a radio
 * selection as in sigil. Methods never fire change.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var INPUT = "input.ah-choice-input";

  function truthy(v) {
    return v === true || v === "true" || v === "on" || v === 1 || v === "1";
  }

  function syncCheckbox(input) {
    var mixed = input.indeterminate;
    var on = input.checked && !mixed;
    $(input).closest(".ah-checkbox")
      .toggleClass("ah-checkbox-checked", on)
      .toggleClass("ah-checkbox-indeterminate", mixed)
      .toggleClass("ah-checkbox-disabled", input.disabled)
      .find(".ah-checkbox-check")
      .toggleClass("ah-checkbox-check-checked", on)
      .toggleClass("ah-checkbox-check-indeterminate", mixed);
  }

  function syncRadio(input) {
    $(input).closest(".ah-radiobutton")
      .toggleClass("ah-radiobutton-checked", input.checked)
      .toggleClass("ah-radiobutton-disabled", input.disabled)
      .find(".ah-radiobutton-check")
      .toggleClass("ah-radiobutton-check-checked", input.checked);
  }

  function syncSwitch(input) {
    $(input).closest(".ah-switch")
      .toggleClass("ah-switch-on", input.checked)
      .toggleClass("ah-switch-disabled", input.disabled);
  }

  function inputOf($el) {
    return $el.find(INPUT).get(0);
  }

  // locked: focusable but the user cannot change it.
  function bindLocked(el, $el) {
    $el.on("click" + NS, INPUT, function (e) {
      if (el.hasAttribute("data-ah-locked")) {
        e.preventDefault();
      }
    });
  }

  function setDisabled(el, input, on, sync) {
    input.disabled = !!on;
    sync(input);
  }

  // kind: {radio, sync(input), item selector, disabled class}
  // A radio group's value is the checked value itself; a checkbox group's
  // is AH.lib.values text ("a,b", commas in values escaped).
  function groupValue($el, kind) {
    var vals = $el.find(INPUT).filter(":checked").map(function () {
      return this.value;
    }).get();
    return kind.radio ? (vals.length ? vals[0] : "") : AH.lib.values.join(vals);
  }

  function syncGroup($el, kind) {
    var $inputs = $el.find(INPUT);
    $inputs.each(function () { kind.sync(this); });
    $el.attr("data-ah-value", groupValue($el, kind));
    if (kind.radio) {
      // roving tab stop: the checked radio, else the first enabled one
      var $enabled = $inputs.filter(":not(:disabled)");
      var $stop = $enabled.filter(":checked");
      if (!$stop.length) { $stop = $enabled.first(); }
      $inputs.attr("tabindex", "-1");
      $stop.first().attr("tabindex", "0");
    }
  }

  function setGroupValue($el, kind, v) {
    var vals = Array.isArray(v) ? v.map(String)
      : (v === null || v === undefined || v === "") ? []
      : kind.radio ? [String(v)] : AH.lib.values.split(v);
    if (kind.radio) { vals = vals.slice(0, 1); }
    $el.find(INPUT).each(function () {
      this.checked = vals.indexOf(this.value) >= 0;
    });
    syncGroup($el, kind);
  }

  function setGroupDisabled(el, $el, kind, on) {
    $el.find(INPUT).each(function () {
      var own = $(this).closest("[data-ah-item-disabled]").length > 0;
      this.disabled = !!on || own;
      if (kind.itemDisabled) {
        $(this).closest(kind.item).toggleClass(kind.itemDisabled, this.disabled);
      }
    });
    if (kind.disabled) { $el.toggleClass(kind.disabled, !!on); }
    if (kind.disabledAttr) { $el.attr("data-disabled", on ? "true" : "false"); }
    if (on) { $el.attr("aria-disabled", "true"); } else { $el.removeAttr("aria-disabled"); }
    syncGroup($el, kind);
  }

  function fireChange(el, $el, kind) {
    syncGroup($el, kind);
    $el.trigger("change");
  }

  // Arrow keys: move to the next/previous enabled radio, select and focus it.
  function arrowKeys(el, $el, kind) {
    $el.on("keydown" + NS, INPUT, function (e) {
      var step = { ArrowRight: 1, ArrowDown: 1, ArrowLeft: -1, ArrowUp: -1 }[e.key];
      if (!step || e.altKey || e.ctrlKey || e.metaKey) { return; }
      var $enabled = $el.find(INPUT).filter(":not(:disabled)");
      var n = $enabled.length;
      if (!n) { return; }
      e.preventDefault();
      var i = $enabled.index(this);
      var next = $enabled.get(((i < 0 ? 0 : i) + step + n) % n);
      next.checked = true;
      next.focus();
      fireChange(el, $el, kind);
    });
  }

  function defineGroup(name, kind) {
    AH.define(name, {
      init: function (el, $el) {
        syncGroup($el, kind);
        $el.on("change" + NS, INPUT, function (e) {
          // one change per user action, fired by the root itself
          e.stopPropagation();
          fireChange(el, $el, kind);
        });
        if (kind.radio) { arrowKeys(el, $el, kind); }
      },
      methods: {
        // a value, an array or "a,b" (AH.lib.values); does not fire change
        setValue: function (el, $el, v) { setGroupValue($el, kind, v); },
        getValue: function (el, $el) {
          var v = groupValue($el, kind);
          return kind.radio ? v : AH.lib.values.split(v);
        },
        setDisabled: function (el, $el, on) { setGroupDisabled(el, $el, kind, on); }
      }
    });
  }

  AH.lib = AH.lib || {};
  AH.lib.choice = {
    INPUT: INPUT,
    truthy: truthy,
    syncCheckbox: syncCheckbox,
    syncRadio: syncRadio,
    syncSwitch: syncSwitch,
    inputOf: inputOf,
    bindLocked: bindLocked,
    setDisabled: setDisabled,
    defineGroup: defineGroup
  };
})(window.jQuery, window.AH);
