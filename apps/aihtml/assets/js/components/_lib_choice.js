/* Shared by the choice behaviours (checkbox.js, radiobutton.js,
 * switch_button.js, checkbox_group.js, radiobutton_group.js,
 * radio_cards.js): mirroring a native input onto sigil's classes, locked
 * controls and the group behaviours.
 *
 * The single controls keep a native <input> inside a <label>: the browser
 * does the toggling, the keyboard (Space) and the change event; the
 * behaviours only mirror the input's state onto sigil's classes and add
 * sigil's extras. The groups keep the root's data-ah-value in sync, stop
 * the inner inputs' change at the root and fire one "change" on the root
 * (a native event, no detail), so data-ah-on on the root sees the group
 * value. Arrow keys move a radio selection as in sigil. Methods never
 * fire change.
 */
import AH from "../core.js";
import "./_lib_values.js";

var INPUT = "input.ah-choice-input";

function truthy(v) {
  return v === true || v === "true" || v === "on" || v === 1 || v === "1";
}

function toggle(node, cls, on) {
  if (node) { node.classList.toggle(cls, !!on); }
}

function syncCheckbox(input) {
  var mixed = input.indeterminate;
  var on = input.checked && !mixed;
  var box = input.closest(".ah-checkbox");
  if (!box) { return; }
  toggle(box, "ah-checkbox-checked", on);
  toggle(box, "ah-checkbox-indeterminate", mixed);
  toggle(box, "ah-checkbox-disabled", input.disabled);
  box.querySelectorAll(".ah-checkbox-check").forEach(function (c) {
    toggle(c, "ah-checkbox-check-checked", on);
    toggle(c, "ah-checkbox-check-indeterminate", mixed);
  });
}

function syncRadio(input) {
  var r = input.closest(".ah-radiobutton");
  if (!r) { return; }
  toggle(r, "ah-radiobutton-checked", input.checked);
  toggle(r, "ah-radiobutton-disabled", input.disabled);
  r.querySelectorAll(".ah-radiobutton-check").forEach(function (c) {
    toggle(c, "ah-radiobutton-check-checked", input.checked);
  });
}

function syncSwitch(input) {
  var s = input.closest(".ah-switch");
  toggle(s, "ah-switch-on", input.checked);
  toggle(s, "ah-switch-disabled", input.disabled);
}

function inputOf(el) {
  return el.querySelector(INPUT);
}

// locked: focusable but the user cannot change it. ctrl: the root's
// AH.Controller.
function bindLocked(ctrl) {
  ctrl.delegate("click", INPUT, function (e) {
    if (ctrl.element.hasAttribute("data-ah-locked")) {
      e.preventDefault();
    }
  });
}

function setDisabled(input, on, sync) {
  input.disabled = !!on;
  sync(input);
}

// kind: {radio, sync(input), item selector, disabled class}
// A radio group's value is the checked value itself; a checkbox group's
// is AH.lib.values text ("a,b", commas in values escaped).
function inputs(el) { return Array.from(el.querySelectorAll(INPUT)); }

function groupValue(el, kind) {
  var vals = inputs(el).filter(function (i) { return i.checked; }).map(function (i) { return i.value; });
  return kind.radio ? (vals.length ? vals[0] : "") : AH.lib.values.join(vals);
}

function syncGroup(el, kind) {
  var all = inputs(el);
  all.forEach(function (i) { kind.sync(i); });
  el.setAttribute("data-ah-value", groupValue(el, kind));
  if (kind.radio) {
    // roving tab stop: the checked radio, else the first enabled one
    var enabled = all.filter(function (i) { return !i.disabled; });
    var stop = enabled.find(function (i) { return i.checked; }) || enabled[0];
    all.forEach(function (i) { i.setAttribute("tabindex", "-1"); });
    if (stop) { stop.setAttribute("tabindex", "0"); }
  }
}

function setGroupValue(el, kind, v) {
  var vals = Array.isArray(v) ? v.map(String)
    : (v === null || v === undefined || v === "") ? []
    : kind.radio ? [String(v)] : AH.lib.values.split(v);
  if (kind.radio) { vals = vals.slice(0, 1); }
  inputs(el).forEach(function (i) {
    i.checked = vals.indexOf(i.value) >= 0;
  });
  syncGroup(el, kind);
}

function setGroupDisabled(el, kind, on) {
  inputs(el).forEach(function (i) {
    var own = !!i.closest("[data-ah-item-disabled]");
    i.disabled = !!on || own;
    if (kind.itemDisabled) {
      toggle(i.closest(kind.item), kind.itemDisabled, i.disabled);
    }
  });
  if (kind.disabled) { el.classList.toggle(kind.disabled, !!on); }
  if (kind.disabledAttr) { el.setAttribute("data-disabled", on ? "true" : "false"); }
  if (on) { el.setAttribute("aria-disabled", "true"); } else { el.removeAttribute("aria-disabled"); }
  syncGroup(el, kind);
}

function defineGroup(name, kind) {
  AH.register(name, class extends AH.Controller {
    setup() {
      var el = this.element;
      syncGroup(el, kind);
      this.delegate("change", INPUT, (e) => {
        // one change per user action, fired by the root itself: the
        // input's own change goes no further (not even to the root's
        // later listeners, as with jQuery's delegated stop before)
        e.stopImmediatePropagation();
        this.changed();
      });
      if (kind.radio) {
        // Arrow keys: move to the next/previous enabled radio, select and
        // focus it.
        this.delegate("keydown", INPUT, (e, input) => {
          var step = { ArrowRight: 1, ArrowDown: 1, ArrowLeft: -1, ArrowUp: -1 }[e.key];
          if (!step || e.altKey || e.ctrlKey || e.metaKey) { return; }
          var enabled = inputs(el).filter(function (i) { return !i.disabled; });
          var n = enabled.length;
          if (!n) { return; }
          e.preventDefault();
          var i = enabled.indexOf(input);
          var next = enabled[((i < 0 ? 0 : i) + step + n) % n];
          next.checked = true;
          next.focus();
          this.changed();
        });
      }
    }

    changed() {
      syncGroup(this.element, kind);
      this.fire("change");
    }

    // methods (aihtml_action:call/4, AH.invoke)
    // a value, an array or "a,b" (AH.lib.values); does not fire change
    setValue(v) { setGroupValue(this.element, kind, v); }
    getValue() {
      var v = groupValue(this.element, kind);
      return kind.radio ? v : AH.lib.values.split(v);
    }
    setDisabled(on) { setGroupDisabled(this.element, kind, on); }
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
