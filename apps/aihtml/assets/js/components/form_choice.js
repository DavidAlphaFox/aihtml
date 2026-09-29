/* Behaviours of the form_choice components (designs/04-components.md).
 *
 * The single controls (checkbox, radiobutton, switch-button) keep a native
 * <input> inside a <label>: the browser does the toggling, the keyboard
 * (Space) and the change event; these behaviours only mirror the input's
 * state onto sigil's classes and add sigil's extras (three states, locked).
 *
 * The groups (checkbox-group, radiobutton-group, radio-cards) keep the
 * root's data-ah-value in sync, stop the inner inputs' change at the root
 * and fire one "change" on the root, so data-ah-on on the root sees the
 * group value. Arrow keys move a radio selection as in sigil.
 *
 * rating is a value-bearing custom control: data-ah-value, the hidden
 * input and a "change" on the root; "ah:hover" [value|null] while the
 * pointer previews a value.
 *
 * Methods never fire change (so a server call does not echo back).
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var INPUT = "input.ah-choice-input";

  function truthy(v) {
    return v === true || v === "true" || v === "on" || v === 1 || v === "1";
  }

  // ------------------------------------------------------------------
  // Mirroring an input onto sigil's classes
  // ------------------------------------------------------------------

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

  function syncCard(input) {
    $(input).closest(".ah-radio-cards__card")
      .attr("data-selected", input.checked ? "true" : "false")
      .attr("data-disabled", input.disabled ? "true" : "false");
  }

  // The radios a browser treats as one group with this one.
  function sameGroup(input) {
    if (!input.name) {
      return $(input);
    }
    return $(input.form || document).find("input[type=radio]").filter(function () {
      return this.name === input.name && this.form === input.form;
    });
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

  // ------------------------------------------------------------------
  // checkbox
  // ------------------------------------------------------------------

  function checkState(input) {
    return input.indeterminate ? "mixed" : input.checked;
  }

  function setCheck(input, v) {
    var mixed = v === "mixed" || v === "indeterminate" || v === null;
    input.indeterminate = mixed;
    input.checked = !mixed && truthy(v);
    syncCheckbox(input);
  }

  AH.define("checkbox", {
    init: function (el, $el) {
      var input = inputOf($el);
      if (!input) { return; }
      if ($el.hasClass("ah-checkbox-indeterminate")) {
        input.indeterminate = true;
      }
      $.data(el, "ah-state", checkState(input));
      bindLocked(el, $el);
      $el.on("change" + NS, INPUT, function (e) {
        // sigil's three states: checked -> mixed -> unchecked -> checked
        if (!e.isTrigger && el.hasAttribute("data-ah-three-states")) {
          var prev = $.data(el, "ah-state");
          setCheck(input, prev === true ? "mixed" : prev === "mixed" ? false : true);
        }
        $.data(el, "ah-state", checkState(input));
        syncCheckbox(input);
      });
    },
    methods: {
      // true | false | "mixed"
      setChecked: function (el, $el, v) {
        var input = inputOf($el);
        setCheck(input, v);
        $.data(el, "ah-state", checkState(input));
      },
      getValue: function (el, $el) { return checkState(inputOf($el)); },
      setDisabled: function (el, $el, on) { setDisabled(el, inputOf($el), on, syncCheckbox); }
    }
  });

  // ------------------------------------------------------------------
  // radiobutton
  // ------------------------------------------------------------------

  function syncRadioGroup(input) {
    sameGroup(input).each(function () { syncRadio(this); });
  }

  AH.define("radiobutton", {
    init: function (el, $el) {
      bindLocked(el, $el);
      $el.on("change" + NS, INPUT, function () { syncRadioGroup(this); });
    },
    methods: {
      setChecked: function (el, $el, v) {
        var input = inputOf($el);
        input.checked = truthy(v);
        syncRadioGroup(input);
      },
      getValue: function (el, $el) { return inputOf($el).checked; },
      setDisabled: function (el, $el, on) { setDisabled(el, inputOf($el), on, syncRadio); }
    }
  });

  // ------------------------------------------------------------------
  // switch-button
  // ------------------------------------------------------------------

  AH.define("switch-button", {
    init: function (el, $el) {
      bindLocked(el, $el);
      $el.on("change" + NS, INPUT, function () { syncSwitch(this); });
    },
    methods: {
      setChecked: function (el, $el, v) {
        var input = inputOf($el);
        input.checked = truthy(v);
        syncSwitch(input);
      },
      getValue: function (el, $el) { return inputOf($el).checked; },
      setDisabled: function (el, $el, on) { setDisabled(el, inputOf($el), on, syncSwitch); }
    }
  });

  // ------------------------------------------------------------------
  // Groups
  // ------------------------------------------------------------------

  // kind: {radio, sync(input), item selector, disabled class}
  function groupValue($el) {
    return $el.find(INPUT).filter(":checked").map(function () {
      return this.value;
    }).get().join(",");
  }

  function syncGroup($el, kind) {
    var $inputs = $el.find(INPUT);
    $inputs.each(function () { kind.sync(this); });
    $el.attr("data-ah-value", groupValue($el));
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
      : (v === null || v === undefined || v === "") ? [] : String(v).split(",");
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
        // a value, an array or "a,b"; does not fire change
        setValue: function (el, $el, v) { setGroupValue($el, kind, v); },
        getValue: function (el, $el) {
          var v = groupValue($el);
          return kind.radio ? v : (v ? v.split(",") : []);
        },
        setDisabled: function (el, $el, on) { setGroupDisabled(el, $el, kind, on); }
      }
    });
  }

  defineGroup("checkbox-group", {
    radio: false, sync: syncCheckbox, item: ".ah-checkbox-group-item",
    itemDisabled: "ah-checkbox-group-item-disabled", disabled: "ah-checkbox-group-disabled"
  });
  defineGroup("radiobutton-group", {
    radio: true, sync: syncRadio, item: ".ah-radiobutton-group-item",
    itemDisabled: "ah-radiobutton-group-item-disabled", disabled: "ah-radiobutton-group-disabled"
  });
  defineGroup("radio-cards", {
    radio: true, sync: syncCard, item: ".ah-radio-cards__card", disabledAttr: true
  });

  // ------------------------------------------------------------------
  // rating
  // ------------------------------------------------------------------

  function ratingState(el) {
    return {
      value: parseFloat(el.getAttribute("data-ah-value")) || 0,
      max: parseInt(el.getAttribute("data-ah-max"), 10) || 5,
      step: el.getAttribute("data-precision") === "0.5" ? 0.5 : 1,
      live: el.getAttribute("data-readonly") !== "true" &&
            el.getAttribute("data-disabled") !== "true",
      clear: el.getAttribute("data-allow-clear") !== "false"
    };
  }

  function paintRating($el, v) {
    $el.find(".ah-rating__star").each(function (i) {
      var r = Math.max(0, Math.min(1, v - i));
      $(this).find(".ah-rating__filled").css("width", (r * 100) + "%");
    });
  }

  function setRating(el, $el, v, fire) {
    var s = ratingState(el);
    v = Math.max(0, Math.min(s.max, Math.round(Number(v) / s.step) * s.step || 0));
    var changed = v !== s.value;
    el.setAttribute("data-ah-value", String(v));
    $el.children("input[type=hidden]").val(String(v));
    $el.find(".ah-rating__star").each(function (i) {
      this.setAttribute("aria-checked", v >= i + 1 ? "true" : "false");
    });
    paintRating($el, v);
    if (fire && changed) {
      $el.trigger("change");
    }
  }

  // The value under the pointer; a keyboard click (detail 0) takes the
  // whole star.
  function ratingAt(el, star, e) {
    var s = ratingState(el);
    var idx = parseInt(star.getAttribute("data-index"), 10);
    if (s.step === 0.5 && e.detail !== 0 && e.clientX !== undefined) {
      var rect = star.getBoundingClientRect();
      return idx + ((e.clientX - rect.left) / rect.width <= 0.5 ? 0.5 : 1);
    }
    return idx + 1;
  }

  AH.define("rating", {
    init: function (el, $el) {
      $el.on("mousemove" + NS, ".ah-rating__star", function (e) {
        if (!ratingState(el).live) { return; }
        var v = ratingAt(el, this, e);
        paintRating($el, v);
        $el.trigger("ah:hover", [v]);
      });
      $el.on("mouseleave" + NS, function () {
        var s = ratingState(el);
        if (!s.live) { return; }
        paintRating($el, s.value);
        $el.trigger("ah:hover", [null]);
      });
      $el.on("click" + NS, ".ah-rating__star", function (e) {
        var s = ratingState(el);
        if (!s.live) { return; }
        var v = ratingAt(el, this, e);
        setRating(el, $el, s.clear && v === s.value ? 0 : v, true);
      });
      $el.on("keydown" + NS, ".ah-rating__star", function (e) {
        var s = ratingState(el);
        if (!s.live) { return; }
        var d = { ArrowRight: 1, ArrowUp: 1, ArrowLeft: -1, ArrowDown: -1 }[e.key];
        if (!d) { return; }
        e.preventDefault();
        setRating(el, $el, s.value + d * s.step, true);
      });
    },
    methods: {
      setValue: function (el, $el, v) { setRating(el, $el, v, false); },
      getValue: function (el) { return ratingState(el).value; }
    }
  });
})(window.jQuery, window.AH);
