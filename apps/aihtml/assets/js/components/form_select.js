/* Behaviours of the form_select components (designs/04-components.md):
   dropdownlist, slider and the validator (validate/1). Ported from sigil
   (sigil.components.form.{dropdownlist, listbox, slider, validator}). */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  function trim(s) { return String(s === null || s === undefined ? "" : s).trim(); }

  function uid(el, prefix) {
    if (!el.id) { el.id = prefix + (++seq); }
    return el.id;
  }

  // ------------------------------------------------------------------
  // Value-bearing helpers (data-ah-value + hidden input + change/input)
  // ------------------------------------------------------------------

  function writeValue($el, value) {
    $el.attr("data-ah-value", value);
    $el.children("input[type=hidden]").val(value);
  }

  // ------------------------------------------------------------------
  // dropdownlist
  // ------------------------------------------------------------------
  //
  // Keyboard (combobox pattern, extending sigil's Enter/Space/Escape/F4/
  // Alt+arrows with listbox navigation):
  //   closed: ArrowDown/ArrowUp/Enter/Space/F4/Alt+Arrow open, a letter
  //           selects the next item starting with it
  //   open:   ArrowUp/Down/Home/End/PageUp/PageDown move, Enter/Space
  //           select, Escape/Alt+Arrow/F4 close, Tab closes, letters
  //           type-ahead (or filter, when filterable)

  function ddState(el) {
    var s = $.data(el, "ah-dd");
    if (!s) {
      s = { open: false, search: "", searchAt: 0 };
      $.data(el, "ah-dd", s);
    }
    return s;
  }

  function ddItems($el, visibleOnly) {
    var $items = $el.find(".ah-listbox-item");
    return visibleOnly
      ? $items.filter(function () { return this.style.display !== "none"; })
      : $items;
  }

  function ddEnabled($items) {
    return $items.filter(function () {
      return !$(this).hasClass("ah-listbox-item-disabled");
    });
  }

  function ddDisabled($el) {
    return $el.hasClass("ah-dropdownlist-disabled");
  }

  function ddSetActive($el, item) {
    var $items = ddItems($el);
    $items.removeClass("ah-listbox-item-focused");
    var $filter = $el.find(".ah-listbox-filter-input");
    if (!item) {
      $el.removeAttr("aria-activedescendant");
      $filter.removeAttr("aria-activedescendant");
      return;
    }
    $(item).addClass("ah-listbox-item-focused");
    $el.attr("aria-activedescendant", item.id);
    $filter.attr("aria-activedescendant", item.id);
    if (item.scrollIntoView) { item.scrollIntoView({ block: "nearest" }); }
  }

  function ddActive($el) {
    return $el.find(".ah-listbox-item-focused")[0] || null;
  }

  // sigil's -above / -below classes, from the side AH.float chose.
  function ddPlacement($el) {
    var $popup = $el.children(".ah-dropdownlist-popup");
    var above = $popup.attr("data-ah-placement") === "top";
    $popup.toggleClass("ah-dropdownlist-popup-above", above)
      .toggleClass("ah-dropdownlist-popup-below", !above);
  }

  function ddOpen(el, $el) {
    var s = ddState(el);
    if (s.open || ddDisabled($el)) { return; }
    s.open = true;
    $el.addClass("ah-dropdownlist-open ah-dropdownlist-state-selected")
      .attr("aria-expanded", "true");
    var $popup = $el.children(".ah-dropdownlist-popup").addClass("ah-dropdownlist-popup-open");
    // Shared positioning: fixed, at least the root's width, flips above
    // when there is no room below, follows scroll and resize.
    s.float = AH.float($popup[0], el, { placement: "bottom", align: "start", offset: 4,
                                        matchWidth: true });
    ddPlacement($el);
    var $sel = ddItems($el).filter(".ah-listbox-item-selected");
    ddSetActive($el, $sel[0] || ddEnabled(ddItems($el, true))[0]);
    var $filter = $el.find(".ah-listbox-filter-input");
    if ($filter.length) { $filter.trigger("focus"); }
    $el.trigger("ah:open");
  }

  function ddClose(el, $el, refocus) {
    var s = ddState(el);
    if (!s.open) { return; }
    s.open = false;
    $el.removeClass("ah-dropdownlist-open ah-dropdownlist-state-selected")
      .attr("aria-expanded", "false");
    if (s.float) { s.float.stop(); s.float = null; }
    $el.children(".ah-dropdownlist-popup")
      .removeClass("ah-dropdownlist-popup-open ah-dropdownlist-popup-above ah-dropdownlist-popup-below");
    ddSetActive($el, null);
    var $filter = $el.find(".ah-listbox-filter-input");
    if ($filter.length && $filter.val()) {
      $filter.val("");
      ddFilter($el, "");
    }
    if (refocus && el.contains(document.activeElement) && document.activeElement !== el) {
      el.focus();
    }
    $el.trigger("ah:close");
  }

  function ddLabel(item) {
    var l = $(item).find(".ah-listbox-label")[0];
    return trim((l || item).textContent);
  }

  // Select an item (null clears). Fires change when the value changes.
  function ddSelect(el, $el, item, silent) {
    var value = item ? item.getAttribute("data-value") : "";
    var old = $el.attr("data-ah-value") || "";
    var $items = ddItems($el);
    $items.removeClass("ah-listbox-item-selected").attr("aria-selected", "false");
    var $content = $el.find(".ah-dropdownlist-content");
    if (item) {
      $(item).addClass("ah-listbox-item-selected").attr("aria-selected", "true");
      $content.text(ddLabel(item)).removeClass("ah-dropdownlist-content-placeholder");
    } else {
      $content.text($el.attr("data-ah-placeholder") || "")
        .addClass("ah-dropdownlist-content-placeholder");
    }
    writeValue($el, value);
    if (!silent && value !== old) {
      $el.trigger("change", [{ value: value, label: item ? ddLabel(item) : null }]);
    }
  }

  function ddFilter($el, text) {
    var q = trim(text).toLowerCase();
    ddItems($el).each(function () {
      this.style.display = !q || ddLabel(this).toLowerCase().indexOf(q) >= 0 ? "" : "none";
    });
    $el.find(".ah-listbox-group").each(function () {
      var any = $(this).nextUntil(".ah-listbox-group").filter(function () {
        return this.style.display !== "none";
      }).length > 0;
      this.style.display = any ? "" : "none";
    });
    ddSetActive($el, ddEnabled(ddItems($el, true))[0]);
    var s = $.data($el[0], "ah-dd");
    if (s && s.float) { s.float.update(); ddPlacement($el); }
  }

  // Type-ahead: letters typed within 800 ms form one prefix (sigil's
  // incremental search). Returns the matching item after the current one.
  function ddTypeahead(el, $el, key) {
    var s = ddState(el);
    var now = Date.now();
    s.search = now - s.searchAt > 800 ? key : s.search + key;
    s.searchAt = now;
    var q = s.search.toLowerCase();
    var items = ddEnabled(ddItems($el, true)).get();
    var cur = s.open ? ddActive($el) : ddItems($el).filter(".ah-listbox-item-selected")[0];
    var start = s.search.length === 1 ? items.indexOf(cur) + 1 : Math.max(items.indexOf(cur), 0);
    for (var i = 0; i < items.length; i++) {
      var it = items[(start + i) % items.length];
      if (ddLabel(it).toLowerCase().indexOf(q) === 0) { return it; }
    }
    return null;
  }

  function ddKeydown(el, $el, e) {
    if (ddDisabled($el)) { return; }
    var s = ddState(el);
    var key = e.key;
    var inFilter = $(e.target).hasClass("ah-listbox-filter-input");
    var toggle = key === "F4" || (e.altKey && (key === "ArrowDown" || key === "ArrowUp"));
    if (!s.open) {
      if (toggle || key === "ArrowDown" || key === "ArrowUp" || key === "Enter" || key === " ") {
        e.preventDefault();
        ddOpen(el, $el);
      } else if (key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
        var hit = ddTypeahead(el, $el, key);
        if (hit) { ddSelect(el, $el, hit); }
      }
      return;
    }
    var items = ddEnabled(ddItems($el, true)).get();
    var idx = items.indexOf(ddActive($el));
    var move = function (i) {
      e.preventDefault();
      if (items.length) { ddSetActive($el, items[Math.max(0, Math.min(items.length - 1, i))]); }
    };
    if (toggle || key === "Escape") {
      e.preventDefault();
      ddClose(el, $el, true);
    } else if (key === "ArrowDown") {
      move(idx + 1);
    } else if (key === "ArrowUp") {
      move(idx < 0 ? items.length - 1 : idx - 1);
    } else if (key === "Home" && !inFilter) {
      move(0);
    } else if (key === "End" && !inFilter) {
      move(items.length - 1);
    } else if (key === "PageDown") {
      move(idx + 10);
    } else if (key === "PageUp") {
      move(idx - 10);
    } else if (key === "Enter" || (key === " " && !inFilter)) {
      e.preventDefault();
      if (idx >= 0) { ddSelect(el, $el, items[idx]); }
      ddClose(el, $el, true);
    } else if (key === "Tab") {
      ddClose(el, $el, false);
    } else if (!inFilter && key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
      var hit = ddTypeahead(el, $el, key);
      if (hit) { ddSetActive($el, hit); }
    }
  }

  AH.define("dropdownlist", {
    init: function (el, $el) {
      var id = uid(el, "ah-dd");
      var $list = $el.find(".ah-listbox-list");
      $list.attr("id", id + "-list");
      $el.attr("aria-controls", id + "-list");
      ddItems($el).each(function () {
        this.id = id + "-opt-" + this.getAttribute("data-idx");
      });
      var ns = ".ahdd" + id.replace(/[^\w-]/g, "_");
      $.data(el, "ah-dd-ns", ns);

      $el.on("click" + NS, ".ah-dropdownlist-input-area", function () {
        if (ddState(el).open) { ddClose(el, $el, true); } else { ddOpen(el, $el); }
      });
      $el.on("click" + NS, ".ah-listbox-item", function (e) {
        e.stopPropagation();
        if ($(this).hasClass("ah-listbox-item-disabled")) { return; }
        ddSelect(el, $el, this);
        ddClose(el, $el, true);
      });
      // Keep focus on the combobox while the pointer is in the popup.
      $el.on("mousedown" + NS, ".ah-dropdownlist-popup", function (e) {
        if (!$(e.target).hasClass("ah-listbox-filter-input")) { e.preventDefault(); }
      });
      $el.on("mousemove" + NS, ".ah-listbox-item", function () {
        if (!$(this).hasClass("ah-listbox-item-disabled") && ddActive($el) !== this) {
          ddSetActive($el, this);
        }
      });
      $el.on("keydown" + NS, function (e) { ddKeydown(el, $el, e); });
      $el.on("input" + NS, ".ah-listbox-filter-input", function (e) {
        e.stopPropagation();           // not the component's own input event
        ddFilter($el, this.value);
      });
      $el.on("change" + NS, ".ah-listbox-filter-input", function (e) { e.stopPropagation(); });
      $el.on("focusin" + NS, function () { $el.addClass("ah-dropdownlist-focused"); });
      $el.on("focusout" + NS, function (e) {
        if (!e.relatedTarget || !el.contains(e.relatedTarget)) {
          $el.removeClass("ah-dropdownlist-focused");
          ddClose(el, $el, false);
        }
      });
      $(document).on("mousedown" + ns, function (e) {
        if (ddState(el).open && !el.contains(e.target)) { ddClose(el, $el, false); }
      });
    },
    destroy: function (el, $el) {
      ddClose(el, $el, false);
      $(document).off($.data(el, "ah-dd-ns"));
    },
    methods: {
      open: function (el, $el) { ddOpen(el, $el); },
      close: function (el, $el) { ddClose(el, $el, false); },
      getValue: function (el, $el) { return $el.attr("data-ah-value") || ""; },
      // setValue(value[, silent]): select the item with that value
      // ("" or null clears); fires change unless silent.
      setValue: function (el, $el, value, silent) {
        var v = value === null || value === undefined ? "" : String(value);
        var item = ddItems($el).filter(function () {
          return this.getAttribute("data-value") === v;
        })[0] || null;
        ddSelect(el, $el, item, silent);
      },
      disable: function (el, $el) {
        ddClose(el, $el, false);
        $el.addClass("ah-dropdownlist-disabled").attr({ "aria-disabled": "true", tabindex: "-1" });
      },
      enable: function (el, $el) {
        $el.removeClass("ah-dropdownlist-disabled").removeAttr("aria-disabled").attr("tabindex", "0");
      }
    }
  });

  // ------------------------------------------------------------------
  // slider
  // ------------------------------------------------------------------
  //
  // Positions are written as calc() fractions of the track (as the server
  // renders them), so nothing needs re-measuring on resize. Keyboard, per
  // sigil: Left/Down decrease, Right/Up increase, Home/End; plus
  // PageUp/PageDown by ten steps. Buttons step like sigil's; the wheel
  // steps while the slider has focus.

  var THUMB = 18;

  function slConf(el, $el) {
    var c = $.data(el, "ah-slider");
    if (!c) {
      var step = parseFloat($el.attr("data-ah-step")) || 1;
      c = {
        min: parseFloat($el.attr("data-ah-min")) || 0,
        max: parseFloat($el.attr("data-ah-max")),
        step: step,
        decimals: (String(step).split(".")[1] || "").length,
        minRange: parseFloat($el.attr("data-ah-min-range")) || 0,
        vertical: $el.hasClass("ah-slider-vertical"),
        range: $el.hasClass("ah-slider-range-slider")
      };
      if (isNaN(c.max)) { c.max = 100; }
      $.data(el, "ah-slider", c);
    }
    return c;
  }

  function slValues($el) {
    return String($el.attr("data-ah-value") || "").split(",").map(parseFloat);
  }

  function slSnap(c, v) {
    var n = Math.round((v - c.min) / c.step);
    var snapped = c.min + n * c.step;
    snapped = Math.max(c.min, Math.min(c.max, snapped));
    return parseFloat(snapped.toFixed(c.decimals));
  }

  function frac(r) {
    return "calc((100% - " + THUMB + "px) * " + (+r.toFixed(4)) + ")";
  }

  function fracCenter(r) {
    return "calc((100% - " + THUMB + "px) * " + (+r.toFixed(4)) + " + " + THUMB / 2 + "px)";
  }

  function slRender(el, $el, vals) {
    var c = slConf(el, $el);
    var ratio = function (v) { return (v - c.min) / (c.max - c.min); };
    var $range = $el.find(".ah-slider-range");
    var $end = $el.find(".ah-slider-thumb-end");
    var $start = $el.find(".ah-slider-thumb-start");
    var pos = function ($t, v) {
      if (c.vertical) { $t.css("top", frac(1 - ratio(v))); } else { $t.css("left", frac(ratio(v))); }
    };
    if (c.range) {
      pos($start, vals[0]);
      pos($end, vals[1]);
      if (c.vertical) {
        $range.css({ bottom: fracCenter(ratio(vals[0])), height: frac(ratio(vals[1]) - ratio(vals[0])) });
      } else {
        $range.css({ left: fracCenter(ratio(vals[0])), width: frac(ratio(vals[1]) - ratio(vals[0])) });
      }
      $start.attr({ "aria-valuenow": vals[0], "aria-valuetext": vals[0] });
      $end.attr({ "aria-valuenow": vals[1], "aria-valuetext": vals[1] });
    } else {
      pos($end, vals[0]);
      if (c.vertical) { $range.css({ bottom: 0, height: fracCenter(ratio(vals[0])) }); }
      else { $range.css({ left: 0, width: fracCenter(ratio(vals[0])) }); }
      $el.attr({ "aria-valuenow": vals[0], "aria-valuetext": vals[0] });
    }
    writeValue($el, vals.join(","));
    slTooltip(el, $el);
  }

  function slTooltip(el, $el) {
    var $tip = $el.children(".ah-slider-tooltip");
    var s = $.data(el, "ah-slider-state") || {};
    if (!$tip.length) { return; }
    var thumb = s.thumb === "start" ? $el.find(".ah-slider-thumb-start")[0]
      : $el.find(".ah-slider-thumb-end")[0];
    var vals = slValues($el);
    $tip.text(s.thumb === "start" ? vals[0] : vals[vals.length - 1]);
    var root = el.getBoundingClientRect();
    var r = thumb.getBoundingClientRect();
    if (slConf(el, $el).vertical) {
      $tip.css("top", (r.top + r.height / 2 - root.top) + "px");
    } else {
      $tip.css("left", (r.left + r.width / 2 - root.left) + "px");
    }
  }

  // Set one thumb ("start" | "end") to v, keeping the range ordered.
  function slSet(el, $el, which, v) {
    var c = slConf(el, $el);
    var vals = slValues($el);
    v = slSnap(c, v);
    if (c.range) {
      if (which === "start") { vals[0] = Math.min(v, vals[1] - c.minRange); }
      else { vals[1] = Math.max(v, vals[0] + c.minRange); }
      vals[0] = Math.max(c.min, vals[0]);
      vals[1] = Math.min(c.max, vals[1]);
    } else {
      vals = [v];
    }
    var old = $el.attr("data-ah-value");
    slRender(el, $el, vals);
    return $el.attr("data-ah-value") !== old;
  }

  function slFromPointer(el, $el, e) {
    var c = slConf(el, $el);
    var r = $el.find(".ah-slider-track")[0].getBoundingClientRect();
    var ratio = c.vertical
      ? 1 - (e.clientY - r.top - THUMB / 2) / (r.height - THUMB)
      : (e.clientX - r.left - THUMB / 2) / (r.width - THUMB);
    ratio = Math.max(0, Math.min(1, ratio));
    return c.min + ratio * (c.max - c.min);
  }

  function slDisabled($el) {
    return $el.hasClass("ah-slider-disabled") || $el.attr("aria-disabled") === "true";
  }

  function slShowTip($el, on) {
    $el.children(".ah-slider-tooltip").toggleClass("ah-slider-tooltip-visible", on);
  }

  function slStep(el, $el, which, delta) {
    var c = slConf(el, $el);
    var vals = slValues($el);
    var cur = c.range ? (which === "start" ? vals[0] : vals[1]) : vals[0];
    if (slSet(el, $el, which, cur + delta)) {
      $el.trigger("input").trigger("change");
    }
  }

  AH.define("slider", {
    init: function (el, $el) {
      var c = slConf(el, $el);
      var state = {};
      $.data(el, "ah-slider-state", state);

      $el.on("pointerdown" + NS, ".ah-slider-content", function (e) {
        if (slDisabled($el) || e.button !== 0) { return; }
        e.preventDefault();
        var $thumb = $(e.target).closest(".ah-slider-thumb");
        var v = slFromPointer(el, $el, e);
        var which = "end";
        if (c.range) {
          if ($thumb.length) {
            which = $thumb.hasClass("ah-slider-thumb-start") ? "start" : "end";
          } else {
            var vals = slValues($el);
            which = Math.abs(v - vals[0]) <= Math.abs(v - vals[1]) ? "start" : "end";
          }
        }
        state.dragging = true;
        state.thumb = which;
        state.startValue = $el.attr("data-ah-value");
        var $t = $el.find(".ah-slider-thumb-" + which).addClass("ah-slider-thumb-dragging");
        (c.range ? $t[0] : el).focus({ preventScroll: true });
        slShowTip($el, true);
        try { el.setPointerCapture(e.pointerId); } catch (err) { /* synthetic event */ }
        // Pressing the track jumps the nearest thumb there.
        if (!$thumb.length && slSet(el, $el, which, v)) { $el.trigger("input"); }
        slTooltip(el, $el);
      });
      $el.on("pointermove" + NS, function (e) {
        if (!state.dragging) { return; }
        if (slSet(el, $el, state.thumb, slFromPointer(el, $el, e))) { $el.trigger("input"); }
      });
      $el.on("pointerup" + NS + " pointercancel" + NS, function (e) {
        if (!state.dragging) { return; }
        state.dragging = false;
        $el.find(".ah-slider-thumb").removeClass("ah-slider-thumb-dragging");
        try { el.releasePointerCapture(e.pointerId); } catch (err) { /* not captured */ }
        if (!el.contains(document.activeElement)) { slShowTip($el, false); }
        if ($el.attr("data-ah-value") !== state.startValue) { $el.trigger("change"); }
      });

      $el.on("click" + NS, ".ah-slider-button", function () {
        if (slDisabled($el)) { return; }
        var inc = $(this).hasClass("ah-slider-button-next");
        // sigil: in range mode "+" moves the end thumb, "-" the start one
        slStep(el, $el, c.range ? (inc ? "end" : "start") : "end", inc ? c.step : -c.step);
      });

      $el.on("keydown" + NS, function (e) {
        if (slDisabled($el)) { return; }
        var which = "end";
        if (c.range) {
          var $t = $(e.target).closest(".ah-slider-thumb");
          if (!$t.length) { return; }
          which = $t.hasClass("ah-slider-thumb-start") ? "start" : "end";
        }
        state.thumb = which;
        var big = c.step * Math.max(1, Math.round((c.max - c.min) / c.step / 10));
        var delta = { ArrowRight: c.step, ArrowUp: c.step, ArrowLeft: -c.step, ArrowDown: -c.step,
                      PageUp: big, PageDown: -big }[e.key];
        if (delta !== undefined) {
          e.preventDefault();
          slShowTip($el, true);
          slStep(el, $el, which, delta);
        } else if (e.key === "Home" || e.key === "End") {
          e.preventDefault();
          slShowTip($el, true);
          if (slSet(el, $el, which, e.key === "Home" ? c.min : c.max)) {
            $el.trigger("input").trigger("change");
          }
        }
      });

      el.addEventListener("wheel", state.wheel = function (e) {
        if (slDisabled($el) || !el.contains(document.activeElement)) { return; }
        e.preventDefault();
        var which = c.range && $(document.activeElement).hasClass("ah-slider-thumb-start")
          ? "start" : "end";
        slStep(el, $el, which, e.deltaY < 0 ? c.step : -c.step);
      }, { passive: false });

      $el.on("focusin" + NS, function (e) {
        $el.addClass("ah-slider-focused");
        if (c.range) {
          state.thumb = $(e.target).hasClass("ah-slider-thumb-start") ? "start" : "end";
        }
        slTooltip(el, $el);
      });
      $el.on("focusout" + NS, function (e) {
        if (!e.relatedTarget || !el.contains(e.relatedTarget)) {
          $el.removeClass("ah-slider-focused");
          if (!state.dragging) { slShowTip($el, false); }
        }
      });
    },
    destroy: function (el) {
      var s = $.data(el, "ah-slider-state");
      if (s && s.wheel) { el.removeEventListener("wheel", s.wheel); }
      $.removeData(el, "ah-slider");
      $.removeData(el, "ah-slider-state");
    },
    methods: {
      getValue: function (el, $el) { return $el.attr("data-ah-value"); },
      // setValue(v | [lo, hi] | "lo,hi"[, silent])
      setValue: function (el, $el, v, silent) {
        var c = slConf(el, $el);
        var vals = Array.isArray(v) ? v : String(v).split(",");
        vals = vals.map(function (x) { return slSnap(c, parseFloat(x)); });
        if (c.range) { vals = [Math.min(vals[0], vals[1]), Math.max(vals[0], vals[1])]; }
        var old = $el.attr("data-ah-value");
        slRender(el, $el, vals);
        if (!silent && $el.attr("data-ah-value") !== old) { $el.trigger("change"); }
      }
    }
  });

  // ------------------------------------------------------------------
  // validator (aihtml_form_select:validate/1)
  // ------------------------------------------------------------------
  //
  // Controls carry data-ah-validate='[{"rule":"required"}, ...]'. They are
  // checked on their trigger events (default blur) and, once invalid, on
  // every input/change until fixed. A form holding such controls is
  // checked on submit, and on a click of its submit button: when a check
  // fails the event is stopped in the capture phase, so neither the
  // browser, data-ah-fetch nor an aihtml action (on/2) sees it.
  //
  // Errors show as in sigil: the error class on the control plus either a
  // tooltip bubble (.ah-validator-hint) or an error label. Inside a
  // field/4 row ("auto", the default) the label goes under the control.
  //
  //   ah:validation-error {invalid: [el]} / ah:validation-success  on the form
  //   AH.fn("validate", target) -> bool     check a form or control
  //   AH.fn("clearValidation", target)      remove every message

  var MESSAGES = {
    required: "This field is required",
    email: "Please enter a valid email address",
    number: "Please enter a number",
    integer: "Please enter a whole number",
    phone: "Please enter a phone number like (555)555-5555",
    zip_code: "Please enter a valid ZIP code",
    ssn: "Please enter a valid SSN",
    not_number: "Digits are not allowed",
    starts_with_letter: "Must start with a letter",
    min_length: "Please enter at least {0} characters",
    max_length: "Please enter at most {0} characters",
    length: "Please enter {0} to {1} characters",
    min: "Must be at least {0}",
    max: "Must be at most {0}",
    range: "Must be between {0} and {1}",
    pattern: "Please match the requested format",
    same_as: "The values do not match"
  };

  function fmt(s, args) {
    return s.replace(/\{(\d)\}/g, function (_, i) { return args[+i]; });
  }

  function isNative(el) {
    return /^(INPUT|SELECT|TEXTAREA)$/.test(el.tagName);
  }

  function valueOf(el) {
    if (isNative(el)) {
      var v = $(el).val();
      return Array.isArray(v) ? v.join(",") : String(v === null || v === undefined ? "" : v);
    }
    return el.getAttribute("data-ah-value") || "";
  }

  function blank(s) { return trim(s) === ""; }

  var RULES = {
    required: function (v, el) {
      if (el.type === "checkbox") { return el.checked; }
      if (el.type === "radio") {
        return !!(el.form || document).querySelector(
          "input[type=radio][name=\"" + CSS.escape(el.name) + "\"]:checked");
      }
      return !blank(v);
    },
    email: function (v) { return blank(v) || /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(trim(v)); },
    number: function (v) { return blank(v) || isFinite(Number(trim(v))); },
    integer: function (v) { return blank(v) || /^[-+]?\d+$/.test(trim(v)); },
    phone: function (v) { return blank(v) || /^\(\d{3}\)\d{3}-\d{4}$/.test(trim(v)); },
    zip_code: function (v) { return blank(v) || /^(\d{5})(-\d{4})?$/.test(trim(v)); },
    ssn: function (v) { return blank(v) || /^\d{3}-\d{2}-\d{4}$/.test(trim(v)); },
    not_number: function (v) { return blank(v) || !/\d/.test(v); },
    starts_with_letter: function (v) { return blank(v) || /^[a-zA-Z]/.test(trim(v)); },
    min_length: function (v, _el, a) { return blank(v) || v.length >= a[0]; },
    max_length: function (v, _el, a) { return v.length <= a[0]; },
    length: function (v, _el, a) { return blank(v) || (v.length >= a[0] && v.length <= a[1]); },
    min: function (v, _el, a) { return blank(v) || Number(v) >= a[0]; },
    max: function (v, _el, a) { return blank(v) || Number(v) <= a[0]; },
    range: function (v, _el, a) {
      return blank(v) || (Number(v) >= a[0] && Number(v) <= a[1]);
    },
    pattern: function (v, _el, a) { return blank(v) || new RegExp("^(?:" + a[0] + ")$").test(v); },
    same_as: function (v, _el, a) {
      var other = $(a[0])[0];
      return !other || valueOf(other) === v;
    }
  };

  function rulesOf(el) {
    var r = $.data(el, "ah-rules");
    if (!r) {
      try { r = JSON.parse(el.getAttribute("data-ah-validate") || "[]"); }
      catch (err) { r = []; }
      $.data(el, "ah-rules", r);
    }
    return r;
  }

  // The first failing rule's message, or null.
  function failure(el) {
    var v = valueOf(el);
    var rules = rulesOf(el);
    for (var i = 0; i < rules.length; i++) {
      var r = rules[i];
      var f = RULES[r.rule];
      if (f && !f(v, el, r.args || [])) {
        return r.msg || fmt(MESSAGES[r.rule] || "Invalid value", r.args || []);
      }
    }
    return null;
  }

  function hintMode(el) {
    var mode = el.getAttribute("data-ah-validate-hint") || "auto";
    if (mode === "auto") {
      return $(el).closest(".ah-form-body").length ? "field" : "tooltip";
    }
    return mode;
  }

  function hideHint(el) {
    var $el = $(el);
    $el.removeClass("ah-validator-error-element").removeAttr("aria-invalid");
    var hint = $.data(el, "ah-hint");
    if (hint) {
      removeHint(hint);
      $.removeData(el, "ah-hint");
    }
    var $body = $el.closest(".ah-form-body");
    if ($body.length && !$body.find(".ah-validator-error-element").length) {
      $body.children(".ah-form-error").remove();
      $body.closest(".ah-form-row-invalid").removeClass("ah-form-row-invalid");
    }
    var desc = $el.attr("aria-describedby");
    if (desc && /ah-vh\d+/.test(desc)) {
      desc = trim(desc.replace(/\bah-vh\d+\b/g, ""));
      if (desc) { $el.attr("aria-describedby", desc); } else { $el.removeAttr("aria-describedby"); }
    }
  }

  // The bubble is anchored with AH.float (flips when out of room, follows
  // scrolling); extra/form_select.css points its arrow back at the control
  // from the side in data-ah-placement.
  function floatTooltip(el, hint) {
    var pos = el.getAttribute("data-ah-validate-position") || "right";
    $.data(hint, "ah-float", AH.float(hint, el, { placement: pos, align: "center", offset: 8 }));
  }

  function removeHint(hint) {
    var f = $.data(hint, "ah-float");
    if (f) { f.stop(); }
    $(hint).remove();
  }

  function showHint(el, message) {
    hideHint(el);
    var $el = $(el);
    var id = "ah-vh" + (++seq);
    $el.addClass("ah-validator-error-element").attr("aria-invalid", "true");
    $el.attr("aria-describedby", trim(($el.attr("aria-describedby") || "") + " " + id));
    var mode = hintMode(el);
    var $hint;
    if (mode === "field") {
      var $body = $el.closest(".ah-form-body");
      $body.children(".ah-form-error").remove();
      $hint = $("<div class=\"ah-form-error ah-validator-error-label\" role=\"alert\"></div>")
        .attr("id", id).text(message).appendTo($body);
      $body.closest(".ah-form-row, .ah-form-col").addClass("ah-form-row-invalid");
      $.data(el, "ah-hint", $hint[0]);
      return;
    }
    if (mode === "label") {
      $hint = $("<label class=\"ah-validator-error-label\" role=\"alert\"></label>")
        .attr({ id: id, "for": el.id || null }).text(message);
      if (el.getAttribute("data-ah-validate-position") === "top") { $hint.insertBefore(el); }
      else { $hint.insertAfter(el); }
      $.data(el, "ah-hint", $hint[0]);
      return;
    }
    $hint = $("<div class=\"ah-validator-hint\" role=\"alert\">" +
              "<div class=\"ah-validator-arrow\"></div></div>")
      .attr("id", id).append(document.createTextNode(message)).appendTo(document.body);
    $hint.data("ah-owner", el);
    floatTooltip(el, $hint[0]);
    $hint.addClass("ah-validator-hint-visible");
    $hint.on("click", function () { hideHint(el); });     // sigil: click closes
    $.data(el, "ah-hint", $hint[0]);
  }

  // Bubbles whose control has left the page (replaced by an action).
  function sweep() {
    $(".ah-validator-hint").each(function () {
      var owner = $(this).data("ah-owner");
      if (!owner || !document.body.contains(owner)) { removeHint(this); }
    });
  }

  function skip(el) {
    return el.disabled || el.type === "hidden" || !$(el).is(":visible");
  }

  function checkOne(el) {
    sweep();
    if (skip(el)) { hideHint(el); return true; }
    var msg = failure(el);
    if (msg) { showHint(el, msg); } else { hideHint(el); }
    return !msg;
  }

  function checkAll(scope) {
    var invalid = [];
    $(scope).find("[data-ah-validate]").addBack("[data-ah-validate]").each(function () {
      if (!checkOne(this)) { invalid.push(this); }
    });
    var $scope = $(scope);
    if (invalid.length) {
      var first = invalid[0];
      if (first.scrollIntoView) { first.scrollIntoView({ block: "nearest", behavior: "smooth" }); }
      first.focus({ preventScroll: true });
      $scope.trigger("ah:validation-error", [{ invalid: invalid }]);
    } else {
      $scope.trigger("ah:validation-success");
    }
    return invalid.length === 0;
  }

  function guarded(form) {
    return form && !form.hasAttribute("data-ah-novalidate") &&
      form.querySelector("[data-ah-validate]");
  }

  function block(e) {
    e.preventDefault();
    e.stopImmediatePropagation();
  }

  // Capture phase on document: runs before the delegated handlers of
  // core.js (actions, fetch), which listen in the bubble phase.
  document.addEventListener("submit", function (e) {
    var form = e.target;
    if (!guarded(form) || (e.submitter && e.submitter.formNoValidate)) { return; }
    if (!checkAll(form)) { block(e); }
  }, true);

  document.addEventListener("click", function (e) {
    var btn = e.target.closest && e.target.closest("button, input[type=submit], input[type=image]");
    if (!btn || btn.type !== "submit" && btn.type !== "image") { return; }
    if (btn.formNoValidate || !guarded(btn.form)) { return; }
    if (!checkAll(btn.form)) { block(e); }
  }, true);

  function triggers(el) {
    return (el.getAttribute("data-ah-validate-on") || "blur").split(/\s+/);
  }

  $(document).on("focusout" + NS, "[data-ah-validate]", function (e) {
    if (e.relatedTarget && this.contains(e.relatedTarget)) { return; }
    if (triggers(this).indexOf("blur") >= 0) { checkOne(this); }
  });
  $(document).on("input" + NS + " change" + NS, "[data-ah-validate]", function (e) {
    if (e.target !== this && !isNative(e.target) && e.type === "input") { return; }
    if (triggers(this).indexOf(e.type) >= 0 || $(this).hasClass("ah-validator-error-element")) {
      checkOne(this);
    }
  });

  AH.fn("validate", function (target) { return checkAll($(target)[0] || document.body); });
  AH.fn("clearValidation", function (target) {
    $(target || document.body).find("[data-ah-validate]").addBack("[data-ah-validate]")
      .each(function () { hideHint(this); });
    sweep();
  });
})(window.jQuery, window.AH);
