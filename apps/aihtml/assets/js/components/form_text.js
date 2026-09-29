/* Behaviours of the form_text components (designs/04-components.md).
 * Ported from sigil: form/input, form/password_input, form/number_input,
 * form/input_otp, form/tag_input. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  // Floating label: up while focused or filled (sigil util/sync-float-label!).
  function syncLabel($el, prefix, filled, focused) {
    $el.find("." + prefix + "-label").toggleClass(prefix + "-label-float", !!(filled || focused));
  }

  // Focus class on the shell plus the floating label (sigil shell).
  function focusShell($el, $input, prefix, after) {
    $input.on("focus" + NS, function () {
      $el.addClass(prefix + "-focused");
      syncLabel($el, prefix, true, true);
    }).on("blur" + NS, function () {
      $el.removeClass(prefix + "-focused");
      syncLabel($el, prefix, $input.val() !== "", false);
      if (after) { after(); }
    });
  }

  // ------------------------------------------------------------------
  // input / textarea
  // ------------------------------------------------------------------

  function inputOf($el) {
    return $el.find("input.ah-input, textarea.ah-input").first();
  }

  function syncInput($el, $input) {
    var filled = $input.val() !== "";
    $el.toggleClass("ah-input-has-value", filled);
    syncLabel($el, "ah-input", filled, $input.is(":focus"));
  }

  function clearInput($el, $input) {
    if ($input.val() === "") { return; }
    $input.val("");
    syncInput($el, $input);
    // Native events, so on(input|change, ...) on the <input> hears them.
    $input.trigger("input").trigger("change");
  }

  AH.define("input", {
    init: function (el, $el) {
      var $input = inputOf($el);
      focusShell($el, $input, "ah-input");
      $input.on("input" + NS, function () { syncInput($el, $input); });
      $el.on("click" + NS, ".ah-input-clear", function (e) {
        e.preventDefault();
        clearInput($el, $input);
        $input.trigger("focus");
      });
      if ($el.hasClass("ah-input-clearable")) {
        $input.on("keydown" + NS, function (e) {
          if (e.key === "Escape" && $input.val() !== "") {
            e.preventDefault();
            clearInput($el, $input);
          }
        });
      }
      syncInput($el, $input);
    },
    methods: {
      getValue: function (el, $el) { return inputOf($el).val(); },
      setValue: function (el, $el, v) {
        var $input = inputOf($el);
        $input.val(v == null ? "" : String(v));
        syncInput($el, $input);
      },
      clear: function (el, $el) { clearInput($el, inputOf($el)); },
      focus: function (el, $el) { inputOf($el).trigger("focus"); },
      selectAll: function (el, $el) {
        var $input = inputOf($el);
        $input.trigger("focus");
        if ($input[0]) { $input[0].select(); }
      }
    }
  });

  // ------------------------------------------------------------------
  // password_input
  // ------------------------------------------------------------------

  var SPECIALS = "<>@!#$%^&*()_+[]{}?:;|'\"\\,./~`-=";

  // sigil password-input.strength/evaluate
  function strength(pw) {
    if (pw.length < 8) { return "too-short"; }
    var letters = 0, numbers = 0, specials = 0;
    for (var i = 0; i < pw.length; i++) {
      var c = pw.charCodeAt(i), ch = pw.charAt(i);
      if ((c >= 65 && c <= 90) || (c >= 97 && c <= 122) ||
          (c >= 128 && c <= 154) || (c >= 160 && c <= 165)) { letters++; }
      else if (c >= 48 && c <= 57) { numbers++; }
      else if (SPECIALS.indexOf(ch) >= 0) { specials++; }
    }
    var score = letters + numbers + 2 * specials + letters * numbers / 2 + pw.length;
    return score < 20 ? "weak" : score < 30 ? "fair" : score < 40 ? "good" : "strong";
  }

  var STRENGTH = {
    "too-short": ["Too short", "20%", "var(--ah-color-error)"],
    weak: ["Weak", "40%", "var(--ah-color-error)"],
    fair: ["Fair", "60%", "var(--ah-color-warning)"],
    good: ["Good", "80%", "var(--ah-color-info)"],
    strong: ["Strong", "100%", "var(--ah-color-success)"]
  };

  function updateStrength($el, pw) {
    var $fill = $el.find(".ah-pwd-strength-fill");
    var $text = $el.find(".ah-pwd-strength-text");
    if (!$fill.length) { return; }
    if (!pw) {
      $fill.css({ width: "0", backgroundColor: "transparent" });
      $text.text("");
      $el.removeAttr("data-strength");
      return;
    }
    var level = strength(pw), d = STRENGTH[level];
    $fill.css({ width: d[1], backgroundColor: d[2] });
    $text.text(d[0]);
    $el.attr("data-strength", level);
  }

  function pwdInput($el) { return $el.find("input.ah-pwd").first(); }

  function setVisible($el, show) {
    var $input = pwdInput($el);
    $el.toggleClass("ah-pwd-visible", show);
    $input.attr("type", show ? "text" : "password");
    $el.find(".ah-pwd-toggle")
      .attr("aria-pressed", show ? "true" : "false")
      .attr("aria-label", show ? "Hide password" : "Show password");
  }

  AH.define("password-input", {
    init: function (el, $el) {
      var $input = pwdInput($el);
      focusShell($el, $input, "ah-pwd");
      $input.on("input" + NS, function () {
        updateStrength($el, $input.val());
        syncLabel($el, "ah-pwd", $input.val() !== "", true);
      });
      // mousedown: keep the focus (and caret) in the field
      $el.on("mousedown" + NS, ".ah-pwd-toggle", function (e) { e.preventDefault(); });
      $el.on("click" + NS, ".ah-pwd-toggle", function (e) {
        e.preventDefault();
        setVisible($el, !$el.hasClass("ah-pwd-visible"));
      });
      updateStrength($el, $input.val());
    },
    methods: {
      getValue: function (el, $el) { return pwdInput($el).val(); },
      setValue: function (el, $el, v) {
        var $input = pwdInput($el);
        $input.val(v == null ? "" : String(v));
        updateStrength($el, $input.val());
        syncLabel($el, "ah-pwd", $input.val() !== "", false);
      },
      toggle: function (el, $el, show) {
        setVisible($el, show === undefined ? !$el.hasClass("ah-pwd-visible") : !!show);
      },
      focus: function (el, $el) { pwdInput($el).trigger("focus"); }
    }
  });

  // ------------------------------------------------------------------
  // number_input
  // ------------------------------------------------------------------

  function numOpts($el) {
    var num = function (a) {
      var v = $el.attr(a);
      return v === undefined || v === "" ? null : parseFloat(v);
    };
    return {
      min: num("data-min"),
      max: num("data-max"),
      step: num("data-step") || 1,
      decimals: parseInt($el.attr("data-decimals") || "0", 10),
      allowNull: $el.attr("data-allow-null") !== "false"
    };
  }

  function clamp(v, o) {
    if (o.min !== null) { v = Math.max(o.min, v); }
    if (o.max !== null) { v = Math.min(o.max, v); }
    return v;
  }

  function parseNum(text) {
    var s = String(text == null ? "" : text).replace(/,/g, "").trim();
    if (s === "") { return null; }
    var n = parseFloat(s);
    return isNaN(n) ? null : n;
  }

  var numSeq = 0;

  function numInput($el) { return $el.find("input.ah-numinput-input").first(); }

  // Write a (clamped, formatted) value; returns the value written.
  function writeNum($el, v) {
    var o = numOpts($el), $input = numInput($el);
    if (v === null) {
      v = o.allowNull ? null : clamp(0, o);
    } else {
      v = clamp(v, o);
    }
    $input.val(v === null ? "" : v.toFixed(o.decimals));
    if (v === null) { $input.removeAttr("aria-valuenow"); } else { $input.attr("aria-valuenow", v); }
    syncLabel($el, "ah-numinput", v !== null, $input.is(":focus"));
    return v;
  }

  function step($el, dir) {
    var $input = numInput($el), input = $input[0];
    if (!input || input.disabled || input.readOnly) { return; }
    var o = numOpts($el);
    var before = $input.val();
    var cur = parseNum(before);
    var next = parseFloat(((cur === null ? 0 : cur) + o.step * dir).toFixed(o.decimals));
    writeNum($el, next);
    if ($input.val() !== before) {
      $input.trigger("input").trigger("change");
    }
  }

  function stopRepeat(el) {
    var t = $.data(el, "ah-spin");
    if (t) { clearTimeout(t.delay); clearInterval(t.every); }
    $.removeData(el, "ah-spin");
  }

  AH.define("number-input", {
    init: function (el, $el) {
      var $input = numInput($el);
      focusShell($el, $input, "ah-numinput");
      $input.on("keydown" + NS, function (e) {
        var k = e.key || "";
        if (e.ctrlKey || e.metaKey || e.altKey) { return; }
        if (k === "ArrowUp" || k === "ArrowDown") {
          e.preventDefault();
          step($el, k === "ArrowUp" ? 1 : -1);
        } else if (k === "PageUp" || k === "PageDown") {
          e.preventDefault();
          step($el, (k === "PageUp" ? 1 : -1) * 10);
        } else if (k === "-") {
          // a minus sign only at the start, once
          if (this.selectionStart !== 0 || this.value.indexOf("-") >= 0) { e.preventDefault(); }
        } else if (k === ".") {
          if (numOpts($el).decimals === 0 || this.value.indexOf(".") >= 0) { e.preventDefault(); }
        } else if (k.length === 1 && !/[0-9]/.test(k)) {
          e.preventDefault();
        }
      });
      // Typing ends in a native change (on blur or Enter): normalise first,
      // this handler runs before the delegated action handlers.
      $input.on("change" + NS, function () {
        writeNum($el, parseNum($input.val()));
      });
      $input.on("wheel" + NS, function (e) {
        if (!$input.is(":focus")) { return; }
        e.preventDefault();
        var dy = (e.originalEvent || e).deltaY;
        step($el, dy < 0 ? 1 : -1);
      });
      // Spin buttons: step, then repeat after 400ms every 75ms (sigil).
      $el.on("mousedown" + NS, ".ah-numinput-spin-up, .ah-numinput-spin-down", function (e) {
        if (e.button !== 0) { return; }
        e.preventDefault();
        var dir = $(this).hasClass("ah-numinput-spin-up") ? 1 : -1;
        stopRepeat(el);
        step($el, dir);
        var t = {};
        t.delay = setTimeout(function () {
          t.every = setInterval(function () { step($el, dir); }, 75);
        }, 400);
        $.data(el, "ah-spin", t);
        if (!$input.is(":focus")) { $input.trigger("focus"); }
      });
      $el.on("mouseleave" + NS, ".ah-numinput-spin", function () { stopRepeat(el); });
      var docNs = NS + "num" + (++numSeq);
      $.data(el, "ah-doc-ns", docNs);
      $(document).on("mouseup" + docNs, function () { stopRepeat(el); });
    },
    destroy: function (el) {
      stopRepeat(el);
      $(document).off("mouseup" + $.data(el, "ah-doc-ns"));
    },
    methods: {
      getValue: function (el, $el) { return parseNum(numInput($el).val()); },
      setValue: function (el, $el, v) {
        var $input = numInput($el), before = $input.val();
        writeNum($el, v === null || v === undefined ? null : parseNum(v));
        if ($input.val() !== before) { $input.trigger("change"); }
      },
      stepUp: function (el, $el) { step($el, 1); },
      stepDown: function (el, $el) { step($el, -1); },
      clear: function (el, $el) {
        var $input = numInput($el), before = $input.val();
        writeNum($el, null);
        if ($input.val() !== before) { $input.trigger("change"); }
      },
      focus: function (el, $el) { numInput($el).trigger("focus"); }
    }
  });

  // ------------------------------------------------------------------
  // Value-bearing helpers
  // ------------------------------------------------------------------

  // Update data-ah-value and the hidden input; fire change on the root.
  function commitValue($el, value) {
    if ($el.attr("data-ah-value") === value) { return false; }
    $el.attr("data-ah-value", value);
    $el.children("input[type=hidden]").val(value);
    $el.trigger("change");
    return true;
  }

  // Native events of the inner fields must not reach on(...) on the root:
  // the root reports its own change.
  function isolate($el, sel) {
    $el.on("change" + NS + " input" + NS, sel, function (e) { e.stopPropagation(); });
  }

  // ------------------------------------------------------------------
  // input_otp
  // ------------------------------------------------------------------

  function otpSlots($el) { return $el.find(".ah-input-otp__slot"); }

  function otpSanitize($el, s, len) {
    var re = $el.attr("data-pattern") === "alphanumeric" ? /[^0-9a-zA-Z]/g : /[^0-9]/g;
    return String(s || "").replace(re, "").slice(0, Math.max(0, len));
  }

  function otpRead($el) {
    return otpSlots($el).map(function () { return this.value; }).get().join("");
  }

  function otpFocus($el, i) {
    var $s = otpSlots($el);
    if (!$s.length) { return; }
    var s = $s.get(Math.max(0, Math.min(i, $s.length - 1)));
    s.focus();
    s.select();
  }

  // Lay a value out over the slots (from the first).
  function otpWrite($el, v) {
    otpSlots($el).each(function (i) {
      this.value = v.charAt(i);
    });
  }

  function otpCommit($el) {
    var $s = otpSlots($el);
    $s.each(function () { this.setAttribute("data-filled", this.value ? "true" : "false"); });
    // The value is the filled prefix: a gap ends it.
    var v = "";
    $s.each(function () {
      if (!this.value) { return false; }
      v += this.value;
    });
    var complete = v.length === $s.length;
    $el.attr("data-complete", complete ? "true" : "false");
    $el.removeAttr("data-invalid");
    if (commitValue($el, v) && complete) {
      $el.trigger("ah:complete", [v]);
    }
  }

  AH.define("input-otp", {
    init: function (el, $el) {
      isolate($el, ".ah-input-otp__slot");
      $el.on("input" + NS, ".ah-input-otp__slot", function () {
        var len = otpSlots($el).length;
        var i = parseInt(this.getAttribute("data-index"), 10);
        var v = otpSanitize($el, this.value, len - i);
        if (v.length > 1) {
          // autofill or IME delivered several characters: spread them
          var cur = otpRead($el);
          otpWrite($el, (cur.slice(0, i) + v).slice(0, len));
          otpCommit($el);
          otpFocus($el, i + v.length);
          return;
        }
        this.value = v;
        otpCommit($el);
        if (v) { otpFocus($el, i + 1); }
      });
      $el.on("keydown" + NS, ".ah-input-otp__slot", function (e) {
        var i = parseInt(this.getAttribute("data-index"), 10);
        var n = otpSlots($el).length;
        switch (e.key) {
          case "Backspace":
            e.preventDefault();
            if (this.value) {
              this.value = "";                  // clear this one only
              otpCommit($el);
            } else if (i > 0) {
              otpSlots($el).get(i - 1).value = ""; // back up and clear
              otpCommit($el);
              otpFocus($el, i - 1);
            }
            break;
          case "Delete":
            e.preventDefault();
            this.value = "";
            otpCommit($el);
            break;
          case "ArrowLeft": e.preventDefault(); otpFocus($el, i - 1); break;
          case "ArrowRight": e.preventDefault(); otpFocus($el, i + 1); break;
          case "Home": e.preventDefault(); otpFocus($el, 0); break;
          case "End": e.preventDefault(); otpFocus($el, n - 1); break;
          default:
            // typing over a filled slot replaces it
            if (e.key && e.key.length === 1 && !e.ctrlKey && !e.metaKey &&
                this.value && this.selectionStart === this.selectionEnd) {
              this.select();
            }
        }
      });
      $el.on("paste" + NS, ".ah-input-otp__slot", function (e) {
        e.preventDefault();
        var cd = (e.originalEvent || e).clipboardData;
        var n = otpSlots($el).length;
        var v = otpSanitize($el, cd ? cd.getData("text") : "", n);
        if (!v) { return; }
        otpWrite($el, v);
        otpCommit($el);
        otpFocus($el, Math.min(v.length, n - 1));
      });
      $el.on("focus" + NS, ".ah-input-otp__slot", function () { this.select(); });
      // A click on an empty slot past the first gap goes to the gap.
      $el.on("mousedown" + NS, ".ah-input-otp__slot", function (e) {
        var gap = otpRead($el).length;
        var i = parseInt(this.getAttribute("data-index"), 10);
        if (!this.value && i > gap) {
          e.preventDefault();
          otpFocus($el, gap);
        }
      });
    },
    methods: {
      getValue: function (el, $el) { return $el.attr("data-ah-value") || ""; },
      setValue: function (el, $el, v) {
        otpWrite($el, otpSanitize($el, v, otpSlots($el).length));
        otpCommit($el);
      },
      clear: function (el, $el) { otpWrite($el, ""); otpCommit($el); },
      focus: function (el, $el) { otpFocus($el, otpRead($el).length); },
      // Mark the code wrong (e.g. after the server rejected it).
      invalid: function (el, $el, on) {
        if (on === false) { $el.removeAttr("data-invalid"); } else { $el.attr("data-invalid", "true"); }
      }
    }
  });

  // ------------------------------------------------------------------
  // tag_input
  // ------------------------------------------------------------------

  function tagField($el) { return $el.find(".ah-tag-input__field"); }

  function readTags($el) {
    return $el.find(".ah-tag-input__chip .ah-chip__label").map(function () {
      return $(this).text();
    }).get();
  }

  // Same markup as the server's first render: templates/tag_input_chip.mustache
  function chip($el, tag, i) {
    return $(AH.tpl.tag_input_chip({
      variant: $el.attr("data-chip-variant") || "soft",
      color: $el.attr("data-chip-color") || "primary",
      index: i,
      label: tag,
      disabled: $el.attr("data-disabled") === "true"
    }));
  }

  function renderTags($el, tags) {
    $el.find(".ah-tag-input__chip").remove();
    var $field = tagField($el);
    $.each(tags, function (i, t) { chip($el, t, i).insertBefore($field); });
    return commitValue($el, tags.join(","));
  }

  // sigil tag-input/add-tag: trim, skip blanks, max-tags and duplicates.
  function addTags($el, list) {
    var tags = readTags($el);
    var max = parseInt($el.attr("data-max-tags"), 10);
    var dup = $el.is("[data-allow-duplicates]");
    var changed = false;
    $.each(list, function (_, raw) {
      var t = String(raw).trim().replace(/,/g, "");
      if (!t || (!isNaN(max) && tags.length >= max) || (!dup && tags.indexOf(t) >= 0)) { return; }
      tags.push(t);
      changed = true;
    });
    return changed ? renderTags($el, tags) : false;
  }

  function addFromField($el) {
    var $f = tagField($el), raw = $f.val();
    $f.val("");
    return addTags($el, [raw]);
  }

  function tagsDisabled($el) { return $el.attr("data-disabled") === "true"; }

  AH.define("tag-input", {
    init: function (el, $el) {
      isolate($el, ".ah-tag-input__field");
      $el.on("click" + NS, ".ah-chip__delete", function (e) {
        e.stopPropagation();
        if (tagsDisabled($el)) { return; }
        var i = parseInt($(this).closest(".ah-tag-input__chip").attr("data-index"), 10);
        var tags = readTags($el);
        if (i >= 0 && i < tags.length) {
          tags.splice(i, 1);
          renderTags($el, tags);
          tagField($el).trigger("focus");
        }
      });
      $el.on("keydown" + NS, ".ah-tag-input__field", function (e) {
        if (e.key === "Enter" || e.key === ",") {
          e.preventDefault();
          addFromField($el);
        } else if (e.key === "Backspace" && this.value.trim() === "") {
          var tags = readTags($el);
          if (tags.length) {
            e.preventDefault();
            tags.pop();
            renderTags($el, tags);
          }
        }
      });
      // A pasted list becomes several tags (sigil takes it as typed text).
      $el.on("paste" + NS, ".ah-tag-input__field", function (e) {
        var cd = (e.originalEvent || e).clipboardData;
        var text = cd ? cd.getData("text") : "";
        if (!/[,\n\r\t]/.test(text)) { return; }
        e.preventDefault();
        var parts = (this.value + text).split(/[,\n\r\t]+/);
        this.value = "";
        addTags($el, parts);
      });
      $el.on("focusout" + NS, ".ah-tag-input__field", function () { addFromField($el); });
      // A click on the empty area focuses the field.
      $el.on("click" + NS, function (e) {
        if (e.target === el) { tagField($el).trigger("focus"); }
      });
    },
    methods: {
      getTags: function (el, $el) { return readTags($el); },
      setTags: function (el, $el, tags) { renderTags($el, $.makeArray(tags).map(String)); },
      add: function (el, $el, tag) { addTags($el, [tag]); },
      remove: function (el, $el, tag) {
        var tags = readTags($el), i = tags.indexOf(String(tag));
        if (i >= 0) { tags.splice(i, 1); renderTags($el, tags); }
      },
      clear: function (el, $el) { renderTags($el, []); },
      focus: function (el, $el) { tagField($el).trigger("focus"); }
    }
  });
})(window.jQuery, window.AH);
