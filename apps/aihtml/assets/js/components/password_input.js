/* Behaviour of the password_input component (designs/04-components.md).
 * Ported from sigil: form/password_input. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.input;

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
      L.focusShell($el, $input, "ah-pwd");
      $input.on("input" + NS, function () {
        updateStrength($el, $input.val());
        L.syncLabel($el, "ah-pwd", $input.val() !== "", true);
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
        L.syncLabel($el, "ah-pwd", $input.val() !== "", false);
      },
      toggle: function (el, $el, show) {
        setVisible($el, show === undefined ? !$el.hasClass("ah-pwd-visible") : !!show);
      },
      focus: function (el, $el) { pwdInput($el).trigger("focus"); }
    }
  });
})(window.jQuery, window.AH);
