/* The validator of aihtml_field:validate/1 (designs/04-components.md):
   client-side checks whose messages show in field/4 and form_layout/4
   rows, or as sigil's tooltip bubble. Ported from sigil
   (sigil.components.form.validator). */
// ah-load: [data-ah-validate]
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;
var seq = 0;

function trim(s) { return String(s === null || s === undefined ? "" : s).trim(); }

// ------------------------------------------------------------------
// validator (aihtml_field:validate/1)
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
// scrolling); extra/field.css points its arrow back at the control
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
