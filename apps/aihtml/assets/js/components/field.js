/* The validator of aihtml_field:validate/1 (designs/04-components.md):
   client-side checks whose messages show in field/4 and form_layout/4
   rows, or as sigil's tooltip bubble. Ported from sigil
   (sigil.components.form.validator). */
// ah-load: [data-ah-validate]
import AH from "../core.js";

var seq = 0;
var rulesCache = new WeakMap();   // control -> parsed rules
var hints = new WeakMap();        // control -> its message element
var owners = new WeakMap();       // tooltip bubble -> control
var floats = new WeakMap();       // tooltip bubble -> AH.float handle

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
//   ah:validation-error (detail {invalid: [el]}) / ah:validation-success
//                                         native events on the form
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
    if (el.tagName === "SELECT" && el.multiple) {
      return Array.from(el.selectedOptions).map(function (o) { return o.value; }).join(",");
    }
    var v = el.value;
    return String(v === null || v === undefined ? "" : v);
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
    var other = find(a[0]);
    return !other || valueOf(other) === v;
  }
};

// An element, or the first match of a selector ("#id").
function find(target) {
  if (!target) { return null; }
  if (typeof target === "string") {
    try { return document.querySelector(target); } catch (err) { return null; }
  }
  if (target.nodeType) { return target; }
  return target[0] || null;      // an array-like of elements
}

function rulesOf(el) {
  var r = rulesCache.get(el);
  if (!r) {
    try { r = JSON.parse(el.getAttribute("data-ah-validate") || "[]"); }
    catch (err) { r = []; }
    rulesCache.set(el, r);
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
    return el.closest(".ah-form-body") ? "field" : "tooltip";
  }
  return mode;
}

function hideHint(el) {
  el.classList.remove("ah-validator-error-element");
  el.removeAttribute("aria-invalid");
  var hint = hints.get(el);
  if (hint) {
    removeHint(hint);
    hints.delete(el);
  }
  var body = el.closest(".ah-form-body");
  if (body && !body.querySelector(".ah-validator-error-element")) {
    Array.prototype.forEach.call(body.querySelectorAll(":scope > .ah-form-error"), function (x) { x.remove(); });
    var row = body.closest(".ah-form-row-invalid");
    if (row) { row.classList.remove("ah-form-row-invalid"); }
  }
  var desc = el.getAttribute("aria-describedby");
  if (desc && /ah-vh\d+/.test(desc)) {
    desc = trim(desc.replace(/\bah-vh\d+\b/g, ""));
    if (desc) { el.setAttribute("aria-describedby", desc); } else { el.removeAttribute("aria-describedby"); }
  }
}

// The bubble is anchored with AH.float (flips when out of room, follows
// scrolling); extra/field.css points its arrow back at the control
// from the side in data-ah-placement.
function floatTooltip(el, hint) {
  var pos = el.getAttribute("data-ah-validate-position") || "right";
  floats.set(hint, AH.float(hint, el, { placement: pos, align: "center", offset: 8 }));
}

function removeHint(hint) {
  var f = floats.get(hint);
  if (f) { f.stop(); floats.delete(hint); }
  hint.remove();
}

function showHint(el, message) {
  hideHint(el);
  var id = "ah-vh" + (++seq);
  el.classList.add("ah-validator-error-element");
  el.setAttribute("aria-invalid", "true");
  el.setAttribute("aria-describedby", trim((el.getAttribute("aria-describedby") || "") + " " + id));
  var mode = hintMode(el);
  var hint;
  if (mode === "field") {
    var body = el.closest(".ah-form-body");
    Array.prototype.forEach.call(body.querySelectorAll(":scope > .ah-form-error"), function (x) { x.remove(); });
    hint = document.createElement("div");
    hint.className = "ah-form-error ah-validator-error-label";
    hint.setAttribute("role", "alert");
    hint.id = id;
    hint.textContent = message;
    body.appendChild(hint);
    var row = body.closest(".ah-form-row, .ah-form-col");
    if (row) { row.classList.add("ah-form-row-invalid"); }
    hints.set(el, hint);
    return;
  }
  if (mode === "label") {
    hint = document.createElement("label");
    hint.className = "ah-validator-error-label";
    hint.setAttribute("role", "alert");
    hint.id = id;
    if (el.id) { hint.htmlFor = el.id; }
    hint.textContent = message;
    if (el.getAttribute("data-ah-validate-position") === "top") { el.before(hint); }
    else { el.after(hint); }
    hints.set(el, hint);
    return;
  }
  hint = document.createElement("div");
  hint.className = "ah-validator-hint";
  hint.setAttribute("role", "alert");
  hint.id = id;
  hint.innerHTML = "<div class=\"ah-validator-arrow\"></div>";
  hint.appendChild(document.createTextNode(message));
  document.body.appendChild(hint);
  owners.set(hint, el);
  floatTooltip(el, hint);
  hint.classList.add("ah-validator-hint-visible");
  hint.addEventListener("click", function () { hideHint(el); });     // sigil: click closes
  hints.set(el, hint);
}

// Bubbles whose control has left the page (replaced by an action).
function sweep() {
  document.querySelectorAll(".ah-validator-hint").forEach(function (hint) {
    var owner = owners.get(hint);
    if (!owner || !document.body.contains(owner)) { removeHint(hint); }
  });
}

function skip(el) {
  return el.disabled || el.type === "hidden" || el.getClientRects().length === 0;
}

function checkOne(el) {
  sweep();
  if (skip(el)) { hideHint(el); return true; }
  var msg = failure(el);
  if (msg) { showHint(el, msg); } else { hideHint(el); }
  return !msg;
}

// scope and its descendants carrying data-ah-validate.
function controls(scope) {
  var out = scope.matches && scope.matches("[data-ah-validate]") ? [scope] : [];
  return out.concat(Array.from(scope.querySelectorAll("[data-ah-validate]")));
}

function fire(target, type, detail) {
  target.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
}

function checkAll(scope) {
  var invalid = [];
  controls(scope).forEach(function (el) {
    if (!checkOne(el)) { invalid.push(el); }
  });
  if (invalid.length) {
    var first = invalid[0];
    if (first.scrollIntoView) { first.scrollIntoView({ block: "nearest", behavior: "smooth" }); }
    first.focus({ preventScroll: true });
    fire(scope, "ah:validation-error", { invalid: invalid });
  } else {
    fire(scope, "ah:validation-success");
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

// Like a delegated jQuery handler: fn(match, e) for every element
// matching the selector from the target up to the document.
function eachMatch(e, selector, fn) {
  var n = e.target && e.target.closest ? e.target.closest(selector) : null;
  while (n) {
    fn(n, e);
    n = n.parentElement ? n.parentElement.closest(selector) : null;
  }
}

document.addEventListener("focusout", function (e) {
  eachMatch(e, "[data-ah-validate]", function (el) {
    if (e.relatedTarget && el.contains(e.relatedTarget)) { return; }
    if (triggers(el).indexOf("blur") >= 0) { checkOne(el); }
  });
});

function onEdit(e) {
  eachMatch(e, "[data-ah-validate]", function (el) {
    if (e.target !== el && !isNative(e.target) && e.type === "input") { return; }
    if (triggers(el).indexOf(e.type) >= 0 || el.classList.contains("ah-validator-error-element")) {
      checkOne(el);
    }
  });
}
document.addEventListener("input", onEdit);
document.addEventListener("change", onEdit);

// validate(target): target is a form or control (an element or a
// selector); the default is the whole page.
AH.fn("validate", function (target) { return checkAll(find(target) || document.body); });
AH.fn("clearValidation", function (target) {
  var scope = target ? find(target) : document.body;
  if (scope) { controls(scope).forEach(hideHint); }
  sweep();
});
