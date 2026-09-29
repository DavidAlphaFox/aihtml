/* Behaviour of expander (value "true" / "false"; fires change). */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_layout.js";

var NS = AH.NS;
var L = AH.lib.layout;

function expIsOpen($el) {
  return $el.children(".ah-expander-header").attr("aria-expanded") === "true";
}

function expSet(el, $el, open, user) {
  if (expIsOpen($el) === open) {
    return;
  }
  var $h = $el.children(".ah-expander-header");
  var $b = $el.children(".ah-expander-body");
  var anim = el.getAttribute("data-animation") || "slide";
  var dur = parseInt(el.getAttribute("data-duration") || "250", 10);
  $el.trigger(open ? "ah:expanding" : "ah:collapsing");
  $h.toggleClass("ah-expander-header-expanded", open).attr("aria-expanded", String(open));
  $h.children(".ah-expander-arrow").toggleClass("ah-expander-arrow-expanded", open);
  L.setValue(el, $el, String(open));
  var done = function () {
    $el.trigger(open ? "ah:expanded" : "ah:collapsed");
  };
  $b.stop(true, true);
  if (anim === "slide") {
    $b[open ? "slideDown" : "slideUp"](dur, done);
  } else if (anim === "fade") {
    $b[open ? "fadeIn" : "fadeOut"](dur, done);
  } else {
    $b[open ? "show" : "hide"]();
    done();
  }
  if (user) {
    $el.trigger("change");
  }
  // Accordion: opening one closes the others sharing its name.
  var group = el.getAttribute("data-accordion");
  if (open && group) {
    $("[data-ah=expander]").each(function () {
      if (this !== el && this.getAttribute("data-accordion") === group) {
        expSet(this, $(this), false, user);
      }
    });
  }
}

AH.define("expander", {
  init: function (el, $el) {
    var mode = el.getAttribute("data-toggle-mode") || "click";
    if (mode === "none") {
      return;
    }
    var fromUser = function (e) {
      if (this.parentNode !== el || $el.hasClass("ah-expander-disabled")) {
        return;
      }
      e.preventDefault();
      expSet(el, $el, !expIsOpen($el), true);
    };
    $el.on(mode + NS, ".ah-expander-header", fromUser);
    $el.on("keydown" + NS, ".ah-expander-header", function (e) {
      if (e.target === this && (L.key(e) === "Enter" || L.key(e) === " ")) {
        fromUser.call(this, e);
      }
    });
  },
  methods: {
    open: function (el, $el) { expSet(el, $el, true, false); },
    close: function (el, $el) { expSet(el, $el, false, false); },
    toggle: function (el, $el) { expSet(el, $el, !expIsOpen($el), false); },
    isOpen: function (el, $el) { return expIsOpen($el); }
  }
});
