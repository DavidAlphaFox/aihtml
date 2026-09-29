/* Behaviour of the navbar component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.js. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_nav.js";

var NS = AH.NS;
var L = AH.lib.nav;
var instanceNs = L.instanceNs;
var setValue = L.setValue;
var byKey = L.byKey;

// ------------------------------------------------------------------
// navbar (sigil navbar.cljs, navbar/popup.cljs)
// ------------------------------------------------------------------

function navbarMark($scope, key) {
  $scope.find(".ah-navbar-item").each(function () {
    var on = this.getAttribute("data-key") === String(key);
    $(this).toggleClass("ah-navbar-item-selected", on).attr("aria-selected", String(on));
    if ($scope.hasClass("ah-navbar")) { this.setAttribute("tabindex", on ? "0" : "-1"); }
  });
}

function navbarSelect($el, item, e) {
  if ($(item).hasClass("ah-navbar-item-disabled") || $el.attr("data-ah-selection") === "false") {
    if (e && !item.getAttribute("href")) { e.preventDefault(); }
    return;
  }
  var key = item.getAttribute("data-key");
  navbarMark($el, key);
  if (!item.getAttribute("href")) {
    if (e) { e.preventDefault(); }
    if ($el.attr("data-ah-value") !== key) { setValue($el, key, "change"); }
  }
}

function navbarPopupClose($el) {
  var $p = $.data($el[0], "ah-navbar-popup");
  if ($p) {
    var h = $.data($p[0], "ah-float");
    if (h) { h.stop(); }
    $p.remove();
    $.removeData($el[0], "ah-navbar-popup");
    $el.find(".ah-navbar-header").attr("aria-expanded", "false");
  }
}

function navbarPopupOpen($el) {
  var $p = $('<div class="ah-navbar-popup" role="listbox"></div>');
  $el.children(".ah-navbar-item").each(function () {
    var $c = $(this).clone().removeAttr("id style").attr({ role: "option", tabindex: "0" });
    $c.find("[id]").removeAttr("id");
    $p.append($c);
  });
  $p.css({ width: $el.outerWidth() + "px", display: "block" });
  $("body").append($p);
  $.data($el[0], "ah-navbar-popup", $p);
  $.data($p[0], "ah-float", AH.float($p[0], $el[0], { placement: "bottom", offset: 0, matchWidth: true }));
  $el.find(".ah-navbar-header").attr("aria-expanded", "true");
  $p.on("click", ".ah-navbar-item", function (e) {
    var $orig = byKey($el, ".ah-navbar-item", "data-key", this.getAttribute("data-key"));
    if ($orig.length) { navbarSelect($el, $orig[0], e); }
    navbarPopupClose($el);
  });
  $p.on("keydown", ".ah-navbar-item", function (e) {
    var items = $p.children(".ah-navbar-item").get();
    var i = items.indexOf(this);
    if (e.key === "ArrowDown" || e.key === "ArrowUp") {
      e.preventDefault();
      items[(i + (e.key === "ArrowDown" ? 1 : -1) + items.length) % items.length].focus();
    } else if (e.key === "Enter" || e.key === " ") {
      e.preventDefault();
      this.click();
      $el.find(".ah-navbar-header")[0].focus();
    } else if (e.key === "Escape") {
      navbarPopupClose($el);
      $el.find(".ah-navbar-header")[0].focus();
    }
  });
  var sel = $p.children(".ah-navbar-item-selected").get(0) || $p.children(".ah-navbar-item").get(0);
  return sel;
}

AH.define("navbar", {
  init: function (el, $el) {
    var ns = instanceNs(el);
    $el.on("click" + NS, ".ah-navbar-item", function (e) { navbarSelect($el, this, e); });
    $el.on("mouseenter" + NS, ".ah-navbar-item", function () { $(this).addClass("ah-navbar-item-hover"); });
    $el.on("mouseleave" + NS, ".ah-navbar-item", function () { $(this).removeClass("ah-navbar-item-hover"); });
    // tabs pattern: arrows move focus, Enter / Space select
    $el.on("keydown" + NS, ".ah-navbar-item", function (e) {
      var items = $el.children(".ah-navbar-item").filter(function () {
        return !$(this).hasClass("ah-navbar-item-disabled");
      }).get();
      var i = items.indexOf(this);
      var n = items.length;
      var to = null;
      switch (e.key) {
        case "ArrowRight": case "ArrowDown": to = (i + 1) % n; break;
        case "ArrowLeft": case "ArrowUp": to = (i - 1 + n) % n; break;
        case "Home": to = 0; break;
        case "End": to = n - 1; break;
        case "Enter": case " ":
          e.preventDefault();
          this.click();
          return;
        default: return;
      }
      e.preventDefault();
      $(items).attr("tabindex", "-1");
      items[to].setAttribute("tabindex", "0");
      items[to].focus();
    });
    var toggle = function () {
      if ($.data(el, "ah-navbar-popup")) { navbarPopupClose($el); return null; }
      return navbarPopupOpen($el);
    };
    $el.on("click" + NS, ".ah-navbar-header", function () { toggle(); });
    $el.on("keydown" + NS, ".ah-navbar-header", function (e) {
      if (e.key === "Enter" || e.key === " " || e.key === "ArrowDown") {
        e.preventDefault();
        var first = $.data(el, "ah-navbar-popup") && e.key === "ArrowDown" ? null : toggle();
        if (first) { first.focus(); }
      } else if (e.key === "Escape") {
        navbarPopupClose($el);
      }
    });
    $(document).on("mousedown" + ns, function (e) {
      var $p = $.data(el, "ah-navbar-popup");
      if ($p && !$.contains($p[0], e.target) && !$(e.target).closest(".ah-navbar-header").length) {
        navbarPopupClose($el);
      }
    });
    var minW = parseInt(el.getAttribute("data-ah-minimize-width"), 10);
    if (minW && el.getAttribute("data-ah-minimized") !== "static") {
      var check = function () {
        var small = window.innerWidth <= minW;
        $el.toggleClass("ah-navbar-minimized", small);
        if (!small) { navbarPopupClose($el); }
      };
      $(window).on("resize" + ns, check);
      check();
    }
  },
  destroy: function (el, $el) {
    var ns = instanceNs(el);
    $(document).off(ns);
    $(window).off(ns);
    navbarPopupClose($el);
  },
  methods: {
    setValue: function (el, $el, key) { navbarMark($el, key); setValue($el, key); },
    select: function (el, $el, key) {
      var $i = byKey($el, ".ah-navbar-item", "data-key", key);
      if ($i.length) { navbarSelect($el, $i[0], null); }
    },
    minimize: function (el, $el) { $el.addClass("ah-navbar-minimized"); },
    restore: function (el, $el) { $el.removeClass("ah-navbar-minimized"); navbarPopupClose($el); }
  }
});
