/* Behaviour of loader: show / hide, optionally with a modal scrim. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_layout.js";

var NS = AH.NS;
var L = AH.lib.layout;

var MODAL_ID = "ah-loader-modal";

function loaderShow(el, $el, left, top) {
  var modal = el.getAttribute("data-modal") === "true";
  if (modal) {
    var $m = $("#" + MODAL_ID);
    if (!$m.length) {
      $m = $("<div>", { id: MODAL_ID, "class": "ah-loader-modal" }).appendTo(document.body);
    }
    $m.removeClass("ah-loader-hidden");
    $(document).off("keyup" + NS + "loader").on("keyup" + NS + "loader", function (e) {
      if (L.key(e) === "Escape") { loaderHide(el, $el); }
    });
  }
  $el.removeClass("ah-loader-hidden").attr("aria-busy", "true");
  if (left !== undefined && left !== null && top !== undefined && top !== null) {
    $el.removeClass("ah-loader-center").css({ left: left + "px", top: top + "px" });
  } else if (modal) {
    $el.addClass("ah-loader-center");
  }
}

function loaderHide(el, $el) {
  $el.addClass("ah-loader-hidden").attr("aria-busy", "false");
  if (el.getAttribute("data-modal") === "true") {
    $("#" + MODAL_ID).addClass("ah-loader-hidden");
    $(document).off("keyup" + NS + "loader");
  }
}

AH.define("loader", {
  init: function (el, $el) {
    if (el.getAttribute("data-modal") === "true" && !$el.hasClass("ah-loader-hidden")) {
      loaderShow(el, $el);
    }
  },
  destroy: function (el, $el) {
    if (el.getAttribute("data-modal") === "true") {
      loaderHide(el, $el);
    }
  },
  methods: {
    show: function (el, $el, left, top) { loaderShow(el, $el, left, top); },
    hide: function (el, $el) { loaderHide(el, $el); },
    toggle: function (el, $el) {
      if ($el.hasClass("ah-loader-hidden")) { loaderShow(el, $el); } else { loaderHide(el, $el); }
    },
    text: function (el, $el, t) {
      $el.children(".ah-loader-text").text(t);
      $el.attr("aria-label", t);
    },
    isOpen: function (el, $el) { return !$el.hasClass("ah-loader-hidden"); }
  }
});
