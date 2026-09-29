/* Behaviour of the sidenav component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.js. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_nav.js";

var NS = AH.NS;
var L = AH.lib.nav;
var visible = L.visible;
var setValue = L.setValue;
var byKey = L.byKey;

// ------------------------------------------------------------------
// sidenav (sigil sidenav.cljs + nav-tree)
// ------------------------------------------------------------------

function sidenavMark($el, key) {
  $el.find(".ah-nav-tree__item.ah-is-active").removeClass("ah-is-active").removeAttr("aria-current");
  var $a = byKey($el, "a.ah-nav-tree__item", "data-route", key);
  $a.addClass("ah-is-active").attr("aria-current", "page");
  $a.parents("details.ah-nav-tree__node").each(function () {
    this.open = true;
    $(this).children("summary").addClass("ah-is-open");
  });
}

function sidenavCollapse($el, collapsed) {
  $el.toggleClass("ah-sidenav-collapsed", collapsed);
  $el.find(".ah-sidenav__toggle").attr("aria-expanded", String(!collapsed));
  $el.trigger("ah:collapse", [{ collapsed: collapsed }]);
}

AH.define("sidenav", {
  init: function (el, $el) {
    $el.on("click" + NS, "a.ah-nav-tree__item", function (e) {
      if (this.getAttribute("aria-disabled") === "true") { e.preventDefault(); return; }
      var key = this.getAttribute("data-route");
      sidenavMark($el, key);
      if (this.getAttribute("href") === "#") {
        e.preventDefault();
        if ($el.attr("data-ah-value") !== key) { setValue($el, key, "change"); }
      }
    });
    // a collapsed sidebar expands when a group is opened
    $el.on("click" + NS, "summary.ah-nav-tree__item", function (e) {
      if ($el.hasClass("ah-sidenav-collapsed")) {
        e.preventDefault();
        sidenavCollapse($el, false);
        this.parentNode.open = true;
        $(this).addClass("ah-is-open");
      }
    });
    var onToggle = function (e) {
      if (e.target.tagName === "DETAILS") {
        $(e.target).children("summary").toggleClass("ah-is-open", e.target.open);
      }
    };
    el.addEventListener("toggle", onToggle, true);
    $.data(el, "ah-sidenav-toggle", onToggle);
    $el.on("click" + NS, ".ah-sidenav__toggle", function () {
      sidenavCollapse($el, !$el.hasClass("ah-sidenav-collapsed"));
    });
    // arrow keys move between the visible entries
    $el.on("keydown" + NS, ".ah-nav-tree__item", function (e) {
      if (!/^(ArrowDown|ArrowUp|Home|End)$/.test(e.key)) { return; }
      var items = $el.find(".ah-nav-tree__item").filter(function () { return visible(this); }).get();
      var i = items.indexOf(this);
      var to = e.key === "Home" ? 0 : e.key === "End" ? items.length - 1
        : Math.max(0, Math.min(items.length - 1, i + (e.key === "ArrowDown" ? 1 : -1)));
      e.preventDefault();
      if (items[to]) { items[to].focus(); }
    });
  },
  destroy: function (el) {
    var f = $.data(el, "ah-sidenav-toggle");
    if (f) { el.removeEventListener("toggle", f, true); }
  },
  methods: {
    setValue: function (el, $el, key) { sidenavMark($el, key); setValue($el, key); },
    collapse: function (el, $el) { sidenavCollapse($el, true); },
    expand: function (el, $el) { sidenavCollapse($el, false); },
    toggle: function (el, $el) { sidenavCollapse($el, !$el.hasClass("ah-sidenav-collapsed")); }
  }
});
