/* Behaviour of the navigation bar (designs/04-components.md), ported from
 * sigil's layout/navigationbar. The value (the expanded indexes "0,2") is
 * kept in data-ah-value on the root, mirrored into a hidden input, and
 * "change" fires when the user changes it. Methods called by the server
 * (AH.invoke / aihtml_action:call) do not fire "change". */
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;

function setValue(el, v) {
  el.setAttribute("data-ah-value", v);
  $(el).children("input[type=hidden]").val(v);
}

// ------------------------------------------------------------------
// NavigationBar: collapsible sections
// ------------------------------------------------------------------

function navItems(el) {
  return $(el).children(".ah-navigationbar-item");
}

function navHeader(el, i) {
  return navItems(el).eq(i).children(".ah-navigationbar-header");
}

function navExpanded(el) {
  return (el.getAttribute("data-ah-value") || "").split(",").filter(function (s) {
    return s !== "";
  }).map(Number);
}

function navStore(el, list) {
  var uniq = list.filter(function (v, i) { return list.indexOf(v) === i; });
  uniq.sort(function (a, b) { return a - b; });
  setValue(el, uniq.join(","));
}

function navMode(el) {
  return el.getAttribute("data-expand-mode") || "single_fit_height";
}

// single_fit_height with a fixed height: the open body fills what the
// headers leave (sigil's sync-content-height!).
function navFit(el) {
  if (!el.hasAttribute("data-fit")) { return; }
  var used = 0;
  navItems(el).each(function () {
    var $it = $(this);
    used += $it.children(".ah-navigationbar-header").outerHeight() +
      ($it.outerHeight() - $it.innerHeight());
  });
  var h = Math.max(0, $(el).innerHeight() - used);
  navItems(el).children(".ah-navigationbar-body").each(function () {
    if (this.style.display !== "none") { $(this).css("height", h + "px"); }
  });
}

function navAnimate(el, $el, i, open) {
  var $h = navHeader(el, i);
  var $body = navItems(el).eq(i).children(".ah-navigationbar-body");
  var anim = el.getAttribute("data-animation") || "slide";
  var ms = parseInt(el.getAttribute(open ? "data-expand-duration" : "data-collapse-duration"), 10);
  if (isNaN(ms)) { ms = 250; }
  $h.toggleClass("ah-navigationbar-header-expanded", open).attr("aria-expanded", String(open));
  $h.children(".ah-navigationbar-arrow").toggleClass("ah-navigationbar-arrow-up", open);
  var done = function () {
    if (open) {
      $body.show();
      navFit(el);
    } else {
      $body.hide();
    }
    $el.trigger(open ? "ah:expand" : "ah:collapse", [{ index: i }]);
  };
  $body.stop(true, true);
  if (anim === "slide") {
    $body[open ? "slideDown" : "slideUp"]({ duration: ms, complete: done });
  } else if (anim === "fade") {
    $body[open ? "fadeIn" : "fadeOut"]({ duration: ms, complete: done });
  } else {
    done();
  }
}

function navValid(el, i) {
  return typeof i === "number" && i >= 0 && i < navItems(el).length;
}

function navDisabled(el, i) {
  return navHeader(el, i).hasClass("ah-navigationbar-disabled");
}

function navCollapse(el, $el, i) {
  var cur = navExpanded(el);
  if (!navValid(el, i) || cur.indexOf(i) < 0) { return false; }
  navStore(el, cur.filter(function (x) { return x !== i; }));
  navAnimate(el, $el, i, false);
  return true;
}

function navExpand(el, $el, i) {
  var cur = navExpanded(el);
  if (!navValid(el, i) || navDisabled(el, i) || cur.indexOf(i) >= 0) { return false; }
  if (navMode(el) !== "multiple") {
    cur.forEach(function (x) { navCollapse(el, $el, x); });
  }
  navStore(el, navExpanded(el).concat([i]));
  navAnimate(el, $el, i, true);
  return true;
}

// A user toggle, as sigil's compute-proposed-indexes: single modes never
// close the open section, none never changes.
function navUserToggle(el, $el, i) {
  var mode = navMode(el);
  if (el.classList.contains("ah-navigationbar-disabled") || navDisabled(el, i) ||
      mode === "none") {
    return;
  }
  var changed;
  if (navExpanded(el).indexOf(i) >= 0) {
    changed = (mode === "single" || mode === "single_fit_height") ? false
      : navCollapse(el, $el, i);
  } else {
    changed = navExpand(el, $el, i);
  }
  if (changed) { $el.trigger("change"); }
}

function navIndex(el, header) {
  return navItems(el).children(".ah-navigationbar-header").index(header);
}

AH.define("navigationbar", {
  init: function (el, $el) {
    var mode = el.getAttribute("data-toggle-mode") || "click";
    var own = function (h) { return $(h).closest(".ah-navigationbar")[0] === el; };
    if (mode !== "none") {
      $el.on(mode + NS, ".ah-navigationbar-header", function () {
        if (own(this)) { navUserToggle(el, $el, navIndex(el, this)); }
      });
    }
    $el.on("keydown" + NS, ".ah-navigationbar-header", function (e) {
      if (!own(this)) { return; }
      var $hs = navItems(el).children(".ah-navigationbar-header").filter("[tabindex=0]");
      var i = $hs.index(this);
      var t;
      switch (e.key) {
        case "Enter": case " ":
          e.preventDefault();
          if (mode !== "none") { navUserToggle(el, $el, navIndex(el, this)); }
          return;
        case "ArrowDown": t = $hs[(i + 1) % $hs.length]; break;
        case "ArrowUp": t = $hs[(i - 1 + $hs.length) % $hs.length]; break;
        case "Home": t = $hs[0]; break;
        case "End": t = $hs[$hs.length - 1]; break;
        default: return;
      }
      e.preventDefault();
      if (t) { t.focus(); }
    });
    navFit(el);
  },
  methods: {
    expand: function (el, $el, i) { navExpand(el, $el, Number(i)); },
    collapse: function (el, $el, i) { navCollapse(el, $el, Number(i)); },
    toggle: function (el, $el, i) {
      i = Number(i);
      if (navExpanded(el).indexOf(i) >= 0) { navCollapse(el, $el, i); } else { navExpand(el, $el, i); }
    },
    setValue: function (el, $el, v) {
      var want = (Array.isArray(v) ? v : String(v == null ? "" : v).split(","))
        .filter(function (s) { return s !== ""; }).map(Number);
      navExpanded(el).forEach(function (i) {
        if (want.indexOf(i) < 0) { navCollapse(el, $el, i); }
      });
      want.forEach(function (i) {
        if (navExpanded(el).indexOf(i) < 0 && navValid(el, i)) {
          // setValue may open several sections whatever the mode
          navStore(el, navExpanded(el).concat([i]));
          navAnimate(el, $el, i, true);
        }
      });
    },
    getValue: function (el) { return navExpanded(el); },
    enable: function (el, $el, i) {
      navHeader(el, Number(i)).removeClass("ah-navigationbar-disabled")
        .removeAttr("aria-disabled").attr("tabindex", "0");
    },
    disable: function (el, $el, i) {
      navHeader(el, Number(i)).addClass("ah-navigationbar-disabled")
        .attr({ "aria-disabled": "true", tabindex: "-1" });
    }
  }
});
