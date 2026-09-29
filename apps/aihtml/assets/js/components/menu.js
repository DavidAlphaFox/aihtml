/* Behaviour of the menu component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.js. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_nav.js";

var NS = AH.NS;
var L = AH.lib.nav;
var instanceNs = L.instanceNs;
var visible = L.visible;
var setValue = L.setValue;
var byKey = L.byKey;

function touchOnly() {
  return window.matchMedia && window.matchMedia("(hover: none)").matches;
}

// ------------------------------------------------------------------
// menu (sigil menu.cljs, menu/submenu, menu/keyboard, menu/responsive)
// ------------------------------------------------------------------

var M_OPEN = "ah-menu-submenu-open";
var M_FOCUS = "ah-menu-link-focus";

// Submenus float (AH.float) so an ancestor with overflow: hidden cannot
// clip them: below a horizontal bar's top-level item, to the right of
// any other item. AH.float flips and clamps them to the viewport.
function subFloat(sub, on, opts) {
  var h = $.data(sub, "ah-float");
  if (h) { h.stop(); $.removeData(sub, "ah-float"); }
  if (on) { $.data(sub, "ah-float", AH.float(sub, opts.anchor, opts)); }
}

function subClose($subs) {
  $subs.each(function () { subFloat(this, false); })
    .removeClass(M_OPEN).css({ left: "", right: "", top: "", bottom: "" });
}

function menuOpenSub($item, $el) {
  var $sub = $item.children(".ah-menu-submenu");
  if (!$sub.length || $sub.hasClass(M_OPEN)) { return; }
  $sub.addClass(M_OPEN);
  if (!$el.hasClass("ah-menu-is-minimized") && !$item.closest(".ah-menu-drawer").length) {
    var bar = $item.parent().hasClass("ah-menu-list") && $el.hasClass("ah-menu-horizontal");
    var $link = $item.children(".ah-menu-link");
    subFloat($sub[0], true, {
      anchor: bar ? $link[0] : $item[0],
      placement: bar ? "bottom" : ($sub.hasClass("ah-menu-open-left") ? "left" : "right"),
      align: $sub.hasClass("ah-menu-open-up") ? "end" : (bar && $sub.hasClass("ah-menu-open-left") ? "end" : "start"),
      offset: 0
    });
  }
  $item.children(".ah-menu-link").attr("aria-expanded", "true");
}

function menuCloseSub($item) {
  var $sub = $item.children(".ah-menu-submenu");
  if (!$sub.length) { return; }
  subClose($sub.find("." + M_OPEN));
  $sub.find("[aria-expanded=true]").attr("aria-expanded", "false");
  subClose($sub);
  $item.children(".ah-menu-link").attr("aria-expanded", "false");
}

function menuCloseSiblings($item) {
  $item.siblings(".ah-menu-has-submenu").each(function () { menuCloseSub($(this)); });
}

function menuCloseAll($el) {
  subClose($el.find("." + M_OPEN));
  $el.find("[aria-expanded=true]").not(".ah-menu-minimized-btn").attr("aria-expanded", "false");
}

function menuFocus($el, link) {
  $el.find("." + M_FOCUS).removeClass(M_FOCUS);
  $(link).addClass(M_FOCUS);
  link.focus();
}

function siblingLinks($item) {
  return $item.parent().children(".ah-menu-item:not(.ah-menu-item-disabled)")
    .children(".ah-menu-link").get();
}

function firstIn($item) {
  return $item.children(".ah-menu-submenu").find(".ah-menu-item:not(.ah-menu-item-disabled) > .ah-menu-link")
    .get(0);
}

function menuPopupClose($el) {
  if ($el.hasClass("ah-menu-popup")) {
    menuCloseAll($el);
    $el.removeClass("ah-menu-open");
  }
}

function menuPopupOpen($el, x, y) {
  $el.addClass("ah-menu-open").css({ left: x + "px", top: y + "px" });
  var w = $el[0].offsetWidth;
  var h = $el[0].offsetHeight;
  var ax = x + w > window.innerWidth ? Math.max(0, window.innerWidth - w) : x;
  var ay = y + h > window.innerHeight ? Math.max(0, window.innerHeight - h) : y;
  $el.css({ left: ax + "px", top: ay + "px" });
  $el[0].focus();
}

// A leaf was chosen: follow its href, or make its key the value.
function menuSelect($el, $link, e) {
  var href = $link.attr("href");
  if (!href) {
    if (e) { e.preventDefault(); }
    $el.find(".ah-menu-link-active").removeClass("ah-menu-link-active").removeAttr("aria-current");
    $link.addClass("ah-menu-link-active").attr("aria-current", "true");
    setValue($el, $link.attr("data-id"), "change");
  }
  menuCloseAll($el);
  $el.find("." + M_FOCUS).removeClass(M_FOCUS);
  menuPopupClose($el);
}

function menuKeydown($el, e) {
  var $focused = $el.find("." + M_FOCUS);
  var $item = $focused.length ? $focused.parent() : null;
  var horizontal = $el.hasClass("ah-menu-horizontal");
  var topLevel = $item && $item.parent().hasClass("ah-menu-list");
  var step = function (d) {
    if (!$item) { return; }
    var links = siblingLinks($item);
    var i = links.indexOf($focused[0]);
    if (links.length) { menuFocus($el, links[(i + d + links.length) % links.length]); }
  };
  var openFirst = function () {
    menuCloseSiblings($item);
    menuOpenSub($item, $el);
    var f = firstIn($item);
    if (f) { menuFocus($el, f); }
  };
  if (!$item && /^Arrow|^Home$|^End$/.test(e.key)) {
    e.preventDefault();
    var first = $el.find(".ah-menu-list > .ah-menu-item:not(.ah-menu-item-disabled) > .ah-menu-link").get(0);
    if (first) { menuFocus($el, first); }
    return;
  }
  if (!$item && e.key !== "Escape" && e.key !== "Tab") { return; }
  switch (e.key) {
    case "ArrowDown":
      e.preventDefault();
      if (horizontal && topLevel && $item.hasClass("ah-menu-has-submenu")) { openFirst(); }
      else { step(1); }
      break;
    case "ArrowUp":
      e.preventDefault();
      step(-1);
      break;
    case "ArrowRight":
      e.preventDefault();
      if (horizontal && topLevel) { menuCloseAll($el); step(1); }
      else if ($item.hasClass("ah-menu-has-submenu")) { openFirst(); }
      else if (horizontal) {
        // leave a submenu to the next top-level item, as menubars do
        var $top = $item.parents(".ah-menu-list > .ah-menu-item").last();
        menuCloseAll($el);
        $focused = $top.children(".ah-menu-link");
        $item = $top;
        step(1);
      }
      break;
    case "ArrowLeft":
      e.preventDefault();
      if (horizontal && topLevel) { menuCloseAll($el); step(-1); }
      else {
        var $parentItem = $item.parent().closest(".ah-menu-item");
        if ($parentItem.length) {
          menuCloseSub($parentItem);
          menuFocus($el, $parentItem.children(".ah-menu-link")[0]);
        }
      }
      break;
    case "Home":
    case "End":
      e.preventDefault();
      if ($item) {
        var ls = siblingLinks($item);
        if (ls.length) { menuFocus($el, ls[e.key === "Home" ? 0 : ls.length - 1]); }
      }
      break;
    case "Enter":
    case " ":
      e.preventDefault();
      if ($item.hasClass("ah-menu-has-submenu")) { openFirst(); }
      else { $focused[0].click(); }
      break;
    case "Escape":
      e.preventDefault();
      var $up = $item ? $item.parent().closest(".ah-menu-item") : $();
      if ($up.length && $up.children(".ah-menu-submenu").hasClass(M_OPEN) && !$el.hasClass("ah-menu-popup")) {
        menuCloseSub($up);
        menuFocus($el, $up.children(".ah-menu-link")[0]);
      } else {
        menuCloseAll($el);
        $el.find("." + M_FOCUS).removeClass(M_FOCUS);
        menuPopupClose($el);
        if (visible($el[0])) { $el[0].focus(); }
      }
      break;
    case "Tab":
      menuCloseAll($el);
      $el.find("." + M_FOCUS).removeClass(M_FOCUS);
      menuPopupClose($el);
      break;
    default:
      break;
  }
}

function menuDrawerClose(st) {
  if (!st.drawer) { return; }
  st.drawer.removeClass("ah-menu-drawer-open");
  st.backdrop.removeClass("ah-menu-drawer-backdrop-visible");
}

function menuMinimize($el, st) {
  if ($el.hasClass("ah-menu-is-minimized")) { return; }
  $el.addClass("ah-menu-is-minimized");
  var title = $el.attr("data-title") || "Menu";
  var $drawer = $('<div class="ah-menu-drawer" role="dialog" aria-modal="true"></div>');
  var $head = $('<div class="ah-menu-drawer-title"><span></span>' +
                '<button type="button" class="ah-menu-drawer-close" aria-label="Close">×</button></div>');
  $head.children("span").text(title);
  var $list = $('<div class="ah-menu-drawer-list"></div>')
    .append($el.children(".ah-menu-list").clone().removeAttr("id"));
  $list.find("[id]").removeAttr("id");
  $list.find("." + M_OPEN).removeClass(M_OPEN);
  $drawer.attr("aria-label", title).append($head, $list);
  var $backdrop = $('<div class="ah-menu-drawer-backdrop"></div>');
  $("body").append($backdrop, $drawer);
  st.drawer = $drawer;
  st.backdrop = $backdrop;
  $drawer.on("click", ".ah-menu-drawer-close", function () { menuDrawerClose(st); });
  $backdrop.on("click", function () { menuDrawerClose(st); });
  $drawer.on("keydown", function (e) {
    if (e.key === "Escape") { menuDrawerClose(st); $el.children(".ah-menu-minimized-btn")[0].focus(); }
  });
  $drawer.on("click", ".ah-menu-has-submenu > .ah-menu-link", function (e) {
    e.preventDefault();
    var $sub = $(this).parent().children(".ah-menu-submenu").toggleClass(M_OPEN);
    $(this).attr("aria-expanded", String($sub.hasClass(M_OPEN)));
  });
  $drawer.on("click", ".ah-menu-item:not(.ah-menu-has-submenu):not(.ah-menu-item-disabled) > .ah-menu-link",
    function (e) {
      var $orig = byKey($el, ".ah-menu-link", "data-id", this.getAttribute("data-id"));
      if (!this.getAttribute("href")) { e.preventDefault(); }
      if ($orig.length) { menuSelect($el, $orig.first(), null); }
      $drawer.find(".ah-menu-link-active").removeClass("ah-menu-link-active");
      $(this).addClass("ah-menu-link-active");
      menuDrawerClose(st);
    });
}

function menuRestore($el, st) {
  if (!$el.hasClass("ah-menu-is-minimized")) { return; }
  $el.removeClass("ah-menu-is-minimized");
  if (st.drawer) { st.drawer.remove(); st.backdrop.remove(); }
  st.drawer = st.backdrop = null;
}

AH.define("menu", {
  init: function (el, $el) {
    var st = { openT: null, closeT: null, drawer: null, backdrop: null };
    $.data(el, "ah-menu", st);
    var ns = instanceNs(el);
    var clickToOpen = el.hasAttribute("data-ah-click-to-open");

    // hover opens submenus, with a short intent delay when a sibling is open
    $el.on("mouseenter" + NS, ".ah-menu-has-submenu", function () {
      if (clickToOpen || $el.hasClass("ah-menu-is-minimized")) { return; }
      var $item = $(this);
      clearTimeout(st.closeT);
      clearTimeout(st.openT);
      var open = function () {
        st.openT = null;
        menuCloseSiblings($item);
        menuOpenSub($item, $el);
      };
      if ($item.parent().find("> .ah-menu-item > ." + M_OPEN).length) {
        st.openT = setTimeout(open, 60);
      } else {
        open();
      }
    });
    $el.on("mouseleave" + NS, ".ah-menu-has-submenu", function () {
      if (clickToOpen) { return; }
      var $item = $(this);
      clearTimeout(st.openT);
      st.closeT = setTimeout(function () {
        menuCloseSiblings($item);
        menuCloseSub($item);
      }, 200);
    });
    // click toggles a submenu (always in click-to-open or touch mode;
    // otherwise it opens one that hover has not opened, e.g. from a
    // screen reader)
    $el.on("click" + NS, ".ah-menu-has-submenu > .ah-menu-link", function (e) {
      e.preventDefault();
      var $item = $(this).parent();
      var open = $item.children(".ah-menu-submenu").hasClass(M_OPEN);
      if (open && (clickToOpen || touchOnly())) {
        menuCloseSub($item);
      } else if (!open) {
        menuCloseSiblings($item);
        menuOpenSub($item, $el);
      }
    });
    $el.on("click" + NS, ".ah-menu-item:not(.ah-menu-has-submenu):not(.ah-menu-item-disabled) > .ah-menu-link",
      function (e) { menuSelect($el, $(this), e); });
    $el.on("click" + NS, ".ah-menu-item-disabled > .ah-menu-link", function (e) { e.preventDefault(); });

    // outside click closes everything
    $(document).on("mousedown" + ns, function (e) {
      if (!$.contains(el, e.target) && !$(e.target).closest(".ah-menu-drawer").length) {
        menuCloseAll($el);
        $el.find("." + M_FOCUS).removeClass(M_FOCUS);
        menuPopupClose($el);
      }
    });

    // context menu
    if ($el.hasClass("ah-menu-popup")) {
      var target = el.getAttribute("data-ah-popup-target");
      $.data(el, "ah-menu-target", target || document);
      $(target || document).on("contextmenu" + ns, function (e) {
        e.preventDefault();
        menuCloseAll($el);
        menuPopupOpen($el, e.clientX, e.clientY);
      });
    }

    if (el.getAttribute("data-ah-keyboard") !== "false") {
      $el.on("keydown" + NS, function (e) {
        if (e.target === el || $(e.target).hasClass("ah-menu-link")) { menuKeydown($el, e); }
      });
      $el.on("focus" + NS, function () {
        if (!$el.find("." + M_FOCUS).length && !$el.hasClass("ah-menu-is-minimized")) {
          var f = $el.find(".ah-menu-list > .ah-menu-item:not(.ah-menu-item-disabled) > .ah-menu-link").get(0);
          if (f) { menuFocus($el, f); }
        }
      });
    }

    // responsive collapse to a hamburger + drawer
    $el.on("click" + NS, ".ah-menu-minimized-btn", function () {
      if (!st.drawer) { return; }
      st.backdrop.addClass("ah-menu-drawer-backdrop-visible");
      requestAnimationFrame(function () {
        st.drawer.addClass("ah-menu-drawer-open");
        var f = st.drawer.find(".ah-menu-link").get(0);
        if (f) { f.setAttribute("tabindex", "0"); f.focus(); }
      });
    });
    var minW = parseInt(el.getAttribute("data-ah-minimize-width"), 10);
    if (minW) {
      var check = function () {
        if (window.innerWidth <= minW) { menuMinimize($el, st); } else { menuRestore($el, st); }
      };
      var t = null;
      $(window).on("resize" + ns, function () { clearTimeout(t); t = setTimeout(check, 150); });
      check();
    }
  },
  destroy: function (el, $el) {
    var st = $.data(el, "ah-menu") || {};
    var ns = instanceNs(el);
    $(document).off(ns);
    $(window).off(ns);
    var target = $.data(el, "ah-menu-target");
    if (target) { $(target).off(ns); }
    clearTimeout(st.openT);
    clearTimeout(st.closeT);
    menuCloseAll($el);
    menuRestore($el, st);
  },
  methods: {
    open: function (el, $el, x, y) { menuPopupOpen($el, x, y); },
    close: function (el, $el) { menuCloseAll($el); $el.removeClass("ah-menu-open"); },
    closeAll: function (el, $el) { menuCloseAll($el); },
    openItem: function (el, $el, key) {
      var $l = byKey($el, ".ah-menu-link", "data-id", key);
      if ($l.length) { menuOpenSub($l.parent(), $el); }
    },
    closeItem: function (el, $el, key) {
      var $l = byKey($el, ".ah-menu-link", "data-id", key);
      if ($l.length) { menuCloseSub($l.parent()); }
    },
    disableItem: function (el, $el, key) {
      byKey($el, ".ah-menu-link", "data-id", key).attr("aria-disabled", "true")
        .parent().addClass("ah-menu-item-disabled");
    },
    enableItem: function (el, $el, key) {
      byKey($el, ".ah-menu-link", "data-id", key).removeAttr("aria-disabled")
        .parent().removeClass("ah-menu-item-disabled");
    },
    setValue: function (el, $el, key) {
      $el.find(".ah-menu-link-active").removeClass("ah-menu-link-active").removeAttr("aria-current");
      byKey($el, ".ah-menu-link", "data-id", key).addClass("ah-menu-link-active").attr("aria-current", "true");
      setValue($el, key);
    },
    minimize: function (el, $el) { menuMinimize($el, $.data(el, "ah-menu")); },
    restore: function (el, $el) { menuRestore($el, $.data(el, "ah-menu")); }
  }
});
