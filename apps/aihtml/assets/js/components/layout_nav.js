/* Behaviours of the layout_nav components (designs/04-components.md):
   menu, navbar, sidenav, toolbar, splitter, listmenu. Ported from sigil's
   components/layout/*.cljs. status_bar is pure CSS. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var uid = 0;

  // ------------------------------------------------------------------
  // Shared helpers
  // ------------------------------------------------------------------

  // A per-instance namespace for document/window handlers, which
  // AH.destroy does not remove by itself.
  function instanceNs(el) {
    var ns = $.data(el, "ah-ns");
    if (!ns) {
      ns = NS + "-nav" + (++uid);
      $.data(el, "ah-ns", ns);
    }
    return ns;
  }

  function visible(el) {
    return !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length);
  }

  // Value contract: data-ah-value on the root, the hidden input, then a
  // jQuery event (change or input) on the root.
  function setValue($el, v, eventName) {
    v = v === null || v === undefined ? "" : String(v);
    $el.attr("data-ah-value", v);
    $el.children("input[type=hidden]").val(v);
    if (eventName) { $el.trigger(eventName); }
  }

  function touchOnly() {
    return window.matchMedia && window.matchMedia("(hover: none)").matches;
  }

  function byKey($scope, sel, attr, key) {
    return $scope.find(sel).filter(function () {
      return this.getAttribute(attr) === String(key);
    });
  }

  function animateSwap(oldEl, newEl, kind, dir, done) {
    if (!oldEl || oldEl === newEl) {
      $(newEl).show();
      done();
      return;
    }
    if (kind === "none" || !newEl.animate) {
      $(oldEl).hide();
      $(newEl).show();
      done();
      return;
    }
    var dur = 250;
    if (kind === "fade") {
      oldEl.animate([{ opacity: 1 }, { opacity: 0 }], { duration: dur / 2 }).onfinish = function () {
        $(oldEl).hide();
        $(newEl).show();
        newEl.animate([{ opacity: 0 }, { opacity: 1 }], { duration: dur / 2 }).onfinish = done;
      };
      return;
    }
    // slide: the old page leaves to one side while the new one comes in
    var s = dir > 0 ? "-100%" : "100%";
    var e = dir > 0 ? "100%" : "-100%";
    $(newEl).show();
    $(oldEl).css({ position: "absolute", top: 0, left: 0, width: "100%" });
    oldEl.animate([{ transform: "translateX(0)" }, { transform: "translateX(" + s + ")" }],
                  { duration: dur, easing: "ease" });
    newEl.animate([{ transform: "translateX(" + e + ")" }, { transform: "translateX(0)" }],
                  { duration: dur, easing: "ease" }).onfinish = function () {
      $(oldEl).hide().css({ position: "", top: "", left: "", width: "" });
      done();
    };
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

  // ------------------------------------------------------------------
  // toolbar (sigil toolbar.cljs, toolbar/overflow.cljs)
  // ------------------------------------------------------------------
  //
  // Tools that do not fit are moved (not copied) into the overflow popup,
  // so their handlers and data-ah-on keep working, and moved back when
  // there is room again.

  function tbState(el) { return $.data(el, "ah-toolbar"); }

  function tbTools($el) {
    return $el.children(".ah-toolbar-tool").map(function () {
      var $prev = $(this).prev();
      return {
        el: this,
        sep: $prev.hasClass("ah-toolbar-separator") ? $prev[0] : null,
        minimizable: this.getAttribute("data-ah-minimizable") !== "false",
        button: $(this).children("button.ah-toolbar-tool-el").length > 0
      };
    }).get();
  }

  function tbMinimize(st, t) {
    if (t.min) { return; }
    t.min = true;
    $(t.el).css("display", "none");
    if (t.sep) { $(t.sep).css("display", "none"); }
    $(t.menuSep).addClass("ah-toolbar-popup-separator-visible");
    $(t.popupTool).append($(t.el).children()).addClass("ah-toolbar-popup-tool-visible");
  }

  function tbRestore(st, t) {
    if (!t.min) { return; }
    t.min = false;
    $(t.el).append($(t.popupTool).children()).css("display", "");
    if (t.sep) { $(t.sep).css("display", ""); }
    $(t.menuSep).removeClass("ah-toolbar-popup-separator-visible");
    $(t.popupTool).removeClass("ah-toolbar-popup-tool-visible");
  }

  function tbGroups(st) {
    var shown = st.tools.filter(function (t) { return !t.min; });
    shown.forEach(function (t, i) {
      var prev = i > 0 && shown[i - 1].button && !t.sep;
      var next = i + 1 < shown.length && shown[i + 1].button && !shown[i + 1].sep;
      var $t = $(t.el).removeClass("ah-toolbar-tool-first ah-toolbar-tool-inner ah-toolbar-tool-last");
      if (!t.button) { return; }
      if (prev && next) { $t.addClass("ah-toolbar-tool-inner"); }
      else if (next) { $t.addClass("ah-toolbar-tool-first"); }
      else if (prev) { $t.addClass("ah-toolbar-tool-last"); }
    });
  }

  function tbLayout($el) {
    var st = tbState($el[0]);
    if (!st || !visible($el[0])) { return; }
    var $btn = $el.children(".ah-toolbar-minimize-btn");
    var avail = function () {
      // the button's margin-left is auto, so count its box only
      return $el.width() - ($btn.hasClass("ah-toolbar-minimize-visible") ? $btn[0].offsetWidth : 0);
    };
    var used = function () {
      return st.tools.reduce(function (acc, t) {
        if (t.min) { return acc; }
        return acc + $(t.el).outerWidth(true) + (t.sep ? $(t.sep).outerWidth(true) : 0);
      }, 0);
    };
    var cands;
    // minimise from the right while the tools overflow
    while (used() > avail() &&
           (cands = st.tools.filter(function (t) { return t.minimizable && !t.min; })).length) {
      $btn.addClass("ah-toolbar-minimize-visible");
      tbMinimize(st, cands[cands.length - 1]);
    }
    // restore from the left while they fit
    var hidden;
    while ((hidden = st.tools.filter(function (t) { return t.minimizable && t.min; })).length) {
      var t = hidden[0];
      tbRestore(st, t);
      if (hidden.length === 1) { $btn.removeClass("ah-toolbar-minimize-visible"); }
      if (used() > avail()) {
        $btn.addClass("ah-toolbar-minimize-visible");
        tbMinimize(st, t);
        break;
      }
    }
    var any = st.tools.some(function (t) { return t.min; });
    $btn.toggleClass("ah-toolbar-minimize-visible", any);
    if (!any) { tbClose($el); }
    tbGroups(st);
  }

  function tbOpen($el) {
    var st = tbState($el[0]);
    if (st.open) { return; }
    var w = parseInt($el.attr("data-ah-popup-width"), 10) || 200;
    st.popup.css({ width: w + "px" }).addClass("ah-toolbar-popup-open");
    st.float = AH.float(st.popup[0], $el[0], { placement: "bottom", align: "end", offset: 0 });
    st.open = true;
    $el.children(".ah-toolbar-minimize-btn").attr("aria-expanded", "true");
    $el.trigger("ah:open");
  }

  function tbClose($el) {
    var st = tbState($el[0]);
    if (!st || !st.open) { return; }
    st.popup.removeClass("ah-toolbar-popup-open");
    if (st.float) { st.float.stop(); st.float = null; }
    st.open = false;
    $el.children(".ah-toolbar-minimize-btn").attr("aria-expanded", "false");
    $el.trigger("ah:close");
  }

  function tbActivate($el, btn) {
    var $b = $(btn);
    var key = $b.attr("data-key");
    if (btn.hasAttribute("data-ah-toggle")) {
      var on = $b.attr("aria-pressed") !== "true";
      $b.attr("aria-pressed", String(on)).toggleClass("ah-btn-toggled", on);
    }
    if (key) { setValue($el, key, "change"); }
  }

  AH.define("toolbar", {
    init: function (el, $el) {
      var ns = instanceNs(el);
      var $popup = $('<div class="ah-toolbar-popup" role="menu" aria-label="Overflow tools"></div>');
      var st = { tools: tbTools($el), popup: $popup, open: false };
      st.tools.forEach(function (t) {
        if (t.sep) {
          t.menuSep = $('<div class="ah-toolbar-popup-separator" role="separator"></div>')
            .appendTo($popup)[0];
        }
        t.popupTool = $('<div class="ah-toolbar-popup-tool"></div>').appendTo($popup)[0];
        t.min = false;
      });
      $("body").append($popup);
      $.data(el, "ah-toolbar", st);

      var onTool = function (e) {
        var btn = e.currentTarget;
        if (btn.disabled) { return; }
        tbActivate($el, btn);
        if ($.contains($popup[0], btn) && !btn.hasAttribute("data-ah-toggle")) { tbClose($el); }
      };
      $el.on("click" + NS, "button.ah-toolbar-tool-el", onTool);
      $popup.on("click", "button.ah-toolbar-tool-el", onTool);
      $popup.on("keydown", function (e) {
        if (e.key === "Escape") {
          tbClose($el);
          $el.children(".ah-toolbar-minimize-btn")[0].focus();
        }
      });
      $el.on("click" + NS, ".ah-toolbar-minimize-btn", function (e) {
        e.stopPropagation();
        if (st.open) { tbClose($el); } else { tbOpen($el); }
      });
      $el.on("keydown" + NS, ".ah-toolbar-minimize-btn", function (e) {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          if (st.open) { tbClose($el); }
          else {
            tbOpen($el);
            var f = $popup.find("button:not([disabled]), select, input, [tabindex]").filter(function () {
              return visible(this);
            }).get(0);
            if (f) { f.focus(); }
          }
        }
      });
      // arrows move between tools, Home / End jump to the ends
      $el.on("keydown" + NS, function (e) {
        if (!/^(ArrowLeft|ArrowRight|Home|End)$/.test(e.key)) { return; }
        if (/^(INPUT|SELECT|TEXTAREA)$/.test(e.target.tagName)) { return; }
        var items = $el.find(".ah-toolbar-tool button:not([disabled]), .ah-toolbar-tool select, " +
                            ".ah-toolbar-tool input, .ah-toolbar-minimize-btn")
          .filter(function () { return visible(this); }).get();
        var i = items.indexOf(e.target);
        if (i < 0) { return; }
        var n = items.length;
        var to = e.key === "Home" ? 0 : e.key === "End" ? n - 1
          : (i + (e.key === "ArrowRight" ? 1 : -1) + n) % n;
        e.preventDefault();
        items[to].focus();
      });
      $(document).on("mousedown" + ns, function (e) {
        if (st.open && !$.contains($popup[0], e.target) &&
            !$(e.target).closest(".ah-toolbar-minimize-btn").length) {
          tbClose($el);
        }
      });
      if (window.ResizeObserver) {
        st.ro = new ResizeObserver(function () { tbLayout($el); });
        st.ro.observe(el);
      } else {
        $(window).on("resize" + ns, function () { tbLayout($el); });
      }
      requestAnimationFrame(function () { tbLayout($el); });
    },
    destroy: function (el) {
      var st = tbState(el);
      var ns = instanceNs(el);
      $(document).off(ns);
      $(window).off(ns);
      if (st) {
        if (st.ro) { st.ro.disconnect(); }
        if (st.float) { st.float.stop(); }
        st.tools.forEach(function (t) { tbRestore(st, t); });
        st.popup.remove();
      }
      $.removeData(el, "ah-toolbar");
    },
    methods: {
      layout: function (el, $el) { tbLayout($el); },
      open: function (el, $el) { tbOpen($el); },
      close: function (el, $el) { tbClose($el); },
      disableTool: function (el, $el, key, disabled) {
        var st = tbState(el);
        var $b = byKey($el.add(st ? st.popup : $()), "button.ah-toolbar-tool-el", "data-key", key);
        $b.prop("disabled", disabled !== false);
      },
      setPressed: function (el, $el, key, pressed) {
        var st = tbState(el);
        byKey($el.add(st ? st.popup : $()), "button.ah-toolbar-tool-el", "data-key", key)
          .attr("aria-pressed", String(!!pressed)).toggleClass("ah-btn-toggled", !!pressed);
      }
    }
  });

  // ------------------------------------------------------------------
  // splitter (sigil splitter.cljs)
  // ------------------------------------------------------------------
  //
  // The first pane's flex-basis is a fraction of the space left by the
  // bar, so the split keeps its proportion when the container resizes.

  function spState(el) {
    var st = $.data(el, "ah-splitter");
    if (st) { return st; }
    var $el = $(el);
    var horiz = $el.hasClass("ah-splitter-horizontal");
    var mins = (el.getAttribute("data-ah-min") || "0,0").split(",").map(function (x) {
      return parseFloat(x) || 0;
    });
    st = {
      horiz: horiz,
      $p: $el.children(".ah-splitter-panel"),
      $bar: $el.children(".ah-splitter-splitbar"),
      min0: mins[0],
      min1: mins[1] || 0,
      frac: null,
      saved: null
    };
    $.data(el, "ah-splitter", st);
    return st;
  }

  function spDim(st, node) { return st.horiz ? node.offsetHeight : node.offsetWidth; }
  function spAvail(el, st) { return spDim(st, el) - spDim(st, st.$bar[0]); }

  function spFormat(f) {
    var a = Math.round(f * 1000) / 10;
    var b = Math.round((100 - a) * 10) / 10;
    return a + "," + b;
  }

  function spApply(el, st, frac) {
    st.frac = Math.max(0, Math.min(1, frac));
    var bar = spDim(st, st.$bar[0]);
    st.$p.eq(0).css("flex", "0 0 calc((100% - " + bar + "px) * " + st.frac.toFixed(5) + ")");
    st.$bar.attr("aria-valuenow", Math.round(st.frac * 100));
  }

  // Resize pane 0 to px (clamped to the minimum sizes).
  function spResize(el, st, px) {
    var avail = spAvail(el, st);
    if (avail <= 0) { return; }
    var clamped = Math.max(st.min0, Math.min(avail - st.min1, px));
    spApply(el, st, clamped / avail);
  }

  function spCollapsed(el, st, on) {
    $(el).toggleClass("ah-splitter-collapsed", on);
    st.$p.eq(0).css(st.horiz ? "min-height" : "min-width", on ? "0px" : st.min0 + "px");
  }

  function spToggle(el, st) {
    if ($(el).hasClass("ah-splitter-collapsed")) {
      spCollapsed(el, st, false);
      spApply(el, st, st.saved !== null ? st.saved : 0.5);
      $(el).trigger("ah:expanded");
    } else {
      st.saved = st.frac;
      spCollapsed(el, st, true);
      spApply(el, st, 0);
      $(el).trigger("ah:collapsed");
    }
    setValue($(el), spFormat(st.frac), "change");
  }

  function spEnabled(el) {
    return !$(el).hasClass("ah-splitter-disabled") && el.getAttribute("data-ah-resizable") !== "false";
  }

  AH.define("splitter", {
    init: function (el, $el) {
      var st = spState(el);
      var avail = spAvail(el, st);
      // measure the initial split (pixels or percent) as a fraction
      if (avail > 0) {
        var v = el.getAttribute("data-ah-value");
        spApply(el, st, v ? parseFloat(v) / 100 : spDim(st, st.$p[0]) / avail);
      }
      var drag = null;
      st.$bar.on("pointerdown" + NS, function (e) {
        if (e.button !== 0 || !spEnabled(el) || $(e.target).closest(".ah-splitter-collapse-btn").length) {
          return;
        }
        e.preventDefault();
        if (this.setPointerCapture) { this.setPointerCapture(e.pointerId); }
        drag = { start: st.horiz ? e.clientY : e.clientX, size: spDim(st, st.$p[0]),
                 value: el.getAttribute("data-ah-value") };
        if ($el.hasClass("ah-splitter-collapsed")) { spCollapsed(el, st, false); }
        $el.addClass("ah-splitter-dragging");
        $el.trigger("ah:resize-start");
      });
      st.$bar.on("pointermove" + NS, function (e) {
        if (!drag) { return; }
        var want = drag.size + (st.horiz ? e.clientY : e.clientX) - drag.start;
        var max = spAvail(el, st) - st.min1;
        st.$bar.toggleClass("ah-splitbar-invalid", want <= st.min0 || want >= max);
        spResize(el, st, want);
        setValue($el, spFormat(st.frac), "input");
      });
      st.$bar.on("pointerup" + NS + " pointercancel" + NS, function () {
        if (!drag) { return; }
        var before = drag.value;
        drag = null;
        st.$bar.removeClass("ah-splitbar-invalid");
        $el.removeClass("ah-splitter-dragging");
        var v = spFormat(st.frac);
        if (v !== before) { setValue($el, v, "change"); }
        $el.trigger("ah:resize");
      });
      st.$bar.on("click" + NS, ".ah-splitter-collapse-btn", function (e) {
        e.stopPropagation();
        if (spEnabled(el) || $el.hasClass("ah-splitter-collapsed")) { spToggle(el, st); }
      });
      st.$bar.on("keydown" + NS, function (e) {
        if (e.target !== this || !spEnabled(el)) { return; }
        var step = (parseInt(el.getAttribute("data-ah-step"), 10) || 10) * (e.shiftKey ? 5 : 1);
        var cur = spDim(st, st.$p[0]);
        var dec = st.horiz ? "ArrowUp" : "ArrowLeft";
        var inc = st.horiz ? "ArrowDown" : "ArrowRight";
        var px;
        switch (e.key) {
          case dec: px = cur - step; break;
          case inc: px = cur + step; break;
          case "Home": px = 0; break;
          case "End": px = Infinity; break;
          case "Enter": e.preventDefault(); spToggle(el, st); return;
          default: return;
        }
        e.preventDefault();
        if ($el.hasClass("ah-splitter-collapsed")) { spCollapsed(el, st, false); }
        spResize(el, st, px);
        var v = spFormat(st.frac);
        if (v !== el.getAttribute("data-ah-value")) { setValue($el, v, "change"); }
      });
    },
    destroy: function (el) {
      $.removeData(el, "ah-splitter");
    },
    methods: {
      // sizes: pane 0 in percent (a number or "30")
      setSizes: function (el, $el, pct) {
        var st = spState(el);
        spCollapsed(el, st, false);
        spApply(el, st, parseFloat(pct) / 100);
        setValue($el, spFormat(st.frac));
      },
      getSizes: function (el) {
        var st = spState(el);
        return [spDim(st, st.$p[0]), spDim(st, st.$p[1])];
      },
      collapse: function (el) {
        if (!$(el).hasClass("ah-splitter-collapsed")) { spToggle(el, spState(el)); }
      },
      expand: function (el) {
        if ($(el).hasClass("ah-splitter-collapsed")) { spToggle(el, spState(el)); }
      }
    }
  });

  // ------------------------------------------------------------------
  // listmenu (sigil listmenu.cljs, listmenu/nav.cljs)
  // ------------------------------------------------------------------

  var LM_FOCUS = "ah-listmenu-item-focus";

  function lmState(el) {
    var st = $.data(el, "ah-listmenu");
    if (!st) {
      var s = el.getAttribute("data-ah-stack");
      st = { stack: s ? s.split(",") : [], busy: false };
      $.data(el, "ah-listmenu", st);
    }
    return st;
  }

  function lmPage($el, id) {
    return $el.find(".ah-listmenu-page").filter(function () {
      return this.getAttribute("data-page-id") === String(id);
    });
  }

  function lmCurrent($el) {
    var st = lmState($el[0]);
    return lmPage($el, st.stack.length ? st.stack[st.stack.length - 1] : "root");
  }

  function lmItemLabel($el, itemId) {
    return $el.find(".ah-listmenu-item").filter(function () {
      return this.getAttribute("data-item-id") === String(itemId);
    }).children(".ah-listmenu-item-label").text();
  }

  function lmHeader($el) {
    var st = lmState($el[0]);
    var root = !st.stack.length;
    $el.find(".ah-listmenu-back").toggle(!root);
    $el.find(".ah-listmenu-title").text(root ? "" : lmItemLabel($el, st.stack[st.stack.length - 1]));
  }

  function lmItems($page) {
    return $page.children(".ah-listmenu-item").filter(function () {
      return !$(this).hasClass("ah-listmenu-item-disabled") && this.style.display !== "none";
    });
  }

  function lmFocus($el, item) {
    $el.find("." + LM_FOCUS).removeClass(LM_FOCUS);
    if (item) {
      $(item).addClass(LM_FOCUS);
      if (item.scrollIntoView) { item.scrollIntoView({ block: "nearest" }); }
    }
  }

  function lmFilter($el, text) {
    var t = (text || "").toLowerCase();
    lmCurrent($el).children(".ah-listmenu-item").each(function () {
      var label = $(this).children(".ah-listmenu-item-label").text().toLowerCase();
      $(this).toggle(!t || label.indexOf(t) !== -1);
    });
  }

  function lmGo($el, pageId, dir, focus) {
    var st = lmState($el[0]);
    if (st.busy) { return; }
    var $old = lmCurrent($el);
    var $new = lmPage($el, pageId === null ? "root" : pageId);
    if (!$new.length) { return; }
    var label = dir > 0 ? lmItemLabel($el, pageId) : lmItemLabel($el, st.stack[st.stack.length - 1]);
    var id = dir > 0 ? pageId : st.stack[st.stack.length - 1];
    if (dir > 0) { st.stack.push(String(pageId)); } else { st.stack.pop(); }
    lmHeader($el);
    var $input = $el.find(".ah-listmenu-filter-input");
    if ($input.val()) { $input.val(""); $old.children().show(); }
    st.busy = true;
    animateSwap($old[0], $new[0], $el.attr("data-ah-animation") || "slide", dir, function () {
      st.busy = false;
      lmFocus($el, focus ? lmItems($new).get(0) : null);
      $el.trigger("ah:navigate", [{ id: id, label: label, page: $new.attr("data-page-id") }]);
    });
  }

  function lmBack($el, focus) {
    var st = lmState($el[0]);
    if (!st.stack.length) { return; }
    lmGo($el, st.stack.length > 1 ? st.stack[st.stack.length - 2] : null, -1, focus);
  }

  function lmMark($el, key) {
    $el.find(".ah-listmenu-item-selected").removeClass("ah-listmenu-item-selected")
      .attr("aria-checked", "false");
    byKey($el, ".ah-listmenu-item:not([aria-haspopup])", "data-key", key)
      .addClass("ah-listmenu-item-selected").attr("aria-checked", "true");
  }

  function lmActivate($el, item, focus) {
    var $i = $(item);
    if ($i.hasClass("ah-listmenu-item-disabled")) { return; }
    if ($i.attr("aria-haspopup")) {
      lmGo($el, $i.attr("data-item-id"), 1, focus);
      return;
    }
    var href = $i.attr("data-href");
    if (href) {
      window.location.href = href;
      return;
    }
    var key = $i.attr("data-key");
    lmMark($el, key);
    if ($el.attr("data-ah-value") !== key) { setValue($el, key, "change"); }
  }

  AH.define("listmenu", {
    init: function (el, $el) {
      lmState(el);
      $el.on("click" + NS, ".ah-listmenu-item", function () {
        lmFocus($el, null);
        lmActivate($el, this, false);
      });
      $el.on("click" + NS, ".ah-listmenu-back", function () { lmBack($el, false); });
      $el.on("input" + NS, ".ah-listmenu-filter-input", function (e) {
        e.stopPropagation();
        lmFilter($el, this.value);
      });
      // the filter's own change must not look like a new value
      $el.on("change" + NS, ".ah-listmenu-filter-input", function (e) { e.stopPropagation(); });
      $el.on("keydown" + NS, function (e) {
        var inFilter = $(e.target).hasClass("ah-listmenu-filter-input");
        var $page = lmCurrent($el);
        var items = lmItems($page).get();
        var cur = items.indexOf($el.find("." + LM_FOCUS)[0]);
        switch (e.key) {
          case "ArrowDown":
            e.preventDefault();
            lmFocus($el, items[Math.min(items.length - 1, cur + 1)]);
            break;
          case "ArrowUp":
            e.preventDefault();
            lmFocus($el, items[Math.max(0, cur - 1)]);
            break;
          case "Home":
          case "End":
            if (inFilter) { return; }
            e.preventDefault();
            lmFocus($el, items[e.key === "Home" ? 0 : items.length - 1]);
            break;
          case "Enter":
          case " ":
          case "ArrowRight":
            if (inFilter && e.key !== "Enter") { return; }
            if (cur < 0) { return; }
            e.preventDefault();
            lmActivate($el, items[cur], true);
            break;
          case "ArrowLeft":
          case "Backspace":
          case "Escape":
            if (inFilter && e.key !== "Escape") { return; }
            if (!lmState(el).stack.length) { return; }
            e.preventDefault();
            lmBack($el, true);
            break;
          default:
            break;
        }
      });
      $el.on("focus" + NS, function () {
        if (!$el.find("." + LM_FOCUS).length) {
          var $sel = lmCurrent($el).children(".ah-listmenu-item-selected");
          lmFocus($el, $sel[0] || lmItems(lmCurrent($el)).get(0));
        }
      });
      $el.on("blur" + NS, function () { lmFocus($el, null); });
    },
    destroy: function (el) {
      $.removeData(el, "ah-listmenu");
    },
    methods: {
      setValue: function (el, $el, key) { lmMark($el, key); setValue($el, key); },
      back: function (el, $el) { lmBack($el, false); },
      navigate: function (el, $el, key) {
        var $i = byKey(lmCurrent($el), ".ah-listmenu-item[aria-haspopup]", "data-key", key);
        if ($i.length) { lmGo($el, $i.attr("data-item-id"), 1, false); }
      },
      filter: function (el, $el, text) {
        $el.find(".ah-listmenu-filter-input").val(text || "");
        lmFilter($el, text);
      },
      currentPage: function (el, $el) { return lmCurrent($el).attr("data-page-id"); }
    }
  });

  // ------------------------------------------------------------------
  // status_bar: the count details float above their segment
  // ------------------------------------------------------------------

  AH.define("status-bar", {
    init: function (el, $el) {
      var show = function () {
        var pop = $(this).children(".ah-status-bar__popover")[0];
        if (!pop) { return; }
        clearTimeout($.data(pop, "ah-hide"));
        if (!$.data(pop, "ah-float")) {
          $.data(pop, "ah-float", AH.float(pop, this, { placement: "top", offset: 8 }));
        }
      };
      var hide = function () {
        var pop = $(this).children(".ah-status-bar__popover")[0];
        if (!pop || this.matches(":hover") || this.contains(document.activeElement)) { return; }
        // keep it in place while the CSS fade-out runs
        $.data(pop, "ah-hide", setTimeout(function () {
          var h = $.data(pop, "ah-float");
          if (h) { h.stop(); $.removeData(pop, "ah-float"); }
        }, 200));
      };
      $el.on("mouseenter" + NS + " focusin" + NS, ".ah-status-bar__count", show);
      $el.on("mouseleave" + NS + " focusout" + NS, ".ah-status-bar__count", hide);
    },
    destroy: function (el, $el) {
      $el.find(".ah-status-bar__popover").each(function () {
        clearTimeout($.data(this, "ah-hide"));
        var h = $.data(this, "ah-float");
        if (h) { h.stop(); }
      });
    }
  });
})(window.jQuery, window.AH);
