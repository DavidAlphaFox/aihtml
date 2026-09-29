/* Behaviours of the layout_bars components (designs/04-components.md):
 * activity-bar, navigationbar and command, ported from sigil's
 * layout/activity_bar, layout/navigationbar and overlay/command.
 *
 * activity-bar and navigationbar keep their value in data-ah-value on the
 * root (the active item; the expanded indexes "0,2"), mirror it into a
 * hidden input and fire "change" when the user changes it. Methods called
 * by the server (AH.invoke / aihtml_action:call) do not fire "change".
 *
 * command filters the commands the server rendered by hiding the ones that
 * do not match; with a search action (data-ah-remote) the server renders
 * the results instead. It builds no HTML.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  function setValue(el, v) {
    el.setAttribute("data-ah-value", v);
    $(el).children("input[type=hidden]").val(v);
  }

  // ------------------------------------------------------------------
  // ActivityBar: a vertical tablist of icon buttons
  // ------------------------------------------------------------------

  function barItems(el) {
    return $(el).children(".ah-activity-bar__item");
  }

  function barActivate(el, id) {
    barItems(el).each(function () {
      var on = this.getAttribute("data-id") === String(id);
      this.setAttribute("data-active", String(on));
      this.setAttribute("aria-selected", String(on));
      this.setAttribute("tabindex", on ? "0" : "-1");
    });
    // keep one item reachable with Tab when nothing is active
    var $items = barItems(el);
    if (!$items.filter("[tabindex=0]").length) {
      $items.not("[data-disabled=true]").first().attr("tabindex", "0");
    }
    setValue(el, id == null ? "" : String(id));
  }

  function barChoose(el, $el, item) {
    if (item.getAttribute("data-disabled") === "true") { return; }
    var id = item.getAttribute("data-id");
    var changed = el.getAttribute("data-ah-value") !== id;
    barActivate(el, id);
    $el.trigger("ah:select", [id]);
    if (changed) { $el.trigger("change"); }
  }

  AH.define("activity-bar", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-activity-bar__item", function () {
        barChoose(el, $el, this);
      });
      // WAI-ARIA tabs: arrows move and activate, Home / End jump
      $el.on("keydown" + NS, ".ah-activity-bar__item", function (e) {
        var $en = barItems(el).not("[data-disabled=true]");
        var i = $en.index(this);
        var next;
        switch (e.key) {
          case "ArrowDown": case "ArrowRight": next = (i + 1) % $en.length; break;
          case "ArrowUp": case "ArrowLeft": next = (i - 1 + $en.length) % $en.length; break;
          case "Home": next = 0; break;
          case "End": next = $en.length - 1; break;
          default: return;
        }
        e.preventDefault();
        var t = $en[next];
        if (t) {
          t.focus();
          barChoose(el, $el, t);
        }
      });
    },
    methods: {
      setValue: function (el, $el, v) { barActivate(el, v); },
      getValue: function (el) { return el.getAttribute("data-ah-value"); }
    }
  });

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

  // ------------------------------------------------------------------
  // Command: search field + filtered list + keyboard navigation
  // ------------------------------------------------------------------

  var hotkeySeq = 0;

  function cmdInput($el) { return $el.find(".ah-command__input"); }
  function cmdVisible($el) { return $el.find(".ah-command__item").not("[hidden]"); }
  function cmdOverlay($el) { return $el.parent(".ah-command-overlay"); }

  function cmdActive(el, $el) {
    return cmdVisible($el).index($el.find(".ah-command__item[data-active=true]"));
  }

  function cmdSetActive(el, $el, i, scroll) {
    var $v = cmdVisible($el);
    var $input = cmdInput($el);
    $el.find(".ah-command__item[data-active=true]")
      .attr({ "data-active": "false", "aria-selected": "false" });
    if (!$v.length) {
      $input.removeAttr("aria-activedescendant");
      return;
    }
    i = ((i % $v.length) + $v.length) % $v.length;
    var it = $v[i];
    it.setAttribute("data-active", "true");
    it.setAttribute("aria-selected", "true");
    if (it.id) { $input.attr("aria-activedescendant", it.id); }
    if (scroll && it.scrollIntoView) { it.scrollIntoView({ block: "nearest" }); }
  }

  // sigil's match?: a case-insensitive substring of value, label or
  // description.
  function cmdFilter(el, $el) {
    if (el.hasAttribute("data-ah-remote")) { return; }
    var q = String(cmdInput($el).val() || "").trim().toLowerCase();
    $el.find(".ah-command__item").each(function () {
      var $it = $(this);
      var hay = [this.getAttribute("data-value") || "",
                 $it.find(".ah-command__item-label").text(),
                 $it.find(".ah-command__item-desc").text()].join("\n").toLowerCase();
      this.hidden = q !== "" && hay.indexOf(q) < 0;
    });
    var any = false;
    $el.find(".ah-command__group").each(function () {
      var shown = $(this).children(".ah-command__item").not("[hidden]").length > 0;
      this.hidden = !shown;
      any = any || shown;
    });
    $el.find(".ah-command__empty").prop("hidden", any);
    cmdSetActive(el, $el, 0, true);
  }

  function cmdFocus($el) {
    var inp = cmdInput($el)[0];
    if (inp) {
      inp.focus();
      var n = inp.value.length;
      try { inp.setSelectionRange(n, n); } catch (e) { /* not a text field */ }
    }
  }

  function cmdSelect(el, $el, it) {
    if (!it || it.getAttribute("data-disabled") === "true") { return; }
    var v = it.getAttribute("data-value");
    el.setAttribute("data-ah-value", v);
    $el.trigger("ah:select", [v]);
    if (cmdOverlay($el).length && el.getAttribute("data-close-on-select") !== "false") {
      cmdClose(el, $el);
    }
    var href = it.getAttribute("data-href");
    if (href) { window.location.href = href; }
  }

  function cmdOpen(el, $el) {
    var ov = cmdOverlay($el)[0];
    if (!ov || !ov.hidden) { return; }
    $.data(el, "ah-cmd-return", document.activeElement);
    if (!el.hasAttribute("data-ah-remote")) {
      cmdInput($el).val(el.getAttribute("data-ah-query") || "");
      cmdFilter(el, $el);
    }
    ov.hidden = false;
    cmdFocus($el);
    $el.trigger("ah:open");
  }

  function cmdClose(el, $el) {
    var ov = cmdOverlay($el)[0];
    if (!ov || ov.hidden) { return; }
    ov.hidden = true;
    var back = $.data(el, "ah-cmd-return");
    $.removeData(el, "ah-cmd-return");
    if (back && back.focus && document.contains(back)) { back.focus(); }
    $el.trigger("ah:close");
  }

  AH.define("command", {
    init: function (el, $el) {
      $el.on("input" + NS, ".ah-command__input", function () {
        cmdFilter(el, $el);
        $el.trigger("ah:query", [this.value]);
      });
      $el.on("keydown" + NS, ".ah-command__input", function (e) {
        var act = cmdActive(el, $el);
        switch (e.key) {
          case "ArrowDown": e.preventDefault(); cmdSetActive(el, $el, act + 1, true); break;
          case "ArrowUp": e.preventDefault(); cmdSetActive(el, $el, act - 1, true); break;
          case "Home": if (e.ctrlKey) { e.preventDefault(); cmdSetActive(el, $el, 0, true); } break;
          case "End": if (e.ctrlKey) { e.preventDefault(); cmdSetActive(el, $el, -1, true); } break;
          case "Enter":
            e.preventDefault();
            cmdSelect(el, $el, cmdVisible($el)[act]);
            break;
          case "Escape":
            e.preventDefault();
            if (cmdOverlay($el).length) { cmdClose(el, $el); } else { $el.trigger("ah:close"); }
            break;
        }
      });
      $el.on("mouseenter" + NS, ".ah-command__item", function () {
        cmdSetActive(el, $el, cmdVisible($el).index(this), false);
      });
      $el.on("click" + NS, ".ah-command__item", function () {
        cmdSelect(el, $el, this);
      });
      var $ov = cmdOverlay($el);
      $ov.on("mousedown" + NS, function (e) {
        if (e.target === e.currentTarget) { cmdClose(el, $el); }
      });
      var key = (el.getAttribute("data-hotkey") || "").toLowerCase();
      if (key) {
        var ns = ".ahcmd" + (++hotkeySeq);
        $.data(el, "ah-cmd-hotkey", ns);
        $(document).on("keydown" + ns, function (e) {
          if ((e.ctrlKey || e.metaKey) && String(e.key).toLowerCase() === key) {
            e.preventDefault();
            if ($ov.length && !$ov[0].hidden) { cmdClose(el, $el); } else { cmdOpen(el, $el); }
          }
        });
      }
      cmdFilter(el, $el);
      if (el.hasAttribute("data-auto-focus") && !$ov.length) {
        setTimeout(function () { cmdFocus($el); }, 0);
      }
    },
    destroy: function (el, $el) {
      var ns = $.data(el, "ah-cmd-hotkey");
      if (ns) { $(document).off(ns); }
      cmdOverlay($el).off(NS);
    },
    methods: {
      open: function (el, $el) { cmdOpen(el, $el); },
      close: function (el, $el) { cmdClose(el, $el); },
      toggle: function (el, $el) {
        var ov = cmdOverlay($el)[0];
        if (ov && !ov.hidden) { cmdClose(el, $el); } else { cmdOpen(el, $el); }
      },
      setQuery: function (el, $el, q) {
        cmdInput($el).val(q == null ? "" : String(q)).trigger("input");
      },
      focus: function (el, $el) { cmdFocus($el); },
      itemsLoaded: function (el, $el) { cmdSetActive(el, $el, 0, true); }
    }
  });
})(window.jQuery, window.AH);
