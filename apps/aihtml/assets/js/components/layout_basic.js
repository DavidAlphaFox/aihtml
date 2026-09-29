/* Behaviours of the layout_basic components (designs/04-components.md).
 *
 * Ported from sigil's layout components (cljs + jQuery). Value-bearing
 * components (tabs, tab-bar, pagination, steps, expander) keep their value
 * in data-ah-value on the root, mirror it into a hidden input when there
 * is one, and fire "change" on the root when the user changes it. Methods
 * called by the server (AH.invoke / aihtml_action:call) update the value
 * without firing "change".
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  function setValue(el, $el, v) {
    el.setAttribute("data-ah-value", v);
    $el.children("input[type=hidden]").val(v);
  }

  function key(e) {
    return e.key;
  }

  // ------------------------------------------------------------------
  // Panel: scroll area with an optional collapsible header
  // ------------------------------------------------------------------

  function panelSet(el, $el, open, user) {
    var $w = $el.children(".ah-panel-wrapper");
    var $t = $el.children(".ah-panel-header").children(".ah-panel-toggle");
    if (!$t.length || ($t.attr("aria-expanded") === "true") === open) {
      return;
    }
    $t.attr("aria-expanded", String(open));
    $el.toggleClass("ah-panel-collapsed", !open);
    $w.stop(true, true)[open ? "slideDown" : "slideUp"](200, function () {
      if (user) {
        $el.trigger(open ? "ah:expand" : "ah:collapse");
      }
    });
  }

  AH.define("panel", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-panel-toggle", function (e) {
        if ($(this).closest(".ah-panel")[0] !== el) {
          return;
        }
        e.preventDefault();
        panelSet(el, $el, this.getAttribute("aria-expanded") !== "true", true);
      });
    },
    methods: {
      scrollTo: function (el, $el, x, y) {
        var w = $el.children(".ah-panel-wrapper")[0];
        if (w) {
          w.scrollLeft = x || 0;
          w.scrollTop = y || 0;
        }
      },
      refresh: function () { /* native scrolling: nothing to measure */ },
      collapse: function (el, $el) { panelSet(el, $el, false, false); },
      expand: function (el, $el) { panelSet(el, $el, true, false); },
      toggle: function (el, $el) {
        panelSet(el, $el, $el.hasClass("ah-panel-collapsed"), false);
      }
    }
  });

  // ------------------------------------------------------------------
  // Expander
  // ------------------------------------------------------------------

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
    setValue(el, $el, String(open));
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
        if (e.target === this && (key(e) === "Enter" || key(e) === " ")) {
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

  // ------------------------------------------------------------------
  // Tabs
  // ------------------------------------------------------------------

  function tabItems($el) {
    return $el.children(".ah-tabs-header").children(".ah-tabs-item");
  }

  function tabPanels($el) {
    return $el.children(".ah-tabs-content").children(".ah-tabs-panel");
  }

  function tabIndexOf($el, k) {
    var idx = -1;
    tabItems($el).each(function (i) {
      if (this.getAttribute("data-key") === String(k)) { idx = i; }
    });
    return idx;
  }

  function tabSelect(el, $el, idx, user) {
    var $items = tabItems($el);
    var $panels = tabPanels($el);
    var cur = $items.index($items.filter(".ah-tabs-item-selected"));
    if (idx < 0 || idx >= $items.length || idx === cur ||
        $items.eq(idx).hasClass("ah-tabs-item-disabled")) {
      return false;
    }
    $items.removeClass("ah-tabs-item-selected").attr({ "aria-selected": "false", tabindex: "-1" });
    $items.eq(idx).addClass("ah-tabs-item-selected").attr({ "aria-selected": "true", tabindex: "0" });
    $panels.attr("aria-hidden", "true");
    var $new = $panels.eq(idx).attr("aria-hidden", "false");
    var $old = cur >= 0 ? $panels.eq(cur) : $panels.not($new);
    $panels.stop(true, true);
    if ((el.getAttribute("data-animation") || "fade") === "fade" && $old.length) {
      $old.fadeOut(100, function () {
        $old.removeClass("ah-tabs-panel-active");
        $new.hide().fadeIn(100, function () { $new.addClass("ah-tabs-panel-active"); });
      });
    } else {
      $old.hide().removeClass("ah-tabs-panel-active");
      $new.css("display", "").addClass("ah-tabs-panel-active");
    }
    setValue(el, $el, $items.eq(idx).attr("data-key"));
    if (user) {
      $el.trigger("change");
    }
    return true;
  }

  // Next enabled index from start in direction dir, wrapping.
  function nextEnabled($items, start, dir, disabledCls) {
    var n = $items.length;
    for (var s = 1, i = (start + dir + n) % n; s <= n; s++, i = (i + dir + n) % n) {
      if (!$items.eq(i).hasClass(disabledCls)) { return i; }
    }
    return start;
  }

  // Arrow keys (by orientation), Home, End; Enter/Space activate.
  function listKeys(e, $items, cur, vertical, disabledCls) {
    var k = key(e);
    var prev = vertical ? "ArrowUp" : "ArrowLeft";
    var next = vertical ? "ArrowDown" : "ArrowRight";
    if (k === "Home") { return nextEnabled($items, -1, 1, disabledCls); }
    if (k === "End") { return nextEnabled($items, $items.length, -1, disabledCls); }
    if (k === prev) { return nextEnabled($items, cur, -1, disabledCls); }
    if (k === next) { return nextEnabled($items, cur, 1, disabledCls); }
    if (k === "Enter" || k === " ") { return cur; }
    return null;
  }

  AH.define("tabs", {
    init: function (el, $el) {
      var $header = $el.children(".ah-tabs-header");
      var ev = el.getAttribute("data-selection-mode") === "hover" ? "mouseenter" : "click";
      $header.on(ev + NS, ".ah-tabs-item", function () {
        if (!$el.hasClass("ah-tabs-disabled")) {
          tabSelect(el, $el, tabItems($el).index(this), true);
        }
      });
      $header.on("keydown" + NS, ".ah-tabs-item", function (e) {
        var $items = tabItems($el);
        var vertical = $el.hasClass("ah-tabs-left") || $el.hasClass("ah-tabs-right");
        var t = listKeys(e, $items, $items.index(this), vertical, "ah-tabs-item-disabled");
        if (t === null) { return; }
        e.preventDefault();
        tabSelect(el, $el, t, true);
        $items.eq(t).trigger("focus");
      });
      $header.on("click" + NS, ".ah-tabs-scroll-btn", function () {
        var step = $(this).hasClass("ah-tabs-scroll-left") ? -80 : 80;
        $header.scrollLeft($header.scrollLeft() + step);
      });
    },
    destroy: function (el, $el) {
      $el.children(".ah-tabs-header").off(NS);
    },
    methods: {
      select: function (el, $el, k) { tabSelect(el, $el, tabIndexOf($el, k), false); },
      disable: function (el, $el, k) {
        tabItems($el).eq(tabIndexOf($el, k)).addClass("ah-tabs-item-disabled").attr("aria-disabled", "true");
      },
      enable: function (el, $el, k) {
        tabItems($el).eq(tabIndexOf($el, k)).removeClass("ah-tabs-item-disabled").removeAttr("aria-disabled");
      },
      value: function (el) { return el.getAttribute("data-ah-value"); }
    }
  });

  // ------------------------------------------------------------------
  // Tab bar
  // ------------------------------------------------------------------

  function barTabs($el) {
    return $el.children(".ah-tab-bar__tab");
  }

  function barFind($el, id) {
    return barTabs($el).filter(function () { return this.getAttribute("data-id") === String(id); });
  }

  function barSelect(el, $el, $tab, user) {
    if (!$tab.length || $tab.attr("data-active") === "true") {
      return;
    }
    barTabs($el).attr({ "data-active": "false", "aria-selected": "false", tabindex: "-1" });
    $tab.attr({ "data-active": "true", "aria-selected": "true", tabindex: "0" });
    setValue(el, $el, $tab.attr("data-id"));
    if (user) {
      $el.trigger("change");
    }
  }

  function barClose(el, $el, $tab, user) {
    if (!$tab.length) {
      return;
    }
    var id = $tab.attr("data-id");
    var $all = barTabs($el);
    var idx = $all.index($tab);
    var wasActive = $tab.attr("data-active") === "true";
    var hadFocus = $.contains($tab[0], document.activeElement) || $tab[0] === document.activeElement;
    $tab.remove();
    $el.trigger("ah:close", [id]);
    if (wasActive) {
      var $left = barTabs($el);
      if ($left.length) {
        var $next = $left.eq(Math.min(idx, $left.length - 1));
        barSelect(el, $el, $next, user);
        if (hadFocus) { $next.trigger("focus"); }
      } else {
        setValue(el, $el, "");
        if (user) { $el.trigger("change"); }
      }
    }
  }

  AH.define("tab-bar", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-tab-bar__close", function (e) {
        e.stopPropagation();
        barClose(el, $el, $(this).closest(".ah-tab-bar__tab"), true);
      });
      $el.on("click" + NS, ".ah-tab-bar__tab", function () {
        barSelect(el, $el, $(this), true);
      });
      $el.on("keydown" + NS, ".ah-tab-bar__tab", function (e) {
        var $items = barTabs($el);
        var cur = $items.index(this);
        if (key(e) === "Delete" && $(this).children(".ah-tab-bar__close").length) {
          e.preventDefault();
          barClose(el, $el, $(this), true);
          return;
        }
        var t = listKeys(e, $items, cur, false, "ah-tab-bar__tab--none");
        if (t === null) { return; }
        e.preventDefault();
        barSelect(el, $el, $items.eq(t), true);
        $items.eq(t).trigger("focus");
      });
    },
    methods: {
      select: function (el, $el, id) { barSelect(el, $el, barFind($el, id), false); },
      close: function (el, $el, id) { barClose(el, $el, barFind($el, id), false); },
      value: function (el) { return el.getAttribute("data-ah-value"); }
    }
  });

  // ------------------------------------------------------------------
  // Pagination
  // ------------------------------------------------------------------

  // Same as aihtml_layout_basic:visible_pages/3; 0 stands for an ellipsis.
  function visiblePages(cur, total, max) {
    var i, out = [];
    max = Math.max(5, max);
    if (total <= max) {
      for (i = 1; i <= total; i++) { out.push(i); }
    } else if (cur <= max - 3) {
      for (i = 1; i <= max - 2; i++) { out.push(i); }
      out.push(0, total);
    } else if (cur >= total - (max - 4)) {
      out.push(1, 0);
      for (i = total - (max - 3); i <= total; i++) { out.push(i); }
    } else {
      var h = Math.floor((max - 5) / 2);
      out.push(1, 0);
      for (i = cur - h; i <= cur + h; i++) { out.push(i); }
      out.push(0, total);
    }
    return out;
  }

  function pgState(el) {
    var total = parseInt(el.getAttribute("data-total"), 10) || 0;
    var size = Math.max(1, parseInt(el.getAttribute("data-page-size"), 10) || 10);
    return {
      page: parseInt(el.getAttribute("data-ah-value"), 10) || 1,
      total: total,
      size: size,
      pages: Math.max(1, Math.ceil(total / size)),
      max: parseInt(el.getAttribute("data-max-visible"), 10) || 7
    };
  }

  function pgHref(el, page, size) {
    return el.getAttribute("data-href").replace(/\{page\}/g, page).replace(/\{size\}/g, size);
  }

  function pgFmt(t, args) {
    return args.reduce(function (acc, a, i) { return acc.split("{" + i + "}").join(String(a)); }, t);
  }

  var NAV_ICONS = { prev: "\u2039", next: "\u203A", first: "\u00AB", last: "\u00BB" };

  function pgEntry(m) {
    return $.extend({ gap: false, info: false, item: false, nav: false, link: false,
                      active: false, disabled: false, first_last: false, href: "",
                      number: 0, tabindex: 0, type: "", label: "", icon: "", text: "" }, m);
  }

  // The view data of templates/pagination_items.mustache; mirrors
  // aihtml_layout_basic:pagination_view/5.
  function pgView(cur, pages, max, size, cfg) {
    var link = function (p) {
      return cfg.href == null ? null
        : cfg.href.replace(/\{page\}/g, p).replace(/\{size\}/g, size);
    };
    var nav = function (type, disabled, target) {
      var url = disabled ? null : link(target);
      return pgEntry({ nav: true, type: type, label: cfg.labels[type], icon: NAV_ICONS[type],
                       first_last: type === "first" || type === "last", disabled: disabled,
                       tabindex: disabled ? -1 : 0, link: url !== null, href: url || "" });
    };
    var middle = cfg.simple
      ? [pgEntry({ info: true, text: pgFmt(cfg.labels.page_info, [cur, pages]) })]
      : visiblePages(cur, pages, max).map(function (p) {
        if (!p) { return pgEntry({ gap: true }); }
        var url = link(p);
        return pgEntry({ item: true, number: p, active: p === cur, tabindex: p === cur ? -1 : 0,
                         link: url !== null, href: url || "" });
      });
    var fl = cfg.first_last && !cfg.simple;
    var entries = [];
    if (fl) { entries.push(nav("first", cur === 1, 1)); }
    entries.push(nav("prev", cur === 1, cur - 1));
    entries = entries.concat(middle);
    entries.push(nav("next", cur === pages, cur + 1));
    if (fl) { entries.push(nav("last", cur === pages, pages)); }
    return { entries: entries };
  }

  function pgRender(el, $el) {
    var s = pgState(el);
    var $ul = $el.children(".ah-pagination-pages");
    var cfg = JSON.parse($ul.attr("data-view"));
    $ul.html(AH.tpl.pagination_items(pgView(s.page, s.pages, s.max, s.size, cfg)));
    var $suffix = $el.find(".ah-pagination-jumper > span").last();
    if ($suffix.length && $suffix.attr("data-template")) {
      $suffix.text(pgFmt($suffix.attr("data-template"), [s.pages]));
    }
  }

  function pgGo(el, $el, page, user) {
    var s = pgState(el);
    var p = Math.min(Math.max(1, page | 0), s.pages);
    if (isNaN(page) || p === s.page) {
      return false;
    }
    if (el.getAttribute("data-href")) {
      window.location.href = pgHref(el, p, s.size);
      return true;
    }
    setValue(el, $el, String(p));
    pgRender(el, $el);
    if (user) {
      $el.trigger("change");
      // keep keyboard focus inside the control after the rebuild
      $el.find(".ah-pagination-item-active").trigger("focus");
    }
    return true;
  }

  function pgSize(el, $el, size, user) {
    var s = pgState(el);
    if (!size || size === s.size) {
      return;
    }
    if (el.getAttribute("data-href")) {
      window.location.href = pgHref(el, 1, size);
      return;
    }
    el.setAttribute("data-page-size", String(size));
    var pages = Math.max(1, Math.ceil(s.total / size));
    setValue(el, $el, String(Math.min(s.page, pages)));
    pgRender(el, $el);
    $el.children(".ah-pagination-size-selector").children("select").val(String(size));
    if (user) {
      $el.trigger("change");
    }
  }

  AH.define("pagination", {
    init: function (el, $el) {
      var blocked = function () { return $el.hasClass("ah-pagination-disabled"); };
      $el.on("click" + NS, "li.ah-pagination-item", function () {
        if (!blocked()) { pgGo(el, $el, parseInt(this.getAttribute("data-page"), 10), true); }
      });
      $el.on("click" + NS, "li.ah-pagination-nav", function () {
        if (blocked() || $(this).hasClass("ah-pagination-nav-disabled")) { return; }
        var s = pgState(el);
        var t = { prev: s.page - 1, next: s.page + 1, first: 1, last: s.pages }[this.getAttribute("data-type")];
        pgGo(el, $el, t, true);
      });
      $el.on("keydown" + NS, "li.ah-pagination-item, li.ah-pagination-nav", function (e) {
        if (key(e) === "Enter" || key(e) === " ") {
          e.preventDefault();
          $(this).trigger("click");
        }
      });
      // Native change events of the inner select/input must not reach the
      // root's action as if the page had changed.
      $el.on("change" + NS, ".ah-pagination-size-select", function (e) {
        e.stopPropagation();
        pgSize(el, $el, parseInt($(this).val(), 10), true);
      });
      $el.on("change" + NS + " input" + NS, ".ah-pagination-jumper-input", function (e) {
        e.stopPropagation();
      });
      var jump = function () {
        var $in = $el.find(".ah-pagination-jumper-input");
        pgGo(el, $el, parseInt($in.val(), 10), true);
        $in.val("");
      };
      $el.on("click" + NS, ".ah-pagination-jumper-btn", jump);
      $el.on("keydown" + NS, ".ah-pagination-jumper-input", function (e) {
        if (key(e) === "Enter") {
          e.preventDefault();
          jump();
        }
      });
    },
    methods: {
      setPage: function (el, $el, p) { pgGo(el, $el, parseInt(p, 10), false); },
      next: function (el, $el) { pgGo(el, $el, pgState(el).page + 1, false); },
      prev: function (el, $el) { pgGo(el, $el, pgState(el).page - 1, false); },
      first: function (el, $el) { pgGo(el, $el, 1, false); },
      last: function (el, $el) { pgGo(el, $el, pgState(el).pages, false); },
      setPageSize: function (el, $el, n) { pgSize(el, $el, parseInt(n, 10), false); },
      setTotal: function (el, $el, n) {
        el.setAttribute("data-total", String(Math.max(0, parseInt(n, 10) || 0)));
        var s = pgState(el);
        setValue(el, $el, String(Math.min(s.page, s.pages)));
        pgRender(el, $el);
      },
      value: function (el) { return pgState(el).page; }
    }
  });

  // ------------------------------------------------------------------
  // Steps
  // ------------------------------------------------------------------

  var STEP_STATES = "ah-steps-item-pending ah-steps-item-active ah-steps-item-completed ah-steps-item-error";

  function stepItems($el) {
    return $el.children(".ah-steps-header").children(".ah-steps-item");
  }

  // Status class and indicator, the indicator from the server's template
  // (templates/steps_indicator.mustache).
  function stepIndicator($it, i, status) {
    $it.addClass("ah-steps-item-" + status);
    $it.children(".ah-steps-indicator").html(AH.tpl.steps_indicator({
      check: status === "completed", error: status === "error",
      plain: status !== "completed" && status !== "error", number: i + 1
    }));
  }

  function stepCur(el) {
    return parseInt(el.getAttribute("data-ah-value"), 10) || 0;
  }

  function stepSelect(el, $el, idx, user) {
    var $items = stepItems($el);
    var cur = stepCur(el);
    var n = $items.length;
    if (idx < 0 || idx >= n || idx === cur || $items.eq(idx).hasClass("ah-steps-item-disabled")) {
      return false;
    }
    var clickable = el.getAttribute("data-clickable") !== "false";
    $items.removeClass("ah-steps-item-selected").removeAttr("aria-current");
    $items.eq(idx).addClass("ah-steps-item-selected").attr("aria-current", "step");
    $items.each(function (i) {
      var $it = $(this);
      if ($it.attr("role") === "button") { $it.attr("tabindex", i === idx ? "0" : "-1"); }
      if ($it.hasClass("ah-steps-item-disabled") || $it.hasClass("ah-steps-item-error")) {
        return;
      }
      $it.removeClass(STEP_STATES);
      stepIndicator($it, i, i < idx ? "completed" : (i === idx ? "active" : "pending"));
    });
    $items.each(function () {
      var $it = $(this);
      $it.children(".ah-steps-connector").toggleClass("ah-steps-connector-done",
                                                      $it.hasClass("ah-steps-item-completed"));
    });
    var $panels = $el.children(".ah-steps-panels").children(".ah-steps-panel");
    $panels.removeClass("ah-steps-panel-active").eq(idx).addClass("ah-steps-panel-active");
    var $nav = $el.children(".ah-steps-nav");
    var toggle = function (action, on) {
      $nav.children("[data-action=" + action + "]").prop("disabled", !on)
        .toggleClass("ah-steps-btn-disabled", !on);
    };
    toggle("prev", clickable && idx > 0);
    toggle("next", clickable && idx < n - 1);
    setValue(el, $el, String(idx));
    if (user) {
      $el.trigger("change");
    }
    return true;
  }

  // Next non-disabled step from cur in direction dir, or cur.
  function stepMove($el, cur, dir) {
    var $items = stepItems($el);
    for (var i = cur + dir; i >= 0 && i < $items.length; i += dir) {
      if (!$items.eq(i).hasClass("ah-steps-item-disabled")) { return i; }
    }
    return cur;
  }

  AH.define("steps", {
    init: function (el, $el) {
      var $header = $el.children(".ah-steps-header");
      $header.on("click" + NS, ".ah-steps-item-clickable", function () {
        if (!$el.hasClass("ah-steps-disabled")) {
          stepSelect(el, $el, stepItems($el).index(this), true);
        }
      });
      $header.on("keydown" + NS, ".ah-steps-item-clickable", function (e) {
        var $items = stepItems($el);
        var vertical = $el.hasClass("ah-steps-vertical");
        var t = listKeys(e, $items, $items.index(this), vertical, "ah-steps-item-disabled");
        if (t === null) { return; }
        e.preventDefault();
        stepSelect(el, $el, t, true);
        $items.eq(t).trigger("focus");
      });
      $el.on("click" + NS, ".ah-steps-btn", function () {
        if (this.parentNode.parentNode !== el || this.disabled) { return; }
        var dir = this.getAttribute("data-action") === "prev" ? -1 : 1;
        stepSelect(el, $el, stepMove($el, stepCur(el), dir), true);
      });
    },
    destroy: function (el, $el) {
      $el.children(".ah-steps-header").off(NS);
    },
    methods: {
      select: function (el, $el, i) { stepSelect(el, $el, parseInt(i, 10), false); },
      next: function (el, $el) { stepSelect(el, $el, stepMove($el, stepCur(el), 1), false); },
      prev: function (el, $el) { stepSelect(el, $el, stepMove($el, stepCur(el), -1), false); },
      first: function (el, $el) { stepSelect(el, $el, 0, false); },
      last: function (el, $el) { stepSelect(el, $el, stepItems($el).length - 1, false); },
      setStatus: function (el, $el, i, status) {
        var $it = stepItems($el).eq(parseInt(i, 10));
        $it.removeClass(STEP_STATES + " ah-steps-item-disabled");
        stepIndicator($it, parseInt(i, 10), status);
        $it.children(".ah-steps-connector").toggleClass("ah-steps-connector-done", status === "completed");
      },
      value: function (el) { return stepCur(el); }
    }
  });

  // ------------------------------------------------------------------
  // Loader
  // ------------------------------------------------------------------

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
        if (key(e) === "Escape") { loaderHide(el, $el); }
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
})(window.jQuery, window.AH);
