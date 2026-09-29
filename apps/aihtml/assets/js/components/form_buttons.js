/* Behaviours of the form_buttons components (designs/04-components.md).
 *
 *   toggle-button      click toggles aria-pressed / data-ah-value, fires change
 *   button-group       radio / checkbox selection (arrow keys in radio mode),
 *                      a short pressed flash in the default mode
 *   segmented-control  single selection, arrow keys move and select
 *   dropdown-button    menu popup: outside click / Escape close, arrow keys
 *   split-button       main action + menu; arrow and menu clicks do not
 *                      reach the root, so on(click) there is the main action
 *
 * Value-bearing roots keep data-ah-value and the hidden input
 * (input[data-ah-input]) in step and fire "change" on the root.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  // Set the value of a value-bearing root; fire change when asked.
  function setValue($el, v, fire) {
    v = v == null ? "" : String(v);
    var old = $el.attr("data-ah-value");
    $el.attr("data-ah-value", v);
    $el.children("input[data-ah-input]").val(v);
    if (fire && old !== v) {
      $el.trigger("change", [v]);
    }
  }

  // Namespace for the document-level handlers of one instance.
  function docNS(el) {
    var ns = $.data(el, "ah-docns");
    if (!ns) {
      ns = NS + "m" + (++seq);
      $.data(el, "ah-docns", ns);
    }
    return ns;
  }

  // Move focus among enabled elements: key -> new index, or -1.
  function step(key, idx, len) {
    switch (key) {
      case "ArrowRight": case "ArrowDown": return (idx + 1) % len;
      case "ArrowLeft": case "ArrowUp": return (idx - 1 + len) % len;
      case "Home": return 0;
      case "End": return len - 1;
      default: return -1;
    }
  }

  // ------------------------------------------------------------------
  // toggle-button
  // ------------------------------------------------------------------

  function setPressed(el, $el, on, fire) {
    var v = on ? "true" : "false";
    $el.toggleClass("ah-btn-toggled", on).attr("aria-pressed", v).val(v);
    setValue($el, v, fire);
  }

  AH.define("toggle-button", {
    init: function (el, $el) {
      $el.on("click" + NS, function () {
        if (el.disabled) { return; }
        setPressed(el, $el, $el.attr("aria-pressed") !== "true", true);
      });
    },
    methods: {
      toggle: function (el, $el) { setPressed(el, $el, $el.attr("aria-pressed") !== "true", false); },
      setValue: function (el, $el, v) { setPressed(el, $el, v === true || v === "true", false); },
      getValue: function (el, $el) { return $el.attr("aria-pressed") === "true"; }
    }
  });

  // ------------------------------------------------------------------
  // button-group
  // ------------------------------------------------------------------

  function groupMode($el) {
    return $el.hasClass("ah-btn-group-radio") ? "radio"
      : $el.hasClass("ah-btn-group-checkbox") ? "checkbox" : "default";
  }

  function groupButtons($el) {
    return $el.children(".ah-btn-group-btn");
  }

  function groupSelect($el, $btn, on) {
    $btn.toggleClass("ah-btn-group-btn-selected", on);
    if (groupMode($el) === "radio") {
      $btn.attr({ "aria-checked": String(on), tabindex: on ? "0" : "-1" });
    } else {
      $btn.attr("aria-pressed", String(on));
    }
  }

  function groupSync($el, fire) {
    var vals = groupButtons($el).filter(".ah-btn-group-btn-selected").map(function () {
      return this.getAttribute("data-value");
    }).get();
    setValue($el, vals.join(","), fire);
  }

  function groupSet($el, values) {
    var set = {};
    $.each(values, function (_, v) { set[String(v)] = true; });
    groupButtons($el).each(function () {
      groupSelect($el, $(this), !!set[this.getAttribute("data-value")]);
    });
    if (groupMode($el) === "radio" && !groupButtons($el).filter("[tabindex=0]").length) {
      groupButtons($el).not(":disabled").first().attr("tabindex", "0");
    }
    groupSync($el, false);
  }

  function groupClick($el, $btn) {
    switch (groupMode($el)) {
      case "radio":
        groupButtons($el).each(function () { groupSelect($el, $(this), this === $btn[0]); });
        groupSync($el, true);
        break;
      case "checkbox":
        groupSelect($el, $btn, !$btn.hasClass("ah-btn-group-btn-selected"));
        groupSync($el, true);
        break;
      default:
        $btn.addClass("ah-btn-group-btn-pressed");
        setTimeout(function () { $btn.removeClass("ah-btn-group-btn-pressed"); }, 150);
    }
  }

  AH.define("button-group", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-btn-group-btn", function () {
        if (this.disabled || $el.hasClass("ah-btn-group-disabled")) { return; }
        groupClick($el, $(this));
      });
      $el.on("mouseenter" + NS, ".ah-btn-group-btn", function () {
        if (!this.disabled) { $(this).addClass("ah-btn-group-btn-hover"); }
      });
      $el.on("mouseleave" + NS, ".ah-btn-group-btn", function () {
        $(this).removeClass("ah-btn-group-btn-hover");
      });
      // Radio mode is a radiogroup: arrows move focus and select.
      $el.on("keydown" + NS, ".ah-btn-group-btn", function (e) {
        if (groupMode($el) !== "radio") { return; }
        var $btns = groupButtons($el).not(":disabled");
        var i = step(e.key, $btns.index(this), $btns.length);
        if (i < 0) { return; }
        e.preventDefault();
        var $to = $btns.eq(i);
        $to.trigger("focus");
        groupClick($el, $to);
      });
    },
    methods: {
      setValue: function (el, $el, v) {
        groupSet($el, Array.isArray(v) ? v : String(v == null ? "" : v).split(",").filter(Boolean));
      },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); },
      clear: function (el, $el) { groupSet($el, []); }
    }
  });

  // ------------------------------------------------------------------
  // segmented-control
  // ------------------------------------------------------------------

  function segItems($el) {
    return $el.children(".ah-segmented-control__item");
  }

  function segSet($el, v, fire) {
    v = String(v == null ? "" : v);
    var any = false;
    segItems($el).each(function () {
      var on = this.getAttribute("data-value") === v;
      any = any || on;
      $(this).attr({ "data-state": on ? "active" : "inactive", "aria-selected": String(on),
                     tabindex: on ? "0" : "-1" });
    });
    if (!any) {
      segItems($el).not(":disabled").first().attr("tabindex", "0");
    }
    setValue($el, v, fire);
  }

  AH.define("segmented-control", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-segmented-control__item", function () {
        if (this.disabled || $el.attr("data-disabled") === "true"
            || this.getAttribute("data-disabled") === "true") { return; }
        segSet($el, this.getAttribute("data-value"), true);
      });
      $el.on("keydown" + NS, ".ah-segmented-control__item", function (e) {
        var $items = segItems($el).not(":disabled");
        var i = step(e.key, $items.index(this), $items.length);
        if (i < 0) { return; }
        e.preventDefault();
        var $to = $items.eq(i);
        $to.trigger("focus");
        segSet($el, $to.attr("data-value"), true);
      });
    },
    methods: {
      setValue: function (el, $el, v) { segSet($el, v, false); },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); }
    }
  });

  // ------------------------------------------------------------------
  // Menus shared by dropdown-button and split-button
  // ------------------------------------------------------------------
  //
  // cfg: {trigger, menu, item, isOpen($el), show($el, on), select($el, $item)}

  function menuItems($el, cfg) {
    return $el.find(cfg.menu).first().find(cfg.item).filter(function () {
      return !this.disabled && this.getAttribute("data-disabled") !== "true";
    });
  }

  function menuOpen($el, cfg, focus) {
    if (cfg.disabled($el)) { return; }
    if (!cfg.isOpen($el)) {
      cfg.show($el, true);
      $el.find(cfg.trigger).attr("aria-expanded", "true");
      $el.trigger("ah:open");
    }
    if (focus) {
      var $items = menuItems($el, cfg);
      (focus === "last" ? $items.last() : $items.first()).trigger("focus");
    }
  }

  function menuClose($el, cfg, refocus) {
    if (!cfg.isOpen($el)) { return; }
    cfg.show($el, false);
    $el.find(cfg.trigger).attr("aria-expanded", "false");
    $el.trigger("ah:close");
    if (refocus) { $el.find(cfg.trigger).trigger("focus"); }
  }

  function menuChoose($el, cfg, $item) {
    if ($item[0].disabled || $item.attr("data-disabled") === "true") { return; }
    cfg.select($el, $item);
    menuClose($el, cfg, true);
    // A menu is a command: choosing the same item again fires again.
    var v = $item.attr("data-value");
    setValue($el, v, false);
    $el.trigger("change", [v]);
  }

  function menuInit(el, $el, cfg) {
    var ns = docNS(el);
    $el.on("click" + NS, cfg.trigger, function () {
      if (cfg.isOpen($el)) { menuClose($el, cfg, false); } else { menuOpen($el, cfg, false); }
    });
    $el.on("click" + NS, cfg.item, function () {
      menuChoose($el, cfg, $(this));
    });
    $el.on("keydown" + NS, function (e) {
      var inMenu = $(e.target).closest(cfg.menu).length > 0;
      var onTrigger = $(e.target).closest(cfg.trigger).length > 0;
      if (e.key === "Escape") {
        if (cfg.isOpen($el)) { e.preventDefault(); menuClose($el, cfg, true); }
      } else if (e.key === "Tab") {
        menuClose($el, cfg, false);
      } else if (onTrigger && (e.key === "ArrowDown" || e.key === "ArrowUp")) {
        e.preventDefault();
        if (e.altKey && e.key === "ArrowUp") { menuClose($el, cfg, false); return; }
        menuOpen($el, cfg, e.altKey ? false : (e.key === "ArrowUp" ? "last" : "first"));
      } else if (inMenu) {
        var $items = menuItems($el, cfg);
        var i = step(e.key, $items.index(e.target), $items.length);
        if (i >= 0 && e.key !== "ArrowLeft" && e.key !== "ArrowRight") {
          e.preventDefault();
          $items.eq(i).trigger("focus");
        }
      }
    });
    $(document).on("mousedown" + ns, function (e) {
      if (cfg.isOpen($el) && !el.contains(e.target)) { menuClose($el, cfg, false); }
    });
  }

  function menuDestroy(el) {
    $(document).off($.data(el, "ah-docns"));
    unfloat(el, 0);
  }

  // Popups are pinned with AH.float (position: fixed), so an ancestor with
  // overflow: hidden cannot clip them. The handle lives on the root.
  function floatMenu(el, menu, opts) {
    clearTimeout($.data(el, "ah-unfloat"));
    var h = $.data(el, "ah-float");
    if (h) { h.update(); return; }
    $.data(el, "ah-float", AH.float(menu, el, opts));
  }

  // delay: let a closing fade finish before the menu drops back in place
  function unfloat(el, delay) {
    clearTimeout($.data(el, "ah-unfloat"));
    var stop = function () {
      var h = $.data(el, "ah-float");
      if (h) { h.stop(); $.removeData(el, "ah-float"); }
    };
    if (delay) { $.data(el, "ah-unfloat", setTimeout(stop, delay)); } else { stop(); }
  }

  // ------------------------------------------------------------------
  // dropdown-button
  // ------------------------------------------------------------------

  var DROPDOWN = {
    trigger: ".ah-dropdown-btn-wrapper",
    menu: ".ah-dropdown-btn-popup",
    item: ".ah-dropdown-btn-item",
    disabled: function ($el) { return $el.hasClass("ah-dropdown-btn-disabled"); },
    isOpen: function ($el) { return $el.hasClass("ah-dropdown-btn-opened"); },
    show: function ($el, on) {
      var $popup = $el.children(".ah-dropdown-btn-popup");
      $el.toggleClass("ah-dropdown-btn-opened", on);
      if (on) {
        $popup.removeAttr("hidden");
        floatMenu($el[0], $popup[0], { placement: "bottom", align: "start", matchWidth: true });
      } else {
        $popup.attr("hidden", "");
        unfloat($el[0], 0);
      }
    },
    select: function ($el, $item) {
      $el.find(".ah-dropdown-btn-item").removeClass("selected");
      $item.addClass("selected");
    }
  };

  AH.define("dropdown-button", {
    init: function (el, $el) {
      menuInit(el, $el, DROPDOWN);
      var $trigger = $el.children(".ah-dropdown-btn-wrapper");
      $el.on("mouseenter" + NS, function () {
        if (DROPDOWN.disabled($el)) { return; }
        $el.addClass("ah-dropdown-btn-hover");
        if ($el.hasClass("ah-dropdown-btn-auto-open")) { menuOpen($el, DROPDOWN, false); }
      });
      $el.on("mouseleave" + NS, function () {
        $el.removeClass("ah-dropdown-btn-hover");
        if ($el.hasClass("ah-dropdown-btn-auto-open")) { menuClose($el, DROPDOWN, false); }
      });
      $trigger.on("focus" + NS, function () {
        // sigil shows the focus ring on focus; keep it to keyboard focus
        var visible = true;
        try { visible = $trigger[0].matches(":focus-visible"); } catch (err) { /* old browser */ }
        if (visible) { $el.addClass("ah-dropdown-btn-focused"); }
      });
      $trigger.on("blur" + NS, function () { $el.removeClass("ah-dropdown-btn-focused"); });
    },
    destroy: function (el, $el) {
      menuDestroy(el);
      $el.children(".ah-dropdown-btn-wrapper").off(NS);
    },
    methods: {
      open: function (el, $el) { menuOpen($el, DROPDOWN, false); },
      close: function (el, $el) { menuClose($el, DROPDOWN, false); },
      toggle: function (el, $el) {
        if (DROPDOWN.isOpen($el)) { menuClose($el, DROPDOWN, false); } else { menuOpen($el, DROPDOWN, false); }
      },
      setValue: function (el, $el, v) {
        var $item = $el.find(".ah-dropdown-btn-item").filter(function () {
          return this.getAttribute("data-value") === String(v);
        });
        $el.find(".ah-dropdown-btn-item").removeClass("selected");
        $item.addClass("selected");
        setValue($el, v, false);
      },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); }
    }
  });

  // ------------------------------------------------------------------
  // split-button
  // ------------------------------------------------------------------

  var SPLIT = {
    trigger: ".ah-split-button__arrow",
    menu: ".ah-split-button__menu",
    item: ".ah-split-button__item",
    disabled: function ($el) { return $el.attr("data-disabled") === "true"; },
    isOpen: function ($el) { return $el.attr("data-open") === "true"; },
    show: function ($el, on) {
      $el.attr("data-open", on ? "true" : "false");
      if (on) {
        floatMenu($el[0], $el.children(".ah-split-button__menu")[0],
                  { placement: "bottom",
                    align: $el.attr("data-menu-align") === "start" ? "start" : "end" });
      } else {
        unfloat($el[0], 150);
      }
    },
    select: function () {}
  };

  AH.define("split-button", {
    init: function (el, $el) {
      menuInit(el, $el, SPLIT);
      // Only the main half's clicks reach the root (and its on(click)).
      $el.on("click" + NS, ".ah-split-button__arrow, .ah-split-button__menu", function (e) {
        e.stopPropagation();
      });
    },
    destroy: function (el) { menuDestroy(el); },
    methods: {
      open: function (el, $el) { menuOpen($el, SPLIT, false); },
      close: function (el, $el) { menuClose($el, SPLIT, false); },
      setValue: function (el, $el, v) { setValue($el, v, false); },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); }
    }
  });
})(window.jQuery, window.AH);
