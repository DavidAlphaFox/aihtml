/* Shared by the button components (toggle_button.js, button_group.js,
 * segmented_control.js, dropdown_button.js, split_button.js): the value of
 * a value-bearing root, arrow-key stepping and the menus of
 * dropdown-button and split-button.
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

  AH.lib = AH.lib || {};
  AH.lib.button = {
    setValue: setValue, step: step,
    menuOpen: menuOpen, menuClose: menuClose, menuInit: menuInit, menuDestroy: menuDestroy,
    floatMenu: floatMenu, unfloat: unfloat
  };
})(window.jQuery, window.AH);
