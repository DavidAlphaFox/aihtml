/* Shared by the button components (toggle_button.js, button_group.js,
 * segmented_control.js, dropdown_button.js, split_button.js): the value of
 * a value-bearing root, arrow-key stepping and the menus of
 * dropdown-button and split-button.
 *
 * Value-bearing roots keep data-ah-value and the hidden input
 * (input[data-ah-input]) in step and fire "change" on the root (a native
 * CustomEvent, detail: the new value).
 */
import AH from "../core.js";

function emit(el, type, detail) {
  return el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
}

// Set the value of a value-bearing root; fire change when asked.
function setValue(el, v, fire) {
  v = v == null ? "" : String(v);
  var old = el.getAttribute("data-ah-value");
  el.setAttribute("data-ah-value", v);
  var hidden = el.querySelector(":scope > input[data-ah-input]");
  if (hidden) { hidden.value = v; }
  if (fire && old !== v) {
    emit(el, "change", v);
  }
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
// cfg: {trigger, menu, item, disabled(el), isOpen(el), show(el, on),
//       select(el, item)}; "ah:open" / "ah:close" on the root (no detail),
// "change" (detail: the item's data-value) on every choice.

function menuItems(el, cfg) {
  var menu = el.querySelector(cfg.menu);
  if (!menu) { return []; }
  return Array.from(menu.querySelectorAll(cfg.item)).filter(function (it) {
    return !it.disabled && it.getAttribute("data-disabled") !== "true";
  });
}

function triggers(el, cfg) { return el.querySelectorAll(cfg.trigger); }

function menuOpen(el, cfg, focus) {
  if (cfg.disabled(el)) { return; }
  if (!cfg.isOpen(el)) {
    cfg.show(el, true);
    triggers(el, cfg).forEach(function (t) { t.setAttribute("aria-expanded", "true"); });
    emit(el, "ah:open");
  }
  if (focus) {
    var items = menuItems(el, cfg);
    var it = focus === "last" ? items[items.length - 1] : items[0];
    if (it) { it.focus(); }
  }
}

function menuClose(el, cfg, refocus) {
  if (!cfg.isOpen(el)) { return; }
  cfg.show(el, false);
  triggers(el, cfg).forEach(function (t) { t.setAttribute("aria-expanded", "false"); });
  emit(el, "ah:close");
  if (refocus) {
    var t = el.querySelector(cfg.trigger);
    if (t) { t.focus(); }
  }
}

function menuChoose(el, cfg, item) {
  if (item.disabled || item.getAttribute("data-disabled") === "true") { return; }
  cfg.select(el, item);
  menuClose(el, cfg, true);
  // A menu is a command: choosing the same item again fires again.
  var v = item.getAttribute("data-value");
  setValue(el, v, false);
  emit(el, "change", v == null ? "" : v);
}

// ctrl: the AH.Controller of the root (its listeners go with it).
function menuInit(ctrl, cfg) {
  var el = ctrl.element;
  // On the trigger and the menu themselves, so a component can keep
  // these clicks from the root (split-button) and still handle them.
  el.querySelectorAll(cfg.trigger).forEach(function (t) {
    ctrl.listen(t, "click", function () {
      if (cfg.isOpen(el)) { menuClose(el, cfg, false); } else { menuOpen(el, cfg, false); }
    });
  });
  var menu = el.querySelector(cfg.menu);
  if (menu) {
    ctrl.delegate("click", cfg.item, function (e, item) {
      menuChoose(el, cfg, item);
    }, menu);
  }
  ctrl.listen(el, "keydown", function (e) {
    var t = e.target;
    var inMenu = !!(t.closest && t.closest(cfg.menu));
    var onTrigger = !!(t.closest && t.closest(cfg.trigger));
    if (e.key === "Escape") {
      if (cfg.isOpen(el)) { e.preventDefault(); menuClose(el, cfg, true); }
    } else if (e.key === "Tab") {
      menuClose(el, cfg, false);
    } else if (onTrigger && (e.key === "ArrowDown" || e.key === "ArrowUp")) {
      e.preventDefault();
      if (e.altKey && e.key === "ArrowUp") { menuClose(el, cfg, false); return; }
      menuOpen(el, cfg, e.altKey ? false : (e.key === "ArrowUp" ? "last" : "first"));
    } else if (inMenu) {
      var items = menuItems(el, cfg);
      var i = step(e.key, items.indexOf(t), items.length);
      if (i >= 0 && e.key !== "ArrowLeft" && e.key !== "ArrowRight") {
        e.preventDefault();
        items[i].focus();
      }
    }
  });
  ctrl.listen(document, "mousedown", function (e) {
    if (cfg.isOpen(el) && !el.contains(e.target)) { menuClose(el, cfg, false); }
  });
}

function menuDestroy(el) {
  unfloat(el, 0);
}

// Popups are pinned with AH.float (position: fixed), so an ancestor with
// overflow: hidden cannot clip them. The handle lives with the root.
var floats = new WeakMap();

function floatMenu(el, menu, opts) {
  var f = floats.get(el) || {};
  clearTimeout(f.timer);
  if (f.handle) { f.handle.update(); return; }
  floats.set(el, { handle: AH.float(menu, el, opts) });
}

// delay: let a closing fade finish before the menu drops back in place
function unfloat(el, delay) {
  var f = floats.get(el);
  if (!f) { return; }
  clearTimeout(f.timer);
  var stop = function () {
    if (f.handle) { f.handle.stop(); }
    floats.delete(el);
  };
  if (delay) { f.timer = setTimeout(stop, delay); } else { stop(); }
}

AH.lib = AH.lib || {};
AH.lib.button = {
  setValue: setValue, step: step,
  menuOpen: menuOpen, menuClose: menuClose, menuInit: menuInit, menuDestroy: menuDestroy,
  floatMenu: floatMenu, unfloat: unfloat
};
