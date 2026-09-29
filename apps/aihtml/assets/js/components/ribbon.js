/* The ribbon behaviour (designs/04-components.md), ported from sigil's
 * layout/ribbon.
 *
 * ribbon keeps the active tab's key in data-ah-value on the root, mirrors
 * it into a hidden input and fires "change" when the user switches tabs.
 * Every [data-command] inside the panels fires "ah:command" on the root
 * with the command copied into data-command (and data-pressed for
 * toggles), so Event.data.command reaches an action. Collapsed and popup
 * ribbons show a panel only while it is open (class ah-ribbon-open).
 * Methods called by the server (AH.invoke / aihtml_action:call) fire no
 * events. The ribbon builds no HTML.
 */
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;
var seq = 0;

function state(el) {
  var s = $.data(el, "ah-ribbon");
  if (!s) {
    s = { ns: ".ahribbon" + (++seq), menu: null, ro: null };
    $.data(el, "ah-ribbon", s);
  }
  return s;
}

function tabs(el) {
  return $(el).children(".ah-ribbon-tabs").find(".ah-ribbon-tab");
}

function panels(el) {
  return $(el).children(".ah-ribbon-tabs-content").children(".ah-ribbon-tab-content");
}

function inner(el) {
  return $(el).children(".ah-ribbon-tabs").children(".ah-ribbon-tabs-inner")[0];
}

function vertical(el) {
  return /\bah-ribbon-position-(left|right)\b/.test(el.className);
}

function floating(el) {
  return /\bah-ribbon-mode-(collapsed|popup)\b/.test(el.className);
}

function tabDisabled(tab) {
  return tab.classList.contains("ah-ribbon-tab-disabled") || tab.disabled;
}

function tabByKey(el, key) {
  return tabs(el).filter(function () { return this.getAttribute("data-key") === String(key); })[0];
}

function enabledTabs(el) {
  return tabs(el).filter(function () { return !tabDisabled(this); });
}

// ------------------------------------------------------------------
// Tab strip: scroll buttons and the selection token
// ------------------------------------------------------------------

function updateScroll(el) {
  var box = inner(el);
  if (!box) { return; }
  var v = vertical(el);
  var pos = v ? box.scrollTop : box.scrollLeft;
  var max = v ? box.scrollHeight - box.clientHeight : box.scrollWidth - box.clientWidth;
  var $bar = $(el).children(".ah-ribbon-tabs");
  $bar.children(".ah-ribbon-scroll-left, .ah-ribbon-scroll-up")
    .toggleClass("ah-ribbon-scroll-visible", pos > 0);
  $bar.children(".ah-ribbon-scroll-right, .ah-ribbon-scroll-down")
    .toggleClass("ah-ribbon-scroll-visible", pos < max - 1);
}

function scrollIntoView(el, tab) {
  var box = inner(el);
  if (!box || !tab) { return; }
  if (vertical(el)) {
    if (tab.offsetTop < box.scrollTop) {
      box.scrollTop = tab.offsetTop;
    } else if (tab.offsetTop + tab.offsetHeight > box.scrollTop + box.clientHeight) {
      box.scrollTop = tab.offsetTop + tab.offsetHeight - box.clientHeight;
    }
  } else if (tab.offsetLeft < box.scrollLeft) {
    box.scrollLeft = tab.offsetLeft;
  } else if (tab.offsetLeft + tab.offsetWidth > box.scrollLeft + box.clientWidth) {
    box.scrollLeft = tab.offsetLeft + tab.offsetWidth - box.clientWidth;
  }
  updateScroll(el);
}

function updateToken(el) {
  var box = inner(el);
  var tab = tabs(el).filter(".ah-ribbon-tab-selected")[0];
  var token = box && $(box).children(".ah-ribbon-selection-token")[0];
  if (!token) { return; }
  if (!tab) {
    token.style.width = token.style.height = "0px";
    return;
  }
  token.style.left = tab.offsetLeft + "px";
  token.style.top = tab.offsetTop + "px";
  token.style.width = tab.offsetWidth + "px";
  token.style.height = tab.offsetHeight + "px";
}

// ------------------------------------------------------------------
// Selecting, opening, collapsing
// ------------------------------------------------------------------

// Make the tab `key' active. Returns whether the value changed.
function select(el, key) {
  var tab = tabByKey(el, key);
  if (!tab) { return false; }
  var k = tab.getAttribute("data-key");
  var changed = el.getAttribute("data-ah-value") !== k;
  tabs(el).each(function () {
    var on = this === tab;
    this.classList.toggle("ah-ribbon-tab-selected", on);
    this.setAttribute("aria-selected", String(on));
    this.setAttribute("tabindex", on ? "0" : "-1");
  });
  panels(el).each(function () {
    this.classList.toggle("ah-ribbon-tab-content-active", this.getAttribute("data-key") === k);
  });
  el.setAttribute("data-ah-value", k);
  $(el).children("input[type=hidden]").val(k);
  updateToken(el);
  scrollIntoView(el, tab);
  return changed;
}

function isOpen(el) {
  return el.classList.contains("ah-ribbon-open");
}

function open(el) {
  if (!floating(el) || isOpen(el)) { return; }
  el.classList.add("ah-ribbon-open");
}

function close(el) {
  closeMenu(el, false);
  el.classList.remove("ah-ribbon-open");
}

function setCollapsed(el, $el, on, notify) {
  if (/\bah-ribbon-mode-popup\b/.test(el.className)) { return; }
  var was = el.classList.contains("ah-ribbon-mode-collapsed");
  if (was === on) { return; }
  close(el);
  el.classList.toggle("ah-ribbon-mode-collapsed", on);
  el.classList.toggle("ah-ribbon-mode-default", !on);
  $(el).children(".ah-ribbon-tabs").children(".ah-ribbon-collapse-btn")
    .attr("aria-expanded", String(!on))
    .attr("aria-label", on ? "Expand the ribbon" : "Collapse the ribbon");
  if (notify) { $el.trigger(on ? "ah:collapse" : "ah:expand"); }
}

function collapsible(el) {
  return el.classList.contains("ah-ribbon-collapsible");
}

// A user choosing a tab: select it, open a floating panel, fire change.
function choose(el, $el, tab, toggle) {
  if (!tab || tabDisabled(tab) || el.classList.contains("ah-ribbon-disabled")) { return; }
  var already = tab.classList.contains("ah-ribbon-tab-selected");
  var changed = select(el, tab.getAttribute("data-key"));
  if (floating(el)) {
    if (toggle && already && isOpen(el)) { close(el); } else { open(el); }
  }
  if (changed) { $el.trigger("change"); }
}

// ------------------------------------------------------------------
// Commands and dropdown menus
// ------------------------------------------------------------------

function menuItems(menu) {
  return $(menu).children(".ah-dropdown-btn-item").filter(function () { return !this.disabled; });
}

function openMenu(el, toggle, focusFirst) {
  var s = state(el);
  closeMenu(el, false);
  var menu = $(toggle).siblings(".ah-ribbon-menu")[0];
  if (!menu) { return; }
  menu.hidden = false;
  toggle.setAttribute("aria-expanded", "true");
  s.menu = { toggle: toggle, menu: menu, float: AH.float(menu, toggle, { placement: "bottom" }) };
  if (focusFirst) {
    var first = menuItems(menu)[0];
    if (first) { first.focus(); }
  }
}

function closeMenu(el, refocus) {
  var s = state(el);
  if (!s.menu) { return; }
  var m = s.menu;
  s.menu = null;
  m.float.stop();
  m.menu.hidden = true;
  m.toggle.setAttribute("aria-expanded", "false");
  if (refocus) { m.toggle.focus(); }
}

function runCommand(el, $el, node) {
  if (node.disabled || el.classList.contains("ah-ribbon-disabled")) { return; }
  var cmd = node.getAttribute("data-command");
  var pressed = null;
  if (node.hasAttribute("data-toggle")) {
    pressed = node.getAttribute("aria-pressed") !== "true";
    setPressed(el, cmd, pressed);
  }
  var inMenu = $(node).closest(".ah-ribbon-menu").length > 0;
  closeMenu(el, inMenu);
  if (floating(el)) {
    close(el);
    var tab = tabs(el).filter(".ah-ribbon-tab-selected")[0];
    if (tab && inMenu) { tab.focus(); }
  }
  el.setAttribute("data-command", cmd);
  if (pressed === null) {
    el.removeAttribute("data-pressed");
  } else {
    el.setAttribute("data-pressed", String(pressed));
  }
  $el.trigger("ah:command", [{ command: cmd, pressed: pressed }]);
}

function commandNodes(el, cmd) {
  return $(el).find("[data-command]").filter(function () {
    return this.getAttribute("data-command") === String(cmd) &&
      $(this).closest(".ah-ribbon")[0] === el;
  });
}

function setPressed(el, cmd, on) {
  commandNodes(el, cmd).filter("[data-toggle]").each(function () {
    this.setAttribute("aria-pressed", String(!!on));
    this.classList.toggle("ah-ribbon-button-pressed", !!on);
  });
}

// ------------------------------------------------------------------
// Keyboard
// ------------------------------------------------------------------

function panelFocusables(el) {
  return panels(el).filter(".ah-ribbon-tab-content-active")
    .find("button, [href], input, select, textarea, [tabindex]")
    .filter(function () {
      return !this.disabled && this.getAttribute("tabindex") !== "-1" &&
        $(this).closest("[hidden]").length === 0;
    });
}

function tabKey(el, $el, tab, e) {
  var v = vertical(el);
  var $en = enabledTabs(el);
  var i = $en.index(tab);
  var next = null;
  var prevKey = v ? "ArrowUp" : "ArrowLeft";
  var nextKey = v ? "ArrowDown" : "ArrowRight";
  var intoKey = { top: "ArrowDown", bottom: "ArrowUp", left: "ArrowRight", right: "ArrowLeft" }[
    (/\bah-ribbon-position-(\w+)\b/.exec(el.className) || [0, "top"])[1]];
  switch (e.key) {
    case prevKey: next = (i - 1 + $en.length) % $en.length; break;
    case nextKey: next = (i + 1) % $en.length; break;
    case "Home": next = 0; break;
    case "End": next = $en.length - 1; break;
    case intoKey:
      // into the panel: its first command
      e.preventDefault();
      choose(el, $el, tab, false);
      open(el);
      var f = panelFocusables(el)[0];
      if (f) { f.focus(); }
      return;
    case "Escape":
      if (isOpen(el)) { e.preventDefault(); close(el); }
      return;
    default: return;
  }
  e.preventDefault();
  var t = $en[next];
  if (t) {
    t.focus();
    choose(el, $el, t, false);
  }
}

function menuKey(el, item, e) {
  var $items = menuItems($(item).closest(".ah-ribbon-menu")[0]);
  var i = $items.index(item);
  var next = null;
  switch (e.key) {
    case "ArrowDown": next = (i + 1) % $items.length; break;
    case "ArrowUp": next = (i - 1 + $items.length) % $items.length; break;
    case "Home": next = 0; break;
    case "End": next = $items.length - 1; break;
    case "Escape": e.preventDefault(); e.stopPropagation(); closeMenu(el, true); return;
    case "Tab": closeMenu(el, false); return;
    default: return;
  }
  e.preventDefault();
  if ($items[next]) { $items[next].focus(); }
}

// ------------------------------------------------------------------
// Behaviour
// ------------------------------------------------------------------

AH.define("ribbon", {
  init: function (el, $el) {
    var s = state(el);
    $el.on("click" + NS, ".ah-ribbon-tab", function () {
      if ($(this).closest(".ah-ribbon")[0] !== el) { return; }
      choose(el, $el, this, true);
    });
    $el.on("mouseenter" + NS, ".ah-ribbon-tab", function () {
      if (el.getAttribute("data-selection-mode") === "hover" &&
          $(this).closest(".ah-ribbon")[0] === el) {
        choose(el, $el, this, false);
      }
    });
    $el.on("dblclick" + NS, ".ah-ribbon-tab", function () {
      if (collapsible(el) && $(this).closest(".ah-ribbon")[0] === el) {
        setCollapsed(el, $el, !el.classList.contains("ah-ribbon-mode-collapsed"), true);
      }
    });
    $el.on("keydown" + NS, ".ah-ribbon-tab", function (e) {
      if ($(this).closest(".ah-ribbon")[0] === el) { tabKey(el, $el, this, e); }
    });
    $el.on("click" + NS, ".ah-ribbon-scroll-btn", function (e) {
      e.preventDefault();
      var box = inner(el);
      var dir = this.getAttribute("data-scroll-direction");
      var amount = vertical(el) ? 50 : 100;
      var sign = (dir === "left" || dir === "up") ? -1 : 1;
      if (vertical(el)) { box.scrollTop += sign * amount; } else { box.scrollLeft += sign * amount; }
      updateScroll(el);
    });
    $el.on("click" + NS, ".ah-ribbon-collapse-btn", function () {
      setCollapsed(el, $el, !el.classList.contains("ah-ribbon-mode-collapsed"), true);
    });
    $el.on("click" + NS, ".ah-ribbon-dropdown-toggle", function () {
      var s2 = state(el);
      if (s2.menu && s2.menu.toggle === this) { closeMenu(el, false); } else { openMenu(el, this, false); }
    });
    $el.on("keydown" + NS, ".ah-ribbon-dropdown-toggle", function (e) {
      if (e.key === "ArrowDown" || e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        openMenu(el, this, true);
      }
    });
    $el.on("keydown" + NS, ".ah-ribbon-menu .ah-dropdown-btn-item", function (e) {
      menuKey(el, this, e);
    });
    $el.on("click" + NS, "[data-command]", function (e) {
      if ($(this).closest(".ah-ribbon")[0] !== el) { return; }
      e.preventDefault();
      runCommand(el, $el, this);
    });
    $el.on("keydown" + NS, function (e) {
      if (e.key === "F1" && (e.ctrlKey || e.metaKey) && collapsible(el)) {
        e.preventDefault();
        setCollapsed(el, $el, !el.classList.contains("ah-ribbon-mode-collapsed"), true);
      } else if (e.key === "Escape" && isOpen(el)) {
        close(el);
        var t = tabs(el).filter(".ah-ribbon-tab-selected")[0];
        if (t) { t.focus(); }
      }
    });
    // scroll does not bubble
    $(inner(el)).on("scroll" + NS, function () { updateScroll(el); });
    $(document).on("pointerdown" + s.ns, function (e) {
      var m = state(el).menu;
      if (m && !$.contains(m.menu, e.target) && !$.contains(m.toggle, e.target) &&
          m.toggle !== e.target) {
        closeMenu(el, false);
      }
      if (isOpen(el) && !$.contains(el, e.target)) { close(el); }
    });
    if (typeof ResizeObserver !== "undefined") {
      s.ro = new ResizeObserver(function () { updateScroll(el); updateToken(el); });
      s.ro.observe(inner(el));
    } else {
      $(window).on("resize" + s.ns, function () { updateScroll(el); updateToken(el); });
    }
    updateScroll(el);
    updateToken(el);
  },
  destroy: function (el) {
    var s = state(el);
    closeMenu(el, false);
    $(document).off(s.ns);
    $(window).off(s.ns);
    $(inner(el)).off(NS);
    if (s.ro) { s.ro.disconnect(); }
    $.removeData(el, "ah-ribbon");
  },
  methods: {
    select: function (el, $el, key) { select(el, key); },
    getValue: function (el) { return el.getAttribute("data-ah-value"); },
    enableTab: function (el, $el, key) {
      var t = tabByKey(el, key);
      if (!t) { return; }
      t.classList.remove("ah-ribbon-tab-disabled");
      t.disabled = el.classList.contains("ah-ribbon-disabled");
      t.removeAttribute("aria-disabled");
    },
    disableTab: function (el, $el, key) {
      var t = tabByKey(el, key);
      if (!t) { return; }
      t.classList.add("ah-ribbon-tab-disabled");
      t.disabled = true;
      t.setAttribute("aria-disabled", "true");
    },
    enableCommand: function (el, $el, cmd) { commandNodes(el, cmd).prop("disabled", false); },
    disableCommand: function (el, $el, cmd) { commandNodes(el, cmd).prop("disabled", true); },
    setPressed: function (el, $el, cmd, on) { setPressed(el, cmd, on); },
    collapse: function (el, $el) { setCollapsed(el, $el, true, false); },
    expand: function (el, $el) { setCollapsed(el, $el, false, false); },
    close: function (el) { close(el); }
  }
});
