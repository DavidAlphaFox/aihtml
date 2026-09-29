/* Behaviour of the toolbar component (designs/04-components.md), ported
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
