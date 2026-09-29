/* Behaviour of the listmenu component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.js. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_nav.js";

var NS = AH.NS;
var L = AH.lib.nav;
var setValue = L.setValue;
var byKey = L.byKey;

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
