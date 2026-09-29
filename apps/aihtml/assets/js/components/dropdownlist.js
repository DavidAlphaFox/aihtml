/* Behaviour of dropdownlist (designs/04-components.md). Ported from sigil
   (sigil.components.form.{dropdownlist, listbox}). */
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;
var seq = 0;

function trim(s) { return String(s === null || s === undefined ? "" : s).trim(); }

function uid(el, prefix) {
  if (!el.id) { el.id = prefix + (++seq); }
  return el.id;
}

// Value-bearing contract: data-ah-value + hidden input (change/input
// are fired by the caller).
function writeValue($el, value) {
  $el.attr("data-ah-value", value);
  $el.children("input[type=hidden]").val(value);
}

// ------------------------------------------------------------------
// dropdownlist
// ------------------------------------------------------------------
//
// Keyboard (combobox pattern, extending sigil's Enter/Space/Escape/F4/
// Alt+arrows with listbox navigation):
//   closed: ArrowDown/ArrowUp/Enter/Space/F4/Alt+Arrow open, a letter
//           selects the next item starting with it
//   open:   ArrowUp/Down/Home/End/PageUp/PageDown move, Enter/Space
//           select, Escape/Alt+Arrow/F4 close, Tab closes, letters
//           type-ahead (or filter, when filterable)

function ddState(el) {
  var s = $.data(el, "ah-dd");
  if (!s) {
    s = { open: false, search: "", searchAt: 0 };
    $.data(el, "ah-dd", s);
  }
  return s;
}

function ddItems($el, visibleOnly) {
  var $items = $el.find(".ah-listbox-item");
  return visibleOnly
    ? $items.filter(function () { return this.style.display !== "none"; })
    : $items;
}

function ddEnabled($items) {
  return $items.filter(function () {
    return !$(this).hasClass("ah-listbox-item-disabled");
  });
}

function ddDisabled($el) {
  return $el.hasClass("ah-dropdownlist-disabled");
}

function ddSetActive($el, item) {
  var $items = ddItems($el);
  $items.removeClass("ah-listbox-item-focused");
  var $filter = $el.find(".ah-listbox-filter-input");
  if (!item) {
    $el.removeAttr("aria-activedescendant");
    $filter.removeAttr("aria-activedescendant");
    return;
  }
  $(item).addClass("ah-listbox-item-focused");
  $el.attr("aria-activedescendant", item.id);
  $filter.attr("aria-activedescendant", item.id);
  if (item.scrollIntoView) { item.scrollIntoView({ block: "nearest" }); }
}

function ddActive($el) {
  return $el.find(".ah-listbox-item-focused")[0] || null;
}

// sigil's -above / -below classes, from the side AH.float chose.
function ddPlacement($el) {
  var $popup = $el.children(".ah-dropdownlist-popup");
  var above = $popup.attr("data-ah-placement") === "top";
  $popup.toggleClass("ah-dropdownlist-popup-above", above)
    .toggleClass("ah-dropdownlist-popup-below", !above);
}

function ddOpen(el, $el) {
  var s = ddState(el);
  if (s.open || ddDisabled($el)) { return; }
  s.open = true;
  $el.addClass("ah-dropdownlist-open ah-dropdownlist-state-selected")
    .attr("aria-expanded", "true");
  var $popup = $el.children(".ah-dropdownlist-popup").addClass("ah-dropdownlist-popup-open");
  // Shared positioning: fixed, at least the root's width, flips above
  // when there is no room below, follows scroll and resize.
  s.float = AH.float($popup[0], el, { placement: "bottom", align: "start", offset: 4,
                                      matchWidth: true });
  ddPlacement($el);
  var $sel = ddItems($el).filter(".ah-listbox-item-selected");
  ddSetActive($el, $sel[0] || ddEnabled(ddItems($el, true))[0]);
  var $filter = $el.find(".ah-listbox-filter-input");
  if ($filter.length) { $filter.trigger("focus"); }
  $el.trigger("ah:open");
}

function ddClose(el, $el, refocus) {
  var s = ddState(el);
  if (!s.open) { return; }
  s.open = false;
  $el.removeClass("ah-dropdownlist-open ah-dropdownlist-state-selected")
    .attr("aria-expanded", "false");
  if (s.float) { s.float.stop(); s.float = null; }
  $el.children(".ah-dropdownlist-popup")
    .removeClass("ah-dropdownlist-popup-open ah-dropdownlist-popup-above ah-dropdownlist-popup-below");
  ddSetActive($el, null);
  var $filter = $el.find(".ah-listbox-filter-input");
  if ($filter.length && $filter.val()) {
    $filter.val("");
    ddFilter($el, "");
  }
  if (refocus && el.contains(document.activeElement) && document.activeElement !== el) {
    el.focus();
  }
  $el.trigger("ah:close");
}

function ddLabel(item) {
  var l = $(item).find(".ah-listbox-label")[0];
  return trim((l || item).textContent);
}

// Select an item (null clears). Fires change when the value changes.
function ddSelect(el, $el, item, silent) {
  var value = item ? item.getAttribute("data-value") : "";
  var old = $el.attr("data-ah-value") || "";
  var $items = ddItems($el);
  $items.removeClass("ah-listbox-item-selected").attr("aria-selected", "false");
  var $content = $el.find(".ah-dropdownlist-content");
  if (item) {
    $(item).addClass("ah-listbox-item-selected").attr("aria-selected", "true");
    $content.text(ddLabel(item)).removeClass("ah-dropdownlist-content-placeholder");
  } else {
    $content.text($el.attr("data-ah-placeholder") || "")
      .addClass("ah-dropdownlist-content-placeholder");
  }
  writeValue($el, value);
  if (!silent && value !== old) {
    $el.trigger("change", [{ value: value, label: item ? ddLabel(item) : null }]);
  }
}

function ddFilter($el, text) {
  var q = trim(text).toLowerCase();
  ddItems($el).each(function () {
    this.style.display = !q || ddLabel(this).toLowerCase().indexOf(q) >= 0 ? "" : "none";
  });
  $el.find(".ah-listbox-group").each(function () {
    var any = $(this).nextUntil(".ah-listbox-group").filter(function () {
      return this.style.display !== "none";
    }).length > 0;
    this.style.display = any ? "" : "none";
  });
  ddSetActive($el, ddEnabled(ddItems($el, true))[0]);
  var s = $.data($el[0], "ah-dd");
  if (s && s.float) { s.float.update(); ddPlacement($el); }
}

// Type-ahead: letters typed within 800 ms form one prefix (sigil's
// incremental search). Returns the matching item after the current one.
function ddTypeahead(el, $el, key) {
  var s = ddState(el);
  var now = Date.now();
  s.search = now - s.searchAt > 800 ? key : s.search + key;
  s.searchAt = now;
  var q = s.search.toLowerCase();
  var items = ddEnabled(ddItems($el, true)).get();
  var cur = s.open ? ddActive($el) : ddItems($el).filter(".ah-listbox-item-selected")[0];
  var start = s.search.length === 1 ? items.indexOf(cur) + 1 : Math.max(items.indexOf(cur), 0);
  for (var i = 0; i < items.length; i++) {
    var it = items[(start + i) % items.length];
    if (ddLabel(it).toLowerCase().indexOf(q) === 0) { return it; }
  }
  return null;
}

function ddKeydown(el, $el, e) {
  if (ddDisabled($el)) { return; }
  var s = ddState(el);
  var key = e.key;
  var inFilter = $(e.target).hasClass("ah-listbox-filter-input");
  var toggle = key === "F4" || (e.altKey && (key === "ArrowDown" || key === "ArrowUp"));
  if (!s.open) {
    if (toggle || key === "ArrowDown" || key === "ArrowUp" || key === "Enter" || key === " ") {
      e.preventDefault();
      ddOpen(el, $el);
    } else if (key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
      var hit = ddTypeahead(el, $el, key);
      if (hit) { ddSelect(el, $el, hit); }
    }
    return;
  }
  var items = ddEnabled(ddItems($el, true)).get();
  var idx = items.indexOf(ddActive($el));
  var move = function (i) {
    e.preventDefault();
    if (items.length) { ddSetActive($el, items[Math.max(0, Math.min(items.length - 1, i))]); }
  };
  if (toggle || key === "Escape") {
    e.preventDefault();
    ddClose(el, $el, true);
  } else if (key === "ArrowDown") {
    move(idx + 1);
  } else if (key === "ArrowUp") {
    move(idx < 0 ? items.length - 1 : idx - 1);
  } else if (key === "Home" && !inFilter) {
    move(0);
  } else if (key === "End" && !inFilter) {
    move(items.length - 1);
  } else if (key === "PageDown") {
    move(idx + 10);
  } else if (key === "PageUp") {
    move(idx - 10);
  } else if (key === "Enter" || (key === " " && !inFilter)) {
    e.preventDefault();
    if (idx >= 0) { ddSelect(el, $el, items[idx]); }
    ddClose(el, $el, true);
  } else if (key === "Tab") {
    ddClose(el, $el, false);
  } else if (!inFilter && key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
    var hit = ddTypeahead(el, $el, key);
    if (hit) { ddSetActive($el, hit); }
  }
}

AH.define("dropdownlist", {
  init: function (el, $el) {
    var id = uid(el, "ah-dd");
    var $list = $el.find(".ah-listbox-list");
    $list.attr("id", id + "-list");
    $el.attr("aria-controls", id + "-list");
    ddItems($el).each(function () {
      this.id = id + "-opt-" + this.getAttribute("data-idx");
    });
    var ns = ".ahdd" + id.replace(/[^\w-]/g, "_");
    $.data(el, "ah-dd-ns", ns);

    $el.on("click" + NS, ".ah-dropdownlist-input-area", function () {
      if (ddState(el).open) { ddClose(el, $el, true); } else { ddOpen(el, $el); }
    });
    $el.on("click" + NS, ".ah-listbox-item", function (e) {
      e.stopPropagation();
      if ($(this).hasClass("ah-listbox-item-disabled")) { return; }
      ddSelect(el, $el, this);
      ddClose(el, $el, true);
    });
    // Keep focus on the combobox while the pointer is in the popup.
    $el.on("mousedown" + NS, ".ah-dropdownlist-popup", function (e) {
      if (!$(e.target).hasClass("ah-listbox-filter-input")) { e.preventDefault(); }
    });
    $el.on("mousemove" + NS, ".ah-listbox-item", function () {
      if (!$(this).hasClass("ah-listbox-item-disabled") && ddActive($el) !== this) {
        ddSetActive($el, this);
      }
    });
    $el.on("keydown" + NS, function (e) { ddKeydown(el, $el, e); });
    $el.on("input" + NS, ".ah-listbox-filter-input", function (e) {
      e.stopPropagation();           // not the component's own input event
      ddFilter($el, this.value);
    });
    $el.on("change" + NS, ".ah-listbox-filter-input", function (e) { e.stopPropagation(); });
    $el.on("focusin" + NS, function () { $el.addClass("ah-dropdownlist-focused"); });
    $el.on("focusout" + NS, function (e) {
      if (!e.relatedTarget || !el.contains(e.relatedTarget)) {
        $el.removeClass("ah-dropdownlist-focused");
        ddClose(el, $el, false);
      }
    });
    $(document).on("mousedown" + ns, function (e) {
      if (ddState(el).open && !el.contains(e.target)) { ddClose(el, $el, false); }
    });
  },
  destroy: function (el, $el) {
    ddClose(el, $el, false);
    $(document).off($.data(el, "ah-dd-ns"));
  },
  methods: {
    open: function (el, $el) { ddOpen(el, $el); },
    close: function (el, $el) { ddClose(el, $el, false); },
    getValue: function (el, $el) { return $el.attr("data-ah-value") || ""; },
    // setValue(value[, silent]): select the item with that value
    // ("" or null clears); fires change unless silent.
    setValue: function (el, $el, value, silent) {
      var v = value === null || value === undefined ? "" : String(value);
      var item = ddItems($el).filter(function () {
        return this.getAttribute("data-value") === v;
      })[0] || null;
      ddSelect(el, $el, item, silent);
    },
    disable: function (el, $el) {
      ddClose(el, $el, false);
      $el.addClass("ah-dropdownlist-disabled").attr({ "aria-disabled": "true", tabindex: "-1" });
    },
    enable: function (el, $el) {
      $el.removeClass("ah-dropdownlist-disabled").removeAttr("aria-disabled").attr("tabindex", "0");
    }
  }
});
