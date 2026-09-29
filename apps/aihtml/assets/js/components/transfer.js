/* Behaviour of transfer (designs/04-components.md), ported from sigil's
 * form/transfer. Both lists are rendered on the server (aihtml_transfer);
 * moving an item moves its node. Shared helpers: _lib_list.js. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_list.js";

var NS = AH.NS;
var LIST = AH.lib.list;
var ensureId = LIST.ensureId, publish = LIST.publish, split = LIST.split, join = LIST.join,
  shown = LIST.shown, enabled = LIST.enabled, scrollInto = LIST.scrollInto;

// ==================================================================
// transfer
// ==================================================================

function trState(el) { return $.data(el, "ah-tr"); }

function trList(st, side) { return side === "source" ? st.$source : st.$target; }
function trItems($list) { return $list.children(".ah-transfer-item").get(); }
function trRows($list) {
  return trItems($list).filter(function (li) { return shown(li) && enabled(li); });
}
function trSelected($list) {
  return trRows($list).filter(function (li) { return $(li).hasClass("ah-transfer-item-selected"); });
}

function trSelect(li, on) {
  $(li).toggleClass("ah-transfer-item-selected", on).attr("aria-selected", String(on));
}

// Counts, move buttons, filters.
function trSync(el) {
  var st = trState(el);
  ["source", "target"].forEach(function (side) {
    var $list = trList(st, side);
    var $panel = $list.closest(".ah-transfer-panel");
    $panel.find(".ah-transfer-panel-count").text(trItems($list).length);
    var q = String($panel.find(".ah-transfer-filter-input").val() || "").trim().toLowerCase();
    trItems($list).forEach(function (li) {
      var hit = !q || $(li).children(".ah-transfer-item-label").text().toLowerCase().indexOf(q) >= 0;
      li.style.display = hit ? "" : "none";
      if (!hit) { trSelect(li, false); }
    });
    var btn = st.$el.find(side === "source" ? ".ah-transfer-btn-to-target" : ".ah-transfer-btn-to-source");
    var off = st.disabled || !trSelected($list).length;
    btn.toggleClass("ah-transfer-btn-disabled", off).prop("disabled", off);
  });
}

function trPublish(el, fire) {
  var st = trState(el);
  publish(el, st.$el, join(trItems(st.$target).map(function (li) {
    return li.getAttribute("data-value");
  })), fire);
}

function idx(li) { return parseInt(li.getAttribute("data-idx"), 10) || 0; }

// Back to the source: in the order of the items.
function trInsert($list, li) {
  var after = trItems($list).filter(function (x) { return idx(x) < idx(li); }).pop();
  if (after) { $(li).insertAfter(after); } else { $list.prepend(li); }
}

function trMove(el, from) {
  var st = trState(el);
  if (st.disabled) { return; }
  var moving = trSelected(trList(st, from));
  if (!moving.length) { return; }
  var to = from === "source" ? "target" : "source";
  var $to = trList(st, to);
  moving.forEach(function (li) {
    trSelect(li, false);
    $(li).removeClass("ah-transfer-item-focused").attr("data-source", to);
    if (to === "target") { $to.append(li); } else { trInsert($to, li); }
  });
  if (st.cursor && moving.indexOf(st.cursor) >= 0) { trCursor(el, null); }
  trSync(el);
  trPublish(el, true);
}

function trCursor(el, li) {
  var st = trState(el);
  st.$el.find(".ah-transfer-item-focused").removeClass("ah-transfer-item-focused");
  st.$source.add(st.$target).removeAttr("aria-activedescendant");
  st.cursor = li || null;
  if (!li) { return; }
  $(li).addClass("ah-transfer-item-focused");
  var list = li.parentNode;
  list.setAttribute("aria-activedescendant", li.id);
  scrollInto(list.parentNode, li);
}

function trKey(el, $list, e) {
  var st = trState(el);
  if (st.disabled) { return; }
  var rows = trRows($list), side = $list.attr("data-panel");
  var i = rows.indexOf(st.cursor);
  switch (e.key) {
    case "ArrowDown": e.preventDefault(); trCursor(el, rows[Math.min(i + 1, rows.length - 1)]); break;
    case "ArrowUp": e.preventDefault(); trCursor(el, rows[Math.max(i - 1, 0)]); break;
    case "Home": e.preventDefault(); trCursor(el, rows[0]); break;
    case "End": e.preventDefault(); trCursor(el, rows[rows.length - 1]); break;
    case " ":
      e.preventDefault();
      if (st.cursor && rows.indexOf(st.cursor) >= 0) {
        trSelect(st.cursor, !$(st.cursor).hasClass("ah-transfer-item-selected"));
        trSync(el);
      }
      break;
    case "Enter":
      e.preventDefault();
      if (!trSelected($list).length && st.cursor && rows.indexOf(st.cursor) >= 0) { trSelect(st.cursor, true); }
      var next = rows.filter(function (li) { return !$(li).hasClass("ah-transfer-item-selected"); })[0];
      trMove(el, side);
      if (next) { trCursor(el, next); }
      break;
    default:
      if ((e.key === "a" || e.key === "A") && (e.ctrlKey || e.metaKey)) {
        e.preventDefault();
        rows.forEach(function (li) { trSelect(li, true); });
        trSync(el);
      }
  }
}

AH.define("transfer", {
  init: function (el, $el) {
    ensureId(el, "ah-tr");
    var st = {
      $el: $el,
      $source: $el.find(".ah-transfer-list[data-panel=source]"),
      $target: $el.find(".ah-transfer-list[data-panel=target]"),
      disabled: $el.hasClass("ah-transfer-disabled"),
      cursor: null
    };
    $.data(el, "ah-tr", st);
    $el.on("mousedown" + NS, ".ah-transfer-item", function (e) {
      if (e.shiftKey || e.detail > 1) { e.preventDefault(); }  // no text selection
    });
    $el.on("click" + NS, ".ah-transfer-item", function () {
      if (st.disabled || !enabled(this)) { return; }
      trSelect(this, !$(this).hasClass("ah-transfer-item-selected"));
      trCursor(el, this);
      trSync(el);
    });
    $el.on("dblclick" + NS, ".ah-transfer-item", function () {
      if (st.disabled || !enabled(this)) { return; }
      trSelect(this, true);
      trMove(el, this.parentNode.getAttribute("data-panel"));
    });
    $el.on("click" + NS, ".ah-transfer-btn", function (e) {
      e.preventDefault();
      trMove(el, this.getAttribute("data-direction") === "to-target" ? "source" : "target");
    });
    $el.on("keydown" + NS, ".ah-transfer-list", function (e) { trKey(el, $(this), e); });
    $el.on("focus" + NS, ".ah-transfer-list", function () {
      if (!st.cursor || st.cursor.parentNode !== this) { trCursor(el, trRows($(this))[0]); }
    });
    $el.on("input" + NS, ".ah-transfer-filter-input", function () { trSync(el); })
      .on("change" + NS, ".ah-transfer-filter-input", function (e) { e.stopPropagation(); });
    trSync(el);
  },
  methods: {
    // The keys of the right list, in order; no change event.
    setValue: function (el, $el, v) {
      var st = trState(el), keys = split(v);
      var all = trItems(st.$source).concat(trItems(st.$target));
      all.sort(function (a, b) { return idx(a) - idx(b); });
      all.forEach(function (li) { trSelect(li, false); });
      keys.forEach(function (k) {
        var li = all.filter(function (x) { return x.getAttribute("data-value") === k; })[0];
        if (li) { $(li).attr("data-source", "target"); st.$target.append(li); }
      });
      all.forEach(function (li) {
        if (keys.indexOf(li.getAttribute("data-value")) < 0) {
          $(li).attr("data-source", "source");
          st.$source.append(li);
        }
      });
      trCursor(el, null);
      trSync(el);
      trPublish(el, false);
    },
    getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
    moveToTarget: function (el) { trMove(el, "source"); },
    moveToSource: function (el) { trMove(el, "target"); },
    selectAll: function (el, $el, side) {
      trRows(trList(trState(el), side || "source")).forEach(function (li) { trSelect(li, true); });
      trSync(el);
    },
    clearSelection: function (el, $el, side) {
      trItems(trList(trState(el), side || "source")).forEach(function (li) { trSelect(li, false); });
      trSync(el);
    }
  }
});
