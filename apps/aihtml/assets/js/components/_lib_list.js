/* Shared by the list selection behaviours (cascader.js, listbox.js,
 * transfer.js, ported from sigil): ids, the value-bearing contract and
 * row helpers. All rows, columns and lists are rendered on the server;
 * the behaviours show, hide, mark and move them. */
(function ($, AH) {
  "use strict";

  var seq = 0;

  function ensureId(el, prefix) {
    if (!el.id) { el.id = prefix + (++seq); }
    return el.id;
  }

  // Value-bearing contract: data-ah-value + hidden input, then `change`.
  function publish(el, $el, value, fire) {
    var old = el.getAttribute("data-ah-value") || "";
    el.setAttribute("data-ah-value", value);
    $el.children("input[type=hidden]").val(value);
    if (fire && old !== value) { $el.trigger("change"); }
  }

  function split(v) {
    if (v == null || v === "") { return []; }
    return Array.isArray(v) ? v.map(String) : String(v).split(",");
  }

  function shown(li) { return li.style.display !== "none"; }
  function enabled(li) { return li.getAttribute("aria-disabled") !== "true"; }

  // Keep a row visible in its scrolling container.
  function scrollInto(box, item) {
    if (!box || !item) { return; }
    var top = item.getBoundingClientRect().top - box.getBoundingClientRect().top - box.clientTop +
      box.scrollTop;
    var bottom = top + item.offsetHeight;
    if (top < box.scrollTop) { box.scrollTop = top; }
    if (bottom > box.scrollTop + box.clientHeight) { box.scrollTop = bottom - box.clientHeight; }
  }

  AH.lib = AH.lib || {};
  AH.lib.list = {
    ensureId: ensureId,
    publish: publish,
    split: split,
    shown: shown,
    enabled: enabled,
    scrollInto: scrollInto
  };
})(window.jQuery, window.AH);
