/* Helpers shared by the layout components (panel, expander, tabs,
 * tab-bar, pagination, steps, loader): AH.lib.layout.
 *
 * Value-bearing components keep their value in data-ah-value on the root,
 * mirror it into a hidden input when there is one, and fire "change" on
 * the root when the user changes it. Methods called by the server
 * (AH.invoke / aihtml_action:call) update the value without firing
 * "change".
 */
(function ($, AH) {
  "use strict";

  function setValue(el, $el, v) {
    el.setAttribute("data-ah-value", v);
    $el.children("input[type=hidden]").val(v);
  }

  function key(e) {
    return e.key;
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

  AH.lib = AH.lib || {};
  AH.lib.layout = { setValue: setValue, key: key, nextEnabled: nextEnabled, listKeys: listKeys };
})(window.jQuery, window.AH);
