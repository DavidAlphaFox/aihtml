/* Shared by the list selection behaviours (cascader.js, listbox.js,
 * transfer.js, ported from sigil): ids, the value-bearing contract and
 * row helpers. All rows, columns and lists are rendered on the server;
 * the behaviours show, hide, mark and move them. */
import AH from "../core.js";
import "./_lib_values.js";

var seq = 0;

function ensureId(el, prefix) {
  if (!el.id) { el.id = prefix + (++seq); }
  return el.id;
}

// Value-bearing contract: data-ah-value + hidden input, then `change`.
// publish(el, value, fire): a native bubbling "change" on el.
function publish(el, value, fire) {
  var old = el.getAttribute("data-ah-value") || "";
  el.setAttribute("data-ah-value", value);
  el.querySelectorAll(":scope > input[type=hidden]").forEach(function (h) { h.value = value; });
  if (fire && old !== value) {
    el.dispatchEvent(new Event("change", { bubbles: true, cancelable: true }));
  }
}

// Direct children of el matching selector.
function kids(el, selector) {
  return el ? Array.prototype.filter.call(el.children, function (c) { return c.matches(selector); }) : [];
}

// Text content of the first direct child matching selector ("" if none).
function childText(el, selector) {
  var c = kids(el, selector)[0];
  return c ? c.textContent : "";
}

// A value text (AH.lib.values) or an array -> values. With `single`
// true a text is one value, commas and all.
function split(v, single) {
  if (v == null || v === "") { return []; }
  if (Array.isArray(v)) { return v.map(String); }
  return single ? [String(v)] : AH.lib.values.split(v);
}

// The value text of values: joined, or with `single` the one value.
function join(vals, single) {
  return single ? (vals.length ? String(vals[0]) : "") : AH.lib.values.join(vals);
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
  kids: kids,
  childText: childText,
  split: split,
  join: join,
  shown: shown,
  enabled: enabled,
  scrollInto: scrollInto
};
