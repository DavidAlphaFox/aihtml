/* Multi-values of the value-bearing components (combobox, listbox,
 * transfer, checkbox_group, tag_input, the grids' selection ...): several
 * values in one text, for data-ah-value, the hidden input and
 * Event.value. Values are joined with commas; a comma or a backslash
 * inside a value is escaped with a backslash ("\," and "\\"), so plain
 * values read "a,b,c" as before. Same rules as aihtml_value.erl.
 * Other lib files load before this one: look it up when called. */
import AH from "../core.js";

function esc(v) {
  var s = v == null ? "" : String(v);
  return /[\\,]/.test(s) ? s.replace(/[\\,]/g, "\\$&") : s;
}

// [values] -> "a,b\,c"
function join(vs) {
  return (vs || []).map(esc).join(",");
}

// "a,b\,c" -> ["a", "b,c"]; "" (or null) -> []. An array is returned as
// strings, so callers can take either.
function split(text) {
  if (Array.isArray(text)) { return text.map(function (v) { return v == null ? "" : String(v); }); }
  if (text == null || text === "") { return []; }
  var s = String(text), out = [], cur = "";
  for (var i = 0; i < s.length; i++) {
    var c = s.charAt(i);
    if (c === "\\") { cur += i + 1 < s.length ? s.charAt(++i) : c; }
    else if (c === ",") { out.push(cur); cur = ""; }
    else { cur += c; }
  }
  out.push(cur);
  return out;
}

AH.lib = AH.lib || {};
AH.lib.values = { split: split, join: join };
