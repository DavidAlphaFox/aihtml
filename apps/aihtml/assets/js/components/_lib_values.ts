/* Multi-values of the value-bearing components (combobox, listbox,
 * transfer, checkbox_group, tag_input, the grids' selection ...): several
 * values in one text, for data-ah-value, the hidden input and
 * Event.value. Values are joined with commas; a comma or a backslash
 * inside a value is escaped with a backslash ("\," and "\\"), so plain
 * values read "a,b,c" as before. Same rules as aihtml_value.erl.
 * Also published as AH.lib.values for page scripts. */
import AH from "../core.ts";

function esc(v: unknown): string {
  const s = v == null ? "" : String(v);
  return /[\\,]/.test(s) ? s.replace(/[\\,]/g, "\\$&") : s;
}

/** [values] -> "a,b\,c" */
export function join(vs: readonly unknown[] | null | undefined): string {
  return (vs || []).map(esc).join(",");
}

/** "a,b\,c" -> ["a", "b,c"]; "" (or null) -> []. An array is returned as
 *  strings, so callers can take either. */
export function split(text: string | readonly unknown[] | null | undefined): string[] {
  if (Array.isArray(text)) { return text.map((v: unknown) => v == null ? "" : String(v)); }
  if (text == null || text === "") { return []; }
  const s = String(text), out: string[] = [];
  let cur = "";
  for (let i = 0; i < s.length; i++) {
    const c = s.charAt(i);
    if (c === "\\") { cur += i + 1 < s.length ? s.charAt(++i) : c; }
    else if (c === ",") { out.push(cur); cur = ""; }
    else { cur += c; }
  }
  out.push(cur);
  return out;
}

AH.lib.values = { split, join };
