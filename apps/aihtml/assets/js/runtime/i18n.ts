// The texts of what the browser builds itself (designs/07-i18n.md).
//
// The catalogs live on the server (priv/i18n/<lang>.json). A page in a
// language other than English carries the part the browser needs as JSON
// in <script type="application/json" id="ah-labels"> (aihtml_page); an
// English page carries nothing. Every call site gives its English text,
// which aihtml_i18n_tests keeps equal to en.json, so the English texts
// still have one source:
//
//   AH.t("node_graph", "copy_nodes", "Copy {0} nodes", [n])
//   AH.format("months_short", ["Jan", ...])

/** What the page carries: texts by scope, and formatting settings. */
interface Catalog {
  messages?: Record<string, Record<string, unknown>>;
  format?: Record<string, unknown>;
}

let catalog: Catalog | null = null;

function load(): Catalog {
  if (catalog) { return catalog; }
  catalog = {};
  const el = document.getElementById("ah-labels");
  if (el && el.textContent) {
    try {
      const parsed: unknown = JSON.parse(el.textContent);
      if (parsed && typeof parsed === "object") { catalog = parsed as Catalog; }
    } catch { /* a broken catalog leaves the English texts */ }
  }
  return catalog;
}

/** The text `key' of `scope' in the page's language, else `fallback'
 *  (the English text); placeholders {0}, {1} ... take `args'. */
export function t(scope: string, key: string, fallback: string,
                  args?: ReadonlyArray<string | number>): string {
  const found = load().messages?.[scope]?.[key];
  let text = typeof found === "string" ? found : fallback;
  if (args) {
    args.forEach((a, i) => { text = text.split("{" + i + "}").join(String(a)); });
  }
  return text;
}

/** A formatting setting of the page's language (month names ...), else
 *  `fallback'. */
export function format<T>(key: string, fallback: T): T {
  const found = load().format?.[key];
  return found === undefined ? fallback : found as T;
}

/** Read the page's catalog again (tests that replace it). */
export function reloadTexts(): void { catalog = null; }
