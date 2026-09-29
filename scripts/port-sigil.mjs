// Imports sigil's stylesheets into apps/aihtml/priv/css/sigil, renaming the
// `sigil-` prefix (classes, custom properties, keyframes, data attributes)
// to `ah-` and the cascade layer `components.sigil` to `components.ah`.
//
// It is an importer, not a build step: the result is checked in and may be
// edited afterwards. Existing files are left alone unless --force is given.
//
//   SIGIL_DIR=../sigil node scripts/port-sigil.mjs [--force]
//
// sigil is MIT licensed, (c) 2026 通九（大连）互联科技有限公司; the notice is
// kept in priv/css/sigil/LICENSE and at the top of every imported file.
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const sigil = process.env.SIGIL_DIR || join(root, "..", "sigil");
const src = join(sigil, "modules", "core", "css");
const dest = join(root, "apps", "aihtml", "priv", "css", "sigil");
const force = process.argv.includes("--force");

const FOUNDATION = [
  "themes/default/palette.css", "tokens.css", "base.css",
  "appearance/light.css", "appearance/dark.css", "appearance/paper.css",
  "primitives/state.css", "primitives/layout.css",
  "typography/serif.css", "typography/grotesk.css",
  "skins/brutal.css", "skins/island.css", "skins/phoqus.css",
  "palettes/arctic.css", "palettes/brutal-blue.css", "palettes/brutal.css",
  "palettes/dracula.css", "palettes/editorial.css", "palettes/ember.css",
  "palettes/green.css", "palettes/island.css", "palettes/luxury.css",
  "palettes/midnight.css", "palettes/nature.css", "palettes/phoqus.css",
  "palettes/retro.css",
];

// The core component set (designs/04-components.md).
export const COMPONENTS = [
  // shared
  "layout",
  // form
  "button", "button_group", "link_button", "dropdown_button", "split_button",
  "segmented-control", "checkbox", "checkbox_group", "radiobutton",
  "radiobutton_group", "radio_cards", "switch_button", "rating_group",
  "input", "password_input", "number_input", "input_otp", "tag_input",
  "dropdownlist", "slider", "form", "validator", "datepicker", "combobox",
  "timepicker", "colorpicker",
  // layout
  "card", "panel", "expander", "tabs", "tab_bar", "breadcrumbs", "pagination",
  "steps", "skeleton", "loader", "empty", "menu", "navbar", "sidenav",
  "toolbar", "splitter", "listmenu", "status_bar", "nav_tree",
  // overlay
  "tooltip", "popover", "drawer", "sheet", "toast", "notification", "window",
  // media, text, data
  "avatar", "badge", "chip", "aspect_ratio", "kbd", "time_ago",
  "expandable_text", "progressbar", "progress_circle", "meter", "statistic",
  "kpi_card", "timeline", "ranking_list", "tag_cloud",
  // second batch: calendar, lists, entry, upload, trees, scrolling, bars,
  // drag and drop (repeat_button has no stylesheet of its own)
  "calendar", "datetime_input", "cascader", "listbox", "transfer",
  "masked_input", "formatted_input", "range_selector", "upload", "tree",
  "diff", "heatmap_calendar", "scrollview", "scrollbar", "responsive_panel",
  "activity_bar", "navigationbar", "command", "sortable", "dragdrop",
];

const NOTICE =
  "/* Ported from sigil, MIT License,\n" +
  "   (c) 2026 通九（大连）互联科技有限公司. See ./LICENSE.\n" +
  "   Imported by scripts/port-sigil.mjs: sigil- renamed to ah-. */\n";

function port(text) {
  return NOTICE + text
    .replace(/components\.sigil\b/g, "components.ah")
    .replace(/\bsigil-/g, "ah-");
}

let written = 0, kept = 0;
function copy(rel, to = rel) {
  const from = join(src, rel);
  if (!existsSync(from)) {
    throw new Error("missing in sigil: " + rel);
  }
  const out = join(dest, to);
  if (existsSync(out) && !force) {
    kept++;
    return;
  }
  mkdirSync(dirname(out), { recursive: true });
  writeFileSync(out, port(readFileSync(from, "utf8")));
  written++;
}

FOUNDATION.forEach((f) => copy(f));
COMPONENTS.forEach((c) => copy(`components/${c}.css`));
const license = join(dest, "LICENSE");
if (!existsSync(license) || force) {
  writeFileSync(license, readFileSync(join(sigil, "LICENSE"), "utf8"));
}
console.log(`sigil css: ${written} written, ${kept} kept (use --force to overwrite)`);
