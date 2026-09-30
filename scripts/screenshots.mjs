// Takes the README screenshots (docs/screenshots/*.png) from the running
// example site: start it first (`rebar3 shell`, http://localhost:8080/).
// Each shot is a 1440x900 viewport of one page with one theme; the list
// is SHOTS below. Page errors are reported and make the run fail.
//
//   node scripts/screenshots.mjs [--base=http://localhost:8080]
//        [--out=docs/screenshots] [name ...]     (default: every shot)
import { existsSync, mkdirSync, readdirSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

// name, page, theme axes (the four selects of the docs pages)
const SHOTS = [
  { name: "home", path: "/" },
  { name: "datagrid", path: "/components/datagrid" },
  { name: "node-graph-dark", path: "/components/node_graph", theme: { appearance: "dark" } },
  { name: "calendar-arctic", path: "/components/calendar", theme: { palette: "arctic" } },
  // the same page in Chinese (?lang=zh, aihtml_example_site:lang/1)
  { name: "calendar-zh", path: "/components/calendar?lang=zh" },
  { name: "bar-chart-editorial", path: "/components/bar_chart", theme: { palette: "editorial" } },
  { name: "markdown-editor-island", path: "/components/markdown_editor",
    theme: { palette: "island", skin: "island" } },
  { name: "live-demo", path: "/demo" }
];

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const args = process.argv.slice(2);
const opt = (k, d) => (args.find((a) => a.startsWith(`--${k}=`)) || `=${d}`).split("=").slice(1).join("=");
const base = opt("base", "http://localhost:8080").replace(/\/$/, "");
const out = opt("out", join(root, "docs", "screenshots"));
const names = args.filter((a) => !a.startsWith("--"));
const shots = names.length ? SHOTS.filter((s) => names.includes(s.name)) : SHOTS;
if (names.length && shots.length !== names.length) {
  console.error("unknown shot; known:", SHOTS.map((s) => s.name).join(", "));
  process.exit(1);
}

try {
  await fetch(base + "/");
} catch {
  console.error(`the example site is not running at ${base}: start it with \`rebar3 shell\``);
  process.exit(1);
}

const require_ = (await import("node:module")).createRequire(import.meta.url);
let playwright;
try { playwright = require_("playwright"); }
catch { playwright = require_("/home/david/workspace/sigil/node_modules/playwright"); }
// Use whichever headless shell is installed; the bundled Playwright may
// expect a different build number.
function findShell() {
  const dir = `${homedir()}/.cache/ms-playwright`;
  if (!existsSync(dir)) { return undefined; }
  for (const d of readdirSync(dir).filter((x) => x.startsWith("chromium_headless_shell-")).sort().reverse()) {
    const exe = `${dir}/${d}/chrome-headless-shell-linux64/chrome-headless-shell`;
    if (existsSync(exe)) { return exe; }
  }
  return undefined;
}

mkdirSync(out, { recursive: true });
const browser = await playwright.chromium.launch({ executablePath: findShell() });
let failed = 0;
for (const shot of shots) {
  const ctx = await browser.newContext({ viewport: { width: 1440, height: 900 }, deviceScaleFactor: 1 });
  const page = await ctx.newPage();
  const errors = [];
  page.on("pageerror", (e) => errors.push(e.message));
  // the saved theme is what the page applies before its first paint
  await page.addInitScript((t) => { localStorage.setItem("aihtml.theme", JSON.stringify(t)); },
                           shot.theme || {});
  // "load", not "networkidle": pages with subscriptions keep a push stream open
  await page.goto(base + shot.path, { waitUntil: "load" });
  await page.waitForFunction(() => window.AH && window.AH.stimulus && window.AH.stimulus());
  await page.evaluate(() => window.AH.loadAll());
  await page.waitForTimeout(1500);           // charts, fonts and editors settle
  const file = join(out, shot.name + ".png");
  await page.screenshot({ path: file });
  console.log(errors.length ? `${shot.name}: ${errors.join("; ")}` : `${shot.name}: ${file}`);
  failed += errors.length ? 1 : 0;
  await ctx.close();
}
await browser.close();
process.exit(failed ? 1 : 0);
