// Screenshots an HTML file in headless Chromium and reports console
// errors and page errors. Each run starts its own browser, so parallel
// runs do not interfere.
//
//   node scripts/preview.mjs page.html out.png [--width=1200] [--theme=dark]
//        [--script=file.js]   (run after load: an async function body that
//                              may return a value, printed as JSON)
import { readFileSync } from "node:fs";
import { basename, dirname, resolve } from "node:path";
import { serve } from "./serve.mjs";
import { pathToFileURL } from "node:url";

const require_ = (await import("node:module")).createRequire(import.meta.url);
let playwright;
try { playwright = require_("playwright"); }
catch { playwright = require_("/home/david/workspace/sigil/node_modules/playwright"); }

const [html, png, ...opts] = process.argv.slice(2);
const opt = (k, d) => (opts.find((o) => o.startsWith(`--${k}=`)) || `=${d}`).split("=")[1];
// Use whichever headless shell is installed; the bundled Playwright may
// expect a different build number.
import { readdirSync as ls, existsSync as exists } from "node:fs";
import { homedir } from "node:os";
function findShell() {
  const base = `${homedir()}/.cache/ms-playwright`;
  if (!exists(base)) { return undefined; }
  const dirs = ls(base).filter((d) => d.startsWith("chromium_headless_shell-")).sort().reverse();
  for (const d of dirs) {
    const exe = `${base}/${d}/chrome-headless-shell-linux64/chrome-headless-shell`;
    if (exists(exe)) { return exe; }
  }
  return undefined;
}
const browser = await playwright.chromium.launch({ executablePath: findShell() });
const page = await browser.newPage({ viewport: { width: +opt("width", 1200), height: 900 } });
const problems = [];
page.on("console", (m) => { if (m.type() === "error") problems.push("console: " + m.text()); });
page.on("pageerror", (e) => problems.push("pageerror: " + e.message));
// served over HTTP (the runtime is an ES module), components all loaded
const srv = await serve(dirname(resolve(html)));
await page.goto(srv.url(basename(html)));
await page.waitForFunction(() => window.AH && window.AH.stimulus && window.AH.stimulus());
await page.evaluate(() => window.AH.loadAll());
const theme = opt("theme", "");
if (theme) { await page.evaluate((t) => document.documentElement.setAttribute("data-theme", t), theme); }
await page.waitForTimeout(300);
const script = opt("script", "");
if (script) {
  const body = readFileSync(script, "utf8");
  const result = await page.evaluate(`(async () => { ${body} })()`);
  console.log("script result:", JSON.stringify(result, null, 2));
}
await page.screenshot({ path: png, fullPage: true });
await browser.close();
await srv.close();
console.log(problems.length ? problems.join("\n") : "no console errors");
