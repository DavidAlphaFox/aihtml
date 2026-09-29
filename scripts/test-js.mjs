// Runs the browser-side tests: apps/aihtml/test/js/*.test.js, each in a
// page (served over HTTP) that loads a fresh build of the runtime bundle
// with every component chunk loaded (AH.loadAll) before the tests run. A test file
// registers tests with AHTest.test(name, async fn) and asserts with
// AHTest.eq / AHTest.ok. Node-only tests (the Mustache compiler) live in
// scripts/mustache.test.mjs and run first.
//
//   node scripts/test-js.mjs [filter]
import { execFileSync } from "node:child_process";
import { mkdtempSync, readdirSync, writeFileSync, copyFileSync, existsSync } from "node:fs";
import { tmpdir, homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { serve, buildRuntime } from "./serve.mjs";
import { createRequire } from "node:module";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const filter = process.argv[2] || "";
let failed = 0;

// 1. node-side tests
if (existsSync(join(root, "scripts", "mustache.test.mjs"))) {
  try {
    execFileSync("node", [join(root, "scripts", "mustache.test.mjs")], { stdio: "inherit" });
  } catch {
    failed++;
  }
}

// 2. browser tests
const dir = mkdtempSync(join(tmpdir(), "aihtml-js-"));
const entry = await buildRuntime(root, join(dir, "js"));
const srv = await serve(dir);

const HARNESS = `
window.AHTest = (function () {
  var tests = [];
  function show(v) {
    if (v instanceof Element) { return "<" + v.tagName.toLowerCase() + (v.id ? "#" + v.id : "") + ">"; }
    try { return JSON.stringify(v); } catch (e) { return String(v); }
  }
  return {
    test: function (name, fn) { tests.push({ name: name, fn: fn }); },
    // Native events (components listen with addEventListener, which
    // jQuery's .trigger does not reach). fire(el, type, init) builds the
    // right event class for the type; key(el, "Enter", {shiftKey: true})
    // is a keydown; ready(root) waits until the components in root are
    // loaded and connected (await it after inserting a fixture).
    fire: function (el, type, init) {
      var o = Object.assign({ bubbles: true, cancelable: true }, init || {});
      var C = /^key/.test(type) ? KeyboardEvent
        : /^pointer/.test(type) ? PointerEvent
        : /^(click|dblclick|mouse|contextmenu)/.test(type) ? MouseEvent
        : /^(focus|blur)/.test(type) ? FocusEvent
        : /^(input|beforeinput)$/.test(type) ? InputEvent
        : /^(change|submit|scroll|reset|select)$/.test(type) ? Event
        : CustomEvent;
      return el.dispatchEvent(new C(type, o));
    },
    key: function (el, key, init) {
      return this.fire(el, "keydown", Object.assign({ key: key }, init || {}));
    },
    ready: function (root) { return window.AH.ready(root); },
    ok: function (v, msg) { if (!v) { throw new Error(msg || "expected truthy"); } },
    eq: function (a, b, msg) {
      var nodes = (a instanceof Node) || (b instanceof Node);
      if (nodes ? a !== b : show(a) !== show(b)) { throw new Error((msg ? msg + ": " : "") + show(a) + " !== " + show(b)); }
    },
    run: async function (filter) {
      var out = [];
      for (var i = 0; i < tests.length; i++) {
        var t = tests[i];
        if (filter && t.name.indexOf(filter) < 0) { continue; }
        document.getElementById("fixture").innerHTML = "";
        try { await t.fn(document.getElementById("fixture")); out.push({ name: t.name, ok: true }); }
        catch (e) { out.push({ name: t.name, ok: false, error: String(e && e.stack || e) }); }
      }
      return out;
    }
  };
})();`;
writeFileSync(join(dir, "harness.js"), HARNESS);

const require_ = createRequire(import.meta.url);
let playwright;
try { playwright = require_("playwright"); }
catch { playwright = require_("/home/david/workspace/sigil/node_modules/playwright"); }
function findShell() {
  const base = `${homedir()}/.cache/ms-playwright`;
  if (!existsSync(base)) { return undefined; }
  for (const d of readdirSync(base).filter((d) => d.startsWith("chromium_headless_shell-")).sort().reverse()) {
    const exe = `${base}/${d}/chrome-headless-shell-linux64/chrome-headless-shell`;
    if (existsSync(exe)) { return exe; }
  }
  return undefined;
}

const testDir = join(root, "apps", "aihtml", "test", "js");
const files = readdirSync(testDir).filter((f) => f.endsWith(".test.js")).sort();
const browser = await playwright.chromium.launch({ executablePath: findShell() });
let total = 0;
for (const f of files) {
  copyFileSync(join(testDir, f), join(dir, f));
  writeFileSync(join(dir, f + ".html"), `<!DOCTYPE html><html><head><meta charset="utf-8"></head>
<body data-ah-action="/aihtml/action"><div id="fixture"></div>
<script src="harness.js"></script>
<script type="module">
  await import("./js/${entry}");
  await window.AH.loadAll();
  const s = document.createElement("script");
  s.src = ${JSON.stringify(f)};
  s.onload = () => { window.AHTestReady = true; };
  document.body.appendChild(s);
</script></body></html>`);
  const tab = await browser.newPage();
  const errors = [];
  tab.on("pageerror", (e) => errors.push(e.message));
  await tab.goto(srv.url(f + ".html"));
  await tab.waitForFunction(() => window.AHTestReady === true);
  const results = await tab.evaluate((flt) => window.AHTest.run(flt), filter);
  for (const r of results) {
    total++;
    if (r.ok) { continue; }
    failed++;
    console.log(`FAIL ${f} :: ${r.name}\n  ${r.error.split("\n").slice(0, 3).join("\n  ")}`);
  }
  for (const e of errors) { failed++; console.log(`PAGE ERROR ${f}: ${e}`); }
  await tab.close();
}
await browser.close();
await srv.close();
console.log(`${total} browser tests, ${failed} failed`);
process.exit(failed ? 1 : 0);
