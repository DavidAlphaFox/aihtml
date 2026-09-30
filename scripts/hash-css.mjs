// Gives a built stylesheet a content hash, as Vite does for the runtime:
// <dir>/<name>.css (the Tailwind CLI output) becomes <dir>/<name>-<hash>.css,
// and <dir>/manifest.json maps "<name>.css" to that name (read by
// aihtml_assets:css/0 for the library, aihtml_example_page:css/0 for the
// example). Older hashed copies are removed, so the directory only ever
// holds the current build. A hashed name never changes content, so the
// application's server may cache it for good.
//
//   node scripts/hash-css.mjs apps/aihtml/priv/static/css/aihtml.css
//        (run by `npm run css:lib' and `npm run css:example')
import { createHash } from "node:crypto";
import { existsSync, readdirSync, readFileSync, renameSync, rmSync, writeFileSync } from "node:fs";
import { basename, dirname, resolve } from "node:path";

const built = process.argv[2];
if (!built || !built.endsWith(".css")) {
  console.error("usage: node scripts/hash-css.mjs <dir>/<name>.css");
  process.exit(1);
}
const file = resolve(built);
const dir = dirname(file);
const base = basename(file, ".css");

const css = readFileSync(file);
// 8 url-safe characters, the length and alphabet of Vite's hashes
const hash = createHash("sha256").update(css).digest("base64url").slice(0, 8);
const name = `${base}-${hash}.css`;
const stale = new RegExp(`^${base}-[A-Za-z0-9_-]{8}\\.css$`);

for (const f of readdirSync(dir)) {
  if (stale.test(f) && f !== name) { rmSync(resolve(dir, f)); }
}
renameSync(file, resolve(dir, name));
const manifestFile = resolve(dir, "manifest.json");
const manifest = existsSync(manifestFile) ? JSON.parse(readFileSync(manifestFile, "utf8")) : {};
manifest[`${base}.css`] = name;
writeFileSync(manifestFile, JSON.stringify(manifest, null, 2) + "\n");
console.log("css:", name);
