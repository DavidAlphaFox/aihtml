// Copies third-party scripts from node_modules into the library's
// priv/static/vendor, so an Erlang consumer gets a working page without
// running npm.
//
//   jquery        always loaded by the page (aihtml:page/2)
//   echarts       charts and relation_graph            } loaded on demand by
//   xlsx          datagrid / pivotgrid Excel export    } AH.vendor(name), only
//   jspdf         datagrid / pivotgrid PDF export      } on pages that use them
//   jspdf-autotable  tables in PDF export (needs jspdf)
//   prosemirror   markdown_editor: ProseMirror + markdown-it, bundled
//
// Each library's licence file is copied next to it.
//
// ProseMirror has no single-file browser build, so its packages (and
// markdown-it, the parser behind prosemirror-markdown) are bundled with
// esbuild from apps/aihtml/assets/vendor/prosemirror.entry.js into one
// minified IIFE, prosemirror.min.js, whose global is AHProseMirror. The
// licences of every package that ends up in the bundle (read from
// esbuild's metafile) are collected into prosemirror.LICENSE.txt.
import { copyFileSync, mkdirSync, readdirSync, readFileSync, statSync,
         writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { build } from "esbuild";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const out = join(root, "apps/aihtml/priv/static/vendor");
mkdirSync(out, { recursive: true });

// [from (under node_modules), to (under vendor/)]
const FILES = [
  ["jquery/dist/jquery.min.js", "jquery.min.js"],
  ["jquery/LICENSE.txt", "jquery.LICENSE.txt"],
  ["echarts/dist/echarts.min.js", "echarts.min.js"],
  ["echarts/LICENSE", "echarts.LICENSE.txt"],
  ["echarts/NOTICE", "echarts.NOTICE.txt"],
  ["xlsx/dist/xlsx.full.min.js", "xlsx.full.min.js"],
  ["xlsx/LICENSE", "xlsx.LICENSE.txt"],
  ["jspdf/dist/jspdf.umd.min.js", "jspdf.umd.min.js"],
  ["jspdf/LICENSE", "jspdf.LICENSE.txt"],
  ["jspdf-autotable/dist/jspdf.plugin.autotable.min.js", "jspdf.plugin.autotable.min.js"],
  ["jspdf-autotable/LICENSE.txt", "jspdf-autotable.LICENSE.txt"],
];

for (const [from, to] of FILES) {
  copyFileSync(join(root, "node_modules", from), join(out, to));
}
console.log(`vendored ${FILES.length} files into`, out);

// ---- the ProseMirror bundle ----
const result = await build({
  entryPoints: [join(root, "apps/aihtml/assets/vendor/prosemirror.entry.js")],
  outfile: join(out, "prosemirror.min.js"),
  bundle: true,
  format: "iife",
  globalName: "AHProseMirror",
  minify: true,
  target: "es2019",
  legalComments: "none",
  metafile: true,
  logLevel: "warning",
});

// The package directories (node_modules/<name> or node_modules/@s/<name>)
// of the bundled inputs, in a stable order.
const pkgDirs = new Set();
for (const input of Object.keys(result.metafile.inputs)) {
  const m = /^(.*node_modules\/(?:@[^/]+\/)?[^/]+)\//.exec(input);
  if (m) { pkgDirs.add(m[1]); }
}
const licence = [
  "Third-party software bundled in prosemirror.min.js (built by scripts/vendor.mjs",
  "from apps/aihtml/assets/vendor/prosemirror.entry.js).",
  "",
];
for (const dir of [...pkgDirs].sort()) {
  const abs = join(root, dir);
  const pkg = JSON.parse(readFileSync(join(abs, "package.json"), "utf8"));
  const file = readdirSync(abs).find((f) => /^(licen[cs]e|copying)/i.test(f) &&
                                            statSync(join(abs, f)).isFile());
  licence.push("=".repeat(78), `${pkg.name} ${pkg.version} (${pkg.license})`, "=".repeat(78), "");
  licence.push(file ? readFileSync(join(abs, file), "utf8").trim() : `Licence: ${pkg.license}`, "");
}
writeFileSync(join(out, "prosemirror.LICENSE.txt"), licence.join("\n"));
const size = statSync(join(out, "prosemirror.min.js")).size;
console.log(`bundled prosemirror.min.js (${Math.round(size / 1024)} KB, ${pkgDirs.size} packages)`);
