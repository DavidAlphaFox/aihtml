// Copies jQuery from node_modules into the library's priv/static/vendor, so
// an Erlang consumer gets it without running npm. aihtml itself does not use
// jQuery; it is there for page scripts that want it (aihtml_page's `jquery'
// option).
//
// The optional libraries the runtime loads on demand (echarts, xlsx, jspdf,
// jspdf-autotable, ProseMirror + markdown-it) are not copied here: Vite
// bundles them into lazily loaded vendor-<name> chunks of priv/static/js,
// and lists their licences in priv/static/js/THIRD-PARTY-LICENSES.txt
// (vite.config.mjs).
import { copyFileSync, mkdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const out = join(root, "apps/aihtml/priv/static/vendor");
mkdirSync(out, { recursive: true });

// [from (under node_modules), to (under vendor/)]
const FILES = [
  ["jquery/dist/jquery.min.js", "jquery.min.js"],
  ["jquery/LICENSE.txt", "jquery.LICENSE.txt"],
];

for (const [from, to] of FILES) {
  copyFileSync(join(root, "node_modules", from), join(out, to));
}
console.log(`vendored ${FILES.length} files into`, out);
