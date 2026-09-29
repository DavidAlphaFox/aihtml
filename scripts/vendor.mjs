// Copies jQuery from node_modules into the library's priv/static, so an
// Erlang consumer gets a working page without running npm.
import { copyFileSync, mkdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const out = join(root, "apps/aihtml/priv/static/vendor");
mkdirSync(out, { recursive: true });
copyFileSync(join(root, "node_modules/jquery/dist/jquery.min.js"), join(out, "jquery.min.js"));
copyFileSync(join(root, "node_modules/jquery/LICENSE.txt"), join(out, "jquery.LICENSE.txt"));
console.log("vendored jquery into", out);
