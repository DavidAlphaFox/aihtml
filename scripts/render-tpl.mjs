// Renders one shared template with every fixture through the browser-side
// compiler and prints the results as a JSON array (used by
// aihtml_tpl_tests to compare with the Erlang side).
//
//   node scripts/render-tpl.mjs <name>
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { compileFn, templateSource } from "./mustache.mjs";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const name = process.argv[2];
const dir = join(root, "apps", "aihtml", "templates");
const fn = compileFn(templateSource(readFileSync(join(dir, name + ".mustache"), "utf8")), name);
const fixtures = JSON.parse(readFileSync(join(dir, name + ".fixtures.json"), "utf8"));
process.stdout.write(JSON.stringify(fixtures.map((d) => fn(d))));
