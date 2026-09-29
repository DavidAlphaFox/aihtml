// The JS Mustache compiler against the official spec cases vendored in
// apps/aihtml/test/mustache-spec (interpolation, sections, inverted,
// comments). Cases that use features we reject (partials, set delimiters)
// are skipped.
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { compileFn } from "./mustache.mjs";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const specDir = join(root, "apps", "aihtml", "test", "mustache-spec");
let pass = 0, fail = 0, skipped = 0;
for (const file of ["interpolation", "sections", "inverted", "comments"]) {
  const spec = JSON.parse(readFileSync(join(specDir, file + ".json"), "utf8"));
  for (const t of spec.tests) {
    const id = `${file}/${t.name}`;
    if (/{{=|{{>/.test(t.template) || t.partials) { skipped++; continue; }
    let got;
    try { got = compileFn(t.template, id)(t.data); }
    catch (e) { got = "ERROR: " + e.message; }
    if (got === t.expected) { pass++; }
    else { fail++; console.log(`FAIL ${id}\n  template: ${JSON.stringify(t.template)}\n  expected: ${JSON.stringify(t.expected)}\n  got:      ${JSON.stringify(got)}`); }
  }
}
console.log(`mustache spec: ${pass} passed, ${fail} failed, ${skipped} skipped (delimiters/partials)`);
process.exit(fail ? 1 : 0);
