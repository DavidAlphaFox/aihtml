// Builds apps/aihtml/priv/static/aihtml.js: the runtime (assets/js/core.js)
// followed by every component behaviour file (assets/js/components/*.js,
// sorted). No bundler: the files are plain scripts sharing window.AH.
import { existsSync, readdirSync, readFileSync, writeFileSync } from "node:fs";
import { compile, RUNTIME, templateSource } from "./mustache.mjs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const js = join(root, "apps", "aihtml", "assets", "js");
// Optional first argument: another output file (previews, parallel work).
const out = process.argv[2] || join(root, "apps", "aihtml", "priv", "static", "aihtml.js");

// Shared Mustache templates (apps/aihtml/templates/*.mustache) compiled to
// AH.tpl.<name>(data) -> string, between the runtime and the components.
const tplDir = join(root, "apps", "aihtml", "templates");
const templates = existsSync(tplDir)
  ? readdirSync(tplDir).filter((f) => f.endsWith(".mustache")).sort() : [];
const tplJs = "/* ---- templates (compiled from apps/aihtml/templates) ---- */\n" +
  "(function (AH) {\n  var R = " + RUNTIME + ";\n  AH.tpl = AH.tpl || {};\n" +
  templates.map((f) => {
    const name = f.replace(/\.mustache$/, "");
    return `  AH.tpl[${JSON.stringify(name)}] = ${compile(templateSource(readFileSync(join(tplDir, f), "utf8")), f)};\n`;
  }).join("") + "})(window.AH);\n";

const components = readdirSync(join(js, "components"))
  .filter((f) => f.endsWith(".js")).sort().map((f) => "components/" + f);
const read = (p) => `/* ---- ${p} ---- */\n` + readFileSync(join(js, p), "utf8");
const body = [read("core.js"), tplJs, ...components.map(read)].join("\n");
writeFileSync(out, body);
console.log(`aihtml.js: core, ${templates.length} templates, ${components.length} component files`);
