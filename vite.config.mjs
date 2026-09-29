// Builds the browser runtime (designs/06-bundling.md): apps/aihtml/assets/js
// -> apps/aihtml/priv/static/js, ES modules with content hashes, one chunk
// per component, and .vite/manifest.json for aihtml_page. `npm run js`.
import { readdirSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig } from "vite";
import { compile, RUNTIME, templateSource } from "./scripts/mustache.mjs";

const root = dirname(fileURLToPath(import.meta.url));
const lib = join(root, "apps/aihtml");
const components = join(lib, "assets/js/components");
const templates = join(lib, "templates");

// virtual:ah-tpl/<name> -> templates/<name>.mustache compiled by the same
// compiler the Erlang side is checked against; importing it registers
// AH.tpl.<name>. Components import the templates they use, so a template
// travels with the chunk that needs it.
function ahTemplates() {
  const prefix = "virtual:ah-tpl/", runtime = "virtual:ah-tpl-runtime";
  return {
    name: "ah-templates",
    resolveId: (s) => (s === runtime || s.startsWith(prefix) ? "\0" + s : null),
    load(s) {
      if (s === "\0" + runtime) {
        return "export const R = " + RUNTIME + ";\n";
      }
      if (!s.startsWith("\0" + prefix)) { return null; }
      const name = s.slice(prefix.length + 1);
      const file = join(templates, name + ".mustache");
      this.addWatchFile(file);
      return `import AH from ${JSON.stringify(join(lib, "assets/js/core.js"))};\n` +
        `import { R } from "${runtime}";\n` +
        `AH.tpl = AH.tpl || {};\n` +
        `AH.tpl[${JSON.stringify(name)}] = ${compile(templateSource(readFileSync(file, "utf8")), name + ".mustache")};\n`;
    },
  };
}

// virtual:ah-registry -> which chunk to load for what, read from the
// component files: AH.define("<name>") behaviours (or "// ah-define: <name>"
// for a behaviour registered through a helper), AH.fn("<name>") page
// functions (deduplicated), and "// ah-load: <selector>" attribute triggers.
function ahRegistry() {
  const id = "virtual:ah-registry", rid = "\0" + id;
  return {
    name: "ah-registry",
    resolveId: (s) => (s === id ? rid : null),
    load(s) {
      if (s !== rid) { return null; }
      const files = readdirSync(components).filter((f) => f.endsWith(".js")).sort();
      const out = { behaviours: [], fns: [], triggers: [] };
      const loaders = [];
      files.forEach((f, i) => {
        const src = readFileSync(join(components, f), "utf8");
        this.addWatchFile(join(components, f));
        const uses = [];
        for (const m of src.matchAll(/AH\.define\(\s*"([a-z][a-z0-9-]*)"/g)) { uses.push(1); out.behaviours.push([m[1], i]); }
        for (const m of src.matchAll(/AH\.fn\(\s*"([A-Za-z][A-Za-z0-9]*)"/g)) {
          uses.push(1);
          if (!out.fns.some(([n]) => n === m[1])) { out.fns.push([m[1], i]); }
        }
        // behaviours a file registers through a helper (AH.define(name, ...)
        // with a computed name) are declared with "// ah-define: <name>"
        for (const m of src.matchAll(/^\/\/ ah-define: ([a-z][a-z0-9-]*)$/gm)) { uses.push(1); out.behaviours.push([m[1], i]); }
        for (const m of src.matchAll(/^\/\/ ah-load: (.+)$/gm)) { uses.push(1); out.triggers.push([m[1].trim(), i]); }
        loaders.push(uses.length ? `() => import(${JSON.stringify(join(components, f))})` : "null");
      });
      return "const L = [\n  " + loaders.join(",\n  ") + "\n];\nexport default {\n" +
        "  behaviours: {" + out.behaviours.map(([n, i]) => `${JSON.stringify(n)}: L[${i}]`).join(", ") + "},\n" +
        "  fns: {" + out.fns.map(([n, i]) => `${JSON.stringify(n)}: L[${i}]`).join(", ") + "},\n" +
        "  triggers: [" + out.triggers.map(([sel, i]) => `[${JSON.stringify(sel)}, L[${i}]]`).join(", ") + "]\n};\n";
    },
  };
}

export default defineConfig(({ mode }) => ({
  root: join(lib, "assets/js"),
  // chunks import each other by relative URL: the bundle works wherever
  // priv/static is mounted (/aihtml/ by default)
  base: "./",
  publicDir: false,
  logLevel: "warn",
  plugins: [ahTemplates(), ahRegistry()],
  build: {
    outDir: process.env.AH_JS_OUT || join(lib, "priv/static/js"),
    emptyOutDir: true,
    manifest: true,
    assetsDir: "",
    target: "es2020",
    sourcemap: mode === "development",
    minify: mode !== "development",
    rollupOptions: { input: join(lib, "assets/js/main.js") },
  },
}));
