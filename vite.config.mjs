// Builds the browser runtime (designs/06-bundling.md): apps/aihtml/assets/js
// -> apps/aihtml/priv/static/js, ES modules with content hashes, one chunk
// per component, the optional third-party libraries (AH.vendor) as lazily
// loaded vendor-<name> chunks, THIRD-PARTY-LICENSES.txt, and
// .vite/manifest.json for aihtml_page. `npm run js`.
import { readdirSync, readFileSync, statSync } from "node:fs";
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
// component files: AH.register("<name>", Class) / AH.define("<name>") behaviours (or "// ah-define: <name>"
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
        for (const m of src.matchAll(/AH\.(?:define|register)\(\s*"([a-z][a-z0-9-]*)"/g)) { uses.push(1); out.behaviours.push([m[1], i]); }
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

// The npm package a module belongs to: [its directory, its name], or null.
// The last node_modules/ in the path wins (nested dependencies).
function packageOf(id) {
  const m = /^(.*\/node_modules\/((?:@[^/]+\/)?[^/]+))\//.exec(id.replace(/\\/g, "/"));
  return m ? [m[1], m[2]] : null;
}

// Chunk names: vendor-<package> for a library imported dynamically (its
// facade module is a package's entry), else Vite's default.
function chunkFileNames(chunk) {
  const id = chunk.facadeModuleId || "";
  const pkg = packageOf(id);
  if (pkg) { return `vendor-${pkg[1].replace(/^@/, "").replace("/", "-")}-[hash].js`; }
  return "[name]-[hash].js";
}

// THIRD-PARTY-LICENSES.txt beside the chunks: every npm package with code
// in the bundle (name, version, licence, licence text and NOTICE), read from
// the chunks' modules after tree-shaking. The chunks themselves carry no
// licence comments (output.comments below), so this file is the complete
// record; it is regenerated by every build.
function thirdPartyLicences() {
  return {
    name: "ah-third-party-licences",
    generateBundle(_opts, bundle) {
      const pkgs = new Map();   // dir -> Set of chunk file names
      for (const chunk of Object.values(bundle)) {
        if (chunk.type !== "chunk") { continue; }
        for (const [id, info] of Object.entries(chunk.modules)) {
          const pkg = packageOf(id);
          if (!pkg || !info.renderedLength) { continue; }
          if (!pkgs.has(pkg[0])) { pkgs.set(pkg[0], new Set()); }
          pkgs.get(pkg[0]).add(chunk.fileName.replace(/-[A-Za-z0-9_-]{8}\.js$/, ".js"));
        }
      }
      const isFile = (dir, f) => statSync(join(dir, f)).isFile();
      const entries = [...pkgs.keys()].map((dir) => {
        const pkg = JSON.parse(readFileSync(join(dir, "package.json"), "utf8"));
        const files = readdirSync(dir).sort();
        const licence = files.filter((f) => /^(licen[cs]e|copying)/i.test(f) && isFile(dir, f));
        const notice = files.filter((f) => /^notice/i.test(f) && isFile(dir, f));
        const lic = typeof pkg.license === "string" ? pkg.license
          : (pkg.license && pkg.license.type) || "see licence text";
        return { name: pkg.name, dir, text: [
          "=".repeat(78),
          `${pkg.name} ${pkg.version} (${lic})`,
          `in: ${[...pkgs.get(dir)].sort().join(", ")}`,
          "=".repeat(78), "",
          ...(licence.length ? licence.map((f) => readFileSync(join(dir, f), "utf8").trim())
              : [`Licence: ${lic} (the package ships no licence file)`]),
          ...notice.map((f) => "\n" + f + ":\n\n" + readFileSync(join(dir, f), "utf8").trim()),
          "",
        ].join("\n") };
      });
      // a package installed twice (different versions) is listed twice
      entries.sort((a, b) => a.name.localeCompare(b.name) || a.dir.localeCompare(b.dir));
      const missing = entries.filter((e) => /the package ships no licence file/.test(e.text));
      if (missing.length) {
        this.warn("no licence file in " + missing.map((e) => e.name).join(", "));
      }
      this.emitFile({
        type: "asset",
        fileName: "THIRD-PARTY-LICENSES.txt",
        source: [
          "Third-party software in the aihtml browser runtime (this directory,",
          "built by Vite from apps/aihtml/assets/js; see vite.config.mjs).",
          `${entries.length} packages.`, "",
          ...entries.map((e) => e.text),
        ].join("\n"),
      });
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
  plugins: [ahTemplates(), ahRegistry(), thirdPartyLicences()],
  build: {
    outDir: process.env.AH_JS_OUT || join(lib, "priv/static/js"),
    emptyOutDir: true,
    manifest: true,
    assetsDir: "",
    target: "es2020",
    sourcemap: mode === "development",
    minify: mode !== "development",
    // the echarts chunk is ~1.1 MB (the chart component takes any echarts
    // option, so every series type is in); it loads only on chart pages
    chunkSizeWarningLimit: 1200,
    rollupOptions: {
      input: join(lib, "assets/js/main.js"),
      output: {
        chunkFileNames,
        // licences are collected into THIRD-PARTY-LICENSES.txt instead
        comments: { legal: false },
      },
    },
  },
}));
