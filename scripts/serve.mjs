// A static file server for local pages (browser tests, previews): ES
// modules cannot be loaded from file:// URLs.
//
//   const srv = await serve(dir);  srv.url("index.html");  await srv.close();
import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join, normalize } from "node:path";

const TYPES = {
  ".html": "text/html; charset=utf-8", ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8", ".css": "text/css; charset=utf-8",
  ".json": "application/json", ".svg": "image/svg+xml", ".png": "image/png",
  ".txt": "text/plain; charset=utf-8",
};

export async function serve(dir) {
  const server = createServer(async (req, res) => {
    const path = normalize(decodeURIComponent(new URL(req.url, "http://x").pathname));
    try {
      const body = await readFile(join(dir, path));
      res.writeHead(200, { "content-type": TYPES[extname(path)] || "application/octet-stream" });
      res.end(body);
    } catch {
      res.writeHead(404);
      res.end();
    }
  });
  await new Promise((ok) => server.listen(0, "127.0.0.1", ok));
  const base = `http://127.0.0.1:${server.address().port}/`;
  return {
    url: (p = "") => base + p,
    close: () => new Promise((ok) => server.close(ok)),
  };
}

// Build the runtime bundle into outDir (vite.config.mjs, AH_JS_OUT) and
// return the entry file name from its manifest.
export async function buildRuntime(root, outDir) {
  const { execFileSync } = await import("node:child_process");
  execFileSync(process.execPath, [join(root, "node_modules/vite/bin/vite.js"), "build"],
               { cwd: root, env: { ...process.env, AH_JS_OUT: outDir }, stdio: ["ignore", "ignore", "inherit"] });
  const manifest = JSON.parse(await readFile(join(outDir, ".vite/manifest.json"), "utf8"));
  return Object.values(manifest).find((m) => m.isEntry).file;
}
