// The global the components reach the runtime by.
import type { AHApi } from "../core.ts";

declare global {
  interface Window {
    // set by main.ts before any component loads; component code refers to
    // it (vite.config.mjs rewrites `import AH from "../core.ts"`)
    AH: AHApi;
  }
}
