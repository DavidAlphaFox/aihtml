// The bundle's virtual modules (vite.config.mjs).

// A compiled shared template (templates/<name>.mustache); importing it
// registers AH.tpl.<name>.
declare module "virtual:ah-tpl/*" {}

// The lazy-loading registry built from components/*.ts.
declare module "virtual:ah-registry" {
  const registry: import("../runtime/behaviours.ts").Registry;
  export default registry;
}
