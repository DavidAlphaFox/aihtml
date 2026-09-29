/*
 * Bundle entry (designs/06-bundling.md): the runtime and the registry of
 * component chunks. Components (with the shared Mustache templates they
 * use) are loaded on demand, when the page first has one; they reach the
 * runtime as window.AH, set here before any of them loads. No jQuery: page
 * scripts that want it load it themselves (aihtml_page's `jquery' option).
 */
import AH from "./core.ts";
import registry from "virtual:ah-registry";

window.AH = AH;
AH.start(registry);
