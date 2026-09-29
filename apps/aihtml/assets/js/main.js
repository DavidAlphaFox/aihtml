/*
 * Bundle entry (designs/06-bundling.md): the runtime and the registry of
 * component chunks. Components (with the shared Mustache templates they
 * use) are loaded on demand, when the page first has one.
 */
import $ from "jquery";
import AH from "./core.js";
import registry from "virtual:ah-registry";

// Page scripts written against the globals keep working while jQuery is
// still part of the bundle (it goes away component by component).
window.jQuery = window.$ = $;
window.AH = AH;

AH.tpl = AH.tpl || {};
AH.start(registry);
