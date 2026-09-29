/* The drawer behaviour (designs/04-components.md), ported from sigil's
 * overlay/drawer: the root is the scrim (ah-drawer__overlay), CSS animates
 * data-state; swipe to dismiss. The behaviour is shared with the sheet
 * (AH.lib.overlay.defineSlide in _lib_overlay.js). */
// ah-define: drawer
import AH from "../core.js";
import "./_lib_overlay.js";

var L = AH.lib.overlay;
var defineSlide = L.defineSlide;

defineSlide("drawer");
