/* The sheet behaviour (designs/04-components.md), ported from sigil's
 * overlay/sheet: the root is the scrim (ah-sheet__overlay), CSS animates
 * data-state. The behaviour is shared with the drawer
 * (AH.lib.overlay.defineSlide in _lib_overlay.js). */
// ah-define: sheet
import AH from "../core.js";
import "./_lib_overlay.js";

var L = AH.lib.overlay;
var defineSlide = L.defineSlide;

defineSlide("sheet");
