/* The drawer behaviour (designs/04-components.md), ported from sigil's
 * overlay/drawer: the root is the scrim (ah-drawer__overlay), CSS animates
 * data-state; swipe to dismiss. The behaviour is shared with the sheet
 * (SlideController in _lib_overlay.ts). Events on the root: ah:opening
 * and ah:closing (cancelable), ah:open, ah:close (detail OverlayClose). */
// ah-define: drawer
import AH from "../core.ts";
import { SlideController } from "./_lib_overlay.ts";

export type { OverlayClose } from "./_lib_overlay.ts";

class DrawerController extends SlideController {
  protected override readonly kind = "drawer";
}

AH.register("drawer", DrawerController);
