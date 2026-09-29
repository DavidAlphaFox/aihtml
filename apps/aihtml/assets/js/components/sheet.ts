/* The sheet behaviour (designs/04-components.md), ported from sigil's
 * overlay/sheet: the root is the scrim (ah-sheet__overlay), CSS animates
 * data-state. The behaviour is shared with the drawer
 * (SlideController in _lib_overlay.ts). Events on the root: ah:opening
 * and ah:closing (cancelable), ah:open, ah:close (detail OverlayClose). */
// ah-define: sheet
import AH from "../core.ts";
import { SlideController } from "./_lib_overlay.ts";

export type { OverlayClose } from "./_lib_overlay.ts";

class SheetController extends SlideController {
  protected override readonly kind = "sheet";
}

AH.register("sheet", SheetController);
