/* The toast page function (designs/04-components.md), ported from sigil's
 * overlay/toast: AH.fn("toast") (also AH.toast) pops a card built from
 * templates/notification.mustache and templates/toast.mustache, and a
 * click on a [data-ah-toast] element (shows_toast/2) pops one without a
 * server round trip. Cards: NotifyCard in _lib_overlay.ts; the card fires
 * ah:open, ah:close (detail OverlayClose) and ah:click on itself. */
// ah-load: [data-ah-toast]
import AH from "../core.ts";
import { NotifyCard, cardView, duration, matching } from "./_lib_overlay.ts";
import type { CardOptions } from "./_lib_overlay.ts";
import "virtual:ah-tpl/notification";
import "virtual:ah-tpl/toast";

export type { OverlayClose } from "./_lib_overlay.ts";

/** AH.toast's options (sigil's toast/show!): duration in ms (default
 *  4000, <= 0 stays). */
export interface ToastOptions extends CardOptions {
  title?: unknown;
  description?: unknown;
  position?: string | null;
  duration?: number | string | null;
}

/** AH.fn("toast", opts): sigil's toast/show!, for client-side triggers
 *  (shows_toast/2). Text is escaped by the template. Returns the card. */
export function toast(opts?: ToastOptions | string | null): HTMLElement | undefined {
  const o: ToastOptions = typeof opts === "string" ? { title: opts } : (opts || {});
  const title = o.title === undefined || o.title === null ? "" : String(o.title);
  const desc = o.description === undefined || o.description === null ? "" : String(o.description);
  const content = AH.tpl.toast({ has_title: title !== "", title: title,
                                 has_description: desc !== "", description: desc });
  return NotifyCard.show(AH.tpl.notification(cardView(o, content)),
                         { position: o.position, duration: duration(o.duration, 4000) });
}

declare module "../core.ts" {
  interface AHApi {
    /** toast(opts) for page scripts, once this chunk has loaded. */
    toast?: typeof toast;
  }
}

AH.fn("toast", toast);
AH.toast = toast;

document.addEventListener("click", (e) => {
  matching(e.target, "[data-ah-toast]").forEach((t) => {
    const a = (n: string): string | undefined => {
      const v = t.getAttribute(n);
      return v === null ? undefined : v;
    };
    toast({
      title: a("data-ah-toast"),
      description: a("data-ah-toast-description"),
      variant: a("data-ah-toast-variant"),
      duration: a("data-ah-toast-duration"),
      position: a("data-ah-toast-position"),
      closable: a("data-ah-toast-closable")
    });
  });
});
