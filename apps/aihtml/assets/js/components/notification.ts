/* The notification behaviour (designs/04-components.md), ported from
 * sigil's overlay/notification: a hidden template whose open clones its
 * card into a corner stack; and the page function AH.fn("notify") (also
 * AH.notify), which places a card rendered on the server (ah_toast/3,
 * notify/2) or built here. Cards: NotifyCard in _lib_overlay.ts. Events
 * on the template: ah:open, ah:close (detail OverlayClose, {result:
 * null}), ah:click. */
import AH from "../core.ts";
import { NotifyCard, cardView, duration, escapeHtml, num } from "./_lib_overlay.ts";
import type { CardOptions } from "./_lib_overlay.ts";
import "virtual:ah-tpl/notification";

export type { OverlayClose } from "./_lib_overlay.ts";

/** AH.notify's options: card is the HTML aihtml_toast:ah_toast/3 and
 *  aihtml_notification:notify/2 render on the server; without it, text
 *  and the CardOptions build one here. duration in ms (default 3000,
 *  <= 0 stays). */
export interface NotifyOptions extends CardOptions {
  card?: string | null;
  text?: unknown;
  position?: string | null;
  duration?: number | string | null;
}

/** AH.fn("notify", opts): a card in its corner; returns the card. */
export function notify(opts?: NotifyOptions | string | null): HTMLElement | undefined {
  const o: NotifyOptions = typeof opts === "string" ? { text: opts } : (opts || {});
  const card = o.card || AH.tpl.notification(
    cardView(o, escapeHtml(String(o.text || ""))));
  return NotifyCard.show(card, { position: o.position, duration: duration(o.duration, 3000) });
}

declare module "../core.ts" {
  interface AHApi {
    /** notify(opts) for page scripts, once this chunk has loaded. */
    notify?: typeof notify;
  }
}

AH.fn("notify", notify);
AH.notify = notify;

class NotificationController extends AH.Controller {
  #cards: HTMLElement[] = [];

  override teardown(): void {
    this.#cards.forEach((c) => { c.remove(); });
    this.#cards = [];
    document.querySelectorAll(".ah-notify-container").forEach((c) => {
      if (!c.children.length) { c.remove(); }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open(): void {
    const el = this.element;
    // the server rendered the card inside the template element
    const src = el.querySelector<HTMLElement>(":scope > .ah-notify");
    if (!src) { return; }
    // a clone of an HTMLElement is one
    const card = NotifyCard.show(src.cloneNode(true) as HTMLElement, {
      position: el.getAttribute("data-ah-position"),
      duration: num(el, "duration", 3000),
      source: el
    });
    this.#cards = this.#live();
    if (card) { this.#cards.push(card); }
  }

  close(): void { this.closeAll(); }

  closeAll(): void {
    this.#cards.forEach((c) => { NotifyCard.close(c); });
    this.#cards = [];
  }

  closeLast(): void {
    const cards = this.#live();
    const last = cards.pop();
    if (last) { NotifyCard.close(last); }
    this.#cards = cards;
  }

  #live(): HTMLElement[] { return this.#cards.filter((c) => document.contains(c)); }
}

AH.register("notification", NotificationController);
