/* Behaviour of the status_bar component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs: the count details float above
   their segment while it is hovered or focused. No events. */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import { hover } from "./_lib_nav.ts";

/** A count's popover: its float handle while shown, the hide timer. */
interface Pop { float: FloatHandle | null; hide: ReturnType<typeof setTimeout> | undefined; }

const POPOVER = ":scope > .ah-status-bar__popover";

class StatusBarController extends AH.Controller {
  readonly #pops = new Map<HTMLElement, Pop>();

  override setup(): void {
    hover(this, ".ah-status-bar__count", (count) => { this.#show(count); }, (count) => { this.#hide(count); });
    this.delegate("focusin", ".ah-status-bar__count", (_e, count) => { this.#show(count); });
    this.delegate("focusout", ".ah-status-bar__count", (_e, count) => { this.#hide(count); });
  }

  override teardown(): void {
    this.#pops.forEach((s) => {
      clearTimeout(s.hide);
      if (s.float) { s.float.stop(); }
    });
    this.#pops.clear();
  }

  #entry(pop: HTMLElement): Pop {
    let s = this.#pops.get(pop);
    if (!s) { s = { float: null, hide: undefined }; this.#pops.set(pop, s); }
    return s;
  }

  #show(count: HTMLElement): void {
    const pop = count.querySelector<HTMLElement>(POPOVER);
    if (!pop) { return; }
    const s = this.#entry(pop);
    clearTimeout(s.hide);
    if (!s.float) {
      s.float = AH.float(pop, count, { placement: "top", offset: 8 });
    }
  }

  #hide(count: HTMLElement): void {
    const pop = count.querySelector<HTMLElement>(POPOVER);
    if (!pop || count.matches(":hover") || count.contains(document.activeElement)) { return; }
    const s = this.#entry(pop);
    // keep it in place while the CSS fade-out runs
    s.hide = setTimeout(() => {
      if (s.float) { s.float.stop(); s.float = null; }
    }, 200);
  }
}

AH.register("status-bar", StatusBarController);
