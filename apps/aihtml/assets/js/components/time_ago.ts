/* time-ago behaviour (designs/04-components.md): sigil's format,
   refreshed every 60 s. */
import AH from "../core.ts";

type AgoKey = "just-now" | "minutes" | "hours" | "days" | "months";

// the defaults, in the page's language ({n}: the number)
const AGO: Record<AgoKey, () => string> = {
  "just-now": () => AH.t("time_ago", "just_now", "just now"), minutes: () => AH.t("time_ago", "minutes", "{n}m ago"),
  hours: () => AH.t("time_ago", "hours", "{n}h ago"), days: () => AH.t("time_ago", "days", "{n}d ago"),
  months: () => AH.t("time_ago", "months", "{n}mo ago")
};

function formatAgo(el: Element, t: number): string {
  const label = (k: AgoKey, n?: number): string => {
    const s = el.getAttribute("data-ah-label-" + k) || AGO[k]();
    return n === undefined ? s : s.split("{n}").join(String(n));
  };
  const secs = Math.floor((Date.now() - t) / 1000);
  const mins = Math.floor(secs / 60), hours = Math.floor(mins / 60);
  const days = Math.floor(hours / 24), months = Math.floor(days / 30);
  if (secs < 60) { return label("just-now"); }
  if (mins < 60) { return label("minutes", mins); }
  if (hours < 24) { return label("hours", hours); }
  if (days < 30) { return label("days", days); }
  return label("months", months);
}

class TimeAgoController extends AH.Controller {
  #timer: ReturnType<typeof setInterval> | null = null;

  override setup(): void {
    this.render();
    if (this.element.getAttribute("data-ah-live") === "false") { return; }
    this.#timer = setInterval(() => { this.render(); }, 60000);
  }

  override teardown(): void {
    if (this.#timer !== null) { clearInterval(this.#timer); }
    this.#timer = null;
  }

  // methods (aihtml_action:call/4, AH.invoke)
  // iso string, Date or epoch milliseconds
  setDate(d: string | number | Date): void {
    const t = typeof d === "number" ? d : Date.parse(d instanceof Date ? d.toISOString() : String(d));
    if (isNaN(t)) { return; }
    this.element.setAttribute("datetime", new Date(t).toISOString().replace(/\.\d{3}Z$/, "Z"));
    this.render();
  }

  refresh(): void { this.render(); }

  private render(): void {
    const el = this.element;
    const t = Date.parse(el.getAttribute("datetime") ?? "");
    if (isNaN(t)) { return; }
    el.textContent = formatAgo(el, t);
    if (el.getAttribute("data-ah-title") === "true") {
      el.setAttribute("title", new Date(t).toLocaleString());
    }
  }
}

AH.register("time-ago", TimeAgoController);
