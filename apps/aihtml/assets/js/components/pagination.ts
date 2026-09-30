/* Behaviour of pagination (value: the current page; fires change).
 *
 * Link mode (data-href, the `href' option): pages are real <a href> links,
 * so the page for every state has a URL the server renders (crawlers,
 * new tabs, bookmarks, no script). Without a server binding for change
 * (data-ah-on) the links simply navigate. With one, a plain click is
 * handled in place: the value changes, change fires (its action renders
 * the new content) and the link's URL is pushed to the history, so back /
 * forward and reloads load that URL from the server. Clicks with a
 * modifier key (new tab, new window) are left to the browser.
 */
import AH from "../core.ts";
import { setValue } from "./_lib_layout.ts";
import "virtual:ah-tpl/pagination_items";

type NavType = "prev" | "next" | "first" | "last";

/** The state read from the root's attributes. */
interface PageState { page: number; total: number; size: number; pages: number; max: number; }

/** The list's data-view (aihtml_pagination's view config). */
interface ViewConfig {
  href: string | null;
  simple: boolean;
  first_last: boolean;
  labels: Record<NavType | "page_info", string>;
}

/** One entry of templates/pagination_items.mustache. */
interface Entry {
  gap: boolean; info: boolean; item: boolean; nav: boolean; link: boolean;
  active: boolean; disabled: boolean; first_last: boolean; href: string;
  number: number; tabindex: number; type: string; label: string; icon: string; text: string;
  page_label: string;
}

const NAV_ICONS: Record<NavType, string> = { prev: "‹", next: "›", first: "«", last: "»" };

function isRecord(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}

function parseConfig(text: string | null): ViewConfig {
  let raw: unknown = null;
  try { raw = JSON.parse(text || "null"); } catch { raw = null; }
  const o = isRecord(raw) ? raw : {};
  const l = isRecord(o.labels) ? o.labels : {};
  const label = (k: string): string => (typeof l[k] === "string" ? l[k] as string : "");
  return {
    href: typeof o.href === "string" ? o.href : null,
    simple: o.simple === true,
    first_last: o.first_last === true,
    labels: { prev: label("prev"), next: label("next"), first: label("first"), last: label("last"),
              page_info: label("page_info") }
  };
}

// Same as aihtml_pagination:visible_pages/3; 0 stands for an ellipsis.
function visiblePages(cur: number, total: number, maxVisible: number): number[] {
  const out: number[] = [];
  const max = Math.max(5, maxVisible);
  if (total <= max) {
    for (let i = 1; i <= total; i++) { out.push(i); }
  } else if (cur <= max - 3) {
    for (let i = 1; i <= max - 2; i++) { out.push(i); }
    out.push(0, total);
  } else if (cur >= total - (max - 4)) {
    out.push(1, 0);
    for (let i = total - (max - 3); i <= total; i++) { out.push(i); }
  } else {
    const h = Math.floor((max - 5) / 2);
    out.push(1, 0);
    for (let i = cur - h; i <= cur + h; i++) { out.push(i); }
    out.push(0, total);
  }
  return out;
}

// A plain left click; with a modifier the browser opens the link itself.
function plainClick(e: MouseEvent): boolean {
  return e.button === 0 && !e.metaKey && !e.ctrlKey && !e.shiftKey && !e.altKey;
}

// Add the URL to the history like aihtml_action:push_url/2 (going back to
// it reloads it, so the server renders that state).
function pushUrl(url: string): void {
  AH.apply([{ op: "url", mode: "push", value: url }]);
}

function fmt(t: string, args: readonly unknown[]): string {
  return args.reduce<string>((acc, a, i) => acc.split("{" + i + "}").join(String(a)), t);
}

function entry(m: Partial<Entry>): Entry {
  return { gap: false, info: false, item: false, nav: false, link: false,
           active: false, disabled: false, first_last: false, href: "",
           number: 0, tabindex: 0, type: "", label: "", icon: "", text: "", page_label: "", ...m };
}

// The view data of templates/pagination_items.mustache; mirrors
// aihtml_pagination:pagination_view/5.
function view(cur: number, pages: number, max: number, size: number, cfg: ViewConfig): { entries: Entry[] } {
  const link = (p: number): string | null =>
    cfg.href == null ? null : cfg.href.replace(/\{page\}/g, String(p)).replace(/\{size\}/g, String(size));
  const nav = (type: NavType, disabled: boolean, target: number): Entry => {
    const url = disabled ? null : link(target);
    return entry({ nav: true, type, label: cfg.labels[type], icon: NAV_ICONS[type],
                   first_last: type === "first" || type === "last", disabled,
                   tabindex: disabled ? -1 : 0, link: url !== null, href: url || "" });
  };
  const middle = cfg.simple
    ? [entry({ info: true, text: fmt(cfg.labels.page_info, [cur, pages]) })]
    : visiblePages(cur, pages, max).map((p) => {
      if (!p) { return entry({ gap: true }); }
      const url = link(p);
      return entry({ item: true, number: p, active: p === cur, tabindex: p === cur ? -1 : 0,
                     page_label: AH.t("common", "page_n", "Page {0}", [p]),
                     link: url !== null, href: url || "" });
    });
  const fl = cfg.first_last && !cfg.simple;
  let entries: Entry[] = [];
  if (fl) { entries.push(nav("first", cur === 1, 1)); }
  entries.push(nav("prev", cur === 1, cur - 1));
  entries = entries.concat(middle);
  entries.push(nav("next", cur === pages, cur + 1));
  if (fl) { entries.push(nav("last", cur === pages, pages)); }
  return { entries };
}

function int(v: string | null): number {
  return parseInt(v || "", 10);
}

class PaginationController extends AH.Controller {
  override setup(): void {
    const el = this.element;
    const blocked = (): boolean => el.classList.contains("ah-pagination-disabled");
    this.delegate("click", "li.ah-pagination-item", (_e, li) => {
      if (!blocked()) { this.go(int(li.getAttribute("data-page")), true); }
    });
    this.delegate("click", "li.ah-pagination-nav", (_e, li) => {
      if (blocked() || li.classList.contains("ah-pagination-nav-disabled")) { return; }
      this.go(this.navTarget(li.getAttribute("data-type")), true);
    });
    // links (href option): followed by the browser unless an action
    // renders the new page in place
    this.delegate("click", "a.ah-pagination-item, a.ah-pagination-nav", (e, a) => {
      if (blocked()) { e.preventDefault(); return; }
      if (!this.bound() || !plainClick(e)) { return; }
      e.preventDefault();
      const t = a.hasAttribute("data-page") ? int(a.getAttribute("data-page"))
        : this.navTarget(a.getAttribute("data-type"));
      this.go(t, true);
    });
    this.delegate("keydown", "li.ah-pagination-item, li.ah-pagination-nav", (e, li) => {
      if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        li.click();
      }
    });
    // Native change events of the inner select/input must not reach the
    // root's action as if the page had changed.
    this.delegate<Event, HTMLSelectElement>("change", ".ah-pagination-size-select", (e, sel) => {
      e.stopPropagation();
      this.resize(int(sel.value), true);
    });
    const stop = (e: Event): void => { e.stopPropagation(); };
    this.delegate("change", ".ah-pagination-jumper-input", stop);
    this.delegate("input", ".ah-pagination-jumper-input", stop);
    const jump = (): void => {
      const input = el.querySelector<HTMLInputElement>(".ah-pagination-jumper-input");
      if (!input) { return; }
      this.go(int(input.value), true);
      input.value = "";
    };
    this.delegate("click", ".ah-pagination-jumper-btn", jump);
    this.delegate("keydown", ".ah-pagination-jumper-input", (e) => {
      if (e.key === "Enter") {
        e.preventDefault();
        jump();
      }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setPage(p: number | string): void { this.go(int(String(p)), false); }
  next(): void { this.go(this.state().page + 1, false); }
  prev(): void { this.go(this.state().page - 1, false); }
  first(): void { this.go(1, false); }
  last(): void { this.go(this.state().pages, false); }
  setPageSize(n: number | string): void { this.resize(int(String(n)), false); }
  setTotal(n: number | string): void {
    const el = this.element;
    el.setAttribute("data-total", String(Math.max(0, int(String(n)) || 0)));
    const s = this.state();
    setValue(el, String(Math.min(s.page, s.pages)));
    this.render();
  }
  value(): number { return this.state().page; }

  private state(): PageState {
    const el = this.element;
    const total = int(el.getAttribute("data-total")) || 0;
    const size = Math.max(1, int(el.getAttribute("data-page-size")) || 10);
    return {
      page: int(el.getAttribute("data-ah-value")) || 1,
      total,
      size,
      pages: Math.max(1, Math.ceil(total / size)),
      max: int(el.getAttribute("data-max-visible")) || 7
    };
  }

  // The page a prev / next / first / last control leads to (NaN: none).
  private navTarget(type: string | null): number {
    const s = this.state();
    switch (type) {
      case "prev": return s.page - 1;
      case "next": return s.page + 1;
      case "first": return 1;
      case "last": return s.pages;
      default: return NaN;
    }
  }

  private href(page: number, size: number): string {
    return (this.element.getAttribute("data-href") || "")
      .replace(/\{page\}/g, String(page)).replace(/\{size\}/g, String(size));
  }

  // A change of the page runs an action (on(change, ...) on the root): the
  // page updates in place instead of loading the link.
  private bound(): boolean {
    return /(^|\s)change:/.test(this.element.getAttribute("data-ah-on") || "");
  }

  private render(): void {
    const el = this.element;
    const s = this.state();
    // the server always renders the page list
    const ul = el.querySelector(":scope > .ah-pagination-pages") as HTMLElement;
    const cfg = parseConfig(ul.getAttribute("data-view"));
    ul.innerHTML = AH.tpl.pagination_items(view(s.page, s.pages, s.max, s.size, cfg));
    const spans = el.querySelectorAll(".ah-pagination-jumper > span");
    const suffix = spans[spans.length - 1];
    const tpl = suffix ? suffix.getAttribute("data-template") : null;
    if (suffix && tpl) {
      suffix.textContent = fmt(tpl, [s.pages]);
    }
  }

  private go(page: number, user: boolean): boolean {
    const el = this.element;
    const s = this.state();
    const p = Math.min(Math.max(1, page | 0), s.pages);
    if (isNaN(page) || p === s.page) {
      return false;
    }
    const linked = !!el.getAttribute("data-href");
    if (linked && !this.bound()) {
      window.location.href = this.href(p, s.size);
      return true;
    }
    setValue(el, String(p));
    this.render();
    if (user) {
      this.fire("change");
      if (linked) { pushUrl(this.href(p, s.size)); }
      // keep keyboard focus inside the control after the rebuild
      const active = el.querySelector<HTMLElement>(".ah-pagination-item-active");
      if (active) { active.focus(); }
    }
    return true;
  }

  private resize(size: number, user: boolean): void {
    const el = this.element;
    const s = this.state();
    if (!size || size === s.size) {
      return;
    }
    const linked = !!el.getAttribute("data-href");
    if (linked && !this.bound()) {
      window.location.href = this.href(1, size);
      return;
    }
    el.setAttribute("data-page-size", String(size));
    const pages = Math.max(1, Math.ceil(s.total / size));
    setValue(el, String(Math.min(s.page, pages)));
    this.render();
    const select = el.querySelector<HTMLSelectElement>(":scope > .ah-pagination-size-selector > select");
    if (select) { select.value = String(size); }
    if (user) {
      this.fire("change");
      if (linked) { pushUrl(this.href(1, size)); }
    }
  }
}

AH.register("pagination", PaginationController);
