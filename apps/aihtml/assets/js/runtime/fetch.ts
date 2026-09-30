// Round trips (fetch mode): an element with data-ah-fetch="get|post|..."
// sends a request to data-ah-url when data-ah-trigger fires (default:
// submit for forms, change for inputs, click otherwise). The response is
// HTML that replaces data-ah-target ("this" or a selector) according to
// data-ah-swap, and the new content is mounted. Requests carry the header
// "X-Aihtml: 1".
import type { Behaviours } from "./behaviours.ts";
import { delegateDocument, elements, fire, one } from "./dom.ts";
import type { Targets } from "./dom.ts";
import { payload } from "./forms.ts";
import { requestStart } from "./requests.ts";
import type { Swapper } from "./swap.ts";

/** Detail of ah:before-fetch (cancelable) and ah:after-fetch. */
export interface FetchEvent { url: string; method?: string; }
/** Detail of ah:error after a failed round trip. */
export interface FetchError { url: string; status: number; body: string; }

function defaultTrigger(el: Element): string {
  if (el.tagName === "FORM") { return "submit"; }
  if (/^(INPUT|SELECT|TEXTAREA)$/.test(el.tagName)) { return "change"; }
  return "click";
}

export class Fetcher {
  readonly #behaviours: Behaviours;
  readonly #swapper: Swapper;

  constructor(behaviours: Behaviours, swapper: Swapper) {
    this.#behaviours = behaviours;
    this.#swapper = swapper;
    // One delegated listener per event type covers content added later.
    ["click", "change", "submit", "input"].forEach((type) => {
      delegateDocument(type, "[data-ah-fetch]", (e, el) => {
        if ((el.getAttribute("data-ah-trigger") || defaultTrigger(el)) !== type) { return; }
        if (type === "submit" || type === "click") { e.preventDefault(); }
        if (el.getAttribute("aria-busy") === "true") { return; }
        void this.fetch(el);
      });
    });
  }

  /** Run el's round trip. Resolves to true once the response is swapped
   *  in, false when the request was cancelled (data-ah-confirm,
   *  ah:before-fetch prevented) or failed (ah:error). GET and HEAD send
   *  the payload in the query string, the other methods as a form-encoded
   *  body. */
  fetch(target: Targets): Promise<boolean> {
    const el = one(target);
    if (!(el instanceof Element)) { return Promise.resolve(false); }
    const method = (el.getAttribute("data-ah-fetch") || "get").toUpperCase();
    const url = el.getAttribute("data-ah-url") ||
      (el.tagName === "FORM" ? el.getAttribute("action") || "" : "");
    const sel = el.getAttribute("data-ah-target") || "this";
    const targets = sel === "this" ? [el] : elements(sel);
    const question = el.getAttribute("data-ah-confirm");

    if (question && !window.confirm(question)) { return Promise.resolve(false); }
    if (!fire<FetchEvent>(el, "ah:before-fetch", { url, method })) { return Promise.resolve(false); }

    const end = requestStart(el);
    const data = payload(el);
    const headers: Record<string, string> = {
      "X-Aihtml": "1", "X-Aihtml-Target": sel,
      "X-Requested-With": "XMLHttpRequest", "Accept": "text/html, */*; q=0.01"
    };
    // the page's language, for the route to render the fragment in
    // (aihtml_i18n:with/2, designs/07-i18n.md)
    if (document.documentElement.lang) { headers["X-Aihtml-Lang"] = document.documentElement.lang; }
    const init: RequestInit = { method, credentials: "same-origin", headers };
    let href = url;
    if (method === "GET" || method === "HEAD") {
      if (data) { href = url.replace(/#.*$/, "") + (url.indexOf("?") >= 0 ? "&" : "?") + data; }
    } else {
      init.body = data;
      headers["Content-Type"] = "application/x-www-form-urlencoded; charset=UTF-8";
    }
    let status = 0;
    return window.fetch(href, init).then((resp) => {
      status = resp.status;
      return resp.text().then((body) => {
        if (!resp.ok) { throw { status, body }; }
        return body;
      });
    }).then((html) => {
      this.#swapper.swap(targets, html, el.getAttribute("data-ah-swap") || "inner").forEach((n) => {
        if (n.nodeType === 1) { this.#behaviours.mount(n as Element); }
      });
      fire<FetchEvent>(el, "ah:after-fetch", { url });
      return true;
    }).catch((err: unknown) => {
      const http = err && typeof err === "object" && "status" in err ? err as { status: number; body: string } : null;
      fire<FetchError>(el, "ah:error", { url, status: http ? http.status : status, body: http ? http.body : "" });
      if (!http) { console.error(err); }
      return false;
    }).then((ok) => {
      end();
      return ok;
    });
  }
}
