// Push (aihtml_push, designs/03-push.md): one EventSource per page for all
// its topics.
//
// Elements carry data-ah-subscribe="TOKEN" (a signed topic) and maybe
// data-ah-refresh="TOKEN" (an action). The page keeps one stream to <body
// data-ah-events>?t=...; it is reopened whenever the set of subscribed
// topics on the page changes. Pushed events are the same CUSTOM
// "aihtml.ui" events actions return. After a reconnect (not the first
// connect) every refresh action runs, since pushes sent while the page was
// away are lost.
import type { ActionSpec, AguiEvent } from "./actions.ts";
import { fire } from "./dom.ts";

/** What the stream needs from the actions. */
export interface PushClient {
  onAgui(el: EventTarget, ev: AguiEvent): void;
  run(el: Element, spec: ActionSpec, e: { type: string }): void;
}

export class PushStream {
  readonly #client: PushClient;
  #es: EventSource | null = null;
  #key = "";
  #id: string | null = null;
  #opened = false;

  constructor(client: PushClient) {
    this.#client = client;
  }

  /** The server's id of the open stream (sent with actions), if any. */
  get id(): string | null { return this.#id; }

  /** Open, reopen or close the stream for the topics on the page. */
  sync(): void {
    const tokens: string[] = [];
    document.querySelectorAll("[data-ah-subscribe]").forEach((el) => {
      const t = el.getAttribute("data-ah-subscribe") || "";
      if (tokens.indexOf(t) < 0) { tokens.push(t); }
    });
    tokens.sort();
    const key = tokens.join(" ");
    if (key === this.#key || !window.EventSource) { return; }
    if (this.#es) { this.#es.close(); }
    this.#es = null;
    this.#key = key;
    this.#id = null;
    this.#opened = false;
    if (!tokens.length) { return; }
    const base = document.body.getAttribute("data-ah-events") || "/aihtml/events";
    const es = new EventSource(base + "?" + tokens.map((t) => "t=" + encodeURIComponent(t)).join("&"));
    this.#es = es;
    es.onopen = () => {
      if (this.#es !== es) { return; }
      if (this.#opened) { this.#refreshAll(); }
      this.#opened = true;
    };
    es.onmessage = (m: MessageEvent<string>) => {
      if (this.#es !== es) { return; }
      const ev = JSON.parse(m.data) as AguiEvent;
      if (ev.type === "CUSTOM" && ev.name === "aihtml.stream") {
        this.#id = (ev.value as { id: string }).id;
      } else {
        this.#client.onAgui(document.body, ev);
      }
    };
    es.onerror = () => {
      // EventSource retries by itself; CLOSED means the server refused the
      // topics (e.g. a secret the server no longer has).
      if (es.readyState === EventSource.CLOSED) {
        fire(document, "ah:error", { stream: true });
      }
    };
  }

  #refreshAll(): void {
    document.querySelectorAll("[data-ah-refresh]").forEach((el) => {
      this.#client.run(el, { event: "refresh", token: el.getAttribute("data-ah-refresh") || "", debounce: 0 },
                       { type: "refresh" });
    });
  }
}
