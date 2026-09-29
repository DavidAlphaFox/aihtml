// Push (aihtml_push, designs/03-push.md): one EventSource per page for all
// its topics.
//
// Elements carry data-ah-subscribe="TOKEN" (a signed topic) and maybe
// data-ah-refresh="TOKEN" (an action). The page keeps one stream to <body
// data-ah-events>?t=...: the server says `hello' with the stream id first,
// then sends `ops' events (the same operations actions return). When the
// set of subscribed topics on the page changes, the page POSTs the new set
// for its stream id and the stream follows it without reconnecting; only
// when that fails (the stream is gone) is it reopened. After a reconnect
// (not the first connect) every refresh action runs, since pushes sent
// while the page was away are lost.
import type { ActionSpec, Op } from "./actions.ts";
import { fire } from "./dom.ts";

/** What the stream needs from the actions. */
export interface PushClient {
  receive(ops: Op[]): void;
  run(el: Element, spec: ActionSpec, e: { type: string }): void;
}

export class PushStream {
  readonly #client: PushClient;
  #es: EventSource | null = null;
  #tokens: string[] = [];        // the topics the page follows now
  #key = "";
  #streamKey = "";               // the topics the stream follows (as far as we know)
  #id: string | null = null;
  #opened = false;
  #updating: Promise<void> = Promise.resolve();

  constructor(client: PushClient) {
    this.#client = client;
  }

  /** The server's id of the open stream (sent with actions), if any. */
  get id(): string | null { return this.#id; }

  static #base(): string {
    return document.body.getAttribute("data-ah-events") || "/aihtml/events";
  }

  /** Follow the topics on the page: open, update or close the stream. */
  sync(): void {
    const tokens: string[] = [];
    document.querySelectorAll("[data-ah-subscribe]").forEach((el) => {
      const t = el.getAttribute("data-ah-subscribe") || "";
      if (tokens.indexOf(t) < 0) { tokens.push(t); }
    });
    tokens.sort();
    const key = tokens.join(" ");
    if (key === this.#key || !window.EventSource) { return; }
    this.#tokens = tokens;
    this.#key = key;
    if (!tokens.length) {
      this.#close();
    } else if (this.#es && this.#id) {
      this.#update();
    } else {
      this.#open();
    }
  }

  #close(): void {
    if (this.#es) { this.#es.close(); }
    this.#es = null;
    this.#id = null;
    this.#opened = false;
    this.#streamKey = "";
  }

  #open(): void {
    this.#close();
    const tokens = this.#tokens;
    const es = new EventSource(PushStream.#base() + "?" + tokens.map((t) => "t=" + encodeURIComponent(t)).join("&"));
    this.#es = es;
    // the URL's topics: also what a reconnect of this EventSource follows
    const urlKey = this.#key;
    es.addEventListener("hello", (m: MessageEvent<string>) => {
      if (this.#es !== es) { return; }
      this.#id = (JSON.parse(m.data) as { id: string }).id;
      this.#streamKey = urlKey;
      if (this.#opened) { this.#refreshAll(); }
      this.#opened = true;
      if (this.#key !== this.#streamKey) { this.#update(); }
    });
    es.addEventListener("ops", (m: MessageEvent<string>) => {
      if (this.#es !== es) { return; }
      this.#client.receive(JSON.parse(m.data) as Op[]);
    });
    es.onerror = () => {
      // EventSource retries by itself (the stream id changes: the next
      // hello brings the new one); CLOSED means the server refused the
      // topics (e.g. a secret the server no longer has).
      if (this.#es !== es) { return; }
      this.#id = null;
      if (es.readyState === EventSource.CLOSED) {
        fire(document, "ah:error", { stream: true });
      }
    };
  }

  // Send the page's current topics for the open stream, one request at a
  // time (each sends the latest set); reopen when the stream is gone.
  #update(): void {
    this.#updating = this.#updating.then(() => {
      const id = this.#id, key = this.#key, tokens = this.#tokens;
      if (!this.#es || !id || key === this.#streamKey) { return; }
      return window.fetch(PushStream.#base(), {
        method: "POST",
        credentials: "same-origin",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ stream: id, topics: tokens })
      }).then((resp) => {
        if (resp.ok) {
          if (this.#id === id) { this.#streamKey = key; }
        } else if (this.#key === key) {
          this.#open();
        }
      }, () => {
        if (this.#key === key) { this.#open(); }
      });
    });
  }

  #refreshAll(): void {
    document.querySelectorAll("[data-ah-refresh]").forEach((el) => {
      this.#client.run(el, { event: "refresh", token: el.getAttribute("data-ah-refresh") || "", debounce: 0 },
                       { type: "refresh" });
    });
  }
}
