// Actions (aihtml_action, designs/02-actions.md): one POST per event, the
// reply is an AG-UI event stream.
//
// Elements carry data-ah-on="click:TOKEN input:TOKEN:300" (event:signed
// action[:debounce ms]). The event POSTs {action, event, threadId, runId}
// to <body data-ah-action>, and the response streams RUN_STARTED, CUSTOM
// "aihtml.ui" (DOM operations), RUN_FINISHED or RUN_ERROR. Nothing is kept
// on the server between requests.
import type { Behaviours } from "./behaviours.ts";
import { delegateDocument, fire, withSelf } from "./dom.ts";
import type { Root } from "./dom.ts";
import { formFields, isControl, setVal, valueOf } from "./forms.ts";
import { PushStream } from "./push.ts";
import { requestStart } from "./requests.ts";
import type { Swapper } from "./swap.ts";

/** The element an operation targets: op.id, else the selector op.sel;
 *  neither means the document (trigger) or the page (call). */
interface OpTarget { id?: string; sel?: string; }

/** A DOM operation from the server (aihtml_action's ops). */
export type Op = OpTarget & (
  | { op: "html"; html: string; swap?: string }
  | { op: "remove" }
  | { op: "attr"; name: string; value: string | null }
  | { op: "class"; add?: string; remove?: string }
  | { op: "val"; value: unknown }
  | { op: "focus" }
  | { op: "title"; value: string }
  | { op: "redirect"; value: string }
  | { op: "trigger"; event: string; detail?: unknown }
  | { op: "url"; value: string; mode?: "push" | "replace" }
  | { op: "call"; method: string; args?: unknown[] }
  | { op: "js"; code: string });

type OpName = Op["op"];
type OpHandlers = { [K in OpName]: (op: Extract<Op, { op: K }>, targets: Element[]) => void };

/** An AG-UI event of an action's reply or the push stream. */
export interface AguiEvent {
  type: string;
  name?: string;
  value?: unknown;
  message?: string;
  code?: string;
}

/** What an action sends as Event (aihtml_action's Event map). */
export interface EventPayload {
  type: string;
  id: string;
  value: string | string[] | null;
  checked: boolean | null;
  key: string | null;
  form: Record<string, string>;
  values: Record<string, string | string[] | boolean>;
  data: Record<string, string | undefined>;
}

/** One binding of data-ah-on. */
export interface ActionSpec { event: string; token: string; debounce: number; }

/** Detail of ah:error from an action. */
export type ActionError = { status: number } | { message?: string; code?: string };

const ACTION_EVENTS = ["click", "dblclick", "change", "input", "submit", "keydown",
                       "keyup", "focusin", "focusout", "mouseenter", "mouseleave"];
// Latest-wins events: a new one cancels the request still in flight.
const LATEST_WINS: Record<string, boolean> = { input: true, change: true, keyup: true, keydown: true };

function newId(): string {
  if (window.crypto && typeof window.crypto.randomUUID === "function") {
    return window.crypto.randomUUID();
  }
  return Date.now().toString(36) + Math.random().toString(36).slice(2);
}

function classList(s: string | undefined): string[] {
  return String(s || "").split(/\s+/).filter(Boolean);
}

/** A running request of a coordination key and the one waiting after it. */
interface Running { ctrl: AbortController; queued: (() => void) | null; }

export class Actions {
  readonly #behaviours: Behaviours;
  readonly #swapper: Swapper;
  readonly #api: () => unknown;
  readonly push: PushStream;
  readonly #threadId = newId();
  #seq = 0;
  readonly #timers = new Map<string, ReturnType<typeof setTimeout>>();
  readonly #syncs = new Map<string, Running>();
  readonly #listening = new Set<string>();
  readonly #ops: OpHandlers;

  /** api: the AH object, handed to the js operation's code. */
  constructor(behaviours: Behaviours, swapper: Swapper, api: () => unknown) {
    this.#behaviours = behaviours;
    this.#swapper = swapper;
    this.#api = api;
    this.push = new PushStream(this);
    this.#ops = this.#handlers();
    ACTION_EVENTS.forEach((type) => { this.#listen(type); });
  }

  // "event:token[:debounce]"; the event itself may contain a colon
  // (ah:close), the token never does.
  static specs(el: Element): ActionSpec[] {
    return (el.getAttribute("data-ah-on") || "").split(/\s+/).filter(Boolean).map((s) => {
      const m = /^(.+?):([A-Za-z0-9_-]+\.[A-Za-z0-9_-]+)(?::(\d+))?$/.exec(s);
      return m ? { event: m[1], token: m[2], debounce: m[3] ? parseInt(m[3], 10) : 0 }
               : { event: "", token: "", debounce: 0 };
    });
  }

  #newElementId(): string { return "ah-e" + (++this.#seq); }

  #eventPayload(el: Element, e: { type: string; key?: string }): EventPayload {
    if (!el.id) { el.id = this.#newElementId(); }
    const form = el instanceof HTMLFormElement ? el : isControl(el) ? el.form : null;
    const fields: Record<string, string> = {};
    if (form) {
      formFields(form).forEach(([k, v]) => { fields[k] = v; });
    }
    const values: Record<string, string | string[] | boolean> = {};
    const include = el.getAttribute("data-ah-include");
    if (include) {
      document.querySelectorAll(include).forEach((x) => {
        const key = x.id || (x as HTMLInputElement).name;
        if (!key) { return; }
        if (x instanceof HTMLInputElement && (x.type === "checkbox" || x.type === "radio")) {
          values[key] = x.checked;
        } else if (isControl(x)) {
          values[key] = valueOf(x);
        }
      });
    }
    const data: Record<string, string | undefined> = {};
    const ds = (el as HTMLElement).dataset || {};
    Object.keys(ds).forEach((k) => {
      if (k.indexOf("ah") !== 0) { data[k] = ds[k]; }
    });
    const check = el instanceof HTMLInputElement && (el.type === "checkbox" || el.type === "radio");
    // A custom component (slider, rating, dropdown list, toggle button, ...)
    // keeps its value in data-ah-value, which wins over a native value; see
    // designs/04-components.md.
    const value = el.hasAttribute("data-ah-value") ? el.getAttribute("data-ah-value")
      : (isControl(el) ? valueOf(el) : null);
    return {
      type: e.type,
      id: el.id,
      value,
      checked: check ? (el as HTMLInputElement).checked : null,
      key: e.key || null,
      form: fields,
      values,
      data
    };
  }

  static #targets(op: OpTarget): Element[] {
    if (op.id !== undefined) {
      const el = document.getElementById(op.id);
      return el ? [el] : [];
    }
    return op.sel === undefined ? [] : Array.from(document.querySelectorAll(op.sel));
  }

  #handlers(): OpHandlers {
    const b = this.#behaviours;
    return {
      html: (op, ts) => {
        this.#swapper.swap(ts, op.html, op.swap).forEach((n) => {
          if (n.nodeType === 1) { b.mount(n as Element); }
        });
      },
      remove: (_op, ts) => {
        b.destroy(ts);
        ts.forEach((el) => { el.remove(); });
      },
      attr: (op, ts) => {
        ts.forEach((el) => {
          if (op.value === null) { el.removeAttribute(op.name); } else { el.setAttribute(op.name, op.value); }
        });
      },
      "class": (op, ts) => {
        ts.forEach((el) => {
          if (op.add) { el.classList.add(...classList(op.add)); }
          if (op.remove) {
            el.classList.remove(...classList(op.remove));
            if (el.getAttribute("class") === "") { el.removeAttribute("class"); }
          }
        });
      },
      val: (op, ts) => {
        ts.forEach((el) => {
          if (typeof op.value === "boolean") { (el as HTMLInputElement).checked = op.value; } else { setVal(el, op.value); }
        });
      },
      focus: (_op, ts) => { ts.forEach((el) => { (el as HTMLElement).focus(); }); },
      title: (op) => { document.title = op.value; },
      redirect: (op) => { window.location.href = op.value; },
      // Fire a DOM event (a bubbling CustomEvent, detail = op.detail) on the
      // target, or on the document without one; elements may bind actions
      // to it with on/2.
      trigger: (op, ts) => {
        const on: EventTarget[] = (op.id === undefined && op.sel === undefined) ? [document] : ts;
        on.forEach((t) => { fire(t, op.event, op.detail === undefined ? null : op.detail); });
      },
      // Browser history: push or replace the URL without a request. Going
      // back or forward to an entry made here reloads that URL, so the
      // server renders it; the pages stay stateless.
      url: (op) => {
        const st = history.state as { ah?: boolean } | null;
        if (!st || !st.ah) {
          history.replaceState({ ah: true }, "", location.href);
        }
        if (op.mode === "replace") {
          history.replaceState({ ah: true }, "", op.value);
        } else {
          history.pushState({ ah: true }, "", op.value);
        }
      },
      call: (op, ts) => {
        const args = op.args || [];
        if (op.id === undefined && op.sel === undefined) {
          b.callFn(op.method, args);
        } else {
          b.invoke(ts, op.method, ...args);
        }
      },
      // AH is in scope; so is the page's global $ when the page has one
      // (the code runs in the global scope).
      js: (op) => {
        new Function("AH", op.code)(this.#api());
      }
    };
  }

  /** Apply the server's operations in order; a failing one is logged. */
  apply(ops: Op[]): void {
    ops.forEach((op) => {
      try {
        (this.#ops[op.op] as (o: Op, ts: Element[]) => void)(op, Actions.#targets(op));
      } catch (err) {
        console.error("aihtml: operation failed", op, err);
      }
    });
  }

  /** Handle one AG-UI event of a reply (el: the element that sent it). */
  onAgui(el: EventTarget, ev: AguiEvent): void {
    switch (ev.type) {
      case "CUSTOM":
        if (ev.name === "aihtml.ui") {
          this.apply(ev.value as Op[]);
          this.push.sync();          // new content may follow other topics
        }
        break;
      case "RUN_ERROR":
        fire<ActionError>(el, "ah:error", { message: ev.message, code: ev.code });
        console.error("aihtml: action failed:", ev.message);
        break;
      default:
        break;
    }
  }

  // Server-sent events over a fetch() body: blocks separated by a blank
  // line, the payload on "data:" lines.
  static #readStream(body: ReadableStream<Uint8Array>, onEvent: (ev: AguiEvent) => void): Promise<void> {
    const reader = body.getReader();
    const decoder = new TextDecoder();
    let buf = "";
    const pump = (): Promise<void> => reader.read().then((r) => {
      buf += decoder.decode(r.value || new Uint8Array(), { stream: !r.done });
      const blocks = buf.split(/\r?\n\r?\n/);
      buf = r.done ? "" : blocks.pop() || "";
      blocks.forEach((block) => {
        const data = block.split(/\r?\n/)
          .filter((l) => l.indexOf("data:") === 0)
          .map((l) => l.slice(5).replace(/^ /, ""))
          .join("\n");
        if (data) { onEvent(JSON.parse(data) as AguiEvent); }
      });
      return r.done ? undefined : pump();
    });
    return pump();
  }

  // ---- request coordination ---------------------------------------
  //
  // Requests are coordinated per key: the element and event, or, with
  // data-ah-sync-scope="<selector>", the closest matching ancestor, so
  // several elements (the fields of one form) share one queue. When a
  // request for the key is already running, data-ah-sync decides:
  //   drop     ignore the new one (default for click, submit, ...)
  //   replace  abort the running one, send the new one (default for
  //            input, change, keyup, keydown)
  //   queue    send the new one when the running one ends; a later one
  //            replaces a waiting one (only the latest waits)

  /** Run the action of spec for an event on el. */
  run(el: Element, spec: ActionSpec, e: { type: string; key?: string }): void {
    const body = this.#eventPayload(el, e);          // also gives el an id
    const scopeSel = el.getAttribute("data-ah-sync-scope");
    const scope = scopeSel ? (el.closest(scopeSel) || el) : el;
    if (!scope.id) { scope.id = this.#newElementId(); }
    const key = scopeSel ? "scope/" + scope.id : el.id + "/" + spec.event;
    const strategy = el.getAttribute("data-ah-sync") || (LATEST_WINS[spec.event] ? "replace" : "drop");
    const running = this.#syncs.get(key);
    if (running) {
      if (strategy === "drop") { return; }
      if (strategy === "queue") {
        running.queued = () => { void this.#send(el, spec, body, key); };
        return;
      }
      running.ctrl.abort();
    }
    void this.#send(el, spec, body, key);
  }

  #send(el: Element, spec: ActionSpec, body: EventPayload, key: string): Promise<void> {
    const url = document.body.getAttribute("data-ah-action") || "/aihtml/action";
    const ctrl = new AbortController();
    const st: Running = { ctrl, queued: null };
    this.#syncs.set(key, st);
    const end = requestStart(el);
    const done = () => {
      end();
      if (this.#syncs.get(key) === st) {
        this.#syncs.delete(key);
        if (st.queued) { st.queued(); }
      }
    };
    return window.fetch(url, {
      method: "POST",
      credentials: "same-origin",
      headers: { "Content-Type": "application/json", "Accept": "text/event-stream" },
      body: JSON.stringify({ threadId: this.#threadId, runId: newId(), action: spec.token,
                             event: body, streamId: this.push.id }),
      signal: ctrl.signal
    }).then((resp) => {
      if (!resp.ok || !resp.body) {
        // 403 invalid_action: the page was rendered with a secret this
        // server does not know (development restart, rotated secret).
        fire<ActionError>(el, "ah:error", { status: resp.status });
        throw new Error("aihtml: action refused with HTTP " + resp.status);
      }
      return Actions.#readStream(resp.body, (ev) => { this.onAgui(el, ev); });
    }).catch((err: unknown) => {
      if (!(err instanceof Error && err.name === "AbortError")) { console.error(err); }
    }).then(done, done);
  }

  // What an event of `type` on an element with data-ah-on does.
  #onEvent(el: Element, e: Event, type: string): void {
    // A value-bearing component reports its own change/input from its
    // root; the same events bubbling up from controls inside it (an input
    // in a tab panel, say) are not its value changing.
    if ((type === "change" || type === "input") && e.target !== el && el.hasAttribute("data-ah-value")) {
      return;
    }
    Actions.specs(el).forEach((s) => {
      if (s.event !== type) { return; }
      if (type === "submit" ||
          (type === "click" && (el.tagName === "A" || (el as HTMLButtonElement).type === "submit"))) {
        e.preventDefault();
      }
      const question = el.getAttribute("data-ah-confirm");
      if (question && !window.confirm(question)) { return; }
      const ev = { type: e.type, key: (e as KeyboardEvent).key };
      if (s.debounce) {
        const k = el.id + s.token;
        clearTimeout(this.#timers.get(k));
        this.#timers.set(k, setTimeout(() => { this.run(el, s, ev); }, s.debounce));
      } else {
        this.run(el, s, ev);
      }
    });
  }

  // One delegated listener per event type, registered on first use: the
  // common DOM events up front, component events (ah:close, ah:remove, ...)
  // when an element on the page binds them (see listenFor). mouseenter and
  // mouseleave do not bubble: they are caught in the capture phase and
  // only count on the element itself.
  #listen(type: string): void {
    if (this.#listening.has(type)) { return; }
    this.#listening.add(type);
    if (type === "mouseenter" || type === "mouseleave") {
      document.addEventListener(type, (e) => {
        const el = e.target;
        if (el instanceof Element && el.matches("[data-ah-on]")) { this.#onEvent(el, e, type); }
      }, true);
    } else {
      delegateDocument(type, "[data-ah-on]", (e, el) => { this.#onEvent(el, e, type); });
    }
  }

  /** Listen for the events the elements in root bind. */
  listenFor(root: Root): void {
    withSelf(root, "[data-ah-on]").forEach((el) => {
      Actions.specs(el).forEach((s) => { this.#listen(s.event); });
    });
  }
}
