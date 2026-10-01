// The base class of every component behaviour (designs/06-bundling.md).
import { Controller as StimulusController } from "@hotwired/stimulus";
import type { Context } from "@hotwired/stimulus";
import { fire } from "./dom.ts";

type Listener<E extends Event> = (e: E) => void;

/** The type of a declared value, written as in Stimulus: String, Number,
 *  Boolean, or Object / Array for JSON. */
export type ValueType = StringConstructor | NumberConstructor | BooleanConstructor
  | ObjectConstructor | ArrayConstructor;

/** A declared value: its type, or its type and the value used while the
 *  attribute is absent. */
export type ValueSpec = ValueType | { type: ValueType; default?: unknown };

/** data-ah-<key>, the key dasherised: maxDepth -> data-ah-max-depth. */
export function valueAttribute(key: string): string {
  return "data-ah-" + key.replace(/[A-Z]/g, (c) => "-" + c.toLowerCase());
}

function specType(spec: ValueSpec): ValueType {
  return typeof spec === "function" ? spec : spec.type;
}

function emptyOf(type: ValueType): unknown {
  return type === String ? "" : type === Number ? 0 : type === Boolean ? false
    : type === Array ? [] : {};
}

/** The typed value of an attribute (raw: null when absent). */
function readValue(spec: ValueSpec, raw: string | null): unknown {
  const type = specType(spec);
  if (raw === null) {
    return typeof spec === "function" || spec.default === undefined ? emptyOf(type) : spec.default;
  }
  if (type === Number) { return Number(raw); }
  if (type === Boolean) { return !(raw === "false" || raw === "0"); }
  if (type === Object || type === Array) {
    try { return JSON.parse(raw) as unknown; } catch { return emptyOf(type); }
  }
  return raw;
}

function writeValue(type: ValueType, v: unknown): string {
  return type === Object || type === Array ? JSON.stringify(v) : String(v);
}

// The classes whose <key>Value accessors are defined.
const blessed = new WeakSet<object>();

/**
 * A Stimulus controller with what every aihtml component needs:
 *
 *   setup() / teardown()   run once when the element enters the page and
 *                          once when it really leaves it; a move (morph,
 *                          preserve) disconnects and reconnects without
 *                          running them again, so the component keeps its
 *                          state. AH.destroy(el) runs teardown at once,
 *                          AH.mount(el) runs setup again (morph does both
 *                          for a component whose DOM changed).
 *   listen(target, type, handler[, options])
 *                          addEventListener, removed on teardown
 *   delegate(type, selector, handler[, root])
 *                          a delegated listener: handler(e, match)
 *   fire(type, detail[, target])
 *                          a native, bubbling, cancelable CustomEvent; the
 *                          server's on(Event, ...) and Stimulus actions
 *                          both see it
 *   signal                 the AbortSignal of this setup (for fetch)
 *
 * Public methods are what aihtml_action:call/4 and AH.invoke(el, method,
 * ...args) reach; the catalog (types/catalog.d.ts) lists the ones the
 * server calls.
 *
 * Values, as in Stimulus but on the element's data-ah-* attributes (and
 * declared as attrs: Stimulus's own static values would bind
 * data-<identifier>-<key>-value):
 *
 *   static override attrs = { value: String, max: { type: Number, default: 100 } };
 *   declare readonly maxValue: number;      // data-ah-max, typed
 *   valueValueChanged(value, old) {...}     // data-ah-value changed
 *
 * <key>Value reads the attribute (the default while it is absent) and,
 * assigned, writes it. <key>ValueChanged(value, old) runs, after setup,
 * whenever the attribute changes, whoever changed it: the component
 * itself, a morph, the server's attr operation, a data-ah-on-client. Unlike
 * Stimulus it does not run for the initial value; setup reads that.
 * It may run for the component's own changes, so it compares with what
 * the element shows and does nothing when they agree.
 *
 * A component that declares attrs follows its attributes, so a morph
 * that changes it patches its DOM and keeps it set up (its state, its
 * listeners) instead of running teardown and setup again. Declare attrs
 * only when setup does not depend on the DOM the server renders inside
 * the element beyond what it reads again when it needs it.
 */
export class Controller<E extends Element = HTMLElement> extends StimulusController<E> {
  /** Runtime hooks (set by core.ts): after setup starts, after connect. */
  static onStart: (el: Element) => void = () => {};
  static onConnect: (el: Element) => void = () => {};

  /** The values this behaviour reads from data-ah-* attributes. */
  static attrs: Record<string, ValueSpec> = {};

  /** True when the behaviour declares attrs (a morph keeps it set up). */
  static get followsAttributes(): boolean { return Object.keys(this.attrs).length > 0; }

  #abort: AbortController | null = null;
  #observer: MutationObserver | null = null;
  // attribute -> its text when last reported
  readonly #seen = new Map<string, string | null>();

  constructor(context: Context) {
    super(context);
    Controller.#bless(this.constructor as typeof Controller);
  }

  // Define <key>Value on the class's prototype, once per class.
  static #bless(klass: typeof Controller): void {
    if (blessed.has(klass)) { return; }
    blessed.add(klass);
    Object.entries(klass.attrs).forEach(([key, spec]) => {
      const attr = valueAttribute(key);
      const name = key + "Value";
      if (Object.prototype.hasOwnProperty.call(klass.prototype, name)) { return; }
      Object.defineProperty(klass.prototype, name, {
        configurable: true,
        get(this: Controller<Element>) { return readValue(spec, this.element.getAttribute(attr)); },
        set(this: Controller<Element>, v: unknown) {
          if (v === undefined || v === null) {
            this.element.removeAttribute(attr);
          } else {
            this.element.setAttribute(attr, writeValue(specType(spec), v));
          }
        }
      });
    });
  }

  // Report changed values to <key>ValueChanged: from now on, for changes
  // after the current attributes.
  #observe(): void {
    const specs = (this.constructor as typeof Controller).attrs;
    const attrs = new Map(Object.entries(specs).map(([key, spec]) => [valueAttribute(key), { key, spec }]));
    if (!attrs.size) { return; }
    attrs.forEach((_v, attr) => { this.#seen.set(attr, this.element.getAttribute(attr)); });
    this.#observer = new MutationObserver(() => {
      attrs.forEach(({ key, spec }, attr) => {
        const raw = this.element.getAttribute(attr);
        const old = this.#seen.get(attr) ?? null;
        if (raw === old || !this.#abort) { return; }
        this.#seen.set(attr, raw);
        const cb = (this as unknown as Record<string, unknown>)[key + "ValueChanged"];
        if (typeof cb === "function") {
          (cb as (v: unknown, o: unknown) => void).call(this, readValue(spec, raw), readValue(spec, old));
        }
      });
    });
    this.#observer.observe(this.element, { attributes: true, attributeFilter: Array.from(attrs.keys()) });
  }

  /** Once, when the element enters the page. */
  setup(): void {}
  /** Once, when the element really leaves the page (listeners are removed). */
  teardown(): void {}

  override connect(): void {
    if (!this.#abort) { this.ahStart(); }
    Controller.onConnect(this.element);
  }

  override disconnect(): void {
    const el = this.element;
    queueMicrotask(() => {
      // Gone from the page, or no longer this behaviour (its data-ah
      // changed); a move reconnects before this runs.
      const names = (el.getAttribute("data-ah") || "").split(/\s+/);
      if (!el.isConnected || names.indexOf(this.identifier) < 0) { this.ahStop(); }
    });
  }

  /** @internal set the behaviour up (the runtime calls it). */
  ahStart(): void {
    this.#abort = new AbortController();
    Controller.onStart(this.element);
    this.setup();
    this.#observe();
  }

  /** @internal tear the behaviour down (the runtime calls it). */
  ahStop(): void {
    if (!this.#abort) { return; }
    this.#abort.abort();
    this.#abort = null;
    if (this.#observer) {
      this.#observer.disconnect();
      this.#observer = null;
    }
    this.#seen.clear();
    this.teardown();
  }

  /** @internal set up and not torn down. */
  get ahLive(): boolean { return this.#abort !== null; }

  /** The AbortSignal of the current setup; aborted on teardown. */
  get signal(): AbortSignal | undefined { return this.#abort ? this.#abort.signal : undefined; }

  /** addEventListener that is removed on teardown. */
  listen<K extends keyof WindowEventMap>(target: Window, type: K, handler: Listener<WindowEventMap[K]>,
                                         options?: AddEventListenerOptions): void;
  listen<K extends keyof DocumentEventMap>(target: Document, type: K, handler: Listener<DocumentEventMap[K]>,
                                           options?: AddEventListenerOptions): void;
  listen<K extends keyof SVGElementEventMap>(target: SVGElement, type: K,
                                             handler: Listener<SVGElementEventMap[K]>,
                                             options?: AddEventListenerOptions): void;
  listen<K extends keyof HTMLElementEventMap>(target: Element, type: K,
                                              handler: Listener<HTMLElementEventMap[K]>,
                                              options?: AddEventListenerOptions): void;
  /** A component event ("ah:hover", ...): say its type, e.g. CustomEvent<number>. */
  listen<V extends Event = CustomEvent>(target: EventTarget, type: string, handler: Listener<V>,
                                        options?: AddEventListenerOptions): void;
  listen(target: EventTarget, type: string, handler: Listener<Event>, options?: AddEventListenerOptions): void {
    if (!this.#abort) { throw new Error("aihtml: listen() outside setup"); }
    target.addEventListener(type, handler, { ...options, signal: this.#abort.signal });
  }

  /** A delegated listener: handler(e, match) for events on the descendants
   *  of root (default the element) matching selector; `this' in a
   *  function handler is the match.
   *  Non-bubbling events (mouseenter, mouseleave) need mouseover/mouseout
   *  or a listener per element. */
  delegate<K extends keyof HTMLElementEventMap, M extends Element = HTMLElement>(
    type: K, selector: string, handler: (this: M, e: HTMLElementEventMap[K], match: M) => void,
    root?: Element | Document): void;
  delegate<V extends Event = CustomEvent, M extends Element = HTMLElement>(
    type: string, selector: string, handler: (this: M, e: V, match: M) => void,
    root?: Element | Document): void;
  delegate(type: string, selector: string, handler: (this: Element, e: Event, match: Element) => void,
           root?: Element | Document): void {
    const scope = root || this.element;
    this.listen(scope, type, (e: Event) => {
      const t = e.target as Element | null;
      const hit = t && t.closest ? t.closest(selector) : null;
      if (hit && scope.contains(hit)) { handler.call(hit, e, hit); }
    });
  }

  /** Fire a native, bubbling, cancelable CustomEvent on target (default
   *  the element); false when a listener cancelled it. */
  fire<D>(type: string, detail?: D, target?: EventTarget): boolean {
    return fire(target || this.element, type, detail);
  }
}
