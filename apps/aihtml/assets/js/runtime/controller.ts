// The base class of every component behaviour (designs/06-bundling.md).
import { Controller as StimulusController } from "@hotwired/stimulus";
import { fire } from "./dom.ts";

type Listener<E extends Event> = (e: E) => void;

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
 */
export class Controller<E extends Element = HTMLElement> extends StimulusController<E> {
  /** Runtime hooks (set by core.ts): after setup starts, after connect. */
  static onStart: (el: Element) => void = () => {};
  static onConnect: (el: Element) => void = () => {};

  #abort: AbortController | null = null;

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
      if (!el.isConnected) { this.ahStop(); }
    });
  }

  /** @internal set the behaviour up (the runtime calls it). */
  ahStart(): void {
    this.#abort = new AbortController();
    Controller.onStart(this.element);
    this.setup();
  }

  /** @internal tear the behaviour down (the runtime calls it). */
  ahStop(): void {
    if (!this.#abort) { return; }
    this.#abort.abort();
    this.#abort = null;
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
