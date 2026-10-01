// Behaviours: the Stimulus application, the registered controller classes,
// calls into them, and lazy loading of their chunks.
import { Application, defaultSchema } from "@hotwired/stimulus";
import type { Context, ControllerConstructor } from "@hotwired/stimulus";
import type { Behaviours as Catalog } from "../types/catalog";
import { Controller } from "./controller.ts";
import { one, roots, elements, withSelf } from "./dom.ts";
import type { Targets } from "./dom.ts";

/** A component chunk: () => import(chunk). */
export type Loader = () => Promise<unknown>;

/** What main.ts hands over, built from components/*.ts (vite.config.mjs):
 *  behaviour name -> loader, page function -> loader, and [selector,
 *  loader] pairs for files that act on attributes rather than on a
 *  data-ah root (tooltips, overlay triggers, validation). */
export interface Registry {
  behaviours: Record<string, Loader>;
  fns: Record<string, Loader>;
  triggers: [string, Loader][];
}

/** A behaviour name the server renders (data-ah="<name>"). */
export type BehaviourName = keyof Catalog;

type Method = (...args: never[]) => unknown;

/** A controller class for behaviour N: an AH.Controller with every method
 *  the catalog lists for N (tsc fails when one is missing). */
export type ControllerClass<N extends BehaviourName> = ControllerConstructor &
  (new (context: Context) => Controller<Element> & { [K in Catalog[N]]: Method });

/** A page-level function (toast, notify, ...), for call(Ctx, global, ...). */
export type PageFunction = (...args: never[]) => unknown;

type Callback = () => void;

// Stimulus reads data-ah (not data-controller), so the server's HTML stays
// the same. Stimulus actions are not used: they call a method with the
// event, where the components' methods take the server's arguments, and
// only reach an ancestor's controller. Bindings in the HTML are aihtml's
// own (data-ah-on, data-ah-on-client in runtime/actions.ts), so the action
// attribute is one no element carries (data-action is used by the
// components themselves).
const SCHEMA = { ...defaultSchema, controllerAttribute: "data-ah", actionAttribute: "data-ah-stimulus-action" };

export class Behaviours {
  #app: Application | null = null;
  #classes = new Map<string, ControllerConstructor>();
  #fns = new Map<string, PageFunction>();
  #lazy: Registry = { behaviours: {}, fns: {}, triggers: [] };
  #pending = new Map<string, Callback[]>();       // behaviour or "fn:<name>" -> waiting
  #connectWaiters = new Map<Element, Callback[]>();
  #loads = new Map<Loader, Promise<unknown>>();

  /** Hook: attach the action listeners of a root (set by core.ts). */
  onMount: (root: Element | Document) => void = () => {};

  constructor() {
    Controller.onConnect = (el) => { this.#connected(el); };
  }

  /** The Stimulus application, once started. */
  get app(): Application | null { return this.#app; }

  register<N extends BehaviourName>(name: N, klass: ControllerClass<N>): void {
    this.#classes.set(name, klass);
    if (this.#app) { this.#app.register(name, klass); }
    this.#release(name);
  }

  fn(name: string, f: PageFunction): void {
    this.#fns.set(name, f);
    this.#release("fn:" + name);
  }

  callFn(name: string, args: unknown[]): void {
    const run = () => { (this.#fns.get(name) as (...a: unknown[]) => unknown)(...args); };
    if (this.#fns.has(name)) {
      run();
    } else if (this.#lazy.fns[name]) {
      this.#whenDefined("fn:" + name, run);
      void this.loadChunk(this.#lazy.fns[name]);
    } else {
      throw new Error("no function " + name);
    }
  }

  /** The controller of el for behaviour name, while Stimulus has one. */
  controllerOf(el: Element, name: string): Controller<Element> | null {
    const c = this.#app ? this.#app.getControllerForElementAndIdentifier(el, name) : null;
    return c instanceof Controller ? c : null;
  }

  /** The controller of el while it is set up, else null. */
  liveController(el: Element): Controller<Element> | null {
    const name = el.getAttribute("data-ah") || "";
    const c = this.#classes.has(name) ? this.controllerOf(el, name) : null;
    return c && c.ahLive ? c : null;
  }

  /** True when el's behaviour is set up and follows its attributes
   *  (declares attrs), so a morph keeps it set up. */
  followsAttributes(el: Element): boolean {
    const c = this.liveController(el);
    return !!c && (c.constructor as typeof Controller).followsAttributes;
  }

  isMounted(el: Element): boolean {
    return el.hasAttribute("data-ah-mounted") || !!this.liveController(el);
  }

  #connected(el: Element): void {
    const cbs = this.#connectWaiters.get(el);
    if (cbs) {
      this.#connectWaiters.delete(el);
      cbs.forEach((cb) => { cb(); });
    }
  }

  /** Call cb once el's behaviour is usable: loaded and connected. Elements
   *  without a behaviour call it at once. */
  whenReady(el: Element, cb: Callback): void {
    const name = el.getAttribute("data-ah") || "";
    if (this.#classes.has(name)) {
      if (this.controllerOf(el, name)) {
        cb();
      } else {
        const cbs = this.#connectWaiters.get(el) || [];
        cbs.push(cb);
        this.#connectWaiters.set(el, cbs);
      }
    } else if (this.#lazy.behaviours[name]) {
      this.#whenDefined(name, () => { this.whenReady(el, cb); });
    } else {
      cb();
    }
  }

  /** Resolves when every component inside root is usable (its chunk
   *  loaded, its controller connected). Tests and page scripts await it
   *  after inserting HTML; the runtime itself queues calls instead. */
  ready(root?: Targets): Promise<void> {
    const node = one(root || document);
    if (!node) { return Promise.resolve(); }
    return Promise.all(withSelf(node, "[data-ah]").map((el) =>
      new Promise<void>((res) => { this.whenReady(el, res); }))).then(() => undefined);
  }

  /** Run a method of the behaviour an element carries (the server's
   *  aihtml_action:call/4, page scripts). A behaviour that is not loaded
   *  or connected yet runs the call once it is (the result is then
   *  undefined for the caller). */
  invoke(target: Targets, method: string, ...args: unknown[]): unknown {
    let result: unknown;
    elements(target).forEach((el) => {
      const name = el.getAttribute("data-ah") || "";
      const run = (): unknown => {
        const c = this.controllerOf(el, name) as unknown as Record<string, unknown> | null;
        const f = c && c[method];
        if (typeof f === "function") { return f.apply(c, args); }
        console.error("aihtml: no method " + method + " on", el);
        return undefined;
      };
      if (this.#classes.has(name) && !this.controllerOf(el, name)) {
        this.whenReady(el, run);
      } else if (this.#classes.has(name) || !this.#lazy.behaviours[name]) {
        result = run();
      } else {
        this.whenReady(el, run);
      }
    });
    return result;
  }

  /** Attach behaviours inside root (default the document): action
   *  listeners, controllers stopped by destroy set up again, missing
   *  chunks loaded. Native controllers of new elements connect on their
   *  own (await ready(root) to use them). Returns the (first) root. */
  mount(target?: Targets): Element | Document | undefined {
    const rs = roots(target || document);
    rs.forEach((root) => {
      this.onMount(root);
      withSelf(root, "[data-ah]").forEach((el) => {
        const name = el.getAttribute("data-ah") || "";
        const c = this.#classes.has(name) ? this.controllerOf(el, name) : null;
        if (c && !c.ahLive && el.isConnected) { c.ahStart(); }
      });
      this.scan(root);
    });
    return rs[0];
  }

  /** Detach the behaviours inside root (teardown now, listeners removed),
   *  before root is removed or re-mounted. */
  destroy(target: Targets): void {
    roots(target).forEach((root) => {
      withSelf(root, "[data-ah]").forEach((el) => {
        const c = this.liveController(el);
        if (c) { c.ahStop(); }
      });
    });
  }

  // ---- lazy loading ------------------------------------------------

  #release(key: string): void {
    const cbs = this.#pending.get(key) || [];
    this.#pending.delete(key);
    cbs.forEach((cb) => { cb(); });
  }

  #whenDefined(key: string, cb: Callback): void {
    const cbs = this.#pending.get(key) || [];
    cbs.push(cb);
    this.#pending.set(key, cbs);
    const loader = key.startsWith("fn:") ? undefined : this.#lazy.behaviours[key];
    if (loader) { void this.loadChunk(loader); }
  }

  loadChunk(loader: Loader): Promise<unknown> {
    let p = this.#loads.get(loader);
    if (!p) {
      p = loader().catch((err: unknown) => {
        this.#loads.delete(loader);
        console.error("aihtml: cannot load a component", err);
      });
      this.#loads.set(loader, p);
    }
    return p;
  }

  /** Load the chunks root needs: runs on start, after every mount, and
   *  for every DOM change. */
  scan(target?: Targets): void {
    const node = one(target || document);
    if (!node) { return; }
    withSelf(node, "[data-ah]").forEach((el) => {
      const name = el.getAttribute("data-ah") || "";
      const loader = this.#lazy.behaviours[name];
      if (!this.#classes.has(name) && loader) { void this.loadChunk(loader); }
    });
    this.#lazy.triggers.forEach(([sel, loader]) => {
      if ((node.nodeType === 1 && (node as Element).matches(sel)) || node.querySelector(sel)) {
        void this.loadChunk(loader);
      }
    });
  }

  /** Load every registered component (tests, pages that want no delay). */
  loadAll(): Promise<unknown[]> {
    const set = new Set<Loader>([...Object.values(this.#lazy.behaviours), ...Object.values(this.#lazy.fns),
                                 ...this.#lazy.triggers.map((t) => t[1])]);
    return Promise.all(Array.from(set, (l) => this.loadChunk(l)));
  }

  /** Start Stimulus with the registry; main.ts calls this once. */
  start(registry?: Registry): void {
    this.#lazy = registry || this.#lazy;
    const app = this.#app = Application.start(document.documentElement, SCHEMA);
    this.#classes.forEach((klass, name) => { app.register(name, klass); });
    new MutationObserver((records) => {
      records.forEach((r) => {
        if (r.type === "attributes") { this.scan(r.target as Element); }
        r.addedNodes.forEach((n) => { if (n.nodeType === 1) { this.scan(n as Element); } });
      });
    }).observe(document.documentElement,
               { childList: true, subtree: true, attributes: true, attributeFilter: ["data-ah"] });
    this.scan(document);
  }
}
