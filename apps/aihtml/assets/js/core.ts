/*!
 * core.ts: the client runtime for aihtml prefabs (main.ts is the bundle
 * entry, see designs/06-bundling.md). Behaviours are Stimulus controllers
 * on [data-ah="<name>"], loaded on demand. The parts are in runtime/;
 * this module wires them together into the AH object.
 *
 * The server renders all HTML. The runtime only adds behaviour to it:
 *
 *   AH.register(name, class extends AH.Controller {...})
 *                                     behaviour for [data-ah="<name>"]
 *   AH.invoke(el, method, ...args)    call a behaviour method
 *   AH.fn(name, f)                    page-level function (toast, ...)
 *   AH.mount(root) / AH.destroy(root) attach / detach behaviours in root
 *   AH.ready(root)                    promise: root's components are usable
 *   AH.swap(target, html, mode)       put server HTML into the page
 *   AH.theme.get() / .set(axis, v)    the four theme axes on <html>
 *   AH.fetch(el)                      run an element's data-ah-fetch
 *   AH.float(popup, anchor, opts)     pin a popup next to its anchor
 *   AH.vendor(name)                   load an optional third-party library
 *
 * Functions taking elements accept an element, a selector, an array of
 * elements or a jQuery-like object (runtime/dom.ts, Targets).
 *
 * Actions (runtime/actions.ts): elements with data-ah-on="event:token"
 * call Erlang; each event is one POST, the reply an AG-UI event stream of
 * DOM operations. Push (runtime/push.ts): data-ah-subscribe="token" opens
 * one EventSource per page. Round trips (runtime/fetch.ts): data-ah-fetch.
 *
 * Component behaviours live in assets/js/components/*.ts, one ES module
 * per component, one chunk each, loaded when the page first needs them.
 * They reach this object as window.AH (vite.config.mjs rewrites their
 * `import AH from "../core.ts"`), so no chunk imports the entry.
 *
 * Events: native, bubbling, cancelable CustomEvents; the data is e.detail.
 *   ah:theme {axis, value}                    on document, after a change
 *   ah:before-fetch {url, method}             on the element; cancelable
 *   ah:after-fetch {url}                      on the element
 *   ah:error {url, status, body} (fetch), {status} or {message, code}
 *            (action), {stream: true} (push, on document)
 *   the server's trigger op: {event} with detail = its Detail
 */
import { Actions } from "./runtime/actions.ts";
import type { Op } from "./runtime/actions.ts";
import { Behaviours } from "./runtime/behaviours.ts";
import type { BehaviourName, ControllerClass, PageFunction, Registry } from "./runtime/behaviours.ts";
import { Controller } from "./runtime/controller.ts";
import { one } from "./runtime/dom.ts";
import type { Root, Targets } from "./runtime/dom.ts";
import { Fetcher } from "./runtime/fetch.ts";
import { FloatingPopup } from "./runtime/float.ts";
import type { FloatHandle, FloatOptions } from "./runtime/float.ts";
import { setVal } from "./runtime/forms.ts";
import { SETTLE_MS, Swapper } from "./runtime/swap.ts";
import type { SwapMode } from "./runtime/swap.ts";
import { Theme } from "./runtime/theme.ts";
import { vendor } from "./runtime/vendor.ts";
import type { Application } from "@hotwired/stimulus";

export type { Op, AguiEvent, EventPayload, ActionError } from "./runtime/actions.ts";
export type { BehaviourName, ControllerClass, Loader, PageFunction, Registry } from "./runtime/behaviours.ts";
export type { Root, Targets } from "./runtime/dom.ts";
export type { FetchError, FetchEvent } from "./runtime/fetch.ts";
export type { FloatHandle, FloatOptions, Placement } from "./runtime/float.ts";
export type { SwapMode } from "./runtime/swap.ts";
export type { Axis, ThemeEvent, ThemeValues } from "./runtime/theme.ts";
export type { VendorLibrary, VendorName } from "./runtime/vendor.ts";
export { Controller };

/** A compiled shared template (templates/<name>.mustache). */
export type Template = (view: unknown) => string;

/** The AH object: window.AH, and the default export of this module. */
export interface AHApi {
  register<N extends BehaviourName>(name: N, klass: ControllerClass<N>): void;
  invoke(target: Targets, method: string, ...args: unknown[]): unknown;
  fn(name: string, f: PageFunction): void;
  mount(root?: Targets): Root | undefined;
  destroy(root: Targets): void;
  ready(root?: Targets): Promise<void>;
  swap(target: Targets, html: string, mode?: SwapMode | string): Node[];
  morph(target: Targets, html: string): void;
  apply(ops: Op[]): void;
  fetch(el: Targets): Promise<boolean>;
  float(popup: Targets, anchor: Targets, opts?: FloatOptions): FloatHandle;
  theme: Theme;
  vendor: typeof vendor;
  Controller: typeof Controller;
  start(registry?: Registry): void;
  loadAll(): Promise<unknown[]>;
  stimulus(): Application | null;
  /** Compiled shared templates, filled by the chunks that import them. */
  tpl: Record<string, Template>;
  /** Helpers some component libraries publish for page scripts and tests. */
  lib: Record<string, unknown>;
  settleDelay: number;
  NS: string;
  version: string;
}

const behaviours = new Behaviours();
const swapper = new Swapper(behaviours);
const fetcher = new Fetcher(behaviours, swapper);
const actions: Actions = new Actions(behaviours, swapper, () => AH);
const theme = new Theme();

Controller.onStart = (el) => { actions.listenFor(el); };
behaviours.onMount = (root) => { actions.listenFor(root); };

const AH: AHApi = {
  register: (name, klass) => { behaviours.register(name, klass); },
  invoke: (target, method, ...args) => behaviours.invoke(target, method, ...args),
  fn: (name, f) => { behaviours.fn(name, f); },
  mount: (root) => behaviours.mount(root),
  destroy: (root) => { behaviours.destroy(root); },
  ready: (root) => behaviours.ready(root),
  swap: (target, html, mode) => swapper.swap(target, html, mode),
  morph: (target, html) => {
    const el = one(target);
    if (el instanceof Element) { swapper.morph(el, html, true); }
  },
  apply: (ops) => { actions.apply(ops); },
  fetch: (el) => fetcher.fetch(el),
  float: (popup, anchor, opts) => new FloatingPopup(popup, anchor, opts),
  theme,
  vendor,
  Controller,
  start: (registry) => { behaviours.start(registry); },
  loadAll: () => behaviours.loadAll(),
  stimulus: () => behaviours.app,
  tpl: {},
  lib: {},
  settleDelay: SETTLE_MS,
  NS: ".ah",
  version: "0.4.0"
};

// The theme switcher (aihtml_theme:switcher/2): one select per axis
// (data-ah-axis), kept in step with <html>, also when the theme changes
// elsewhere (AH.theme.set, another switcher).
class ThemeSwitcher extends Controller {
  override setup(): void {
    this.sync();
    this.listen(document, "ah:theme", () => { this.sync(); });
    this.delegate<Event, HTMLSelectElement>("change", "[data-ah-axis]", (_e, sel) => {
      theme.set(sel.getAttribute("data-ah-axis") || "", sel.value);
    });
  }

  sync(): void {
    const current = theme.get() as Record<string, string | null>;
    this.element.querySelectorAll("[data-ah-axis]").forEach((sel) => {
      const v = current[sel.getAttribute("data-ah-axis") || ""];
      if (v) { setVal(sel, v); }
    });
  }
}
AH.register("theme-switcher", ThemeSwitcher);

window.addEventListener("popstate", (e) => {
  const st = e.state as { ah?: boolean } | null;
  if (st && st.ah) { window.location.reload(); }
});

// Once the document is parsed (and after main.ts has started Stimulus):
// mount the page and open the push stream.
function boot(): void {
  behaviours.mount(document);
  actions.push.sync();
}
if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", boot);
} else {
  setTimeout(boot);
}

export default AH;
