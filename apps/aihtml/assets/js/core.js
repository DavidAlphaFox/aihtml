/*!
 * core.js: the client runtime for aihtml prefabs (an ES module; main.js is
 * the bundle entry, see designs/06-bundling.md). Behaviours are Stimulus
 * controllers on [data-ah="<name>"], loaded on demand.
 *
 * The server renders all HTML. This file only adds behaviour to it:
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
 * elements or a jQuery-like object. AH.swap returns the inserted top-level
 * nodes (an array), AH.mount the root element; AH.destroy returns nothing.
 *
 * Actions: elements with data-ah-on="event:token" call Erlang. Each event
 * is one POST to <body data-ah-action>; the reply is an AG-UI event stream
 * whose CUSTOM "aihtml.ui" events carry DOM operations. The server keeps
 * no state between requests.
 *
 * Push: elements with data-ah-subscribe="token" make the page open one
 * EventSource to <body data-ah-events>; published operations arrive as
 * the same AG-UI events.
 *
 * Round trips: an element with data-ah-fetch="get|post|..." sends a
 * request to data-ah-url when data-ah-trigger fires (default: submit for
 * forms, change for inputs, click otherwise). The response is HTML that
 * replaces data-ah-target ("this" or a selector) according to
 * data-ah-swap (inner | outer | append | prepend | none), and the new
 * content is mounted. Requests carry the header "X-Aihtml: 1".
 *
 * Component behaviours live in assets/js/components/*.js, one ES module per
 * component; Vite bundles them into priv/static/js, one chunk per module,
 * loaded when the page first needs them (see lazy loading below).
 *
 * Events: native, bubbling, cancelable CustomEvents; the data is e.detail.
 *   ah:theme {axis, value}                    on document, after a change
 *   ah:before-fetch {url, method}             on the element; cancelable
 *   ah:after-fetch {url}                      on the element
 *   ah:error {url, status, body} (fetch), {status} or {message, code}
 *            (action), {stream: true} (push, on document)
 *   the server's trigger op: {event} with detail = its Detail
 */
import { Application, Controller, defaultSchema } from "@hotwired/stimulus";

const AH = (function () {
  "use strict";

  var NS = ".ah";

  // ------------------------------------------------------------------
  // Elements
  // ------------------------------------------------------------------

  // The elements x stands for: an element (or document), a selector, an
  // array or NodeList, or a jQuery-like object (anything with .jquery).
  function all(x) {
    if (!x) { return []; }
    if (typeof x === "string") { return Array.from(document.querySelectorAll(x)); }
    if (x.nodeType) { return [x]; }
    if (x.jquery || Array.isArray(x) || typeof x.length === "number") {
      return Array.prototype.filter.call(x, function (n) { return n && n.nodeType; });
    }
    return [];
  }

  function one(x) {
    return x && x.nodeType ? x : all(x)[0];
  }

  // node itself when it matches, then its descendants that do
  function withSelf(node, sel) {
    if (!node || !node.querySelectorAll) { return []; }
    var out = node.matches && node.matches(sel) ? [node] : [];
    Array.prototype.push.apply(out, node.querySelectorAll(sel));
    return out;
  }

  function fire(target, type, detail) {
    return target.dispatchEvent(
      new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
  }

  // One listener on document for events on elements matching selector,
  // like jQuery's delegated .on(type, selector, fn): handler(e, match)
  // runs for the target and every matching ancestor, innermost first,
  // until one stops propagation. As in jQuery, a click with a button
  // other than the primary one, and a click on a disabled element, is
  // not delegated.
  function delegateDocument(type, selector, handler) {
    document.addEventListener(type, function (e) {
      if (type === "click" && e.button >= 1) { return; }
      var n = e.target && e.target.nodeType === 1 ? e.target
            : e.target && e.target.parentElement;
      for (n = n && n.closest(selector); n; n = n.parentElement && n.parentElement.closest(selector)) {
        if (type === "click" && n.disabled === true) { continue; }
        handler(e, n);
        if (e.cancelBubble) { return; }
      }
    });
  }

  // ------------------------------------------------------------------
  // Behaviours
  // ------------------------------------------------------------------

  // Behaviours are Stimulus controllers. Stimulus is configured to read
  // data-ah (not data-controller), so the server's HTML stays the same,
  // and data-ah-do for in-browser actions (data-action is used by the
  // components themselves).
  var app = null;
  var SCHEMA = Object.assign({}, defaultSchema, {
    controllerAttribute: "data-ah",
    actionAttribute: "data-ah-do"
  });

  // ---- Native controllers (designs/06-bundling.md, phase 2) ----
  //
  // AH.register(name, class extends AH.Controller { ... }) registers a
  // Stimulus controller written without jQuery. AH.Controller adds what
  // every aihtml component needs on top of Stimulus:
  //   setup() / teardown()   run once when the element enters the page and
  //                          once when it really leaves it; a move (morph,
  //                          preserve) disconnects and reconnects without
  //                          running them again, so the component keeps
  //                          its state. AH.destroy(el) runs teardown at
  //                          once, AH.mount(el) runs setup again (morph
  //                          does both for a component whose DOM changed).
  //   this.listen(target, type, handler[, options])
  //                          addEventListener that is removed on teardown
  //                          (one AbortController per element)
  //   this.fire(type, detail[, target])
  //                          a native, bubbling, cancelable CustomEvent;
  //                          the server's on(Event, ...) and Stimulus
  //                          actions both see it
  //   this.delegate(type, selector, handler[, root])
  //                          a delegated listener (handler(e, match))
  //   this.signal            the AbortSignal of this setup (for fetch)
  // Public methods of the controller are what aihtml_action:call/4 and
  // AH.invoke(el, method, ...args) reach.
  var classes = {};
  var connectWaiters = new Map();

  class AHController extends Controller {
    connect() {
      if (!this._ah) { this._ahStart(); }
      connected(this.element);
    }
    disconnect() {
      var self = this, el = this.element;
      queueMicrotask(function () {
        if (!el.isConnected) { self._ahStop(); }
      });
    }
    _ahStart() {
      this._ah = new AbortController();
      listenFor(this.element);
      if (this.setup) { this.setup(); }
    }
    _ahStop() {
      if (!this._ah) { return; }
      this._ah.abort();
      this._ah = null;
      if (this.teardown) { this.teardown(); }
    }
    get signal() { return this._ah ? this._ah.signal : undefined; }
    listen(target, type, handler, options) {
      target.addEventListener(type, handler,
                              Object.assign({}, options || {}, { signal: this._ah.signal }));
    }
    fire(type, detail, target) {
      return fire(target || this.element, type, detail);
    }
    // Delegated listener: handler(e, match) for events on descendants of
    // root (default the element) matching selector, like jQuery's
    // .on(type, selector, fn). Non-bubbling events (mouseenter,
    // mouseleave) need mouseover/mouseout or a listener per element.
    delegate(type, selector, handler, root) {
      var scope = root || this.element;
      this.listen(scope, type, function (e) {
        var hit = e.target && e.target.closest ? e.target.closest(selector) : null;
        if (hit && scope.contains(hit)) { handler.call(hit, e, hit); }
      });
    }
  }

  function register(name, Klass) {
    classes[name] = Klass;
    if (app) { app.register(name, Klass); }
    var cbs = pending[name] || [];
    delete pending[name];
    cbs.forEach(function (cb) { cb(); });
  }

  function controllerOf(el, name) {
    return app ? app.getControllerForElementAndIdentifier(el, name) : null;
  }

  // The native controller of el while it is set up, else null.
  function liveController(el) {
    var name = el.getAttribute("data-ah");
    var c = classes[name] ? controllerOf(el, name) : null;
    return c && c._ah ? c : null;
  }

  function isMounted(el) {
    return el.hasAttribute("data-ah-mounted") || !!liveController(el);
  }

  function connected(el) {
    var cbs = connectWaiters.get(el);
    if (cbs) {
      connectWaiters.delete(el);
      cbs.forEach(function (cb) { cb(); });
    }
  }

  // Call cb once el's behaviour is usable: loaded and connected. Elements
  // without a behaviour call it at once.
  function whenReady(el, cb) {
    var name = el.getAttribute("data-ah");
    if (classes[name]) {
      if (controllerOf(el, name)) {
        cb();
      } else {
        var cbs = connectWaiters.get(el) || [];
        cbs.push(cb);
        connectWaiters.set(el, cbs);
      }
    } else if (lazy.behaviours[name]) {
      whenDefined(name, function () { whenReady(el, cb); });
    } else {
      cb();
    }
  }

  // A promise that resolves when every component inside root is usable
  // (its chunk loaded, its controller connected). Tests and page scripts
  // await it after inserting HTML; the runtime itself queues calls instead.
  function ready(root) {
    var node = one(root || document);
    if (!node) { return Promise.resolve(); }
    return Promise.all(withSelf(node, "[data-ah]").map(function (el) {
      return new Promise(function (res) { whenReady(el, res); });
    })).then(function () { return undefined; });
  }

  // Run a method of the behaviour an element carries. Used by the server's
  // aihtml_action:call/4 and by pages. A behaviour that is not loaded yet
  // is loaded first, and the call runs once it is (the result is then
  // undefined for the caller).
  function invoke(target, method) {
    var args = Array.prototype.slice.call(arguments, 2);
    var result;
    all(target).forEach(function (el) {
      var name = el.getAttribute("data-ah");
      var run = function () {
        var c = controllerOf(el, name);
        if (c && typeof c[method] === "function") {
          return c[method].apply(c, args);
        }
        console.error("aihtml: no method " + method + " on", el);
        return undefined;
      };
      if (classes[name] && !controllerOf(el, name)) {
        whenReady(el, run);
      } else if (classes[name] || !lazy.behaviours[name]) {
        result = run();
      } else {
        whenReady(el, run);
      }
    });
    return result;
  }

  // Page-level functions (toast, notify, ...), for call(Ctx, global, ...).
  var fns = {};
  function fn(name, f) {
    fns[name] = f;
    var cbs = pending["fn:" + name] || [];
    delete pending["fn:" + name];
    cbs.forEach(function (cb) { cb(); });
  }

  function callFn(name, args) {
    if (fns[name]) {
      fns[name].apply(null, args);
    } else if (lazy.fns[name]) {
      whenDefined("fn:" + name, function () { fns[name].apply(null, args); });
      loadChunk(lazy.fns[name]);
    } else {
      throw new Error("no function " + name);
    }
  }

  // Attach behaviours inside root (default the document): action listeners
  // for the events root binds, controllers stopped by AH.destroy set up
  // again, missing chunks
  // loaded. Native controllers of new elements connect on their own
  // (await AH.ready(root) to use them). Returns the (first) root element.
  function mount(rootEl) {
    var roots = all(rootEl || document);
    roots.forEach(function (root) {
      listenFor(root);
      withSelf(root, "[data-ah]").forEach(function (el) {
        var name = el.getAttribute("data-ah");
        var c = classes[name] ? controllerOf(el, name) : null;
        if (c && !c._ah && el.isConnected) { c._ahStart(); }
      });
      scan(root);
    });
    return roots[0];
  }

  // Detach the behaviours inside root (teardown now, listeners removed),
  // before root is removed or re-mounted.
  function destroy(rootEl) {
    all(rootEl).forEach(function (root) {
      withSelf(root, "[data-ah]").forEach(function (el) {
        var c = liveController(el);
        if (c) { c._ahStop(); }
      });
    });
  }

  // ------------------------------------------------------------------
  // Lazy loading
  // ------------------------------------------------------------------
  //
  // main.js hands over the registry built from components/*.js (see
  // vite.config.mjs): behaviour name -> loader, page function -> loader,
  // and [selector, loader] pairs for files that act on attributes rather
  // than on a data-ah root (tooltips, overlay triggers, validation). A
  // loader is () => import(chunk). scan(root) loads what root needs; it
  // runs on start, after every mount, and for every DOM change.
  var lazy = { behaviours: {}, fns: {}, triggers: [] };
  var pending = {};
  var loads = new Map();

  function whenDefined(key, cb) {
    (pending[key] = pending[key] || []).push(cb);
    var name = key.indexOf("fn:") === 0 ? null : key;
    if (name && lazy.behaviours[name]) { loadChunk(lazy.behaviours[name]); }
  }

  function loadChunk(loader) {
    if (!loads.has(loader)) {
      loads.set(loader, loader().catch(function (err) {
        loads.delete(loader);
        console.error("aihtml: cannot load a component", err);
      }));
    }
    return loads.get(loader);
  }

  function scan(root) {
    var node = one(root || document);
    if (!node || !node.querySelectorAll) { return; }
    withSelf(node, "[data-ah]").forEach(function (el) {
      var name = el.getAttribute("data-ah");
      if (!classes[name] && lazy.behaviours[name]) {
        loadChunk(lazy.behaviours[name]);
      }
    });
    lazy.triggers.forEach(function (t) {
      if ((node.matches && node.matches(t[0])) || node.querySelector(t[0])) { loadChunk(t[1]); }
    });
  }

  // Load every registered component (tests, pages that want no delay).
  function loadAll() {
    var set = new Set();
    Object.keys(lazy.behaviours).forEach(function (k) { set.add(lazy.behaviours[k]); });
    Object.keys(lazy.fns).forEach(function (k) { set.add(lazy.fns[k]); });
    lazy.triggers.forEach(function (t) { set.add(t[1]); });
    return Promise.all(Array.from(set).map(loadChunk));
  }

  // Start Stimulus with the registry; main.js calls this once.
  function start(registry) {
    lazy = registry || lazy;
    app = Application.start(document.documentElement, SCHEMA);
    Object.keys(classes).forEach(function (n) { app.register(n, classes[n]); });
    new MutationObserver(function (records) {
      records.forEach(function (r) {
        if (r.type === "attributes") { scan(r.target); }
        r.addedNodes.forEach(function (n) { if (n.nodeType === 1) { scan(n); } });
      });
    }).observe(document.documentElement,
               { childList: true, subtree: true, attributes: true, attributeFilter: ["data-ah"] });
    scan(document);
  }

  // ------------------------------------------------------------------
  // Floating popups
  // ------------------------------------------------------------------
  //
  // AH.float(popup, anchor, opts) pins a popup next to its anchor with
  // position: fixed, so no ancestor with overflow: hidden (a card, a panel,
  // a scroll area) can clip it, and keeps it there while the page scrolls
  // or resizes. It flips to the other side when the preferred one lacks
  // room and stays inside the viewport. Returns {update(), stop()}; call
  // stop() when the popup closes.
  //
  //   opts.placement  "bottom" (default) | "top" | "right" | "left"
  //   opts.align      "start" (default) | "end" | "center"
  //   opts.offset     gap in px, default 4
  //   opts.matchWidth at least the anchor's width (for lists), default false

  var floating = [];

  function floatPopup(popup, anchor, opts) {
    popup = one(popup);
    anchor = one(anchor);
    opts = Object.assign({ placement: "bottom", align: "start", offset: 4, matchWidth: false }, opts);
    var handle = {
      update: function () { place(popup, anchor, opts); },
      stop: function () {
        floating = floating.filter(function (h) { return h !== handle; });
        popup.style.position = popup.style.top = popup.style.left = "";
        popup.style.right = popup.style.bottom = popup.style.minWidth = "";
        popup.removeAttribute("data-ah-placement");
        if (!floating.length) {
          window.removeEventListener("resize", updateAll);
          window.removeEventListener("scroll", updateAll, true);
        }
      }
    };
    if (!floating.length) {
      window.addEventListener("resize", updateAll);
      window.addEventListener("scroll", updateAll, true);
    }
    floating.push(handle);
    handle.update();
    return handle;
  }

  function updateAll() {
    floating.forEach(function (h) { h.update(); });
  }

  function place(popup, anchor, opts) {
    if (!document.contains(anchor)) { return; }
    var a = anchor.getBoundingClientRect();
    popup.style.position = "fixed";
    popup.style.right = popup.style.bottom = "auto";
    if (opts.matchWidth) { popup.style.minWidth = a.width + "px"; }
    popup.style.top = "0px";
    popup.style.left = "0px";
    var p = popup.getBoundingClientRect();
    var vw = document.documentElement.clientWidth, vh = document.documentElement.clientHeight;
    var gap = opts.offset, side = opts.placement, top, left;
    var room = { bottom: vh - a.bottom, top: a.top, right: vw - a.right, left: a.left };
    var need = (side === "bottom" || side === "top") ? p.height + gap : p.width + gap;
    var other = { bottom: "top", top: "bottom", right: "left", left: "right" }[side];
    if (room[side] < need && room[other] > room[side]) { side = other; }
    if (side === "bottom" || side === "top") {
      top = side === "bottom" ? a.bottom + gap : a.top - gap - p.height;
      left = opts.align === "end" ? a.right - p.width
           : opts.align === "center" ? a.left + (a.width - p.width) / 2 : a.left;
    } else {
      left = side === "right" ? a.right + gap : a.left - gap - p.width;
      top = opts.align === "end" ? a.bottom - p.height
          : opts.align === "center" ? a.top + (a.height - p.height) / 2 : a.top;
    }
    left = Math.max(4, Math.min(left, vw - p.width - 4));
    top = Math.max(4, Math.min(top, vh - p.height - 4));
    popup.style.top = Math.round(top) + "px";
    popup.style.left = Math.round(left) + "px";
    popup.setAttribute("data-ah-placement", side);
  }

  // ------------------------------------------------------------------
  // Theme: the four axes
  // ------------------------------------------------------------------

  var AXES = {
    appearance: "data-theme",
    palette: "data-palette",
    typography: "data-typography",
    skin: "data-skin"
  };
  var STORE = "aihtml.theme";

  function load() {
    try {
      return JSON.parse(window.localStorage.getItem(STORE) || "{}");
    } catch (e) {
      return {};
    }
  }

  function save(t) {
    try {
      window.localStorage.setItem(STORE, JSON.stringify(t));
    } catch (e) {
      /* private mode or storage disabled: the choice lasts for this page */
    }
  }

  var theme = {
    axes: AXES,
    get: function () {
      var out = {};
      var html = document.documentElement;
      Object.keys(AXES).forEach(function (axis) {
        out[axis] = html.getAttribute(AXES[axis]);
      });
      return out;
    },
    set: function (axis, value) {
      if (!AXES[axis]) {
        throw new Error("unknown theme axis: " + axis);
      }
      document.documentElement.setAttribute(AXES[axis], value);
      var t = load();
      t[axis] = value;
      save(t);
      fire(document, "ah:theme", { axis: axis, value: value });
    },
    reset: function () {
      save({});
    }
  };

  // The theme switcher (aihtml_theme:switcher/2): one select per axis
  // (data-ah-axis), kept in step with <html>, also when the theme changes
  // elsewhere (AH.theme.set, another switcher).
  register("theme-switcher", class extends AHController {
    setup() {
      this.sync();
      this.listen(document, "ah:theme", () => this.sync());
      this.delegate("change", "[data-ah-axis]", (e, sel) => {
        theme.set(sel.getAttribute("data-ah-axis"), sel.value);
      });
    }
    sync() {
      var current = theme.get();
      this.element.querySelectorAll("[data-ah-axis]").forEach(function (sel) {
        var v = current[sel.getAttribute("data-ah-axis")];
        if (v) { setVal(sel, v); }
      });
    }
  });

  // ------------------------------------------------------------------
  // Form values
  // ------------------------------------------------------------------

  // A control's value, as jQuery's .val() reads it: an array for a
  // multiple select, the value otherwise.
  function valueOf(el) {
    if (el.tagName === "SELECT" && el.multiple) {
      return Array.from(el.selectedOptions).map(function (o) { return o.value; });
    }
    return el.value;
  }

  // Set a control's value, as jQuery's .val(v) does: a select selects the
  // matching option(s) (none when nothing matches), a checkbox or radio
  // given an array is checked when its value is in it.
  function setVal(el, v) {
    if (el.tagName === "SELECT") {
      var vals = [].concat(v === null || v === undefined ? [] : v).map(String);
      var hits = Array.from(el.options).filter(function (o) { return vals.indexOf(o.value) >= 0; });
      if (!el.multiple) { hits = hits.slice(-1); }       // the last one, as in jQuery
      // (deselecting the chosen option of a single select selects the
      // first one again, so select the hits instead of reading back)
      Array.from(el.options).forEach(function (o) { o.selected = hits.indexOf(o) >= 0; });
      if (!hits.length) { el.selectedIndex = -1; }
    } else if (Array.isArray(v) && (el.type === "checkbox" || el.type === "radio")) {
      el.checked = v.map(String).indexOf(el.value) >= 0;
    } else {
      el.value = v === null || v === undefined ? "" : String(v);
    }
  }

  // The successful controls of a form, as [name, value] pairs, like
  // jQuery's serializeArray: named, enabled input/select/textarea, no
  // file or button inputs, checkboxes and radios only when checked.
  function formFields(form) {
    var out = [];
    Array.from(form.elements).forEach(function (el) {
      if (!el.name || el.matches(":disabled") || !/^(INPUT|SELECT|TEXTAREA)$/.test(el.tagName) ||
          /^(submit|button|image|reset|file)$/i.test(el.type) ||
          ((el.type === "checkbox" || el.type === "radio") && !el.checked) ||
          (el.tagName === "SELECT" && !el.multiple && el.selectedIndex < 0)) {
        return;
      }
      [].concat(valueOf(el)).forEach(function (v) {
        out.push([el.name, String(v).replace(/\r?\n/g, "\r\n")]);
      });
    });
    return out;
  }

  // ------------------------------------------------------------------
  // Server round trips
  // ------------------------------------------------------------------

  function defaultTrigger(el) {
    if (el.tagName === "FORM") {
      return "submit";
    }
    if (/^(INPUT|SELECT|TEXTAREA)$/.test(el.tagName)) {
      return "change";
    }
    return "click";
  }

  // A form sends its fields. Any other element sends its own name=value;
  // an unchecked checkbox or radio sends the name with an empty value so
  // the server sees the change.
  function payload(el) {
    if (el.tagName === "FORM") {
      return new URLSearchParams(formFields(el)).toString();
    }
    if (!el.name) {
      return "";
    }
    var off = (el.type === "checkbox" || el.type === "radio") && !el.checked;
    return new URLSearchParams([[el.name, off ? "" : String(valueOf(el))]]).toString();
  }

  // ------------------------------------------------------------------
  // Swapping server HTML into the page
  // ------------------------------------------------------------------
  //
  // Modes: inner (default) | outer | append | prepend | none, and the two
  // morph modes, which patch the existing DOM towards the new HTML instead
  // of replacing it: morph (the target element itself) and morph_inner
  // (its children). Morphing keeps every node that survives, so focus,
  // caret, scroll position, open popups and component state stay put.
  //
  // Every mode restores focus afterwards: the focused element is found
  // again by id and its selection is put back.
  //
  // swap(target, html, mode) returns the inserted top-level nodes (an
  // array; empty for morph, which mounts what it adds itself, and none).
  // Scripts in the HTML are dropped, as with jQuery's parseHTML.

  function swap(target, html, mode) {
    var focus = captureFocus();
    var added = doSwap(all(target), html, mode);
    restoreFocus(focus);
    return added;
  }

  var EXECUTABLE = /^$|^module$|\/(?:java|ecma)script/i;

  function parseHTML(html) {
    var tpl = document.createElement("template");
    tpl.innerHTML = html;
    tpl.content.querySelectorAll("script").forEach(function (s) {
      if (EXECUTABLE.test(s.type)) { s.remove(); }
    });
    return Array.from(tpl.content.childNodes);
  }

  function doSwap(targets, html, mode) {
    if (mode === "morph" || mode === "morph_inner") {
      targets.forEach(function (t) { morph(t, html, mode === "morph"); });
      return [];
    }
    if (mode === "none" || !targets.length) {
      return [];
    }
    var nodes = parseHTML(html);
    var pantry = stashPreserved(nodes);
    var settle = prepareSettle(nodes);
    var added = insert(targets, nodes, mode);
    restorePreserved(pantry);
    finishSettle(targets, added.filter(function (n) { return n.nodeType === 1; }), settle);
    return added;
  }

  // With several targets, all but the last get a copy of the nodes (as
  // jQuery's manipulation methods do). Returns every inserted node.
  function insert(targets, nodes, mode) {
    var added = [];
    targets.forEach(function (t, i) {
      var these = i === targets.length - 1 ? nodes
        : nodes.map(function (n) { return n.cloneNode(true); });
      Array.prototype.push.apply(added, these);
      switch (mode) {
        case "outer":
          destroy(t);
          t.replaceWith.apply(t, these);
          break;
        case "append":
          t.append.apply(t, these);
          break;
        case "prepend":
          t.prepend.apply(t, these);
          break;
        default:
          destroy(Array.from(t.children));
          t.replaceChildren.apply(t, these);
          break;
      }
    });
    return added;
  }

  // ---- preserve -----------------------------------------------------
  //
  // An element with data-ah-preserve and an id is never replaced: when new
  // content brings an element with the same id, the existing one (a
  // playing video, an editor with unsaved text, a mounted component) is
  // moved into its place and the new copy is dropped. Moves use
  // moveBefore where the browser has it, which keeps iframes and media
  // running.

  function moveTo(parent, node, before) {
    if (parent.moveBefore && node.isConnected && parent.isConnected) {
      parent.moveBefore(node, before || null);
    } else {
      parent.insertBefore(node, before || null);
    }
  }

  function stashPreserved(nodes) {
    var found = [];
    nodes.forEach(function (n) {
      withSelf(n, "[data-ah-preserve][id]").forEach(function (ph) {
        var old = document.getElementById(ph.id);
        if (old && old !== ph && old.hasAttribute("data-ah-preserve")) {
          found.push({ placeholder: ph, el: old });
        }
      });
    });
    if (!found.length) {
      return found;
    }
    var pantry = document.getElementById("ah-preserve-pantry");
    if (!pantry) {
      pantry = document.createElement("div");
      pantry.id = "ah-preserve-pantry";
      pantry.hidden = true;
      document.body.appendChild(pantry);
    }
    found.forEach(function (f) { moveTo(pantry, f.el); });
    return found;
  }

  function restorePreserved(found) {
    found.forEach(function (f) {
      var ph = f.placeholder;
      if (ph.parentNode) {
        moveTo(ph.parentNode, f.el, ph);
        ph.parentNode.removeChild(ph);
      }
    });
  }

  // ---- settle -------------------------------------------------------
  //
  // After a swap the new top-level elements carry the class ah-added and
  // the swap target ah-settling, both removed SETTLE_MS later, so CSS can
  // animate what just arrived (.ah-added { opacity: 0 } plus a transition).
  // An element whose id already existed first takes the old element's
  // class, style, width and height, and gets its new ones after the delay:
  // a class or style change between the two renders becomes a CSS
  // transition. Component roots (data-ah) are left out; their behaviours
  // read the real attributes on init.

  var SETTLE_MS = 20;
  var SETTLE_ATTRS = ["class", "style", "width", "height"];

  function prepareSettle(nodes) {
    var list = [];
    nodes.forEach(function (n) {
      withSelf(n, "[id]").forEach(function (el) {
        var old = document.getElementById(el.id);
        if (!old || old === el || el.hasAttribute("data-ah") ||
            el.hasAttribute("data-ah-preserve")) {
          return;
        }
        var saved = {};
        SETTLE_ATTRS.forEach(function (a) {
          saved[a] = el.getAttribute(a);
          var v = old.getAttribute(a);
          if (v === null) { el.removeAttribute(a); } else { el.setAttribute(a, v); }
        });
        list.push({ el: el, saved: saved });
      });
    });
    return list;
  }

  function finishSettle(targets, added, list) {
    added.forEach(function (el) { el.classList.add("ah-added"); });
    targets.forEach(function (el) { el.classList.add("ah-settling"); });
    setTimeout(function () {
      list.forEach(function (x) {
        SETTLE_ATTRS.forEach(function (a) {
          if (x.saved[a] === null) { x.el.removeAttribute(a); } else { x.el.setAttribute(a, x.saved[a]); }
        });
      });
      dropClass(added, "ah-added");
      dropClass(targets, "ah-settling");
    }, SETTLE_MS);
  }

  // Remove a transient class without leaving class="" behind.
  function dropClass(els, cls) {
    els.forEach(function (el) {
      el.classList.remove(cls);
      if (el.getAttribute("class") === "") { el.removeAttribute("class"); }
    });
  }

  function captureFocus() {
    var a = document.activeElement;
    if (!a || a === document.body || !a.id) {
      return null;
    }
    var f = { id: a.id, start: null, end: null, dir: null };
    try {                          // throws for inputs without a caret
      f.start = a.selectionStart;
      f.end = a.selectionEnd;
      f.dir = a.selectionDirection;
    } catch (e) { /* no selection to keep */ }
    return f;
  }

  function restoreFocus(f) {
    if (!f) {
      return;
    }
    var el = document.getElementById(f.id);
    if (!el) {
      return;
    }
    if (el !== document.activeElement) {
      el.focus({ preventScroll: true });
    }
    if (f.start !== null && el.setSelectionRange) {
      try { el.setSelectionRange(f.start, f.end, f.dir || "none"); } catch (e) { /* ignore */ }
    }
  }

  // ---- morph --------------------------------------------------------
  //
  // Children are matched by id first, otherwise by position when the
  // node type and tag agree. Attributes are synced, except data-ah-mounted.
  // Form controls take the server's value unless they have the focus,
  // where the user is typing. A mounted component whose subtree changed is
  // re-initialised on its existing nodes (destroy, then mount: teardown
  // and setup for a native controller); added nodes are mounted; removed
  // ones are destroyed first.

  function morph(target, html, outer) {
    var tpl = document.createElement("template");
    tpl.innerHTML = html;
    var ctx = { added: [], changed: [] };
    if (outer) {
      var roots = elementChildren(tpl.content);
      if (roots.length !== 1) {
        throw new Error("aihtml: morph needs exactly one root element");
      }
      morphNode(target, roots[0], ctx);
    } else {
      morphChildren(target, tpl.content, ctx);
    }
    // Re-initialise only the outermost changed components: destroy and
    // mount already cover the components nested inside them.
    ctx.changed.filter(function (el) {
      return !ctx.changed.some(function (other) { return other !== el && other.contains(el); });
    }).forEach(function (el) {
      if (document.contains(el) && isMounted(el)) {
        destroy(el);
        mount(el);
      }
    });
    var added = ctx.added.filter(function (el) {
      return el.nodeType === 1 && document.contains(el);
    });
    added.forEach(function (el) { mount(el); });
    finishSettle([target], added, []);
  }

  function elementChildren(node) {
    return Array.prototype.filter.call(node.childNodes, function (n) {
      return n.nodeType === 1 ||
        (n.nodeType === 3 && n.nodeValue.trim() !== "");
    });
  }

  function sameKind(a, b) {
    return a.nodeType === b.nodeType &&
      (a.nodeType !== 1 || a.tagName === b.tagName);
  }

  // Returns true when anything under `old` changed.
  function morphNode(old, neu, ctx) {
    if (old.nodeType === 1 && old.id && old.hasAttribute("data-ah-preserve")) {
      return false;                 // preserved: never patched
    }
    if (!sameKind(old, neu)) {
      var fresh = document.importNode(neu, true);
      if (old.nodeType === 1) { destroy(old); }
      old.parentNode.replaceChild(fresh, old);
      ctx.added.push(fresh);
      return true;
    }
    if (old.nodeType !== 1) {
      if (old.nodeValue !== neu.nodeValue) {
        old.nodeValue = neu.nodeValue;
        return true;
      }
      return false;
    }
    var changed = syncAttributes(old, neu);
    if (old.tagName === "TEXTAREA") {
      changed = syncValue(old, neu.textContent) || changed;
    } else {
      changed = morphChildren(old, neu, ctx) || changed;
    }
    if (old.tagName === "INPUT") {
      changed = syncInput(old, neu) || changed;
    } else if (old.tagName === "SELECT" && old !== document.activeElement) {
      Array.prototype.forEach.call(old.options, function (o) { o.selected = o.hasAttribute("selected"); });
    }
    if (changed && isMounted(old) && ctx.changed.indexOf(old) < 0) {
      ctx.changed.push(old);
    }
    return changed;
  }

  function morphChildren(oldParent, newParent, ctx) {
    var changed = false;
    var newKids = Array.prototype.slice.call(newParent.childNodes);
    var pos = 0;
    newKids.forEach(function (nk) {
      var cur = oldParent.childNodes[pos] || null;
      var match = null;
      if (nk.nodeType === 1 && nk.id) {
        var byId = null;
        for (var i = pos; i < oldParent.childNodes.length; i++) {
          var c = oldParent.childNodes[i];
          if (c.nodeType === 1 && c.id === nk.id) { byId = c; break; }
        }
        match = byId && sameKind(byId, nk) ? byId : null;
      } else if (cur && sameKind(cur, nk) && !(cur.nodeType === 1 && cur.id)) {
        match = cur;
      }
      if (match) {
        if (match !== cur) {
          oldParent.insertBefore(match, cur);
          changed = true;
        }
        changed = morphNode(match, nk, ctx) || changed;
      } else {
        var fresh = document.importNode(nk, true);
        oldParent.insertBefore(fresh, cur);
        ctx.added.push(fresh);
        changed = true;
      }
      pos++;
    });
    while (oldParent.childNodes.length > pos) {
      var gone = oldParent.childNodes[pos];
      if (gone.nodeType === 1) { destroy(gone); }
      oldParent.removeChild(gone);
      changed = true;
    }
    return changed;
  }

  function syncAttributes(old, neu) {
    var changed = false;
    Array.prototype.forEach.call(neu.attributes, function (a) {
      if (old.getAttribute(a.name) !== a.value) {
        old.setAttribute(a.name, a.value);
        changed = true;
      }
    });
    Array.prototype.slice.call(old.attributes).forEach(function (a) {
      if (a.name !== "data-ah-mounted" && !neu.hasAttribute(a.name)) {
        old.removeAttribute(a.name);
        changed = true;
      }
    });
    return changed;
  }

  // The focused control keeps what the user is typing.
  function syncValue(el, value) {
    if (el === document.activeElement || el.value === value) {
      return false;
    }
    el.value = value;
    return true;
  }

  function syncInput(old, neu) {
    if (old.type === "checkbox" || old.type === "radio") {
      var on = neu.hasAttribute("checked");
      if (old !== document.activeElement && old.checked !== on) {
        old.checked = on;
        return true;
      }
      return false;
    }
    return syncValue(old, neu.getAttribute("value") || "");
  }

  // ---- request state ------------------------------------------------
  //
  // While a request runs: aria-busy on the element, the class ah-request
  // on it and on its indicators (data-ah-indicator), and the elements named
  // by data-ah-disable are disabled. Selectors may be "this" or
  // "closest <selector>". Overlapping requests are counted, so the first
  // one to finish does not clear the others' state.

  function resolve(el, sel) {
    if (!sel) { return []; }
    if (sel === "this") { return [el]; }
    var m = /^closest\s+(.+)$/.exec(sel);
    if (m) {
      var c = el.closest(m[1]);
      return c ? [c] : [];
    }
    return Array.from(document.querySelectorAll(sel));
  }

  var counts = new WeakMap();        // el -> {class or "disabled": n, wasDisabled}

  function bump(els, cls, by) {
    els.forEach(function (el) {
      var st = counts.get(el) || {};
      counts.set(el, st);
      var n = (st[cls] || 0) + by;
      st[cls] = n;
      if (cls === "disabled") {
        if (n > 0 && by > 0 && n === 1) {
          st.wasDisabled = el.disabled;
          el.disabled = true;
        } else if (n === 0) {
          el.disabled = !!st.wasDisabled;
        }
      } else {
        el.classList.toggle(cls, n > 0);
      }
    });
  }

  function requestStart(el) {
    var ind = Array.from(new Set([el].concat(resolve(el, el.getAttribute("data-ah-indicator")))));
    var dis = resolve(el, el.getAttribute("data-ah-disable"));
    el.setAttribute("aria-busy", "true");
    bump(ind, "ah-request", 1);
    bump(dis, "disabled", 1);
    var ended = false;
    return function () {
      if (ended) { return; }
      ended = true;
      bump(ind, "ah-request", -1);
      bump(dis, "disabled", -1);
      if (!el.classList.contains("ah-request")) { el.removeAttribute("aria-busy"); }
    };
  }

  // Run el's data-ah-fetch round trip. Returns a promise that resolves to
  // true once the response is swapped in, false when the request was
  // cancelled (data-ah-confirm, ah:before-fetch prevented) or failed
  // (ah:error). GET and HEAD send the payload in the query string, the
  // other methods as a form-encoded body.
  function fetchFor(target) {
    var el = one(target);
    var method = (el.getAttribute("data-ah-fetch") || "get").toUpperCase();
    var url = el.getAttribute("data-ah-url") ||
      (el.tagName === "FORM" ? el.getAttribute("action") || "" : "");
    var sel = el.getAttribute("data-ah-target") || "this";
    var targets = sel === "this" ? [el] : all(sel);
    var question = el.getAttribute("data-ah-confirm");

    if (question && !window.confirm(question)) {
      return Promise.resolve(false);
    }
    if (!fire(el, "ah:before-fetch", { url: url, method: method })) {
      return Promise.resolve(false);
    }

    var end = requestStart(el);
    var data = payload(el);
    var init = {
      method: method,
      credentials: "same-origin",
      headers: { "X-Aihtml": "1", "X-Aihtml-Target": sel,
                 "X-Requested-With": "XMLHttpRequest", "Accept": "text/html, */*; q=0.01" }
    };
    var href = url;
    if (method === "GET" || method === "HEAD") {
      if (data) { href = url.replace(/#.*$/, "") + (url.indexOf("?") >= 0 ? "&" : "?") + data; }
    } else {
      init.body = data;
      init.headers["Content-Type"] = "application/x-www-form-urlencoded; charset=UTF-8";
    }
    var status = 0;
    return window.fetch(href, init).then(function (resp) {
      status = resp.status;
      return resp.text().then(function (body) {
        if (!resp.ok) { throw { status: status, body: body }; }
        return body;
      });
    }).then(function (html) {
      swap(targets, html, el.getAttribute("data-ah-swap") || "inner").forEach(function (n) {
        if (n.nodeType === 1) { mount(n); }
      });
      fire(el, "ah:after-fetch", { url: url });
      return true;
    }).catch(function (err) {
      var failed = err && err.status !== undefined ? err : { status: status, body: "" };
      fire(el, "ah:error", { url: url, status: failed.status, body: failed.body });
      if (!(err && err.status !== undefined)) { console.error(err); }
      return false;
    }).then(function (ok) {
      end();
      return ok;
    });
  }

  // One delegated listener per event type covers content added later.
  ["click", "change", "submit", "input"].forEach(function (type) {
    delegateDocument(type, "[data-ah-fetch]", function (e, el) {
      if ((el.getAttribute("data-ah-trigger") || defaultTrigger(el)) !== type) {
        return;
      }
      if (type === "submit" || type === "click") {
        e.preventDefault();
      }
      if (el.getAttribute("aria-busy") === "true") {
        return;
      }
      fetchFor(el);
    });
  });

  // ------------------------------------------------------------------
  // Actions (aihtml_action): one POST per event, the reply is an AG-UI
  // event stream
  // ------------------------------------------------------------------
  //
  // Elements carry data-ah-on="click:TOKEN input:TOKEN:300"
  // (event:signed action[:debounce ms]). The event POSTs
  // {action, event, threadId, runId} to <body data-ah-action>, and the
  // response streams RUN_STARTED, CUSTOM "aihtml.ui" (DOM operations),
  // RUN_FINISHED or RUN_ERROR. Nothing is kept on the server between
  // requests.

  var ACTION_EVENTS = ["click", "dblclick", "change", "input", "submit", "keydown",
                       "keyup", "focusin", "focusout", "mouseenter", "mouseleave"];
  // Latest-wins events: a new one cancels the request still in flight.
  var LATEST_WINS = { input: true, change: true, keyup: true, keydown: true };
  var threadId = newId();
  var seq = 0;
  var timers = {};

  function newId() {
    if (window.crypto && window.crypto.randomUUID) {
      return window.crypto.randomUUID();
    }
    return Date.now().toString(36) + Math.random().toString(36).slice(2);
  }

  // "event:token[:debounce]"; the event itself may contain a colon
  // (ah:close), the token never does.
  function specs(el) {
    return (el.getAttribute("data-ah-on") || "").split(/\s+/).filter(Boolean).map(function (s) {
      var m = /^(.+?):([A-Za-z0-9_-]+\.[A-Za-z0-9_-]+)(?::(\d+))?$/.exec(s);
      return m ? { event: m[1], token: m[2], debounce: m[3] ? parseInt(m[3], 10) : 0 }
               : { event: "", token: "", debounce: 0 };
    });
  }

  function eventPayload(el, e) {
    if (!el.id) {
      el.id = "ah-e" + (++seq);
    }
    var form = el.tagName === "FORM" ? el : el.form;
    var fields = {};
    if (form) {
      formFields(form).forEach(function (f) { fields[f[0]] = f[1]; });
    }
    var values = {};
    var include = el.getAttribute("data-ah-include");
    if (include) {
      document.querySelectorAll(include).forEach(function (x) {
        var key = x.id || x.name;
        var check = x.type === "checkbox" || x.type === "radio";
        if (key) { values[key] = check ? x.checked : valueOf(x); }
      });
    }
    var data = {};
    Object.keys(el.dataset).forEach(function (k) {
      if (k.indexOf("ah") !== 0) { data[k] = el.dataset[k]; }
    });
    var control = /^(INPUT|SELECT|TEXTAREA|BUTTON)$/.test(el.tagName);
    var check = el.type === "checkbox" || el.type === "radio";
    // A custom component (slider, rating, dropdown list, toggle button, ...)
    // keeps its value in data-ah-value, which wins over a native value;
    // see designs/04-components.md.
    var value = el.hasAttribute("data-ah-value") ? el.getAttribute("data-ah-value")
      : (control ? valueOf(el) : null);
    return {
      type: e.type,
      id: el.id,
      value: value,
      checked: check ? el.checked : null,
      key: e.key || null,
      form: fields,
      values: values,
      data: data
    };
  }

  // The elements an operation targets (op.id, else the selector op.sel).
  function actionTargets(op) {
    if (op.id !== undefined) {
      var el = document.getElementById(op.id);
      return el ? [el] : [];
    }
    return op.sel === undefined ? [] : Array.from(document.querySelectorAll(op.sel));
  }

  function classList(s) {
    return String(s || "").split(/\s+/).filter(Boolean);
  }

  var OPS = {
    html: function (op, ts) {
      swap(ts, op.html, op.swap).forEach(function (n) {
        if (n.nodeType === 1) { mount(n); }
      });
    },
    remove: function (op, ts) {
      destroy(ts);
      ts.forEach(function (el) { el.remove(); });
    },
    attr: function (op, ts) {
      ts.forEach(function (el) {
        if (op.value === null) { el.removeAttribute(op.name); } else { el.setAttribute(op.name, op.value); }
      });
    },
    "class": function (op, ts) {
      ts.forEach(function (el) {
        if (op.add) { el.classList.add.apply(el.classList, classList(op.add)); }
        if (op.remove) {
          el.classList.remove.apply(el.classList, classList(op.remove));
          if (el.getAttribute("class") === "") { el.removeAttribute("class"); }
        }
      });
    },
    val: function (op, ts) {
      ts.forEach(function (el) {
        if (typeof op.value === "boolean") { el.checked = op.value; } else { setVal(el, op.value); }
      });
    },
    focus: function (op, ts) { ts.forEach(function (el) { el.focus(); }); },
    title: function (op) { document.title = op.value; },
    redirect: function (op) { window.location.href = op.value; },
    // Fire a DOM event (a bubbling CustomEvent, detail = op.detail) on the
    // target, or on the document without one; elements may bind actions
    // to it with on/2.
    trigger: function (op, ts) {
      var on = (op.id === undefined && op.sel === undefined) ? [document] : ts;
      on.forEach(function (t) {
        fire(t, op.event, op.detail === undefined ? null : op.detail);
      });
    },
    // Browser history: push or replace the URL without a request. Going
    // back or forward to an entry made here reloads that URL, so the
    // server renders it; the pages stay stateless.
    url: function (op) {
      if (!history.state || !history.state.ah) {
        history.replaceState({ ah: true }, "", location.href);
      }
      if (op.mode === "replace") {
        history.replaceState({ ah: true }, "", op.value);
      } else {
        history.pushState({ ah: true }, "", op.value);
      }
    },
    call: function (op, ts) {
      var args = op.args || [];
      if (op.id === undefined && op.sel === undefined) {
        callFn(op.method, args);
      } else {
        invoke.apply(null, [ts, op.method].concat(args));
      }
    },
    // AH is in scope; so is the page's global $ when the page has one
    // (the code runs in the global scope).
    js: function (op) {
      /* jshint evil: true */
      new Function("AH", op.code)(api);
    }
  };

  function applyOps(ops) {
    ops.forEach(function (op) {
      try {
        OPS[op.op](op, actionTargets(op));
      } catch (err) {
        console.error("aihtml: operation failed", op, err);
      }
    });
  }

  function onAgui(el, ev) {
    switch (ev.type) {
      case "CUSTOM":
        if (ev.name === "aihtml.ui") {
          applyOps(ev.value);
          syncStream();          // new content may follow other topics
        }
        break;
      case "RUN_ERROR":
        fire(el, "ah:error", { message: ev.message, code: ev.code });
        console.error("aihtml: action failed:", ev.message);
        break;
      default:
        break;
    }
  }

  // Server-sent events over a fetch() body: blocks separated by a blank
  // line, the payload on "data:" lines.
  function readStream(body, onEvent) {
    var reader = body.getReader();
    var decoder = new TextDecoder();
    var buf = "";
    function pump() {
      return reader.read().then(function (r) {
        buf += decoder.decode(r.value || new Uint8Array(), { stream: !r.done });
        var blocks = buf.split(/\r?\n\r?\n/);
        buf = r.done ? "" : blocks.pop();
        blocks.forEach(function (block) {
          var data = block.split(/\r?\n/)
            .filter(function (l) { return l.indexOf("data:") === 0; })
            .map(function (l) { return l.slice(5).replace(/^ /, ""); })
            .join("\n");
          if (data) { onEvent(JSON.parse(data)); }
        });
        return r.done ? undefined : pump();
      });
    }
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

  var syncs = {};

  function runAction(el, spec, e) {
    var body = eventPayload(el, e);          // also gives el an id
    var scopeSel = el.getAttribute("data-ah-sync-scope");
    var scope = scopeSel ? (el.closest(scopeSel) || el) : el;
    if (!scope.id) {
      scope.id = "ah-e" + (++seq);
    }
    var key = scopeSel ? "scope/" + scope.id : el.id + "/" + spec.event;
    var strategy = el.getAttribute("data-ah-sync") ||
      (LATEST_WINS[spec.event] ? "replace" : "drop");
    var running = syncs[key];
    if (running) {
      if (strategy === "drop") {
        return;
      }
      if (strategy === "queue") {
        running.queued = function () { send(el, spec, body, key); };
        return;
      }
      running.ctrl.abort();
    }
    send(el, spec, body, key);
  }

  function send(el, spec, body, key) {
    var url = document.body.getAttribute("data-ah-action") || "/aihtml/action";
    var ctrl = new AbortController();
    var st = { ctrl: ctrl, queued: null };
    syncs[key] = st;
    var end = requestStart(el);
    var done = function () {
      end();
      if (syncs[key] === st) {
        delete syncs[key];
        if (st.queued) { st.queued(); }
      }
    };
    return window.fetch(url, {
      method: "POST",
      credentials: "same-origin",
      headers: { "Content-Type": "application/json", "Accept": "text/event-stream" },
      body: JSON.stringify({ threadId: threadId, runId: newId(), action: spec.token,
                             event: body, streamId: stream.id }),
      signal: ctrl.signal
    }).then(function (resp) {
      if (!resp.ok) {
        // 403 invalid_action: the page was rendered with a secret this
        // server does not know (development restart, rotated secret).
        fire(el, "ah:error", { status: resp.status });
        throw new Error("aihtml: action refused with HTTP " + resp.status);
      }
      return readStream(resp.body, function (ev) { onAgui(el, ev); });
    }).catch(function (err) {
      if (err.name !== "AbortError") { console.error(err); }
    }).then(done, done);
  }

  // What an event of `type` on an element with data-ah-on does.
  function onActionEvent(el, e, type) {
    // A value-bearing component reports its own change/input from its
    // root; the same events bubbling up from controls inside it (an
    // input in a tab panel, say) are not its value changing.
    if ((type === "change" || type === "input") && e.target !== el &&
        el.hasAttribute("data-ah-value")) {
      return;
    }
    specs(el).forEach(function (s) {
      if (s.event !== type) {
        return;
      }
      if (type === "submit" || (type === "click" && (el.tagName === "A" || el.type === "submit"))) {
        e.preventDefault();
      }
      var question = el.getAttribute("data-ah-confirm");
      if (question && !window.confirm(question)) {
        return;
      }
      if (s.debounce) {
        clearTimeout(timers[el.id + s.token]);
        timers[el.id + s.token] = setTimeout(function () { runAction(el, s, e); }, s.debounce);
      } else {
        runAction(el, s, e);
      }
    });
  }

  // One delegated listener per event type, registered on first use: the
  // common DOM events up front, component events (ah:close, ah:remove, ...)
  // when an element on the page binds them (see mount).
  var listening = {};

  // A delegated listener on document for bubbling events; mouseenter/mouseleave do not bubble, so they are caught in
  // the capture phase and only count on the element itself (what
  // jQuery's delegated mouseenter emulates).
  function listen(type) {
    if (listening[type]) {
      return;
    }
    listening[type] = true;
    if (type === "mouseenter" || type === "mouseleave") {
      document.addEventListener(type, function (e) {
        var el = e.target;
        if (el && el.nodeType === 1 && el.matches("[data-ah-on]")) { onActionEvent(el, e, type); }
      }, true);
    } else {
      delegateDocument(type, "[data-ah-on]", function (e, el) { onActionEvent(el, e, type); });
    }
  }

  ACTION_EVENTS.forEach(function (type) { listen(type); });

  function listenFor(root) {
    withSelf(root, "[data-ah-on]").forEach(function (el) {
      specs(el).forEach(function (s) { listen(s.event); });
    });
  }

  // ------------------------------------------------------------------
  // Push (aihtml_push): one EventSource per page for all its topics
  // ------------------------------------------------------------------
  //
  // Elements carry data-ah-subscribe="TOKEN" (a signed topic) and maybe
  // data-ah-refresh="TOKEN" (an action). The page keeps one stream to
  // <body data-ah-events>?t=...; it is reopened whenever the set of
  // subscribed topics on the page changes. Pushed events are the same
  // CUSTOM "aihtml.ui" events actions return. After a reconnect (not the
  // first connect) every refresh action runs, since pushes sent while the
  // page was away are lost.

  var stream = { es: null, key: "", id: null, opened: false };

  function syncStream() {
    var tokens = [];
    document.querySelectorAll("[data-ah-subscribe]").forEach(function (el) {
      var t = el.getAttribute("data-ah-subscribe");
      if (tokens.indexOf(t) < 0) { tokens.push(t); }
    });
    tokens.sort();
    var key = tokens.join(" ");
    if (key === stream.key || !window.EventSource) {
      return;
    }
    if (stream.es) {
      stream.es.close();
    }
    stream = { es: null, key: key, id: null, opened: false };
    if (!tokens.length) {
      return;
    }
    var base = document.body.getAttribute("data-ah-events") || "/aihtml/events";
    var es = new EventSource(base + "?" + tokens.map(function (t) {
      return "t=" + encodeURIComponent(t);
    }).join("&"));
    stream.es = es;
    es.onopen = function () {
      if (stream.es !== es) { return; }
      if (stream.opened) {
        refreshAll();
      }
      stream.opened = true;
    };
    es.onmessage = function (m) {
      if (stream.es !== es) { return; }
      var ev = JSON.parse(m.data);
      if (ev.type === "CUSTOM" && ev.name === "aihtml.stream") {
        stream.id = ev.value.id;
      } else {
        onAgui(document.body, ev);
      }
    };
    es.onerror = function () {
      // EventSource retries by itself; CLOSED means the server refused the
      // topics (e.g. a secret the server no longer has).
      if (es.readyState === 2) {
        fire(document, "ah:error", { stream: true });
      }
    };
  }

  function refreshAll() {
    document.querySelectorAll("[data-ah-refresh]").forEach(function (el) {
      runAction(el, { event: "refresh", token: el.getAttribute("data-ah-refresh") },
                { type: "refresh" });
    });
  }

  window.addEventListener("popstate", function (e) {
    if (e.state && e.state.ah) {
      window.location.reload();
    }
  });

  // ------------------------------------------------------------------
  // Optional third-party libraries, loaded on demand
  // ------------------------------------------------------------------

  // AH.vendor("echarts").then(function (echarts) { ... }) loads a library
  // once per page and resolves with it; a list resolves with the list of
  // libraries, in the same order. Each library is its own chunk of the
  // bundle (vite.config.mjs), fetched by a dynamic import() the first time
  // it is asked for: a page without charts, exports or the Markdown editor
  // downloads none of them. Their licences are in js/THIRD-PARTY-LICENSES.txt.
  //
  //   echarts          the echarts namespace (init, graphic, ...)
  //   xlsx             the SheetJS namespace (utils, write, writeFile, ...)
  //   jspdf            the jsPDF namespace ({jsPDF, ...})
  //   jspdf-autotable  the autoTable(doc, options) function
  //   prosemirror      ProseMirror + markdown-it (assets/vendor/prosemirror.entry.js)
  //
  // A namespace resolves as a plain object copy of the module namespace
  // (which is frozen), so that, as with the globals of the old script
  // files, a page or a test can wrap or replace a library function
  // (XLSX.writeFile, ...) for every component using it.
  function plain(m) { return Object.assign({}, m); }
  var VENDOR = {
    echarts: function () { return import("echarts").then(plain); },
    xlsx: function () { return import("xlsx").then(plain); },
    jspdf: function () { return import("jspdf").then(plain); },
    "jspdf-autotable": function () {
      return import("jspdf-autotable").then(function (m) { return m.autoTable; });
    },
    prosemirror: function () {
      return import("../vendor/prosemirror.entry.js").then(function (m) {
        // also the global of the old script bundle, still read by page
        // scripts (and tests) written against it
        return (window.AHProseMirror = plain(m));
      });
    }
  };
  var vendorLoads = {};

  function vendor(names) {
    if (Array.isArray(names)) { return Promise.all(names.map(vendor)); }
    var load = Object.prototype.hasOwnProperty.call(VENDOR, names) && VENDOR[names];
    if (!load) { return Promise.reject(new Error("aihtml: unknown vendor library " + names)); }
    if (!vendorLoads[names]) {
      vendorLoads[names] = load().catch(function (err) {
        delete vendorLoads[names];   // a later call tries again
        throw err;
      });
    }
    return vendorLoads[names];
  }

  // Once the document is parsed (and after main.js has started Stimulus):
  // mount the page and open the push stream.
  function boot() {
    mount(document);
    syncStream();
  }
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    setTimeout(boot);
  }

  var api = {
    invoke: invoke,
    fn: fn,
    float: floatPopup,
    mount: mount,
    destroy: destroy,
    theme: theme,
    fetch: fetchFor,
    vendor: vendor,
    Controller: AHController,
    register: register,
    ready: ready,
    start: start,
    loadAll: loadAll,
    stimulus: function () { return app; },
    apply: applyOps,
    swap: swap,
    morph: function (target, html) { morph(one(target), html, true); },
    settleDelay: SETTLE_MS,
    NS: NS,
    version: "0.3.0"
  };
  return api;
})();

export default AH;
