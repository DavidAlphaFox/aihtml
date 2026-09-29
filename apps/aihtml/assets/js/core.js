/*!
 * core.js: the client runtime for aihtml prefabs (an ES module; main.js is
 * the bundle entry, see designs/06-bundling.md). Behaviours are Stimulus
 * controllers on [data-ah="<name>"], loaded on demand.
 *
 * The server renders all HTML. This file only adds behaviour to it:
 *
 *   AH.define(name, {init, destroy, methods})  behaviour for [data-ah="<name>"]
 *   AH.invoke(el, method, ...args)    call a behaviour method
 *   AH.fn(name, f)                    page-level function (toast, ...)
 *   AH.mount(root)                    attach behaviours inside root
 *   AH.theme.get() / .set(axis, v)    the four theme axes on <html>
 *   AH.fetch(el)                      run an element's data-ah-fetch
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
 * Events, all namespaced so AH.destroy can remove them:
 *   ah:theme {axis, value}       on document, after a theme change
 *   ah:before-fetch / ah:after-fetch / ah:error   around a round trip
 */
import $ from "jquery";
import { Application, Controller, defaultSchema } from "@hotwired/stimulus";

const AH = (function () {
  "use strict";

  var NS = ".ah";
  var behaviors = {};
  // this module's own URL (see vendor below)
  var SELF = import.meta.url;

  // ------------------------------------------------------------------
  // Behaviours
  // ------------------------------------------------------------------

  // Behaviours are Stimulus controllers. Stimulus is configured to read
  // data-ah (not data-controller), so the server's HTML stays the same,
  // and data-ah-do for in-browser actions (data-action is used by the
  // components themselves).
  //
  // AH.define(name, {init, destroy, methods}) is the adapter for the
  // behaviours written before Stimulus: it registers a controller that
  // runs init on connect and destroy on disconnect. Two things keep the
  // old guarantees:
  // - mount(root) initialises the loaded behaviours inside root at once, so
  //   code that inserts a component and uses it right away (a server "call"
  //   op after an "html" op, tests) works; Stimulus connects later and sees
  //   the element is already initialised.
  // - Stimulus disconnects an element that is moved (morph, preserve) and
  //   connects it again; destroy therefore waits a microtask and only runs
  //   if the element really left the page, so moved components keep state.
  // spec: {init(el, $el), destroy(el, $el), methods: {name(el, $el, ...args)}}
  var app = null;
  var SCHEMA = $.extend({}, defaultSchema, {
    controllerAttribute: "data-ah",
    actionAttribute: "data-ah-do"
  });

  function define(name, spec) {
    behaviors[name] = spec;
    if (app) { register(name); }
    var cbs = pending[name] || [];
    delete pending[name];
    cbs.forEach(function (cb) { cb(); });
  }

  function register(name) {
    app.register(name, class extends Controller {
      connect() {
        listenFor(this.element);
        initElement(this.element);
      }
      disconnect() {
        var el = this.element;
        queueMicrotask(function () {
          if (!el.isConnected) { teardown(el); }
        });
      }
    });
  }

  function initElement(el) {
    var b = behaviors[el.getAttribute("data-ah")];
    if (!b || el.hasAttribute("data-ah-mounted")) {
      return;
    }
    el.setAttribute("data-ah-mounted", "");
    if (b.init) {
      b.init(el, $(el));
    }
  }

  function teardown(el) {
    if (!el.hasAttribute("data-ah-mounted")) {
      return;
    }
    var b = behaviors[el.getAttribute("data-ah")];
    if (b && b.destroy) {
      b.destroy(el, $(el));
    }
    $(el).off(NS).removeAttr("data-ah-mounted");
  }

  // Run a method of the behaviour an element carries. Used by the server's
  // aihtml_action:call/4 and by pages. A behaviour that is not loaded yet
  // is loaded first, and the call runs once it is (the result is then
  // undefined for the caller).
  function invoke(target, method) {
    var args = Array.prototype.slice.call(arguments, 2);
    var result;
    $(target).each(function () {
      var el = this;
      var name = el.getAttribute("data-ah");
      var run = function () {
        var b = behaviors[name];
        if (!b || !b.methods || !b.methods[method]) {
          console.error("aihtml: no method " + method + " on", el);
          return undefined;
        }
        initElement(el);
        return b.methods[method].apply(null, [el, $(el)].concat(args));
      };
      if (behaviors[name] || !lazy.behaviours[name]) {
        result = run();
      } else {
        whenDefined(name, run);
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

  function mount(rootEl) {
    var $root = $(rootEl || document);
    listenFor($root);
    $root.find("[data-ah]").addBack("[data-ah]").each(function () {
      initElement(this);
    });
    scan($root);
    return $root;
  }

  function destroy(rootEl) {
    $(rootEl).find("[data-ah-mounted]").addBack("[data-ah-mounted]").each(function () {
      teardown(this);
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
    var node = $(root || document)[0];
    if (!node || !node.querySelectorAll) { return; }
    var els = node.matches && node.matches("[data-ah]") ? [node] : [];
    Array.prototype.push.apply(els, node.querySelectorAll("[data-ah]"));
    els.forEach(function (el) {
      var name = el.getAttribute("data-ah");
      if (!behaviors[name] && lazy.behaviours[name]) { loadChunk(lazy.behaviours[name]); }
    });
    lazy.triggers.forEach(function (t) {
      if ((node.matches && node.matches(t[0])) || node.querySelector(t[0])) { loadChunk(t[1]); }
    });
  }

  // Load every registered component (tests, pages that want no delay).
  function loadAll() {
    var all = new Set();
    Object.keys(lazy.behaviours).forEach(function (k) { all.add(lazy.behaviours[k]); });
    Object.keys(lazy.fns).forEach(function (k) { all.add(lazy.fns[k]); });
    lazy.triggers.forEach(function (t) { all.add(t[1]); });
    return Promise.all(Array.from(all).map(loadChunk));
  }

  // Start Stimulus with the registry; main.js calls this once.
  function start(registry) {
    lazy = registry || lazy;
    app = Application.start(document.documentElement, SCHEMA);
    Object.keys(behaviors).forEach(register);
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
    popup = $(popup)[0];
    anchor = $(anchor)[0];
    opts = $.extend({ placement: "bottom", align: "start", offset: 4, matchWidth: false }, opts);
    var handle = {
      update: function () { place(popup, anchor, opts); },
      stop: function () {
        floating = floating.filter(function (h) { return h !== handle; });
        popup.style.position = popup.style.top = popup.style.left = "";
        popup.style.right = popup.style.bottom = popup.style.minWidth = "";
        popup.removeAttribute("data-ah-placement");
        if (!floating.length) { $(window).off(".ahfloat"); }
      }
    };
    if (!floating.length) {
      $(window).on("resize.ahfloat", updateAll);
      window.addEventListener("scroll", updateAll, true);
    }
    floating.push(handle);
    handle.update();
    return handle;
  }

  function updateAll() {
    if (!floating.length) {
      window.removeEventListener("scroll", updateAll, true);
      return;
    }
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
      $.each(AXES, function (axis, attr) {
        out[axis] = html.getAttribute(attr);
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
      $(document).trigger("ah:theme", [{ axis: axis, value: value }]);
    },
    reset: function () {
      save({});
    }
  };

  define("theme-switcher", {
    init: function (el, $el) {
      function sync() {
        var current = theme.get();
        $el.find("[data-ah-axis]").each(function () {
          var axis = this.getAttribute("data-ah-axis");
          if (current[axis]) {
            $(this).val(current[axis]);
          }
        });
      }
      sync();
      // Follow changes made elsewhere (AH.theme.set, another switcher).
      $.data(el, "ah-sync", sync);
      $(document).on("ah:theme" + NS, sync);
      $el.on("change" + NS, "[data-ah-axis]", function () {
        theme.set(this.getAttribute("data-ah-axis"), $(this).val());
      });
    },
    destroy: function (el) {
      $(document).off("ah:theme" + NS, $.data(el, "ah-sync"));
    }
  });

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
      return $(el).serialize();
    }
    if (!el.name) {
      return "";
    }
    var off = (el.type === "checkbox" || el.type === "radio") && !el.checked;
    return $.param([{ name: el.name, value: off ? "" : $(el).val() }]);
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

  function swap($target, html, mode) {
    var focus = captureFocus();
    var $new = doSwap($target, html, mode);
    restoreFocus(focus);
    return $new;
  }

  function doSwap($target, html, mode) {
    if (mode === "morph" || mode === "morph_inner") {
      $target.each(function () { morph(this, html, mode === "morph"); });
      return $();                  // morph mounts what it adds itself
    }
    if (mode === "none") {
      return $();
    }
    var $new = $($.parseHTML(html, document, false));
    var pantry = stashPreserved($new);
    var settle = prepareSettle($new);
    insert($target, $new, mode);
    restorePreserved(pantry);
    finishSettle($target, $new.filter(function () { return this.nodeType === 1; }), settle);
    return $new;
  }

  function insert($target, $new, mode) {
    switch (mode) {
      case "outer":
        destroy($target);
        if ($new.length) {
          $target.replaceWith($new);
        } else {
          $target.remove();
        }
        return $new;
      case "append":
        $target.append($new);
        return $new;
      case "prepend":
        $target.prepend($new);
        return $new;
      default:
        destroy($target.children());
        $target.empty().append($new);
        return $new;
    }
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

  function stashPreserved($new) {
    var found = [];
    $new.find("[data-ah-preserve][id]").addBack("[data-ah-preserve][id]").each(function () {
      var old = document.getElementById(this.id);
      if (old && old !== this && old.hasAttribute("data-ah-preserve")) {
        found.push({ placeholder: this, el: old });
      }
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

  function prepareSettle($new) {
    var list = [];
    $new.find("[id]").addBack("[id]").each(function () {
      var old = document.getElementById(this.id);
      if (!old || old === this || this.hasAttribute("data-ah") ||
          this.hasAttribute("data-ah-preserve")) {
        return;
      }
      var el = this, saved = {};
      SETTLE_ATTRS.forEach(function (a) {
        saved[a] = el.getAttribute(a);
        var v = old.getAttribute(a);
        if (v === null) { el.removeAttribute(a); } else { el.setAttribute(a, v); }
      });
      list.push({ el: el, saved: saved });
    });
    return list;
  }

  function finishSettle($target, $added, list) {
    $added.addClass("ah-added");
    $target.addClass("ah-settling");
    setTimeout(function () {
      list.forEach(function (x) {
        SETTLE_ATTRS.forEach(function (a) {
          if (x.saved[a] === null) { x.el.removeAttribute(a); } else { x.el.setAttribute(a, x.saved[a]); }
        });
      });
      dropClass($added, "ah-added");
      dropClass($target, "ah-settling");
    }, SETTLE_MS);
  }

  // Remove a transient class without leaving class="" behind.
  function dropClass($els, cls) {
    $els.each(function () {
      this.classList.remove(cls);
      if (this.getAttribute("class") === "") { this.removeAttribute("class"); }
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
  // re-initialised on its existing nodes (destroy, then mount); added nodes
  // are mounted; removed ones are destroyed first.

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
      if (document.contains(el) && el.hasAttribute("data-ah-mounted")) {
        destroy(el);
        mount(el);
      }
    });
    var added = ctx.added.filter(function (el) {
      return el.nodeType === 1 && document.contains(el);
    });
    added.forEach(function (el) { mount(el); });
    finishSettle($(target), $(added), []);
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
      destroy(old);
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
    if (changed && old.hasAttribute("data-ah-mounted") && ctx.changed.indexOf(old) < 0) {
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
    if (!sel) { return $(); }
    if (sel === "this") { return $(el); }
    var m = /^closest\s+(.+)$/.exec(sel);
    return m ? $(el).closest(m[1]) : $(sel);
  }

  function bump($els, cls, by) {
    $els.each(function () {
      var n = ($.data(this, "ah-count-" + cls) || 0) + by;
      $.data(this, "ah-count-" + cls, n);
      if (cls === "disabled") {
        if (n > 0 && by > 0 && n === 1) {
          $.data(this, "ah-was-disabled", this.disabled);
          this.disabled = true;
        } else if (n === 0) {
          this.disabled = !!$.data(this, "ah-was-disabled");
        }
      } else {
        $(this).toggleClass(cls, n > 0);
      }
    });
  }

  function requestStart(el) {
    var $ind = $(el).add(resolve(el, el.getAttribute("data-ah-indicator")));
    var $dis = resolve(el, el.getAttribute("data-ah-disable"));
    el.setAttribute("aria-busy", "true");
    bump($ind, "ah-request", 1);
    bump($dis, "disabled", 1);
    var ended = false;
    return function () {
      if (ended) { return; }
      ended = true;
      bump($ind, "ah-request", -1);
      bump($dis, "disabled", -1);
      if (!$(el).hasClass("ah-request")) { el.removeAttribute("aria-busy"); }
    };
  }

  function fetchFor(el) {
    var $el = $(el);
    var method = ($el.attr("data-ah-fetch") || "get").toUpperCase();
    var url = $el.attr("data-ah-url") || (el.tagName === "FORM" ? $el.attr("action") : "");
    var sel = $el.attr("data-ah-target") || "this";
    var $target = sel === "this" ? $el : $(sel);
    var question = $el.attr("data-ah-confirm");

    if (question && !window.confirm(question)) {
      return $.Deferred().reject().promise();
    }
    var before = $.Event("ah:before-fetch");
    $el.trigger(before, [{ url: url, method: method }]);
    if (before.isDefaultPrevented()) {
      return $.Deferred().reject().promise();
    }

    var end = requestStart(el);
    return $.ajax({
      url: url,
      method: method,
      data: payload(el),
      dataType: "html",
      headers: { "X-Aihtml": "1", "X-Aihtml-Target": sel }
    })
      .done(function (html) {
        var $new = swap($target, html, $el.attr("data-ah-swap") || "inner");
        $new.each(function () {
          if (this.nodeType === 1) {
            mount(this);
          }
        });
        $el.trigger("ah:after-fetch", [{ url: url }]);
      })
      .fail(function (xhr) {
        $el.trigger("ah:error", [{ url: url, status: xhr.status, body: xhr.responseText }]);
      })
      .always(end);
  }

  // One delegated listener per event type covers content added later.
  $.each(["click", "change", "submit", "input"], function (_, type) {
    $(document).on(type + NS, "[data-ah-fetch]", function (e) {
      if ((this.getAttribute("data-ah-trigger") || defaultTrigger(this)) !== type) {
        return;
      }
      if (type === "submit" || type === "click") {
        e.preventDefault();
      }
      if (this.getAttribute("aria-busy") === "true") {
        return;
      }
      fetchFor(this);
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
  var inflight = {};

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
      $.each($(form).serializeArray(), function (_, f) { fields[f.name] = f.value; });
    }
    var values = {};
    var include = el.getAttribute("data-ah-include");
    if (include) {
      $(include).each(function () {
        var key = this.id || this.name;
        var check = this.type === "checkbox" || this.type === "radio";
        if (key) { values[key] = check ? this.checked : $(this).val(); }
      });
    }
    var data = {};
    $.each(el.dataset, function (k, v) {
      if (k.indexOf("ah") !== 0) { data[k] = v; }
    });
    var control = /^(INPUT|SELECT|TEXTAREA|BUTTON)$/.test(el.tagName);
    var check = el.type === "checkbox" || el.type === "radio";
    // A custom component (slider, rating, dropdown list, toggle button, ...)
    // keeps its value in data-ah-value, which wins over a native value;
    // see designs/04-components.md.
    var value = el.hasAttribute("data-ah-value") ? el.getAttribute("data-ah-value")
      : (control ? $(el).val() : null);
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

  function actionTarget(op) {
    if (op.id !== undefined) {
      var el = document.getElementById(op.id);
      return el ? $(el) : $();
    }
    return $(op.sel);
  }

  var OPS = {
    html: function (op, $t) {
      swap($t, op.html, op.swap).each(function () {
        if (this.nodeType === 1) { mount(this); }
      });
    },
    remove: function (op, $t) {
      destroy($t);
      $t.remove();
    },
    attr: function (op, $t) {
      if (op.value === null) { $t.removeAttr(op.name); } else { $t.attr(op.name, op.value); }
    },
    "class": function (op, $t) {
      if (op.add) { $t.addClass(op.add); }
      if (op.remove) { $t.removeClass(op.remove); }
    },
    val: function (op, $t) {
      if (typeof op.value === "boolean") { $t.prop("checked", op.value); } else { $t.val(op.value); }
    },
    focus: function (op, $t) { $t.trigger("focus"); },
    title: function (op) { document.title = op.value; },
    redirect: function (op) { window.location.href = op.value; },
    // Fire a DOM event (jQuery trigger, bubbles) on the target, or on the
    // document without one; elements may bind actions to it with on/2.
    trigger: function (op, $t) {
      var $on = (op.id === undefined && op.sel === undefined) ? $(document) : $t;
      $on.trigger(op.event, [op.detail === undefined ? null : op.detail]);
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
    call: function (op, $t) {
      var args = op.args || [];
      if (op.id === undefined && op.sel === undefined) {
        callFn(op.method, args);
      } else {
        invoke.apply(null, [$t, op.method].concat(args));
      }
    },
    js: function (op) {
      /* jshint evil: true */
      new Function("$", "AH", op.code)($, api);
    }
  };

  function applyOps(ops) {
    ops.forEach(function (op) {
      try {
        OPS[op.op](op, actionTarget(op));
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
        $(el).trigger("ah:error", [{ message: ev.message, code: ev.code }]);
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
    var payload = eventPayload(el, e);          // also gives el an id
    var scopeSel = el.getAttribute("data-ah-sync-scope");
    var scope = scopeSel ? ($(el).closest(scopeSel)[0] || el) : el;
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
        running.queued = function () { send(el, spec, payload, key); };
        return;
      }
      running.ctrl.abort();
    }
    send(el, spec, payload, key);
  }

  function send(el, spec, payload, key) {
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
                             event: payload, streamId: stream.id }),
      signal: ctrl.signal
    }).then(function (resp) {
      if (!resp.ok) {
        // 403 invalid_action: the page was rendered with a secret this
        // server does not know (development restart, rotated secret).
        $(el).trigger("ah:error", [{ status: resp.status }]);
        throw new Error("aihtml: action refused with HTTP " + resp.status);
      }
      return readStream(resp.body, function (ev) { onAgui(el, ev); });
    }).catch(function (err) {
      if (err.name !== "AbortError") { console.error(err); }
    }).then(done, done);
  }

  // One delegated listener per event type, registered on first use: the
  // common DOM events up front, component events (ah:close, ah:remove, ...)
  // when an element on the page binds them (see mount).
  var listening = {};
  function listen(type) {
    if (listening[type]) {
      return;
    }
    listening[type] = true;
    $(document).on(type + NS, "[data-ah-on]", function (e) {
      var el = this;
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
    });
  }
  $.each(ACTION_EVENTS, function (_, type) { listen(type); });

  function listenFor(root) {
    $(root).find("[data-ah-on]").addBack("[data-ah-on]").each(function () {
      specs(this).forEach(function (s) { listen(s.event); });
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
    $("[data-ah-subscribe]").each(function () {
      var t = this.getAttribute("data-ah-subscribe");
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
        $(document).trigger("ah:error", [{ stream: true }]);
      }
    };
  }

  function refreshAll() {
    $("[data-ah-refresh]").each(function () {
      runAction(this, { event: "refresh", token: this.getAttribute("data-ah-refresh") },
                { type: "refresh" });
    });
  }

  window.addEventListener("popstate", function (e) {
    if (e.state && e.state.ah) {
      window.location.reload();
    }
  });

  // ------------------------------------------------------------------
  // Optional third-party scripts (priv/static/vendor), loaded on demand
  // ------------------------------------------------------------------

  // AH.vendor("echarts").then(function (echarts) { ... }) loads a library
  // once per page and resolves with its global; a list loads in order and
  // resolves with the list of globals. A library already on the page (its
  // global is defined) is not loaded again. Files come from the body's
  // data-ah-vendor directory, else from vendor/ beside the bundle's js/.
  var VENDOR = {
    echarts: { file: "echarts.min.js", global: "echarts" },
    xlsx: { file: "xlsx.full.min.js", global: "XLSX" },
    jspdf: { file: "jspdf.umd.min.js", global: "jspdf" },
    "jspdf-autotable": { file: "jspdf.plugin.autotable.min.js", global: "autoTable",
                         deps: ["jspdf"] },
    prosemirror: { file: "prosemirror.min.js", global: "AHProseMirror" }
  };
  var vendorLoads = {};

  function vendorDir() {
    var dir = document.body && document.body.getAttribute("data-ah-vendor");
    if (dir) { return dir.replace(/\/?$/, "/"); }
    // the bundle is in priv/static/js, the vendor files in priv/static/vendor
    return SELF ? SELF.replace(/[^\/]*$/, "") + "../vendor/" : "/aihtml/vendor/";
  }

  function vendor(names) {
    if (Array.isArray(names)) {
      return names.reduce(function (p, n) {
        return p.then(function (acc) {
          return vendor(n).then(function (g) { return acc.concat([g]); });
        });
      }, Promise.resolve([]));
    }
    var lib = VENDOR[names];
    if (!lib) { return Promise.reject(new Error("aihtml: unknown vendor library " + names)); }
    if (!vendorLoads[names]) {
      vendorLoads[names] = vendor(lib.deps || []).then(function () {
        if (window[lib.global]) { return window[lib.global]; }
        return new Promise(function (ok, fail) {
          var s = document.createElement("script");
          s.src = vendorDir() + lib.file;
          s.onload = function () { ok(window[lib.global]); };
          s.onerror = function () {
            delete vendorLoads[names];
            fail(new Error("aihtml: cannot load " + s.src));
          };
          document.head.appendChild(s);
        });
      });
    }
    return vendorLoads[names];
  }

  $(function () {
    mount(document);
    syncStream();
  });

  var api = {
    define: define,
    invoke: invoke,
    fn: fn,
    float: floatPopup,
    mount: mount,
    destroy: destroy,
    theme: theme,
    fetch: fetchFor,
    vendor: vendor,
    start: start,
    loadAll: loadAll,
    stimulus: function () { return app; },
    apply: applyOps,
    swap: swap,
    morph: function (target, html) { morph($(target)[0], html, true); },
    settleDelay: SETTLE_MS,
    NS: NS,
    version: "0.3.0"
  };
  return api;
})();

export default AH;
