/* ---- core.js ---- */
/*!
 * aihtml.js: client runtime for aihtml prefabs. Requires jQuery 3.5+ or 4.
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
 * Component behaviours live in assets/js/components/*.js; scripts/build-js.mjs
 * concatenates them after this file into priv/static/aihtml.js.
 *
 * Events, all namespaced so AH.destroy can remove them:
 *   ah:theme {axis, value}       on document, after a theme change
 *   ah:before-fetch / ah:after-fetch / ah:error   around a round trip
 */
(function (root, factory) {
  root.AH = factory(root.jQuery);
})(typeof window !== "undefined" ? window : this, function ($) {
  "use strict";

  if (!$) {
    throw new Error("aihtml.js needs jQuery; load it first");
  }

  var NS = ".ah";
  var behaviors = {};

  // ------------------------------------------------------------------
  // Behaviours
  // ------------------------------------------------------------------

  // spec: {init(el, $el), destroy(el, $el), methods: {name(el, $el, ...args)}}
  function define(name, spec) {
    behaviors[name] = spec;
  }

  // Run a method of the behaviour an element carries (mounting it first
  // if needed). Used by the server's aihtml_action:call/4 and by pages.
  function invoke(target, method) {
    var args = Array.prototype.slice.call(arguments, 2);
    var result;
    $(target).each(function () {
      var name = this.getAttribute("data-ah");
      var b = behaviors[name];
      if (!b || !b.methods || !b.methods[method]) {
        console.error("aihtml: no method " + method + " on", this);
        return;
      }
      if (!this.hasAttribute("data-ah-mounted")) { mount(this); }
      result = b.methods[method].apply(null, [this, $(this)].concat(args));
    });
    return result;
  }

  // Page-level functions (toast, notify, ...), for call(Ctx, global, ...).
  var fns = {};
  function fn(name, f) {
    fns[name] = f;
  }

  function mount(rootEl) {
    var $root = $(rootEl || document);
    listenFor($root);
    $root.find("[data-ah]").addBack("[data-ah]").each(function () {
      var el = this;
      var name = el.getAttribute("data-ah");
      if (el.hasAttribute("data-ah-mounted") || !behaviors[name]) {
        return;
      }
      el.setAttribute("data-ah-mounted", "");
      if (behaviors[name].init) {
        behaviors[name].init(el, $(el));
      }
    });
    return $root;
  }

  function destroy(rootEl) {
    $(rootEl).find("[data-ah-mounted]").addBack("[data-ah-mounted]").each(function () {
      var name = this.getAttribute("data-ah");
      if (behaviors[name] && behaviors[name].destroy) {
        behaviors[name].destroy(this, $(this));
      }
      $(this).off(NS).removeAttr("data-ah-mounted");
    });
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
        if (!fns[op.method]) { throw new Error("no function " + op.method); }
        fns[op.method].apply(null, args);
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
    apply: applyOps,
    swap: swap,
    morph: function (target, html) { morph($(target)[0], html, true); },
    settleDelay: SETTLE_MS,
    NS: NS,
    version: "0.3.0"
  };
  return api;
});

/* ---- templates (compiled from apps/aihtml/templates) ---- */
(function (AH) {
  var R = {
  esc: function (v) {
    return this.str(v).replace(/[&<>"']/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c];
    });
  },
  str: function (v) {
    return v === undefined || v === null ? "" : String(v);
  },
  falsy: function (v) {
    return v === undefined || v === null || v === false || v === "" ||
      (Array.isArray(v) && v.length === 0);
  },
  lookup: function (stack, keys) {
    if (keys.length === 0) { return stack[0]; }
    var v, found = false;
    for (var i = 0; i < stack.length; i++) {
      var f = stack[i];
      if (f !== null && typeof f === "object" && !Array.isArray(f) &&
          Object.prototype.hasOwnProperty.call(f, keys[0])) {
        v = f[keys[0]]; found = true; break;
      }
    }
    if (!found) { return undefined; }
    for (var k = 1; k < keys.length; k++) {
      if (v === null || typeof v !== "object" || Array.isArray(v) ||
          !Object.prototype.hasOwnProperty.call(v, keys[k])) { return undefined; }
      v = v[keys[k]];
    }
    return v;
  }
};
  AH.tpl = AH.tpl || {};
  AH.tpl["calendar_list"] = function(d){var S=[d],o="";o+="<div class=\"ah-calendar-list\">";o+="\n";var v0=R.lookup(S,["empty"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-calendar-list-empty\"><div class=\"ah-calendar-list-empty-icon\">📅</div><div>";o+=R.esc(R.lookup(S,["no_events"]));o+="</div><div class=\"ah-calendar-list-empty-hint\">";o+=R.esc(R.lookup(S,["no_events_hint"]));o+="</div></div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-calendar-list-empty\"><div class=\"ah-calendar-list-empty-icon\">📅</div><div>";o+=R.esc(R.lookup(S,["no_events"]));o+="</div><div class=\"ah-calendar-list-empty-hint\">";o+=R.esc(R.lookup(S,["no_events_hint"]));o+="</div></div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-calendar-list-empty\"><div class=\"ah-calendar-list-empty-icon\">📅</div><div>";o+=R.esc(R.lookup(S,["no_events"]));o+="</div><div class=\"ah-calendar-list-empty-hint\">";o+=R.esc(R.lookup(S,["no_events_hint"]));o+="</div></div>";S.shift();}}o+="\n";var v0=R.lookup(S,["groups"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-calendar-list-day-group\"><div class=\"ah-calendar-list-day-header\"><span class=\"ah-calendar-list-day-name\">";o+=R.esc(R.lookup(S,["name"]));o+="</span><span class=\"ah-calendar-list-day-date\">";o+=R.esc(R.lookup(S,["date"]));o+="</span></div><div class=\"ah-calendar-list-day-events\">";var v1=R.lookup(S,["events"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-calendar-list-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_status"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}o+="<div class=\"ah-calendar-list-event-dot\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\"></div><div class=\"ah-calendar-list-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div><div class=\"ah-calendar-list-event-title\">";var v2=R.lookup(S,["recurring"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}o+=R.esc(R.lookup(S,["title"]));o+="</div></div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-calendar-list-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_status"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}o+="<div class=\"ah-calendar-list-event-dot\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\"></div><div class=\"ah-calendar-list-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div><div class=\"ah-calendar-list-event-title\">";var v2=R.lookup(S,["recurring"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}o+=R.esc(R.lookup(S,["title"]));o+="</div></div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-calendar-list-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_status"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}o+="<div class=\"ah-calendar-list-event-dot\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\"></div><div class=\"ah-calendar-list-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div><div class=\"ah-calendar-list-event-title\">";var v2=R.lookup(S,["recurring"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}o+=R.esc(R.lookup(S,["title"]));o+="</div></div>";S.shift();}}o+="</div></div>";o+="\n";S.shift();}}else if(v0===true){o+="<div class=\"ah-calendar-list-day-group\"><div class=\"ah-calendar-list-day-header\"><span class=\"ah-calendar-list-day-name\">";o+=R.esc(R.lookup(S,["name"]));o+="</span><span class=\"ah-calendar-list-day-date\">";o+=R.esc(R.lookup(S,["date"]));o+="</span></div><div class=\"ah-calendar-list-day-events\">";var v1=R.lookup(S,["events"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-calendar-list-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_status"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}o+="<div class=\"ah-calendar-list-event-dot\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\"></div><div class=\"ah-calendar-list-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div><div class=\"ah-calendar-list-event-title\">";var v2=R.lookup(S,["recurring"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}o+=R.esc(R.lookup(S,["title"]));o+="</div></div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-calendar-list-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_status"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}o+="<div class=\"ah-calendar-list-event-dot\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\"></div><div class=\"ah-calendar-list-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div><div class=\"ah-calendar-list-event-title\">";var v2=R.lookup(S,["recurring"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}o+=R.esc(R.lookup(S,["title"]));o+="</div></div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-calendar-list-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_status"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}o+="<div class=\"ah-calendar-list-event-dot\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\"></div><div class=\"ah-calendar-list-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div><div class=\"ah-calendar-list-event-title\">";var v2=R.lookup(S,["recurring"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}o+=R.esc(R.lookup(S,["title"]));o+="</div></div>";S.shift();}}o+="</div></div>";o+="\n";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-calendar-list-day-group\"><div class=\"ah-calendar-list-day-header\"><span class=\"ah-calendar-list-day-name\">";o+=R.esc(R.lookup(S,["name"]));o+="</span><span class=\"ah-calendar-list-day-date\">";o+=R.esc(R.lookup(S,["date"]));o+="</span></div><div class=\"ah-calendar-list-day-events\">";var v1=R.lookup(S,["events"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-calendar-list-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_status"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}o+="<div class=\"ah-calendar-list-event-dot\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\"></div><div class=\"ah-calendar-list-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div><div class=\"ah-calendar-list-event-title\">";var v2=R.lookup(S,["recurring"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}o+=R.esc(R.lookup(S,["title"]));o+="</div></div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-calendar-list-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_status"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}o+="<div class=\"ah-calendar-list-event-dot\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\"></div><div class=\"ah-calendar-list-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div><div class=\"ah-calendar-list-event-title\">";var v2=R.lookup(S,["recurring"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}o+=R.esc(R.lookup(S,["title"]));o+="</div></div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-calendar-list-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_status"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-list-event-status\" style=\"background:";o+=R.esc(R.lookup(S,["status_color"]));o+=";\"></div>";S.shift();}}o+="<div class=\"ah-calendar-list-event-dot\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\"></div><div class=\"ah-calendar-list-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div><div class=\"ah-calendar-list-event-title\">";var v2=R.lookup(S,["recurring"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-recurring-icon\">↻ </span>";S.shift();}}o+=R.esc(R.lookup(S,["title"]));o+="</div></div>";S.shift();}}o+="</div></div>";o+="\n";S.shift();}}o+="</div>";return o;};
  AH.tpl["calendar_month"] = function(d){var S=[d],o="";o+="<div class=\"ah-calendar-daygrid\">";o+="\n";o+="<div class=\"ah-calendar-daygrid-header\">";var v0=R.lookup(S,["headers"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-calendar-daygrid-header-cell\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-calendar-daygrid-header-cell\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-calendar-daygrid-header-cell\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";S.shift();}}o+="</div>";o+="\n";o+="<div class=\"ah-calendar-daygrid-body\">";o+="\n";var v0=R.lookup(S,["weeks"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-calendar-week-row\"><div class=\"ah-calendar-week-bg\">";var v1=R.lookup(S,["days"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"";o+=R.esc(R.lookup(S,["bg_cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\"></div>";S.shift();}}else if(v1===true){o+="<div class=\"";o+=R.esc(R.lookup(S,["bg_cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\"></div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"";o+=R.esc(R.lookup(S,["bg_cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\"></div>";S.shift();}}o+="</div><div class=\"ah-calendar-week-content\">";var v1=R.lookup(S,["days"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"";o+=R.esc(R.lookup(S,["num_cls"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:1;\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"";o+=R.esc(R.lookup(S,["num_cls"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:1;\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"";o+=R.esc(R.lookup(S,["num_cls"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:1;\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}var v1=R.lookup(S,["events"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["sc"]));o+="/";o+=R.esc(R.lookup(S,["ec"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_time"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}o+="<span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";S.shift();}}else if(v1===true){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["sc"]));o+="/";o+=R.esc(R.lookup(S,["ec"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_time"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}o+="<span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["sc"]));o+="/";o+=R.esc(R.lookup(S,["ec"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_time"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}o+="<span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";S.shift();}}var v1=R.lookup(S,["more"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-calendar-day-more\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";\" role=\"button\" tabindex=\"0\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-calendar-day-more\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";\" role=\"button\" tabindex=\"0\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-calendar-day-more\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";\" role=\"button\" tabindex=\"0\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";S.shift();}}o+="</div></div>";o+="\n";S.shift();}}else if(v0===true){o+="<div class=\"ah-calendar-week-row\"><div class=\"ah-calendar-week-bg\">";var v1=R.lookup(S,["days"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"";o+=R.esc(R.lookup(S,["bg_cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\"></div>";S.shift();}}else if(v1===true){o+="<div class=\"";o+=R.esc(R.lookup(S,["bg_cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\"></div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"";o+=R.esc(R.lookup(S,["bg_cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\"></div>";S.shift();}}o+="</div><div class=\"ah-calendar-week-content\">";var v1=R.lookup(S,["days"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"";o+=R.esc(R.lookup(S,["num_cls"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:1;\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"";o+=R.esc(R.lookup(S,["num_cls"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:1;\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"";o+=R.esc(R.lookup(S,["num_cls"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:1;\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}var v1=R.lookup(S,["events"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["sc"]));o+="/";o+=R.esc(R.lookup(S,["ec"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_time"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}o+="<span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";S.shift();}}else if(v1===true){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["sc"]));o+="/";o+=R.esc(R.lookup(S,["ec"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_time"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}o+="<span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["sc"]));o+="/";o+=R.esc(R.lookup(S,["ec"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_time"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}o+="<span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";S.shift();}}var v1=R.lookup(S,["more"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-calendar-day-more\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";\" role=\"button\" tabindex=\"0\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-calendar-day-more\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";\" role=\"button\" tabindex=\"0\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-calendar-day-more\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";\" role=\"button\" tabindex=\"0\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";S.shift();}}o+="</div></div>";o+="\n";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-calendar-week-row\"><div class=\"ah-calendar-week-bg\">";var v1=R.lookup(S,["days"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"";o+=R.esc(R.lookup(S,["bg_cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\"></div>";S.shift();}}else if(v1===true){o+="<div class=\"";o+=R.esc(R.lookup(S,["bg_cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\"></div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"";o+=R.esc(R.lookup(S,["bg_cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\"></div>";S.shift();}}o+="</div><div class=\"ah-calendar-week-content\">";var v1=R.lookup(S,["days"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"";o+=R.esc(R.lookup(S,["num_cls"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:1;\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"";o+=R.esc(R.lookup(S,["num_cls"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:1;\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"";o+=R.esc(R.lookup(S,["num_cls"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:1;\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}var v1=R.lookup(S,["events"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["sc"]));o+="/";o+=R.esc(R.lookup(S,["ec"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_time"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}o+="<span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";S.shift();}}else if(v1===true){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["sc"]));o+="/";o+=R.esc(R.lookup(S,["ec"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_time"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}o+="<span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["sc"]));o+="/";o+=R.esc(R.lookup(S,["ec"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\">";var v2=R.lookup(S,["has_time"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}else if(v2===true){o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<span class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</span>";S.shift();}}o+="<span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";S.shift();}}var v1=R.lookup(S,["more"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-calendar-day-more\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";\" role=\"button\" tabindex=\"0\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-calendar-day-more\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";\" role=\"button\" tabindex=\"0\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-calendar-day-more\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" style=\"grid-column:";o+=R.esc(R.lookup(S,["col"]));o+=";grid-row:";o+=R.esc(R.lookup(S,["row"]));o+=";\" role=\"button\" tabindex=\"0\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";S.shift();}}o+="</div></div>";o+="\n";S.shift();}}o+="</div>";o+="\n";o+="</div>";return o;};
  AH.tpl["calendar_timegrid"] = function(d){var S=[d],o="";o+="<div class=\"ah-calendar-timegrid\">";o+="\n";o+="<div class=\"ah-calendar-timegrid-header\"><div class=\"ah-calendar-timegrid-gutter-header\"></div>";var v0=R.lookup(S,["days"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"";o+=R.esc(R.lookup(S,["head_cls"]));o+="\"><span class=\"ah-calendar-timegrid-header-day\">";o+=R.esc(R.lookup(S,["dow"]));o+="</span><span class=\"ah-calendar-timegrid-header-date\">";o+=R.esc(R.lookup(S,["num"]));o+="</span></div>";S.shift();}}else if(v0===true){o+="<div class=\"";o+=R.esc(R.lookup(S,["head_cls"]));o+="\"><span class=\"ah-calendar-timegrid-header-day\">";o+=R.esc(R.lookup(S,["dow"]));o+="</span><span class=\"ah-calendar-timegrid-header-date\">";o+=R.esc(R.lookup(S,["num"]));o+="</span></div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"";o+=R.esc(R.lookup(S,["head_cls"]));o+="\"><span class=\"ah-calendar-timegrid-header-day\">";o+=R.esc(R.lookup(S,["dow"]));o+="</span><span class=\"ah-calendar-timegrid-header-date\">";o+=R.esc(R.lookup(S,["num"]));o+="</span></div>";S.shift();}}o+="</div>";o+="\n";o+="<div class=\"ah-calendar-timegrid-allday-row\"><div class=\"ah-calendar-timegrid-gutter\">";o+=R.esc(R.lookup(S,["all_day"]));o+="</div><div class=\"ah-calendar-timegrid-allday\">";var v0=R.lookup(S,["days"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-calendar-timegrid-allday-cell\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\">";var v1=R.lookup(S,["allday"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-calendar-event ah-calendar-allday-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-calendar-event ah-calendar-allday-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-calendar-event ah-calendar-allday-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";S.shift();}}o+="</div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-calendar-timegrid-allday-cell\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\">";var v1=R.lookup(S,["allday"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-calendar-event ah-calendar-allday-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-calendar-event ah-calendar-allday-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-calendar-event ah-calendar-allday-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";S.shift();}}o+="</div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-calendar-timegrid-allday-cell\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\">";var v1=R.lookup(S,["allday"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-calendar-event ah-calendar-allday-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-calendar-event ah-calendar-allday-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-calendar-event ah-calendar-allday-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><span class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</span></div>";S.shift();}}o+="</div>";S.shift();}}o+="</div></div>";o+="\n";o+="<div class=\"ah-calendar-timegrid-scroll\"><div class=\"ah-calendar-timegrid-body\">";o+="\n";o+="<div class=\"ah-calendar-timegrid-gutter\">";var v0=R.lookup(S,["slots"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-calendar-timegrid-slot\" style=\"height:";o+=R.esc(R.lookup(S,["slot_height"]));o+="px;\"><div class=\"ah-calendar-timegrid-slot-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</div><div class=\"ah-calendar-timegrid-slot-line\"></div></div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-calendar-timegrid-slot\" style=\"height:";o+=R.esc(R.lookup(S,["slot_height"]));o+="px;\"><div class=\"ah-calendar-timegrid-slot-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</div><div class=\"ah-calendar-timegrid-slot-line\"></div></div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-calendar-timegrid-slot\" style=\"height:";o+=R.esc(R.lookup(S,["slot_height"]));o+="px;\"><div class=\"ah-calendar-timegrid-slot-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</div><div class=\"ah-calendar-timegrid-slot-line\"></div></div>";S.shift();}}o+="</div>";o+="\n";o+="<div class=\"ah-calendar-timegrid-cols\">";var v0=R.lookup(S,["days"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"";o+=R.esc(R.lookup(S,["col_cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" style=\"position:relative;height:";o+=R.esc(R.lookup(S,["col_height"]));o+="px;\">";var v1=R.lookup(S,["timed"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-calendar-event ah-calendar-timegrid-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"position:absolute;top:";o+=R.esc(R.lookup(S,["top"]));o+="px;height:";o+=R.esc(R.lookup(S,["height"]));o+="px;left:";o+=R.esc(R.lookup(S,["left"]));o+=";width:";o+=R.esc(R.lookup(S,["width"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><div class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div><div class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div>";var v2=R.lookup(S,["resizable"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-calendar-event ah-calendar-timegrid-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"position:absolute;top:";o+=R.esc(R.lookup(S,["top"]));o+="px;height:";o+=R.esc(R.lookup(S,["height"]));o+="px;left:";o+=R.esc(R.lookup(S,["left"]));o+=";width:";o+=R.esc(R.lookup(S,["width"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><div class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div><div class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div>";var v2=R.lookup(S,["resizable"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-calendar-event ah-calendar-timegrid-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"position:absolute;top:";o+=R.esc(R.lookup(S,["top"]));o+="px;height:";o+=R.esc(R.lookup(S,["height"]));o+="px;left:";o+=R.esc(R.lookup(S,["left"]));o+=";width:";o+=R.esc(R.lookup(S,["width"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><div class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div><div class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div>";var v2=R.lookup(S,["resizable"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}o+="</div>";S.shift();}}o+="</div>";S.shift();}}else if(v0===true){o+="<div class=\"";o+=R.esc(R.lookup(S,["col_cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" style=\"position:relative;height:";o+=R.esc(R.lookup(S,["col_height"]));o+="px;\">";var v1=R.lookup(S,["timed"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-calendar-event ah-calendar-timegrid-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"position:absolute;top:";o+=R.esc(R.lookup(S,["top"]));o+="px;height:";o+=R.esc(R.lookup(S,["height"]));o+="px;left:";o+=R.esc(R.lookup(S,["left"]));o+=";width:";o+=R.esc(R.lookup(S,["width"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><div class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div><div class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div>";var v2=R.lookup(S,["resizable"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-calendar-event ah-calendar-timegrid-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"position:absolute;top:";o+=R.esc(R.lookup(S,["top"]));o+="px;height:";o+=R.esc(R.lookup(S,["height"]));o+="px;left:";o+=R.esc(R.lookup(S,["left"]));o+=";width:";o+=R.esc(R.lookup(S,["width"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><div class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div><div class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div>";var v2=R.lookup(S,["resizable"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-calendar-event ah-calendar-timegrid-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"position:absolute;top:";o+=R.esc(R.lookup(S,["top"]));o+="px;height:";o+=R.esc(R.lookup(S,["height"]));o+="px;left:";o+=R.esc(R.lookup(S,["left"]));o+=";width:";o+=R.esc(R.lookup(S,["width"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><div class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div><div class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div>";var v2=R.lookup(S,["resizable"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}o+="</div>";S.shift();}}o+="</div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"";o+=R.esc(R.lookup(S,["col_cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" style=\"position:relative;height:";o+=R.esc(R.lookup(S,["col_height"]));o+="px;\">";var v1=R.lookup(S,["timed"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-calendar-event ah-calendar-timegrid-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"position:absolute;top:";o+=R.esc(R.lookup(S,["top"]));o+="px;height:";o+=R.esc(R.lookup(S,["height"]));o+="px;left:";o+=R.esc(R.lookup(S,["left"]));o+=";width:";o+=R.esc(R.lookup(S,["width"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><div class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div><div class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div>";var v2=R.lookup(S,["resizable"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-calendar-event ah-calendar-timegrid-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"position:absolute;top:";o+=R.esc(R.lookup(S,["top"]));o+="px;height:";o+=R.esc(R.lookup(S,["height"]));o+="px;left:";o+=R.esc(R.lookup(S,["left"]));o+=";width:";o+=R.esc(R.lookup(S,["width"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><div class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div><div class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div>";var v2=R.lookup(S,["resizable"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-calendar-event ah-calendar-timegrid-event\" data-eventid=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" style=\"position:absolute;top:";o+=R.esc(R.lookup(S,["top"]));o+="px;height:";o+=R.esc(R.lookup(S,["height"]));o+="px;left:";o+=R.esc(R.lookup(S,["left"]));o+=";width:";o+=R.esc(R.lookup(S,["width"]));o+=";background:";o+=R.esc(R.lookup(S,["color"]));o+=";\" title=\"";o+=R.esc(R.lookup(S,["title"]));o+="\" role=\"button\" tabindex=\"0\"><div class=\"ah-calendar-event-title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div><div class=\"ah-calendar-event-time\">";o+=R.esc(R.lookup(S,["time"]));o+="</div>";var v2=R.lookup(S,["resizable"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-calendar-timegrid-resize-handle\"></div>";S.shift();}}o+="</div>";S.shift();}}o+="</div>";S.shift();}}o+="</div>";o+="\n";o+="<div class=\"ah-calendar-timegrid-now-indicator\" style=\"display:none;\"></div>";o+="\n";o+="</div></div>";o+="\n";o+="</div>";return o;};
  AH.tpl["combobox_tag"] = function(d){var S=[d],o="";o+="<span class=\"ah-combobox-tag\"><span class=\"ah-combobox-tag-text\">";o+=R.esc(R.lookup(S,["label"]));o+="</span><span class=\"ah-combobox-tag-close\" data-value=\"";o+=R.esc(R.lookup(S,["value"]));o+="\" role=\"button\" aria-label=\"Remove ";o+=R.esc(R.lookup(S,["label"]));o+="\">&times;</span></span>";return o;};
  AH.tpl["datepicker_month"] = function(d){var S=[d],o="";o+="<div class=\"ah-datepicker-calendar\">";o+="\n";o+="<div class=\"ah-datepicker-header\"><div class=\"ah-datepicker-nav-group\"><button type=\"button\" tabindex=\"-1\" class=\"ah-datepicker-nav-prev ah-datepicker-nav-year\" data-nav=\"-12\" aria-label=\"";o+=R.esc(R.lookup(S,["prev_year"]));o+="\">&laquo;</button><button type=\"button\" tabindex=\"-1\" class=\"ah-datepicker-nav-prev\" data-nav=\"-1\" aria-label=\"";o+=R.esc(R.lookup(S,["prev_month"]));o+="\">&lsaquo;</button></div><div class=\"ah-datepicker-title\" id=\"";o+=R.esc(R.lookup(S,["title_id"]));o+="\" aria-live=\"polite\">";o+=R.esc(R.lookup(S,["title"]));o+="</div><div class=\"ah-datepicker-nav-group\"><button type=\"button\" tabindex=\"-1\" class=\"ah-datepicker-nav-next\" data-nav=\"1\" aria-label=\"";o+=R.esc(R.lookup(S,["next_month"]));o+="\">&rsaquo;</button><button type=\"button\" tabindex=\"-1\" class=\"ah-datepicker-nav-next ah-datepicker-nav-year\" data-nav=\"12\" aria-label=\"";o+=R.esc(R.lookup(S,["next_year"]));o+="\">&raquo;</button></div></div>";o+="\n";o+="<div class=\"ah-datepicker-week-header\" aria-hidden=\"true\">";var v0=R.lookup(S,["week_numbers"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-datepicker-week-num-header\">Wk</div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-datepicker-week-num-header\">Wk</div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-datepicker-week-num-header\">Wk</div>";S.shift();}}var v0=R.lookup(S,["weekdays"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-datepicker-weekday\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-datepicker-weekday\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-datepicker-weekday\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";S.shift();}}o+="</div>";o+="\n";o+="<div class=\"ah-datepicker-body\" role=\"grid\" aria-labelledby=\"";o+=R.esc(R.lookup(S,["title_id"]));o+="\">";o+="\n";var v0=R.lookup(S,["weeks"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-datepicker-week\" role=\"row\">";var v1=R.lookup(S,["week_numbers"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}var v1=R.lookup(S,["days"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}S.shift();}}o+="</div>";o+="\n";S.shift();}}else if(v0===true){o+="<div class=\"ah-datepicker-week\" role=\"row\">";var v1=R.lookup(S,["week_numbers"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}var v1=R.lookup(S,["days"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}S.shift();}}o+="</div>";o+="\n";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-datepicker-week\" role=\"row\">";var v1=R.lookup(S,["week_numbers"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}var v1=R.lookup(S,["days"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}S.shift();}}o+="</div>";o+="\n";S.shift();}}o+="</div>";o+="\n";o+="<div class=\"ah-datepicker-footer\"><button type=\"button\" tabindex=\"-1\" class=\"ah-datepicker-today-btn\">";o+=R.esc(R.lookup(S,["today"]));o+="</button></div>";o+="\n";o+="</div>";return o;};
  AH.tpl["datetime_input_calendar"] = function(d){var S=[d],o="";o+="<div class=\"ah-dti-cal-nav\"><div class=\"ah-dti-cal-prev\" data-action=\"prev-month\" role=\"button\" aria-label=\"";o+=R.esc(R.lookup(S,["prev_month"]));o+="\">◀</div><div class=\"ah-dti-cal-title\" aria-live=\"polite\">";o+=R.esc(R.lookup(S,["title"]));o+="</div><div class=\"ah-dti-cal-next\" data-action=\"next-month\" role=\"button\" aria-label=\"";o+=R.esc(R.lookup(S,["next_month"]));o+="\">▶</div></div>";o+="\n";o+="<div class=\"ah-dti-cal-header\" aria-hidden=\"true\">";var v0=R.lookup(S,["weekdays"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<span>";o+=R.esc(R.lookup(S,["label"]));o+="</span>";S.shift();}}else if(v0===true){o+="<span>";o+=R.esc(R.lookup(S,["label"]));o+="</span>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<span>";o+=R.esc(R.lookup(S,["label"]));o+="</span>";S.shift();}}o+="</div>";o+="\n";o+="<div class=\"ah-dti-cal-grid\">";var v0=R.lookup(S,["days"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" role=\"button\" aria-label=\"";o+=R.esc(R.lookup(S,["date"]));o+="\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}var v1=R.lookup(S,["selected"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-pressed=\"true\"";S.shift();}}else if(v1===true){o+=" aria-pressed=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-pressed=\"true\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["day"]));o+="</div>";S.shift();}}else if(v0===true){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" role=\"button\" aria-label=\"";o+=R.esc(R.lookup(S,["date"]));o+="\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}var v1=R.lookup(S,["selected"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-pressed=\"true\"";S.shift();}}else if(v1===true){o+=" aria-pressed=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-pressed=\"true\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["day"]));o+="</div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" role=\"button\" aria-label=\"";o+=R.esc(R.lookup(S,["date"]));o+="\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}var v1=R.lookup(S,["selected"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-pressed=\"true\"";S.shift();}}else if(v1===true){o+=" aria-pressed=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-pressed=\"true\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["day"]));o+="</div>";S.shift();}}o+="</div>";o+="\n";var v0=R.lookup(S,["show_time"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-dti-time-row\"><span class=\"ah-dti-time-label\">";o+=R.esc(R.lookup(S,["time_label"]));o+="</span><input class=\"ah-dti-time-input\" data-field=\"hours\" type=\"text\" inputmode=\"numeric\" maxlength=\"2\" value=\"";o+=R.esc(R.lookup(S,["hours"]));o+="\" aria-label=\"Hours\"><span class=\"ah-dti-time-sep\">:</span><input class=\"ah-dti-time-input\" data-field=\"minutes\" type=\"text\" inputmode=\"numeric\" maxlength=\"2\" value=\"";o+=R.esc(R.lookup(S,["minutes"]));o+="\" aria-label=\"Minutes\"></div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-dti-time-row\"><span class=\"ah-dti-time-label\">";o+=R.esc(R.lookup(S,["time_label"]));o+="</span><input class=\"ah-dti-time-input\" data-field=\"hours\" type=\"text\" inputmode=\"numeric\" maxlength=\"2\" value=\"";o+=R.esc(R.lookup(S,["hours"]));o+="\" aria-label=\"Hours\"><span class=\"ah-dti-time-sep\">:</span><input class=\"ah-dti-time-input\" data-field=\"minutes\" type=\"text\" inputmode=\"numeric\" maxlength=\"2\" value=\"";o+=R.esc(R.lookup(S,["minutes"]));o+="\" aria-label=\"Minutes\"></div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-dti-time-row\"><span class=\"ah-dti-time-label\">";o+=R.esc(R.lookup(S,["time_label"]));o+="</span><input class=\"ah-dti-time-input\" data-field=\"hours\" type=\"text\" inputmode=\"numeric\" maxlength=\"2\" value=\"";o+=R.esc(R.lookup(S,["hours"]));o+="\" aria-label=\"Hours\"><span class=\"ah-dti-time-sep\">:</span><input class=\"ah-dti-time-input\" data-field=\"minutes\" type=\"text\" inputmode=\"numeric\" maxlength=\"2\" value=\"";o+=R.esc(R.lookup(S,["minutes"]));o+="\" aria-label=\"Minutes\"></div>";S.shift();}}return o;};
  AH.tpl["notification"] = function(d){var S=[d],o="";o+="<div class=\"ah-notify ah-notify-";o+=R.esc(R.lookup(S,["variant"]));var v0=R.lookup(S,["clickable"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" ah-notify-clickable";S.shift();}}else if(v0===true){o+=" ah-notify-clickable";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" ah-notify-clickable";S.shift();}}o+="\" role=\"alert\"";var v0=R.lookup(S,["width"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" style=\"width:";o+=R.esc(R.lookup(S,["width"]));o+="\"";S.shift();}}else if(v0===true){o+=" style=\"width:";o+=R.esc(R.lookup(S,["width"]));o+="\"";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" style=\"width:";o+=R.esc(R.lookup(S,["width"]));o+="\"";S.shift();}}o+="><span class=\"ah-notify-icon\">";var v0=R.lookup(S,["info"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"16\" x2=\"12\" y2=\"12\"/><line x1=\"12\" y1=\"8\" x2=\"12.01\" y2=\"8\"/></svg>";S.shift();}}else if(v0===true){o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"16\" x2=\"12\" y2=\"12\"/><line x1=\"12\" y1=\"8\" x2=\"12.01\" y2=\"8\"/></svg>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"16\" x2=\"12\" y2=\"12\"/><line x1=\"12\" y1=\"8\" x2=\"12.01\" y2=\"8\"/></svg>";S.shift();}}var v0=R.lookup(S,["success"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M22 11.08V12a10 10 0 1 1-5.93-9.14\"/><polyline points=\"22 4 12 14.01 9 11.01\"/></svg>";S.shift();}}else if(v0===true){o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M22 11.08V12a10 10 0 1 1-5.93-9.14\"/><polyline points=\"22 4 12 14.01 9 11.01\"/></svg>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M22 11.08V12a10 10 0 1 1-5.93-9.14\"/><polyline points=\"22 4 12 14.01 9 11.01\"/></svg>";S.shift();}}var v0=R.lookup(S,["warning"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z\"/><line x1=\"12\" y1=\"9\" x2=\"12\" y2=\"13\"/><line x1=\"12\" y1=\"17\" x2=\"12.01\" y2=\"17\"/></svg>";S.shift();}}else if(v0===true){o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z\"/><line x1=\"12\" y1=\"9\" x2=\"12\" y2=\"13\"/><line x1=\"12\" y1=\"17\" x2=\"12.01\" y2=\"17\"/></svg>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z\"/><line x1=\"12\" y1=\"9\" x2=\"12\" y2=\"13\"/><line x1=\"12\" y1=\"17\" x2=\"12.01\" y2=\"17\"/></svg>";S.shift();}}var v0=R.lookup(S,["error"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"15\" y1=\"9\" x2=\"9\" y2=\"15\"/><line x1=\"9\" y1=\"9\" x2=\"15\" y2=\"15\"/></svg>";S.shift();}}else if(v0===true){o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"15\" y1=\"9\" x2=\"9\" y2=\"15\"/><line x1=\"9\" y1=\"9\" x2=\"15\" y2=\"15\"/></svg>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"15\" y1=\"9\" x2=\"9\" y2=\"15\"/><line x1=\"9\" y1=\"9\" x2=\"15\" y2=\"15\"/></svg>";S.shift();}}o+="</span><div class=\"ah-notify-content\">";o+=R.str(R.lookup(S,["content"]));o+="</div>";var v0=R.lookup(S,["closable"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<span class=\"ah-notify-close\" role=\"button\" tabindex=\"0\" aria-label=\"Close\"></span>";S.shift();}}else if(v0===true){o+="<span class=\"ah-notify-close\" role=\"button\" tabindex=\"0\" aria-label=\"Close\"></span>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<span class=\"ah-notify-close\" role=\"button\" tabindex=\"0\" aria-label=\"Close\"></span>";S.shift();}}o+="</div>";return o;};
  AH.tpl["pagination_items"] = function(d){var S=[d],o="";var v0=R.lookup(S,["entries"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);var v1=R.lookup(S,["gap"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";S.shift();}}else if(v1===true){o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";S.shift();}}var v1=R.lookup(S,["info"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";S.shift();}}else if(v1===true){o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";S.shift();}}var v1=R.lookup(S,["item"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}S.shift();}}var v1=R.lookup(S,["nav"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}S.shift();}}S.shift();}}else if(v0===true){var v1=R.lookup(S,["gap"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";S.shift();}}else if(v1===true){o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";S.shift();}}var v1=R.lookup(S,["info"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";S.shift();}}else if(v1===true){o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";S.shift();}}var v1=R.lookup(S,["item"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}S.shift();}}var v1=R.lookup(S,["nav"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}S.shift();}}}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);var v1=R.lookup(S,["gap"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";S.shift();}}else if(v1===true){o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";S.shift();}}var v1=R.lookup(S,["info"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";S.shift();}}else if(v1===true){o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";S.shift();}}var v1=R.lookup(S,["item"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}S.shift();}}var v1=R.lookup(S,["nav"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}S.shift();}}S.shift();}}return o;};
  AH.tpl["steps_indicator"] = function(d){var S=[d],o="";var v0=R.lookup(S,["check"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<span class=\"ah-steps-check\">✓</span>";S.shift();}}else if(v0===true){o+="<span class=\"ah-steps-check\">✓</span>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<span class=\"ah-steps-check\">✓</span>";S.shift();}}var v0=R.lookup(S,["error"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<span class=\"ah-steps-error-icon\">✕</span>";S.shift();}}else if(v0===true){o+="<span class=\"ah-steps-error-icon\">✕</span>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<span class=\"ah-steps-error-icon\">✕</span>";S.shift();}}var v0=R.lookup(S,["plain"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=R.esc(R.lookup(S,["number"]));S.shift();}}else if(v0===true){o+=R.esc(R.lookup(S,["number"]));}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=R.esc(R.lookup(S,["number"]));S.shift();}}return o;};
  AH.tpl["tag_input_chip"] = function(d){var S=[d],o="";o+="<span class=\"ah-chip ah-tag-input__chip\" data-variant=\"";o+=R.esc(R.lookup(S,["variant"]));o+="\" data-color=\"";o+=R.esc(R.lookup(S,["color"]));o+="\" data-size=\"small\" data-index=\"";o+=R.esc(R.lookup(S,["index"]));o+="\"><span class=\"ah-chip__label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span><button type=\"button\" class=\"ah-chip__delete\" tabindex=\"-1\" aria-label=\"Remove ";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v0=R.lookup(S,["disabled"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" disabled";S.shift();}}else if(v0===true){o+=" disabled";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" disabled";S.shift();}}o+=">&times;</button></span>";return o;};
  AH.tpl["timepicker_header"] = function(d){var S=[d],o="";o+="<span class=\"ah-timepicker-header-hours";var v0=R.lookup(S,["hours_active"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" ah-timepicker-header-active";S.shift();}}else if(v0===true){o+=" ah-timepicker-header-active";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" ah-timepicker-header-active";S.shift();}}o+="\" data-action=\"select-hours\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v0=R.lookup(S,["hours_active"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="true";S.shift();}}else if(v0===true){o+="true";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["hours_active"]))){o+="false";}o+="\" aria-label=\"Hours\"";var v0=R.lookup(S,["disabled"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v0===true){o+=" aria-disabled=\"true\"";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" aria-disabled=\"true\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["hours"]));o+="</span><span class=\"ah-timepicker-header-sep\">:</span><span class=\"ah-timepicker-header-minutes";var v0=R.lookup(S,["minutes_active"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" ah-timepicker-header-active";S.shift();}}else if(v0===true){o+=" ah-timepicker-header-active";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" ah-timepicker-header-active";S.shift();}}o+="\" data-action=\"select-minutes\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v0=R.lookup(S,["minutes_active"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="true";S.shift();}}else if(v0===true){o+="true";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["minutes_active"]))){o+="false";}o+="\" aria-label=\"Minutes\"";var v0=R.lookup(S,["disabled"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v0===true){o+=" aria-disabled=\"true\"";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" aria-disabled=\"true\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["minutes"]));o+="</span>";var v0=R.lookup(S,["twelve"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<span class=\"ah-timepicker-header-period\"><span class=\"ah-timepicker-header-am";var v1=R.lookup(S,["am"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-header-am-active";S.shift();}}else if(v1===true){o+=" ah-timepicker-header-am-active";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-header-am-active";S.shift();}}o+="\" data-action=\"set-am\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v1=R.lookup(S,["am"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="true";S.shift();}}else if(v1===true){o+="true";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["am"]))){o+="false";}o+="\" aria-label=\"AM\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}o+=">AM</span><span class=\"ah-timepicker-header-pm";var v1=R.lookup(S,["pm"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-header-pm-active";S.shift();}}else if(v1===true){o+=" ah-timepicker-header-pm-active";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-header-pm-active";S.shift();}}o+="\" data-action=\"set-pm\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v1=R.lookup(S,["pm"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="true";S.shift();}}else if(v1===true){o+="true";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["pm"]))){o+="false";}o+="\" aria-label=\"PM\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}o+=">PM</span></span>";S.shift();}}else if(v0===true){o+="<span class=\"ah-timepicker-header-period\"><span class=\"ah-timepicker-header-am";var v1=R.lookup(S,["am"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-header-am-active";S.shift();}}else if(v1===true){o+=" ah-timepicker-header-am-active";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-header-am-active";S.shift();}}o+="\" data-action=\"set-am\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v1=R.lookup(S,["am"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="true";S.shift();}}else if(v1===true){o+="true";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["am"]))){o+="false";}o+="\" aria-label=\"AM\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}o+=">AM</span><span class=\"ah-timepicker-header-pm";var v1=R.lookup(S,["pm"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-header-pm-active";S.shift();}}else if(v1===true){o+=" ah-timepicker-header-pm-active";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-header-pm-active";S.shift();}}o+="\" data-action=\"set-pm\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v1=R.lookup(S,["pm"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="true";S.shift();}}else if(v1===true){o+="true";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["pm"]))){o+="false";}o+="\" aria-label=\"PM\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}o+=">PM</span></span>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<span class=\"ah-timepicker-header-period\"><span class=\"ah-timepicker-header-am";var v1=R.lookup(S,["am"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-header-am-active";S.shift();}}else if(v1===true){o+=" ah-timepicker-header-am-active";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-header-am-active";S.shift();}}o+="\" data-action=\"set-am\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v1=R.lookup(S,["am"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="true";S.shift();}}else if(v1===true){o+="true";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["am"]))){o+="false";}o+="\" aria-label=\"AM\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}o+=">AM</span><span class=\"ah-timepicker-header-pm";var v1=R.lookup(S,["pm"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-header-pm-active";S.shift();}}else if(v1===true){o+=" ah-timepicker-header-pm-active";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-header-pm-active";S.shift();}}o+="\" data-action=\"set-pm\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v1=R.lookup(S,["pm"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="true";S.shift();}}else if(v1===true){o+="true";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["pm"]))){o+="false";}o+="\" aria-label=\"PM\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}o+=">PM</span></span>";S.shift();}}return o;};
  AH.tpl["timepicker_numbers"] = function(d){var S=[d],o="";var v0=R.lookup(S,["numbers"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<text class=\"ah-timepicker-number";var v1=R.lookup(S,["inner"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-inner";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-inner";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-inner";S.shift();}}var v1=R.lookup(S,["selected"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-selected";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-selected";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-selected";S.shift();}}var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-disabled";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-disabled";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-disabled";S.shift();}}o+="\" x=\"";o+=R.esc(R.lookup(S,["x"]));o+="\" y=\"";o+=R.esc(R.lookup(S,["y"]));o+="\" data-val=\"";o+=R.esc(R.lookup(S,["val"]));o+="\">";o+=R.esc(R.lookup(S,["label"]));o+="</text>";S.shift();}}else if(v0===true){o+="<text class=\"ah-timepicker-number";var v1=R.lookup(S,["inner"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-inner";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-inner";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-inner";S.shift();}}var v1=R.lookup(S,["selected"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-selected";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-selected";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-selected";S.shift();}}var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-disabled";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-disabled";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-disabled";S.shift();}}o+="\" x=\"";o+=R.esc(R.lookup(S,["x"]));o+="\" y=\"";o+=R.esc(R.lookup(S,["y"]));o+="\" data-val=\"";o+=R.esc(R.lookup(S,["val"]));o+="\">";o+=R.esc(R.lookup(S,["label"]));o+="</text>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<text class=\"ah-timepicker-number";var v1=R.lookup(S,["inner"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-inner";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-inner";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-inner";S.shift();}}var v1=R.lookup(S,["selected"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-selected";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-selected";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-selected";S.shift();}}var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-disabled";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-disabled";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-disabled";S.shift();}}o+="\" x=\"";o+=R.esc(R.lookup(S,["x"]));o+="\" y=\"";o+=R.esc(R.lookup(S,["y"]));o+="\" data-val=\"";o+=R.esc(R.lookup(S,["val"]));o+="\">";o+=R.esc(R.lookup(S,["label"]));o+="</text>";S.shift();}}return o;};
  AH.tpl["toast"] = function(d){var S=[d],o="";var v0=R.lookup(S,["has_title"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-toast__title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-toast__title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-toast__title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div>";S.shift();}}var v0=R.lookup(S,["has_description"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-toast__description\">";o+=R.esc(R.lookup(S,["description"]));o+="</div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-toast__description\">";o+=R.esc(R.lookup(S,["description"]));o+="</div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-toast__description\">";o+=R.esc(R.lookup(S,["description"]));o+="</div>";S.shift();}}return o;};
  AH.tpl["upload_item"] = function(d){var S=[d],o="";o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" data-file-id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\"><div class=\"ah-upload-item-icon ah-upload-item-icon-";o+=R.esc(R.lookup(S,["icon"]));o+="\" aria-hidden=\"true\"></div><div class=\"ah-upload-item-info\"><div class=\"ah-upload-item-name-row\"><span class=\"ah-upload-item-name\" title=\"";o+=R.esc(R.lookup(S,["name"]));o+="\">";o+=R.esc(R.lookup(S,["name"]));o+="</span><span class=\"ah-upload-item-size\">";o+=R.esc(R.lookup(S,["size"]));o+="</span></div>";var v0=R.lookup(S,["uploading"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-upload-item-progress\" role=\"progressbar\" aria-valuemin=\"0\" aria-valuemax=\"100\" aria-valuenow=\"";o+=R.esc(R.lookup(S,["percent"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["name"]));o+="\"><div class=\"ah-upload-item-progress-bar\" style=\"width:";o+=R.esc(R.lookup(S,["percent"]));o+="%\"></div></div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-upload-item-progress\" role=\"progressbar\" aria-valuemin=\"0\" aria-valuemax=\"100\" aria-valuenow=\"";o+=R.esc(R.lookup(S,["percent"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["name"]));o+="\"><div class=\"ah-upload-item-progress-bar\" style=\"width:";o+=R.esc(R.lookup(S,["percent"]));o+="%\"></div></div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-upload-item-progress\" role=\"progressbar\" aria-valuemin=\"0\" aria-valuemax=\"100\" aria-valuenow=\"";o+=R.esc(R.lookup(S,["percent"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["name"]));o+="\"><div class=\"ah-upload-item-progress-bar\" style=\"width:";o+=R.esc(R.lookup(S,["percent"]));o+="%\"></div></div>";S.shift();}}var v0=R.lookup(S,["has_error"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-upload-item-error\" role=\"alert\">";o+=R.esc(R.lookup(S,["error"]));o+="</div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-upload-item-error\" role=\"alert\">";o+=R.esc(R.lookup(S,["error"]));o+="</div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-upload-item-error\" role=\"alert\">";o+=R.esc(R.lookup(S,["error"]));o+="</div>";S.shift();}}o+="</div><div class=\"ah-upload-item-actions\"><button type=\"button\" class=\"ah-upload-item-remove\" title=\"";o+=R.esc(R.lookup(S,["remove"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["remove"]));o+=" ";o+=R.esc(R.lookup(S,["name"]));o+="\"";var v0=R.lookup(S,["disabled"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" disabled";S.shift();}}else if(v0===true){o+=" disabled";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" disabled";S.shift();}}o+=">&times;</button></div></div>";return o;};
})(window.AH);

/* ---- components/data_tree.js ---- */
/* Behaviours of the data_tree components (designs/04-components.md).
 *
 * Ported from sigil (data/tree, layout/nav_tree, data/heatmap_calendar;
 * data/diff needs no behaviour). The server renders every node, link and
 * cell; this file only moves state around in that DOM:
 *
 *   tree              expand / collapse (slide), single selection, keyboard
 *                     (roving tabindex, arrows, Home/End, Enter/Space, *,
 *                     type-ahead), lazy nodes loaded through the tree's
 *                     load action (data-load: a signed token), which
 *                     answers with set_children/3 -> childrenLoaded
 *   nav-tree          active link, open nodes, change on navigation
 *   heatmap-calendar  hover tooltip (AH.float), ah:select on click
 *
 * Value-bearing roots keep their value in data-ah-value, mirror it into a
 * hidden input and fire "change" when the user changes it; methods called
 * by the server (AH.invoke / aihtml_action:call) do not fire it.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  // ------------------------------------------------------------------
  // Tree
  // ------------------------------------------------------------------

  function items(el) {
    return $(el).find("li[role=treeitem]");
  }

  function own(el, node) {
    return $(node).closest(".ah-tree")[0] === el;
  }

  function byValue(el, v) {
    if (v === null || v === undefined) { return null; }
    v = String(v);
    var found = null;
    items(el).each(function () {
      if (this.getAttribute("data-value") === v) { found = this; return false; }
    });
    return found;
  }

  function expandable(li) { return li.hasAttribute("aria-expanded"); }
  function isOpen(li) { return li.getAttribute("aria-expanded") === "true"; }
  function isDisabled(li) { return li.getAttribute("aria-disabled") === "true"; }
  function group(li) { return $(li).children("ul.ah-tree-list"); }
  function parentItem(el, li) {
    var p = $(li).parent().closest("li[role=treeitem]")[0];
    return p && $.contains(el, p) ? p : null;
  }
  function label(li) {
    return $(li).children(".ah-tree-row").find(".ah-tree-label").first().text();
  }
  function info(li) {
    return { value: li.getAttribute("data-value"), label: label(li), id: li.id };
  }

  // Items whose ancestors are all open, in document order.
  function visible(el) {
    return items(el).filter(function () {
      for (var p = parentItem(el, this); p; p = parentItem(el, p)) {
        if (!isOpen(p)) { return false; }
      }
      return true;
    }).toArray();
  }

  function treeOff(el) {
    return el.getAttribute("aria-disabled") === "true" || $(el).hasClass("ah-tree-disabled");
  }

  // Roving tabindex: exactly one item is in the tab order.
  function focusItem(el, li, move) {
    if (!li) { return; }
    items(el).attr("tabindex", "-1");
    li.setAttribute("tabindex", "0");
    if (move) { li.focus(); }
  }

  function setOpen(el, li, open, animate) {
    if (!li || !expandable(li) || isOpen(li) === open) { return; }
    if (open && li.getAttribute("data-lazy") === "true") {
      load(el, li);
      return;
    }
    li.setAttribute("aria-expanded", String(open));
    $(li).children(".ah-tree-row").children(".ah-tree-toggle").toggleClass("ah-tree-toggle-open", open);
    // collapsing hides the focused item: the node itself takes the tab stop
    if (!open && $(li).find("li[tabindex='0']").length) { focusItem(el, li, $.contains(li, document.activeElement)); }
    var $ul = group(li);
    var done = function () { $(el).trigger(open ? "ah:expand" : "ah:collapse", [info(li)]); };
    if (animate !== false && el.getAttribute("data-animation") !== "none" && $.fn.slideDown) {
      $ul.stop(true, true)[open ? "slideDown" : "slideUp"](200, done);
    } else {
      $ul.css("display", open ? "" : "none");
      done();
    }
  }

  // A lazy node asks the server for its children: the tree's load token is
  // bound to the node (ah:load), so each node has its own request.
  function load(el, li) {
    if ($(li).hasClass("ah-tree-item-loading")) { return; }
    $(li).addClass("ah-tree-item-loading").attr("aria-busy", "true");
    var token = el.getAttribute("data-load");
    if (token && !li.hasAttribute("data-ah-on")) {
      li.setAttribute("data-ah-on", "ah:load:" + token);
      AH.mount(li);                 // registers the ah:load listener
    }
    $(li).trigger("ah:load", [info(li)]);
  }

  function childrenLoaded(el, li) {
    if (!li) { return; }
    $(li).removeClass("ah-tree-item-loading").removeAttr("aria-busy")
      .removeAttr("data-lazy").removeAttr("data-ah-on");
    if (!group(li).children("li").length) {
      li.removeAttribute("aria-expanded");
      $(li).addClass("ah-tree-item-leaf");
      $(li).children(".ah-tree-row").children(".ah-tree-toggle").addClass("ah-tree-toggle-leaf");
      return;
    }
    var sel = byValue(el, el.getAttribute("data-ah-value"));
    if (sel && $.contains(li, sel)) { mark(el, sel); }
    setOpen(el, li, true);
  }

  function mark(el, li) {
    items(el).filter("[aria-selected]").removeAttr("aria-selected")
      .children(".ah-tree-row").removeClass("ah-tree-row-selected");
    if (li) {
      li.setAttribute("aria-selected", "true");
      $(li).children(".ah-tree-row").addClass("ah-tree-row-selected");
    }
  }

  function writeValue(el, v) {
    el.setAttribute("data-ah-value", v);
    $(el).children("input[type=hidden]").val(v);
  }

  function select(el, li, user) {
    if (!li || isDisabled(li)) { return; }
    var prev = el.getAttribute("data-ah-value");
    var v = li.getAttribute("data-value");
    mark(el, li);
    writeValue(el, v);
    focusItem(el, li, false);
    if (user && prev !== v) { $(el).trigger("change"); }
  }

  function ensureVisible(el, li) {
    for (var p = parentItem(el, li); p; p = parentItem(el, p)) { setOpen(el, p, true, false); }
  }

  function typeahead(el, current, ch) {
    var vis = visible(el);
    var start = vis.indexOf(current);
    for (var i = 1; i <= vis.length; i++) {
      var li = vis[(start + i) % vis.length];
      if (label(li).trim().toLowerCase().indexOf(ch) === 0) { return li; }
    }
    return null;
  }

  function keydown(el, e) {
    var li = $(e.target).closest("li[role=treeitem]")[0];
    if (!li || !own(el, li) || treeOff(el) || e.altKey || e.ctrlKey || e.metaKey) { return; }
    var vis = visible(el);
    var i = vis.indexOf(li);
    var to = null;
    switch (e.key) {
      case "ArrowDown": to = vis[Math.min(i + 1, vis.length - 1)]; break;
      case "ArrowUp": to = vis[Math.max(i - 1, 0)]; break;
      case "Home": to = vis[0]; break;
      case "End": to = vis[vis.length - 1]; break;
      case "ArrowRight":
        if (expandable(li) && !isOpen(li)) { setOpen(el, li, true); }
        else if (isOpen(li)) { to = group(li).children("li")[0] || null; }
        break;
      case "ArrowLeft":
        if (isOpen(li)) { setOpen(el, li, false); } else { to = parentItem(el, li); }
        break;
      case "Enter":
      case " ":
        select(el, li, true);
        break;
      case "*":
        $(li).siblings("li[aria-expanded]").addBack().each(function () { setOpen(el, this, true); });
        break;
      default:
        if (e.key && e.key.length === 1 && /\S/.test(e.key)) {
          to = typeahead(el, li, e.key.toLowerCase());
          if (!to) { return; }
        } else {
          return;
        }
    }
    e.preventDefault();
    if (to) { focusItem(el, to, true); }
  }

  function rowClick(el, e, dbl) {
    var li = $(e.target).closest("li[role=treeitem]")[0];
    if (!li || !own(el, li) || isDisabled(li) || treeOff(el)) { return; }
    var onToggle = $(e.target).closest(".ah-tree-toggle").length > 0;
    var mode = el.getAttribute("data-toggle-mode") || "click";
    if (dbl) {
      if (mode === "dblclick" && !onToggle) { setOpen(el, li, !isOpen(li)); }
      return;
    }
    $(el).trigger("ah:item-click", [info(li)]);
    select(el, li, true);
    focusItem(el, li, true);
    if (expandable(li) && (onToggle || mode === "click")) { setOpen(el, li, !isOpen(li)); }
  }

  AH.define("tree", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-tree-row", function (e) { rowClick(el, e, false); });
      $el.on("dblclick" + NS, ".ah-tree-row", function (e) { rowClick(el, e, true); });
      $el.on("keydown" + NS, function (e) { keydown(el, e); });
      if (!items(el).filter("[tabindex='0']").length) { focusItem(el, visible(el)[0], false); }
    },
    methods: {
      setValue: function (el, $el, v) {
        var li = byValue(el, v);
        if (!li) {
          mark(el, null);
          writeValue(el, "");
          return;
        }
        ensureVisible(el, li);
        select(el, li, false);
      },
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      expand: function (el, $el, v) { setOpen(el, byValue(el, v), true); },
      collapse: function (el, $el, v) { setOpen(el, byValue(el, v), false); },
      expandAll: function (el) {
        items(el).filter("[aria-expanded]").not("[data-lazy]").each(function () { setOpen(el, this, true); });
      },
      collapseAll: function (el) {
        items(el).filter("[aria-expanded]").each(function () { setOpen(el, this, false); });
      },
      ensureVisible: function (el, $el, v) {
        var li = byValue(el, v);
        if (li) { ensureVisible(el, li); }
      },
      childrenLoaded: function (el, $el, id) { childrenLoaded(el, document.getElementById(id)); }
    }
  });

  // ------------------------------------------------------------------
  // NavTree: the server renders links and <details>; this keeps the
  // active link and fires change when the user follows one
  // ------------------------------------------------------------------

  function navActivate(el, route) {
    var $el = $(el);
    $el.find(".ah-nav-tree__item.ah-is-active").removeClass("ah-is-active").removeAttr("aria-current");
    var $a = $el.find("a.ah-nav-tree__item").filter(function () {
      return this.getAttribute("data-route") === route;
    }).first();
    $a.addClass("ah-is-active").attr("aria-current", "page");
    // as sigil re-renders: the nodes around the active link are open, others closed
    $el.find("details.ah-nav-tree__node").each(function () {
      var hit = $a.length > 0 && $.contains(this, $a[0]);
      this.open = hit;
      $(this).children("summary").toggleClass("ah-is-open", hit);
    });
    el.setAttribute("data-ah-value", route || "");
  }

  AH.define("nav-tree", {
    init: function (el, $el) {
      $el.on("click" + NS, "a.ah-nav-tree__item[data-route]", function () {
        var route = this.getAttribute("data-route");
        var prev = el.getAttribute("data-ah-value");
        navActivate(el, route);
        if (route !== prev) { $el.trigger("change"); }
      });
    },
    methods: {
      setValue: function (el, $el, route) { navActivate(el, route === null || route === undefined ? "" : String(route)); },
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; }
    }
  });

  // ------------------------------------------------------------------
  // HeatmapCalendar: tooltip and select; the grid is server-rendered
  // ------------------------------------------------------------------

  AH.define("heatmap-calendar", {
    init: function (el, $el) {
      var tip = $el.children(".ah-heatmap-calendar__tooltip")[0];
      var fmt = el.getAttribute("data-tip") || "{value} · {date}";
      var st = { float: null };
      $.data(el, "ah-heatmap", st);
      function hide() {
        if (st.float) { st.float.stop(); st.float = null; }
        if (tip) { tip.setAttribute("data-visible", "false"); }
      }
      $el.on("mouseenter" + NS, ".ah-heatmap-calendar__cell", function () {
        if (!tip) { return; }
        hide();
        tip.textContent = fmt.split("{date}").join(this.getAttribute("data-date"))
                             .split("{value}").join(this.getAttribute("data-value"));
        tip.setAttribute("data-visible", "true");
        st.float = AH.float(tip, this, { placement: "top", align: "center", offset: 6 });
      });
      $el.on("mouseleave" + NS, ".ah-heatmap-calendar__cell", hide);
      $el.on("click" + NS, ".ah-heatmap-calendar__cell", function () {
        var date = this.getAttribute("data-date");
        el.setAttribute("data-ah-value", date);
        $el.trigger("ah:select", [{ date: date, value: parseFloat(this.getAttribute("data-value")) }]);
      });
    },
    destroy: function (el) {
      var st = $.data(el, "ah-heatmap");
      if (st && st.float) { st.float.stop(); }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/display.js ---- */
/* Behaviours of the display components (designs/04-components.md):
   avatar, badge, chip, time-ago, expandable-text, progressbar,
   progress-circle, kpi-card, timeline, ranking-list, tag-cloud, alert. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  function num(v, dflt) {
    var n = parseFloat(v);
    return isNaN(n) ? dflt : n;
  }

  function clamp(v, lo, hi) {
    return Math.max(lo, Math.min(hi, v));
  }

  // Remove an element the way the runtime does (behaviours destroyed first).
  function drop(el) {
    AH.destroy(el);
    $(el).remove();
  }

  // Enter / Space act as a click on focusable non-button elements.
  function keyClick(e) {
    if (e.key === "Enter" || e.key === " ") {
      e.preventDefault();
      $(e.currentTarget).trigger("click");
    }
  }

  // ------------------------------------------------------------------
  // avatar: a failed image shows the fallback underneath
  // ------------------------------------------------------------------

  AH.define("avatar", {
    init: function (el, $el) {
      $el.find(".ah-avatar__image").each(function () {
        var img = this;
        var broken = function () { $(img).addClass("ah-avatar__image--broken"); };
        // error does not bubble, and may have happened before init
        $(img).on("error" + NS, broken);
        if (img.complete && img.naturalWidth === 0) { broken(); }
      });
    },
    destroy: function (el, $el) {
      $el.find(".ah-avatar__image").off(NS);
    }
  });

  // ------------------------------------------------------------------
  // badge: setCount(n) with the same max / zero rules as the server
  // ------------------------------------------------------------------

  AH.define("badge", {
    methods: {
      setCount: function (el, $el, n) {
        var $ind = $el.children(".ah-badge-indicator");
        if ($ind.attr("data-dot") === "true") { return; }
        var max = num($el.attr("data-ah-max"), 99);
        var isNum = typeof n === "number" || (n !== "" && n !== null && !isNaN(n));
        var v = isNum ? Number(n) : n;
        $ind.text(v === null || v === undefined ? "" : (isNum && v > max ? max + "+" : String(v)));
        var hide = isNum && v === 0 && $el.attr("data-ah-show-zero") !== "true";
        $ind.attr("data-invisible", hide ? "true" : "false");
      }
    }
  });

  // ------------------------------------------------------------------
  // chip: remove button, keyboard
  // ------------------------------------------------------------------

  function removeChip(el, $el) {
    var ev = $.Event("ah:remove");
    $el.trigger(ev, [{ value: $el.attr("data-ah-value") }]);
    if (ev.isDefaultPrevented()) { return false; }
    // value contract: the root's change carries data-ah-value
    $el.trigger("change");
    drop(el);
    return true;
  }

  AH.define("chip", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-chip__delete", function (e) {
        e.stopPropagation();
        if ($el.attr("data-disabled") !== "true") { removeChip(el, $el); }
      });
      $el.on("keydown" + NS, function (e) {
        if (e.target !== el || $el.attr("data-disabled") === "true") { return; }
        if ((e.key === "Backspace" || e.key === "Delete") && $el.find(".ah-chip__delete").length) {
          e.preventDefault();
          var $next = $el.next("[tabindex]").length ? $el.next("[tabindex]") : $el.prev("[tabindex]");
          if (removeChip(el, $el)) { $next.trigger("focus"); }
        } else if ($el.attr("data-clickable") === "true") {
          keyClick(e);
        }
      });
    },
    methods: {
      remove: function (el, $el) { removeChip(el, $el); }
    }
  });

  // ------------------------------------------------------------------
  // time-ago: sigil's format, refreshed every 60 s
  // ------------------------------------------------------------------

  var AGO = { "just-now": "just now", minutes: "{n}m ago", hours: "{n}h ago",
              days: "{n}d ago", months: "{n}mo ago" };

  function formatAgo(el, t) {
    var label = function (k, n) {
      var s = el.getAttribute("data-ah-label-" + k) || AGO[k];
      return n === undefined ? s : s.split("{n}").join(String(n));
    };
    var secs = Math.floor((Date.now() - t) / 1000);
    var mins = Math.floor(secs / 60), hours = Math.floor(mins / 60);
    var days = Math.floor(hours / 24), months = Math.floor(days / 30);
    if (secs < 60) { return label("just-now"); }
    if (mins < 60) { return label("minutes", mins); }
    if (hours < 24) { return label("hours", hours); }
    if (days < 30) { return label("days", days); }
    return label("months", months);
  }

  function renderAgo(el) {
    var t = Date.parse(el.getAttribute("datetime"));
    if (isNaN(t)) { return; }
    el.textContent = formatAgo(el, t);
    if (el.getAttribute("data-ah-title") === "true") {
      el.setAttribute("title", new Date(t).toLocaleString());
    }
  }

  function stopAgo(el) {
    clearInterval($.data(el, "ah-timer"));
    $.removeData(el, "ah-timer");
  }

  AH.define("time-ago", {
    init: function (el) {
      renderAgo(el);
      if (el.getAttribute("data-ah-live") === "false") { return; }
      $.data(el, "ah-timer", setInterval(function () {
        // removed without AH.destroy: stop instead of leaking
        if (!document.documentElement.contains(el)) { stopAgo(el); return; }
        renderAgo(el);
      }, 60000));
    },
    destroy: stopAgo,
    methods: {
      // iso string, Date or epoch milliseconds
      setDate: function (el, $el, d) {
        var t = typeof d === "number" ? d : Date.parse(d instanceof Date ? d.toISOString() : d);
        if (isNaN(t)) { return; }
        el.setAttribute("datetime", new Date(t).toISOString().replace(/\.\d{3}Z$/, "Z"));
        renderAgo(el);
      },
      refresh: function (el) { renderAgo(el); }
    }
  });

  // ------------------------------------------------------------------
  // expandable-text
  // ------------------------------------------------------------------

  function setExpanded(el, $el, on) {
    var $btn = $el.children(".ah-expandable-text__toggle");
    if (!$btn.length || ($el.attr("data-expanded") === "true") === on) { return; }
    $el.attr("data-expanded", on ? "true" : "false");
    $el.find("[data-ah-part=short]").prop("hidden", on);
    $el.find("[data-ah-part=full]").prop("hidden", !on);
    $btn.attr("aria-expanded", on ? "true" : "false")
      .text($btn.attr(on ? "data-ah-collapse-label" : "data-ah-expand-label"));
    $el.trigger("ah:toggle", [on]);
  }

  AH.define("expandable-text", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-expandable-text__toggle", function () {
        setExpanded(el, $el, $el.attr("data-expanded") !== "true");
      });
    },
    methods: {
      toggle: function (el, $el) { setExpanded(el, $el, $el.attr("data-expanded") !== "true"); },
      expand: function (el, $el) { setExpanded(el, $el, true); },
      collapse: function (el, $el) { setExpanded(el, $el, false); }
    }
  });

  // ------------------------------------------------------------------
  // progressbar / progress-circle: setValue
  // ------------------------------------------------------------------

  function pct(v, lo, hi) {
    return hi > lo ? 100 * (v - lo) / (hi - lo) : 0;
  }

  function fireProgress($el, old, v, max) {
    if (old === v) { return; }
    $el.trigger("change", [{ previous: old, value: v }]);
    if (v === max) { $el.trigger("ah:complete", [{ previous: old, value: v }]); }
  }

  AH.define("progressbar", {
    methods: {
      setValue: function (el, $el, value, text) {
        var lo = num($el.attr("data-ah-min"), 0), hi = num($el.attr("data-ah-max"), 100);
        var old = num($el.attr("data-ah-value"), lo);
        var v = clamp(num(value, lo), lo, hi);
        var p = pct(v, lo, hi);
        var dim = $el.hasClass("ah-progressbar-vertical") ? "height" : "width";
        $el.removeClass("ah-progressbar-indeterminate").removeAttr("aria-busy");
        $el.children(".ah-progressbar-value, .ah-progressbar-value-vertical").css(dim, p + "%");
        $el.children(".ah-progressbar-range").each(function () {
          var stop = num(this.getAttribute("data-ah-stop"), hi);
          $(this).css(dim, pct(Math.min(stop, hi, v), lo, hi) + "%");
        });
        var label = text !== undefined && text !== null ? String(text)
          : ($el.attr("data-ah-text") === "custom" ? null : Math.round(p) + "%");
        if (label !== null) { $el.find(".ah-progressbar-text").text(label); }
        $el.attr({ "data-ah-value": v, "aria-valuenow": v,
                   "aria-valuetext": label !== null ? label : Math.round(p) + "%" });
        fireProgress($el, old, v, hi);
      },
      getValue: function (el, $el) { return num($el.attr("data-ah-value"), 0); }
    }
  });

  var CIRC = 2 * Math.PI * 45;

  AH.define("progress-circle", {
    methods: {
      setValue: function (el, $el, value) {
        var old = num($el.attr("data-ah-value"), 0);
        var v = Math.trunc(clamp(num(value, 0), 0, 100));
        $el.removeClass("ah-progress-circle--indeterminate").removeAttr("aria-busy");
        $el.find(".ah-progress-circle-fill").attr("stroke-dashoffset", CIRC * (1 - v / 100));
        $el.find(".ah-progress-circle-value").text(v + "%");
        $el.attr({ "data-ah-value": v, "aria-valuenow": v, "aria-valuetext": v + "%" });
        fireProgress($el, old, v, 100);
      },
      getValue: function (el, $el) { return num($el.attr("data-ah-value"), 0); }
    }
  });

  // ------------------------------------------------------------------
  // kpi-card: setValue / setTrend
  // ------------------------------------------------------------------

  AH.define("kpi-card", {
    methods: {
      setValue: function (el, $el, value) {
        $el.find(".ah-kpi-card-value").text(String(value));
      },
      setTrend: function (el, $el, trend) {
        var t = num(trend, NaN);
        var $t = $el.find(".ah-kpi-card-trend");
        if (isNaN(t) || !$t.length) { return; }
        var up = t > 0, cls = up ? "ah-kpi-card-trend-up" : "ah-kpi-card-trend-down";
        $el.removeClass("ah-kpi-card-trend-up ah-kpi-card-trend-down").addClass(cls);
        $t.children().first().removeClass("ah-kpi-card-trend-up ah-kpi-card-trend-down")
          .addClass(cls);
        $t.find(".ah-kpi-card-trend-value").text((up ? "+" : "") + t.toFixed(1) + "%");
        // swap the arrow: mirror the polylines vertically
        $t.find(".ah-kpi-card-trend-icon polyline").each(function (i) {
          var pts = [["23 6 13.5 15.5 8.5 10.5 1 18", "17 6 23 6 23 12"],
                     ["23 18 13.5 8.5 8.5 13.5 1 6", "17 18 23 18 23 12"]][up ? 0 : 1];
          this.setAttribute("points", pts[i] || pts[0]);
        });
      }
    }
  });

  // ------------------------------------------------------------------
  // timeline: cards with a description expand on click / Enter
  // ------------------------------------------------------------------

  AH.define("timeline", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-timeline-item[ah-collapsible]", function () {
        var $item = $(this).toggleClass("ah-timeline-item-expanded");
        var on = $item.hasClass("ah-timeline-item-expanded");
        $item.attr("aria-expanded", on ? "true" : "false");
        // three grid cells per item
        var cell = $item.closest(".ah-timeline-near-cell, .ah-timeline-far-cell").index();
        $el.trigger("ah:toggle", [on, Math.floor(cell / 3)]);
      });
      $el.on("keydown" + NS, ".ah-timeline-item[ah-collapsible]", keyClick);
    }
  });

  // ------------------------------------------------------------------
  // ranking-list: clickable rows
  // ------------------------------------------------------------------

  AH.define("ranking-list", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-ranking-list__item--clickable", function () {
        $el.trigger("ah:item-click", [{ index: num(this.getAttribute("data-idx"), 0) }]);
      });
      $el.on("keydown" + NS, ".ah-ranking-list__item--clickable", keyClick);
    }
  });

  // ------------------------------------------------------------------
  // tag-cloud: ah:tag-click; links without a url behave as buttons
  // ------------------------------------------------------------------

  function tagItem($el, index) {
    return $el.find(".ah-tagcloud-item[data-index='" + parseInt(index, 10) + "']");
  }

  AH.define("tag-cloud", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-tagcloud-link", function (e) {
        var a = this;
        var ev = $.Event("ah:tag-click");
        $el.trigger(ev, [{
          label: a.getAttribute("data-ah-label"),
          value: num(a.getAttribute("data-ah-weight"), 0),
          url: a.getAttribute("href"),
          index: num($(a).closest(".ah-tagcloud-item").attr("data-index"), 0)
        }]);
        if (ev.isDefaultPrevented() || !a.hasAttribute("href")) { e.preventDefault(); }
      });
      $el.on("keydown" + NS, ".ah-tagcloud-link:not([href])", keyClick);
    },
    methods: {
      hideItem: function (el, $el, i) { tagItem($el, i).hide(); },
      showItem: function (el, $el, i) { tagItem($el, i).show(); }
    }
  });

  // ------------------------------------------------------------------
  // alert: dismiss
  // ------------------------------------------------------------------

  function dismiss(el, $el) {
    var ev = $.Event("ah:dismiss");
    $el.trigger(ev);
    if (!ev.isDefaultPrevented()) { drop(el); }
  }

  AH.define("alert", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-alert-close", function () { dismiss(el, $el); });
    },
    methods: {
      dismiss: dismiss
    }
  });
})(window.jQuery, window.AH);

/* ---- components/form_buttons.js ---- */
/* Behaviours of the form_buttons components (designs/04-components.md).
 *
 *   toggle-button      click toggles aria-pressed / data-ah-value, fires change
 *   button-group       radio / checkbox selection (arrow keys in radio mode),
 *                      a short pressed flash in the default mode
 *   segmented-control  single selection, arrow keys move and select
 *   dropdown-button    menu popup: outside click / Escape close, arrow keys
 *   split-button       main action + menu; arrow and menu clicks do not
 *                      reach the root, so on(click) there is the main action
 *
 * Value-bearing roots keep data-ah-value and the hidden input
 * (input[data-ah-input]) in step and fire "change" on the root.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  // Set the value of a value-bearing root; fire change when asked.
  function setValue($el, v, fire) {
    v = v == null ? "" : String(v);
    var old = $el.attr("data-ah-value");
    $el.attr("data-ah-value", v);
    $el.children("input[data-ah-input]").val(v);
    if (fire && old !== v) {
      $el.trigger("change", [v]);
    }
  }

  // Namespace for the document-level handlers of one instance.
  function docNS(el) {
    var ns = $.data(el, "ah-docns");
    if (!ns) {
      ns = NS + "m" + (++seq);
      $.data(el, "ah-docns", ns);
    }
    return ns;
  }

  // Move focus among enabled elements: key -> new index, or -1.
  function step(key, idx, len) {
    switch (key) {
      case "ArrowRight": case "ArrowDown": return (idx + 1) % len;
      case "ArrowLeft": case "ArrowUp": return (idx - 1 + len) % len;
      case "Home": return 0;
      case "End": return len - 1;
      default: return -1;
    }
  }

  // ------------------------------------------------------------------
  // toggle-button
  // ------------------------------------------------------------------

  function setPressed(el, $el, on, fire) {
    var v = on ? "true" : "false";
    $el.toggleClass("ah-btn-toggled", on).attr("aria-pressed", v).val(v);
    setValue($el, v, fire);
  }

  AH.define("toggle-button", {
    init: function (el, $el) {
      $el.on("click" + NS, function () {
        if (el.disabled) { return; }
        setPressed(el, $el, $el.attr("aria-pressed") !== "true", true);
      });
    },
    methods: {
      toggle: function (el, $el) { setPressed(el, $el, $el.attr("aria-pressed") !== "true", false); },
      setValue: function (el, $el, v) { setPressed(el, $el, v === true || v === "true", false); },
      getValue: function (el, $el) { return $el.attr("aria-pressed") === "true"; }
    }
  });

  // ------------------------------------------------------------------
  // button-group
  // ------------------------------------------------------------------

  function groupMode($el) {
    return $el.hasClass("ah-btn-group-radio") ? "radio"
      : $el.hasClass("ah-btn-group-checkbox") ? "checkbox" : "default";
  }

  function groupButtons($el) {
    return $el.children(".ah-btn-group-btn");
  }

  function groupSelect($el, $btn, on) {
    $btn.toggleClass("ah-btn-group-btn-selected", on);
    if (groupMode($el) === "radio") {
      $btn.attr({ "aria-checked": String(on), tabindex: on ? "0" : "-1" });
    } else {
      $btn.attr("aria-pressed", String(on));
    }
  }

  function groupSync($el, fire) {
    var vals = groupButtons($el).filter(".ah-btn-group-btn-selected").map(function () {
      return this.getAttribute("data-value");
    }).get();
    setValue($el, vals.join(","), fire);
  }

  function groupSet($el, values) {
    var set = {};
    $.each(values, function (_, v) { set[String(v)] = true; });
    groupButtons($el).each(function () {
      groupSelect($el, $(this), !!set[this.getAttribute("data-value")]);
    });
    if (groupMode($el) === "radio" && !groupButtons($el).filter("[tabindex=0]").length) {
      groupButtons($el).not(":disabled").first().attr("tabindex", "0");
    }
    groupSync($el, false);
  }

  function groupClick($el, $btn) {
    switch (groupMode($el)) {
      case "radio":
        groupButtons($el).each(function () { groupSelect($el, $(this), this === $btn[0]); });
        groupSync($el, true);
        break;
      case "checkbox":
        groupSelect($el, $btn, !$btn.hasClass("ah-btn-group-btn-selected"));
        groupSync($el, true);
        break;
      default:
        $btn.addClass("ah-btn-group-btn-pressed");
        setTimeout(function () { $btn.removeClass("ah-btn-group-btn-pressed"); }, 150);
    }
  }

  AH.define("button-group", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-btn-group-btn", function () {
        if (this.disabled || $el.hasClass("ah-btn-group-disabled")) { return; }
        groupClick($el, $(this));
      });
      $el.on("mouseenter" + NS, ".ah-btn-group-btn", function () {
        if (!this.disabled) { $(this).addClass("ah-btn-group-btn-hover"); }
      });
      $el.on("mouseleave" + NS, ".ah-btn-group-btn", function () {
        $(this).removeClass("ah-btn-group-btn-hover");
      });
      // Radio mode is a radiogroup: arrows move focus and select.
      $el.on("keydown" + NS, ".ah-btn-group-btn", function (e) {
        if (groupMode($el) !== "radio") { return; }
        var $btns = groupButtons($el).not(":disabled");
        var i = step(e.key, $btns.index(this), $btns.length);
        if (i < 0) { return; }
        e.preventDefault();
        var $to = $btns.eq(i);
        $to.trigger("focus");
        groupClick($el, $to);
      });
    },
    methods: {
      setValue: function (el, $el, v) {
        groupSet($el, Array.isArray(v) ? v : String(v == null ? "" : v).split(",").filter(Boolean));
      },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); },
      clear: function (el, $el) { groupSet($el, []); }
    }
  });

  // ------------------------------------------------------------------
  // segmented-control
  // ------------------------------------------------------------------

  function segItems($el) {
    return $el.children(".ah-segmented-control__item");
  }

  function segSet($el, v, fire) {
    v = String(v == null ? "" : v);
    var any = false;
    segItems($el).each(function () {
      var on = this.getAttribute("data-value") === v;
      any = any || on;
      $(this).attr({ "data-state": on ? "active" : "inactive", "aria-selected": String(on),
                     tabindex: on ? "0" : "-1" });
    });
    if (!any) {
      segItems($el).not(":disabled").first().attr("tabindex", "0");
    }
    setValue($el, v, fire);
  }

  AH.define("segmented-control", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-segmented-control__item", function () {
        if (this.disabled || $el.attr("data-disabled") === "true"
            || this.getAttribute("data-disabled") === "true") { return; }
        segSet($el, this.getAttribute("data-value"), true);
      });
      $el.on("keydown" + NS, ".ah-segmented-control__item", function (e) {
        var $items = segItems($el).not(":disabled");
        var i = step(e.key, $items.index(this), $items.length);
        if (i < 0) { return; }
        e.preventDefault();
        var $to = $items.eq(i);
        $to.trigger("focus");
        segSet($el, $to.attr("data-value"), true);
      });
    },
    methods: {
      setValue: function (el, $el, v) { segSet($el, v, false); },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); }
    }
  });

  // ------------------------------------------------------------------
  // Menus shared by dropdown-button and split-button
  // ------------------------------------------------------------------
  //
  // cfg: {trigger, menu, item, isOpen($el), show($el, on), select($el, $item)}

  function menuItems($el, cfg) {
    return $el.find(cfg.menu).first().find(cfg.item).filter(function () {
      return !this.disabled && this.getAttribute("data-disabled") !== "true";
    });
  }

  function menuOpen($el, cfg, focus) {
    if (cfg.disabled($el)) { return; }
    if (!cfg.isOpen($el)) {
      cfg.show($el, true);
      $el.find(cfg.trigger).attr("aria-expanded", "true");
      $el.trigger("ah:open");
    }
    if (focus) {
      var $items = menuItems($el, cfg);
      (focus === "last" ? $items.last() : $items.first()).trigger("focus");
    }
  }

  function menuClose($el, cfg, refocus) {
    if (!cfg.isOpen($el)) { return; }
    cfg.show($el, false);
    $el.find(cfg.trigger).attr("aria-expanded", "false");
    $el.trigger("ah:close");
    if (refocus) { $el.find(cfg.trigger).trigger("focus"); }
  }

  function menuChoose($el, cfg, $item) {
    if ($item[0].disabled || $item.attr("data-disabled") === "true") { return; }
    cfg.select($el, $item);
    menuClose($el, cfg, true);
    // A menu is a command: choosing the same item again fires again.
    var v = $item.attr("data-value");
    setValue($el, v, false);
    $el.trigger("change", [v]);
  }

  function menuInit(el, $el, cfg) {
    var ns = docNS(el);
    $el.on("click" + NS, cfg.trigger, function () {
      if (cfg.isOpen($el)) { menuClose($el, cfg, false); } else { menuOpen($el, cfg, false); }
    });
    $el.on("click" + NS, cfg.item, function () {
      menuChoose($el, cfg, $(this));
    });
    $el.on("keydown" + NS, function (e) {
      var inMenu = $(e.target).closest(cfg.menu).length > 0;
      var onTrigger = $(e.target).closest(cfg.trigger).length > 0;
      if (e.key === "Escape") {
        if (cfg.isOpen($el)) { e.preventDefault(); menuClose($el, cfg, true); }
      } else if (e.key === "Tab") {
        menuClose($el, cfg, false);
      } else if (onTrigger && (e.key === "ArrowDown" || e.key === "ArrowUp")) {
        e.preventDefault();
        if (e.altKey && e.key === "ArrowUp") { menuClose($el, cfg, false); return; }
        menuOpen($el, cfg, e.altKey ? false : (e.key === "ArrowUp" ? "last" : "first"));
      } else if (inMenu) {
        var $items = menuItems($el, cfg);
        var i = step(e.key, $items.index(e.target), $items.length);
        if (i >= 0 && e.key !== "ArrowLeft" && e.key !== "ArrowRight") {
          e.preventDefault();
          $items.eq(i).trigger("focus");
        }
      }
    });
    $(document).on("mousedown" + ns, function (e) {
      if (cfg.isOpen($el) && !el.contains(e.target)) { menuClose($el, cfg, false); }
    });
  }

  function menuDestroy(el) {
    $(document).off($.data(el, "ah-docns"));
    unfloat(el, 0);
  }

  // Popups are pinned with AH.float (position: fixed), so an ancestor with
  // overflow: hidden cannot clip them. The handle lives on the root.
  function floatMenu(el, menu, opts) {
    clearTimeout($.data(el, "ah-unfloat"));
    var h = $.data(el, "ah-float");
    if (h) { h.update(); return; }
    $.data(el, "ah-float", AH.float(menu, el, opts));
  }

  // delay: let a closing fade finish before the menu drops back in place
  function unfloat(el, delay) {
    clearTimeout($.data(el, "ah-unfloat"));
    var stop = function () {
      var h = $.data(el, "ah-float");
      if (h) { h.stop(); $.removeData(el, "ah-float"); }
    };
    if (delay) { $.data(el, "ah-unfloat", setTimeout(stop, delay)); } else { stop(); }
  }

  // ------------------------------------------------------------------
  // dropdown-button
  // ------------------------------------------------------------------

  var DROPDOWN = {
    trigger: ".ah-dropdown-btn-wrapper",
    menu: ".ah-dropdown-btn-popup",
    item: ".ah-dropdown-btn-item",
    disabled: function ($el) { return $el.hasClass("ah-dropdown-btn-disabled"); },
    isOpen: function ($el) { return $el.hasClass("ah-dropdown-btn-opened"); },
    show: function ($el, on) {
      var $popup = $el.children(".ah-dropdown-btn-popup");
      $el.toggleClass("ah-dropdown-btn-opened", on);
      if (on) {
        $popup.removeAttr("hidden");
        floatMenu($el[0], $popup[0], { placement: "bottom", align: "start", matchWidth: true });
      } else {
        $popup.attr("hidden", "");
        unfloat($el[0], 0);
      }
    },
    select: function ($el, $item) {
      $el.find(".ah-dropdown-btn-item").removeClass("selected");
      $item.addClass("selected");
    }
  };

  AH.define("dropdown-button", {
    init: function (el, $el) {
      menuInit(el, $el, DROPDOWN);
      var $trigger = $el.children(".ah-dropdown-btn-wrapper");
      $el.on("mouseenter" + NS, function () {
        if (DROPDOWN.disabled($el)) { return; }
        $el.addClass("ah-dropdown-btn-hover");
        if ($el.hasClass("ah-dropdown-btn-auto-open")) { menuOpen($el, DROPDOWN, false); }
      });
      $el.on("mouseleave" + NS, function () {
        $el.removeClass("ah-dropdown-btn-hover");
        if ($el.hasClass("ah-dropdown-btn-auto-open")) { menuClose($el, DROPDOWN, false); }
      });
      $trigger.on("focus" + NS, function () {
        // sigil shows the focus ring on focus; keep it to keyboard focus
        var visible = true;
        try { visible = $trigger[0].matches(":focus-visible"); } catch (err) { /* old browser */ }
        if (visible) { $el.addClass("ah-dropdown-btn-focused"); }
      });
      $trigger.on("blur" + NS, function () { $el.removeClass("ah-dropdown-btn-focused"); });
    },
    destroy: function (el, $el) {
      menuDestroy(el);
      $el.children(".ah-dropdown-btn-wrapper").off(NS);
    },
    methods: {
      open: function (el, $el) { menuOpen($el, DROPDOWN, false); },
      close: function (el, $el) { menuClose($el, DROPDOWN, false); },
      toggle: function (el, $el) {
        if (DROPDOWN.isOpen($el)) { menuClose($el, DROPDOWN, false); } else { menuOpen($el, DROPDOWN, false); }
      },
      setValue: function (el, $el, v) {
        var $item = $el.find(".ah-dropdown-btn-item").filter(function () {
          return this.getAttribute("data-value") === String(v);
        });
        $el.find(".ah-dropdown-btn-item").removeClass("selected");
        $item.addClass("selected");
        setValue($el, v, false);
      },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); }
    }
  });

  // ------------------------------------------------------------------
  // split-button
  // ------------------------------------------------------------------

  var SPLIT = {
    trigger: ".ah-split-button__arrow",
    menu: ".ah-split-button__menu",
    item: ".ah-split-button__item",
    disabled: function ($el) { return $el.attr("data-disabled") === "true"; },
    isOpen: function ($el) { return $el.attr("data-open") === "true"; },
    show: function ($el, on) {
      $el.attr("data-open", on ? "true" : "false");
      if (on) {
        floatMenu($el[0], $el.children(".ah-split-button__menu")[0],
                  { placement: "bottom",
                    align: $el.attr("data-menu-align") === "start" ? "start" : "end" });
      } else {
        unfloat($el[0], 150);
      }
    },
    select: function () {}
  };

  AH.define("split-button", {
    init: function (el, $el) {
      menuInit(el, $el, SPLIT);
      // Only the main half's clicks reach the root (and its on(click)).
      $el.on("click" + NS, ".ah-split-button__arrow, .ah-split-button__menu", function (e) {
        e.stopPropagation();
      });
    },
    destroy: function (el) { menuDestroy(el); },
    methods: {
      open: function (el, $el) { menuOpen($el, SPLIT, false); },
      close: function (el, $el) { menuClose($el, SPLIT, false); },
      setValue: function (el, $el, v) { setValue($el, v, false); },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/form_calendar.js ---- */
/* Behaviours of the form_calendar components (designs/04-components.md).
 * Ported from sigil: form/calendar (+ daygrid, timegrid, list, shared,
 * recurrence, util) and form/datetime_input (+ format, editor, dropdown).
 *
 * Dates are day numbers (days since 1970-01-01) and times minutes since
 * day 0, computed with UTC arithmetic: event times are local wall times
 * without a zone, so there is no DST or zone shifting. The view builders
 * (calMonth, calTimegrid, calList) are the twins of month_view/4,
 * timegrid_view/4 and list_view/4 in aihtml_form_calendar.erl: both feed
 * the same templates (calendar_month, calendar_timegrid, calendar_list). */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var DAY = 1440;
  var MAX_ITERS = 5000;
  var seq = 0;

  function ensureId(el, prefix) {
    if (!el.id) { el.id = prefix + (++seq); }
    return el.id;
  }

  function outside(el, e) {
    return e.target.isConnected !== false && !$.contains(el, e.target) && e.target !== el;
  }

  // ------------------------------------------------------------------
  // Day numbers
  // ------------------------------------------------------------------

  function dnum(y, m, d) { return Math.round(Date.UTC(y, m - 1, d) / 864e5); }   // m 1..12
  function ymd(n) {
    var d = new Date(n * 864e5);
    return [d.getUTCFullYear(), d.getUTCMonth() + 1, d.getUTCDate()];
  }
  function dow(n) { return ((n + 4) % 7 + 7) % 7; }                               // 0 = Sunday
  function sow(n, first) { return n - (dow(n) - first + 7) % 7; }
  function lastDay(y, m) { return new Date(Date.UTC(y, m, 0)).getUTCDate(); }
  // date-fns addMonths: the day clamped to the target month's length
  function addMonths(n, k) {
    var p = ymd(n), t = p[0] * 12 + (p[1] - 1) + k;
    var y = Math.floor(t / 12), m = t - y * 12 + 1;
    return dnum(y, m, Math.min(p[2], lastDay(y, m)));
  }
  function pad(n) { return (n < 10 ? "0" : "") + n; }
  function pad4(n) { return ("000" + n).slice(-4); }
  function isoDate(n) { var p = ymd(n); return pad4(p[0]) + "-" + pad(p[1]) + "-" + pad(p[2]); }
  function todayNum() { var t = new Date(); return dnum(t.getFullYear(), t.getMonth() + 1, t.getDate()); }
  function validYmd(y, m, d) { return m >= 1 && m <= 12 && d >= 1 && d <= lastDay(y, m); }
  function parseDate(s) {
    var m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(s || "");
    return m && validYmd(+m[1], +m[2], +m[3]) ? dnum(+m[1], +m[2], +m[3]) : null;
  }
  // ISO date or date-time -> { t: minutes, dateOnly }
  function parseTime(s) {
    var m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2}))?/.exec(String(s || ""));
    if (!m || !validYmd(+m[1], +m[2], +m[3])) { return null; }
    var t = dnum(+m[1], +m[2], +m[3]) * DAY;
    if (m[4] === undefined) { return { t: t, dateOnly: true }; }
    if (+m[4] > 23 || +m[5] > 59) { return null; }
    return { t: t + (+m[4]) * 60 + (+m[5]), dateOnly: false };
  }
  function isoTime(t, allDay) {
    var d = Math.floor(t / DAY), r = t - d * DAY;
    if (allDay && r === 0) { return isoDate(d); }
    return isoDate(d) + "T" + pad(Math.floor(r / 60)) + ":" + pad(r % 60);
  }

  // Display formats: yyyy yy MMMM MMM MM M dd d EEEE EEE (fmt/3 in Erlang).
  function fmtDate(n, f, L) {
    var p = ymd(n);
    return f.replace(/yyyy|yy|MMMM|MMM|MM|M|dd|d|EEEE|EEE/g, function (t) {
      switch (t) {
        case "yyyy": return String(p[0]);
        case "yy": return pad(p[0] % 100);
        case "MMMM": return L.months[p[1] - 1];
        case "MMM": return L.months_short[p[1] - 1];
        case "MM": return pad(p[1]);
        case "M": return String(p[1]);
        case "dd": return pad(p[2]);
        case "d": return String(p[2]);
        case "EEEE": return L.weekdays[dow(n)];
        default: return L.weekdays_short[dow(n)];
      }
    });
  }

  function readJson(el, attr, dflt) {
    try { return JSON.parse(el.getAttribute(attr) || "null") || dflt; } catch (err) { return dflt; }
  }

  // ==================================================================
  // calendar
  // ==================================================================

  var CAL_LABELS = {
    today: "Today", prev: "Previous", next: "Next",
    month: "Month", week: "Week", day: "Day", list: "Agenda",
    all_day: "All day", all_day_short: "all-day", more: "+{n} more",
    no_events: "No events in this period",
    no_events_hint: "Try navigating to a different date range",
    am: "AM", pm: "PM",
    months: ["January", "February", "March", "April", "May", "June", "July",
             "August", "September", "October", "November", "December"],
    months_short: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    weekdays: ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"],
    weekdays_short: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
    title_month: "MMMM yyyy", title_day: "EEEE, MMMM d, yyyy",
    range_start: "MMM d", range_end: "MMM d, yyyy", list_date: "MMMM d, yyyy"
  };
  var DEFAULT_COLOR = "var(--ah-color-primary)";
  var COLOR_RE = /^[#a-zA-Z0-9(),.%\s-]+$/;
  var STATUS_COLORS = { confirmed: "var(--ah-color-success)", tentative: "var(--ah-color-warning)",
                        cancelled: "var(--ah-color-error)" };

  // An event as the server sends it (normalize_event/2), from any event
  // object a page passes: start required, end and allDay defaulted.
  function calNormalize(e, n) {
    var s = parseTime(e.start);
    if (!s) { throw new Error("calendar: bad event start " + e.start); }
    var allDayFlag = e.allDay === true || e.all_day === true || s.dateOnly;
    var end = e.end !== undefined && e.end !== null ? parseTime(e.end) : null;
    var et = end ? end.t : s.t + (allDayFlag ? DAY : 60);
    var allDay = allDayFlag || (s.t % DAY === 0 && et % DAY === 0 && s.t !== et);
    var out = {
      id: e.id !== undefined && e.id !== null ? String(e.id) : "ev" + n,
      title: e.title === undefined || e.title === null ? "" : String(e.title),
      start: isoTime(s.t, allDay), end: isoTime(et, allDay), allDay: allDay
    };
    if (e.color && COLOR_RE.test(e.color)) { out.color = String(e.color); }
    if (e.rrule) { out.rrule = String(e.rrule); }
    if (e.exdates) { out.exdates = e.exdates.slice(); }
    if (e.status) { out.status = String(e.status); }
    return out;
  }

  // ---- recurrence (sigil's calendar/recurrence.cljs) ----

  var RDAYS = ["SU", "MO", "TU", "WE", "TH", "FR", "SA"];

  function parseRRule(str) {
    var r = {};
    String(str).replace(/^RRULE:/, "").split(";").forEach(function (part) {
      if (!part) { return; }
      var i = part.indexOf("=");
      var k = part.slice(0, i).toUpperCase(), v = part.slice(i + 1);
      switch (k) {
        case "FREQ": r.freq = v.toLowerCase(); break;
        case "INTERVAL": r.interval = parseInt(v, 10); break;
        case "COUNT": r.count = parseInt(v, 10); break;
        case "UNTIL": {
          var m = /^(\d{4})(\d{2})(\d{2})(?:T(\d{2})(\d{2}))?/.exec(v);
          if (m) { r.until = dnum(+m[1], +m[2], +m[3]) * DAY + (m[4] ? +m[4] * 60 + (+m[5]) : 0); }
          break;
        }
        case "BYDAY": r.byday = v.split(",").map(function (d) { return RDAYS.indexOf(d.toUpperCase()); }); break;
        case "BYMONTHDAY": r.bymonthday = v.split(",").map(function (d) { return parseInt(d, 10); }); break;
        case "BYMONTH": r.bymonth = v.split(",").map(function (d) { return parseInt(d, 10); }); break;
        default: break;
      }
    });
    return r;
  }

  function advance(c, freq, i) {
    var d = Math.floor(c / DAY), r = c - d * DAY;
    switch (freq) {
      case "weekly": return c + 7 * i * DAY;
      case "monthly": return addMonths(d, i) * DAY + r;
      case "yearly": return addMonths(d, 12 * i) * DAY + r;
      default: return c + i * DAY;
    }
  }

  function matches(c, rule) {
    var d = Math.floor(c / DAY), p = ymd(d);
    return (!rule.byday || rule.byday.indexOf(dow(d)) >= 0) &&
      (!rule.bymonthday || rule.bymonthday.indexOf(p[2]) >= 0) &&
      (!rule.bymonth || rule.bymonth.indexOf(p[1]) >= 0);
  }

  // The occurrences [start, end] of a series overlapping [rs, re), minutes.
  function expand(s, e, rule, rs, re, ex) {
    if (!rule.freq) { return []; }
    var dur = e - s, I = rule.interval || 1, out = [], iter = 0, count = 0;
    var countOk = function () { return rule.count === undefined || count < rule.count; };
    var untilOk = function (t) { return rule.until === undefined || t <= rule.until; };
    var add = function (c) {
      if (ex.indexOf(isoDate(Math.floor(c / DAY))) < 0 && c < re && c + dur > rs) { out.push([c, c + dur]); }
    };
    if (rule.freq === "weekly" && rule.byday && rule.byday.length) {
      var byday = rule.byday.slice().sort(function (a, b) { return a - b; });
      var offset = s % DAY;
      for (var w = sow(Math.floor(s / DAY), 1);
           iter < MAX_ITERS && w * DAY < re && untilOk(w * DAY) && countOk(); w += 7 * I) {
        var wd = dow(w);
        var cands = byday.map(function (d) { return (w + (d - wd + 7) % 7) * DAY + offset; })
          .sort(function (a, b) { return a - b; });
        for (var k = 0; k < cands.length; k++) {
          var c = cands[k];
          if (countOk() && iter < MAX_ITERS && c >= s && untilOk(c) && c < re) {
            iter++; count++; add(c);
          }
        }
      }
    } else {
      for (var t = s; iter < MAX_ITERS && t < re && untilOk(t) && countOk(); t = advance(t, rule.freq, I)) {
        iter++;
        if (matches(t, rule)) { count++; add(t); }
      }
    }
    return out;
  }

  function stamp(t) {
    var d = Math.floor(t / DAY), p = ymd(d), r = t - d * DAY;
    return pad4(p[0]) + pad(p[1]) + pad(p[2]) + "T" + pad(Math.floor(r / 60)) + pad(r % 60) + "00";
  }

  // Instances overlapping [rs, re), in event order.
  function instances(events, rs, re) {
    var out = [];
    events.forEach(function (ev) {
      var s = parseTime(ev.start).t, e = parseTime(ev.end).t;
      if (!ev.rrule) {
        if (s < re && e > rs) { out.push({ id: ev.id, src: ev, s: s, e: e, allDay: ev.allDay }); }
        return;
      }
      expand(s, e, parseRRule(ev.rrule), rs, re, ev.exdates || []).forEach(function (p) {
        out.push({ id: ev.id + "_" + stamp(p[0]), src: ev, s: p[0], e: p[1], allDay: ev.allDay });
      });
    });
    return out;
  }

  function inRange(insts, from, to) {
    return insts.filter(function (i) { return i.s < to && i.e > from; });
  }

  // ---- views ----

  function cls(base, opts) {
    return base + opts.filter(function (o) { return o[0]; }).map(function (o) { return " " + o[1]; }).join("");
  }

  function h12(h) { return h === 0 ? 12 : (h > 12 ? h - 12 : h); }
  function fmtClock(t, st) {
    var r = ((t % DAY) + DAY) % DAY, h = Math.floor(r / 60), m = pad(r % 60);
    return st.hour24 ? pad(h) + ":" + m : h12(h) + ":" + m + " " + (h < 12 ? st.L.am : st.L.pm);
  }
  function srcColor(src) { return src.color || DEFAULT_COLOR; }

  // [rs, re) in days and the title
  function calProfile(st) {
    var L = st.L, cur = st.cur, p;
    switch (st.view) {
      case "week":
        p = sow(cur, st.first);
        return [p, p + 7, fmtDate(p, L.range_start, L) + " – " + fmtDate(p + 6, L.range_end, L)];
      case "day":
        return [cur, cur + 1, fmtDate(cur, L.title_day, L)];
      case "list":
        return [cur, cur + st.agendaDays,
                fmtDate(cur, L.range_start, L) + " – " + fmtDate(cur + st.agendaDays - 1, L.range_end, L)];
      default: {
        var y = ymd(cur);
        return [sow(dnum(y[0], y[1], 1), st.first), sow(dnum(y[0], y[1], lastDay(y[0], y[1])), st.first) + 7,
                fmtDate(cur, L.title_month, L)];
      }
    }
  }

  // sigil's compute-week-segments (week_segments/2 in Erlang)
  function weekSegments(w, insts) {
    var segs = inRange(insts, w * DAY, (w + 7) * DAY).map(function (i, n) {
      var vs = Math.floor(i.s / DAY);
      var ve = i.allDay ? Math.max(vs + 1, Math.floor((i.e + DAY - 1) / DAY)) : vs + 1;
      var cs = Math.max(vs, w), ce = Math.min(ve, w + 7);
      return { inst: i, sc: cs - w + 1, ec: ce - w + 1, span: ce - cs, multi: ve - vs > 1,
               cont: vs < w, conts: ve > w + 7, n: n };
    }).filter(function (g) { return g.span > 0; });
    segs.sort(function (a, b) {
      return ((a.multi ? 0 : 1) - (b.multi ? 0 : 1)) || (b.span - a.span) ||
        (a.inst.s % DAY - b.inst.s % DAY) || (a.n - b.n);
    });
    var rows = [];
    segs.forEach(function (g) {
      var r = 0;
      for (; r < rows.length; r++) {
        if (!rows[r].some(function (x) { return g.sc < x[1] && g.ec > x[0]; })) { break; }
      }
      if (r === rows.length) { rows.push([]); }
      rows[r].push([g.sc, g.ec]);
      g.row = r;
    });
    return segs;
  }

  function calMonth(st, rs, re, insts) {
    var L = st.L, today = todayNum(), curM = ymd(st.cur)[1], max = st.maxEvents;
    var headers = [];
    for (var i = 0; i < 7; i++) { headers.push({ label: L.weekdays_short[(st.first + i) % 7] }); }
    var weeks = [];
    for (var w = rs; w < re; w += 7) {
      var segs = weekSegments(w, insts), over = {};
      segs.forEach(function (g) {
        if (g.row >= max) { for (var c = g.sc; c < g.ec; c++) { over[c] = (over[c] || 0) + 1; } }
      });
      var days = [];
      for (var k = 0; k < 7; k++) {
        var d = w + k, p = ymd(d), other = p[1] !== curM;
        days.push({
          bg_cls: cls("ah-calendar-day", [[d === today, "ah-calendar-day-today"], [other, "ah-calendar-day-other"]]),
          num_cls: cls("ah-calendar-day-num", [[d === today, "ah-calendar-day-num-today"],
                                               [other, "ah-calendar-day-num-other"]]),
          date: isoDate(d), col: String(k + 1), num: String(p[2])
        });
      }
      weeks.push({
        days: days,
        events: segs.filter(function (g) { return g.row < max; }).map(function (g) {
          return {
            cls: cls("ah-calendar-event ah-calendar-daygrid-event",
                     [[g.multi, "ah-calendar-daygrid-event-multi"], [g.cont, "ah-calendar-daygrid-event-start"],
                      [g.conts, "ah-calendar-daygrid-event-end"]]),
            id: g.inst.id, sc: String(g.sc), ec: String(g.ec), row: String(g.row + 2),
            color: srcColor(g.inst.src), title: g.inst.src.title,
            has_time: !g.inst.allDay && !g.multi, time: fmtClock(g.inst.s, st)
          };
        }),
        more: Object.keys(over).map(Number).sort(function (a, b) { return a - b; })
          .filter(function (c) { return over[c] > 0; }).map(function (c) {
            return { date: isoDate(w + c - 1), col: String(c), row: String(max + 2),
                     label: L.more.split("{n}").join(String(over[c])) };
          })
      });
    }
    return { headers: headers, weeks: weeks };
  }

  // sigil's assign-columns (columns/2 in Erlang)
  function columns(d, insts) {
    var items = insts.map(function (i, n) {
      var ts = Math.max(i.s - d * DAY, 0), te = Math.min(i.e - d * DAY, DAY);
      return { inst: i, ts: ts, te: te <= ts ? ts + 30 : te, n: n };
    });
    items.sort(function (a, b) { return (a.ts - b.ts) || (a.n - b.n); });
    var cols = [];
    items.forEach(function (it) {
      var k = 0;
      for (; k < cols.length; k++) {
        if (!cols[k].some(function (o) { return it.ts < o[1] && it.te > o[0]; })) { break; }
      }
      if (k === cols.length) { cols.push([]); }
      cols[k].push([it.ts, it.te]);
      it.col = k;
    });
    items.forEach(function (it) { it.cols = cols.length; });
    return items;
  }

  function calTimegrid(st, rs, re, insts) {
    var L = st.L, today = todayNum(), dur = st.slotDur, sh = st.slotH;
    var total = Math.round(DAY * sh / dur);
    var slots = [];
    for (var h = 0; h < 24; h++) {
      slots.push({ slot_height: String(Math.round(60 * sh / dur)),
                   label: st.hour24 ? pad(h) + ":00" : h12(h) + " " + (h < 12 ? L.am : L.pm) });
    }
    var days = [];
    for (var d = rs; d < re; d++) {
      var dayI = inRange(insts, d * DAY, (d + 1) * DAY);
      days.push({
        date: isoDate(d), dow: L.weekdays_short[dow(d)], num: String(ymd(d)[2]),
        head_cls: cls("ah-calendar-timegrid-header-cell", [[d === today, "ah-calendar-timegrid-header-today"]]),
        col_cls: cls("ah-calendar-timegrid-day-col", [[d === today, "ah-calendar-timegrid-day-today"]]),
        col_height: String(total),
        allday: dayI.filter(function (i) { return i.allDay; }).map(function (i) {
          return { id: i.id, color: srcColor(i.src), title: i.src.title };
        }),
        timed: columns(d, dayI.filter(function (i) { return !i.allDay; })).map(function (it) {
          return {
            id: it.inst.id, color: srcColor(it.inst.src), title: it.inst.src.title,
            top: String(Math.round(it.ts * sh / dur)), height: String(Math.round((it.te - it.ts) * sh / dur)),
            left: "calc(100% * " + it.col + " / " + it.cols + ")", width: "calc(100% / " + it.cols + ")",
            time: fmtClock(it.inst.s, st) + " – " + fmtClock(it.inst.e, st),
            resizable: st.editable
          };
        })
      });
    }
    return { all_day: L.all_day_short, slots: slots, days: days };
  }

  function calList(st, rs, re, insts) {
    var L = st.L, groups = [];
    for (var d = rs; d < re; d++) {
      var evs = inRange(insts, d * DAY, (d + 1) * DAY).map(function (i, n) { return [i, n]; });
      evs.sort(function (a, b) { return (a[0].s - b[0].s) || (a[1] - b[1]); });
      if (!evs.length) { continue; }
      groups.push({
        name: L.weekdays[dow(d)], date: fmtDate(d, L.list_date, L),
        events: evs.map(function (p) {
          var i = p[0], src = i.src;
          return {
            id: i.id, color: srcColor(src), title: src.title,
            time: i.allDay ? L.all_day : fmtClock(i.s, st) + " – " + fmtClock(i.e, st),
            recurring: !!src.rrule, has_status: src.status !== undefined,
            status_color: STATUS_COLORS[src.status] || "var(--ah-color-grey-300)"
          };
        })
      });
    }
    return { empty: !groups.length, no_events: L.no_events, no_events_hint: L.no_events_hint,
             groups: groups };
  }

  // The view HTML of the current state (calView), as the server renders it.
  function calView(st) {
    var p = calProfile(st);
    var insts = instances(st.events, p[0] * DAY, p[1] * DAY);
    var html = st.view === "month" ? AH.tpl.calendar_month(calMonth(st, p[0], p[1], insts))
      : st.view === "list" ? AH.tpl.calendar_list(calList(st, p[0], p[1], insts))
      : AH.tpl.calendar_timegrid(calTimegrid(st, p[0], p[1], insts));
    return { rs: p[0], re: p[1], title: p[2], insts: insts, html: html };
  }

  // ---- behaviour ----

  function calState(el) { return $.data(el, "ah-cal"); }

  function calRender(el, $el) {
    var st = calState(el);
    var $scroll = st.$container.find(".ah-calendar-timegrid-scroll");
    var scroll = $scroll.length && st.renderedView === st.view ? $scroll[0].scrollTop : null;
    var v = calView(st);
    st.insts = {};
    v.insts.forEach(function (i) { st.insts[i.id] = i; });
    st.range = [v.rs, v.re];
    st.$container.html(v.html);
    st.$title.text(v.title);
    $el.find(".ah-calendar-view-btn").each(function () {
      var on = this.getAttribute("data-view") === st.view;
      $(this).toggleClass("ah-calendar-view-btn-active", on).attr("aria-pressed", String(on));
    });
    var iso = isoDate(st.cur);
    el.setAttribute("data-ah-value", iso);
    $el.children("input[type=hidden]").val(iso);
    el.setAttribute("data-view", st.view);
    el.setAttribute("data-start", isoDate(v.rs));
    el.setAttribute("data-end", isoDate(v.re));
    clearInterval(st.timer);
    st.timer = null;
    $scroll = st.$container.find(".ah-calendar-timegrid-scroll");
    if ($scroll.length) {
      // keep the scroll position while the view stays, else show the morning
      $scroll[0].scrollTop = scroll !== null ? scroll : Math.max(Math.round(7 * 60 * st.slotH / st.slotDur) - 10, 0);
      calNow(el);
      st.timer = setInterval(function () { calNow(el); }, 60000);
    }
    st.renderedView = st.view;
  }

  // The current time line, when today is in the visible range.
  function calNow(el) {
    var st = calState(el);
    var $ind = st.$container.find(".ah-calendar-timegrid-now-indicator");
    var now = new Date(), t = todayNum();
    if (t >= st.range[0] && t < st.range[1]) {
      $ind.css({ display: "block",
                 top: Math.round((now.getHours() * 60 + now.getMinutes()) * st.slotH / st.slotDur) + "px" });
    } else {
      $ind.css({ display: "none" });
    }
  }

  // Navigation re-renders and fires change when the date or view changed.
  function calGo(el, $el, cur, view) {
    var st = calState(el);
    var changed = cur !== st.cur || view !== st.view;
    st.cur = cur;
    st.view = view;
    calRender(el, $el);
    if (changed) { $el.trigger("change"); }
  }

  function calStep(el, $el, dir) {
    var st = calState(el), c = st.cur;
    switch (st.view) {
      case "month": c = addMonths(c, dir); break;
      case "week": c += 7 * dir; break;
      case "day": c += dir; break;
      default: c += st.agendaDays * dir; break;
    }
    calGo(el, $el, c, st.view);
  }

  // Details of an interaction as data-* on the root (Event.data of a
  // postback), then the component event.
  var DETAIL_ATTRS = ["data-event", "data-from", "data-to", "data-days", "data-all-day", "data-date"];
  function calFire(el, $el, name, detail) {
    DETAIL_ATTRS.forEach(function (a) { el.removeAttribute(a); });
    $.each(detail, function (k, v) {
      if (k === "raw") { return; }
      el.setAttribute("data-" + k.replace(/[A-Z]/g, function (c) { return "-" + c.toLowerCase(); }), String(v));
    });
    var ev = $.Event(name);
    $el.trigger(ev, [detail]);
    return ev;
  }

  function calSource(st, instId) {
    var inst = st.insts[instId];
    return inst ? inst.src : null;
  }

  function calEventClick(el, $el, target) {
    var st = calState(el);
    var inst = st.insts[target.getAttribute("data-eventid")];
    if (!inst) { return; }
    calFire(el, $el, "ah:event-click", { event: inst.src.id, raw: $.extend({}, inst.src,
      { start: isoTime(inst.s, inst.allDay), end: isoTime(inst.e, inst.allDay) }) });
  }

  function calMore(el, $el, target) {
    var st = calState(el);
    var date = target.getAttribute("data-date");
    var ev = calFire(el, $el, "ah:more-click", { date: date });
    if (!ev.isDefaultPrevented() && st.views.indexOf("day") >= 0) {
      calGo(el, $el, parseDate(date), "day");
    }
  }

  // Shift an event (the whole series for a recurring one) and re-render.
  function calMove(el, $el, src, dStart, dEnd, name, days) {
    var st = calState(el);
    var s = parseTime(src.start).t + dStart, e = parseTime(src.end).t + dEnd;
    if (e <= s) { return; }
    var allDay = src.allDay && s % DAY === 0 && e % DAY === 0;
    src.start = isoTime(s, allDay);
    src.end = isoTime(e, allDay);
    src.allDay = allDay;
    calRender(el, $el);
    var detail = { event: src.id, from: src.start, to: src.end, allDay: allDay };
    if (days !== undefined) { detail.days = days; }
    calFire(el, $el, name, detail);
    st.justDragged = true;
  }

  // Hit testing by rectangles (the event layer covers the day cells).
  function cellAt($cells, x, y) {
    var hit = null;
    $cells.each(function () {
      var r = this.getBoundingClientRect();
      if (x >= r.left && x <= r.right && (y === null || (y >= r.top && y <= r.bottom))) { hit = this; return false; }
    });
    return hit;
  }

  function slotMinutes(st, col, y) {
    var rel = y - col.getBoundingClientRect().top;
    var m = Math.min(Math.max(Math.round(rel / st.slotH * st.slotDur), 0), DAY);
    return Math.round(m / st.slotDur) * st.slotDur;
  }

  function ghost(evEl, e) {
    var r = evEl.getBoundingClientRect();
    var $g = $(evEl).clone().addClass("ah-calendar-event-ghost").removeAttr("tabindex role")
      .css({ position: "fixed", zIndex: 9999, opacity: 0.7, pointerEvents: "none", margin: 0,
             width: r.width + "px", height: r.height + "px", left: r.left + "px", top: r.top + "px" })
      .appendTo(document.body);
    return { $g: $g, dx: e.clientX - r.left, dy: e.clientY - r.top };
  }

  // One drag at a time: mousedown decides the mode, document mousemove
  // and mouseup (namespaced per calendar) carry it out.
  function calDragStart(el, $el, e) {
    var st = calState(el);
    if (e.which !== 1) { return; }
    var $t = $(e.target), $c = st.$container;
    var evEl = $t.closest(".ah-calendar-daygrid-event, .ah-calendar-timegrid-event, .ah-calendar-allday-event")[0];
    var d = null;
    if ($t.hasClass("ah-calendar-timegrid-resize-handle") && st.editable) {
      var rEv = $t.closest(".ah-calendar-timegrid-event")[0];
      d = { mode: "resize", ev: rEv, inst: st.insts[rEv.getAttribute("data-eventid")],
            col: $t.closest(".ah-calendar-timegrid-day-col")[0] };
    } else if (evEl && st.editable) {
      var kind = $(evEl).hasClass("ah-calendar-daygrid-event") ? "month"
        : ($(evEl).hasClass("ah-calendar-allday-event") ? "allday" : "timed");
      d = { mode: "move", kind: kind, ev: evEl, inst: st.insts[evEl.getAttribute("data-eventid")],
            x0: e.clientX, y0: e.clientY, started: false };
      if (kind === "month") {
        var c0 = cellAt($c.find(".ah-calendar-day"), e.clientX, e.clientY);
        d.origin = c0 ? parseDate(c0.getAttribute("data-date")) : Math.floor(d.inst.s / DAY);
      } else if (kind === "allday") {
        d.origin = parseDate($(evEl).closest(".ah-calendar-timegrid-allday-cell").attr("data-date"));
      }
    } else if (!evEl && st.selectable && $t.closest(".ah-calendar-daygrid-body").length &&
               !$t.closest(".ah-calendar-day-more").length) {
      var cell = cellAt($c.find(".ah-calendar-day"), e.clientX, e.clientY);
      if (cell) { d = { mode: "select", from: parseDate(cell.getAttribute("data-date")) }; d.to = d.from; }
    } else if (!evEl && st.selectable && $t.closest(".ah-calendar-timegrid-day-col").length) {
      var col = $t.closest(".ah-calendar-timegrid-day-col")[0];
      var m0 = Math.min(slotMinutes(st, col, e.clientY), DAY - st.slotDur);
      d = { mode: "create", col: col, day: parseDate(col.getAttribute("data-date")), m0: m0,
            top: m0, bot: m0 + st.slotDur,
            $ph: $('<div class="ah-calendar-timegrid-create-placeholder"></div>')
              .css({ left: 0, right: 0 }).appendTo(col) };
    }
    if (!d || !d.inst && (d.mode === "move" || d.mode === "resize")) { return; }
    e.preventDefault();
    st.drag = d;
    calDragPaint(el, e);
    $(document).on("mousemove" + st.ns, function (me) { calDragPaint(el, me); })
      .on("mouseup" + st.ns, function (ue) { calDragEnd(el, $el, ue); })
      .on("keydown" + st.ns, function (ke) {
        if (ke.key === "Escape") { calDragCancel(el); }
      });
  }

  function calDragPaint(el, e) {
    var st = calState(el), d = st.drag, $c = st.$container;
    if (!d) { return; }
    switch (d.mode) {
      case "move":
        if (!d.started) {
          if (Math.abs(e.clientX - d.x0) + Math.abs(e.clientY - d.y0) < 4) { return; }
          d.started = true;
          d.g = ghost(d.ev, e);
        }
        d.g.$g.css({ left: e.clientX - d.g.dx + "px", top: e.clientY - d.g.dy + "px" });
        break;
      case "resize": {
        var m = Math.max(slotMinutes(st, d.col, e.clientY), d.inst.s % DAY + st.slotDur);
        d.end = m;
        $(d.ev).css("height", Math.round((m - d.inst.s % DAY) * st.slotH / st.slotDur) + "px");
        break;
      }
      case "select": {
        var cell = cellAt($c.find(".ah-calendar-day"), e.clientX, e.clientY);
        if (cell) { d.to = parseDate(cell.getAttribute("data-date")); }
        var a = Math.min(d.from, d.to), b = Math.max(d.from, d.to);
        $c.find(".ah-calendar-day").each(function () {
          var n = parseDate(this.getAttribute("data-date"));
          $(this).toggleClass("ah-calendar-day-selected", n >= a && n <= b);
        });
        break;
      }
      case "create": {
        var cur = slotMinutes(st, d.col, e.clientY);
        d.top = Math.min(d.m0, cur);
        d.bot = Math.min(Math.max(d.m0 + st.slotDur, cur + st.slotDur), DAY);
        d.$ph.css({ top: Math.round(d.top * st.slotH / st.slotDur) + "px",
                    height: Math.round((d.bot - d.top) * st.slotH / st.slotDur) + "px" });
        break;
      }
      default: break;
    }
  }

  function calDragCancel(el) {
    var st = calState(el), d = st.drag;
    $(document).off(st.ns);
    st.drag = null;
    if (!d) { return; }
    if (d.g) { d.g.$g.remove(); }
    if (d.$ph) { d.$ph.remove(); }
    st.$container.find(".ah-calendar-day-selected").removeClass("ah-calendar-day-selected");
    if (d.mode === "resize") { calRender(el, $(el)); }
  }

  function calDragEnd(el, $el, e) {
    var st = calState(el), d = st.drag, $c = st.$container;
    calDragCancel(el);
    if (!d) { return; }
    var src = d.inst ? d.inst.src : null;
    switch (d.mode) {
      case "move": {
        if (!d.started) { return; }                    // a click
        st.justDragged = true;
        if (d.kind === "timed") {
          var col = cellAt($c.find(".ah-calendar-timegrid-day-col"), e.clientX, null);
          if (!col) { return; }
          var day = parseDate(col.getAttribute("data-date"));
          var top = e.clientY - d.g.dy;                // the ghost's top edge
          var start = day * DAY + Math.min(slotMinutes(st, col, top), DAY - st.slotDur);
          var delta = start - d.inst.s;
          if (delta) {
            calMove(el, $el, src, delta, delta, "ah:event-drop", day - Math.floor(d.inst.s / DAY));
          }
        } else {
          var sel = d.kind === "month" ? ".ah-calendar-day" : ".ah-calendar-timegrid-allday-cell";
          var cell = cellAt($c.find(sel), e.clientX, d.kind === "month" ? e.clientY : null);
          if (!cell) { return; }
          var days = parseDate(cell.getAttribute("data-date")) - d.origin;
          if (days) { calMove(el, $el, src, days * DAY, days * DAY, "ah:event-drop", days); }
        }
        break;
      }
      case "resize": {
        st.justDragged = true;
        var end = Math.floor(d.inst.s / DAY) * DAY + (d.end === undefined ? d.inst.e % DAY : d.end);
        if (d.end !== undefined && end !== d.inst.e) {
          calMove(el, $el, src, 0, end - d.inst.e, "ah:event-resize");
        }
        break;
      }
      case "select": {
        var a = Math.min(d.from, d.to), b = Math.max(d.from, d.to);
        calFire(el, $el, "ah:select", { from: isoDate(a), to: isoDate(b + 1), allDay: true });
        break;
      }
      case "create":
        st.justDragged = true;
        calFire(el, $el, "ah:select", { from: isoTime(d.day * DAY + d.top, false),
                                        to: isoTime(d.day * DAY + d.bot, false), allDay: false });
        break;
      default: break;
    }
  }

  function calFind(st, id) {
    for (var i = 0; i < st.events.length; i++) {
      if (st.events[i].id === String(id)) { return i; }
    }
    return -1;
  }

  AH.define("calendar", {
    init: function (el, $el) {
      ensureId(el, "ah-cal");
      var num = function (a, dflt) {
        var n = parseInt(el.getAttribute(a) || "", 10);
        return n > 0 || (n === 0 && dflt === 0) ? n : dflt;
      };
      var st = {
        ns: ".ahcal" + (++seq),
        $container: $el.children(".ah-calendar-view-container"),
        $title: $el.find(".ah-calendar-title"),
        events: readJson(el, "data-ah-events", []).map(calNormalize),
        view: el.getAttribute("data-ah-view") || "month",
        views: $el.find(".ah-calendar-view-btn").map(function () {
          return this.getAttribute("data-view"); }).get(),
        cur: parseDate(el.getAttribute("data-ah-value")) || todayNum(),
        first: Math.min(num("data-ah-first-day", 0), 6),
        agendaDays: num("data-ah-agenda-days", 30),
        maxEvents: num("data-ah-day-max-events", 3),
        slotDur: num("data-ah-slot-duration", 30),
        slotH: num("data-ah-slot-height", 20),
        hour24: el.getAttribute("data-ah-hour-format") === "24",
        L: $.extend({}, CAL_LABELS, readJson(el, "data-ah-labels", {})),
        editable: $el.hasClass("ah-calendar-editable"),
        selectable: $el.hasClass("ah-calendar-selectable"),
        insts: {}, range: [0, 0], timer: null, drag: null, justDragged: false
      };
      $.data(el, "ah-cal", st);
      // the server rendered the same view; render again for the browser's today
      calRender(el, $el);

      $el.on("click" + NS, ".ah-calendar-btn-prev", function () { calStep(el, $el, -1); })
        .on("click" + NS, ".ah-calendar-btn-next", function () { calStep(el, $el, 1); })
        .on("click" + NS, ".ah-calendar-btn-today", function () { calGo(el, $el, todayNum(), st.view); })
        .on("click" + NS, ".ah-calendar-view-btn", function () {
          calGo(el, $el, st.cur, this.getAttribute("data-view"));
        })
        .on("click" + NS, ".ah-calendar-event, .ah-calendar-list-event", function (e) {
          e.stopPropagation();
          if (st.justDragged) { st.justDragged = false; return; }
          calEventClick(el, $el, this);
        })
        .on("click" + NS, ".ah-calendar-day-more", function (e) {
          e.stopPropagation();
          calMore(el, $el, this);
        })
        .on("keydown" + NS, ".ah-calendar-event, .ah-calendar-list-event, .ah-calendar-day-more", function (e) {
          if (e.key !== "Enter" && e.key !== " ") { return; }
          e.preventDefault();
          if ($(this).hasClass("ah-calendar-day-more")) { calMore(el, $el, this); } else { calEventClick(el, $el, this); }
        });
      st.$container.on("mousedown" + NS, function (e) {
        st.justDragged = false;
        calDragStart(el, $el, e);
      });
    },
    destroy: function (el) {
      var st = calState(el);
      if (!st) { return; }
      if (st.drag) { calDragCancel(el); }
      $(document).off(st.ns);
      clearInterval(st.timer);
    },
    methods: {
      prev: function (el, $el) { calStep(el, $el, -1); },
      next: function (el, $el) { calStep(el, $el, 1); },
      today: function (el, $el) { calGo(el, $el, todayNum(), calState(el).view); },
      changeView: function (el, $el, v) {
        if (["month", "week", "day", "list"].indexOf(v) >= 0) { calGo(el, $el, calState(el).cur, v); }
      },
      setValue: function (el, $el, v) {
        var d = parseDate(String(v || "").slice(0, 10));
        if (d !== null) { calState(el).cur = d; calRender(el, $el); }
      },
      getValue: function (el) { return el.getAttribute("data-ah-value"); },
      setEvents: function (el, $el, evs) {
        calState(el).events = (evs || []).map(calNormalize);
        calRender(el, $el);
      },
      addEvent: function (el, $el, ev) {
        var st = calState(el), n = calNormalize(ev, st.events.length + 1), i = calFind(st, n.id);
        if (i >= 0) { st.events[i] = n; } else { st.events.push(n); }
        calRender(el, $el);
      },
      updateEvent: function (el, $el, id, changes) {
        var st = calState(el), i = calFind(st, id);
        if (i < 0) { return; }
        var merged = $.extend({}, st.events[i], changes || {});
        if (changes && (changes.start || changes.end) && changes.allDay === undefined) { delete merged.allDay; }
        st.events[i] = calNormalize(merged, i + 1);
        calRender(el, $el);
      },
      removeEvent: function (el, $el, id) {
        var st = calState(el), i = calFind(st, id);
        if (i >= 0) { st.events.splice(i, 1); calRender(el, $el); }
      },
      getEvents: function (el) {
        return calState(el).events.map(function (e) { return $.extend({}, e); });
      }
    }
  });

  // ==================================================================
  // datetime_input
  // ==================================================================

  var DTI_LABELS = {
    months: CAL_LABELS.months,
    weekdays: ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"],
    title: "MMMM yyyy", time: "Time",
    prev_month: "Previous month", next_month: "Next month"
  };
  // [pattern, type, min, max]; single letters are two digits wide, as in
  // segments/1 of the Erlang side.
  var TOKENS = [["yyyy", "year", 1900, 2100], ["yy", "year2", 0, 99], ["MM", "month", 1, 12],
                ["M", "month", 1, 12], ["dd", "day", 1, 31], ["d", "day", 1, 31],
                ["HH", "hour", 0, 23], ["H", "hour", 0, 23], ["hh", "hour12", 1, 12],
                ["h", "hour12", 1, 12], ["mm", "minute", 0, 59], ["m", "minute", 0, 59],
                ["ss", "second", 0, 59], ["s", "second", 0, 59], ["aa", "ampm", 0, 1],
                ["a", "ampm", 0, 1]];
  var SEG_NAMES = { year: "Year", year2: "Year", month: "Month", day: "Day", hour: "Hour",
                    hour12: "Hour", minute: "Minute", second: "Second", ampm: "AM/PM" };

  // format.cljs parse-format: [{type, start, end, len, min, max, editable}]
  function dtiSegments(f) {
    var segs = [], pos = 0, i = 0;
    while (i < f.length) {
      var tok = null;
      for (var k = 0; k < TOKENS.length; k++) {
        if (f.substr(i, TOKENS[k][0].length) === TOKENS[k][0]) { tok = TOKENS[k]; break; }
      }
      if (tok) {
        var len = tok[1] === "year" ? 4 : 2;
        segs.push({ type: tok[1], start: pos, end: pos + len, len: len, min: tok[2], max: tok[3],
                    editable: true });
        pos += len;
        i += tok[0].length;
      } else {
        var last = segs[segs.length - 1];
        if (last && !last.editable) { last.text += f[i]; last.end++; last.len++; } else {
          segs.push({ type: "literal", text: f[i], start: pos, end: pos + 1, len: 1, editable: false });
        }
        pos++;
        i++;
      }
    }
    return segs;
  }

  function dtiKind(segs) {
    var date = false, time = false, sec = false;
    segs.forEach(function (s) {
      if (/^(year|year2|month|day)$/.test(s.type)) { date = true; }
      if (/^(hour|hour12|minute|second|ampm)$/.test(s.type)) { time = true; }
      if (s.type === "second") { sec = true; }
    });
    return { date: date || !time, time: time, sec: sec };
  }

  // values are {y, mo, d, h, mi, s}
  function dtiParse(s, kind) {
    s = String(s || "");
    var m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2})(?::(\d{2}))?)?$/.exec(s);
    if (m && validYmd(+m[1], +m[2], +m[3])) {
      return { y: +m[1], mo: +m[2], d: +m[3], h: +(m[4] || 0), mi: +(m[5] || 0), s: +(m[6] || 0) };
    }
    m = /^(\d{2}):(\d{2})(?::(\d{2}))?$/.exec(s);
    if (m && kind.time) {
      var t = ymd(todayNum());
      return { y: t[0], mo: t[1], d: t[2], h: +m[1], mi: +m[2], s: +(m[3] || 0) };
    }
    return null;
  }

  function dtiIso(v, kind) {
    if (!v) { return ""; }
    var date = pad4(v.y) + "-" + pad(v.mo) + "-" + pad(v.d);
    var time = pad(v.h) + ":" + pad(v.mi) + (kind.sec ? ":" + pad(v.s) : "");
    return !kind.time ? date : (kind.date ? date + "T" + time : time);
  }

  // A number that orders values of this kind.
  function dtiOrd(v, kind) {
    var t = v.h * 3600 + v.mi * 60 + v.s, d = dnum(v.y, v.mo, v.d);
    return !kind.time ? d : (kind.date ? d * 86400 + t : t);
  }

  function segValue(v, seg) {
    switch (seg.type) {
      case "year": return v.y;
      case "year2": return v.y % 100;
      case "month": return v.mo;
      case "day": return v.d;
      case "hour": return v.h;
      case "hour12": return h12(v.h);
      case "minute": return v.mi;
      case "second": return v.s;
      case "ampm": return v.h < 12 ? 0 : 1;
      default: return null;
    }
  }

  // The day is clamped to the month's length (sigil's js/Date rolls over).
  function setSeg(v0, seg, val) {
    var v = $.extend({}, v0);
    switch (seg.type) {
      case "year": v.y = val; break;
      case "year2": v.y = Math.floor(v.y / 100) * 100 + val; break;
      case "month": v.mo = val; break;
      case "day": v.d = val; break;
      case "hour": v.h = val; break;
      case "hour12": {
        var pm = v.h >= 12;
        v.h = pm ? (val === 12 ? 12 : val + 12) : (val === 12 ? 0 : val);
        break;
      }
      case "minute": v.mi = val; break;
      case "second": v.s = val; break;
      case "ampm": v.h = val === 0 ? (v.h >= 12 ? v.h - 12 : v.h) : (v.h < 12 ? v.h + 12 : v.h); break;
      default: break;
    }
    v.d = Math.min(v.d, lastDay(v.y, v.mo));
    return v;
  }

  function segText(v, seg) {
    if (!seg.editable) { return seg.text; }
    if (seg.type === "ampm") { return v.h < 12 ? "AM" : "PM"; }
    var n = segValue(v, seg);
    return seg.len === 4 ? pad4(n) : pad(n);
  }

  function segMax(v, seg) { return seg.type === "day" ? lastDay(v.y, v.mo) : seg.max; }

  function dtiState(el) { return $.data(el, "ah-dti"); }

  function dtiEditable(st) { return st.segs.map(function (s, i) { return s.editable ? i : -1; })
    .filter(function (i) { return i >= 0; }); }

  function dtiDisplay(st) {
    return st.value ? st.segs.map(function (s) { return segText(st.value, s); }).join("") : "";
  }

  function dtiSelect(st) {
    var seg = st.segs[st.active];
    if (!seg || !st.value || document.activeElement !== st.input) { return; }
    try { st.input.setSelectionRange(seg.start, seg.end); } catch (err) { /* not focused */ }
  }

  // Show the value, publish it (data-ah-value, hidden input) and fire
  // `input' when it changed; `change' fires on leaving (dtiCommit).
  function dtiShow(el, $el) {
    var st = dtiState(el);
    st.input.value = dtiDisplay(st);
    dtiSelect(st);
    var iso = dtiIso(st.value, st.kind), old = el.getAttribute("data-ah-value") || "";
    el.setAttribute("data-ah-value", iso);
    $el.children("input[type=hidden]").val(iso);
    $el.find(".ah-dti-label").toggleClass("ah-dti-label-float", !!st.value || $el.hasClass("ah-dti-focused"));
    if (iso !== old) { $el.trigger("input"); }
    if (st.open) { dtiRenderCal(el); }
  }

  function dtiClamp(st, v) {
    if (!v) { return v; }
    var k = dtiOrd(v, st.kind);
    if (st.min && k < dtiOrd(st.min, st.kind)) { return $.extend({}, st.min); }
    if (st.max && k > dtiOrd(st.max, st.kind)) { return $.extend({}, st.max); }
    return v;
  }

  function dtiCommit(el, $el) {
    var st = dtiState(el);
    dtiFlush(st);
    st.value = dtiClamp(st, st.value);
    dtiShow(el, $el);
    var iso = dtiIso(st.value, st.kind);
    if (iso !== st.committed) {
      st.committed = iso;
      $el.trigger("change");
    }
  }

  function dtiEnsure(st) {
    if (!st.value) {
      var n = new Date();
      st.value = { y: n.getFullYear(), mo: n.getMonth() + 1, d: n.getDate(),
                   h: n.getHours(), mi: n.getMinutes(), s: 0 };
    }
  }

  function dtiFlush(st) {
    if (st.active === null || !st.buf) { return; }
    var seg = st.segs[st.active], n = parseInt(st.buf, 10);
    st.buf = "";
    if (st.value && !isNaN(n)) { st.value = setSeg(st.value, seg, Math.max(seg.min, Math.min(seg.max, n))); }
  }

  function dtiFocusSeg(el, $el, i) {
    var st = dtiState(el);
    dtiFlush(st);
    st.active = i;
    dtiShow(el, $el);
  }

  function dtiAnnounce(el, $el) {
    var st = dtiState(el), seg = st.segs[st.active];
    if (seg && st.value) { $el.find(".ah-dti-live").text(SEG_NAMES[seg.type] + " " + segText(st.value, seg)); }
  }

  // editor.cljs handle-digit!: buffer until the part is full, then commit
  // and move on
  function dtiDigit(el, $el, ch) {
    var st = dtiState(el), seg = st.segs[st.active];
    if (!seg || !seg.editable || seg.type === "ampm") { return; }
    dtiEnsure(st);
    st.buf += ch;
    if (st.buf.length >= seg.len) {
      var n = parseInt(st.buf, 10);
      st.buf = "";
      st.value = setSeg(st.value, seg, Math.max(seg.min, Math.min(segMax(st.value, seg), n)));
      var next = dtiEditable(st).filter(function (i) { return i > st.active; })[0];
      if (next !== undefined) { st.active = next; }
      dtiShow(el, $el);
    } else {
      // preview: the typed digits right-aligned in the part
      var shown = dtiDisplay(st), p = (new Array(seg.len - st.buf.length + 1)).join(" ") + st.buf;
      st.input.value = shown.slice(0, seg.start) + p + shown.slice(seg.end);
      dtiSelect(st);
    }
  }

  function dtiStep(el, $el, delta, big) {
    var st = dtiState(el), seg = st.segs[st.active];
    if (!seg || !seg.editable) { return; }
    dtiEnsure(st);
    dtiFlush(st);
    var v = st.value;
    if (seg.type === "ampm") {
      st.value = setSeg(v, seg, segValue(v, seg) ? 0 : 1);
    } else {
      var mn = seg.min, mx = segMax(v, seg), cur = segValue(v, seg), n;
      if (big) {
        n = mn + (((cur - mn + delta) % (mx - mn + 1)) + (mx - mn + 1)) % (mx - mn + 1);
      } else {
        n = cur + delta;
        n = n < mn ? mx : (n > mx ? mn : n);
      }
      st.value = setSeg(v, seg, n);
    }
    dtiShow(el, $el);
    dtiAnnounce(el, $el);
  }

  function dtiMoveSeg(el, $el, dir) {
    var st = dtiState(el), eds = dtiEditable(st);
    var next = dir > 0 ? eds.filter(function (i) { return i > st.active; })[0]
      : eds.filter(function (i) { return i < st.active; }).pop();
    if (next === undefined) { dtiFlush(st); return false; }
    dtiFocusSeg(el, $el, next);
    return true;
  }

  function dtiBlocked($el) { return $el.hasClass("ah-dti-disabled") || $el.hasClass("ah-dti-readonly"); }

  // editor.cljs on-keydown
  function dtiKey(el, $el, e) {
    var st = dtiState(el), k = e.key;
    if (k === "Tab") {
      if (!dtiBlocked($el) && dtiMoveSeg(el, $el, e.shiftKey ? -1 : 1)) { e.preventDefault(); } else { dtiClose(el, $el); }
      return;
    }
    if (e.ctrlKey || e.metaKey) { return; }
    if (k === "Escape") {
      if (st.open) { e.preventDefault(); dtiClose(el, $el); }
      return;
    }
    e.preventDefault();
    if (dtiBlocked($el)) { return; }
    if ((k === "ArrowDown" && e.altKey) || k === "F4") {
      if (st.open) { dtiClose(el, $el); } else { dtiOpen(el, $el); }
      return;
    }
    if (/^[0-9]$/.test(k)) { dtiDigit(el, $el, k); return; }
    var eds = dtiEditable(st), seg = st.segs[st.active];
    switch (k) {
      case "ArrowUp": dtiStep(el, $el, 1, false); break;
      case "ArrowDown": dtiStep(el, $el, -1, false); break;
      case "PageUp": dtiStep(el, $el, 10, true); break;
      case "PageDown": dtiStep(el, $el, -10, true); break;
      case "ArrowLeft": dtiMoveSeg(el, $el, -1); break;
      case "ArrowRight": dtiMoveSeg(el, $el, 1); break;
      case "Home": dtiFocusSeg(el, $el, eds[0]); break;
      case "End": dtiFocusSeg(el, $el, eds[eds.length - 1]); break;
      case "Backspace":
      case "Delete":
        if (seg && seg.editable && st.value) {
          st.buf = "";
          st.value = setSeg(st.value, seg, seg.min);
          dtiShow(el, $el);
        }
        break;
      case "a": case "A": case "p": case "P":
        if (seg && seg.type === "ampm" && st.value) {
          st.value = setSeg(st.value, seg, /a/i.test(k) ? 0 : 1);
          dtiShow(el, $el);
        }
        break;
      default: break;
    }
  }

  // format.cljs segment-at-cursor: the part under the caret, or the nearest
  function dtiSegAt(st, pos) {
    var eds = dtiEditable(st), best = eds[0], dist = Infinity;
    for (var i = 0; i < eds.length; i++) {
      var s = st.segs[eds[i]];
      if (pos >= s.start && pos < s.end) { return eds[i]; }
      var dd = Math.min(Math.abs(pos - s.start), Math.abs(pos - s.end));
      if (dd < dist) { dist = dd; best = eds[i]; }
    }
    return best;
  }

  // ---- drop-down calendar ----

  function dtiCalView(st) {
    var L = st.L, y = st.navY, m = st.navM, today = todayNum();
    var start = sow(dnum(y, m, 1), st.first);
    var sel = st.value ? dnum(st.value.y, st.value.mo, st.value.d) : null;
    var min = st.min && st.kind.date ? dnum(st.min.y, st.min.mo, st.min.d) : null;
    var max = st.max && st.kind.date ? dnum(st.max.y, st.max.mo, st.max.d) : null;
    var days = [];
    for (var i = 0; i < 42; i++) {
      var d = start + i, p = ymd(d);
      var dis = (min !== null && d < min) || (max !== null && d > max);
      days.push({
        cls: cls("ah-dti-cal-day", [[d === today, "ah-dti-cal-day-today"], [d === sel, "ah-dti-cal-day-selected"],
                                    [p[1] !== m, "ah-dti-cal-day-other"], [dis, "ah-dti-cal-day-disabled"]]),
        date: isoDate(d), day: String(p[2]), disabled: dis, selected: d === sel
      });
    }
    var weekdays = [];
    for (var k = 0; k < 7; k++) { weekdays.push({ label: L.weekdays[(st.first + k) % 7] }); }
    var title = L.title.replace(/yyyy|MMMM|MM|M/g, function (t) {
      return t === "yyyy" ? String(y) : t === "MMMM" ? L.months[m - 1] : t === "MM" ? pad(m) : String(m);
    });
    return { title: title, prev_month: L.prev_month, next_month: L.next_month, weekdays: weekdays,
             days: days, show_time: st.showTime && !!st.value, time_label: L.time,
             hours: st.value ? pad(st.value.h) : "", minutes: st.value ? pad(st.value.mi) : "" };
  }

  function dtiRenderCal(el) {
    var st = dtiState(el);
    var focused = document.activeElement;
    var field = focused && $.contains(st.$dd[0], focused) ? focused.getAttribute("data-field") : null;
    st.$dd.html(AH.tpl.datetime_input_calendar(dtiCalView(st)));
    if (field) { st.$dd.find('[data-field="' + field + '"]').trigger("focus"); }
    if (st.float) { st.float.update(); }
  }

  function dtiOpen(el, $el) {
    var st = dtiState(el);
    if (st.open || !st.$dd.length || dtiBlocked($el)) { return; }
    var v = st.value;
    var t = ymd(todayNum());
    st.navY = v ? v.y : t[0];
    st.navM = v ? v.mo : t[1];
    st.open = true;
    st.$dd.prop("hidden", false);
    dtiRenderCal(el);
    st.float = AH.float(st.$dd[0], $el.children(".ah-dti-row")[0], { offset: 2 });
    $(st.input).attr("aria-expanded", "true");
    $(document).on("mousedown" + st.ns, function (e) { if (outside(el, e)) { dtiClose(el, $el); } });
    $el.trigger("ah:open");
  }

  function dtiClose(el, $el) {
    var st = dtiState(el);
    if (!st.open) { return; }
    st.open = false;
    st.$dd.prop("hidden", true).empty();
    if (st.float) { st.float.stop(); st.float = null; }
    $(st.input).attr("aria-expanded", "false");
    $(document).off(st.ns);
    $el.trigger("ah:close");
  }

  // dropdown.cljs on-day-click: keep the time, clamp, fire change
  function dtiPickDay(el, $el, iso) {
    var st = dtiState(el), d = parseDate(iso);
    if (d === null) { return; }
    var p = ymd(d), v = st.value || { h: 0, mi: 0, s: 0 };
    st.value = dtiClamp(st, { y: p[0], mo: p[1], d: p[2], h: v.h, mi: v.mi, s: v.s });
    st.navY = st.value.y;
    st.navM = st.value.mo;
    dtiShow(el, $el);
    dtiCommit(el, $el);
    if (!st.showTime) { dtiClose(el, $el); st.input.focus(); }
  }

  function dtiSpinStop(st) { clearTimeout(st.spin); st.spin = null; }

  AH.define("datetime_input", {
    init: function (el, $el) {
      ensureId(el, "ah-dti");
      var segs = dtiSegments(el.getAttribute("data-ah-format") || "yyyy-MM-dd");
      var kind = dtiKind(segs);
      var first = parseInt(el.getAttribute("data-ah-first-day") || "0", 10);
      var st = {
        ns: ".ahdti" + (++seq),
        segs: segs, kind: kind,
        input: $el.find("input.ah-dti-input")[0],
        $dd: $el.children(".ah-dti-dropdown"),
        value: dtiParse(el.getAttribute("data-ah-value"), kind),
        min: dtiParse(el.getAttribute("data-ah-min"), kind),
        max: dtiParse(el.getAttribute("data-ah-max"), kind),
        first: first >= 0 && first <= 6 ? first : 0,
        showTime: el.hasAttribute("data-ah-show-time"),
        L: $.extend({}, DTI_LABELS, readJson(el, "data-ah-labels", {})),
        active: null, buf: "", open: false, float: null, spin: null
      };
      st.committed = dtiIso(st.value, kind);
      $.data(el, "ah-dti", st);
      var $in = $(st.input);
      $in.on("focus" + NS, function () {
        $el.addClass("ah-dti-focused");
        if (st.active === null) { st.active = dtiEditable(st)[0]; }
        $el.find(".ah-dti-label").addClass("ah-dti-label-float");
        setTimeout(function () { dtiSelect(st); }, 0);
      }).on("mouseup" + NS, function () {
        if (!st.value) { return; }
        dtiFocusSeg(el, $el, dtiSegAt(st, st.input.selectionStart || 0));
      }).on("keydown" + NS, function (e) { dtiKey(el, $el, e); })
        // the text field is internal: only the root reports changes
        .on("change" + NS + " input" + NS, function (e) { e.stopPropagation(); });
      // leaving the component (the time fields of the drop-down are inside)
      $el.on("focusout" + NS, function (e) {
        if (e.relatedTarget && $.contains(el, e.relatedTarget)) { return; }
        setTimeout(function () {
          if ($.contains(el, document.activeElement)) { return; }
          $el.removeClass("ah-dti-focused");
          dtiCommit(el, $el);
          dtiClose(el, $el);
        }, 0);
      });
      $el.on("click" + NS, ".ah-dti-cal-btn", function () {
        if (dtiBlocked($el)) { return; }
        st.input.focus();
        if (st.open) { dtiClose(el, $el); } else { dtiOpen(el, $el); }
      });
      $el.on("mousedown" + NS, ".ah-dti-cal-btn", function (e) { e.preventDefault(); });
      // spinner: step, then repeat after 400ms every 120ms while held
      $el.on("mousedown" + NS, ".ah-dti-spin", function (e) {
        e.preventDefault();
        if (dtiBlocked($el)) { return; }
        var delta = $(this).hasClass("ah-dti-spin-up") ? 1 : -1;
        if (st.active === null) { st.active = dtiEditable(st)[0]; }
        st.input.focus();
        dtiSpinStop(st);
        dtiStep(el, $el, delta, false);
        var rep = function () { dtiStep(el, $el, delta, false); st.spin = setTimeout(rep, 120); };
        st.spin = setTimeout(rep, 400);
      }).on("mouseup" + NS + " mouseleave" + NS, ".ah-dti-spin", function () { dtiSpinStop(st); });
      // drop-down: keep the focus in the field, except for the time inputs
      st.$dd.on("mousedown" + NS, function (e) {
        if (!$(e.target).is("input")) { e.preventDefault(); }
      }).on("click" + NS, ".ah-dti-cal-day", function () {
        if (!$(this).hasClass("ah-dti-cal-day-disabled")) { dtiPickDay(el, $el, this.getAttribute("data-date")); }
      }).on("click" + NS, "[data-action]", function () {
        var n = dnum(st.navY, st.navM, 1);
        var t = ymd(addMonths(n, this.getAttribute("data-action") === "prev-month" ? -1 : 1));
        st.navY = t[0];
        st.navM = t[1];
        dtiRenderCal(el);
      }).on("change" + NS, ".ah-dti-time-input", function (e) {
        e.stopPropagation();
        var n = parseInt(this.value, 10);
        if (isNaN(n)) { return; }
        dtiEnsure(st);
        st.value = $.extend({}, st.value);
        if (this.getAttribute("data-field") === "hours") { st.value.h = Math.max(0, Math.min(23, n)); }
        else { st.value.mi = Math.max(0, Math.min(59, n)); }
        dtiShow(el, $el);
        dtiCommit(el, $el);
      }).on("input" + NS, ".ah-dti-time-input", function (e) { e.stopPropagation(); })
        .on("keydown" + NS, ".ah-dti-time-input", function (e) {
          if (e.key === "Escape") { e.preventDefault(); dtiClose(el, $el); st.input.focus(); }
          if (e.key === "Enter") { e.preventDefault(); $(this).trigger("change"); }
        });
    },
    destroy: function (el, $el) {
      var st = dtiState(el);
      if (!st) { return; }
      dtiSpinStop(st);
      dtiClose(el, $el);
    },
    methods: {
      setValue: function (el, $el, v) {
        var st = dtiState(el);
        st.value = v ? dtiParse(v, st.kind) : null;
        st.buf = "";
        st.committed = dtiIso(st.value, st.kind);
        dtiShow(el, $el);
      },
      getValue: function (el) { return el.getAttribute("data-ah-value"); },
      clear: function (el, $el) {
        var st = dtiState(el);
        st.value = null;
        st.buf = "";
        dtiShow(el, $el);
        dtiCommit(el, $el);
      },
      open: function (el, $el) { dtiOpen(el, $el); },
      close: function (el, $el) { dtiClose(el, $el); }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/form_choice.js ---- */
/* Behaviours of the form_choice components (designs/04-components.md).
 *
 * The single controls (checkbox, radiobutton, switch-button) keep a native
 * <input> inside a <label>: the browser does the toggling, the keyboard
 * (Space) and the change event; these behaviours only mirror the input's
 * state onto sigil's classes and add sigil's extras (three states, locked).
 *
 * The groups (checkbox-group, radiobutton-group, radio-cards) keep the
 * root's data-ah-value in sync, stop the inner inputs' change at the root
 * and fire one "change" on the root, so data-ah-on on the root sees the
 * group value. Arrow keys move a radio selection as in sigil.
 *
 * rating is a value-bearing custom control: data-ah-value, the hidden
 * input and a "change" on the root; "ah:hover" [value|null] while the
 * pointer previews a value.
 *
 * Methods never fire change (so a server call does not echo back).
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var INPUT = "input.ah-choice-input";

  function truthy(v) {
    return v === true || v === "true" || v === "on" || v === 1 || v === "1";
  }

  // ------------------------------------------------------------------
  // Mirroring an input onto sigil's classes
  // ------------------------------------------------------------------

  function syncCheckbox(input) {
    var mixed = input.indeterminate;
    var on = input.checked && !mixed;
    $(input).closest(".ah-checkbox")
      .toggleClass("ah-checkbox-checked", on)
      .toggleClass("ah-checkbox-indeterminate", mixed)
      .toggleClass("ah-checkbox-disabled", input.disabled)
      .find(".ah-checkbox-check")
      .toggleClass("ah-checkbox-check-checked", on)
      .toggleClass("ah-checkbox-check-indeterminate", mixed);
  }

  function syncRadio(input) {
    $(input).closest(".ah-radiobutton")
      .toggleClass("ah-radiobutton-checked", input.checked)
      .toggleClass("ah-radiobutton-disabled", input.disabled)
      .find(".ah-radiobutton-check")
      .toggleClass("ah-radiobutton-check-checked", input.checked);
  }

  function syncSwitch(input) {
    $(input).closest(".ah-switch")
      .toggleClass("ah-switch-on", input.checked)
      .toggleClass("ah-switch-disabled", input.disabled);
  }

  function syncCard(input) {
    $(input).closest(".ah-radio-cards__card")
      .attr("data-selected", input.checked ? "true" : "false")
      .attr("data-disabled", input.disabled ? "true" : "false");
  }

  // The radios a browser treats as one group with this one.
  function sameGroup(input) {
    if (!input.name) {
      return $(input);
    }
    return $(input.form || document).find("input[type=radio]").filter(function () {
      return this.name === input.name && this.form === input.form;
    });
  }

  function inputOf($el) {
    return $el.find(INPUT).get(0);
  }

  // locked: focusable but the user cannot change it.
  function bindLocked(el, $el) {
    $el.on("click" + NS, INPUT, function (e) {
      if (el.hasAttribute("data-ah-locked")) {
        e.preventDefault();
      }
    });
  }

  function setDisabled(el, input, on, sync) {
    input.disabled = !!on;
    sync(input);
  }

  // ------------------------------------------------------------------
  // checkbox
  // ------------------------------------------------------------------

  function checkState(input) {
    return input.indeterminate ? "mixed" : input.checked;
  }

  function setCheck(input, v) {
    var mixed = v === "mixed" || v === "indeterminate" || v === null;
    input.indeterminate = mixed;
    input.checked = !mixed && truthy(v);
    syncCheckbox(input);
  }

  AH.define("checkbox", {
    init: function (el, $el) {
      var input = inputOf($el);
      if (!input) { return; }
      if ($el.hasClass("ah-checkbox-indeterminate")) {
        input.indeterminate = true;
      }
      $.data(el, "ah-state", checkState(input));
      bindLocked(el, $el);
      $el.on("change" + NS, INPUT, function (e) {
        // sigil's three states: checked -> mixed -> unchecked -> checked
        if (!e.isTrigger && el.hasAttribute("data-ah-three-states")) {
          var prev = $.data(el, "ah-state");
          setCheck(input, prev === true ? "mixed" : prev === "mixed" ? false : true);
        }
        $.data(el, "ah-state", checkState(input));
        syncCheckbox(input);
      });
    },
    methods: {
      // true | false | "mixed"
      setChecked: function (el, $el, v) {
        var input = inputOf($el);
        setCheck(input, v);
        $.data(el, "ah-state", checkState(input));
      },
      getValue: function (el, $el) { return checkState(inputOf($el)); },
      setDisabled: function (el, $el, on) { setDisabled(el, inputOf($el), on, syncCheckbox); }
    }
  });

  // ------------------------------------------------------------------
  // radiobutton
  // ------------------------------------------------------------------

  function syncRadioGroup(input) {
    sameGroup(input).each(function () { syncRadio(this); });
  }

  AH.define("radiobutton", {
    init: function (el, $el) {
      bindLocked(el, $el);
      $el.on("change" + NS, INPUT, function () { syncRadioGroup(this); });
    },
    methods: {
      setChecked: function (el, $el, v) {
        var input = inputOf($el);
        input.checked = truthy(v);
        syncRadioGroup(input);
      },
      getValue: function (el, $el) { return inputOf($el).checked; },
      setDisabled: function (el, $el, on) { setDisabled(el, inputOf($el), on, syncRadio); }
    }
  });

  // ------------------------------------------------------------------
  // switch-button
  // ------------------------------------------------------------------

  AH.define("switch-button", {
    init: function (el, $el) {
      bindLocked(el, $el);
      $el.on("change" + NS, INPUT, function () { syncSwitch(this); });
    },
    methods: {
      setChecked: function (el, $el, v) {
        var input = inputOf($el);
        input.checked = truthy(v);
        syncSwitch(input);
      },
      getValue: function (el, $el) { return inputOf($el).checked; },
      setDisabled: function (el, $el, on) { setDisabled(el, inputOf($el), on, syncSwitch); }
    }
  });

  // ------------------------------------------------------------------
  // Groups
  // ------------------------------------------------------------------

  // kind: {radio, sync(input), item selector, disabled class}
  function groupValue($el) {
    return $el.find(INPUT).filter(":checked").map(function () {
      return this.value;
    }).get().join(",");
  }

  function syncGroup($el, kind) {
    var $inputs = $el.find(INPUT);
    $inputs.each(function () { kind.sync(this); });
    $el.attr("data-ah-value", groupValue($el));
    if (kind.radio) {
      // roving tab stop: the checked radio, else the first enabled one
      var $enabled = $inputs.filter(":not(:disabled)");
      var $stop = $enabled.filter(":checked");
      if (!$stop.length) { $stop = $enabled.first(); }
      $inputs.attr("tabindex", "-1");
      $stop.first().attr("tabindex", "0");
    }
  }

  function setGroupValue($el, kind, v) {
    var vals = Array.isArray(v) ? v.map(String)
      : (v === null || v === undefined || v === "") ? [] : String(v).split(",");
    if (kind.radio) { vals = vals.slice(0, 1); }
    $el.find(INPUT).each(function () {
      this.checked = vals.indexOf(this.value) >= 0;
    });
    syncGroup($el, kind);
  }

  function setGroupDisabled(el, $el, kind, on) {
    $el.find(INPUT).each(function () {
      var own = $(this).closest("[data-ah-item-disabled]").length > 0;
      this.disabled = !!on || own;
      if (kind.itemDisabled) {
        $(this).closest(kind.item).toggleClass(kind.itemDisabled, this.disabled);
      }
    });
    if (kind.disabled) { $el.toggleClass(kind.disabled, !!on); }
    if (kind.disabledAttr) { $el.attr("data-disabled", on ? "true" : "false"); }
    if (on) { $el.attr("aria-disabled", "true"); } else { $el.removeAttr("aria-disabled"); }
    syncGroup($el, kind);
  }

  function fireChange(el, $el, kind) {
    syncGroup($el, kind);
    $el.trigger("change");
  }

  // Arrow keys: move to the next/previous enabled radio, select and focus it.
  function arrowKeys(el, $el, kind) {
    $el.on("keydown" + NS, INPUT, function (e) {
      var step = { ArrowRight: 1, ArrowDown: 1, ArrowLeft: -1, ArrowUp: -1 }[e.key];
      if (!step || e.altKey || e.ctrlKey || e.metaKey) { return; }
      var $enabled = $el.find(INPUT).filter(":not(:disabled)");
      var n = $enabled.length;
      if (!n) { return; }
      e.preventDefault();
      var i = $enabled.index(this);
      var next = $enabled.get(((i < 0 ? 0 : i) + step + n) % n);
      next.checked = true;
      next.focus();
      fireChange(el, $el, kind);
    });
  }

  function defineGroup(name, kind) {
    AH.define(name, {
      init: function (el, $el) {
        syncGroup($el, kind);
        $el.on("change" + NS, INPUT, function (e) {
          // one change per user action, fired by the root itself
          e.stopPropagation();
          fireChange(el, $el, kind);
        });
        if (kind.radio) { arrowKeys(el, $el, kind); }
      },
      methods: {
        // a value, an array or "a,b"; does not fire change
        setValue: function (el, $el, v) { setGroupValue($el, kind, v); },
        getValue: function (el, $el) {
          var v = groupValue($el);
          return kind.radio ? v : (v ? v.split(",") : []);
        },
        setDisabled: function (el, $el, on) { setGroupDisabled(el, $el, kind, on); }
      }
    });
  }

  defineGroup("checkbox-group", {
    radio: false, sync: syncCheckbox, item: ".ah-checkbox-group-item",
    itemDisabled: "ah-checkbox-group-item-disabled", disabled: "ah-checkbox-group-disabled"
  });
  defineGroup("radiobutton-group", {
    radio: true, sync: syncRadio, item: ".ah-radiobutton-group-item",
    itemDisabled: "ah-radiobutton-group-item-disabled", disabled: "ah-radiobutton-group-disabled"
  });
  defineGroup("radio-cards", {
    radio: true, sync: syncCard, item: ".ah-radio-cards__card", disabledAttr: true
  });

  // ------------------------------------------------------------------
  // rating
  // ------------------------------------------------------------------

  function ratingState(el) {
    return {
      value: parseFloat(el.getAttribute("data-ah-value")) || 0,
      max: parseInt(el.getAttribute("data-ah-max"), 10) || 5,
      step: el.getAttribute("data-precision") === "0.5" ? 0.5 : 1,
      live: el.getAttribute("data-readonly") !== "true" &&
            el.getAttribute("data-disabled") !== "true",
      clear: el.getAttribute("data-allow-clear") !== "false"
    };
  }

  function paintRating($el, v) {
    $el.find(".ah-rating__star").each(function (i) {
      var r = Math.max(0, Math.min(1, v - i));
      $(this).find(".ah-rating__filled").css("width", (r * 100) + "%");
    });
  }

  function setRating(el, $el, v, fire) {
    var s = ratingState(el);
    v = Math.max(0, Math.min(s.max, Math.round(Number(v) / s.step) * s.step || 0));
    var changed = v !== s.value;
    el.setAttribute("data-ah-value", String(v));
    $el.children("input[type=hidden]").val(String(v));
    $el.find(".ah-rating__star").each(function (i) {
      this.setAttribute("aria-checked", v >= i + 1 ? "true" : "false");
    });
    paintRating($el, v);
    if (fire && changed) {
      $el.trigger("change");
    }
  }

  // The value under the pointer; a keyboard click (detail 0) takes the
  // whole star.
  function ratingAt(el, star, e) {
    var s = ratingState(el);
    var idx = parseInt(star.getAttribute("data-index"), 10);
    if (s.step === 0.5 && e.detail !== 0 && e.clientX !== undefined) {
      var rect = star.getBoundingClientRect();
      return idx + ((e.clientX - rect.left) / rect.width <= 0.5 ? 0.5 : 1);
    }
    return idx + 1;
  }

  AH.define("rating", {
    init: function (el, $el) {
      $el.on("mousemove" + NS, ".ah-rating__star", function (e) {
        if (!ratingState(el).live) { return; }
        var v = ratingAt(el, this, e);
        paintRating($el, v);
        $el.trigger("ah:hover", [v]);
      });
      $el.on("mouseleave" + NS, function () {
        var s = ratingState(el);
        if (!s.live) { return; }
        paintRating($el, s.value);
        $el.trigger("ah:hover", [null]);
      });
      $el.on("click" + NS, ".ah-rating__star", function (e) {
        var s = ratingState(el);
        if (!s.live) { return; }
        var v = ratingAt(el, this, e);
        setRating(el, $el, s.clear && v === s.value ? 0 : v, true);
      });
      $el.on("keydown" + NS, ".ah-rating__star", function (e) {
        var s = ratingState(el);
        if (!s.live) { return; }
        var d = { ArrowRight: 1, ArrowUp: 1, ArrowLeft: -1, ArrowDown: -1 }[e.key];
        if (!d) { return; }
        e.preventDefault();
        setRating(el, $el, s.value + d * s.step, true);
      });
    },
    methods: {
      setValue: function (el, $el, v) { setRating(el, $el, v, false); },
      getValue: function (el) { return ratingState(el).value; }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/form_entry.js ---- */
/* Behaviours of the form_entry components (designs/04-components.md),
 * after sigil's form/masked_input, formatted_input, range_selector and
 * repeat_button.
 *
 *   masked-input     only characters that fit the mask are typed, deleted
 *                    or pasted; input on each edit, change on blur
 *   formatted-input  an integer (BigInt) typed in radix 2/8/10/16; arrow
 *                    keys and the spin buttons (held: repeat) step it; a
 *                    radix menu (AH.float); data-ah-value stays decimal
 *   range-selector   drag a marker or the bar between them; the markers
 *                    are ARIA sliders; input while dragging, change after
 *   repeat-button    click on press, then every interval ms after delay ms
 *                    while held; the browser's click on release is dropped
 *
 * Value-bearing roots keep data-ah-value and their hidden input in step
 * and fire "input" / "change" on the root. The server renders the whole
 * first state, so init only binds events.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  function docNS(el) {
    var ns = $.data(el, "ah-docns");
    if (!ns) {
      ns = NS + "e" + (++seq);
      $.data(el, "ah-docns", ns);
    }
    return ns;
  }

  function setValue($el, v) {
    $el.attr("data-ah-value", v);
    $el.children("input[type=hidden]").val(v);
  }

  // ------------------------------------------------------------------
  // masked-input
  // ------------------------------------------------------------------
  //
  // The mask is a list of positions {re, ch} (editable, ch null when
  // empty) or {lit} (a literal), parsed as aihtml_form_entry:parse_mask.

  var MASK_RE = { "9": "\\d", "0": "\\d", "#": "[\\d|+|-]", "A": "\\w", "a": "\\w",
                  "L": "[a-zA-Z]", "l": "[a-zA-Z]", "c": ".", "C": "." };

  function maskParse(mask) {
    var items = [], chars = Array.from(mask || ""), i, j;
    for (i = 0; i < chars.length; i++) {
      if (chars[i] === "[") {
        j = chars.indexOf("]", i);
        if (j < 0) { j = chars.length - 1; }
        items.push({ re: new RegExp("^(?:(" + chars.slice(i, j + 1).join("") + "))$", "i"), ch: null });
        i = j;
      } else if (MASK_RE[chars[i]]) {
        items.push({ re: new RegExp("^(?:" + MASK_RE[chars[i]] + ")$", "i"), ch: null });
      } else {
        items.push({ lit: chars[i] });
      }
    }
    return items;
  }

  // Fill the editable positions from text, as the server does.
  function maskFill(items, text) {
    var cs = Array.from(text == null ? "" : String(text)), k = 0;
    items.forEach(function (it) {
      if (it.lit != null) {
        if (cs[k] === it.lit) { k++; }
        return;
      }
      it.ch = null;
      while (k < cs.length && !it.re.test(cs[k])) { k++; }
      if (k < cs.length) { it.ch = cs[k++]; }
    });
    return items;
  }

  function maskState(el) { return $.data(el, "ah-mask"); }

  function maskDisplay(st) {
    return st.items.map(function (it) {
      return it.lit != null ? it.lit : (it.ch == null ? st.prompt : it.ch);
    }).join("");
  }

  function maskRaw(st) {
    return st.items.map(function (it) { return it.lit == null && it.ch != null ? it.ch : ""; }).join("");
  }

  function maskValue(st) {
    var raw = maskRaw(st);
    return st.literals ? (raw ? maskDisplay(st) : "") : raw;
  }

  function editable(st, i) { return i >= 0 && i < st.items.length && st.items[i].lit == null; }
  function nextEditable(st, i) { for (; i < st.items.length; i++) { if (editable(st, i)) { return i; } } return -1; }
  function prevEditable(st, i) { for (i--; i >= 0; i--) { if (editable(st, i)) { return i; } } return -1; }
  function firstEmpty(st) {
    for (var i = 0; i < st.items.length; i++) { if (editable(st, i) && st.items[i].ch == null) { return i; } }
    return -1;
  }

  // Positions are characters; the input's selection counts UTF-16 units.
  function unitsTo(st, pos) {
    var n = 0;
    for (var i = 0; i < pos && i < st.items.length; i++) {
      var it = st.items[i];
      n += (it.lit != null ? it.lit : (it.ch == null ? st.prompt : it.ch)).length;
    }
    return n;
  }
  function posOf(st, units) {
    var n = 0;
    for (var i = 0; i < st.items.length; i++) {
      if (n >= units) { return i; }
      var it = st.items[i];
      n += (it.lit != null ? it.lit : (it.ch == null ? st.prompt : it.ch)).length;
    }
    return st.items.length;
  }
  function selection(st) {
    var inp = st.input;
    return { start: posOf(st, inp.selectionStart), end: posOf(st, inp.selectionEnd) };
  }
  function setCursor(st, pos) {
    var u = unitsTo(st, Math.max(0, Math.min(pos, st.items.length)));
    try { st.input.setSelectionRange(u, u); } catch (e) { /* not focused */ }
  }

  function clearRange(st, a, b) {
    for (var i = a; i < b; i++) { if (editable(st, i)) { st.items[i].ch = null; } }
  }

  // What the field shows: the mask, or nothing for an empty field with a
  // floating label (the label stands there) while it has no focus.
  function maskText(st) {
    return st.$label.length && document.activeElement !== st.input && maskRaw(st) === ""
      ? "" : maskDisplay(st);
  }

  // Show the items; fire input when the value changed.
  function maskShow(el, $el, cursor, fire) {
    var st = maskState(el), old = $el.attr("data-ah-value"), v = maskValue(st);
    st.input.value = maskText(st);
    setValue($el, v);
    if (cursor != null) { setCursor(st, cursor); }
    if (st.$label.length && document.activeElement !== st.input) {
      st.$label.toggleClass("ah-masked-input-label-float", maskRaw(st) !== "");
    }
    if (fire && v !== old) { $el.trigger("input", [v]); }
  }

  // Type one character (sigil's do-insert-char!: into the next editable
  // position if it fits, else jump past the literal it names) or paste a
  // text (sigil's do-paste!: characters that do not fit are skipped).
  function maskInsert(el, $el, text) {
    var st = maskState(el), sel = selection(st), chars = Array.from(text), pos = sel.start, i, j;
    var cleared = sel.end > sel.start;
    if (cleared) { clearRange(st, sel.start, sel.end); }
    if (chars.length === 1) {
      i = nextEditable(st, pos);
      if (i >= 0 && st.items[i].re.test(chars[0])) {
        st.items[i].ch = chars[0];
        j = nextEditable(st, i + 1);
        maskShow(el, $el, j < 0 ? st.items.length : j, true);
        return;
      }
      for (j = pos; j < st.items.length; j++) {
        if (st.items[j].lit === chars[0]) {
          if (cleared) { maskShow(el, $el, j + 1, true); } else { setCursor(st, j + 1); }
          return;
        }
      }
      if (cleared) { maskShow(el, $el, sel.start, true); }
      return;
    }
    var k = 0;
    while (k < chars.length && pos < st.items.length) {
      if (!editable(st, pos)) { pos++; continue; }
      if (st.items[pos].re.test(chars[k])) { st.items[pos].ch = chars[k]; pos++; }
      k++;
    }
    i = firstEmpty(st);
    maskShow(el, $el, i < 0 ? st.items.length : i, true);
  }

  function maskDelete(el, $el, back) {
    var st = maskState(el), sel = selection(st), i;
    if (sel.end > sel.start) {
      clearRange(st, sel.start, sel.end);
      maskShow(el, $el, sel.start, true);
    } else if (back) {
      i = prevEditable(st, sel.start);
      if (i >= 0) { st.items[i].ch = null; maskShow(el, $el, i, true); }
    } else {
      i = nextEditable(st, sel.start);
      if (i >= 0) { st.items[i].ch = null; maskShow(el, $el, sel.start, true); }
    }
  }

  function maskBlocked(st) { return st.input.disabled || st.input.readOnly; }

  AH.define("masked-input", {
    init: function (el, $el) {
      var $in = $el.children("input.ah-masked-input");
      var st = {
        input: $in[0],
        $label: $el.children(".ah-masked-input-label"),
        prompt: el.getAttribute("data-ah-prompt") || "_",
        literals: el.hasAttribute("data-ah-literals"),
        items: maskParse(el.getAttribute("data-ah-mask")),
        focusValue: null
      };
      $.data(el, "ah-mask", st);
      // the server filled the mask: read the positions back
      Array.from($in.val()).forEach(function (c, i) {
        if (editable(st, i) && c !== st.prompt) { st.items[i].ch = c; }
      });

      $in.on("keydown" + NS, function (e) {
        var k = e.key || "";
        if (e.keyCode === 229) { return; }                       // IME: beforeinput
        if (e.ctrlKey || e.metaKey || e.altKey) {
          if ((k === "x" || k === "X") && !maskBlocked(st)) {
            var sel = selection(st);
            // after the browser copied the selection
            setTimeout(function () {
              if (st.input.value === maskDisplay(st) && sel.end > sel.start) {
                clearRange(st, sel.start, sel.end);
                maskShow(el, $el, sel.start, true);
              }
            }, 10);
          }
          return;
        }
        if (k === "Backspace" || k === "Delete") {
          e.preventDefault();
          if (!maskBlocked(st)) { maskDelete(el, $el, k === "Backspace"); }
        } else if (k.length > 0 && Array.from(k).length === 1) {
          e.preventDefault();
          if (!maskBlocked(st)) { maskInsert(el, $el, k); }
        }
      });
      $in.on("beforeinput" + NS, function (je) {
        var e = je.originalEvent;
        if (!e) { return; }
        switch (e.inputType) {
          case "insertText": case "insertReplacementText": case "insertCompositionText":
            e.preventDefault();
            if (e.data && !maskBlocked(st)) { maskInsert(el, $el, e.data); }
            break;
          case "insertFromPaste": case "insertFromDrop":
            e.preventDefault();                              // the paste handler
            break;
          case "deleteContentBackward": case "deleteByCut":
            e.preventDefault();
            if (!maskBlocked(st)) { maskDelete(el, $el, true); }
            break;
          case "deleteContentForward":
            e.preventDefault();
            if (!maskBlocked(st)) { maskDelete(el, $el, false); }
            break;
        }
      });
      $in.on("paste" + NS, function (je) {
        var e = je.originalEvent, text = e && e.clipboardData && e.clipboardData.getData("text/plain");
        je.preventDefault();
        if (text && !maskBlocked(st)) { maskInsert(el, $el, text); }
      });
      // safety net: whatever got through, show the items again
      $in.on("input" + NS, function () {
        var s = selection(st);
        if (st.input.value !== maskText(st)) {
          st.input.value = maskText(st);
          setCursor(st, s.start);
        }
      });
      // sigil's snap-to-editable: a click lands on an editable position
      $in.on("mouseup" + NS, function () {
        if (maskBlocked(st)) { return; }
        var s = selection(st), n = s.start, e = firstEmpty(st);
        if (s.start !== s.end) { return; }
        // not past the first empty position
        if (e >= 0 && n > e) { setCursor(st, e); return; }
        if (editable(st, n)) { return; }
        n = n >= st.items.length ? prevEditable(st, st.items.length) : nextEditable(st, n);
        if (n < 0) { n = prevEditable(st, s.start); }
        setCursor(st, n < 0 ? 0 : n);
      });
      $in.on("focus" + NS, function () {
        $el.addClass("ah-masked-input-focused");
        st.$label.addClass("ah-masked-input-label-float");
        st.input.value = maskDisplay(st);
        st.focusValue = $el.attr("data-ah-value");
        setTimeout(function () {
          if (document.activeElement !== st.input) { return; }
          var e = firstEmpty(st);
          if (e >= 0) { setCursor(st, e); }
        }, 0);
      });
      $in.on("blur" + NS, function () {
        $el.removeClass("ah-masked-input-focused");
        st.$label.toggleClass("ah-masked-input-label-float", maskRaw(st) !== "");
        st.input.value = maskText(st);
        var v = $el.attr("data-ah-value");
        if (st.focusValue !== null && v !== st.focusValue) { $el.trigger("change", [v]); }
        st.focusValue = null;
      });
    },
    methods: {
      setValue: function (el, $el, v) {
        var st = maskState(el);
        maskFill(st.items, v);
        maskShow(el, $el, null, false);
        if (st.focusValue !== null) { st.focusValue = $el.attr("data-ah-value"); }
      },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); },
      getMaskedValue: function (el) { return maskDisplay(maskState(el)); },
      isComplete: function (el) {
        return maskState(el).items.every(function (it) { return it.lit != null || it.ch != null; });
      },
      clear: function (el, $el) {
        var st = maskState(el), old = $el.attr("data-ah-value");
        maskFill(st.items, "");
        maskShow(el, $el, null, false);
        if (old !== "") { $el.trigger("change", [""]); }
      },
      setMask: function (el, $el, mask) {
        var st = maskState(el), raw = maskRaw(st);
        st.items = maskFill(maskParse(mask), raw);
        $el.attr("data-ah-mask", mask);
        maskShow(el, $el, null, false);
      },
      focus: function (el) { maskState(el).input.focus(); }
    }
  });

  // ------------------------------------------------------------------
  // formatted-input
  // ------------------------------------------------------------------

  var RADIX_PREFIX = { 2: "0b", 8: "0o", 10: "", 16: "0x" };
  var RADIX_CHARS = { 2: /^[01]$/, 8: /^[0-7]$/, 10: /^[0-9]$/, 16: /^[0-9a-f]$/i };

  // Text in a radix -> BigInt; empty or invalid -> 0n.
  function bigParse(s, radix) {
    s = String(s == null ? "" : s).trim();
    if (!s) { return BigInt(0); }
    var neg = s.charAt(0) === "-";
    if (neg) { s = s.slice(1); }
    if (!s) { s = "0"; }
    try {
      var b = BigInt(RADIX_PREFIX[radix] + s);
      return neg ? -b : b;
    } catch (e) {
      return BigInt(0);
    }
  }

  function bigText(b, radix, upper, expo) {
    var s = b.toString(radix);
    if (upper) { s = s.toUpperCase(); }
    if (expo && radix === 10) {
      var neg = s.charAt(0) === "-", abs = neg ? s.slice(1) : s;
      if (abs.length > 1) { s = (neg ? "-" : "") + abs.charAt(0) + "." + abs.slice(1) + "e+" + (abs.length - 1); }
    }
    return s;
  }

  function fmtState(el) { return $.data(el, "ah-fmt"); }

  function fmtClamp(st, b) {
    if (st.min !== null && b < st.min) { return st.min; }
    if (st.max !== null && b > st.max) { return st.max; }
    return b;
  }

  // Show the value in the radix (plain while the field has focus).
  function fmtShow(st) {
    var editing = document.activeElement === st.$input[0];
    var t = bigText(st.value, st.radix, st.upper, st.expo && !editing);
    st.$input.val(t).attr({ "aria-valuenow": st.value.toString(), "aria-valuetext": t });
  }

  // Set the value (clamped); fire change when asked and it changed.
  function fmtSet(el, $el, b, fire) {
    var st = fmtState(el), old = $el.attr("data-ah-value");
    st.value = fmtClamp(st, b);
    fmtShow(st);
    var v = st.value.toString();
    setValue($el, v);
    if (fire && v !== old) { $el.trigger("change", [v]); }
  }

  function fmtStep(el, $el, dir) {
    var st = fmtState(el);
    if (st.$input[0].disabled) { return; }
    fmtSet(el, $el, st.value + st.step * BigInt(dir), true);
    if (st.editing) { st.focusValue = $el.attr("data-ah-value"); }
  }

  function fmtItems(st) { return st.$popup.children(".ah-fmt-popup-item"); }

  function fmtActive(st, $item) {
    fmtItems(st).removeClass("ah-fmt-popup-item-hover");
    $item.addClass("ah-fmt-popup-item-hover");
    st.$input.attr("aria-activedescendant", $item.attr("id"));
  }

  function fmtOpen(el, $el) {
    var st = fmtState(el);
    if (st.open || !st.$popup.length || st.$input[0].disabled) { return; }
    st.open = true;
    st.$popup.addClass("ah-fmt-popup-open");
    st.float = AH.float(st.$popup[0], $el.children(".ah-fmt-input-row")[0], { placement: "bottom", align: "end" });
    st.$btn.attr("aria-expanded", "true");
    fmtActive(st, fmtItems(st).filter(".ah-fmt-popup-item-active"));
    $(document).on("mousedown" + st.ns + "p", function (e) {
      if (!$.contains(el, e.target) && e.target !== el) { fmtClose(el, $el); }
    });
    $el.trigger("ah:open");
  }

  function fmtClose(el, $el) {
    var st = fmtState(el);
    if (!st.open) { return; }
    st.open = false;
    st.$popup.removeClass("ah-fmt-popup-open");
    if (st.float) { st.float.stop(); st.float = null; }
    st.$btn.attr("aria-expanded", "false");
    st.$input.removeAttr("aria-activedescendant");
    fmtItems(st).removeClass("ah-fmt-popup-item-hover");
    $(document).off("mousedown" + st.ns + "p");
    $el.trigger("ah:close");
  }

  function fmtRadix(el, $el, radix) {
    var st = fmtState(el), old = st.radix;
    radix = parseInt(radix, 10);
    if (!RADIX_CHARS[radix]) { return; }
    fmtClose(el, $el);
    if (radix === old) { return; }
    st.radix = radix;
    $el.attr("data-ah-radix", String(radix));
    fmtItems(st).each(function () {
      var on = this.getAttribute("data-radix") === String(radix);
      $(this).toggleClass("ah-fmt-popup-item-active", on).attr("aria-selected", String(on));
    });
    fmtShow(st);
    $el.trigger("ah:radix-change", [radix, old]);
  }

  function stopRepeat(st) {
    if (st.timer) { clearTimeout(st.timer); st.timer = null; }
    if (st.iv) { clearInterval(st.iv); st.iv = null; }
  }

  function startRepeat(st, f, delay, interval) {
    stopRepeat(st);
    f();
    st.timer = setTimeout(function () {
      st.timer = null;
      st.iv = setInterval(f, interval);
    }, delay);
  }

  AH.define("formatted-input", {
    init: function (el, $el) {
      var min = el.getAttribute("data-ah-min"), max = el.getAttribute("data-ah-max");
      var st = {
        ns: docNS(el),
        $input: $el.find("input.ah-fmt-input"),
        $popup: $el.children(".ah-fmt-popup"),
        $btn: $el.find(".ah-fmt-dropdown-btn"),
        radix: parseInt(el.getAttribute("data-ah-radix"), 10) || 10,
        min: min === null ? null : BigInt(min),
        max: max === null ? null : BigInt(max),
        step: BigInt(el.getAttribute("data-ah-step") || "1"),
        upper: el.hasAttribute("data-ah-upper"),
        expo: el.getAttribute("data-ah-notation") === "exponential",
        value: BigInt(el.getAttribute("data-ah-value") || "0"),
        open: false, editing: false, focusValue: null
      };
      $.data(el, "ah-fmt", st);
      var $in = st.$input;

      $in.on("keydown" + NS, function (e) {
        var k = e.key || "", $items, idx;
        if (st.open && (k === "ArrowDown" || k === "ArrowUp")) {
          e.preventDefault();
          $items = fmtItems(st);
          idx = $items.index($items.filter(".ah-fmt-popup-item-hover"));
          idx = (idx + (k === "ArrowDown" ? 1 : -1) + $items.length) % $items.length;
          fmtActive(st, $items.eq(idx));
          return;
        }
        if (st.open && (k === "Enter" || k === " ")) {
          e.preventDefault();
          fmtRadix(el, $el, fmtItems(st).filter(".ah-fmt-popup-item-hover").attr("data-radix"));
          return;
        }
        if (k === "Escape") {
          if (st.open) { e.preventDefault(); fmtClose(el, $el); }
          return;
        }
        if (e.altKey && (k === "ArrowDown" || k === "ArrowUp")) {
          e.preventDefault();
          if (k === "ArrowDown") { fmtOpen(el, $el); } else { fmtClose(el, $el); }
          return;
        }
        if (e.ctrlKey || e.metaKey || e.altKey) { return; }
        if (k === "ArrowUp" || k === "ArrowDown") {
          e.preventDefault();
          fmtSet(el, $el, bigParse($in.val(), st.radix), false);   // what was typed so far
          fmtStep(el, $el, k === "ArrowUp" ? 1 : -1);
          return;
        }
        if (k === "-") {
          if (this.selectionStart !== 0 || $in.val().charAt(0) === "-" && this.selectionEnd === 0) {
            e.preventDefault();
          }
          return;
        }
        if (k.length === 1 && !RADIX_CHARS[st.radix].test(k)) { e.preventDefault(); }
      });
      $in.on("input" + NS, function () {
        var b = bigParse($in.val(), st.radix), v = b.toString();
        st.value = b;
        if (v !== $el.attr("data-ah-value")) {
          setValue($el, v);
          $el.trigger("input", [v]);
        }
      });
      $in.on("focus" + NS, function () {
        $el.addClass("ah-fmt-input-focused");
        st.editing = true;
        st.focusValue = $el.attr("data-ah-value");
        if (st.expo) { fmtShow(st); }
      });
      $in.on("blur" + NS, function () {
        $el.removeClass("ah-fmt-input-focused");
        st.editing = false;
        var before = st.focusValue;
        st.focusValue = null;
        fmtSet(el, $el, bigParse($in.val(), st.radix), false);
        var v = $el.attr("data-ah-value");
        if (before !== null && v !== before) { $el.trigger("change", [v]); }
      });

      $el.on("mousedown" + NS, ".ah-fmt-spin-up, .ah-fmt-spin-down", function (e) {
        if (e.button !== 0 || $in[0].disabled) { return; }
        e.preventDefault();
        var dir = $(this).hasClass("ah-fmt-spin-up") ? 1 : -1;
        if (st.editing) { fmtSet(el, $el, bigParse($in.val(), st.radix), false); }
        startRepeat(st, function () { fmtStep(el, $el, dir); }, 400, 75);
        $(document).on("mouseup" + st.ns + "s", function () {
          stopRepeat(st);
          $(document).off("mouseup" + st.ns + "s");
        });
      });
      $el.on("mousedown" + NS, ".ah-fmt-dropdown-btn", function (e) {
        e.preventDefault();
        if (st.open) { fmtClose(el, $el); } else { fmtOpen(el, $el); }
      });
      st.$popup.on("mousedown" + NS, ".ah-fmt-popup-item", function (e) {
        e.preventDefault();
        fmtRadix(el, $el, this.getAttribute("data-radix"));
      });
    },
    destroy: function (el) {
      var st = fmtState(el);
      if (!st) { return; }
      stopRepeat(st);
      if (st.float) { st.float.stop(); st.float = null; }
      st.$popup.off(NS);
      $(document).off(st.ns + "p").off(st.ns + "s");
    },
    methods: {
      setValue: function (el, $el, v) {
        var st = fmtState(el);
        fmtSet(el, $el, bigParse(String(v), 10), false);
        if (st.editing) { st.focusValue = $el.attr("data-ah-value"); }
      },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); },
      setRadix: function (el, $el, radix) { fmtRadix(el, $el, radix); },
      getRadix: function (el) { return fmtState(el).radix; },
      open: function (el, $el) { fmtOpen(el, $el); },
      close: function (el, $el) { fmtClose(el, $el); }
    }
  });

  // ------------------------------------------------------------------
  // range-selector
  // ------------------------------------------------------------------

  var MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

  function erlRound(v) { return v < 0 ? -Math.round(-v) : Math.round(v); }

  // The same formats as aihtml_form_entry:format/2.
  function rsFormat(v, f) {
    var d, h, s;
    switch (f.f) {
      case "fixed": s = v.toFixed(f.n); break;
      case "currency":
        d = erlRound(v);
        s = "$" + (d < 0 ? "-" : "") + String(Math.abs(d)).replace(/\B(?=(\d{3})+(?!\d))/g, ",");
        break;
      case "date":
        d = new Date(Math.floor(v));
        s = (d.getUTCMonth() + 1) + "/" + d.getUTCDate() + "/" + d.getUTCFullYear();
        break;
      case "month": s = MONTHS[new Date(Math.floor(v)).getUTCMonth()]; break;
      case "time":
        d = new Date(Math.floor(v));
        h = d.getUTCHours();
        s = ((h % 12) || 12) + ":" + (d.getUTCMinutes() < 10 ? "0" : "") + d.getUTCMinutes() +
          (h >= 12 ? " PM" : " AM");
        break;
      default:
        s = Math.abs(v - erlRound(v)) < 0.001 ? String(erlRound(v)) : v.toFixed(2);
    }
    return (f.p || "") + s + (f.s || "");
  }

  function tidy(v) { return parseFloat(v.toFixed(9)); }

  function rsState(el) { return $.data(el, "ah-rs"); }

  function rsSnap(st, v) {
    v = st.min + Math.round((v - st.min) / st.step) * st.step;
    return tidy(Math.max(st.min, Math.min(st.max, v)));
  }

  function pct(st, v) { return tidy((v - st.min) / (st.max - st.min) * 100); }

  function rsLayout(el, $el) {
    var st = rsState(el), a = pct(st, st.lo), b = pct(st, st.hi);
    st.$slider.css({ left: a + "%", width: tidy(b - a) + "%" });
    st.$shutL.css({ width: a + "%" });
    st.$shutR.css({ left: b + "%", width: tidy(100 - b) + "%" });
    [[st.$mL, st.lo, a], [st.$mR, st.hi, b]].forEach(function (m) {
      var t = rsFormat(m[1], st.format);
      m[0].css("left", m[2] + "%").attr({ "aria-valuenow": String(m[1]), "aria-valuetext": t });
      m[0].children(".ah-range-selector-marker-value").text(t);
    });
    setValue($el, st.lo + "," + st.hi);
  }

  // Set lo / hi (already bounded); input when it changed, change if asked.
  function rsSet(el, $el, lo, hi, change) {
    var st = rsState(el), old = $el.attr("data-ah-value");
    st.lo = lo; st.hi = hi;
    rsLayout(el, $el);
    var v = $el.attr("data-ah-value");
    if (v !== old) { $el.trigger("input", [v]); }
    if (change && v !== st.committed) {
      st.committed = v;
      $el.trigger("change", [v]);
    }
  }

  // Move one end to v, kept min_span away from the other.
  function rsMoveEnd(el, $el, left, v, change) {
    var st = rsState(el);
    v = rsSnap(st, v);
    if (left) {
      rsSet(el, $el, Math.max(st.min, Math.min(v, tidy(st.hi - st.minSpan))), st.hi, change);
    } else {
      rsSet(el, $el, st.lo, Math.min(st.max, Math.max(v, tidy(st.lo + st.minSpan))), change);
    }
  }

  function pointX(e) {
    var o = e.originalEvent, t = o && (o.touches && o.touches[0] || o.changedTouches && o.changedTouches[0]);
    return (t || e).clientX;
  }

  function rsValueAt(st, x) {
    var r = st.$track[0].getBoundingClientRect();
    var p = r.width > 0 ? Math.max(0, Math.min(1, (x - r.left) / r.width)) : 0;
    return st.min + p * (st.max - st.min);
  }

  function rsDisabled($el) { return $el.hasClass("ah-range-selector-disabled"); }

  function rsDrag(el, $el, e, move) {
    var st = rsState(el);
    if (e.type === "mousedown") {
      if (e.button !== 0) { return; }
      e.preventDefault();
    }
    $(document).off(st.ns);
    $(document).on("mousemove" + st.ns + " touchmove" + st.ns, function (me) {
      if (me.type === "mousemove") { me.preventDefault(); }
      move(pointX(me));
    }).on("mouseup" + st.ns + " touchend" + st.ns + " touchcancel" + st.ns, function () {
      $(document).off(st.ns);
      $el.removeClass("ah-range-selector-dragging");
      rsSet(el, $el, st.lo, st.hi, true);
    });
    $el.addClass("ah-range-selector-dragging");
  }

  AH.define("range-selector", {
    init: function (el, $el) {
      var num = function (a) { return parseFloat(el.getAttribute(a)); };
      var v = (el.getAttribute("data-ah-value") || "").split(",");
      var st = {
        ns: docNS(el),
        min: num("data-ah-min"), max: num("data-ah-max"), step: num("data-ah-step") || 1,
        page: num("data-ah-page") || 10, minSpan: num("data-ah-min-span") || 0,
        format: JSON.parse(el.getAttribute("data-ah-format") || "{}"),
        lo: parseFloat(v[0]), hi: parseFloat(v[1]),
        committed: el.getAttribute("data-ah-value"),
        $track: $el.children(".ah-range-selector-track")
      };
      st.$slider = st.$track.children(".ah-range-selector-slider");
      st.$shutL = st.$track.children(".ah-range-selector-shutter-left");
      st.$shutR = st.$track.children(".ah-range-selector-shutter-right");
      st.$mL = st.$track.children(".ah-range-selector-marker-left");
      st.$mR = st.$track.children(".ah-range-selector-marker-right");
      $.data(el, "ah-rs", st);

      st.$track.on("mousedown" + NS + " touchstart" + NS, ".ah-range-selector-marker", function (e) {
        if (rsDisabled($el)) { return; }
        var left = $(this).hasClass("ah-range-selector-marker-left");
        this.focus();
        rsDrag(el, $el, e, function (x) { rsMoveEnd(el, $el, left, rsValueAt(st, x), false); });
      });
      st.$slider.on("mousedown" + NS + " touchstart" + NS, function (e) {
        if (rsDisabled($el)) { return; }
        var span = tidy(st.hi - st.lo), grab = rsValueAt(st, pointX(e)) - st.lo;
        rsDrag(el, $el, e, function (x) {
          var lo = rsSnap(st, Math.max(st.min, Math.min(st.max - span, rsValueAt(st, x) - grab)));
          rsSet(el, $el, lo, Math.min(st.max, tidy(lo + span)), false);
        });
      });
      st.$track.on("keydown" + NS, ".ah-range-selector-marker", function (e) {
        if (rsDisabled($el)) { return; }
        var left = $(this).hasClass("ah-range-selector-marker-left"), cur = left ? st.lo : st.hi, to;
        switch (e.key) {
          case "ArrowRight": case "ArrowUp": to = cur + st.step; break;
          case "ArrowLeft": case "ArrowDown": to = cur - st.step; break;
          case "PageUp": to = cur + st.page; break;
          case "PageDown": to = cur - st.page; break;
          case "Home": to = st.min; break;
          case "End": to = st.max; break;
          default: return;
        }
        e.preventDefault();
        rsMoveEnd(el, $el, left, to, true);
      });
    },
    destroy: function (el) {
      var st = rsState(el);
      if (st) { $(document).off(st.ns); }
    },
    methods: {
      setValue: function (el, $el, v) {
        var st = rsState(el);
        if (typeof v === "string") { v = v.split(","); }
        var lo = rsSnap(st, parseFloat(v[0])), hi = rsSnap(st, parseFloat(v[1]));
        if (lo > hi) { var t = lo; lo = hi; hi = t; }
        st.lo = lo; st.hi = hi;
        rsLayout(el, $el);
        st.committed = $el.attr("data-ah-value");
      },
      getValue: function (el) { var st = rsState(el); return [st.lo, st.hi]; }
    }
  });

  // ------------------------------------------------------------------
  // repeat-button
  // ------------------------------------------------------------------

  function rbState(el) { return $.data(el, "ah-rb"); }

  function rbRelease(el) {
    var st = rbState(el);
    if (!st || !st.active) { return; }
    st.active = false;
    stopRepeat(st);
    $(el).removeClass("ah-btn-pressed");
    // the click the browser sends for this release is not another repetition
    st.swallow = true;
    setTimeout(function () { st.swallow = false; }, 0);
  }

  AH.define("repeat-button", {
    init: function (el, $el) {
      var st = { active: false, swallow: false, timer: null, iv: null };
      $.data(el, "ah-rb", st);
      var delay = parseInt(el.getAttribute("data-ah-delay"), 10);
      var interval = parseInt(el.getAttribute("data-ah-interval"), 10) || 50;
      if (isNaN(delay)) { delay = 300; }
      function press() {
        if (el.disabled || st.active) { return; }
        st.active = true;
        $el.addClass("ah-btn-pressed");
        startRepeat(st, function () {
          if (el.disabled) { rbRelease(el); return; }
          $el.trigger("click");
        }, delay, interval);
      }
      $el.on("mousedown" + NS, function (e) { if (e.button === 0) { press(); } });
      $el.on("touchstart" + NS, function (e) {
        e.preventDefault();                   // no emulated mouse events, no click
        press();
      });
      $el.on("mouseup" + NS + " mouseleave" + NS + " touchend" + NS + " touchcancel" + NS +
             " blur" + NS, function () { rbRelease(el); });
      $el.on("keydown" + NS, function (e) {
        if (e.key !== "Enter" && e.key !== " ") { return; }
        e.preventDefault();
        press();                              // auto-repeated keydowns are ignored
      });
      $el.on("keyup" + NS, function (e) {
        if (e.key === "Enter" || e.key === " ") { e.preventDefault(); rbRelease(el); }
      });
      $el.on("click" + NS, function (e) {
        if (!e.isTrigger && (st.swallow || st.active)) {
          e.preventDefault();
          e.stopImmediatePropagation();
        }
      });
    },
    destroy: function (el) {
      var st = rbState(el);
      if (st) { st.active = false; stopRepeat(st); }
    },
    methods: {
      stop: function (el) { rbRelease(el); }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/form_lists.js ---- */
/* Behaviours of the form_lists components (designs/04-components.md).
 * Ported from sigil: form/cascader, form/listbox and form/transfer. All
 * rows, columns and lists are rendered on the server
 * (aihtml_form_lists); the behaviours show, hide, mark and move them. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  function ensureId(el, prefix) {
    if (!el.id) { el.id = prefix + (++seq); }
    return el.id;
  }

  // A mousedown outside the component.
  function outside(el, e) {
    return e.target.isConnected !== false && !$.contains(el, e.target) && e.target !== el;
  }

  // Value-bearing contract: data-ah-value + hidden input, then `change`.
  function publish(el, $el, value, fire) {
    var old = el.getAttribute("data-ah-value") || "";
    el.setAttribute("data-ah-value", value);
    $el.children("input[type=hidden]").val(value);
    if (fire && old !== value) { $el.trigger("change"); }
  }

  function split(v) {
    if (v == null || v === "") { return []; }
    return Array.isArray(v) ? v.map(String) : String(v).split(",");
  }

  // The first case-insensitive occurrence of the query in <b>, as DOM
  // nodes (combobox's highlight).
  function highlight(node, text, q) {
    var i = q ? text.toLowerCase().indexOf(q.toLowerCase()) : -1;
    node.textContent = i < 0 ? text : text.slice(0, i);
    if (i < 0) { return; }
    var b = document.createElement("b");
    b.textContent = text.slice(i, i + q.length);
    node.appendChild(b);
    node.appendChild(document.createTextNode(text.slice(i + q.length)));
  }

  function shown(li) { return li.style.display !== "none"; }
  function enabled(li) { return li.getAttribute("aria-disabled") !== "true"; }

  // Keep a row visible in its scrolling container.
  function scrollInto(box, item) {
    if (!box || !item) { return; }
    var top = item.getBoundingClientRect().top - box.getBoundingClientRect().top - box.clientTop +
      box.scrollTop;
    var bottom = top + item.offsetHeight;
    if (top < box.scrollTop) { box.scrollTop = top; }
    if (bottom > box.scrollTop + box.clientHeight) { box.scrollTop = bottom - box.clientHeight; }
  }

  // ==================================================================
  // cascader
  // ==================================================================

  function csState(el) { return $.data(el, "ah-cs"); }

  function csColumns(st) { return st.$menus.children(".ah-cascader-menu-column"); }

  // The column holding the children of `path` (an array); the last one
  // wins when a lazy level was loaded twice.
  function csColumn(st, path) {
    var key = path.join(",");
    return csColumns(st).filter(function () {
      return this.getAttribute("data-parent") === key;
    }).last();
  }

  function csItem($col, v) {
    return $col.find("li[data-value]").filter(function () {
      return this.getAttribute("data-value") === v;
    })[0] || null;
  }

  function csLabel(li) {
    return $(li).children(".ah-cascader-menu-item-label").text();
  }

  // The labels along a path; values without a row show as themselves.
  function csLabels(st, path) {
    var out = [];
    for (var i = 0; i < path.length; i++) {
      var li = csItem(csColumn(st, path.slice(0, i)), path[i]);
      out.push(li ? csLabel(li) : path[i]);
    }
    return out;
  }

  function csPathOf(li) {
    var $col = $(li).closest(".ah-cascader-menu-column");
    return split($col.attr("data-parent")).concat([li.getAttribute("data-value")]);
  }

  function csBranch(li) { return $(li).hasClass("has-children"); }

  // Show the columns of the open path and mark its rows active.
  function csShow(el) {
    var st = csState(el);
    csColumns(st).attr("hidden", "hidden");
    st.$menus.find("li.active").removeClass("active").attr("aria-selected", "false");
    var $col = csColumn(st, []);
    for (var i = 0; $col.length; i++) {
      $col.removeAttr("hidden");
      var li = i < st.open.length ? csItem($col, st.open[i]) : null;
      if (!li) { break; }
      $(li).addClass("active").attr("aria-selected", "true");
      if (!csBranch(li)) { break; }
      $col = csColumn(st, st.open.slice(0, i + 1));
    }
    csPosition(el);
  }

  function csCursor(el, li) {
    var st = csState(el);
    st.$menus.find(".ah-cascader-menu-item-focused").removeClass("ah-cascader-menu-item-focused");
    st.cursor = li || null;
    if (!li) { st.$input.removeAttr("aria-activedescendant"); return; }
    ensureId(li, el.id + "-o");
    $(li).addClass("ah-cascader-menu-item-focused");
    st.$input.attr("aria-activedescendant", li.id);
    scrollInto($(li).closest(".ah-cascader-menu")[0], li);
  }

  function csPosition(el) {
    var st = csState(el);
    if (!st.isOpen) { return; }
    if (st.float) { st.float.update(); } else { st.float = AH.float(st.$popup[0], el); }
  }

  function csBlocked($el) { return $el.hasClass("ah-cascader-disabled"); }

  function csOpen(el, $el) {
    var st = csState(el);
    if (st.isOpen || csBlocked($el)) { return; }
    st.isOpen = true;
    st.open = st.value.slice();
    st.$popup.addClass("ah-cascader-popup-open");
    csShow(el);
    var last = st.value.length ? csItem(csColumn(st, st.value.slice(0, -1)), st.value[st.value.length - 1]) : null;
    csCursor(el, last || csRows(csColumn(st, []))[0]);
    $el.addClass("ah-cascader-open");
    st.$input.attr("aria-expanded", "true");
    $(document).on("mousedown" + st.ns, function (e) {
      if (outside(el, e)) { csClose(el, $el); }
    });
    $el.trigger("ah:open");
  }

  function csClose(el, $el) {
    var st = csState(el);
    if (!st.isOpen) { return; }
    st.isOpen = false;
    st.$popup.removeClass("ah-cascader-popup-open");
    if (st.float) { st.float.stop(); st.float = null; }
    $el.removeClass("ah-cascader-open");
    st.$input.attr("aria-expanded", "false");
    csCursor(el, null);
    st.$menus.children(".ah-cascader-loading").remove();
    if (st.query) { csQuery(el, ""); }
    st.$input.val(st.display);
    $(document).off(st.ns);
    $el.trigger("ah:close");
  }

  function csSet(el, $el, path, fire) {
    var st = csState(el);
    st.value = path.slice();
    st.display = csLabels(st, path).join(st.sep);
    if (!st.query) { st.$input.val(st.display); }
    st.$clear.prop("hidden", !path.length);
    publish(el, $el, path.join(","), fire);
  }

  function csRows($col) {
    return $col.find("li[data-value]").filter(function () { return enabled(this); }).get();
  }

  // Open a branch: its column (loaded or not), or a leaf: pick it.
  function csChoose(el, $el, li, kbd) {
    var st = csState(el);
    if (!li || !enabled(li)) { return; }
    var path = csPathOf(li);
    if (!csBranch(li)) {
      csSet(el, $el, path, true);
      csClose(el, $el);
      return;
    }
    st.open = path;
    if (st.cos) { csSet(el, $el, path, true); }
    var $col = csColumn(st, path);
    st.$menus.children(".ah-cascader-loading").remove();
    if ($col.length) {
      csShow(el);
      csCursor(el, kbd ? csRows($col)[0] : li);
    } else if (li.hasAttribute("data-lazy") && st.$loader.length) {
      csShow(el);
      csCursor(el, li);
      st.pending = { path: path.join(","), kbd: kbd };
      st.$menus.append($('<div class="ah-cascader-loading"></div>').text("Loading…"));
      csPosition(el);
      st.$loader.attr("data-ah-value", path.join(",")).trigger("ah:load");
    } else {
      csLeaf(li);
      csChoose(el, $el, li, kbd);
    }
  }

  function csLeaf(li) {
    $(li).removeClass("has-children").removeAttr("data-lazy aria-haspopup")
      .children(".ah-cascader-menu-item-arrow").remove();
  }

  function csMove(el, dir, edge) {
    var st = csState(el);
    var $col = st.cursor ? $(st.cursor).closest(".ah-cascader-menu-column") : csColumn(st, []);
    var rows = csRows($col);
    if (!rows.length) { return; }
    var i = rows.indexOf(st.cursor);
    if (edge) { i = dir > 0 ? rows.length - 1 : 0; } else if (i < 0) { i = 0; } else {
      i = (i + dir + rows.length) % rows.length;
    }
    // moving within a column closes the columns to its right
    var level = parseInt($col.attr("data-level"), 10) || 0;
    if (st.open.length > level) { st.open = st.open.slice(0, level); csShow(el); }
    csCursor(el, rows[i]);
  }

  // Search (filterable): the server-rendered path list, filtered here.
  function csQuery(el, q) {
    var st = csState(el);
    st.query = q;
    st.$popup.children(".ah-cascader-empty").remove();
    if (!q) {
      st.$search.attr("hidden", "hidden");
      st.$menus.removeAttr("hidden");
      st.searchActive = -1;
      csPosition(el);
      return;
    }
    st.$menus.attr("hidden", "hidden");
    st.$search.removeAttr("hidden");
    var lower = q.toLowerCase(), any = false;
    st.$search.children("li").each(function () {
      var label = this.getAttribute("data-label");
      var hit = label.toLowerCase().indexOf(lower) >= 0;
      this.style.display = hit ? "" : "none";
      if (hit) { any = true; highlight(this.firstChild, label, q); }
    });
    if (!any) {
      st.$search.attr("hidden", "hidden");
      st.$popup.append($('<div class="ah-cascader-empty"></div>').text(st.empty));
    }
    csSearchActive(el, -1);
    csPosition(el);
  }

  function csSearchRows(st) {
    return st.$search.children("li").filter(function () { return shown(this) && enabled(this); }).get();
  }

  function csSearchActive(el, i) {
    var st = csState(el);
    var rows = csSearchRows(st);
    st.$search.children(".active").removeClass("active").attr("aria-selected", "false");
    st.searchActive = rows[i] ? i : -1;
    if (!rows[i]) { st.$input.removeAttr("aria-activedescendant"); return; }
    ensureId(rows[i], el.id + "-s");
    $(rows[i]).addClass("active").attr("aria-selected", "true");
    st.$input.attr("aria-activedescendant", rows[i].id);
    scrollInto(st.$search[0], rows[i]);
  }

  function csSearchPick(el, $el, li) {
    if (!li || !enabled(li)) { return; }
    csState(el).query = "";
    csSet(el, $el, split(li.getAttribute("data-path")), true);
    csClose(el, $el);
  }

  function csKey(el, $el, e) {
    var st = csState(el);
    if (csBlocked($el)) { return; }
    var k = e.key;
    if (!st.isOpen) {
      if (k === "ArrowDown" || k === "ArrowUp" || k === "Enter" || (k === " " && !st.filterable)) {
        e.preventDefault();
        csOpen(el, $el);
      }
      return;
    }
    if (st.query) {
      var rows = csSearchRows(st);
      switch (k) {
        case "ArrowDown": e.preventDefault(); csSearchActive(el, (st.searchActive + 1) % Math.max(rows.length, 1)); return;
        case "ArrowUp": e.preventDefault(); csSearchActive(el, st.searchActive <= 0 ? rows.length - 1 : st.searchActive - 1); return;
        case "Enter": e.preventDefault(); csSearchPick(el, $el, rows[st.searchActive] || (rows.length === 1 ? rows[0] : null)); return;
        case "Escape": e.preventDefault(); st.$input.val(""); csQuery(el, ""); return;
        case "Tab": csClose(el, $el); return;
        default: return;
      }
    }
    switch (k) {
      case "ArrowDown": e.preventDefault(); csMove(el, 1); break;
      case "ArrowUp": e.preventDefault(); csMove(el, -1); break;
      case "Home": if (!st.filterable) { e.preventDefault(); csMove(el, -1, true); } break;
      case "End": if (!st.filterable) { e.preventDefault(); csMove(el, 1, true); } break;
      case "ArrowRight":
        if (st.cursor && csBranch(st.cursor)) { e.preventDefault(); csChoose(el, $el, st.cursor, true); }
        break;
      case "ArrowLeft":
        var $col = st.cursor ? $(st.cursor).closest(".ah-cascader-menu-column") : $();
        var level = parseInt($col.attr("data-level"), 10) || 0;
        if (level > 0) {
          e.preventDefault();
          var parent = split($col.attr("data-parent"));
          st.open = parent.slice(0, -1);
          csShow(el);
          csCursor(el, csItem(csColumn(st, parent.slice(0, -1)), parent[parent.length - 1]));
        }
        break;
      case " ":
        if (st.filterable) { break; }
        e.preventDefault(); csChoose(el, $el, st.cursor, true); break;
      case "Enter": e.preventDefault(); csChoose(el, $el, st.cursor, true); break;
      case "Escape": e.preventDefault(); csClose(el, $el); break;
      case "Tab": csClose(el, $el); break;
      default: break;
    }
  }

  AH.define("cascader", {
    init: function (el, $el) {
      ensureId(el, "ah-cs");
      var $popup = $el.children(".ah-cascader-popup");
      var st = {
        ns: ".ahcs" + (++seq),
        $input: $el.find("input.ah-cascader-input"),
        $clear: $el.find(".ah-cascader-clear"),
        $popup: $popup,
        $menus: $popup.children(".ah-cascader-menus"),
        $search: $popup.children(".ah-cascader-search-panel"),
        $loader: $el.children(".ah-cascader-loader"),
        sep: el.getAttribute("data-ah-separator") || " / ",
        empty: el.getAttribute("data-ah-empty") || "No results found",
        cos: el.hasAttribute("data-ah-change-on-select"),
        filterable: $el.hasClass("ah-cascader-filterable"),
        value: split(el.getAttribute("data-ah-value")),
        open: [], isOpen: false, cursor: null, query: "", searchActive: -1, pending: null
      };
      $.data(el, "ah-cs", st);
      st.display = String(st.$input.val());
      st.$input
        .on("focus" + NS, function () { $el.addClass("ah-cascader-focused"); })
        .on("blur" + NS, function () {
          $el.removeClass("ah-cascader-focused");
          setTimeout(function () {
            if (document.activeElement !== st.$input[0]) { csClose(el, $el); }
          }, 150);
        })
        .on("click" + NS, function (e) {
          e.preventDefault();
          if (st.isOpen && !st.filterable) { csClose(el, $el); } else { csOpen(el, $el); }
        })
        .on("input" + NS, function () {
          if (!st.filterable) { return; }
          csOpen(el, $el);
          csQuery(el, String(st.$input.val()));
        })
        .on("keydown" + NS, function (e) { csKey(el, $el, e); })
        // the text field is internal: only the root reports changes
        .on("change" + NS, function (e) { e.stopPropagation(); });
      $el.on("mousedown" + NS, ".ah-cascader-arrow, .ah-cascader-clear", function (e) {
        e.preventDefault();
      });
      $el.on("click" + NS, ".ah-cascader-arrow", function (e) {
        e.preventDefault();
        st.$input.trigger("focus");
        if (st.isOpen) { csClose(el, $el); } else { csOpen(el, $el); }
      });
      $el.on("click" + NS, ".ah-cascader-clear", function (e) {
        e.preventDefault();
        e.stopPropagation();
        if (csBlocked($el)) { return; }
        csSet(el, $el, [], true);
        csClose(el, $el);
      });
      // the loader's request failed: drop the loading message
      st.$loader.on("ah:error" + NS, function () {
        st.pending = null;
        st.$menus.children(".ah-cascader-loading").remove();
      });
      $popup.on("mousedown" + NS, function (e) { e.preventDefault(); })
        .on("click" + NS, ".ah-cascader-menu li[data-value]", function (e) {
          e.preventDefault();
          csChoose(el, $el, this, false);
        })
        .on("click" + NS, ".ah-cascader-search-item", function () { csSearchPick(el, $el, this); });
    },
    destroy: function (el) {
      var st = csState(el);
      if (st) {
        if (st.float) { st.float.stop(); st.float = null; }
        $(document).off(st.ns);
      }
    },
    methods: {
      // Called by aihtml_form_lists:cascader_children/3 after it appended
      // the column(s) of `path`; no column means the node is a leaf.
      childrenLoaded: function (el, $el, path) {
        var st = csState(el);
        var p = split(path);
        var key = p.join(",");
        var pending = st.pending && st.pending.path === key ? st.pending : null;
        if (pending) { st.pending = null; st.$menus.children(".ah-cascader-loading").remove(); }
        var $cols = csColumns(st).filter(function () { return this.getAttribute("data-parent") === key; });
        $cols.slice(0, -1).remove();
        var li = csItem(csColumn(st, p.slice(0, -1)), p[p.length - 1]);
        if (li) { li.removeAttribute("data-lazy"); }
        if (!$cols.length) {
          if (li) { csLeaf(li); }
          if (pending && st.isOpen && li) { csChoose(el, $el, li, pending.kbd); }
          return;
        }
        if (st.isOpen && st.open.join(",") === key) {
          csShow(el);
          if (pending && pending.kbd) { csCursor(el, csRows($cols.last())[0]); }
        }
      },
      // A path "a,b,c" or ["a", "b", "c"]; no change event.
      setValue: function (el, $el, v) { csSet(el, $el, split(v), false); },
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      getLabels: function (el) { var st = csState(el); return csLabels(st, st.value); },
      clear: function (el, $el) { csSet(el, $el, [], true); },
      open: function (el, $el) { csOpen(el, $el); },
      close: function (el, $el) { csClose(el, $el); }
    }
  });

  // ==================================================================
  // listbox
  // ==================================================================

  function lbState(el) { return $.data(el, "ah-lb"); }

  function lbItems(st) { return st.$list.children(".ah-listbox-item").get(); }
  // Rows keyboard and check-all work on: shown and enabled.
  function lbRows(st) { return lbItems(st).filter(function (li) { return shown(li) && enabled(li); }); }

  function lbMark(el) {
    var st = lbState(el);
    lbItems(st).forEach(function (li) {
      var sel = st.selected.indexOf(li.getAttribute("data-value")) >= 0;
      $(li).toggleClass("ah-listbox-item-selected", sel).attr("aria-selected", String(sel))
        .children(".ah-listbox-checkbox").toggleClass("ah-listbox-checkbox-checked", sel);
    });
    if (st.$checkAll.length) {
      var rows = lbRows(st);
      var n = rows.filter(function (li) { return st.selected.indexOf(li.getAttribute("data-value")) >= 0; }).length;
      var all = rows.length > 0 && n === rows.length;
      st.$checkAll.attr("aria-pressed", all ? "true" : (n ? "mixed" : "false"))
        .children(".ah-listbox-checkbox")
        .toggleClass("ah-listbox-checkbox-checked", all)
        .toggleClass("ah-listbox-checkbox-indeterminate", n > 0 && !all);
    }
  }

  function lbSet(el, $el, values, fire) {
    var st = lbState(el);
    st.selected = st.multi ? values.slice() : values.slice(0, 1);
    lbMark(el);
    publish(el, $el, st.selected.join(","), fire);
  }

  function lbCursor(el, li) {
    var st = lbState(el);
    $(lbItems(st)).removeClass("ah-listbox-item-focused");
    st.cursor = li || null;
    if (!li) { $(el).removeAttr("aria-activedescendant"); return; }
    $(li).addClass("ah-listbox-item-focused");
    el.setAttribute("aria-activedescendant", li.id);
    scrollInto(st.$content[0], li);
  }

  function lbValue(li) { return li.getAttribute("data-value"); }

  function lbRange(st, a, b) {
    var rows = lbRows(st), i = rows.indexOf(a), j = rows.indexOf(b);
    if (i < 0) { i = j; }
    return rows.slice(Math.min(i, j), Math.max(i, j) + 1).map(lbValue);
  }

  function lbToggle(el, $el, li) {
    var st = lbState(el);
    var v = lbValue(li), next = st.selected.slice(), i = next.indexOf(v);
    if (i >= 0) { next.splice(i, 1); } else { next.push(v); }
    lbSet(el, $el, next, true);
  }

  // A click: sigil's select-item! (single, Ctrl toggle, Shift range) and
  // toggle-checkbox! (check boxes).
  function lbClick(el, $el, li, e) {
    var st = lbState(el);
    if (!enabled(li)) { return; }
    if (st.checkboxes || (st.multi && (e.ctrlKey || e.metaKey))) {
      lbToggle(el, $el, li);
      st.anchor = li;
    } else if (st.multi && e.shiftKey && st.anchor) {
      var add = lbRange(st, st.anchor, li);
      lbSet(el, $el, st.selected.concat(add.filter(function (v) { return st.selected.indexOf(v) < 0; })), true);
    } else {
      lbSet(el, $el, [lbValue(li)], true);
      st.anchor = li;
    }
    lbCursor(el, li);
  }

  // Arrow keys and friends: move the cursor and select like sigil (a
  // single row), or extend (Shift) or only move (Ctrl, check boxes).
  function lbGo(el, $el, li, e) {
    var st = lbState(el);
    if (!li) { return; }
    var from = st.anchor || st.cursor || li;
    lbCursor(el, li);
    if (st.checkboxes || (st.multi && (e.ctrlKey || e.metaKey))) { return; }
    if (st.multi && e.shiftKey) {
      st.anchor = from;
      lbSet(el, $el, lbRange(st, from, li), true);
      return;
    }
    st.anchor = li;
    lbSet(el, $el, [lbValue(li)], true);
  }

  function lbKey(el, $el, e) {
    var st = lbState(el);
    var inFilter = e.target !== el;
    var rows = lbRows(st);
    if (!rows.length) { return; }
    var i = rows.indexOf(st.cursor);
    var PAGE = 10;
    switch (e.key) {
      case "ArrowDown": e.preventDefault(); lbGo(el, $el, rows[Math.min(i + 1, rows.length - 1)], e); break;
      case "ArrowUp": e.preventDefault(); lbGo(el, $el, rows[Math.max(i - 1, 0)], e); break;
      case "PageDown": e.preventDefault(); lbGo(el, $el, rows[Math.min(Math.max(i, 0) + PAGE, rows.length - 1)], e); break;
      case "PageUp": e.preventDefault(); lbGo(el, $el, rows[Math.max(i - PAGE, 0)], e); break;
      case "Home": if (!inFilter) { e.preventDefault(); lbGo(el, $el, rows[0], e); } break;
      case "End": if (!inFilter) { e.preventDefault(); lbGo(el, $el, rows[rows.length - 1], e); } break;
      case " ":
        if (inFilter || !st.cursor) { break; }
        e.preventDefault();
        if (st.multi) { lbToggle(el, $el, st.cursor); st.anchor = st.cursor; } else { lbSet(el, $el, [lbValue(st.cursor)], true); }
        break;
      case "Enter":
        if (!st.cursor) { break; }
        e.preventDefault();
        if (st.checkboxes) { lbToggle(el, $el, st.cursor); } else if (!st.multi) { lbSet(el, $el, [lbValue(st.cursor)], true); }
        break;
      default:
        if (inFilter) { break; }
        if ((e.key === "a" || e.key === "A") && (e.ctrlKey || e.metaKey) && st.multi) {
          e.preventDefault();
          lbSet(el, $el, rows.map(lbValue), true);
          break;
        }
        // sigil's incremental search: typed letters within 800 ms
        if (e.key && e.key.length === 1 && !e.ctrlKey && !e.altKey && !e.metaKey) {
          var now = Date.now();
          st.typed = (now - (st.typedAt || 0) > 800 ? "" : st.typed) + e.key.toLowerCase();
          st.typedAt = now;
          var hit = rows.filter(function (li) {
            return $(li).children(".ah-listbox-label").text().toLowerCase().indexOf(st.typed) === 0;
          })[0];
          if (hit) { e.preventDefault(); lbGo(el, $el, hit, {}); }
        }
    }
  }

  // sigil's filter-items!: hide rows without the text, and empty groups.
  function lbFilter(el, text) {
    var st = lbState(el);
    var q = String(text || "").trim().toLowerCase();
    if (!st.remote) {
      lbItems(st).forEach(function (li) {
        var hit = !q || $(li).children(".ah-listbox-label").text().toLowerCase().indexOf(q) >= 0;
        li.style.display = hit ? "" : "none";
      });
    }
    lbGroups(el);
  }

  function lbGroups(el) {
    var st = lbState(el);
    st.$list.children(".ah-listbox-group").each(function () {
      var $rows = $(this).nextUntil(".ah-listbox-group");
      this.style.display = $rows.filter(function () { return shown(this); }).length ? "" : "none";
    });
    var any = lbItems(st).some(shown);
    st.$empty.prop("hidden", any);
    if (st.cursor && (!st.cursor.isConnected || !shown(st.cursor))) { lbCursor(el, null); }
    lbMark(el);
  }

  AH.define("listbox", {
    init: function (el, $el) {
      ensureId(el, "ah-lb");
      var st = {
        $list: $el.find(".ah-listbox-list"),
        $content: $el.children(".ah-listbox-content"),
        $empty: $el.find(".ah-listbox-empty"),
        $checkAll: $el.children(".ah-listbox-check-all"),
        $filter: $el.find(".ah-listbox-filter-input"),
        checkboxes: $el.hasClass("ah-listbox-checkboxes"),
        remote: $el.hasClass("ah-listbox-remote"),
        selected: split(el.getAttribute("data-ah-value")),
        cursor: null, anchor: null, typed: ""
      };
      st.multi = st.checkboxes || $el.hasClass("ah-listbox-multiple");
      $.data(el, "ah-lb", st);
      var blocked = function () { return $el.hasClass("ah-listbox-disabled"); };
      $el.on("mousedown" + NS, ".ah-listbox-item, .ah-listbox-check-all", function (e) {
        if (e.shiftKey) { e.preventDefault(); }  // no text selection on Shift+click
      });
      $el.on("click" + NS, ".ah-listbox-item", function (e) {
        if (!blocked()) { lbClick(el, $el, this, e); }
      });
      $el.on("click" + NS, ".ah-listbox-check-all", function () {
        if (blocked()) { return; }
        var rows = lbRows(st).map(lbValue);
        var all = rows.length && rows.every(function (v) { return st.selected.indexOf(v) >= 0; });
        var rest = st.selected.filter(function (v) { return rows.indexOf(v) < 0; });
        lbSet(el, $el, all ? rest : rest.concat(rows), true);
      });
      $el.on("keydown" + NS, function (e) { if (!blocked()) { lbKey(el, $el, e); } });
      $el.on("focus" + NS, function () {
        if (!st.cursor) {
          var rows = lbRows(st);
          var sel = rows.filter(function (li) { return st.selected.indexOf(lbValue(li)) >= 0; })[0];
          if (sel || rows[0]) { lbCursor(el, sel || rows[0]); }
        }
      });
      st.$filter
        .on("input" + NS, function () { lbFilter(el, this.value); })
        .on("change" + NS, function (e) { e.stopPropagation(); });
    },
    methods: {
      // A value or a list (multiple); no change event (the server set it).
      setValue: function (el, $el, v) { lbSet(el, $el, split(v), false); },
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      clear: function (el, $el) { lbSet(el, $el, [], true); },
      filter: function (el, $el, text) {
        lbState(el).$filter.val(text);
        lbFilter(el, text);
      },
      // Called by aihtml_form_lists:listbox_items/3 after it morphed the
      // server-rendered rows into the list.
      itemsLoaded: function (el) {
        lbState(el).anchor = null;
        lbGroups(el);
      }
    }
  });

  // ==================================================================
  // transfer
  // ==================================================================

  function trState(el) { return $.data(el, "ah-tr"); }

  function trList(st, side) { return side === "source" ? st.$source : st.$target; }
  function trItems($list) { return $list.children(".ah-transfer-item").get(); }
  function trRows($list) {
    return trItems($list).filter(function (li) { return shown(li) && enabled(li); });
  }
  function trSelected($list) {
    return trRows($list).filter(function (li) { return $(li).hasClass("ah-transfer-item-selected"); });
  }

  function trSelect(li, on) {
    $(li).toggleClass("ah-transfer-item-selected", on).attr("aria-selected", String(on));
  }

  // Counts, move buttons, filters.
  function trSync(el) {
    var st = trState(el);
    ["source", "target"].forEach(function (side) {
      var $list = trList(st, side);
      var $panel = $list.closest(".ah-transfer-panel");
      $panel.find(".ah-transfer-panel-count").text(trItems($list).length);
      var q = String($panel.find(".ah-transfer-filter-input").val() || "").trim().toLowerCase();
      trItems($list).forEach(function (li) {
        var hit = !q || $(li).children(".ah-transfer-item-label").text().toLowerCase().indexOf(q) >= 0;
        li.style.display = hit ? "" : "none";
        if (!hit) { trSelect(li, false); }
      });
      var btn = st.$el.find(side === "source" ? ".ah-transfer-btn-to-target" : ".ah-transfer-btn-to-source");
      var off = st.disabled || !trSelected($list).length;
      btn.toggleClass("ah-transfer-btn-disabled", off).prop("disabled", off);
    });
  }

  function trPublish(el, fire) {
    var st = trState(el);
    publish(el, st.$el, trItems(st.$target).map(function (li) {
      return li.getAttribute("data-value");
    }).join(","), fire);
  }

  function idx(li) { return parseInt(li.getAttribute("data-idx"), 10) || 0; }

  // Back to the source: in the order of the items.
  function trInsert($list, li) {
    var after = trItems($list).filter(function (x) { return idx(x) < idx(li); }).pop();
    if (after) { $(li).insertAfter(after); } else { $list.prepend(li); }
  }

  function trMove(el, from) {
    var st = trState(el);
    if (st.disabled) { return; }
    var moving = trSelected(trList(st, from));
    if (!moving.length) { return; }
    var to = from === "source" ? "target" : "source";
    var $to = trList(st, to);
    moving.forEach(function (li) {
      trSelect(li, false);
      $(li).removeClass("ah-transfer-item-focused").attr("data-source", to);
      if (to === "target") { $to.append(li); } else { trInsert($to, li); }
    });
    if (st.cursor && moving.indexOf(st.cursor) >= 0) { trCursor(el, null); }
    trSync(el);
    trPublish(el, true);
  }

  function trCursor(el, li) {
    var st = trState(el);
    st.$el.find(".ah-transfer-item-focused").removeClass("ah-transfer-item-focused");
    st.$source.add(st.$target).removeAttr("aria-activedescendant");
    st.cursor = li || null;
    if (!li) { return; }
    $(li).addClass("ah-transfer-item-focused");
    var list = li.parentNode;
    list.setAttribute("aria-activedescendant", li.id);
    scrollInto(list.parentNode, li);
  }

  function trKey(el, $list, e) {
    var st = trState(el);
    if (st.disabled) { return; }
    var rows = trRows($list), side = $list.attr("data-panel");
    var i = rows.indexOf(st.cursor);
    switch (e.key) {
      case "ArrowDown": e.preventDefault(); trCursor(el, rows[Math.min(i + 1, rows.length - 1)]); break;
      case "ArrowUp": e.preventDefault(); trCursor(el, rows[Math.max(i - 1, 0)]); break;
      case "Home": e.preventDefault(); trCursor(el, rows[0]); break;
      case "End": e.preventDefault(); trCursor(el, rows[rows.length - 1]); break;
      case " ":
        e.preventDefault();
        if (st.cursor && rows.indexOf(st.cursor) >= 0) {
          trSelect(st.cursor, !$(st.cursor).hasClass("ah-transfer-item-selected"));
          trSync(el);
        }
        break;
      case "Enter":
        e.preventDefault();
        if (!trSelected($list).length && st.cursor && rows.indexOf(st.cursor) >= 0) { trSelect(st.cursor, true); }
        var next = rows.filter(function (li) { return !$(li).hasClass("ah-transfer-item-selected"); })[0];
        trMove(el, side);
        if (next) { trCursor(el, next); }
        break;
      default:
        if ((e.key === "a" || e.key === "A") && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          rows.forEach(function (li) { trSelect(li, true); });
          trSync(el);
        }
    }
  }

  AH.define("transfer", {
    init: function (el, $el) {
      ensureId(el, "ah-tr");
      var st = {
        $el: $el,
        $source: $el.find(".ah-transfer-list[data-panel=source]"),
        $target: $el.find(".ah-transfer-list[data-panel=target]"),
        disabled: $el.hasClass("ah-transfer-disabled"),
        cursor: null
      };
      $.data(el, "ah-tr", st);
      $el.on("mousedown" + NS, ".ah-transfer-item", function (e) {
        if (e.shiftKey || e.detail > 1) { e.preventDefault(); }  // no text selection
      });
      $el.on("click" + NS, ".ah-transfer-item", function () {
        if (st.disabled || !enabled(this)) { return; }
        trSelect(this, !$(this).hasClass("ah-transfer-item-selected"));
        trCursor(el, this);
        trSync(el);
      });
      $el.on("dblclick" + NS, ".ah-transfer-item", function () {
        if (st.disabled || !enabled(this)) { return; }
        trSelect(this, true);
        trMove(el, this.parentNode.getAttribute("data-panel"));
      });
      $el.on("click" + NS, ".ah-transfer-btn", function (e) {
        e.preventDefault();
        trMove(el, this.getAttribute("data-direction") === "to-target" ? "source" : "target");
      });
      $el.on("keydown" + NS, ".ah-transfer-list", function (e) { trKey(el, $(this), e); });
      $el.on("focus" + NS, ".ah-transfer-list", function () {
        if (!st.cursor || st.cursor.parentNode !== this) { trCursor(el, trRows($(this))[0]); }
      });
      $el.on("input" + NS, ".ah-transfer-filter-input", function () { trSync(el); })
        .on("change" + NS, ".ah-transfer-filter-input", function (e) { e.stopPropagation(); });
      trSync(el);
    },
    methods: {
      // The keys of the right list, in order; no change event.
      setValue: function (el, $el, v) {
        var st = trState(el), keys = split(v);
        var all = trItems(st.$source).concat(trItems(st.$target));
        all.sort(function (a, b) { return idx(a) - idx(b); });
        all.forEach(function (li) { trSelect(li, false); });
        keys.forEach(function (k) {
          var li = all.filter(function (x) { return x.getAttribute("data-value") === k; })[0];
          if (li) { $(li).attr("data-source", "target"); st.$target.append(li); }
        });
        all.forEach(function (li) {
          if (keys.indexOf(li.getAttribute("data-value")) < 0) {
            $(li).attr("data-source", "source");
            st.$source.append(li);
          }
        });
        trCursor(el, null);
        trSync(el);
        trPublish(el, false);
      },
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      moveToTarget: function (el) { trMove(el, "source"); },
      moveToSource: function (el) { trMove(el, "target"); },
      selectAll: function (el, $el, side) {
        trRows(trList(trState(el), side || "source")).forEach(function (li) { trSelect(li, true); });
        trSync(el);
      },
      clearSelection: function (el, $el, side) {
        trItems(trList(trState(el), side || "source")).forEach(function (li) { trSelect(li, false); });
        trSync(el);
      }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/form_pickers.js ---- */
/* Behaviours of the form_pickers components (designs/04-components.md).
 * Ported from sigil: form/datepicker (+ calendar, popup, util) and
 * form/combobox (+ popup, search). Date math is plain JS on local dates. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  function ensureId(el, prefix) {
    if (!el.id) { el.id = prefix + (++seq); }
    return el.id;
  }

  // A mousedown outside the component. A target that is gone was inside
  // a list re-rendered by this very mousedown (picking in multiple mode).
  function outside(el, e) {
    return e.target.isConnected !== false && !$.contains(el, e.target) && e.target !== el;
  }

  // Value-bearing contract: data-ah-value + hidden input, then `change`.
  function publish(el, $el, value, fire) {
    var old = el.getAttribute("data-ah-value") || "";
    el.setAttribute("data-ah-value", value);
    $el.children("input[type=hidden]").val(value);
    if (fire && old !== value) { $el.trigger("change"); }
  }

  // ==================================================================
  // datepicker
  // ==================================================================

  var DAY = 86400000;

  function pad(n) { return (n < 10 ? "0" : "") + n; }
  function mk(y, m, d) { return new Date(y, m, d); }          // m: 0..11, overflow ok
  function iso(d) { return d ? d.getFullYear() + "-" + pad(d.getMonth() + 1) + "-" + pad(d.getDate()) : ""; }
  function parse(s) {
    var m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(s || "");
    return m ? mk(+m[1], +m[2] - 1, +m[3]) : null;
  }
  function today() { var n = new Date(); return mk(n.getFullYear(), n.getMonth(), n.getDate()); }
  function same(a, b) { return !!(a && b) && a.getTime() === b.getTime(); }
  function addDays(d, n) { return mk(d.getFullYear(), d.getMonth(), d.getDate() + n); }
  // Month arithmetic keeps the day, clamped to the target month (date-fns addMonths).
  function addMonths(d, n) {
    var t = mk(d.getFullYear(), d.getMonth() + n, 1);
    var last = mk(t.getFullYear(), t.getMonth() + 1, 0).getDate();
    return mk(t.getFullYear(), t.getMonth(), Math.min(d.getDate(), last));
  }
  function startOfWeek(d, first) { return addDays(d, -((d.getDay() - first + 7) % 7)); }
  // date-fns getWeek with weekStartsOn = first, firstWeekContainsDate = 1.
  function weekNumber(d, first) {
    var y = d.getFullYear();
    var wy = d >= startOfWeek(mk(y + 1, 0, 1), first) ? y + 1
      : (d >= startOfWeek(mk(y, 0, 1), first) ? y : y - 1);
    var diff = startOfWeek(d, first) - startOfWeek(mk(wy, 0, 1), first);
    return Math.round(diff / (7 * DAY)) + 1;
  }

  // The display formats the Erlang side knows: yyyy yy MMMM MMM MM M dd d.
  function format(d, fmt, L) {
    if (!d) { return ""; }
    return fmt.replace(/yyyy|yy|MMMM|MMM|MM|M|dd|d/g, function (t) {
      switch (t) {
        case "yyyy": return String(d.getFullYear());
        case "yy": return String(d.getFullYear()).slice(2);
        case "MMMM": return L.months[d.getMonth()];
        case "MMM": return L.months_short[d.getMonth()];
        case "MM": return pad(d.getMonth() + 1);
        case "M": return String(d.getMonth() + 1);
        case "dd": return pad(d.getDate());
        default: return String(d.getDate());
      }
    });
  }

  var DEFAULT_LABELS = {
    months: ["January", "February", "March", "April", "May", "June", "July",
             "August", "September", "October", "November", "December"],
    months_short: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    weekdays: ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"],
    title: "MMMM yyyy", today: "Today", clear: "Clear",
    prev_month: "Previous month", next_month: "Next month",
    prev_year: "Previous year", next_year: "Next year"
  };

  function dpState(el) { return $.data(el, "ah-dp"); }

  function dpDisabled(st, d) {
    return (st.min && d < st.min) || (st.max && d > st.max) || !!st.off[iso(d)];
  }

  function dpDisplay(st) {
    if (st.range) {
      if (!st.from) { return ""; }
      return format(st.from, st.fmt, st.L) + (st.to ? " - " + format(st.to, st.fmt, st.L) : "");
    }
    return format(st.value, st.fmt, st.L);
  }

  function dpIsoValue(st) {
    return st.range ? (st.from ? iso(st.from) + "," + iso(st.to) : "") : iso(st.value);
  }

  // The view of templates/datepicker_month.mustache: every class, label
  // and id computed here (the Erlang month_view/1 builds the same view for
  // the server's `inline' render).
  function dpView(el) {
    var st = dpState(el);
    var L = st.L;
    var y = st.viewY, m = st.viewM;
    var first = st.firstDay;
    var monthStart = mk(y, m, 1);
    var gridStart = startOfWeek(monthStart, first);
    var gridEnd = addDays(startOfWeek(mk(y, m + 1, 0), first), 7);
    var now = today();
    var from = st.range ? (st.pending || st.from) : null;
    var to = st.range ? (st.pending ? null : st.to) : null;
    var weeks = [];
    for (var w = gridStart; w < gridEnd; w = addDays(w, 7)) {
      var days = [];
      for (var k = 0; k < 7; k++) {
        var d = addDays(w, k);
        var other = d.getMonth() !== m;
        if (other && !st.otherMonth) { days.push({ empty: true }); continue; }
        var dis = !!dpDisabled(st, d);
        var isStart = same(d, from), isEnd = same(d, to);
        var sel = st.range ? (isStart || isEnd) : same(d, st.value);
        var inRange = !!(st.range && from && to && d >= from && d <= to);
        var dow = d.getDay();
        days.push({
          empty: false,
          cls: "ah-datepicker-day" +
            (other ? " ah-datepicker-day-other-month" : "") +
            (same(d, now) ? " ah-datepicker-day-today" : "") +
            (st.weekends && (dow === 0 || dow === 6) ? " ah-datepicker-day-weekend" : "") +
            (dis ? " ah-datepicker-day-disabled" : "") +
            (sel ? " ah-datepicker-day-selected" : "") +
            (inRange ? " ah-datepicker-day-in-range" : "") +
            (isStart ? " ah-datepicker-day-range-start" : "") +
            (isEnd ? " ah-datepicker-day-range-end" : "") +
            (same(d, st.focus) ? " ah-datepicker-day-focused" : ""),
          id: el.id + "-d" + iso(d),
          date: iso(d),
          selected: String(sel),
          disabled: String(dis),
          label: format(d, "d MMMM yyyy", L),
          day: String(d.getDate())
        });
      }
      weeks.push({ num: String(weekNumber(w, first)), days: days });
    }
    var weekdays = [];
    for (var i = 0; i < 7; i++) { weekdays.push({ label: L.weekdays[(first + i) % 7] }); }
    return {
      title_id: el.id + "-title",
      title: format(monthStart, L.title, L),
      prev_year: L.prev_year, prev_month: L.prev_month,
      next_month: L.next_month, next_year: L.next_year,
      today: L.today,
      week_numbers: st.weekNumbers,
      weekdays: weekdays,
      weeks: weeks
    };
  }

  function dpRender(el) {
    var st = dpState(el);
    st.$popup.html(AH.tpl.datepicker_month(dpView(el)));
    st.$input.attr("aria-activedescendant", st.focus ? el.id + "-d" + iso(st.focus) : null);
  }

  // popup.cljs position!: at least 280px wide (the viewport minus a
  // margin on small screens); AH.float keeps it fixed under the field (or
  // above, when there is no room), inside the viewport and following
  // scrolls, so clipping ancestors do not cut it.
  function dpPosition(el) {
    var st = dpState(el);
    var w = Math.min(Math.max(el.getBoundingClientRect().width, 280), window.innerWidth - 16);
    st.$popup.css({ width: w + "px" });
    if (st.float) { st.float.update(); } else { st.float = AH.float(st.$popup[0], el); }
  }

  function dpBlocked(el, $el) {
    return $el.hasClass("ah-datepicker-disabled") || $el.hasClass("ah-datepicker-readonly");
  }

  function dpOpen(el, $el) {
    var st = dpState(el);
    if (st.open || (dpBlocked(el, $el) && !st.inline)) { return; }
    st.pending = null;
    st.focus = (st.range ? st.from : st.value) || today();
    st.viewY = st.focus.getFullYear();
    st.viewM = st.focus.getMonth();
    st.open = true;
    dpRender(el);
    if (st.inline) { return; }        // always shown, in the flow
    st.$popup.show();
    dpPosition(el);
    $el.addClass("ah-datepicker-open");
    st.$input.attr("aria-expanded", "true");
    $(document).on("mousedown" + st.ns, function (e) {
      if (outside(el, e)) { dpClose(el, $el); }
    });
    $el.trigger("ah:open");
  }

  function dpClose(el, $el) {
    var st = dpState(el);
    if (!st.open || st.inline) { return; }
    st.open = false;
    st.pending = null;
    st.$popup.hide();
    $el.removeClass("ah-datepicker-open");
    st.$input.attr("aria-expanded", "false").removeAttr("aria-activedescendant");
    if (st.float) { st.float.stop(); st.float = null; }
    $(document).off(st.ns);
    $el.trigger("ah:close");
  }

  function dpCommit(el, $el, fire) {
    var st = dpState(el);
    st.$input.val(dpDisplay(st));
    publish(el, $el, dpIsoValue(st), fire);
  }

  function dpSetFocus(el, d) {
    var st = dpState(el);
    st.focus = d;
    st.viewY = d.getFullYear();
    st.viewM = d.getMonth();
    dpRender(el);
    if (st.open && !st.inline) { dpPosition(el); }
  }

  function dpNavMonths(el, n) {
    var st = dpState(el);
    var f = st.focus && st.focus.getMonth() === st.viewM ? st.focus : mk(st.viewY, st.viewM, 1);
    dpSetFocus(el, addMonths(f, n));
  }

  // calendar.cljs handle-day-click: single selects and closes; range takes
  // a start, then an end (sorted), then closes.
  function dpPick(el, $el, d) {
    var st = dpState(el);
    if (!d || dpDisabled(st, d) || dpBlocked(el, $el)) { return; }
    if (!st.range) {
      st.value = d;
      st.focus = d;
      dpCommit(el, $el, true);
      dpClose(el, $el);
      if (st.inline) { dpRender(el); }
    } else if (!st.pending) {
      st.pending = d;
      st.focus = d;
      dpRender(el);
    } else {
      var a = st.pending;
      st.from = d < a ? d : a;
      st.to = d < a ? a : d;
      st.pending = null;
      dpCommit(el, $el, true);
      dpClose(el, $el);
      if (st.inline) { dpRender(el); }
    }
  }

  function dpHover(el, d) {
    var st = dpState(el);
    if (!st.range || !st.pending) { return; }
    var a = st.pending < d ? st.pending : d;
    var b = st.pending < d ? d : st.pending;
    st.$popup.find(".ah-datepicker-day[data-date]").each(function () {
      var c = parse(this.getAttribute("data-date"));
      $(this).toggleClass("ah-datepicker-day-hover-range", c >= a && c <= b);
    });
  }

  // popup.cljs handle-keydown. Up/Down move a week (sigil moves a day),
  // Shift+PageUp/PageDown a year.
  function dpKey(el, $el, e) {
    var st = dpState(el);
    if (dpBlocked(el, $el)) { return; }
    var open = st.open;
    var f = st.focus || today();
    switch (e.key) {
      case "ArrowDown":
        e.preventDefault();
        if (e.altKey || !open) { dpOpen(el, $el); } else { dpSetFocus(el, addDays(f, 7)); }
        break;
      case "ArrowUp":
        if (!open) { return; }
        e.preventDefault();
        if (e.altKey) { dpClose(el, $el); } else { dpSetFocus(el, addDays(f, -7)); }
        break;
      case "ArrowLeft":
        if (open) { e.preventDefault(); dpSetFocus(el, addDays(f, -1)); }
        break;
      case "ArrowRight":
        if (open) { e.preventDefault(); dpSetFocus(el, addDays(f, 1)); }
        break;
      case "Enter":
      case " ":
        e.preventDefault();
        if (open) { dpPick(el, $el, f); } else { dpOpen(el, $el); }
        break;
      case "Escape":
        if (open) { e.preventDefault(); dpClose(el, $el); }
        break;
      case "Tab":
        dpClose(el, $el);
        break;
      case "PageUp":
        if (open) { e.preventDefault(); dpNavMonths(el, e.shiftKey ? -12 : -1); }
        break;
      case "PageDown":
        if (open) { e.preventDefault(); dpNavMonths(el, e.shiftKey ? 12 : 1); }
        break;
      case "Home":
        if (open) { e.preventDefault(); dpSetFocus(el, mk(st.viewY, st.viewM, 1)); }
        break;
      case "End":
        if (open) { e.preventDefault(); dpSetFocus(el, mk(st.viewY, st.viewM + 1, 0)); }
        break;
      case "Backspace":
      case "Delete":
        if ($el.hasClass("ah-datepicker-clearable")) { e.preventDefault(); dpClear(el, $el, true); }
        break;
      default:
        break;
    }
  }

  function dpClear(el, $el, fire) {
    var st = dpState(el);
    st.value = st.from = st.to = st.pending = null;
    dpCommit(el, $el, fire);
    if (st.open) { dpRender(el); }
  }

  function dpSet(el, $el, v) {
    var st = dpState(el);
    if (Array.isArray(v)) { v = v.join(","); }
    v = v || "";
    if (st.range) {
      var p = v.split(",");
      var a = parse(p[0]), b = parse(p[1]);
      st.from = a && b && b < a ? b : a;
      st.to = a && b && b < a ? a : b;
    } else {
      st.value = parse(v);
    }
    st.pending = null;
    dpCommit(el, $el, false);
    if (st.open) { dpRender(el); }
  }

  AH.define("datepicker", {
    init: function (el, $el) {
      ensureId(el, "ah-dp");
      var labels = {};
      try { labels = JSON.parse(el.getAttribute("data-ah-labels") || "{}"); } catch (err) { labels = {}; }
      var off = {};
      (el.getAttribute("data-ah-disabled-dates") || "").split(",").forEach(function (d) {
        if (d) { off[d] = true; }
      });
      var st = {
        ns: ".ahdp" + (++seq),
        $input: $el.find("input.ah-datepicker-input"),
        $popup: $el.children(".ah-datepicker-popup"),
        range: el.hasAttribute("data-ah-range"),
        fmt: el.getAttribute("data-ah-format") || "yyyy-MM-dd",
        min: parse(el.getAttribute("data-ah-min")),
        max: parse(el.getAttribute("data-ah-max")),
        off: off,
        firstDay: parseInt(el.getAttribute("data-ah-first-day") || "0", 10) || 0,
        weekNumbers: el.hasAttribute("data-ah-week-numbers"),
        weekends: el.hasAttribute("data-ah-weekends"),
        otherMonth: el.getAttribute("data-ah-other-month-days") !== "false",
        L: $.extend({}, DEFAULT_LABELS, labels),
        inline: $el.hasClass("ah-datepicker-inline"),
        open: false, value: null, from: null, to: null, pending: null, focus: null
      };
      $.data(el, "ah-dp", st);
      st.$popup.attr("id", el.id + "-popup");
      st.$input.attr("aria-controls", el.id + "-popup");
      var v = el.getAttribute("data-ah-value") || "";
      if (st.range) {
        var p = v.split(",");
        st.from = parse(p[0]);
        st.to = parse(p[1]);
      } else {
        st.value = parse(v);
      }

      st.$input
        .on("focus" + NS, function () { $el.addClass("ah-datepicker-focused"); })
        .on("blur" + NS, function () { $el.removeClass("ah-datepicker-focused"); })
        .on("click" + NS, function (e) {
          e.preventDefault();
          if (st.open) { dpClose(el, $el); } else { dpOpen(el, $el); }
        })
        .on("keydown" + NS, function (e) { dpKey(el, $el, e); })
        // the text field is internal: only the root reports changes
        .on("change" + NS + " input" + NS, function (e) { e.stopPropagation(); });
      $el.on("click" + NS, ".ah-datepicker-trigger", function (e) {
        e.preventDefault();
        e.stopPropagation();
        st.$input.trigger("focus");
        if (st.open) { dpClose(el, $el); } else { dpOpen(el, $el); }
      });
      $el.on("mousedown" + NS, ".ah-datepicker-clear", function (e) { e.preventDefault(); });
      $el.on("click" + NS, ".ah-datepicker-clear", function (e) {
        e.preventDefault();
        e.stopPropagation();
        dpClear(el, $el, true);
      });
      // Keep the focus in the text field while using the calendar.
      st.$popup.on("mousedown" + NS, function (e) {
        e.preventDefault();
        if (st.inline && !dpBlocked(el, $el)) { st.$input[0].focus(); }
      })
        .on("click" + NS, function (e) { e.stopPropagation(); })
        .on("click" + NS, ".ah-datepicker-day[data-date]", function () {
          dpPick(el, $el, parse(this.getAttribute("data-date")));
        })
        .on("mouseenter" + NS, ".ah-datepicker-day[data-date]", function () {
          dpHover(el, parse(this.getAttribute("data-date")));
        })
        .on("click" + NS, "[data-nav]", function () {
          dpNavMonths(el, parseInt(this.getAttribute("data-nav"), 10));
        })
        .on("click" + NS, ".ah-datepicker-today-btn", function () { dpSetFocus(el, today()); });
      if (st.inline) { dpOpen(el, $el); }
    },
    destroy: function (el) {
      var st = dpState(el);
      if (st) {
        if (st.float) { st.float.stop(); st.float = null; }
        $(document).off(st.ns);
        st.$popup.off(NS);
      }
    },
    methods: {
      // setValue("2026-09-29"), setValue("from,to") or setValue([from, to]);
      // no change event (the server set it).
      setValue: function (el, $el, v) { dpSet(el, $el, v); },
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      clear: function (el, $el) { dpClear(el, $el, true); },
      open: function (el, $el) { dpOpen(el, $el); },
      close: function (el, $el) { dpClose(el, $el); }
    }
  });

  // ==================================================================
  // combobox
  // ==================================================================

  function cbState(el) { return $.data(el, "ah-cb"); }

  // search.cljs match-fn
  var MATCH = {
    contains_ignore_case: function (t, q) { return t.toLowerCase().indexOf(q.toLowerCase()) >= 0; },
    contains: function (t, q) { return t.indexOf(q) >= 0; },
    starts_with_ignore_case: function (t, q) { return t.toLowerCase().indexOf(q.toLowerCase()) === 0; },
    starts_with: function (t, q) { return t.indexOf(q) === 0; },
    equals_ignore_case: function (t, q) { return t.toLowerCase() === q.toLowerCase(); },
    equals: function (t, q) { return t === q; },
    none: function () { return true; }
  };

  // search.cljs highlight-match, as DOM nodes: the first case-insensitive
  // occurrence of the query in <b>.
  function highlight(node, text, q) {
    var i = q ? text.toLowerCase().indexOf(q.toLowerCase()) : -1;
    node.textContent = i < 0 ? text : text.slice(0, i);
    if (i < 0) { return; }
    var b = document.createElement("b");
    b.textContent = text.slice(i, i + q.length);
    node.appendChild(b);
    node.appendChild(document.createTextNode(text.slice(i + q.length)));
  }

  // The items are the server-rendered <li>s (first render, or morphed in
  // by aihtml_form_pickers:set_items); this reads them back.
  function cbRead(el) {
    var st = cbState(el);
    st.items = st.$list.children(".ah-combobox-item").map(function () {
      var it = {
        el: this,
        value: this.getAttribute("data-value"),
        label: this.getAttribute("data-label"),
        disabled: this.getAttribute("aria-disabled") === "true"
      };
      st.labels[it.value] = it.label;
      return it;
    }).get();
  }

  function cbLabel(st, v) {
    return Object.prototype.hasOwnProperty.call(st.labels, v) ? st.labels[v] : v;
  }

  // Selected state of the rows (after picks in multiple mode, or new rows).
  function cbMark(el) {
    var st = cbState(el);
    st.items.forEach(function (it) {
      var sel = st.selected.indexOf(it.value) >= 0;
      var $li = $(it.el).toggleClass("ah-combobox-item-selected", sel)
        .attr("aria-selected", String(sel));
      var $box = $li.children(".ah-combobox-checkbox").toggleClass("ah-combobox-checkbox-checked", sel);
      if (!$box.length) { return; }
      var $icon = $box.children(".ah-combobox-checkbox-icon");
      if (sel && !$icon.length) {
        $box.append($('<span class="ah-combobox-checkbox-icon"></span>').text("✓"));
      } else if (!sel) {
        $icon.remove();
      }
    });
  }

  // popup.cljs update-query!: show the matching rows (all of them for
  // server results), highlight the query, hide empty groups, and show the
  // empty or loading message.
  function cbFilter(el) {
    var st = cbState(el);
    var match = (st.remote || !st.query || st.mode === "none") ? null
      : (MATCH[st.mode] || MATCH.contains_ignore_case);
    st.visible = [];
    st.items.forEach(function (it) {
      var show = !match || match(it.label, st.query);
      it.el.style.display = show ? "" : "none";
      if (show) { st.visible.push(it); }
      highlight(it.el.querySelector(".ah-combobox-item-label"), it.label, st.query);
    });
    st.$list.children(".ah-combobox-group-header").each(function () {
      var $rows = $(this).nextUntil(".ah-combobox-group-header");
      this.style.display = $rows.filter(function () { return this.style.display !== "none"; }).length
        ? "" : "none";
    });
    cbMark(el);
    st.$popup.children(".ah-combobox-empty, .ah-combobox-loading").remove();
    if (st.loading) {
      st.$popup.append($('<div class="ah-combobox-loading"></div>').text("Loading…"));
    } else if (!st.visible.length) {
      st.$popup.append($('<div class="ah-combobox-empty"></div>').text(st.empty));
    }
    st.$list.toggle(!st.loading && st.visible.length > 0);
    cbActive(el, -1);
  }

  function cbActive(el, idx) {
    var st = cbState(el);
    st.active = idx;
    $(st.items.map(function (it) { return it.el; })).removeClass("ah-combobox-item-active");
    var item = idx >= 0 && st.visible[idx] ? st.visible[idx].el : null;
    if (!item) {
      st.active = -1;
      st.$input.removeAttr("aria-activedescendant");
      return;
    }
    $(item).addClass("ah-combobox-item-active");
    st.$input.attr("aria-activedescendant", item.id || null);
    var p = st.$popup[0];               // popup.cljs scroll-item-into-view!
    if (item.offsetTop < p.scrollTop) { p.scrollTop = item.offsetTop; }
    if (item.offsetTop + item.offsetHeight > p.scrollTop + p.clientHeight) {
      p.scrollTop = item.offsetTop + item.offsetHeight - p.clientHeight;
    }
  }

  function cbMove(el, dir) {
    var st = cbState(el);
    var n = st.visible.length;
    if (!n) { return; }
    var i = st.active;
    for (var k = 0; k < n; k++) {
      i = dir > 0 ? (i < n - 1 ? i + 1 : 0) : (i > 0 ? i - 1 : n - 1);
      if (!st.visible[i].disabled) { cbActive(el, i); return; }
    }
  }

  // AH.float: fixed at the field, at least as wide, flipped above when
  // there is no room; update() after the list or the tags change size.
  function cbPosition(el) {
    var st = cbState(el);
    if (!st.open) { return; }
    if (st.float) {
      st.float.update();
    } else {
      st.float = AH.float(st.$popup[0], el, { matchWidth: true });
    }
  }

  function cbBlocked($el) { return $el.hasClass("ah-combobox-disabled"); }

  function cbOpen(el, $el) {
    var st = cbState(el);
    if (cbBlocked($el)) { return; }
    cbFilter(el);
    if (st.open) { cbPosition(el); return; }
    st.open = true;
    st.$popup.addClass("ah-combobox-popup-open");
    cbPosition(el);
    $el.addClass("ah-combobox-open");
    st.$input.attr("aria-expanded", "true");
    $(document).on("mousedown" + st.ns, function (e) {
      if (outside(el, e)) { cbClose(el, $el); }
    });
    $el.trigger("ah:open");
  }

  function cbClose(el, $el) {
    var st = cbState(el);
    if (!st.open) { return; }
    st.open = false;
    st.$popup.removeClass("ah-combobox-popup-open");
    if (st.float) { st.float.stop(); st.float = null; }
    $el.removeClass("ah-combobox-open");
    st.$input.attr("aria-expanded", "false").removeAttr("aria-activedescendant");
    st.active = -1;
    $(document).off(st.ns);
    $el.trigger("ah:close");
  }

  // Tags added in the browser use the server's markup:
  // templates/combobox_tag.mustache
  function cbTags(el) {
    var st = cbState(el);
    st.$input.siblings(".ah-combobox-tag").remove();
    st.selected.forEach(function (v) {
      $(AH.tpl.combobox_tag({ value: v, label: cbLabel(st, v) })).insertBefore(st.$input);
    });
    st.$input.attr("placeholder", st.selected.length ? "" : st.placeholder);
  }

  function cbSync(el, $el, fire) {
    var st = cbState(el);
    if (st.multi) {
      cbTags(el);
    } else {
      st.$input.val(st.selected.length ? cbLabel(st, st.selected[0]) : "");
    }
    publish(el, $el, st.selected.join(","), fire);
  }

  // popup.cljs select-single-item! / toggle-multi-item!
  function cbPick(el, $el, it) {
    var st = cbState(el);
    if (!it || it.disabled) { return; }
    if (st.multi) {
      var i = st.selected.indexOf(it.value);
      if (i >= 0) { st.selected.splice(i, 1); } else { st.selected.push(it.value); }
      cbSync(el, $el, true);
      cbMark(el);
      cbPosition(el);
    } else {
      st.selected = [it.value];
      st.query = "";
      cbSync(el, $el, true);
      cbClose(el, $el);
    }
  }

  // Leaving the field: free text becomes the value, otherwise the text
  // goes back to the selected item's label (an emptied field clears it).
  function cbSettle(el, $el) {
    var st = cbState(el);
    if (st.multi) {
      if (!st.free) { st.$input.val(""); st.query = ""; return; }
      var t = String(st.$input.val()).trim();
      if (t && st.selected.indexOf(t) < 0) { st.selected.push(t); st.labels[t] = t; }
      st.$input.val("");
      st.query = "";
      cbSync(el, $el, true);
      return;
    }
    var text = String(st.$input.val());
    var current = st.selected.length ? cbLabel(st, st.selected[0]) : "";
    st.query = "";
    if (text === current) { return; }
    if (text === "") {
      st.selected = [];
    } else if (st.free) {
      var exact = st.items.filter(function (it) { return it.label === text; })[0];
      st.selected = [exact ? exact.value : text];
      if (!exact) { st.labels[text] = text; }
    }
    cbSync(el, $el, true);
  }

  function cbKey(el, $el, e) {
    var st = cbState(el);
    if (cbBlocked($el)) { return; }
    switch (e.key) {
      case "ArrowDown":
        e.preventDefault();
        if (!st.open || e.altKey) { cbOpen(el, $el); } else { cbMove(el, 1); }
        break;
      case "ArrowUp":
        e.preventDefault();
        if (e.altKey) { cbClose(el, $el); } else if (st.open) { cbMove(el, -1); }
        break;
      case "Enter":
        e.preventDefault();
        if (!st.open) { cbOpen(el, $el); return; }
        if (st.active >= 0) {
          cbPick(el, $el, st.visible[st.active]);
        } else if (st.free) {
          cbSettle(el, $el);
          cbClose(el, $el);
        } else {
          var enabled = st.visible.filter(function (it) { return !it.disabled; });
          if (enabled.length === 1) { cbPick(el, $el, enabled[0]); }
        }
        break;
      case "Escape":
        if (st.open) {
          e.preventDefault();
          cbClose(el, $el);
        } else if (!st.multi) {
          st.$input.val(st.selected.length ? cbLabel(st, st.selected[0]) : "");
          st.query = "";
        }
        break;
      case "Tab":
        if (st.open && st.active >= 0 && !st.multi) {
          cbPick(el, $el, st.visible[st.active]);
        } else {
          cbClose(el, $el);
        }
        break;
      case "Backspace":
        if (st.multi && st.$input.val() === "" && st.selected.length) {
          st.selected.pop();
          cbSync(el, $el, true);
          if (st.open) { cbMark(el); cbPosition(el); }
        }
        break;
      default:
        break;
    }
  }

  function cbSetValue(el, $el, v, fire) {
    var st = cbState(el);
    if (v == null || v === "") { v = []; }
    if (!Array.isArray(v)) { v = st.multi ? String(v).split(",") : [String(v)]; }
    st.selected = v.map(String).slice(0, st.multi ? v.length : 1);
    st.query = "";
    cbSync(el, $el, fire);
    if (st.open) { cbFilter(el); } else { cbMark(el); }
  }

  AH.define("combobox", {
    init: function (el, $el) {
      ensureId(el, "ah-cb");
      var $popup = $el.children(".ah-combobox-popup");
      var st = {
        ns: ".ahcb" + (++seq),
        $input: $el.find("input.ah-combobox-input"),
        $popup: $popup,
        $list: $popup.children(".ah-combobox-list"),
        multi: $el.hasClass("ah-combobox-multiple") || $el.hasClass("ah-combobox-checkboxes"),
        free: $el.hasClass("ah-combobox-free-text"),
        remote: el.hasAttribute("data-ah-remote"),
        mode: el.getAttribute("data-ah-search-mode") || "contains_ignore_case",
        minLength: parseInt(el.getAttribute("data-ah-min-length") || "0", 10) || 0,
        empty: el.getAttribute("data-ah-empty") || "No results found",
        placeholder: el.getAttribute("data-ah-placeholder") || "",
        items: [], labels: {}, selected: [], visible: [],
        query: "", open: false, active: -1, loading: false
      };
      $.data(el, "ah-cb", st);
      if (!st.multi) { st.placeholder = st.$input.attr("placeholder") || ""; }
      cbRead(el);
      $el.find(".ah-combobox-tag-close").each(function () {
        var v = this.getAttribute("data-value");
        if (!(v in st.labels)) { st.labels[v] = $(this).siblings(".ah-combobox-tag-text").text(); }
      });
      var v = el.getAttribute("data-ah-value") || "";
      st.selected = v === "" ? [] : (st.multi ? v.split(",") : [v]);
      if (!st.multi && st.selected.length && !(st.selected[0] in st.labels)) {
        st.labels[st.selected[0]] = st.$input.val();
      }
      st.$input.attr("data-combobox", el.id).attr("aria-controls", st.$list.attr("id") || null);

      st.$input
        .on("focus" + NS, function () { $el.addClass("ah-combobox-focused"); })
        .on("blur" + NS, function () {
          $el.removeClass("ah-combobox-focused");
          // popup.cljs: close a moment later, after a click on an item
          setTimeout(function () {
            if (document.activeElement !== st.$input[0]) {
              cbClose(el, $el);
              cbSettle(el, $el);
            }
          }, 150);
        })
        .on("input" + NS, function () {
          st.query = String(st.$input.val());
          if (st.query.length < st.minLength) { cbClose(el, $el); return; }
          st.loading = st.remote;     // until set_items answers (itemsLoaded)
          cbOpen(el, $el);
        })
        .on("click" + NS, function (e) {
          e.preventDefault();
          if (st.open) { cbClose(el, $el); } else { cbOpen(el, $el); }
        })
        .on("keydown" + NS, function (e) { cbKey(el, $el, e); })
        // the text field is internal: only the root reports changes
        .on("change" + NS, function (e) { e.stopPropagation(); })
        .on("ah:error" + NS, function () {
          if (st.loading) { st.loading = false; if (st.open) { cbFilter(el); cbPosition(el); } }
        });
      $el.on("mousedown" + NS, ".ah-combobox-arrow, .ah-combobox-tag-close", function (e) {
        e.preventDefault();
      });
      $el.on("click" + NS, ".ah-combobox-arrow", function (e) {
        e.preventDefault();
        st.$input.trigger("focus");
        if (st.open) { cbClose(el, $el); } else { cbOpen(el, $el); }
      });
      $el.on("click" + NS, ".ah-combobox-tag-close", function (e) {
        e.preventDefault();
        e.stopPropagation();
        if (cbBlocked($el)) { return; }
        var i = st.selected.indexOf(this.getAttribute("data-value"));
        if (i >= 0) { st.selected.splice(i, 1); }
        cbSync(el, $el, true);
        if (st.open) { cbMark(el); cbPosition(el); }
      });
      // popup.cljs selects on mousedown, keeping the focus in the field.
      $popup.on("mousedown" + NS, function (e) { e.preventDefault(); })
        .on("mousedown" + NS, ".ah-combobox-item", function () {
          var li = this;
          cbPick(el, $el, st.visible.filter(function (it) { return it.el === li; })[0]);
        })
        .on("mouseenter" + NS, ".ah-combobox-item", function () {
          var li = this;
          cbActive(el, st.visible.map(function (it) { return it.el; }).indexOf(li));
        });
    },
    destroy: function (el) {
      var st = cbState(el);
      if (st) {
        if (st.float) { st.float.stop(); st.float = null; }
        $(document).off(st.ns);
        st.$popup.off(NS);
      }
    },
    methods: {
      // Called by aihtml_form_pickers:set_items/3,4 after it morphed the
      // server-rendered items into the list.
      itemsLoaded: function (el, $el) {
        var st = cbState(el);
        st.loading = false;
        cbRead(el);
        if (st.open || document.activeElement === st.$input[0]) {
          cbOpen(el, $el);
        } else {
          cbFilter(el);
        }
      },
      // A value or a list (multiple); no change event (the server set it).
      setValue: function (el, $el, v) { cbSetValue(el, $el, v, false); },
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      clear: function (el, $el) { cbSetValue(el, $el, [], true); },
      open: function (el, $el) { cbOpen(el, $el); },
      close: function (el, $el) { cbClose(el, $el); }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/form_select.js ---- */
/* Behaviours of the form_select components (designs/04-components.md):
   dropdownlist, slider and the validator (validate/1). Ported from sigil
   (sigil.components.form.{dropdownlist, listbox, slider, validator}). */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  function trim(s) { return String(s === null || s === undefined ? "" : s).trim(); }

  function uid(el, prefix) {
    if (!el.id) { el.id = prefix + (++seq); }
    return el.id;
  }

  // ------------------------------------------------------------------
  // Value-bearing helpers (data-ah-value + hidden input + change/input)
  // ------------------------------------------------------------------

  function writeValue($el, value) {
    $el.attr("data-ah-value", value);
    $el.children("input[type=hidden]").val(value);
  }

  // ------------------------------------------------------------------
  // dropdownlist
  // ------------------------------------------------------------------
  //
  // Keyboard (combobox pattern, extending sigil's Enter/Space/Escape/F4/
  // Alt+arrows with listbox navigation):
  //   closed: ArrowDown/ArrowUp/Enter/Space/F4/Alt+Arrow open, a letter
  //           selects the next item starting with it
  //   open:   ArrowUp/Down/Home/End/PageUp/PageDown move, Enter/Space
  //           select, Escape/Alt+Arrow/F4 close, Tab closes, letters
  //           type-ahead (or filter, when filterable)

  function ddState(el) {
    var s = $.data(el, "ah-dd");
    if (!s) {
      s = { open: false, search: "", searchAt: 0 };
      $.data(el, "ah-dd", s);
    }
    return s;
  }

  function ddItems($el, visibleOnly) {
    var $items = $el.find(".ah-listbox-item");
    return visibleOnly
      ? $items.filter(function () { return this.style.display !== "none"; })
      : $items;
  }

  function ddEnabled($items) {
    return $items.filter(function () {
      return !$(this).hasClass("ah-listbox-item-disabled");
    });
  }

  function ddDisabled($el) {
    return $el.hasClass("ah-dropdownlist-disabled");
  }

  function ddSetActive($el, item) {
    var $items = ddItems($el);
    $items.removeClass("ah-listbox-item-focused");
    var $filter = $el.find(".ah-listbox-filter-input");
    if (!item) {
      $el.removeAttr("aria-activedescendant");
      $filter.removeAttr("aria-activedescendant");
      return;
    }
    $(item).addClass("ah-listbox-item-focused");
    $el.attr("aria-activedescendant", item.id);
    $filter.attr("aria-activedescendant", item.id);
    if (item.scrollIntoView) { item.scrollIntoView({ block: "nearest" }); }
  }

  function ddActive($el) {
    return $el.find(".ah-listbox-item-focused")[0] || null;
  }

  // sigil's -above / -below classes, from the side AH.float chose.
  function ddPlacement($el) {
    var $popup = $el.children(".ah-dropdownlist-popup");
    var above = $popup.attr("data-ah-placement") === "top";
    $popup.toggleClass("ah-dropdownlist-popup-above", above)
      .toggleClass("ah-dropdownlist-popup-below", !above);
  }

  function ddOpen(el, $el) {
    var s = ddState(el);
    if (s.open || ddDisabled($el)) { return; }
    s.open = true;
    $el.addClass("ah-dropdownlist-open ah-dropdownlist-state-selected")
      .attr("aria-expanded", "true");
    var $popup = $el.children(".ah-dropdownlist-popup").addClass("ah-dropdownlist-popup-open");
    // Shared positioning: fixed, at least the root's width, flips above
    // when there is no room below, follows scroll and resize.
    s.float = AH.float($popup[0], el, { placement: "bottom", align: "start", offset: 4,
                                        matchWidth: true });
    ddPlacement($el);
    var $sel = ddItems($el).filter(".ah-listbox-item-selected");
    ddSetActive($el, $sel[0] || ddEnabled(ddItems($el, true))[0]);
    var $filter = $el.find(".ah-listbox-filter-input");
    if ($filter.length) { $filter.trigger("focus"); }
    $el.trigger("ah:open");
  }

  function ddClose(el, $el, refocus) {
    var s = ddState(el);
    if (!s.open) { return; }
    s.open = false;
    $el.removeClass("ah-dropdownlist-open ah-dropdownlist-state-selected")
      .attr("aria-expanded", "false");
    if (s.float) { s.float.stop(); s.float = null; }
    $el.children(".ah-dropdownlist-popup")
      .removeClass("ah-dropdownlist-popup-open ah-dropdownlist-popup-above ah-dropdownlist-popup-below");
    ddSetActive($el, null);
    var $filter = $el.find(".ah-listbox-filter-input");
    if ($filter.length && $filter.val()) {
      $filter.val("");
      ddFilter($el, "");
    }
    if (refocus && el.contains(document.activeElement) && document.activeElement !== el) {
      el.focus();
    }
    $el.trigger("ah:close");
  }

  function ddLabel(item) {
    var l = $(item).find(".ah-listbox-label")[0];
    return trim((l || item).textContent);
  }

  // Select an item (null clears). Fires change when the value changes.
  function ddSelect(el, $el, item, silent) {
    var value = item ? item.getAttribute("data-value") : "";
    var old = $el.attr("data-ah-value") || "";
    var $items = ddItems($el);
    $items.removeClass("ah-listbox-item-selected").attr("aria-selected", "false");
    var $content = $el.find(".ah-dropdownlist-content");
    if (item) {
      $(item).addClass("ah-listbox-item-selected").attr("aria-selected", "true");
      $content.text(ddLabel(item)).removeClass("ah-dropdownlist-content-placeholder");
    } else {
      $content.text($el.attr("data-ah-placeholder") || "")
        .addClass("ah-dropdownlist-content-placeholder");
    }
    writeValue($el, value);
    if (!silent && value !== old) {
      $el.trigger("change", [{ value: value, label: item ? ddLabel(item) : null }]);
    }
  }

  function ddFilter($el, text) {
    var q = trim(text).toLowerCase();
    ddItems($el).each(function () {
      this.style.display = !q || ddLabel(this).toLowerCase().indexOf(q) >= 0 ? "" : "none";
    });
    $el.find(".ah-listbox-group").each(function () {
      var any = $(this).nextUntil(".ah-listbox-group").filter(function () {
        return this.style.display !== "none";
      }).length > 0;
      this.style.display = any ? "" : "none";
    });
    ddSetActive($el, ddEnabled(ddItems($el, true))[0]);
    var s = $.data($el[0], "ah-dd");
    if (s && s.float) { s.float.update(); ddPlacement($el); }
  }

  // Type-ahead: letters typed within 800 ms form one prefix (sigil's
  // incremental search). Returns the matching item after the current one.
  function ddTypeahead(el, $el, key) {
    var s = ddState(el);
    var now = Date.now();
    s.search = now - s.searchAt > 800 ? key : s.search + key;
    s.searchAt = now;
    var q = s.search.toLowerCase();
    var items = ddEnabled(ddItems($el, true)).get();
    var cur = s.open ? ddActive($el) : ddItems($el).filter(".ah-listbox-item-selected")[0];
    var start = s.search.length === 1 ? items.indexOf(cur) + 1 : Math.max(items.indexOf(cur), 0);
    for (var i = 0; i < items.length; i++) {
      var it = items[(start + i) % items.length];
      if (ddLabel(it).toLowerCase().indexOf(q) === 0) { return it; }
    }
    return null;
  }

  function ddKeydown(el, $el, e) {
    if (ddDisabled($el)) { return; }
    var s = ddState(el);
    var key = e.key;
    var inFilter = $(e.target).hasClass("ah-listbox-filter-input");
    var toggle = key === "F4" || (e.altKey && (key === "ArrowDown" || key === "ArrowUp"));
    if (!s.open) {
      if (toggle || key === "ArrowDown" || key === "ArrowUp" || key === "Enter" || key === " ") {
        e.preventDefault();
        ddOpen(el, $el);
      } else if (key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
        var hit = ddTypeahead(el, $el, key);
        if (hit) { ddSelect(el, $el, hit); }
      }
      return;
    }
    var items = ddEnabled(ddItems($el, true)).get();
    var idx = items.indexOf(ddActive($el));
    var move = function (i) {
      e.preventDefault();
      if (items.length) { ddSetActive($el, items[Math.max(0, Math.min(items.length - 1, i))]); }
    };
    if (toggle || key === "Escape") {
      e.preventDefault();
      ddClose(el, $el, true);
    } else if (key === "ArrowDown") {
      move(idx + 1);
    } else if (key === "ArrowUp") {
      move(idx < 0 ? items.length - 1 : idx - 1);
    } else if (key === "Home" && !inFilter) {
      move(0);
    } else if (key === "End" && !inFilter) {
      move(items.length - 1);
    } else if (key === "PageDown") {
      move(idx + 10);
    } else if (key === "PageUp") {
      move(idx - 10);
    } else if (key === "Enter" || (key === " " && !inFilter)) {
      e.preventDefault();
      if (idx >= 0) { ddSelect(el, $el, items[idx]); }
      ddClose(el, $el, true);
    } else if (key === "Tab") {
      ddClose(el, $el, false);
    } else if (!inFilter && key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
      var hit = ddTypeahead(el, $el, key);
      if (hit) { ddSetActive($el, hit); }
    }
  }

  AH.define("dropdownlist", {
    init: function (el, $el) {
      var id = uid(el, "ah-dd");
      var $list = $el.find(".ah-listbox-list");
      $list.attr("id", id + "-list");
      $el.attr("aria-controls", id + "-list");
      ddItems($el).each(function () {
        this.id = id + "-opt-" + this.getAttribute("data-idx");
      });
      var ns = ".ahdd" + id.replace(/[^\w-]/g, "_");
      $.data(el, "ah-dd-ns", ns);

      $el.on("click" + NS, ".ah-dropdownlist-input-area", function () {
        if (ddState(el).open) { ddClose(el, $el, true); } else { ddOpen(el, $el); }
      });
      $el.on("click" + NS, ".ah-listbox-item", function (e) {
        e.stopPropagation();
        if ($(this).hasClass("ah-listbox-item-disabled")) { return; }
        ddSelect(el, $el, this);
        ddClose(el, $el, true);
      });
      // Keep focus on the combobox while the pointer is in the popup.
      $el.on("mousedown" + NS, ".ah-dropdownlist-popup", function (e) {
        if (!$(e.target).hasClass("ah-listbox-filter-input")) { e.preventDefault(); }
      });
      $el.on("mousemove" + NS, ".ah-listbox-item", function () {
        if (!$(this).hasClass("ah-listbox-item-disabled") && ddActive($el) !== this) {
          ddSetActive($el, this);
        }
      });
      $el.on("keydown" + NS, function (e) { ddKeydown(el, $el, e); });
      $el.on("input" + NS, ".ah-listbox-filter-input", function (e) {
        e.stopPropagation();           // not the component's own input event
        ddFilter($el, this.value);
      });
      $el.on("change" + NS, ".ah-listbox-filter-input", function (e) { e.stopPropagation(); });
      $el.on("focusin" + NS, function () { $el.addClass("ah-dropdownlist-focused"); });
      $el.on("focusout" + NS, function (e) {
        if (!e.relatedTarget || !el.contains(e.relatedTarget)) {
          $el.removeClass("ah-dropdownlist-focused");
          ddClose(el, $el, false);
        }
      });
      $(document).on("mousedown" + ns, function (e) {
        if (ddState(el).open && !el.contains(e.target)) { ddClose(el, $el, false); }
      });
    },
    destroy: function (el, $el) {
      ddClose(el, $el, false);
      $(document).off($.data(el, "ah-dd-ns"));
    },
    methods: {
      open: function (el, $el) { ddOpen(el, $el); },
      close: function (el, $el) { ddClose(el, $el, false); },
      getValue: function (el, $el) { return $el.attr("data-ah-value") || ""; },
      // setValue(value[, silent]): select the item with that value
      // ("" or null clears); fires change unless silent.
      setValue: function (el, $el, value, silent) {
        var v = value === null || value === undefined ? "" : String(value);
        var item = ddItems($el).filter(function () {
          return this.getAttribute("data-value") === v;
        })[0] || null;
        ddSelect(el, $el, item, silent);
      },
      disable: function (el, $el) {
        ddClose(el, $el, false);
        $el.addClass("ah-dropdownlist-disabled").attr({ "aria-disabled": "true", tabindex: "-1" });
      },
      enable: function (el, $el) {
        $el.removeClass("ah-dropdownlist-disabled").removeAttr("aria-disabled").attr("tabindex", "0");
      }
    }
  });

  // ------------------------------------------------------------------
  // slider
  // ------------------------------------------------------------------
  //
  // Positions are written as calc() fractions of the track (as the server
  // renders them), so nothing needs re-measuring on resize. Keyboard, per
  // sigil: Left/Down decrease, Right/Up increase, Home/End; plus
  // PageUp/PageDown by ten steps. Buttons step like sigil's; the wheel
  // steps while the slider has focus.

  var THUMB = 18;

  function slConf(el, $el) {
    var c = $.data(el, "ah-slider");
    if (!c) {
      var step = parseFloat($el.attr("data-ah-step")) || 1;
      c = {
        min: parseFloat($el.attr("data-ah-min")) || 0,
        max: parseFloat($el.attr("data-ah-max")),
        step: step,
        decimals: (String(step).split(".")[1] || "").length,
        minRange: parseFloat($el.attr("data-ah-min-range")) || 0,
        vertical: $el.hasClass("ah-slider-vertical"),
        range: $el.hasClass("ah-slider-range-slider")
      };
      if (isNaN(c.max)) { c.max = 100; }
      $.data(el, "ah-slider", c);
    }
    return c;
  }

  function slValues($el) {
    return String($el.attr("data-ah-value") || "").split(",").map(parseFloat);
  }

  function slSnap(c, v) {
    var n = Math.round((v - c.min) / c.step);
    var snapped = c.min + n * c.step;
    snapped = Math.max(c.min, Math.min(c.max, snapped));
    return parseFloat(snapped.toFixed(c.decimals));
  }

  function frac(r) {
    return "calc((100% - " + THUMB + "px) * " + (+r.toFixed(4)) + ")";
  }

  function fracCenter(r) {
    return "calc((100% - " + THUMB + "px) * " + (+r.toFixed(4)) + " + " + THUMB / 2 + "px)";
  }

  function slRender(el, $el, vals) {
    var c = slConf(el, $el);
    var ratio = function (v) { return (v - c.min) / (c.max - c.min); };
    var $range = $el.find(".ah-slider-range");
    var $end = $el.find(".ah-slider-thumb-end");
    var $start = $el.find(".ah-slider-thumb-start");
    var pos = function ($t, v) {
      if (c.vertical) { $t.css("top", frac(1 - ratio(v))); } else { $t.css("left", frac(ratio(v))); }
    };
    if (c.range) {
      pos($start, vals[0]);
      pos($end, vals[1]);
      if (c.vertical) {
        $range.css({ bottom: fracCenter(ratio(vals[0])), height: frac(ratio(vals[1]) - ratio(vals[0])) });
      } else {
        $range.css({ left: fracCenter(ratio(vals[0])), width: frac(ratio(vals[1]) - ratio(vals[0])) });
      }
      $start.attr({ "aria-valuenow": vals[0], "aria-valuetext": vals[0] });
      $end.attr({ "aria-valuenow": vals[1], "aria-valuetext": vals[1] });
    } else {
      pos($end, vals[0]);
      if (c.vertical) { $range.css({ bottom: 0, height: fracCenter(ratio(vals[0])) }); }
      else { $range.css({ left: 0, width: fracCenter(ratio(vals[0])) }); }
      $el.attr({ "aria-valuenow": vals[0], "aria-valuetext": vals[0] });
    }
    writeValue($el, vals.join(","));
    slTooltip(el, $el);
  }

  function slTooltip(el, $el) {
    var $tip = $el.children(".ah-slider-tooltip");
    var s = $.data(el, "ah-slider-state") || {};
    if (!$tip.length) { return; }
    var thumb = s.thumb === "start" ? $el.find(".ah-slider-thumb-start")[0]
      : $el.find(".ah-slider-thumb-end")[0];
    var vals = slValues($el);
    $tip.text(s.thumb === "start" ? vals[0] : vals[vals.length - 1]);
    var root = el.getBoundingClientRect();
    var r = thumb.getBoundingClientRect();
    if (slConf(el, $el).vertical) {
      $tip.css("top", (r.top + r.height / 2 - root.top) + "px");
    } else {
      $tip.css("left", (r.left + r.width / 2 - root.left) + "px");
    }
  }

  // Set one thumb ("start" | "end") to v, keeping the range ordered.
  function slSet(el, $el, which, v) {
    var c = slConf(el, $el);
    var vals = slValues($el);
    v = slSnap(c, v);
    if (c.range) {
      if (which === "start") { vals[0] = Math.min(v, vals[1] - c.minRange); }
      else { vals[1] = Math.max(v, vals[0] + c.minRange); }
      vals[0] = Math.max(c.min, vals[0]);
      vals[1] = Math.min(c.max, vals[1]);
    } else {
      vals = [v];
    }
    var old = $el.attr("data-ah-value");
    slRender(el, $el, vals);
    return $el.attr("data-ah-value") !== old;
  }

  function slFromPointer(el, $el, e) {
    var c = slConf(el, $el);
    var r = $el.find(".ah-slider-track")[0].getBoundingClientRect();
    var ratio = c.vertical
      ? 1 - (e.clientY - r.top - THUMB / 2) / (r.height - THUMB)
      : (e.clientX - r.left - THUMB / 2) / (r.width - THUMB);
    ratio = Math.max(0, Math.min(1, ratio));
    return c.min + ratio * (c.max - c.min);
  }

  function slDisabled($el) {
    return $el.hasClass("ah-slider-disabled") || $el.attr("aria-disabled") === "true";
  }

  function slShowTip($el, on) {
    $el.children(".ah-slider-tooltip").toggleClass("ah-slider-tooltip-visible", on);
  }

  function slStep(el, $el, which, delta) {
    var c = slConf(el, $el);
    var vals = slValues($el);
    var cur = c.range ? (which === "start" ? vals[0] : vals[1]) : vals[0];
    if (slSet(el, $el, which, cur + delta)) {
      $el.trigger("input").trigger("change");
    }
  }

  AH.define("slider", {
    init: function (el, $el) {
      var c = slConf(el, $el);
      var state = {};
      $.data(el, "ah-slider-state", state);

      $el.on("pointerdown" + NS, ".ah-slider-content", function (e) {
        if (slDisabled($el) || e.button !== 0) { return; }
        e.preventDefault();
        var $thumb = $(e.target).closest(".ah-slider-thumb");
        var v = slFromPointer(el, $el, e);
        var which = "end";
        if (c.range) {
          if ($thumb.length) {
            which = $thumb.hasClass("ah-slider-thumb-start") ? "start" : "end";
          } else {
            var vals = slValues($el);
            which = Math.abs(v - vals[0]) <= Math.abs(v - vals[1]) ? "start" : "end";
          }
        }
        state.dragging = true;
        state.thumb = which;
        state.startValue = $el.attr("data-ah-value");
        var $t = $el.find(".ah-slider-thumb-" + which).addClass("ah-slider-thumb-dragging");
        (c.range ? $t[0] : el).focus({ preventScroll: true });
        slShowTip($el, true);
        try { el.setPointerCapture(e.pointerId); } catch (err) { /* synthetic event */ }
        // Pressing the track jumps the nearest thumb there.
        if (!$thumb.length && slSet(el, $el, which, v)) { $el.trigger("input"); }
        slTooltip(el, $el);
      });
      $el.on("pointermove" + NS, function (e) {
        if (!state.dragging) { return; }
        if (slSet(el, $el, state.thumb, slFromPointer(el, $el, e))) { $el.trigger("input"); }
      });
      $el.on("pointerup" + NS + " pointercancel" + NS, function (e) {
        if (!state.dragging) { return; }
        state.dragging = false;
        $el.find(".ah-slider-thumb").removeClass("ah-slider-thumb-dragging");
        try { el.releasePointerCapture(e.pointerId); } catch (err) { /* not captured */ }
        if (!el.contains(document.activeElement)) { slShowTip($el, false); }
        if ($el.attr("data-ah-value") !== state.startValue) { $el.trigger("change"); }
      });

      $el.on("click" + NS, ".ah-slider-button", function () {
        if (slDisabled($el)) { return; }
        var inc = $(this).hasClass("ah-slider-button-next");
        // sigil: in range mode "+" moves the end thumb, "-" the start one
        slStep(el, $el, c.range ? (inc ? "end" : "start") : "end", inc ? c.step : -c.step);
      });

      $el.on("keydown" + NS, function (e) {
        if (slDisabled($el)) { return; }
        var which = "end";
        if (c.range) {
          var $t = $(e.target).closest(".ah-slider-thumb");
          if (!$t.length) { return; }
          which = $t.hasClass("ah-slider-thumb-start") ? "start" : "end";
        }
        state.thumb = which;
        var big = c.step * Math.max(1, Math.round((c.max - c.min) / c.step / 10));
        var delta = { ArrowRight: c.step, ArrowUp: c.step, ArrowLeft: -c.step, ArrowDown: -c.step,
                      PageUp: big, PageDown: -big }[e.key];
        if (delta !== undefined) {
          e.preventDefault();
          slShowTip($el, true);
          slStep(el, $el, which, delta);
        } else if (e.key === "Home" || e.key === "End") {
          e.preventDefault();
          slShowTip($el, true);
          if (slSet(el, $el, which, e.key === "Home" ? c.min : c.max)) {
            $el.trigger("input").trigger("change");
          }
        }
      });

      el.addEventListener("wheel", state.wheel = function (e) {
        if (slDisabled($el) || !el.contains(document.activeElement)) { return; }
        e.preventDefault();
        var which = c.range && $(document.activeElement).hasClass("ah-slider-thumb-start")
          ? "start" : "end";
        slStep(el, $el, which, e.deltaY < 0 ? c.step : -c.step);
      }, { passive: false });

      $el.on("focusin" + NS, function (e) {
        $el.addClass("ah-slider-focused");
        if (c.range) {
          state.thumb = $(e.target).hasClass("ah-slider-thumb-start") ? "start" : "end";
        }
        slTooltip(el, $el);
      });
      $el.on("focusout" + NS, function (e) {
        if (!e.relatedTarget || !el.contains(e.relatedTarget)) {
          $el.removeClass("ah-slider-focused");
          if (!state.dragging) { slShowTip($el, false); }
        }
      });
    },
    destroy: function (el) {
      var s = $.data(el, "ah-slider-state");
      if (s && s.wheel) { el.removeEventListener("wheel", s.wheel); }
      $.removeData(el, "ah-slider");
      $.removeData(el, "ah-slider-state");
    },
    methods: {
      getValue: function (el, $el) { return $el.attr("data-ah-value"); },
      // setValue(v | [lo, hi] | "lo,hi"[, silent])
      setValue: function (el, $el, v, silent) {
        var c = slConf(el, $el);
        var vals = Array.isArray(v) ? v : String(v).split(",");
        vals = vals.map(function (x) { return slSnap(c, parseFloat(x)); });
        if (c.range) { vals = [Math.min(vals[0], vals[1]), Math.max(vals[0], vals[1])]; }
        var old = $el.attr("data-ah-value");
        slRender(el, $el, vals);
        if (!silent && $el.attr("data-ah-value") !== old) { $el.trigger("change"); }
      }
    }
  });

  // ------------------------------------------------------------------
  // validator (aihtml_form_select:validate/1)
  // ------------------------------------------------------------------
  //
  // Controls carry data-ah-validate='[{"rule":"required"}, ...]'. They are
  // checked on their trigger events (default blur) and, once invalid, on
  // every input/change until fixed. A form holding such controls is
  // checked on submit, and on a click of its submit button: when a check
  // fails the event is stopped in the capture phase, so neither the
  // browser, data-ah-fetch nor an aihtml action (on/2) sees it.
  //
  // Errors show as in sigil: the error class on the control plus either a
  // tooltip bubble (.ah-validator-hint) or an error label. Inside a
  // field/4 row ("auto", the default) the label goes under the control.
  //
  //   ah:validation-error {invalid: [el]} / ah:validation-success  on the form
  //   AH.fn("validate", target) -> bool     check a form or control
  //   AH.fn("clearValidation", target)      remove every message

  var MESSAGES = {
    required: "This field is required",
    email: "Please enter a valid email address",
    number: "Please enter a number",
    integer: "Please enter a whole number",
    phone: "Please enter a phone number like (555)555-5555",
    zip_code: "Please enter a valid ZIP code",
    ssn: "Please enter a valid SSN",
    not_number: "Digits are not allowed",
    starts_with_letter: "Must start with a letter",
    min_length: "Please enter at least {0} characters",
    max_length: "Please enter at most {0} characters",
    length: "Please enter {0} to {1} characters",
    min: "Must be at least {0}",
    max: "Must be at most {0}",
    range: "Must be between {0} and {1}",
    pattern: "Please match the requested format",
    same_as: "The values do not match"
  };

  function fmt(s, args) {
    return s.replace(/\{(\d)\}/g, function (_, i) { return args[+i]; });
  }

  function isNative(el) {
    return /^(INPUT|SELECT|TEXTAREA)$/.test(el.tagName);
  }

  function valueOf(el) {
    if (isNative(el)) {
      var v = $(el).val();
      return Array.isArray(v) ? v.join(",") : String(v === null || v === undefined ? "" : v);
    }
    return el.getAttribute("data-ah-value") || "";
  }

  function blank(s) { return trim(s) === ""; }

  var RULES = {
    required: function (v, el) {
      if (el.type === "checkbox") { return el.checked; }
      if (el.type === "radio") {
        return !!(el.form || document).querySelector(
          "input[type=radio][name=\"" + CSS.escape(el.name) + "\"]:checked");
      }
      return !blank(v);
    },
    email: function (v) { return blank(v) || /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(trim(v)); },
    number: function (v) { return blank(v) || isFinite(Number(trim(v))); },
    integer: function (v) { return blank(v) || /^[-+]?\d+$/.test(trim(v)); },
    phone: function (v) { return blank(v) || /^\(\d{3}\)\d{3}-\d{4}$/.test(trim(v)); },
    zip_code: function (v) { return blank(v) || /^(\d{5})(-\d{4})?$/.test(trim(v)); },
    ssn: function (v) { return blank(v) || /^\d{3}-\d{2}-\d{4}$/.test(trim(v)); },
    not_number: function (v) { return blank(v) || !/\d/.test(v); },
    starts_with_letter: function (v) { return blank(v) || /^[a-zA-Z]/.test(trim(v)); },
    min_length: function (v, _el, a) { return blank(v) || v.length >= a[0]; },
    max_length: function (v, _el, a) { return v.length <= a[0]; },
    length: function (v, _el, a) { return blank(v) || (v.length >= a[0] && v.length <= a[1]); },
    min: function (v, _el, a) { return blank(v) || Number(v) >= a[0]; },
    max: function (v, _el, a) { return blank(v) || Number(v) <= a[0]; },
    range: function (v, _el, a) {
      return blank(v) || (Number(v) >= a[0] && Number(v) <= a[1]);
    },
    pattern: function (v, _el, a) { return blank(v) || new RegExp("^(?:" + a[0] + ")$").test(v); },
    same_as: function (v, _el, a) {
      var other = $(a[0])[0];
      return !other || valueOf(other) === v;
    }
  };

  function rulesOf(el) {
    var r = $.data(el, "ah-rules");
    if (!r) {
      try { r = JSON.parse(el.getAttribute("data-ah-validate") || "[]"); }
      catch (err) { r = []; }
      $.data(el, "ah-rules", r);
    }
    return r;
  }

  // The first failing rule's message, or null.
  function failure(el) {
    var v = valueOf(el);
    var rules = rulesOf(el);
    for (var i = 0; i < rules.length; i++) {
      var r = rules[i];
      var f = RULES[r.rule];
      if (f && !f(v, el, r.args || [])) {
        return r.msg || fmt(MESSAGES[r.rule] || "Invalid value", r.args || []);
      }
    }
    return null;
  }

  function hintMode(el) {
    var mode = el.getAttribute("data-ah-validate-hint") || "auto";
    if (mode === "auto") {
      return $(el).closest(".ah-form-body").length ? "field" : "tooltip";
    }
    return mode;
  }

  function hideHint(el) {
    var $el = $(el);
    $el.removeClass("ah-validator-error-element").removeAttr("aria-invalid");
    var hint = $.data(el, "ah-hint");
    if (hint) {
      removeHint(hint);
      $.removeData(el, "ah-hint");
    }
    var $body = $el.closest(".ah-form-body");
    if ($body.length && !$body.find(".ah-validator-error-element").length) {
      $body.children(".ah-form-error").remove();
      $body.closest(".ah-form-row-invalid").removeClass("ah-form-row-invalid");
    }
    var desc = $el.attr("aria-describedby");
    if (desc && /ah-vh\d+/.test(desc)) {
      desc = trim(desc.replace(/\bah-vh\d+\b/g, ""));
      if (desc) { $el.attr("aria-describedby", desc); } else { $el.removeAttr("aria-describedby"); }
    }
  }

  // The bubble is anchored with AH.float (flips when out of room, follows
  // scrolling); extra/form_select.css points its arrow back at the control
  // from the side in data-ah-placement.
  function floatTooltip(el, hint) {
    var pos = el.getAttribute("data-ah-validate-position") || "right";
    $.data(hint, "ah-float", AH.float(hint, el, { placement: pos, align: "center", offset: 8 }));
  }

  function removeHint(hint) {
    var f = $.data(hint, "ah-float");
    if (f) { f.stop(); }
    $(hint).remove();
  }

  function showHint(el, message) {
    hideHint(el);
    var $el = $(el);
    var id = "ah-vh" + (++seq);
    $el.addClass("ah-validator-error-element").attr("aria-invalid", "true");
    $el.attr("aria-describedby", trim(($el.attr("aria-describedby") || "") + " " + id));
    var mode = hintMode(el);
    var $hint;
    if (mode === "field") {
      var $body = $el.closest(".ah-form-body");
      $body.children(".ah-form-error").remove();
      $hint = $("<div class=\"ah-form-error ah-validator-error-label\" role=\"alert\"></div>")
        .attr("id", id).text(message).appendTo($body);
      $body.closest(".ah-form-row, .ah-form-col").addClass("ah-form-row-invalid");
      $.data(el, "ah-hint", $hint[0]);
      return;
    }
    if (mode === "label") {
      $hint = $("<label class=\"ah-validator-error-label\" role=\"alert\"></label>")
        .attr({ id: id, "for": el.id || null }).text(message);
      if (el.getAttribute("data-ah-validate-position") === "top") { $hint.insertBefore(el); }
      else { $hint.insertAfter(el); }
      $.data(el, "ah-hint", $hint[0]);
      return;
    }
    $hint = $("<div class=\"ah-validator-hint\" role=\"alert\">" +
              "<div class=\"ah-validator-arrow\"></div></div>")
      .attr("id", id).append(document.createTextNode(message)).appendTo(document.body);
    $hint.data("ah-owner", el);
    floatTooltip(el, $hint[0]);
    $hint.addClass("ah-validator-hint-visible");
    $hint.on("click", function () { hideHint(el); });     // sigil: click closes
    $.data(el, "ah-hint", $hint[0]);
  }

  // Bubbles whose control has left the page (replaced by an action).
  function sweep() {
    $(".ah-validator-hint").each(function () {
      var owner = $(this).data("ah-owner");
      if (!owner || !document.body.contains(owner)) { removeHint(this); }
    });
  }

  function skip(el) {
    return el.disabled || el.type === "hidden" || !$(el).is(":visible");
  }

  function checkOne(el) {
    sweep();
    if (skip(el)) { hideHint(el); return true; }
    var msg = failure(el);
    if (msg) { showHint(el, msg); } else { hideHint(el); }
    return !msg;
  }

  function checkAll(scope) {
    var invalid = [];
    $(scope).find("[data-ah-validate]").addBack("[data-ah-validate]").each(function () {
      if (!checkOne(this)) { invalid.push(this); }
    });
    var $scope = $(scope);
    if (invalid.length) {
      var first = invalid[0];
      if (first.scrollIntoView) { first.scrollIntoView({ block: "nearest", behavior: "smooth" }); }
      first.focus({ preventScroll: true });
      $scope.trigger("ah:validation-error", [{ invalid: invalid }]);
    } else {
      $scope.trigger("ah:validation-success");
    }
    return invalid.length === 0;
  }

  function guarded(form) {
    return form && !form.hasAttribute("data-ah-novalidate") &&
      form.querySelector("[data-ah-validate]");
  }

  function block(e) {
    e.preventDefault();
    e.stopImmediatePropagation();
  }

  // Capture phase on document: runs before the delegated handlers of
  // core.js (actions, fetch), which listen in the bubble phase.
  document.addEventListener("submit", function (e) {
    var form = e.target;
    if (!guarded(form) || (e.submitter && e.submitter.formNoValidate)) { return; }
    if (!checkAll(form)) { block(e); }
  }, true);

  document.addEventListener("click", function (e) {
    var btn = e.target.closest && e.target.closest("button, input[type=submit], input[type=image]");
    if (!btn || btn.type !== "submit" && btn.type !== "image") { return; }
    if (btn.formNoValidate || !guarded(btn.form)) { return; }
    if (!checkAll(btn.form)) { block(e); }
  }, true);

  function triggers(el) {
    return (el.getAttribute("data-ah-validate-on") || "blur").split(/\s+/);
  }

  $(document).on("focusout" + NS, "[data-ah-validate]", function (e) {
    if (e.relatedTarget && this.contains(e.relatedTarget)) { return; }
    if (triggers(this).indexOf("blur") >= 0) { checkOne(this); }
  });
  $(document).on("input" + NS + " change" + NS, "[data-ah-validate]", function (e) {
    if (e.target !== this && !isNative(e.target) && e.type === "input") { return; }
    if (triggers(this).indexOf(e.type) >= 0 || $(this).hasClass("ah-validator-error-element")) {
      checkOne(this);
    }
  });

  AH.fn("validate", function (target) { return checkAll($(target)[0] || document.body); });
  AH.fn("clearValidation", function (target) {
    $(target || document.body).find("[data-ah-validate]").addBack("[data-ah-validate]")
      .each(function () { hideHint(this); });
    sweep();
  });
})(window.jQuery, window.AH);

/* ---- components/form_text.js ---- */
/* Behaviours of the form_text components (designs/04-components.md).
 * Ported from sigil: form/input, form/password_input, form/number_input,
 * form/input_otp, form/tag_input. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  // Floating label: up while focused or filled (sigil util/sync-float-label!).
  function syncLabel($el, prefix, filled, focused) {
    $el.find("." + prefix + "-label").toggleClass(prefix + "-label-float", !!(filled || focused));
  }

  // Focus class on the shell plus the floating label (sigil shell).
  function focusShell($el, $input, prefix, after) {
    $input.on("focus" + NS, function () {
      $el.addClass(prefix + "-focused");
      syncLabel($el, prefix, true, true);
    }).on("blur" + NS, function () {
      $el.removeClass(prefix + "-focused");
      syncLabel($el, prefix, $input.val() !== "", false);
      if (after) { after(); }
    });
  }

  // ------------------------------------------------------------------
  // input / textarea
  // ------------------------------------------------------------------

  function inputOf($el) {
    return $el.find("input.ah-input, textarea.ah-input").first();
  }

  function syncInput($el, $input) {
    var filled = $input.val() !== "";
    $el.toggleClass("ah-input-has-value", filled);
    syncLabel($el, "ah-input", filled, $input.is(":focus"));
  }

  function clearInput($el, $input) {
    if ($input.val() === "") { return; }
    $input.val("");
    syncInput($el, $input);
    // Native events, so on(input|change, ...) on the <input> hears them.
    $input.trigger("input").trigger("change");
  }

  AH.define("input", {
    init: function (el, $el) {
      var $input = inputOf($el);
      focusShell($el, $input, "ah-input");
      $input.on("input" + NS, function () { syncInput($el, $input); });
      $el.on("click" + NS, ".ah-input-clear", function (e) {
        e.preventDefault();
        clearInput($el, $input);
        $input.trigger("focus");
      });
      if ($el.hasClass("ah-input-clearable")) {
        $input.on("keydown" + NS, function (e) {
          if (e.key === "Escape" && $input.val() !== "") {
            e.preventDefault();
            clearInput($el, $input);
          }
        });
      }
      syncInput($el, $input);
    },
    methods: {
      getValue: function (el, $el) { return inputOf($el).val(); },
      setValue: function (el, $el, v) {
        var $input = inputOf($el);
        $input.val(v == null ? "" : String(v));
        syncInput($el, $input);
      },
      clear: function (el, $el) { clearInput($el, inputOf($el)); },
      focus: function (el, $el) { inputOf($el).trigger("focus"); },
      selectAll: function (el, $el) {
        var $input = inputOf($el);
        $input.trigger("focus");
        if ($input[0]) { $input[0].select(); }
      }
    }
  });

  // ------------------------------------------------------------------
  // password_input
  // ------------------------------------------------------------------

  var SPECIALS = "<>@!#$%^&*()_+[]{}?:;|'\"\\,./~`-=";

  // sigil password-input.strength/evaluate
  function strength(pw) {
    if (pw.length < 8) { return "too-short"; }
    var letters = 0, numbers = 0, specials = 0;
    for (var i = 0; i < pw.length; i++) {
      var c = pw.charCodeAt(i), ch = pw.charAt(i);
      if ((c >= 65 && c <= 90) || (c >= 97 && c <= 122) ||
          (c >= 128 && c <= 154) || (c >= 160 && c <= 165)) { letters++; }
      else if (c >= 48 && c <= 57) { numbers++; }
      else if (SPECIALS.indexOf(ch) >= 0) { specials++; }
    }
    var score = letters + numbers + 2 * specials + letters * numbers / 2 + pw.length;
    return score < 20 ? "weak" : score < 30 ? "fair" : score < 40 ? "good" : "strong";
  }

  var STRENGTH = {
    "too-short": ["Too short", "20%", "var(--ah-color-error)"],
    weak: ["Weak", "40%", "var(--ah-color-error)"],
    fair: ["Fair", "60%", "var(--ah-color-warning)"],
    good: ["Good", "80%", "var(--ah-color-info)"],
    strong: ["Strong", "100%", "var(--ah-color-success)"]
  };

  function updateStrength($el, pw) {
    var $fill = $el.find(".ah-pwd-strength-fill");
    var $text = $el.find(".ah-pwd-strength-text");
    if (!$fill.length) { return; }
    if (!pw) {
      $fill.css({ width: "0", backgroundColor: "transparent" });
      $text.text("");
      $el.removeAttr("data-strength");
      return;
    }
    var level = strength(pw), d = STRENGTH[level];
    $fill.css({ width: d[1], backgroundColor: d[2] });
    $text.text(d[0]);
    $el.attr("data-strength", level);
  }

  function pwdInput($el) { return $el.find("input.ah-pwd").first(); }

  function setVisible($el, show) {
    var $input = pwdInput($el);
    $el.toggleClass("ah-pwd-visible", show);
    $input.attr("type", show ? "text" : "password");
    $el.find(".ah-pwd-toggle")
      .attr("aria-pressed", show ? "true" : "false")
      .attr("aria-label", show ? "Hide password" : "Show password");
  }

  AH.define("password-input", {
    init: function (el, $el) {
      var $input = pwdInput($el);
      focusShell($el, $input, "ah-pwd");
      $input.on("input" + NS, function () {
        updateStrength($el, $input.val());
        syncLabel($el, "ah-pwd", $input.val() !== "", true);
      });
      // mousedown: keep the focus (and caret) in the field
      $el.on("mousedown" + NS, ".ah-pwd-toggle", function (e) { e.preventDefault(); });
      $el.on("click" + NS, ".ah-pwd-toggle", function (e) {
        e.preventDefault();
        setVisible($el, !$el.hasClass("ah-pwd-visible"));
      });
      updateStrength($el, $input.val());
    },
    methods: {
      getValue: function (el, $el) { return pwdInput($el).val(); },
      setValue: function (el, $el, v) {
        var $input = pwdInput($el);
        $input.val(v == null ? "" : String(v));
        updateStrength($el, $input.val());
        syncLabel($el, "ah-pwd", $input.val() !== "", false);
      },
      toggle: function (el, $el, show) {
        setVisible($el, show === undefined ? !$el.hasClass("ah-pwd-visible") : !!show);
      },
      focus: function (el, $el) { pwdInput($el).trigger("focus"); }
    }
  });

  // ------------------------------------------------------------------
  // number_input
  // ------------------------------------------------------------------

  function numOpts($el) {
    var num = function (a) {
      var v = $el.attr(a);
      return v === undefined || v === "" ? null : parseFloat(v);
    };
    return {
      min: num("data-min"),
      max: num("data-max"),
      step: num("data-step") || 1,
      decimals: parseInt($el.attr("data-decimals") || "0", 10),
      allowNull: $el.attr("data-allow-null") !== "false"
    };
  }

  function clamp(v, o) {
    if (o.min !== null) { v = Math.max(o.min, v); }
    if (o.max !== null) { v = Math.min(o.max, v); }
    return v;
  }

  function parseNum(text) {
    var s = String(text == null ? "" : text).replace(/,/g, "").trim();
    if (s === "") { return null; }
    var n = parseFloat(s);
    return isNaN(n) ? null : n;
  }

  var numSeq = 0;

  function numInput($el) { return $el.find("input.ah-numinput-input").first(); }

  // Write a (clamped, formatted) value; returns the value written.
  function writeNum($el, v) {
    var o = numOpts($el), $input = numInput($el);
    if (v === null) {
      v = o.allowNull ? null : clamp(0, o);
    } else {
      v = clamp(v, o);
    }
    $input.val(v === null ? "" : v.toFixed(o.decimals));
    if (v === null) { $input.removeAttr("aria-valuenow"); } else { $input.attr("aria-valuenow", v); }
    syncLabel($el, "ah-numinput", v !== null, $input.is(":focus"));
    return v;
  }

  function step($el, dir) {
    var $input = numInput($el), input = $input[0];
    if (!input || input.disabled || input.readOnly) { return; }
    var o = numOpts($el);
    var before = $input.val();
    var cur = parseNum(before);
    var next = parseFloat(((cur === null ? 0 : cur) + o.step * dir).toFixed(o.decimals));
    writeNum($el, next);
    if ($input.val() !== before) {
      $input.trigger("input").trigger("change");
    }
  }

  function stopRepeat(el) {
    var t = $.data(el, "ah-spin");
    if (t) { clearTimeout(t.delay); clearInterval(t.every); }
    $.removeData(el, "ah-spin");
  }

  AH.define("number-input", {
    init: function (el, $el) {
      var $input = numInput($el);
      focusShell($el, $input, "ah-numinput");
      $input.on("keydown" + NS, function (e) {
        var k = e.key || "";
        if (e.ctrlKey || e.metaKey || e.altKey) { return; }
        if (k === "ArrowUp" || k === "ArrowDown") {
          e.preventDefault();
          step($el, k === "ArrowUp" ? 1 : -1);
        } else if (k === "PageUp" || k === "PageDown") {
          e.preventDefault();
          step($el, (k === "PageUp" ? 1 : -1) * 10);
        } else if (k === "-") {
          // a minus sign only at the start, once
          if (this.selectionStart !== 0 || this.value.indexOf("-") >= 0) { e.preventDefault(); }
        } else if (k === ".") {
          if (numOpts($el).decimals === 0 || this.value.indexOf(".") >= 0) { e.preventDefault(); }
        } else if (k.length === 1 && !/[0-9]/.test(k)) {
          e.preventDefault();
        }
      });
      // Typing ends in a native change (on blur or Enter): normalise first,
      // this handler runs before the delegated action handlers.
      $input.on("change" + NS, function () {
        writeNum($el, parseNum($input.val()));
      });
      $input.on("wheel" + NS, function (e) {
        if (!$input.is(":focus")) { return; }
        e.preventDefault();
        var dy = (e.originalEvent || e).deltaY;
        step($el, dy < 0 ? 1 : -1);
      });
      // Spin buttons: step, then repeat after 400ms every 75ms (sigil).
      $el.on("mousedown" + NS, ".ah-numinput-spin-up, .ah-numinput-spin-down", function (e) {
        if (e.button !== 0) { return; }
        e.preventDefault();
        var dir = $(this).hasClass("ah-numinput-spin-up") ? 1 : -1;
        stopRepeat(el);
        step($el, dir);
        var t = {};
        t.delay = setTimeout(function () {
          t.every = setInterval(function () { step($el, dir); }, 75);
        }, 400);
        $.data(el, "ah-spin", t);
        if (!$input.is(":focus")) { $input.trigger("focus"); }
      });
      $el.on("mouseleave" + NS, ".ah-numinput-spin", function () { stopRepeat(el); });
      var docNs = NS + "num" + (++numSeq);
      $.data(el, "ah-doc-ns", docNs);
      $(document).on("mouseup" + docNs, function () { stopRepeat(el); });
    },
    destroy: function (el) {
      stopRepeat(el);
      $(document).off("mouseup" + $.data(el, "ah-doc-ns"));
    },
    methods: {
      getValue: function (el, $el) { return parseNum(numInput($el).val()); },
      setValue: function (el, $el, v) {
        var $input = numInput($el), before = $input.val();
        writeNum($el, v === null || v === undefined ? null : parseNum(v));
        if ($input.val() !== before) { $input.trigger("change"); }
      },
      stepUp: function (el, $el) { step($el, 1); },
      stepDown: function (el, $el) { step($el, -1); },
      clear: function (el, $el) {
        var $input = numInput($el), before = $input.val();
        writeNum($el, null);
        if ($input.val() !== before) { $input.trigger("change"); }
      },
      focus: function (el, $el) { numInput($el).trigger("focus"); }
    }
  });

  // ------------------------------------------------------------------
  // Value-bearing helpers
  // ------------------------------------------------------------------

  // Update data-ah-value and the hidden input; fire change on the root.
  function commitValue($el, value) {
    if ($el.attr("data-ah-value") === value) { return false; }
    $el.attr("data-ah-value", value);
    $el.children("input[type=hidden]").val(value);
    $el.trigger("change");
    return true;
  }

  // Native events of the inner fields must not reach on(...) on the root:
  // the root reports its own change.
  function isolate($el, sel) {
    $el.on("change" + NS + " input" + NS, sel, function (e) { e.stopPropagation(); });
  }

  // ------------------------------------------------------------------
  // input_otp
  // ------------------------------------------------------------------

  function otpSlots($el) { return $el.find(".ah-input-otp__slot"); }

  function otpSanitize($el, s, len) {
    var re = $el.attr("data-pattern") === "alphanumeric" ? /[^0-9a-zA-Z]/g : /[^0-9]/g;
    return String(s || "").replace(re, "").slice(0, Math.max(0, len));
  }

  function otpRead($el) {
    return otpSlots($el).map(function () { return this.value; }).get().join("");
  }

  function otpFocus($el, i) {
    var $s = otpSlots($el);
    if (!$s.length) { return; }
    var s = $s.get(Math.max(0, Math.min(i, $s.length - 1)));
    s.focus();
    s.select();
  }

  // Lay a value out over the slots (from the first).
  function otpWrite($el, v) {
    otpSlots($el).each(function (i) {
      this.value = v.charAt(i);
    });
  }

  function otpCommit($el) {
    var $s = otpSlots($el);
    $s.each(function () { this.setAttribute("data-filled", this.value ? "true" : "false"); });
    // The value is the filled prefix: a gap ends it.
    var v = "";
    $s.each(function () {
      if (!this.value) { return false; }
      v += this.value;
    });
    var complete = v.length === $s.length;
    $el.attr("data-complete", complete ? "true" : "false");
    $el.removeAttr("data-invalid");
    if (commitValue($el, v) && complete) {
      $el.trigger("ah:complete", [v]);
    }
  }

  AH.define("input-otp", {
    init: function (el, $el) {
      isolate($el, ".ah-input-otp__slot");
      $el.on("input" + NS, ".ah-input-otp__slot", function () {
        var len = otpSlots($el).length;
        var i = parseInt(this.getAttribute("data-index"), 10);
        var v = otpSanitize($el, this.value, len - i);
        if (v.length > 1) {
          // autofill or IME delivered several characters: spread them
          var cur = otpRead($el);
          otpWrite($el, (cur.slice(0, i) + v).slice(0, len));
          otpCommit($el);
          otpFocus($el, i + v.length);
          return;
        }
        this.value = v;
        otpCommit($el);
        if (v) { otpFocus($el, i + 1); }
      });
      $el.on("keydown" + NS, ".ah-input-otp__slot", function (e) {
        var i = parseInt(this.getAttribute("data-index"), 10);
        var n = otpSlots($el).length;
        switch (e.key) {
          case "Backspace":
            e.preventDefault();
            if (this.value) {
              this.value = "";                  // clear this one only
              otpCommit($el);
            } else if (i > 0) {
              otpSlots($el).get(i - 1).value = ""; // back up and clear
              otpCommit($el);
              otpFocus($el, i - 1);
            }
            break;
          case "Delete":
            e.preventDefault();
            this.value = "";
            otpCommit($el);
            break;
          case "ArrowLeft": e.preventDefault(); otpFocus($el, i - 1); break;
          case "ArrowRight": e.preventDefault(); otpFocus($el, i + 1); break;
          case "Home": e.preventDefault(); otpFocus($el, 0); break;
          case "End": e.preventDefault(); otpFocus($el, n - 1); break;
          default:
            // typing over a filled slot replaces it
            if (e.key && e.key.length === 1 && !e.ctrlKey && !e.metaKey &&
                this.value && this.selectionStart === this.selectionEnd) {
              this.select();
            }
        }
      });
      $el.on("paste" + NS, ".ah-input-otp__slot", function (e) {
        e.preventDefault();
        var cd = (e.originalEvent || e).clipboardData;
        var n = otpSlots($el).length;
        var v = otpSanitize($el, cd ? cd.getData("text") : "", n);
        if (!v) { return; }
        otpWrite($el, v);
        otpCommit($el);
        otpFocus($el, Math.min(v.length, n - 1));
      });
      $el.on("focus" + NS, ".ah-input-otp__slot", function () { this.select(); });
      // A click on an empty slot past the first gap goes to the gap.
      $el.on("mousedown" + NS, ".ah-input-otp__slot", function (e) {
        var gap = otpRead($el).length;
        var i = parseInt(this.getAttribute("data-index"), 10);
        if (!this.value && i > gap) {
          e.preventDefault();
          otpFocus($el, gap);
        }
      });
    },
    methods: {
      getValue: function (el, $el) { return $el.attr("data-ah-value") || ""; },
      setValue: function (el, $el, v) {
        otpWrite($el, otpSanitize($el, v, otpSlots($el).length));
        otpCommit($el);
      },
      clear: function (el, $el) { otpWrite($el, ""); otpCommit($el); },
      focus: function (el, $el) { otpFocus($el, otpRead($el).length); },
      // Mark the code wrong (e.g. after the server rejected it).
      invalid: function (el, $el, on) {
        if (on === false) { $el.removeAttr("data-invalid"); } else { $el.attr("data-invalid", "true"); }
      }
    }
  });

  // ------------------------------------------------------------------
  // tag_input
  // ------------------------------------------------------------------

  function tagField($el) { return $el.find(".ah-tag-input__field"); }

  function readTags($el) {
    return $el.find(".ah-tag-input__chip .ah-chip__label").map(function () {
      return $(this).text();
    }).get();
  }

  // Same markup as the server's first render: templates/tag_input_chip.mustache
  function chip($el, tag, i) {
    return $(AH.tpl.tag_input_chip({
      variant: $el.attr("data-chip-variant") || "soft",
      color: $el.attr("data-chip-color") || "primary",
      index: i,
      label: tag,
      disabled: $el.attr("data-disabled") === "true"
    }));
  }

  function renderTags($el, tags) {
    $el.find(".ah-tag-input__chip").remove();
    var $field = tagField($el);
    $.each(tags, function (i, t) { chip($el, t, i).insertBefore($field); });
    return commitValue($el, tags.join(","));
  }

  // sigil tag-input/add-tag: trim, skip blanks, max-tags and duplicates.
  function addTags($el, list) {
    var tags = readTags($el);
    var max = parseInt($el.attr("data-max-tags"), 10);
    var dup = $el.is("[data-allow-duplicates]");
    var changed = false;
    $.each(list, function (_, raw) {
      var t = String(raw).trim().replace(/,/g, "");
      if (!t || (!isNaN(max) && tags.length >= max) || (!dup && tags.indexOf(t) >= 0)) { return; }
      tags.push(t);
      changed = true;
    });
    return changed ? renderTags($el, tags) : false;
  }

  function addFromField($el) {
    var $f = tagField($el), raw = $f.val();
    $f.val("");
    return addTags($el, [raw]);
  }

  function tagsDisabled($el) { return $el.attr("data-disabled") === "true"; }

  AH.define("tag-input", {
    init: function (el, $el) {
      isolate($el, ".ah-tag-input__field");
      $el.on("click" + NS, ".ah-chip__delete", function (e) {
        e.stopPropagation();
        if (tagsDisabled($el)) { return; }
        var i = parseInt($(this).closest(".ah-tag-input__chip").attr("data-index"), 10);
        var tags = readTags($el);
        if (i >= 0 && i < tags.length) {
          tags.splice(i, 1);
          renderTags($el, tags);
          tagField($el).trigger("focus");
        }
      });
      $el.on("keydown" + NS, ".ah-tag-input__field", function (e) {
        if (e.key === "Enter" || e.key === ",") {
          e.preventDefault();
          addFromField($el);
        } else if (e.key === "Backspace" && this.value.trim() === "") {
          var tags = readTags($el);
          if (tags.length) {
            e.preventDefault();
            tags.pop();
            renderTags($el, tags);
          }
        }
      });
      // A pasted list becomes several tags (sigil takes it as typed text).
      $el.on("paste" + NS, ".ah-tag-input__field", function (e) {
        var cd = (e.originalEvent || e).clipboardData;
        var text = cd ? cd.getData("text") : "";
        if (!/[,\n\r\t]/.test(text)) { return; }
        e.preventDefault();
        var parts = (this.value + text).split(/[,\n\r\t]+/);
        this.value = "";
        addTags($el, parts);
      });
      $el.on("focusout" + NS, ".ah-tag-input__field", function () { addFromField($el); });
      // A click on the empty area focuses the field.
      $el.on("click" + NS, function (e) {
        if (e.target === el) { tagField($el).trigger("focus"); }
      });
    },
    methods: {
      getTags: function (el, $el) { return readTags($el); },
      setTags: function (el, $el, tags) { renderTags($el, $.makeArray(tags).map(String)); },
      add: function (el, $el, tag) { addTags($el, [tag]); },
      remove: function (el, $el, tag) {
        var tags = readTags($el), i = tags.indexOf(String(tag));
        if (i >= 0) { tags.splice(i, 1); renderTags($el, tags); }
      },
      clear: function (el, $el) { renderTags($el, []); },
      focus: function (el, $el) { tagField($el).trigger("focus"); }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/form_time_color.js ---- */
/* Behaviours of the form_time_color components (designs/04-components.md).
 * Ported from sigil: form/timepicker (+ timepicker/math, timepicker/svg)
 * and form/colorpicker (+ colorpicker/color, events, render).
 *
 * Both are value-bearing: data-ah-value and the hidden input follow the
 * value, the root fires `change` on commit (the colour picker also fires
 * `input` while dragging). Native input/change events of the inner text
 * fields are stopped at the root so they are not taken for the
 * component's own events. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  function uid(prefix) {
    seq += 1;
    return prefix + seq + "-" + Math.random().toString(36).slice(2, 7);
  }

  function clamp(v, lo, hi) {
    return Math.max(lo, Math.min(hi, v));
  }

  function disabled(el) {
    return el.getAttribute("aria-disabled") === "true";
  }

  // Keep the inner fields' native events inside the component.
  function fenceNativeEvents($el) {
    $el.on("input" + NS + " change" + NS, "input", function (e) {
      e.stopPropagation();
    });
  }

  // ------------------------------------------------------------------
  // Popup shared by both pickers: a field toggles a panel placed by
  // AH.float (below, above when there is no room), closed by Escape or a
  // press outside.
  // ------------------------------------------------------------------

  function popupOf($el) {
    return $el.children(".ah-timepicker-popup, .ah-colorpicker-popup").first();
  }

  function isOpen($el) {
    var $p = popupOf($el);
    return $p.length > 0 && !$p.prop("hidden");
  }

  function openPopup(el, $el, cls, $opener, onOpen) {
    var $p = popupOf($el);
    if (!$p.length || !$p.prop("hidden") || disabled(el)) { return; }
    if (onOpen) { onOpen(); }
    $p.prop("hidden", false);
    $el.addClass(cls + "-open");
    $opener.attr("aria-expanded", "true");
    // Fixed positioning at the field (the input area or the trigger), so
    // an overflow:hidden ancestor such as a card does not clip it; flips
    // above when there is no room below and follows scrolling.
    $.data(el, "ah-float", AH.float($p[0], $el.children().first()[0],
                                    { placement: "bottom", align: "start", offset: 4 }));
    var ns = ".ahpop" + el.getAttribute("data-ah-uid");
    $(document).off(ns).on("mousedown" + ns + " touchstart" + ns + " focusin" + ns, function (e) {
      if (!$.contains(el, e.target) && e.target !== el) {
        closePopup(el, $el, cls, $opener, false);
      }
    });
  }

  function closePopup(el, $el, cls, $opener, refocus) {
    var $p = popupOf($el);
    stopFloat(el);
    if (!$p.length || $p.prop("hidden")) { return; }
    $p.prop("hidden", true);
    $el.removeClass(cls + "-open");
    $opener.attr("aria-expanded", "false");
    if (refocus) { $opener.trigger("focus"); }
  }

  function stopFloat(el) {
    $(document).off(".ahpop" + el.getAttribute("data-ah-uid"));
    var h = $.data(el, "ah-float");
    if (h) { h.stop(); $.removeData(el, "ah-float"); }
  }

  function pointerXY(e) {
    var oe = e.originalEvent || e;
    var t = oe.touches && oe.touches[0] ? oe.touches[0] : oe;
    return { x: t.clientX, y: t.clientY };
  }

  // Pointer drag on one element: start(e), move(e), end(e). Uses pointer
  // capture, so nothing is bound on document.
  function drag($target, el, start, move, end) {
    $target.on("pointerdown" + NS, function (e) {
      if (disabled(el) || (e.button !== undefined && e.button !== 0)) { return; }
      if (start(e) === false) { return; }
      e.preventDefault();
      var node = this;
      var id = e.originalEvent && e.originalEvent.pointerId;
      if (id !== undefined && node.setPointerCapture) {
        try { node.setPointerCapture(id); } catch (err) { /* synthetic event */ }
      }
      node.focus && node.focus({ preventScroll: true });
      var $n = $(node);
      $n.on("pointermove" + NS + "drag", function (m) { move(m); });
      $n.on("pointerup" + NS + "drag pointercancel" + NS + "drag", function (u) {
        $n.off(NS + "drag");
        end(u);
      });
    });
  }

  // ------------------------------------------------------------------
  // timepicker (sigil timepicker, timepicker/math, timepicker/svg)
  // ------------------------------------------------------------------

  var TWO_PI = 2 * Math.PI;
  var CX = 130, CY = 130, OUTER_R = 105, INNER_R = 70;
  var TP = "ah-timepicker";

  function pad2(n) { return n < 10 ? "0" + n : String(n); }

  function to12(h) {
    if (h === 0) { return { h12: 12, period: "am" }; }
    if (h < 12) { return { h12: h, period: "am" }; }
    if (h === 12) { return { h12: 12, period: "pm" }; }
    return { h12: h - 12, period: "pm" };
  }

  function to24(h12, period) {
    if (period === "am") { return h12 === 12 ? 0 : h12; }
    return h12 === 12 ? 12 : h12 + 12;
  }

  function angleXY(angle, r) {
    return { x: CX + r * Math.sin(angle), y: CY - r * Math.cos(angle) };
  }

  function round2(n) { return Math.round(n * 100) / 100; }

  // "14:30", "14:30:00", "2:30 pm", "2 pm", "1430" -> minutes of the day
  function parseTime(s) {
    var m = /^\s*(\d{1,2})(?::?(\d{2}))?(?::\d{2})?\s*([ap])?\.?m?\.?\s*$/i.exec(s || "");
    if (!m) { return null; }
    var h = parseInt(m[1], 10);
    var min = m[2] ? parseInt(m[2], 10) : 0;
    if (min > 59) { return null; }
    if (m[3]) {
      if (h < 1 || h > 12) { return null; }
      h = to24(h, m[3].toLowerCase() === "a" ? "am" : "pm");
    } else if (h > 23) {
      return null;
    }
    return h * 60 + min;
  }

  function tpState(el) { return $.data(el, "ah-tp"); }

  function tpAllowed(st, t) { return t >= st.lo && t <= st.hi; }

  function tpHourAllowed(st, h) {
    for (var m = 0; m < 60; m += st.step) {
      if (tpAllowed(st, h * 60 + m)) { return true; }
    }
    return false;
  }

  function tpClampState(st) {
    var t = clamp(st.h * 60 + st.m, st.lo, st.hi);
    st.h = Math.floor(t / 60);
    st.m = t % 60;
  }

  function tpDisplay(st, value) {
    if (value === "") { return ""; }
    var t = parseTime(value);
    var h = Math.floor(t / 60), m = t % 60;
    if (st.format === "24h") { return pad2(h) + ":" + pad2(m); }
    var p = to12(h);
    return p.h12 + ":" + pad2(m) + " " + p.period.toUpperCase();
  }

  // View data for templates/timepicker_header.mustache, as the server
  // builds it in aihtml_form_time_color:time_header/5.
  function tpHeaderView(st, isDisabled) {
    var p = to12(st.h);
    return {
      hours: st.format === "24h" ? pad2(st.h) : String(p.h12),
      minutes: pad2(st.m),
      hours_active: st.mode === "hours",
      minutes_active: st.mode === "minutes",
      twelve: st.format === "12h",
      am: p.period === "am",
      pm: p.period === "pm",
      disabled: isDisabled,
      tabindex: isDisabled ? -1 : 0
    };
  }

  // Redraw header and clock from the state (sigil sync-header!, sync-clock!).
  function tpRender(el) {
    var st = tpState(el);
    var $el = $(el);
    var $header = $el.find("." + TP + "-header");
    var focusedAction = $header.find(":focus").attr("data-action");
    // Same markup as the server's first render: templates/timepicker_header.mustache
    $header.html(AH.tpl.timepicker_header(tpHeaderView(st, disabled(el))));
    if (focusedAction) { $header.find("[data-action='" + focusedAction + "']").trigger("focus"); }

    var svg = $el.find("." + TP + "-svg")[0];
    if (!svg) { return; }
    var g = svg.querySelector("." + TP + "-numbers");
    var p = to12(st.h);
    var angle, r, selected, items = [];
    if (st.mode === "hours") {
      var i;
      for (i = 1; i <= 12; i++) {
        items.push({ label: String(i), val: i, r: OUTER_R, inner: false,
                     ok: tpHourAllowed(st, st.format === "24h" ? i : to24(i, p.period)) });
      }
      if (st.format === "24h") {
        [0, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23].forEach(function (v) {
          items.push({ label: pad2(v), val: v, r: INNER_R, inner: true, ok: tpHourAllowed(st, v) });
        });
      }
      var disp = st.format === "24h" ? st.h % 12 : p.h12 % 12;
      angle = disp / 12 * TWO_PI;
      r = (st.format === "24h" && (st.h === 0 || st.h >= 13)) ? INNER_R : OUTER_R;
      selected = st.format === "24h" ? st.h : p.h12;
      items.forEach(function (it) { it.angle = (it.val % 12) / 12 * TWO_PI; });
    } else {
      for (var m = 0; m < 60; m += Math.max(st.step, 5)) {
        items.push({ label: pad2(m), val: m, r: OUTER_R, inner: false, angle: m / 60 * TWO_PI,
                     ok: tpAllowed(st, st.h * 60 + m) });
      }
      angle = st.m / 60 * TWO_PI;
      r = OUTER_R;
      selected = st.m;
    }
    // Same markup as the server's first render: templates/timepicker_numbers.mustache
    // (innerHTML on an SVG element parses its children as SVG).
    g.innerHTML = AH.tpl.timepicker_numbers({ numbers: items.map(function (it) {
      var xy = angleXY(it.angle, it.r);
      return { label: it.label, val: it.val, x: String(round2(xy.x)), y: String(round2(xy.y)),
               inner: it.inner, selected: it.val === selected, disabled: !it.ok };
    }) });
    var end = angleXY(angle, r);
    var hand = svg.querySelector("." + TP + "-hand");
    hand.setAttribute("x2", round2(end.x));
    hand.setAttribute("y2", round2(end.y));
    var sel = svg.querySelector("." + TP + "-selection");
    sel.setAttribute("cx", round2(end.x));
    sel.setAttribute("cy", round2(end.y));
    var hoursMode = st.mode === "hours";
    svg.setAttribute("aria-label", hoursMode ? "Hours" : "Minutes");
    svg.setAttribute("aria-valuemax", hoursMode ? "23" : "59");
    svg.setAttribute("aria-valuenow", hoursMode ? st.h : st.m);
    svg.setAttribute("aria-valuetext", hoursMode ? String(selected) : pad2(st.m));
  }

  // Commit a value ("HH:MM" or ""): data-ah-value, hidden input, field
  // text, clear button; `change` when it differs from the current one.
  function tpCommit(el, value, silent) {
    var st = tpState(el);
    var $el = $(el);
    var old = el.getAttribute("data-ah-value") || "";
    el.setAttribute("data-ah-value", value);
    $el.children("input[type=hidden]").val(value);
    $el.find("." + TP + "-input").val(tpDisplay(st, value));
    $el.find("." + TP + "-clear").prop("hidden", value === "");
    if (!silent && value !== old) {
      $el.trigger("change", [{ value: value }]);
    }
  }

  function tpCurrent(st) { return pad2(st.h) + ":" + pad2(st.m); }

  // Load the committed value into the state, or now (rounded to the step)
  // when empty, like sigil's default.
  function tpLoad(el) {
    var st = tpState(el);
    var t = parseTime(el.getAttribute("data-ah-value"));
    if (t === null) {
      var now = new Date();
      t = now.getHours() * 60 + Math.round(now.getMinutes() / st.step) * st.step;
      t = t % (24 * 60);
    }
    st.h = Math.floor(t / 60);
    st.m = t % 60;
    tpClampState(st);
  }

  function tpFromPolar(el, e) {
    var st = tpState(el);
    var svg = $(el).find("." + TP + "-svg")[0];
    var rect = svg.getBoundingClientRect();
    var pt = pointerXY(e);
    var dx = (pt.x - rect.left) / rect.width * 260 - CX;
    var dy = (pt.y - rect.top) / rect.height * 260 - CY;
    var angle = Math.atan2(dx, -dy);
    if (angle < 0) { angle += TWO_PI; }
    return { angle: angle, radius: Math.sqrt(dx * dx + dy * dy) };
  }

  // sigil compute-from-polar with snap-hour / snap-minute.
  function tpApplyPolar(el, polar) {
    var st = tpState(el);
    if (st.mode === "hours") {
      var idx = Math.round(polar.angle / (TWO_PI / 12)) % 12;
      var h;
      if (st.format === "24h") {
        h = polar.radius < 87.5 ? (idx === 0 ? 0 : idx + 12) : (idx === 0 ? 12 : idx);
      } else {
        h = to24(idx === 0 ? 12 : idx, to12(st.h).period);
      }
      if (!tpHourAllowed(st, h)) { return false; }
      st.h = h;
      tpClampState(st);
    } else {
      var raw = Math.round(polar.angle / (TWO_PI / 60)) % 60;
      var m = (Math.round(raw / st.step) * st.step) % 60;
      if (!tpAllowed(st, st.h * 60 + m)) { return false; }
      st.m = m;
    }
    return true;
  }

  // Arrow keys: next allowed hour / minute step in direction dir.
  function tpStep(st, dir) {
    var i, t;
    if (st.mode === "hours") {
      var h = st.h;
      for (i = 0; i < 24; i++) {
        h = (h + dir + 24) % 24;
        if (tpHourAllowed(st, h)) { st.h = h; tpClampState(st); return true; }
      }
    } else {
      var m = st.m - (st.m % st.step);
      if (dir < 0 && m !== st.m) { m += st.step; }
      for (i = 0; i < 60; i++) {
        m = (m + dir * st.step + 60) % 60;
        t = st.h * 60 + m;
        if (tpAllowed(st, t)) { st.m = m; return true; }
      }
    }
    return false;
  }

  function tpSetMode(el, mode) {
    tpState(el).mode = mode;
    tpRender(el);
  }

  function tpSetPeriod(el, period) {
    var st = tpState(el);
    var h = to24(to12(st.h).h12, period);
    var t = clamp(h * 60 + st.m, st.lo, st.hi);
    st.h = Math.floor(t / 60);
    st.m = t % 60;
    tpRender(el);
    tpCommit(el, tpCurrent(st));
  }

  AH.define("timepicker", {
    init: function (el, $el) {
      el.setAttribute("data-ah-uid", ++seq);
      var lo = parseTime(el.getAttribute("data-min"));
      var hi = parseTime(el.getAttribute("data-max"));
      var st = {
        mode: "hours",
        format: el.getAttribute("data-format") === "24h" ? "24h" : "12h",
        step: parseInt(el.getAttribute("data-step"), 10) || 5,
        auto: el.getAttribute("data-auto-switch") !== "false",
        lo: lo === null ? 0 : lo,
        hi: hi === null ? 24 * 60 - 1 : hi,
        h: 12, m: 0
      };
      $.data(el, "ah-tp", st);
      tpLoad(el);
      var $input = $el.find("." + TP + "-input");
      var $popup = popupOf($el);
      var $svg = $el.find("." + TP + "-svg");
      var popup = $popup.length > 0;
      if (popup) {
        $popup.attr("id", $popup.attr("id") || uid("ah-tp-popup-"));
        $input.attr("aria-controls", $popup.attr("id"));
      }
      fenceNativeEvents($el);

      function open(focusClock) {
        openPopup(el, $el, TP, $input, function () {
          st.mode = "hours";
          tpLoad(el);
          tpRender(el);
        });
        if (focusClock) { $svg.trigger("focus"); }
      }
      function close(refocus) { closePopup(el, $el, TP, $input, refocus); }
      $.data(el, "ah-tp-open", open);
      $.data(el, "ah-tp-close", close);

      // Field: click toggles; typing a time commits on change.
      $el.on("mousedown" + NS, "." + TP + "-input-area", function (e) {
        if ($(e.target).closest("." + TP + "-clear").length) { return; }
        if (isOpen($el)) {
          if (!$(e.target).is($input)) { e.preventDefault(); close(true); }
        } else {
          open(false);
        }
      });
      $input.on("keydown" + NS, function (e) {
        if (e.key === "ArrowDown" || (e.key === " " && !$input.val())) {
          e.preventDefault();
          open(true);
        } else if (e.key === "Enter" && !isOpen($el)) {
          e.preventDefault();
          $input.trigger("change");
        }
      });
      $input.on("change" + NS, function () {
        var text = $input.val();
        if (String(text).trim() === "") { tpCommit(el, ""); return; }
        var t = parseTime(text);
        if (t === null) {
          $input.val(tpDisplay(st, el.getAttribute("data-ah-value") || ""));
          return;
        }
        t = clamp(t, st.lo, st.hi);
        st.h = Math.floor(t / 60);
        st.m = t % 60;
        tpRender(el);
        tpCommit(el, tpCurrent(st));
      });
      $el.on("click" + NS, "." + TP + "-clear", function (e) {
        e.preventDefault();
        tpCommit(el, "");
        close(false);
        $input.trigger("focus");
      });
      $el.on("keydown" + NS, function (e) {
        if (e.key === "Escape" && isOpen($el)) {
          e.preventDefault();
          e.stopPropagation();
          close(true);
        }
      });

      // Header: hours / minutes / AM / PM (sigil setup-header-clicks!).
      function headerAction(action) {
        if (disabled(el)) { return; }
        if (action === "select-hours") { tpSetMode(el, "hours"); }
        else if (action === "select-minutes") { tpSetMode(el, "minutes"); }
        else if (action === "set-am") { tpSetPeriod(el, "am"); }
        else if (action === "set-pm") { tpSetPeriod(el, "pm"); }
      }
      $el.on("click" + NS, "." + TP + "-header [data-action]", function () {
        headerAction(this.getAttribute("data-action"));
      });
      $el.on("keydown" + NS, "." + TP + "-header [data-action]", function (e) {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          headerAction(this.getAttribute("data-action"));
        }
      });

      // Clock: drag (sigil setup-clock-drag!) and keyboard.
      var dragMode = null;
      drag($svg, el, function (e) {
        var polar = tpFromPolar(el, e);
        if (polar.radius <= 20) { return false; }
        dragMode = st.mode;
        if (tpApplyPolar(el, polar)) { tpRender(el); }
      }, function (e) {
        if (tpApplyPolar(el, tpFromPolar(el, e))) { tpRender(el); }
      }, function () {
        tpCommit(el, tpCurrent(st));
        if (dragMode === "hours" && st.auto) {
          tpSetMode(el, "minutes");
        } else if (dragMode === "minutes" && popup) {
          close(true);
        }
        dragMode = null;
      });
      $svg.on("keydown" + NS, function (e) {
        if (disabled(el)) { return; }
        var dir = { ArrowUp: 1, ArrowRight: 1, ArrowDown: -1, ArrowLeft: -1 }[e.key];
        if (dir) {
          e.preventDefault();
          if (tpStep(st, dir)) {
            tpRender(el);
            tpCommit(el, tpCurrent(st));
          }
        } else if (e.key === "Home" || e.key === "End") {
          e.preventDefault();
          var t = e.key === "Home" ? st.lo : st.hi;
          if (st.mode === "hours") {
            st.h = Math.floor(t / 60);
            tpClampState(st);
          } else {
            var base = st.h * 60;
            var m = e.key === "Home" ? 0 : 60 - st.step;
            while (!tpAllowed(st, base + m) && m >= 0 && m < 60) { m += e.key === "Home" ? st.step : -st.step; }
            if (m >= 0 && m < 60) { st.m = m; }
          }
          tpRender(el);
          tpCommit(el, tpCurrent(st));
        } else if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          tpCommit(el, tpCurrent(st));
          if (st.mode === "hours") {
            tpSetMode(el, "minutes");
          } else if (popup) {
            close(true);
          }
        }
      });
      if (!popup) { tpRender(el); }
    },
    destroy: function (el) { stopFloat(el); },
    methods: {
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      // Set the value without firing change (server driven).
      setValue: function (el, $el, v) {
        var t = parseTime(v == null ? "" : String(v));
        var st = tpState(el);
        if (t === null) { tpCommit(el, "", true); return; }
        st.h = Math.floor(t / 60);
        st.m = t % 60;
        tpRender(el);
        tpCommit(el, tpCurrent(st), true);
      },
      clear: function (el) { tpCommit(el, ""); },
      open: function (el) { $.data(el, "ah-tp-open")(true); },
      close: function (el) { $.data(el, "ah-tp-close")(false); },
      setMode: function (el, $el, mode) { tpSetMode(el, mode === "minutes" ? "minutes" : "hours"); }
    }
  });

  // ------------------------------------------------------------------
  // colorpicker (sigil colorpicker, colorpicker/color, events, render)
  // ------------------------------------------------------------------

  var CP = "ah-colorpicker";

  function hsvToRgb(h, s, v) {
    s /= 100; v /= 100;
    var c = v * s;
    var x = c * (1 - Math.abs(((h / 60) % 2) - 1));
    var m = v - c;
    var rgb = h < 60 ? [c, x, 0] : h < 120 ? [x, c, 0] : h < 180 ? [0, c, x]
      : h < 240 ? [0, x, c] : h < 300 ? [x, 0, c] : [c, 0, x];
    return { r: Math.round((rgb[0] + m) * 255), g: Math.round((rgb[1] + m) * 255),
             b: Math.round((rgb[2] + m) * 255) };
  }

  function rgbToHsv(r, g, b) {
    r /= 255; g /= 255; b /= 255;
    var max = Math.max(r, g, b), min = Math.min(r, g, b), d = max - min;
    var h = d === 0 ? 0 : max === r ? 60 * ((((g - b) / d) % 6 + 6) % 6)
      : max === g ? 60 * ((b - r) / d + 2) : 60 * ((r - g) / d + 4);
    return { h: Math.round(h) % 360, s: Math.round(max === 0 ? 0 : d / max * 100),
             v: Math.round(max * 100) };
  }

  function hex2(n) { return (n < 16 ? "0" : "") + n.toString(16); }

  // "#rgb", "rgb", "#rrggbb", with alpha also 4 and 8 digits -> {r,g,b,a}
  function parseHex(s, alpha) {
    var h = String(s || "").trim().replace(/^#/, "");
    if (!/^[0-9a-f]+$/i.test(h)) { return null; }
    if (h.length === 3 || (alpha && h.length === 4)) {
      h = h.replace(/./g, "$&$&");
    }
    if (h.length !== 6 && !(alpha && h.length === 8)) { return null; }
    return { r: parseInt(h.slice(0, 2), 16), g: parseInt(h.slice(2, 4), 16),
             b: parseInt(h.slice(4, 6), 16), a: h.length === 8 ? parseInt(h.slice(6, 8), 16) : 255 };
  }

  function cpState(el) { return $.data(el, "ah-cp"); }

  // The exact RGB a colour was loaded with (hex, RGB inputs, swatch) is
  // kept until the HSV controls change it, so rounding through HSV does
  // not alter a typed colour.
  function cpRgb(st) {
    var e = st.exact;
    if (e && e.h === st.h && e.s === st.s && e.v === st.v) { return e.rgb; }
    return hsvToRgb(st.h, st.s, st.v);
  }

  function cpHex(st) {
    var c = cpRgb(st);
    var hex = "#" + hex2(c.r) + hex2(c.g) + hex2(c.b);
    return st.alpha && st.a < 255 ? hex + hex2(st.a) : hex;
  }

  function cpRgba(st) {
    var c = cpRgb(st);
    return "rgba(" + c.r + "," + c.g + "," + c.b + "," + round2(st.a / 255) + ")";
  }

  function cpLoad(st, c) {
    var hsv = rgbToHsv(c.r, c.g, c.b);
    // Keep the hue when the colour is grey or black, so the area does
    // not jump back to red.
    if (hsv.s === 0 || hsv.v === 0) { hsv.h = st.h; }
    if (hsv.v === 0) { hsv.s = st.s; }
    st.h = hsv.h; st.s = hsv.s; st.v = hsv.v; st.a = c.a;
    st.exact = { h: st.h, s: st.s, v: st.v, rgb: { r: c.r, g: c.g, b: c.b } };
  }

  // sigil 同步全部UI!: area colour, pointers, preview, inputs; plus the
  // alpha bar, swatches, ARIA and the popup trigger.
  function cpSync(el, skip) {
    var st = cpState(el);
    var $el = $(el);
    var c = cpRgb(st);
    var bright = 0.299 * c.r + 0.587 * c.g + 0.114 * c.b > 150;
    var hex6 = "#" + hex2(c.r) + hex2(c.g) + hex2(c.b);
    var $map = $el.find("." + CP + "-map");
    $map.css("background-color", "hsl(" + st.h + ", 100%, 50%)")
      .attr({ "aria-valuenow": st.s,
              "aria-valuetext": "Saturation " + st.s + "%, brightness " + st.v + "%" });
    $map.find("." + CP + "-map-pointer").css({ left: st.s + "%", top: (100 - st.v) + "%" })
      .toggleClass(CP + "-map-pointer-dark", bright)
      .toggleClass(CP + "-map-pointer-light", !bright);
    var $hue = $el.find("." + CP + "-bar").not("." + CP + "-alpha");
    $hue.attr("aria-valuenow", st.h).find("." + CP + "-bar-pointer").css("top", (st.h / 360 * 100) + "%");
    var pct = Math.round(st.a / 255 * 100);
    var $alpha = $el.find("." + CP + "-alpha");
    $alpha.css("--ah-cp-rgb", hex6).attr({ "aria-valuenow": pct, "aria-valuetext": pct + "%" })
      .find("." + CP + "-bar-pointer").css("top", (100 - pct) + "%");
    $el.find("." + CP + "-preview").css("background-color", cpRgba(st));
    if (skip !== "hex") {
      $el.find("." + CP + "-hex-input").val(cpHex(st).slice(1));
    }
    if (skip !== "rgb") {
      $el.find("." + CP + "-r-input").val(c.r);
      $el.find("." + CP + "-g-input").val(c.g);
      $el.find("." + CP + "-b-input").val(c.b);
      $el.find("." + CP + "-a-input").val(pct);
    }
  }

  // The value-bearing side: data-ah-value, hidden input, trigger, swatches.
  function cpSetValue(el, value) {
    var $el = $(el);
    var st = cpState(el);
    el.setAttribute("data-ah-value", value);
    $el.children("input[type=hidden]").val(value);
    var $trigger = $el.children("." + CP + "-trigger");
    $trigger.find("." + CP + "-trigger-swatch")
      .toggleClass(CP + "-trigger-empty", value === "")
      .css("--ah-cp-swatch", value === "" ? "" : cpRgba(st));
    $trigger.find("." + CP + "-trigger-text")
      .text(value === "" ? ($trigger.attr("data-placeholder") || "") : value);
    $el.find("." + CP + "-swatch").each(function () {
      this.setAttribute("aria-pressed", String(this.getAttribute("data-color") === value));
    });
  }

  // type: "input" (live) or "change" (commit). change fires only when the
  // value differs from the last committed one.
  function cpEmit(el, type, skip) {
    var st = cpState(el);
    cpSync(el, skip);
    var value = cpHex(st);
    var before = el.getAttribute("data-ah-value");
    cpSetValue(el, value);
    if (type === "input") {
      if (value !== before) { $(el).trigger("input", [{ value: value }]); }
    } else {
      cpCommit(el, value);
    }
  }

  function cpCommit(el, value) {
    var st = cpState(el);
    if (value !== st.committed) {
      st.committed = value;
      $(el).trigger("change", [{ value: value }]);
    }
  }

  function cpClear(el) {
    cpSetValue(el, "");
    cpCommit(el, "");
  }

  AH.define("colorpicker", {
    init: function (el, $el) {
      el.setAttribute("data-ah-uid", ++seq);
      var value = el.getAttribute("data-ah-value") || "";
      var alpha = el.getAttribute("data-alpha") === "true";
      var st = { h: 0, s: 100, v: 100, a: 255, alpha: alpha, committed: value };
      $.data(el, "ah-cp", st);
      var start = parseHex(value, alpha);
      if (start) { cpLoad(st, start); }
      var $trigger = $el.children("." + CP + "-trigger");
      var $popup = popupOf($el);
      var popup = $popup.length > 0;
      if (popup) {
        $popup.attr("id", $popup.attr("id") || uid("ah-cp-popup-"));
        $trigger.attr("aria-controls", $popup.attr("id"));
      }
      fenceNativeEvents($el);

      function open() {
        openPopup(el, $el, CP, $trigger, function () { cpSync(el); });
        $el.find("." + CP + "-map").trigger("focus");
      }
      function close(refocus) { closePopup(el, $el, CP, $trigger, refocus); }
      $.data(el, "ah-cp-open", open);
      $.data(el, "ah-cp-close", close);

      $trigger.on("click" + NS, function (e) {
        e.preventDefault();
        if (isOpen($el)) { close(true); } else { open(); }
      });
      $trigger.on("keydown" + NS, function (e) {
        if (e.key === "ArrowDown") { e.preventDefault(); open(); }
      });
      $el.on("keydown" + NS, function (e) {
        if (e.key === "Escape" && isOpen($el)) {
          e.preventDefault();
          e.stopPropagation();
          close(true);
        }
      });

      // Saturation/value area (sigil 处理面板拖拽).
      var $map = $el.find("." + CP + "-map");
      function fromMap(e) {
        var r = $map[0].getBoundingClientRect();
        var p = pointerXY(e);
        st.s = Math.round(clamp((p.x - r.left) / r.width, 0, 1) * 100);
        st.v = Math.round((1 - clamp((p.y - r.top) / r.height, 0, 1)) * 100);
        cpEmit(el, "input");
      }
      function endDrag() { cpEmit(el, "change"); }
      drag($map, el, fromMap, fromMap, endDrag);

      // Hue bar (sigil 处理色相拖拽) and alpha bar.
      var $hue = $el.find("." + CP + "-bar").not("." + CP + "-alpha");
      function fromHue(e) {
        var r = $hue[0].getBoundingClientRect();
        st.h = Math.round(clamp((pointerXY(e).y - r.top) / r.height, 0, 1) * 360) % 360;
        cpEmit(el, "input");
      }
      drag($hue, el, fromHue, fromHue, endDrag);
      var $alpha = $el.find("." + CP + "-alpha");
      function fromAlpha(e) {
        var r = $alpha[0].getBoundingClientRect();
        st.a = Math.round((1 - clamp((pointerXY(e).y - r.top) / r.height, 0, 1)) * 255);
        cpEmit(el, "input");
      }
      if ($alpha.length) { drag($alpha, el, fromAlpha, fromAlpha, endDrag); }

      // Keyboard: arrows move by 1, Shift by 10; Home / End.
      function keys($t, apply) {
        $t.on("keydown" + NS, function (e) {
          if (disabled(el)) { return; }
          var n = e.shiftKey ? 10 : 1;
          if (apply(e.key, n) !== false) {
            e.preventDefault();
            cpEmit(el, "input");
            cpEmit(el, "change");
          }
        });
      }
      keys($map, function (key, n) {
        switch (key) {
          case "ArrowLeft": st.s = clamp(st.s - n, 0, 100); break;
          case "ArrowRight": st.s = clamp(st.s + n, 0, 100); break;
          case "ArrowUp": st.v = clamp(st.v + n, 0, 100); break;
          case "ArrowDown": st.v = clamp(st.v - n, 0, 100); break;
          case "Home": st.s = 0; break;
          case "End": st.s = 100; break;
          default: return false;
        }
      });
      // The hue grows downwards on the bar, so Down increases it.
      keys($hue, function (key, n) {
        switch (key) {
          case "ArrowDown": case "ArrowRight": st.h = (st.h + n) % 360; break;
          case "ArrowUp": case "ArrowLeft": st.h = (st.h - n + 360) % 360; break;
          case "Home": st.h = 0; break;
          case "End": st.h = 359; break;
          default: return false;
        }
      });
      keys($alpha, function (key, n) {
        var step = Math.round(n * 2.55);
        switch (key) {
          case "ArrowUp": case "ArrowRight": st.a = clamp(st.a + step, 0, 255); break;
          case "ArrowDown": case "ArrowLeft": st.a = clamp(st.a - step, 0, 255); break;
          case "Home": st.a = 0; break;
          case "End": st.a = 255; break;
          default: return false;
        }
      });

      // Hex input (sigil 处理hex输入): live while valid, commit on change.
      var $hexIn = $el.find("." + CP + "-hex-input");
      $hexIn.on("input" + NS, function () {
        var c = parseHex($hexIn.val(), alpha);
        var len = String($hexIn.val()).trim().replace(/^#/, "").length;
        if (c && len >= 6) {
          cpLoad(st, c);
          cpEmit(el, "input", "hex");
        }
      });
      $hexIn.on("change" + NS, function () {
        var c = parseHex($hexIn.val(), alpha);
        if (c) { cpLoad(st, c); }
        cpEmit(el, "change");
      });
      $hexIn.on("keydown" + NS, function (e) {
        if (e.key === "Enter") { e.preventDefault(); $hexIn.trigger("change"); }
      });

      // RGB(A) inputs (sigil 处理rgb输入).
      var $rgbIn = $el.find("." + CP + "-r-input, ." + CP + "-g-input, ." + CP + "-b-input, ." + CP + "-a-input");
      function fromRgb() {
        var n = function (cls, max) {
          var v = parseInt($el.find("." + CP + "-" + cls + "-input").val(), 10);
          return isNaN(v) ? null : clamp(v, 0, max);
        };
        var r = n("r", 255), g = n("g", 255), b = n("b", 255);
        if (r === null || g === null || b === null) { return false; }
        var a = alpha ? n("a", 100) : 100;
        cpLoad(st, { r: r, g: g, b: b, a: a === null ? st.a : Math.round(a * 2.55) });
        return true;
      }
      $rgbIn.on("input" + NS, function () {
        if (fromRgb()) { cpEmit(el, "input", "rgb"); }
      });
      $rgbIn.on("change" + NS, function () {
        fromRgb();
        cpEmit(el, "change");
      });

      // Swatches and the clear link (sigil's transparent link).
      $el.on("click" + NS, "." + CP + "-swatch", function (e) {
        e.preventDefault();
        var c = parseHex(this.getAttribute("data-color"), alpha);
        if (!c || disabled(el)) { return; }
        cpLoad(st, c);
        cpEmit(el, "input");
        cpEmit(el, "change");
      });
      $el.on("click" + NS, "." + CP + "-transparent a", function (e) {
        e.preventDefault();
        if (disabled(el)) { return; }
        cpClear(el);
        close(true);
      });
      cpSync(el);
    },
    destroy: function (el) { stopFloat(el); },
    methods: {
      getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
      // Set the value without firing events (server driven); "" clears.
      setValue: function (el, $el, v) {
        var st = cpState(el);
        var c = parseHex(v == null ? "" : String(v), st.alpha);
        if (!c) {
          st.committed = "";
          cpSetValue(el, "");
          return;
        }
        cpLoad(st, c);
        cpSync(el);
        st.committed = cpHex(st);
        cpSetValue(el, st.committed);
      },
      clear: function (el) { cpClear(el); },
      open: function (el) { $.data(el, "ah-cp-open")(); },
      close: function (el) { $.data(el, "ah-cp-close")(false); }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/form_upload.js ---- */
/* Upload behaviour (form_upload.js), ported from sigil form/upload and its
 * upload.core, drop-zone and picker helpers (designs/04-components.md).
 *
 * The root carries the configuration rendered by aihtml_form_upload:
 * data-ah-url (absent: native file input mode), data-ah-field,
 * data-ah-max-size, data-ah-max-count, data-ah-manual (no auto upload),
 * data-ah-headers / data-ah-extra / data-ah-labels (JSON),
 * data-ah-credentials. The value is a JSON array in data-ah-value (and the
 * hidden input): the server's answers for the uploaded files, or in
 * native mode {name, size, type} of the selected files. List rows come
 * from the shared template AH.tpl.upload_item. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  function json(el, attr, dflt) {
    var s = el.getAttribute(attr);
    if (!s) { return dflt; }
    try { return JSON.parse(s); } catch (e) { return dflt; }
  }

  function intAttr(el, attr) {
    var n = parseInt(el.getAttribute(attr), 10);
    return isNaN(n) ? null : n;
  }

  // sigil upload.core/format-size
  function formatSize(bytes) {
    if (bytes == null) { return ""; }
    if (bytes < 1024) { return bytes + " B"; }
    if (bytes < 1024 * 1024) { return (bytes / 1024).toFixed(1) + " KB"; }
    return (bytes / (1024 * 1024)).toFixed(1) + " MB";
  }

  function iconKind(type) {
    var m = /^(image|video|audio)\//.exec(type || "");
    return m ? m[1] : "file";
  }

  // sigil upload.core/file-accepted?: "image/*", "image/png", ".pdf"
  function accepted(file, accept) {
    if (!accept || accept === "*" || accept === "*/*") { return true; }
    var type = file.type || "", name = (file.name || "").toLowerCase();
    return accept.split(",").some(function (p) {
      p = p.trim().toLowerCase();
      if (!p) { return false; }
      if (p === "*" || p === "*/*") { return true; }
      if (p.charAt(0) === ".") { return name.slice(-p.length) === p; }
      if (/\/\*$/.test(p)) { return type.toLowerCase().indexOf(p.slice(0, -1)) === 0; }
      return type.toLowerCase() === p;
    });
  }

  // A value entry as shown in the list: a name, or an object with name/size/type.
  function describe(v) {
    if (v !== null && typeof v === "object") {
      return { name: v.name == null ? "" : String(v.name),
               size: typeof v.size === "number" ? v.size : null,
               type: v.type ? String(v.type) : "" };
    }
    return { name: v == null ? "" : String(v), size: null, type: "" };
  }

  function st(el) { return $.data(el, "ahUpload"); }

  function cfg(el) { return st(el).cfg; }

  function view(el, f) {
    var s = st(el);
    var uploading = f.status === "uploading";
    return {
      cls: "ah-upload-item ah-upload-item-" + f.status,
      id: f.id, icon: iconKind(f.type), name: f.name, size: formatSize(f.size),
      uploading: uploading, percent: Math.round(f.percent || 0),
      has_error: f.status === "error", error: f.error || "",
      remove: s.cfg.labels.remove || "Remove", disabled: s.disabled
    };
  }

  function $list(el) { return $(el).children(".ah-upload-list"); }

  function rowOf(el, id) {
    return $list(el).children().filter(function () {
      return this.getAttribute("data-file-id") === id;
    });
  }

  function renderRow(el, f) {
    var $l = $list(el);
    if (!$l.length) { return; }
    var html = AH.tpl.upload_item(view(el, f));
    var $row = rowOf(el, f.id);
    if ($row.length) { $row.replaceWith(html); } else { $l.append(html); }
  }

  function renderAll(el) {
    var s = st(el), $l = $list(el);
    if (!$l.length) { return; }
    $l.html(s.files.map(function (f) { return AH.tpl.upload_item(view(el, f)); }).join(""));
  }

  function find(el, id) {
    var files = st(el).files;
    for (var i = 0; i < files.length; i++) { if (files[i].id === id) { return files[i]; } }
    return null;
  }

  // ------------------------------------------------------------------
  // Value
  // ------------------------------------------------------------------

  function valueOf(el) {
    var s = st(el);
    var out = [];
    s.files.forEach(function (f) {
      if (f.status === "success") { out.push(f.value); }
      else if (s.cfg.native && f.status === "pending") {
        out.push({ name: f.name, size: f.size, type: f.type });
      }
    });
    return out;
  }

  // Native mode: the file input holds the selected files, for the form.
  function syncInput(el) {
    var s = st(el);
    if (!s.cfg.native || typeof DataTransfer === "undefined") { return; }
    try {
      var dt = new DataTransfer();
      s.files.forEach(function (f) { if (f.file && f.status === "pending") { dt.items.add(f.file); } });
      s.input.files = dt.files;
    } catch (e) { /* old browsers: the input keeps the last pick */ }
  }

  // Writes data-ah-value and the hidden input; fires change when it moved.
  function commit(el, silent) {
    var v = JSON.stringify(valueOf(el));
    syncInput(el);
    if (v === el.getAttribute("data-ah-value")) { return; }
    el.setAttribute("data-ah-value", v);
    $(el).children("input[type=hidden]").val(v);
    if (!silent) { $(el).trigger("change"); }
  }

  // ------------------------------------------------------------------
  // Adding files (sigil add-files! and upload.core/validate-files)
  // ------------------------------------------------------------------

  function addFiles(el, list) {
    var s = st(el), c = s.cfg;
    if (s.disabled) { return; }
    var files = Array.prototype.slice.call(list || []);
    if (!files.length) { return; }
    if (!c.multiple) {
      files = files.slice(0, 1);
      s.files.slice().forEach(function (f) { drop(el, f); });
    }
    var kept = s.files.filter(function (f) { return f.status !== "error"; }).length;
    var valid = [], added = [];
    files.forEach(function (file) {
      var reason = null;
      if (!accepted(file, c.accept)) { reason = "type_mismatch"; }
      else if (c.maxSize && file.size > c.maxSize) { reason = "too_large"; }
      else if (c.maxCount && kept + valid.length >= c.maxCount) { reason = "too_many"; }
      var f = { id: "f" + (++seq), file: file, name: file.name, size: file.size,
                type: file.type, status: reason ? "error" : "pending", percent: 0,
                error: reason ? (c.labels[reason] || reason) : "", value: null, xhr: null };
      s.files.push(f);
      added.push(f);
      if (reason) {
        $(el).trigger("ah:upload-error", [{ id: f.id, name: f.name, reason: reason }]);
      } else {
        valid.push(f);
      }
    });
    added.forEach(function (f) { renderRow(el, f); });
    if (valid.length) {
      $(el).trigger("ah:select", [{ files: valid.map(function (f) { return f.file; }) }]);
    }
    commit(el);
    if (!c.native && !c.manual) { valid.forEach(function (f) { start(el, f); }); }
  }

  // ------------------------------------------------------------------
  // XHR upload (sigil upload.core/upload!)
  // ------------------------------------------------------------------

  function start(el, f) {
    var c = cfg(el);
    if (c.native || f.status !== "pending") { return; }
    var xhr = new XMLHttpRequest();
    var data = new FormData();
    data.append(c.field, f.file, f.name);
    $.each(c.extra, function (k, v) { data.append(k, v); });
    f.xhr = xhr;
    f.status = "uploading";
    f.percent = 0;
    renderRow(el, f);
    xhr.upload.onprogress = function (e) {
      if (!e.lengthComputable || f.xhr !== xhr) { return; }
      f.percent = e.total ? (e.loaded / e.total) * 100 : 0;
      rowOf(el, f.id).find(".ah-upload-item-progress")
        .attr("aria-valuenow", Math.round(f.percent))
        .children(".ah-upload-item-progress-bar").css("width", f.percent + "%");
      $(el).trigger("ah:upload-progress", [{ id: f.id, percent: f.percent,
                                             loaded: e.loaded, total: e.total }]);
    };
    xhr.onload = function () {
      if (f.xhr !== xhr) { return; }
      f.xhr = null;
      var body = null;
      try { body = JSON.parse(xhr.responseText); } catch (e) { body = null; }
      if (xhr.status >= 200 && xhr.status <= 299) {
        f.status = "success";
        f.percent = 100;
        f.value = body !== null && typeof body === "object"
          ? body : { name: f.name, size: f.size, type: f.type };
        renderRow(el, f);
        $(el).trigger("ah:upload-success", [{ id: f.id, response: f.value }]);
        commit(el);
      } else {
        var msg = body && typeof body === "object" && (body.error || body.message);
        fail(el, f, typeof msg === "string" && msg ? msg : c.labels.upload_failed,
             { status: xhr.status, response: body });
      }
    };
    xhr.onerror = function () {
      if (f.xhr !== xhr) { return; }
      f.xhr = null;
      fail(el, f, c.labels.network_error, { status: 0 });
    };
    xhr.open("POST", c.url, true);
    if (c.credentials) { xhr.withCredentials = true; }
    $.each(c.headers, function (k, v) { xhr.setRequestHeader(k, v); });
    xhr.send(data);
  }

  function fail(el, f, message, detail) {
    f.status = "error";
    f.error = message || "Upload failed";
    renderRow(el, f);
    $(el).trigger("ah:upload-error", [$.extend({ id: f.id, name: f.name, message: f.error },
                                               detail)]);
  }

  // Take a row out of the state (aborting its upload), without committing.
  function drop(el, f) {
    var s = st(el);
    if (f.xhr) { var x = f.xhr; f.xhr = null; x.abort(); }
    s.files = s.files.filter(function (g) { return g !== f; });
    rowOf(el, f.id).remove();
  }

  function remove(el, id) {
    var f = find(el, id);
    if (!f) { return; }
    drop(el, f);
    commit(el);
  }

  // ------------------------------------------------------------------
  // Picker and drop zone
  // ------------------------------------------------------------------

  function open(el) {
    var s = st(el);
    if (s.disabled) { return; }
    // native mode keeps the selection in the input (a cancelled dialog
    // must not lose it); XHR mode empties it so the same file can be re-picked
    if (!s.cfg.native) { s.input.value = ""; }
    s.input.click();
  }

  function setDisabled(el, $el, off) {
    var s = st(el);
    s.disabled = !!off;
    $el.toggleClass("ah-upload-disabled", s.disabled);
    if (s.disabled) { $el.attr("aria-disabled", "true"); } else { $el.removeAttr("aria-disabled"); }
    $el.children(".ah-upload-dragger").attr("tabindex", s.disabled ? "-1" : "0")
      .attr("aria-disabled", s.disabled ? "true" : null);
    s.input.disabled = s.disabled;
    $list(el).find(".ah-upload-item-remove").prop("disabled", s.disabled);
  }

  function initialFiles(el, values) {
    var ids = $list(el).children().map(function () {
      return this.getAttribute("data-file-id");
    }).get();
    return values.map(function (v, i) {
      var d = describe(v);
      return { id: ids[i] || "s" + i, file: null, name: d.name, size: d.size, type: d.type,
               status: "success", percent: 100, error: "", value: v, xhr: null };
    });
  }

  AH.define("upload", {
    init: function (el, $el) {
      var url = el.getAttribute("data-ah-url");
      var input = $el.children("input.ah-upload-input")[0];
      var s = {
        input: input,
        disabled: $el.hasClass("ah-upload-disabled"),
        drag: 0,
        cfg: {
          url: url, native: !url,
          field: el.getAttribute("data-ah-field") || "file",
          accept: input.getAttribute("accept") || "",
          multiple: input.multiple,
          maxSize: intAttr(el, "data-ah-max-size"),
          maxCount: intAttr(el, "data-ah-max-count"),
          manual: el.hasAttribute("data-ah-manual"),
          credentials: el.hasAttribute("data-ah-credentials"),
          headers: json(el, "data-ah-headers", {}),
          extra: json(el, "data-ah-extra", {}),
          labels: json(el, "data-ah-labels", {})
        },
        files: []
      };
      $.data(el, "ahUpload", s);
      s.files = initialFiles(el, json(el, "data-ah-value", []));

      var $dragger = $el.children(".ah-upload-dragger");
      $dragger.on("click" + NS, function (e) {
        e.preventDefault();
        open(el);
      }).on("keydown" + NS, function (e) {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          open(el);
        }
      });
      // The input's own change is not the component's.
      $(input).on("change" + NS, function (e) {
        e.stopPropagation();
        var picked = Array.prototype.slice.call(input.files || []);
        addFiles(el, picked);
        if (!s.cfg.native) { input.value = ""; }
      }).on("click" + NS, function (e) { e.stopPropagation(); });

      // sigil drop-zone: a counter for nested dragenter/dragleave.
      $el.on("dragenter" + NS, function (e) {
        e.preventDefault();
        if (s.disabled) { return; }
        if (++s.drag === 1) { $dragger.addClass("ah-upload-dragger-active"); }
      }).on("dragover" + NS, function (e) {
        e.preventDefault();
        var dt = e.originalEvent && e.originalEvent.dataTransfer;
        if (dt) { dt.dropEffect = s.disabled ? "none" : "copy"; }
      }).on("dragleave" + NS, function (e) {
        e.preventDefault();
        if (--s.drag <= 0) {
          s.drag = 0;
          $dragger.removeClass("ah-upload-dragger-active");
        }
      }).on("drop" + NS, function (e) {
        e.preventDefault();
        s.drag = 0;
        $dragger.removeClass("ah-upload-dragger-active");
        var dt = e.originalEvent && e.originalEvent.dataTransfer;
        if (dt && dt.files && dt.files.length) { addFiles(el, dt.files); }
      });

      $el.on("click" + NS, ".ah-upload-item-remove", function (e) {
        e.preventDefault();
        e.stopPropagation();
        if (s.disabled) { return; }
        var $row = $(this).closest(".ah-upload-item");
        var $next = $row.next().find(".ah-upload-item-remove");
        if (!$next.length) { $next = $row.prev().find(".ah-upload-item-remove"); }
        remove(el, $row.attr("data-file-id"));
        ($next.length ? $next : $dragger).trigger("focus");
      });
    },
    destroy: function (el) {
      var s = st(el);
      if (!s) { return; }
      s.files.forEach(function (f) { if (f.xhr) { var x = f.xhr; f.xhr = null; x.abort(); } });
      $.removeData(el, "ahUpload");
    },
    methods: {
      getValue: function (el) { return valueOf(el); },
      setValue: function (el, $el, files) {
        var s = st(el);
        s.files.slice().forEach(function (f) { if (f.status === "success") { drop(el, f); } });
        s.files = (files || []).map(function (v) {
          var d = describe(v);
          return { id: "f" + (++seq), file: null, name: d.name, size: d.size, type: d.type,
                   status: "success", percent: 100, error: "", value: v, xhr: null };
        }).concat(s.files);
        renderAll(el);
        commit(el, true);
      },
      getFiles: function (el) {
        return st(el).files.map(function (f) {
          return { id: f.id, name: f.name, size: f.size, type: f.type, status: f.status,
                   percent: f.percent, value: f.value, error: f.error || null, file: f.file };
        });
      },
      uploadAll: function (el) {
        st(el).files.slice().forEach(function (f) { start(el, f); });
      },
      upload: function (el, $el, id) {
        var f = find(el, id);
        if (f) { start(el, f); }
      },
      remove: function (el, $el, id) { remove(el, id); },
      clear: function (el) {
        st(el).files.slice().forEach(function (f) { drop(el, f); });
        commit(el);
      },
      open: function (el) { open(el); },
      enable: function (el, $el) { setDisabled(el, $el, false); },
      disable: function (el, $el) { setDisabled(el, $el, true); }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/layout_bars.js ---- */
/* Behaviours of the layout_bars components (designs/04-components.md):
 * activity-bar, navigationbar and command, ported from sigil's
 * layout/activity_bar, layout/navigationbar and overlay/command.
 *
 * activity-bar and navigationbar keep their value in data-ah-value on the
 * root (the active item; the expanded indexes "0,2"), mirror it into a
 * hidden input and fire "change" when the user changes it. Methods called
 * by the server (AH.invoke / aihtml_action:call) do not fire "change".
 *
 * command filters the commands the server rendered by hiding the ones that
 * do not match; with a search action (data-ah-remote) the server renders
 * the results instead. It builds no HTML.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  function setValue(el, v) {
    el.setAttribute("data-ah-value", v);
    $(el).children("input[type=hidden]").val(v);
  }

  // ------------------------------------------------------------------
  // ActivityBar: a vertical tablist of icon buttons
  // ------------------------------------------------------------------

  function barItems(el) {
    return $(el).children(".ah-activity-bar__item");
  }

  function barActivate(el, id) {
    barItems(el).each(function () {
      var on = this.getAttribute("data-id") === String(id);
      this.setAttribute("data-active", String(on));
      this.setAttribute("aria-selected", String(on));
      this.setAttribute("tabindex", on ? "0" : "-1");
    });
    // keep one item reachable with Tab when nothing is active
    var $items = barItems(el);
    if (!$items.filter("[tabindex=0]").length) {
      $items.not("[data-disabled=true]").first().attr("tabindex", "0");
    }
    setValue(el, id == null ? "" : String(id));
  }

  function barChoose(el, $el, item) {
    if (item.getAttribute("data-disabled") === "true") { return; }
    var id = item.getAttribute("data-id");
    var changed = el.getAttribute("data-ah-value") !== id;
    barActivate(el, id);
    $el.trigger("ah:select", [id]);
    if (changed) { $el.trigger("change"); }
  }

  AH.define("activity-bar", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-activity-bar__item", function () {
        barChoose(el, $el, this);
      });
      // WAI-ARIA tabs: arrows move and activate, Home / End jump
      $el.on("keydown" + NS, ".ah-activity-bar__item", function (e) {
        var $en = barItems(el).not("[data-disabled=true]");
        var i = $en.index(this);
        var next;
        switch (e.key) {
          case "ArrowDown": case "ArrowRight": next = (i + 1) % $en.length; break;
          case "ArrowUp": case "ArrowLeft": next = (i - 1 + $en.length) % $en.length; break;
          case "Home": next = 0; break;
          case "End": next = $en.length - 1; break;
          default: return;
        }
        e.preventDefault();
        var t = $en[next];
        if (t) {
          t.focus();
          barChoose(el, $el, t);
        }
      });
    },
    methods: {
      setValue: function (el, $el, v) { barActivate(el, v); },
      getValue: function (el) { return el.getAttribute("data-ah-value"); }
    }
  });

  // ------------------------------------------------------------------
  // NavigationBar: collapsible sections
  // ------------------------------------------------------------------

  function navItems(el) {
    return $(el).children(".ah-navigationbar-item");
  }

  function navHeader(el, i) {
    return navItems(el).eq(i).children(".ah-navigationbar-header");
  }

  function navExpanded(el) {
    return (el.getAttribute("data-ah-value") || "").split(",").filter(function (s) {
      return s !== "";
    }).map(Number);
  }

  function navStore(el, list) {
    var uniq = list.filter(function (v, i) { return list.indexOf(v) === i; });
    uniq.sort(function (a, b) { return a - b; });
    setValue(el, uniq.join(","));
  }

  function navMode(el) {
    return el.getAttribute("data-expand-mode") || "single_fit_height";
  }

  // single_fit_height with a fixed height: the open body fills what the
  // headers leave (sigil's sync-content-height!).
  function navFit(el) {
    if (!el.hasAttribute("data-fit")) { return; }
    var used = 0;
    navItems(el).each(function () {
      var $it = $(this);
      used += $it.children(".ah-navigationbar-header").outerHeight() +
        ($it.outerHeight() - $it.innerHeight());
    });
    var h = Math.max(0, $(el).innerHeight() - used);
    navItems(el).children(".ah-navigationbar-body").each(function () {
      if (this.style.display !== "none") { $(this).css("height", h + "px"); }
    });
  }

  function navAnimate(el, $el, i, open) {
    var $h = navHeader(el, i);
    var $body = navItems(el).eq(i).children(".ah-navigationbar-body");
    var anim = el.getAttribute("data-animation") || "slide";
    var ms = parseInt(el.getAttribute(open ? "data-expand-duration" : "data-collapse-duration"), 10);
    if (isNaN(ms)) { ms = 250; }
    $h.toggleClass("ah-navigationbar-header-expanded", open).attr("aria-expanded", String(open));
    $h.children(".ah-navigationbar-arrow").toggleClass("ah-navigationbar-arrow-up", open);
    var done = function () {
      if (open) {
        $body.show();
        navFit(el);
      } else {
        $body.hide();
      }
      $el.trigger(open ? "ah:expand" : "ah:collapse", [{ index: i }]);
    };
    $body.stop(true, true);
    if (anim === "slide") {
      $body[open ? "slideDown" : "slideUp"]({ duration: ms, complete: done });
    } else if (anim === "fade") {
      $body[open ? "fadeIn" : "fadeOut"]({ duration: ms, complete: done });
    } else {
      done();
    }
  }

  function navValid(el, i) {
    return typeof i === "number" && i >= 0 && i < navItems(el).length;
  }

  function navDisabled(el, i) {
    return navHeader(el, i).hasClass("ah-navigationbar-disabled");
  }

  function navCollapse(el, $el, i) {
    var cur = navExpanded(el);
    if (!navValid(el, i) || cur.indexOf(i) < 0) { return false; }
    navStore(el, cur.filter(function (x) { return x !== i; }));
    navAnimate(el, $el, i, false);
    return true;
  }

  function navExpand(el, $el, i) {
    var cur = navExpanded(el);
    if (!navValid(el, i) || navDisabled(el, i) || cur.indexOf(i) >= 0) { return false; }
    if (navMode(el) !== "multiple") {
      cur.forEach(function (x) { navCollapse(el, $el, x); });
    }
    navStore(el, navExpanded(el).concat([i]));
    navAnimate(el, $el, i, true);
    return true;
  }

  // A user toggle, as sigil's compute-proposed-indexes: single modes never
  // close the open section, none never changes.
  function navUserToggle(el, $el, i) {
    var mode = navMode(el);
    if (el.classList.contains("ah-navigationbar-disabled") || navDisabled(el, i) ||
        mode === "none") {
      return;
    }
    var changed;
    if (navExpanded(el).indexOf(i) >= 0) {
      changed = (mode === "single" || mode === "single_fit_height") ? false
        : navCollapse(el, $el, i);
    } else {
      changed = navExpand(el, $el, i);
    }
    if (changed) { $el.trigger("change"); }
  }

  function navIndex(el, header) {
    return navItems(el).children(".ah-navigationbar-header").index(header);
  }

  AH.define("navigationbar", {
    init: function (el, $el) {
      var mode = el.getAttribute("data-toggle-mode") || "click";
      var own = function (h) { return $(h).closest(".ah-navigationbar")[0] === el; };
      if (mode !== "none") {
        $el.on(mode + NS, ".ah-navigationbar-header", function () {
          if (own(this)) { navUserToggle(el, $el, navIndex(el, this)); }
        });
      }
      $el.on("keydown" + NS, ".ah-navigationbar-header", function (e) {
        if (!own(this)) { return; }
        var $hs = navItems(el).children(".ah-navigationbar-header").filter("[tabindex=0]");
        var i = $hs.index(this);
        var t;
        switch (e.key) {
          case "Enter": case " ":
            e.preventDefault();
            if (mode !== "none") { navUserToggle(el, $el, navIndex(el, this)); }
            return;
          case "ArrowDown": t = $hs[(i + 1) % $hs.length]; break;
          case "ArrowUp": t = $hs[(i - 1 + $hs.length) % $hs.length]; break;
          case "Home": t = $hs[0]; break;
          case "End": t = $hs[$hs.length - 1]; break;
          default: return;
        }
        e.preventDefault();
        if (t) { t.focus(); }
      });
      navFit(el);
    },
    methods: {
      expand: function (el, $el, i) { navExpand(el, $el, Number(i)); },
      collapse: function (el, $el, i) { navCollapse(el, $el, Number(i)); },
      toggle: function (el, $el, i) {
        i = Number(i);
        if (navExpanded(el).indexOf(i) >= 0) { navCollapse(el, $el, i); } else { navExpand(el, $el, i); }
      },
      setValue: function (el, $el, v) {
        var want = (Array.isArray(v) ? v : String(v == null ? "" : v).split(","))
          .filter(function (s) { return s !== ""; }).map(Number);
        navExpanded(el).forEach(function (i) {
          if (want.indexOf(i) < 0) { navCollapse(el, $el, i); }
        });
        want.forEach(function (i) {
          if (navExpanded(el).indexOf(i) < 0 && navValid(el, i)) {
            // setValue may open several sections whatever the mode
            navStore(el, navExpanded(el).concat([i]));
            navAnimate(el, $el, i, true);
          }
        });
      },
      getValue: function (el) { return navExpanded(el); },
      enable: function (el, $el, i) {
        navHeader(el, Number(i)).removeClass("ah-navigationbar-disabled")
          .removeAttr("aria-disabled").attr("tabindex", "0");
      },
      disable: function (el, $el, i) {
        navHeader(el, Number(i)).addClass("ah-navigationbar-disabled")
          .attr({ "aria-disabled": "true", tabindex: "-1" });
      }
    }
  });

  // ------------------------------------------------------------------
  // Command: search field + filtered list + keyboard navigation
  // ------------------------------------------------------------------

  var hotkeySeq = 0;

  function cmdInput($el) { return $el.find(".ah-command__input"); }
  function cmdVisible($el) { return $el.find(".ah-command__item").not("[hidden]"); }
  function cmdOverlay($el) { return $el.parent(".ah-command-overlay"); }

  function cmdActive(el, $el) {
    return cmdVisible($el).index($el.find(".ah-command__item[data-active=true]"));
  }

  function cmdSetActive(el, $el, i, scroll) {
    var $v = cmdVisible($el);
    var $input = cmdInput($el);
    $el.find(".ah-command__item[data-active=true]")
      .attr({ "data-active": "false", "aria-selected": "false" });
    if (!$v.length) {
      $input.removeAttr("aria-activedescendant");
      return;
    }
    i = ((i % $v.length) + $v.length) % $v.length;
    var it = $v[i];
    it.setAttribute("data-active", "true");
    it.setAttribute("aria-selected", "true");
    if (it.id) { $input.attr("aria-activedescendant", it.id); }
    if (scroll && it.scrollIntoView) { it.scrollIntoView({ block: "nearest" }); }
  }

  // sigil's match?: a case-insensitive substring of value, label or
  // description.
  function cmdFilter(el, $el) {
    if (el.hasAttribute("data-ah-remote")) { return; }
    var q = String(cmdInput($el).val() || "").trim().toLowerCase();
    $el.find(".ah-command__item").each(function () {
      var $it = $(this);
      var hay = [this.getAttribute("data-value") || "",
                 $it.find(".ah-command__item-label").text(),
                 $it.find(".ah-command__item-desc").text()].join("\n").toLowerCase();
      this.hidden = q !== "" && hay.indexOf(q) < 0;
    });
    var any = false;
    $el.find(".ah-command__group").each(function () {
      var shown = $(this).children(".ah-command__item").not("[hidden]").length > 0;
      this.hidden = !shown;
      any = any || shown;
    });
    $el.find(".ah-command__empty").prop("hidden", any);
    cmdSetActive(el, $el, 0, true);
  }

  function cmdFocus($el) {
    var inp = cmdInput($el)[0];
    if (inp) {
      inp.focus();
      var n = inp.value.length;
      try { inp.setSelectionRange(n, n); } catch (e) { /* not a text field */ }
    }
  }

  function cmdSelect(el, $el, it) {
    if (!it || it.getAttribute("data-disabled") === "true") { return; }
    var v = it.getAttribute("data-value");
    el.setAttribute("data-ah-value", v);
    $el.trigger("ah:select", [v]);
    if (cmdOverlay($el).length && el.getAttribute("data-close-on-select") !== "false") {
      cmdClose(el, $el);
    }
    var href = it.getAttribute("data-href");
    if (href) { window.location.href = href; }
  }

  function cmdOpen(el, $el) {
    var ov = cmdOverlay($el)[0];
    if (!ov || !ov.hidden) { return; }
    $.data(el, "ah-cmd-return", document.activeElement);
    if (!el.hasAttribute("data-ah-remote")) {
      cmdInput($el).val(el.getAttribute("data-ah-query") || "");
      cmdFilter(el, $el);
    }
    ov.hidden = false;
    cmdFocus($el);
    $el.trigger("ah:open");
  }

  function cmdClose(el, $el) {
    var ov = cmdOverlay($el)[0];
    if (!ov || ov.hidden) { return; }
    ov.hidden = true;
    var back = $.data(el, "ah-cmd-return");
    $.removeData(el, "ah-cmd-return");
    if (back && back.focus && document.contains(back)) { back.focus(); }
    $el.trigger("ah:close");
  }

  AH.define("command", {
    init: function (el, $el) {
      $el.on("input" + NS, ".ah-command__input", function () {
        cmdFilter(el, $el);
        $el.trigger("ah:query", [this.value]);
      });
      $el.on("keydown" + NS, ".ah-command__input", function (e) {
        var act = cmdActive(el, $el);
        switch (e.key) {
          case "ArrowDown": e.preventDefault(); cmdSetActive(el, $el, act + 1, true); break;
          case "ArrowUp": e.preventDefault(); cmdSetActive(el, $el, act - 1, true); break;
          case "Home": if (e.ctrlKey) { e.preventDefault(); cmdSetActive(el, $el, 0, true); } break;
          case "End": if (e.ctrlKey) { e.preventDefault(); cmdSetActive(el, $el, -1, true); } break;
          case "Enter":
            e.preventDefault();
            cmdSelect(el, $el, cmdVisible($el)[act]);
            break;
          case "Escape":
            e.preventDefault();
            if (cmdOverlay($el).length) { cmdClose(el, $el); } else { $el.trigger("ah:close"); }
            break;
        }
      });
      $el.on("mouseenter" + NS, ".ah-command__item", function () {
        cmdSetActive(el, $el, cmdVisible($el).index(this), false);
      });
      $el.on("click" + NS, ".ah-command__item", function () {
        cmdSelect(el, $el, this);
      });
      var $ov = cmdOverlay($el);
      $ov.on("mousedown" + NS, function (e) {
        if (e.target === e.currentTarget) { cmdClose(el, $el); }
      });
      var key = (el.getAttribute("data-hotkey") || "").toLowerCase();
      if (key) {
        var ns = ".ahcmd" + (++hotkeySeq);
        $.data(el, "ah-cmd-hotkey", ns);
        $(document).on("keydown" + ns, function (e) {
          if ((e.ctrlKey || e.metaKey) && String(e.key).toLowerCase() === key) {
            e.preventDefault();
            if ($ov.length && !$ov[0].hidden) { cmdClose(el, $el); } else { cmdOpen(el, $el); }
          }
        });
      }
      cmdFilter(el, $el);
      if (el.hasAttribute("data-auto-focus") && !$ov.length) {
        setTimeout(function () { cmdFocus($el); }, 0);
      }
    },
    destroy: function (el, $el) {
      var ns = $.data(el, "ah-cmd-hotkey");
      if (ns) { $(document).off(ns); }
      cmdOverlay($el).off(NS);
    },
    methods: {
      open: function (el, $el) { cmdOpen(el, $el); },
      close: function (el, $el) { cmdClose(el, $el); },
      toggle: function (el, $el) {
        var ov = cmdOverlay($el)[0];
        if (ov && !ov.hidden) { cmdClose(el, $el); } else { cmdOpen(el, $el); }
      },
      setQuery: function (el, $el, q) {
        cmdInput($el).val(q == null ? "" : String(q)).trigger("input");
      },
      focus: function (el, $el) { cmdFocus($el); },
      itemsLoaded: function (el, $el) { cmdSetActive(el, $el, 0, true); }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/layout_basic.js ---- */
/* Behaviours of the layout_basic components (designs/04-components.md).
 *
 * Ported from sigil's layout components (cljs + jQuery). Value-bearing
 * components (tabs, tab-bar, pagination, steps, expander) keep their value
 * in data-ah-value on the root, mirror it into a hidden input when there
 * is one, and fire "change" on the root when the user changes it. Methods
 * called by the server (AH.invoke / aihtml_action:call) update the value
 * without firing "change".
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  function setValue(el, $el, v) {
    el.setAttribute("data-ah-value", v);
    $el.children("input[type=hidden]").val(v);
  }

  function key(e) {
    return e.key;
  }

  // ------------------------------------------------------------------
  // Panel: scroll area with an optional collapsible header
  // ------------------------------------------------------------------

  function panelSet(el, $el, open, user) {
    var $w = $el.children(".ah-panel-wrapper");
    var $t = $el.children(".ah-panel-header").children(".ah-panel-toggle");
    if (!$t.length || ($t.attr("aria-expanded") === "true") === open) {
      return;
    }
    $t.attr("aria-expanded", String(open));
    $el.toggleClass("ah-panel-collapsed", !open);
    $w.stop(true, true)[open ? "slideDown" : "slideUp"](200, function () {
      if (user) {
        $el.trigger(open ? "ah:expand" : "ah:collapse");
      }
    });
  }

  AH.define("panel", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-panel-toggle", function (e) {
        if ($(this).closest(".ah-panel")[0] !== el) {
          return;
        }
        e.preventDefault();
        panelSet(el, $el, this.getAttribute("aria-expanded") !== "true", true);
      });
    },
    methods: {
      scrollTo: function (el, $el, x, y) {
        var w = $el.children(".ah-panel-wrapper")[0];
        if (w) {
          w.scrollLeft = x || 0;
          w.scrollTop = y || 0;
        }
      },
      refresh: function () { /* native scrolling: nothing to measure */ },
      collapse: function (el, $el) { panelSet(el, $el, false, false); },
      expand: function (el, $el) { panelSet(el, $el, true, false); },
      toggle: function (el, $el) {
        panelSet(el, $el, $el.hasClass("ah-panel-collapsed"), false);
      }
    }
  });

  // ------------------------------------------------------------------
  // Expander
  // ------------------------------------------------------------------

  function expIsOpen($el) {
    return $el.children(".ah-expander-header").attr("aria-expanded") === "true";
  }

  function expSet(el, $el, open, user) {
    if (expIsOpen($el) === open) {
      return;
    }
    var $h = $el.children(".ah-expander-header");
    var $b = $el.children(".ah-expander-body");
    var anim = el.getAttribute("data-animation") || "slide";
    var dur = parseInt(el.getAttribute("data-duration") || "250", 10);
    $el.trigger(open ? "ah:expanding" : "ah:collapsing");
    $h.toggleClass("ah-expander-header-expanded", open).attr("aria-expanded", String(open));
    $h.children(".ah-expander-arrow").toggleClass("ah-expander-arrow-expanded", open);
    setValue(el, $el, String(open));
    var done = function () {
      $el.trigger(open ? "ah:expanded" : "ah:collapsed");
    };
    $b.stop(true, true);
    if (anim === "slide") {
      $b[open ? "slideDown" : "slideUp"](dur, done);
    } else if (anim === "fade") {
      $b[open ? "fadeIn" : "fadeOut"](dur, done);
    } else {
      $b[open ? "show" : "hide"]();
      done();
    }
    if (user) {
      $el.trigger("change");
    }
    // Accordion: opening one closes the others sharing its name.
    var group = el.getAttribute("data-accordion");
    if (open && group) {
      $("[data-ah=expander]").each(function () {
        if (this !== el && this.getAttribute("data-accordion") === group) {
          expSet(this, $(this), false, user);
        }
      });
    }
  }

  AH.define("expander", {
    init: function (el, $el) {
      var mode = el.getAttribute("data-toggle-mode") || "click";
      if (mode === "none") {
        return;
      }
      var fromUser = function (e) {
        if (this.parentNode !== el || $el.hasClass("ah-expander-disabled")) {
          return;
        }
        e.preventDefault();
        expSet(el, $el, !expIsOpen($el), true);
      };
      $el.on(mode + NS, ".ah-expander-header", fromUser);
      $el.on("keydown" + NS, ".ah-expander-header", function (e) {
        if (e.target === this && (key(e) === "Enter" || key(e) === " ")) {
          fromUser.call(this, e);
        }
      });
    },
    methods: {
      open: function (el, $el) { expSet(el, $el, true, false); },
      close: function (el, $el) { expSet(el, $el, false, false); },
      toggle: function (el, $el) { expSet(el, $el, !expIsOpen($el), false); },
      isOpen: function (el, $el) { return expIsOpen($el); }
    }
  });

  // ------------------------------------------------------------------
  // Tabs
  // ------------------------------------------------------------------

  function tabItems($el) {
    return $el.children(".ah-tabs-header").children(".ah-tabs-item");
  }

  function tabPanels($el) {
    return $el.children(".ah-tabs-content").children(".ah-tabs-panel");
  }

  function tabIndexOf($el, k) {
    var idx = -1;
    tabItems($el).each(function (i) {
      if (this.getAttribute("data-key") === String(k)) { idx = i; }
    });
    return idx;
  }

  function tabSelect(el, $el, idx, user) {
    var $items = tabItems($el);
    var $panels = tabPanels($el);
    var cur = $items.index($items.filter(".ah-tabs-item-selected"));
    if (idx < 0 || idx >= $items.length || idx === cur ||
        $items.eq(idx).hasClass("ah-tabs-item-disabled")) {
      return false;
    }
    $items.removeClass("ah-tabs-item-selected").attr({ "aria-selected": "false", tabindex: "-1" });
    $items.eq(idx).addClass("ah-tabs-item-selected").attr({ "aria-selected": "true", tabindex: "0" });
    $panels.attr("aria-hidden", "true");
    var $new = $panels.eq(idx).attr("aria-hidden", "false");
    var $old = cur >= 0 ? $panels.eq(cur) : $panels.not($new);
    $panels.stop(true, true);
    if ((el.getAttribute("data-animation") || "fade") === "fade" && $old.length) {
      $old.fadeOut(100, function () {
        $old.removeClass("ah-tabs-panel-active");
        $new.hide().fadeIn(100, function () { $new.addClass("ah-tabs-panel-active"); });
      });
    } else {
      $old.hide().removeClass("ah-tabs-panel-active");
      $new.css("display", "").addClass("ah-tabs-panel-active");
    }
    setValue(el, $el, $items.eq(idx).attr("data-key"));
    if (user) {
      $el.trigger("change");
    }
    return true;
  }

  // Next enabled index from start in direction dir, wrapping.
  function nextEnabled($items, start, dir, disabledCls) {
    var n = $items.length;
    for (var s = 1, i = (start + dir + n) % n; s <= n; s++, i = (i + dir + n) % n) {
      if (!$items.eq(i).hasClass(disabledCls)) { return i; }
    }
    return start;
  }

  // Arrow keys (by orientation), Home, End; Enter/Space activate.
  function listKeys(e, $items, cur, vertical, disabledCls) {
    var k = key(e);
    var prev = vertical ? "ArrowUp" : "ArrowLeft";
    var next = vertical ? "ArrowDown" : "ArrowRight";
    if (k === "Home") { return nextEnabled($items, -1, 1, disabledCls); }
    if (k === "End") { return nextEnabled($items, $items.length, -1, disabledCls); }
    if (k === prev) { return nextEnabled($items, cur, -1, disabledCls); }
    if (k === next) { return nextEnabled($items, cur, 1, disabledCls); }
    if (k === "Enter" || k === " ") { return cur; }
    return null;
  }

  AH.define("tabs", {
    init: function (el, $el) {
      var $header = $el.children(".ah-tabs-header");
      var ev = el.getAttribute("data-selection-mode") === "hover" ? "mouseenter" : "click";
      $header.on(ev + NS, ".ah-tabs-item", function () {
        if (!$el.hasClass("ah-tabs-disabled")) {
          tabSelect(el, $el, tabItems($el).index(this), true);
        }
      });
      $header.on("keydown" + NS, ".ah-tabs-item", function (e) {
        var $items = tabItems($el);
        var vertical = $el.hasClass("ah-tabs-left") || $el.hasClass("ah-tabs-right");
        var t = listKeys(e, $items, $items.index(this), vertical, "ah-tabs-item-disabled");
        if (t === null) { return; }
        e.preventDefault();
        tabSelect(el, $el, t, true);
        $items.eq(t).trigger("focus");
      });
      $header.on("click" + NS, ".ah-tabs-scroll-btn", function () {
        var step = $(this).hasClass("ah-tabs-scroll-left") ? -80 : 80;
        $header.scrollLeft($header.scrollLeft() + step);
      });
    },
    destroy: function (el, $el) {
      $el.children(".ah-tabs-header").off(NS);
    },
    methods: {
      select: function (el, $el, k) { tabSelect(el, $el, tabIndexOf($el, k), false); },
      disable: function (el, $el, k) {
        tabItems($el).eq(tabIndexOf($el, k)).addClass("ah-tabs-item-disabled").attr("aria-disabled", "true");
      },
      enable: function (el, $el, k) {
        tabItems($el).eq(tabIndexOf($el, k)).removeClass("ah-tabs-item-disabled").removeAttr("aria-disabled");
      },
      value: function (el) { return el.getAttribute("data-ah-value"); }
    }
  });

  // ------------------------------------------------------------------
  // Tab bar
  // ------------------------------------------------------------------

  function barTabs($el) {
    return $el.children(".ah-tab-bar__tab");
  }

  function barFind($el, id) {
    return barTabs($el).filter(function () { return this.getAttribute("data-id") === String(id); });
  }

  function barSelect(el, $el, $tab, user) {
    if (!$tab.length || $tab.attr("data-active") === "true") {
      return;
    }
    barTabs($el).attr({ "data-active": "false", "aria-selected": "false", tabindex: "-1" });
    $tab.attr({ "data-active": "true", "aria-selected": "true", tabindex: "0" });
    setValue(el, $el, $tab.attr("data-id"));
    if (user) {
      $el.trigger("change");
    }
  }

  function barClose(el, $el, $tab, user) {
    if (!$tab.length) {
      return;
    }
    var id = $tab.attr("data-id");
    var $all = barTabs($el);
    var idx = $all.index($tab);
    var wasActive = $tab.attr("data-active") === "true";
    var hadFocus = $.contains($tab[0], document.activeElement) || $tab[0] === document.activeElement;
    $tab.remove();
    $el.trigger("ah:close", [id]);
    if (wasActive) {
      var $left = barTabs($el);
      if ($left.length) {
        var $next = $left.eq(Math.min(idx, $left.length - 1));
        barSelect(el, $el, $next, user);
        if (hadFocus) { $next.trigger("focus"); }
      } else {
        setValue(el, $el, "");
        if (user) { $el.trigger("change"); }
      }
    }
  }

  AH.define("tab-bar", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-tab-bar__close", function (e) {
        e.stopPropagation();
        barClose(el, $el, $(this).closest(".ah-tab-bar__tab"), true);
      });
      $el.on("click" + NS, ".ah-tab-bar__tab", function () {
        barSelect(el, $el, $(this), true);
      });
      $el.on("keydown" + NS, ".ah-tab-bar__tab", function (e) {
        var $items = barTabs($el);
        var cur = $items.index(this);
        if (key(e) === "Delete" && $(this).children(".ah-tab-bar__close").length) {
          e.preventDefault();
          barClose(el, $el, $(this), true);
          return;
        }
        var t = listKeys(e, $items, cur, false, "ah-tab-bar__tab--none");
        if (t === null) { return; }
        e.preventDefault();
        barSelect(el, $el, $items.eq(t), true);
        $items.eq(t).trigger("focus");
      });
    },
    methods: {
      select: function (el, $el, id) { barSelect(el, $el, barFind($el, id), false); },
      close: function (el, $el, id) { barClose(el, $el, barFind($el, id), false); },
      value: function (el) { return el.getAttribute("data-ah-value"); }
    }
  });

  // ------------------------------------------------------------------
  // Pagination
  // ------------------------------------------------------------------

  // Same as aihtml_layout_basic:visible_pages/3; 0 stands for an ellipsis.
  function visiblePages(cur, total, max) {
    var i, out = [];
    max = Math.max(5, max);
    if (total <= max) {
      for (i = 1; i <= total; i++) { out.push(i); }
    } else if (cur <= max - 3) {
      for (i = 1; i <= max - 2; i++) { out.push(i); }
      out.push(0, total);
    } else if (cur >= total - (max - 4)) {
      out.push(1, 0);
      for (i = total - (max - 3); i <= total; i++) { out.push(i); }
    } else {
      var h = Math.floor((max - 5) / 2);
      out.push(1, 0);
      for (i = cur - h; i <= cur + h; i++) { out.push(i); }
      out.push(0, total);
    }
    return out;
  }

  function pgState(el) {
    var total = parseInt(el.getAttribute("data-total"), 10) || 0;
    var size = Math.max(1, parseInt(el.getAttribute("data-page-size"), 10) || 10);
    return {
      page: parseInt(el.getAttribute("data-ah-value"), 10) || 1,
      total: total,
      size: size,
      pages: Math.max(1, Math.ceil(total / size)),
      max: parseInt(el.getAttribute("data-max-visible"), 10) || 7
    };
  }

  function pgHref(el, page, size) {
    return el.getAttribute("data-href").replace(/\{page\}/g, page).replace(/\{size\}/g, size);
  }

  function pgFmt(t, args) {
    return args.reduce(function (acc, a, i) { return acc.split("{" + i + "}").join(String(a)); }, t);
  }

  var NAV_ICONS = { prev: "\u2039", next: "\u203A", first: "\u00AB", last: "\u00BB" };

  function pgEntry(m) {
    return $.extend({ gap: false, info: false, item: false, nav: false, link: false,
                      active: false, disabled: false, first_last: false, href: "",
                      number: 0, tabindex: 0, type: "", label: "", icon: "", text: "" }, m);
  }

  // The view data of templates/pagination_items.mustache; mirrors
  // aihtml_layout_basic:pagination_view/5.
  function pgView(cur, pages, max, size, cfg) {
    var link = function (p) {
      return cfg.href == null ? null
        : cfg.href.replace(/\{page\}/g, p).replace(/\{size\}/g, size);
    };
    var nav = function (type, disabled, target) {
      var url = disabled ? null : link(target);
      return pgEntry({ nav: true, type: type, label: cfg.labels[type], icon: NAV_ICONS[type],
                       first_last: type === "first" || type === "last", disabled: disabled,
                       tabindex: disabled ? -1 : 0, link: url !== null, href: url || "" });
    };
    var middle = cfg.simple
      ? [pgEntry({ info: true, text: pgFmt(cfg.labels.page_info, [cur, pages]) })]
      : visiblePages(cur, pages, max).map(function (p) {
        if (!p) { return pgEntry({ gap: true }); }
        var url = link(p);
        return pgEntry({ item: true, number: p, active: p === cur, tabindex: p === cur ? -1 : 0,
                         link: url !== null, href: url || "" });
      });
    var fl = cfg.first_last && !cfg.simple;
    var entries = [];
    if (fl) { entries.push(nav("first", cur === 1, 1)); }
    entries.push(nav("prev", cur === 1, cur - 1));
    entries = entries.concat(middle);
    entries.push(nav("next", cur === pages, cur + 1));
    if (fl) { entries.push(nav("last", cur === pages, pages)); }
    return { entries: entries };
  }

  function pgRender(el, $el) {
    var s = pgState(el);
    var $ul = $el.children(".ah-pagination-pages");
    var cfg = JSON.parse($ul.attr("data-view"));
    $ul.html(AH.tpl.pagination_items(pgView(s.page, s.pages, s.max, s.size, cfg)));
    var $suffix = $el.find(".ah-pagination-jumper > span").last();
    if ($suffix.length && $suffix.attr("data-template")) {
      $suffix.text(pgFmt($suffix.attr("data-template"), [s.pages]));
    }
  }

  function pgGo(el, $el, page, user) {
    var s = pgState(el);
    var p = Math.min(Math.max(1, page | 0), s.pages);
    if (isNaN(page) || p === s.page) {
      return false;
    }
    if (el.getAttribute("data-href")) {
      window.location.href = pgHref(el, p, s.size);
      return true;
    }
    setValue(el, $el, String(p));
    pgRender(el, $el);
    if (user) {
      $el.trigger("change");
      // keep keyboard focus inside the control after the rebuild
      $el.find(".ah-pagination-item-active").trigger("focus");
    }
    return true;
  }

  function pgSize(el, $el, size, user) {
    var s = pgState(el);
    if (!size || size === s.size) {
      return;
    }
    if (el.getAttribute("data-href")) {
      window.location.href = pgHref(el, 1, size);
      return;
    }
    el.setAttribute("data-page-size", String(size));
    var pages = Math.max(1, Math.ceil(s.total / size));
    setValue(el, $el, String(Math.min(s.page, pages)));
    pgRender(el, $el);
    $el.children(".ah-pagination-size-selector").children("select").val(String(size));
    if (user) {
      $el.trigger("change");
    }
  }

  AH.define("pagination", {
    init: function (el, $el) {
      var blocked = function () { return $el.hasClass("ah-pagination-disabled"); };
      $el.on("click" + NS, "li.ah-pagination-item", function () {
        if (!blocked()) { pgGo(el, $el, parseInt(this.getAttribute("data-page"), 10), true); }
      });
      $el.on("click" + NS, "li.ah-pagination-nav", function () {
        if (blocked() || $(this).hasClass("ah-pagination-nav-disabled")) { return; }
        var s = pgState(el);
        var t = { prev: s.page - 1, next: s.page + 1, first: 1, last: s.pages }[this.getAttribute("data-type")];
        pgGo(el, $el, t, true);
      });
      $el.on("keydown" + NS, "li.ah-pagination-item, li.ah-pagination-nav", function (e) {
        if (key(e) === "Enter" || key(e) === " ") {
          e.preventDefault();
          $(this).trigger("click");
        }
      });
      // Native change events of the inner select/input must not reach the
      // root's action as if the page had changed.
      $el.on("change" + NS, ".ah-pagination-size-select", function (e) {
        e.stopPropagation();
        pgSize(el, $el, parseInt($(this).val(), 10), true);
      });
      $el.on("change" + NS + " input" + NS, ".ah-pagination-jumper-input", function (e) {
        e.stopPropagation();
      });
      var jump = function () {
        var $in = $el.find(".ah-pagination-jumper-input");
        pgGo(el, $el, parseInt($in.val(), 10), true);
        $in.val("");
      };
      $el.on("click" + NS, ".ah-pagination-jumper-btn", jump);
      $el.on("keydown" + NS, ".ah-pagination-jumper-input", function (e) {
        if (key(e) === "Enter") {
          e.preventDefault();
          jump();
        }
      });
    },
    methods: {
      setPage: function (el, $el, p) { pgGo(el, $el, parseInt(p, 10), false); },
      next: function (el, $el) { pgGo(el, $el, pgState(el).page + 1, false); },
      prev: function (el, $el) { pgGo(el, $el, pgState(el).page - 1, false); },
      first: function (el, $el) { pgGo(el, $el, 1, false); },
      last: function (el, $el) { pgGo(el, $el, pgState(el).pages, false); },
      setPageSize: function (el, $el, n) { pgSize(el, $el, parseInt(n, 10), false); },
      setTotal: function (el, $el, n) {
        el.setAttribute("data-total", String(Math.max(0, parseInt(n, 10) || 0)));
        var s = pgState(el);
        setValue(el, $el, String(Math.min(s.page, s.pages)));
        pgRender(el, $el);
      },
      value: function (el) { return pgState(el).page; }
    }
  });

  // ------------------------------------------------------------------
  // Steps
  // ------------------------------------------------------------------

  var STEP_STATES = "ah-steps-item-pending ah-steps-item-active ah-steps-item-completed ah-steps-item-error";

  function stepItems($el) {
    return $el.children(".ah-steps-header").children(".ah-steps-item");
  }

  // Status class and indicator, the indicator from the server's template
  // (templates/steps_indicator.mustache).
  function stepIndicator($it, i, status) {
    $it.addClass("ah-steps-item-" + status);
    $it.children(".ah-steps-indicator").html(AH.tpl.steps_indicator({
      check: status === "completed", error: status === "error",
      plain: status !== "completed" && status !== "error", number: i + 1
    }));
  }

  function stepCur(el) {
    return parseInt(el.getAttribute("data-ah-value"), 10) || 0;
  }

  function stepSelect(el, $el, idx, user) {
    var $items = stepItems($el);
    var cur = stepCur(el);
    var n = $items.length;
    if (idx < 0 || idx >= n || idx === cur || $items.eq(idx).hasClass("ah-steps-item-disabled")) {
      return false;
    }
    var clickable = el.getAttribute("data-clickable") !== "false";
    $items.removeClass("ah-steps-item-selected").removeAttr("aria-current");
    $items.eq(idx).addClass("ah-steps-item-selected").attr("aria-current", "step");
    $items.each(function (i) {
      var $it = $(this);
      if ($it.attr("role") === "button") { $it.attr("tabindex", i === idx ? "0" : "-1"); }
      if ($it.hasClass("ah-steps-item-disabled") || $it.hasClass("ah-steps-item-error")) {
        return;
      }
      $it.removeClass(STEP_STATES);
      stepIndicator($it, i, i < idx ? "completed" : (i === idx ? "active" : "pending"));
    });
    $items.each(function () {
      var $it = $(this);
      $it.children(".ah-steps-connector").toggleClass("ah-steps-connector-done",
                                                      $it.hasClass("ah-steps-item-completed"));
    });
    var $panels = $el.children(".ah-steps-panels").children(".ah-steps-panel");
    $panels.removeClass("ah-steps-panel-active").eq(idx).addClass("ah-steps-panel-active");
    var $nav = $el.children(".ah-steps-nav");
    var toggle = function (action, on) {
      $nav.children("[data-action=" + action + "]").prop("disabled", !on)
        .toggleClass("ah-steps-btn-disabled", !on);
    };
    toggle("prev", clickable && idx > 0);
    toggle("next", clickable && idx < n - 1);
    setValue(el, $el, String(idx));
    if (user) {
      $el.trigger("change");
    }
    return true;
  }

  // Next non-disabled step from cur in direction dir, or cur.
  function stepMove($el, cur, dir) {
    var $items = stepItems($el);
    for (var i = cur + dir; i >= 0 && i < $items.length; i += dir) {
      if (!$items.eq(i).hasClass("ah-steps-item-disabled")) { return i; }
    }
    return cur;
  }

  AH.define("steps", {
    init: function (el, $el) {
      var $header = $el.children(".ah-steps-header");
      $header.on("click" + NS, ".ah-steps-item-clickable", function () {
        if (!$el.hasClass("ah-steps-disabled")) {
          stepSelect(el, $el, stepItems($el).index(this), true);
        }
      });
      $header.on("keydown" + NS, ".ah-steps-item-clickable", function (e) {
        var $items = stepItems($el);
        var vertical = $el.hasClass("ah-steps-vertical");
        var t = listKeys(e, $items, $items.index(this), vertical, "ah-steps-item-disabled");
        if (t === null) { return; }
        e.preventDefault();
        stepSelect(el, $el, t, true);
        $items.eq(t).trigger("focus");
      });
      $el.on("click" + NS, ".ah-steps-btn", function () {
        if (this.parentNode.parentNode !== el || this.disabled) { return; }
        var dir = this.getAttribute("data-action") === "prev" ? -1 : 1;
        stepSelect(el, $el, stepMove($el, stepCur(el), dir), true);
      });
    },
    destroy: function (el, $el) {
      $el.children(".ah-steps-header").off(NS);
    },
    methods: {
      select: function (el, $el, i) { stepSelect(el, $el, parseInt(i, 10), false); },
      next: function (el, $el) { stepSelect(el, $el, stepMove($el, stepCur(el), 1), false); },
      prev: function (el, $el) { stepSelect(el, $el, stepMove($el, stepCur(el), -1), false); },
      first: function (el, $el) { stepSelect(el, $el, 0, false); },
      last: function (el, $el) { stepSelect(el, $el, stepItems($el).length - 1, false); },
      setStatus: function (el, $el, i, status) {
        var $it = stepItems($el).eq(parseInt(i, 10));
        $it.removeClass(STEP_STATES + " ah-steps-item-disabled");
        stepIndicator($it, parseInt(i, 10), status);
        $it.children(".ah-steps-connector").toggleClass("ah-steps-connector-done", status === "completed");
      },
      value: function (el) { return stepCur(el); }
    }
  });

  // ------------------------------------------------------------------
  // Loader
  // ------------------------------------------------------------------

  var MODAL_ID = "ah-loader-modal";

  function loaderShow(el, $el, left, top) {
    var modal = el.getAttribute("data-modal") === "true";
    if (modal) {
      var $m = $("#" + MODAL_ID);
      if (!$m.length) {
        $m = $("<div>", { id: MODAL_ID, "class": "ah-loader-modal" }).appendTo(document.body);
      }
      $m.removeClass("ah-loader-hidden");
      $(document).off("keyup" + NS + "loader").on("keyup" + NS + "loader", function (e) {
        if (key(e) === "Escape") { loaderHide(el, $el); }
      });
    }
    $el.removeClass("ah-loader-hidden").attr("aria-busy", "true");
    if (left !== undefined && left !== null && top !== undefined && top !== null) {
      $el.removeClass("ah-loader-center").css({ left: left + "px", top: top + "px" });
    } else if (modal) {
      $el.addClass("ah-loader-center");
    }
  }

  function loaderHide(el, $el) {
    $el.addClass("ah-loader-hidden").attr("aria-busy", "false");
    if (el.getAttribute("data-modal") === "true") {
      $("#" + MODAL_ID).addClass("ah-loader-hidden");
      $(document).off("keyup" + NS + "loader");
    }
  }

  AH.define("loader", {
    init: function (el, $el) {
      if (el.getAttribute("data-modal") === "true" && !$el.hasClass("ah-loader-hidden")) {
        loaderShow(el, $el);
      }
    },
    destroy: function (el, $el) {
      if (el.getAttribute("data-modal") === "true") {
        loaderHide(el, $el);
      }
    },
    methods: {
      show: function (el, $el, left, top) { loaderShow(el, $el, left, top); },
      hide: function (el, $el) { loaderHide(el, $el); },
      toggle: function (el, $el) {
        if ($el.hasClass("ah-loader-hidden")) { loaderShow(el, $el); } else { loaderHide(el, $el); }
      },
      text: function (el, $el, t) {
        $el.children(".ah-loader-text").text(t);
        $el.attr("aria-label", t);
      },
      isOpen: function (el, $el) { return !$el.hasClass("ah-loader-hidden"); }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/layout_dnd.js ---- */
/* Behaviours of the layout_dnd components (designs/04-components.md).
 * Ported from sigil: layout/sortable (+ sortable/geometry) and
 * layout/dragdrop. Pointer events (mouse, pen, touch) instead of sigil's
 * mouse + touch sequence; one drag at a time, tracked on the document
 * with its own namespace and unbound when the drag ends or the
 * component is destroyed.
 *
 *   sortable   reorder by dragging (or from the keyboard); the value is
 *              the order of the item keys (data-value), change after a
 *              drop that changed it; lists with the same data-ah-group
 *              exchange items
 *   dragdrop   [data-ah-drag] items dropped on [data-ah-drop] zones fire
 *              ah:drop on the zone; the root carries data-drag, data-drop
 *              and data-from for the action payload
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var DOC_NS = ".ahdnd";
  var DISTANCE = 5;            // px of movement before a press becomes a drag
  var EDGE = 20, SPEED = 10;   // auto scroll: edge width and px per frame

  // ------------------------------------------------------------------
  // Shared
  // ------------------------------------------------------------------

  // The drag in progress (one per page): {kind, el, pointerId, ...}
  var drag = null;

  function pageRect(el) {
    var r = el.getBoundingClientRect();
    var sx = window.pageXOffset, sy = window.pageYOffset;
    return { left: r.left + sx, top: r.top + sy, right: r.right + sx, bottom: r.bottom + sy,
             width: r.width, height: r.height };
  }

  // Page coordinates of a pointer event (from clientX, which every
  // pointer event has, synthetic ones included).
  function px(e) { return e.clientX + window.pageXOffset; }
  function py(e) { return e.clientY + window.pageYOffset; }

  function inside(x, y, r) {
    return x >= r.left && x <= r.right && y >= r.top && y <= r.bottom;
  }

  // The copy that follows the pointer lives in <body>, outside the scope of
  // the custom properties it inherited (skins, a themed container), so they
  // are copied onto it, as sigil does.
  function copyVars(src, dst) {
    var cs = window.getComputedStyle(src);
    for (var i = 0; i < cs.length; i++) {
      var p = cs.item(i);
      if (p && p.indexOf("--") === 0) { dst.style.setProperty(p, cs.getPropertyValue(p)); }
    }
  }

  function floatingCopy(el, cls, opacity) {
    var r = pageRect(el);
    var copy = el.cloneNode(true);
    copy.removeAttribute("id");
    $(copy).find("[id]").removeAttr("id");
    copy.removeAttribute("tabindex");
    copy.setAttribute("aria-hidden", "true");
    copy.className += " " + cls;
    copyVars(el, copy);
    $(copy).css({ position: "absolute", margin: 0, boxSizing: "border-box",
                  width: r.width + "px", height: r.height + "px",
                  left: r.left + "px", top: r.top + "px", opacity: opacity,
                  zIndex: 999999, pointerEvents: "none" });
    document.body.appendChild(copy);
    return copy;
  }

  function scrollParent(el) {
    for (var cur = el.parentElement; cur && cur !== document.body; cur = cur.parentElement) {
      var s = window.getComputedStyle(cur);
      if ((/auto|scroll/.test(s.overflowY) && cur.scrollHeight > cur.clientHeight) ||
          (/auto|scroll/.test(s.overflowX) && cur.scrollWidth > cur.clientWidth)) {
        return cur;
      }
    }
    return null;
  }

  // Scroll the nearest scrollable ancestor (or the window) when the
  // pointer is near its edge.
  function autoScroll(box, cx, cy) {
    if (box) {
      var r = box.getBoundingClientRect();
      if (cy - r.top < EDGE) { box.scrollTop -= SPEED; }
      else if (r.bottom - cy < EDGE) { box.scrollTop += SPEED; }
      if (cx - r.left < EDGE) { box.scrollLeft -= SPEED; }
      else if (r.right - cx < EDGE) { box.scrollLeft += SPEED; }
    }
    if (cy < EDGE) { window.scrollBy(0, -SPEED); }
    else if (window.innerHeight - cy < EDGE) { window.scrollBy(0, SPEED); }
  }

  // Swallow the click that follows a drag (items may be links).
  function swallowClick() {
    var stop = function (e) { e.stopPropagation(); e.preventDefault(); };
    document.addEventListener("click", stop, true);
    setTimeout(function () { document.removeEventListener("click", stop, true); }, 0);
  }

  function editable(t) {
    return $(t).closest("input, textarea, select, [contenteditable=''], [contenteditable=true]").length > 0;
  }

  function announce($live, text) {
    $live.text("");
    setTimeout(function () { $live.text(text); }, 20);
  }

  function label(el) {
    return el.getAttribute("aria-label") || $(el).text().trim().replace(/\s+/g, " ").slice(0, 60);
  }

  // Document listeners for one drag; `move', `end' and `cancel' get the
  // pointer event (or nothing for a cancel from the keyboard).
  function track(d, move, end, cancel) {
    drag = d;
    var $doc = $(document);
    $doc.on("pointermove" + DOC_NS, function (e) {
      if (e.pointerId === d.pointerId) { move(e); }
    });
    $doc.on("pointerup" + DOC_NS, function (e) {
      if (e.pointerId === d.pointerId) { untrack(); end(e); }
    });
    $doc.on("pointercancel" + DOC_NS, function (e) {
      if (e.pointerId === d.pointerId) { untrack(); cancel(); }
    });
    $doc.on("keydown" + DOC_NS, function (e) {
      if (e.key === "Escape") { e.preventDefault(); untrack(); cancel(); }
    });
    d.cancel = function () { untrack(); cancel(); };
  }

  function untrack() {
    $(document).off(DOC_NS);
    if (drag && drag.raf) { cancelAnimationFrame(drag.raf); }
    drag = null;
  }

  // Cancel the drag in progress if it belongs to el.
  function cancelFor(el) {
    if (drag && drag.el === el) { drag.cancel(); }
  }

  // ==================================================================
  // sortable
  // ==================================================================

  function soItems(list) {
    return $(list).children(".ah-sortable-item").get();
  }

  function soKey(item) { return item.getAttribute("data-value") || ""; }

  function soOrder(list) {
    return soItems(list).map(soKey).join(",");
  }

  function soDisabled(list) {
    return $(list).hasClass("ah-sortable-disabled");
  }

  function soPublish(list, fire) {
    var v = soOrder(list);
    var old = list.getAttribute("data-ah-value") || "";
    list.setAttribute("data-ah-value", v);
    $(list).children("input[type=hidden]").val(v);
    if (fire && v !== old) { $(list).trigger("change"); }
  }

  // Roving tabindex: `item' (or the first item) is the one in the tab order.
  function soRove(list, item) {
    var items = soItems(list);
    if (!item || items.indexOf(item) < 0) {
      item = $(items).filter("[tabindex=0]")[0] || items[0];
    }
    var off = soDisabled(list);
    $(items).attr("tabindex", "-1");
    if (item && !off) { item.setAttribute("tabindex", "0"); }
  }

  function soLayout(list) {
    var $l = $(list);
    return $l.hasClass("ah-sortable-grid") ? "grid"
      : $l.hasClass("ah-sortable-horizontal") ? "horizontal" : "vertical";
  }

  // Where the placeholder goes for the pointer at (x, y): the item to insert
  // before, or null for the end. Vertical lists compare with the middle
  // of each item; horizontal lists and grids go in reading order (sigil's
  // find-grid-insertion), which also handles wrapped rows.
  function soInsertion(items, layout, x, y) {
    for (var i = 0; i < items.length; i++) {
      var r = pageRect(items[i]);
      if (layout === "vertical") {
        if (y < r.top + r.height / 2) { return items[i]; }
      } else if (y < r.top || (y <= r.bottom && x < r.left + r.width / 2)) {
        return items[i];
      }
    }
    return null;
  }

  // Put `node' before `ref', or after the last item of the list.
  function soPlace(list, node, ref) {
    if (ref) {
      if (ref.previousSibling !== node) { list.insertBefore(node, ref); }
      return;
    }
    var items = $(list).children(".ah-sortable-item, .ah-sortable-placeholder").get()
      .filter(function (n) { return n !== node && n.style.display !== "none"; });
    var last = items[items.length - 1];
    var after = last ? last.nextSibling : list.firstChild;
    if (after !== node) { list.insertBefore(node, after); }
  }

  // Connected lists under the pointer: the innermost (smallest) one wins,
  // as in sigil's check-connected-containers!.
  function soTarget(d, x, y) {
    var group = d.el.getAttribute("data-ah-group");
    if (!group) { return d.list; }
    var best = null, area = Infinity;
    $(".ah-sortable[data-ah-group]").each(function () {
      if (this.getAttribute("data-ah-group") !== group || (soDisabled(this) && this !== d.el)) {
        return;
      }
      var r = pageRect(this);
      if (inside(x, y, r) && r.width * r.height < area) { best = this; area = r.width * r.height; }
    });
    return best || d.list;
  }

  function soStart(d) {
    var item = d.item;
    var r = pageRect(item);
    var cs = window.getComputedStyle(item);
    var ph = document.createElement(item.tagName);
    ph.className = "ah-sortable-placeholder";
    $(ph).css({ width: r.width + "px", height: r.height + "px", margin: cs.margin,
                flex: "none" });
    d.helper = floatingCopy(item, "ah-sortable-helper", 0.85);
    d.offX = d.x0 - r.left;
    d.offY = d.y0 - r.top;
    d.ph = ph;
    d.index = soItems(d.el).indexOf(item);
    d.next = item.nextSibling;
    d.list = d.el;
    d.box = scrollParent(d.el);
    d.started = true;
    item.parentNode.insertBefore(ph, item.nextSibling);
    d.display = item.style.display;
    item.style.display = "none";
    $(d.el).addClass("ah-sortable-active");
    $("body").addClass("ah-disableselect");
    $(d.el).trigger("ah:sort-start", [{ key: soKey(item), index: d.index }]);
  }

  function soMove(d, e) {
    d.px = px(e); d.py = py(e); d.cx = e.clientX; d.cy = e.clientY;
    if (d.raf) { return; }
    d.raf = requestAnimationFrame(function () {
      d.raf = 0;
      if (drag !== d) { return; }
      $(d.helper).css({ left: (d.px - d.offX) + "px", top: (d.py - d.offY) + "px" });
      autoScroll(d.box, d.cx, d.cy);
      var target = soTarget(d, d.px, d.py);
      if (target !== d.list) {
        $(d.list).removeClass("ah-sortable-receiving");
        if (d.list !== d.el) { $(d.list).removeClass("ah-sortable-active"); }
        $(d.list).trigger("ah:sort-remove", [{ key: soKey(d.item) }]);
        d.list = target;
        if (target !== d.el) { $(target).addClass("ah-sortable-receiving ah-sortable-active"); }
        $(target).trigger("ah:sort-receive", [{ key: soKey(d.item) }]);
      }
      var items = soItems(d.list).filter(function (n) { return n !== d.item; });
      var ref = soInsertion(items, soLayout(d.list), d.px, d.py);
      var before = d.ph.nextSibling, parent = d.ph.parentNode;
      soPlace(d.list, d.ph, ref);
      if (d.ph.nextSibling !== before || d.ph.parentNode !== parent) {
        $(d.list).trigger("ah:sort-change", [{ key: soKey(d.item) }]);
      }
    });
  }

  function soCleanup(d) {
    d.item.style.display = d.display;
    $(d.ph).remove();
    $(d.helper).remove();
    $(d.el).add(d.list).removeClass("ah-sortable-active ah-sortable-receiving");
    $("body").removeClass("ah-disableselect");
  }

  function soEnd(d) {
    var item = d.item, from = d.el, to = d.list;
    to.insertBefore(item, d.ph);
    soCleanup(d);
    swallowClick();
    soRove(to, item);
    if (from !== to) { soRove(from, null); }
    var index = soItems(to).indexOf(item);
    $(to).trigger("ah:sort-stop", [{ key: soKey(item), index: index }]);
    soPublish(from, true);
    if (from !== to) { soPublish(to, true); }
    try { item.focus({ preventScroll: true }); } catch (err) { /* detached */ }
  }

  function soCancel(d) {
    if (!d.started) { return; }
    soCleanup(d);
    $(d.el).trigger("ah:sort-cancel", [{ key: soKey(d.item) }]);
  }

  // ---- keyboard: a picked-up item moves with the arrows ----

  function soState(list) {
    var st = $.data(list, "ahSortable");
    if (!st) { st = { grabbed: null, order: null }; $.data(list, "ahSortable", st); }
    return st;
  }

  function soPos(list, item) {
    var items = soItems(list);
    return (items.indexOf(item) + 1) + " of " + items.length;
  }

  // Move `item' by `delta' places (or to the start / end for -/+Infinity)
  // by moving its neighbours, so the item itself keeps the focus.
  function soShift(list, item, delta) {
    var moved = false;
    while (delta < 0) {
      var prev = $(item).prevAll(".ah-sortable-item")[0];
      if (!prev) { break; }
      list.insertBefore(prev, item.nextSibling);
      moved = true; delta++;
    }
    while (delta > 0) {
      var next = $(item).nextAll(".ah-sortable-item")[0];
      if (!next) { break; }
      list.insertBefore(next, item);
      moved = true; delta--;
    }
    return moved;
  }

  function soSetOrder(list, keys) {
    var items = soItems(list);
    var byKey = {};
    items.forEach(function (it) { byKey[soKey(it)] = it; });
    var named = [];
    keys.forEach(function (k) {
      if (byKey[k] && named.indexOf(byKey[k]) < 0) { named.push(byKey[k]); }
    });
    var rest = items.filter(function (it) { return named.indexOf(it) < 0; });
    var anchor = $(list).children(".ah-sortable-item").last()[0];
    anchor = anchor ? anchor.nextSibling : list.firstChild;
    named.concat(rest).forEach(function (it) {
      if (it !== anchor) { list.insertBefore(it, anchor); } else { anchor = it.nextSibling; }
    });
  }

  function soGrab(list, item) {
    var st = soState(list);
    st.grabbed = item;
    st.order = soOrder(list);
    $(item).addClass("ah-sortable-item-grabbed").attr("aria-pressed", "true");
    announce($(list).children(".ah-sortable-live"),
             "Picked up " + label(item) + ", position " + soPos(list, item) +
             ". Arrow keys move it, Space drops it, Escape cancels.");
  }

  function soRelease(list, commit) {
    var st = soState(list), item = st.grabbed;
    if (!item) { return; }
    st.grabbed = null;
    $(item).removeClass("ah-sortable-item-grabbed").removeAttr("aria-pressed");
    var $live = $(list).children(".ah-sortable-live");
    if (commit) {
      announce($live, label(item) + " dropped at position " + soPos(list, item) + ".");
      soPublish(list, true);
    } else {
      soSetOrder(list, st.order.split(","));
      item.focus();
      announce($live, "Cancelled, " + label(item) + " is back at position " + soPos(list, item) + ".");
    }
  }

  function soKeys(layout) {
    return layout === "vertical" ? { prev: ["ArrowUp"], next: ["ArrowDown"] }
      : layout === "horizontal" ? { prev: ["ArrowLeft"], next: ["ArrowRight"] }
      : { prev: ["ArrowLeft", "ArrowUp"], next: ["ArrowRight", "ArrowDown"] };
  }

  function soKeydown(list, item, e) {
    var st = soState(list);
    var keys = soKeys(soLayout(list));
    var dir = keys.prev.indexOf(e.key) >= 0 ? -1 : keys.next.indexOf(e.key) >= 0 ? 1
      : e.key === "Home" ? -Infinity : e.key === "End" ? Infinity : 0;
    var off = soDisabled(list);
    if (st.grabbed === item) {
      if (dir) {
        e.preventDefault();
        if (soShift(list, item, dir)) {
          announce($(list).children(".ah-sortable-live"),
                   label(item) + ", position " + soPos(list, item) + ".");
        }
      } else if (e.key === " " || e.key === "Enter") {
        e.preventDefault();
        soRelease(list, true);
      } else if (e.key === "Escape") {
        e.preventDefault();
        soRelease(list, false);
      }
      return;
    }
    if (dir && e.altKey && !off) {
      e.preventDefault();
      if (soShift(list, item, dir)) { soPublish(list, true); }
      return;
    }
    if (dir) {
      e.preventDefault();
      var items = soItems(list), i = items.indexOf(item);
      var j = dir === -Infinity ? 0 : dir === Infinity ? items.length - 1
        : Math.max(0, Math.min(items.length - 1, i + dir));
      soRove(list, items[j]);
      items[j].focus();
    } else if ((e.key === " " || e.key === "Enter") && !off) {
      e.preventDefault();
      soGrab(list, item);
    }
  }

  AH.define("sortable", {
    init: function (el, $el) {
      soRove(el, null);
      $el.on("pointerdown" + NS, ".ah-sortable-item", function (e) {
        var item = this;
        if (item.parentNode !== el || drag || soDisabled(el) ||
            (e.pointerType === "mouse" && e.button !== 0) || editable(e.target)) {
          return;
        }
        if ($(item).hasClass("ah-sortable-handle-mode") &&
            !$(e.target).closest(".ah-sortable-handle").length) {
          return;
        }
        e.preventDefault();
        soRelease(el, true);
        soRove(el, item);
        try { item.focus({ preventScroll: true }); } catch (err) { /* ignore */ }
        var d = { kind: "sortable", el: el, item: item, pointerId: e.pointerId,
                  x0: px(e), y0: py(e), started: false };
        track(d, function (me) {
          if (!d.started) {
            if (Math.abs(px(me) - d.x0) + Math.abs(py(me) - d.y0) <= DISTANCE) { return; }
            soStart(d);
          }
          soMove(d, me);
        }, function () {
          if (d.started) { soEnd(d); }
        }, function () {
          soCancel(d);
        });
      });
      $el.on("keydown" + NS, ".ah-sortable-item", function (e) {
        if (e.target === this && this.parentNode === el) { soKeydown(el, this, e); }
      });
      $el.on("focusout" + NS, ".ah-sortable-item", function () {
        var item = this;
        // a Tab away (or a click elsewhere) drops the picked-up item
        setTimeout(function () {
          if (soState(el).grabbed === item && document.activeElement !== item) {
            soRelease(el, true);
          }
        }, 0);
      });
      $el.on("focusin" + NS, ".ah-sortable-item", function () {
        if (this.parentNode === el && !soDisabled(el)) { soRove(el, this); }
      });
    },
    destroy: function (el) {
      cancelFor(el);
      $.removeData(el, "ahSortable");
    },
    methods: {
      getValue: function (el) { return soOrder(el); },
      setValue: function (el, $el, order) {
        var keys = Array.isArray(order) ? order.map(String)
          : String(order || "").split(",").filter(Boolean);
        soSetOrder(el, keys);
        soPublish(el, false);
      },
      enable: function (el, $el) {
        $el.removeClass("ah-sortable-disabled").removeAttr("aria-disabled");
        soRove(el, null);
      },
      disable: function (el, $el) {
        cancelFor(el);
        soRelease(el, true);
        $el.addClass("ah-sortable-disabled").attr("aria-disabled", "true");
        soRove(el, null);
      },
      cancel: function (el) {
        cancelFor(el);
        if (soState(el).grabbed) { soRelease(el, false); }
      }
    }
  });

  // ==================================================================
  // dragdrop
  // ==================================================================

  function ddScope(el, node) {
    return $(node).closest("[data-ah=dragdrop]")[0] === el;
  }

  function ddUsable(el, item) {
    return !$(el).hasClass("ah-dragdrop-disabled") && !$(item).hasClass("ah-draggable-disabled");
  }

  // The zones of this scope that take `item' (its data-ah-drag-type must
  // be in a zone's data-ah-drop-accept, when the zone has one).
  function ddZones(el, item) {
    var type = item.getAttribute("data-ah-drag-type") || "";
    return $(el).find("[data-ah-drop]").get().filter(function (z) {
      if (!ddScope(el, z) || z.getAttribute("data-ah-drop-disabled") === "true" ||
          $.contains(item, z) || z === item) {
        return false;
      }
      var accept = z.getAttribute("data-ah-drop-accept");
      return !accept || accept.split(",").indexOf(type) >= 0;
    });
  }

  // sigil's hit-test: the last zone (in document order, so an inner zone
  // beats its container) that the copy overlaps (intersect), lies in
  // (fit), or that the pointer is on (pointer).
  function ddHit(d, x, y) {
    var tol = d.el.getAttribute("data-ah-tolerance") || "intersect";
    var f = pageRect(d.copy), hit = null;
    d.zones.forEach(function (z) {
      var r = pageRect(z);
      var ok = tol === "pointer" ? inside(x, y, r)
        : tol === "fit" ? f.left >= r.left && f.right <= r.right && f.top >= r.top && f.bottom <= r.bottom
        : f.left < r.right && f.right > r.left && f.top < r.bottom && f.bottom > r.top;
      if (ok) { hit = z; }
    });
    return hit;
  }

  function ddTarget(d, zone) {
    if (zone === d.target) { return; }
    if (d.target) {
      $(d.target).removeClass("ah-drop-target-active").trigger("ah:drop-target-leave");
    }
    d.target = zone;
    if (zone) {
      $(zone).addClass("ah-drop-target-active").trigger("ah:drop-target-enter");
    }
  }

  function ddBegin(d) {
    var item = d.item;
    d.zones = ddZones(d.el, item);
    var from = $(item).parent().closest("[data-ah-drop]")[0];
    d.from = from && ddScope(d.el, from) ? from.getAttribute("data-ah-drop") : "";
    d.target = null;
    $(item).addClass("ah-dragging");
    $(d.zones).addClass("ah-drop-zone-accepting");
    $(d.el).addClass("ah-dragdrop-active");
    $(item).trigger("ah:drag-start", [{ key: item.getAttribute("data-ah-drag") }]);
  }

  function ddFinish(d, dropped) {
    var item = d.item;
    $(item).removeClass("ah-dragging");
    $(d.zones).removeClass("ah-drop-zone-accepting ah-drop-target-active");
    $(d.el).removeClass("ah-dragdrop-active");
    $("body").removeClass("ah-disableselect");
    var copy = d.copy;
    if (copy) {
      if (!dropped && d.el.hasAttribute("data-ah-revert") && d.orig) {
        $(copy).css("transition", "left .2s ease, top .2s ease")
          .css({ left: d.orig.left + "px", top: d.orig.top + "px" });
        setTimeout(function () { $(copy).remove(); }, 220);
      } else {
        $(copy).remove();
      }
    }
    $(item).trigger("ah:drag-end");
  }

  function ddDrop(d) {
    var zone = d.target, item = d.item, el = d.el;
    var key = item.getAttribute("data-ah-drag");
    var dest = zone.getAttribute("data-ah-drop");
    var focused = document.activeElement === item;
    $(zone).removeClass("ah-drop-target-active");
    if (el.hasAttribute("data-ah-move") && item.parentNode !== zone) {
      zone.appendChild(item);
      if (focused) { item.focus(); }
    }
    ddFinish(d, true);
    el.setAttribute("data-drag", key);
    el.setAttribute("data-drop", dest);
    el.setAttribute("data-from", d.from);
    $(zone).trigger("ah:drop", [{ drag: key, drop: dest, from: d.from }]);
    announce(ddLive(el), label(item) + " dropped on " + label(zone) + ".");
  }

  function ddCancel(d) {
    ddTarget(d, null);
    ddFinish(d, false);
    $(d.item).trigger("ah:drag-cancel");
  }

  function ddLive(el) {
    var $live = $(el).children(".ah-dnd-live");
    if (!$live.length) {
      $live = $('<span class="ah-sortable-live ah-dnd-live" aria-live="assertive" aria-atomic="true"></span>')
        .appendTo(el);
    }
    return $live;
  }

  // Keyboard: Space/Enter picks up, arrows walk the zones, Space/Enter
  // drops, Escape (or leaving the item) cancels.
  function ddKeydown(el, item, e) {
    var d = drag && drag.kind === "dragdrop-key" && drag.item === item ? drag : null;
    var pick = e.key === " " || e.key === "Enter";
    if (!d) {
      if (pick && !drag && ddUsable(el, item)) {
        e.preventDefault();
        d = { kind: "dragdrop-key", el: el, item: item };
        ddBegin(d);
        if (!d.zones.length) {
          ddFinish(d, false);
          announce(ddLive(el), "No drop zone takes " + label(item) + ".");
          return;
        }
        drag = d;
        d.cancel = function () { drag = null; ddCancel(d); };
        d.pos = -1;
        announce(ddLive(el), "Picked up " + label(item) + ". Arrow keys choose one of " +
                 d.zones.length + " drop zones, Space drops, Escape cancels.");
      }
      return;
    }
    var n = d.zones.length;
    if (/^Arrow/.test(e.key)) {
      e.preventDefault();
      var step = e.key === "ArrowUp" || e.key === "ArrowLeft" ? -1 : 1;
      d.pos = d.pos < 0 ? (step > 0 ? 0 : n - 1) : (d.pos + step + n) % n;
      ddTarget(d, d.zones[d.pos]);
      announce(ddLive(el), label(d.zones[d.pos]) + ", drop zone " + (d.pos + 1) + " of " + n + ".");
    } else if (pick) {
      e.preventDefault();
      drag = null;
      if (d.target) { ddDrop(d); } else { ddCancel(d); }
    } else if (e.key === "Escape") {
      e.preventDefault();
      d.cancel();
      announce(ddLive(el), "Cancelled.");
    }
  }

  AH.define("dragdrop", {
    init: function (el, $el) {
      $el.on("pointerdown" + NS, "[data-ah-drag]", function (e) {
        var item = this;
        if (drag || !ddScope(el, item) || !ddUsable(el, item) ||
            (e.pointerType === "mouse" && e.button !== 0) || editable(e.target)) {
          return;
        }
        if ($(e.target).closest("[data-ah-drag]")[0] !== item) { return; }
        e.preventDefault();
        try { item.focus({ preventScroll: true }); } catch (err) { /* ignore */ }
        var d = { kind: "dragdrop", el: el, item: item, pointerId: e.pointerId,
                  x0: px(e), y0: py(e), started: false };
        track(d, function (me) {
          if (!d.started) {
            if (Math.abs(px(me) - d.x0) + Math.abs(py(me) - d.y0) <= DISTANCE) { return; }
            d.started = true;
            var r = pageRect(item);
            d.orig = { left: r.left, top: r.top };
            d.offX = d.x0 - r.left;
            d.offY = d.y0 - r.top;
            d.box = scrollParent(el);
            d.copy = floatingCopy(item, "ah-drag-feedback", 0.6);
            $("body").addClass("ah-disableselect");
            ddBegin(d);
          }
          d.px = px(me); d.py = py(me); d.cx = me.clientX; d.cy = me.clientY;
          if (d.raf) { return; }
          d.raf = requestAnimationFrame(function () {
            d.raf = 0;
            if (drag !== d) { return; }
            $(d.copy).css({ left: (d.px - d.offX) + "px", top: (d.py - d.offY) + "px" });
            autoScroll(d.box, d.cx, d.cy);
            ddTarget(d, ddHit(d, d.px, d.py));
            $(item).trigger("ah:dragging", [{ pageX: d.px, pageY: d.py }]);
          });
        }, function (ue) {
          if (!d.started) { return; }
          swallowClick();
          // the last frame may not have run: test where the pointer let go
          $(d.copy).css({ left: (px(ue) - d.offX) + "px", top: (py(ue) - d.offY) + "px" });
          ddTarget(d, ddHit(d, px(ue), py(ue)));
          if (d.target) { ddDrop(d); } else { ddFinish(d, false); }
        }, function () {
          if (d.started) { ddCancel(d); }
        });
      });
      $el.on("keydown" + NS, "[data-ah-drag]", function (e) {
        if (e.target === this && ddScope(el, this)) { ddKeydown(el, this, e); }
      });
      $el.on("focusout" + NS, "[data-ah-drag]", function () {
        var item = this;
        setTimeout(function () {
          if (drag && drag.kind === "dragdrop-key" && drag.item === item &&
              document.activeElement !== item) {
            drag.cancel();
          }
        }, 0);
      });
    },
    destroy: function (el, $el) {
      cancelFor(el);
      $el.children(".ah-dnd-live").remove();
    },
    methods: {
      enable: function (el, $el) {
        $el.removeClass("ah-dragdrop-disabled").removeAttr("aria-disabled");
      },
      disable: function (el, $el) {
        cancelFor(el);
        $el.addClass("ah-dragdrop-disabled").attr("aria-disabled", "true");
      },
      cancel: function (el) { cancelFor(el); }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/layout_nav.js ---- */
/* Behaviours of the layout_nav components (designs/04-components.md):
   menu, navbar, sidenav, toolbar, splitter, listmenu. Ported from sigil's
   components/layout/*.cljs. status_bar is pure CSS. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var uid = 0;

  // ------------------------------------------------------------------
  // Shared helpers
  // ------------------------------------------------------------------

  // A per-instance namespace for document/window handlers, which
  // AH.destroy does not remove by itself.
  function instanceNs(el) {
    var ns = $.data(el, "ah-ns");
    if (!ns) {
      ns = NS + "-nav" + (++uid);
      $.data(el, "ah-ns", ns);
    }
    return ns;
  }

  function visible(el) {
    return !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length);
  }

  // Value contract: data-ah-value on the root, the hidden input, then a
  // jQuery event (change or input) on the root.
  function setValue($el, v, eventName) {
    v = v === null || v === undefined ? "" : String(v);
    $el.attr("data-ah-value", v);
    $el.children("input[type=hidden]").val(v);
    if (eventName) { $el.trigger(eventName); }
  }

  function touchOnly() {
    return window.matchMedia && window.matchMedia("(hover: none)").matches;
  }

  function byKey($scope, sel, attr, key) {
    return $scope.find(sel).filter(function () {
      return this.getAttribute(attr) === String(key);
    });
  }

  function animateSwap(oldEl, newEl, kind, dir, done) {
    if (!oldEl || oldEl === newEl) {
      $(newEl).show();
      done();
      return;
    }
    if (kind === "none" || !newEl.animate) {
      $(oldEl).hide();
      $(newEl).show();
      done();
      return;
    }
    var dur = 250;
    if (kind === "fade") {
      oldEl.animate([{ opacity: 1 }, { opacity: 0 }], { duration: dur / 2 }).onfinish = function () {
        $(oldEl).hide();
        $(newEl).show();
        newEl.animate([{ opacity: 0 }, { opacity: 1 }], { duration: dur / 2 }).onfinish = done;
      };
      return;
    }
    // slide: the old page leaves to one side while the new one comes in
    var s = dir > 0 ? "-100%" : "100%";
    var e = dir > 0 ? "100%" : "-100%";
    $(newEl).show();
    $(oldEl).css({ position: "absolute", top: 0, left: 0, width: "100%" });
    oldEl.animate([{ transform: "translateX(0)" }, { transform: "translateX(" + s + ")" }],
                  { duration: dur, easing: "ease" });
    newEl.animate([{ transform: "translateX(" + e + ")" }, { transform: "translateX(0)" }],
                  { duration: dur, easing: "ease" }).onfinish = function () {
      $(oldEl).hide().css({ position: "", top: "", left: "", width: "" });
      done();
    };
  }

  // ------------------------------------------------------------------
  // menu (sigil menu.cljs, menu/submenu, menu/keyboard, menu/responsive)
  // ------------------------------------------------------------------

  var M_OPEN = "ah-menu-submenu-open";
  var M_FOCUS = "ah-menu-link-focus";

  // Submenus float (AH.float) so an ancestor with overflow: hidden cannot
  // clip them: below a horizontal bar's top-level item, to the right of
  // any other item. AH.float flips and clamps them to the viewport.
  function subFloat(sub, on, opts) {
    var h = $.data(sub, "ah-float");
    if (h) { h.stop(); $.removeData(sub, "ah-float"); }
    if (on) { $.data(sub, "ah-float", AH.float(sub, opts.anchor, opts)); }
  }

  function subClose($subs) {
    $subs.each(function () { subFloat(this, false); })
      .removeClass(M_OPEN).css({ left: "", right: "", top: "", bottom: "" });
  }

  function menuOpenSub($item, $el) {
    var $sub = $item.children(".ah-menu-submenu");
    if (!$sub.length || $sub.hasClass(M_OPEN)) { return; }
    $sub.addClass(M_OPEN);
    if (!$el.hasClass("ah-menu-is-minimized") && !$item.closest(".ah-menu-drawer").length) {
      var bar = $item.parent().hasClass("ah-menu-list") && $el.hasClass("ah-menu-horizontal");
      var $link = $item.children(".ah-menu-link");
      subFloat($sub[0], true, {
        anchor: bar ? $link[0] : $item[0],
        placement: bar ? "bottom" : ($sub.hasClass("ah-menu-open-left") ? "left" : "right"),
        align: $sub.hasClass("ah-menu-open-up") ? "end" : (bar && $sub.hasClass("ah-menu-open-left") ? "end" : "start"),
        offset: 0
      });
    }
    $item.children(".ah-menu-link").attr("aria-expanded", "true");
  }

  function menuCloseSub($item) {
    var $sub = $item.children(".ah-menu-submenu");
    if (!$sub.length) { return; }
    subClose($sub.find("." + M_OPEN));
    $sub.find("[aria-expanded=true]").attr("aria-expanded", "false");
    subClose($sub);
    $item.children(".ah-menu-link").attr("aria-expanded", "false");
  }

  function menuCloseSiblings($item) {
    $item.siblings(".ah-menu-has-submenu").each(function () { menuCloseSub($(this)); });
  }

  function menuCloseAll($el) {
    subClose($el.find("." + M_OPEN));
    $el.find("[aria-expanded=true]").not(".ah-menu-minimized-btn").attr("aria-expanded", "false");
  }

  function menuFocus($el, link) {
    $el.find("." + M_FOCUS).removeClass(M_FOCUS);
    $(link).addClass(M_FOCUS);
    link.focus();
  }

  function siblingLinks($item) {
    return $item.parent().children(".ah-menu-item:not(.ah-menu-item-disabled)")
      .children(".ah-menu-link").get();
  }

  function firstIn($item) {
    return $item.children(".ah-menu-submenu").find(".ah-menu-item:not(.ah-menu-item-disabled) > .ah-menu-link")
      .get(0);
  }

  function menuPopupClose($el) {
    if ($el.hasClass("ah-menu-popup")) {
      menuCloseAll($el);
      $el.removeClass("ah-menu-open");
    }
  }

  function menuPopupOpen($el, x, y) {
    $el.addClass("ah-menu-open").css({ left: x + "px", top: y + "px" });
    var w = $el[0].offsetWidth;
    var h = $el[0].offsetHeight;
    var ax = x + w > window.innerWidth ? Math.max(0, window.innerWidth - w) : x;
    var ay = y + h > window.innerHeight ? Math.max(0, window.innerHeight - h) : y;
    $el.css({ left: ax + "px", top: ay + "px" });
    $el[0].focus();
  }

  // A leaf was chosen: follow its href, or make its key the value.
  function menuSelect($el, $link, e) {
    var href = $link.attr("href");
    if (!href) {
      if (e) { e.preventDefault(); }
      $el.find(".ah-menu-link-active").removeClass("ah-menu-link-active").removeAttr("aria-current");
      $link.addClass("ah-menu-link-active").attr("aria-current", "true");
      setValue($el, $link.attr("data-id"), "change");
    }
    menuCloseAll($el);
    $el.find("." + M_FOCUS).removeClass(M_FOCUS);
    menuPopupClose($el);
  }

  function menuKeydown($el, e) {
    var $focused = $el.find("." + M_FOCUS);
    var $item = $focused.length ? $focused.parent() : null;
    var horizontal = $el.hasClass("ah-menu-horizontal");
    var topLevel = $item && $item.parent().hasClass("ah-menu-list");
    var step = function (d) {
      if (!$item) { return; }
      var links = siblingLinks($item);
      var i = links.indexOf($focused[0]);
      if (links.length) { menuFocus($el, links[(i + d + links.length) % links.length]); }
    };
    var openFirst = function () {
      menuCloseSiblings($item);
      menuOpenSub($item, $el);
      var f = firstIn($item);
      if (f) { menuFocus($el, f); }
    };
    if (!$item && /^Arrow|^Home$|^End$/.test(e.key)) {
      e.preventDefault();
      var first = $el.find(".ah-menu-list > .ah-menu-item:not(.ah-menu-item-disabled) > .ah-menu-link").get(0);
      if (first) { menuFocus($el, first); }
      return;
    }
    if (!$item && e.key !== "Escape" && e.key !== "Tab") { return; }
    switch (e.key) {
      case "ArrowDown":
        e.preventDefault();
        if (horizontal && topLevel && $item.hasClass("ah-menu-has-submenu")) { openFirst(); }
        else { step(1); }
        break;
      case "ArrowUp":
        e.preventDefault();
        step(-1);
        break;
      case "ArrowRight":
        e.preventDefault();
        if (horizontal && topLevel) { menuCloseAll($el); step(1); }
        else if ($item.hasClass("ah-menu-has-submenu")) { openFirst(); }
        else if (horizontal) {
          // leave a submenu to the next top-level item, as menubars do
          var $top = $item.parents(".ah-menu-list > .ah-menu-item").last();
          menuCloseAll($el);
          $focused = $top.children(".ah-menu-link");
          $item = $top;
          step(1);
        }
        break;
      case "ArrowLeft":
        e.preventDefault();
        if (horizontal && topLevel) { menuCloseAll($el); step(-1); }
        else {
          var $parentItem = $item.parent().closest(".ah-menu-item");
          if ($parentItem.length) {
            menuCloseSub($parentItem);
            menuFocus($el, $parentItem.children(".ah-menu-link")[0]);
          }
        }
        break;
      case "Home":
      case "End":
        e.preventDefault();
        if ($item) {
          var ls = siblingLinks($item);
          if (ls.length) { menuFocus($el, ls[e.key === "Home" ? 0 : ls.length - 1]); }
        }
        break;
      case "Enter":
      case " ":
        e.preventDefault();
        if ($item.hasClass("ah-menu-has-submenu")) { openFirst(); }
        else { $focused[0].click(); }
        break;
      case "Escape":
        e.preventDefault();
        var $up = $item ? $item.parent().closest(".ah-menu-item") : $();
        if ($up.length && $up.children(".ah-menu-submenu").hasClass(M_OPEN) && !$el.hasClass("ah-menu-popup")) {
          menuCloseSub($up);
          menuFocus($el, $up.children(".ah-menu-link")[0]);
        } else {
          menuCloseAll($el);
          $el.find("." + M_FOCUS).removeClass(M_FOCUS);
          menuPopupClose($el);
          if (visible($el[0])) { $el[0].focus(); }
        }
        break;
      case "Tab":
        menuCloseAll($el);
        $el.find("." + M_FOCUS).removeClass(M_FOCUS);
        menuPopupClose($el);
        break;
      default:
        break;
    }
  }

  function menuDrawerClose(st) {
    if (!st.drawer) { return; }
    st.drawer.removeClass("ah-menu-drawer-open");
    st.backdrop.removeClass("ah-menu-drawer-backdrop-visible");
  }

  function menuMinimize($el, st) {
    if ($el.hasClass("ah-menu-is-minimized")) { return; }
    $el.addClass("ah-menu-is-minimized");
    var title = $el.attr("data-title") || "Menu";
    var $drawer = $('<div class="ah-menu-drawer" role="dialog" aria-modal="true"></div>');
    var $head = $('<div class="ah-menu-drawer-title"><span></span>' +
                  '<button type="button" class="ah-menu-drawer-close" aria-label="Close">×</button></div>');
    $head.children("span").text(title);
    var $list = $('<div class="ah-menu-drawer-list"></div>')
      .append($el.children(".ah-menu-list").clone().removeAttr("id"));
    $list.find("[id]").removeAttr("id");
    $list.find("." + M_OPEN).removeClass(M_OPEN);
    $drawer.attr("aria-label", title).append($head, $list);
    var $backdrop = $('<div class="ah-menu-drawer-backdrop"></div>');
    $("body").append($backdrop, $drawer);
    st.drawer = $drawer;
    st.backdrop = $backdrop;
    $drawer.on("click", ".ah-menu-drawer-close", function () { menuDrawerClose(st); });
    $backdrop.on("click", function () { menuDrawerClose(st); });
    $drawer.on("keydown", function (e) {
      if (e.key === "Escape") { menuDrawerClose(st); $el.children(".ah-menu-minimized-btn")[0].focus(); }
    });
    $drawer.on("click", ".ah-menu-has-submenu > .ah-menu-link", function (e) {
      e.preventDefault();
      var $sub = $(this).parent().children(".ah-menu-submenu").toggleClass(M_OPEN);
      $(this).attr("aria-expanded", String($sub.hasClass(M_OPEN)));
    });
    $drawer.on("click", ".ah-menu-item:not(.ah-menu-has-submenu):not(.ah-menu-item-disabled) > .ah-menu-link",
      function (e) {
        var $orig = byKey($el, ".ah-menu-link", "data-id", this.getAttribute("data-id"));
        if (!this.getAttribute("href")) { e.preventDefault(); }
        if ($orig.length) { menuSelect($el, $orig.first(), null); }
        $drawer.find(".ah-menu-link-active").removeClass("ah-menu-link-active");
        $(this).addClass("ah-menu-link-active");
        menuDrawerClose(st);
      });
  }

  function menuRestore($el, st) {
    if (!$el.hasClass("ah-menu-is-minimized")) { return; }
    $el.removeClass("ah-menu-is-minimized");
    if (st.drawer) { st.drawer.remove(); st.backdrop.remove(); }
    st.drawer = st.backdrop = null;
  }

  AH.define("menu", {
    init: function (el, $el) {
      var st = { openT: null, closeT: null, drawer: null, backdrop: null };
      $.data(el, "ah-menu", st);
      var ns = instanceNs(el);
      var clickToOpen = el.hasAttribute("data-ah-click-to-open");

      // hover opens submenus, with a short intent delay when a sibling is open
      $el.on("mouseenter" + NS, ".ah-menu-has-submenu", function () {
        if (clickToOpen || $el.hasClass("ah-menu-is-minimized")) { return; }
        var $item = $(this);
        clearTimeout(st.closeT);
        clearTimeout(st.openT);
        var open = function () {
          st.openT = null;
          menuCloseSiblings($item);
          menuOpenSub($item, $el);
        };
        if ($item.parent().find("> .ah-menu-item > ." + M_OPEN).length) {
          st.openT = setTimeout(open, 60);
        } else {
          open();
        }
      });
      $el.on("mouseleave" + NS, ".ah-menu-has-submenu", function () {
        if (clickToOpen) { return; }
        var $item = $(this);
        clearTimeout(st.openT);
        st.closeT = setTimeout(function () {
          menuCloseSiblings($item);
          menuCloseSub($item);
        }, 200);
      });
      // click toggles a submenu (always in click-to-open or touch mode;
      // otherwise it opens one that hover has not opened, e.g. from a
      // screen reader)
      $el.on("click" + NS, ".ah-menu-has-submenu > .ah-menu-link", function (e) {
        e.preventDefault();
        var $item = $(this).parent();
        var open = $item.children(".ah-menu-submenu").hasClass(M_OPEN);
        if (open && (clickToOpen || touchOnly())) {
          menuCloseSub($item);
        } else if (!open) {
          menuCloseSiblings($item);
          menuOpenSub($item, $el);
        }
      });
      $el.on("click" + NS, ".ah-menu-item:not(.ah-menu-has-submenu):not(.ah-menu-item-disabled) > .ah-menu-link",
        function (e) { menuSelect($el, $(this), e); });
      $el.on("click" + NS, ".ah-menu-item-disabled > .ah-menu-link", function (e) { e.preventDefault(); });

      // outside click closes everything
      $(document).on("mousedown" + ns, function (e) {
        if (!$.contains(el, e.target) && !$(e.target).closest(".ah-menu-drawer").length) {
          menuCloseAll($el);
          $el.find("." + M_FOCUS).removeClass(M_FOCUS);
          menuPopupClose($el);
        }
      });

      // context menu
      if ($el.hasClass("ah-menu-popup")) {
        var target = el.getAttribute("data-ah-popup-target");
        $.data(el, "ah-menu-target", target || document);
        $(target || document).on("contextmenu" + ns, function (e) {
          e.preventDefault();
          menuCloseAll($el);
          menuPopupOpen($el, e.clientX, e.clientY);
        });
      }

      if (el.getAttribute("data-ah-keyboard") !== "false") {
        $el.on("keydown" + NS, function (e) {
          if (e.target === el || $(e.target).hasClass("ah-menu-link")) { menuKeydown($el, e); }
        });
        $el.on("focus" + NS, function () {
          if (!$el.find("." + M_FOCUS).length && !$el.hasClass("ah-menu-is-minimized")) {
            var f = $el.find(".ah-menu-list > .ah-menu-item:not(.ah-menu-item-disabled) > .ah-menu-link").get(0);
            if (f) { menuFocus($el, f); }
          }
        });
      }

      // responsive collapse to a hamburger + drawer
      $el.on("click" + NS, ".ah-menu-minimized-btn", function () {
        if (!st.drawer) { return; }
        st.backdrop.addClass("ah-menu-drawer-backdrop-visible");
        requestAnimationFrame(function () {
          st.drawer.addClass("ah-menu-drawer-open");
          var f = st.drawer.find(".ah-menu-link").get(0);
          if (f) { f.setAttribute("tabindex", "0"); f.focus(); }
        });
      });
      var minW = parseInt(el.getAttribute("data-ah-minimize-width"), 10);
      if (minW) {
        var check = function () {
          if (window.innerWidth <= minW) { menuMinimize($el, st); } else { menuRestore($el, st); }
        };
        var t = null;
        $(window).on("resize" + ns, function () { clearTimeout(t); t = setTimeout(check, 150); });
        check();
      }
    },
    destroy: function (el, $el) {
      var st = $.data(el, "ah-menu") || {};
      var ns = instanceNs(el);
      $(document).off(ns);
      $(window).off(ns);
      var target = $.data(el, "ah-menu-target");
      if (target) { $(target).off(ns); }
      clearTimeout(st.openT);
      clearTimeout(st.closeT);
      menuCloseAll($el);
      menuRestore($el, st);
    },
    methods: {
      open: function (el, $el, x, y) { menuPopupOpen($el, x, y); },
      close: function (el, $el) { menuCloseAll($el); $el.removeClass("ah-menu-open"); },
      closeAll: function (el, $el) { menuCloseAll($el); },
      openItem: function (el, $el, key) {
        var $l = byKey($el, ".ah-menu-link", "data-id", key);
        if ($l.length) { menuOpenSub($l.parent(), $el); }
      },
      closeItem: function (el, $el, key) {
        var $l = byKey($el, ".ah-menu-link", "data-id", key);
        if ($l.length) { menuCloseSub($l.parent()); }
      },
      disableItem: function (el, $el, key) {
        byKey($el, ".ah-menu-link", "data-id", key).attr("aria-disabled", "true")
          .parent().addClass("ah-menu-item-disabled");
      },
      enableItem: function (el, $el, key) {
        byKey($el, ".ah-menu-link", "data-id", key).removeAttr("aria-disabled")
          .parent().removeClass("ah-menu-item-disabled");
      },
      setValue: function (el, $el, key) {
        $el.find(".ah-menu-link-active").removeClass("ah-menu-link-active").removeAttr("aria-current");
        byKey($el, ".ah-menu-link", "data-id", key).addClass("ah-menu-link-active").attr("aria-current", "true");
        setValue($el, key);
      },
      minimize: function (el, $el) { menuMinimize($el, $.data(el, "ah-menu")); },
      restore: function (el, $el) { menuRestore($el, $.data(el, "ah-menu")); }
    }
  });

  // ------------------------------------------------------------------
  // navbar (sigil navbar.cljs, navbar/popup.cljs)
  // ------------------------------------------------------------------

  function navbarMark($scope, key) {
    $scope.find(".ah-navbar-item").each(function () {
      var on = this.getAttribute("data-key") === String(key);
      $(this).toggleClass("ah-navbar-item-selected", on).attr("aria-selected", String(on));
      if ($scope.hasClass("ah-navbar")) { this.setAttribute("tabindex", on ? "0" : "-1"); }
    });
  }

  function navbarSelect($el, item, e) {
    if ($(item).hasClass("ah-navbar-item-disabled") || $el.attr("data-ah-selection") === "false") {
      if (e && !item.getAttribute("href")) { e.preventDefault(); }
      return;
    }
    var key = item.getAttribute("data-key");
    navbarMark($el, key);
    if (!item.getAttribute("href")) {
      if (e) { e.preventDefault(); }
      if ($el.attr("data-ah-value") !== key) { setValue($el, key, "change"); }
    }
  }

  function navbarPopupClose($el) {
    var $p = $.data($el[0], "ah-navbar-popup");
    if ($p) {
      var h = $.data($p[0], "ah-float");
      if (h) { h.stop(); }
      $p.remove();
      $.removeData($el[0], "ah-navbar-popup");
      $el.find(".ah-navbar-header").attr("aria-expanded", "false");
    }
  }

  function navbarPopupOpen($el) {
    var $p = $('<div class="ah-navbar-popup" role="listbox"></div>');
    $el.children(".ah-navbar-item").each(function () {
      var $c = $(this).clone().removeAttr("id style").attr({ role: "option", tabindex: "0" });
      $c.find("[id]").removeAttr("id");
      $p.append($c);
    });
    $p.css({ width: $el.outerWidth() + "px", display: "block" });
    $("body").append($p);
    $.data($el[0], "ah-navbar-popup", $p);
    $.data($p[0], "ah-float", AH.float($p[0], $el[0], { placement: "bottom", offset: 0, matchWidth: true }));
    $el.find(".ah-navbar-header").attr("aria-expanded", "true");
    $p.on("click", ".ah-navbar-item", function (e) {
      var $orig = byKey($el, ".ah-navbar-item", "data-key", this.getAttribute("data-key"));
      if ($orig.length) { navbarSelect($el, $orig[0], e); }
      navbarPopupClose($el);
    });
    $p.on("keydown", ".ah-navbar-item", function (e) {
      var items = $p.children(".ah-navbar-item").get();
      var i = items.indexOf(this);
      if (e.key === "ArrowDown" || e.key === "ArrowUp") {
        e.preventDefault();
        items[(i + (e.key === "ArrowDown" ? 1 : -1) + items.length) % items.length].focus();
      } else if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        this.click();
        $el.find(".ah-navbar-header")[0].focus();
      } else if (e.key === "Escape") {
        navbarPopupClose($el);
        $el.find(".ah-navbar-header")[0].focus();
      }
    });
    var sel = $p.children(".ah-navbar-item-selected").get(0) || $p.children(".ah-navbar-item").get(0);
    return sel;
  }

  AH.define("navbar", {
    init: function (el, $el) {
      var ns = instanceNs(el);
      $el.on("click" + NS, ".ah-navbar-item", function (e) { navbarSelect($el, this, e); });
      $el.on("mouseenter" + NS, ".ah-navbar-item", function () { $(this).addClass("ah-navbar-item-hover"); });
      $el.on("mouseleave" + NS, ".ah-navbar-item", function () { $(this).removeClass("ah-navbar-item-hover"); });
      // tabs pattern: arrows move focus, Enter / Space select
      $el.on("keydown" + NS, ".ah-navbar-item", function (e) {
        var items = $el.children(".ah-navbar-item").filter(function () {
          return !$(this).hasClass("ah-navbar-item-disabled");
        }).get();
        var i = items.indexOf(this);
        var n = items.length;
        var to = null;
        switch (e.key) {
          case "ArrowRight": case "ArrowDown": to = (i + 1) % n; break;
          case "ArrowLeft": case "ArrowUp": to = (i - 1 + n) % n; break;
          case "Home": to = 0; break;
          case "End": to = n - 1; break;
          case "Enter": case " ":
            e.preventDefault();
            this.click();
            return;
          default: return;
        }
        e.preventDefault();
        $(items).attr("tabindex", "-1");
        items[to].setAttribute("tabindex", "0");
        items[to].focus();
      });
      var toggle = function () {
        if ($.data(el, "ah-navbar-popup")) { navbarPopupClose($el); return null; }
        return navbarPopupOpen($el);
      };
      $el.on("click" + NS, ".ah-navbar-header", function () { toggle(); });
      $el.on("keydown" + NS, ".ah-navbar-header", function (e) {
        if (e.key === "Enter" || e.key === " " || e.key === "ArrowDown") {
          e.preventDefault();
          var first = $.data(el, "ah-navbar-popup") && e.key === "ArrowDown" ? null : toggle();
          if (first) { first.focus(); }
        } else if (e.key === "Escape") {
          navbarPopupClose($el);
        }
      });
      $(document).on("mousedown" + ns, function (e) {
        var $p = $.data(el, "ah-navbar-popup");
        if ($p && !$.contains($p[0], e.target) && !$(e.target).closest(".ah-navbar-header").length) {
          navbarPopupClose($el);
        }
      });
      var minW = parseInt(el.getAttribute("data-ah-minimize-width"), 10);
      if (minW && el.getAttribute("data-ah-minimized") !== "static") {
        var check = function () {
          var small = window.innerWidth <= minW;
          $el.toggleClass("ah-navbar-minimized", small);
          if (!small) { navbarPopupClose($el); }
        };
        $(window).on("resize" + ns, check);
        check();
      }
    },
    destroy: function (el, $el) {
      var ns = instanceNs(el);
      $(document).off(ns);
      $(window).off(ns);
      navbarPopupClose($el);
    },
    methods: {
      setValue: function (el, $el, key) { navbarMark($el, key); setValue($el, key); },
      select: function (el, $el, key) {
        var $i = byKey($el, ".ah-navbar-item", "data-key", key);
        if ($i.length) { navbarSelect($el, $i[0], null); }
      },
      minimize: function (el, $el) { $el.addClass("ah-navbar-minimized"); },
      restore: function (el, $el) { $el.removeClass("ah-navbar-minimized"); navbarPopupClose($el); }
    }
  });

  // ------------------------------------------------------------------
  // sidenav (sigil sidenav.cljs + nav-tree)
  // ------------------------------------------------------------------

  function sidenavMark($el, key) {
    $el.find(".ah-nav-tree__item.ah-is-active").removeClass("ah-is-active").removeAttr("aria-current");
    var $a = byKey($el, "a.ah-nav-tree__item", "data-route", key);
    $a.addClass("ah-is-active").attr("aria-current", "page");
    $a.parents("details.ah-nav-tree__node").each(function () {
      this.open = true;
      $(this).children("summary").addClass("ah-is-open");
    });
  }

  function sidenavCollapse($el, collapsed) {
    $el.toggleClass("ah-sidenav-collapsed", collapsed);
    $el.find(".ah-sidenav__toggle").attr("aria-expanded", String(!collapsed));
    $el.trigger("ah:collapse", [{ collapsed: collapsed }]);
  }

  AH.define("sidenav", {
    init: function (el, $el) {
      $el.on("click" + NS, "a.ah-nav-tree__item", function (e) {
        if (this.getAttribute("aria-disabled") === "true") { e.preventDefault(); return; }
        var key = this.getAttribute("data-route");
        sidenavMark($el, key);
        if (this.getAttribute("href") === "#") {
          e.preventDefault();
          if ($el.attr("data-ah-value") !== key) { setValue($el, key, "change"); }
        }
      });
      // a collapsed sidebar expands when a group is opened
      $el.on("click" + NS, "summary.ah-nav-tree__item", function (e) {
        if ($el.hasClass("ah-sidenav-collapsed")) {
          e.preventDefault();
          sidenavCollapse($el, false);
          this.parentNode.open = true;
          $(this).addClass("ah-is-open");
        }
      });
      var onToggle = function (e) {
        if (e.target.tagName === "DETAILS") {
          $(e.target).children("summary").toggleClass("ah-is-open", e.target.open);
        }
      };
      el.addEventListener("toggle", onToggle, true);
      $.data(el, "ah-sidenav-toggle", onToggle);
      $el.on("click" + NS, ".ah-sidenav__toggle", function () {
        sidenavCollapse($el, !$el.hasClass("ah-sidenav-collapsed"));
      });
      // arrow keys move between the visible entries
      $el.on("keydown" + NS, ".ah-nav-tree__item", function (e) {
        if (!/^(ArrowDown|ArrowUp|Home|End)$/.test(e.key)) { return; }
        var items = $el.find(".ah-nav-tree__item").filter(function () { return visible(this); }).get();
        var i = items.indexOf(this);
        var to = e.key === "Home" ? 0 : e.key === "End" ? items.length - 1
          : Math.max(0, Math.min(items.length - 1, i + (e.key === "ArrowDown" ? 1 : -1)));
        e.preventDefault();
        if (items[to]) { items[to].focus(); }
      });
    },
    destroy: function (el) {
      var f = $.data(el, "ah-sidenav-toggle");
      if (f) { el.removeEventListener("toggle", f, true); }
    },
    methods: {
      setValue: function (el, $el, key) { sidenavMark($el, key); setValue($el, key); },
      collapse: function (el, $el) { sidenavCollapse($el, true); },
      expand: function (el, $el) { sidenavCollapse($el, false); },
      toggle: function (el, $el) { sidenavCollapse($el, !$el.hasClass("ah-sidenav-collapsed")); }
    }
  });

  // ------------------------------------------------------------------
  // toolbar (sigil toolbar.cljs, toolbar/overflow.cljs)
  // ------------------------------------------------------------------
  //
  // Tools that do not fit are moved (not copied) into the overflow popup,
  // so their handlers and data-ah-on keep working, and moved back when
  // there is room again.

  function tbState(el) { return $.data(el, "ah-toolbar"); }

  function tbTools($el) {
    return $el.children(".ah-toolbar-tool").map(function () {
      var $prev = $(this).prev();
      return {
        el: this,
        sep: $prev.hasClass("ah-toolbar-separator") ? $prev[0] : null,
        minimizable: this.getAttribute("data-ah-minimizable") !== "false",
        button: $(this).children("button.ah-toolbar-tool-el").length > 0
      };
    }).get();
  }

  function tbMinimize(st, t) {
    if (t.min) { return; }
    t.min = true;
    $(t.el).css("display", "none");
    if (t.sep) { $(t.sep).css("display", "none"); }
    $(t.menuSep).addClass("ah-toolbar-popup-separator-visible");
    $(t.popupTool).append($(t.el).children()).addClass("ah-toolbar-popup-tool-visible");
  }

  function tbRestore(st, t) {
    if (!t.min) { return; }
    t.min = false;
    $(t.el).append($(t.popupTool).children()).css("display", "");
    if (t.sep) { $(t.sep).css("display", ""); }
    $(t.menuSep).removeClass("ah-toolbar-popup-separator-visible");
    $(t.popupTool).removeClass("ah-toolbar-popup-tool-visible");
  }

  function tbGroups(st) {
    var shown = st.tools.filter(function (t) { return !t.min; });
    shown.forEach(function (t, i) {
      var prev = i > 0 && shown[i - 1].button && !t.sep;
      var next = i + 1 < shown.length && shown[i + 1].button && !shown[i + 1].sep;
      var $t = $(t.el).removeClass("ah-toolbar-tool-first ah-toolbar-tool-inner ah-toolbar-tool-last");
      if (!t.button) { return; }
      if (prev && next) { $t.addClass("ah-toolbar-tool-inner"); }
      else if (next) { $t.addClass("ah-toolbar-tool-first"); }
      else if (prev) { $t.addClass("ah-toolbar-tool-last"); }
    });
  }

  function tbLayout($el) {
    var st = tbState($el[0]);
    if (!st || !visible($el[0])) { return; }
    var $btn = $el.children(".ah-toolbar-minimize-btn");
    var avail = function () {
      // the button's margin-left is auto, so count its box only
      return $el.width() - ($btn.hasClass("ah-toolbar-minimize-visible") ? $btn[0].offsetWidth : 0);
    };
    var used = function () {
      return st.tools.reduce(function (acc, t) {
        if (t.min) { return acc; }
        return acc + $(t.el).outerWidth(true) + (t.sep ? $(t.sep).outerWidth(true) : 0);
      }, 0);
    };
    var cands;
    // minimise from the right while the tools overflow
    while (used() > avail() &&
           (cands = st.tools.filter(function (t) { return t.minimizable && !t.min; })).length) {
      $btn.addClass("ah-toolbar-minimize-visible");
      tbMinimize(st, cands[cands.length - 1]);
    }
    // restore from the left while they fit
    var hidden;
    while ((hidden = st.tools.filter(function (t) { return t.minimizable && t.min; })).length) {
      var t = hidden[0];
      tbRestore(st, t);
      if (hidden.length === 1) { $btn.removeClass("ah-toolbar-minimize-visible"); }
      if (used() > avail()) {
        $btn.addClass("ah-toolbar-minimize-visible");
        tbMinimize(st, t);
        break;
      }
    }
    var any = st.tools.some(function (t) { return t.min; });
    $btn.toggleClass("ah-toolbar-minimize-visible", any);
    if (!any) { tbClose($el); }
    tbGroups(st);
  }

  function tbOpen($el) {
    var st = tbState($el[0]);
    if (st.open) { return; }
    var w = parseInt($el.attr("data-ah-popup-width"), 10) || 200;
    st.popup.css({ width: w + "px" }).addClass("ah-toolbar-popup-open");
    st.float = AH.float(st.popup[0], $el[0], { placement: "bottom", align: "end", offset: 0 });
    st.open = true;
    $el.children(".ah-toolbar-minimize-btn").attr("aria-expanded", "true");
    $el.trigger("ah:open");
  }

  function tbClose($el) {
    var st = tbState($el[0]);
    if (!st || !st.open) { return; }
    st.popup.removeClass("ah-toolbar-popup-open");
    if (st.float) { st.float.stop(); st.float = null; }
    st.open = false;
    $el.children(".ah-toolbar-minimize-btn").attr("aria-expanded", "false");
    $el.trigger("ah:close");
  }

  function tbActivate($el, btn) {
    var $b = $(btn);
    var key = $b.attr("data-key");
    if (btn.hasAttribute("data-ah-toggle")) {
      var on = $b.attr("aria-pressed") !== "true";
      $b.attr("aria-pressed", String(on)).toggleClass("ah-btn-toggled", on);
    }
    if (key) { setValue($el, key, "change"); }
  }

  AH.define("toolbar", {
    init: function (el, $el) {
      var ns = instanceNs(el);
      var $popup = $('<div class="ah-toolbar-popup" role="menu" aria-label="Overflow tools"></div>');
      var st = { tools: tbTools($el), popup: $popup, open: false };
      st.tools.forEach(function (t) {
        if (t.sep) {
          t.menuSep = $('<div class="ah-toolbar-popup-separator" role="separator"></div>')
            .appendTo($popup)[0];
        }
        t.popupTool = $('<div class="ah-toolbar-popup-tool"></div>').appendTo($popup)[0];
        t.min = false;
      });
      $("body").append($popup);
      $.data(el, "ah-toolbar", st);

      var onTool = function (e) {
        var btn = e.currentTarget;
        if (btn.disabled) { return; }
        tbActivate($el, btn);
        if ($.contains($popup[0], btn) && !btn.hasAttribute("data-ah-toggle")) { tbClose($el); }
      };
      $el.on("click" + NS, "button.ah-toolbar-tool-el", onTool);
      $popup.on("click", "button.ah-toolbar-tool-el", onTool);
      $popup.on("keydown", function (e) {
        if (e.key === "Escape") {
          tbClose($el);
          $el.children(".ah-toolbar-minimize-btn")[0].focus();
        }
      });
      $el.on("click" + NS, ".ah-toolbar-minimize-btn", function (e) {
        e.stopPropagation();
        if (st.open) { tbClose($el); } else { tbOpen($el); }
      });
      $el.on("keydown" + NS, ".ah-toolbar-minimize-btn", function (e) {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          if (st.open) { tbClose($el); }
          else {
            tbOpen($el);
            var f = $popup.find("button:not([disabled]), select, input, [tabindex]").filter(function () {
              return visible(this);
            }).get(0);
            if (f) { f.focus(); }
          }
        }
      });
      // arrows move between tools, Home / End jump to the ends
      $el.on("keydown" + NS, function (e) {
        if (!/^(ArrowLeft|ArrowRight|Home|End)$/.test(e.key)) { return; }
        if (/^(INPUT|SELECT|TEXTAREA)$/.test(e.target.tagName)) { return; }
        var items = $el.find(".ah-toolbar-tool button:not([disabled]), .ah-toolbar-tool select, " +
                            ".ah-toolbar-tool input, .ah-toolbar-minimize-btn")
          .filter(function () { return visible(this); }).get();
        var i = items.indexOf(e.target);
        if (i < 0) { return; }
        var n = items.length;
        var to = e.key === "Home" ? 0 : e.key === "End" ? n - 1
          : (i + (e.key === "ArrowRight" ? 1 : -1) + n) % n;
        e.preventDefault();
        items[to].focus();
      });
      $(document).on("mousedown" + ns, function (e) {
        if (st.open && !$.contains($popup[0], e.target) &&
            !$(e.target).closest(".ah-toolbar-minimize-btn").length) {
          tbClose($el);
        }
      });
      if (window.ResizeObserver) {
        st.ro = new ResizeObserver(function () { tbLayout($el); });
        st.ro.observe(el);
      } else {
        $(window).on("resize" + ns, function () { tbLayout($el); });
      }
      requestAnimationFrame(function () { tbLayout($el); });
    },
    destroy: function (el) {
      var st = tbState(el);
      var ns = instanceNs(el);
      $(document).off(ns);
      $(window).off(ns);
      if (st) {
        if (st.ro) { st.ro.disconnect(); }
        if (st.float) { st.float.stop(); }
        st.tools.forEach(function (t) { tbRestore(st, t); });
        st.popup.remove();
      }
      $.removeData(el, "ah-toolbar");
    },
    methods: {
      layout: function (el, $el) { tbLayout($el); },
      open: function (el, $el) { tbOpen($el); },
      close: function (el, $el) { tbClose($el); },
      disableTool: function (el, $el, key, disabled) {
        var st = tbState(el);
        var $b = byKey($el.add(st ? st.popup : $()), "button.ah-toolbar-tool-el", "data-key", key);
        $b.prop("disabled", disabled !== false);
      },
      setPressed: function (el, $el, key, pressed) {
        var st = tbState(el);
        byKey($el.add(st ? st.popup : $()), "button.ah-toolbar-tool-el", "data-key", key)
          .attr("aria-pressed", String(!!pressed)).toggleClass("ah-btn-toggled", !!pressed);
      }
    }
  });

  // ------------------------------------------------------------------
  // splitter (sigil splitter.cljs)
  // ------------------------------------------------------------------
  //
  // The first pane's flex-basis is a fraction of the space left by the
  // bar, so the split keeps its proportion when the container resizes.

  function spState(el) {
    var st = $.data(el, "ah-splitter");
    if (st) { return st; }
    var $el = $(el);
    var horiz = $el.hasClass("ah-splitter-horizontal");
    var mins = (el.getAttribute("data-ah-min") || "0,0").split(",").map(function (x) {
      return parseFloat(x) || 0;
    });
    st = {
      horiz: horiz,
      $p: $el.children(".ah-splitter-panel"),
      $bar: $el.children(".ah-splitter-splitbar"),
      min0: mins[0],
      min1: mins[1] || 0,
      frac: null,
      saved: null
    };
    $.data(el, "ah-splitter", st);
    return st;
  }

  function spDim(st, node) { return st.horiz ? node.offsetHeight : node.offsetWidth; }
  function spAvail(el, st) { return spDim(st, el) - spDim(st, st.$bar[0]); }

  function spFormat(f) {
    var a = Math.round(f * 1000) / 10;
    var b = Math.round((100 - a) * 10) / 10;
    return a + "," + b;
  }

  function spApply(el, st, frac) {
    st.frac = Math.max(0, Math.min(1, frac));
    var bar = spDim(st, st.$bar[0]);
    st.$p.eq(0).css("flex", "0 0 calc((100% - " + bar + "px) * " + st.frac.toFixed(5) + ")");
    st.$bar.attr("aria-valuenow", Math.round(st.frac * 100));
  }

  // Resize pane 0 to px (clamped to the minimum sizes).
  function spResize(el, st, px) {
    var avail = spAvail(el, st);
    if (avail <= 0) { return; }
    var clamped = Math.max(st.min0, Math.min(avail - st.min1, px));
    spApply(el, st, clamped / avail);
  }

  function spCollapsed(el, st, on) {
    $(el).toggleClass("ah-splitter-collapsed", on);
    st.$p.eq(0).css(st.horiz ? "min-height" : "min-width", on ? "0px" : st.min0 + "px");
  }

  function spToggle(el, st) {
    if ($(el).hasClass("ah-splitter-collapsed")) {
      spCollapsed(el, st, false);
      spApply(el, st, st.saved !== null ? st.saved : 0.5);
      $(el).trigger("ah:expanded");
    } else {
      st.saved = st.frac;
      spCollapsed(el, st, true);
      spApply(el, st, 0);
      $(el).trigger("ah:collapsed");
    }
    setValue($(el), spFormat(st.frac), "change");
  }

  function spEnabled(el) {
    return !$(el).hasClass("ah-splitter-disabled") && el.getAttribute("data-ah-resizable") !== "false";
  }

  AH.define("splitter", {
    init: function (el, $el) {
      var st = spState(el);
      var avail = spAvail(el, st);
      // measure the initial split (pixels or percent) as a fraction
      if (avail > 0) {
        var v = el.getAttribute("data-ah-value");
        spApply(el, st, v ? parseFloat(v) / 100 : spDim(st, st.$p[0]) / avail);
      }
      var drag = null;
      st.$bar.on("pointerdown" + NS, function (e) {
        if (e.button !== 0 || !spEnabled(el) || $(e.target).closest(".ah-splitter-collapse-btn").length) {
          return;
        }
        e.preventDefault();
        if (this.setPointerCapture) { this.setPointerCapture(e.pointerId); }
        drag = { start: st.horiz ? e.clientY : e.clientX, size: spDim(st, st.$p[0]),
                 value: el.getAttribute("data-ah-value") };
        if ($el.hasClass("ah-splitter-collapsed")) { spCollapsed(el, st, false); }
        $el.addClass("ah-splitter-dragging");
        $el.trigger("ah:resize-start");
      });
      st.$bar.on("pointermove" + NS, function (e) {
        if (!drag) { return; }
        var want = drag.size + (st.horiz ? e.clientY : e.clientX) - drag.start;
        var max = spAvail(el, st) - st.min1;
        st.$bar.toggleClass("ah-splitbar-invalid", want <= st.min0 || want >= max);
        spResize(el, st, want);
        setValue($el, spFormat(st.frac), "input");
      });
      st.$bar.on("pointerup" + NS + " pointercancel" + NS, function () {
        if (!drag) { return; }
        var before = drag.value;
        drag = null;
        st.$bar.removeClass("ah-splitbar-invalid");
        $el.removeClass("ah-splitter-dragging");
        var v = spFormat(st.frac);
        if (v !== before) { setValue($el, v, "change"); }
        $el.trigger("ah:resize");
      });
      st.$bar.on("click" + NS, ".ah-splitter-collapse-btn", function (e) {
        e.stopPropagation();
        if (spEnabled(el) || $el.hasClass("ah-splitter-collapsed")) { spToggle(el, st); }
      });
      st.$bar.on("keydown" + NS, function (e) {
        if (e.target !== this || !spEnabled(el)) { return; }
        var step = (parseInt(el.getAttribute("data-ah-step"), 10) || 10) * (e.shiftKey ? 5 : 1);
        var cur = spDim(st, st.$p[0]);
        var dec = st.horiz ? "ArrowUp" : "ArrowLeft";
        var inc = st.horiz ? "ArrowDown" : "ArrowRight";
        var px;
        switch (e.key) {
          case dec: px = cur - step; break;
          case inc: px = cur + step; break;
          case "Home": px = 0; break;
          case "End": px = Infinity; break;
          case "Enter": e.preventDefault(); spToggle(el, st); return;
          default: return;
        }
        e.preventDefault();
        if ($el.hasClass("ah-splitter-collapsed")) { spCollapsed(el, st, false); }
        spResize(el, st, px);
        var v = spFormat(st.frac);
        if (v !== el.getAttribute("data-ah-value")) { setValue($el, v, "change"); }
      });
    },
    destroy: function (el) {
      $.removeData(el, "ah-splitter");
    },
    methods: {
      // sizes: pane 0 in percent (a number or "30")
      setSizes: function (el, $el, pct) {
        var st = spState(el);
        spCollapsed(el, st, false);
        spApply(el, st, parseFloat(pct) / 100);
        setValue($el, spFormat(st.frac));
      },
      getSizes: function (el) {
        var st = spState(el);
        return [spDim(st, st.$p[0]), spDim(st, st.$p[1])];
      },
      collapse: function (el) {
        if (!$(el).hasClass("ah-splitter-collapsed")) { spToggle(el, spState(el)); }
      },
      expand: function (el) {
        if ($(el).hasClass("ah-splitter-collapsed")) { spToggle(el, spState(el)); }
      }
    }
  });

  // ------------------------------------------------------------------
  // listmenu (sigil listmenu.cljs, listmenu/nav.cljs)
  // ------------------------------------------------------------------

  var LM_FOCUS = "ah-listmenu-item-focus";

  function lmState(el) {
    var st = $.data(el, "ah-listmenu");
    if (!st) {
      var s = el.getAttribute("data-ah-stack");
      st = { stack: s ? s.split(",") : [], busy: false };
      $.data(el, "ah-listmenu", st);
    }
    return st;
  }

  function lmPage($el, id) {
    return $el.find(".ah-listmenu-page").filter(function () {
      return this.getAttribute("data-page-id") === String(id);
    });
  }

  function lmCurrent($el) {
    var st = lmState($el[0]);
    return lmPage($el, st.stack.length ? st.stack[st.stack.length - 1] : "root");
  }

  function lmItemLabel($el, itemId) {
    return $el.find(".ah-listmenu-item").filter(function () {
      return this.getAttribute("data-item-id") === String(itemId);
    }).children(".ah-listmenu-item-label").text();
  }

  function lmHeader($el) {
    var st = lmState($el[0]);
    var root = !st.stack.length;
    $el.find(".ah-listmenu-back").toggle(!root);
    $el.find(".ah-listmenu-title").text(root ? "" : lmItemLabel($el, st.stack[st.stack.length - 1]));
  }

  function lmItems($page) {
    return $page.children(".ah-listmenu-item").filter(function () {
      return !$(this).hasClass("ah-listmenu-item-disabled") && this.style.display !== "none";
    });
  }

  function lmFocus($el, item) {
    $el.find("." + LM_FOCUS).removeClass(LM_FOCUS);
    if (item) {
      $(item).addClass(LM_FOCUS);
      if (item.scrollIntoView) { item.scrollIntoView({ block: "nearest" }); }
    }
  }

  function lmFilter($el, text) {
    var t = (text || "").toLowerCase();
    lmCurrent($el).children(".ah-listmenu-item").each(function () {
      var label = $(this).children(".ah-listmenu-item-label").text().toLowerCase();
      $(this).toggle(!t || label.indexOf(t) !== -1);
    });
  }

  function lmGo($el, pageId, dir, focus) {
    var st = lmState($el[0]);
    if (st.busy) { return; }
    var $old = lmCurrent($el);
    var $new = lmPage($el, pageId === null ? "root" : pageId);
    if (!$new.length) { return; }
    var label = dir > 0 ? lmItemLabel($el, pageId) : lmItemLabel($el, st.stack[st.stack.length - 1]);
    var id = dir > 0 ? pageId : st.stack[st.stack.length - 1];
    if (dir > 0) { st.stack.push(String(pageId)); } else { st.stack.pop(); }
    lmHeader($el);
    var $input = $el.find(".ah-listmenu-filter-input");
    if ($input.val()) { $input.val(""); $old.children().show(); }
    st.busy = true;
    animateSwap($old[0], $new[0], $el.attr("data-ah-animation") || "slide", dir, function () {
      st.busy = false;
      lmFocus($el, focus ? lmItems($new).get(0) : null);
      $el.trigger("ah:navigate", [{ id: id, label: label, page: $new.attr("data-page-id") }]);
    });
  }

  function lmBack($el, focus) {
    var st = lmState($el[0]);
    if (!st.stack.length) { return; }
    lmGo($el, st.stack.length > 1 ? st.stack[st.stack.length - 2] : null, -1, focus);
  }

  function lmMark($el, key) {
    $el.find(".ah-listmenu-item-selected").removeClass("ah-listmenu-item-selected")
      .attr("aria-checked", "false");
    byKey($el, ".ah-listmenu-item:not([aria-haspopup])", "data-key", key)
      .addClass("ah-listmenu-item-selected").attr("aria-checked", "true");
  }

  function lmActivate($el, item, focus) {
    var $i = $(item);
    if ($i.hasClass("ah-listmenu-item-disabled")) { return; }
    if ($i.attr("aria-haspopup")) {
      lmGo($el, $i.attr("data-item-id"), 1, focus);
      return;
    }
    var href = $i.attr("data-href");
    if (href) {
      window.location.href = href;
      return;
    }
    var key = $i.attr("data-key");
    lmMark($el, key);
    if ($el.attr("data-ah-value") !== key) { setValue($el, key, "change"); }
  }

  AH.define("listmenu", {
    init: function (el, $el) {
      lmState(el);
      $el.on("click" + NS, ".ah-listmenu-item", function () {
        lmFocus($el, null);
        lmActivate($el, this, false);
      });
      $el.on("click" + NS, ".ah-listmenu-back", function () { lmBack($el, false); });
      $el.on("input" + NS, ".ah-listmenu-filter-input", function (e) {
        e.stopPropagation();
        lmFilter($el, this.value);
      });
      // the filter's own change must not look like a new value
      $el.on("change" + NS, ".ah-listmenu-filter-input", function (e) { e.stopPropagation(); });
      $el.on("keydown" + NS, function (e) {
        var inFilter = $(e.target).hasClass("ah-listmenu-filter-input");
        var $page = lmCurrent($el);
        var items = lmItems($page).get();
        var cur = items.indexOf($el.find("." + LM_FOCUS)[0]);
        switch (e.key) {
          case "ArrowDown":
            e.preventDefault();
            lmFocus($el, items[Math.min(items.length - 1, cur + 1)]);
            break;
          case "ArrowUp":
            e.preventDefault();
            lmFocus($el, items[Math.max(0, cur - 1)]);
            break;
          case "Home":
          case "End":
            if (inFilter) { return; }
            e.preventDefault();
            lmFocus($el, items[e.key === "Home" ? 0 : items.length - 1]);
            break;
          case "Enter":
          case " ":
          case "ArrowRight":
            if (inFilter && e.key !== "Enter") { return; }
            if (cur < 0) { return; }
            e.preventDefault();
            lmActivate($el, items[cur], true);
            break;
          case "ArrowLeft":
          case "Backspace":
          case "Escape":
            if (inFilter && e.key !== "Escape") { return; }
            if (!lmState(el).stack.length) { return; }
            e.preventDefault();
            lmBack($el, true);
            break;
          default:
            break;
        }
      });
      $el.on("focus" + NS, function () {
        if (!$el.find("." + LM_FOCUS).length) {
          var $sel = lmCurrent($el).children(".ah-listmenu-item-selected");
          lmFocus($el, $sel[0] || lmItems(lmCurrent($el)).get(0));
        }
      });
      $el.on("blur" + NS, function () { lmFocus($el, null); });
    },
    destroy: function (el) {
      $.removeData(el, "ah-listmenu");
    },
    methods: {
      setValue: function (el, $el, key) { lmMark($el, key); setValue($el, key); },
      back: function (el, $el) { lmBack($el, false); },
      navigate: function (el, $el, key) {
        var $i = byKey(lmCurrent($el), ".ah-listmenu-item[aria-haspopup]", "data-key", key);
        if ($i.length) { lmGo($el, $i.attr("data-item-id"), 1, false); }
      },
      filter: function (el, $el, text) {
        $el.find(".ah-listmenu-filter-input").val(text || "");
        lmFilter($el, text);
      },
      currentPage: function (el, $el) { return lmCurrent($el).attr("data-page-id"); }
    }
  });

  // ------------------------------------------------------------------
  // status_bar: the count details float above their segment
  // ------------------------------------------------------------------

  AH.define("status-bar", {
    init: function (el, $el) {
      var show = function () {
        var pop = $(this).children(".ah-status-bar__popover")[0];
        if (!pop) { return; }
        clearTimeout($.data(pop, "ah-hide"));
        if (!$.data(pop, "ah-float")) {
          $.data(pop, "ah-float", AH.float(pop, this, { placement: "top", offset: 8 }));
        }
      };
      var hide = function () {
        var pop = $(this).children(".ah-status-bar__popover")[0];
        if (!pop || this.matches(":hover") || this.contains(document.activeElement)) { return; }
        // keep it in place while the CSS fade-out runs
        $.data(pop, "ah-hide", setTimeout(function () {
          var h = $.data(pop, "ah-float");
          if (h) { h.stop(); $.removeData(pop, "ah-float"); }
        }, 200));
      };
      $el.on("mouseenter" + NS + " focusin" + NS, ".ah-status-bar__count", show);
      $el.on("mouseleave" + NS + " focusout" + NS, ".ah-status-bar__count", hide);
    },
    destroy: function (el, $el) {
      $el.find(".ah-status-bar__popover").each(function () {
        clearTimeout($.data(this, "ah-hide"));
        var h = $.data(this, "ah-float");
        if (h) { h.stop(); }
      });
    }
  });
})(window.jQuery, window.AH);

/* ---- components/layout_scroll.js ---- */
/* Behaviours of the layout_scroll components (designs/04-components.md).
 *
 * Ported from sigil's scrollview, scrollbar and responsive-panel (cljs +
 * jQuery). The scrollview and the standalone scrollbar keep their value in
 * data-ah-value on the root, mirror it into a hidden input when there is
 * one, and fire "change" on the root when the user changes it. Methods
 * called by the server (AH.invoke / aihtml_action:call) update the value
 * without firing "change".
 *
 * Drags use pointer capture, so nothing is bound on document except the
 * responsive panel's click-outside listener, which destroy removes.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  function setValue(el, $el, v) {
    el.setAttribute("data-ah-value", v);
    $el.children("input[type=hidden]").val(v);
  }

  function num(el, name, dflt) {
    var v = parseFloat(el.getAttribute(name));
    return isNaN(v) ? dflt : v;
  }

  function capture(node, e) {
    var id = e.originalEvent && e.originalEvent.pointerId;
    if (id !== undefined && node.setPointerCapture) {
      try { node.setPointerCapture(id); } catch (err) { /* synthetic event */ }
    }
  }

  // ------------------------------------------------------------------
  // ScrollView: a horizontal pager (sigil layout/scrollview)
  // ------------------------------------------------------------------
  //
  // Pages are 100% wide and the wrapper moves by margin-left in percent,
  // so the layout follows the container without measuring; drags move it
  // in pixels and the page change animates back to a percentage.

  var SV = "ah-scrollview";
  var SV_DEAD_ZONE = 15;

  function svState(el) {
    var st = $.data(el, "ahScrollview");
    if (!st) {
      st = { timer: null, anim: null, paused: false };
      $.data(el, "ahScrollview", st);
    }
    return st;
  }

  function svWrapper($el) { return $el.children("." + SV + "-wrapper"); }
  function svPages($el) { return svWrapper($el).children("." + SV + "-page"); }
  function svIndex(el) { return parseInt(el.getAttribute("data-ah-value"), 10) || 0; }
  function svDisabled($el) { return $el.hasClass(SV + "-disabled"); }

  function svDuration(el) { return num(el, "data-duration", 300); }

  // Turn the transition on for one move and off again when it ends.
  function svAnimate(el, $el) {
    var st = svState(el);
    $el.addClass(SV + "-animating");
    clearTimeout(st.anim);
    st.anim = setTimeout(function () {
      st.anim = null;
      $el.removeClass(SV + "-animating");
    }, svDuration(el) + 50);
  }

  function svPlace($el, idx) {
    svWrapper($el)[0].style.marginLeft = idx ? (-idx * 100) + "%" : "";
  }

  function svMark($el, idx) {
    svPages($el).each(function (i) {
      var on = i === idx;
      if (on) {
        this.removeAttribute("aria-hidden");
        this.removeAttribute("inert");
      } else {
        this.setAttribute("aria-hidden", "true");
        this.setAttribute("inert", "");
      }
    });
    $el.children("." + SV + "-buttons").children("." + SV + "-button").each(function (i) {
      var on = i === idx;
      $(this).toggleClass(SV + "-button-active", on);
      if (on) { this.setAttribute("aria-current", "true"); } else { this.removeAttribute("aria-current"); }
    });
  }

  // how: "user" fires change, "api" and "auto" only ah:page-changed.
  function svGo(el, $el, idx, how) {
    var cnt = svPages($el).length;
    if (!cnt) { return; }
    idx = Math.max(0, Math.min(cnt - 1, idx));
    var old = svIndex(el);
    svAnimate(el, $el);
    svPlace($el, idx);
    if (idx === old) { return; }
    svMark($el, idx);
    setValue(el, $el, String(idx));
    $el.trigger("ah:page-changed", [{ page: idx, old: old }]);
    if (how === "user") { $el.trigger("change"); }
  }

  function svStart(el, $el) {
    var st = svState(el);
    if (st.timer) { return; }
    st.timer = setInterval(function () {
      if (st.paused || svDisabled($el)) { return; }
      var cnt = svPages($el).length, cur = svIndex(el);
      svGo(el, $el, cur + 1 >= cnt ? 0 : cur + 1, "auto");
    }, num(el, "data-slide-duration", 3000));
  }

  function svStop(el) {
    var st = svState(el);
    clearInterval(st.timer);
    st.timer = null;
  }

  AH.define("scrollview", {
    init: function (el, $el) {
      var st = svState(el);
      var $w = svWrapper($el);
      var drag = null;
      svMark($el, svIndex(el));

      $w.on("pointerdown" + NS, function (e) {
        if (svDisabled($el) || (e.button !== undefined && e.button !== 0)) { return; }
        $el.removeClass(SV + "-animating");
        drag = { x: e.clientX, ml: parseFloat(getComputedStyle(this).marginLeft) || 0,
                 moving: false, node: this, e: e };
      });
      $w.on("pointermove" + NS, function (e) {
        if (!drag) { return; }
        var dx = e.clientX - drag.x;
        if (!drag.moving) {
          if (Math.abs(dx) <= SV_DEAD_ZONE) { return; }
          drag.moving = true;
          capture(drag.node, drag.e);
          $el.addClass(SV + "-dragging");
        }
        e.preventDefault();
        var cw = $el.width(), cnt = svPages($el).length;
        var ml = drag.ml + dx;
        if (el.getAttribute("data-bounce") === "false") {
          ml = Math.max(-(cnt - 1) * cw, Math.min(0, ml));
        }
        this.style.marginLeft = ml + "px";
      });
      $w.on("pointerup" + NS + " pointercancel" + NS, function () {
        if (!drag) { return; }
        var d = drag;
        drag = null;
        if (!d.moving) { return; }
        $el.removeClass(SV + "-dragging");
        // swallow the click that ends a drag, so links in a page stay put
        $w.one("click" + NS, function (c) { c.preventDefault(); c.stopPropagation(); });
        setTimeout(function () { $w.off("click" + NS); }, 0);
        var dx = (parseFloat(this.style.marginLeft) || 0) - d.ml;
        var cw = $el.width(), threshold = num(el, "data-threshold", 0.5) * cw;
        var cur = svIndex(el), target = cur;
        if (dx < -threshold) { target = cur + 1; } else if (dx > threshold) { target = cur - 1; }
        svGo(el, $el, target, "user");
      });
      $w.on("dragstart" + NS, function (e) { e.preventDefault(); });

      $el.on("click" + NS, "." + SV + "-button", function () {
        if ($(this).closest("." + SV)[0] !== el || svDisabled($el)) { return; }
        svGo(el, $el, $(this).index(), "user");
      });

      $el.on("keydown" + NS, function (e) {
        if (e.target !== el || svDisabled($el)) { return; }
        var cur = svIndex(el), cnt = svPages($el).length, to = null;
        switch (e.key) {
          case "ArrowLeft": case "ArrowUp": case "PageUp": to = cur - 1; break;
          case "ArrowRight": case "ArrowDown": case "PageDown": to = cur + 1; break;
          case "Home": to = 0; break;
          case "End": to = cnt - 1; break;
          default: return;
        }
        e.preventDefault();
        svGo(el, $el, to, "user");
      });

      // the slide show waits while the pointer or the focus is inside
      $el.on("mouseenter" + NS + " focusin" + NS, function () { st.paused = true; });
      $el.on("mouseleave" + NS + " focusout" + NS, function (e) {
        if (e.type === "focusout" && e.relatedTarget && $.contains(el, e.relatedTarget)) { return; }
        st.paused = false;
      });
      if (el.hasAttribute("data-slide-show")) { svStart(el, $el); }
    },
    destroy: function (el) {
      var st = svState(el);
      svStop(el);
      clearTimeout(st.anim);
      $.removeData(el, "ahScrollview");
    },
    methods: {
      setValue: function (el, $el, v) { svGo(el, $el, parseInt(v, 10) || 0, "api"); },
      getValue: function (el) { return svIndex(el); },
      forward: function (el, $el) {
        var cur = svIndex(el);
        if (cur + 1 < svPages($el).length) { svGo(el, $el, cur + 1, "api"); }
      },
      back: function (el, $el) {
        var cur = svIndex(el);
        if (cur > 0) { svGo(el, $el, cur - 1, "api"); }
      },
      startSlideShow: function (el, $el) { svStart(el, $el); },
      stopSlideShow: function (el) { svStop(el); },
      refresh: function (el, $el) {
        var cnt = svPages($el).length, cur = Math.min(svIndex(el), Math.max(0, cnt - 1));
        setValue(el, $el, String(cur));
        svPlace($el, cur);
        svMark($el, cur);
      }
    }
  });

  // ------------------------------------------------------------------
  // Scrollbar (sigil layout/scrollbar, and the bars of sigil's panel)
  // ------------------------------------------------------------------
  //
  // One bar is .ah-scrollbar with five parts: up button, track before the
  // thumb, thumb, track after it, down button. sbLayout sizes them from a
  // value in [min, max] exactly as sigil's arrange! does.

  var SB = "ah-scrollbar";
  var SB_BTN = 14;

  function sbParts(bar) {
    var $b = $(bar);
    return {
      up: $b.children("." + SB + "-btn-up")[0],
      tu: $b.children("." + SB + "-track-up")[0],
      thumb: $b.children("." + SB + "-thumb")[0],
      td: $b.children("." + SB + "-track-down")[0],
      down: $b.children("." + SB + "-btn-down")[0]
    };
  }

  function sbVertical(bar) { return $(bar).hasClass(SB + "-vertical"); }

  // Geometry of a bar for a value: track length, thumb size and position.
  function sbGeom(bar, o) {
    var vert = sbVertical(bar);
    var total = vert ? bar.clientHeight : bar.clientWidth;
    var track = Math.max(0, total - (o.buttons ? 2 * SB_BTN : 0));
    var range = o.max - o.min;
    var ts = range <= 0 ? track : Math.max(o.thumbMin, track * track / (track + range));
    ts = Math.min(ts, track);
    var tp = range <= 0 ? 0 : (o.value - o.min) / range * (track - ts);
    return { vert: vert, track: track, ts: ts, tp: tp };
  }

  function sbLayout(bar, o) {
    var g = sbGeom(bar, o), p = sbParts(bar), dim = g.vert ? "height" : "width";
    p.up.style.display = p.down.style.display = o.buttons ? "" : "none";
    p.tu.style[dim] = g.tp + "px";
    p.thumb.style[dim] = g.ts + "px";
    p.td.style[dim] = Math.max(0, g.track - g.ts - g.tp) + "px";
    return g;
  }

  function sbPosToValue(bar, o, pos) {
    var g = sbGeom(bar, o), free = g.track - g.ts;
    return free <= 0 ? o.min : o.min + Math.max(0, Math.min(free, pos)) / free * (o.max - o.min);
  }

  // Repeat fn while a button is held: once now, again after 300ms, then
  // every 50ms (sigil util/start-repeat-timer!).
  function sbRepeat(node, e, fn) {
    fn();
    var iv = null;
    var t = setTimeout(function () { iv = setInterval(fn, 50); }, 300);
    capture(node, e);
    $(node).on("pointerup.ahrep pointercancel.ahrep lostpointercapture.ahrep", function () {
      clearTimeout(t);
      clearInterval(iv);
      $(node).off(".ahrep");
    });
  }

  // Wire one bar. api: {opts(), set(value, phase), start()} where phase is
  // "input" while dragging, "change" otherwise and "end" when a drag ends.
  function sbBind(bar, api) {
    var p = sbParts(bar);
    $(p.up).add(p.down).on("pointerdown" + NS, function (e) {
      if (e.button !== undefined && e.button !== 0) { return; }
      e.preventDefault();
      var dir = this === p.up ? -1 : 1;
      sbRepeat(this, e, function () { var o = api.opts(); api.set(o.value + dir * o.step, "change"); });
    });
    $(p.tu).add(p.td).on("pointerdown" + NS, function (e) {
      if (e.button !== undefined && e.button !== 0) { return; }
      e.preventDefault();
      var o = api.opts();
      api.set(o.value + (this === p.tu ? -1 : 1) * o.large, "change");
    });
    var drag = null;
    $(p.thumb).on("pointerdown" + NS, function (e) {
      if (e.button !== undefined && e.button !== 0) { return; }
      e.preventDefault();
      var o = api.opts(), g = sbGeom(bar, o);
      drag = { start: g.vert ? e.clientY : e.clientX, pos: g.tp, vert: g.vert, value: o.value };
      capture(this, e);
      $(this).addClass(SB + "-thumb-pressed");
      if (api.start) { api.start(); }
    });
    $(p.thumb).on("pointermove" + NS, function (e) {
      if (!drag) { return; }
      var cur = drag.vert ? e.clientY : e.clientX;
      api.set(sbPosToValue(bar, api.opts(), drag.pos + cur - drag.start), "input");
    });
    $(p.thumb).on("pointerup" + NS + " pointercancel" + NS, function () {
      if (!drag) { return; }
      var before = drag.value;
      drag = null;
      $(this).removeClass(SB + "-thumb-pressed");
      api.set(api.opts().value, "end", before);
    });
  }

  function sbObserve(el, targets, fn) {
    var st = $.data(el, "ahScrollbar");
    if (window.ResizeObserver) {
      st.ro = new ResizeObserver(function () { fn(); });
      targets.forEach(function (t) { if (t) { st.ro.observe(t); } });
    } else {
      st.ns = ".ahsb" + (++seq);
      $(window).on("resize" + st.ns, fn);
    }
  }

  // --- standalone bar ---------------------------------------------------

  function sbOpts(el) {
    return {
      min: num(el, "data-min", 0), max: num(el, "data-max", 1000),
      value: num(el, "data-ah-value", 0),
      step: num(el, "data-step", 10), large: num(el, "data-large-step", 50),
      thumbMin: num(el, "data-thumb-min", 10),
      buttons: el.getAttribute("data-buttons") !== "false"
    };
  }

  function sbIntegral(o) {
    return o.min % 1 === 0 && o.max % 1 === 0 && o.step % 1 === 0 && o.large % 1 === 0;
  }

  function sbBar($el) { return $el.children("." + SB)[0]; }

  function sbSet(el, $el, v, event) {
    var o = sbOpts(el);
    v = Math.max(o.min, Math.min(o.max, +v || 0));
    if (sbIntegral(o)) { v = Math.round(v); }
    var changed = v !== o.value;
    if (changed) {
      setValue(el, $el, String(v));
      el.setAttribute("aria-valuenow", String(v));
    }
    o.value = v;
    sbLayout(sbBar($el), o);
    if (changed && event) { $el.trigger(event); }
    return changed;
  }

  function sbStandalone(el, $el) {
    var bar = sbBar($el);
    var disabled = function () { return $el.hasClass(SB + "-disabled"); };
    sbBind(bar, {
      opts: function () { return sbOpts(el); },
      set: function (v, phase, before) {
        if (disabled()) { return; }
        if (phase === "end") {
          if (String(before) !== el.getAttribute("data-ah-value")) { $el.trigger("change"); }
          return;
        }
        sbSet(el, $el, v, phase);
      },
      start: function () { el.focus({ preventScroll: true }); }
    });
    $el.on("keydown" + NS, function (e) {
      if (e.target !== el || disabled()) { return; }
      var o = sbOpts(el), vert = sbVertical(bar), to;
      switch (e.key) {
        case "ArrowLeft": if (vert) { return; } to = o.value - o.step; break;
        case "ArrowRight": if (vert) { return; } to = o.value + o.step; break;
        case "ArrowUp": if (!vert) { return; } to = o.value - o.step; break;
        case "ArrowDown": if (!vert) { return; } to = o.value + o.step; break;
        case "PageUp": to = o.value - o.large; break;
        case "PageDown": to = o.value + o.large; break;
        case "Home": to = o.min; break;
        case "End": to = o.max; break;
        default: return;
      }
      e.preventDefault();
      sbSet(el, $el, to, "change");
    });
    var relayout = function () { sbLayout(bar, sbOpts(el)); };
    sbObserve(el, [el], relayout);
    relayout();
  }

  // --- scroll area --------------------------------------------------------

  function saParts($el) {
    return {
      vp: $el.children("." + SB + "-viewport")[0],
      v: $el.children("." + SB + "-vertical")[0],
      h: $el.children("." + SB + "-horizontal")[0]
    };
  }

  function saOpts(el, vp, vert) {
    var max = vert ? vp.scrollHeight - vp.clientHeight : vp.scrollWidth - vp.clientWidth;
    var page = vert ? vp.clientHeight : vp.clientWidth;
    return {
      min: 0, max: Math.max(0, max),
      value: vert ? vp.scrollTop : vp.scrollLeft,
      step: num(el, "data-step", 10) * 3, large: Math.max(10, page * 0.9),
      thumbMin: num(el, "data-thumb-min", 10),
      buttons: el.getAttribute("data-buttons") !== "false"
    };
  }

  // Which bars the content needs: showing one narrows the viewport, which
  // may make the other one necessary, so settle it in two passes.
  function saLayout(el, $el) {
    var p = saParts($el), vp = p.vp;
    var needV = false, needH = false;
    for (var i = 0; i < 2; i++) {
      $el.toggleClass(SB + "-area-v", needV).toggleClass(SB + "-area-h", needH);
      needV = vp.scrollHeight > vp.clientHeight + 1;
      needH = vp.scrollWidth > vp.clientWidth + 1;
    }
    $el.toggleClass(SB + "-area-v", needV).toggleClass(SB + "-area-h", needH);
    if (needV) { sbLayout(p.v, saOpts(el, vp, true)); }
    if (needH) { sbLayout(p.h, saOpts(el, vp, false)); }
  }

  function saSync(el, $el) {
    var p = saParts($el);
    if ($el.hasClass(SB + "-area-v")) { sbLayout(p.v, saOpts(el, p.vp, true)); }
    if ($el.hasClass(SB + "-area-h")) { sbLayout(p.h, saOpts(el, p.vp, false)); }
  }

  function saArea(el, $el) {
    var p = saParts($el);
    [[p.v, true], [p.h, false]].forEach(function (b) {
      var vert = b[1];
      sbBind(b[0], {
        opts: function () { return saOpts(el, p.vp, vert); },
        set: function (v, phase) {
          if (phase === "end" || $el.hasClass(SB + "-disabled")) { return; }
          if (vert) { p.vp.scrollTop = v; } else { p.vp.scrollLeft = v; }
          saSync(el, $el);
        }
      });
    });
    $(p.vp).on("scroll" + NS, function () { saSync(el, $el); });
    var content = $(p.vp).children("." + SB + "-content")[0];
    sbObserve(el, [p.vp, content], function () { saLayout(el, $el); });
    saLayout(el, $el);
  }

  AH.define("scrollbar", {
    init: function (el, $el) {
      $.data(el, "ahScrollbar", {});
      if (el.hasAttribute("data-area")) { saArea(el, $el); } else { sbStandalone(el, $el); }
    },
    destroy: function (el) {
      var st = $.data(el, "ahScrollbar") || {};
      if (st.ro) { st.ro.disconnect(); }
      if (st.ns) { $(window).off(st.ns); }
      $.removeData(el, "ahScrollbar");
    },
    methods: {
      setValue: function (el, $el, v) {
        if (!el.hasAttribute("data-area")) { sbSet(el, $el, v, null); }
      },
      getValue: function (el) { return num(el, "data-ah-value", 0); },
      setMax: function (el, $el, max) {
        el.setAttribute("data-max", String(max));
        el.setAttribute("aria-valuemax", String(max));
        sbSet(el, $el, num(el, "data-ah-value", 0), null);
        sbLayout(sbBar($el), sbOpts(el));
      },
      scrollTo: function (el, $el, x, y) {
        var vp = saParts($el).vp;
        if (vp) {
          vp.scrollLeft = x || 0;
          vp.scrollTop = y || 0;
        }
      },
      refresh: function (el, $el) {
        if (el.hasAttribute("data-area")) { saLayout(el, $el); } else { sbLayout(sbBar($el), sbOpts(el)); }
      }
    }
  });

  // ------------------------------------------------------------------
  // Responsive panel (sigil layout/responsive_panel)
  // ------------------------------------------------------------------
  //
  // Folded when the parent is at most data-breakpoint px wide: the content
  // then floats below the toggle (AH.float) while open.

  var RP = "ah-responsive-panel";

  function rpState(el) { return $.data(el, "ahRpanel"); }
  function rpToggle($el) { return $el.children("." + RP + "-toggle"); }
  function rpContent($el) { return $el.children("." + RP + "-content"); }
  function rpDisabled($el) { return $el.hasClass(RP + "-disabled"); }

  function rpLoad($el) {
    var st = rpState($el[0]), $c = rpContent($el);
    if (st.loaded) { return; }
    st.loaded = true;
    if (/(^|\s)ah:load:/.test($c.attr("data-ah-on") || "")) { $c.trigger("ah:load"); }
  }

  function rpSpeed(el, name) { return num(el, name, 200); }

  function rpClearStyles($c) {
    $c.stop(true, true).css({ display: "", opacity: "", width: "" });
  }

  function rpOpen(el, $el) {
    var st = rpState(el);
    if (!st.collapsed || st.open || rpDisabled($el)) { return; }
    var $c = rpContent($el), $t = rpToggle($el);
    var anim = el.getAttribute("data-animation") || "fade", speed = rpSpeed(el, "data-show-duration");
    var cw = el.getAttribute("data-collapse-width");
    rpClearStyles($c);
    if (cw) { $c.css("width", /^\d+(\.\d+)?$/.test(cw) ? cw + "px" : cw); }
    st.open = true;
    $el.addClass(RP + "-open");
    $t.attr("aria-expanded", "true");
    st.float = AH.float($c[0], $t[0], { placement: "bottom", align: "start", offset: 4 });
    var shown = function () {
      if (st.float) { st.float.update(); }
      $el.trigger("ah:open");
    };
    if (anim === "fade") {
      $c.css("opacity", 0).animate({ opacity: 1 }, speed, shown);
    } else if (anim === "slide") {
      $c.hide().slideDown(speed, shown);
    } else {
      shown();
    }
    rpLoad($el);
  }

  function rpClose(el, $el, instant) {
    var st = rpState(el);
    if (!st.open) { return; }
    var $c = rpContent($el), $t = rpToggle($el);
    var anim = instant ? "none" : (el.getAttribute("data-animation") || "fade");
    var speed = rpSpeed(el, "data-hide-duration");
    st.open = false;
    $t.attr("aria-expanded", "false");
    if ($.contains($c[0], document.activeElement)) { $t[0].focus(); }
    var hidden = function () {
      $el.removeClass(RP + "-open");
      if (st.float) { st.float.stop(); st.float = null; }
      $c.css({ display: "", opacity: "" });
      if (!instant) { $el.trigger("ah:close"); }
    };
    $c.stop(true, true);
    if (anim === "fade") { $c.fadeOut(speed, hidden); }
    else if (anim === "slide") { $c.slideUp(speed, hidden); }
    else { hidden(); }
  }

  function rpCheck(el, $el) {
    var st = rpState(el);
    var bp = num(el, "data-breakpoint", 1000);
    var pw = $el.parent().width();
    if (!st.collapsed && pw <= bp) {
      if (st.open) { rpClose(el, $el, true); }
      st.collapsed = true;
      $el.addClass(RP + "-collapsed");
      $el.trigger("ah:collapse");
    } else if (st.collapsed && pw > bp) {
      rpClose(el, $el, true);
      st.collapsed = false;
      $el.removeClass(RP + "-collapsed " + RP + "-open");
      rpClearStyles(rpContent($el));
      $el.trigger("ah:expand");
      rpLoad($el);
    }
  }

  function rpFlip(el, $el) {
    if (rpState(el).open) { rpClose(el, $el); } else { rpOpen(el, $el); }
  }

  AH.define("responsive-panel", {
    init: function (el, $el) {
      var st = { collapsed: false, open: false, loaded: false, float: null,
                 ns: ".ahrp" + (++seq), ro: null, $ext: $() };
      $.data(el, "ahRpanel", st);
      var $t = rpToggle($el);
      $t.on("click" + NS, function () {
        if (!rpDisabled($el)) { rpFlip(el, $el); }
      });
      $t.on("keydown" + NS, function (e) {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          if (!rpDisabled($el)) { rpFlip(el, $el); }
        }
      });
      $el.on("keydown" + NS, function (e) {
        if (e.key === "Escape" && st.open) {
          e.stopPropagation();
          rpClose(el, $el);
          $t[0].focus();
        }
      });
      var sel = el.getAttribute("data-toggle-button");
      if (sel) {
        st.$ext = $(sel).on("click" + st.ns, function () {
          if (!rpDisabled($el)) { rpFlip(el, $el); }
        });
      }
      $(document).on("click" + st.ns, function (e) {
        if (st.open && el.getAttribute("data-auto-close") !== "false" &&
            !$.contains(el, e.target) && e.target !== el &&
            !st.$ext.filter(function () { return this === e.target || $.contains(this, e.target); }).length) {
          rpClose(el, $el);
        }
      });
      var check = function () { rpCheck(el, $el); };
      if (window.ResizeObserver && el.parentNode) {
        st.ro = new ResizeObserver(check);
        st.ro.observe(el.parentNode);
      }
      $(window).on("resize" + st.ns, check);
      check();
      if (!st.collapsed) { rpLoad($el); }
    },
    destroy: function (el, $el) {
      var st = rpState(el);
      if (!st) { return; }
      if (st.float) { st.float.stop(); }
      if (st.ro) { st.ro.disconnect(); }
      rpContent($el).stop(true, true);
      $(document).off(st.ns);
      $(window).off(st.ns);
      st.$ext.off(st.ns);
      $.removeData(el, "ahRpanel");
    },
    methods: {
      open: function (el, $el) { rpOpen(el, $el); },
      close: function (el, $el) { rpClose(el, $el); },
      toggle: function (el, $el) { if (rpState(el).collapsed) { rpFlip(el, $el); } },
      refresh: function (el, $el) { rpCheck(el, $el); },
      isCollapsed: function (el) { return !!rpState(el).collapsed; },
      isOpen: function (el) { return !!rpState(el).open; }
    }
  });
})(window.jQuery, window.AH);

/* ---- components/overlay.js ---- */
/* Behaviours of the overlay components (designs/04-components.md):
 * tooltip, popover, drawer, sheet, window, notification, plus the page
 * functions AH.fn("toast") and AH.fn("notify").
 *
 * Declarative triggers (aihtml_overlay:opens/toggles/closes) are one
 * delegated click listener: data-ah-open / data-ah-toggle / data-ah-close
 * hold a selector; an empty data-ah-close closes the enclosing overlay.
 *
 * Shared machinery, ported from sigil's internal/common and scroll_lock:
 *   - one z-index counter, so the overlay opened last is on top
 *   - one scroll-lock counter (body.ah-scroll-locked)
 *   - one stack of open overlays: Escape closes the top one, Tab is
 *     trapped in the top one when it is modal, focus returns to the
 *     element that had it when the overlay closes
 *
 * Events on the component root: ah:open, ah:close [{result}], and for
 * window ah:collapse, ah:expand, ah:moved, ah:resize; notification cards
 * fire ah:click. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var GNS = ".ah-overlay";          // document-level listeners of this file
  var FOCUSABLE = "a[href], area[href], button:not([disabled]), " +
    "input:not([disabled]):not([type='hidden']), select:not([disabled]), " +
    "textarea:not([disabled]), iframe, [tabindex]:not([tabindex='-1']), " +
    "[contenteditable='true']";
  var OVERLAYS = '[data-ah="drawer"],[data-ah="sheet"],[data-ah="window"],' +
    '[data-ah="popover"],[data-ah="tooltip"],[data-ah="notification"]';
  var seq = 0;

  function uid(prefix) { return prefix + (++seq); }

  // data-ah-<name>: "false" is false, absent is the default
  function flag(el, name, dflt) {
    var v = el.getAttribute("data-ah-" + name);
    if (v === null || v === "") { return dflt; }
    return v !== "false";
  }

  function num(el, name, dflt) {
    var v = parseFloat(el.getAttribute("data-ah-" + name));
    return isNaN(v) ? dflt : v;
  }

  // ------------------------------------------------------------------
  // z-index, scroll lock, stack of open overlays
  // ------------------------------------------------------------------

  // Above sigil's fixed drawer (20600) and sheet (20500) layers, below the
  // notification corner (99999).
  var z = 21000;
  function nextZ() { return ++z; }

  var locks = 0;
  function lock() {
    if (locks++ === 0) { $(document.body).addClass("ah-scroll-locked"); }
  }
  function unlock() {
    locks = Math.max(0, locks - 1);
    if (locks === 0) { $(document.body).removeClass("ah-scroll-locked"); }
  }

  // {el, trap: element to keep Tab in or null, esc: function -> handled?}
  var stack = [];

  function pushStack(entry) {
    pullStack(entry.el, false);
    entry.returnTo = document.activeElement;
    stack.push(entry);
  }

  function pullStack(el, restore) {
    var kept = [];
    var gone = null;
    for (var i = 0; i < stack.length; i++) {
      if (stack[i].el === el) { gone = stack[i]; } else { kept.push(stack[i]); }
    }
    stack = kept;
    if (restore && gone && gone.returnTo && document.contains(gone.returnTo)) {
      var active = document.activeElement;
      if (!active || active === document.body || $.contains(el, active) || active === el) {
        try { gone.returnTo.focus({ preventScroll: true }); } catch (e) { /* not focusable */ }
      }
    }
  }

  function topOfStack() { return stack[stack.length - 1]; }

  function focusables(container) {
    return $(container).find(FOCUSABLE).filter(function () {
      return this.offsetWidth || this.offsetHeight || this.getClientRects().length;
    });
  }

  function trapTab(e, container) {
    var $f = focusables(container);
    var active = document.activeElement;
    if (!$f.length) {
      e.preventDefault();
      container.focus();
      return;
    }
    var first = $f[0];
    var last = $f[$f.length - 1];
    if (!$.contains(container, active) && active !== container) {
      e.preventDefault();
      first.focus();
    } else if (e.shiftKey && (active === first || active === container)) {
      e.preventDefault();
      last.focus();
    } else if (!e.shiftKey && active === last) {
      e.preventDefault();
      first.focus();
    }
  }

  $(document).on("keydown" + GNS, function (e) {
    if (e.key === "Escape") {
      if (closeTips()) { return; }
      var t = topOfStack();
      if (t && t.esc(e)) { e.preventDefault(); }
    } else if (e.key === "Tab") {
      var top = topOfStack();
      if (top && top.trap) { trapTab(e, top.trap); }
    }
  });

  // ------------------------------------------------------------------
  // Declarative triggers
  // ------------------------------------------------------------------

  $(document).on("click" + GNS, "[data-ah-open],[data-ah-toggle],[data-ah-close]", function (e) {
    var trigger = this;
    if (trigger.tagName === "A") { e.preventDefault(); }
    var sel;
    if ((sel = trigger.getAttribute("data-ah-open"))) {
      AH.invoke($(sel), "open", { invoker: trigger });
    }
    if ((sel = trigger.getAttribute("data-ah-toggle"))) {
      AH.invoke($(sel), "toggle", { invoker: trigger });
    }
    if (trigger.hasAttribute("data-ah-close")) {
      sel = trigger.getAttribute("data-ah-close");
      var $t = sel ? $(sel) : $(trigger).closest(OVERLAYS);
      if ($t.length) {
        AH.invoke($t, "close", trigger.getAttribute("data-ah-result"));
      }
    }
  });

  // Enter / Space on non-button close controls (popover title bar).
  $(document).on("keydown" + GNS, "[data-ah-close][role='button']", function (e) {
    if (e.key === "Enter" || e.key === " ") {
      e.preventDefault();
      $(this).trigger("click");
    }
  });

  // ------------------------------------------------------------------
  // Positioning: AH.float (core.js) places the bubble with position:
  // fixed, flips it and follows scrolling; the side it used lands in
  // data-ah-placement, which is mirrored into sigil's arrow class
  // (<prefix><side>) whenever it changes.
  // ------------------------------------------------------------------

  function floatWithArrow(el, anchor, side, offset, prefix, sideClasses) {
    function sync() {
      var used = el.getAttribute("data-ah-placement");
      if (used) { $(el).removeClass(sideClasses).addClass(prefix + used); }
    }
    var h = AH.float(el, anchor, { placement: side, align: "center", offset: offset });
    sync();
    var mo = window.MutationObserver ? new MutationObserver(sync) : null;
    if (mo) { mo.observe(el, { attributes: true, attributeFilter: ["data-ah-placement"] }); }
    return {
      update: function () { h.update(); },
      stop: function () {
        if (mo) { mo.disconnect(); }
        h.stop();
      }
    };
  }

  // ------------------------------------------------------------------
  // Tooltip: [data-ah="tooltip"] wrappers and [data-ah-tooltip] elements
  // ------------------------------------------------------------------

  var TIP_HOSTS = '[data-ah="tooltip"],[data-ah-tooltip]';
  var TIP_POSITIONS = "ah-tooltip-top ah-tooltip-bottom ah-tooltip-left ah-tooltip-right";
  var openTips = [];
  var touchOnly = !!(window.matchMedia && window.matchMedia("(hover: none)").matches);

  function tipOpt(host, name, dflt) {
    var v = host.getAttribute("data-ah-tip-" + name);
    return v === null || v === "" ? dflt : v;
  }

  function tipState(host) {
    var st = $.data(host, "ahTip");
    if (!st) {
      st = { open: false, showTimer: null, hideTimer: null, tip: null, created: false };
      $.data(host, "ahTip", st);
    }
    return st;
  }

  function tipElement(host, st) {
    if (st.tip) { return st.tip; }
    if (host.getAttribute("data-ah") === "tooltip") {
      st.tip = $(host).children(".ah-tooltip")[0];
    } else {
      var $tip = $('<span class="ah-tooltip" role="tooltip">' +
                   '<span class="ah-tooltip-arrow" aria-hidden="true"></span>' +
                   '<span class="ah-tooltip-content"></span></span>');
      $tip.find(".ah-tooltip-content").text(host.getAttribute("data-ah-tooltip"));
      if (tipOpt(host, "arrow", "true") === "false") { $tip.addClass("ah-tooltip-no-arrow"); }
      $tip.appendTo(document.body);
      st.tip = $tip[0];
      st.created = true;
    }
    if (st.tip && !st.tip.id) { st.tip.id = uid("ah-tip-"); }
    return st.tip;
  }

  function positionTip(host, tip, e) {
    var pos = tipOpt(host, "position", "bottom");
    unfloatTip(tipState(host));
    tip.style.position = "fixed";
    if (pos === "mouse") {
      $(tip).addClass("ah-tooltip-no-arrow");
      var x = e && e.clientX !== undefined ? e.clientX : host.getBoundingClientRect().left;
      var y = e && e.clientY !== undefined ? e.clientY : host.getBoundingClientRect().bottom;
      tip.style.left = (x + 10) + "px";
      tip.style.top = (y + 10) + "px";
      return;
    }
    // the tooltip's arrow is 6px (tooltip.css)
    var anchor = host.getAttribute("data-ah") === "tooltip"
      ? ($(host).children().not(".ah-tooltip")[0] || host) : host;
    tipState(host).float = floatWithArrow(tip, anchor, pos, 6, "ah-tooltip-", TIP_POSITIONS);
  }

  function unfloatTip(st) {
    if (st.float) {
      st.float.stop();
      st.float = null;
    }
  }

  function openTip(host, e) {
    var st = tipState(host);
    if (st.open || tipOpt(host, "disabled", "false") === "true") { return; }
    var ev = $.Event("ah:opening");
    $(host).trigger(ev);
    if (ev.isDefaultPrevented()) { return; }
    var tip = tipElement(host, st);
    if (!tip) { return; }
    $(tip).stop(true).css({ display: "block", visibility: "hidden", opacity: 0 });
    positionTip(host, tip, e);
    $(tip).css({ visibility: "visible" }).animate({ opacity: 0.9 }, 200);
    host.setAttribute("aria-describedby", tip.id);
    st.open = true;
    openTips.push(host);
    $(host).trigger("ah:open");
    if (tipOpt(host, "auto-hide", "true") !== "false") {
      clearTimeout(st.hideTimer);
      st.hideTimer = setTimeout(function () { closeTip(host); },
                                parseInt(tipOpt(host, "hide-delay", "3000"), 10));
    }
  }

  function closeTip(host, now) {
    var st = tipState(host);
    clearTimeout(st.showTimer);
    clearTimeout(st.hideTimer);
    if (!st.open) { return; }
    st.open = false;
    openTips = openTips.filter(function (h) { return h !== host; });
    host.removeAttribute("aria-describedby");
    var tip = st.tip;
    var done = function () {
      unfloatTip(st);
      tip.style.display = "none";
      tip.style.visibility = "hidden";
      if (st.created) {
        $(tip).remove();
        st.tip = null;
      }
    };
    if (now) {
      $(tip).stop(true);
      done();
    } else {
      $(tip).stop(true).animate({ opacity: 0 }, "fast", done);
    }
    $(host).trigger("ah:close", [{ result: null }]);
  }

  function closeTips() {
    var had = openTips.length > 0;
    openTips.slice().forEach(function (h) { closeTip(h); });
    return had;
  }

  function tipTrigger(host) {
    var t = tipOpt(host, "trigger", "hover");
    return t === "hover" && touchOnly ? "click" : t;
  }

  $(document)
    .on("mouseenter" + GNS, TIP_HOSTS, function (e) {
      var host = this;
      if (tipTrigger(host) !== "hover") { return; }
      var st = tipState(host);
      clearTimeout(st.showTimer);
      var ev = { clientX: e.clientX, clientY: e.clientY };
      st.showTimer = setTimeout(function () {
        if (document.contains(host)) { openTip(host, ev); }
      }, parseInt(tipOpt(host, "delay", "100"), 10));
    })
    .on("mouseleave" + GNS, TIP_HOSTS, function () {
      if (tipTrigger(this) === "hover" && !$.contains(this, document.activeElement)) {
        closeTip(this);
      }
    })
    .on("mousemove" + GNS, TIP_HOSTS, function (e) {
      var st = $.data(this, "ahTip");
      if (st && st.open && st.tip && tipOpt(this, "position", "") === "mouse") {
        st.tip.style.left = (e.clientX + 10) + "px";
        st.tip.style.top = (e.clientY + 10) + "px";
      }
    })
    .on("focusin" + GNS, TIP_HOSTS, function () {
      if (tipTrigger(this) === "hover") { openTip(this); }
    })
    .on("focusout" + GNS, TIP_HOSTS, function (e) {
      if (tipTrigger(this) === "hover" && !$.contains(this, e.relatedTarget)) {
        closeTip(this);
      }
    })
    .on("click" + GNS, TIP_HOSTS, function (e) {
      if (tipTrigger(this) !== "click") { return; }
      if ($(e.target).closest(TIP_HOSTS)[0] !== this) { return; }
      if (tipState(this).open) { closeTip(this); } else { openTip(this, e); }
    })
    .on("click" + GNS, function (e) {
      // click-triggered tooltips close on a click elsewhere
      openTips.slice().forEach(function (host) {
        if (tipTrigger(host) === "click" && host !== e.target && !$.contains(host, e.target)) {
          closeTip(host);
        }
      });
    });

  AH.define("tooltip", {
    init: function () { /* delegated listeners above do the work */ },
    destroy: function (el) {
      closeTip(el, true);
      $.removeData(el, "ahTip");
    },
    methods: {
      open: function (el) { openTip(el); },
      close: function (el) { closeTip(el); },
      toggle: function (el) {
        if (tipState(el).open) { closeTip(el); } else { openTip(el); }
      },
      setContent: function (el, $el, text) {
        $el.find(".ah-tooltip-content").text(text);
      }
    }
  });

  // ------------------------------------------------------------------
  // Popover
  // ------------------------------------------------------------------

  var POP_POSITIONS = "ah-popover-top ah-popover-bottom ah-popover-left ah-popover-right";

  function popOpen(el) { return el.getAttribute("data-state") === "open"; }

  function popUnfloat(el) {
    var f = $.data(el, "ahFloat");
    if (f) {
      f.stop();
      $.removeData(el, "ahFloat");
    }
  }

  function popoverOpen(el, opts) {
    if (popOpen(el)) { return; }
    var anchor = (opts && opts.invoker) || $(el.getAttribute("data-ah-anchor") || null)[0];
    if (!anchor) {
      console.error("aihtml: popover has no anchor", el);
      return;
    }
    var ev = $.Event("ah:opening");
    $(el).trigger(ev);
    if (ev.isDefaultPrevented()) { return; }
    $.data(el, "ahAnchor", anchor);
    var zi = nextZ();
    var $el = $(el).stop(true, true);
    popUnfloat(el);
    $el.css({ zIndex: zi, display: "block", visibility: "hidden", opacity: 0 });
    $.data(el, "ahFloat", floatWithArrow(el, anchor, el.getAttribute("data-ah-position") || "bottom",
                                         8, "ah-popover-", POP_POSITIONS));
    if (flag(el, "modal", false)) {
      var $bd = $('<div class="ah-popover-modal-backdrop"></div>').css("z-index", zi - 1);
      $bd.insertBefore(el);
      $.data(el, "ahBackdrop", $bd[0]);
    }
    $el.css({ visibility: "visible" }).animate({ opacity: 1 }, "fast");
    el.setAttribute("data-state", "open");
    anchor.setAttribute("aria-expanded", "true");
    pushStack({ el: el, trap: null, esc: function () { popoverClose(el); return true; } });
    $el.trigger("ah:open");
  }

  function popoverClose(el, result) {
    if (!popOpen(el)) { return; }
    var anchor = $.data(el, "ahAnchor");
    el.setAttribute("data-state", "closed");
    if (anchor) { anchor.setAttribute("aria-expanded", "false"); }
    var bd = $.data(el, "ahBackdrop");
    if (bd) {
      $(bd).remove();
      $.removeData(el, "ahBackdrop");
    }
    var focusInside = $.contains(el, document.activeElement);
    pullStack(el, false);
    if (focusInside && anchor) { anchor.focus(); }
    $(el).stop(true).fadeOut("fast", function () { popUnfloat(el); });
    $(el).trigger("ah:close", [{ result: result || null }]);
  }

  AH.define("popover", {
    init: function (el) {
      var id = uid("pop");
      $.data(el, "ahNs", id);
      var anchorSel = el.getAttribute("data-ah-anchor");
      if (anchorSel) {
        // sigil's `selector' prop: the anchor toggles the popover
        $(document).on("click" + GNS + id, anchorSel, function (e) {
          if ($(this).is("[data-ah-open],[data-ah-toggle]")) { return; }
          e.preventDefault();
          AH.invoke(el, "toggle", { invoker: this });
        });
      }
      $(document).on("click" + GNS + id, function (e) {
        if (!popOpen(el) || !flag(el, "auto-close", true) || flag(el, "modal", false)) { return; }
        var anchor = $.data(el, "ahAnchor");
        var t = e.target;
        if (t === el || $.contains(el, t) || (anchor && (t === anchor || $.contains(anchor, t)))) {
          return;
        }
        popoverClose(el);
      });
    },
    destroy: function (el) {
      var id = $.data(el, "ahNs");
      $(document).off(GNS + id);
      popUnfloat(el);
      var bd = $.data(el, "ahBackdrop");
      if (bd) { $(bd).remove(); }
      pullStack(el, false);
    },
    methods: {
      open: function (el, $el, opts) { popoverOpen(el, opts); },
      close: function (el, $el, result) { popoverClose(el, result); },
      toggle: function (el, $el, opts) {
        if (popOpen(el)) { popoverClose(el); } else { popoverOpen(el, opts); }
      },
      isOpen: function (el) { return popOpen(el); }
    }
  });

  // ------------------------------------------------------------------
  // Drawer and sheet: the root is the scrim (…__overlay), CSS animates
  // data-state; the drawer adds sigil's swipe-to-dismiss gesture.
  // ------------------------------------------------------------------

  var DRAG_AXIS = {
    right: { prop: "translateX", sign: 1, dim: "w" },
    left: { prop: "translateX", sign: -1, dim: "w" },
    bottom: { prop: "translateY", sign: 1, dim: "h" },
    top: { prop: "translateY", sign: -1, dim: "h" }
  };

  function defineSlide(name) {
    var P = "ah-" + name;

    function panel(el) { return el.querySelector("." + P + "__panel"); }
    function isOpen(el) { return el.getAttribute("data-state") === "open"; }
    function setState(el, s) {
      el.setAttribute("data-state", s);
      var p = panel(el);
      if (p) { p.setAttribute("data-state", s); }
    }

    function open(el) {
      if (isOpen(el)) { return; }
      var ev = $.Event("ah:opening");
      $(el).trigger(ev);
      if (ev.isDefaultPrevented()) { return; }
      el.style.zIndex = nextZ();
      void el.offsetHeight;             // first open: let the transition run
      setState(el, "open");
      lock();
      var p = panel(el);
      pushStack({
        el: el, trap: p,
        esc: function () {
          if (!flag(el, "esc", true)) { return false; }
          close(el);
          return true;
        }
      });
      if (p) { p.focus({ preventScroll: true }); }
      $(el).trigger("ah:open");
    }

    function close(el, result) {
      if (!isOpen(el)) { return; }
      var ev = $.Event("ah:closing");
      $(el).trigger(ev);
      if (ev.isDefaultPrevented()) { return; }
      var p = panel(el);
      if (p) {
        p.style.transform = "";
        p.setAttribute("data-dragging", "false");
      }
      setState(el, "closed");
      unlock();
      pullStack(el, true);
      $(el).trigger("ah:close", [{ result: result || null }]);
    }

    function addDrag(el, $el) {
      var st = null;
      $el.on("pointerdown" + NS, function (e) {
        var oe = e.originalEvent;
        var p = panel(el);
        if (!p || !flag(el, "dismissible", true) || !p.contains(oe.target) ||
            $(oe.target).closest("button, a, input, textarea, select, ." + P + "__body").length) {
          return;
        }
        var side = p.getAttribute("data-side") || "bottom";
        var r = p.getBoundingClientRect();
        st = { side: side, size: DRAG_AXIS[side].dim === "w" ? r.width : r.height,
               x0: oe.clientX, y0: oe.clientY, t0: oe.timeStamp, d: 0 };
        p.setAttribute("data-dragging", "true");
        try { p.setPointerCapture(oe.pointerId); } catch (err) { /* no capture */ }
      });
      $el.on("pointermove" + NS, function (e) {
        if (!st) { return; }
        var oe = e.originalEvent;
        var ax = DRAG_AXIS[st.side];
        var raw = ax.dim === "w" ? oe.clientX - st.x0 : oe.clientY - st.y0;
        st.d = Math.max(0, ax.sign * raw);   // only towards closing
        panel(el).style.transform = ax.prop + "(" + (ax.sign * st.d) + "px)";
      });
      $el.on("pointerup" + NS + " pointercancel" + NS, function (e) {
        if (!st) { return; }
        var s = st;
        st = null;
        var p = panel(el);
        var dt = Math.max(1, e.originalEvent.timeStamp - s.t0);
        p.setAttribute("data-dragging", "false");
        // past 30% of the panel, or a flick faster than 0.5px/ms that
        // also moved 50px (a short tap must not count as a flick)
        if (s.d > s.size * 0.3 || (s.d / dt > 0.5 && s.d > 50)) {
          close(el);
        } else {
          p.style.transform = "";
        }
      });
    }

    AH.define(name, {
      init: function (el, $el) {
        $el.on("mousedown" + NS, function (e) {
          if (e.target === el && flag(el, "scrim", true)) { close(el); }
        });
        if (name === "drawer") { addDrag(el, $el); }
        if (el.getAttribute("data-ah-initial") === "open") { open(el); }
      },
      destroy: function (el) {
        if (isOpen(el)) {
          unlock();
          pullStack(el, false);
        }
      },
      methods: {
        open: function (el) { open(el); },
        close: function (el, $el, result) { close(el, result); },
        toggle: function (el) { if (isOpen(el)) { close(el); } else { open(el); } },
        isOpen: function (el) { return isOpen(el); }
      }
    });
  }

  defineSlide("drawer");
  defineSlide("sheet");

  // ------------------------------------------------------------------
  // Window (sigil overlay/window/*)
  // ------------------------------------------------------------------

  function winOpen(el) { return el.getAttribute("data-state") === "open"; }

  function winFront(el) {
    var zi = nextZ();
    el.style.zIndex = zi;
    var bd = $.data(el, "ahBackdrop");
    if (bd) { bd.style.zIndex = zi - 1; }
  }

  function windowOpen(el) {
    if (winOpen(el)) { return; }
    var ev = $.Event("ah:opening");
    $(el).trigger(ev);
    if (ev.isDefaultPrevented()) { return; }
    var modal = flag(el, "modal", false);
    var $el = $(el).stop(true, true);
    if (!el.hasAttribute("data-ah-placed")) {
      // centre in the viewport on first open (sigil: position :center)
      el.style.visibility = "hidden";
      el.style.display = "flex";
      var w = el.offsetWidth;
      var h = el.offsetHeight;
      el.style.left = Math.max(0, (window.innerWidth - w) / 2) + "px";
      el.style.top = Math.max(0, (window.innerHeight - h) / 2) + "px";
      el.style.display = "none";
      el.style.visibility = "";
      el.setAttribute("data-ah-placed", "");
    }
    if (modal) {
      var $bd = $('<div class="ah-window-modal-backdrop"></div>');
      $bd.insertBefore(el).hide().fadeIn(250);
      $bd.on("mousedown" + NS, function () {
        if (flag(el, "scrim", false)) { windowClose(el); }
      });
      $.data(el, "ahBackdrop", $bd[0]);
      lock();
    }
    winFront(el);
    el.setAttribute("data-state", "open");
    pushStack({
      el: el, trap: modal ? el : null,
      esc: function () {
        if (!flag(el, "esc", true)) { return false; }
        var a = document.activeElement;
        if (!modal && a !== el && !$.contains(el, a)) { return false; }
        windowClose(el);
        return true;
      }
    });
    $el.fadeIn(250);
    el.focus({ preventScroll: true });
    $el.trigger("ah:open");
  }

  function windowClose(el, result) {
    if (!winOpen(el)) { return; }
    var ev = $.Event("ah:closing");
    $(el).trigger(ev, [{ result: result || null }]);
    if (ev.isDefaultPrevented()) { return; }
    el.setAttribute("data-state", "closed");
    var bd = $.data(el, "ahBackdrop");
    if (bd) {
      $.removeData(el, "ahBackdrop");
      $(bd).stop(true).fadeOut(250, function () { $(bd).remove(); });
      unlock();
    }
    pullStack(el, true);
    $(el).stop(true, true).fadeOut(250);
    $(el).trigger("ah:close", [{ result: result || null }]);
  }

  function windowCollapse(el, collapsed) {
    $(el).toggleClass("ah-window-collapsed", collapsed);
    $(el).find(".ah-window-collapse-btn").attr("aria-expanded", collapsed ? "false" : "true");
    $(el).trigger(collapsed ? "ah:collapse" : "ah:expand");
  }

  function windowMove(el, x, y) {
    el.style.left = x + "px";
    el.style.top = y + "px";
    el.setAttribute("data-ah-placed", "");
    $(el).trigger("ah:moved", [{ x: x, y: y }]);
  }

  function windowResize(el, w, h) {
    el.style.width = w + "px";
    el.style.height = h + "px";
    $(el).trigger("ah:resize", [{ width: w, height: h }]);
  }

  // One pointer drag: move(dx, dy) while the pointer moves, end() once.
  function drag(e, ns, move, end) {
    var x0 = e.clientX;
    var y0 = e.clientY;
    $(document)
      .on("pointermove" + ns, function (me) { move(me.clientX - x0, me.clientY - y0); })
      .on("pointerup" + ns + " pointercancel" + ns, function () {
        $(document).off(ns);
        end();
      });
  }

  var MIN_W = 100;
  var MIN_H = 60;

  AH.define("window", {
    init: function (el, $el) {
      var id = uid("win");
      $.data(el, "ahNs", GNS + id);
      $el.on("mousedown" + NS + " pointerdown" + NS, function () {
        if (winOpen(el)) { winFront(el); }
      });
      $el.on("click" + NS, ".ah-window-collapse-btn", function (e) {
        e.stopPropagation();
        windowCollapse(el, !$el.hasClass("ah-window-collapsed"));
      });
      // drag by the title bar
      $el.on("pointerdown" + NS, ".ah-window-header", function (e) {
        if (!flag(el, "draggable", true) || $(e.target).closest("button").length ||
            e.button !== 0) { return; }
        e.preventDefault();
        var l0 = el.offsetLeft;
        var t0 = el.offsetTop;
        drag(e, GNS + id + "-drag", function (dx, dy) {
          var x = Math.min(Math.max(0, l0 + dx), window.innerWidth - el.offsetWidth);
          var y = Math.min(Math.max(0, t0 + dy), window.innerHeight - el.offsetHeight);
          el.style.left = Math.max(0, x) + "px";
          el.style.top = Math.max(0, y) + "px";
          el.setAttribute("data-ah-placed", "");
          $el.trigger("ah:moving", [{ x: x, y: y }]);
        }, function () {
          $el.trigger("ah:moved", [{ x: el.offsetLeft, y: el.offsetTop }]);
        });
      });
      // eight resize handles
      $el.on("pointerdown" + NS, ".ah-window-resize-handle", function (e) {
        if (!$el.hasClass("ah-window-resizable") || e.button !== 0) { return; }
        e.preventDefault();
        e.stopPropagation();
        var dir = this.getAttribute("data-dir") || "";
        var w0 = el.offsetWidth;
        var h0 = el.offsetHeight;
        var l0 = el.offsetLeft;
        var t0 = el.offsetTop;
        drag(e, GNS + id + "-resize", function (dx, dy) {
          var w = w0;
          var h = h0;
          if (dir.indexOf("e") >= 0) { w = w0 + dx; }
          if (dir.indexOf("w") >= 0) { w = w0 - dx; }
          if (dir.indexOf("s") >= 0) { h = h0 + dy; }
          if (dir.indexOf("n") >= 0) { h = h0 - dy; }
          w = Math.max(MIN_W, w);
          h = Math.max(MIN_H, h);
          el.style.width = w + "px";
          el.style.height = h + "px";
          if (dir.indexOf("w") >= 0) { el.style.left = (l0 + w0 - w) + "px"; }
          if (dir.indexOf("n") >= 0) { el.style.top = (t0 + h0 - h) + "px"; }
          el.setAttribute("data-ah-placed", "");
        }, function () {
          $el.trigger("ah:resize", [{ width: el.offsetWidth, height: el.offsetHeight }]);
        });
      });
      // arrows move, Ctrl+arrows resize (only when the window itself or
      // its title bar has focus, so inputs keep their arrow keys)
      $el.on("keydown" + NS, function (e) {
        if (e.target !== el && !$(e.target).closest(".ah-window-header").length) { return; }
        var k = e.key;
        if (k !== "ArrowLeft" && k !== "ArrowRight" && k !== "ArrowUp" && k !== "ArrowDown") {
          return;
        }
        e.preventDefault();
        var dx = k === "ArrowLeft" ? -10 : k === "ArrowRight" ? 10 : 0;
        var dy = k === "ArrowUp" ? -10 : k === "ArrowDown" ? 10 : 0;
        if (e.ctrlKey) {
          windowResize(el, Math.max(MIN_W, el.offsetWidth + dx), Math.max(MIN_H, el.offsetHeight + dy));
        } else {
          windowMove(el, el.offsetLeft + dx, el.offsetTop + dy);
        }
      });
      if (el.getAttribute("data-ah-initial") === "open") { windowOpen(el); }
    },
    destroy: function (el) {
      var ns = $.data(el, "ahNs");
      $(document).off(ns + "-drag").off(ns + "-resize");
      var bd = $.data(el, "ahBackdrop");
      if (bd) {
        $(bd).remove();
        unlock();
      }
      pullStack(el, false);
    },
    methods: {
      open: function (el) { windowOpen(el); },
      close: function (el, $el, result) { windowClose(el, result); },
      toggle: function (el) { if (winOpen(el)) { windowClose(el); } else { windowOpen(el); } },
      collapse: function (el) { windowCollapse(el, true); },
      expand: function (el) { windowCollapse(el, false); },
      move: function (el, $el, x, y) { windowMove(el, x, y); },
      resize: function (el, $el, w, h) { windowResize(el, w, h); },
      bringToFront: function (el) { winFront(el); },
      isOpen: function (el) { return winOpen(el); }
    }
  });

  // ------------------------------------------------------------------
  // Notification cards, toast (sigil overlay/notification + toast)
  // ------------------------------------------------------------------

  // Card markup comes from templates/notification.mustache (toast content
  // from templates/toast.mustache), the same templates aihtml_overlay
  // renders on the server. Only the corner container is built here.
  var VARIANTS = { info: 1, success: 1, warning: 1, error: 1 };
  var CORNERS = { "top-right": 1, "top-left": 1, "bottom-right": 1, "bottom-left": 1 };

  function corner(pos) {
    pos = CORNERS[pos] ? pos : "top-right";
    var $c = $("body > .ah-notify-container.ah-notify-" + pos);
    if (!$c.length) {
      $c = $('<div class="ah-notify-container ah-notify-' + pos + '"></div>').appendTo(document.body);
    }
    return $c;
  }

  // The view for templates/notification.mustache; aihtml_overlay:card/2
  // builds the same one. contentHtml must be trusted HTML.
  function cardView(o, contentHtml) {
    var v = VARIANTS[o.variant] ? o.variant : "info";
    var w = o.width;
    return {
      variant: v, info: v === "info", success: v === "success",
      warning: v === "warning", error: v === "error",
      clickable: o.closeOnClick !== false && o.closeOnClick !== "false",
      closable: o.closable !== false && o.closable !== "false",
      width: w === undefined || w === null || w === "" ? null
        : (typeof w === "number" ? w + "px" : String(w)),
      content: contentHtml
    };
  }

  function duration(v, dflt) {
    return v === undefined || v === null || v === "" ? dflt : Number(v);
  }

  // Put a card (HTML string or element) in its corner and run it.
  // o: {position, duration (ms, <= 0 stays), source}. Whether a click on
  // the card closes it is read from the markup (.ah-notify-clickable).
  // Events go to o.source (a notification template) or the card.
  function showCard(card, o) {
    var $card = typeof card === "string" ? $($.parseHTML(card)).filter(".ah-notify") : $(card);
    var pos = CORNERS[o.position] ? o.position : "top-right";
    var $c = corner(pos);
    if (pos.indexOf("bottom") === 0) { $card.prependTo($c); } else { $card.appendTo($c); }
    var target = o.source || $card[0];
    var timer = null;
    var closed = false;
    function close() {
      if (closed) { return; }
      closed = true;
      clearTimeout(timer);
      $card.stop(true).fadeOut(300, function () {
        $card.remove();
        if (!$c.children().length) { $c.remove(); }
        $(target).trigger("ah:close", [{ result: null }]);
      });
    }
    function arm() {
      if (o.duration > 0) { timer = setTimeout(close, o.duration); }
    }
    $card.data("ahClose", close);
    $card.on("click", ".ah-notify-close", function (e) {
      e.stopPropagation();
      close();
    });
    $card.on("keydown", ".ah-notify-close", function (e) {
      if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        close();
      }
    });
    if ($card.hasClass("ah-notify-clickable")) {
      $card.on("click", function () {
        $(target).trigger("ah:click");
        close();
      });
    }
    // hovering keeps the card (sigil lets it expire under the pointer)
    $card.on("mouseenter", function () { clearTimeout(timer); })
      .on("mouseleave", function () { if (!closed) { arm(); } });
    arm();
    $card.css({ display: "flex", opacity: 0 }).animate({ opacity: 0.95 }, 300, function () {
      $card.css("opacity", "");
      $(target).trigger("ah:open");
    });
    return $card[0];
  }

  // AH.fn("toast", {title, description, variant, duration = 4000, position,
  // closable, closeOnClick, width}): sigil's toast/show!, for client-side
  // triggers (shows_toast/2). Text is escaped by the template.
  function toast(o) {
    o = typeof o === "string" ? { title: o } : (o || {});
    var title = o.title === undefined || o.title === null ? "" : String(o.title);
    var desc = o.description === undefined || o.description === null ? "" : String(o.description);
    var content = AH.tpl.toast({ has_title: title !== "", title: title,
                                 has_description: desc !== "", description: desc });
    return showCard(AH.tpl.notification(cardView(o, content)),
                    { position: o.position, duration: duration(o.duration, 4000) });
  }

  // AH.fn("notify", {card, position, duration = 3000}): card is the HTML
  // aihtml_overlay:toast/3 and notify/2 render on the server. Without it,
  // {text, variant, closable, closeOnClick, width} builds one here.
  function notify(o) {
    o = typeof o === "string" ? { text: o } : (o || {});
    var card = o.card || AH.tpl.notification(
      cardView(o, $("<div>").text(String(o.text || "")).html()));   // escaped text
    return showCard(card, { position: o.position, duration: duration(o.duration, 3000) });
  }

  AH.fn("toast", toast);
  AH.fn("notify", notify);
  AH.toast = toast;
  AH.notify = notify;

  $(document).on("click" + GNS, "[data-ah-toast]", function () {
    var $t = $(this);
    toast({
      title: $t.attr("data-ah-toast"),
      description: $t.attr("data-ah-toast-description"),
      variant: $t.attr("data-ah-toast-variant"),
      duration: $t.attr("data-ah-toast-duration"),
      position: $t.attr("data-ah-toast-position"),
      closable: $t.attr("data-ah-toast-closable")
    });
  });

  AH.define("notification", {
    init: function (el) { $.data(el, "ahCards", []); },
    destroy: function (el) {
      ($.data(el, "ahCards") || []).forEach(function (c) { $(c).remove(); });
      $(".ah-notify-container").each(function () {
        if (!$(this).children().length) { $(this).remove(); }
      });
    },
    methods: {
      open: function (el) {
        // the server rendered the card inside the template element
        var card = showCard($(el).children(".ah-notify").first().clone(), {
          position: el.getAttribute("data-ah-position"),
          duration: num(el, "duration", 3000),
          source: el
        });
        var cards = ($.data(el, "ahCards") || []).filter(function (c) {
          return document.contains(c);
        });
        cards.push(card);
        $.data(el, "ahCards", cards);
      },
      close: function (el) { AH.invoke(el, "closeAll"); },
      closeAll: function (el) {
        ($.data(el, "ahCards") || []).forEach(function (c) {
          var f = $(c).data("ahClose");
          if (f) { f(); }
        });
        $.data(el, "ahCards", []);
      },
      closeLast: function (el) {
        var cards = ($.data(el, "ahCards") || []).filter(function (c) {
          return document.contains(c);
        });
        var last = cards.pop();
        if (last && $(last).data("ahClose")) { $(last).data("ahClose")(); }
        $.data(el, "ahCards", cards);
      }
    }
  });
})(window.jQuery, window.AH);
