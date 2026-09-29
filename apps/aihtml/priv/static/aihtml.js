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
    var $new = $($.parseHTML(html, document, false));
    switch (mode) {
      case "none":
        return $();
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
    ctx.added.forEach(function (el) {
      if (el.nodeType === 1 && document.contains(el)) {
        mount(el);
      }
    });
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

    $el.attr("aria-busy", "true");
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
      .always(function () {
        $el.removeAttr("aria-busy");
      });
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

  function runAction(el, spec, e) {
    var url = document.body.getAttribute("data-ah-action") || "/aihtml/action";
    var payload = eventPayload(el, e);          // also gives el an id
    var key = el.id + "/" + spec.event;
    if (LATEST_WINS[spec.event] && inflight[key]) {
      inflight[key].abort();
    }
    var ctrl = new AbortController();
    inflight[key] = ctrl;
    el.setAttribute("aria-busy", "true");
    var done = function () {
      if (inflight[key] === ctrl) {
        delete inflight[key];
        el.removeAttribute("aria-busy");
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
        if (!LATEST_WINS[type] && el.getAttribute("aria-busy") === "true") {
          return;             // ignore double clicks while a run is going
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
  AH.tpl["combobox_tag"] = function(d){var S=[d],o="";o+="<span class=\"ah-combobox-tag\"><span class=\"ah-combobox-tag-text\">";o+=R.esc(R.lookup(S,["label"]));o+="</span><span class=\"ah-combobox-tag-close\" data-value=\"";o+=R.esc(R.lookup(S,["value"]));o+="\" role=\"button\" aria-label=\"Remove ";o+=R.esc(R.lookup(S,["label"]));o+="\">&times;</span></span>";return o;};
  AH.tpl["datepicker_month"] = function(d){var S=[d],o="";o+="<div class=\"ah-datepicker-calendar\">";o+="\n";o+="<div class=\"ah-datepicker-header\"><div class=\"ah-datepicker-nav-group\"><button type=\"button\" tabindex=\"-1\" class=\"ah-datepicker-nav-prev ah-datepicker-nav-year\" data-nav=\"-12\" aria-label=\"";o+=R.esc(R.lookup(S,["prev_year"]));o+="\">&laquo;</button><button type=\"button\" tabindex=\"-1\" class=\"ah-datepicker-nav-prev\" data-nav=\"-1\" aria-label=\"";o+=R.esc(R.lookup(S,["prev_month"]));o+="\">&lsaquo;</button></div><div class=\"ah-datepicker-title\" id=\"";o+=R.esc(R.lookup(S,["title_id"]));o+="\" aria-live=\"polite\">";o+=R.esc(R.lookup(S,["title"]));o+="</div><div class=\"ah-datepicker-nav-group\"><button type=\"button\" tabindex=\"-1\" class=\"ah-datepicker-nav-next\" data-nav=\"1\" aria-label=\"";o+=R.esc(R.lookup(S,["next_month"]));o+="\">&rsaquo;</button><button type=\"button\" tabindex=\"-1\" class=\"ah-datepicker-nav-next ah-datepicker-nav-year\" data-nav=\"12\" aria-label=\"";o+=R.esc(R.lookup(S,["next_year"]));o+="\">&raquo;</button></div></div>";o+="\n";o+="<div class=\"ah-datepicker-week-header\" aria-hidden=\"true\">";var v0=R.lookup(S,["week_numbers"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-datepicker-week-num-header\">Wk</div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-datepicker-week-num-header\">Wk</div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-datepicker-week-num-header\">Wk</div>";S.shift();}}var v0=R.lookup(S,["weekdays"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-datepicker-weekday\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-datepicker-weekday\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-datepicker-weekday\">";o+=R.esc(R.lookup(S,["label"]));o+="</div>";S.shift();}}o+="</div>";o+="\n";o+="<div class=\"ah-datepicker-body\" role=\"grid\" aria-labelledby=\"";o+=R.esc(R.lookup(S,["title_id"]));o+="\">";o+="\n";var v0=R.lookup(S,["weeks"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-datepicker-week\" role=\"row\">";var v1=R.lookup(S,["week_numbers"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}var v1=R.lookup(S,["days"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}S.shift();}}o+="</div>";o+="\n";S.shift();}}else if(v0===true){o+="<div class=\"ah-datepicker-week\" role=\"row\">";var v1=R.lookup(S,["week_numbers"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}var v1=R.lookup(S,["days"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}S.shift();}}o+="</div>";o+="\n";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-datepicker-week\" role=\"row\">";var v1=R.lookup(S,["week_numbers"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}else if(v1===true){o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<div class=\"ah-datepicker-week-num\">";o+=R.esc(R.lookup(S,["num"]));o+="</div>";S.shift();}}var v1=R.lookup(S,["days"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["empty"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}else if(v2===true){o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<div class=\"ah-datepicker-day ah-datepicker-day-empty\" role=\"gridcell\"></div>";S.shift();}}if(R.falsy(R.lookup(S,["empty"]))){o+="<div class=\"";o+=R.esc(R.lookup(S,["cls"]));o+="\" role=\"gridcell\" id=\"";o+=R.esc(R.lookup(S,["id"]));o+="\" data-date=\"";o+=R.esc(R.lookup(S,["date"]));o+="\" aria-selected=\"";o+=R.esc(R.lookup(S,["selected"]));o+="\" aria-disabled=\"";o+=R.esc(R.lookup(S,["disabled"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-datepicker-day-text\">";o+=R.esc(R.lookup(S,["day"]));o+="</span></div>";}S.shift();}}o+="</div>";o+="\n";S.shift();}}o+="</div>";o+="\n";o+="<div class=\"ah-datepicker-footer\"><button type=\"button\" tabindex=\"-1\" class=\"ah-datepicker-today-btn\">";o+=R.esc(R.lookup(S,["today"]));o+="</button></div>";o+="\n";o+="</div>";return o;};
  AH.tpl["notification"] = function(d){var S=[d],o="";o+="<div class=\"ah-notify ah-notify-";o+=R.esc(R.lookup(S,["variant"]));var v0=R.lookup(S,["clickable"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" ah-notify-clickable";S.shift();}}else if(v0===true){o+=" ah-notify-clickable";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" ah-notify-clickable";S.shift();}}o+="\" role=\"alert\"";var v0=R.lookup(S,["width"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" style=\"width:";o+=R.esc(R.lookup(S,["width"]));o+="\"";S.shift();}}else if(v0===true){o+=" style=\"width:";o+=R.esc(R.lookup(S,["width"]));o+="\"";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" style=\"width:";o+=R.esc(R.lookup(S,["width"]));o+="\"";S.shift();}}o+="><span class=\"ah-notify-icon\">";var v0=R.lookup(S,["info"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"16\" x2=\"12\" y2=\"12\"/><line x1=\"12\" y1=\"8\" x2=\"12.01\" y2=\"8\"/></svg>";S.shift();}}else if(v0===true){o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"16\" x2=\"12\" y2=\"12\"/><line x1=\"12\" y1=\"8\" x2=\"12.01\" y2=\"8\"/></svg>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"12\" y1=\"16\" x2=\"12\" y2=\"12\"/><line x1=\"12\" y1=\"8\" x2=\"12.01\" y2=\"8\"/></svg>";S.shift();}}var v0=R.lookup(S,["success"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M22 11.08V12a10 10 0 1 1-5.93-9.14\"/><polyline points=\"22 4 12 14.01 9 11.01\"/></svg>";S.shift();}}else if(v0===true){o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M22 11.08V12a10 10 0 1 1-5.93-9.14\"/><polyline points=\"22 4 12 14.01 9 11.01\"/></svg>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M22 11.08V12a10 10 0 1 1-5.93-9.14\"/><polyline points=\"22 4 12 14.01 9 11.01\"/></svg>";S.shift();}}var v0=R.lookup(S,["warning"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z\"/><line x1=\"12\" y1=\"9\" x2=\"12\" y2=\"13\"/><line x1=\"12\" y1=\"17\" x2=\"12.01\" y2=\"17\"/></svg>";S.shift();}}else if(v0===true){o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z\"/><line x1=\"12\" y1=\"9\" x2=\"12\" y2=\"13\"/><line x1=\"12\" y1=\"17\" x2=\"12.01\" y2=\"17\"/></svg>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z\"/><line x1=\"12\" y1=\"9\" x2=\"12\" y2=\"13\"/><line x1=\"12\" y1=\"17\" x2=\"12.01\" y2=\"17\"/></svg>";S.shift();}}var v0=R.lookup(S,["error"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"15\" y1=\"9\" x2=\"9\" y2=\"15\"/><line x1=\"9\" y1=\"9\" x2=\"15\" y2=\"15\"/></svg>";S.shift();}}else if(v0===true){o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"15\" y1=\"9\" x2=\"9\" y2=\"15\"/><line x1=\"9\" y1=\"9\" x2=\"15\" y2=\"15\"/></svg>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><circle cx=\"12\" cy=\"12\" r=\"10\"/><line x1=\"15\" y1=\"9\" x2=\"9\" y2=\"15\"/><line x1=\"9\" y1=\"9\" x2=\"15\" y2=\"15\"/></svg>";S.shift();}}o+="</span><div class=\"ah-notify-content\">";o+=R.str(R.lookup(S,["content"]));o+="</div>";var v0=R.lookup(S,["closable"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<span class=\"ah-notify-close\" role=\"button\" tabindex=\"0\" aria-label=\"Close\"></span>";S.shift();}}else if(v0===true){o+="<span class=\"ah-notify-close\" role=\"button\" tabindex=\"0\" aria-label=\"Close\"></span>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<span class=\"ah-notify-close\" role=\"button\" tabindex=\"0\" aria-label=\"Close\"></span>";S.shift();}}o+="</div>";return o;};
  AH.tpl["pagination_items"] = function(d){var S=[d],o="";var v0=R.lookup(S,["entries"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);var v1=R.lookup(S,["gap"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";S.shift();}}else if(v1===true){o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";S.shift();}}var v1=R.lookup(S,["info"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";S.shift();}}else if(v1===true){o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";S.shift();}}var v1=R.lookup(S,["item"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}S.shift();}}var v1=R.lookup(S,["nav"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}S.shift();}}S.shift();}}else if(v0===true){var v1=R.lookup(S,["gap"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";S.shift();}}else if(v1===true){o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";S.shift();}}var v1=R.lookup(S,["info"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";S.shift();}}else if(v1===true){o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";S.shift();}}var v1=R.lookup(S,["item"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}S.shift();}}var v1=R.lookup(S,["nav"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}S.shift();}}}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);var v1=R.lookup(S,["gap"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";S.shift();}}else if(v1===true){o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li>";S.shift();}}var v1=R.lookup(S,["info"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";S.shift();}}else if(v1===true){o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="<li class=\"ah-pagination-simple-info\">";o+=R.esc(R.lookup(S,["text"]));o+="</li>";S.shift();}}var v1=R.lookup(S,["item"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-item";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-item-active";S.shift();}}else if(v3===true){o+=" ah-pagination-item-active";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-item-active";S.shift();}}o+="\" data-page=\"";o+=R.esc(R.lookup(S,["number"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"Page ";o+=R.esc(R.lookup(S,["number"]));o+="\"";var v3=R.lookup(S,["active"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-current=\"page\"";S.shift();}}else if(v3===true){o+=" aria-current=\"page\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-current=\"page\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["number"]));o+="</li>";}S.shift();}}var v1=R.lookup(S,["nav"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}S.shift();}}else if(v1===true){var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);var v2=R.lookup(S,["link"]);if(!R.falsy(v2)){if(Array.isArray(v2)){for(var i2=0;i2<v2.length;i2++){S.unshift(v2[i2]);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}else if(v2===true){o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";}else if(typeof v2==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v2);o+="<li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}o+="\" href=\"";o+=R.esc(R.lookup(S,["href"]));o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></a></li>";S.shift();}}if(R.falsy(R.lookup(S,["link"]))){o+="<li class=\"ah-pagination-nav";var v3=R.lookup(S,["first_last"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-first-last";S.shift();}}else if(v3===true){o+=" ah-pagination-first-last";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-first-last";S.shift();}}var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" ah-pagination-nav-disabled";S.shift();}}else if(v3===true){o+=" ah-pagination-nav-disabled";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" ah-pagination-nav-disabled";S.shift();}}o+="\" data-type=\"";o+=R.esc(R.lookup(S,["type"]));o+="\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-label=\"";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v3=R.lookup(S,["disabled"]);if(!R.falsy(v3)){if(Array.isArray(v3)){for(var i3=0;i3<v3.length;i3++){S.unshift(v3[i3]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v3===true){o+=" aria-disabled=\"true\"";}else if(typeof v3==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v3);o+=" aria-disabled=\"true\"";S.shift();}}o+="><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">";o+=R.esc(R.lookup(S,["icon"]));o+="</span><span class=\"ah-pagination-nav-label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span></li>";}S.shift();}}S.shift();}}return o;};
  AH.tpl["steps_indicator"] = function(d){var S=[d],o="";var v0=R.lookup(S,["check"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<span class=\"ah-steps-check\">✓</span>";S.shift();}}else if(v0===true){o+="<span class=\"ah-steps-check\">✓</span>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<span class=\"ah-steps-check\">✓</span>";S.shift();}}var v0=R.lookup(S,["error"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<span class=\"ah-steps-error-icon\">✕</span>";S.shift();}}else if(v0===true){o+="<span class=\"ah-steps-error-icon\">✕</span>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<span class=\"ah-steps-error-icon\">✕</span>";S.shift();}}var v0=R.lookup(S,["plain"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=R.esc(R.lookup(S,["number"]));S.shift();}}else if(v0===true){o+=R.esc(R.lookup(S,["number"]));}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=R.esc(R.lookup(S,["number"]));S.shift();}}return o;};
  AH.tpl["tag_input_chip"] = function(d){var S=[d],o="";o+="<span class=\"ah-chip ah-tag-input__chip\" data-variant=\"";o+=R.esc(R.lookup(S,["variant"]));o+="\" data-color=\"";o+=R.esc(R.lookup(S,["color"]));o+="\" data-size=\"small\" data-index=\"";o+=R.esc(R.lookup(S,["index"]));o+="\"><span class=\"ah-chip__label\">";o+=R.esc(R.lookup(S,["label"]));o+="</span><button type=\"button\" class=\"ah-chip__delete\" tabindex=\"-1\" aria-label=\"Remove ";o+=R.esc(R.lookup(S,["label"]));o+="\"";var v0=R.lookup(S,["disabled"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" disabled";S.shift();}}else if(v0===true){o+=" disabled";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" disabled";S.shift();}}o+=">&times;</button></span>";return o;};
  AH.tpl["timepicker_header"] = function(d){var S=[d],o="";o+="<span class=\"ah-timepicker-header-hours";var v0=R.lookup(S,["hours_active"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" ah-timepicker-header-active";S.shift();}}else if(v0===true){o+=" ah-timepicker-header-active";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" ah-timepicker-header-active";S.shift();}}o+="\" data-action=\"select-hours\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v0=R.lookup(S,["hours_active"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="true";S.shift();}}else if(v0===true){o+="true";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["hours_active"]))){o+="false";}o+="\" aria-label=\"Hours\"";var v0=R.lookup(S,["disabled"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v0===true){o+=" aria-disabled=\"true\"";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" aria-disabled=\"true\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["hours"]));o+="</span><span class=\"ah-timepicker-header-sep\">:</span><span class=\"ah-timepicker-header-minutes";var v0=R.lookup(S,["minutes_active"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" ah-timepicker-header-active";S.shift();}}else if(v0===true){o+=" ah-timepicker-header-active";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" ah-timepicker-header-active";S.shift();}}o+="\" data-action=\"select-minutes\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v0=R.lookup(S,["minutes_active"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="true";S.shift();}}else if(v0===true){o+="true";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["minutes_active"]))){o+="false";}o+="\" aria-label=\"Minutes\"";var v0=R.lookup(S,["disabled"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v0===true){o+=" aria-disabled=\"true\"";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+=" aria-disabled=\"true\"";S.shift();}}o+=">";o+=R.esc(R.lookup(S,["minutes"]));o+="</span>";var v0=R.lookup(S,["twelve"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<span class=\"ah-timepicker-header-period\"><span class=\"ah-timepicker-header-am";var v1=R.lookup(S,["am"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-header-am-active";S.shift();}}else if(v1===true){o+=" ah-timepicker-header-am-active";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-header-am-active";S.shift();}}o+="\" data-action=\"set-am\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v1=R.lookup(S,["am"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="true";S.shift();}}else if(v1===true){o+="true";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["am"]))){o+="false";}o+="\" aria-label=\"AM\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}o+=">AM</span><span class=\"ah-timepicker-header-pm";var v1=R.lookup(S,["pm"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-header-pm-active";S.shift();}}else if(v1===true){o+=" ah-timepicker-header-pm-active";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-header-pm-active";S.shift();}}o+="\" data-action=\"set-pm\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v1=R.lookup(S,["pm"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="true";S.shift();}}else if(v1===true){o+="true";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["pm"]))){o+="false";}o+="\" aria-label=\"PM\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}o+=">PM</span></span>";S.shift();}}else if(v0===true){o+="<span class=\"ah-timepicker-header-period\"><span class=\"ah-timepicker-header-am";var v1=R.lookup(S,["am"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-header-am-active";S.shift();}}else if(v1===true){o+=" ah-timepicker-header-am-active";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-header-am-active";S.shift();}}o+="\" data-action=\"set-am\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v1=R.lookup(S,["am"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="true";S.shift();}}else if(v1===true){o+="true";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["am"]))){o+="false";}o+="\" aria-label=\"AM\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}o+=">AM</span><span class=\"ah-timepicker-header-pm";var v1=R.lookup(S,["pm"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-header-pm-active";S.shift();}}else if(v1===true){o+=" ah-timepicker-header-pm-active";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-header-pm-active";S.shift();}}o+="\" data-action=\"set-pm\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v1=R.lookup(S,["pm"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="true";S.shift();}}else if(v1===true){o+="true";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["pm"]))){o+="false";}o+="\" aria-label=\"PM\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}o+=">PM</span></span>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<span class=\"ah-timepicker-header-period\"><span class=\"ah-timepicker-header-am";var v1=R.lookup(S,["am"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-header-am-active";S.shift();}}else if(v1===true){o+=" ah-timepicker-header-am-active";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-header-am-active";S.shift();}}o+="\" data-action=\"set-am\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v1=R.lookup(S,["am"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="true";S.shift();}}else if(v1===true){o+="true";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["am"]))){o+="false";}o+="\" aria-label=\"AM\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}o+=">AM</span><span class=\"ah-timepicker-header-pm";var v1=R.lookup(S,["pm"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-header-pm-active";S.shift();}}else if(v1===true){o+=" ah-timepicker-header-pm-active";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-header-pm-active";S.shift();}}o+="\" data-action=\"set-pm\" role=\"button\" tabindex=\"";o+=R.esc(R.lookup(S,["tabindex"]));o+="\" aria-pressed=\"";var v1=R.lookup(S,["pm"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+="true";S.shift();}}else if(v1===true){o+="true";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+="true";S.shift();}}if(R.falsy(R.lookup(S,["pm"]))){o+="false";}o+="\" aria-label=\"PM\"";var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" aria-disabled=\"true\"";S.shift();}}else if(v1===true){o+=" aria-disabled=\"true\"";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" aria-disabled=\"true\"";S.shift();}}o+=">PM</span></span>";S.shift();}}return o;};
  AH.tpl["timepicker_numbers"] = function(d){var S=[d],o="";var v0=R.lookup(S,["numbers"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<text class=\"ah-timepicker-number";var v1=R.lookup(S,["inner"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-inner";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-inner";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-inner";S.shift();}}var v1=R.lookup(S,["selected"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-selected";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-selected";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-selected";S.shift();}}var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-disabled";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-disabled";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-disabled";S.shift();}}o+="\" x=\"";o+=R.esc(R.lookup(S,["x"]));o+="\" y=\"";o+=R.esc(R.lookup(S,["y"]));o+="\" data-val=\"";o+=R.esc(R.lookup(S,["val"]));o+="\">";o+=R.esc(R.lookup(S,["label"]));o+="</text>";S.shift();}}else if(v0===true){o+="<text class=\"ah-timepicker-number";var v1=R.lookup(S,["inner"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-inner";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-inner";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-inner";S.shift();}}var v1=R.lookup(S,["selected"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-selected";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-selected";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-selected";S.shift();}}var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-disabled";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-disabled";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-disabled";S.shift();}}o+="\" x=\"";o+=R.esc(R.lookup(S,["x"]));o+="\" y=\"";o+=R.esc(R.lookup(S,["y"]));o+="\" data-val=\"";o+=R.esc(R.lookup(S,["val"]));o+="\">";o+=R.esc(R.lookup(S,["label"]));o+="</text>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<text class=\"ah-timepicker-number";var v1=R.lookup(S,["inner"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-inner";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-inner";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-inner";S.shift();}}var v1=R.lookup(S,["selected"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-selected";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-selected";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-selected";S.shift();}}var v1=R.lookup(S,["disabled"]);if(!R.falsy(v1)){if(Array.isArray(v1)){for(var i1=0;i1<v1.length;i1++){S.unshift(v1[i1]);o+=" ah-timepicker-number-disabled";S.shift();}}else if(v1===true){o+=" ah-timepicker-number-disabled";}else if(typeof v1==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v1);o+=" ah-timepicker-number-disabled";S.shift();}}o+="\" x=\"";o+=R.esc(R.lookup(S,["x"]));o+="\" y=\"";o+=R.esc(R.lookup(S,["y"]));o+="\" data-val=\"";o+=R.esc(R.lookup(S,["val"]));o+="\">";o+=R.esc(R.lookup(S,["label"]));o+="</text>";S.shift();}}return o;};
  AH.tpl["toast"] = function(d){var S=[d],o="";var v0=R.lookup(S,["has_title"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-toast__title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-toast__title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-toast__title\">";o+=R.esc(R.lookup(S,["title"]));o+="</div>";S.shift();}}var v0=R.lookup(S,["has_description"]);if(!R.falsy(v0)){if(Array.isArray(v0)){for(var i0=0;i0<v0.length;i0++){S.unshift(v0[i0]);o+="<div class=\"ah-toast__description\">";o+=R.esc(R.lookup(S,["description"]));o+="</div>";S.shift();}}else if(v0===true){o+="<div class=\"ah-toast__description\">";o+=R.esc(R.lookup(S,["description"]));o+="</div>";}else if(typeof v0==="function"){throw new Error("lambdas are not supported");}else{S.unshift(v0);o+="<div class=\"ah-toast__description\">";o+=R.esc(R.lookup(S,["description"]));o+="</div>";S.shift();}}return o;};
})(window.AH);

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
