/*!
 * aihtml.js: client runtime for aihtml prefabs. Requires jQuery 3.5+ or 4.
 *
 * The server renders all HTML. This file only adds behaviour to it:
 *
 *   AH.define(name, {init, destroy})  behaviour for [data-ah="<name>"]
 *   AH.mount(root)                    attach behaviours inside root
 *   AH.theme.get() / .set(axis, v)    the four theme axes on <html>
 *   AH.fetch(el)                      run an element's data-ah-fetch
 *
 * Actions: elements with data-ah-on="event:token" call Erlang. Each event
 * is one POST to <body data-ah-action>; the reply is an AG-UI event stream
 * whose CUSTOM "aihtml.ui" events carry DOM operations. The server keeps
 * no state between requests.
 *
 * Round trips: an element with data-ah-fetch="get|post|..." sends a
 * request to data-ah-url when data-ah-trigger fires (default: submit for
 * forms, change for inputs, click otherwise). The response is HTML that
 * replaces data-ah-target ("this" or a selector) according to
 * data-ah-swap (inner | outer | append | prepend | none), and the new
 * content is mounted. Requests carry the header "X-Aihtml: 1".
 *
 * Events, all namespaced so AH.destroy can remove them:
 *   ah:change {checked, value}   checkbox, radio, switch
 *   ah:tab {key}                 tabs
 *   ah:dismiss                   alert, before it is removed
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

  function define(name, spec) {
    behaviors[name] = spec;
  }

  function mount(rootEl) {
    var $root = $(rootEl || document);
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
  // Prefab behaviours
  // ------------------------------------------------------------------

  define("check", {
    init: function (el, $el) {
      $el.on("change" + NS, "input", function () {
        $el.trigger("ah:change", [{ checked: this.checked, value: this.value }]);
      });
    }
  });

  define("tabs", {
    init: function (el, $el) {
      function select($tab, focus) {
        var key = $tab.attr("data-ah-tab");
        $el.find("[data-ah-tab]").each(function () {
          var on = this === $tab[0];
          $(this).attr({ "aria-selected": String(on), tabindex: on ? 0 : -1 });
        });
        $el.find("[data-ah-panel]").each(function () {
          this.hidden = this.getAttribute("data-ah-panel") !== key;
        });
        if (focus) {
          $tab.trigger("focus");
        }
        $el.trigger("ah:tab", [{ key: key }]);
      }
      $el.on("click" + NS, "[data-ah-tab]", function () {
        select($(this), false);
      });
      $el.on("keydown" + NS, "[data-ah-tab]", function (e) {
        var $tabs = $el.find("[data-ah-tab]");
        var i = $tabs.index(this);
        var next = { ArrowRight: i + 1, ArrowLeft: i - 1, Home: 0, End: $tabs.length - 1 }[e.key];
        if (next === undefined) {
          return;
        }
        e.preventDefault();
        select($tabs.eq((next + $tabs.length) % $tabs.length), true);
      });
    }
  });

  define("alert", {
    init: function (el, $el) {
      $el.on("click" + NS, "[data-ah-dismiss]", function () {
        $el.trigger("ah:dismiss");
        destroy(el);
        $el.remove();
      });
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

  function swap($target, html, mode) {
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

  function specs(el) {
    return (el.getAttribute("data-ah-on") || "").split(/\s+/).filter(Boolean).map(function (s) {
      var p = s.split(":");
      return { event: p[0], token: p[1], debounce: p[2] ? parseInt(p[2], 10) : 0 };
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
    return {
      type: e.type,
      id: el.id,
      value: control ? $(el).val() : null,
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
        if (ev.name === "aihtml.ui") { applyOps(ev.value); }
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
                             event: eventPayload(el, e) }),
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

  $.each(ACTION_EVENTS, function (_, type) {
    $(document).on(type + NS, "[data-ah-on]", function (e) {
      var el = this;
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
  });

  $(function () {
    mount(document);
  });

  var api = {
    define: define,
    mount: mount,
    destroy: destroy,
    theme: theme,
    fetch: fetchFor,
    apply: applyOps,
    version: "0.3.0"
  };
  return api;
});
