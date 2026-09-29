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
