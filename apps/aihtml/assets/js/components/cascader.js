/* Behaviour of cascader (designs/04-components.md), ported from sigil's
 * form/cascader. The columns and the search list are rendered on the
 * server (aihtml_cascader); the behaviour shows, hides and marks them.
 * Shared helpers: _lib_list.js. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var LIST = AH.lib.list;
  var ensureId = LIST.ensureId, publish = LIST.publish, split = LIST.split, join = LIST.join,
    shown = LIST.shown, enabled = LIST.enabled, scrollInto = LIST.scrollInto;
  var seq = 0;

  // A mousedown outside the component.
  function outside(el, e) {
    return e.target.isConnected !== false && !$.contains(el, e.target) && e.target !== el;
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

  // ==================================================================
  // cascader
  // ==================================================================

  function csState(el) { return $.data(el, "ah-cs"); }

  function csColumns(st) { return st.$menus.children(".ah-cascader-menu-column"); }

  // The column holding the children of `path` (an array); the last one
  // wins when a lazy level was loaded twice.
  function csColumn(st, path) {
    var key = join(path);
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
    publish(el, $el, join(path), fire);
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
      st.pending = { path: join(path), kbd: kbd };
      st.$menus.append($('<div class="ah-cascader-loading"></div>').text("Loading…"));
      csPosition(el);
      st.$loader.attr("data-ah-value", join(path)).trigger("ah:load");
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
      // Called by aihtml_cascader:cascader_children/3 after it appended
      // the column(s) of `path`; no column means the node is a leaf.
      childrenLoaded: function (el, $el, path) {
        var st = csState(el);
        var p = split(path);
        var key = join(p);
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
        if (st.isOpen && join(st.open) === key) {
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
})(window.jQuery, window.AH);
