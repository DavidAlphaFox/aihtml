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
