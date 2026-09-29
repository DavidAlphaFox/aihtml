/* Internal: what the drag and drop behaviours share (sortable.js,
 * dragdrop.js), ported from sigil's layout/sortable and layout/dragdrop.
 * Pointer events (mouse, pen, touch) instead of sigil's mouse + touch
 * sequence; one drag at a time on the page (AH.lib.dnd.drag, shared by
 * both components), tracked on the document with its own namespace and
 * unbound when the drag ends or the component is destroyed.
 */
(function ($, AH) {
  "use strict";

  var DOC_NS = ".ahdnd";
  var EDGE = 20, SPEED = 10;   // auto scroll: edge width and px per frame

  AH.lib = AH.lib || {};
  // drag: the drag in progress (one per page): {kind, el, pointerId, ...}
  var L = AH.lib.dnd = { DISTANCE: 5, drag: null };   // DISTANCE: px of movement before a press becomes a drag

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
    L.drag = d;
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
    if (L.drag && L.drag.raf) { cancelAnimationFrame(L.drag.raf); }
    L.drag = null;
  }

  // Cancel the drag in progress if it belongs to el.
  function cancelFor(el) {
    if (L.drag && L.drag.el === el) { L.drag.cancel(); }
  }

  L.pageRect = pageRect;
  L.px = px;
  L.py = py;
  L.inside = inside;
  L.floatingCopy = floatingCopy;
  L.scrollParent = scrollParent;
  L.autoScroll = autoScroll;
  L.swallowClick = swallowClick;
  L.editable = editable;
  L.announce = announce;
  L.label = label;
  L.track = track;
  L.untrack = untrack;
  L.cancelFor = cancelFor;
})(window.jQuery, window.AH);
