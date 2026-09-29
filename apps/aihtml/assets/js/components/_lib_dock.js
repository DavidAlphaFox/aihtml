/* Internal: what the docking behaviours share (docking.js,
 * dock_layout.js), ported from sigil's layout/docking and
 * layout/dock_layout. Pointer events (mouse, pen, touch) instead of
 * sigil's mouse sequences; one drag at a time, tracked on the document
 * with its own namespace and unbound when it ends or the component is
 * destroyed. The arrangement is the value: JSON in data-ah-value (and the
 * hidden input), rewritten after every change (commit).
 */
(function ($, AH) {
  "use strict";

  var DISTANCE = 5;          // px of movement before a press becomes a drag

  AH.lib = AH.lib || {};
  // seq: one counter for drag namespaces and the ids of new groups,
  // float windows and menus (next())
  var L = AH.lib.dock = { seq: 0 };

  function inside(x, y, r) {
    return x >= r.left && x <= r.right && y >= r.top && y <= r.bottom;
  }

  function parse(json) {
    try { return JSON.parse(json || "null"); } catch (e) { return null; }
  }

  function commit(el, json, fire) {
    if (json === el.getAttribute("data-ah-value")) { return false; }
    el.setAttribute("data-ah-value", json);
    $(el).children("input[type=hidden]").val(json);
    if (fire) { $(el).trigger("change"); }
    return true;
  }

  // A component event with its payload in data-* attributes of the root,
  // which the action receives as Event.data.
  function fire(el, type, key, value) {
    el.setAttribute("data-" + key, value);
    $(el).trigger(type);
  }

  // Follow one pointer press on the document: `start' after DISTANCE px
  // (or at once with threshold 0; returning false gives up), then `move',
  // then `end(ev, cancelled)' on release, pointercancel or Escape.
  function track(owner, e, h, threshold) {
    var ns = ".ahdock" + (++L.seq);
    var sx = e.clientX, sy = e.clientY, started = false;
    var min = threshold === undefined ? DISTANCE : threshold;
    var $doc = $(document);
    function stop() {
      $doc.off(ns);
      $(document.documentElement).removeClass("ah-dock-dragging");
      $.removeData(owner, "ah-dock-stop");
    }
    function begin(ev) {
      started = true;
      if (h.start(ev) === false) { stop(); return false; }
      $(document.documentElement).addClass("ah-dock-dragging");
      return true;
    }
    var prev = $.data(owner, "ah-dock-stop");
    if (prev) { prev(); }
    $.data(owner, "ah-dock-stop", function () {
      stop();
      if (started) { h.end(null, true); }
    });
    if (min === 0 && !begin(e)) { return; }
    $doc.on("pointermove" + ns, function (ev) {
      if (!started) {
        if (Math.abs(ev.clientX - sx) < min && Math.abs(ev.clientY - sy) < min) { return; }
        if (!begin(ev)) { return; }
      }
      ev.preventDefault();
      h.move(ev);
    });
    $doc.on("pointerup" + ns + " pointercancel" + ns, function (ev) {
      stop();
      if (started) { h.end(ev, ev.type === "pointercancel"); }
    });
    $doc.on("keydown" + ns, function (ev) {
      if (ev.key === "Escape" && started) {
        ev.preventDefault();
        stop();
        h.end(ev, true);
      }
    });
  }

  function stopDrag(el) {
    var stop = $.data(el, "ah-dock-stop");
    if (stop) { stop(); }
  }

  function own(el, node, rootSel) { return $(node).closest(rootSel)[0] === el; }

  L.inside = inside;
  L.parse = parse;
  L.commit = commit;
  L.fire = fire;
  L.track = track;
  L.stopDrag = stopDrag;
  L.own = own;
  L.next = function () { return ++L.seq; };
})(window.jQuery, window.AH);
