/* Behaviour of repeat_button (designs/04-components.md), after sigil's
 * form/repeat_button.
 *
 *   repeat-button    click on press, then every interval ms after delay ms
 *                    while held; the browser's click on release is dropped
 *
 * The server renders the whole first state, so init only binds events.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

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
