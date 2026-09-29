/* Behaviour of the status_bar component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs. */
import AH from "../core.js";
import "./_lib_nav.js";

// ------------------------------------------------------------------
// status_bar: the count details float above their segment
// ------------------------------------------------------------------

AH.register("status-bar", class extends AH.Controller {
  setup() {
    // per popover: {float, hide} (the float handle and the hide timer)
    this.pops = new Map();
    const pops = this.pops;
    const entry = function (pop) {
      let s = pops.get(pop);
      if (!s) { s = { float: null, hide: null }; pops.set(pop, s); }
      return s;
    };
    const show = function (count) {
      const pop = count.querySelector(":scope > .ah-status-bar__popover");
      if (!pop) { return; }
      const s = entry(pop);
      clearTimeout(s.hide);
      if (!s.float) {
        s.float = AH.float(pop, count, { placement: "top", offset: 8 });
      }
    };
    const hide = function (count) {
      const pop = count.querySelector(":scope > .ah-status-bar__popover");
      if (!pop || count.matches(":hover") || count.contains(document.activeElement)) { return; }
      const s = entry(pop);
      // keep it in place while the CSS fade-out runs
      s.hide = setTimeout(function () {
        if (s.float) { s.float.stop(); s.float = null; }
      }, 200);
    };
    AH.lib.nav.hover(this, ".ah-status-bar__count", show, hide);
    this.delegate("focusin", ".ah-status-bar__count", function (e, count) { show(count); });
    this.delegate("focusout", ".ah-status-bar__count", function (e, count) { hide(count); });
  }

  teardown() {
    this.pops.forEach(function (s) {
      clearTimeout(s.hide);
      if (s.float) { s.float.stop(); }
    });
    this.pops.clear();
  }
});
