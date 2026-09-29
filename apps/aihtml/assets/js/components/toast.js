/* The toast page function (designs/04-components.md), ported from sigil's
 * overlay/toast: AH.fn("toast") (also AH.toast) pops a card built from
 * templates/notification.mustache and templates/toast.mustache, and a
 * click on a [data-ah-toast] element (shows_toast/2) pops one without a
 * server round trip. Cards: _lib_overlay.js. */
// ah-load: [data-ah-toast]
import AH from "../core.js";
import "./_lib_overlay.js";
import "virtual:ah-tpl/notification";
import "virtual:ah-tpl/toast";

var L = AH.lib.overlay;
var cardView = L.cardView,
    duration = L.duration,
    showCard = L.showCard;

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

AH.fn("toast", toast);
AH.toast = toast;

document.addEventListener("click", function (e) {
  L.matching(e.target, "[data-ah-toast]").forEach(function (t) {
    function a(n) { var v = t.getAttribute(n); return v === null ? undefined : v; }
    toast({
      title: a("data-ah-toast"),
      description: a("data-ah-toast-description"),
      variant: a("data-ah-toast-variant"),
      duration: a("data-ah-toast-duration"),
      position: a("data-ah-toast-position"),
      closable: a("data-ah-toast-closable")
    });
  });
});
