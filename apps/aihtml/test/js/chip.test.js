/* chip behaviour (chip.js). Fixtures are reduced copies of aihtml_chip's
 * render (removable and clickable chips). */
(function (T, AH) {
  "use strict";

  function chip(v, opts) {
    opts = opts || {};
    return '<span class="ah-chip" data-disabled="' + !!opts.disabled + '" data-clickable="' + !!opts.click +
      '" data-ah="chip" data-ah-value="' + v + '" tabindex="0"' + (opts.click ? ' role="button"' : "") +
      '><span class="ah-chip__label">' + v + "</span>" +
      (opts.click ? "" : '<button class="ah-chip__delete" type="button" aria-label="Remove" tabindex="-1">×</button>') +
      "</span>";
  }

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function log(el) {
    var seen = [];
    el.addEventListener("ah:remove", function (e) { seen.push("remove:" + e.detail.value); });
    el.addEventListener("change", function () { seen.push("change:" + el.getAttribute("data-ah-value")); });
    return seen;
  }
  function tick() { return new Promise(function (res) { setTimeout(res, 0); }); }

  T.test("chip: the delete button fires ah:remove, change, then removes", async function (fx) {
    var el = await mount(fx, chip("erlang")), seen = log(el);
    el.querySelector(".ah-chip__delete").click();
    T.eq(seen, ["remove:erlang", "change:erlang"]);
    T.ok(!el.isConnected, "removed");
  });

  T.test("chip: Backspace removes and focuses the next chip; cancel keeps it", async function (fx) {
    fx.innerHTML = chip("a") + chip("b");
    await T.ready(fx);
    var a = fx.children[0], b = fx.children[1];
    var keep = function (e) { e.preventDefault(); };
    b.addEventListener("ah:remove", keep);
    b.focus();
    T.key(b, "Delete");
    T.ok(b.isConnected, "cancelled");
    a.focus();
    T.key(a, "Backspace");
    T.ok(!a.isConnected, "removed");
    T.ok(document.activeElement === b, "focus moves to the next chip");
  });

  T.test("chip: disabled ignores; clickable Enter clicks; remove() from the server", async function (fx) {
    var el = await mount(fx, chip("x", { disabled: true }));
    el.querySelector(".ah-chip__delete").click();
    T.ok(el.isConnected, "disabled");
    el = await mount(fx, chip("c", { click: true }));
    var clicks = 0;
    el.addEventListener("click", function () { clicks++; });
    T.key(el, "Enter");
    T.key(el, " ");
    T.eq(clicks, 2);
    el = await mount(fx, chip("m"));
    var seen = log(el);
    AH.invoke(el, "remove");
    T.eq(seen, ["remove:m", "change:m"]);
    T.ok(!el.isConnected);
  });

  T.test("chip: removed and inserted again, it works", async function (fx) {
    var el = await mount(fx, chip("r"));
    el.remove();
    await tick();
    fx.appendChild(el);
    await T.ready(fx);
    var seen = log(el);
    el.querySelector(".ah-chip__delete").click();
    T.eq(seen, ["remove:r", "change:r"]);
  });
})(window.AHTest, window.AH);
