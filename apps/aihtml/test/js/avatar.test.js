/* avatar behaviour (avatar.js). Fixtures are reduced copies of
 * aihtml_avatar's render (an image over the fallback initials). */
(function (T, AH) {
  "use strict";

  function avatar(src) {
    return '<span class="ah-avatar" data-size="lg" data-shape="circle" data-color="primary" data-ah="avatar">' +
      '<img class="ah-avatar__image" src="' + src + '" alt="Jane">' +
      '<span class="ah-avatar__fallback" aria-hidden="true">JD</span></span>';
  }
  var GOOD = "data:image/svg+xml;utf8,%3Csvg xmlns='http://www.w3.org/2000/svg' width='4' height='4'/%3E";
  var BAD = "data:image/png;base64,broken";

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function loaded(img) {
    return new Promise(function (res) {
      if (img.complete) { res(); return; }
      img.addEventListener("load", res);
      img.addEventListener("error", function () { setTimeout(res, 0); });
    });
  }
  function tick() { return new Promise(function (res) { setTimeout(res, 0); }); }

  T.test("avatar: a broken image is marked, the fallback shows", async function (fx) {
    var el = await mount(fx, avatar(BAD));
    var img = el.querySelector("img");
    await loaded(img);
    T.ok(img.classList.contains("ah-avatar__image--broken"), "marked broken");
  });

  T.test("avatar: an image that loads stays; an error later still marks it", async function (fx) {
    var el = await mount(fx, avatar(GOOD));
    var img = el.querySelector("img");
    await loaded(img);
    T.ok(!img.classList.contains("ah-avatar__image--broken"), "not broken");
    T.fire(img, "error", { bubbles: false });
    T.ok(img.classList.contains("ah-avatar__image--broken"), "error listener");
  });

  T.test("avatar: removed and inserted again, it works; teardown unbinds", async function (fx) {
    var el = await mount(fx, avatar(GOOD));
    var img = el.querySelector("img");
    await loaded(img);
    el.remove();
    await tick();
    T.fire(img, "error", { bubbles: false });
    T.ok(!img.classList.contains("ah-avatar__image--broken"), "listener gone after teardown");
    fx.appendChild(el);
    await T.ready(fx);
    T.fire(img, "error", { bubbles: false });
    T.ok(img.classList.contains("ah-avatar__image--broken"), "bound again");
  });
})(window.AHTest, window.AH);
