/* tag-cloud behaviour (tag_cloud.js). Fixtures are reduced copies of
 * aihtml_tag_cloud's render (tags without a url, one with a url). */
(function (T, AH) {
  "use strict";

  function tag(i, label, w, href) {
    return '<li class="ah-tagcloud-item" data-index="' + i + '"><a class="ah-tagcloud-link"' +
      (href ? ' href="' + href + '"' : ' tabindex="0" role="button"') +
      ' data-ah-label="' + label + '" data-ah-weight="' + w + '">' + label + "</a></li>";
  }
  var CLOUD = '<div class="ah-tagcloud" data-ah="tag-cloud"><ul class="ah-tagcloud">' +
    tag(0, "Erlang", 40) + tag(1, "CSS", 15) + tag(2, "OTP", 35, "#otp") + "</ul></div>";

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function link(el, i) { return el.querySelectorAll(".ah-tagcloud-link")[i]; }
  function item(el, i) { return el.querySelectorAll(".ah-tagcloud-item")[i]; }
  function tick() { return new Promise(function (res) { setTimeout(res, 0); }); }

  T.test("tag-cloud: a click fires ah:tag-click; Enter on a button tag", async function (fx) {
    var el = await mount(fx, CLOUD), seen = [];
    el.addEventListener("ah:tag-click", function (e) { seen.push(e.detail); });
    link(el, 0).click();
    T.key(link(el, 1), "Enter");
    T.eq(seen, [{ label: "Erlang", value: 40, url: null, index: 0 },
                { label: "CSS", value: 15, url: null, index: 1 }]);
  });

  T.test("tag-cloud: a url is followed unless ah:tag-click is cancelled", async function (fx) {
    var el = await mount(fx, CLOUD);
    var prevented = [];
    // the last listener sees whether the behaviour prevented the navigation
    var watch = function (e) { prevented.push(e.defaultPrevented); e.preventDefault(); };
    document.addEventListener("click", watch);
    link(el, 2).click();
    var cancel = function (e) { e.preventDefault(); };
    el.addEventListener("ah:tag-click", cancel);
    link(el, 2).click();
    el.removeEventListener("ah:tag-click", cancel);
    link(el, 0).click();
    document.removeEventListener("click", watch);
    T.eq(prevented, [false, true, true]);
  });

  T.test("tag-cloud: hideItem / showItem; removed and inserted again, it works", async function (fx) {
    var el = await mount(fx, CLOUD);
    AH.invoke(el, "hideItem", 1);
    T.eq(getComputedStyle(item(el, 1)).display, "none");
    AH.invoke(el, "showItem", "1");
    T.ok(getComputedStyle(item(el, 1)).display !== "none", "shown");
    el.remove();
    await tick();
    fx.appendChild(el);
    await T.ready(fx);
    var n = 0;
    el.addEventListener("ah:tag-click", function () { n++; });
    link(el, 0).click();
    T.eq(n, 1);
  });
})(window.AHTest, window.AH);
