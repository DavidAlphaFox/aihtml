/* ranking-list behaviour (ranking_list.js). Fixtures are reduced copies
 * of aihtml_ranking_list's render (clickable rows). */
(function (T, AH) {
  "use strict";

  function item(i, name) {
    return '<div class="ah-ranking-list__item ah-ranking-list__item--clickable" data-idx="' + i +
      '" role="button" tabindex="0"><span class="ah-ranking-list__rank">' + (i + 1) + '</span>' +
      '<div class="ah-ranking-list__content"><span class="ah-ranking-list__primary">' + name + "</span></div></div>";
  }
  var LIST = '<div class="ah-ranking-list" data-ah="ranking-list"><div class="ah-ranking-list__list">' +
    item(0, "Erlang") + item(1, "Elixir") + item(2, "Gleam") + "</div></div>";

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function log(el) {
    var seen = [];
    el.addEventListener("ah:item-click", function (e) { seen.push(e.detail.index); });
    return seen;
  }
  function rows(el) { return el.querySelectorAll(".ah-ranking-list__item"); }
  function tick() { return new Promise(function (res) { setTimeout(res, 0); }); }

  T.test("ranking-list: clicks and Enter / Space fire ah:item-click {index}", async function (fx) {
    var el = await mount(fx, LIST), seen = log(el);
    rows(el)[1].querySelector(".ah-ranking-list__primary").click();
    T.key(rows(el)[2], "Enter");
    T.key(rows(el)[0], " ");
    T.key(rows(el)[0], "a");
    T.eq(seen, [1, 2, 0]);
  });

  T.test("ranking-list: removed and inserted again, it works", async function (fx) {
    var el = await mount(fx, LIST);
    el.remove();
    await tick();
    fx.appendChild(el);
    await T.ready(fx);
    var seen = log(el);
    rows(el)[2].click();
    T.eq(seen, [2], "one listener, not two");
  });
})(window.AHTest, window.AH);
