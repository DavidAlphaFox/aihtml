/* timeline behaviour (timeline.js). Fixtures are reduced copies of
 * aihtml_timeline's render (collapsible cards, three grid cells per item). */
(function (T, AH) {
  "use strict";

  function card(title, open) {
    return '<div class="ah-timeline-item' + (open ? " ah-timeline-item-expanded" : "") +
      '" ah-collapsible role="button" tabindex="0" aria-expanded="' + !!open + '"><div class="ah-timeline-item-content">' +
      '<div class="ah-timeline-item-title">' + title + '</div><div class="ah-timeline-item-description">d</div></div></div>';
  }
  var LINE = '<div class="ah-timeline ah-collapsible" data-ah="timeline"><div class="ah-timeline-container">' +
    '<div class="ah-timeline-near-cell"><div class="ah-timeline-date">2026-01</div></div>' +
    '<div class="ah-timeline-track-cell"></div><div class="ah-timeline-far-cell">' + card("Start") + "</div>" +
    '<div class="ah-timeline-near-cell">' + card("Alpha", true) + "</div>" +
    '<div class="ah-timeline-track-cell"></div><div class="ah-timeline-far-cell"><div class="ah-timeline-date">2026-03</div></div>' +
    '<div class="ah-timeline-near-cell"></div><div class="ah-timeline-track-cell"></div>' +
    '<div class="ah-timeline-far-cell"><div class="ah-timeline-item"><div class="ah-timeline-item-title">Beta</div></div></div>' +
    "</div></div>";

  async function mount(fx, html) {
    fx.innerHTML = html;
    await T.ready(fx);
    return fx.firstChild;
  }
  function cards(el) { return el.querySelectorAll(".ah-timeline-item"); }
  function log(el) {
    var seen = [];
    el.addEventListener("ah:toggle", function (e) { seen.push(e.detail); });
    return seen;
  }
  function tick() { return new Promise(function (res) { setTimeout(res, 0); }); }

  T.test("timeline: click and Enter toggle a card; ah:toggle {expanded, index}", async function (fx) {
    var el = await mount(fx, LINE), seen = log(el);
    var c = cards(el);
    c[0].querySelector(".ah-timeline-item-title").click();
    T.ok(c[0].classList.contains("ah-timeline-item-expanded"));
    T.eq(c[0].getAttribute("aria-expanded"), "true");
    T.key(c[1], "Enter");
    T.ok(!c[1].classList.contains("ah-timeline-item-expanded"));
    T.eq(c[1].getAttribute("aria-expanded"), "false");
    c[2].click();
    T.eq(seen, [{ expanded: true, index: 0 }, { expanded: false, index: 1 }], "plain cards do nothing");
  });

  T.test("timeline: removed and inserted again, it works", async function (fx) {
    var el = await mount(fx, LINE);
    el.remove();
    await tick();
    fx.appendChild(el);
    await T.ready(fx);
    var seen = log(el);
    cards(el)[0].click();
    T.eq(seen, [{ expanded: true, index: 0 }]);
  });
})(window.AHTest, window.AH);
