/* Gantt behaviour (gantt.js). The fixtures are server renders from
 * aihtml_gantt, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"gantt":"<div class=\"ah-gantt\" id=\"t-g\" data-ah=\"gantt\" style=\"height:300px;\" data-origin=\"2026-08-25\" data-column-width=\"60\" data-row-height=\"40\" data-editable data-collapsed=\"\"><div class=\"ah-gantt-sidebar\" style=\"width:250px;\"><div class=\"ah-gantt-sidebar-header\">Task</div><div class=\"ah-gantt-sidebar-body\" role=\"tree\" aria-label=\"Task\"><div class=\"ah-gantt-sidebar-row\" data-rowid=\"p\" data-level=\"0\" role=\"treeitem\" aria-level=\"1\" aria-expanded=\"true\" tabindex=\"0\" style=\"height:40px;padding-left:16px;\"><span class=\"ah-gantt-expand-icon ah-gantt-expand-icon-expanded\" aria-hidden=\"true\">▶</span><span class=\"ah-gantt-sidebar-label\">Phase</span></div><div class=\"ah-gantt-sidebar-row\" data-rowid=\"c1\" data-parent=\"p\" data-level=\"1\" role=\"treeitem\" aria-level=\"2\" tabindex=\"-1\" style=\"height:40px;padding-left:36px;\"><span class=\"ah-gantt-sidebar-label\">One</span></div><div class=\"ah-gantt-sidebar-row\" data-rowid=\"c2\" data-parent=\"p\" data-level=\"1\" role=\"treeitem\" aria-level=\"2\" tabindex=\"-1\" style=\"height:40px;padding-left:36px;\"><span class=\"ah-gantt-sidebar-label\">Two</span></div><div class=\"ah-gantt-sidebar-row\" data-rowid=\"z\" data-level=\"0\" role=\"treeitem\" aria-level=\"1\" tabindex=\"-1\" style=\"height:40px;padding-left:16px;\"><span class=\"ah-gantt-sidebar-label\">Z</span></div></div></div><div class=\"ah-gantt-main\"><div class=\"ah-gantt-timeline-header\" aria-hidden=\"true\"><div class=\"ah-gantt-header-months\"><div class=\"ah-gantt-month-cell\" style=\"width:420px;\">Aug 2026</div><div class=\"ah-gantt-month-cell\" style=\"width:1800px;\">Sep 2026</div><div class=\"ah-gantt-month-cell\" style=\"width:420px;\">Oct 2026</div></div><div class=\"ah-gantt-header-days\"><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">25</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">26</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">27</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">28</div><div class=\"ah-gantt-day-cell ah-gantt-day-weekend\" style=\"width:60px;\">29</div><div class=\"ah-gantt-day-cell ah-gantt-day-weekend\" style=\"width:60px;\">30</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">31</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">1</div><div class=\"ah-gantt-day-cell ah-gantt-day-today\" style=\"width:60px;\">2</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">3</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">4</div><div class=\"ah-gantt-day-cell ah-gantt-day-weekend\" style=\"width:60px;\">5</div><div class=\"ah-gantt-day-cell ah-gantt-day-weekend\" style=\"width:60px;\">6</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">7</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">8</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">9</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">10</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">11</div><div class=\"ah-gantt-day-cell ah-gantt-day-weekend\" style=\"width:60px;\">12</div><div class=\"ah-gantt-day-cell ah-gantt-day-weekend\" style=\"width:60px;\">13</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">14</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">15</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">16</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">17</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">18</div><div class=\"ah-gantt-day-cell ah-gantt-day-weekend\" style=\"width:60px;\">19</div><div class=\"ah-gantt-day-cell ah-gantt-day-weekend\" style=\"width:60px;\">20</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">21</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">22</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">23</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">24</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">25</div><div class=\"ah-gantt-day-cell ah-gantt-day-weekend\" style=\"width:60px;\">26</div><div class=\"ah-gantt-day-cell ah-gantt-day-weekend\" style=\"width:60px;\">27</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">28</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">29</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">30</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">1</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">2</div><div class=\"ah-gantt-day-cell ah-gantt-day-weekend\" style=\"width:60px;\">3</div><div class=\"ah-gantt-day-cell ah-gantt-day-weekend\" style=\"width:60px;\">4</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">5</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">6</div><div class=\"ah-gantt-day-cell\" style=\"width:60px;\">7</div></div></div><div class=\"ah-gantt-timeline-body\"><div class=\"ah-gantt-tasks-layer\" style=\"width:2640px;height:160px;\"><div class=\"ah-gantt-grid-row\" data-rowid=\"p\" style=\"height:40px;\"></div><div class=\"ah-gantt-grid-row\" data-rowid=\"c1\" style=\"height:40px;\"></div><div class=\"ah-gantt-grid-row\" data-rowid=\"c2\" style=\"height:40px;\"></div><div class=\"ah-gantt-grid-row\" data-rowid=\"z\" style=\"height:40px;\"></div><div class=\"ah-gantt-summary-bar\" data-rowid=\"p\" data-count=\"2\" hidden style=\"position:absolute;left:420px;top:4px;width:300px;height:32px;\"><div class=\"ah-gantt-summary-cap ah-gantt-summary-cap-left\"></div><div class=\"ah-gantt-summary-label\">2 tasks</div><div class=\"ah-gantt-summary-cap ah-gantt-summary-cap-right\"></div></div><div class=\"ah-gantt-task-bar\" data-taskid=\"a\" data-rowid=\"c1\" data-start=\"2026-09-01\" data-end=\"2026-09-03\" tabindex=\"0\" role=\"button\" aria-label=\"A: 2026-09-01 – 2026-09-03\" style=\"position:absolute;left:420px;top:44px;width:120px;height:32px;background:var(--ah-color-primary);\"><div class=\"ah-gantt-task-label\">A</div><div class=\"ah-gantt-resize-handle ah-gantt-resize-left\"></div><div class=\"ah-gantt-resize-handle ah-gantt-resize-right\"></div></div><div class=\"ah-gantt-task-bar\" data-taskid=\"b\" data-rowid=\"c2\" data-start=\"2026-09-03\" data-end=\"2026-09-06\" data-deps=\"a\" tabindex=\"0\" role=\"button\" aria-label=\"B: 2026-09-03 – 2026-09-06\" style=\"position:absolute;left:540px;top:84px;width:180px;height:32px;background:var(--ah-color-primary);\"><div class=\"ah-gantt-task-label\">B</div><div class=\"ah-gantt-resize-handle ah-gantt-resize-left\"></div><div class=\"ah-gantt-resize-handle ah-gantt-resize-right\"></div></div><div class=\"ah-gantt-task-bar\" data-taskid=\"c\" data-rowid=\"z\" data-start=\"2026-09-06\" data-end=\"2026-09-08\" data-deps=\"b\" tabindex=\"0\" role=\"button\" aria-label=\"C: 2026-09-06 – 2026-09-08\" style=\"position:absolute;left:720px;top:124px;width:120px;height:32px;background:var(--ah-color-primary);\"><div class=\"ah-gantt-task-label\">C</div><div class=\"ah-gantt-resize-handle ah-gantt-resize-left\"></div><div class=\"ah-gantt-resize-handle ah-gantt-resize-right\"></div></div></div><svg class=\"ah-gantt-deps-layer\" viewBox=\"0 0 2640 160\" aria-hidden=\"true\" style=\"position:absolute;top:0;left:0;pointer-events:none;width:2640px;height:160px;\"><path class=\"ah-gantt-dep-line\" d=\"M540,60 C570,60 510,100 540,100\" fill=\"none\" stroke=\"var(--ah-color-grey-400)\" stroke-width=\"1.5\" data-from=\"a\" data-to=\"b\"></path><path class=\"ah-gantt-dep-line\" d=\"M720,100 C750,100 690,140 720,140\" fill=\"none\" stroke=\"var(--ah-color-grey-400)\" stroke-width=\"1.5\" data-from=\"b\" data-to=\"c\"></path></svg><div class=\"ah-gantt-today-marker\" style=\"display:block;left:510px;\"></div></div></div></div>"};

  async function mount(fx, html) { fx.innerHTML = html; await T.ready(fx); return fx.firstChild; }
  function events(el, name) {
    var seen = [];
    el.addEventListener(name, function (e) {
      if (e.target === el) { seen.push(e.detail === null || e.detail === undefined ? el.getAttribute("data-ah-value") : e.detail); }
    });
    return seen;
  }
  function mouse(target, type, x, y) {
    T.fire(target, type, { clientX: x, clientY: y, button: 0 });
  }
  function q(el, sel) { return el.querySelector(sel); }

  T.test("gantt: collapse a parent row, summary bar, dependency paths", async function (fx) {
    var el = await mount(fx, FX.gantt), seen = events(el, "ah:row-expand");
    var bar = function (id) { return q(el, '[data-taskid="' + id + '"]'); };
    var path = function (f, t) { return q(el, '.ah-gantt-dep-line[data-from="' + f + '"][data-to="' + t + '"]').getAttribute("d"); };
    T.eq(bar("c").style.top, "124px");
    T.ok(path("a", "b").length > 0, "a -> b drawn");
    q(el, '[data-rowid="p"] .ah-gantt-expand-icon').click();
    T.eq(q(el, '.ah-gantt-sidebar-row[data-rowid="c1"]').hidden, true, "child hidden");
    T.eq(bar("a").hidden, true);
    T.eq(bar("c").style.top, "44px", "z moves up");
    var sum = q(el, ".ah-gantt-summary-bar");
    T.eq(sum.hidden, false);
    T.eq([sum.style.left, sum.style.width, sum.style.top], ["420px", "300px", "4px"]);
    T.eq(path("a", "b"), "");
    T.eq(el.getAttribute("data-collapsed"), "p");
    T.eq(seen, [{ row: "p", expanded: false }]);
    AH.invoke(el, "expandRow", "p");
    T.eq(bar("a").hidden, false);
    T.eq(sum.hidden, true);
    T.eq(seen.length, 1, "methods fire nothing");
  });

  T.test("gantt: keyboard moves, resizes and changes rows; ah:task-change", async function (fx) {
    var el = await mount(fx, FX.gantt), seen = events(el, "ah:task-change");
    var c = q(el, '[data-taskid="c"]');
    c.focus();
    T.key(c, "ArrowRight");
    T.eq([c.getAttribute("data-start"), c.getAttribute("data-end")], ["2026-09-07", "2026-09-09"]);
    T.eq(el.getAttribute("data-kind"), "move");
    T.key(c, "ArrowLeft", { shiftKey: true });
    T.eq(c.getAttribute("data-end"), "2026-09-08");
    T.key(c, "ArrowUp", { altKey: true });
    T.eq([c.getAttribute("data-rowid"), c.style.top], ["c2", "84px"]);
    T.eq(q(el, '.ah-gantt-dep-line[data-from="b"]').getAttribute("d"), "M720,100 L780,100", "same row: a line");
    T.eq(seen.map(function (d) { return d.kind + d.days; }), ["move1", "resize-1", "move0"]);
    T.eq([el.getAttribute("data-task"), el.getAttribute("data-row"), el.getAttribute("data-to")],
         ["c", "c2", "2026-09-08"]);
    var clicks = events(el, "ah:task-click");
    T.key(c, "Enter");
    T.eq(clicks, [{ task: "c" }]);
  });

  T.test("gantt: drag a bar by two days; tree keyboard", async function (fx) {
    var el = await mount(fx, FX.gantt), seen = events(el, "ah:task-change");
    var a = q(el, '[data-taskid="a"]'), r = a.getBoundingClientRect();
    mouse(q(a, ".ah-gantt-task-label"), "mousedown", r.left + 10, r.top + 5);
    mouse(document, "mousemove", r.left + 60, r.top + 5);
    mouse(document, "mousemove", r.left + 135, r.top + 5);
    T.eq(document.querySelectorAll(".ah-gantt-task-ghost").length, 1, "ghost follows");
    mouse(document, "mouseup", r.left + 135, r.top + 5);
    T.eq(document.querySelectorAll(".ah-gantt-task-ghost").length, 0);
    T.eq([a.getAttribute("data-start"), a.getAttribute("data-end")], ["2026-09-03", "2026-09-05"]);
    T.eq(seen.length, 1);
    var rows = el.querySelectorAll(".ah-gantt-sidebar-row");
    rows[0].focus();
    T.key(rows[0], "ArrowLeft");
    T.eq(rows[0].getAttribute("aria-expanded"), "false");
    T.key(rows[0], "ArrowDown");
    T.eq(document.activeElement, rows[3], "skips hidden rows");
    AH.invoke(el, "setTask", "a", "2026-09-10", "2026-09-12");
    T.eq(a.getAttribute("data-start"), "2026-09-10");
    T.eq(seen.length, 1);
  });

  T.test("gantt: row click, collapseRow, synced scrolling; removed and re-inserted", async function (fx) {
    var el = await mount(fx, FX.gantt), clicks = events(el, "ah:row-click");
    var row = q(el, '.ah-gantt-sidebar-row[data-rowid="p"]');
    row.click();
    T.eq(clicks, [{ row: "p" }]);
    T.eq([row.getAttribute("tabindex"), document.activeElement], ["0", row]);
    AH.invoke(el, "collapseRow", "p");
    T.eq(row.getAttribute("aria-expanded"), "false");
    T.eq(clicks.length, 1);
    var body = q(el, ".ah-gantt-timeline-body"), head = q(el, ".ah-gantt-timeline-header");
    body.scrollLeft = 30;
    await new Promise(function (res) { requestAnimationFrame(function () { setTimeout(res, 0); }); });
    T.eq(head.scrollLeft, body.scrollLeft, "the header follows the body");
    fx.removeChild(el);
    await new Promise(function (res) { setTimeout(res, 0); });
    fx.appendChild(el);
    await T.ready(fx);
    row.click();
    T.eq(clicks.length, 2, "one listener after re-insertion");
  });
})(window.AHTest, window.AH);
