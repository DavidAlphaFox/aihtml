/* Runtime behaviour shared by all components (core.js). fetch() is stubbed:
   each test sees the action requests the page would send. */
(function (T, $, AH) {
  "use strict";

  var sent = [];
  window.fetch = function (url, opts) {
    sent.push(JSON.parse(opts.body));
    var body = 'data: {"type":"RUN_STARTED"}\n\ndata: {"type":"RUN_FINISHED"}\n\n';
    return Promise.resolve(new Response(body, { status: 200 }));
  };
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  var TOKEN = "AAAA.BBBB";

  T.test("data-ah-value wins over a native value", async function (fx) {
    sent = [];
    fx.innerHTML = '<button id="tb" value="native" data-ah-value="true" data-ah-on="click:' + TOKEN + '">x</button>';
    AH.mount(fx);
    document.getElementById("tb").click();
    await wait(20);
    T.eq(sent.length, 1);
    T.eq(sent[0].event.value, "true");
  });

  T.test("a change bubbling from inside a value-bearing root is not its change", async function (fx) {
    sent = [];
    fx.innerHTML = '<div id="root" data-ah-value="a" data-ah-on="change:' + TOKEN + '"><input id="inner"></div>';
    AH.mount(fx);
    $("#inner").trigger("change");
    await wait(20);
    T.eq(sent.length, 0, "bubbled change ignored");
    $("#root").trigger("change");
    await wait(20);
    T.eq(sent.length, 1, "the root's own change is sent");
    T.eq(sent[0].event.value, "a");
  });

  T.test("component events (ah:close) can carry actions", async function (fx) {
    sent = [];
    fx.innerHTML = '<div id="tabs" data-ah-on="ah:close:' + TOKEN + '"></div>';
    AH.mount(fx);                    // registers a listener for ah:close
    $("#tabs").trigger("ah:close");
    await wait(20);
    T.eq(sent.length, 1);
    T.eq(sent[0].action, TOKEN);
    T.eq(sent[0].event.type, "ah:close");
  });

  T.test("call ops run behaviour methods and page functions", function (fx) {
    var calls = [];
    AH.define("t-call", { methods: { open: function (el, $el, a, b) { calls.push([el.id, a, b]); } } });
    AH.fn("t-global", function (x) { calls.push(["global", x]); });
    fx.innerHTML = '<div id="w" data-ah="t-call"></div>';
    AH.mount(fx);
    AH.apply([{ op: "call", id: "w", method: "open", args: [1, "two"] },
              { op: "call", method: "t-global", args: [{ k: 3 }] }]);
    T.eq(calls, [["w", 1, "two"], ["global", { k: 3 }]]);
  });

  T.test("shared templates are compiled into AH.tpl", function () {
    var html = AH.tpl.tag_input_chip({ variant: "soft", color: "primary", index: 2,
                                        label: "<x>", disabled: true });
    T.ok(html.indexOf("&lt;x&gt;") > 0, "escaped");
    T.ok(/ disabled>/.test(html), "section rendered");
    T.ok(!/\n$/.test(html), "no trailing newline");
  });
})(window.AHTest, window.jQuery, window.AH);
