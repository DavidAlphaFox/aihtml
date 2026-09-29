/* pagination: the page list the browser re-renders (shared template
 * pagination_items) equals what the server renders for the same state.
 * SERVER holds server renders of aihtml_pagination:pagination/4 (id "p"),
 * generated from Erlang; regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
  "p1": "<div class=\"ah-pagination\" role=\"navigation\" aria-label=\"Pagination\" data-ah=\"pagination\" data-ah-value=\"1\" data-total=\"200\" data-page-size=\"10\" data-max-visible=\"7\" id=\"p\"><ul class=\"ah-pagination-pages\" data-view=\"{&quot;simple&quot;:false,&quot;labels&quot;:{&quot;first&quot;:&quot;First&quot;,&quot;last&quot;:&quot;Last&quot;,&quot;next&quot;:&quot;Next&quot;,&quot;prev&quot;:&quot;Previous&quot;,&quot;page_info&quot;:&quot;Page {0} / {1}&quot;},&quot;href&quot;:null,&quot;first_last&quot;:true}\"><li class=\"ah-pagination-nav ah-pagination-first-last ah-pagination-nav-disabled\" data-type=\"first\" role=\"button\" tabindex=\"-1\" aria-label=\"First\" aria-disabled=\"true\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">«</span><span class=\"ah-pagination-nav-label\">First</span></li><li class=\"ah-pagination-nav ah-pagination-nav-disabled\" data-type=\"prev\" role=\"button\" tabindex=\"-1\" aria-label=\"Previous\" aria-disabled=\"true\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">‹</span><span class=\"ah-pagination-nav-label\">Previous</span></li><li class=\"ah-pagination-item ah-pagination-item-active\" data-page=\"1\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 1\" aria-current=\"page\">1</li><li class=\"ah-pagination-item\" data-page=\"2\" role=\"button\" tabindex=\"0\" aria-label=\"Page 2\">2</li><li class=\"ah-pagination-item\" data-page=\"3\" role=\"button\" tabindex=\"0\" aria-label=\"Page 3\">3</li><li class=\"ah-pagination-item\" data-page=\"4\" role=\"button\" tabindex=\"0\" aria-label=\"Page 4\">4</li><li class=\"ah-pagination-item\" data-page=\"5\" role=\"button\" tabindex=\"0\" aria-label=\"Page 5\">5</li><li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li><li class=\"ah-pagination-item\" data-page=\"20\" role=\"button\" tabindex=\"0\" aria-label=\"Page 20\">20</li><li class=\"ah-pagination-nav\" data-type=\"next\" role=\"button\" tabindex=\"0\" aria-label=\"Next\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">›</span><span class=\"ah-pagination-nav-label\">Next</span></li><li class=\"ah-pagination-nav ah-pagination-first-last\" data-type=\"last\" role=\"button\" tabindex=\"0\" aria-label=\"Last\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">»</span><span class=\"ah-pagination-nav-label\">Last</span></li></ul></div>",
  "p10": "<div class=\"ah-pagination\" role=\"navigation\" aria-label=\"Pagination\" data-ah=\"pagination\" data-ah-value=\"10\" data-total=\"200\" data-page-size=\"10\" data-max-visible=\"7\" id=\"p\"><ul class=\"ah-pagination-pages\" data-view=\"{&quot;simple&quot;:false,&quot;labels&quot;:{&quot;first&quot;:&quot;First&quot;,&quot;last&quot;:&quot;Last&quot;,&quot;next&quot;:&quot;Next&quot;,&quot;prev&quot;:&quot;Previous&quot;,&quot;page_info&quot;:&quot;Page {0} / {1}&quot;},&quot;href&quot;:null,&quot;first_last&quot;:true}\"><li class=\"ah-pagination-nav ah-pagination-first-last\" data-type=\"first\" role=\"button\" tabindex=\"0\" aria-label=\"First\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">«</span><span class=\"ah-pagination-nav-label\">First</span></li><li class=\"ah-pagination-nav\" data-type=\"prev\" role=\"button\" tabindex=\"0\" aria-label=\"Previous\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">‹</span><span class=\"ah-pagination-nav-label\">Previous</span></li><li class=\"ah-pagination-item\" data-page=\"1\" role=\"button\" tabindex=\"0\" aria-label=\"Page 1\">1</li><li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li><li class=\"ah-pagination-item\" data-page=\"9\" role=\"button\" tabindex=\"0\" aria-label=\"Page 9\">9</li><li class=\"ah-pagination-item ah-pagination-item-active\" data-page=\"10\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 10\" aria-current=\"page\">10</li><li class=\"ah-pagination-item\" data-page=\"11\" role=\"button\" tabindex=\"0\" aria-label=\"Page 11\">11</li><li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li><li class=\"ah-pagination-item\" data-page=\"20\" role=\"button\" tabindex=\"0\" aria-label=\"Page 20\">20</li><li class=\"ah-pagination-nav\" data-type=\"next\" role=\"button\" tabindex=\"0\" aria-label=\"Next\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">›</span><span class=\"ah-pagination-nav-label\">Next</span></li><li class=\"ah-pagination-nav ah-pagination-first-last\" data-type=\"last\" role=\"button\" tabindex=\"0\" aria-label=\"Last\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">»</span><span class=\"ah-pagination-nav-label\">Last</span></li></ul></div>",
  "p17": "<div class=\"ah-pagination\" role=\"navigation\" aria-label=\"Pagination\" data-ah=\"pagination\" data-ah-value=\"17\" data-total=\"200\" data-page-size=\"10\" data-max-visible=\"7\" id=\"p\"><ul class=\"ah-pagination-pages\" data-view=\"{&quot;simple&quot;:false,&quot;labels&quot;:{&quot;first&quot;:&quot;First&quot;,&quot;last&quot;:&quot;Last&quot;,&quot;next&quot;:&quot;Next&quot;,&quot;prev&quot;:&quot;Previous&quot;,&quot;page_info&quot;:&quot;Page {0} / {1}&quot;},&quot;href&quot;:null,&quot;first_last&quot;:true}\"><li class=\"ah-pagination-nav ah-pagination-first-last\" data-type=\"first\" role=\"button\" tabindex=\"0\" aria-label=\"First\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">«</span><span class=\"ah-pagination-nav-label\">First</span></li><li class=\"ah-pagination-nav\" data-type=\"prev\" role=\"button\" tabindex=\"0\" aria-label=\"Previous\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">‹</span><span class=\"ah-pagination-nav-label\">Previous</span></li><li class=\"ah-pagination-item\" data-page=\"1\" role=\"button\" tabindex=\"0\" aria-label=\"Page 1\">1</li><li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li><li class=\"ah-pagination-item\" data-page=\"16\" role=\"button\" tabindex=\"0\" aria-label=\"Page 16\">16</li><li class=\"ah-pagination-item ah-pagination-item-active\" data-page=\"17\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 17\" aria-current=\"page\">17</li><li class=\"ah-pagination-item\" data-page=\"18\" role=\"button\" tabindex=\"0\" aria-label=\"Page 18\">18</li><li class=\"ah-pagination-item\" data-page=\"19\" role=\"button\" tabindex=\"0\" aria-label=\"Page 19\">19</li><li class=\"ah-pagination-item\" data-page=\"20\" role=\"button\" tabindex=\"0\" aria-label=\"Page 20\">20</li><li class=\"ah-pagination-nav\" data-type=\"next\" role=\"button\" tabindex=\"0\" aria-label=\"Next\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">›</span><span class=\"ah-pagination-nav-label\">Next</span></li><li class=\"ah-pagination-nav ah-pagination-first-last\" data-type=\"last\" role=\"button\" tabindex=\"0\" aria-label=\"Last\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">»</span><span class=\"ah-pagination-nav-label\">Last</span></li></ul></div>",
  "p20": "<div class=\"ah-pagination\" role=\"navigation\" aria-label=\"Pagination\" data-ah=\"pagination\" data-ah-value=\"20\" data-total=\"200\" data-page-size=\"10\" data-max-visible=\"7\" id=\"p\"><ul class=\"ah-pagination-pages\" data-view=\"{&quot;simple&quot;:false,&quot;labels&quot;:{&quot;first&quot;:&quot;First&quot;,&quot;last&quot;:&quot;Last&quot;,&quot;next&quot;:&quot;Next&quot;,&quot;prev&quot;:&quot;Previous&quot;,&quot;page_info&quot;:&quot;Page {0} / {1}&quot;},&quot;href&quot;:null,&quot;first_last&quot;:true}\"><li class=\"ah-pagination-nav ah-pagination-first-last\" data-type=\"first\" role=\"button\" tabindex=\"0\" aria-label=\"First\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">«</span><span class=\"ah-pagination-nav-label\">First</span></li><li class=\"ah-pagination-nav\" data-type=\"prev\" role=\"button\" tabindex=\"0\" aria-label=\"Previous\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">‹</span><span class=\"ah-pagination-nav-label\">Previous</span></li><li class=\"ah-pagination-item\" data-page=\"1\" role=\"button\" tabindex=\"0\" aria-label=\"Page 1\">1</li><li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li><li class=\"ah-pagination-item\" data-page=\"16\" role=\"button\" tabindex=\"0\" aria-label=\"Page 16\">16</li><li class=\"ah-pagination-item\" data-page=\"17\" role=\"button\" tabindex=\"0\" aria-label=\"Page 17\">17</li><li class=\"ah-pagination-item\" data-page=\"18\" role=\"button\" tabindex=\"0\" aria-label=\"Page 18\">18</li><li class=\"ah-pagination-item\" data-page=\"19\" role=\"button\" tabindex=\"0\" aria-label=\"Page 19\">19</li><li class=\"ah-pagination-item ah-pagination-item-active\" data-page=\"20\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 20\" aria-current=\"page\">20</li><li class=\"ah-pagination-nav ah-pagination-nav-disabled\" data-type=\"next\" role=\"button\" tabindex=\"-1\" aria-label=\"Next\" aria-disabled=\"true\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">›</span><span class=\"ah-pagination-nav-label\">Next</span></li><li class=\"ah-pagination-nav ah-pagination-first-last ah-pagination-nav-disabled\" data-type=\"last\" role=\"button\" tabindex=\"-1\" aria-label=\"Last\" aria-disabled=\"true\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">»</span><span class=\"ah-pagination-nav-label\">Last</span></li></ul></div>",
  "s1": "<div class=\"ah-pagination ah-pagination-simple\" role=\"navigation\" aria-label=\"Pagination\" data-ah=\"pagination\" data-ah-value=\"1\" data-total=\"95\" data-page-size=\"10\" data-max-visible=\"7\" id=\"p\"><ul class=\"ah-pagination-pages\" data-view=\"{&quot;simple&quot;:true,&quot;labels&quot;:{&quot;first&quot;:&quot;First&quot;,&quot;last&quot;:&quot;Last&quot;,&quot;next&quot;:&quot;Next&quot;,&quot;prev&quot;:&quot;Previous&quot;,&quot;page_info&quot;:&quot;&lt;b&gt;{0}&lt;/b&gt; of {1}&quot;},&quot;href&quot;:null,&quot;first_last&quot;:false}\"><li class=\"ah-pagination-nav ah-pagination-nav-disabled\" data-type=\"prev\" role=\"button\" tabindex=\"-1\" aria-label=\"Previous\" aria-disabled=\"true\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">‹</span><span class=\"ah-pagination-nav-label\">Previous</span></li><li class=\"ah-pagination-simple-info\">&lt;b&gt;1&lt;/b&gt; of 10</li><li class=\"ah-pagination-nav\" data-type=\"next\" role=\"button\" tabindex=\"0\" aria-label=\"Next\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">›</span><span class=\"ah-pagination-nav-label\">Next</span></li></ul></div>",
  "s4": "<div class=\"ah-pagination ah-pagination-simple\" role=\"navigation\" aria-label=\"Pagination\" data-ah=\"pagination\" data-ah-value=\"4\" data-total=\"95\" data-page-size=\"10\" data-max-visible=\"7\" id=\"p\"><ul class=\"ah-pagination-pages\" data-view=\"{&quot;simple&quot;:true,&quot;labels&quot;:{&quot;first&quot;:&quot;First&quot;,&quot;last&quot;:&quot;Last&quot;,&quot;next&quot;:&quot;Next&quot;,&quot;prev&quot;:&quot;Previous&quot;,&quot;page_info&quot;:&quot;&lt;b&gt;{0}&lt;/b&gt; of {1}&quot;},&quot;href&quot;:null,&quot;first_last&quot;:false}\"><li class=\"ah-pagination-nav\" data-type=\"prev\" role=\"button\" tabindex=\"0\" aria-label=\"Previous\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">‹</span><span class=\"ah-pagination-nav-label\">Previous</span></li><li class=\"ah-pagination-simple-info\">&lt;b&gt;4&lt;/b&gt; of 10</li><li class=\"ah-pagination-nav\" data-type=\"next\" role=\"button\" tabindex=\"0\" aria-label=\"Next\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">›</span><span class=\"ah-pagination-nav-label\">Next</span></li></ul></div>",
  "sib1": "<div class=\"ah-pagination\" role=\"navigation\" aria-label=\"Pagination\" data-ah=\"pagination\" data-ah-value=\"1\" data-total=\"300\" data-page-size=\"10\" data-max-visible=\"9\" id=\"p\"><ul class=\"ah-pagination-pages\" data-view=\"{&quot;simple&quot;:false,&quot;labels&quot;:{&quot;first&quot;:&quot;First&quot;,&quot;last&quot;:&quot;Last&quot;,&quot;next&quot;:&quot;Next&quot;,&quot;prev&quot;:&quot;Previous&quot;,&quot;page_info&quot;:&quot;Page {0} / {1}&quot;},&quot;href&quot;:null,&quot;first_last&quot;:false}\"><li class=\"ah-pagination-nav ah-pagination-nav-disabled\" data-type=\"prev\" role=\"button\" tabindex=\"-1\" aria-label=\"Previous\" aria-disabled=\"true\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">‹</span><span class=\"ah-pagination-nav-label\">Previous</span></li><li class=\"ah-pagination-item ah-pagination-item-active\" data-page=\"1\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 1\" aria-current=\"page\">1</li><li class=\"ah-pagination-item\" data-page=\"2\" role=\"button\" tabindex=\"0\" aria-label=\"Page 2\">2</li><li class=\"ah-pagination-item\" data-page=\"3\" role=\"button\" tabindex=\"0\" aria-label=\"Page 3\">3</li><li class=\"ah-pagination-item\" data-page=\"4\" role=\"button\" tabindex=\"0\" aria-label=\"Page 4\">4</li><li class=\"ah-pagination-item\" data-page=\"5\" role=\"button\" tabindex=\"0\" aria-label=\"Page 5\">5</li><li class=\"ah-pagination-item\" data-page=\"6\" role=\"button\" tabindex=\"0\" aria-label=\"Page 6\">6</li><li class=\"ah-pagination-item\" data-page=\"7\" role=\"button\" tabindex=\"0\" aria-label=\"Page 7\">7</li><li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li><li class=\"ah-pagination-item\" data-page=\"30\" role=\"button\" tabindex=\"0\" aria-label=\"Page 30\">30</li><li class=\"ah-pagination-nav\" data-type=\"next\" role=\"button\" tabindex=\"0\" aria-label=\"Next\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">›</span><span class=\"ah-pagination-nav-label\">Next</span></li></ul></div>",
  "sib15": "<div class=\"ah-pagination\" role=\"navigation\" aria-label=\"Pagination\" data-ah=\"pagination\" data-ah-value=\"15\" data-total=\"300\" data-page-size=\"10\" data-max-visible=\"9\" id=\"p\"><ul class=\"ah-pagination-pages\" data-view=\"{&quot;simple&quot;:false,&quot;labels&quot;:{&quot;first&quot;:&quot;First&quot;,&quot;last&quot;:&quot;Last&quot;,&quot;next&quot;:&quot;Next&quot;,&quot;prev&quot;:&quot;Previous&quot;,&quot;page_info&quot;:&quot;Page {0} / {1}&quot;},&quot;href&quot;:null,&quot;first_last&quot;:false}\"><li class=\"ah-pagination-nav\" data-type=\"prev\" role=\"button\" tabindex=\"0\" aria-label=\"Previous\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">‹</span><span class=\"ah-pagination-nav-label\">Previous</span></li><li class=\"ah-pagination-item\" data-page=\"1\" role=\"button\" tabindex=\"0\" aria-label=\"Page 1\">1</li><li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li><li class=\"ah-pagination-item\" data-page=\"13\" role=\"button\" tabindex=\"0\" aria-label=\"Page 13\">13</li><li class=\"ah-pagination-item\" data-page=\"14\" role=\"button\" tabindex=\"0\" aria-label=\"Page 14\">14</li><li class=\"ah-pagination-item ah-pagination-item-active\" data-page=\"15\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 15\" aria-current=\"page\">15</li><li class=\"ah-pagination-item\" data-page=\"16\" role=\"button\" tabindex=\"0\" aria-label=\"Page 16\">16</li><li class=\"ah-pagination-item\" data-page=\"17\" role=\"button\" tabindex=\"0\" aria-label=\"Page 17\">17</li><li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li><li class=\"ah-pagination-item\" data-page=\"30\" role=\"button\" tabindex=\"0\" aria-label=\"Page 30\">30</li><li class=\"ah-pagination-nav\" data-type=\"next\" role=\"button\" tabindex=\"0\" aria-label=\"Next\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">›</span><span class=\"ah-pagination-nav-label\">Next</span></li></ul></div>",
  "t200": "<div class=\"ah-pagination\" role=\"navigation\" aria-label=\"Pagination\" data-ah=\"pagination\" data-ah-value=\"3\" data-total=\"200\" data-page-size=\"10\" data-max-visible=\"7\" id=\"p\"><ul class=\"ah-pagination-pages\" data-view=\"{&quot;simple&quot;:false,&quot;labels&quot;:{&quot;first&quot;:&quot;First&quot;,&quot;last&quot;:&quot;Last&quot;,&quot;next&quot;:&quot;Next&quot;,&quot;prev&quot;:&quot;Previous&quot;,&quot;page_info&quot;:&quot;Page {0} / {1}&quot;},&quot;href&quot;:null,&quot;first_last&quot;:false}\"><li class=\"ah-pagination-nav\" data-type=\"prev\" role=\"button\" tabindex=\"0\" aria-label=\"Previous\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">‹</span><span class=\"ah-pagination-nav-label\">Previous</span></li><li class=\"ah-pagination-item\" data-page=\"1\" role=\"button\" tabindex=\"0\" aria-label=\"Page 1\">1</li><li class=\"ah-pagination-item\" data-page=\"2\" role=\"button\" tabindex=\"0\" aria-label=\"Page 2\">2</li><li class=\"ah-pagination-item ah-pagination-item-active\" data-page=\"3\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 3\" aria-current=\"page\">3</li><li class=\"ah-pagination-item\" data-page=\"4\" role=\"button\" tabindex=\"0\" aria-label=\"Page 4\">4</li><li class=\"ah-pagination-item\" data-page=\"5\" role=\"button\" tabindex=\"0\" aria-label=\"Page 5\">5</li><li class=\"ah-pagination-ellipsis\" aria-hidden=\"true\">…</li><li class=\"ah-pagination-item\" data-page=\"20\" role=\"button\" tabindex=\"0\" aria-label=\"Page 20\">20</li><li class=\"ah-pagination-nav\" data-type=\"next\" role=\"button\" tabindex=\"0\" aria-label=\"Next\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">›</span><span class=\"ah-pagination-nav-label\">Next</span></li></ul></div>",
  "t25": "<div class=\"ah-pagination\" role=\"navigation\" aria-label=\"Pagination\" data-ah=\"pagination\" data-ah-value=\"3\" data-total=\"25\" data-page-size=\"10\" data-max-visible=\"7\" id=\"p\"><ul class=\"ah-pagination-pages\" data-view=\"{&quot;simple&quot;:false,&quot;labels&quot;:{&quot;first&quot;:&quot;First&quot;,&quot;last&quot;:&quot;Last&quot;,&quot;next&quot;:&quot;Next&quot;,&quot;prev&quot;:&quot;Previous&quot;,&quot;page_info&quot;:&quot;Page {0} / {1}&quot;},&quot;href&quot;:null,&quot;first_last&quot;:false}\"><li class=\"ah-pagination-nav\" data-type=\"prev\" role=\"button\" tabindex=\"0\" aria-label=\"Previous\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">‹</span><span class=\"ah-pagination-nav-label\">Previous</span></li><li class=\"ah-pagination-item\" data-page=\"1\" role=\"button\" tabindex=\"0\" aria-label=\"Page 1\">1</li><li class=\"ah-pagination-item\" data-page=\"2\" role=\"button\" tabindex=\"0\" aria-label=\"Page 2\">2</li><li class=\"ah-pagination-item ah-pagination-item-active\" data-page=\"3\" role=\"button\" tabindex=\"-1\" aria-label=\"Page 3\" aria-current=\"page\">3</li><li class=\"ah-pagination-nav ah-pagination-nav-disabled\" data-type=\"next\" role=\"button\" tabindex=\"-1\" aria-label=\"Next\" aria-disabled=\"true\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">›</span><span class=\"ah-pagination-nav-label\">Next</span></li></ul></div>"
};

  // Markup with attributes and class tokens sorted, so that the order in
  // which the runtime adds them does not matter.
  function norm(node) {
    if (node.nodeType === 3) { return node.nodeValue; }
    var attrs = Array.prototype.map.call(node.attributes, function (a) {
      var v = a.name === "class" ? a.value.split(/\s+/).filter(Boolean).sort().join(" ") : a.value;
      return a.name + "=" + JSON.stringify(v);
    }).sort();
    return "<" + node.tagName.toLowerCase() + " " + attrs.join(" ") + ">" +
      Array.prototype.map.call(node.childNodes, norm).join("") + "</" + node.tagName.toLowerCase() + ">";
  }

  function server(name, sel) {
    var d = document.createElement("div");
    d.innerHTML = SERVER[name];
    return norm(d.querySelector(sel));
  }

  async function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    await T.ready(fx);
    return fx.firstChild;
  }

  function serverValue(name) {
    var d = document.createElement("div");
    d.innerHTML = SERVER[name];
    return d.firstChild.getAttribute("data-ah-value");
  }

  function samePages(el, name) {
    T.eq(norm(el.querySelector(".ah-pagination-pages")), server(name, ".ah-pagination-pages"), name);
    T.eq(el.getAttribute("data-ah-value"), serverValue(name), name + " value");
  }

  function nav(el, type) { return el.querySelector("[data-type=" + type + "]"); }

  T.test("pagination: setPage re-renders the list as the server does", async function (fx) {
    var el = await mount(fx, "p1");
    samePages(el, "p1");
    [["p10", 10], ["p17", 17], ["p20", 20], ["p1", 1]].forEach(function (c) {
      AH.invoke(el, "setPage", c[1]);
      samePages(el, c[0]);
    });
  });

  T.test("pagination: clicks and siblings give the server list", async function (fx) {
    var el = await mount(fx, "sib1");
    AH.invoke(el, "setPage", 15);
    samePages(el, "sib15");
    AH.invoke(el, "first");
    samePages(el, "sib1");
  });

  T.test("pagination: simple mode info text (escaped label)", async function (fx) {
    var el = await mount(fx, "s1");
    var changed = 0;
    el.addEventListener("change", function () { changed++; });
    nav(el, "next").click();
    nav(el, "next").click();
    nav(el, "next").click();
    samePages(el, "s4");
    T.eq(changed, 3);
  });

  T.test("pagination: setTotal clamps the page and re-renders", async function (fx) {
    var el = await mount(fx, "t200");
    AH.invoke(el, "setTotal", 25);
    samePages(el, "t25");
  });

  T.test("pagination: page items, keys and the value contract", async function (fx) {
    var el = await mount(fx, "p1");
    var seen = [];
    el.addEventListener("change", function (e) {
      if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); }
    });
    el.querySelector("[data-page='3']").click();
    T.eq(seen, ["3"]);
    T.eq(document.activeElement, el.querySelector(".ah-pagination-item-active"), "focus kept");
    T.key(nav(el, "last"), "Enter");
    T.eq(seen, ["3", "20"]);
    T.key(nav(el, "prev"), " ");
    T.eq(el.getAttribute("data-ah-value"), "19");
    nav(el, "next").click();
    nav(el, "next").click();
    T.eq(seen, ["3", "20", "19", "20"], "disabled next does nothing");
    AH.invoke(el, "prev");
    T.eq(AH.invoke(el, "value"), 19);
    T.eq(seen.length, 4, "methods fire no change");
    // taken out and put back: one listener
    fx.removeChild(el);
    await new Promise(function (r) { setTimeout(r, 0); });
    fx.appendChild(el);
    await T.ready(fx);
    el.querySelector("[data-page='1']").click();
    T.eq(seen, ["3", "20", "19", "20", "1"]);
  });

  // ---- links (href): crawlable pages, intercepted when an action renders ----

  // aihtml_pagination:pagination(95, 3, [], [{href, <<"/list?page={page}&size={size}">>},
  // {page_size, 20}, {show_size_selector, false}, {id, <<"p">>}])
  var LINKS = "<div class=\"ah-pagination ah-pagination-links\" role=\"navigation\" aria-label=\"Pagination\" data-ah=\"pagination\" data-ah-value=\"3\" data-total=\"95\" data-page-size=\"20\" data-max-visible=\"7\" data-href=\"/list?page={page}&amp;size={size}\" id=\"p\"><ul class=\"ah-pagination-pages\" data-view=\"{&quot;first_last&quot;:false,&quot;href&quot;:&quot;/list?page={page}&amp;size={size}&quot;,&quot;labels&quot;:{&quot;first&quot;:&quot;First&quot;,&quot;last&quot;:&quot;Last&quot;,&quot;next&quot;:&quot;Next&quot;,&quot;page_info&quot;:&quot;Page {0} / {1}&quot;,&quot;prev&quot;:&quot;Previous&quot;},&quot;simple&quot;:false}\"><li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav\" href=\"/list?page=2&amp;size=20\" data-type=\"prev\" aria-label=\"Previous\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">‹</span><span class=\"ah-pagination-nav-label\">Previous</span></a></li><li class=\"ah-pagination-li\"><a class=\"ah-pagination-item\" href=\"/list?page=1&amp;size=20\" data-page=\"1\" aria-label=\"Page 1\">1</a></li><li class=\"ah-pagination-li\"><a class=\"ah-pagination-item\" href=\"/list?page=2&amp;size=20\" data-page=\"2\" aria-label=\"Page 2\">2</a></li><li class=\"ah-pagination-li\"><a class=\"ah-pagination-item ah-pagination-item-active\" href=\"/list?page=3&amp;size=20\" data-page=\"3\" aria-label=\"Page 3\" aria-current=\"page\">3</a></li><li class=\"ah-pagination-li\"><a class=\"ah-pagination-item\" href=\"/list?page=4&amp;size=20\" data-page=\"4\" aria-label=\"Page 4\">4</a></li><li class=\"ah-pagination-li\"><a class=\"ah-pagination-item\" href=\"/list?page=5&amp;size=20\" data-page=\"5\" aria-label=\"Page 5\">5</a></li><li class=\"ah-pagination-li\"><a class=\"ah-pagination-nav\" href=\"/list?page=4&amp;size=20\" data-type=\"next\" aria-label=\"Next\"><span class=\"ah-pagination-nav-icon\" aria-hidden=\"true\">›</span><span class=\"ah-pagination-nav-label\">Next</span></a></li></ul></div>";

  // Action requests are answered by an empty run; `sent' records them.
  var sent = [];
  window.fetch = function (url, opts) {
    sent.push(JSON.parse(opts.body));
    return Promise.resolve(new Response('{"ops":[]}',
                                        { status: 200 }));
  };
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }

  async function mountLinks(fx, bound) {
    fx.innerHTML = LINKS;
    var el = fx.firstChild;
    if (bound) { el.setAttribute("data-ah-on", "change:AAAA.BBBB"); }
    await T.ready(fx);
    return el;
  }

  // Record whether the component handled a click (bubble phase, after
  // it), then stop the navigation so the test page stays.
  function clickOutcome(fx, a, init) {
    var out = {};
    var h = function (e) { out.prevented = e.defaultPrevented; e.preventDefault(); };
    fx.addEventListener("click", h);
    T.fire(a, "click", Object.assign({ button: 0 }, init || {}));
    fx.removeEventListener("click", h);
    return out.prevented;
  }

  T.test("pagination: a link with an action updates in place and pushes its URL", async function (fx) {
    var start = location.href;
    try {
      sent = [];
      var el = await mountLinks(fx, true);
      var changed = [];
      el.addEventListener("change", function (e) { if (e.target === el) { changed.push(el.getAttribute("data-ah-value")); } });
      T.eq(clickOutcome(fx, el.querySelector("a[data-page='5']")), true, "handled");
      T.eq(changed, ["5"]);
      T.ok(/\/list\?page=5&size=20$/.test(location.href), "pushed " + location.href);
      T.ok(history.state && history.state.ah, "history entry of aihtml");
      T.eq(el.querySelector("a[data-type=prev]").getAttribute("href"), "/list?page=4&size=20", "links follow the page");
      T.ok(!el.querySelector("a[data-type=next]"), "next is disabled on the last page");
      T.eq(clickOutcome(fx, el.querySelector("a[data-type=prev]")), true);
      T.eq(changed, ["5", "4"]);
      T.ok(/\/list\?page=4&size=20$/.test(location.href));
      await wait(20);
      T.eq(sent.length, 2, "the action ran for each click");
      T.eq(sent[1].event.value, "4");
    } finally {
      history.replaceState(null, "", start);
    }
  });

  T.test("pagination: modified clicks and links without an action are left to the browser", async function (fx) {
    var start = location.href;
    var el = await mountLinks(fx, true);
    T.eq(clickOutcome(fx, el.querySelector("a[data-page='5']"), { ctrlKey: true }), false, "ctrl-click");
    T.eq(clickOutcome(fx, el.querySelector("a[data-page='5']"), { button: 1 }), false, "middle click");
    T.eq(el.getAttribute("data-ah-value"), "3");
    el = await mountLinks(fx, false);
    var changed = 0;
    el.addEventListener("change", function () { changed++; });
    T.eq(clickOutcome(fx, el.querySelector("a[data-page='5']")), false, "no action: the link loads");
    T.eq(changed, 0);
    T.eq(el.getAttribute("data-ah-value"), "3");
    T.eq(location.href, start, "nothing pushed");
  });
})(window.AHTest, window.AH);
