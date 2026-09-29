// AH.vendor: optional third-party libraries, each a vendor-<name> chunk of
// the bundle loaded by a dynamic import() the first time it is asked for.
function vendorChunks() {
  return performance.getEntriesByType("resource")
    .map(function (e) { return e.name.replace(/^.*\//, ""); })
    .filter(function (n) { return /^vendor-/.test(n); });
}

// Runs first: the page has loaded every component chunk (AH.loadAll) but
// no component has asked for a library yet.
AHTest.test("vendor: component chunks load no library by themselves", async function () {
  AHTest.eq(vendorChunks(), []);
  AHTest.ok(!window.echarts && !window.XLSX && !window.jspdf, "no globals");
});

AHTest.test("vendor loads a library once, as its own chunk, and resolves with it", async function () {
  var a = await AH.vendor("echarts");
  AHTest.ok(a && typeof a.init === "function", "echarts.init");
  var b = await AH.vendor("echarts");
  AHTest.eq(a === b, true);
  AHTest.eq(vendorChunks().filter(function (n) { return /^vendor-echarts-/.test(n); }).length, 1);
  AHTest.eq(document.querySelectorAll('script[src*="echarts"]').length, 0, "no script tag");
  AHTest.ok(!window.echarts, "no global");
});

AHTest.test("vendor loads a list and resolves with the libraries in order", async function () {
  var libs = await AH.vendor(["xlsx", "jspdf", "jspdf-autotable"]);
  AHTest.ok(libs[0] && typeof libs[0].utils.aoa_to_sheet === "function", "XLSX.utils");
  AHTest.ok(typeof libs[1].jsPDF === "function", "jsPDF");
  AHTest.ok(typeof libs[2] === "function", "autoTable");
  // the three work together as datagrid's PDF export uses them
  var doc = new libs[1].jsPDF({ format: "a4" });
  libs[2](doc, { head: [["a", "b"]], body: [["1", "2"]] });
  AHTest.ok(doc.lastAutoTable && doc.lastAutoTable.finalY > 0, "table drawn");
  AHTest.ok(doc.output("arraybuffer").byteLength > 0, "pdf written");
  var wb = libs[0].utils.book_new();
  libs[0].utils.book_append_sheet(wb, libs[0].utils.aoa_to_sheet([["a"], [1]]), "S");
  AHTest.ok(libs[0].write(wb, { bookType: "xlsx", type: "array" }).byteLength > 0, "xlsx written");
  // jspdf's optional html() helpers (html2canvas, canvg, dompurify) stay unloaded
  AHTest.eq(vendorChunks().filter(function (n) { return /^vendor-(html2canvas|canvg|dompurify)-/.test(n); }), []);
});

AHTest.test("vendor rejects an unknown library", async function () {
  var err = null;
  try { await AH.vendor("nope"); } catch (e) { err = e; }
  AHTest.ok(err && /unknown vendor library/.test(err.message));
  err = null;
  try { await AH.vendor("toString"); } catch (e) { err = e; }
  AHTest.ok(err && /unknown vendor library/.test(err.message), "not an Object.prototype name");
});
