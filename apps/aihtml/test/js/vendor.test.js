// AH.vendor: optional third-party scripts loaded on demand from vendor/.
AHTest.test("vendor loads a library once and resolves with its global", async function () {
  var a = await AH.vendor("echarts");
  AHTest.ok(a && typeof a.init === "function", "echarts.init");
  var b = await AH.vendor("echarts");
  AHTest.eq(a, b);
  AHTest.eq(document.querySelectorAll('script[src$="echarts.min.js"]').length, 1);
});

AHTest.test("vendor loads a list in order, dependencies first", async function () {
  var libs = await AH.vendor(["xlsx", "jspdf-autotable"]);
  AHTest.ok(libs[0] && typeof libs[0].utils === "object", "XLSX.utils");
  AHTest.ok(typeof libs[1] === "function", "autoTable");
  AHTest.ok(window.jspdf && typeof window.jspdf.jsPDF === "function", "jspdf loaded as a dependency");
});

AHTest.test("vendor loads the ProseMirror bundle (markdown_editor)", async function () {
  var P = await AH.vendor("prosemirror");
  AHTest.ok(P && typeof P.view.EditorView === "function", "EditorView");
  AHTest.ok(typeof P.state.EditorState.create === "function", "EditorState");
  AHTest.ok(typeof P.markdown.MarkdownParser === "function", "MarkdownParser");
  AHTest.ok(typeof P.tables.tableEditing === "function", "tableEditing");
  AHTest.eq(P.markdownit().render("**x**").trim(), "<p><strong>x</strong></p>");
  AHTest.eq(window.AHProseMirror, P);
});

AHTest.test("vendor rejects an unknown library", async function () {
  var err = null;
  try { await AH.vendor("nope"); } catch (e) { err = e; }
  AHTest.ok(err && /unknown vendor library/.test(err.message));
});
