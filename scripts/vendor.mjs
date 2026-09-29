// Copies third-party scripts from node_modules into the library's
// priv/static/vendor, so an Erlang consumer gets a working page without
// running npm.
//
//   jquery        always loaded by the page (aihtml:page/2)
//   echarts       charts and relation_graph            } loaded on demand by
//   xlsx          datagrid / pivotgrid Excel export    } AH.vendor(name), only
//   jspdf         datagrid / pivotgrid PDF export      } on pages that use them
//   jspdf-autotable  tables in PDF export (needs jspdf)
//
// Each library's licence file is copied next to it.
import { copyFileSync, mkdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const out = join(root, "apps/aihtml/priv/static/vendor");
mkdirSync(out, { recursive: true });

// [from (under node_modules), to (under vendor/)]
const FILES = [
  ["jquery/dist/jquery.min.js", "jquery.min.js"],
  ["jquery/LICENSE.txt", "jquery.LICENSE.txt"],
  ["echarts/dist/echarts.min.js", "echarts.min.js"],
  ["echarts/LICENSE", "echarts.LICENSE.txt"],
  ["echarts/NOTICE", "echarts.NOTICE.txt"],
  ["xlsx/dist/xlsx.full.min.js", "xlsx.full.min.js"],
  ["xlsx/LICENSE", "xlsx.LICENSE.txt"],
  ["jspdf/dist/jspdf.umd.min.js", "jspdf.umd.min.js"],
  ["jspdf/LICENSE", "jspdf.LICENSE.txt"],
  ["jspdf-autotable/dist/jspdf.plugin.autotable.min.js", "jspdf.plugin.autotable.min.js"],
  ["jspdf-autotable/LICENSE.txt", "jspdf-autotable.LICENSE.txt"],
];

for (const [from, to] of FILES) {
  copyFileSync(join(root, "node_modules", from), join(out, to));
}
console.log(`vendored ${FILES.length} files into`, out);
