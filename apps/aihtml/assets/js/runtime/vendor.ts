// Optional third-party libraries, loaded on demand.
//
// vendor("echarts").then((echarts) => ...) loads a library once per page
// and resolves with it; a list resolves with the list of libraries, in
// the same order. Each library is its own chunk of the bundle
// (vite.config.mjs), fetched by a dynamic import() the first time it is
// asked for: a page without charts or exports downloads none of them.
// Their licences are in js/THIRD-PARTY-LICENSES.txt.
//
// A namespace resolves as a plain object copy of the module namespace
// (which is frozen), so that a page or a test can wrap or replace a
// library function (XLSX.writeFile, ...) for every component using it.

type Plain<T> = { -readonly [K in keyof T]: T[K] };

function plain<T extends object>(m: T): Plain<T> { return { ...m }; }

const LIBRARIES = {
  /** the echarts namespace (init, graphic, ...) */
  echarts: () => import("echarts").then(plain),
  /** the SheetJS namespace (utils, write, writeFile, ...) */
  xlsx: () => import("xlsx").then(plain),
  /** the jsPDF namespace ({jsPDF, ...}) */
  jspdf: () => import("jspdf").then(plain),
  /** the autoTable(doc, options) function */
  "jspdf-autotable": () => import("jspdf-autotable").then((m) => m.autoTable)
};

export type VendorName = keyof typeof LIBRARIES;
/** What vendor(name) resolves with. */
export type VendorLibrary<N extends VendorName> = Awaited<ReturnType<typeof LIBRARIES[N]>>;

const loads = new Map<string, Promise<unknown>>();

function isVendorName(n: string): n is VendorName {
  return Object.prototype.hasOwnProperty.call(LIBRARIES, n);
}

function loadOne(name: string): Promise<unknown> {
  if (!isVendorName(name)) { return Promise.reject(new Error("aihtml: unknown vendor library " + name)); }
  let p = loads.get(name);
  if (!p) {
    p = LIBRARIES[name]().catch((err: unknown) => {
      loads.delete(name);   // a later call tries again
      throw err;
    });
    loads.set(name, p);
  }
  return p;
}

export function vendor<N extends VendorName>(name: N): Promise<VendorLibrary<N>>;
export function vendor<const L extends readonly VendorName[]>(
  names: L): Promise<{ -readonly [I in keyof L]: VendorLibrary<L[I]> }>;
export function vendor(names: string | readonly string[]): Promise<unknown> {
  return Array.isArray(names) ? Promise.all(names.map(loadOne)) : loadOne(names as string);
}
