/* The relation-graph behaviour (aihtml_relation_graph), ported from
 * sigil (data/relation_graph): the panel around a nested chart (the
 * `chart' behaviour of _lib_chart.ts draws the graph): node selection
 * (ah:select, data-ah-value), detail card, toolbar (fit, refresh ->
 * ah:refresh), keyboard (arrows, Home/End, Escape) and a live region
 * naming the node. The visually hidden table of nodes and links beside
 * the canvas (aihtml_lib_chart:data_text/2) follows the graph's data:
 * the server's new table when setOption brings one (chart_update/3),
 * else rebuilt from the canvas's option (ah:chart-data).
 *
 * Events (native CustomEvents on the root, bubbling):
 *   ah:select      detail: {id} (null when cleared), user selections only
 *   ah:node-click  detail: {id}
 *   ah:refresh     no detail
 */
import AH from "../core.ts";
import { updateText } from "./_lib_chart.ts";
import type { ChartDataEvent, ChartItemEvent, ChartOption } from "./_lib_chart.ts";
import type { EChartsType } from "echarts";

/** Detail of ah:select (id null when cleared). */
export interface RelationSelect { id: string | null; }
/** Detail of ah:node-click. */
export interface RelationNodeClick { id: string; }

/** A node of the graph, in data order (index: its data index in a graph
 *  series; tree nodes have none). */
interface GraphNode { id: string; name: unknown; index?: number; }

/** echarts' model, which its types declare private: the part that finds
 *  a node's layout. */
interface LayoutModel {
  getSeriesByIndex(i: number): { getData(): { getItemLayout(i: number): unknown } } | undefined;
}

type Rec = Record<string, unknown>;

function isMap(x: unknown): x is Rec { return !!x && typeof x === "object" && !Array.isArray(x); }

// the server renders the canvas in every relation graph
function canvas(el: Element): Element {
  return el.querySelector(":scope > .ah-relation-graph__canvas") as Element;
}

// The nodes in data order: [{id, name, index}]; tree nodes depth first.
function nodes(el: Element): GraphNode[] {
  const o = AH.invoke(canvas(el), "getOption");
  const series = isMap(o) && Array.isArray(o["series"]) ? o["series"] as unknown[] : [];
  const sr = series[0];
  if (!isMap(sr)) { return []; }
  const out: GraphNode[] = [];
  if (sr["type"] === "tree") {
    const walk = (list: unknown): void => {
      (Array.isArray(list) ? list as unknown[] : []).forEach((n) => {
        if (!isMap(n)) { return; }
        if (n["id"] !== "__root__") { out.push({ id: String(n["id"]), name: n["name"] }); }
        walk(n["children"]);
      });
    };
    walk(sr["data"]);
  } else {
    (Array.isArray(sr["data"]) ? sr["data"] as unknown[] : []).forEach((n, i) => {
      if (!isMap(n)) { return; }
      out.push({ id: String(n["id"] !== undefined ? n["id"] : n["name"]), name: n["name"], index: i });
    });
  }
  return out;
}

function findNode(el: Element, id: string): GraphNode | null {
  return nodes(el).find((n) => n.id === id) || null;
}

function highlight(el: Element): void {
  const c = canvas(el);
  const id = el.getAttribute("data-ah-value");
  AH.invoke(c, "dispatchAction", { type: "downplay", seriesIndex: 0 });
  const n = id ? findNode(el, id) : null;
  if (n) {
    AH.invoke(c, "dispatchAction", n.index !== undefined
      ? { type: "highlight", seriesIndex: 0, dataIndex: n.index }
      : { type: "highlight", seriesIndex: 0, name: n.name });
  }
}

function showDetail(el: Element, id: string): void {
  const card = el.querySelector(":scope > .ah-relation-graph__detail");
  if (!card) { return; }
  let hits = 0;
  card.querySelectorAll(".ah-relation-graph__detail-item").forEach((item) => {
    const hit = item.getAttribute("data-node") === id;
    if (hit) { hits++; item.removeAttribute("hidden"); }
    else { item.setAttribute("hidden", "hidden"); }
  });
  card.setAttribute("data-visible", id && hits ? "true" : "false");
}

// Pan so that the node is in the middle (graph layouts only).
function focusNode(el: Element, id: unknown): void {
  const chart = AH.invoke(canvas(el), "instance") as EChartsType | null;   // the chart behaviour's
  const n = id ? findNode(el, String(id)) : null;
  if (!chart || !n || n.index === undefined) { return; }
  const getModel = (chart as unknown as { getModel?: () => LayoutModel }).getModel;
  if (typeof getModel !== "function") { return; }
  const sm = getModel.call(chart).getSeriesByIndex(0);
  const layout: unknown = sm && sm.getData().getItemLayout(n.index);
  if (!Array.isArray(layout)) { return; }
  const p = chart.convertToPixel({ seriesIndex: 0 }, layout as number[]);
  if (!p) { return; }
  chart.dispatchAction({ type: "graphRoam", seriesIndex: 0,
                         dx: chart.getWidth() / 2 - (p[0] as number), dy: chart.getHeight() / 2 - (p[1] as number) });
}

class RelationGraphController extends AH.Controller {
  #focused = false;

  override setup(): void {
    const el = this.element;
    const c = canvas(el);
    const label = el.getAttribute("aria-label");
    this.#focused = false;
    this.listen<CustomEvent<ChartItemEvent | null>>(c, "ah:chart-click", (e) => {
      const info = e.detail;
      if (!info || info.componentType !== "series" || info.dataType === "edge") { return; }
      const d = isMap(info.data) ? info.data : {};
      const id = d["id"] !== undefined ? String(d["id"]) : info.name;
      if (!id || id === "__root__") { return; }
      const sid = String(id);
      this.fire<RelationNodeClick>("ah:node-click", { id: sid });
      this.pick(sid, true);
    });
    this.listen(c, "ah:chart-ready", () => {
      highlight(el);
      const f = el.getAttribute("data-ah-focus");
      if (f && !this.#focused) {
        this.#focused = true;
        // a force layout settles for a while first
        setTimeout(() => { if (this.signal && !this.signal.aborted) { focusNode(el, f); } },
                   el.getAttribute("data-layout") === "force" ? 800 : 50);
      }
    });
    this.listen<CustomEvent<ChartDataEvent | null>>(c, "ah:chart-data", (e) => {
      const html = e.detail && e.detail.text;
      if (typeof html === "string") { updateText(el, c, null, html, label); }
      else { updateText(el, c, AH.invoke(c, "getOption"), undefined, label); }
    });
    this.delegate("click", ".ah-relation-graph__tool", (_e, tool) => {
      const act = tool.getAttribute("data-act");
      AH.invoke(c, "resetView");
      if (act === "refresh") { this.fire("ah:refresh"); }
    });
    this.delegate("click", ".ah-relation-graph__detail-close", () => {
      this.pick(null, true);
      el.focus();
    });
    this.listen(el, "keydown", (e) => {
      if (e.target !== el) { return; }
      const list = nodes(el);
      if (!list.length) { return; }
      const cur = el.getAttribute("data-ah-value");
      let i = -1;
      list.forEach((n, j) => { if (n.id === cur) { i = j; } });
      let next: GraphNode | undefined;
      switch (e.key) {
        case "ArrowRight": case "ArrowDown": next = list[(i + 1) % list.length]; break;
        case "ArrowLeft": case "ArrowUp": next = list[i <= 0 ? list.length - 1 : i - 1]; break;
        case "Home": next = list[0]; break;
        case "End": next = list[list.length - 1]; break;
        case "Escape":
          if (!cur) { return; }
          e.preventDefault();
          this.pick(null, true);
          return;
        default: return;
      }
      e.preventDefault();
      if (next) { this.pick(next.id, true); }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  select(id: string | number | null): void { this.pick(id, false); }
  getSelected(): string | null { return this.element.getAttribute("data-ah-value") || null; }
  focus(id: string | number): void { focusNode(this.element, id); }
  fit(): void { AH.invoke(canvas(this.element), "resetView"); }
  setOption(option: ChartOption, notMerge?: boolean | string[] | null, html?: string | null): void {
    AH.invoke(canvas(this.element), "setOption", option, notMerge, html);
    highlight(this.element);
  }

  private pick(raw: string | number | null | undefined, user: boolean): void {
    const el = this.element;
    const id = raw === null || raw === undefined ? "" : String(raw);
    const changed = el.getAttribute("data-ah-value") !== id;
    el.setAttribute("data-ah-value", id);
    const n = id ? findNode(el, id) : null;
    el.querySelectorAll(":scope > .ah-relation-graph__live").forEach((live) => {
      live.textContent = n && n.name !== undefined && n.name !== null ? String(n.name) : "";
    });
    showDetail(el, id);
    highlight(el);
    if (user && changed) {
      this.fire<RelationSelect>("ah:select", { id: id || null });
    }
  }
}

AH.register("relation-graph", RelationGraphController);
