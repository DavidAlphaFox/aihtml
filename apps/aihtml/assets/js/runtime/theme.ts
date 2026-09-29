// The four theme axes on <html> (README, 四轴主题), saved for the next
// page (aihtml_page restores them before the first paint).
import { fire } from "./dom.ts";

export type Axis = "appearance" | "palette" | "typography" | "skin";

export type ThemeValues = Record<Axis, string | null>;

/** Detail of the ah:theme event (on document, after a change). */
export interface ThemeEvent { axis: Axis; value: string; }

const AXES: Record<Axis, string> = {
  appearance: "data-theme",
  palette: "data-palette",
  typography: "data-typography",
  skin: "data-skin"
};
const STORE = "aihtml.theme";

function isAxis(a: string): a is Axis {
  return Object.prototype.hasOwnProperty.call(AXES, a);
}

export class Theme {
  /** axis -> the attribute on <html> */
  readonly axes = AXES;

  get(): ThemeValues {
    const html = document.documentElement;
    const out = {} as ThemeValues;
    (Object.keys(AXES) as Axis[]).forEach((axis) => { out[axis] = html.getAttribute(AXES[axis]); });
    return out;
  }

  set(axis: string, value: string): void {
    if (!isAxis(axis)) { throw new Error("unknown theme axis: " + axis); }
    document.documentElement.setAttribute(AXES[axis], value);
    const t = Theme.#load();
    t[axis] = value;
    Theme.#save(t);
    fire<ThemeEvent>(document, "ah:theme", { axis, value });
  }

  /** Forget the saved choice (the page keeps its current theme). */
  reset(): void { Theme.#save({}); }

  static #load(): Partial<Record<Axis, string>> {
    try {
      return JSON.parse(window.localStorage.getItem(STORE) || "{}") as Partial<Record<Axis, string>>;
    } catch {
      return {};
    }
  }

  static #save(t: Partial<Record<Axis, string>>): void {
    try {
      window.localStorage.setItem(STORE, JSON.stringify(t));
    } catch {
      /* private mode or storage disabled: the choice lasts for this page */
    }
  }
}
