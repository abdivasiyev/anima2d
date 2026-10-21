/**
 * The editor's own settings: the tool and each tool's variant, colour, size, opacity,
 * stabiliser, onion skin, symmetry. Remembered per device in local storage and parsed back
 * like anything else read from outside the program; a stored value from an older version
 * that no longer parses falls back to its default, field by field.
 */
import { brandChecked, type Brand } from "./brand";
import { andThen, err, ok, type Result } from "./result";
import { integer, number, object } from "./wire";
import {
  BRUSHES,
  SCATTERS,
  parseBrush,
  parseBrushSize,
  parseColor,
  parseOpacity,
  parseScatter,
  type BrushKind,
  type BrushSize,
  type Color,
  type Opacity,
  type ScatterKind,
} from "./project";

export const TOOLS = ["select", "brush", "scatter", "eraser", "shape", "fill", "eyedropper"] as const;
export type Tool = (typeof TOOLS)[number];

export const ERASERS = ["pixel", "object"] as const;
export type EraserMode = (typeof ERASERS)[number];

/** The shape tool's variants: the item shapes plus the two-step curve. */
export const SHAPE_TOOLS = ["line", "curve", "rect", "ellipse", "triangle"] as const;
export type ShapeTool = (typeof SHAPE_TOOLS)[number];

/** How wide a gap in the line art the fill bucket bridges, in canvas pixels. */
export const GAPS = [0, 4, 10, 20] as const;
export type Gap = (typeof GAPS)[number];

const oneOf = <T extends string | number>(values: readonly T[], expected: string) => (raw: unknown): Result<T> => {
  const found = values.find((v) => v === raw);
  return found === undefined ? err(expected) : ok(found);
};

export const parseTool = oneOf(TOOLS, "expected a known tool");
export const parseEraserMode = oneOf(ERASERS, 'expected an eraser mode of "pixel" or "object"');
export const parseShapeTool = oneOf(SHAPE_TOOLS, "expected a known shape");
export const parseGap = oneOf(GAPS, "expected a gap of 0, 4, 10 or 20");

/** How hard the line is pulled back toward its smoothed path: 0 (off) to 0.95. */
export type Stabilizer = Brand<number, "Stabilizer">;

export const parseStabilizer = (raw: unknown): Result<Stabilizer> =>
  andThen(number(raw), (n) => (n >= 0 && n <= 0.95 ? ok(brandChecked<number, "Stabilizer">(n)) : err("expected a stabiliser from 0 to 0.95")));

/** Drawings shown before and after the current one: 0 to 5 each way. */
export type OnionRange = Brand<number, "OnionRange">;

export const parseOnionRange = (raw: unknown): Result<OnionRange> =>
  andThen(integer(raw), (n) => (n >= 0 && n <= 5 ? ok(brandChecked<number, "OnionRange">(n)) : err("expected an onion skin range from 0 to 5")));

const parseBool = (raw: unknown): Result<boolean> => (typeof raw === "boolean" ? ok(raw) : err("expected true or false"));

export interface SettingsFields {
  readonly tool: Tool;
  readonly brush: BrushKind;
  readonly scatter: ScatterKind;
  readonly eraser: EraserMode;
  readonly shape: ShapeTool;
  readonly shapeFilled: boolean;
  readonly gap: Gap;
  readonly color: Color;
  /** Each tool family keeps its own size: a grass brush and a fine pen want different ones. */
  readonly size: BrushSize;
  readonly scatterSize: BrushSize;
  readonly eraserSize: BrushSize;
  readonly shapeSize: BrushSize;
  readonly opacity: Opacity;
  readonly stabilizer: Stabilizer;
  readonly symmetry: boolean;
  readonly onion: boolean;
  readonly onionBefore: OnionRange;
  readonly onionAfter: OnionRange;
}

export type Settings = Brand<SettingsFields, "Settings">;

const DEFAULTS = {
  tool: "brush",
  brush: "pen",
  scatter: "grass",
  eraser: "pixel",
  shape: "line",
  shapeFilled: false,
  gap: 4,
  color: "#1c1c1e",
  size: 6,
  scatterSize: 40,
  eraserSize: 24,
  shapeSize: 4,
  opacity: 1,
  stabilizer: 0.4,
  symmetry: false,
  onion: true,
  onionBefore: 2,
  onionAfter: 1,
} as const;

/** Each field from `raw` if it parses, else its default: settings never fail to load. */
export const parseSettings = (raw: unknown): Settings => {
  const o = object(raw);
  const get = <T>(name: keyof typeof DEFAULTS, parse: (raw: unknown) => Result<T>): T => {
    const stored = o.ok ? parse(o.value[name]) : err<T>("no settings");
    if (stored.ok) return stored.value;
    const fallback = parse(DEFAULTS[name]);
    if (!fallback.ok) throw new Error(`default ${name}: ${fallback.error}`);
    return fallback.value;
  };
  return brandChecked<SettingsFields, "Settings">({
    tool: get("tool", parseTool),
    brush: get("brush", parseBrush),
    scatter: get("scatter", parseScatter),
    eraser: get("eraser", parseEraserMode),
    shape: get("shape", parseShapeTool),
    shapeFilled: get("shapeFilled", parseBool),
    gap: get("gap", parseGap),
    color: get("color", parseColor),
    size: get("size", parseBrushSize),
    scatterSize: get("scatterSize", parseBrushSize),
    eraserSize: get("eraserSize", parseBrushSize),
    shapeSize: get("shapeSize", parseBrushSize),
    opacity: get("opacity", parseOpacity),
    stabilizer: get("stabilizer", parseStabilizer),
    symmetry: get("symmetry", parseBool),
    onion: get("onion", parseBool),
    onionBefore: get("onionBefore", parseOnionRange),
    onionAfter: get("onionAfter", parseOnionRange),
  });
};

/** Which size field the current tool draws with. */
export const sizeKey = (s: Settings): "size" | "scatterSize" | "eraserSize" | "shapeSize" => {
  switch (s.tool) {
    case "scatter":
      return "scatterSize";
    case "eraser":
      return "eraserSize";
    case "shape":
      return "shapeSize";
    case "brush":
    case "select":
    case "fill":
    case "eyedropper":
      return "size";
  }
};

/** The size the current tool draws with. */
export const activeSize = (s: Settings): BrushSize => s[sizeKey(s)];

export const unparseSettings = (s: Settings): unknown => ({ ...s });

/** Replace one field with an already-parsed value. */
export const withSetting = <K extends keyof SettingsFields>(s: Settings, key: K, value: SettingsFields[K]): Settings => {
  const next: SettingsFields = { ...s, [key]: value };
  return brandChecked<SettingsFields, "Settings">(next);
};

export { BRUSHES, SCATTERS };
