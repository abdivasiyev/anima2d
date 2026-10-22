/**
 * The pictures and fonts items draw with. Each picture is loaded once per asset and shared
 * by every renderer; how it is loaded depends on where the renderer runs (the editor fetches
 * it from the server, the render agent reads the file), so the loader is set from outside.
 * Until a picture arrives its item draws as a placeholder, and listeners hear when it lands,
 * so cached frames can be drawn again.
 */
import type { Project, TextFields } from "../domain/project";
import type { Ctx } from "./render";

export type ImageLoader = (asset: string) => Promise<CanvasImageSource & { readonly width: number; readonly height: number }>;

type Slot = { readonly state: "loading" } | { readonly state: "failed" } | { readonly state: "ready"; readonly image: CanvasImageSource };

let loader: ImageLoader | undefined;
const images = new Map<string, Slot>();
const listeners = new Set<() => void>();

const changed = (): void => {
  for (const f of listeners) f();
};

export const setImageLoader = (l: ImageLoader): void => {
  loader = l;
};

/** Hear when a picture or font arrives; returns the way to stop hearing. */
export const onMediaReady = (f: () => void): (() => void) => {
  listeners.add(f);
  return () => {
    listeners.delete(f);
  };
};

/** Tell listeners something drawn has changed (a font arrived). */
export const mediaChanged = changed;

const load = (asset: string): Promise<void> => {
  if (loader === undefined || images.has(asset)) return Promise.resolve();
  images.set(asset, { state: "loading" });
  return loader(asset).then(
    (image) => {
      images.set(asset, { state: "ready", image });
      changed();
    },
    () => {
      images.set(asset, { state: "failed" });
    },
  );
};

/** The picture for an asset if it has arrived; asking starts loading it. */
export const imageFor = (asset: string): CanvasImageSource | undefined => {
  const slot = images.get(asset);
  if (slot === undefined) void load(asset);
  return slot?.state === "ready" ? slot.image : undefined;
};

/** Every picture a project's drawings use, so exported frames never draw a placeholder. */
export const projectImages = (p: Project): string[] =>
  p.layers.flatMap((l) => (l.kind === "drawing" ? l.cels.flatMap((c) => c.items.flatMap((i) => (i.kind === "image" ? [i.asset] : []))) : []));

/** Load every picture first (export and the render agent draw each frame once, so they wait). */
export const preloadImages = async (assets: Iterable<string>): Promise<void> => {
  await Promise.all([...new Set(assets)].map(load));
};

// Text ------------------------------------------------------------------------------------

/** Families every browser (and the render agent) has; others come from the project's fonts. */
export const GENERIC_FONTS = ["sans-serif", "serif", "monospace", "cursive", "system-ui"] as const;

const STYLE_CSS: Record<TextFields["style"], string> = { normal: "", bold: "bold ", italic: "italic ", "bold-italic": "italic bold " };

export const fontCss = (t: Pick<TextFields, "font" | "style"> & { readonly size: number }): string => {
  const generic = GENERIC_FONTS.some((g) => g === t.font);
  return `${STYLE_CSS[t.style]}${t.size}px ${generic ? t.font : `"${t.font}", sans-serif`}`;
};

export const LINE_HEIGHT = 1.2;

export interface TextLayout {
  readonly lines: readonly string[];
  readonly widths: readonly number[];
  /** The block's size: its widest line, and its lines' height. */
  readonly width: number;
  readonly height: number;
}

let measurer: OffscreenCanvasRenderingContext2D | null | undefined;
const layouts = new Map<string, TextLayout>();

/** How a text item lays out: its lines and their widths, measured in its font. */
export const textLayout = (t: Pick<TextFields, "text" | "font" | "style" | "size">): TextLayout => {
  const font = fontCss(t);
  const key = `${font}\n${t.text}`;
  const known = layouts.get(key);
  if (known !== undefined) return known;
  measurer ??= new OffscreenCanvas(1, 1).getContext("2d");
  const lines = t.text.split("\n");
  if (measurer !== null) measurer.font = font;
  // Without a context to measure with, guess from the size: about half an em a character.
  const widths = lines.map((l) => (measurer === null || measurer === undefined ? l.length * t.size * 0.55 : measurer.measureText(l).width));
  const out = { lines, widths, width: Math.max(1, ...widths), height: lines.length * t.size * LINE_HEIGHT };
  if (layouts.size > 2000) layouts.clear();
  layouts.set(key, out);
  return out;
};

/** Measured sizes go stale when a font arrives. */
export const forgetLayouts = (): void => {
  layouts.clear();
};

export const drawText = (ctx: Ctx, t: TextFields, color: string): void => {
  const lay = textLayout(t);
  ctx.translate(t.center[0], t.center[1]);
  ctx.rotate(t.rotation);
  ctx.font = fontCss(t);
  ctx.fillStyle = color;
  ctx.textBaseline = "middle";
  ctx.textAlign = t.align;
  const x = t.align === "left" ? -lay.width / 2 : t.align === "right" ? lay.width / 2 : 0;
  const lh = t.size * LINE_HEIGHT;
  for (const [i, line] of lay.lines.entries()) ctx.fillText(line, x, -lay.height / 2 + lh * (i + 0.5));
};
