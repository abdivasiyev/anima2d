/**
 * Drawing items. One set of functions renders the canvas, live previews, onion skins,
 * thumbnails and playback, so they all agree on what an item looks like.
 *
 * A drawing renders as two layers: colour (paint, fills) under line (everything else).
 * The pixel eraser cuts both at its place in the order. Rendering takes `ItemFields`
 * rather than `Item`: a draft still being drawn is made of parsed values too, it just has
 * not been committed yet.
 */
import { getStroke } from "perfect-freehand";

import type { BrushKind, Frame, ItemFields, Point, ScatterKind } from "../domain/project";
import { shapeCorners } from "./geometry";

/** Either kind of 2D context: on-screen canvases and off-screen layers share every call used here. */
export type Ctx = CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D;

export interface DrawOptions {
  /** Draw every mark in this colour (onion skins). */
  readonly tint?: string;
  /** Lines at least this wide, in canvas pixels (thumbnails). */
  readonly minSize?: number;
}

// Freehand ------------------------------------------------------------------------------

interface Outline {
  readonly thinning: number;
  readonly smoothing: number;
  readonly streamline: number;
  readonly scale: number;
}

const OUTLINES: Record<BrushKind | "erase", Outline> = {
  pen: { thinning: 0.62, smoothing: 0.55, streamline: 0.2, scale: 1 },
  pencil: { thinning: 0.35, smoothing: 0.4, streamline: 0.15, scale: 0.7 },
  marker: { thinning: 0, smoothing: 0.6, streamline: 0.25, scale: 1 },
  paint: { thinning: 0.2, smoothing: 0.65, streamline: 0.3, scale: 1 },
  erase: { thinning: 0.2, smoothing: 0.5, streamline: 0.2, scale: 1 },
};

const outline = (points: readonly Point[], size: number, o: Outline): number[][] =>
  getStroke(
    points.map((p) => [p[0], p[1], p[2]]),
    { thinning: o.thinning, smoothing: o.smoothing, streamline: o.streamline, size: size * o.scale, last: true, simulatePressure: false },
  );

const tracePolygon = (ctx: Ctx, poly: readonly number[][]): void => {
  const [first, ...rest] = poly;
  if (first === undefined) return;
  ctx.moveTo(first[0] ?? 0, first[1] ?? 0);
  // Quadratic curves through the midpoints: a smooth edge without extra outline points.
  for (const [i, p] of rest.entries()) {
    const next = rest[i + 1] ?? first;
    ctx.quadraticCurveTo(p[0] ?? 0, p[1] ?? 0, ((p[0] ?? 0) + (next[0] ?? 0)) / 2, ((p[1] ?? 0) + (next[1] ?? 0)) / 2);
  }
  ctx.closePath();
};

interface Box {
  readonly x: number;
  readonly y: number;
  readonly w: number;
  readonly h: number;
}

const polyBounds = (poly: readonly number[][]): Box => {
  let x0 = Infinity;
  let y0 = Infinity;
  let x1 = -Infinity;
  let y1 = -Infinity;
  for (const p of poly) {
    const x = p[0] ?? 0;
    const y = p[1] ?? 0;
    x0 = Math.min(x0, x);
    y0 = Math.min(y0, y);
    x1 = Math.max(x1, x);
    y1 = Math.max(y1, y);
  }
  return Number.isFinite(x0) ? { x: x0, y: y0, w: x1 - x0, h: y1 - y0 } : { x: 0, y: 0, w: 0, h: 0 };
};

/** A small fast PRNG: a seed fixes every stamp's place, size and turn, and the pencil grain. */
export const mulberry32 = (seed: number): (() => number) => {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
};

/** Speckles cut out of pencil strokes, so they read as graphite rather than ink. */
let grainCanvas: OffscreenCanvas | undefined;
const grain = (): OffscreenCanvas => {
  if (grainCanvas !== undefined) return grainCanvas;
  const c = new OffscreenCanvas(96, 96);
  const g = c.getContext("2d");
  const rnd = mulberry32(7);
  if (g !== null)
    for (let i = 0; i < 1400; i++) {
      g.fillStyle = `rgba(0,0,0,${(0.15 + rnd() * 0.5).toFixed(2)})`;
      g.fillRect(Math.floor(rnd() * 96), Math.floor(rnd() * 96), 1, 1);
    }
  grainCanvas = c;
  return c;
};

const drawPencil = (ctx: Ctx, poly: number[][], color: string): void => {
  // Grain needs a layer of its own: cut into this one it would cut what lies under the stroke.
  const box = polyBounds(poly);
  const w = Math.max(1, Math.ceil(box.w) + 2);
  const h = Math.max(1, Math.ceil(box.h) + 2);
  const layer = new OffscreenCanvas(w, h);
  const l = layer.getContext("2d");
  if (l === null) return;
  l.translate(1 - box.x, 1 - box.y);
  l.fillStyle = color;
  l.beginPath();
  tracePolygon(l, poly);
  l.fill();
  const pattern = l.createPattern(grain(), "repeat");
  if (pattern !== null) {
    l.globalCompositeOperation = "destination-out";
    l.fillStyle = pattern;
    l.fillRect(box.x - 1, box.y - 1, w, h);
  }
  ctx.drawImage(layer, box.x - 1, box.y - 1);
};

// Scatter -------------------------------------------------------------------------------

/** `hex` lightened (amount > 0) or darkened (< 0). */
const shade = (hex: string, amount: number): string => {
  const n = parseInt(hex.slice(1), 16);
  const ch = (shift: number): number => {
    const v = (n >> shift) & 255;
    return Math.round(amount >= 0 ? v + (255 - v) * amount : v * (1 + amount));
  };
  return `rgb(${ch(16)}, ${ch(8)}, ${ch(0)})`;
};

/** Distance between stamps, as a fraction of the stamp size. */
const SPACING: Record<ScatterKind, number> = { grass: 0.32, birds: 2.4, stars: 1.3, leaves: 0.95, flowers: 1.5, spray: 0.18 };

type Stamp = (ctx: Ctx, s: number, rnd: () => number, color: string, tinted: boolean) => void;

const STAMPS: Record<ScatterKind, Stamp> = {
  grass: (ctx, s, rnd, color, tinted) => {
    const blades = 3 + Math.floor(rnd() * 3);
    for (let i = 0; i < blades; i++) {
      const bx = (rnd() - 0.5) * 0.6 * s;
      const h = s * (0.8 + rnd() * 0.9);
      const lean = (rnd() - 0.5) * 0.9 * s;
      const w = s * (0.06 + rnd() * 0.05);
      const tone = (rnd() - 0.5) * 0.3;
      ctx.fillStyle = tinted ? color : shade(color, tone);
      ctx.beginPath();
      ctx.moveTo(bx - w, 0);
      ctx.quadraticCurveTo(bx - w * 0.2 + lean * 0.25, -h * 0.55, bx + lean, -h);
      ctx.quadraticCurveTo(bx + w * 0.6 + lean * 0.25, -h * 0.55, bx + w, 0);
      ctx.closePath();
      ctx.fill();
    }
  },
  birds: (ctx, s, rnd, color, tinted) => {
    const flap = 0.35 + rnd() * 0.6;
    ctx.rotate((rnd() - 0.5) * 0.5);
    const tone = (rnd() - 0.5) * 0.2;
    ctx.strokeStyle = tinted ? color : shade(color, tone);
    ctx.lineWidth = Math.max(1, s * 0.1);
    ctx.lineCap = "round";
    ctx.lineJoin = "round";
    ctx.beginPath();
    ctx.moveTo(-s * 0.6, -s * 0.1 * flap);
    ctx.quadraticCurveTo(-s * 0.3, -s * 0.45 * flap, 0, 0);
    ctx.quadraticCurveTo(s * 0.3, -s * 0.45 * flap, s * 0.6, -s * 0.1 * flap);
    ctx.stroke();
  },
  stars: (ctx, s, rnd, color, tinted) => {
    const tone = (rnd() - 0.5) * 0.3;
    ctx.fillStyle = tinted ? color : shade(color, tone);
    if (rnd() < 0.35) {
      const r = s * (0.05 + rnd() * 0.06);
      ctx.beginPath();
      ctx.arc(0, 0, r, 0, Math.PI * 2);
      ctx.fill();
      return;
    }
    ctx.rotate(rnd() * Math.PI);
    const outer = s * (0.25 + rnd() * 0.25);
    const inner = outer * 0.42;
    ctx.beginPath();
    for (let i = 0; i < 10; i++) {
      const r = i % 2 === 0 ? outer : inner;
      const a = (i * Math.PI) / 5 - Math.PI / 2;
      ctx.lineTo(Math.cos(a) * r, Math.sin(a) * r);
    }
    ctx.closePath();
    ctx.fill();
  },
  leaves: (ctx, s, rnd, color, tinted) => {
    ctx.rotate(rnd() * Math.PI * 2);
    const l = s * (0.45 + rnd() * 0.3);
    const tone = (rnd() - 0.5) * 0.35;
    ctx.fillStyle = tinted ? color : shade(color, tone);
    ctx.beginPath();
    ctx.moveTo(0, -l);
    ctx.quadraticCurveTo(l * 0.75, 0, 0, l);
    ctx.quadraticCurveTo(-l * 0.75, 0, 0, -l);
    ctx.fill();
    ctx.strokeStyle = tinted ? color : shade(color, -0.3);
    ctx.lineWidth = Math.max(0.5, l * 0.06);
    ctx.beginPath();
    ctx.moveTo(0, -l * 0.85);
    ctx.lineTo(0, l * 0.85);
    ctx.stroke();
  },
  flowers: (ctx, s, rnd, color, tinted) => {
    ctx.rotate(rnd() * Math.PI);
    const r = s * (0.18 + rnd() * 0.1);
    const tone = (rnd() - 0.5) * 0.3;
    ctx.fillStyle = tinted ? color : shade(color, tone);
    for (let i = 0; i < 5; i++) {
      const a = (i * Math.PI * 2) / 5;
      ctx.beginPath();
      ctx.arc(Math.cos(a) * r * 1.15, Math.sin(a) * r * 1.15, r, 0, Math.PI * 2);
      ctx.fill();
    }
    ctx.fillStyle = tinted ? color : "#f3c843";
    ctx.beginPath();
    ctx.arc(0, 0, r * 0.75, 0, Math.PI * 2);
    ctx.fill();
  },
  spray: (ctx, s, rnd, color) => {
    ctx.fillStyle = color;
    for (let i = 0; i < 7; i++) {
      const a = rnd() * Math.PI * 2;
      const d = Math.sqrt(rnd()) * s * 0.6;
      const r = Math.max(0.4, s * (0.02 + rnd() * 0.035));
      ctx.beginPath();
      ctx.arc(Math.cos(a) * d, Math.sin(a) * d, r, 0, Math.PI * 2);
      ctx.fill();
    }
  },
};

/** Stamps along the path, evenly spaced by distance, every random draw from the seed. */
const drawScatter = (ctx: Ctx, item: Extract<ItemFields, { kind: "scatter" }>, color: string, tinted: boolean): void => {
  const rnd = mulberry32(item.seed);
  const base = item.size * 1.6;
  const step = Math.max(1, base * SPACING[item.brush]);
  // Birds spread up and down across the sky; the rest hug the path.
  const lift = item.brush === "birds" ? 1.6 : 0.35;
  const stamp = (x: number, y: number, pressure: number): void => {
    const s = base * (0.55 + pressure * 0.65) * (0.7 + rnd() * 0.5);
    const jx = (rnd() - 0.5) * s * 0.5;
    const jy = (rnd() - 0.5) * s * lift;
    ctx.save();
    ctx.translate(x + jx, y + jy);
    STAMPS[item.brush](ctx, s, rnd, color, tinted);
    ctx.restore();
  };
  let prev = item.points[0];
  stamp(prev[0], prev[1], prev[2]);
  let carry = 0;
  for (const p of item.points.slice(1)) {
    const dx = p[0] - prev[0];
    const dy = p[1] - prev[1];
    const d = Math.hypot(dx, dy);
    let t = step - carry;
    while (t <= d) {
      const k = d === 0 ? 0 : t / d;
      stamp(prev[0] + dx * k, prev[1] + dy * k, prev[2] + (p[2] - prev[2]) * k);
      t += step;
    }
    carry = d - (t - step);
    prev = p;
  }
};

// Items ---------------------------------------------------------------------------------

/** Draw one item onto a layer. Erasers cut whatever is already on it. */
export const drawItem = (ctx: Ctx, item: ItemFields, opts: DrawOptions = {}): void => {
  const minSize = opts.minSize ?? 0;
  ctx.save();
  switch (item.kind) {
    case "stroke": {
      ctx.globalAlpha = item.opacity;
      const poly = outline(item.points, Math.max(item.size, minSize), OUTLINES[item.brush]);
      if (item.brush === "pencil" && opts.tint === undefined) drawPencil(ctx, poly, item.color);
      else {
        ctx.fillStyle = opts.tint ?? item.color;
        ctx.beginPath();
        tracePolygon(ctx, poly);
        ctx.fill();
      }
      break;
    }
    case "erase":
      ctx.globalCompositeOperation = "destination-out";
      ctx.fillStyle = "#000";
      ctx.beginPath();
      tracePolygon(ctx, outline(item.points, item.size, OUTLINES.erase));
      ctx.fill();
      break;
    case "scatter":
      ctx.globalAlpha = item.opacity;
      drawScatter(ctx, item, opts.tint ?? item.color, opts.tint !== undefined);
      break;
    case "shape": {
      ctx.globalAlpha = item.opacity;
      ctx.lineWidth = Math.max(item.size, minSize);
      ctx.lineJoin = "round";
      ctx.lineCap = "round";
      ctx.strokeStyle = opts.tint ?? item.color;
      ctx.beginPath();
      if (item.shape === "ellipse") {
        ctx.ellipse(item.center[0], item.center[1], item.width / 2, item.height / 2, item.rotation, 0, Math.PI * 2);
      } else {
        for (const [i, c] of shapeCorners(item).entries()) {
          if (i === 0) ctx.moveTo(c[0], c[1]);
          else ctx.lineTo(c[0], c[1]);
        }
        if (item.shape !== "line") ctx.closePath();
      }
      if (item.fill !== null && item.shape !== "line") {
        ctx.fillStyle = opts.tint ?? item.fill;
        ctx.fill();
      }
      ctx.stroke();
      break;
    }
    case "curve": {
      const [a, c, b] = item.points;
      ctx.globalAlpha = item.opacity;
      ctx.lineWidth = Math.max(item.size, minSize);
      ctx.lineCap = "round";
      ctx.strokeStyle = opts.tint ?? item.color;
      ctx.beginPath();
      ctx.moveTo(a[0], a[1]);
      ctx.quadraticCurveTo(c[0], c[1], b[0], b[1]);
      ctx.stroke();
      break;
    }
    case "fill":
      ctx.globalAlpha = item.opacity;
      ctx.fillStyle = opts.tint ?? item.color;
      ctx.beginPath();
      for (const poly of item.polygons) {
        for (const [i, v] of poly.entries()) {
          if (i === 0) ctx.moveTo(v[0], v[1]);
          else ctx.lineTo(v[0], v[1]);
        }
        ctx.closePath();
      }
      ctx.fill("evenodd");
      break;
  }
  ctx.restore();
};

const onLayer = (item: ItemFields, layer: "line" | "color"): boolean => item.kind === "erase" || item.layer === layer;

// Layers --------------------------------------------------------------------------------

/**
 * The two layers of one drawing, rendered once and reused while drafts are drawn over
 * them. `scale` draws the whole drawing smaller (thumbnails) without touching its items.
 */
export class LayeredFrame {
  readonly color: OffscreenCanvas;
  readonly line: OffscreenCanvas;
  private readonly tmp: OffscreenCanvas;

  constructor(
    readonly width: number,
    readonly height: number,
    private readonly scale = 1,
  ) {
    this.color = new OffscreenCanvas(width, height);
    this.line = new OffscreenCanvas(width, height);
    this.tmp = new OffscreenCanvas(width, height);
  }

  /** Render `items` (in order) into the two layers. */
  render(items: readonly ItemFields[], opts: DrawOptions = {}): void {
    for (const [canvas, layer] of [
      [this.color, "color"],
      [this.line, "line"],
    ] as const) {
      const ctx = canvas.getContext("2d");
      if (ctx === null) continue;
      ctx.setTransform(1, 0, 0, 1, 0, 0);
      ctx.clearRect(0, 0, this.width, this.height);
      ctx.setTransform(this.scale, 0, 0, this.scale, 0, 0);
      for (const item of items) if (onLayer(item, layer)) drawItem(ctx, item, opts);
    }
  }

  /** Both layers onto `ctx`, with `drafts` drawn into their own layers on top. */
  composite(ctx: Ctx, drafts: readonly ItemFields[] = []): void {
    for (const [canvas, layer] of [
      [this.color, "color"],
      [this.line, "line"],
    ] as const) {
      const mine = drafts.filter((d) => onLayer(d, layer));
      const t = mine.length === 0 ? null : this.tmp.getContext("2d");
      if (t === null) {
        ctx.drawImage(canvas, 0, 0);
        continue;
      }
      t.setTransform(1, 0, 0, 1, 0, 0);
      t.clearRect(0, 0, this.width, this.height);
      t.drawImage(canvas, 0, 0);
      t.setTransform(this.scale, 0, 0, this.scale, 0, 0);
      for (const d of mine) drawItem(t, d);
      ctx.drawImage(this.tmp, 0, 0);
    }
  }
}

/** Clear `ctx` and draw a whole drawing through `layers`. */
export const drawFrame = (ctx: Ctx, frame: Frame, layers: LayeredFrame, opts: DrawOptions = {}): void => {
  layers.render(frame.items, opts);
  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
  layers.composite(ctx);
};

/** `frame` drawn small on white paper, for the timeline. */
export const drawThumbnail = (ctx: Ctx, frame: Frame, canvasW: number, canvasH: number): void => {
  const { width, height } = ctx.canvas;
  const k = Math.min(width / canvasW, height / canvasH);
  const w = Math.max(1, Math.ceil(canvasW * k));
  const h = Math.max(1, Math.ceil(canvasH * k));
  ctx.fillStyle = "#ffffff";
  ctx.fillRect(0, 0, width, height);
  const layers = new LayeredFrame(w, h, k);
  // About 1.5 thumbnail pixels at the least, whatever the brush size.
  layers.render(frame.items, { minSize: 1.5 / k });
  ctx.save();
  ctx.translate((width - w) / 2, (height - h) / 2);
  layers.composite(ctx);
  ctx.restore();
};
