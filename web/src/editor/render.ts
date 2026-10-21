/**
 * Drawing strokes. One function renders a frame for the canvas, the onion skins, the
 * timeline thumbnails and playback, so they all agree on what a stroke looks like.
 */
import { getStroke } from "perfect-freehand";

import type { Frame, Stroke } from "../domain/project";
import type { Sample } from "./stabilizer";

/** Either kind of 2D context: on-screen canvases and the off-screen layers share every call used here. */
export type Ctx = CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D;

/** How a stroke's outline responds to pressure; shared by live ink and stored strokes. */
const OUTLINE = { thinning: 0.62, smoothing: 0.55, streamline: 0.2, simulatePressure: false } as const;

const outline = (points: readonly (readonly [number, number, number])[], size: number, last: boolean): number[][] =>
  getStroke(
    points.map((p) => [p[0], p[1], p[2]]),
    { ...OUTLINE, size, last },
  );

const fillOutline = (ctx: Ctx, poly: number[][]): void => {
  const [first, ...rest] = poly;
  if (first === undefined) return;
  ctx.beginPath();
  ctx.moveTo(first[0] ?? 0, first[1] ?? 0);
  // Quadratic curves through the midpoints: a smooth edge without extra outline points.
  for (const [i, p] of rest.entries()) {
    const next = rest[i + 1] ?? first;
    ctx.quadraticCurveTo(p[0] ?? 0, p[1] ?? 0, ((p[0] ?? 0) + (next[0] ?? 0)) / 2, ((p[1] ?? 0) + (next[1] ?? 0)) / 2);
  }
  ctx.closePath();
  ctx.fill();
};

/** `minSize` keeps lines legible when drawn very small, as in thumbnails. */
export const drawStroke = (ctx: Ctx, stroke: Stroke, tint?: string, minSize = 0): void => {
  ctx.save();
  ctx.globalCompositeOperation = stroke.tool === "eraser" ? "destination-out" : "source-over";
  ctx.fillStyle = stroke.tool === "eraser" ? "#000" : (tint ?? stroke.color);
  fillOutline(ctx, outline(stroke.points, Math.max(stroke.size, minSize), true));
  ctx.restore();
};

/** The stroke still being drawn, from samples not yet parsed into a `Stroke`. */
export const drawLive = (ctx: Ctx, samples: readonly Sample[], size: number, color: string, eraser: boolean): void => {
  ctx.save();
  // The live layer sits above the drawing, so erasing shows as a translucent wash.
  ctx.fillStyle = eraser ? "rgba(120, 120, 128, 0.35)" : color;
  fillOutline(
    ctx,
    outline(
      samples.map((s) => [s.x, s.y, s.pressure]),
      size,
      false,
    ),
  );
  ctx.restore();
};

/** Clear `ctx` and draw every stroke of `frame` at the canvas' own resolution. */
export const drawFrame = (ctx: Ctx, frame: Frame, tint?: string): void => {
  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
  for (const s of frame.strokes) drawStroke(ctx, s, tint);
};

/** `frame` drawn small, for the timeline. */
export const drawThumbnail = (ctx: Ctx, frame: Frame, canvasW: number, canvasH: number): void => {
  const { width, height } = ctx.canvas;
  ctx.save();
  ctx.clearRect(0, 0, width, height);
  ctx.fillStyle = "#ffffff";
  ctx.fillRect(0, 0, width, height);
  const k = Math.min(width / canvasW, height / canvasH);
  // Erasers must cut only ink, not the white paper: draw ink on its own layer first.
  const layer = new OffscreenCanvas(Math.ceil(canvasW * k), Math.ceil(canvasH * k));
  const lctx = layer.getContext("2d");
  if (lctx !== null) {
    lctx.scale(k, k);
    // About 1.5 thumbnail pixels at the least, whatever the brush size.
    for (const s of frame.strokes) drawStroke(lctx, s, undefined, 1.5 / k);
    ctx.drawImage(layer, (width - canvasW * k) / 2, (height - canvasH * k) / 2);
  }
  ctx.restore();
};
