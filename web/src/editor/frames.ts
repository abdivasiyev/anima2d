/**
 * Exports that are frames rather than a video: a ZIP of PNGs (for compositing or another
 * editor; a transparent background stays transparent) and an animated GIF (for stickers and
 * previews; transparency is one bit, edges against nothing are cut hard). Each frame comes
 * from the same output renderer as playback and video export.
 */
import { Zip, ZipPassThrough } from "fflate";
import { GIFEncoder, applyPalette, quantize } from "gifenc";

import { presetFps, type Preset } from "../domain/export";
import { duration, type Project } from "../domain/project";
import { err, ok, type Result } from "../domain/result";
import { preloadImages, projectImages } from "./media";
import { CelCache, drawOutput } from "./output";
import type { Progress } from "./video";

interface Frames {
  readonly ctx: OffscreenCanvasRenderingContext2D;
  readonly canvas: OffscreenCanvas;
  readonly frames: number;
  readonly fps: number;
  draw(i: number): void;
}

const framesOf = async (p: Project, preset: Preset): Promise<Result<Frames>> => {
  const fps = presetFps(preset, p.fps);
  const frames = Math.max(1, Math.round((duration(p) / p.fps) * fps));
  const canvas = new OffscreenCanvas(preset.width, preset.height);
  const ctx = canvas.getContext("2d");
  if (ctx === null) return err("this browser cannot draw off screen");
  await preloadImages(projectImages(p));
  const cache = new CelCache(p.canvas.width, p.canvas.height, 32);
  return ok({
    ctx,
    canvas,
    frames,
    fps,
    draw: (i) => {
      drawOutput(ctx, p, Math.floor((i * p.fps) / fps), cache, preset.width, preset.height, preset.camera, preset.fit, preset.background);
    },
  });
};

/** Every frame as frame-00001.png … in a ZIP (stored, not squeezed again: PNGs are compressed). */
export const renderPngZip = async (p: Project, preset: Preset, onProgress: (pr: Progress) => void, signal: AbortSignal): Promise<Result<Blob>> => {
  const f = await framesOf(p, preset);
  if (!f.ok) return f;
  const parts: Uint8Array<ArrayBuffer>[] = [];
  let failed: string | undefined;
  const zip = new Zip((e, chunk) => {
    if (e !== null) failed = e.message;
    else parts.push(new Uint8Array(chunk));
  });
  for (let i = 0; i < f.value.frames; i++) {
    if (signal.aborted) return err("export cancelled");
    f.value.draw(i);
    const png = new Uint8Array(await (await f.value.canvas.convertToBlob({ type: "image/png" })).arrayBuffer());
    const entry = new ZipPassThrough(`frame-${String(i + 1).padStart(5, "0")}.png`);
    zip.add(entry);
    entry.push(png, true);
    onProgress({ frame: i + 1, frames: f.value.frames });
  }
  zip.end();
  if (failed !== undefined) return err(`the ZIP could not be written: ${failed}`);
  return ok(new Blob(parts, { type: "application/zip" }));
};

/** An animated GIF that loops forever; 256 colours a frame, delays rounded to GIF's hundredths. */
export const renderGif = async (p: Project, preset: Preset, onProgress: (pr: Progress) => void, signal: AbortSignal): Promise<Result<Blob>> => {
  const f = await framesOf(p, preset);
  if (!f.ok) return f;
  const { width, height } = preset;
  const transparent = preset.background === "transparent";
  const gif = GIFEncoder();
  const delay = Math.max(20, Math.round(100 / f.value.fps) * 10);
  for (let i = 0; i < f.value.frames; i++) {
    if (signal.aborted) return err("export cancelled");
    f.value.draw(i);
    const rgba = f.value.ctx.getImageData(0, 0, width, height).data;
    const format = transparent ? "rgba4444" : "rgb565";
    const palette = quantize(rgba, 256, transparent ? { format, oneBitAlpha: true, clearAlpha: true } : { format });
    const index = applyPalette(rgba, palette, format);
    const clear = transparent ? palette.findIndex((c) => c[3] === 0) : -1;
    gif.writeFrame(index, width, height, { palette, delay, repeat: 0, ...(clear >= 0 ? { transparent: true, transparentIndex: clear } : {}) });
    onProgress({ frame: i + 1, frames: f.value.frames });
    // Let the page breathe: quantising a large frame takes a while.
    await new Promise((r) => setTimeout(r, 0));
  }
  gif.finish();
  return ok(new Blob([new Uint8Array(gif.bytes())], { type: "image/gif" }));
};
