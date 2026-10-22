/**
 * The browser canvas the shared renderer expects, provided by Skia (@napi-rs/canvas).
 *
 * The renderer (web/src/editor) draws on `OffscreenCanvas`; here that name is given a
 * Skia canvas of the same size. This is the one adapter between the two worlds, so it is
 * where the types are bridged: Skia's context implements every call the renderer makes.
 */
import { GlobalFonts, createCanvas, loadImage } from "@napi-rs/canvas";

import type { ImageLoader } from "../../web/src/editor/media";

class SkiaOffscreenCanvas {
  constructor(width: number, height: number) {
    return createCanvas(width, height) as unknown as SkiaOffscreenCanvas;
  }
}

Reflect.set(globalThis, "OffscreenCanvas", SkiaOffscreenCanvas);

/** Pictures from a file path, as Skia images; the renderer draws them like browser bitmaps. */
export const skiaImages = (pathOf: (asset: string) => string): ImageLoader => async (asset) =>
  (await loadImage(pathOf(asset))) as unknown as CanvasImageSource & { readonly width: number; readonly height: number };

/** The canvas's pixels as a PNG (Skia encodes it). */
export const pngOf = (canvas: OffscreenCanvas): Buffer => (canvas as unknown as { toBuffer(mime: "image/png"): Buffer }).toBuffer("image/png");

/** A project's font file, under the family its text items name. */
export const registerFont = (path: string, family: string): boolean => GlobalFonts.registerFromPath(path, family) !== null;
