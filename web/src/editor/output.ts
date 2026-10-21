/**
 * The finished picture at a frame: every visible drawing layer's cel, through the camera,
 * on white paper. Playback with the camera on and video export both draw through here,
 * so what plays in the editor is what is exported.
 */
import { celAt, isDrawing, type Cel, type Ease, type Layer, type Project } from "../domain/project";
import { LayeredFrame, type Ctx } from "./render";

// Camera ----------------------------------------------------------------------------------

export interface Camera {
  readonly cx: number;
  readonly cy: number;
  readonly zoom: number;
  readonly rotation: number;
}

const EASE: Record<Ease, (u: number) => number> = {
  linear: (u) => u,
  in: (u) => u * u * u,
  out: (u) => 1 - (1 - u) ** 3,
  inOut: (u) => (u < 0.5 ? 4 * u * u * u : 1 - (-2 * u + 2) ** 3 / 2),
};

export const ease = (e: Ease, u: number): number => EASE[e](Math.max(0, Math.min(1, u)));

/** Where the camera is at frame `t`; undefined when the project has no camera keys. */
export const cameraAt = (p: Project, t: number): Camera | undefined => {
  const keys = p.camera;
  const first = keys[0];
  if (first === undefined) return undefined;
  const at = (k: (typeof keys)[number]): Camera => ({ cx: k.center[0], cy: k.center[1], zoom: k.zoom, rotation: k.rotation });
  if (t <= first.frame) return at(first);
  for (let i = 0; i + 1 < keys.length; i++) {
    const a = keys[i];
    const b = keys[i + 1];
    if (a === undefined || b === undefined || t > b.frame) continue;
    const u = ease(a.ease, (t - a.frame) / (b.frame - a.frame));
    // Zoom moves evenly in ratio, so a zoom from 1 to 4 passes 2 halfway; turns take the short way.
    const turn = Math.atan2(Math.sin(b.rotation - a.rotation), Math.cos(b.rotation - a.rotation));
    return {
      cx: a.center[0] + (b.center[0] - a.center[0]) * u,
      cy: a.center[1] + (b.center[1] - a.center[1]) * u,
      zoom: Math.exp(Math.log(a.zoom) + (Math.log(b.zoom) - Math.log(a.zoom)) * u),
      rotation: a.rotation + turn * u,
    };
  }
  const last = keys[keys.length - 1] ?? first;
  return at(last);
};

/** Canvas → output: the camera's centre lands in the middle, zoomed and turned. */
export const applyCamera = (ctx: Ctx, cam: Camera, w: number, h: number): void => {
  ctx.translate(w / 2, h / 2);
  ctx.scale(cam.zoom, cam.zoom);
  ctx.rotate(-cam.rotation);
  ctx.translate(-cam.cx, -cam.cy);
};

// Cels ----------------------------------------------------------------------------------

/**
 * Cels flattened to one bitmap each, the most recent `limit` kept. Cels are immutable, so
 * a cel object is its own cache key and an edit (a new cel) can never show a stale one.
 */
export class CelCache {
  private readonly map = new Map<Cel, OffscreenCanvas>();
  private readonly layers: LayeredFrame;

  constructor(
    private readonly width: number,
    private readonly height: number,
    private readonly limit = 48,
  ) {
    this.layers = new LayeredFrame(width, height);
  }

  get(cel: Cel): OffscreenCanvas {
    const hit = this.map.get(cel);
    if (hit !== undefined) {
      this.map.delete(cel);
      this.map.set(cel, hit);
      return hit;
    }
    const c = new OffscreenCanvas(this.width, this.height);
    const ctx = c.getContext("2d");
    if (ctx !== null) {
      this.layers.render(cel.items);
      this.layers.composite(ctx);
    }
    this.map.set(cel, c);
    for (const old of this.map.keys()) {
      if (this.map.size <= this.limit) break;
      this.map.delete(old);
    }
    return c;
  }
}

// Scenes --------------------------------------------------------------------------------

export interface SceneOptions {
  /** Layers to draw: indices into the project's layers, bottom to top. */
  readonly layers?: readonly number[];
  /** Draws a reference layer (editor only; export leaves them out). */
  readonly reference?: (ctx: Ctx, layer: Layer) => void;
}

/** The layers in `opts.layers` (default all) at frame `t`, onto `ctx` as it stands. */
export const drawLayers = (ctx: Ctx, p: Project, t: number, cache: CelCache, opts: SceneOptions = {}): void => {
  const which = opts.layers ?? p.layers.map((_, i) => i);
  for (const i of which) {
    const layer = p.layers[i];
    if (!layer?.visible) continue;
    ctx.save();
    ctx.globalAlpha = layer.opacity;
    if (isDrawing(layer)) {
      const c = celAt(layer, t);
      const cel = c === undefined ? undefined : layer.cels[c];
      if (cel !== undefined && cel.items.length > 0) ctx.drawImage(cache.get(cel), 0, 0);
    } else opts.reference?.(ctx, layer);
    ctx.restore();
  }
};

/** The finished frame: white paper, every visible drawing layer, through the camera. */
export const drawOutput = (ctx: Ctx, p: Project, t: number, cache: CelCache, outW: number, outH: number, camera = true, fit: "contain" | "cover" = "contain"): void => {
  ctx.save();
  ctx.setTransform(1, 0, 0, 1, 0, 0);
  ctx.fillStyle = "#ffffff";
  ctx.fillRect(0, 0, outW, outH);
  const cam = camera ? cameraAt(p, t) : undefined;
  // The canvas is scaled to fill the output (an export preset may differ in size).
  const k = (fit === "cover" ? Math.max : Math.min)(outW / p.canvas.width, outH / p.canvas.height);
  if (cam === undefined) {
    ctx.translate((outW - p.canvas.width * k) / 2, (outH - p.canvas.height * k) / 2);
    ctx.scale(k, k);
  } else {
    ctx.translate(outW / 2, outH / 2);
    ctx.scale(k, k);
    ctx.translate(-(p.canvas.width / 2), -(p.canvas.height / 2));
    applyCamera(ctx, cam, p.canvas.width, p.canvas.height);
  }
  drawLayers(ctx, p, t, cache, { layers: p.layers.flatMap((l, i) => (l.kind === "drawing" ? [i] : [])) });
  ctx.restore();
};
