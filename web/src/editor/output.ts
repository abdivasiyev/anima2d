/**
 * The finished picture at a frame: every visible drawing layer's cel, through the camera,
 * on white paper. Playback with the camera on and video export both draw through here,
 * so what plays in the editor is what is exported.
 */
import { celAt, isDrawing, type Blend, type Cel, type Ease, type Layer, type Project } from "../domain/project";
import { about, compose, IDENTITY, rotation, scaling, translate, type Affine } from "./geometry";
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

// Layer motion -----------------------------------------------------------------------------

/** Where a layer is drawn at a frame, and how faded. */
export interface Placement {
  readonly m: Affine;
  readonly fade: number;
}

const STILL: Placement = { m: IDENTITY, fade: 1 };

/** A layer's position offset, scale, turn and fade at a frame, before they become a matrix. */
export interface MotionValues {
  readonly x: number;
  readonly y: number;
  readonly scale: number;
  readonly rotation: number;
  readonly opacity: number;
}

export const REST: MotionValues = { x: 0, y: 0, scale: 1, rotation: 0, opacity: 1 };

/** A layer's motion at frame `t`: held before its first key and after its last, eased between. */
export const motionAt = (layer: Layer, t: number): MotionValues => {
  const keys = layer.motion?.keys;
  if (keys === undefined) return REST;
  const at = (k: (typeof keys)[number]): MotionValues => ({ x: k.position[0], y: k.position[1], scale: k.scale, rotation: k.rotation, opacity: k.opacity });
  const first = keys[0];
  if (t <= first.frame) return at(first);
  for (let i = 0; i + 1 < keys.length; i++) {
    const a = keys[i];
    const b = keys[i + 1];
    if (a === undefined || b === undefined || t > b.frame) continue;
    const u = ease(a.ease, (t - a.frame) / (b.frame - a.frame));
    // Scale moves evenly in ratio; turns take the short way.
    const turn = Math.atan2(Math.sin(b.rotation - a.rotation), Math.cos(b.rotation - a.rotation));
    return {
      x: a.position[0] + (b.position[0] - a.position[0]) * u,
      y: a.position[1] + (b.position[1] - a.position[1]) * u,
      scale: Math.exp(Math.log(a.scale) + (Math.log(b.scale) - Math.log(a.scale)) * u),
      rotation: a.rotation + turn * u,
      opacity: a.opacity + (b.opacity - a.opacity) * u,
    };
  }
  return at(keys[keys.length - 1] ?? first);
};

/** The matrix for motion values about a pivot: scale and turn about it, then move. */
export const motionMatrix = (v: MotionValues, pivot: readonly [number, number]): Affine =>
  compose(translate(v.x, v.y), about(pivot[0], pivot[1], compose(rotation(v.rotation), scaling(v.scale))));

export const placementAt = (layer: Layer, t: number): Placement => {
  if (layer.motion === null) return STILL;
  const v = motionAt(layer, t);
  return { m: motionMatrix(v, layer.motion.pivot), fade: v.opacity };
};

/** The canvas operation for a blend; the CSS `mix-blend-mode` is the same name but for "add". */
export const COMPOSITE: Record<Blend, GlobalCompositeOperation> = {
  normal: "source-over",
  multiply: "multiply",
  screen: "screen",
  overlay: "overlay",
  add: "lighter",
  darken: "darken",
  lighten: "lighten",
};

export const CSS_BLEND: Record<Blend, string> = { ...COMPOSITE, normal: "normal", add: "plus-lighter" };

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

  private scratch: [OffscreenCanvas, OffscreenCanvas, OffscreenCanvas] | undefined;

  /** Forget every frame (a picture or font arrived, so they would draw differently now). */
  clear(): void {
    this.map.clear();
  }

  /** Three canvases for clipping groups (the group, its mask, one clipped layer); made on first use. */
  clipCanvases(): [OffscreenCanvas, OffscreenCanvas, OffscreenCanvas] {
    this.scratch ??= [new OffscreenCanvas(this.width, this.height), new OffscreenCanvas(this.width, this.height), new OffscreenCanvas(this.width, this.height)];
    return this.scratch;
  }

  get(cel: Cel): OffscreenCanvas {
    const hit = this.map.get(cel);
    if (hit !== undefined) {
      this.map.delete(cel);
      this.map.set(cel, hit);
      return hit;
    }
    // Full, the least recently used canvas is drawn over rather than a new one made: the
    // memory stays fixed however long playback runs (Safari frees canvases late).
    const oldest = this.map.size >= this.limit ? this.map.keys().next() : undefined;
    let c: OffscreenCanvas | undefined;
    if (oldest !== undefined && oldest.done !== true) {
      c = this.map.get(oldest.value);
      this.map.delete(oldest.value);
    }
    c ??= new OffscreenCanvas(this.width, this.height);
    const ctx = c.getContext("2d");
    if (ctx !== null) {
      ctx.clearRect(0, 0, this.width, this.height);
      this.layers.render(cel.items);
      this.layers.composite(ctx);
    }
    this.map.set(cel, c);
    return c;
  }
}

// Scenes --------------------------------------------------------------------------------

export interface SceneOptions {
  /** Layers to draw: indices into the project's layers, bottom to top. */
  readonly layers?: readonly number[];
  /** Draws a reference layer (editor only; export leaves them out). */
  readonly reference?: (ctx: Ctx, layer: Layer) => void;
  /** Draws one layer's content instead of its cel (the editor's live drawing, with strokes in progress). */
  readonly override?: { readonly index: number; readonly paint: (ctx: Ctx) => void };
}

/**
 * The layers in `opts.layers` (default all) at frame `t`, onto `ctx` as it stands: each
 * moved by its motion, faded, and blended. A clipped layer shows only where the nearest
 * unclipped layer below it is; that base and the layers clipped to it are composed apart
 * and then blended in as one, by the base's blend.
 */
export const drawLayers = (ctx: Ctx, p: Project, t: number, cache: CelCache, opts: SceneOptions = {}): void => {
  const which = opts.layers ?? p.layers.map((_, i) => i);
  const paint = (c: Ctx, i: number, alpha: boolean): void => {
    const layer = p.layers[i];
    if (layer === undefined) return;
    const at = placementAt(layer, t);
    c.save();
    c.transform(...at.m);
    if (alpha) c.globalAlpha *= layer.opacity * at.fade;
    if (opts.override?.index === i) opts.override.paint(c);
    else if (isDrawing(layer)) {
      const k = celAt(layer, t);
      const cel = k === undefined ? undefined : layer.cels[k];
      if (cel !== undefined && cel.items.length > 0) c.drawImage(cache.get(cel), 0, 0);
    } else opts.reference?.(c, layer);
    c.restore();
  };
  for (let n = 0; n < which.length; n++) {
    const i = which[n];
    const layer = i === undefined ? undefined : p.layers[i];
    // A clipped layer without its base, or over a hidden one, shows nothing.
    if (i === undefined || layer === undefined || layer.clip) continue;
    const clipped: number[] = [];
    for (let j = n + 1; j < which.length; j++) {
      const c = which[j];
      const cl = c === undefined ? undefined : p.layers[c];
      if (c === undefined || cl?.clip !== true) break;
      if (cl.visible) clipped.push(c);
    }
    if (!layer.visible) continue;
    ctx.save();
    ctx.globalCompositeOperation = COMPOSITE[layer.blend];
    if (clipped.length === 0) paint(ctx, i, true);
    else {
      const [group, mask, one] = cache.clipCanvases();
      const g = group.getContext("2d");
      const m = mask.getContext("2d");
      const o = one.getContext("2d");
      if (g !== null && m !== null && o !== null) {
        for (const c of [g, m]) c.clearRect(0, 0, group.width, group.height);
        paint(g, i, true);
        paint(m, i, false);
        for (const c of clipped) {
          const cl = p.layers[c];
          if (cl === undefined) continue;
          o.globalCompositeOperation = "source-over";
          o.clearRect(0, 0, one.width, one.height);
          paint(o, c, true);
          o.globalCompositeOperation = "destination-in";
          o.drawImage(mask, 0, 0);
          g.globalCompositeOperation = COMPOSITE[cl.blend];
          g.drawImage(one, 0, 0);
          g.globalCompositeOperation = "source-over";
        }
        ctx.drawImage(group, 0, 0);
      }
    }
    ctx.restore();
  }
};

/** The finished frame: white paper (or none), every visible drawing layer, through the camera. */
export const drawOutput = (
  ctx: Ctx,
  p: Project,
  t: number,
  cache: CelCache,
  outW: number,
  outH: number,
  camera = true,
  fit: "contain" | "cover" = "contain",
  background: "white" | "transparent" = "white",
): void => {
  ctx.save();
  ctx.setTransform(1, 0, 0, 1, 0, 0);
  if (background === "white") {
    ctx.fillStyle = "#ffffff";
    ctx.fillRect(0, 0, outW, outH);
  } else ctx.clearRect(0, 0, outW, outH);
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
