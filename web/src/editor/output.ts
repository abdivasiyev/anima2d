/**
 * The finished picture at a frame: every visible drawing layer's cel, through the camera,
 * on white paper. Playback with the camera on and video export both draw through here,
 * so what plays in the editor is what is exported.
 */
import { celAt, type Blend, type Cel, type Ease, type Effects, type Layer, type Light, type Project } from "../domain/project";
import { about, compose, IDENTITY, rotation, scaling, translate, type Affine } from "./geometry";
import { LayeredFrame, type Ctx } from "./render";

// Camera ----------------------------------------------------------------------------------

export interface Camera {
  readonly cx: number;
  readonly cy: number;
  readonly zoom: number;
  readonly rotation: number;
  /** Pixels of handheld shake. */
  readonly shake: number;
  /** The depth in focus, and blur in canvas pixels per unit of depth away from it. */
  readonly focus: number;
  readonly aperture: number;
  /** A colour over the frame, and how much of it. */
  readonly tint: string | null;
  readonly tintAmount: number;
}

const EASE: Record<Ease, (u: number) => number> = {
  linear: (u) => u,
  in: (u) => u * u * u,
  out: (u) => 1 - (1 - u) ** 3,
  inOut: (u) => (u < 0.5 ? 4 * u * u * u : 1 - (-2 * u + 2) ** 3 / 2),
};

export const ease = (e: Ease, u: number): number => EASE[e](Math.max(0, Math.min(1, u)));

const mixHex = (a: string, b: string, u: number): string => {
  const x = parseInt(a.slice(1), 16);
  const y = parseInt(b.slice(1), 16);
  const ch = (shift: number): number => Math.round(((x >> shift) & 255) + (((y >> shift) & 255) - ((x >> shift) & 255)) * u);
  return `#${((1 << 24) | (ch(16) << 16) | (ch(8) << 8) | ch(0)).toString(16).slice(1)}`;
};

/** Where the camera is at frame `t`; undefined when the project has no camera keys. */
export const cameraAt = (p: Project, t: number): Camera | undefined => {
  const keys = p.camera;
  const first = keys[0];
  if (first === undefined) return undefined;
  const at = (k: (typeof keys)[number]): Camera => ({
    cx: k.center[0],
    cy: k.center[1],
    zoom: k.zoom,
    rotation: k.rotation,
    shake: k.shake,
    focus: k.focus,
    aperture: k.aperture,
    tint: k.tint,
    tintAmount: k.tint === null ? 0 : k.tintAmount,
  });
  if (t <= first.frame) return at(first);
  for (let i = 0; i + 1 < keys.length; i++) {
    const a = keys[i];
    const b = keys[i + 1];
    if (a === undefined || b === undefined || t > b.frame) continue;
    const u = ease(a.ease, (t - a.frame) / (b.frame - a.frame));
    const lerp = (x: number, y: number): number => x + (y - x) * u;
    // Zoom moves evenly in ratio, so a zoom from 1 to 4 passes 2 halfway; turns take the short way.
    const turn = Math.atan2(Math.sin(b.rotation - a.rotation), Math.cos(b.rotation - a.rotation));
    // A tint fades in from (or out to) none in its own colour.
    const ta = a.tint === null ? 0 : a.tintAmount;
    const tb = b.tint === null ? 0 : b.tintAmount;
    const tint = a.tint !== null && b.tint !== null ? mixHex(a.tint, b.tint, u) : (a.tint ?? b.tint);
    return {
      cx: lerp(a.center[0], b.center[0]),
      cy: lerp(a.center[1], b.center[1]),
      zoom: Math.exp(lerp(Math.log(a.zoom), Math.log(b.zoom))),
      rotation: a.rotation + turn * u,
      shake: lerp(a.shake, b.shake),
      focus: lerp(a.focus, b.focus),
      aperture: lerp(a.aperture, b.aperture),
      tint,
      tintAmount: lerp(ta, tb),
    };
  }
  const last = keys[keys.length - 1] ?? first;
  return at(last);
};

/** Handheld shake at a frame: a few sines at odd rates, the same every time it plays. */
export const shakeAt = (cam: Camera, t: number): readonly [number, number] => {
  if (cam.shake === 0) return [0, 0];
  const x = Math.sin(t * 2.13) + 0.6 * Math.sin(t * 5.31 + 1.3) + 0.3 * Math.sin(t * 11.7 + 2.1);
  const y = Math.sin(t * 2.71 + 0.7) + 0.6 * Math.sin(t * 4.63 + 2.9) + 0.3 * Math.sin(t * 13.1 + 0.4);
  return [(cam.shake * x) / 1.9, (cam.shake * y) / 1.9];
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

/**
 * Canvas → output for a layer at `depth`: the camera's centre lands in the middle, zoomed
 * and turned. Farther layers (depth over 1) travel and zoom less, nearer ones more: parallax.
 */
export const applyCamera = (ctx: Ctx, cam: Camera, w: number, h: number, depth = 1, shake: readonly [number, number] = [0, 0]): void => {
  const cx = w / 2 + (cam.cx + shake[0] - w / 2) / depth;
  const cy = h / 2 + (cam.cy + shake[1] - h / 2) / depth;
  const zoom = cam.zoom ** (1 / depth);
  ctx.translate(w / 2, h / 2);
  ctx.scale(zoom, zoom);
  ctx.rotate(-cam.rotation);
  ctx.translate(-cx, -cy);
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

  private fx: [OffscreenCanvas, OffscreenCanvas] | undefined;
  private small = new Map<string, OffscreenCanvas>();

  /** Two canvases for a layer's effects (its picture, and its shadow or rim); made on first use. */
  effectCanvases(): [OffscreenCanvas, OffscreenCanvas] {
    this.fx ??= [new OffscreenCanvas(this.width, this.height), new OffscreenCanvas(this.width, this.height)];
    return this.fx;
  }

  /** A small canvas, for blurring where the canvas has no filter (drawn small, then stretched). */
  tiny(w: number, h: number): OffscreenCanvas {
    const key = `${w}x${h}`;
    let c = this.small.get(key);
    if (c === undefined) {
      if (this.small.size > 8) this.small.clear();
      c = new OffscreenCanvas(w, h);
      this.small.set(key, c);
    }
    return c;
  }

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
  /**
   * Through the camera: where a layer at a depth is placed (parallax), and how much it
   * blurs (depth of field, in canvas pixels). Without it, layers draw where they are.
   */
  readonly camera?: { readonly place: (ctx: Ctx, depth: number) => void; readonly blur: (depth: number) => number };
}

const rgba = (hex: string, a: number): string => {
  const n = parseInt(hex.slice(1), 16);
  return `rgba(${(n >> 16) & 255}, ${(n >> 8) & 255}, ${n & 255}, ${a})`;
};

/** A light's gradient, over more than the canvas so it still covers it when moved. */
const drawLight = (c: Ctx, light: Light, w: number, h: number): void => {
  const [x, y] = light.center;
  const full = rgba(light.color, light.intensity);
  const none = rgba(light.color, 0);
  let g: CanvasGradient;
  if (light.shape === "radial") {
    g = c.createRadialGradient(x, y, Math.min(light.radius * 0.999, light.radius * (1 - light.softness)), x, y, light.radius);
    g.addColorStop(0, full);
    g.addColorStop(1, none);
  } else {
    // A band across the canvas, brightest on the line through the centre, fading along `angle`.
    const dx = Math.cos(light.angle) * light.radius;
    const dy = Math.sin(light.angle) * light.radius;
    g = c.createLinearGradient(x - dx, y - dy, x + dx, y + dy);
    const k = Math.min(0.4995, (1 - light.softness) / 2);
    g.addColorStop(0, none);
    g.addColorStop(0.5 - k, full);
    g.addColorStop(0.5 + k, full);
    g.addColorStop(1, none);
  }
  c.fillStyle = g;
  c.fillRect(-w, -h, w * 3, h * 3);
};

let filterWorks: boolean | undefined;
/** Whether canvases here blur by filter (Chrome, Skia); iPad Safari's do not. */
const canFilter = (): boolean => {
  if (filterWorks === undefined) {
    try {
      const probe = new OffscreenCanvas(1, 1).getContext("2d");
      if (probe === null) filterWorks = false;
      else {
        probe.filter = "blur(2px)";
        filterWorks = probe.filter === "blur(2px)";
      }
    } catch {
      filterWorks = false;
    }
  }
  return filterWorks;
};

/** `src` onto `c` at (dx, dy), softened by `px` canvas pixels: by filter, or drawn small and stretched back. */
const softDraw = (c: Ctx, src: OffscreenCanvas, dx: number, dy: number, px: number, cache: CelCache): void => {
  if (px < 0.5) {
    c.drawImage(src, dx, dy);
    return;
  }
  if (canFilter()) {
    const m = c.getTransform();
    const k = Math.sqrt(Math.abs(m.a * m.d - m.b * m.c));
    c.filter = `blur(${px * k}px)`;
    c.drawImage(src, dx, dy);
    c.filter = "none";
    return;
  }
  const s = Math.max(1 / 32, Math.min(1, 1.5 / px));
  const t = cache.tiny(Math.max(1, Math.ceil(src.width * s)), Math.max(1, Math.ceil(src.height * s)));
  const tc = t.getContext("2d");
  if (tc === null) {
    c.drawImage(src, dx, dy);
    return;
  }
  tc.clearRect(0, 0, t.width, t.height);
  tc.imageSmoothingEnabled = true;
  tc.drawImage(src, 0, 0, t.width, t.height);
  c.imageSmoothingEnabled = true;
  c.drawImage(t, dx, dy, src.width, src.height);
};

/** A layer's own picture onto `c`, with its drop shadow under and rim light over. */
const withEffects = (c: Ctx, effects: Effects | null, cache: CelCache, content: (c: Ctx) => void): void => {
  if (effects === null || (effects.shadow === null && effects.rim === null)) {
    content(c);
    return;
  }
  const [pic, fx] = cache.effectCanvases();
  const p = pic.getContext("2d");
  const f = fx.getContext("2d");
  if (p === null || f === null) {
    content(c);
    return;
  }
  p.setTransform(1, 0, 0, 1, 0, 0);
  p.clearRect(0, 0, pic.width, pic.height);
  content(p);
  /** The picture's shape in one colour, optionally with `cut` (the shape moved) taken out. */
  const silhouette = (color: string, cut?: readonly [number, number]): void => {
    f.setTransform(1, 0, 0, 1, 0, 0);
    f.globalCompositeOperation = "source-over";
    f.clearRect(0, 0, fx.width, fx.height);
    f.drawImage(pic, 0, 0);
    if (cut !== undefined) {
      f.globalCompositeOperation = "destination-out";
      f.drawImage(pic, cut[0], cut[1]);
    }
    f.globalCompositeOperation = "source-in";
    f.fillStyle = color;
    f.fillRect(0, 0, fx.width, fx.height);
    f.globalCompositeOperation = "source-over";
  };
  const sh = effects.shadow;
  if (sh !== null) {
    silhouette(sh.color);
    c.save();
    c.globalAlpha *= sh.opacity;
    softDraw(c, fx, sh.offset[0], sh.offset[1], sh.blur, cache);
    c.restore();
  }
  c.drawImage(pic, 0, 0);
  const rim = effects.rim;
  if (rim !== null && rim.width > 0) {
    // The edge facing the light: where the shape is, and is not once moved towards the light.
    silhouette(rim.color, [-Math.cos(rim.angle) * rim.width, -Math.sin(rim.angle) * rim.width]);
    c.save();
    c.globalAlpha *= rim.opacity;
    softDraw(c, fx, 0, 0, Math.min(4, rim.width / 4), cache);
    c.restore();
  }
};

/**
 * The layers in `opts.layers` (default all) at frame `t`, onto `ctx` as it stands: each
 * moved by its motion, faded, blended, with its effects; lights drawn as light. A clipped
 * layer shows only where the nearest unclipped layer below it is; that base and the layers
 * clipped to it are composed apart and blended in as one, by the base's blend, at the
 * base's depth (through the camera, a group moves and blurs together).
 */
export const drawLayers = (ctx: Ctx, p: Project, t: number, cache: CelCache, opts: SceneOptions = {}): void => {
  const which = opts.layers ?? p.layers.map((_, i) => i);
  const w = p.canvas.width;
  const h = p.canvas.height;
  const paint = (c: Ctx, i: number, alpha: boolean): void => {
    const layer = p.layers[i];
    if (layer === undefined) return;
    const at = placementAt(layer, t);
    c.save();
    c.transform(...at.m);
    if (alpha) c.globalAlpha *= layer.opacity * at.fade;
    if (layer.kind === "light") drawLight(c, layer.light, w, h);
    else if (layer.kind === "reference") {
      if (opts.override?.index === i) opts.override.paint(c);
      else opts.reference?.(c, layer);
    } else
      withEffects(c, layer.effects, cache, (into) => {
        if (opts.override?.index === i) opts.override.paint(into);
        else {
          const k = celAt(layer, t);
          const cel = k === undefined ? undefined : layer.cels[k];
          if (cel !== undefined && cel.items.length > 0) into.drawImage(cache.get(cel), 0, 0);
        }
      });
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
    const blur = opts.camera?.blur(layer.depth) ?? 0;
    ctx.save();
    opts.camera?.place(ctx, layer.depth);
    ctx.globalCompositeOperation = COMPOSITE[layer.blend];
    if (clipped.length === 0 && blur < 0.5) paint(ctx, i, true);
    else {
      const [group, mask, one] = cache.clipCanvases();
      const g = group.getContext("2d");
      const m = mask.getContext("2d");
      const o = one.getContext("2d");
      if (g !== null && m !== null && o !== null) {
        g.setTransform(1, 0, 0, 1, 0, 0);
        g.clearRect(0, 0, group.width, group.height);
        paint(g, i, true);
        if (clipped.length > 0) {
          m.setTransform(1, 0, 0, 1, 0, 0);
          m.clearRect(0, 0, mask.width, mask.height);
          paint(m, i, false);
        }
        for (const c of clipped) {
          const cl = p.layers[c];
          if (cl === undefined) continue;
          o.setTransform(1, 0, 0, 1, 0, 0);
          o.globalCompositeOperation = "source-over";
          o.clearRect(0, 0, one.width, one.height);
          paint(o, c, true);
          o.globalCompositeOperation = "destination-in";
          o.drawImage(mask, 0, 0);
          o.globalCompositeOperation = "source-over";
          g.globalCompositeOperation = COMPOSITE[cl.blend];
          g.drawImage(one, 0, 0);
          g.globalCompositeOperation = "source-over";
        }
        softDraw(ctx, group, 0, 0, blur, cache);
      }
    }
    ctx.restore();
  }
};

/**
 * The finished frame: white paper (or none), every visible drawing and light layer,
 * through the camera with its parallax, shake, depth of field and tint.
 */
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
  const w = p.canvas.width;
  const h = p.canvas.height;
  // The canvas is scaled to fill the output (an export preset may differ in size).
  const k = (fit === "cover" ? Math.max : Math.min)(outW / w, outH / h);
  const layers = p.layers.flatMap((l, i) => (l.kind === "reference" ? [] : [i]));
  if (cam === undefined) {
    ctx.translate((outW - w * k) / 2, (outH - h * k) / 2);
    ctx.scale(k, k);
    drawLayers(ctx, p, t, cache, { layers });
  } else {
    ctx.translate(outW / 2, outH / 2);
    ctx.scale(k, k);
    ctx.translate(-(w / 2), -(h / 2));
    const shake = shakeAt(cam, t);
    drawLayers(ctx, p, t, cache, {
      layers,
      camera: {
        place: (c, depth) => {
          applyCamera(c, cam, w, h, depth, shake);
        },
        blur: (depth) => cam.aperture * Math.abs(depth - cam.focus),
      },
    });
    if (cam.tint !== null && cam.tintAmount > 0) {
      // White paper takes the tint as light does (multiplied); an empty background only where drawn.
      ctx.setTransform(1, 0, 0, 1, 0, 0);
      ctx.globalCompositeOperation = background === "white" ? "multiply" : "source-atop";
      ctx.globalAlpha = cam.tintAmount;
      ctx.fillStyle = cam.tint;
      ctx.fillRect(0, 0, outW, outH);
    }
  }
  ctx.restore();
};
