/**
 * The paint bucket: the area around a tap, bounded by the line art, as polygons.
 *
 * The line layer is rasterised small, its lines thickened by the gap setting so a nearly
 * closed outline still holds the paint, and the area flood-filled from the tap. The area
 * is then grown back under the lines (so no white halo shows between paint and ink) and
 * traced with marching squares. Every outline is kept, holes included, and the result
 * renders even-odd: an island inside the area stays unpainted.
 */
import { each, err, ok, type Result } from "../domain/result";
import { parseVec, type Vec } from "../domain/project";
import type { XY } from "./geometry";

/** The longest side the line art is rasterised at; plenty for a fill and quick to trace. */
export const FILL_RASTER = 1600;

export interface Raster {
  readonly width: number;
  readonly height: number;
  /** One byte per pixel: 1 where the line art is. */
  readonly walls: Uint8Array;
  /** Raster pixels per canvas pixel. */
  readonly scale: number;
}

export const rasterFromAlpha = (rgba: Uint8ClampedArray, width: number, height: number, scale: number): Raster => {
  const walls = new Uint8Array(width * height);
  for (let i = 0; i < walls.length; i++) walls[i] = (rgba[i * 4 + 3] ?? 0) > 64 ? 1 : 0;
  return { width, height, walls, scale };
};

/** Square dilation by `r` pixels, as two passes of a sliding maximum. */
const dilate = (src: Uint8Array, w: number, h: number, r: number): Uint8Array => {
  if (r <= 0) return src;
  const tmp = new Uint8Array(w * h);
  const out = new Uint8Array(w * h);
  for (let y = 0; y < h; y++) {
    let run = -Infinity;
    for (let x = -r; x < w; x++) {
      const ahead = x + r;
      if (ahead < w && src[y * w + ahead] === 1) run = ahead;
      if (x >= 0 && run >= x - r) tmp[y * w + x] = 1;
    }
  }
  for (let x = 0; x < w; x++) {
    let run = -Infinity;
    for (let y = -r; y < h; y++) {
      const ahead = y + r;
      if (ahead < h && tmp[ahead * w + x] === 1) run = ahead;
      if (y >= 0 && run >= y - r) out[y * w + x] = 1;
    }
  }
  return out;
};

/** 4-connected flood fill of the open pixels from (sx, sy). */
const flood = (walls: Uint8Array, w: number, h: number, sx: number, sy: number): Uint8Array => {
  const region = new Uint8Array(w * h);
  const stack = new Int32Array(w * h);
  let top = 0;
  stack[top++] = sy * w + sx;
  region[sy * w + sx] = 1;
  while (top > 0) {
    const i = stack[--top] ?? 0;
    const x = i % w;
    const y = (i - x) / w;
    const push = (j: number): void => {
      if (region[j] === 0 && walls[j] === 0) {
        region[j] = 1;
        stack[top++] = j;
      }
    };
    if (x > 0) push(i - 1);
    if (x < w - 1) push(i + 1);
    if (y > 0) push(i - w);
    if (y < h - 1) push(i + w);
  }
  return region;
};

/**
 * Marching squares over the region: every boundary, outer and holes, as closed loops of
 * edge midpoints in raster pixels. Saddles are split the same way every time.
 */
const contours = (m: Uint8Array, w: number, h: number): XY[][] => {
  const at = (x: number, y: number): number => (x < 0 || y < 0 || x >= w || y >= h ? 0 : (m[y * w + x] ?? 0));
  // Midpoints are keyed on doubled coordinates, which are integers.
  const stride = 2 * w + 8;
  const key = (x2: number, y2: number): number => (y2 + 2) * stride + (x2 + 2);
  const ends = new Map<number, number[]>();
  const segs: [number, number][] = [];
  const pos = new Map<number, XY>();
  const add = (a: XY, b: XY): void => {
    const ka = key(a[0] * 2, a[1] * 2);
    const kb = key(b[0] * 2, b[1] * 2);
    pos.set(ka, a);
    pos.set(kb, b);
    const id = segs.length;
    segs.push([ka, kb]);
    for (const k of [ka, kb]) {
      const list = ends.get(k);
      if (list === undefined) ends.set(k, [id]);
      else list.push(id);
    }
  };
  for (let j = -1; j < h; j++)
    for (let i = -1; i < w; i++) {
      const code = (at(i, j) << 3) | (at(i + 1, j) << 2) | (at(i + 1, j + 1) << 1) | at(i, j + 1);
      if (code === 0 || code === 15) continue;
      const top: XY = [i + 1, j + 0.5];
      const right: XY = [i + 1.5, j + 1];
      const bottom: XY = [i + 1, j + 1.5];
      const left: XY = [i + 0.5, j + 1];
      switch (code) {
        case 1:
        case 14:
          add(left, bottom);
          break;
        case 2:
        case 13:
          add(bottom, right);
          break;
        case 3:
        case 12:
          add(left, right);
          break;
        case 4:
        case 11:
          add(top, right);
          break;
        case 6:
        case 9:
          add(top, bottom);
          break;
        case 7:
        case 8:
          add(top, left);
          break;
        case 5:
          add(top, right);
          add(left, bottom);
          break;
        case 10:
          add(top, left);
          add(bottom, right);
          break;
        default:
          break;
      }
    }
  const used = new Uint8Array(segs.length);
  const loops: XY[][] = [];
  for (let s = 0; s < segs.length; s++) {
    if (used[s] === 1) continue;
    const seg = segs[s];
    if (seg === undefined) continue;
    used[s] = 1;
    const start = seg[0];
    let here = seg[1];
    const loop: XY[] = [];
    const startPos = pos.get(start);
    if (startPos !== undefined) loop.push(startPos);
    while (here !== start) {
      const p = pos.get(here);
      if (p !== undefined) loop.push(p);
      const next = (ends.get(here) ?? []).find((id) => used[id] === 0);
      if (next === undefined) break;
      used[next] = 1;
      const [a, b] = segs[next] ?? [here, here];
      here = a === here ? b : a;
    }
    if (loop.length >= 3) loops.push(loop);
  }
  return loops;
};

/** Ramer–Douglas–Peucker on a closed loop. */
const simplify = (loop: readonly XY[], epsilon: number): XY[] => {
  const rdp = (pts: readonly XY[]): XY[] => {
    const a = pts[0];
    const b = pts[pts.length - 1];
    if (a === undefined || b === undefined || pts.length < 3) return [...pts];
    let worst = 0;
    let at = 0;
    const dx = b[0] - a[0];
    const dy = b[1] - a[1];
    const len = Math.hypot(dx, dy) || 1;
    for (let i = 1; i < pts.length - 1; i++) {
      const p = pts[i];
      if (p === undefined) continue;
      const d = Math.abs(dy * p[0] - dx * p[1] + b[0] * a[1] - b[1] * a[0]) / len;
      if (d > worst) {
        worst = d;
        at = i;
      }
    }
    if (worst <= epsilon) return [a, b];
    const left = rdp(pts.slice(0, at + 1));
    return [...left.slice(0, -1), ...rdp(pts.slice(at))];
  };
  const half = Math.floor(loop.length / 2);
  const out = [...rdp(loop.slice(0, half + 1)).slice(0, -1), ...rdp([...loop.slice(half), ...loop.slice(0, 1)]).slice(0, -1)];
  return out;
};

const area = (loop: readonly XY[]): number => {
  let s = 0;
  for (let i = 0, j = loop.length - 1; i < loop.length; j = i++) {
    const a = loop[i];
    const b = loop[j];
    if (a !== undefined && b !== undefined) s += (b[0] + a[0]) * (b[1] - a[1]);
  }
  return Math.abs(s / 2);
};

/**
 * The polygons a tap at canvas (x, y) fills, in canvas pixels. `gap` is in canvas pixels.
 * The point budget is the domain's: outlines are simplified harder until they fit.
 */
export const traceFill = (r: Raster, x: number, y: number, gap: number, maxPoints: number): Result<Vec[][]> => {
  const sx = Math.floor(x * r.scale);
  const sy = Math.floor(y * r.scale);
  if (sx < 0 || sy < 0 || sx >= r.width || sy >= r.height) return err("Tap inside the canvas to fill");
  const g = Math.round(gap * r.scale);
  const walls = dilate(r.walls, r.width, r.height, g);
  if (walls[sy * r.width + sx] === 1) return err("Tap inside an outlined area to fill");
  const region = dilate(flood(walls, r.width, r.height, sx, sy), r.width, r.height, g + 1);
  const loops = contours(region, r.width, r.height).filter((l) => area(l) >= 2);
  for (let epsilon = 0.6; epsilon < 64; epsilon *= 1.6) {
    const simple = loops.map((l) => simplify(l, epsilon)).filter((l) => l.length >= 3);
    if (simple.reduce((n, l) => n + l.length, 0) <= maxPoints && simple.length <= 500)
      return each(simple, (loop) => each(loop, ([px, py]) => parseVec([px / r.scale, py / r.scale])));
  }
  return ok([]);
};
