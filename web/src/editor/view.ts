/**
 * The view onto the canvas: pan, zoom and rotation, as one similarity transform.
 *
 * screen = translate(tx, ty) · rotate(rotation) · scale(scale) · canvas
 */
export interface View {
  readonly tx: number;
  readonly ty: number;
  readonly scale: number;
  /** Radians. */
  readonly rotation: number;
}

export const MIN_SCALE = 0.05;
export const MAX_SCALE = 32;

export const clampScale = (s: number): number => Math.min(MAX_SCALE, Math.max(MIN_SCALE, s));

/** A screen point (relative to the viewport's top-left) in canvas pixels. */
export const toCanvas = (v: View, x: number, y: number): readonly [number, number] => {
  const dx = x - v.tx;
  const dy = y - v.ty;
  const c = Math.cos(-v.rotation);
  const s = Math.sin(-v.rotation);
  return [(dx * c - dy * s) / v.scale, (dx * s + dy * c) / v.scale];
};

/** A canvas point on screen. */
export const toScreen = (v: View, x: number, y: number): readonly [number, number] => {
  const c = Math.cos(v.rotation);
  const s = Math.sin(v.rotation);
  return [v.tx + (x * c - y * s) * v.scale, v.ty + (x * s + y * c) * v.scale];
};

/** Fit a canvas of `w` × `h` into a viewport, centred, upright, with a margin. */
export const fit = (w: number, h: number, viewportW: number, viewportH: number, margin = 48): View => {
  const scale = clampScale(Math.min((viewportW - margin * 2) / w, (viewportH - margin * 2) / h));
  return { tx: (viewportW - w * scale) / 2, ty: (viewportH - h * scale) / 2, scale, rotation: 0 };
};

/**
 * The view after a two-finger gesture: the canvas point that was under the fingers'
 * midpoint stays under it, and the canvas turns and scales with the fingers.
 */
export const pinch = (
  start: View,
  a0: readonly [number, number],
  b0: readonly [number, number],
  a1: readonly [number, number],
  b1: readonly [number, number],
): View => {
  const d0 = Math.hypot(b0[0] - a0[0], b0[1] - a0[1]);
  const d1 = Math.hypot(b1[0] - a1[0], b1[1] - a1[1]);
  const scale = clampScale(start.scale * (d0 > 0 ? d1 / d0 : 1));
  const turn = Math.atan2(b1[1] - a1[1], b1[0] - a1[0]) - Math.atan2(b0[1] - a0[1], b0[0] - a0[0]);
  const rotation = start.rotation + turn;
  const m0: readonly [number, number] = [(a0[0] + b0[0]) / 2, (a0[1] + b0[1]) / 2];
  const m1: readonly [number, number] = [(a1[0] + b1[0]) / 2, (a1[1] + b1[1]) / 2];
  const [cx, cy] = toCanvas(start, m0[0], m0[1]);
  const c = Math.cos(rotation);
  const s = Math.sin(rotation);
  return { scale, rotation, tx: m1[0] - (cx * c - cy * s) * scale, ty: m1[1] - (cx * s + cy * c) * scale };
};

/** Snap a rotation within 4° of a quarter turn onto it, so the canvas comes back straight. */
export const snapRotation = (v: View): View => {
  const quarter = Math.PI / 2;
  const nearest = Math.round(v.rotation / quarter) * quarter;
  return Math.abs(v.rotation - nearest) < (4 * Math.PI) / 180 ? { ...v, rotation: nearest } : v;
};

export const cssTransform = (v: View): string => `translate(${v.tx}px, ${v.ty}px) rotate(${v.rotation}rad) scale(${v.scale})`;
