/**
 * Geometry of items: outlines, bounds, hit tests and transforms. Pure functions, used by
 * the renderer, the object eraser and the selection tool.
 *
 * A transform builds a new item through the domain's parsers, so moving something off
 * the edge of the world is a rejected edit, never an invalid item.
 */
import { andThen, each, err, ok, type Result } from "../domain/result";
import {
  makeItem,
  normalizeAngle,
  parseBrushSize,
  parseExtent,
  parsePoint,
  parseVec,
  type Color,
  type Item,
  type ItemFields,
  type Point,
  type ShapeFields,
  type Vec,
} from "../domain/project";

export type XY = readonly [number, number];

// Shapes --------------------------------------------------------------------------------

const rotateXY = (x: number, y: number, a: number): XY => [x * Math.cos(a) - y * Math.sin(a), x * Math.sin(a) + y * Math.cos(a)];

/** A shape's outline in canvas pixels; ellipses are sampled. */
export const shapeCorners = (s: Pick<ShapeFields, "shape" | "center" | "width" | "height" | "rotation">): XY[] => {
  const hw = s.width / 2;
  const hh = s.height / 2;
  const local: XY[] =
    s.shape === "line"
      ? [
          [-hw, 0],
          [hw, 0],
        ]
      : s.shape === "rect"
        ? [
            [-hw, -hh],
            [hw, -hh],
            [hw, hh],
            [-hw, hh],
          ]
        : s.shape === "triangle"
          ? [
              [0, -hh],
              [hw, hh],
              [-hw, hh],
            ]
          : Array.from({ length: 48 }, (_, i): XY => [Math.cos((i / 48) * Math.PI * 2) * hw, Math.sin((i / 48) * Math.PI * 2) * hh]);
  return local.map(([x, y]) => {
    const [rx, ry] = rotateXY(x, y, s.rotation);
    return [s.center[0] + rx, s.center[1] + ry];
  });
};

const curveSamples = (pts: readonly [Vec, Vec, Vec], n = 32): XY[] => {
  const [a, c, b] = pts;
  return Array.from({ length: n + 1 }, (_, i): XY => {
    const t = i / n;
    const u = 1 - t;
    return [u * u * a[0] + 2 * u * t * c[0] + t * t * b[0], u * u * a[1] + 2 * u * t * c[1] + t * t * b[1]];
  });
};

/** Points that trace the item, for lasso selection and bounds. */
export const itemOutline = (item: ItemFields): XY[] => {
  switch (item.kind) {
    case "stroke":
    case "erase":
    case "scatter":
      return item.points.map((p): XY => [p[0], p[1]]);
    case "shape":
      return shapeCorners(item);
    case "curve":
      return curveSamples(item.points);
    case "fill":
      return item.polygons.flatMap((poly) => poly.map((v): XY => [v[0], v[1]]));
  }
};

/** How far marks reach past the item's own points. */
const reach = (item: ItemFields): number => {
  switch (item.kind) {
    case "stroke":
    case "erase":
    case "shape":
    case "curve":
      return item.size / 2;
    case "scatter":
      // Stamps are about 1.6× the size, lifted off the path.
      return item.size * (item.brush === "birds" ? 3 : 1.8);
    case "fill":
      return 0;
  }
};

export interface Box {
  readonly x0: number;
  readonly y0: number;
  readonly x1: number;
  readonly y1: number;
}

export const itemBounds = (item: ItemFields): Box => {
  let x0 = Infinity;
  let y0 = Infinity;
  let x1 = -Infinity;
  let y1 = -Infinity;
  for (const [x, y] of itemOutline(item)) {
    x0 = Math.min(x0, x);
    y0 = Math.min(y0, y);
    x1 = Math.max(x1, x);
    y1 = Math.max(y1, y);
  }
  const r = reach(item);
  return { x0: x0 - r, y0: y0 - r, x1: x1 + r, y1: y1 + r };
};

export const unionBounds = (boxes: readonly Box[]): Box | undefined =>
  boxes.length === 0
    ? undefined
    : {
        x0: Math.min(...boxes.map((b) => b.x0)),
        y0: Math.min(...boxes.map((b) => b.y0)),
        x1: Math.max(...boxes.map((b) => b.x1)),
        y1: Math.max(...boxes.map((b) => b.y1)),
      };

// Hit tests -----------------------------------------------------------------------------

const segmentDistance = (px: number, py: number, a: XY, b: XY): number => {
  const dx = b[0] - a[0];
  const dy = b[1] - a[1];
  const len = dx * dx + dy * dy;
  const t = len === 0 ? 0 : Math.max(0, Math.min(1, ((px - a[0]) * dx + (py - a[1]) * dy) / len));
  return Math.hypot(px - (a[0] + dx * t), py - (a[1] + dy * t));
};

const polylineDistance = (px: number, py: number, pts: readonly XY[], closed: boolean): number => {
  const [first] = pts;
  if (first === undefined) return Infinity;
  if (pts.length === 1) return Math.hypot(px - first[0], py - first[1]);
  let best = Infinity;
  for (let i = 0; i + 1 < pts.length; i++) {
    const a = pts[i];
    const b = pts[i + 1];
    if (a !== undefined && b !== undefined) best = Math.min(best, segmentDistance(px, py, a, b));
  }
  const last = pts[pts.length - 1];
  if (closed && last !== undefined) best = Math.min(best, segmentDistance(px, py, last, first));
  return best;
};

/** Even-odd point in polygon. */
export const insidePolygon = (px: number, py: number, poly: readonly XY[]): boolean => {
  let inside = false;
  for (let i = 0, j = poly.length - 1; i < poly.length; j = i++) {
    const a = poly[i];
    const b = poly[j];
    if (a === undefined || b === undefined) continue;
    if (a[1] > py !== b[1] > py && px < ((b[0] - a[0]) * (py - a[1])) / (b[1] - a[1]) + a[0]) inside = !inside;
  }
  return inside;
};

/** Whether a press of radius `r` at (x, y) touches the item's marks. Erasers are never hit. */
export const hits = (item: ItemFields, x: number, y: number, r: number): boolean => {
  const b = itemBounds(item);
  if (x < b.x0 - r || x > b.x1 + r || y < b.y0 - r || y > b.y1 + r) return false;
  switch (item.kind) {
    case "erase":
      return false;
    case "stroke":
    case "scatter":
      return polylineDistance(x, y, itemOutline(item), false) <= r + reach(item);
    case "curve":
      return polylineDistance(x, y, itemOutline(item), false) <= r + reach(item);
    case "shape": {
      const outline = shapeCorners(item);
      if (item.fill !== null && item.shape !== "line" && insidePolygon(x, y, outline)) return true;
      return polylineDistance(x, y, outline, item.shape !== "line") <= r + reach(item);
    }
    case "fill": {
      let inside = false;
      for (const poly of item.polygons) if (insidePolygon(x, y, poly)) inside = !inside;
      return inside;
    }
  }
};

/** The topmost item under (x, y): line over colour, later over earlier. */
export const topmostAt = (items: readonly Item[], x: number, y: number, r: number): number | undefined => {
  for (const layer of ["line", "color"] as const)
    for (let i = items.length - 1; i >= 0; i--) {
      const item = items[i];
      if (item !== undefined && item.kind !== "erase" && item.layer === layer && hits(item, x, y, r)) return i;
    }
  return undefined;
};

// Transforms ----------------------------------------------------------------------------

/** x' = a·x + c·y + e, y' = b·x + d·y + f. */
export type Affine = readonly [a: number, b: number, c: number, d: number, e: number, f: number];

export const IDENTITY: Affine = [1, 0, 0, 1, 0, 0];

export const apply = (m: Affine, x: number, y: number): XY => [m[0] * x + m[2] * y + m[4], m[1] * x + m[3] * y + m[5]];

/** `m` after `n`: apply `n` first. */
export const compose = (m: Affine, n: Affine): Affine => [
  m[0] * n[0] + m[2] * n[1],
  m[1] * n[0] + m[3] * n[1],
  m[0] * n[2] + m[2] * n[3],
  m[1] * n[2] + m[3] * n[3],
  m[0] * n[4] + m[2] * n[5] + m[4],
  m[1] * n[4] + m[3] * n[5] + m[5],
];

export const translate = (dx: number, dy: number): Affine => [1, 0, 0, 1, dx, dy];

export const about = (cx: number, cy: number, m: Affine): Affine => compose(translate(cx, cy), compose(m, translate(-cx, -cy)));

export const rotation = (a: number): Affine => [Math.cos(a), Math.sin(a), -Math.sin(a), Math.cos(a), 0, 0];

export const scaling = (k: number): Affine => [k, 0, 0, k, 0, 0];

export const flipX: Affine = [-1, 0, 0, 1, 0, 0];

export const flipY: Affine = [1, 0, 0, -1, 0, 0];

const clampSize = (n: number): number => Math.max(0.5, Math.min(200, n));

const mapPoints = (m: Affine, pts: readonly Point[]): Result<Point[]> =>
  each(pts, (p) => {
    const [x, y] = apply(m, p[0], p[1]);
    return parsePoint([x, y, p[2]]);
  });

const mapVec = (m: Affine, v: Vec): Result<Vec> => parseVec(apply(m, v[0], v[1]));

const first = <T>(xs: readonly T[]): Result<readonly [T, ...T[]]> => {
  const [h, ...t] = xs;
  return h === undefined ? err("expected at least one point") : ok([h, ...t]);
};

/**
 * The item moved, turned, scaled or mirrored by `m` (a similarity: no shear). Line widths
 * scale with it; scatter stamps keep their seed, so they stay the same stamps.
 */
export const transformItem = (item: Item, m: Affine): Result<Item> => {
  const det = m[0] * m[3] - m[1] * m[2];
  const k = Math.sqrt(Math.abs(det));
  const sized = (n: number) => parseBrushSize(clampSize(n * k));
  switch (item.kind) {
    case "stroke":
    case "scatter":
    case "erase":
      return andThen(sized(item.size), (size) =>
        andThen(andThen(mapPoints(m, item.points), first), (points) => makeItem({ ...item, size, points })),
      );
    case "curve":
      return andThen(sized(item.size), (size) =>
        andThen(mapVec(m, item.points[0]), (a) =>
          andThen(mapVec(m, item.points[1]), (c) => andThen(mapVec(m, item.points[2]), (b) => makeItem({ ...item, size, points: [a, c, b] }))),
        ),
      );
    case "fill":
      return andThen(
        each(item.polygons, (poly) => andThen(each(poly, (v) => mapVec(m, v)), first)),
        (polys) => andThen(first(polys), (polygons) => makeItem({ ...item, polygons })),
      );
    case "shape": {
      // The shape's own x axis after the transform. A mirror flips its y axis too, which
      // every shape here is symmetric under once its x axis is turned half a turn.
      const ux = m[0] * Math.cos(item.rotation) + m[2] * Math.sin(item.rotation);
      const uy = m[1] * Math.cos(item.rotation) + m[3] * Math.sin(item.rotation);
      const turned = det < 0 ? Math.atan2(-uy, -ux) : Math.atan2(uy, ux);
      return andThen(sized(item.size), (size) =>
        andThen(mapVec(m, item.center), (center) =>
          andThen(parseExtent(item.width * k), (width) =>
            andThen(parseExtent(item.height * k), (height) => makeItem({ ...item, size, center, width, height, rotation: normalizeAngle(turned) })),
          ),
        ),
      );
    }
  }
};

/** The item in another colour; shape fills follow, erasers have none. */
export const recolor = (item: Item, color: Color): Result<Item> => {
  switch (item.kind) {
    case "erase":
      return ok(item);
    case "shape":
      return makeItem({ ...item, color, fill: item.fill === null ? null : color });
    case "stroke":
    case "scatter":
    case "curve":
    case "fill":
      return makeItem({ ...item, color });
  }
};
