/**
 * An animation project, as the editor holds it.
 *
 * This mirrors the Haskell `Anima.Types.Project` field for field and rule for rule: the
 * server parses the same JSON with the same limits and the same messages (both suites run
 * the cases in fixtures/), so a project the editor can hold is one the server will store.
 * Every type is branded; the parsers and the smart constructors below are the only way to
 * get one.
 *
 * Format 2 (written): each drawing is an ordered list of items on two layers. Colour items
 * (paint, fills) always render under line items, so colouring never covers the linework.
 * Format 1 (read only): a list of pen and eraser strokes, upgraded on parse.
 */
import { brandChecked, type Brand } from "./brand";
import { andThen, at, each, err, map, ok, type Result } from "./result";
import { array, bind, bounded, field, integer, number, object, parseWith, string } from "./wire";

export const MAX_FRAMES = 10000;
/** The longest timeline, in frames. */
export const MAX_ITEMS_PER_FRAME = 5000;
export const MAX_POINTS_PER_ITEM = 20000;
export const MAX_POLYGONS_PER_FILL = 500;
export const MAX_POINTS_PER_FILL = 50000;
const MAX_COORDINATE = 100000;
const MAX_EXTENT = 200000;
const MAX_SEED = 2147483647;

const oneOf = <T extends string>(values: readonly T[], expected: string) => (raw: unknown): Result<T> => {
  const found = values.find((v) => v === raw);
  return found === undefined ? err(expected) : ok(found);
};

// Identity ------------------------------------------------------------------------------

/** 1 to 64 of a–z, 0–9 and "-": a UUID in practice, and a safe file name. */
export type ProjectId = Brand<string, "ProjectId">;

export const parseProjectId = (raw: unknown): Result<ProjectId> =>
  andThen(string(raw), (s) =>
    s.length < 1 || s.length > 64
      ? err("expected a project id of 1 to 64 characters")
      : /^[a-z0-9-]+$/.test(s)
        ? ok(brandChecked<string, "ProjectId">(s))
        : err("expected a project id of a-z, 0-9 and '-'"),
  );

/** 1 to 80 characters, trimmed, no control characters. */
export type ProjectName = Brand<string, "ProjectName">;

export const parseProjectName = (raw: unknown): Result<ProjectName> =>
  andThen(string(raw), (s) => {
    const trimmed = s.trim();
    if (trimmed.length < 1 || trimmed.length > 80) return err("expected a project name of 1 to 80 characters");
    // eslint-disable-next-line no-control-regex -- control characters are exactly what is refused
    if (/[\u0000-\u001f\u007f-\u009f]/.test(trimmed)) return err("expected a project name without control characters");
    return ok(brandChecked<string, "ProjectName">(trimmed));
  });

// Settings ------------------------------------------------------------------------------

export const FPS_VALUES = [12, 24, 25, 30, 48, 50, 60] as const;
export type Fps = (typeof FPS_VALUES)[number];

export const parseFps = (raw: unknown): Result<Fps> => {
  const found = FPS_VALUES.find((f) => f === raw);
  return found === undefined ? err("expected a frame rate of 12, 24, 25, 30, 48, 50 or 60") : ok(found);
};

/** Even (encoders refuse odd sizes), 64 to 7680. Width and height never swap: two brands. */
export type Width = Brand<number, "Width">;
export type Height = Brand<number, "Height">;

const dimension = (what: string, raw: unknown): Result<number> =>
  andThen(integer(raw), (n) =>
    n < 64 || n > 7680 ? err(`expected a ${what} from 64 to 7680 pixels`) : n % 2 !== 0 ? err(`expected an even ${what}`) : ok(n),
  );

export const parseWidth = (raw: unknown): Result<Width> => map(dimension("width", raw), (n) => brandChecked<number, "Width">(n));
export const parseHeight = (raw: unknown): Result<Height> => map(dimension("height", raw), (n) => brandChecked<number, "Height">(n));

export type Canvas = Brand<{ readonly width: Width; readonly height: Height }, "Canvas">;

export const parseCanvas = (raw: unknown): Result<Canvas> =>
  andThen(object(raw), (o) =>
    andThen(at("width", parseWidth(o.width)), (width) =>
      map(at("height", parseHeight(o.height)), (height) => brandChecked<{ width: Width; height: Height }, "Canvas">({ width, height })),
    ),
  );

// Values items are made of --------------------------------------------------------------

export const PLANES = ["line", "color"] as const;
/** Which plane of a drawing an item is on: colour renders under line, always. (On the wire: "layer".) */
export type Plane = (typeof PLANES)[number];
export const parsePlane = oneOf(PLANES, 'expected a layer of "line" or "color"');

export const BRUSHES = ["pen", "pencil", "marker", "paint"] as const;
export type BrushKind = (typeof BRUSHES)[number];
export const parseBrush = oneOf(BRUSHES, 'expected a brush of "pen", "pencil", "marker" or "paint"');

export const SCATTERS = ["grass", "birds", "stars", "leaves", "flowers", "spray"] as const;
export type ScatterKind = (typeof SCATTERS)[number];
export const parseScatter = oneOf(SCATTERS, 'expected a scatter brush of "grass", "birds", "stars", "leaves", "flowers" or "spray"');

export const SHAPES = ["line", "rect", "ellipse", "triangle"] as const;
export type ShapeKind = (typeof SHAPES)[number];
export const parseShape = oneOf(SHAPES, 'expected a shape of "line", "rect", "ellipse" or "triangle"');

/** sRGB as #rrggbb, kept lower-case. */
export type Color = Brand<string, "Color">;

export const parseColor = (raw: unknown): Result<Color> =>
  andThen(string(raw), (s) =>
    /^#[0-9a-fA-F]{6}$/.test(s) ? ok(brandChecked<string, "Color">(s.toLowerCase())) : err("expected a colour as #rrggbb"),
  );

/** A shape's fill: a colour, or none (`null` on the wire). */
export const parseFillColor = (raw: unknown): Result<Color | null> => {
  if (raw === null) return ok(null);
  const c = parseColor(raw);
  return c.ok ? c : err("expected a colour as #rrggbb or null");
};

/** Diameter in canvas pixels at full pressure, 0.5 to 200. */
export type BrushSize = Brand<number, "BrushSize">;

export const parseBrushSize = (raw: unknown): Result<BrushSize> =>
  andThen(number(raw), (n) => (n >= 0.5 && n <= 200 ? ok(brandChecked<number, "BrushSize">(n)) : err("expected a brush size from 0.5 to 200")));

export type Opacity = Brand<number, "Opacity">;

export const parseOpacity = (raw: unknown): Result<Opacity> =>
  andThen(number(raw), (n) => (n >= 0.05 && n <= 1 ? ok(brandChecked<number, "Opacity">(n)) : err("expected an opacity from 0.05 to 1")));

export type Pressure = Brand<number, "Pressure">;

export const parsePressure = (raw: unknown): Result<Pressure> =>
  andThen(number(raw), (n) => (n >= 0 && n <= 1 ? ok(brandChecked<number, "Pressure">(n)) : err("expected a pressure from 0 to 1")));

/** Fixes a scatter stroke's randomness, so it renders the same every time. */
export type Seed = Brand<number, "Seed">;

export const parseSeed = (raw: unknown): Result<Seed> =>
  andThen(integer(raw), (n) => (n >= 0 && n <= MAX_SEED ? ok(brandChecked<number, "Seed">(n)) : err(`expected a seed from 0 to ${MAX_SEED}`)));

/** A shape's width or height in canvas pixels, 0 to 200 000. */
export type Extent = Brand<number, "Extent">;

export const parseExtent = (raw: unknown): Result<Extent> =>
  andThen(number(raw), (n) => (n >= 0 && n <= MAX_EXTENT ? ok(brandChecked<number, "Extent">(n)) : err(`expected a size from 0 to ${MAX_EXTENT} pixels`)));

/** Radians, -π to π. */
export type Angle = Brand<number, "Angle">;

export const parseAngle = (raw: unknown): Result<Angle> =>
  andThen(number(raw), (n) => (n >= -Math.PI && n <= Math.PI ? ok(brandChecked<number, "Angle">(n)) : err("expected an angle from -pi to pi")));

/** Any finite angle folded into -π to π, for constructors that turn things. */
export const normalizeAngle = (a: number): Angle => {
  const turned = Math.atan2(Math.sin(a), Math.cos(a));
  return brandChecked<number, "Angle">(Math.max(-Math.PI, Math.min(Math.PI, turned)));
};

const coordinate = (raw: unknown): Result<number> =>
  andThen(number(raw), (n) => (Math.abs(n) <= MAX_COORDINATE ? ok(n) : err("out of range")));

/** A position in canvas pixels, at most 100 000 px from the origin. May lie off the canvas. */
export type Vec = Brand<readonly [x: number, y: number], "Vec">;

export const parseVec = (raw: unknown): Result<Vec> => {
  const expected = "expected a position as [x, y] within 100000 pixels of the origin";
  const xs = array(raw);
  if (!xs.ok || xs.value.length !== 2) return err(expected);
  const x = coordinate(xs.value[0]);
  const y = coordinate(xs.value[1]);
  return x.ok && y.ok ? ok(brandChecked<readonly [number, number], "Vec">([x.value, y.value])) : err(expected);
};

/** One pen sample: position and pressure. */
export type Point = Brand<readonly [x: number, y: number, pressure: Pressure], "Point">;

export const parsePoint = (raw: unknown): Result<Point> =>
  andThen(array(raw), (xs) => {
    if (xs.length !== 3) return err("expected a point as [x, y, pressure]");
    const [rx, ry, rp] = xs;
    const x = coordinate(rx);
    const y = coordinate(ry);
    if (!x.ok || !y.ok) return err("expected coordinates within 100000 pixels of the origin");
    return map(parsePressure(rp), (p) => brandChecked<readonly [number, number, Pressure], "Point">([x.value, y.value, p]));
  });

type NonEmpty<T> = readonly [T, ...T[]];

const nonEmpty = <T>(xs: readonly T[]): NonEmpty<T> | undefined => {
  const [first, ...rest] = xs;
  return first === undefined ? undefined : [first, ...rest];
};

// Items ---------------------------------------------------------------------------------

/** A freehand line: pen, pencil, marker, or the paint brush. */
export interface StrokeFields {
  readonly kind: "stroke";
  readonly layer: Plane;
  readonly brush: BrushKind;
  readonly color: Color;
  readonly size: BrushSize;
  readonly opacity: Opacity;
  readonly points: NonEmpty<Point>;
}

/** The pixel eraser's path. It cuts both layers, under and over, at its place in the order. */
export interface EraseFields {
  readonly kind: "erase";
  readonly size: BrushSize;
  readonly points: NonEmpty<Point>;
}

/** A path stamped along with grass, birds, stars and the like. */
export interface ScatterFields {
  readonly kind: "scatter";
  readonly layer: Plane;
  readonly brush: ScatterKind;
  readonly color: Color;
  readonly size: BrushSize;
  readonly opacity: Opacity;
  readonly seed: Seed;
  readonly points: NonEmpty<Point>;
}

/** A line, rectangle, ellipse or triangle, by its centre, size and turn. */
export interface ShapeFields {
  readonly kind: "shape";
  readonly layer: Plane;
  readonly shape: ShapeKind;
  readonly color: Color;
  readonly size: BrushSize;
  readonly opacity: Opacity;
  readonly fill: Color | null;
  readonly center: Vec;
  readonly width: Extent;
  readonly height: Extent;
  readonly rotation: Angle;
}

/** A quadratic Bézier: start, control point, end. */
export interface CurveFields {
  readonly kind: "curve";
  readonly layer: Plane;
  readonly color: Color;
  readonly size: BrushSize;
  readonly opacity: Opacity;
  readonly points: readonly [Vec, Vec, Vec];
}

/** A paint-bucket fill: one or more outlines, filled even-odd (so holes stay open). */
export interface FillFields {
  readonly kind: "fill";
  readonly layer: Plane;
  readonly color: Color;
  readonly opacity: Opacity;
  readonly polygons: NonEmpty<NonEmpty<Vec>>;
}

export type ItemFields = StrokeFields | EraseFields | ScatterFields | ShapeFields | CurveFields | FillFields;
export type Item = Brand<ItemFields, "Item">;
export type ItemKind = ItemFields["kind"];

/** Build an item from values already parsed; only counts are left to check. */
export const makeItem = (fields: ItemFields): Result<Item> => {
  switch (fields.kind) {
    case "stroke":
    case "erase":
    case "scatter":
      if (fields.points.length > MAX_POINTS_PER_ITEM) return err(`expected 1 to ${MAX_POINTS_PER_ITEM} points`);
      break;
    case "fill": {
      if (fields.polygons.length > MAX_POLYGONS_PER_FILL) return err(`expected 1 to ${MAX_POLYGONS_PER_FILL} polygons`);
      const total = fields.polygons.reduce((n, p) => n + p.length, 0);
      if (fields.polygons.some((p) => p.length < 3 || p.length > MAX_POINTS_PER_ITEM)) return err(`expected 3 to ${MAX_POINTS_PER_ITEM} points`);
      if (total > MAX_POINTS_PER_FILL) return err(`expected at most ${MAX_POINTS_PER_FILL} points in a fill`);
      break;
    }
    case "shape":
    case "curve":
      break;
  }
  return ok(brandChecked<ItemFields, "Item">(fields));
};

const parsePoints = (raw: unknown): Result<NonEmpty<Point>> =>
  andThen(andThen(array(raw), (xs) => bounded("points", 1, MAX_POINTS_PER_ITEM, xs)), (xs) =>
    andThen(each(xs, parsePoint), (ps) => {
      const ne = nonEmpty(ps);
      return ne === undefined ? err(`expected 1 to ${MAX_POINTS_PER_ITEM} points`) : ok(ne);
    }),
  );

const parseCurvePoints = (raw: unknown): Result<readonly [Vec, Vec, Vec]> => {
  const xs = array(raw);
  if (!xs.ok || xs.value.length !== 3) return err("expected a curve as [start, control, end]");
  return andThen(each(xs.value, parseVec), (vs) => {
    const [a, c, b] = vs;
    return a === undefined || c === undefined || b === undefined ? err("expected a curve as [start, control, end]") : ok([a, c, b] as const);
  });
};

const parsePolygons = (raw: unknown): Result<NonEmpty<NonEmpty<Vec>>> =>
  andThen(andThen(array(raw), (xs) => bounded("polygons", 1, MAX_POLYGONS_PER_FILL, xs)), (polys) =>
    andThen(
      each(polys, (poly) =>
        andThen(andThen(array(poly), (xs) => bounded("points", 3, MAX_POINTS_PER_ITEM, xs)), (xs) =>
          andThen(each(xs, parseVec), (vs) => {
            const ne = nonEmpty(vs);
            return ne === undefined ? err(`expected 3 to ${MAX_POINTS_PER_ITEM} points`) : ok(ne);
          }),
        ),
      ),
      (ps) => {
        const ne = nonEmpty(ps);
        return ne === undefined ? err(`expected 1 to ${MAX_POLYGONS_PER_FILL} polygons`) : ok(ne);
      },
    ),
  );

const ITEM_KINDS = ["stroke", "erase", "scatter", "shape", "curve", "fill"] as const;
const parseKind = oneOf(ITEM_KINDS, 'expected an item kind of "stroke", "erase", "scatter", "shape", "curve" or "fill"');

/** Fields are read in wire order, so the first rejection is the same one the server names. */
export const parseItem = (raw: unknown): Result<Item> =>
  parseWith(function* () {
    const o = yield* bind(object(raw));
    switch (yield* field(o, "kind", parseKind)) {
      case "stroke":
        return makeItem({
          kind: "stroke",
          layer: yield* field(o, "layer", parsePlane),
          brush: yield* field(o, "brush", parseBrush),
          color: yield* field(o, "color", parseColor),
          size: yield* field(o, "size", parseBrushSize),
          opacity: yield* field(o, "opacity", parseOpacity),
          points: yield* field(o, "points", parsePoints),
        });
      case "erase":
        return makeItem({ kind: "erase", size: yield* field(o, "size", parseBrushSize), points: yield* field(o, "points", parsePoints) });
      case "scatter":
        return makeItem({
          kind: "scatter",
          layer: yield* field(o, "layer", parsePlane),
          brush: yield* field(o, "brush", parseScatter),
          color: yield* field(o, "color", parseColor),
          size: yield* field(o, "size", parseBrushSize),
          opacity: yield* field(o, "opacity", parseOpacity),
          seed: yield* field(o, "seed", parseSeed),
          points: yield* field(o, "points", parsePoints),
        });
      case "shape":
        return makeItem({
          kind: "shape",
          layer: yield* field(o, "layer", parsePlane),
          shape: yield* field(o, "shape", parseShape),
          color: yield* field(o, "color", parseColor),
          size: yield* field(o, "size", parseBrushSize),
          opacity: yield* field(o, "opacity", parseOpacity),
          fill: yield* field(o, "fill", parseFillColor),
          center: yield* field(o, "center", parseVec),
          width: yield* field(o, "width", parseExtent),
          height: yield* field(o, "height", parseExtent),
          rotation: yield* field(o, "rotation", parseAngle),
        });
      case "curve":
        return makeItem({
          kind: "curve",
          layer: yield* field(o, "layer", parsePlane),
          color: yield* field(o, "color", parseColor),
          size: yield* field(o, "size", parseBrushSize),
          opacity: yield* field(o, "opacity", parseOpacity),
          points: yield* field(o, "points", parseCurvePoints),
        });
      case "fill":
        return makeItem({
          kind: "fill",
          layer: yield* field(o, "layer", parsePlane),
          color: yield* field(o, "color", parseColor),
          opacity: yield* field(o, "opacity", parseOpacity),
          polygons: yield* field(o, "polygons", parsePolygons),
        });
    }
  });

const vec = (v: Vec): unknown => [v[0], v[1]];
const point = (p: Point): unknown => [p[0], p[1], p[2]];

export const unparseItem = (item: Item): unknown => {
  switch (item.kind) {
    case "stroke":
      return { kind: item.kind, layer: item.layer, brush: item.brush, color: item.color, size: item.size, opacity: item.opacity, points: item.points.map(point) };
    case "erase":
      return { kind: item.kind, size: item.size, points: item.points.map(point) };
    case "scatter":
      return { kind: item.kind, layer: item.layer, brush: item.brush, color: item.color, size: item.size, opacity: item.opacity, seed: item.seed, points: item.points.map(point) };
    case "shape":
      return {
        kind: item.kind,
        layer: item.layer,
        shape: item.shape,
        color: item.color,
        size: item.size,
        opacity: item.opacity,
        fill: item.fill,
        center: vec(item.center),
        width: item.width,
        height: item.height,
        rotation: item.rotation,
      };
    case "curve":
      return { kind: item.kind, layer: item.layer, color: item.color, size: item.size, opacity: item.opacity, points: item.points.map(vec) };
    case "fill":
      return { kind: item.kind, layer: item.layer, color: item.color, opacity: item.opacity, polygons: item.polygons.map((p) => p.map(vec)) };
  }
};

// Cels ----------------------------------------------------------------------------------

/** How many timeline frames one drawing is exposed for: 1 (on ones) to 24. */
export type Hold = Brand<number, "Hold">;

export const parseHold = (raw: unknown): Result<Hold> =>
  andThen(integer(raw), (n) => (n >= 1 && n <= 24 ? ok(brandChecked<number, "Hold">(n)) : err("expected a hold from 1 to 24 frames")));

export const MARKS = ["none", "key", "breakdown", "inbetween"] as const;
/** The classic timing marks: keys, the breakdowns between them, and the inbetweens. */
export type Mark = (typeof MARKS)[number];
export const parseMark = oneOf(MARKS, 'expected a mark of "none", "key", "breakdown" or "inbetween"');

/** One drawing on a layer's timeline, held for `hold` frames. */
export type Cel = Brand<{ readonly hold: Hold; readonly mark: Mark; readonly items: readonly Item[] }, "Cel">;

export const makeCel = (hold: Hold, mark: Mark, items: readonly Item[]): Result<Cel> =>
  items.length > MAX_ITEMS_PER_FRAME ? err(`expected 0 to ${MAX_ITEMS_PER_FRAME} items`) : ok(brandChecked({ hold, mark, items }));

const parseItems = (key: string, parse: (raw: unknown) => Result<Item>) => (raw: unknown): Result<Item[]> =>
  andThen(andThen(array(raw), (xs) => bounded(key, 0, MAX_ITEMS_PER_FRAME, xs)), (xs) => each(xs, parse));

const parseCelV3 = (raw: unknown): Result<Cel> =>
  parseWith(function* () {
    const o = yield* bind(object(raw));
    const hold = yield* field(o, "hold", parseHold);
    const mark = yield* field(o, "mark", parseMark);
    const items = yield* field(o, "items", parseItems("items", parseItem));
    return makeCel(hold, mark, items);
  });

const parseCelV2 = (raw: unknown): Result<Cel> =>
  parseWith(function* () {
    const o = yield* bind(object(raw));
    const hold = yield* field(o, "hold", parseHold);
    const items = yield* field(o, "items", parseItems("items", parseItem));
    return makeCel(hold, "none", items);
  });

/** A format 1 stroke: a pen line or an eraser path. */
const parseStrokeV1 = (raw: unknown): Result<Item> =>
  parseWith(function* () {
    const o = yield* bind(object(raw));
    const tool = yield* field(o, "tool", oneOf(["pen", "eraser"] as const, 'expected a tool of "pen" or "eraser"'));
    const color = yield* field(o, "color", parseColor);
    const size = yield* field(o, "size", parseBrushSize);
    const points = yield* field(o, "points", parsePoints);
    return tool === "eraser"
      ? makeItem({ kind: "erase", size, points })
      : makeItem({ kind: "stroke", layer: "line", brush: "pen", color, size, opacity: brandChecked<number, "Opacity">(1), points });
  });

const parseCelV1 = (raw: unknown): Result<Cel> =>
  parseWith(function* () {
    const o = yield* bind(object(raw));
    const hold = yield* field(o, "hold", parseHold);
    const items = yield* field(o, "strokes", parseItems("strokes", parseStrokeV1));
    return makeCel(hold, "none", items);
  });

// Layers --------------------------------------------------------------------------------

export const MAX_LAYERS = 20;

/** A layer's id within its project: 1 to 32 of a–z, 0–9 and "-". */
export type LayerId = Brand<string, "LayerId">;

export const parseLayerId = (raw: unknown): Result<LayerId> =>
  andThen(string(raw), (s) =>
    s.length >= 1 && s.length <= 32 && /^[a-z0-9-]+$/.test(s) ? ok(brandChecked<string, "LayerId">(s)) : err("expected a layer id of 1 to 32 of a-z, 0-9 and '-'"),
  );

/** An uploaded file (reference image or video, audio): 1 to 64 of a–z, 0–9 and "-". */
export type AssetId = Brand<string, "AssetId">;

export const parseAssetId = (raw: unknown): Result<AssetId> =>
  andThen(string(raw), (s) =>
    s.length >= 1 && s.length <= 64 && /^[a-z0-9-]+$/.test(s) ? ok(brandChecked<string, "AssetId">(s)) : err("expected an asset id of 1 to 64 of a-z, 0-9 and '-'"),
  );

/** A layer's or track's name: 1 to 40 characters, trimmed, no control characters. */
export type LabelName = Brand<string, "LabelName">;

export const parseLabel = (raw: unknown): Result<LabelName> =>
  andThen(string(raw), (s) => {
    const t = s.trim();
    if (t.length < 1 || t.length > 40) return err("expected a name of 1 to 40 characters");
    // eslint-disable-next-line no-control-regex -- control characters are exactly what is refused
    if (/[\u0000-\u001f\u007f-\u009f]/.test(t)) return err("expected a name without control characters");
    return ok(brandChecked<string, "LabelName">(t));
  });

export const MEDIA = ["image", "video"] as const;
export type Media = (typeof MEDIA)[number];
export const parseMedia = oneOf(MEDIA, 'expected media of "image" or "video"');

const parseBool = (raw: unknown): Result<boolean> => (typeof raw === "boolean" ? ok(raw) : err("expected true or false"));

/** Drawings on a timeline. `holdLast` keeps the last one showing to the end (backgrounds). */
export interface DrawingLayerFields {
  readonly kind: "drawing";
  readonly id: LayerId;
  readonly name: LabelName;
  readonly visible: boolean;
  readonly locked: boolean;
  readonly opacity: Opacity;
  readonly holdLast: boolean;
  readonly cels: NonEmpty<Cel>;
}

/** An image or video to trace over; shown in the editor, never exported. */
export interface ReferenceLayerFields {
  readonly kind: "reference";
  readonly id: LayerId;
  readonly name: LabelName;
  readonly visible: boolean;
  readonly locked: boolean;
  readonly opacity: Opacity;
  readonly asset: AssetId;
  readonly media: Media;
}

export type LayerFields = DrawingLayerFields | ReferenceLayerFields;
export type Layer = Brand<LayerFields, "Layer">;
export type DrawingLayer = Brand<DrawingLayerFields, "Layer">;

export const makeLayer = (fields: LayerFields): Result<Layer> =>
  fields.kind === "drawing" && fields.cels.length > MAX_FRAMES ? err(`expected 1 to ${MAX_FRAMES} cels`) : ok(brandChecked<LayerFields, "Layer">(fields));

export const isDrawing = (l: Layer): l is DrawingLayer => l.kind === "drawing";

/** A layer's cels; formats 1 and 2 called them frames, and their messages still do. */
const parseCels = (parseCel: (raw: unknown) => Result<Cel>, what = "cels") => (raw: unknown): Result<NonEmpty<Cel>> =>
  andThen(andThen(andThen(array(raw), (xs) => bounded(what, 1, MAX_FRAMES, xs)), (xs) => each(xs, parseCel)), (cs) => {
    const ne = nonEmpty(cs);
    return ne === undefined ? err(`expected 1 to ${MAX_FRAMES} ${what}`) : ok(ne);
  });

export const parseLayer = (raw: unknown): Result<Layer> =>
  parseWith(function* () {
    const o = yield* bind(object(raw));
    const kind = yield* field(o, "kind", oneOf(["drawing", "reference"] as const, 'expected a layer kind of "drawing" or "reference"'));
    const id = yield* field(o, "id", parseLayerId);
    const name = yield* field(o, "name", parseLabel);
    const visible = yield* field(o, "visible", parseBool);
    const locked = yield* field(o, "locked", parseBool);
    const opacity = yield* field(o, "opacity", parseOpacity);
    if (kind === "drawing") {
      const holdLast = yield* field(o, "holdLast", parseBool);
      const cels = yield* field(o, "cels", parseCels(parseCelV3));
      return makeLayer({ kind, id, name, visible, locked, opacity, holdLast, cels });
    }
    const asset = yield* field(o, "asset", parseAssetId);
    const media = yield* field(o, "media", parseMedia);
    return makeLayer({ kind, id, name, visible, locked, opacity, asset, media });
  });

const unparseLayer = (l: Layer): unknown =>
  l.kind === "drawing"
    ? {
        kind: l.kind,
        id: l.id,
        name: l.name,
        visible: l.visible,
        locked: l.locked,
        opacity: l.opacity,
        holdLast: l.holdLast,
        cels: l.cels.map((c) => ({ hold: c.hold, mark: c.mark, items: c.items.map(unparseItem) })),
      }
    : { kind: l.kind, id: l.id, name: l.name, visible: l.visible, locked: l.locked, opacity: l.opacity, asset: l.asset, media: l.media };

// Camera --------------------------------------------------------------------------------

export const MAX_CAMERA_KEYS = 1000;
export const MAX_TIMELINE_FRAME = 99999;

export const EASES = ["linear", "in", "out", "inOut"] as const;
export type Ease = (typeof EASES)[number];
export const parseEase = oneOf(EASES, 'expected an ease of "linear", "in", "out" or "inOut"');

/** A frame on the timeline, from 0. */
export type FrameIndex = Brand<number, "FrameIndex">;

export const parseFrameIndex = (raw: unknown): Result<FrameIndex> =>
  andThen(integer(raw), (n) => (n >= 0 && n <= MAX_TIMELINE_FRAME ? ok(brandChecked<number, "FrameIndex">(n)) : err(`expected a frame from 0 to ${MAX_TIMELINE_FRAME}`)));

/** How far the camera is zoomed in: 0.1 to 10. */
export type Zoom = Brand<number, "Zoom">;

export const parseZoom = (raw: unknown): Result<Zoom> =>
  andThen(number(raw), (n) => (n >= 0.1 && n <= 10 ? ok(brandChecked<number, "Zoom">(n)) : err("expected a zoom from 0.1 to 10")));

/** Where the camera looks at a frame: its centre on the canvas, zoom and turn, and how it eases into the next key. */
export type CameraKey = Brand<{ readonly frame: FrameIndex; readonly center: Vec; readonly zoom: Zoom; readonly rotation: Angle; readonly ease: Ease }, "CameraKey">;

export const parseCameraKey = (raw: unknown): Result<CameraKey> =>
  parseWith(function* () {
    const o = yield* bind(object(raw));
    const frame = yield* field(o, "frame", parseFrameIndex);
    const center = yield* field(o, "center", parseVec);
    const zoom = yield* field(o, "zoom", parseZoom);
    const rotation = yield* field(o, "rotation", parseAngle);
    const ease = yield* field(o, "ease", parseEase);
    return ok(brandChecked({ frame, center, zoom, rotation, ease }));
  });

export const makeCameraKey = (frame: FrameIndex, center: Vec, zoom: Zoom, rotation: Angle, ease: Ease): CameraKey => brandChecked({ frame, center, zoom, rotation, ease });

// Audio ---------------------------------------------------------------------------------

export const MAX_AUDIO_TRACKS = 8;

/** Frames an audio track starts after (or, negative, before) the first frame. */
export type Offset = Brand<number, "Offset">;

export const parseOffset = (raw: unknown): Result<Offset> =>
  andThen(integer(raw), (n) => (Math.abs(n) <= MAX_TIMELINE_FRAME ? ok(brandChecked<number, "Offset">(n)) : err(`expected an offset within ${MAX_TIMELINE_FRAME} frames`)));

export type Volume = Brand<number, "Volume">;

export const parseVolume = (raw: unknown): Result<Volume> =>
  andThen(number(raw), (n) => (n >= 0 && n <= 1 ? ok(brandChecked<number, "Volume">(n)) : err("expected a volume from 0 to 1")));

export type AudioTrack = Brand<
  { readonly id: LayerId; readonly asset: AssetId; readonly name: LabelName; readonly offset: Offset; readonly volume: Volume; readonly muted: boolean },
  "AudioTrack"
>;

export const parseAudioTrack = (raw: unknown): Result<AudioTrack> =>
  parseWith(function* () {
    const o = yield* bind(object(raw));
    const id = yield* field(o, "id", parseLayerId);
    const asset = yield* field(o, "asset", parseAssetId);
    const name = yield* field(o, "name", parseLabel);
    const offset = yield* field(o, "offset", parseOffset);
    const volume = yield* field(o, "volume", parseVolume);
    const muted = yield* field(o, "muted", parseBool);
    return ok(brandChecked({ id, asset, name, offset, volume, muted }));
  });

export const makeAudioTrack = (id: LayerId, asset: AssetId, name: LabelName, offset: Offset, volume: Volume, muted: boolean): AudioTrack =>
  brandChecked({ id, asset, name, offset, volume, muted });

// Project -------------------------------------------------------------------------------

export type Project = Brand<
  {
    readonly id: ProjectId;
    readonly name: ProjectName;
    readonly canvas: Canvas;
    readonly fps: Fps;
    /** Bottom to top. */
    readonly layers: NonEmpty<Layer>;
    /** In frame order, one key per frame at most. */
    readonly camera: readonly CameraKey[];
    readonly audio: readonly AudioTrack[];
  },
  "Project"
>;

/** Build a project from values already parsed; what is left are the cross-field rules. */
export const makeProject = (
  id: ProjectId,
  name: ProjectName,
  canvas: Canvas,
  fps: Fps,
  layers: readonly Layer[],
  camera: readonly CameraKey[] = [],
  audio: readonly AudioTrack[] = [],
): Result<Project> => {
  const ne = nonEmpty(layers);
  if (ne === undefined || layers.length > MAX_LAYERS) return err(`layers: expected 1 to ${MAX_LAYERS} layers`);
  if (!layers.some((l) => l.kind === "drawing")) return err("layers: expected at least one drawing layer");
  if (new Set(layers.map((l) => l.id)).size !== layers.length) return err("layers: expected every layer id to be different");
  if (camera.length > MAX_CAMERA_KEYS) return err(`camera: expected 0 to ${MAX_CAMERA_KEYS} keys`);
  if (camera.some((k, i) => i > 0 && k.frame <= (camera[i - 1]?.frame ?? -1))) return err("camera: expected keys in frame order, one per frame");
  if (audio.length > MAX_AUDIO_TRACKS) return err(`audio: expected 0 to ${MAX_AUDIO_TRACKS} tracks`);
  if (new Set(audio.map((a) => a.id)).size !== audio.length) return err("audio: expected every track id to be different");
  return ok(brandChecked({ id, name, canvas, fps, layers: ne, camera, audio }));
};

const firstLayer = (cels: NonEmpty<Cel>): Result<Layer> => {
  const id = parseLayerId("layer-1");
  const name = parseLabel("Layer 1");
  if (!id.ok) return id;
  if (!name.ok) return name;
  return makeLayer({ kind: "drawing", id: id.value, name: name.value, visible: true, locked: false, opacity: brandChecked<number, "Opacity">(1), holdLast: false, cels });
};

export const parseProject = (raw: unknown): Result<Project> =>
  parseWith(function* () {
    const o = yield* bind(object(raw));
    const format = o.format;
    if (format !== 1 && format !== 2 && format !== 3) return err("format: expected project format 1, 2 or 3");
    const id = yield* field(o, "id", parseProjectId);
    const name = yield* field(o, "name", parseProjectName);
    const canvas = yield* field(o, "canvas", parseCanvas);
    const fps = yield* field(o, "fps", parseFps);
    if (format !== 3) {
      // Formats 1 and 2: one list of drawings, which becomes the first layer.
      const cels = yield* field(o, "frames", parseCels(format === 1 ? parseCelV1 : parseCelV2, "frames"));
      const layer = yield* bind(firstLayer(cels));
      return makeProject(id, name, canvas, fps, [layer]);
    }
    const layers = yield* field(o, "layers", (v) => andThen(andThen(array(v), (xs) => bounded("layers", 1, MAX_LAYERS, xs)), (xs) => each(xs, parseLayer)));
    const camera = yield* field(o, "camera", (v) => andThen(andThen(array(v), (xs) => bounded("keys", 0, MAX_CAMERA_KEYS, xs)), (xs) => each(xs, parseCameraKey)));
    const audio = yield* field(o, "audio", (v) => andThen(andThen(array(v), (xs) => bounded("tracks", 0, MAX_AUDIO_TRACKS, xs)), (xs) => each(xs, parseAudioTrack)));
    return makeProject(id, name, canvas, fps, layers, camera, audio);
  });

/** The wire shape the server parses; always format 3. */
export const unparseProject = (p: Project): unknown => ({
  format: 3,
  id: p.id,
  name: p.name,
  canvas: { width: p.canvas.width, height: p.canvas.height },
  fps: p.fps,
  layers: p.layers.map(unparseLayer),
  camera: p.camera.map((k) => ({ frame: k.frame, center: [k.center[0], k.center[1]], zoom: k.zoom, rotation: k.rotation, ease: k.ease })),
  audio: p.audio.map((a) => ({ id: a.id, asset: a.asset, name: a.name, offset: a.offset, volume: a.volume, muted: a.muted })),
});

// Time ----------------------------------------------------------------------------------

/** A drawing layer's length in frames, counting each cel's hold. */
export const layerLength = (l: DrawingLayer): number => l.cels.reduce((n, c) => n + c.hold, 0);

/** The timeline's length: its longest drawing layer. */
export const duration = (p: Project): number => Math.max(1, ...p.layers.filter(isDrawing).map(layerLength));

/** The frame cel `i` starts on. */
export const celStart = (l: DrawingLayer, i: number): number => l.cels.slice(0, i).reduce((n, c) => n + c.hold, 0);

/** The cel showing at frame `t`, if any: past its end a layer shows nothing, unless it holds its last cel. */
export const celAt = (l: DrawingLayer, t: number): number | undefined => {
  let acc = 0;
  for (const [i, c] of l.cels.entries()) {
    acc += c.hold;
    if (t < acc) return i;
  }
  return l.holdLast ? l.cels.length - 1 : undefined;
};

/** How many times the server has saved a project; a save names the one it started from. */
export type Revision = Brand<number, "Revision">;

export const parseRevision = (raw: unknown): Result<Revision> =>
  andThen(integer(raw), (n) => (n >= 0 ? ok(brandChecked<number, "Revision">(n)) : err("expected a revision of 0 or more")));

/** What the project list shows. */
export type ProjectSummary = Brand<
  {
    readonly id: ProjectId;
    readonly name: ProjectName;
    readonly canvas: Canvas;
    readonly fps: Fps;
    readonly frameCount: number;
    readonly revision: Revision;
  },
  "ProjectSummary"
>;

export const parseProjectSummary = (raw: unknown): Result<ProjectSummary> =>
  parseWith(function* () {
    const o = yield* bind(object(raw));
    const id = yield* field(o, "id", parseProjectId);
    const name = yield* field(o, "name", parseProjectName);
    const canvas = yield* field(o, "canvas", parseCanvas);
    const fps = yield* field(o, "fps", parseFps);
    const frameCount = yield* field(o, "frameCount", (v) =>
      andThen(integer(v), (n) => (n >= 1 && n <= MAX_FRAMES ? ok(n) : err(`expected a frame count from 1 to ${MAX_FRAMES}`))),
    );
    const revision = yield* field(o, "revision", parseRevision);
    return ok(brandChecked({ id, name, canvas, fps, frameCount, revision }));
  });

/** A summary for a project the server has not seen yet (made offline). */
export const localSummary = (p: Project): ProjectSummary =>
  brandChecked({ id: p.id, name: p.name, canvas: p.canvas, fps: p.fps, frameCount: duration(p), revision: brandChecked<number, "Revision">(0) });

export const parseSummaries = (raw: unknown): Result<ProjectSummary[]> => andThen(array(raw), (xs) => each(xs, parseProjectSummary));
