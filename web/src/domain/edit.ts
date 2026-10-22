/**
 * Edits to a project. Each returns a new project (the old one is kept for undo) and goes
 * through the smart constructors, so an edit can no more break a limit than a file from
 * disk can.
 *
 * A cel is addressed by `Where`: its layer (bottom to top) and its place on that layer.
 */
import { brandChecked } from "./brand";
import { andThen, each, err, ok, unwrap, type Result } from "./result";
import {
  isDrawing,
  makeCel,
  makeItem,
  makeMotion,
  makeLayer,
  makeProject,
  parseCanvas,
  parseHold,
  parseLabel,
  parseLayerId,
  type AssetId,
  type AudioTrack,
  type CameraKey,
  type Color,
  type MotionKey,
  type Vec,
  type Canvas,
  type Cel,
  type DrawingLayer,
  type FrameIndex,
  type Fps,
  type Hold,
  type Item,
  type LabelName,
  type Layer,
  type LayerId,
  type LayerFields,
  type Mark,
  type Media,
  type Pose,
  type Project,
  type ProjectId,
  type ProjectName,
  type Rig,
  PLAIN_LOOK,
} from "./project";

export interface Where {
  readonly layer: number;
  readonly cel: number;
}

const ONE: Hold = unwrap(parseHold(1));

const rebuild = (p: Project, layers: readonly Layer[], camera = p.camera, audio = p.audio, palette = p.palette): Result<Project> =>
  makeProject(p.id, p.name, p.canvas, p.fps, layers, camera, audio, palette, p.fonts);

const blankCel = (): Cel => unwrap(makeCel(ONE, "none", []));

const drawingAt = (p: Project, i: number): Result<DrawingLayer> => {
  const l = p.layers[i];
  if (l === undefined) return err("expected a layer that exists");
  return isDrawing(l) ? ok(l) : err("expected a drawing layer");
};

/** Replace layer `i`'s cels (at least one stays). */
const withCels = (p: Project, i: number, f: (cels: readonly Cel[]) => Result<readonly Cel[]>): Result<Project> =>
  andThen(drawingAt(p, i), (l) =>
    andThen(f(l.cels), (cels) => {
      const [first, ...rest] = cels;
      if (first === undefined) return err("expected to keep at least one drawing on a layer");
      return andThen(makeLayer({ ...l, cels: [first, ...rest] }), (layer) => rebuild(p, p.layers.map((x, j) => (j === i ? layer : x))));
    }),
  );

const celAt = (p: Project, at: Where): Result<Cel> =>
  andThen(drawingAt(p, at.layer), (l) => {
    const c = l.cels[at.cel];
    return c === undefined ? err("expected a drawing that exists") : ok(c);
  });

const withCel = (p: Project, at: Where, f: (c: Cel) => Result<Cel>): Result<Project> =>
  andThen(celAt(p, at), (c) => andThen(f(c), (next) => withCels(p, at.layer, (cels) => ok(cels.map((x, j) => (j === at.cel ? next : x))))));

const newId = (prefix: string): Result<LayerId> =>
  parseLayerId(`${prefix}-${crypto.randomUUID().slice(0, 8)}`);

// Projects ------------------------------------------------------------------------------

export const newProject = (id: ProjectId, name: ProjectName, canvas: Canvas, fps: Fps): Project =>
  unwrap(
    andThen(parseLayerId("layer-1"), (lid) =>
      andThen(parseLabel("Layer 1"), (lname) =>
        andThen(makeLayer({ kind: "drawing", id: lid, name: lname, ...PLAIN_LOOK, holdLast: false, cels: [blankCel()], rig: null }), (layer) =>
          makeProject(id, name, canvas, fps, [layer]),
        ),
      ),
    ),
  );

export const rename = (p: Project, name: ProjectName): Result<Project> => makeProject(p.id, name, p.canvas, p.fps, p.layers, p.camera, p.audio, p.palette, p.fonts);

export const setFps = (p: Project, fps: Fps): Result<Project> => makeProject(p.id, p.name, p.canvas, fps, p.layers, p.camera, p.audio, p.palette, p.fonts);

// Items ---------------------------------------------------------------------------------

/** Put `items` on top of a cel. */
export const addItems = (p: Project, at: Where, items: readonly Item[]): Result<Project> =>
  withCel(p, at, (c) => makeCel(c.hold, c.mark, [...c.items, ...items]));

/** Replace a cel's items wholesale: object erasing, moving, recolouring, reordering. */
export const setItems = (p: Project, at: Where, items: readonly Item[]): Result<Project> => withCel(p, at, (c) => makeCel(c.hold, c.mark, items));

export const clearCel = (p: Project, at: Where): Result<Project> => setItems(p, at, []);

// Cels ----------------------------------------------------------------------------------

export const setHold = (p: Project, at: Where, hold: Hold): Result<Project> => withCel(p, at, (c) => makeCel(hold, c.mark, c.items, c.pose));

export const setMark = (p: Project, at: Where, mark: Mark): Result<Project> => withCel(p, at, (c) => makeCel(c.hold, mark, c.items, c.pose));

/** Insert a blank drawing after `at`. */
export const insertBlank = (p: Project, at: Where): Result<Project> =>
  withCels(p, at.layer, (cels) => ok([...cels.slice(0, at.cel + 1), blankCel(), ...cels.slice(at.cel + 1)]));

/** Insert drawings after `at` (generated inbetweens, a motion along a path). */
export const insertCels = (p: Project, at: Where, added: readonly Cel[]): Result<Project> =>
  withCels(p, at.layer, (cels) => ok([...cels.slice(0, at.cel + 1), ...added, ...cels.slice(at.cel + 1)]));

/** Insert a copy of the drawing at `at` right after it. */
export const duplicate = (p: Project, at: Where): Result<Project> =>
  andThen(celAt(p, at), (c) => withCels(p, at.layer, (cels) => ok([...cels.slice(0, at.cel + 1), c, ...cels.slice(at.cel + 1)])));

/** Remove a drawing; a layer always keeps one. */
export const remove = (p: Project, at: Where): Result<Project> => withCels(p, at.layer, (cels) => ok(cels.filter((_, i) => i !== at.cel)));

/** Move the drawing at `at` to place `to` on its layer. */
export const move = (p: Project, at: Where, to: number): Result<Project> =>
  andThen(celAt(p, at), (c) =>
    withCels(p, at.layer, (cels) => {
      const rest = cels.filter((_, i) => i !== at.cel);
      const k = Math.max(0, Math.min(rest.length, to));
      return ok([...rest.slice(0, k), c, ...rest.slice(k)]);
    }),
  );

// Rigs ----------------------------------------------------------------------------------

/** Give a drawing layer a skeleton, or take it away (null). */
export const setRig = (p: Project, layer: number, rig: Rig | null): Result<Project> =>
  andThen(drawingAt(p, layer), (l) => andThen(makeLayer({ ...l, rig }), (next) => rebuild(p, p.layers.map((x, j) => (j === layer ? next : x)))));

/** A drawing posed from the rig: its baked items and the pose they came from. */
export const setPosed = (p: Project, at: Where, items: readonly Item[], pose: Pose): Result<Project> => withCel(p, at, (c) => makeCel(c.hold, c.mark, items, pose));

// Ranges (sequence tools) ---------------------------------------------------------------

export interface Range {
  readonly layer: number;
  readonly from: number;
  /** Inclusive. */
  readonly to: number;
}

const ordered = (r: Range): readonly [number, number] => [Math.min(r.from, r.to), Math.max(r.from, r.to)];

/** Play a run of drawings backwards. */
export const reverseRange = (p: Project, r: Range): Result<Project> =>
  withCels(p, r.layer, (cels) => {
    const [a, b] = ordered(r);
    return ok([...cels.slice(0, a), ...cels.slice(a, b + 1).reverse(), ...cels.slice(b + 1)]);
  });

/** The run again, right after itself. */
export const duplicateRange = (p: Project, r: Range): Result<Project> => repeatRange(p, r, 2);

/** The run `times` times in a row: a walk or run cycle. */
export const repeatRange = (p: Project, r: Range, times: number): Result<Project> =>
  withCels(p, r.layer, (cels) => {
    const [a, b] = ordered(r);
    const run = cels.slice(a, b + 1);
    const n = Math.max(1, Math.min(100, Math.floor(times)));
    return ok([...cels.slice(0, a), ...Array.from({ length: n }, () => run).flat(), ...cels.slice(b + 1)]);
  });

/** Play the run forward then back, without doubling its ends: a ping-pong cycle. */
export const pingPongRange = (p: Project, r: Range): Result<Project> =>
  withCels(p, r.layer, (cels) => {
    const [a, b] = ordered(r);
    const run = cels.slice(a, b + 1);
    return ok([...cels.slice(0, a), ...run, ...run.slice(1, -1).reverse(), ...cels.slice(b + 1)]);
  });

export const removeRange = (p: Project, r: Range): Result<Project> =>
  withCels(p, r.layer, (cels) => {
    const [a, b] = ordered(r);
    return ok(cels.filter((_, i) => i < a || i > b));
  });

// Layers --------------------------------------------------------------------------------

/** A new drawing layer above layer `below`, one blank drawing on it. */
export const addDrawingLayer = (p: Project, below: number, name: LabelName, holdLast = false): Result<Project> =>
  andThen(newId("layer"), (id) =>
    andThen(makeLayer({ kind: "drawing", id, name, ...PLAIN_LOOK, holdLast, cels: [blankCel()], rig: null }), (layer) =>
      rebuild(p, [...p.layers.slice(0, below + 1), layer, ...p.layers.slice(below + 1)]),
    ),
  );

/** A reference image or video at the bottom, half transparent. */
export const addReferenceLayer = (p: Project, asset: AssetId, media: Media, name: LabelName): Result<Project> =>
  andThen(newId("ref"), (id) =>
    andThen(makeLayer({ kind: "reference", id, name, ...PLAIN_LOOK, locked: true, opacity: brandChecked<number, "Opacity">(0.5), asset, media }), (layer) =>
      rebuild(p, [layer, ...p.layers]),
    ),
  );

export const removeLayer = (p: Project, i: number): Result<Project> => rebuild(p, p.layers.filter((_, j) => j !== i));

export const moveLayer = (p: Project, from: number, to: number): Result<Project> => {
  const l = p.layers[from];
  if (l === undefined) return err("expected a layer that exists");
  const rest = p.layers.filter((_, j) => j !== from);
  const k = Math.max(0, Math.min(rest.length, to));
  return rebuild(p, [...rest.slice(0, k), l, ...rest.slice(k)]);
};

type LookPatch = Partial<Pick<LayerFields, "name" | "visible" | "locked" | "opacity" | "blend" | "clip" | "motion">>;
type LayerPatch = LookPatch & { readonly holdLast?: boolean };

/** Change a layer's name, visibility, lock, opacity, blend, clipping, motion or (drawing layers) hold-last. */
export const updateLayer = (p: Project, i: number, patch: LayerPatch): Result<Project> => {
  const l = p.layers[i];
  if (l === undefined) return err("expected a layer that exists");
  const next: LayerFields =
    l.kind === "drawing"
      ? { ...l, ...pick(patch), holdLast: patch.holdLast ?? l.holdLast }
      : { ...l, ...pick(patch) };
  return andThen(makeLayer(next), (layer) => rebuild(p, p.layers.map((x, j) => (j === i ? layer : x))));
};

const pick = (patch: LayerPatch): LookPatch => {
  const out: { -readonly [K in keyof LookPatch]: LookPatch[K] } = {};
  if (patch.blend !== undefined) out.blend = patch.blend;
  if (patch.clip !== undefined) out.clip = patch.clip;
  if (patch.motion !== undefined) out.motion = patch.motion;
  if (patch.name !== undefined) out.name = patch.name;
  if (patch.visible !== undefined) out.visible = patch.visible;
  if (patch.locked !== undefined) out.locked = patch.locked;
  if (patch.opacity !== undefined) out.opacity = patch.opacity;
  return out;
};

// Camera and audio ----------------------------------------------------------------------

/** Set the camera key on its frame, replacing any key already there. */
export const setCameraKey = (p: Project, key: CameraKey): Result<Project> =>
  rebuild(p, p.layers, [...p.camera.filter((k) => k.frame !== key.frame), key].sort((a, b) => a.frame - b.frame));

export const removeCameraKey = (p: Project, frame: FrameIndex): Result<Project> => rebuild(p, p.layers, p.camera.filter((k) => k.frame !== frame));

export const addAudio = (p: Project, track: AudioTrack): Result<Project> => rebuild(p, p.layers, p.camera, [...p.audio, track]);

export const replaceAudio = (p: Project, track: AudioTrack): Result<Project> => rebuild(p, p.layers, p.camera, p.audio.map((a) => (a.id === track.id ? track : a)));

export const removeAudio = (p: Project, id: string): Result<Project> => rebuild(p, p.layers, p.camera, p.audio.filter((a) => a.id !== id));

// New projects --------------------------------------------------------------------------

/** The canvas presets the new-project dialog offers. */
export const CANVAS_PRESETS: readonly { readonly label: string; readonly detail: string; readonly canvas: Canvas }[] = [
  { label: "Full HD", detail: "1920 × 1080 · 16:9", canvas: unwrap(parseCanvas({ width: 1920, height: 1080 })) },
  { label: "4K", detail: "3840 × 2160 · 16:9", canvas: unwrap(parseCanvas({ width: 3840, height: 2160 })) },
  { label: "Shorts", detail: "1080 × 1920 · 9:16", canvas: unwrap(parseCanvas({ width: 1080, height: 1920 })) },
  { label: "Square", detail: "1080 × 1080 · 1:1", canvas: unwrap(parseCanvas({ width: 1080, height: 1080 })) },
];

// Layer motion ---------------------------------------------------------------------------

/** Set the key at `key.frame` (adding it, or replacing the one there); a first key also sets the pivot. */
export const setMotionKey = (p: Project, i: number, key: MotionKey, pivot: Vec): Result<Project> => {
  const l = p.layers[i];
  if (l === undefined) return err("expected a layer that exists");
  const keys = [...(l.motion?.keys ?? []).filter((k) => k.frame !== key.frame), key].sort((a, b) => a.frame - b.frame);
  return andThen(makeMotion(l.motion?.pivot ?? pivot, keys), (motion) => updateLayer(p, i, { motion }));
};

/** Remove the key at `frame`; removing the last one leaves the layer still. */
export const removeMotionKey = (p: Project, i: number, frame: number): Result<Project> => {
  const l = p.layers[i];
  if (l?.motion == null) return err("expected a layer with motion");
  const keys = l.motion.keys.filter((k) => k.frame !== frame);
  if (keys.length === l.motion.keys.length) return err("No key on this frame");
  if (keys.length === 0) return clearMotion(p, i);
  return andThen(makeMotion(l.motion.pivot, keys), (motion) => updateLayer(p, i, { motion }));
};

/** Move the point a layer turns and scales about. */
export const setPivot = (p: Project, i: number, pivot: Vec): Result<Project> => {
  const l = p.layers[i];
  if (l?.motion == null) return err("expected a layer with motion");
  return andThen(makeMotion(pivot, l.motion.keys), (motion) => updateLayer(p, i, { motion }));
};

/** Drop every key: the layer stays where its drawings are. */
export const clearMotion = (p: Project, i: number): Result<Project> => {
  const l = p.layers[i];
  if (l === undefined) return err("expected a layer that exists");
  return andThen(makeLayer({ ...l, motion: null }), (layer) => rebuild(p, p.layers.map((x, j) => (j === i ? layer : x))));
};

// Palette --------------------------------------------------------------------------------

const recolorItem = (item: Item, swap: (c: Color) => Color): Result<Item> => {
  switch (item.kind) {
    case "erase":
    case "image":
      return ok(item);
    case "shape":
      return makeItem({ ...item, color: swap(item.color), fill: item.fill === null ? null : swap(item.fill) });
    case "text":
    case "stroke":
    case "scatter":
    case "curve":
    case "fill":
      return makeItem({ ...item, color: swap(item.color) });
  }
};

export const setPalette = (p: Project, palette: readonly Color[]): Result<Project> => rebuild(p, p.layers, p.camera, p.audio, palette);

/** Every item, in every drawing of every layer, drawn in `from` is drawn in `to`; the palette too. */
export const recolorAll = (p: Project, from: Color, to: Color): Result<Project> => {
  const swap = (c: Color): Color => (c === from ? to : c);
  const layers = each([...p.layers], (l) => {
    if (l.kind !== "drawing") return ok(l);
    const cels = each([...l.cels], (c) => andThen(each([...c.items], (item) => recolorItem(item, swap)), (items) => makeCel(c.hold, c.mark, items, c.pose)));
    return andThen(cels, (cs) => {
      const [first, ...rest] = cs;
      return first === undefined ? err("expected a drawing") : makeLayer({ ...l, cels: [first, ...rest] });
    });
  });
  return andThen(layers, (ls) => rebuild(p, ls, p.camera, p.audio, [...new Set(p.palette.map(swap))]));
};
