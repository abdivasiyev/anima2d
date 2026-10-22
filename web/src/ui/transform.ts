/**
 * The transform tool: the whole layer moved, scaled and turned at the playhead, keyed as it
 * goes (auto-key). Drag inside the box to move, a corner to scale, the round handle to turn
 * (Shift snaps to 15°), the cross to move the pivot. The first change on a layer that does
 * not move yet also keys it where it is on the first frame, so it travels from there.
 */
import { removeMotionKey, setMotionKey, updateLayer } from "../domain/edit";
import {
  makeMotion,
  makeMotionKey,
  normalizeAngle,
  parseFade,
  parseFrameIndex,
  parseScale,
  parseVec,
  type Cel,
  type Ease,
  type Layer,
  type MotionKey,
  type Project,
} from "../domain/project";
import { andThen, err, ok, type Result } from "../domain/result";
import { apply, invert, itemBounds, unionBounds, type Box, type XY } from "../editor/geometry";
import { motionAt, motionMatrix, placementAt, REST, type MotionValues } from "../editor/output";
import type { Ctx } from "../editor/render";
import type { Sample } from "../editor/stabilizer";
import type { ToolController } from "../editor/tools";

export interface TransformHost {
  readonly width: number;
  readonly height: number;
  /** The committed project (not a preview). */
  base(): Project;
  layer(): number;
  frame(): number;
  cel(): Cel | undefined;
  /** Screen pixels per canvas pixel. */
  scale(): number;
  shift(): boolean;
  /** Show a project that is not committed yet; undefined shows the committed one again. */
  preview(p: Project | undefined): void;
  edit(r: Result<Project>): boolean;
  /** Guides in canvas space; null clears them. */
  guides(draw: ((ctx: Ctx, pixel: number) => void) | null): void;
  say(text: string): void;
}

type Handle = "move" | "scale" | "turn" | "pivot";

interface Grab {
  readonly handle: Handle;
  readonly from: XY;
  readonly start: MotionValues;
  /** The pivot on the canvas when the drag began. */
  readonly centre: XY;
}

/** The tool, and `refresh` to draw its handles again after the playhead or layer changes. */
export const transformTool = (host: TransformHost): ToolController & { refresh(): void } => {
  let grab: Grab | undefined;
  let shown: Project | undefined;

  const layerOf = (p: Project): Layer | undefined => p.layers[host.layer()];

  /** The drawing's box in the layer's own space; the whole canvas when it is empty. */
  const box = (): Box =>
    unionBounds((host.cel()?.items ?? []).filter((i) => i.kind !== "erase").map(itemBounds)) ?? { x0: 0, y0: 0, x1: host.width, y1: host.height };

  const pivotOf = (l: Layer): XY => {
    if (l.motion !== null) return [l.motion.pivot[0], l.motion.pivot[1]];
    const b = box();
    return [(b.x0 + b.x1) / 2, (b.y0 + b.y1) / 2];
  };

  /** Where the handles are on the canvas, for the project being shown. */
  const handles = (p: Project): { corners: XY[]; top: XY; turn: XY; pivot: XY } | undefined => {
    const l = layerOf(p);
    if (l === undefined) return undefined;
    const m = placementAt(l, host.frame()).m;
    const b = box();
    const corners = (
      [
        [b.x0, b.y0],
        [b.x1, b.y0],
        [b.x1, b.y1],
        [b.x0, b.y1],
      ] as const
    ).map(([x, y]) => apply(m, x, y));
    const top = apply(m, (b.x0 + b.x1) / 2, b.y0);
    const mid = apply(m, (b.x0 + b.x1) / 2, (b.y0 + b.y1) / 2);
    const len = Math.hypot(top[0] - mid[0], top[1] - mid[1]) || 1;
    const out = 30 / host.scale();
    const turn: XY = [top[0] + ((top[0] - mid[0]) / len) * out, top[1] + ((top[1] - mid[1]) / len) * out];
    const pv = pivotOf(l);
    return { corners, top, turn, pivot: l.motion === null ? pv : apply(m, pv[0], pv[1]) };
  };

  const show = (): void => {
    const p = shown ?? host.base();
    const hs = handles(p);
    const l = layerOf(p);
    if (hs === undefined || l === undefined) {
      host.guides(null);
      return;
    }
    host.guides((ctx, px) => {
      ctx.save();
      // The path the pivot travels, with a dot on each key.
      const keys = l.motion?.keys;
      const first = keys?.[0];
      const last = keys?.[keys.length - 1];
      if (keys !== undefined && first !== undefined && last !== undefined && last.frame > first.frame && l.motion !== null) {
        const pv = l.motion.pivot;
        ctx.strokeStyle = "rgba(47, 107, 255, 0.55)";
        ctx.lineWidth = 1.5 * px;
        ctx.setLineDash([4 * px, 4 * px]);
        ctx.beginPath();
        for (let f = first.frame; f <= last.frame; f++) {
          const q = apply(motionMatrix(motionAt(l, f), pv), pv[0], pv[1]);
          if (f === first.frame) ctx.moveTo(q[0], q[1]);
          else ctx.lineTo(q[0], q[1]);
        }
        ctx.stroke();
        ctx.setLineDash([]);
        ctx.fillStyle = "#2f6bff";
        for (const k of keys) {
          const q = apply(motionMatrix(motionAt(l, k.frame), pv), pv[0], pv[1]);
          ctx.beginPath();
          ctx.arc(q[0], q[1], (k.frame === host.frame() ? 5 : 3.5) * px, 0, Math.PI * 2);
          ctx.fill();
        }
      }
      ctx.strokeStyle = "#2f6bff";
      ctx.lineWidth = 1.5 * px;
      ctx.beginPath();
      for (const [i, c] of hs.corners.entries()) {
        if (i === 0) ctx.moveTo(c[0], c[1]);
        else ctx.lineTo(c[0], c[1]);
      }
      ctx.closePath();
      ctx.moveTo(hs.top[0], hs.top[1]);
      ctx.lineTo(hs.turn[0], hs.turn[1]);
      ctx.stroke();
      ctx.fillStyle = "#ffffff";
      for (const c of hs.corners) {
        ctx.fillRect(c[0] - 5 * px, c[1] - 5 * px, 10 * px, 10 * px);
        ctx.strokeRect(c[0] - 5 * px, c[1] - 5 * px, 10 * px, 10 * px);
      }
      ctx.beginPath();
      ctx.arc(hs.turn[0], hs.turn[1], 6 * px, 0, Math.PI * 2);
      ctx.fill();
      ctx.stroke();
      // The pivot: a cross in a ring.
      const [x, y] = hs.pivot;
      ctx.beginPath();
      ctx.arc(x, y, 7 * px, 0, Math.PI * 2);
      ctx.moveTo(x - 11 * px, y);
      ctx.lineTo(x + 11 * px, y);
      ctx.moveTo(x, y - 11 * px);
      ctx.lineTo(x, y + 11 * px);
      ctx.stroke();
      ctx.restore();
    });
  };

  /** The ease a new key takes: the one already on this frame, else the key before's. */
  const easeAt = (l: Layer, frame: number): Ease => {
    const keys = l.motion?.keys ?? [];
    return keys.find((k) => k.frame === frame)?.ease ?? [...keys].reverse().find((k) => k.frame < frame)?.ease ?? "inOut";
  };

  const keyOf = (frame: number, v: MotionValues, ease: Ease): Result<MotionKey> => {
    const f = parseFrameIndex(frame);
    const pos = parseVec([v.x, v.y]);
    const k = parseScale(Math.max(0.01, Math.min(100, v.scale)));
    const fade = parseFade(Math.max(0, Math.min(1, v.opacity)));
    if (!f.ok) return f;
    if (!pos.ok) return pos;
    if (!k.ok) return k;
    if (!fade.ok) return fade;
    return ok(makeMotionKey(f.value, pos.value, k.value, normalizeAngle(v.rotation), fade.value, ease));
  };

  /** The base project with the layer keyed at the playhead with `v`. */
  const keyed = (v: MotionValues): Result<Project> => {
    const p = host.base();
    const i = host.layer();
    const l = layerOf(p);
    if (l === undefined) return err("expected a layer");
    const pv = parseVec(pivotOf(l));
    if (!pv.ok) return pv;
    const t = host.frame();
    const ease = easeAt(l, t);
    let start: Result<Project> = ok(p);
    if (l.motion === null && t > 0) start = andThen(keyOf(0, REST, ease), (k0) => setMotionKey(p, i, k0, pv.value));
    return andThen(start, (q) => andThen(keyOf(t, v, ease), (k) => setMotionKey(q, i, k, pv.value)));
  };

  /** The base project with the pivot moved to `to` (layer space), every key compensated so nothing jumps. */
  const repivoted = (to: XY): Result<Project> => {
    const p = host.base();
    const i = host.layer();
    const l = layerOf(p);
    if (l === undefined) return err("expected a layer");
    const pv = parseVec(to);
    if (!pv.ok) return pv;
    if (l.motion === null) {
      // Not moving yet: a still key here, about the new pivot.
      return andThen(keyOf(host.frame(), REST, "inOut"), (k) => setMotionKey(p, i, k, pv.value));
    }
    const [ox, oy] = l.motion.pivot;
    const dx = ox - to[0];
    const dy = oy - to[1];
    const keys: MotionKey[] = [];
    for (const k of l.motion.keys) {
      // pos' = pos + (I − R·S)(old − new): the layer stays where it was on every key.
      const c = Math.cos(k.rotation) * k.scale;
      const s = Math.sin(k.rotation) * k.scale;
      const next = keyOf(k.frame, { x: k.position[0] + dx - (c * dx - s * dy), y: k.position[1] + dy - (s * dx + c * dy), scale: k.scale, rotation: k.rotation, opacity: k.opacity }, k.ease);
      if (!next.ok) return next;
      keys.push(next.value);
    }
    return andThen(makeMotion(pv.value, keys), (motion) => updateLayer(p, i, { motion }));
  };

  const near = (a: XY, b: XY, r = 14): boolean => Math.hypot(a[0] - b[0], a[1] - b[1]) < r / host.scale();

  /** A layer sample (in the layer's own space) back on the canvas. */
  const onCanvas = (s: Sample): XY => {
    const l = layerOf(host.base());
    return l === undefined ? [s.x, s.y] : apply(placementAt(l, host.frame()).m, s.x, s.y);
  };

  return {
    refresh: () => {
      if (grab === undefined) show();
    },
    down: (s) => {
      const p = host.base();
      const l = layerOf(p);
      if (l === undefined) return;
      if (l.locked) {
        host.say("This layer is locked");
        return;
      }
      const c = onCanvas(s);
      const hs = handles(p);
      if (hs === undefined) return;
      const handle: Handle = hs.corners.some((k) => near(k, c)) ? "scale" : near(hs.turn, c) ? "turn" : near(hs.pivot, c) ? "pivot" : "move";
      grab = { handle, from: c, start: motionAt(l, host.frame()), centre: hs.pivot };
    },
    move: (s) => {
      if (grab === undefined) return;
      const c = onCanvas(s);
      const g = grab;
      const v = g.start;
      let r: Result<Project>;
      switch (g.handle) {
        case "move":
          r = keyed({ ...v, x: v.x + c[0] - g.from[0], y: v.y + c[1] - g.from[1] });
          break;
        case "scale": {
          const k = Math.hypot(c[0] - g.centre[0], c[1] - g.centre[1]) / Math.max(1e-6, Math.hypot(g.from[0] - g.centre[0], g.from[1] - g.centre[1]));
          r = keyed({ ...v, scale: v.scale * k });
          break;
        }
        case "turn": {
          let turn = v.rotation + Math.atan2(c[1] - g.centre[1], c[0] - g.centre[0]) - Math.atan2(g.from[1] - g.centre[1], g.from[0] - g.centre[0]);
          if (host.shift()) turn = Math.round(turn / (Math.PI / 12)) * (Math.PI / 12);
          r = keyed({ ...v, rotation: Math.atan2(Math.sin(turn), Math.cos(turn)) });
          break;
        }
        case "pivot": {
          const l = layerOf(host.base());
          if (l === undefined) return;
          r = repivoted(apply(invert(placementAt(l, host.frame()).m), c[0], c[1]));
          break;
        }
      }
      if (!r.ok) return;
      shown = r.value;
      host.preview(shown);
      show();
    },
    up: () => {
      const next = shown;
      grab = undefined;
      shown = undefined;
      host.preview(undefined);
      if (next !== undefined) host.edit(ok(next));
      show();
    },
    cancel: () => {
      grab = undefined;
      shown = undefined;
      host.preview(undefined);
      show();
    },
    deactivate: () => {
      grab = undefined;
      shown = undefined;
      host.preview(undefined);
      host.guides(null);
    },
    key: (key) => {
      if (key !== "Delete" && key !== "Backspace") return false;
      const r = removeMotionKey(host.base(), host.layer(), host.frame());
      if (r.ok) host.edit(r);
      else host.say(r.error);
      show();
      return true;
    },
  };
};
