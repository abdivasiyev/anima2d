/**
 * Cut-out rigging: a skeleton of bones over a layer's rest drawing (its first), and poses
 * that turn the bones. A posed drawing's items are the rest items moved by their bones,
 * baked into the cel, so rendering, onion skins, inbetweening and export need nothing new.
 *
 * Bones are listed parents first. A bone's pose is its turn about its own joint, carried
 * by its parent's; the root of the whole rig can also be moved.
 */
import { each, err, ok, type Result } from "../domain/result";
import {
  makeBinding,
  makePose,
  makeRig,
  normalizeAngle,
  parseVec,
  type Angle,
  type Bone,
  type Cel,
  type Item,
  type LayerId,
  type Pose,
  type Rig,
} from "../domain/project";
import { IDENTITY, about, apply, compose, itemBounds, rotation, transformItem, translate, type Affine, type XY } from "./geometry";

/** Each bone's transform from rest to the pose, parents applied first. */
export const boneTransforms = (rig: Rig, pose: Pose | null): Map<string, Affine> => {
  const turns = new Map<string, number>(pose?.angles.map(([b, a]) => [b, a]) ?? []);
  const root: Affine = pose === null ? IDENTITY : translate(pose.root[0], pose.root[1]);
  const out = new Map<string, Affine>();
  for (const b of rig.bones) {
    const parent = b.parent === null ? root : (out.get(b.parent) ?? root);
    out.set(b.id, compose(parent, about(b.origin[0], b.origin[1], rotation(turns.get(b.id) ?? 0))));
  }
  return out;
};

/** How far a bone points, in the pose: its rest angle plus every turn above it. */
const posedAngle = (rig: Rig, pose: Pose | null, id: LayerId): number => {
  const turns = new Map<string, number>(pose?.angles.map(([b, a]) => [b, a]) ?? []);
  const byId = new Map(rig.bones.map((b) => [b.id, b]));
  let bone = byId.get(id);
  if (bone === undefined) return 0;
  let a: number = bone.angle;
  while (bone !== undefined) {
    a += turns.get(bone.id) ?? 0;
    bone = bone.parent === null ? undefined : byId.get(bone.parent);
  }
  return a;
};

export const restEnd = (b: Bone): XY => [b.origin[0] + Math.cos(b.angle) * b.length, b.origin[1] + Math.sin(b.angle) * b.length];

/** Where each bone's joint and tip are in the pose. */
export const posedBones = (rig: Rig, pose: Pose | null): { bone: Bone; from: XY; to: XY }[] => {
  const t = boneTransforms(rig, pose);
  return rig.bones.map((bone) => {
    const m = t.get(bone.id) ?? IDENTITY;
    const end = restEnd(bone);
    return { bone, from: apply(m, bone.origin[0], bone.origin[1]), to: apply(m, end[0], end[1]) };
  });
};

/** The rest drawing's items moved by their bones; unbound items stay where they are. */
export const bake = (rig: Rig, rest: Cel, pose: Pose): Result<Item[]> => {
  const t = boneTransforms(rig, pose);
  const boneOf = new Map(rig.bindings.map((k) => [k.item, k.bone]));
  return each(rest.items, (item, i) => {
    const bone = boneOf.get(i);
    const m = bone === undefined ? undefined : t.get(bone);
    return m === undefined ? ok(item) : transformItem(item, m);
  });
};

const withTurn = (pose: Pose, id: LayerId, turn: number): Pose =>
  makePose(pose.root, [...pose.angles.filter(([b]) => b !== id), [id, normalizeAngle(turn)] as const]);

export const restPose = (): Pose => {
  const zero = parseVec([0, 0]);
  if (!zero.ok) throw new Error("origin");
  return makePose(zero.value, []);
};

/** Forward kinematics: turn one bone so its tip points at `target`. */
export const pointBone = (rig: Rig, pose: Pose, id: LayerId, target: XY): Pose => {
  const posed = posedBones(rig, pose).find((p) => p.bone.id === id);
  if (posed === undefined) return pose;
  const want = Math.atan2(target[1] - posed.from[1], target[0] - posed.from[0]);
  const now = posedAngle(rig, pose, id);
  const current = pose.angles.find(([b]) => b === id)?.[1] ?? 0;
  return withTurn(pose, posed.bone.id, current + (want - now));
};

/** Inverse kinematics (cyclic coordinate descent): turn the chain up to the root so the tip reaches `target`. */
export const reachWith = (rig: Rig, pose: Pose, id: LayerId, target: XY): Pose => {
  const byId = new Map(rig.bones.map((b) => [b.id, b]));
  const chain: Bone[] = [];
  for (let b = byId.get(id); b !== undefined; b = b.parent === null ? undefined : byId.get(b.parent)) chain.push(b);
  let p = pose;
  for (let iter = 0; iter < 12; iter++) {
    for (const joint of chain) {
      const tip = posedBones(rig, p).find((x) => x.bone.id === id)?.to;
      const at = posedBones(rig, p).find((x) => x.bone.id === joint.id)?.from;
      if (tip === undefined || at === undefined) return p;
      const a = Math.atan2(tip[1] - at[1], tip[0] - at[0]);
      const b = Math.atan2(target[1] - at[1], target[0] - at[0]);
      const current = p.angles.find(([x]) => x === joint.id)?.[1] ?? 0;
      p = withTurn(p, joint.id, current + Math.atan2(Math.sin(b - a), Math.cos(b - a)));
    }
  }
  return p;
};

/** Move the whole rig. */
export const moveRoot = (pose: Pose, dx: number, dy: number): Pose => {
  const v = parseVec([pose.root[0] + dx, pose.root[1] + dy]);
  return v.ok ? makePose(v.value, pose.angles) : pose;
};

const distanceToSegment = (p: XY, a: XY, b: XY): number => {
  const dx = b[0] - a[0];
  const dy = b[1] - a[1];
  const len = dx * dx + dy * dy;
  const t = len === 0 ? 0 : Math.max(0, Math.min(1, ((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / len));
  return Math.hypot(p[0] - (a[0] + dx * t), p[1] - (a[1] + dy * t));
};

/** Bind every item of the rest drawing to the bone nearest its middle. */
export const bindNearest = (rig: Rig, rest: Cel): Result<Rig> => {
  if (rig.bones.length === 0) return err("Add a bone first");
  const bindings = each(rest.items, (item, i) => {
    const b = itemBounds(item);
    const mid: XY = [(b.x0 + b.x1) / 2, (b.y0 + b.y1) / 2];
    let best = rig.bones[0];
    let bestD = Infinity;
    for (const bone of rig.bones) {
      const d = distanceToSegment(mid, [bone.origin[0], bone.origin[1]], restEnd(bone));
      if (d < bestD) {
        bestD = d;
        best = bone;
      }
    }
    return best === undefined ? err("Add a bone first") : makeBinding(i, best.id);
  });
  return bindings.ok ? makeRig(rig.bones, bindings.value.filter((k) => rest.items[k.item]?.kind !== "erase")) : bindings;
};

/** Bind the given items to one bone, keeping every other binding. */
export const bindTo = (rig: Rig, items: readonly number[], bone: LayerId): Result<Rig> => {
  const chosen = new Set(items);
  const kept = rig.bindings.filter((k) => !chosen.has(k.item));
  const added = each([...chosen], (i) => makeBinding(i, bone));
  return added.ok ? makeRig(rig.bones, [...kept, ...added.value]) : added;
};

/** A pose `u` of the way from `a` to `b`: bone turns and the root move evenly. */
export const lerpPose = (a: Pose, b: Pose, u: number): Pose => {
  const ids = new Set([...a.angles.map(([x]) => x), ...b.angles.map(([x]) => x)]);
  const turn = (p: Pose, id: string): number => p.angles.find(([x]) => x === id)?.[1] ?? 0;
  const angles: (readonly [LayerId, Angle])[] = [];
  for (const id of ids) {
    const ta = turn(a, id);
    const tb = turn(b, id);
    const bone = a.angles.find(([x]) => x === id)?.[0] ?? b.angles.find(([x]) => x === id)?.[0];
    if (bone !== undefined) angles.push([bone, normalizeAngle(ta + Math.atan2(Math.sin(tb - ta), Math.cos(tb - ta)) * u)]);
  }
  const root = parseVec([a.root[0] + (b.root[0] - a.root[0]) * u, a.root[1] + (b.root[1] - a.root[1]) * u]);
  return makePose(root.ok ? root.value : a.root, angles);
};


