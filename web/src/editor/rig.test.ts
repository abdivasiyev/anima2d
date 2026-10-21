import { describe, expect, it } from "vitest";

import { makeBinding, makeBone, makeCel, makeRig, normalizeAngle, parseExtent, parseHold, parseItem, parseLayerId, parseVec, type Item } from "../domain/project";
import { unwrap } from "../domain/result";
import { bake, bindNearest, pointBone, posedBones, reachWith, restPose } from "./rig";

const id = (s: string) => unwrap(parseLayerId(s));
const v = (x: number, y: number) => unwrap(parseVec([x, y]));
// An arm along the x axis: shoulder at 0, elbow at 100, wrist at 200.
const upper = makeBone(id("upper"), null, v(0, 0), unwrap(parseExtent(100)), normalizeAngle(0));
const lower = makeBone(id("lower"), id("upper"), v(100, 0), unwrap(parseExtent(100)), normalizeAngle(0));
const hand: Item = unwrap(parseItem({ kind: "shape", layer: "line", shape: "ellipse", color: "#000000", size: 2, opacity: 1, fill: null, center: [200, 0], width: 20, height: 20, rotation: 0 }));
const rig = unwrap(makeRig([upper, lower], [unwrap(makeBinding(0, id("lower")))]));
const rest = unwrap(makeCel(unwrap(parseHold(1)), "key", [hand]));
const round = (p: readonly number[]) => p.map((n) => Math.round(n));

describe("rig", () => {
  it("turning the forearm carries the hand around the elbow", () => {
    const pose = pointBone(rig, restPose(), id("lower"), [100, 100]);
    const baked = unwrap(bake(rig, rest, pose));
    const h = baked[0];
    expect(h?.kind === "shape" && round(h.center)).toEqual([100, 100]);
  });

  it("reaching moves the whole arm so the wrist lands on the target", () => {
    const pose = reachWith(rig, restPose(), id("lower"), [120, 120]);
    const wrist = posedBones(rig, pose).find((b) => b.bone.id === "lower")?.to ?? [0, 0];
    expect(Math.hypot(wrist[0] - 120, wrist[1] - 120)).toBeLessThan(1);
  });

  it("binds each part to the bone nearest its middle", () => {
    const bound = unwrap(bindNearest(unwrap(makeRig([upper, lower], [])), rest));
    expect(bound.bindings.map((b) => b.bone)).toEqual(["lower"]);
  });

  it("refuses a bone whose parent comes after it", () => {
    expect(makeRig([lower, upper], []).ok).toBe(false);
  });
});
