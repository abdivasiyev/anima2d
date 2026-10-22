import { describe, expect, it } from "vitest";

import { addItems, newProject, recolorAll, removeMotionKey, setMotionKey, typewriter } from "./edit";
import {
  isDrawing,
  makeMotionKey,
  parseCanvas,
  parseColor,
  parseFade,
  parseFrameIndex,
  parseHold,
  parseItem,
  parseProjectId,
  parseProjectName,
  parseScale,
  parseVec,
  normalizeAngle,
  type Project,
} from "./project";
import { unwrap } from "./result";
import { motionAt } from "../editor/output";

const blank = (): Project =>
  newProject(unwrap(parseProjectId("p")), unwrap(parseProjectName("Test")), unwrap(parseCanvas({ width: 1920, height: 1080 })), 24);

const key = (frame: number, x: number, scale = 1) =>
  makeMotionKey(unwrap(parseFrameIndex(frame)), unwrap(parseVec([x, 0])), unwrap(parseScale(scale)), normalizeAngle(0), unwrap(parseFade(1)), "linear");

const text = unwrap(parseItem({ kind: "text", layer: "line", text: "Hi there", font: "serif", style: "normal", size: 40, align: "center", color: "#112233", opacity: 1, center: [10, 10], rotation: 0 }));

describe("layer motion", () => {
  const pivot = unwrap(parseVec([0, 0]));

  it("keeps keys in frame order, one per frame, and eases between them", () => {
    let p = unwrap(setMotionKey(blank(), 0, key(10, 100, 4), pivot));
    p = unwrap(setMotionKey(p, 0, key(0, 0, 1), pivot));
    p = unwrap(setMotionKey(p, 0, key(10, 200, 4), pivot));
    const l = p.layers[0];
    expect(l.motion?.keys.map((k) => k.frame)).toEqual([0, 10]);
    expect(motionAt(l, 5).x).toBeCloseTo(100);
    // Scale moves evenly in ratio: halfway from 1 to 4 is 2.
    expect(motionAt(l, 5).scale).toBeCloseTo(2);
    expect(motionAt(l, 50).x).toBe(200);
  });

  it("drops the motion with its last key", () => {
    const p = unwrap(setMotionKey(blank(), 0, key(3, 50), pivot));
    expect(unwrap(removeMotionKey(p, 0, 3)).layers[0].motion).toBeNull();
  });
});

describe("text", () => {
  it("types on a letter at a time, skipping spaces", () => {
    const p = unwrap(addItems(blank(), { layer: 0, cel: 0 }, [text]));
    const typed = unwrap(typewriter(p, { layer: 0, cel: 0 }, 0, "letter", unwrap(parseHold(2))));
    const l = typed.layers[0];
    const shown = isDrawing(l) ? l.cels.map((c) => (c.items[0]?.kind === "text" ? c.items[0].text : "")) : [];
    expect(shown).toEqual(["H", "Hi", "Hi t", "Hi th", "Hi the", "Hi ther", "Hi there"]);
  });

  it("recolours every item in a colour, everywhere", () => {
    const p = unwrap(addItems(blank(), { layer: 0, cel: 0 }, [text]));
    const q = unwrap(recolorAll(p, unwrap(parseColor("#112233")), unwrap(parseColor("#ff0000"))));
    const l = q.layers[0];
    const item = isDrawing(l) ? l.cels[0].items[0] : undefined;
    expect(item?.kind === "text" ? item.color : "").toBe("#ff0000");
  });
});
