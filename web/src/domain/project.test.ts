import fc from "fast-check";
import { describe, expect, it } from "vitest";

import v1Raw from "../../../fixtures/project-v1.json?raw";
import v1UpgradedRaw from "../../../fixtures/project-v1-upgraded.json?raw";
import v2Raw from "../../../fixtures/project-v2.json?raw";
import rejectionsRaw from "../../../fixtures/rejections.json?raw";
import { parseColor, parseHeight, parseProject, parseProjectId, parseProjectName, parseWidth, unparseProject } from "./project";
import { unwrap } from "./result";
import { array, decodeJson, object, string } from "./wire";

const coord = fc.double({ min: -5000, max: 5000, noNaN: true });
const pressure = fc.double({ min: 0, max: 1, noNaN: true });
const color = fc.stringMatching(/^#[0-9a-f]{6}$/);
const size = fc.double({ min: 0.5, max: 200, noNaN: true });
const opacity = fc.double({ min: 0.05, max: 1, noNaN: true });
const layer = fc.constantFrom("line", "color");
const points = fc.array(fc.tuple(coord, coord, pressure), { minLength: 1, maxLength: 8 });
const vec = fc.tuple(coord, coord);

const item = fc.oneof(
  fc.record({ kind: fc.constant("stroke"), layer, brush: fc.constantFrom("pen", "pencil", "marker", "paint"), color, size, opacity, points }),
  fc.record({ kind: fc.constant("erase"), size, points }),
  fc.record({
    kind: fc.constant("scatter"),
    layer,
    brush: fc.constantFrom("grass", "birds", "stars", "leaves", "flowers", "spray"),
    color,
    size,
    opacity,
    seed: fc.integer({ min: 0, max: 2147483647 }),
    points,
  }),
  fc.record({
    kind: fc.constant("shape"),
    layer,
    shape: fc.constantFrom("line", "rect", "ellipse", "triangle"),
    color,
    size,
    opacity,
    fill: fc.option(color, { nil: null }),
    center: vec,
    width: fc.double({ min: 0, max: 200000, noNaN: true }),
    height: fc.double({ min: 0, max: 200000, noNaN: true }),
    rotation: fc.double({ min: -Math.PI, max: Math.PI, noNaN: true }),
  }),
  fc.record({ kind: fc.constant("curve"), layer, color, size, opacity, points: fc.tuple(vec, vec, vec) }),
  fc.record({ kind: fc.constant("fill"), layer, color, opacity, polygons: fc.array(fc.array(vec, { minLength: 3, maxLength: 6 }), { minLength: 1, maxLength: 3 }) }),
);

const rawProject = fc.record({
  format: fc.constant(2),
  id: fc.stringMatching(/^[a-z0-9-]{1,64}$/),
  name: fc.stringMatching(/^[A-Za-z0-9][A-Za-z0-9 ]{0,78}[A-Za-z0-9]$/),
  canvas: fc.record({ width: fc.integer({ min: 32, max: 3840 }).map((n) => n * 2), height: fc.integer({ min: 32, max: 3840 }).map((n) => n * 2) }),
  fps: fc.constantFrom(12, 24, 25, 30, 48, 50, 60),
  frames: fc.array(fc.record({ hold: fc.integer({ min: 1, max: 24 }), items: fc.array(item, { maxLength: 4 }) }), { minLength: 1, maxLength: 3 }),
});

const fixture = (raw: string): unknown => unwrap(decodeJson(raw));

describe("Project", () => {
  it("parse ∘ unparse is the identity on what parse accepts", () => {
    fc.assert(
      fc.property(rawProject, (raw) => {
        const p = parseProject(raw);
        expect(p.ok).toBe(true);
        if (!p.ok) return;
        expect(parseProject(unparseProject(p.value))).toEqual(p);
      }),
    );
  });

  it("writes back exactly the format 2 it read", () => {
    fc.assert(
      fc.property(rawProject, (raw) => {
        const p = parseProject(raw);
        if (p.ok) expect(unparseProject(p.value)).toEqual(raw);
      }),
    );
  });
});

// The same files drive the Haskell suite (test/Anima/FixtureSpec.hs): both sides must
// accept, upgrade and reject alike, word for word.
describe("shared fixtures", () => {
  it("a format 2 project with every kind of item round-trips", () => {
    const raw = fixture(v2Raw);
    expect(unparseProject(unwrap(parseProject(raw)))).toEqual(raw);
  });

  it("a format 1 project upgrades to format 2", () => {
    expect(unparseProject(unwrap(parseProject(fixture(v1Raw))))).toEqual(fixture(v1UpgradedRaw));
  });

  const cases = unwrap(array(fixture(rejectionsRaw)));
  for (const c of cases) {
    const o = unwrap(object(c));
    const name = unwrap(string(o.name));
    it(`rejects: ${name}`, () => {
      expect(parseProject(o.input)).toEqual({ ok: false, error: unwrap(string(o.error)) });
    });
  }
});

describe("values", () => {
  it("ProjectId refuses paths and capitals", () => {
    for (const bad of ["../etc", "ABC", "", "a".repeat(65)]) expect(parseProjectId(bad).ok).toBe(false);
  });

  it("ProjectName is trimmed and never repeats a refused input", () => {
    expect(parseProjectName("  Walk cycle ")).toEqual({ ok: true, value: "Walk cycle" });
    const r = parseProjectName("secret".repeat(20));
    expect(r.ok).toBe(false);
    if (!r.ok) expect(r.error).not.toContain("secret");
  });

  it("dimensions are even and in range", () => {
    expect(parseWidth(3840).ok).toBe(true);
    expect(parseHeight(2160).ok).toBe(true);
    for (const n of [1919, 62, 7682, 1.5]) expect(parseWidth(n).ok).toBe(false);
  });

  it("Color is kept lower-case", () => {
    expect(parseColor("#A1B2C3")).toEqual({ ok: true, value: "#a1b2c3" });
    expect(parseColor("#abc").ok).toBe(false);
  });
});
